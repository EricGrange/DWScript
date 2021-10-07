{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
{
  Server based on HTTP.sys 2.0 (Win 7 or Win 2k8 minimum)

  This file is based on Synopse framework's example.

  Synopse framework. Copyright (C) 2012 Arnaud Bouchez
    Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL
}
unit USynopseSimpleWebServer;

interface

uses
  Windows, SysUtils, Classes,
  SynZip,
  dwsHTTPSysServer, dwsHTTPSysAPI,
  dwsUtils, dwsWebEnvironment, dwsSynopseWebEnv, dwsFileSystem,
  dwsDirectoryNotifier, dwsJSON, dwsXPlatform, dwsWebServerHelpers,
  DSimpleDWScript;

type

   TSynopseSimpleServer = class
      protected
         FPath : TFileName;
         FServer : THttpApi2Server;
         FFileSystem : TdwsRestrictedFileSystem;
         FDWS : TSynDWScript;
         FNotifier : TdwsDirectoryNotifier;
         FPort : Integer;
         FSSLPort : Integer;
         FRelativeURI : String;
         FSSLRelativeURI : String;
         FDirectoryIndex : TDirectoryIndexCache;

         procedure DirectoryChanged(sender : TdwsDirectoryNotifier);

      public
         constructor Create(const basePath : TFileName; options : TdwsJSONValue);
         destructor Destroy; override;

         function Process(const inRequest : TSynHttpServerRequest;
                          var outResponse : TSynHttpServerResponse) : cardinal;

         function FindDirectoryIndex(var pathFileName : String) : Boolean;

         property Port : Integer read FPort;
         property SSLPort : Integer read FSSLPort;
         property RelativeURI : String read FRelativeURI;
         property SSLRelativeURI : String read FSSLRelativeURI;
  end;

const
   cDefaultServerOptions =
      '{'
         // http server port, if zero, no http port is opened
         +'"Port": 888,'
         // http relative URI
         +'"RelativeURI": "",'
         // https server port, if zero, no https port is opened
         +'"SSLPort": 0,'
         // https relative URI
         +'"SSLRelativeURI": "",'
         // Base path for served files
         // If not defined, assumes a www subfolder of the folder where the exe is
         +'"WWWPath": "",'
         // Number of WorkerThreads
         +'"WorkerThreads": 8,'
         // Directory for log files (NCSA)
         // If empty, logs are not active
         +'"LogDirectory": "",'
         // Maximum number of connections
         // Zero means "infinite"
         +'"MaxConnections": 0,'
         // Maximum bandwidth in bytes per second
         // Zero means "infinite"
         +'"MaxBandwidth": 0,'
         // Maximum input http request length in bytes
         // If zero or negative, defaults to 10 Megabytes
         // requests larger than this value will get canceled
         +'"MaxInputLength": 0'
      +'}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSynopseSimpleServer ------------------
// ------------------

constructor TSynopseSimpleServer.Create(const basePath : TFileName; options : TdwsJSONValue);
var
   logPath : TdwsJSONValue;
   serverOptions : TdwsJSONValue;
   nbThreads : Integer;
begin
   FPath:=IncludeTrailingPathDelimiter(ExpandFileName(basePath));

   FFileSystem:=TdwsRestrictedFileSystem.Create(nil);
   FFileSystem.Paths.Add(FPath);

   FDWS:=TSynDWScript.Create(nil);
   FDWS.FileSystem:=FFileSystem;

   FDWS.PathVariables.Values['www']:=FPath;

   FDirectoryIndex:=TDirectoryIndexCache.Create;
   FDirectoryIndex.IndexFileNames.CommaText:='"index.dws","index.htm","index.html"';

   serverOptions:=TdwsJSONValue.ParseString(cDefaultServerOptions);
   try
      serverOptions.Extend(options['Server']);

      FServer:=THttpApi2Server.Create(False);

      FRelativeURI:=serverOptions['RelativeURI'].AsString;
      FPort:=serverOptions['Port'].AsInteger;
      if FPort<>0 then
         FServer.AddUrl(FRelativeURI, FPort, False, '+');

      FSSLRelativeURI:=serverOptions['SSLRelativeURI'].AsString;
      FSSLPort:=serverOptions['SSLPort'].AsInteger;
      if FSSLPort<>0 then begin
         FServer.AddUrl('', FSSLPort, True, '+');
      end;
      FServer.RegisterCompress(CompressDeflate);
      FServer.OnRequest:=Process;

      nbThreads:=serverOptions['WorkerThreads'].AsInteger;

      logPath:=serverOptions['LogDirectory'];
      if (logPath.ValueType=jvtString) and (logPath.AsString<>'') then begin
         FServer.LogDirectory:=IncludeTrailingPathDelimiter(FDWS.ApplyPathVariables(logPath.AsString));
         FServer.LogRolloverSize:=1024*1024;
         FServer.Logging:=True;
      end;

      FServer.MaxConnections:=serverOptions['MaxConnections'].AsInteger;

      FServer.MaxBandwidth:=serverOptions['MaxBandwidth'].AsInteger;

      FServer.MaxInputCountLength:=serverOptions['MaxInputLength'].AsInteger;
   finally
      serverOptions.Free;
   end;

   FDWS.LoadCPUOptions(options['CPU']);

   FDWS.LoadDWScriptOptions(options['DWScript']);

   FNotifier:=TdwsDirectoryNotifier.Create(FPath, dnoDirectoryAndSubTree);
   FNotifier.OnDirectoryChanged:=DirectoryChanged;

   if nbThreads>1 then
      FServer.Clone(nbThreads-1);
end;

destructor TSynopseSimpleServer.Destroy;
begin
   FNotifier.Free;
   FServer.Free;
   FDWS.Free;
   FFileSystem.Free;
   FDirectoryIndex.Free;
   inherited;
end;


function TSynopseSimpleServer.Process(
      const inRequest : TSynHttpServerRequest;
      var outResponse : TSynHttpServerResponse) : Cardinal;
var
   pathFileName : String;
   params : String;
   request : TSynopseWebRequest;
   response : TSynopseWebResponse;
   fileAttribs : Cardinal;
begin
   HttpRequestUrlDecode(inRequest.InURL, pathFileName, params);

   pathFileName:=ExpandFileName(FPath+pathFileName);

   if Pos('\.', pathFileName)>0 then

      // Directories or files beginning with a '.' are invisible
      fileAttribs:=INVALID_FILE_ATTRIBUTES

   else if not StrBeginsWith(pathFileName, FPath) then begin

      // request is outside base path
      outResponse.OutContent:='<h1>Not authorized</h1>';
      outResponse.OutContentType:=cHTMTL_UTF8_CONTENT_TYPE;
      Result:=401;
      Exit;

   end else begin

      {$WARN SYMBOL_PLATFORM OFF}
      fileAttribs:=GetFileAttributes(Pointer(pathFileName));
      if fileAttribs<>INVALID_FILE_ATTRIBUTES then begin
         if (fileAttribs and faHidden)<>0 then
            fileAttribs:=INVALID_FILE_ATTRIBUTES
         else if (fileAttribs and faDirectory)<>0 then begin
            if not FindDirectoryIndex(pathFileName) then
               fileAttribs:=INVALID_FILE_ATTRIBUTES;
         end;
      end;
      {$WARN SYMBOL_PLATFORM ON}

   end;

   if fileAttribs=INVALID_FILE_ATTRIBUTES then begin

      outResponse.OutContent:='<h1>Not found</h1>';
      outResponse.OutContentType:=cHTMTL_UTF8_CONTENT_TYPE;
      Result:=404;
      Exit;

   end else if StrEndsWith(pathFileName, '.dws') then begin

      request:=TSynopseWebRequest.Create;
      response:=TSynopseWebResponse.Create;
      try
         request.RemoteIP:=inRequest.RemoteIP;
         request.InURL:=inRequest.InURL;
         request.InMethod:=inRequest.InMethod;
         request.InHeaders:=inRequest.InHeaders;
         request.InContent:=inRequest.InContent;
         request.InContentType:=inRequest.InContentType;
         case inRequest.Security of
            hrsSSL : request.Security:=Format('SSL, %d bits', [inRequest.SecurityBytes*8]);
         else
            request.Security:='';
         end;

         response.StatusCode:=200;
         response.ContentType:=cHTMTL_UTF8_CONTENT_TYPE;

         FDWS.HandleDWS(pathFileName, request, response);

         outResponse.OutContent:=response.ContentData;
         outResponse.OutContentType:=response.ContentType;
         if response.HasHeaders then
            outResponse.OutCustomHeader:=outResponse.OutCustomHeader+response.CompiledHeaders;

         Result:=response.StatusCode;
      finally
         request.Free;
         response.Free;
      end;

   end else begin

      // http.sys will send the specified file from kernel mode
      outResponse.OutContent:=UTF8Encode(pathFileName);
      outResponse.OutContentType:=HTTP_RESP_STATICFILE;
      Result:=200; // THttpApiServer.Execute will return 404 if not found

   end;
end;

// FindDirectoryIndex
//
function TSynopseSimpleServer.FindDirectoryIndex(var pathFileName : String) : Boolean;
begin
   Result:=FDirectoryIndex.IndexFileForDirectory(pathFileName);
end;

// DirectoryChanged
//
procedure TSynopseSimpleServer.DirectoryChanged(sender : TdwsDirectoryNotifier);
begin
   FDWS.FlushDWSCache;
   FDirectoryIndex.Flush;
end;

end.
