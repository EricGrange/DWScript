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

  This file is loosely based on Synopse framework's example.

  Synopse framework. Copyright (C) 2012 Arnaud Bouchez
    Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL
}
unit UHttpSys2WebServer;

interface

uses
  Windows, SysUtils, Classes,
  SynZip,
  dwsHTTPSysServer, dwsHTTPSysAPI,
  dwsUtils, dwsWebEnvironment, dwsSynopseWebEnv, dwsFileSystem,
  dwsDirectoryNotifier, dwsJSON, dwsXPlatform, dwsWebServerHelpers,
  DSimpleDWScript;

type

   THttpSys2WebServer = class
      protected
         FPath : TFileName;
         FServer : THttpApi2Server;
         FDWS : TSimpleDWScript;
         FNotifier : TdwsFileNotifier;
         FPort : Integer;
         FSSLPort : Integer;
         FRelativeURI : String;
         FSSLRelativeURI : String;
         FDirectoryIndex : TDirectoryIndexCache;
         FAutoRedirectFolders : Boolean;

         procedure FileChanged(sender : TdwsFileNotifier; const fileName : String;
                               changeAction : TFileNotificationAction);

         procedure LoadAuthenticateOptions(authOptions : TdwsJSONValue);

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
         property AutoRedirectFolders : Boolean read FAutoRedirectFolders;
  end;

const
   cDefaultServerOptions =
      '{'
         // name to report in responses
         +'"Name": "DWScript",'
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
         // Enabled Authentication options
         // Allowed values are "Basic", "Digest", "NTLM", "Negotiate" and "*" for all
         +'"Authentication": [],'
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
         +'"MaxInputLength": 0,'
         // If true folder requests that don't include the trailing path delimiter
         // will automatically be redirected with a 301 error
         +'"AutoRedirectFolders": true'
      +'}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ THttpSys2WebServer ------------------
// ------------------

constructor THttpSys2WebServer.Create(const basePath : TFileName; options : TdwsJSONValue);
var
   logPath : TdwsJSONValue;
   serverOptions : TdwsJSONValue;
   nbThreads : Integer;
begin
   FPath:=IncludeTrailingPathDelimiter(ExpandFileName(basePath));

   FDWS:=TSimpleDWScript.Create(nil);

   FDWS.PathVariables.Values['www']:=ExcludeTrailingPathDelimiter(FPath);

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

      FServer.ServerName:=serverOptions['Name'].AsString;

      nbThreads:=serverOptions['WorkerThreads'].AsInteger;

      logPath:=serverOptions['LogDirectory'];
      if (logPath.ValueType=jvtString) and (logPath.AsString<>'') then begin
         FServer.LogDirectory:=IncludeTrailingPathDelimiter(FDWS.ApplyPathVariables(logPath.AsString));
         FServer.LogRolloverSize:=1024*1024;
         FServer.Logging:=True;
      end;

      LoadAuthenticateOptions(serverOptions['Authentication']);

      FServer.MaxConnections:=serverOptions['MaxConnections'].AsInteger;

      FServer.MaxBandwidth:=serverOptions['MaxBandwidth'].AsInteger;

      FServer.MaxInputCountLength:=serverOptions['MaxInputLength'].AsInteger;

      FAutoRedirectFolders:=serverOptions['AutoRedirectFolders'].AsBoolean;
   finally
      serverOptions.Free;
   end;

   FDWS.LoadCPUOptions(options['CPU']);

   FDWS.LoadDWScriptOptions(options['DWScript']);

   FNotifier:=TdwsFileNotifier.Create(FPath, dnoDirectoryAndSubTree);
   FNotifier.OnFileChanged:=FileChanged;

   if nbThreads>1 then
      FServer.Clone(nbThreads-1);
end;

destructor THttpSys2WebServer.Destroy;
begin
   FNotifier.Free;
   FServer.Free;
   FDWS.Free;
   FDirectoryIndex.Free;
   inherited;
end;


function THttpSys2WebServer.Process(
      const inRequest : TSynHttpServerRequest;
      var outResponse : TSynHttpServerResponse) : Cardinal;
var
   pathFileName : String;
   params : String;
   request : TSynopseWebRequest;
   response : TSynopseWebResponse;
   fileAttribs : Cardinal;
   noTrailingPathDelimiter : Boolean;
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
            noTrailingPathDelimiter:=AutoRedirectFolders and (not StrEndsWith(pathFileName, '\'));
            if not FindDirectoryIndex(pathFileName) then
               fileAttribs:=INVALID_FILE_ATTRIBUTES
            else if noTrailingPathDelimiter then begin
               outResponse.OutCustomHeader:='Location: '+inRequest.InURL+'/';
               Result:=301;
               Exit;
            end;
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
         request.InURL:=inRequest.InURL;
         request.InMethod:=inRequest.InMethod;
         request.InHeaders:=inRequest.InHeaders;
         request.InContent:=inRequest.InContent;
         request.InContentType:=inRequest.InContentType;
         request.Authentication:=inRequest.Authentication;
         request.AuthenticatedUser:=inRequest.AuthenticatedUser;
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
function THttpSys2WebServer.FindDirectoryIndex(var pathFileName : String) : Boolean;
var
   noTrailingPathDelimiter : Boolean;
begin
   noTrailingPathDelimiter:=AutoRedirectFolders and (not StrEndsWith(pathFileName, '\'));

   Result:=FDirectoryIndex.IndexFileForDirectory(pathFileName);
   if Result and noTrailingPathDelimiter then

end;

// FileChanged
//
procedure THttpSys2WebServer.FileChanged(sender : TdwsFileNotifier; const fileName : String;
                                         changeAction : TFileNotificationAction);
begin
   if    StrEndsWith(fileName, '.dws')
      or StrEndsWith(fileName, '.inc')
      or StrEndsWith(fileName, '.pas') then
      FDWS.FlushDWSCache;
   if (Pos('\.', fileName)<=0) and (Pos('\index.', fileName)>0) then
      FDirectoryIndex.Flush;
end;

// LoadAuthenticateOptions
//
procedure THttpSys2WebServer.LoadAuthenticateOptions(authOptions : TdwsJSONValue);
const
   cAuthName : array [0..5] of String = (
      'Basic', 'Digest', 'NTLM', 'Negotiate', 'Kerberos', '*'
   );
   cAuthMasks : array [0..5] of Cardinal = (
      HTTP_AUTH_ENABLE_BASIC, HTTP_AUTH_ENABLE_DIGEST, HTTP_AUTH_ENABLE_NTLM,
      HTTP_AUTH_ENABLE_NEGOTIATE, HTTP_AUTH_ENABLE_KERBEROS, HTTP_AUTH_ENABLE_ALL
      );
var
   authMask : Cardinal;
   authName : String;
   i, j : Integer;
begin
   authMask:=0;
   for i:=0 to authOptions.ElementCount-1 do begin
      authName:=authOptions.Elements[i].AsString;
      for j:=Low(cAuthName) to High(cAuthName) do begin
         if UnicodeSameText(authName, cAuthName[j]) then begin
            authMask:=authMask or cAuthMasks[j];
            Break;
         end;
      end;
   end;
   if authMask<>0 then
      FServer.SetAuthentication(authMask);
end;

end.
