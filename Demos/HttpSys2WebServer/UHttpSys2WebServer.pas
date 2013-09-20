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

{$I dws.inc}

interface

uses
  Windows, SysUtils, Classes,
  SynZip,
  dwsHTTPSysServer, dwsHTTPSysAPI,
  dwsUtils, dwsWebEnvironment, dwsFileSystem,
  dwsDirectoryNotifier, dwsJSON, dwsXPlatform,
  dwsWebServerHelpers, dwsWebServerInfo,
  DSimpleDWScript;

type
   IHttpSys2WebServer = interface
      procedure Shutdown;

      function HttpPort : Integer;
      function HttpsPort : Integer;
   end;

   THttpSys2WebServer = class (TInterfacedSelfObject, IHttpSys2WebServer, IWebServerInfo)
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
         // Used to implement a lazy flush on FileAccessInfoCaches
         FCacheCounter : Cardinal;
         FFileAccessInfoCacheSize : Integer;

         procedure FileChanged(sender : TdwsFileNotifier; const fileName : String;
                               changeAction : TFileNotificationAction);

         procedure LoadAuthenticateOptions(authOptions : TdwsJSONValue);

         function HttpPort : Integer;
         function HttpsPort : Integer;

      public
         constructor Create(const basePath : TFileName; options : TdwsJSONValue);
         destructor Destroy; override;

         procedure Shutdown;

         procedure Process(request : TWebRequest; response : TWebResponse);

         procedure Redirect301TrailingPathDelimiter(request : TWebRequest; response : TWebResponse);

         function FindDirectoryIndex(var pathFileName : String) : Boolean;

         function Name : String;

         function Authentications : TWebRequestAuthentications;

         property Port : Integer read FPort;
         property SSLPort : Integer read FSSLPort;
         property RelativeURI : String read FRelativeURI;
         property SSLRelativeURI : String read FSSLRelativeURI;
         property AutoRedirectFolders : Boolean read FAutoRedirectFolders;
         property FileAccessInfoCacheSize : Integer read FFileAccessInfoCacheSize write FFileAccessInfoCacheSize;

         property DWS : TSimpleDWScript read FDWS;
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
         // is HTTP compression activated
         +'"Compression": true,'
         // Base path for served files,
         // If not defined, assumes a www subfolder of the folder where the exe is
         +'"WWWPath": "",'
         // Enabled Authentication options
         // Allowed values are "Basic", "Digest", "NTLM", "Negotiate" and "*" for all
         +'"Authentication": [],'
         // Number of WorkerThreads
         +'"WorkerThreads": 16,'
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

// ExpandPathFileName
//
function ExpandPathFileName(const path : String; var fileName : String) : Boolean;
var
   fileNamePtr : PChar;
   bufferIn : array [0..MAX_PATH] of Char;
   bufferOut : array [0..MAX_PATH] of Char;
   n1, n2, n : Integer;
begin
   n1:=Length(path);
   Move(path[1], bufferIn[0], n1*SizeOf(Char));
   n2:=Length(fileName);
   if n1+n2>MAX_PATH then
      Exit(False);
   if n2>0 then
      Move(fileName[1], bufferIn[n1], n2*SizeOf(char));
   bufferIn[n1+n2]:=#0;

   n:=GetFullPathName(bufferIn, Length(bufferOut), bufferOut, fileNamePtr);
   if Cardinal(n)<=Cardinal(Length(bufferOut)) then begin
      SetString(fileName, bufferOut, n);
      Result:=True;
   end else Result:=False;
end;

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
   FDWS.Initialize(Self);

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

      if serverOptions['Compression'].AsBoolean then
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

      FFileAccessInfoCacheSize:=256;
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

// Shutdown
//
procedure THttpSys2WebServer.Shutdown;
begin
   FDWS.Finalize;
end;

// Process
//
procedure THttpSys2WebServer.Process(request : TWebRequest; response : TWebResponse);
var
   noTrailingPathDelimiter : Boolean;
   infoCache : TFileAccessInfoCache;
   fileInfo : TFileAccessInfo;
begin
   infoCache:=TFileAccessInfoCache(request.Custom);
   if infoCache=nil then begin
      infoCache:=TFileAccessInfoCache.Create(FileAccessInfoCacheSize);
      request.Custom:=infoCache;
      infoCache.CacheCounter:=FCacheCounter;
   end else if infoCache.CacheCounter<>FCacheCounter then begin
      infoCache.Flush;
      infoCache.CacheCounter:=FCacheCounter;
   end;

   fileInfo:=infoCache.FileAccessInfo(request.PathInfo);
   if fileInfo=nil then begin

      fileInfo:=infoCache.CreateFileAccessInfo(request.PathInfo);

      if not ExpandPathFileName(FPath, fileInfo.CookedPathName) then

         // invalid pathFileName
         fileInfo.fileAttribs:=INVALID_FILE_ATTRIBUTES

      else if StrContains(fileInfo.CookedPathName, '\.') then

         // Directories or files beginning with a '.' are invisible
         fileInfo.fileAttribs:=INVALID_FILE_ATTRIBUTES

      else if not StrBeginsWith(fileInfo.CookedPathName, FPath) then begin

         // request is outside base path
         fileInfo.fileAttribs:=FILE_ATTRIBUTE_SYSTEM;

      end else begin

         {$WARN SYMBOL_PLATFORM OFF}
         fileInfo.fileAttribs:=GetFileAttributes(Pointer(fileInfo.CookedPathName));
         if fileInfo.fileAttribs<>INVALID_FILE_ATTRIBUTES then begin
            if (fileInfo.fileAttribs and faHidden)<>0 then
               fileInfo.fileAttribs:=INVALID_FILE_ATTRIBUTES
            else if (fileInfo.fileAttribs and faDirectory)<>0 then begin
               noTrailingPathDelimiter:=AutoRedirectFolders and (not StrEndsWith(request.PathInfo, '/'));
               if not FindDirectoryIndex(fileInfo.CookedPathName) then
                  fileInfo.fileAttribs:=INVALID_FILE_ATTRIBUTES
               else if noTrailingPathDelimiter then
                  fileInfo.fileAttribs:=FILE_ATTRIBUTE_DIRECTORY
               else fileInfo.FileAttribs:=FILE_ATTRIBUTE_VIRTUAL;
            end;
         end;
         {$WARN SYMBOL_PLATFORM ON}

         fileInfo.DWScript := StrEndsWith(fileInfo.CookedPathName, '.dws');

      end;

   end;


   case fileInfo.FileAttribs of
      INVALID_FILE_ATTRIBUTES : begin
         response.ContentData:='<h1>Not found</h1>';
         response.StatusCode:=404;
      end;
      FILE_ATTRIBUTE_SYSTEM : begin
         response.ContentData:='<h1>Not authorized</h1>';
         response.StatusCode:=401;
      end;
      FILE_ATTRIBUTE_DIRECTORY :
         Redirect301TrailingPathDelimiter(request, response);
   else
      if fileInfo.DWScript then begin

         FDWS.HandleDWS(fileInfo.CookedPathName, request, response);

      end else begin

         // http.sys will send the specified file from kernel mode
         // THttpApiServer.Execute will return 404 if not found
         response.ContentData:=UTF8Encode(fileInfo.CookedPathName);
         response.ContentType:=HTTP_RESP_STATICFILE;

      end;
   end;
end;

// Redirect301TrailingPathDelimiter
//
procedure THttpSys2WebServer.Redirect301TrailingPathDelimiter(request : TWebRequest; response : TWebResponse);
begin
   response.Headers.Add('Location='+request.PathInfo+'/');
   response.StatusCode:=301;
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

// Name
//
function THttpSys2WebServer.Name : String;
begin
   Result:=FServer.ServerName;
end;

// Authentications
//
function THttpSys2WebServer.Authentications : TWebRequestAuthentications;
var
   auth : Cardinal;
begin
   auth:=FServer.Authentication;
   Result:=[];
   if (HTTP_AUTH_ENABLE_BASIC and auth)<>0 then
      Include(Result, wraBasic);
   if (HTTP_AUTH_ENABLE_DIGEST and auth)<>0 then
      Include(Result, wraDigest);
   if (HTTP_AUTH_ENABLE_NTLM and auth)<>0 then
      Include(Result, wraNTLM);
   if (HTTP_AUTH_ENABLE_NEGOTIATE and auth)<>0 then
      Include(Result, wraNegotiate);
   if (HTTP_AUTH_ENABLE_KERBEROS and auth)<>0 then
      Include(Result, wraKerberos);
end;

// FileChanged
//
procedure THttpSys2WebServer.FileChanged(sender : TdwsFileNotifier; const fileName : String;
                                         changeAction : TFileNotificationAction);
begin
   FDWS.FlushDWSCache(fileName);
   if (not StrContains(fileName, '\.')) and StrContains(fileName, '\index.') then
      FDirectoryIndex.Flush;
   Inc(FCacheCounter);
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

// HttpPort
//
function THttpSys2WebServer.HttpPort : Integer;
begin
   Result:=Port;
end;

// HttpsPort
//
function THttpSys2WebServer.HttpsPort : Integer;
begin
   Result:=SSLPort;
end;

end.
