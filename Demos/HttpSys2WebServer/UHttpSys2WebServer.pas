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

{$define EnablePas2JS}

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, System.Types,
  dwsHTTPSysServer, dwsHTTPSysAPI, dwsHTTPSysServerEvents, dwsXXHash,
  dwsUtils, dwsWebEnvironment, dwsWebEnvironmentTypes, dwsFileSystem,
  dwsJSON, dwsXPlatform, dwsURLRewriter,
  dwsWebServerHelpers, dwsWebServerInfo, dwsWebUtils,
  DSimpleDWScript;

type
   IHttpSys2WebServer = interface
      function URLInfos : THttpSys2URLInfos;

      procedure Shutdown;
   end;

   TDWSExtension = record
      Str : String;
      Typ : TFileAccessType;
   end;
   TDWSExtensions = array of TDWSExtension;
   PDWSExtensions = ^TDWSExtensions;

   THttpSys2WebServer = class (TInterfacedSelfObject, IHttpSys2WebServer, IWebServerInfo)
      protected
         FPath : TFileName;
         FServer : THttpApi2Server;
         FServerEvents : IdwsHTTPServerEvents;
         FDWS : TSimpleDWScript;
         FURLInfos : THttpSys2URLInfos;
         FDirectoryIndex : TDirectoryIndexCache;
         FAutoRedirectFolders : Boolean;
         FErrorPagesPath : String;
         // Used to implement a lazy flush on FileAccessInfoCaches
         FCacheCounter : Cardinal;
         FFileAccessInfoCacheSize : Integer;
         FDWSExtensions : array ['a'..'z' ] of TDWSExtensions;
         FDefaultFileAccessType : TFileAccessType;
         FMethodsNotAllowed : TWebRequestMethodVerbs;
         FURLRewriter : TdwsURLRewriter;

         procedure LoadAuthenticateOptions(authOptions : TdwsJSONValue);

         function URLInfos : THttpSys2URLInfos;
         function HttpPort : Integer;
         function HttpsPort : Integer;

         function NormalizedExtensionLastChar(lastChar : Char) : Char; inline;
         procedure RegisterExtensions(list : TdwsJSONValue; typ : TFileAccessType);
         procedure SortExtensionsByLength;

         function FileAccessTypeFromFileName(const fileName : TFileName) : TFileAccessType;

         function ParseMethodList(list : TdwsJSONValue) : TWebRequestMethodVerbs;

         procedure Initialize(const basePath : TFileName; options : TdwsJSONValue;
                              const aServiceName : String); virtual;

         function  GetURLRewriteRules : String;
         procedure SetURLRewriteRules(const json : String);

         procedure DoOnHTTPThreadException(sender : TThread; var e : Exception);

      public
         constructor Create; overload;
         class function Create(const basePath : TFileName; options : TdwsJSONValue;
                               const aServiceName : String) : THttpSys2WebServer; overload;
         destructor Destroy; override;

         procedure Shutdown;

         procedure Process(request : TWebRequest; response : TWebResponse);
         procedure ProcessStaticFile(const pathName : String; request : TWebRequest; response : TWebResponse);
         procedure ProcessStandardError(request : TWebRequest; statusCode : Integer; const defaultText : String; response : TWebResponse);

         procedure Redirect301TrailingPathDelimiter(request : TWebRequest; response : TWebResponse);

         function FindDirectoryIndex(var pathFileName : String) : Boolean;

         function Name : String;

         function Authentications : TWebRequestAuthentications;

         function LiveQueries : String;

         function CompilationInfoJSON(const sourceName : String) : String;
         function ExecutionInfoJSON(const sourceName : String) : String;
         function CompiledPrograms : TStringDynArray;
         procedure FlushCompiledPrograms;

         function ServerEvents : IdwsHTTPServerEvents;

         property URLRewriter : TdwsURLRewriter read FURLRewriter;

         class function EnumerateURLInfos(options : TdwsJSONValue) : THttpSys2URLInfos;

         property AutoRedirectFolders : Boolean read FAutoRedirectFolders;
         property FileAccessInfoCacheSize : Integer read FFileAccessInfoCacheSize write FFileAccessInfoCacheSize;
         property ErrorPagesPath : String read FErrorPagesPath;

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
         // http domain name URI
         +'"DomainName": "+",'
         // https server port, if zero, no https port is opened
         +'"SSLPort": 0,'
         // http domain name URI
         +'"SSLDomainName": "+",'
         // https relative URI
         +'"SSLRelativeURI": "",'
         // supplemental domains array of {Port, Name, RelativeURI, SSL}
         +'"Domains": [],'
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
         // Directory for DWScript error log files
         // If empty, DWS error logs are not active
         +'"DWSErrorLogDirectory": "",'
         // Directory for unhandled DWScript exception log files
         // If empty, DWS exceptions logs are not active
         +'"DWSExceptionLogDirectory": "",'
         // Directory for log files (NCSA)
         // If empty, logs are not active
         +'"LogDirectory": "",'
         // Maximum number of connections
         // Zero means "infinite"
         +'"MaxConnections": 0,'
         // Adjust http server queue length
         // Zero means default
         +'"MaxQueueLength": 0,'
         // Maximum bandwidth in bytes per second
         // Zero means "infinite"
         +'"MaxBandwidth": 0,'
         // Maximum input http request length in bytes
         // If zero or negative, defaults to 10 Megabytes
         // requests larger than this value will get canceled
         +'"MaxInputLength": 0,'
         // If true folder requests that don't include the trailing path delimiter
         // will automatically be redirected with a 301 error
         +'"AutoRedirectFolders": true,'
         // List of extensions that go through the script filter
         +'"ScriptedExtensions": [".dws"],'
         // List of extensions that go through the Pascal To JavaScript  filter
         +'"P2JSExtensions": [".p2js",".pas.js"],'
         // List of extensions to serve as static files (use null to serve all as static)
         +'"StaticExtensions": ['
            +'".htm",".html",".js",".css",".xml",'    // web
            +'".png",".jpg",".webp",".svg",'          // images
            +'".zip",".7z",".gz",".exe",'             // archives &binaries
            +'".txt",".csv",".json"'                  // text
            +'],'
         // List of HTTP methods that will return a 405 'Not Allowed' response
         +'"MethodsNotAllowed": ["TRACE"]'
      +'}';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SynZip;

const
   cFileCacheExpiryMilliseconds = 1000;
   cFileCacheCountFlush = 200;

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

// Create
//
constructor THttpSys2WebServer.Create;
begin
   inherited;
end;

// Create
//
class function THttpSys2WebServer.Create(const basePath : TFileName; options : TdwsJSONValue;
                                         const aServiceName : String) : THttpSys2WebServer;
begin
   Result:=Self.Create;
   Result.Initialize(basePath, options, aServiceName);
end;

// Destroy
//
destructor THttpSys2WebServer.Destroy;
begin
   if FServer <> nil then begin
      FServer.OnHttpThreadException := nil;
      FServer.Free;
      FServer := nil;
   end;
   FDWS.Free;
   FDirectoryIndex.Free;
   FURLRewriter.Free;
   inherited;
end;

// NormalizedExtensionLastChar
//
function THttpSys2WebServer.NormalizedExtensionLastChar(lastChar : Char) : Char;
begin
   case lastChar of
      'a'..'z' : Result := lastChar;
      'A'..'Z' : Result := Char(Ord(lastChar) + (Ord('a') - Ord('A')));
   else
      Result := Low(FDWSExtensions); //catch all
   end;
end;

// RegisterExtensions
//
procedure THttpSys2WebServer.RegisterExtensions(list : TdwsJSONValue; typ : TFileAccessType);
var
   i, n : Integer;
   lastChar : Char;
   ext : String;
begin
   for i := 0 to list.ElementCount-1 do begin
      ext := list.Elements[i].AsString;
      n := Length(ext);
      if (n < 2) or (ext[1] <> '.') then
         raise Exception.CreateFmt('Invalid file extension "%s"', [ ext ]);
      lastChar := NormalizedExtensionLastChar(ext[n]);

      n := Length(FDWSExtensions[lastChar]);
      SetLength(FDWSExtensions[lastChar], n+1);

      FDWSExtensions[lastChar][n].Str := ext;
      FDWSExtensions[lastChar][n].Typ := typ;
   end;
end;

// SortExtensionsByLength
//
procedure THttpSys2WebServer.SortExtensionsByLength;
begin
   for var lastChar := Low(FDWSExtensions) to High(FDWSExtensions) do begin
      var extensions : PDWSExtensions := @FDWSExtensions[lastChar];
      var n := Length(extensions^);
      for var i := 0 to n - 2 do begin
         var maxIdx := i;
         for var j := i + 1 to n - 1 do
            if Length(extensions^[j].Str) > Length(extensions^[maxIdx].Str) then
               maxIdx := j;
         if maxIdx <> i then
            ExchangeBytes(extensions^[i], extensions^[maxIdx], SizeOf(TDWSExtension));
      end;
   end;
end;

// FileAccessTypeFromFileName
//
function THttpSys2WebServer.FileAccessTypeFromFileName(const fileName : TFileName) : TFileAccessType;
var
   lastChar : Char;
begin
   if fileName <> '' then
      lastChar := NormalizedExtensionLastChar(fileName[Length(fileName)])
   else lastChar := Low(FDWSExtensions);

   var extensions : PDWSExtensions := @FDWSExtensions[lastChar];
   for var i := 0 to High(extensions^) do begin
      if StrIEndsWith(fileName, extensions^[i].Str) then
         Exit(extensions^[i].Typ);
   end;
   Result := FDefaultFileAccessType;
end;

// ParseMethodList
//
function THttpSys2WebServer.ParseMethodList(list : TdwsJSONValue) : TWebRequestMethodVerbs;
var
   i : Integer;
   s : String;
   v : TWebRequestMethodVerb;
   found : Boolean;
begin
   Result:=[];
   for i:=0 to list.ElementCount-1 do begin
      s:=list.Elements[i].AsString;
      found:=False;
      for v:=Low(cWebRequestMethodVerbs) to High(cWebRequestMethodVerbs) do begin
         if UnicodeSameText(s, cWebRequestMethodVerbs[v]) then begin
            Include(Result, v);
            found:=True;
            Break;
         end;
      end;
      if not found then
         FDWS.LogError(Format('Unknown method verb "%s"', [s]));
   end;
end;

// Initialize
//
procedure THttpSys2WebServer.Initialize(const basePath : TFileName; options : TdwsJSONValue;
                                        const aServiceName : String);
var
   logPath, errorLogPath, exceptionLogPath : TdwsJSONValue;
   serverOptions : TdwsJSONValue;
   staticExtensions : TdwsJSONValue;
   env: TdwsJSONObject;
   i, nbThreads : Integer;
begin
   FPath:=IncludeTrailingPathDelimiter(ExpandFileName(basePath));

   FDWS:=TSimpleDWScript.Create(nil);
   FDWS.Initialize(Self);

   serverOptions:=TdwsJSONValue.ParseString(cDefaultServerOptions);
   try
      serverOptions.Extend(options['Server']);

      FDWS.PathVariables.Values['www']:=ExcludeTrailingPathDelimiter(FPath);
      env := options['Server']['Environment'] as TdwsJSONObject;
      if assigned(env) then
         for i := 0 to env.ElementCount - 1 do
            fDWS.PathVariables.Values[env.Names[i]] := env.Elements[i].AsString;

      errorLogPath:=serverOptions['DWSErrorLogDirectory'];
      if (errorLogPath.ValueType=jvtString) and (errorLogPath.AsString<>'') then begin
         FDWS.ErrorLogDirectory:=IncludeTrailingPathDelimiter(FDWS.ApplyPathVariables(errorLogPath.AsString));
      end;

      FErrorPagesPath:=IncludeTrailingPathDelimiter(FPath+'.errors');

      exceptionLogPath:=serverOptions['DWSExceptionLogDirectory'];
      if (exceptionLogPath.ValueType=jvtString) and (exceptionLogPath.AsString<>'') then begin
         FDWS.ExceptionLogDirectory:=IncludeTrailingPathDelimiter(FDWS.ApplyPathVariables(exceptionLogPath.AsString));
      end;

      FDirectoryIndex:=TDirectoryIndexCache.Create;
      FDirectoryIndex.IndexFileNames.CommaText:='"index.dws","index.htm","index.html"';

      FDWS.LoadCPUOptions(options['CPU']);

      FDWS.LoadDWScriptOptions(options['DWScript']);

      FURLRewriter := TdwsURLRewriter.Create;

      FDWS.Startup;

      FServer:=THttpApi2Server.Create(False, aServiceName);

      RegisterExtensions(serverOptions['ScriptedExtensions'], fatDWS);
      RegisterExtensions(serverOptions['P2JSExtensions'], fatP2JS);

      staticExtensions := serverOptions['StaticExtensions'];
      if not staticExtensions.IsNull then begin
         RegisterExtensions(serverOptions['StaticExtensions'], fatRAW);
         FDefaultFileAccessType := fatNone;
      end else begin
         FDefaultFileAccessType := fatRAW;
      end;

      SortExtensionsByLength;

      FURLInfos := EnumerateURLInfos(serverOptions);
      for i := 0 to High(FURLInfos) do
         FServer.AddUrl(FURLInfos[i]);

      if serverOptions['Compression'].AsBoolean then
         FServer.RegisterCompress(CompressDeflate);

      FServer.OnHttpThreadException := DoOnHTTPThreadException;
      FServer.OnRequest := Process;

      FServer.ServerName:=serverOptions['Name'].AsString;

      nbThreads:=serverOptions['WorkerThreads'].AsInteger;

      FServer.LogRolloverSize:=1024*1024;

      logPath:=serverOptions['LogDirectory'];
      if (logPath.ValueType=jvtString) and (logPath.AsString<>'') then begin
         FServer.LogDirectory:=IncludeTrailingPathDelimiter(FDWS.ApplyPathVariables(logPath.AsString));
         FServer.LogRolloverSize:=1024*1024;
         FServer.Logging:=True;
      end;

      LoadAuthenticateOptions(serverOptions['Authentication']);

      FServer.MaxConnections := serverOptions['MaxConnections'].AsInteger;

      FServer.MaxQueueLength := serverOptions['MaxQueueLength'].AsInteger;

      FServer.MaxBandwidth := serverOptions['MaxBandwidth'].AsInteger;

      FServer.MaxInputCountLength:=serverOptions['MaxInputLength'].AsInteger;

      FAutoRedirectFolders:=serverOptions['AutoRedirectFolders'].AsBoolean;

      FMethodsNotAllowed:=ParseMethodList(serverOptions['MethodsNotAllowed']);

      FFileAccessInfoCacheSize:=256;
   finally
      serverOptions.Free;
   end;

   FServerEvents := TdwsHTTPServerEvents.Create;
   FServer.ServerEvents := FServerEvents;
   FServer.URLRewriter := FURLRewriter;

   if nbThreads>1 then
      FServer.Clone(nbThreads-1);
end;

// GetURLRewriteRules
//
function THttpSys2WebServer.GetURLRewriteRules : String;
begin
   Result := FURLRewriter.AsJSON
end;

// SetURLRewriteRules
//
procedure THttpSys2WebServer.SetURLRewriteRules(const json : String);
begin
   FURLRewriter.AsJSON := json;
end;

// DoOnHTTPThreadException
//
procedure THttpSys2WebServer.DoOnHTTPThreadException(sender : TThread; var e : Exception);
begin
   if Assigned(FDWS) then
      FDWS.LogError(e.ClassName + ': ' + e.Message);
end;

// Shutdown
//
procedure THttpSys2WebServer.Shutdown;
begin
   FDWS.StopDWS;
   FServerEvents := nil;
   FServer.Free;
   FServer := nil;
   FDWS.Shutdown;
   FDWS.Finalize;
end;

// Process
//
procedure THttpSys2WebServer.Process(request : TWebRequest; response : TWebResponse);
var
   noTrailingPathDelimiter : Boolean;
   infoCache : TFileAccessInfoCache;
   fileInfo : TFileAccessInfo;
   t : TFileTime;
begin
   if request.MethodVerb in FMethodsNotAllowed then begin
      ProcessStandardError(request, 405, 'method not allowed',  response);
      Exit;
   end;

   infoCache := TFileAccessInfoCache(request.Custom);
   if infoCache = nil then begin
      infoCache := TFileAccessInfoCache.Create(FileAccessInfoCacheSize);
      request.Custom := infoCache;
   end else if infoCache.Count > cFileCacheCountFlush then begin
      infoCache.Flush;
   end;

   GetSystemTimeAsFileTime(t);
   fileInfo := infoCache.FileAccessInfo(request.PathInfo);
   if (fileInfo = nil) or (Int64(t) > fileInfo.NextCheck) then begin

      if fileInfo = nil then begin
         fileInfo := infoCache.CreateFileAccessInfo(request.PathInfo);
         fileInfo.DefaultMimeType := MIMETypeCache.MIMEType(fileInfo.CookedPathName);
      end else fileInfo.CookedPathName := request.PathInfo;
      fileInfo.NextCheck := Int64(t) + cFileCacheExpiryMilliseconds * 10000;

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

         fileInfo.Typ := FileAccessTypeFromFileName(fileInfo.CookedPathName);
      end;

   end;


   case fileInfo.FileAttribs of
      INVALID_FILE_ATTRIBUTES : begin
         ProcessStandardError(request, 404, 'not found',  response);
      end;
      FILE_ATTRIBUTE_SYSTEM : begin
         ProcessStandardError(request, 401, 'not authorized',  response);
      end;
      FILE_ATTRIBUTE_DIRECTORY :
         Redirect301TrailingPathDelimiter(request, response);
   else
      case fileInfo.Typ of
         fatRAW :
            ProcessStaticFile(fileInfo.CookedPathName, request, response);
         {$ifdef EnablePas2JS}
         fatP2JS :
            FDWS.HandleP2JS(fileInfo.CookedPathName, request, response);
         {$endif}
         fatNone :
            ProcessStandardError(request, 404, 'not found',  response);
      else
         if fileInfo.DefaultMimeType <> '' then
            response.ContentType := fileInfo.DefaultMimeType;
         FDWS.HandleDWS(fileInfo.CookedPathName, fileInfo.Typ, request, response, []);
      end;
   end;
end;

// ProcessStaticFile
//
procedure THttpSys2WebServer.ProcessStaticFile(const pathName : String; request : TWebRequest; response : TWebResponse);
var
   ifModifiedSince : TdwsDateTime;
   lastModified : TdwsDateTime;
begin
   lastModified := FileDateTime(pathName);
   if lastModified.IsZero then begin
      ProcessStandardError(request, 404, 'not found',  response);
      Exit;
   end;

   ifModifiedSince := request.IfModifiedSince;

   // compare with a precision to the second and no more
   if lastModified.MillisecondsAheadOf(ifModifiedSince) >= 1000 then begin

      // http.sys will send the specified file from kernel mode

      response.ContentData := UTF8Encode(pathName);
      response.ContentType := HTTP_RESP_STATICFILE;
      response.LastModified := lastModified;

   end else begin

      response.StatusCode := 304;

   end;
end;

// ProcessStandardError
//
procedure THttpSys2WebServer.ProcessStandardError(
   request : TWebRequest;
   statusCode : Integer; const defaultText : String;
   response : TWebResponse);
var
   errorFile : String;
begin
   response.StatusCode := statusCode;
   errorFile := FErrorPagesPath+IntToStr(statusCode)+'.htm';

   if FileExists(errorFile) then begin

      response.ContentData := UTF8Encode(errorFile);
      response.ContentType := HTTP_RESP_STATICFILE;

   end else begin

      response.ContentText['plain'] := 'Error '+IntToStr(statusCode)+': '+defaultText;

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

// LiveQueries
//
function THttpSys2WebServer.LiveQueries : String;
begin
   Result:=FDWS.LiveQueries;
end;

// CompilationInfoJSON
//
function THttpSys2WebServer.CompilationInfoJSON(const sourceName : String) : String;
var
   fat : TFileAccessType;
begin
   fat := FileAccessTypeFromFileName(sourceName);
   if fat = fatRAW then
      fat := fatPAS;
   Result := FDWS.CompilationInfoJSON(sourceName, fat);
end;

// ExecutionInfoJSON
//
function THttpSys2WebServer.ExecutionInfoJSON(const sourceName : String) : String;
var
   fat : TFileAccessType;
begin
   fat := FileAccessTypeFromFileName(sourceName);
   if fat = fatRAW then
      fat := fatPAS;
   Result := FDWS.ExecutionInfoJSON(sourceName, fat);
end;

// CompiledPrograms
//
function THttpSys2WebServer.CompiledPrograms : TStringDynArray;
begin
   Result := FDWS.CompiledPrograms;
end;

// FlushCompiledPrograms
//
procedure THttpSys2WebServer.FlushCompiledPrograms;
begin
   FDWS.FlushDWSCache;
end;

// ServerEvents
//
function THttpSys2WebServer.ServerEvents : IdwsHTTPServerEvents;
begin
   Result := FServerEvents;
end;

// EnumerateURLs
//
class function THttpSys2WebServer.EnumerateURLInfos(options : TdwsJSONValue) : THttpSys2URLInfos;

   procedure AddInfo(const relativeURI, domainName : String; aPort : Integer; isHTTPS : Boolean);
   var
      n : Integer;
   begin
      n := Length(Result);
      Setlength(Result, n+1);
      if relativeURI = 'undefined' then
         Result[n].RelativeURI := ''
      else Result[n].RelativeURI := relativeURI;
      if (domainName = 'undefined') or (domainName = '') then
         Result[n].DomainName := '*'
      else Result[n].DomainName := domainName;
      Result[n].Port := aPort;
      Result[n].HTTPS := isHTTPS;
   end;

var
   i, port : Integer;
   extraDomains, domain : TdwsJSONValue;
begin
   port := options['Port'].AsInteger;
   if port <> 0 then
      AddInfo(options['RelativeURI'].AsString, options['DomainName'].AsString, port, False);

   port := options['SSLPort'].AsInteger;
   if port <> 0 then
      AddInfo(options['SSLRelativeURI'].AsString, options['SSLDomainName'].AsString, port, True);

   extraDomains := options['Domains'];
   for i:=0 to extraDomains.ElementCount-1 do begin
      domain := extraDomains.Elements[i];
      port := domain['Port'].AsInteger;
      if port = 0 then begin
         if domain['SSL'].AsBoolean then
            port := 443
         else port := 80;
      end;
      AddInfo(domain['RelativeURI'].AsString, domain['Name'].AsString, port, domain['SSL'].AsBoolean);
   end;
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

// URLInfos
//
function THttpSys2WebServer.URLInfos : THttpSys2URLInfos;
begin
   Result := FURLInfos;
end;

// HttpPort
//
function THttpSys2WebServer.HttpPort : Integer;
var
   i : Integer;
begin
   for i := 0 to High(FURLInfos) do
      if not FURLInfos[i].HTTPS then
         Exit(FURLInfos[i].Port);
   Result := 0;
end;

// HttpsPort
//
function THttpSys2WebServer.HttpsPort : Integer;
var
   i : Integer;
begin
   for i := 0 to High(FURLInfos) do
      if FURLInfos[i].HTTPS then
         Exit(FURLInfos[i].Port);
   Result := 0;
end;

end.
