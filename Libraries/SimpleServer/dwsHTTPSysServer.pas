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

  This file is based on Synopse framework.

  HTTP.Sys API & server extracted, moved to Unicode-only

  Synopse framework. Copyright (C) 2012 Arnaud Bouchez
    Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL
}
unit dwsHTTPSysServer;

interface

uses
   Windows,
   ActiveX,
   SysUtils,
   Classes,
   SynWinSock, Registry,
   dwsHTTPSysAPI, dwsUtils, dwsXPlatform,
   dwsWebEnvironment, dwsHttpSysWebEnv, dwsWebServerHelpers,
   dwsHTTPSysServerEvents;

type
   /// FPC 64 compatibility Integer type
   PtrInt = Integer;
   /// FPC 64 compatibility pointer type
   PPtrInt = ^PtrInt;

   PtrUInt = NativeUInt;
   PPtrUInt = ^PtrUInt;

   /// event used to compress or uncompress some data during HTTP protocol
   // - should always return the protocol name for ACCEPT-ENCODING: header
   // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
   // your own (like 'synlzo' or 'synlz')
   // - the data is compressed (if Compress=TRUE) or uncompressed (if
   // Compress=FALSE) in the Data variable (i.e. it is modified in-place)
   // - to be used with THttpSocket.RegisterCompress method
   // - type is a generic AnsiString, which should be in practice a
   // RawByteString or a RawByteString
   THttpSocketCompress = function(var DataRawByteString; Compress : boolean) : AnsiString;

   /// used to maintain a list of known compression algorithms
   THttpSocketCompressRec = record
      /// the compression name, as in ACCEPT-ENCODING: header (gzip,deflate,synlz)
      Name : AnsiString;
      NameLength : Integer;
      /// the function handling compression and decompression
      Func : THttpSocketCompress;
   end;
   PHttpSocketCompressRec = ^THttpSocketCompressRec;

   THttpSocketCompressRecDynArray = array of THttpSocketCompressRec;

   THttpSocketCompressSet = set of 0..31;

   THttpRequestSecurity = (
      hrsNone,
      hrsSSL
   );

   TOnHttpServerRequest = procedure(inRequest : TWebRequest; outResponse : TWebResponse) of object;

   /// event prototype used e.g. by THttpServerGeneric.OnHttpThreadStart
   TNotifyThreadEvent = procedure(Sender : TThread) of object;

   THttpSys2URLInfo = record
      Port : Integer;
      DomainName : String;
      RelativeURI : String;
      HTTPS : Boolean;
      function ToString : String;
   end;
   THttpSys2URLInfos = array of THttpSys2URLInfo;

   /// generic HTTP server
   THttpServerGeneric = class (TThread)
      protected
         /// optional event handler for the virtual Request method
         FOnRequest : TOnHttpServerRequest;
         /// list of all registered compression algorithms
         FCompress : THttpSocketCompressRecDynArray;
         /// set by RegisterCompress method
         FCompressAcceptEncoding : RawByteString;
         FOnHttpThreadStart : TNotifyThreadEvent;
         FOnHttpThreadTerminate : TNotifyEvent;

         procedure DoTerminate; override;
         /// server main loop: just launch FOnHttpThreadStart event (if any)
         // - should be called by all overriden methods
         procedure Execute; override;

      public
         /// override this function to customize your http server
         // - InURL/InMethod/InContent properties are input parameters
         // - OutContent/OutContentType/OutCustomHeader are output parameters
         // - result of the function is the HTTP error code (200 if OK, e.g.)
         // - OutCustomHeader will handle Content-Type/Location
         // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE'),
         // then OutContent is the UTF-8 file name of a file which must be sent to the
         // client via http.sys (much faster than manual buffering/sending)
         // - default implementation is to call the OnRequest event (if existing)
         // - warning: this process must be thread-safe (can be called by several
         // threads simultaneously)
         procedure DoRequest(request : TWebRequest; response : TWebResponse); virtual;
         /// will register a compression algorithm
         // - used e.g. to compress on the fly the data, with standard gzip/deflate
         // or custom (synlzo/synlz) protocols
         // - the first registered algorithm will be the prefered one for compression
         procedure RegisterCompress(aFunction : THttpSocketCompress); virtual;
         /// event handler called by the default implementation of the
         // virtual Request method
         // - warning: this process must be thread-safe (can be called by several
         // threads simultaneously)
         property OnRequest : TOnHttpServerRequest read FOnRequest write FOnRequest;
         /// event handler called when the Thread is just initiated
         // - called in the thread context at first place in THttpServerGeneric.Execute
         property OnHttpThreadStart : TNotifyThreadEvent read FOnHttpThreadStart write FOnHttpThreadStart;
         /// event handler called when the Thread is terminating, in the thread context
         // - the TThread.OnTerminate event will be called within a Synchronize()
         // wrapper, so it won't fit our purpose
         // - to be used e.g. to call CoUnInitialize from thread in which CoInitialize
         // was made, for instance via a method defined as such:
         // ! procedure TMyServer.OnHttpThreadTerminate(Sender: TObject);
         // ! begin // TSQLDBConnectionPropertiesThreadSafe
         // !   fMyConnectionProps.EndCurrentThread;
         // ! end;
         property OnHttpThreadTerminate : TNotifyEvent read FOnHttpThreadTerminate write FOnHttpThreadTerminate;
   end;

   THttpApiFragmentCache = class
      private
         FQueue : THandle;
         FSize : Integer;
         FMaxSize : Integer;
         FMaxChunkSize : Integer;
         FPrefix : String;
         FLock : TFixedCriticalSection;

      public
         constructor Create(queueHandle : THandle; const aPrefix : String; aMaxSize : Integer);
         destructor Destroy; override;

         procedure Send(const fileName : String;
                        request : PHTTP_REQUEST_V2;
                        response : PHTTP_RESPONSE_V2);
         procedure Flush;

         property Size : Integer read FSize write FSize;
         property MaxSize : Integer read FMaxSize write FMaxSize;
         property MaxChunkSize : Integer read FMaxChunkSize write FMaxChunkSize;

         property Prefix : String read FPrefix write FPrefix;
   end;

  {  HTTP server using fast http.sys 2.0 kernel-mode server
     Requires Win2008 or Vista. }
   THttpApi2Server = class (THttpServerGeneric)
      protected
         /// the internal request queue
         FReqQueue : THandle;
         /// contain clones list
         FClones : TSimpleList<THttpApi2Server>;
         /// list of all registered URL (Unicode-encoded)
         FRegisteredUrl : array of String;
         FMaxInputCountLength : Cardinal;

         FWebRequest : THttpSysWebRequest;
         FWebResponse : THttpSysWebResponse;

         FOutStatus : RawByteString;
         FDataChunkForErrorContent : HTTP_DATA_CHUNK_INMEMORY;

         // Http API 2.0
         FServerSessionID : HTTP_SERVER_SESSION_ID;
         FUrlGroupID : HTTP_URL_GROUP_ID;
         FLogFieldsData : HTTP_LOG_FIELDS_DATA;
         FLogDataPtr : PHTTP_LOG_DATA;
         FLogDirectory : String;
         FLogType : HTTP_LOGGING_TYPE;
         FLogFields : Cardinal;
         FLogRolloverSize : Cardinal;
         FServerName : UTF8String;
         FServerNameLength : Integer;
         FServiceName : UTF8String;
         FMaxBandwidth : Cardinal;
         FMaxConnections : Cardinal;
         FAuthentication : Cardinal;
         FMimeInfos : TMIMETypeCache;

         FServerEvents : IdwsHTTPServerEvents;

         /// server main loop - don't change directly
         // - will call the Request public virtual method with the appropriate
         // parameters to retrive the content
         procedure Execute; override;

         /// create a clone
         constructor CreateClone(From : THttpApi2Server);

         procedure SendStaticFile(request : PHTTP_REQUEST_V2; response : PHTTP_RESPONSE_V2);
         procedure AdjustContentTypeFromFileName(const fileName : String; response : PHTTP_RESPONSE_V2);

         procedure SendError(request : PHTTP_REQUEST_V2; response : PHTTP_RESPONSE_V2;
                             statusCode : Cardinal; const errorMsg : String);
         function GetRequestContentBody(request : PHTTP_REQUEST_V2; response : PHTTP_RESPONSE_V2;
                                        var inContent : RawByteString) : Boolean;

         function GetHttpResponseFlags: Cardinal; virtual;

         function GetLogging : Boolean; inline;
         procedure SetLogging(const val : Boolean);
         procedure SetMaxInputCountLength(const val : Cardinal);
         procedure SetLogDirectory(const val : String);
         procedure SetLogType(const val : HTTP_LOGGING_TYPE);
         procedure SetLogFields(const val : Cardinal);
         procedure SetLogRolloverSize(const val : Cardinal);

         procedure UpdateLogInfo;
         procedure UpdateLogFieldsData;

         function GetServerName : String;
         procedure SetServerName(const val : String);
         function GetServiceName : String;
         procedure SetServiceName(const val : String);

         procedure SetMaxBandwidth(val : Cardinal);
         procedure SetMaxConnections(val : Cardinal);

      public
         /// initialize the HTTP Service
         // - will raise an exception if http.sys is not available (e.g. before
         // Windows XP SP2) or if the request queue creation failed
         // - if you override this contructor, put the AddUrl() methods within,
         // and you can set CreateSuspended to TRUE
         // - if you will call AddUrl() methods later, set CreateSuspended to FALSE,
         // then call explicitely the Resume method, after all AddUrl() calls, in
         // order to start the server
         constructor Create(CreateSuspended : Boolean);
         /// release all associated memory and handles
         destructor Destroy; override;

         /// will clone this thread into multiple other threads
         // - could speed up the process on multi-core CPU
         // - will work only if the OnProcess property was set (this is the case
         // e.g. in TSQLHttpServer.Create() constructor)
         // - maximum value is 256 - higher should not be worth it
         procedure Clone(ChildThreadCount : Integer);
         /// register the URLs to Listen On
         // - e.g. AddUrl('root','888')
         // - aDomainName could be either a fully qualified case-insensitive domain
         // name, an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
         // to all domain names for the specified port, '*' will accept the request
         // when no other listening hostnames match the request for that port)
         // - return 0 (NO_ERROR) on success, an error code if failed: under Vista
         // and Seven, you could have ERROR_ACCESS_DENIED if the process is not
         // running with enough rights (by default, UAC requires administrator rights
         // for adding an URL to http.sys registration list) - solution is to call
         // the THttpApi2Server.AddUrlAuthorize class method during program setup
         // - if this method is not used within an overriden constructor, default
         // Create must have be called with CreateSuspended = TRUE and then call the
         // Resume method after all Url have been added
         function AddUrl(const info : THttpSys2URLInfo) : Integer;
         /// un-register the URLs to Listen On
         // - this method expect the same parameters as specified to AddUrl()
         // - return 0 (NO_ERROR) on success, an error code if failed (e.g.
         // -1 if the corresponding parameters do not match any previous AddUrl)
         function RemoveUrl(const info : THttpSys2URLInfo) : Integer;

         /// will authorize a specified URL prefix
         // - will allow to call AddUrl() later for any user on the computer
         // - if aRoot is left '', it will authorize any root for this port
         // - must be called with Administrator rights: this class function is to be
         // used in a Setup program for instance, especially under Vista or Seven,
         // to reserve the Url for the server
         // - add a new record to the http.sys URL reservation store
         // - return '' on success, an error message otherwise
         // - will first delete any matching rule for this URL prefix
         // - if OnlyDelete is true, will delete but won't add the new authorization;
         // in this case, any error message at deletion will be returned
         class function AddUrlAuthorize(const info : THttpSys2URLInfo; OnlyDelete : boolean = false) : string;
         /// will register a compression algorithm
         // - overriden method which will handle any cloned instances
         procedure RegisterCompress(aFunction : THttpSocketCompress); override;

         procedure SetAuthentication(schemeFlags : Cardinal);

         property MaxInputCountLength : Cardinal read FMaxInputCountLength write SetMaxInputCountLength;

         property Logging : Boolean read GetLogging write SetLogging;
         property LogDirectory : String read FLogDirectory write SetLogDirectory;
         property LogType : HTTP_LOGGING_TYPE read FLogType write SetLogType;
         property LogFields : Cardinal read FLogFields write SetLogFields;
         property LogRolloverSize : Cardinal read FLogRolloverSize write SetLogRolloverSize;
         property ServerName : String read GetServerName write SetServerName;
         property ServiceName : String read GetServiceName write SetServiceName;

         property MaxBandwidth : Cardinal read FMaxBandwidth write SetMaxBandwidth;
         property MaxConnections : Cardinal read FMaxConnections write SetMaxConnections;
         property Authentication : Cardinal read FAuthentication;

         property ServerEvents : IdwsHTTPServerEvents read FServerEvents write FServerEvents;
   end;

const
   /// used by THttpApi2Server.Request for http.sys to send a static file
   // - the OutCustomHeader should contain the proper 'Content-type: ....'
   // corresponding to the file (e.g. by calling GetMimeContentType() function
   // from SynCommons supplyings the file name)
   HTTP_RESP_STATICFILE = '!STATICFILE';

function RegURL(aRoot : String; aPort : Integer; isHttps : boolean;
                aDomainName : String) : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDefaultMaxInputCountLength = 10*1024*1024; // 10 MB

var
   vWsaDataOnce : TWSADATA;

function GetNextItemUInt64(var P : PAnsiChar) : Int64;
var
   c : PtrUInt;
begin
   if P = nil then begin
      result := 0;
      exit;
   end;
   result := byte(P^)-48;  // caller ensured that P^ in ['0'..'9']
   inc(P);
   repeat
      c := byte(P^)-48;
      if c>9 then
         break
      else result := result*10+c;
      inc(P);
   until false;
end; // P^ will point to the first non digit char

function GetCardinal(P, PEnd : PAnsiChar) : cardinal; overload;
var
   c : cardinal;
begin
   result := 0;
   if (P = nil) or (P>=PEnd) then
      exit;
   if P^ = ' ' then
      repeat
         inc(P);
         if P = PEnd then
            exit;
      until P^<>' ';
   c := byte(P^)-48;
   if c>9 then
      exit;
   result := c;
   inc(P);
   while P<PEnd do begin
      c := byte(P^)-48;
      if c>9 then
         break
      else result := result*10+c;
      inc(P);
   end;
end;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function SetCompressHeader(const compress : THttpSocketCompressRecDynArray;
   P : PAnsiChar; len : Integer) : THttpSocketCompressSet;
var
   i, n : Integer;
   Beg : PAnsiChar;
begin
   Integer(result) := 0;
   if (P<>nil) and (len>0) then begin
      repeat
         while (P^ in [' ', ',']) and (len>0) do begin
            Inc(P);
            Dec(Len);
         end;
         Beg := P; // 'gzip;q=1.0, deflate' -> aName='gzip' then 'deflate'
         while (not (P^ in [';', ',', #0])) and (len>0) do begin
            inc(P);
            Dec(len);
         end;
         n:=P-Beg;
         for i := 0 to high(Compress) do
            if     (n=Compress[i].NameLength)
               and CompareMem(Beg, Pointer(Compress[i].Name), n) then
               include(result, i);
         while (not (P^ in [',', #0])) and (len>0) do begin
            inc(P);
            Dec(len);
         end;
      until len<=0;
   end;
end;

function RegisterCompressFunc(var Compress : THttpSocketCompressRecDynArray;
   aFunction : THttpSocketCompress; var aAcceptEncoding : RawByteString) : AnsiString;
var
   i, n : Integer;
   dummy, aName : RawByteString;
begin
   result := '';
   if @aFunction = nil then
      exit;
   aName := aFunction(dummy, true);
   n := length(Compress);
   if n = 32 then
      exit; // fCompressHeader is 0..31 (casted as Integer)
   for i := 0 to n-1 do begin
      if (@Compress[i].Func = @aFunction) or (Compress[i].Name = aName) then
         exit;
   end;
   setLength(Compress, n+1);
   with Compress[n] do begin
      Name := aName;
      NameLength := Length(aName);
      @Func := @aFunction;
   end;
   if aAcceptEncoding = '' then
      aAcceptEncoding := 'Accept-Encoding: '+aName
   else aAcceptEncoding := aAcceptEncoding+','+aName;
   result := aName;
end;

const
   // below this size (in bytes), no compression will be done (not worth it)
   COMPRESS_MIN_SIZE = 1024;

function CompressDataAndGetHeaders(Accepted : THttpSocketCompressSet;
   var Handled : THttpSocketCompressRecDynArray; const OutContentType : RawByteString;
   var contentData : RawByteString) : RawByteString;
var
   i : Integer;
begin
   if     (Integer(Accepted)<>0)
      and (OutContentType<>'')
      and (Handled<>nil)
      and (Length(contentData)>=COMPRESS_MIN_SIZE)
      and (   StrBeginsWithA(OutContentType, 'text/')
           or (    StrBeginsWithA(OutContentType, 'application/')
               and (   StrBeginsWithA(OutContentType, 'application/json')
                    or StrBeginsWithA(OutContentType, 'application/xml')
                    or StrBeginsWithA(OutContentType, 'application/javascript')
                    )
               )
           )
      then begin
      for i := 0 to high(Handled) do begin
         if i in Accepted then begin
            // compression of the OutContent + update header
            result := Handled[i].Func(contentData, true);
            exit; // first in FCompress[] is prefered
         end;
      end;
   end;
   result := '';
end;

function RegURL(aRoot : String; aPort : Integer; isHttps : boolean;
   aDomainName : String) : String;
const
   Prefix : array[boolean] of String = ('http://', 'https://');
begin
   if aPort=0 then
      aPort := 80;
   aRoot := Trim(aRoot);
   aDomainName := Trim(aDomainName);
   if aDomainName = '' then begin
      result := '';
      exit;
   end;
   if aRoot<>'' then begin
      if aRoot[1]<>'/' then
         insert('/', aRoot, 1);
      if aRoot[length(aRoot)]<>'/' then
         aRoot := aRoot+'/';
   end else begin
      aRoot := '/'; // allow for instance 'http://*:2869/'
   end;
   result := Prefix[isHttps]+aDomainName+':'+IntToStr(aPort)+aRoot;
end;

// ToString
//
function THttpSys2URLInfo.ToString : String;
begin
   if HTTPS then
      Result := 'https://'
   else Result := 'http://';
   Result := Result + DomainName + ':' + IntToStr(Port) + '/' + RelativeURI;
end;

// Create
//
constructor THttpApi2Server.Create(CreateSuspended : Boolean);
var
   bindInfo : HTTP_BINDING_INFO;
begin
   inherited Create(true);

   FLogType:=HttpLoggingTypeNCSA;

   HttpApi.InitializeAPI; // will raise an exception in case of failure

   HttpAPI.Check(
      HttpAPI.Initialize(HTTPAPI_VERSION_2, HTTP_INITIALIZE_SERVER),
      hInitialize);

   HttpAPI.Check(
      HttpAPI.CreateServerSession(HTTPAPI_VERSION_2, FServerSessionID),
      hCreateServerSession);

   HttpAPI.Check(
      HttpAPI.CreateUrlGroup(FServerSessionID, FUrlGroupID),
      hCreateUrlGroup);

   HttpAPI.Check(
      HttpAPI.CreateRequestQueue(HTTPAPI_VERSION_2, nil, nil, 0, FReqQueue),
      hCreateRequestQueue);

   bindInfo.Flags := 1;
   bindInfo.RequestQueueHandle := FReqQueue;

   HttpAPI.Check(
      HttpAPI.SetUrlGroupProperty(FUrlGroupID, HttpServerBindingProperty,
                                  @bindInfo, SizeOf(bindInfo)),
      hSetUrlGroupProperty);

   FClones := TSimpleList<THttpApi2Server>.Create;
   if not CreateSuspended then
      Suspended := False;
end;

destructor THttpApi2Server.Destroy;
var
   i : Integer;
begin
   if FClones<>nil then begin  // FClones=nil for clone threads

      for i := FClones.Count-1 downto 0 do
         FClones[i].Terminate;

      if FReqQueue<>0 then begin

         for i := 0 to high(FRegisteredUrl) do
            HttpAPI.RemoveUrlFromUrlGroup(FUrlGroupID, Pointer(FRegisteredUrl[i]));

         if FUrlGroupID<>0 then
            HttpAPI.CloseUrlGroup(FUrlGroupID);

         CloseHandle(FReqQueue); // will break all THttpApi2Server.Execute

         if FServerSessionID<>0 then
            HttpAPI.CloseServerSession(FServerSessionID);

         HttpAPI.Terminate(HTTP_INITIALIZE_SERVER);

      end;

      for i := FClones.Count-1 downto 0 do
         FClones[i].Free;
      FClones.Free;

   end;

   FMimeInfos.Free;
   FWebRequest.Free;
   FWebResponse.Free;

   inherited Destroy;
end;

constructor THttpApi2Server.CreateClone(From : THttpApi2Server);
begin
   inherited Create(false);
   FReqQueue := From.FReqQueue;
   FOnRequest := From.OnRequest;
   FCompress := From.FCompress;
   ServerName := From.ServerName;
   OnHttpThreadStart := From.OnHttpThreadStart;
   OnHttpThreadTerminate := From.OnHttpThreadTerminate;
   FCompressAcceptEncoding := From.FCompressAcceptEncoding;
   if From.Logging then begin
      FLogDataPtr:=@FLogFieldsData;
      ServiceName:=From.ServiceName;
   end;
   FServerEvents := From.FServerEvents;
end;

// SendStaticFile
//
procedure THttpApi2Server.SendStaticFile(request : PHTTP_REQUEST_V2; response : PHTTP_RESPONSE_V2);
var
   fileHandle : THandle;
   dataChunkFile : HTTP_DATA_CHUNK_FILEHANDLE;
   rangeStart, rangeLength : Int64;
   flags, bytesSent : Cardinal;
   fileName : String;
   contentRange : RawByteString;
   R : PAnsiChar;
begin
   fileName := UTF8ToUnicodeString(FWebResponse.ContentData);
   fileHandle := FileOpen(fileName, fmOpenRead or fmShareDenyNone);
   if PtrInt(fileHandle)<0 then begin
      SendError(request, response, 404, SysErrorMessage(GetLastError));
      exit;
   end;
   try
      dataChunkFile.DataChunkType := hctFromFileHandle;
      dataChunkFile.FileHandle := fileHandle;
      flags := 0;
      dataChunkFile.ByteRange.StartingOffset.QuadPart := 0;
      Int64(dataChunkFile.ByteRange.Length.QuadPart) := -1; // to eof
      with request^.Headers.KnownHeaders[reqRange] do begin
         if     (RawValueLength>6)
            and StrBeginsWithA(pRawValue, 'bytes=')
            and (pRawValue[6] in ['0'..'9']) then begin
            SetString(contentRange, pRawValue+6, RawValueLength-6); // need #0 end
            R := pointer(contentRange);
            rangeStart := GetNextItemUInt64(R);
            if R^ = '-' then begin
               inc(R);
               flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
               dataChunkFile.ByteRange.StartingOffset := ULARGE_INTEGER(rangeStart);
               if R^ in ['0'..'9'] then begin
                  rangeLength := GetNextItemUInt64(R)-rangeStart+1;
                  if rangeLength>=0 then // "bytes=0-499" -> start=0, len=500
                     dataChunkFile.ByteRange.Length := ULARGE_INTEGER(rangeLength);
               end; // "bytes=1000-" -> start=1000, len=-1 (to eof)
            end;
         end;
      end;
      AdjustContentTypeFromFileName(fileName, response);
      response^.EntityChunkCount := 1;
      response^.pEntityChunks := @dataChunkFile;
      HttpAPI.SendHttpResponse(FReqQueue, request^.RequestId, flags, response^,
                               nil, bytesSent, nil, 0, nil, FLogDataPtr);
   finally
      FileClose(fileHandle);
   end;
end;

// AdjustContentTypeFromFileName
//
procedure THttpApi2Server.AdjustContentTypeFromFileName(const fileName : String; response : PHTTP_RESPONSE_V2);
var
   mimeType : RawByteString;
begin
   mimeType:=FMimeInfos.MIMEType(fileName);
   with response^.Headers.KnownHeaders[reqContentType] do begin
      pRawValue:=PAnsiChar(mimeType);
      RawValueLength:=Length(mimeType);
   end;
end;

// SendError
//
procedure THttpApi2Server.SendError(request : PHTTP_REQUEST_V2; response : PHTTP_RESPONSE_V2;
                                    statusCode : Cardinal; const errorMsg : String);
var
   bytesSent : Cardinal;
begin
   FLogFieldsData.ProtocolStatus := statusCode;
   response^.SetErrorStatus(statusCode, FOutStatus, @FDataChunkForErrorContent, errorMsg);
   HttpAPI.SendHttpResponse(FReqQueue, request^.RequestId, 0, response^, nil, bytesSent, nil, 0, nil, FLogDataPtr);
end;

function THttpApi2Server.AddUrl(const info : THttpSys2URLInfo) : Integer;
var
   s : String;
   n : Integer;
begin
   result := -1;
   if (Self = nil) or (FReqQueue = 0) or (HttpAPI.Module = 0) then
      exit;
   s := RegURL(info.RelativeURI, info.Port, info.HTTPS, info.DomainName);
   if s = '' then
      exit; // invalid parameters

   HttpAPI.Check(
      HttpAPI.AddUrlToUrlGroup(FUrlGroupID, Pointer(s)),
      hAddUrlToUrlGroup);

   n := length(FRegisteredUrl);
   SetLength(FRegisteredUrl, n+1);
   FRegisteredUrl[n] := s;
end;

function THttpApi2Server.RemoveUrl(const info : THttpSys2URLInfo) : Integer;
var
   s : String;
   i, j, n : Integer;
begin
   result := -1;
   if (Self = nil) or (FReqQueue = 0) or (HttpAPI.Module = 0) then
      exit;
   s := RegURL(info.RelativeURI, info.Port, info.HTTPS, info.DomainName);
   if s = '' then
      exit; // invalid parameters
   n := High(FRegisteredUrl);
   for i := 0 to n do begin
      if FRegisteredUrl[i] = s then begin
         HttpAPI.Check(
            HttpAPI.RemoveUrlFromUrlGroup(FUrlGroupID, Pointer(FRegisteredUrl[i])),
            hRemoveUrlFromUrlGroup);
         for j := i to n-1 do
            FRegisteredUrl[j] := FRegisteredUrl[j+1];
         SetLength(FRegisteredUrl, n);
         exit;
      end;
   end;
end;

const
   /// will allow AddUrl() registration to everyone
   // - 'GA' (GENERIC_ALL) to grant all access
   // - 'S-1-1-0'   defines a group that includes all users
   HTTPADDURLSECDESC : PWideChar = 'D:(A;;GA;;;S-1-1-0)';

class function THttpApi2Server.AddUrlAuthorize(const info : THttpSys2URLInfo; OnlyDelete : boolean = false) : string;
var
   prefix : String;
   Error : HRESULT;
   Config : HTTP_SERVICE_CONFIG_URLACL_SET;
begin
   try
      HttpApi.InitializeAPI;
      prefix := RegURL(info.RelativeURI, info.Port, info.HTTPS, info.DomainName);
      if prefix = '' then
         result := 'Invalid parameters'
      else begin
         Error := HttpAPI.Initialize(HTTPAPI_VERSION_2, HTTP_INITIALIZE_CONFIG);
         if Error<>NO_ERROR then
            raise EHttpApiServer.Create(hInitialize, Error);
         try
            fillchar(Config, sizeof(Config), 0);
            Config.KeyDesc.pUrlPrefix := pointer(prefix);
            // first delete any existing information
            Error := HttpAPI.DeleteServiceConfiguration(0, hscUrlAclInfo, @Config, Sizeof(Config));
            // then add authorization rule
            if not OnlyDelete then begin
               Config.KeyDesc.pUrlPrefix := pointer(prefix);
               Config.ParamDesc.pStringSecurityDescriptor := HTTPADDURLSECDESC;
               Error := HttpAPI.SetServiceConfiguration(0, hscUrlAclInfo, @Config, Sizeof(Config));
            end;
            if (Error<>NO_ERROR) and (Error<>ERROR_ALREADY_EXISTS) then
               raise EHttpApiServer.Create(hSetServiceConfiguration, Error);
            result := ''; // success
         finally
            HttpAPI.Terminate(HTTP_INITIALIZE_CONFIG);
         end;
      end;
   except
      on E : Exception do
         result := E.Message;
   end;
end;

procedure THttpApi2Server.Clone(ChildThreadCount : Integer);
var
   i : Integer;
begin
   if (FReqQueue = 0) or not Assigned(OnRequest) or (ChildThreadCount<=0) then
      exit; // nothing to clone (need a queue and a process event)
   if ChildThreadCount>256 then
      ChildThreadCount := 256; // not worth adding
   for i := 1 to ChildThreadCount do
      FClones.Add(THttpApi2Server.CreateClone(self));
end;

// GetServerName
//
function THttpApi2Server.GetServerName : String;
begin
   Result := UTF8ToString(FServerName);
end;

// SetServerName
//
procedure THttpApi2Server.SetServerName(const val : String);
begin
   FServerName := UTF8Encode(val);
   FLogFieldsData.ServerNameLength := Length(FServerName);
   FLogFieldsData.ServerName := Pointer(FServerName);
end;

function THttpApi2Server.GetHttpResponseFlags: Cardinal;
begin
  Result := 0;
end;

// GetLogging
//
function THttpApi2Server.GetLogging : Boolean;
begin
   Result:=(FLogDataPtr<>nil);
end;

// SetLogging
//
procedure THttpApi2Server.SetLogging(const val : Boolean);
var
   i : Integer;
   clone : THttpApi2Server;
begin
   if val=Logging then Exit;

   if val then
      FLogDataPtr:=@FLogFieldsData
   else FLogDataPtr:=nil;

   if FClones<>nil then begin
      for i:=0 to FClones.Count-1 do begin
         clone:=FClones[i];
         if val then
            clone.FLogDataPtr:=@clone.FLogFieldsData
         else clone.FLogDataPtr:=nil;
      end;
   end;

   UpdateLogInfo;
end;

// SetMaxInputCountLength
//
procedure THttpApi2Server.SetMaxInputCountLength(const val : Cardinal);
begin
   if val<=0 then
      FMaxInputCountLength:=cDefaultMaxInputCountLength
   else FMaxInputCountLength:=val;
end;

// SetLogDirectory
//
procedure THttpApi2Server.SetLogDirectory(const val : String);
begin
   FLogDirectory:=val;
   UpdateLogInfo;
end;

// SetLogType
//
procedure THttpApi2Server.SetLogType(const val : HTTP_LOGGING_TYPE);
begin
   FLogType:=val;
   UpdateLogInfo;
end;

// SetLogFields
//
procedure THttpApi2Server.SetLogFields(const val : Cardinal);
begin
   FLogFields:=val;
   UpdateLogInfo;
end;

// SetLogRolloverSize
//
procedure THttpApi2Server.SetLogRolloverSize(const val : Cardinal);
begin
   FLogRolloverSize:=val;
   UpdateLogInfo;
end;

// GetServiceName
//
function THttpApi2Server.GetServiceName : String;
begin
   Result := UTF8ToString(FServiceName);
end;

// SetServiceName
//
procedure THttpApi2Server.SetServiceName(const val : String);
begin
   FServiceName := UTF8Encode(val);
   FLogFieldsData.ServerNameLength := Length(FServiceName);
   FLogFieldsData.ServerName := Pointer(FServiceName);
end;

// SetMaxBandwidth
//
procedure THttpApi2Server.SetMaxBandwidth(val : Cardinal);
var
   qosInfo : HTTP_QOS_SETTING_INFO;
   limitInfo : HTTP_BANDWIDTH_LIMIT_INFO;
begin
   if val<=0 then
      val:=HTTP_LIMIT_INFINITE
   else if val<HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE then
      val:=HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE;
   FMaxBandwidth:=val;

   qosInfo.QosType:=HttpQosSettingTypeBandwidth;
   qosInfo.QosSetting:=@limitInfo;

   limitInfo.Flags:=1;
   limitInfo.MaxBandwidth:=FMaxBandwidth;

   HttpAPI.Check(
      HttpAPI.SetServerSessionProperty(FServerSessionID, HttpServerQosProperty,
                                       @qosInfo, SizeOf(qosInfo)),
      hSetServerSessionProperty);
end;

// SetMaxConnections
//
procedure THttpApi2Server.SetMaxConnections(val : Cardinal);
var
   qosInfo : HTTP_QOS_SETTING_INFO;
   limitInfo : HTTP_CONNECTION_LIMIT_INFO;
begin
   if val<=0 then
      val:=HTTP_LIMIT_INFINITE;
   FMaxConnections:=val;

   qosInfo.QosType:=HttpQosSettingTypeBandwidth;
   qosInfo.QosSetting:=@limitInfo;

   limitInfo.Flags:=1;
   limitInfo.MaxConnections:=FMaxConnections;

   HttpAPI.Check(
      HttpAPI.SetServerSessionProperty(FServerSessionID, HttpServerQosProperty,
                                       @qosInfo, SizeOf(qosInfo)),
      hSetServerSessionProperty);
end;

procedure THttpApi2Server.Execute;

   procedure SetKnownHeader(var h : HTTP_KNOWN_HEADER; const value : RawByteString);
   begin
      h.RawValueLength := Length(value);
      h.pRawValue := Pointer(value);
   end;


var
   request : PHTTP_REQUEST_V2;
   requestID : HTTP_REQUEST_ID;
   requestBuffer, responseBuffer : RawByteString;
   bytesRead, bytesSent : Cardinal;
   errCode : HRESULT;
   inCompressAccept : THttpSocketCompressSet;
   outContentData, outContentEncoding : RawByteString;
   outCustomHeader : RawByteString;
   inContent, inContentType : RawByteString;
   sourceName : String;
   response : PHTTP_RESPONSE_V2;
   headers : HTTP_UNKNOWN_HEADER_ARRAY;
   dataChunkInMemory : HTTP_DATA_CHUNK_INMEMORY;
   pContentEncoding : PHTTP_KNOWN_HEADER;
   pRespServer : PHTTP_KNOWN_HEADER;
begin
   NameThreadForDebugging('THttpApi2Server');

   // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
   inherited Execute;
   CoInitialize(nil);

   FWebRequest:=THttpSysWebRequest.Create;
   FWebResponse:=THttpSysWebResponse.Create;
   FMimeInfos:=TMIMETypeCache.Create;

   // reserve working buffers
   SetLength(headers, 64);
   SetLength(responseBuffer, SizeOf(response^));
   response := Pointer(responseBuffer);
   SetLength(requestBuffer, 16384+SizeOf(HTTP_REQUEST_V2)); // space for request^ + 16 KB of headers
   request := Pointer(requestBuffer);

   // main loop
   requestID := 0;
   repeat
      // retrieve next pending request, and read its headers
      FillChar(request^, SizeOf(HTTP_REQUEST_V2), 0);
      errCode := HttpAPI.ReceiveHttpRequest(FReqQueue, requestID, 0, request^,
                                            Length(requestBuffer), bytesRead);
      if Terminated then
         break;
      case errCode of
         NO_ERROR : begin
            // parse method and headers
            FWebRequest.Request:=request;

            with request^.Headers.KnownHeaders[reqContentType] do
               SetString(inContentType, pRawValue, RawValueLength);
            with request^.Headers.KnownHeaders[reqAcceptEncoding] do
               inCompressAccept := SetCompressHeader(FCompress, pRawValue, RawValueLength);

            if FLogDataPtr<>nil then begin
               UpdateLogFieldsData;
            end;

            // retrieve body
            inContent := '';
            if HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and request^.Flags<>0 then begin
               if not GetRequestContentBody(request, response, inContent) then
                  continue;
            end;

            // prepare WebRequest
            FWebRequest.InContent:=inContent;
            FWebRequest.InContentType:=inContentType;

            // cleanup response
            FillChar(response^, SizeOf(response^), 0);
            FWebResponse.Clear;

            try
               // compute response
               DoRequest(FWebRequest, FWebResponse);

               if Terminated then
                  exit;

               if FWebResponse.HasHeaders then
                  outCustomHeader := FWebResponse.CompiledHeaders
               else outCustomHeader := '';

               FLogFieldsData.ProtocolStatus := FWebResponse.StatusCode;

               response^.SetStatus(FWebResponse.StatusCode, FOutStatus);

               pRespServer:=@response^.Headers.KnownHeaders[respServer];
               pRespServer^.pRawValue:=FLogFieldsData.ServerName;
               pRespServer^.RawValueLength:=FLogFieldsData.ServerNameLength;

               // send response
               response^.Version := request^.Version;
               response^.SetHeaders(Pointer(outCustomHeader), headers);
               if FWebResponse.ContentType = HTTP_RESP_STATICFILE then begin

                  // response is file -> let http.sys serve it (OutContent is UTF-8)
                  SendStaticFile(request, response);

               end else begin

                  if Assigned(FServerEvents) and StrBeginsWithA(FWebResponse.ContentType, 'text/event-stream') then begin

                     sourceName := StrAfterChar(UTF8ToString(FWebResponse.ContentType), ',');
                     FWebResponse.ContentType := 'text/event-stream';
                     SetKnownHeader(response^.Headers.KnownHeaders[reqCacheControl], 'no-cache');
                     SetKnownHeader(response^.Headers.KnownHeaders[reqConnection], 'keep-alive');
                     response^.SetContent(dataChunkInMemory, FWebResponse.ContentData, FWebResponse.ContentType);

                     FServerEvents.AddRequest(sourceName,
                                              FReqQueue, request^.RequestId,
                                              FWebRequest.RemoteIP,
                                              response);

                  end else begin

                     // response is in OutContent -> sent it from memory
                     if (FCompress<>nil) and FWebResponse.Compression then begin
                        pContentEncoding := @response^.Headers.KnownHeaders[reqContentEncoding];
                        if pContentEncoding^.RawValueLength = 0 then begin
                           // no previous encoding -> try if any compression
                           outContentData := FWebResponse.ContentData;
                           outContentEncoding := CompressDataAndGetHeaders(inCompressAccept,
                              FCompress, FWebResponse.ContentType, outContentData);
                           FWebResponse.ContentData := outContentData;
                           pContentEncoding^.pRawValue := Pointer(outContentEncoding);
                           pContentEncoding^.RawValueLength := Length(outContentEncoding);
                        end;
                     end;

                     if FWebResponse.StatusCode <> 304 then begin
                        response^.SetContent(dataChunkInMemory, FWebResponse.ContentData, FWebResponse.ContentType);
                     end;
                     HttpAPI.Check(
                        HttpAPI.SendHttpResponse(FReqQueue, request^.RequestId, GetHttpResponseFlags,
                                                 response^, nil, bytesSent, nil, 0, nil, FLogDataPtr),
                        hSendHttpResponse);

                  end;
               end;
            except
               // handle any exception raised during process: show must go on!
               on E : Exception do begin
                  SendError(request, response, 500, E.Message);
               end;
            end;
            // reset Request ID to handle the next pending request
            requestID := 0;
         end;
         ERROR_MORE_DATA : begin
            // input buffer was too small to hold the request headers
            // -> increase buffer size and call the API again
            requestID := request^.RequestId;
            SetLength(requestBuffer, bytesRead);
            request := Pointer(requestBuffer);
         end;
         ERROR_CONNECTION_INVALID :
            if requestID = 0 then
               break
            else // TCP connection was corrupted by the peer -> ignore + next request
               requestID := 0;
      else
         break;
      end;
   until Terminated;
   CoUninitialize;
end;

procedure THttpApi2Server.RegisterCompress(aFunction : THttpSocketCompress);
var
   i : Integer;
begin
   inherited;
   if FClones<>nil then
      for i := 0 to FClones.Count-1 do
         FClones[i].RegisterCompress(aFunction);
end;

// GetRequestContentBody
//
function THttpApi2Server.GetRequestContentBody(request : PHTTP_REQUEST_V2; response : PHTTP_RESPONSE_V2;
                                               var inContent : RawByteString) : Boolean;
var
   inContentLength, inContentLengthRead, bytesRead : Cardinal;
   bufRead : PAnsiChar;
   flags, errCode : Integer;
   i, n : Integer;
   compressRec : PHttpSocketCompressRec;
   contentEncoding : PHTTP_KNOWN_HEADER;
begin
   with request^.Headers.KnownHeaders[reqContentLength] do
      inContentLength := GetCardinal(pRawValue, pRawValue+RawValueLength);
   if inContentLength<>0 then begin
      if (MaxInputCountLength>0) and (inContentLength>MaxInputCountLength) then begin
         // ideally SendError(412, 'Content-Length too large');
         // but if we don't cancel the request, it'll eat all the
         // incoming bandwidth anyway...
         HttpAPI.CancelHttpRequest(FReqQueue, request^.RequestId, nil);
         Exit(False);
      end;
      SetLength(inContent, inContentLength);
      bufRead:=Pointer(inContent);
      inContentLengthRead := 0;
      repeat
         bytesRead := 0;
         if Win32MajorVersion>5 then // speed optimization for Vista+
            flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER
         else flags := 0;
         errCode := HttpAPI.ReceiveRequestEntityBody(FReqQueue, request^.RequestId, flags,
            bufRead, inContentLength-inContentLengthRead, bytesRead);
         inc(inContentLengthRead, BytesRead);
         if errCode = ERROR_HANDLE_EOF then begin
            if inContentLengthRead<inContentLength then
               SetLength(inContent, inContentLengthRead);
            errCode := NO_ERROR;
            break; // should loop until returns ERROR_HANDLE_EOF
         end;
         if errCode<>NO_ERROR then
            break;
         inc(bufRead, BytesRead);
      until inContentLengthRead = inContentLength;
      if errCode<>NO_ERROR then begin
         SendError(request, response, 406, SysErrorMessage(errCode));
         Exit(False);
      end;
      // attempt to decompress if necessary
      contentEncoding:=@request^.Headers.KnownHeaders[reqContentEncoding];
      n:=contentEncoding.RawValueLength;
      if n>0 then begin
         for i:=0 to High(FCompress) do begin
            compressRec:=@FCompress[i];
            if     (compressRec.NameLength=n)
               and CompareMem(Pointer(compressRec.Name), contentEncoding.pRawValue, n) then begin
               compressRec.Func(inContent, false); // uncompress
               break;
            end;
         end;
      end;
   end;
   Result:=True;
end;

// SetAuthentication
//
procedure THttpApi2Server.SetAuthentication(schemeFlags : Cardinal);
var
   authInfo : HTTP_SERVER_AUTHENTICATION_INFO;
begin
   FillChar(authInfo, SizeOf(authInfo), 0);
   authInfo.Flags:=1;
   authInfo.AuthSchemes:=schemeFlags;
   authInfo.ReceiveMutualAuth:=True;

   HttpAPI.Check(
      HttpAPI.SetUrlGroupProperty(FUrlGroupID, HttpServerAuthenticationProperty,
                                  @authInfo, SizeOf(authInfo)),
      hSetServerSessionProperty);
end;

// UpdateLogInfo
//
procedure THttpApi2Server.UpdateLogInfo;
var
   logInfo : HTTP_LOGGING_INFO;
begin
   if not Logging then Exit;
   if FClones=nil then Exit;

   FillChar(logInfo, SizeOf(logInfo), 0);
   logInfo.Flags := 1;
   logInfo.LoggingFlags := HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION;
   // Http API limit,
   // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa364532(v=vs.85).aspx
   Assert(Length(FLogDirectory)<212);
   logInfo.DirectoryName := Pointer(LogDirectory);
   logInfo.DirectoryNameLength := Length(LogDirectory)*SizeOf(Char);
   logInfo.Format :=  LogType;
   if LogType = HttpLoggingTypeNCSA then
      logInfo.Fields := HTTP_ALL_NON_ERROR_LOG_FIELDS
   else logInfo.Fields := LogFields;
   if LogRolloverSize>0 then begin
      logInfo.RolloverType := HttpLoggingRolloverSize;
      logInfo.RolloverSize := LogRolloverSize;
   end else begin
      logInfo.RolloverType := HttpLoggingRolloverMonthly;
   end;

   HttpAPI.Check(
      HttpAPI.SetUrlGroupProperty(FUrlGroupID, HttpServerLoggingProperty,
                                  @logInfo, SizeOf(logInfo)),
      hSetServerSessionProperty);
end;

// UpdateLogFieldsData
//
procedure THttpApi2Server.UpdateLogFieldsData;
const
   cVERB_TEXT : array [hvOPTIONS..hvSEARCH] of String[9] = (
      'OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE',
      'CONNECT', 'TRACK', 'MOVE', 'COPY', 'PROPFIND', 'PROPPATCH',
      'MKCOL', 'LOCK', 'UNLOCK', 'SEARCH' );
var
   verb : THttpVerb;
   request : PHTTP_REQUEST_V2;
begin
   request := FWebRequest.Request;
   verb := request^.Verb;
   FLogFieldsData.MethodNum := verb;
   case verb of
      Low(cVERB_TEXT)..High(cVERB_TEXT) : begin
         FLogFieldsData.MethodLength := Length(cVERB_TEXT[verb]);
         FLogFieldsData.Method := @cVERB_TEXT[verb][1];
      end;
   else
      FLogFieldsData.MethodLength := request^.UnknownVerbLength;
      FLogFieldsData.Method := request^.pUnknownVerb;
   end;

   FLogFieldsData.UriStemLength := request^.CookedUrl.AbsPathLength;
   FLogFieldsData.UriStem := request^.CookedUrl.pAbsPath;

   FLogFieldsData.ClientIpLength := FWebRequest.RemoteIP_UTF8_Length;
   FLogFieldsData.ClientIp := FWebRequest.RemoteIP_UTF8;
end;

{ THttpServerGeneric }

procedure THttpServerGeneric.RegisterCompress(aFunction : THttpSocketCompress);
begin
   RegisterCompressFunc(FCompress, aFunction, FCompressAcceptEncoding);
end;

procedure THttpServerGeneric.DoRequest(request : TWebRequest; response : TWebResponse);
begin
   if Assigned(OnRequest) then
      OnRequest(request, response)
   else response.StatusCode := 404; // 404 NOT FOUND
end;

procedure THttpServerGeneric.Execute;
begin
   if Assigned(FOnHttpThreadStart) then
      FOnHttpThreadStart(self);
end;

{$ifndef LVCL}
procedure THttpServerGeneric.DoTerminate;
begin
   if Assigned(FOnHttpThreadTerminate) then
      FOnHttpThreadTerminate(self);
   inherited DoTerminate;
end;

{$endif}

// ------------------
// ------------------ THttpApiFragmentCache ------------------
// ------------------

// Create
//
constructor THttpApiFragmentCache.Create(queueHandle : THandle; const aPrefix : String; aMaxSize : Integer);
begin
   inherited Create;
   FPrefix:=aPrefix;
   FQueue:=queueHandle;
   FMaxSize:=aMaxSize;
   FMaxChunkSize:=aMaxSize div 10;
   FLock:=TFixedCriticalSection.Create;
end;

// Destroy
//
destructor THttpApiFragmentCache.Destroy;
begin
   Flush;
   inherited;
end;

// Send
//
procedure THttpApiFragmentCache.Send(const fileName : String;
                        request : PHTTP_REQUEST_V2;
                        response : PHTTP_RESPONSE_V2);
begin
   // TODO
end;

// Flush
//
procedure THttpApiFragmentCache.Flush;
begin
   HttpAPI.FlushResponseCache(FQueue, PChar(FPrefix), 0, nil);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   Assert((sizeof(HTTP_REQUEST_V2) = 464+8) and (sizeof(HTTP_SSL_INFO) = 28) and
      (sizeof(HTTP_DATA_CHUNK_INMEMORY) = 24) and
      (sizeof(HTTP_DATA_CHUNK_FILEHANDLE) = 32) and
      (sizeof(HTTP_REQUEST_HEADERS) = 344) and
      (sizeof(HTTP_RESPONSE_HEADERS) = 256) and (sizeof(HTTP_COOKED_URL) = 24) and
      (sizeof(HTTP_RESPONSE_V2) = 288) and (ord(reqUserAgent) = 40) and
      (ord(respLocation) = 23) and (sizeof(THttpHeader) = 4));

   if InitSocketInterface then
      WSAStartup(WinsockLevel, vWsaDataOnce)
   else
      fillchar(vWsaDataOnce, sizeof(vWsaDataOnce), 0);

finalization

   if vWsaDataOnce.wVersion<>0 then
      WSACleanup;
   DestroySocketInterface;

end.
