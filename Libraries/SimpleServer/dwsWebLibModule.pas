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
unit dwsWebLibModule;

interface

uses
   Windows, WinInet, Variants,
   SysUtils, Classes, StrUtils,
   SynZip, SynCrtSock, SynCommons, SynWinSock,
   dwsUtils, dwsComp, dwsExprs, dwsWebEnvironment, dwsExprList, dwsSymbols,
   dwsJSONConnector, dwsCryptoXPlatform, dwsHTTPSysServerEvents, dwsWebServerInfo,
   dwsXPlatform, dwsCustomData;

type
  TdwsWebLib = class(TDataModule)
    dwsWeb: TdwsUnit;
    procedure dwsWebClassesWebRequestMethodsURLEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsMethodEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsPathInfoEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsQueryStringEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsRemoteIPEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsContentDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsContentTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsContentEncodingEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHeaderEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHeadersEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsCookieEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsCookiesEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsQueryFieldEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsQueryFieldsEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetContentTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsSecureEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsUserAgentEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsAuthenticatedUserEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetStatusCodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetHeaderEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsAuthenticationEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsRequestAuthenticationEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHasQueryFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetCookieEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetCookie2Eval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetCompressionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentLengthEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsWebFunctionsDeflateCompressFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsWebFunctionsDeflateDecompressionFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsWebClassesWebRequestMethodsGetContentFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHasContentFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsGetDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsGetTextEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsPostDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsIfModifiedSinceEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetLastModifiedEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetCredentialsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsClearCredentialsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsPutDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsDeleteEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetContentJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetIgnoreSSLCertificateErrorsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsGetIgnoreSSLCertificateErrorsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebFunctionsGetHostByAddrEval(info: TProgramInfo);
    procedure dwsWebFunctionsGetHostByNameEval(info: TProgramInfo);
    procedure dwsWebClassesWebResponseMethodsSetStaticEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsFullURLEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHostEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetConnectTimeoutMSecEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetSendTimeoutMSecEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetReceiveTimeoutMSecEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetProxyNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetCustomHeadersEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpRequestCleanUp(ExternalObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsRequestEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsMethodEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsURLEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsStatusCodeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsHeadersEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsGetHeaderEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsContentLengthEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsContentTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsContentDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsCompletedEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsErrorEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpRequestMethodsCurrentContentSizeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetContentEventStreamEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebServerSentEventsMethodsPostRawEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebServerSentEventsMethodsCloseEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebServerSentEventsMethodsConnectionsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebServerSentEventsMethodsSourceNamesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebServerSentEventMethodsPostEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebServerSentEventMethodsToRawDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsRawURLEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    { Private declarations }
    FServer :  IWebServerInfo;
  public
    { Public declaration }
    property Server : IWebServerInfo read FServer write FServer;
  end;

implementation

{$R *.dfm}

uses dwsWinHTTP;

// WebServerSentEventToRawData
//
function WebServerSentEventToRawData(const obj : TScriptObjInstance) : RawByteString;
var
   i : Integer;
   dyn : TScriptDynamicStringArray;
   buf : String;
begin
   buf := obj.AsString[obj.FieldAddress('ID')];
   if buf <> '' then
      Result := 'id: ' + StringToUTF8(buf) + #10;
   buf := obj.AsString[obj.FieldAddress('Name')];
   if buf <> '' then
      Result := Result + 'event: ' + StringToUTF8(buf) + #10;
   i := obj.AsInteger[obj.FieldAddress('Retry')];
   if i > 0 then
      Result := Result + 'retry: ' + ScriptStringToRawByteString(IntToStr(i)) + #10;
   dyn := (obj.AsInterface[obj.FieldAddress('Data')] as IScriptDynArray).GetSelf as TScriptDynamicStringArray;
   for i := 0 to dyn.ArrayLength-1 do
      Result := Result + 'data: ' + StringToUTF8(dyn.AsString[i]) + #10;
   Result := Result + #10;
end;

// DeflateCompress
//
procedure DeflateCompress(var data : RawByteString; compressionLevel : Integer);
var
   strm : TZStream;
   tmp : RawByteString;
begin
   StreamInit(strm);
   strm.next_in := Pointer(data);
   strm.avail_in := Length(data);
   SetString(tmp, nil, strm.avail_in+256+strm.avail_in shr 3); // max mem required
   strm.next_out := Pointer(tmp);
   strm.avail_out := Length(tmp);
   if deflateInit2_(strm, compressionLevel, Z_DEFLATED, -MAX_WBITS, DEF_MEM_LEVEL,
                    Z_DEFAULT_STRATEGY, ZLIB_VERSION, SizeOf(strm))<0 then
      raise Exception.Create('deflateInit2_ failed');
   try
      Check(deflate(strm,Z_FINISH),[Z_STREAM_END]);
   finally
      deflateEnd(strm);
   end;
   SetString(data, PAnsiChar(Pointer(tmp)), strm.total_out);
end;

// DeflateDecompress
//
procedure DeflateDecompress(var data : RawByteString);
var
   strm : TZStream;
   code, len : integer;
   tmp : RawByteString;
begin
   StreamInit(strm);
   strm.next_in := Pointer(data);
   strm.avail_in := Length(data);
   len := (strm.avail_in*20) shr 3; // initial chunk size = comp. ratio of 60%
   SetString(tmp,nil,len);
   strm.next_out := Pointer(tmp);
   strm.avail_out := len;
   if inflateInit2_(strm, -MAX_WBITS, ZLIB_VERSION, SizeOf(strm))<0 then
      raise Exception.Create('inflateInit2_ failed');
   try
      repeat
         code := Check(inflate(strm, Z_FINISH),[Z_OK,Z_STREAM_END,Z_BUF_ERROR]);
         if strm.avail_out=0 then begin
            // need to increase buffer by chunk
            SetLength(tmp,length(tmp)+len);
            strm.next_out := PAnsiChar(pointer(tmp))+length(tmp)-len;
            strm.avail_out := len;
         end;
      until code=Z_STREAM_END;
   finally
      inflateEnd(strm);
   end;
   SetString(data, PAnsiChar(Pointer(tmp)), strm.total_out);
end;

const
   cWinHttpConnection : TGUID = '{0B47FA19-7BE9-41AE-A3BD-2C686117D669}';

   cWinHttpCredentials : TGUID = '{FB60EB3D-1085-4A88-9923-DE895B5CAB76}';
   cWinHttpIgnoreSSLCertificateErrors : TGUID = '{42AC8563-761B-4E3D-9767-A21F8F32201C}';
   cWinHttpProxyName : TGUID = '{2449F585-D6C6-4FDC-8D86-0266E01CA99C}';
   cWinHttpConnectTimeout : TGUID = '{8D322334-D1DD-4EBF-945F-193CFCA001FB}';
   cWinHttpSendTimeout : TGUID = '{1DE21769-65B5-4039-BB66-62D405FB00B7}';
   cWinHttpReceiveTimeout : TGUID = '{0D14B470-4F8A-48AE-BAD2-426E15FE4E03}';
   cWinHttpCustomHeaders : TGUID = '{FD05B54E-FBF2-498A-BD1F-0B1F18F27A1E}';

// HttpQuery
//
function HttpQuery(exec : TdwsProgramExecution;
                   const method, url : RawByteString;
                   const requestData, requestContentType : RawByteString;
                   var replyHeaders : SockString; var replyData : String;
                   asText : Boolean; onProgress : TWinHttpProgress = nil;
                   customStates : TdwsCustomStates = nil) : Integer;
var
   uri : TURI;
   conn : TdwsWinHttpConnection;
   iconn : IGetSelf;
   unassignedVariant : Variant;
begin
   if not uri.From(url) then
      raise Exception.CreateFmt('Invalid url "%s"', [url]);

   if exec <> nil then
      iconn := exec.CustomInterfaces[cWinHttpConnection] as IGetSelf;
   if iconn = nil then begin
      iconn := IGetSelf(TdwsWinHttpConnection.Create);
      if exec <> nil then
         exec.CustomInterfaces[cWinHttpConnection] := iconn;
   end;

   try
      if (customStates = nil) and (exec <> nil) then begin
         if exec.HasCustomStates then
            customStates := exec.CustomStates;
      end;

      conn := iconn.GetSelf as TdwsWinHttpConnection;
      if customStates <> nil then begin
         conn.ConnectServer(uri, customStates.StringStateDef(cWinHttpProxyName, ''),
                            customStates.IntegerStateDef(cWinHttpConnectTimeout, HTTP_DEFAULT_CONNECTTIMEOUT),
                            customStates.IntegerStateDef(cWinHttpSendTimeout, HTTP_DEFAULT_SENDTIMEOUT),
                            customStates.IntegerStateDef(cWinHttpReceiveTimeout, HTTP_DEFAULT_RECEIVETIMEOUT));
         conn.SetIgnoreSSLErrors(customStates[cWinHttpIgnoreSSLCertificateErrors]);
         conn.SetCredentials(customStates[cWinHttpCredentials]);
         conn.SetCustomHeaders(customStates[cWinHttpCustomHeaders]);
      end else begin
         conn.ConnectServer(uri, '', HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT, HTTP_DEFAULT_RECEIVETIMEOUT);
         conn.SetIgnoreSSLErrors(unassignedVariant);
         conn.SetCredentials(unassignedVariant);
         conn.SetCustomHeaders(unassignedVariant);
      end;
      conn.SetOnProgress(onProgress);

      Result := conn.Request(uri, method, 0, '', requestData, requestContentType, replyHeaders, replyData, asText);
   except
      on EWinHTTP do begin
         if exec <> nil then
            exec.CustomInterfaces[cWinHttpConnection] := nil;
         raise;
      end;
   end;
end;

type
   THttpRequestThread = class (TThread)
      Method, URL : RawByteString;
      RequestData, RequestContentType : RawByteString;
      ResponseData : String;
      RawResponseHeaders : SockString;
      ResponseHeaders : TStrings;
      CustomStates : TdwsCustomStates;
      CurrentSize, ContentLength : DWORD;
      StatusCode : Integer;
      Error : String;
      Completed, Released : Boolean;
      ReleaseLock : TMultiReadSingleWrite;
      constructor CreateQuery(const method, url : RawByteString;
                              const requestData, requestContentType : RawByteString;
                              const customStates : TdwsCustomStates);
      destructor Destroy; override;
      procedure PrepareHeaders;
      procedure Execute; override;
      procedure Release;
      function Wait : THttpRequestThread;
      procedure DoProgress(Sender: TWinHttpAPI; CurrentSize, ContentLength: DWORD);
   end;

// CreateQuery
//
constructor THttpRequestThread.CreateQuery(const method, url : RawByteString;
                              const requestData, requestContentType : RawByteString;
                              const customStates : TdwsCustomStates);
begin
   inherited Create;
   Self.Method := method;
   Self.URL := url;
   Self.RequestData := requestData;
   Self.RequestContentType := requestContentType;
   Self.CustomStates := customStates;
   FreeOnTerminate := False;
   ReleaseLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor THttpRequestThread.Destroy;
begin
   inherited;
   ReleaseLock.Free;
   ResponseHeaders.Free;
   CustomStates.Free;
end;

// PrepareHeaders
//
procedure THttpRequestThread.PrepareHeaders;

   procedure AddHeader(const s : String);
   var
      k : Integer;
   begin
      k := Pos(':', s);
      if k > 0 then
         ResponseHeaders.Add(SysUtils.TrimRight(Copy(s, 1, k-1) + '=' + SysUtils.Trim(Copy(s, k+1))));
   end;

var
   h : String;
   p, pn : Integer;
begin
   RawByteStringToScriptString(RawResponseHeaders, h);
   ResponseHeaders := TFastCompareTextList.Create;

   p := 1;
   while True do begin
      pn := StrUtils.PosEx(#13#10, h, p);
      if pn > 0 then begin
         AddHeader(Copy(h, p, pn-p));
         p := pn + 2;
      end else break;
   end;
   AddHeader(Copy(h, p));
end;

// Execute
//
procedure THttpRequestThread.Execute;
begin
   if not Released then try
      StatusCode := HttpQuery(nil, Method, URL, RequestData, RequestContentType,
                              RawResponseHeaders, ResponseData, False,
                              DoProgress, CustomStates);
      FreeAndNil(CustomStates);
      RequestData := '';
      RequestContentType := '';
      ContentLength := Length(ResponseData);
      CurrentSize := ContentLength;
      if not Released then
         PrepareHeaders;
   except
      on E: Exception do
         Error := E.Message;
   end;
   ReleaseLock.BeginWrite;
   Completed := True;
   if Released then
      FreeOnTerminate := True;
   ReleaseLock.EndWrite;
end;

// Release
//
procedure THttpRequestThread.Release;
begin
   ReleaseLock.BeginWrite;
   if Completed then begin
      if not Finished then
         WaitFor;
      Free;
   end else Released := True;
   ReleaseLock.EndWrite;
end;

// Wait
//
function THttpRequestThread.Wait : THttpRequestThread;
begin
   if not Finished then WaitFor;
   Result := Self;
end;

// DoProgress
//
procedure THttpRequestThread.DoProgress(Sender: TWinHttpAPI; CurrentSize, ContentLength: DWORD);
begin
   Self.CurrentSize := CurrentSize;
   Self.ContentLength := ContentLength;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsDeleteEval(Info: TProgramInfo;
  ExtObject: TObject);
var
   replyHeaders : SockString;
   buf : String;
begin
   Info.ResultAsInteger:=HttpQuery(Info.Execution, 'DELETE', Info.ParamAsDataString[0],
                                   '', '', replyHeaders, buf, False);
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsGetDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   replyHeaders : SockString;
   buf : String;
begin
   Info.ResultAsInteger:=HttpQuery(Info.Execution, 'GET', Info.ParamAsDataString[0],
                                   '', '', replyHeaders, buf, False);
   Info.ParamAsString[1]:=buf;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsGetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   replyHeaders : SockString;
   buf : String;
begin
   Info.ResultAsInteger:=HttpQuery(Info.Execution, 'GET', Info.ParamAsDataString[0],
                                   '', '', replyHeaders, buf, True);
   Info.ParamAsString[1]:=buf;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsPostDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
   headers : SockString;
begin
   Info.ResultAsInteger:=HttpQuery(
      Info.Execution, 'POST', Info.ParamAsDataString[0],
      Info.ParamAsDataString[1], Info.ParamAsDataString[2], headers, buf, False);
   Info.ParamAsString[3]:=buf;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsPutDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
   headers : SockString;
begin
   Info.ResultAsInteger:=HttpQuery(
      Info.Execution, 'PUT', Info.ParamAsDataString[0],
      Info.ParamAsDataString[1], Info.ParamAsDataString[2], headers, buf, False);
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsRequestEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   rq : THttpRequestThread;
   obj : IScriptObj;
begin
   obj := TScriptObjInstance.Create(Info.Table.FindTypeSymbol('HttpRequest', cvPublic) as TClassSymbol,
                                    Info.Execution);
   rq := THttpRequestThread.CreateQuery(
      Info.ParamAsDataString[0], Info.ParamAsDataString[1],
      Info.ParamAsDataString[2], Info.ParamAsDataString[3],
      Info.Execution.CustomStates.Clone
      );
   obj.ExternalObject := rq;
   Info.ResultAsVariant := obj;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetConnectTimeoutMSecEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.CustomStates[cWinHttpConnectTimeout]:=Info.ParamAsInteger[0];
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetSendTimeoutMSecEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.CustomStates[cWinHttpSendTimeout]:=Info.ParamAsInteger[0];
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetReceiveTimeoutMSecEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.CustomStates[cWinHttpReceiveTimeout]:=Info.ParamAsInteger[0];
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetCredentialsEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   v : Variant;
begin
   v:=VarArrayCreate([0, 2], varVariant);
   v[0]:=Info.ParamAsInteger[0];
   v[1]:=Info.ParamAsString[1];
   v[2]:=Info.ParamAsString[2];
   Info.Execution.CustomStates[cWinHttpCredentials]:=v;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetCustomHeadersEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   dyn : IScriptDynArray;
   headers : TdwsCustomHeaders;
   i, n : Integer;
begin
   dyn := Info.ParamAsScriptDynArray[0];
   n := dyn.ArrayLength;
   if n > 0 then begin
      headers := TdwsCustomHeaders.Create;
      SetLength(headers.Headers, n);
      for i := 0 to n-1 do
         ScriptStringToRawByteString(dyn.AsString[i], headers.Headers[i]);
      Info.Execution.CustomStates[cWinHttpCustomHeaders] := headers as IGetSelf;
   end else Info.Execution.CustomStates[cWinHttpCustomHeaders] := Unassigned;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsClearCredentialsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.CustomStates[cWinHttpCredentials]:=Unassigned;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetIgnoreSSLCertificateErrorsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.CustomStates[cWinHttpIgnoreSSLCertificateErrors]:=Info.ParamAsBoolean[0];
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsGetIgnoreSSLCertificateErrorsEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   v : Variant;
begin
   v:=Info.Execution.CustomStates[cWinHttpIgnoreSSLCertificateErrors];
   if VarType(v)<>varBoolean then
      v:=False;
   Info.ResultAsBoolean:=v;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsSetProxyNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.Execution.CustomStates[cWinHttpProxyName]:=Info.ParamAsString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsAuthenticatedUserEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.AuthenticatedUser;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsAuthenticationEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Ord(Info.WebRequest.Authentication);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=Info.WebRequest.ContentData;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentLengthEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Length(Info.WebRequest.ContentData);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=Info.WebRequest.ContentType;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookieEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Cookies.Values[Info.ParamAsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookiesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Cookies.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsFullURLEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.FullURL;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsGetContentFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.ContentFields.Values[Info.ParamAsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHasContentFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=Info.WebRequest.HasContentField(Info.ParamAsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHasQueryFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=Info.WebRequest.HasQueryField(Info.ParamAsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Header(Info.ParamAsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHeadersEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Headers.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHostEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Host;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsIfModifiedSinceEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=Info.WebRequest.IfModifiedSince;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsMethodEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Method;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsPathInfoEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.PathInfo;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.QueryFields.Values[Info.ParamAsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryFieldsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.QueryFields.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryStringEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.QueryString;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsRawURLEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := Info.WebRequest.RawURL;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsRemoteIPEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   cloudflareIP : String;
begin
   cloudflareIP:=Info.WebRequest.Header('CF-Connecting-IP');
   if cloudflareIP<>'' then
      Info.ResultAsString:=cloudflareIP
   else Info.ResultAsString:=Info.WebRequest.RemoteIP;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsSecureEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Security;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsURLEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.URL;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsUserAgentEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.UserAgent;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsContentDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentData:=Info.ParamAsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsContentEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentEncoding:=Info.ParamAsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentType:=Info.ParamAsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsRequestAuthenticationEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   wra : TWebRequestAuthentication;
begin
   wra:=TWebRequestAuthentication(Info.ParamAsInteger[0]);
   Info.WebResponse.Headers.Values['WWW-Authenticate']:=cWebRequestAuthenticationToString[wra];
   Info.WebResponse.StatusCode:=401;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCompressionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.Compression:=Info.ParamAsBoolean[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentEventStreamEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   sourceName : String;
begin
   sourceName := Info.ParamAsString[0];
   if sourceName = '' then
      sourceName := CryptographicToken;
   Info.WebResponse.ContentType := 'text/event-stream,' + ScriptStringToRawByteString(sourceName);
   Info.ResultAsString := sourceName;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   intf : IUnknown;
   json : String;
begin
   intf := IUnknown(Info.ParamAsVariant[0]);
   if intf <> nil then
      json := (intf as IBoxedJSONValue).Value.ToUnicodeString
   else json := '';
   Info.WebResponse.ContentJSON := json;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentText[Info.ParamAsDataString[0]]:=Info.ParamAsString[1];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCookieEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   cookie : TWebResponseCookie;
begin
   cookie:=Info.WebResponse.Cookies.AddCookie(Info.ParamAsString[0]);
   cookie.Value:=Info.ParamAsString[1];
   cookie.ExpiresGMT:=Info.ParamAsFloat[2];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCookie2Eval(
  Info: TProgramInfo; ExtObject: TObject);
var
   cookie : TWebResponseCookie;
begin
   cookie := Info.WebResponse.Cookies.AddCookie(Info.ParamAsString[0]);
   cookie.Value := Info.ParamAsString[1];
   cookie.ExpiresGMT := Info.ParamAsFloat[2];
   cookie.Path := Info.ParamAsString[3];
   cookie.Domain := Info.ParamAsString[4];
   cookie.Flags := Info.ParamAsInteger[5];
   cookie.SameSite := TWebResponseCookieSameSite(Info.ParamAsInteger[6]);
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.Headers.Values[Info.ParamAsString[0]]:=Info.ParamAsString[1];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetLastModifiedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.LastModified:=Info.ParamAsFloat[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetStaticEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   h : TWebResponseHints;
begin
   h:=Info.WebResponse.Hints;
   if Info.ParamAsBoolean[0] then
      h:=h+[shStatic]
   else h:=h-[shStatic];
   Info.WebResponse.Hints:=h;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetStatusCodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.StatusCode:=Info.ParamAsInteger[0];
end;

procedure TdwsWebLib.dwsWebClassesWebServerSentEventMethodsPostEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   obj : TScriptObjInstance;
begin
   obj := Info.ScriptObj.GetSelf as TScriptObjInstance;
   FServer.ServerEvents.PostEvent(Info.ParamAsString[0], WebServerSentEventToRawData(obj));
end;

procedure TdwsWebLib.dwsWebClassesWebServerSentEventMethodsToRawDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   obj : TScriptObjInstance;
begin
   obj := Info.ScriptObj.GetSelf as TScriptObjInstance;
   Info.ResultAsDataString := WebServerSentEventToRawData(obj);
end;

procedure TdwsWebLib.dwsWebClassesWebServerSentEventsMethodsCloseEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.ServerEvents.CloseRequests(Info.ParamAsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebServerSentEventsMethodsConnectionsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.ServerEvents.SourceRequests(Info.ParamAsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebServerSentEventsMethodsPostRawEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.ServerEvents.PostEvent(Info.ParamAsString[0], Info.ParamAsDataString[1]);
end;

procedure TdwsWebLib.dwsWebClassesWebServerSentEventsMethodsSourceNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.ServerEvents.SourceNames;
end;

function TdwsWebLib.dwsWebFunctionsDeflateCompressFastEval(
  const args: TExprBaseListExec): Variant;
var
   data : RawByteString;
   lvl : Integer;
begin
   data:=args.AsDataString[0];
   lvl := args.AsInteger[1];
   if (lvl<0) or (lvl>9) then
      raise Exception.CreateFmt('Invalid deflate compression level %d', [lvl]);
   DeflateCompress(data, lvl);
   Result:=RawByteStringToScriptString(data);
end;

function TdwsWebLib.dwsWebFunctionsDeflateDecompressionFastEval(
  const args: TExprBaseListExec): Variant;
var
   data : RawByteString;
begin
   data:=args.AsDataString[0];
   DeflateDecompress(data);
   Result:=RawByteStringToScriptString(data);
end;

procedure TdwsWebLib.dwsWebFunctionsGetHostByAddrEval(info: TProgramInfo);
var
   addr : RawByteString;
begin
   addr := info.ParamAsDataString[0];
   if (addr = '127.0.0.1') or (addr = '::1') then
      info.ResultAsString := 'localhost'
   else info.ResultAsDataString := ResolveIPToName(addr, 0, 0, SOCK_STREAM);
end;

procedure TdwsWebLib.dwsWebFunctionsGetHostByNameEval(info: TProgramInfo);
var
   host : RawByteString;
begin
   host := info.ParamAsDataString[0];
   if host = 'localhost' then
      info.ResultAsString := '127.0.0.1'
   else info.ResultAsDataString := ResolveName(host);
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestCleanUp(ExternalObject: TObject);
var
   t : THttpRequestThread;
begin
   t := (ExternalObject as THttpRequestThread);
   t.Release;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsCompletedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean := (ExtObject as THttpRequestThread).Completed;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsContentDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as THttpRequestThread).Wait.ResponseData;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsContentLengthEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := (ExtObject as THttpRequestThread).ContentLength;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as THttpRequestThread).Wait.ResponseHeaders.Values['Content-Type'];
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsCurrentContentSizeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := (ExtObject as THttpRequestThread).CurrentSize;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsErrorEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as THttpRequestThread).Error;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsGetHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as THttpRequestThread).Wait.ResponseHeaders.Values[Info.ParamAsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsHeadersEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := (ExtObject as THttpRequestThread).Wait.RawResponseHeaders;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsMethodEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := (ExtObject as THttpRequestThread).Method;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsStatusCodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := (ExtObject as THttpRequestThread).Wait.StatusCode;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsURLEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
   Info.ResultAsDataString := (ExtObject as THttpRequestThread).URL;
end;

end.
