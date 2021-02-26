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
   dwsXPlatform, dwsCustomData, dwsDataContext;

type
  TdwsWebLib = class(TDataModule)
    dwsWeb: TdwsUnit;
    function dwsWebFunctionsDeflateCompressFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsWebFunctionsDeflateDecompressionFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsWebClassesHttpQueryMethodsGetDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsGetTextEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsPostDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetCredentialsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsClearCredentialsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsPutDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsDeleteEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsSetIgnoreSSLCertificateErrorsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesHttpQueryMethodsGetIgnoreSSLCertificateErrorsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebFunctionsGetHostByAddrEval(info: TProgramInfo);
    procedure dwsWebFunctionsGetHostByNameEval(info: TProgramInfo);
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
    procedure dwsWebClassesWebRequestMethodsGetQueryFieldFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsGetHeaderFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsGetCookieFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsWebClassesWebRequestMethodsHasQueryFieldFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsWebClassesWebRequestMethodsGetContentFieldFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsWebClassesWebRequestMethodsHasContentFieldFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsWebClassesWebRequestMethodsFullURLFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsURLFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsRawURLFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsMethodFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsHostFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsPathInfoFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsQueryStringFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsRemoteIPFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsHeadersFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsCookiesFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsQueryFieldsFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsSecurityFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsUserAgentFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsAuthenticatedUserFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsWebClassesWebRequestMethodsAuthenticationFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    procedure dwsWebClassesWebRequestMethodsContentTypeFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebRequestMethodsContentDataFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsWebClassesWebRequestMethodsContentLengthFastEvalInteger(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
    function dwsWebClassesWebRequestMethodsIfModifiedSinceFastEvalFloat(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
    procedure dwsWebClassesWebResponseMethodsSetStatusCodeFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetContentDataFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebResponseMethodsSetContentEventStreamFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebResponseMethodsSetContentTypeFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetContentEncodingFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetHeaderFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetContentTextFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetContentJSONFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetContentFileFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsRequestAuthenticationFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetCookie_StringStringFloat_FastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetCookie_StringStringFloatStringStringIntegerWebCookieSameSite_FastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetCompressionFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetLastModifiedFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetStaticFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetStatusPlainTextFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebRequestMethodsIfNoneMatchFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsWebClassesWebResponseMethodsSetETagFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesWebResponseMethodsSetCacheControlFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsWebClassesHttpRequestMethodsContentSubDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentFieldsEval(
      Info: TProgramInfo; ExtObject: TObject);
    function dwsWebFunctionsPingIPv4FastEval(
      const args: TExprBaseListExec): Variant;
  private
    { Private declarations }
    FServer :  IWebServerInfo;
  public
    { Public declaration }
    property Server : IWebServerInfo read FServer write FServer;
  end;

implementation

{$R *.dfm}

uses dwsWinHTTP, dwsDynamicArrays, dwsICMP;

// WebServerSentEventToRawData
//
function WebServerSentEventToRawData(const obj : TScriptObjInstance) : RawByteString;
var
   i : Integer;
   dyn : IScriptDynArray;
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
   dyn := (obj.AsInterface[obj.FieldAddress('Data')] as IScriptDynArray);
   for i := 0 to dyn.ArrayLength-1 do begin
      dyn.EvalAsString(i, buf);
      Result := Result + 'data: ' + StringToUTF8(buf) + #10;
   end;
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
                   var replyHeaders, replyData : SockString;
                   onProgress : TWinHttpProgress = nil;
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

      Result := conn.Request(uri, method, 0, '', requestData, requestContentType, replyHeaders, replyData);
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
      ResponseData : SockString;
      RawResponseHeaders : SockString;
      FResponseHeaders : TStrings;
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
      procedure PrepareResponseHeaders;
      function GetResponseHeader(const name : String) : String;
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
   FResponseHeaders.Free;
   CustomStates.Free;
end;

// PrepareResponseHeaders
//
procedure THttpRequestThread.PrepareResponseHeaders;

   procedure AddHeader(const s : String);
   var
      k : Integer;
   begin
      k := Pos(':', s);
      if k > 0 then
         FResponseHeaders.Add(SysUtils.TrimRight(Copy(s, 1, k-1) + '=' + SysUtils.Trim(Copy(s, k+1))));
   end;

var
   h : String;
   p, pn : Integer;
begin
   FResponseHeaders := TFastCompareTextList.Create;
   if (RawResponseHeaders <> '') and (not Released) then begin
      RawByteStringToScriptString(RawResponseHeaders, h);

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
end;

// GetResponseHeader
//
function THttpRequestThread.GetResponseHeader(const name : String) : String;
begin
   if FResponseHeaders = nil then
      PrepareResponseHeaders;
end;

// Execute
//
procedure THttpRequestThread.Execute;
begin
   if not Released then try
      StatusCode := HttpQuery(nil, Method, URL, RequestData, RequestContentType,
                              RawResponseHeaders, ResponseData,
                              DoProgress, CustomStates);
      FreeAndNil(CustomStates);
      RequestData := '';
      RequestContentType := '';
      ContentLength := Length(ResponseData);
      CurrentSize := ContentLength;
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
   end else begin
      Released := True;
      ReleaseLock.EndWrite;
   end;
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
   replyHeaders, buf : SockString;
begin
   Info.ResultAsInteger:=HttpQuery(Info.Execution, 'DELETE', Info.ParamAsDataString[0],
                                   '', '', replyHeaders, buf);
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsGetDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   replyHeaders, buf : SockString;
begin
   Info.ResultAsInteger := HttpQuery(Info.Execution, 'GET', Info.ParamAsDataString[0],
                                     '', '', replyHeaders, buf);
   Info.ParamAsDataString[1] := buf;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsGetTextEval(
  Info: TProgramInfo; ExtObject: TObject);
const
   cContentType : RawUTF8 = 'Content-Type:';
var
   replyHeaders, buf : SockString;
   mimeType : SockString;
   p1, p2, n : Integer;
   text : String;
   isUTF8 : Boolean;
begin
   Info.ResultAsInteger := HttpQuery(Info.Execution, 'GET', Info.ParamAsDataString[0],
                                     '', '', replyHeaders, buf);

   p1 := Pos(cContentType, RawUTF8(replyHeaders));
   if p1 > 0 then begin
      Inc(p1, Length(cContentType));
      p2 := PosEx(#13, replyHeaders, p1);
      if p2 > p1 then
         mimeType := Copy(replyHeaders, p1, p2-p1);
   end;

   n := Length(buf);
   isUTF8 := StrIEndsWithA(mimeType, 'charset=utf-8');
   if not isUTF8 then begin
      if StrEndsWithA(mimeType, '/xml') or StrEndsWithA(mimeType, '+xml') then begin
         // unqualified xml content, may still be utf-8, check data header
         if (n >= 3) and (PByte(buf)[0] = $EF) and (PByte(buf)[0] = $BB) and (PByte(buf)[0] = $BF) then
            isUTF8 := True
         else begin
            p1 := PosA('?>', buf);
            isUTF8 :=     (p1>0)
                      and (PosA('encoding="utf-8"', LowerCaseA(Copy(buf, 1, p1)))>0);
         end;
      end;
   end;

   if isUTF8 then begin
      // strip BOM if present
      if (n >= 3) and (PByte(buf)[0] = $EF) and (PByte(buf)[0] = $BB) and (PByte(buf)[0] = $BF) then
         UTF8DecodeToUnicodeString(@PUTF8Char(buf)[3], n, text)
      else UTF8DecodeToUnicodeString(PUTF8Char(buf), n, text);
   end else RawByteStringToScriptString(buf, text);

   Info.ParamAsString[1] := text;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsPostDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   headers, buf : SockString;
begin
   Info.ResultAsInteger := HttpQuery(
      Info.Execution, 'POST', Info.ParamAsDataString[0],
      Info.ParamAsDataString[1], Info.ParamAsDataString[2], headers, buf
      );
   Info.ParamAsDataString[3] := buf;
end;

procedure TdwsWebLib.dwsWebClassesHttpQueryMethodsPutDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   headers, buf : SockString;
begin
   Info.ResultAsInteger := HttpQuery(
      Info.Execution, 'PUT', Info.ParamAsDataString[0],
      Info.ParamAsDataString[1], Info.ParamAsDataString[2], headers, buf);
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
   n : Integer;
begin
   dyn := Info.ParamAsScriptDynArray[0];
   n := dyn.ArrayLength;
   if n > 0 then begin
      headers := TdwsCustomHeaders.Create;
      headers.Headers := dyn.ToStringArray;
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

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsAuthenticatedUserFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.AuthenticatedUser;
end;

function TdwsWebLib.dwsWebClassesWebRequestMethodsAuthenticationFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := Ord(args.WebRequest.Authentication);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentDataFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   RawByteStringToScriptString(args.WebRequest.ContentData, result);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentFieldsEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   names : TStringDynArray;
   fields : TStrings;
   i : Integer;
begin
   fields := Info.WebRequest.ContentFields;
   SetLength(names, fields.Count);
   for i := 0 to fields.Count-1 do
      names[i] := fields.Names[i];
   Info.ResultAsStringArray := names;
end;

function TdwsWebLib.dwsWebClassesWebRequestMethodsContentLengthFastEvalInteger(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Int64;
begin
   Result := args.WebRequest.ContentLength;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentTypeFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   RawByteStringToScriptString(args.WebRequest.ContentType, result);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookiesFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Cookies.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsFullURLFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   result := args.WebRequest.FullURL;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsGetContentFieldFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.ContentFields.Values[args.AsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsGetCookieFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Cookies.Values[args.AsString[0]];
end;

function TdwsWebLib.dwsWebClassesWebRequestMethodsHasContentFieldFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := args.WebRequest.HasContentField(args.AsString[0]);
end;

function TdwsWebLib.dwsWebClassesWebRequestMethodsHasQueryFieldFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := args.WebRequest.HasQueryField(args.AsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsGetHeaderFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Header(args.AsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHeadersFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Headers.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHostFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Host;
end;

function TdwsWebLib.dwsWebClassesWebRequestMethodsIfModifiedSinceFastEvalFloat(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
begin
   Result := args.WebRequest.IfModifiedSince.AsLocalDateTime;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsIfNoneMatchFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.IfNoneMatch;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsMethodFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Method;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsPathInfoFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.PathInfo;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsGetQueryFieldFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   result := args.WebRequest.QueryFields.Values[args.AsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryFieldsFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.QueryFields.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryStringFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.QueryString;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsRawURLFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.RawURL;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsRemoteIPFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
var
   wr : TWebRequest;
begin
   wr := args.WebRequest;
   Result := wr.Header('CF-Connecting-IP');
   if Result = '' then
      Result := wr.RemoteIP;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsSecurityFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.Security;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsURLFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.URL;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsUserAgentFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := args.WebRequest.UserAgent;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentDataFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.ContentData := args.AsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentEncodingFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.ContentEncoding := args.AsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentTypeFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.ContentType := args.AsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsRequestAuthenticationFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wra : TWebRequestAuthentication;
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      wra := TWebRequestAuthentication(args.AsInteger[0]);
      wr.Headers.Values['WWW-Authenticate'] := cWebRequestAuthenticationToString[wra];
      wr.StatusCode := 401;
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCompressionFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.Compression := args.AsBoolean[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentEventStreamFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
var
   sourceName : String;
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      args.EvalAsString(0, sourceName);
      if sourceName = '' then
         sourceName := CryptographicToken;
      wr.ContentType := 'text/event-stream,' + ScriptStringToRawByteString(sourceName);
      result := sourceName;
   end else result := '';
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentJSONFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
   v : Variant;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      args.EvalAsVariant(0, v);
      if (VarType(v) = varUnknown) and (TVarData(v).VUnknown <> nil) then begin
         wr.ContentJSON := (IUnknown(TVarData(v).VUnknown) as IBoxedJSONValue).Value.ToUnicodeString
      end else begin
         wr.ContentJSON := '';
      end;
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentFileFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   fileName, contenType : String;
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      fileName := (args.Exec as TdwsProgramExecution).FileSystem.ValidateFileName(args.AsString[0]);
      if fileName = '' then
         raise Exception.Create('SetContentFile failed: file does not exists or access denied');
      args.EvalAsString(1, contenType);
      if contenType <> '' then
         fileName := fileName + #0 + contenType;
      wr.ContentData := StringToUTF8(fileName);
      wr.ContentType := HTTP_RESP_STATICFILE;
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentTextFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
   buf : String;
   typ : RawByteString;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      args.EvalAsString(0, buf);
      ScriptStringToRawByteString(buf, typ);
      args.EvalAsString(1, buf);
      wr.ContentText[typ] := buf;
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetStatusPlainTextFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      wr.StatusCode := args.AsInteger[0];
      wr.ContentText['plain'] := args.AsString[1];
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCookie_StringStringFloat_FastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   cookie : TWebResponseCookie;
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      cookie := wr.Cookies.AddCookie(args.AsString[0]);
      cookie.Value := args.AsString[1];
      cookie.ExpiresGMT := args.AsFloat[2];
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCookie_StringStringFloatStringStringIntegerWebCookieSameSite_FastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   cookie : TWebResponseCookie;
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      cookie := wr.Cookies.AddCookie(args.AsString[0]);
      cookie.Value := args.AsString[1];
      cookie.ExpiresGMT := args.AsFloat[2];
      cookie.Path := args.AsString[3];
      cookie.Domain := args.AsString[4];
      cookie.Flags := args.AsInteger[5];
      cookie.SameSite := TWebResponseCookieSameSite(args.AsInteger[6]);
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetHeaderFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.Headers.Values[args.AsString[0]] := args.AsString[1];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetLastModifiedFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.LastModified := TdwsDateTime.FromLocalDateTime(args.AsFloat[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetETagFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.ETag := args.AsString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCacheControlFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   var wr := args.WebResponse;
   if wr <> nil then
      wr.CacheControl := args.AsString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetStaticFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   h : TWebResponseHints;
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then begin
      h := wr.Hints;
      if args.AsBoolean[0] then
         Include(h, shStatic)
      else Exclude(h, shStatic);
      wr.Hints := h;
   end;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetStatusCodeFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   wr : TWebResponse;
begin
   wr := args.WebResponse;
   if wr <> nil then
      wr.StatusCode := args.AsInteger[0];
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

function TdwsWebLib.dwsWebFunctionsPingIPv4FastEval(
  const args: TExprBaseListExec): Variant;
begin
   Result := PingIPv4(args.AsString[0], args.AsInteger[1]);
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
   Info.ResultAsDataString := (ExtObject as THttpRequestThread).Wait.ResponseData;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsContentLengthEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := (ExtObject as THttpRequestThread).ContentLength;
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsContentSubDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := Copy((ExtObject as THttpRequestThread).Wait.ResponseData,
                                   Info.ParamAsInteger[0]+1, Info.ParamAsInteger[1]);
end;

procedure TdwsWebLib.dwsWebClassesHttpRequestMethodsContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as THttpRequestThread).Wait.GetResponseHeader('Content-Type');
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
   Info.ResultAsString := (ExtObject as THttpRequestThread).Wait.GetResponseHeader(Info.ParamAsString[0]);
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
