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
unit dwsWinHTTP;

interface

uses
   Windows, SysUtils, Winapi.WinInet, Winapi.WinHTTP, Variants,
   SynCrtSock, SynCommons,
   dwsUtils, dwsWebEnvironment, dwsXPlatform;

type
   TdwsCustomHeaders = class (TInterfacedSelfObject)
      Headers : TStringDynArray;
   end;

   TdwsWinHTTP = class;

   TdwsHttpCertificateInfo = class
      Expiry, Start : Int64;
      SubjectInfo, IssuerInfo : String;
      //ProtocolName, SignatureAlgName, EncryptionAlgName : PWideChar;
      KeySize : Cardinal;
      procedure Clear;
      procedure Read(conn : TdwsWinHTTP);
   end;

   TdwsWinHTTP = class (TWinHTTP)
      protected
         AuthorizationHeader : String;
         CustomHeaders : TdwsCustomHeaders;
         procedure InternalCreateRequest(const aMethod,aURL: SockString); override;
         procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
         procedure InternalSendRequest(const aMethod,aData: SockString); override;
      public
         CertificateInfo : TdwsHttpCertificateInfo;
         DisableRedirects : Boolean;
         function GetCertificateInfo(var certInfo : WINHTTP_CERTIFICATE_INFO) : Boolean;
   end;

   TdwsWinHttpConnection = class
      FProxyName : String;
      FConnectTimeout, FSendTimeout, FReceiveTimeout : Integer;
      FPort : SockString;
      FWinHttp : TdwsWinHTTP;

      destructor Destroy; override;

      procedure ConnectServer(const uri : TURI; const proxyName : String;
                              connectTimeout, sendTimeout, receiveTimeout : Integer);

      procedure SetCredentials(const credentials : Variant);
      procedure SetCustomHeaders(const customHeaders : Variant);
      procedure SetIgnoreSSLErrors(const ignore : Variant);
      procedure SetOnProgress(const event : TWinHttpProgress);

      function Request(const uri : TURI; const method: SockString; keepAlive: cardinal;
                       const inHeader, inData, inDataType : SockString;
                       var replyHeaders, replyData : SockString) : Integer;
      class procedure ReplyToText(const replyHeaders, replyData : SockString;
                                  var textData : String);

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsWinHttp ------------------
// ------------------

// InternalCreateRequest
//
procedure TdwsWinHTTP.InternalCreateRequest(const aMethod,aURL: SockString);
const ALL_ACCEPT: array[0..1] of PWideChar = ('*/*',nil);
      ACCEPT_TYPES: array[boolean] of PLPWSTR = (@ALL_ACCEPT,nil);
var
   flags : DWORD;
begin
   flags := WINHTTP_FLAG_REFRESH; // options for a true RESTful request
   if fHttps then
      flags := flags or WINHTTP_FLAG_SECURE;
   fRequest := WinHttpOpenRequest(
      fConnection, Pointer(UnicodeString(aMethod)),
      Pointer(UnicodeString(aURL)), nil, nil,
      ACCEPT_TYPES[fNoAllAccept], flags
   );
   if fRequest = nil then
      RaiseLastOSError;
   if (fKeepAlive = 0) or DisableRedirects then begin
      flags := WINHTTP_DISABLE_KEEP_ALIVE * Ord(fKeepAlive = 0)
             + WINHTTP_DISABLE_REDIRECTS * Ord(DisableRedirects);
      if not WinHttpSetOption(fRequest, WINHTTP_OPTION_DISABLE_FEATURE, @flags, SizeOf(flags)) then
         RaiseLastOSError;
   end;
end;

// InternalConnect
//
procedure TdwsWinHTTP.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
begin
   UserAgent := 'Mozilla/5.0 (Windows; Synopse DWScript)';
   inherited;
end;

// InternalSendRequest
//
procedure TdwsWinHTTP.InternalSendRequest(const aMethod,aData: SockString);
var
   i : Integer;
   header : String;
   acceptSeen : Boolean;
   flag : DWORD;
begin
   if AuthorizationHeader<>'' then
      if not WinHttpAddRequestHeaders(FRequest, Pointer(AuthorizationHeader), Length(AuthorizationHeader),
                                      WINHTTP_ADDREQ_FLAG_COALESCE) then
         RaiseLastOSError;
   acceptSeen := False;
   if Assigned(CustomHeaders) then begin
      for i := 0 to High(CustomHeaders.Headers) do begin
         header := CustomHeaders.Headers[i];
         if header = '' then continue;
         flag := WINHTTP_ADDREQ_FLAG_COALESCE;
         if not acceptSeen then begin
            if StrBeginsWith(header, 'Accept:') then begin
               flag := WINHTTP_ADDREQ_FLAG_REPLACE;
               acceptSeen := True;
            end;
         end;
         if not WinHttpAddRequestHeaders(FRequest, Pointer(header), Length(header), flag) then
            RaiseLastOSError;
      end;
   end;
   inherited;
   if CertificateInfo <> nil then
      CertificateInfo.Read(Self);
end;

// GetCertificateInfo
//
function TdwsWinHTTP.GetCertificateInfo(var certInfo : WINHTTP_CERTIFICATE_INFO) : Boolean;
var
   bufLen : Cardinal;
begin
   bufLen := SizeOf(certInfo);
   Result := WinHttpQueryOption(fRequest, WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT, certInfo, bufLen);
   if not Result then RaiseLastOSError;
end;

// ------------------
// ------------------ TdwsWinHttpConnection ------------------
// ------------------

// Destroy
//
destructor TdwsWinHttpConnection.Destroy;
begin
   inherited;
   FWinHttp.Free;
end;

// ConnectServer
//
procedure TdwsWinHttpConnection.ConnectServer(const uri : TURI; const proxyName : String;
                                              connectTimeout, sendTimeout, receiveTimeout : Integer);
begin
   if FWinHttp <> nil then begin
      if    (FWinHttp.Server <> uri.Server)
         or (FPort <> uri.Port)
         or (FWinHttp.Https <> uri.Https)
         or (FProxyName <> proxyName)
         or (FConnectTimeout <> connectTimeout)
         or (FSendTimeout <> sendTimeout)
         or (FReceiveTimeout <> receiveTimeout) then begin
         FWinHttp.Free;
         FWinHttp := nil;
      end;
   end;
   if FWinHttp = nil then begin
      FWinHttp := TdwsWinHTTP.Create(uri.Server, uri.Port, uri.Https,
                                     SynUnicodeToUtf8(proxyName), '',
                                     connectTimeout, sendTimeout, receiveTimeout);
      FProxyName := proxyName;
      FPort := uri.Port;
      FConnectTimeout := connectTimeout;
      FSendTimeout := sendTimeout;
      FReceiveTimeout := receiveTimeout;
   end;
end;

// SetCredentials
//
procedure TdwsWinHttpConnection.SetCredentials(const credentials : Variant);
begin
   if VariantIsArray(credentials) then begin
      case TWebRequestAuthentication(credentials[0]) of
         wraBasic : FWinHttp.AuthScheme := THttpRequestAuthentication.wraBasic;
         wraDigest : FWinHttp.AuthScheme := THttpRequestAuthentication.wraDigest;
         wraNegotiate : FWinHttp.AuthScheme := THttpRequestAuthentication.wraNegotiate;
         wraAuthorization : FWinHttp.AuthorizationHeader := 'Authorization: ' + credentials[1];
      else
         FWinHttp.AuthScheme := THttpRequestAuthentication.wraNone;
      end;
      FWinHttp.AuthUserName := credentials[1];
      FWinHttp.AuthPassword := credentials[2];
   end else begin
      FWinHttp.AuthScheme := THttpRequestAuthentication.wraNone;
      FWinHttp.AuthUserName := '';
      FWinHttp.AuthPassword := '';
   end;
end;

// SetCustomHeaders
//
procedure TdwsWinHttpConnection.SetCustomHeaders(const customHeaders : Variant);
begin
   if VariantType(customHeaders) = varUnknown then
      FWinHttp.CustomHeaders := IGetSelf(TVarData(customHeaders).VUnknown).GetSelf as TdwsCustomHeaders
   else FWinHttp.CustomHeaders := nil;
end;

// SetIgnoreSSLErrors
//
procedure TdwsWinHttpConnection.SetIgnoreSSLErrors(const ignore : Variant);
begin
   if (VariantType(ignore)=varBoolean) and TVarData(ignore).VBoolean then
       FWinHttp.IgnoreSSLCertificateErrors := True
   else FWinHttp.IgnoreSSLCertificateErrors := False;
end;

// SetOnProgress
//
procedure TdwsWinHttpConnection.SetOnProgress(const event : TWinHttpProgress);
begin
   FWinHttp.OnProgress := event;
end;

// Request
//
function TdwsWinHttpConnection.Request(const uri : TURI; const method: SockString; keepAlive: cardinal;
                                       const inHeader, inData, inDataType : SockString;
                                       var replyHeaders, replyData : SockString) : Integer;
begin
   Result := FWinHttp.Request(uri.Address, method, keepAlive, inHeader, inData, inDataType,
                              replyHeaders, replyData);
end;

// ReplyToText
//
class procedure TdwsWinHttpConnection.ReplyToText(const replyHeaders, replyData : SockString;
                                                  var textData : String);

   procedure ReplyToText(const buf : SockString; var replyData : String);
   const
      cContentType : RawUTF8 = 'Content-Type:';
   var
      mimeType : SockString;
      p1, p2, n : Integer;
   begin
      p1:=Pos(cContentType, RawUTF8(replyHeaders));
      if p1>0 then begin
         Inc(p1, Length(cContentType));
         p2:=PosEx(#13, replyHeaders, p1);
         if p2>p1 then
            mimeType:=Copy(replyHeaders, p1, p2-p1);
      end;
      if StrEndsWithA(mimeType, '/xml') or StrEndsWithA(mimeType, '+xml') then begin
         // unqualified xml content, may still be utf-8, check data header
         if StrBeginsWithBytes(buf, [$EF, $BB, $BF]) then
            mimeType := mimeType + '; charset=utf-8'
         else begin
            p1:=PosA('?>', buf);
            if     (p1>0)
               and (PosA('encoding="utf-8"', LowerCaseA(Copy(buf, 1, p1)))>0) then begin
               mimeType := mimeType + '; charset=utf-8';
            end;
         end;
      end;

      if StrIEndsWithA(mimeType, 'charset=utf-8') then begin
         // strip BOM if present
         n := Length(buf);
         if (n >= 3) and (PByte(buf)[0] = $EF) and (PByte(buf)[0] = $BB) and (PByte(buf)[0] = $BF) then
            UTF8DecodeToUnicodeString(@PUTF8Char(buf)[3], n, replyData)
         else UTF8DecodeToUnicodeString(PUTF8Char(buf), n, replyData);
      end else RawByteStringToScriptString(buf, replyData);
   end;

begin
   ReplyToText(replyData, textData);
end;

// ------------------
// ------------------ TdwsHttpCertificateInfo ------------------
// ------------------

// Clear
//
procedure TdwsHttpCertificateInfo.Clear;
begin
   Expiry := 0;
   Start := 0;
   SubjectInfo := '';
   IssuerInfo := '';
   //ProtocolName := nil;
   //SignatureAlgName := nil;
   //EncryptionAlgName := '';
   KeySize := 0;
end;

// Read
//
procedure TdwsHttpCertificateInfo.Read(conn : TdwsWinHTTP);
var
   buf : WINHTTP_CERTIFICATE_INFO;
   bufLen : Cardinal;
begin
   bufLen := SizeOf(buf);
   if WinHttpQueryOption(conn.fRequest, WINHTTP_OPTION_SECURITY_CERTIFICATE_STRUCT, buf, bufLen) then begin
      try
         Expiry := FileTimeToUnixTime(buf.ftExpiry);
         Start := FileTimeToUnixTime(buf.ftExpiry);
         SubjectInfo := buf.lpszSubjectInfo;
         IssuerInfo := buf.lpszIssuerInfo;
         //ProtocolName := buf.lpszProtocolName;
         //SignatureAlgName := buf.lpszSignatureAlgName;
         //EncryptionAlgName := buf.lpszEncryptionAlgName;
         KeySize := buf.dwKeySize;
      finally
         LocalFree(buf.lpszSubjectInfo);
         LocalFree(buf.lpszIssuerInfo);
      end;
   end else Clear;
end;

end.
