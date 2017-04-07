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
   Windows, SysUtils,
   SynCrtSock, SynCommons,
   dwsUtils, dwsWebEnvironment;

type
   TdwsCustomHeaders = class (TInterfacedSelfObject)
      Headers : array of RawByteString;
   end;

   TdwsWinHTTP = class (TWinHTTP)
      protected
         AuthorizationHeader : RawByteString;
         CustomHeaders : TdwsCustomHeaders;
         procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
         procedure InternalSendRequest(const aData: SockString); override;
   end;

   TdwsWinHttpConnection = class (TInterfacedSelfObject)
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
                       var replyHeaders : SockString; var replyData : String;
                       asText : Boolean) : Integer;

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

// InternalConnect
//
procedure TdwsWinHTTP.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
begin
   UserAgent := 'Mozilla/5.0 (Windows; Synopse DWScript)';
   inherited;
end;

// InternalSendRequest
//
procedure TdwsWinHTTP.InternalSendRequest(const aData: SockString);
var
   i : Integer;
begin
   if AuthorizationHeader<>'' then
      InternalAddHeader('Authorization: '+AuthorizationHeader);
   if Assigned(CustomHeaders) then
      for i := 0 to High(CustomHeaders.Headers) do
         InternalAddHeader(CustomHeaders.Headers[i]);

   inherited;
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
         wraAuthorization : FWinHttp.AuthorizationHeader := UTF8Encode(credentials[1]);
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
                                       var replyHeaders : SockString; var replyData : String;
                                       asText : Boolean) : Integer;

   procedure ReplyToText(var buf : SockString; var replyData : String);
   const
      cContentType : RawUTF8 = 'Content-Type:';
   var
      mimeType : SockString;
      p1, p2 : Integer;
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
         if StrBeginsWithBytes(buf, [$EF, $BB, $BF]) then
            Delete(buf, 1, 3);
         replyData:=UTF8DecodeToUnicodeString(buf);
      end else RawByteStringToScriptString(buf, replyData);
   end;

var
   buf : SockString;
begin
   Result := FWinHttp.Request(uri.Address, method, keepAlive, inHeader, inData, inDataType,
                              replyHeaders, buf);
   if asText then
      ReplyToText(buf, replyData)
   else RawByteStringToScriptString(buf, replyData);
end;

end.
