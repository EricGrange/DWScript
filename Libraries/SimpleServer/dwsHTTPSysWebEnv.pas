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
    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info
}
unit dwsHTTPSysWebEnv;

interface

uses
   Windows, Classes, SysUtils, StrUtils,
   SynCommons, SynWinSock,
   dwsWebEnvironment, dwsUtils, dwsHTTPSysAPI, dwsWebServerHelpers;

type

   THttpSysWebRequestPrepares = set of (
      prepAuthentication,
      prepHeaders,
      prepIP_UTF8
   );

   THttpSysWebRequest = class (TWebRequest)
      private
         FRequest : PHTTP_REQUEST_V2;

         FPrepared : THttpSysWebRequestPrepares;

         FAuthentication : TWebRequestAuthentication;
         FAuthenticatedUser : String;

         FURL : String;

         FHeaders : TStrings;

         FIP_UTF8 : RawByteString;
         FLastIP : TVarSin;

         FInContent : RawByteString;
         FInContentType : RawByteString;

      protected
         procedure SetRequest(val : PHTTP_REQUEST_V2);
         function  GetHeaders : TStrings; override;

         procedure PrepareAuthenticationInfo;
         procedure PrepareURL;
         procedure PrepareHeaders;
         procedure PrepareIP_UTF8;

         function GetAuthentication : TWebRequestAuthentication; override;
         function GetAuthenticatedUser : String; override;

      public
         constructor Create;
         destructor Destroy; override;

         property Request : PHTTP_REQUEST_V2 read FRequest write SetRequest;

         function RemoteIP : String; override;
         procedure GetRemoteIP(var ip : RawByteString);
         function RemoteIP_UTF8 : PAnsiChar;
         function RemoteIP_UTF8_Length : Integer;

         function RawURL : RawByteString; override;
         function URL : String; override;
         function Method : String; override;
         function MethodVerb : TWebRequestMethodVerb; override;
         function Security : String; override;
         function Secure : Boolean; override;

         function ContentLength : Integer; override;
         function ContentData : RawByteString; override;
         function ContentType : RawByteString; override;

         property InContent : RawByteString read FInContent write FInContent;
         property InContentType : RawByteString read FInContentType write FInContentType;
         property Authentication : TWebRequestAuthentication read FAuthentication write FAuthentication;
         property AuthenticatedUser : String read FAuthenticatedUser write FAuthenticatedUser;
   end;

   THttpSysWebResponse = class (TWebResponse)
      private

      public

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure GetDomainUserFromToken(hToken : THandle; var result: String);
var
   err : Boolean;
   buffer : array [0..511] of Byte;
   bufferSize, userSize, domainSize : DWORD;
   pBuffer : PTokenUser;
   nameUse : SID_NAME_USE;
   p : PChar;
begin
   err:=GetTokenInformation(hToken, TokenUser, @buffer, SizeOf(buffer), bufferSize);
   if not err then Exit;
   pBuffer:=@buffer;

   // get field sizes
   userSize:=0;
   domainSize:=0;
   LookupAccountSid(nil, pBuffer.User.Sid, nil, userSize, nil, domainSize, nameUse);
   if (userSize=0) or (domainSize=0) then Exit;
   SetLength(result, userSize+domainSize-1);
   p:=Pointer(result);
   err:=LookupAccountSid(nil, pBuffer.User.Sid, @p[domainSize], userSize, p, domainSize, nameUse);
   if err then
      p[domainSize]:='\'
   else result:='';
end;

procedure GetSinIP(const Sin: TVarSin; var Result : RawByteString);
var
   p : PAnsiChar;
   host : array[0..NI_MAXHOST] of AnsiChar;
   hostlen, servlen : integer;
   r : integer;
begin
   if not IsNewApi(Sin.AddressFamily) then begin
      p := inet_ntoa(Sin.sin_addr);
      if p <> nil then
         Result := p
      else Result := ''
   end else begin
      hostlen := NI_MAXHOST;
      servlen := NI_MAXSERV;
      r := getnameinfo(@sin, SizeOfVarSin(sin), host, hostlen,
                       nil, servlen, NI_NUMERICHOST + NI_NUMERICSERV);
      if r=0 then begin
         hostlen := StrLen(PAnsiChar(@host));
         SetLength(Result, hostlen);
         Move(host, Pointer(Result)^, hostlen);
      end else Result := '';
   end;
end;

// ------------------
// ------------------ THttpSysWebRequest ------------------
// ------------------

// Create
//
constructor THttpSysWebRequest.Create;
begin
   inherited;
   FHeaders:=TFastCompareStringList.Create;
end;

// Destroy
//
destructor THttpSysWebRequest.Destroy;
begin
   FHeaders.Free;
   inherited;
end;

// SetRequest
//
procedure THttpSysWebRequest.SetRequest(val : PHTTP_REQUEST_V2);
var
   p : PChar;
   n : Integer;
begin
   FRequest:=val;

   SetString(FPathInfo, FRequest.CookedUrl.pAbsPath, FRequest.CookedUrl.AbsPathLength div SizeOf(Char));

   // eliminate leading '?'
   p:=FRequest.CookedUrl.pQueryString;
   n:=FRequest.CookedUrl.QueryStringLength;
   if (p<>nil) and (p^='?') then begin
      Inc(p);
      Dec(n);
   end;
   SetString(FQueryString, p, n div SizeOf(Char));

   FPrepared:=[];
   FURL:='';
   ResetCookies;
   ResetFields;
end;

// PrepareAuthenticationInfo
//
procedure THttpSysWebRequest.PrepareAuthenticationInfo;
const
   cHRAtoWRA : array [HTTP_REQUEST_AUTH_TYPE] of TWebRequestAuthentication = (
      wraNone, wraBasic, wraDigest, wraNTLM, wraNegotiate, wraKerberos
   );
var
   authInfo : PHTTP_REQUEST_AUTH_INFO;
begin
   Include(FPrepared, prepAuthentication);

   FAuthentication:=wraNone;
   FAuthenticatedUser:='';
   if (request.RequestInfoCount>0) and (request.pRequestInfo.InfoType=HttpRequestInfoTypeAuth) then begin
      authInfo:=PHTTP_REQUEST_AUTH_INFO(request.pRequestInfo.pInfo);
      case authInfo.AuthStatus of
         HttpAuthStatusSuccess : begin
            FAuthentication:=cHRAtoWRA[authInfo.AuthType];
            if authInfo.AccessToken<>0 then
               GetDomainUserFromToken(authInfo.AccessToken, FAuthenticatedUser);
         end;
         HttpAuthStatusFailure :
            FAuthentication:=wraFailed;
      end;
   end;
end;

// PrepareURL
//
procedure THttpSysWebRequest.PrepareURL;
begin
   FURL:=UTF8ToString(UrlDecode(Request^.pRawUrl));
end;

// PrepareHeaders
//
procedure THttpSysWebRequest.PrepareHeaders;
const
   cKNOWNHEADERS_NAME : array [reqCacheControl..reqUserAgent] of String = (
      'Cache-Control', 'Connection', 'Date', 'Keep-Alive', 'Pragma', 'Trailer',
      'Transfer-Encoding', 'Upgrade', 'Via', 'Warning', 'Allow', 'Content-Length',
      'Content-Type', 'Content-Encoding', 'Content-Language', 'Content-Location',
      'Content-MD5', 'Content-Range', 'Expires', 'Last-Modified', 'Accept',
      'Accept-Charset', 'Accept-Encoding', 'Accept-Language', 'Authorization',
      'Cookie', 'Expect', 'From', 'Host', 'If-Match', 'If-Modified-Since',
      'If-None-Match', 'If-Range', 'If-Unmodified-Since', 'Max-Forwards',
      'Proxy-Authorization', 'Referer', 'Range', 'TE', 'Translate', 'User-Agent');
var
   i: Integer;
   h : THttpHeader;
   p : PHTTP_UNKNOWN_HEADER;
   head : PHTTP_REQUEST_HEADERS;
   buf : String;
begin
   Include(FPrepared, prepHeaders);
   FHeaders.Clear;

   head:=@request.Headers;

   Assert(Low(cKNOWNHEADERS_NAME) = Low(head.KnownHeaders));
   Assert(High(cKNOWNHEADERS_NAME) = High(head.KnownHeaders));
   // set headers content
   for h:=Low(head.KnownHeaders) to High(head.KnownHeaders) do begin
      if head.KnownHeaders[h].RawValueLength<>0 then begin
         buf:= cKNOWNHEADERS_NAME[h]
              +'='
              +UTF8DecodeToString(PUTF8Char(head.KnownHeaders[h].pRawValue),
                                  head.KnownHeaders[h].RawValueLength);
         FHeaders.Add(buf);
      end;
   end;
   p:=head.pUnknownHeaders;
   if p<>nil then begin
      for i:=1 to head.UnknownHeaderCount do begin
         buf:= UTF8DecodeToString(PUTF8Char(p.pName), p.NameLength)
              +'='
              +UTF8DecodeToString(PUTF8Char(p.pRawValue), p.RawValueLength);
         FHeaders.Add(buf);
         Inc(p);
      end;
   end;
end;

// GetHeaders

// PrepareIP_UTF8
//
var cNullIPVarSin : TVarSin;
procedure THttpSysWebRequest.PrepareIP_UTF8;
var
   p : PVarSin;
begin
   Include(FPrepared, prepIP_UTF8);

   if Request^.Address.pRemoteAddress<>nil then
      p:=PVarSin(Request^.Address.pRemoteAddress)
   else p:=@cNullIPVarSin;

   if not CompareMem(p, @FLastIP, SizeOf(FLastIP)) then begin
      FLastIP:=p^;
      GetSinIP(p^, FIP_UTF8);
   end;
end;
//
function THttpSysWebRequest.GetHeaders : TStrings;
begin
   if not (prepHeaders in FPrepared) then
      PrepareHeaders;
   Result:=FHeaders;
end;

// GetAuthentication
//
function THttpSysWebRequest.GetAuthentication : TWebRequestAuthentication;
begin
   if not (prepAuthentication in FPrepared) then
      PrepareAuthenticationInfo;
   Result:=FAuthentication;
end;

// GetAuthenticatedUser
//
function THttpSysWebRequest.GetAuthenticatedUser : String;
begin
   if not (prepAuthentication in FPrepared) then
      PrepareAuthenticationInfo;
   Result:=FAuthenticatedUser;
end;

// RemoteIP
//
function THttpSysWebRequest.RemoteIP : String;
begin
   Result:=UTF8ToString(RemoteIP_UTF8);
end;

// GetRemoteIP
//
procedure THttpSysWebRequest.GetRemoteIP(var ip : RawByteString);
begin
   if not (prepIP_UTF8 in FPrepared) then
      PrepareIP_UTF8;

   ip:=FIP_UTF8;
end;

// RemoteIP_UTF8
//
function THttpSysWebRequest.RemoteIP_UTF8 : PAnsiChar;
begin
   if not (prepIP_UTF8 in FPrepared) then
      PrepareIP_UTF8;
   Result:=Pointer(FIP_UTF8);
end;

// RemoteIP_UTF8_Length
//
function THttpSysWebRequest.RemoteIP_UTF8_Length : Integer;
begin
   if not (prepIP_UTF8 in FPrepared) then
      PrepareIP_UTF8;

   Result:=Length(FIP_UTF8);
end;

// RawURL
//
function THttpSysWebRequest.RawURL : RawByteString;
begin
   Result:=Request^.pRawUrl;
end;

// URL
//
function THttpSysWebRequest.URL : String;
begin
   if FURL='' then
      PrepareURL;
   Result:=FURL;
end;

// Method
//
function THttpSysWebRequest.Method : String;
const
   cVERB_TEXT : array [hvOPTIONS..hvSEARCH] of String = (
      'OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE',
      'CONNECT', 'TRACK', 'MOVE', 'COPY', 'PROPFIND', 'PROPPATCH',
      'MKCOL', 'LOCK', 'UNLOCK', 'SEARCH' );
begin
   case Request^.Verb of
      Low(cVERB_TEXT)..High(cVERB_TEXT) :
         Result:=cVERB_TEXT[Request^.Verb]
   else
      SetString(Result, Request^.pUnknownVerb, Request^.UnknownVerbLength);
   end;
end;

// MethodVerb
//
function THttpSysWebRequest.MethodVerb : TWebRequestMethodVerb;
begin
   case Request^.Verb of
      hvOPTIONS..hvSEARCH :
         Result:=TWebRequestMethodVerb(Ord(Request^.Verb)+(Ord(wrmvOPTIONS)-Ord(hvOPTIONS)));
   else
      Result:=wrmvUnknown;
   end;
end;

// Security
//
function THttpSysWebRequest.Security : String;
begin
   if request^.pSslInfo<>nil then
      Result:=Format('SSL, %d bits', [request^.pSslInfo^.ConnectionKeySize*8])
   else Result:='';
end;

// Secure
//
function THttpSysWebRequest.Secure : Boolean;
begin
   Result:=(request^.pSslInfo<>nil);
end;

// ContentLength
//
function THttpSysWebRequest.ContentLength : Integer;
begin
   Result:=Length(InContent);
end;

// ContentData
//
function THttpSysWebRequest.ContentData : RawByteString;
begin
   Result:=InContent;
end;

// ContentType
//
function THttpSysWebRequest.ContentType : RawByteString;
begin
   Result:=InContentType;
end;


end.
