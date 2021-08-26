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
   dwsWebEnvironment, dwsUtils, dwsHTTPSysAPI, dwsWebServerHelpers,
   dwsURLRewriter;

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

         FRawURL, FURL : String;

         FHeaders : TStrings;

         FIP_UTF8 : RawByteString;
         FLastIP : TVarSin;

         FInContent : RawByteString;
         FInContentType : RawByteString;

      protected
         function  GetHeaders : TStrings; override;

         procedure PrepareAuthenticationInfo;
         procedure PrepareHeaders;
         procedure PrepareIP_UTF8;

         function GetAuthentication : TWebRequestAuthentication; override;
         function GetAuthenticatedUser : String; override;

      public
         constructor Create;
         destructor Destroy; override;

         procedure SetRequest(val : PHTTP_REQUEST_V2; rewriter : TdwsURLRewriter);
         property Request : PHTTP_REQUEST_V2 read FRequest;

         function RemoteIP : String; override;
         procedure GetRemoteIP(var ip : RawByteString);
         function RemoteIP_UTF8 : PAnsiChar;
         function RemoteIP_UTF8_Length : Integer;

         function RawURL : String; override;
         function URL : String; override;
         function FullURL : String; override;
         function Method : String; override;
         function MethodVerb : TWebRequestMethodVerb; override;
         function Security : String; override;
         function Secure : Boolean; override;

         function ContentLength : Integer; override;
         function ContentData : RawByteString; override;
         function ContentType : RawByteString; override;

         property InContent : RawByteString read FInContent write FInContent;
         property InContentType : RawByteString read FInContentType write FInContentType;
         property Authentication : TWebRequestAuthentication read GetAuthentication;
         property AuthenticatedUser : String read GetAuthenticatedUser;
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
   host : array [0..NI_MAXHOST] of AnsiChar;
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
      r := GetNameInfo(@sin, SizeOfVarSin(sin), host, hostlen,
                       nil, servlen, NI_NUMERICHOST + NI_NUMERICSERV);
      if r = 0 then
         Result := PAnsiChar(@host)
      else Result := '';
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
   FHeaders := TFastCompareTextList.Create;
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
procedure THttpSysWebRequest.SetRequest(val : PHTTP_REQUEST_V2; rewriter : TdwsURLRewriter);
var
   p : PChar;
   n, urlLength : Integer;
   checkQueryString : Boolean;
begin
   FRequest := val;

   if val.CookedUrl.QueryStringLength > 0 then begin
      urlLength := (val.CookedUrl.AbsPathLength + val.CookedUrl.QueryStringLength) div SizeOf(Char);
      SetLength(FRawURL, urlLength);
      p := Pointer(FRawURL);
      System.Move(val.CookedUrl.pAbsPath^, p^, val.CookedUrl.AbsPathLength);
      Inc(p, val.CookedUrl.AbsPathLength  div SizeOf(Char));
      System.Move(val.CookedUrl.pQueryString^, p^, val.CookedUrl.QueryStringLength);
      checkQueryString := True;
   end else begin
      urlLength := val.CookedUrl.AbsPathLength div SizeOf(Char);
      SetString(FRawURL, val.CookedUrl.pAbsPath, urlLength);
      checkQueryString := False;
   end;
   if (rewriter <> nil) and (rewriter.Count > 0) and rewriter.Apply(FRawURL, FURL) then begin
      checkQueryString := True;
      urlLength := Length(FURL);
   end else FURL := FRawURL;

   // breakup path & querystring
   if checkQueryString then begin
      p := Pointer(FURL);
      for n := 1 to urlLength do begin
         if p^ = '?' then begin
            SetString(FPathInfo, PChar(Pointer(FURL)), n-1);
            Inc(p);
            SetString(FQueryString, p, urlLength-n);
            checkQueryString := False;
            Break;
         end;
         Inc(p);
      end;
      if checkQueryString then begin
         FPathInfo := FURL;
         FQueryString := '';
      end;
   end else begin
      FPathInfo := FURL;
      FQueryString := '';
   end;

   FPrepared := [];
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
function THttpSysWebRequest.RawURL : String;
begin
   Result := FRawURL;
end;

// URL
//
function THttpSysWebRequest.URL : String;
begin
   Result := FURL;
end;

// FullURL
//
function THttpSysWebRequest.FullURL : String;
begin
   SetString(Result, FRequest.CookedUrl.pFullUrl, FRequest.CookedUrl.FullUrlLength div SizeOf(Char));
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
