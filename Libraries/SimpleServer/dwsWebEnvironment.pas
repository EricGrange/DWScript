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
unit dwsWebEnvironment;

interface

uses Classes, SysUtils, StrUtils, dwsExprs, dwsUtils;

type
   TWebRequestAuthentication = (
      wraNone,
      wraFailed,
      wraBasic,
      wraDigest,
      wraNTLM,
      wraNegotiate,
      wraKerberos
   );

   TWebRequestMethodVerb = (
      wrmvUnknown,
      wrmvOPTIONS,
      wrmvGET,
      wrmvHEAD,
      wrmvPOST,
      wrmvPUT,
      wrmvDELETE,
      wrmvTRACE,
      wrmvCONNECT,
      wrmvTRACK,
      wrmvMOVE,
      wrmvCOPY,
      wrmvPROPFIND,
      wrmvPROPPATCH,
      wrmvMKCOL,
      wrmvLOCK,
      wrmvUNLOCK,
      wrmvSEARCH
   );

   TWebRequest = class
      private
         FCookies : TStrings;
         FQueryFields : TStrings;
         FCustom : TObject;

      protected
         FPathInfo : String;
         FQueryString : String;

         function GetHeaders : TStrings; virtual; abstract;
         function GetCookies : TStrings;
         function GetQueryFields : TStrings;

         function GetUserAgent : String;

         function GetAuthentication : TWebRequestAuthentication; virtual;
         function GetAuthenticatedUser : String; virtual;

         function PrepareCookies : TStrings; virtual;
         function PrepareQueryFields : TStrings; virtual;

      public
         constructor Create;
         destructor Destroy; override;

         function Header(const headerName : String) : String;

         function RemoteIP : String; virtual; abstract;

         function RawURL : RawByteString; virtual; abstract;
         function URL : String; virtual; abstract;
         function Method : String; virtual; abstract;
         function MethodVerb : TWebRequestMethodVerb; virtual; abstract;
         function Security : String; virtual; abstract;

         function ContentData : RawByteString; virtual; abstract;
         function ContentType : RawByteString; virtual; abstract;

         property PathInfo : String read FPathInfo write FPathInfo;
         property QueryString : String read FQueryString write FQueryString;
         property UserAgent : String read GetUserAgent;

         property Headers : TStrings read GetHeaders;
         property Cookies : TStrings read GetCookies;
         property QueryFields : TStrings read GetQueryFields;

         function HasQueryField(const name : String) : Boolean;

         property Authentication : TWebRequestAuthentication read GetAuthentication;
         property AuthenticatedUser : String read GetAuthenticatedUser;

         // custom object field, freed with the request
         property Custom : TObject read FCustom write FCustom;
   end;

   TWebResponse = class
      private
         FStatusCode : Integer;
         FContentData : RawByteString;
         FContentType : RawByteString;
         FContentEncoding : RawByteString;
         FHeaders : TStrings;

      protected
         procedure SetContentText(const textType : RawByteString; const text : String);

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear; virtual;

         function HasHeaders : Boolean; inline;
         function CompiledHeaders : RawByteString;

         property StatusCode : Integer read FStatusCode write FStatusCode;
         property ContentText[const textType : RawByteString] : String write SetContentText;
         property ContentData : RawByteString read FContentData write FContentData;
         property ContentType : RawByteString read FContentType write FContentType;
         property ContentEncoding : RawByteString read FContentEncoding write FContentEncoding;

         property Headers : TStrings read FHeaders;
   end;

   IWebEnvironment = interface
      ['{797FDC50-0643-4290-88D1-8BD3C0D7C303}']
      function GetWebRequest : TWebRequest;
      function GetWebResponse : TWebResponse;

      property WebRequest : TWebRequest read GetWebRequest;
      property WebResponse : TWebResponse read GetWebResponse;
   end;

   TWebEnvironment = class (TInterfacedSelfObject, IdwsEnvironment, IWebEnvironment)
      protected
         function GetWebRequest : TWebRequest;
         function GetWebResponse : TWebResponse;

      public
         WebRequest : TWebRequest;
         WebResponse : TWebResponse;
   end;

   TWebEnvironmentHelper = class helper for TProgramInfo
      function WebEnvironment : IWebEnvironment; inline;
      function WebRequest : TWebRequest; inline;
      function WebResponse : TWebResponse; inline;
   end;

const
   cWebRequestAuthenticationToString : array [TWebRequestAuthentication] of String = (
      'None', 'Failed', 'Basic', 'Digest', 'NTLM', 'Negotiate', 'Kerberos'
   );

const
   cWebRequestMethodVerbs : array [TWebRequestMethodVerb] of String = (
      '?', 'OPTIONS', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE',
      'CONNECT', 'TRACK', 'MOVE', 'COPY', 'PROPFIND', 'PROPPATCH',
      'MKCOL', 'LOCK', 'UNLOCK', 'SEARCH' );

   cHTMTL_UTF8_CONTENT_TYPE = 'text/html; charset=utf-8';

implementation

// ------------------
// ------------------ TWebEnvironmentHelper ------------------
// ------------------

// WebEnvironment
//
function TWebEnvironmentHelper.WebEnvironment : IWebEnvironment;
begin
   Result:=(Execution.Environment as IWebEnvironment);
end;

// WebRequest
//
function TWebEnvironmentHelper.WebRequest : TWebRequest;
begin
   Result:=WebEnvironment.WebRequest;
end;

// WebResponse
//
function TWebEnvironmentHelper.WebResponse : TWebResponse;
begin
   Result:=WebEnvironment.WebResponse;
end;

// ------------------
// ------------------ TWebRequest ------------------
// ------------------

// Create
//
constructor TWebRequest.Create;
begin
   inherited;
end;

// Destroy
//
destructor TWebRequest.Destroy;
begin
   FQueryFields.Free;
   FCookies.Free;
   FCustom.Free;
   inherited;
end;

// PrepareCookies
//
function TWebRequest.PrepareCookies : TStrings;
var
   base, next, p : Integer;
   cookieField : String;
begin
   Result:=TFastCompareTextList.Create;

   cookieField:=Header('Cookie');
   p:=0;
   base:=1;
   while True do begin
      p:=PosEx('=', cookieField, base);
      next:=PosEx(';', cookieField, p);
      if (p>base) and (next>p) then begin
         Result.Add(Trim(Copy(cookieField, base, p-base))
                    +'='
                    +Copy(cookieField, p+1, next-p));
         base:=next+1;
      end else Break;
   end;
   if (p>base) and (base<Length(cookieField)) then
      Result.Add(Trim(Copy(cookieField, base, p-base))
                 +'='
                 +Copy(cookieField, p+1));
end;

// PrepareQueryFields
//
function TWebRequest.PrepareQueryFields : TStrings;
var
   fields : String;
   base, next : Integer;
begin
   Result:=TStringList.Create;

   fields:=QueryString;
   base:=1;
   while True do begin
      next:=PosEx('&', fields, base);
      if next>base then begin
         Result.Add(Copy(fields, base, next-base));
         base:=next+1;
      end else begin
         if base<Length(fields) then
            Result.Add(Copy(fields, base));
         Break;
      end;
   end;
end;

// Header
//
function TWebRequest.Header(const headerName : String) : String;
begin
   Result:=Headers.Values[headerName];
end;

// GetCookies
//
function TWebRequest.GetCookies : TStrings;
begin
   if FCookies=nil then
      FCookies:=PrepareCookies;
   Result:=FCookies;
end;

// GetQueryFields
//
function TWebRequest.GetQueryFields : TStrings;
begin
   if FQueryFields=nil then
      FQueryFields:=PrepareQueryFields;
   Result:=FQueryFields;
end;

// HasQueryField
//
function TWebRequest.HasQueryField(const name : String) : Boolean;
var
   i, n : Integer;
   fields : TStrings;
   elem : String;
begin
   fields:=QueryFields;
   for i:=0 to fields.Count-1 do begin
      elem:=fields[i];
      if StrBeginsWith(elem, name) then begin
         n:=Length(elem);
         if (n=Length(name)) or (elem[n]='=') then
            Exit(True);
      end;
   end;
   Result:=False;
end;

// GetUserAgent
//
function TWebRequest.GetUserAgent : String;
begin
   Result:=Header('User-Agent');
end;

// GetAuthentication
//
function TWebRequest.GetAuthentication : TWebRequestAuthentication;
begin
   Result:=wraNone;
end;

// GetAuthenticatedUser
//
function TWebRequest.GetAuthenticatedUser : String;
begin
   Result:='';
end;

// ------------------
// ------------------ TWebEnvironment ------------------
// ------------------

// GetWebRequest
//
function TWebEnvironment.GetWebRequest : TWebRequest;
begin
   Result:=WebRequest;
end;

// GetWebResponse
//
function TWebEnvironment.GetWebResponse : TWebResponse;
begin
   Result:=WebResponse;
end;

// ------------------
// ------------------ TWebResponse ------------------
// ------------------

// Create
//
constructor TWebResponse.Create;
begin
   inherited;
   FHeaders:=TFastCompareStringList.Create;
end;

// Destroy
//
destructor TWebResponse.Destroy;
begin
   FHeaders.Free;
   inherited;
end;

// Clear
//
procedure TWebResponse.Clear;
begin
   FStatusCode:=200;
   FContentType:=cHTMTL_UTF8_CONTENT_TYPE;
   FContentData:='';
   FContentEncoding:='';
   FHeaders.Clear;
end;

// HasHeaders
//
function TWebResponse.HasHeaders : Boolean;
begin
   Result:=(FHeaders.Count>0);
end;

// CompiledHeaders
//
function TWebResponse.CompiledHeaders : RawByteString;
var
   i : Integer;
begin
   Result:='';
   for i:=0 to Headers.Count-1 do
      Result:=Result+UTF8Encode(FHeaders.Names[i]+': '+FHeaders.ValueFromIndex[i])+#13#10;
end;

// SetContentText
//
procedure TWebResponse.SetContentText(const textType : RawByteString; const text : String);
begin
   ContentType:='text/'+textType+'; charset=utf-8';
   ContentData:=UTF8Encode(text);
end;

end.
