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


uses
   Classes, SysUtils, StrUtils, DateUtils,
   SynCrtSock, SynCommons,
   dwsExprs, dwsUtils, dwsWebUtils, dwsWebServerUtils, dwsWebServerHelpers,
   dwsSymbols, dwsExprList;

type
   TWebRequestAuthentication = (
      wraNone,
      wraFailed,
      wraBasic,
      wraDigest,
      wraNTLM,
      wraNegotiate,
      wraKerberos,
      wraAuthorization
   );
   TWebRequestAuthentications = set of TWebRequestAuthentication;

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
   TWebRequestMethodVerbs = set of TWebRequestMethodVerb;

   TWebServerEventData = array of RawByteString;

   TWebRequest = class
      private
         FCookies : TStrings;
         FQueryFields : TStrings;
         FContentFields : TStrings;
         FCustom : TObject;
         FID : Int64;
         FAppUser : String;

      protected
         FPathInfo : String;
         FQueryString : String;

         function GetHeaders : TStrings; virtual; abstract;
         function GetCookies : TStrings;
         function GetQueryFields : TStrings;
         function GetContentFields : TStrings;

         function GetUserAgent : String;

         function GetAuthentication : TWebRequestAuthentication; virtual;
         function GetAuthenticatedUser : String; virtual;

         function PrepareCookies : TStrings; virtual;
         function PrepareQueryFields : TStrings; virtual;
         function PrepareContentFields : TStrings; virtual;

      public
         destructor Destroy; override;

         procedure ResetCookies; inline;
         procedure ResetQueryFields; inline;
         procedure ResetContentFields; inline;
         procedure ResetFields; inline;

         function Header(const headerName : String) : String;

         function RemoteIP : String; virtual; abstract;

         function RawURL : String; virtual; abstract;
         function URL : String; virtual; abstract;
         function FullURL : String; virtual; abstract;
         function Method : String; virtual; abstract;
         function MethodVerb : TWebRequestMethodVerb; virtual; abstract;
         function Security : String; virtual; abstract;
         function Secure : Boolean; virtual; abstract;
         function Host : String; virtual;

         function ContentLength : Integer; virtual; abstract;
         function ContentData : RawByteString; virtual; abstract;
         function ContentType : RawByteString; virtual; abstract;

         property PathInfo : String read FPathInfo write FPathInfo;
         property QueryString : String read FQueryString write FQueryString;
         property UserAgent : String read GetUserAgent;

         property Headers : TStrings read GetHeaders;
         property Cookies : TStrings read GetCookies;
         property QueryFields : TStrings read GetQueryFields;
         property ContentFields : TStrings read GetContentFields;

         function HasQueryField(const name : String) : Boolean;
         function HasContentField(const name : String) : Boolean;

         function IfModifiedSince : TDateTime;
         function IfNoneMatch : String;

         property Authentication : TWebRequestAuthentication read GetAuthentication;
         property AuthenticatedUser : String read GetAuthenticatedUser;

         // custom request ID
         property ID : Int64 read FID write FID;
         // custom Application User
         property AppUser : String read FAppUser write FAppUser;
         // custom object field, freed with the request
         property Custom : TObject read FCustom write FCustom;
   end;

   TWebResponseCookieFlag = (wrcfSecure = 1, wrcfHttpOnly = 2);
   TWebResponseCookieSameSite = (wrcssUnspecified = 0, wrcssStrict = 1, wrcssLax = 2);

   TWebResponseCookie = class (TRefCountedObject)
      public
         Name : String;
         Value : String;
         ExpiresGMT : TDateTime;
         Domain : String;
         Path : String;
         MaxAge : Integer;
         Flags : Integer;
         SameSite : TWebResponseCookieSameSite;

         procedure WriteStringLn(dest : TWriteOnlyBlockStream);
   end;

   TWebResponseCookies = class (TObjectList<TWebResponseCookie>)
      public
         function AddCookie(const name : String) : TWebResponseCookie;
   end;

   TWebResponseHint = (shStatic, shCompression);
   TWebResponseHints = set of TWebResponseHint;

   TWebResponse = class
      private
         FStatusCode : Integer;
         FContentData : RawByteString;
         FContentType : RawByteString;
         FContentEncoding : RawByteString;
         FHeaders : TStrings;
         FCookies : TWebResponseCookies;  // lazy initialization
         FProcessingTime : Integer;
         FHints : TWebResponseHints;

      protected
         procedure SetContentText(const textType : RawByteString; const text : String);
         procedure SetContentJSON(const json : String);
         procedure SetLastModified(v : TDateTime);
         procedure SetETag(const v : String);
         procedure SetCacheControl(const v : String);
         function  GetCookies : TWebResponseCookies;
         function  GetCompression : Boolean; inline;
         procedure SetCompression(v : Boolean);

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear; virtual;

         function HasHeaders : Boolean; inline;
         function HasCookies : Boolean; inline;
         function CompiledHeaders : RawByteString;

         property StatusCode : Integer read FStatusCode write FStatusCode;
         property ContentText[const textType : RawByteString] : String write SetContentText;
         property ContentJSON : String write SetContentJSON;
         property ContentData : RawByteString read FContentData write FContentData;
         property ContentType : RawByteString read FContentType write FContentType;
         property ContentEncoding : RawByteString read FContentEncoding write FContentEncoding;

         property Headers : TStrings read FHeaders;
         property Cookies : TWebResponseCookies read GetCookies;
         property Compression : Boolean read GetCompression write SetCompression;
         property LastModified : TDateTime write SetLastModified;
         property ETag : String write SetETag;
         property CacheControl : String write SetCacheControl;
         property Hints : TWebResponseHints read FHints write FHints;

         // optional, informative, time it took to process the response in microseconds
         property ProcessingTime : Integer read FProcessingTime write FProcessingTime;
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

   TWebEnvironmentRecordHelper = record helper for TExprBaseListExec
      function WebEnvironment : IWebEnvironment; inline;
      function WebRequest : TWebRequest; inline;
      function WebResponse : TWebResponse; inline;
   end;

   TWebStaticCacheEntry = class (TInterfacedSelfObject)
      private
         FStatusCode : Integer;
         FContentType : RawByteString;
         FContentData : RawByteString;
         FETag : String;
         FCacheControl : String;

      public
         constructor Create(fromResponse : TWebResponse);
         procedure HandleStatic(request : TWebRequest; response : TWebResponse);

         property StatusCode : Integer read FStatusCode write FStatusCode;

         property ContentData : RawByteString read FContentData write FContentData;
         property ContentType : RawByteString read FContentType write FContentType;

         property ETag : String read FETag write FETag;
         property CacheControl : String read FCacheControl write FCacheControl;

         function Size : Integer;
   end;

   TEmptyWebRequest = class(TWebRequest)
      protected
         FMethod : String;
         FHeaders : TStringList;

         function GetHeaders : TStrings; override;

      public
         destructor Destroy; override;

         function RemoteIP : String; override;

         function RawURL : String; override;
         function URL : String; override;
         function FullURL : String; override;
         function Method : String; override;
         function MethodVerb : TWebRequestMethodVerb; override;
         function Security : String; override;
         function Secure : Boolean; override;
         function Host : String; override;

         function ContentLength : Integer; override;
         function ContentData : RawByteString; override;
         function ContentType : RawByteString; override;

         property DirectMethod : String read FMethod write FMethod;
   end;

const
   cWebRequestAuthenticationToString : array [TWebRequestAuthentication] of String = (
      'None', 'Failed', 'Basic', 'Digest', 'NTLM', 'Negotiate', 'Kerberos', 'Header'
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
   Result := (Execution.Environment as IWebEnvironment);
end;

// WebRequest
//
function TWebEnvironmentHelper.WebRequest : TWebRequest;
begin
   Result := (Execution.Environment as IWebEnvironment).WebRequest;
end;

// WebResponse
//
function TWebEnvironmentHelper.WebResponse : TWebResponse;
begin
   Result := (Execution.Environment as IWebEnvironment).WebResponse;
end;

// ------------------
// ------------------ TWebEnvironmentRecordHelper ------------------
// ------------------

// WebEnvironment
//
function TWebEnvironmentRecordHelper.WebEnvironment : IWebEnvironment;
begin
   Result := (Exec.Environment as IWebEnvironment);
end;

// WebRequest
//
function TWebEnvironmentRecordHelper.WebRequest : TWebRequest;
begin
   Result := (Exec.Environment as IWebEnvironment).WebRequest;
end;

// WebResponse
//
function TWebEnvironmentRecordHelper.WebResponse : TWebResponse;
begin
   Result := (Exec.Environment as IWebEnvironment).WebResponse;
end;

// ------------------
// ------------------ TWebRequest ------------------
// ------------------

// Destroy
//
destructor TWebRequest.Destroy;
begin
   FQueryFields.Free;
   FContentFields.Free;
   FCookies.Free;
   FCustom.Free;
   inherited;
end;

// ResetCookies
//
procedure TWebRequest.ResetCookies;
begin
   if FCookies<>nil then begin
      FCookies.Free;
      FCookies:=nil;
   end;
end;

// ResetQueryFields
//
procedure TWebRequest.ResetQueryFields;
begin
   if FQueryFields<>nil then begin
      FQueryFields.Free;
      FQueryFields:=nil;
   end;
end;

// ResetContentFields
//
procedure TWebRequest.ResetContentFields;
begin
   if FContentFields<>nil then begin
      FContentFields.Free;
      FContentFields:=nil;
   end;
end;

// ResetFields
//
procedure TWebRequest.ResetFields;
begin
   ResetQueryFields;
   ResetContentFields;
end;

// PrepareCookies
//
function TWebRequest.PrepareCookies : TStrings;

   procedure AddCookie(const name, value : String);
   var
      i : Integer;
   begin
      i := Result.IndexOfName(name);
      if i >= 0 then begin
         if value <> '' then
            Result[i] := name + '=' + value;
      end else begin
         Result.Add(name + '=' + value);
      end;
   end;

var
   base, next, p : Integer;
   cookieField : String;
begin
   Result:=TFastCompareTextList.Create;

   cookieField:=Header('Cookie');
   base:=1;
   while True do begin
      p:=StrUtils.PosEx('=', cookieField, base);
      next:=StrUtils.PosEx(';', cookieField, p);
      if (p>base) and (next>p) then begin
         AddCookie(SysUtils.Trim(Copy(cookieField, base, p-base)), Copy(cookieField, p+1, pred(next-p)));
         base:=next+1;
      end else Break;
   end;
   if (p>base) and (base<Length(cookieField)) then
      AddCookie(SysUtils.Trim(Copy(cookieField, base, p-base)), Copy(cookieField, p+1));
end;

// PrepareQueryFields
//
function TWebRequest.PrepareQueryFields : TStrings;
begin
   Result:=TStringList.Create;
   WebUtils.ParseURLEncoded(ScriptStringToRawByteString(QueryString), Result);
end;

// PrepareContentFields
//
function TWebRequest.PrepareContentFields : TStrings;
begin
   Result:=TStringList.Create;

   if StrBeginsWithA(ContentType, 'application/x-www-form-urlencoded') then begin
      // TODO: handle case where encoding isn't utf-8
      WebUtils.ParseURLEncoded(ContentData, Result);
   end;
end;

// Header
//
function TWebRequest.Header(const headerName : String) : String;
begin
   Result:=Headers.Values[headerName];
end;

// Host
//
function TWebRequest.Host : String;
begin
   Result:=Headers.Values['Host'];
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

// GetContentFields
//
function TWebRequest.GetContentFields : TStrings;
begin
   if FContentFields=nil then
      FContentFields:=PrepareContentFields;
   Result:=FContentFields;
end;

// HasQueryField
//
function TWebRequest.HasQueryField(const name : String) : Boolean;
begin
   Result:=WebUtils.HasFieldName(QueryFields, name);
end;

// HasContentField
//
function TWebRequest.HasContentField(const name : String) : Boolean;
begin
   Result:=WebUtils.HasFieldName(ContentFields, name);
end;

// IfModifiedSince
//
function TWebRequest.IfModifiedSince : TDateTime;
var
   v : String;
begin
   v:=Header('If-Modified-Since');
   if v<>'' then
      Result:=WebUtils.RFC822ToDateTime(v)
   else Result:=0;
end;

// IfNoneMatch
//
function TWebRequest.IfNoneMatch : String;
begin
   Result := Header('If-None-Match');
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
   FHeaders := TFastCompareStringList.Create;
   FHints := [ shCompression ];
end;

// Destroy
//
destructor TWebResponse.Destroy;
begin
   FHeaders.Free;
   FCookies.Free;
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
   if FCookies<>nil then
      FCookies.Clear;
   FHints:=[shCompression];
end;

// HasCookies
//
function TWebResponse.HasCookies : Boolean;
begin
   Result:=(FCookies<>nil) and (FCookies.Count>0);
end;

// HasHeaders
//
function TWebResponse.HasHeaders : Boolean;
begin
   Result:=(FHeaders.Count>0) or HasCookies;
end;

// CompiledHeaders
//
function TWebResponse.CompiledHeaders : RawByteString;
var
   i, p : Integer;
   wobs : TWriteOnlyBlockStream;
   buf : String;
begin
   wobs:=TWriteOnlyBlockStream.AllocFromPool;
   try
      for i:=0 to Headers.Count-1 do begin
         buf:=FHeaders[i];
         p:=Pos('=', buf);
         wobs.WriteSubString(buf, 1, p-1);
         wobs.WriteString(': ');
         wobs.WriteSubString(buf, p+1);
         wobs.WriteCRLF;
      end;
      if HasCookies then
         for i:=0 to Cookies.Count-1 do
            FCookies[i].WriteStringLn(wobs);
      Result:=wobs.ToUTF8String;
   finally
      wobs.ReturnToPool;
   end;
end;

// SetLastModified
//
procedure TWebResponse.SetLastModified(v : TDateTime);
begin
   Headers.Values['Last-Modified'] := WebUtils.DateTimeToRFC822(v);
end;

// SetETag
//
procedure TWebResponse.SetETag(const v : String);
begin
   Headers.Values['ETag'] := v;
end;

// SetCacheControl
//
procedure TWebResponse.SetCacheControl(const v : String);
begin
   Headers.Values['Cache-Control'] := v;
end;

// SetContentText
//
procedure TWebResponse.SetContentText(const textType : RawByteString; const text : String);
begin
   ContentType:='text/'+textType+'; charset=utf-8';
   ContentData:=StringToUTF8(text);
end;

// SetContentJSON
//
procedure TWebResponse.SetContentJSON(const json : String);
begin
   ContentType:='application/json';
   ContentData:=StringToUTF8(json);
end;

// GetCookies
//
function TWebResponse.GetCookies : TWebResponseCookies;
begin
   if FCookies=nil then
      FCookies:=TWebResponseCookies.Create;
   Result:=FCookies;
end;

// GetCompression
//
function TWebResponse.GetCompression : Boolean;
begin
   Result:=(shCompression in FHints);
end;

// SetCompression
//
procedure TWebResponse.SetCompression(v : Boolean);
begin
   if v then
      Include(FHints, shCompression)
   else Exclude(FHints, shCompression);
end;

// ------------------
// ------------------ TWebResponseCookies ------------------
// ------------------

// WriteStringLn
//
procedure TWebResponseCookie.WriteStringLn(dest : TWriteOnlyBlockStream);
const
   cMonths : array [1..12] of String = (
      'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' );
   cWeekDays : array [1..7] of String = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun' );
var
   y, m, d : Word;
   h, n, s, z : Word;
begin
   dest.WriteString('Set-Cookie: ');
   dest.WriteString(Name);
   dest.WriteChar('=');
   dest.WriteString(Value);

   if ExpiresGMT<>0 then begin
      dest.WriteString('; Expires=');
      if ExpiresGMT<0 then
         dest.WriteString('Sat, 01 Jan 2000 00:00:01 GMT')
      else begin
         dest.WriteString(cWeekDays[DayOfTheWeek(ExpiresGMT)]);
         DecodeDateTime(ExpiresGMT, y, m, d, h, n, s, z);
         dest.WriteString(Format(', %.02d %s %d %.02d:%.02d:%.02d GMT',
                                 [d, cMonths[m], y, h, n, s]));
      end;
   end;

   if MaxAge>0 then begin
      dest.WriteString('; Max-Age=');
      dest.WriteString(MaxAge);
   end;

   if Path<>'' then begin
      dest.WriteString('; Path=');
      dest.WriteString(Path);
   end;

   if Domain<>'' then begin
      dest.WriteString('; Domain=');
      dest.WriteString(Domain);
   end;

   if (Flags and Ord(wrcfSecure))<>0 then
      dest.WriteString('; Secure');

   if (Flags and Ord(wrcfHttpOnly))<>0 then
      dest.WriteString('; HttpOnly');

   case SameSite of
      wrcssStrict : dest.WriteString('; SameSite=Strict');
      wrcssLax : dest.WriteString('; SameSite=Lax');
   end;

   dest.WriteCRLF;
end;

// ------------------
// ------------------ TWebResponseCookie ------------------
// ------------------

// AddCookie
//
function TWebResponseCookies.AddCookie(const name : String) : TWebResponseCookie;
begin
   Result:=TWebResponseCookie.Create;
   Result.Name:=name;
   Add(Result);
end;

// ------------------
// ------------------ TWebStaticCacheEntry ------------------
// ------------------

// Create
//
constructor TWebStaticCacheEntry.Create(fromResponse : TWebResponse);
begin
   inherited Create;
   FContentType:=fromResponse.ContentType;
   FContentData:=fromResponse.ContentData;
   FStatusCode:=fromResponse.StatusCode;
   FCacheControl:=fromResponse.Headers.Values['Cache-Control'];
   FETag:=fromResponse.Headers.Values['ETag'];
   if FETag='' then begin
      FEtag:=WebServerUtils.ETag([FStatusCode, FCacheControl, FContentType, FContentData]);
      fromResponse.Headers.Values['ETag']:=FETag;
   end;
end;

// HandleStatic
//
procedure TWebStaticCacheEntry.HandleStatic(request : TWebRequest; response : TWebResponse);
begin
   if request.Header('If-None-Match') = FETag then begin
      response.StatusCode := 304;
      Exit;
   end;
   response.Headers.Values['ETag'] := FETag;
   if FCacheControl<>'' then
      response.Headers.Values['Cache-Control'] := FCacheControl;
   response.ContentType := FContentType;
   response.ContentData := FContentData;
   response.StatusCode := FStatusCode;
end;

// Size
//
function TWebStaticCacheEntry.Size : Integer;
begin
   Result:=Length(FContentData);
end;

// ------------------
// ------------------ TEmptyWebRequest ------------------
// ------------------

destructor TEmptyWebRequest.Destroy;
begin
   inherited;
   FHeaders.Free;
end;

function TEmptyWebRequest.GetHeaders : TStrings;
begin
   if FHeaders = nil then
      FHeaders := TStringList.Create;
   Result := FHeaders;
end;

function TEmptyWebRequest.RemoteIP : String;
begin
   Result := '';
end;

function TEmptyWebRequest.RawURL : String;
begin
   Result := '';
end;

function TEmptyWebRequest.URL : String;
begin
   Result := '';
end;

function TEmptyWebRequest.FullURL : String;
begin
   Result := '';
end;

function TEmptyWebRequest.Method : String;
begin
   Result := FMethod;
end;

function TEmptyWebRequest.MethodVerb : TWebRequestMethodVerb;
begin
   Result := wrmvUnknown;
end;

function TEmptyWebRequest.Security : String;
begin
   Result := '';
end;

function TEmptyWebRequest.Secure : Boolean;
begin
   Result := False;
end;

function TEmptyWebRequest.Host : String;
begin
   Result := '';
end;

function TEmptyWebRequest.ContentLength : Integer;
begin
   Result := 0;
end;

function TEmptyWebRequest.ContentData : RawByteString;
begin
   Result := '';
end;

function TEmptyWebRequest.ContentType : RawByteString;
begin
   Result := '';
end;

end.
