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

uses Classes, SysUtils, StrUtils, dwsUtils;

type
   {$M+}
   TWebRequest = class
      private
         FRemoteIP : String;
         FURL : String;
         FMethod : String;

         FHeaders : TStrings;
         FCookies : TStrings;
         FQueryFields : TStrings;

      protected
         function GetPathInfo : String;
         function GetQueryString : String;
         function GetCookies : TStrings;
         function GetQueryFields : TStrings;

         function PrepareCookies : TStrings; virtual;
         function PrepareQueryFields : TStrings; virtual;

      public
         constructor Create;
         destructor Destroy; override;

      published
         function Header(const headerName : String) : String;

         property RemoteIP : String read FRemoteIP write FRemoteIP;
         property URL : String read FURL write FURL;
         property Method : String read FMethod write FMethod;
         property PathInfo : String read GetPathInfo;
         property QueryString : String read GetQueryString;

         property Headers : TStrings read FHeaders;
         property Cookies : TStrings read GetCookies;
         property QueryFields : TStrings read GetQueryFields;

   end;

   TWebResponse = class
      private
         FStatusCode : Integer;
         FContentData : RawByteString;
         FContentType : RawByteString;
         FContentEncoding : RawByteString;

      public

      published
         property StatusCode : Integer read FStatusCode write FStatusCode;
         property ContentData : RawByteString read FContentData write FContentData;
         property ContentType : RawByteString read FContentType write FContentType;
         property ContentEncoding : RawByteString read FContentEncoding write FContentEncoding;
   end;

   TWebEnvironment = class
      private

      public

      published
         Response : TWebResponse;
         Request : TWebRequest;
   end;
   {$M-}

implementation

// ------------------
// ------------------ TWebRequest ------------------
// ------------------

// Create
//
constructor TWebRequest.Create;
begin
   inherited;
   FHeaders:=TFastCompareTextList.Create;
end;

// Destroy
//
destructor TWebRequest.Destroy;
begin
   FQueryFields.Free;
   FCookies.Free;
   FHeaders.Free;
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
   Result:=FHeaders.Values[headerName];
end;

// GetPathInfo
//
function TWebRequest.GetPathInfo : String;
begin
   Result:=StrBeforeChar(URL, '?');
end;

// GetQueryString
//
function TWebRequest.GetQueryString : String;
begin
   Result:=StrAfterChar(URL, '?');
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

end.
