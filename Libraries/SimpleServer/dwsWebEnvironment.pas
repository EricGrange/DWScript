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
   TWebRequest = class
      private
         FRemoteIP : String;
         FURL : String;
         FMethod : String;
         FSecurity : String;

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

         function Header(const headerName : String) : String;

         property RemoteIP : String read FRemoteIP write FRemoteIP;
         property URL : String read FURL write FURL;
         property Method : String read FMethod write FMethod;
         property Security : String read FSecurity write FSecurity;
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
         FHeaders : TStrings;

      protected
         function GetHeaders : TStrings;

         procedure SetContentText(const textType : RawByteString; const text : String);

      public
         destructor Destroy; override;

         function HasHeaders : Boolean; inline;
         function CompiledHeaders : RawByteString;

         property StatusCode : Integer read FStatusCode write FStatusCode;
         property ContentText[const textType : RawByteString] : String write SetContentText;
         property ContentData : RawByteString read FContentData write FContentData;
         property ContentType : RawByteString read FContentType write FContentType;
         property ContentEncoding : RawByteString read FContentEncoding write FContentEncoding;

         property Headers : TStrings read GetHeaders;
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
   FHeaders:=TFastCompareStringList.Create;
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

// Destroy
//
destructor TWebResponse.Destroy;
begin
   FHeaders.Free;
   inherited;
end;

// HasHeaders
//
function TWebResponse.HasHeaders : Boolean;
begin
   Result:=(FHeaders<>nil);
end;

// CompiledHeaders
//
function TWebResponse.CompiledHeaders : RawByteString;
var
   i : Integer;
begin
   Result:='';
   if FHeaders=nil then Exit;
   for i:=0 to Headers.Count-1 do
      Result:=Result+UTF8Encode(FHeaders.Names[i]+': '+FHeaders.ValueFromIndex[i])+#13#10;
end;

// GetHeaders
//
function TWebResponse.GetHeaders : TStrings;
begin
   if FHeaders=nil then
      FHeaders:=TFastCompareStringList.Create;
   Result:=FHeaders;
end;

// SetContentText
//
procedure TWebResponse.SetContentText(const textType : RawByteString; const text : String);
begin
   ContentType:='text/'+textType+'; charset=utf-8';
   ContentData:=UTF8Encode(text);
end;

end.
