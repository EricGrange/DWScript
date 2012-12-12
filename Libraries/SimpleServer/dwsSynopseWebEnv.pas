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
unit dwsSynopseWebEnv;

interface

uses Classes, SynCommons, SysUtils, dwsWebEnvironment, dwsUtils, StrUtils;

type

   TSynopseWebRequest = class (TWebRequest)
      private
         FInContent : RawByteString;
         FInContentType : RawByteString;

      protected
         procedure SetInURL(const val : RawByteString);
         procedure SetInMethod(const val : RawByteString);
         procedure SetInHeaders(const val : RawByteString);

      public
         property InURL : RawByteString write SetInURL;
         property InMethod : RawByteString write SetInMethod;
         property InHeaders : RawByteString write SetInHeaders;
         property InContent : RawByteString read FInContent write FInContent;
         property InContentType : RawByteString read FInContentType write FInContentType;
   end;

   TSynopseWebResponse = class (TWebResponse)
      private

      public

      published

   end;

implementation

// ------------------
// ------------------ TSynopseWebRequest ------------------
// ------------------

// SetInURL
//
procedure TSynopseWebRequest.SetInURL(const val : RawByteString);
begin
   URL:=UTF8ToString(UrlDecode(val));
end;

// SetInMethod
//
procedure TSynopseWebRequest.SetInMethod(const val : RawByteString);
begin
   Method:=RawByteStringToScriptString(val);
end;

// SetInHeaders
//
procedure TSynopseWebRequest.SetInHeaders(const val : RawByteString);
var
   i, n, p : Integer;
   buf : String;
begin
   n:=1;
   for i:=1 to Length(val) do begin
      if val[i]=#13 then begin
         buf:=UTF8ToString(Copy(val, n, i-n));
         n:=i+2;
         p:=Pos(': ', buf);
         if p>0 then
            Headers.Values[Copy(buf, 1, p-1)]:=Copy(buf, p+2);
      end;
   end;
   RemoteIP:=Headers.Values['RemoteIP'];
end;

end.
