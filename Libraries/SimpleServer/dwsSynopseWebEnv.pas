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

Currently in stasis and not working
Replaced by dwsHTTPSysWebEnv

unit dwsSynopseWebEnv;

interface

uses Classes, SynCommons, SysUtils, dwsWebEnvironment, dwsUtils, StrUtils;

type

   TSynopseWebRequest = class (TWebRequest)
      private
         FInContent : RawByteString;
         FInContentType : RawByteString;
         FAuthentication : TWebRequestAuthentication;
         FAuthenticatedUser : String;

      protected
         procedure SetInURL(const val : RawByteString);
         procedure SetInMethod(const val : RawByteString);
         procedure SetInHeaders(const val : RawByteString);

         function GetAuthentication : TWebRequestAuthentication; override;
         function GetAuthenticatedUser : String; override;

      public
         function RemoteIP : String; override;

         property InURL : RawByteString write SetInURL;
         property InMethod : RawByteString write SetInMethod;
         property InHeaders : RawByteString write SetInHeaders;
         property InContent : RawByteString read FInContent write FInContent;
         property InContentType : RawByteString read FInContentType write FInContentType;
         property Authentication : TWebRequestAuthentication read FAuthentication write FAuthentication;
         property AuthenticatedUser : String read FAuthenticatedUser write FAuthenticatedUser;
   end;

   TSynopseWebResponse = class (TWebResponse)
      private

      public

   end;

implementation

// ------------------
// ------------------ TSynopseWebRequest ------------------
// ------------------

// SetInHeaders
//
procedure TSynopseWebRequest.SetInHeaders(const val : RawByteString);
var
   i, n : Integer;
   bufUTF8 : RawByteString;
   pColon : Integer;
begin
   pColon:=0;
   n:=1;
   for i:=1 to Length(val) do begin
      case val[i] of
         ':' : if pColon=0 then pColon:=i;
         #13 : begin
            if (pColon>0) and (val[pColon+1]=' ') and (i>n+1) then begin
               SetLength(bufUTF8, i-n-1);
               Move(val[n], bufUTF8[1], pColon-n);
               bufUTF8[pColon-n+1]:='=';
               Move(val[pColon+2], bufUTF8[pColon-n+2], i-pColon-2);
               Headers.Add(UTF8ToString(bufUTF8));
            end;
            n:=i+2;
            pColon:=0;
         end;
      end;
   end;
end;

// GetAuthentication
//
function TSynopseWebRequest.GetAuthentication : TWebRequestAuthentication;
begin
   Result:=FAuthentication;
end;

// GetAuthenticatedUser
//
function TSynopseWebRequest.GetAuthenticatedUser : String;
begin
   Result:=FAuthenticatedUser;
end;

// RemoteIP
//
function TSynopseWebRequest.RemoteIP : String;
begin
   Result:='';
end;

end.
