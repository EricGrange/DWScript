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
unit dwsWebServerUtils;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsXPlatform,
   SynCommons, SynCrypto;

type

   WebServerUtils = class
      public
         class function ETag(const data : array of const) : String; static;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ETag
//
class function WebServerUtils.ETag(const data : array of const) : String;
var
   i : Integer;
   hash : TSHA256;
   digest : TSHA256Digest;
begin
   hash.Init;
   for i:=0 to High(data) do begin
      case data[i].VType of
         vtInteger       : hash.Update(@data[i].VInteger, SizeOf(Integer));
         vtInt64         : hash.Update(@data[i].VInt64, SizeOf(Int64));
         vtBoolean       : hash.Update(@data[i].VBoolean, SizeOf(Boolean));
         vtChar          : hash.Update(@data[i].VChar, SizeOf(AnsiChar));
         vtExtended      : hash.Update(@data[i].VExtended, SizeOf(Extended));
         vtAnsiString    : hash.Update(data[i].VAnsiString, Length(AnsiString(data[i].VAnsiString)));
         vtUnicodeString : hash.Update(data[i].VUnicodeString, 2*Length(UnicodeString(data[i].VUnicodeString)));
      else
         raise Exception.CreateFmt('Unsupported VType %d in WebUtils.ETag', [data[i].VType]);
      end;
   end;
   hash.Final(digest);
   Result:='"'+RawByteStringToScriptString(BinToBase64URI(@digest, SizeOf(digest) div 2))+'"';
end;

end.
