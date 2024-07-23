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
unit dwsUTF8;

{$I dws.inc}

interface

// This unit holds low-level UTF8 implementations or redirections to implementations
// by other frameworks, you should not be using it directly

function StringToUTF8(const unicodeString : String) : RawByteString;
function UTF8ToString(const utf8String : RawByteString) : String;

implementation

uses SynCommons;

// StringToUTF8
//
function StringToUTF8(const unicodeString : String) : RawByteString;
begin
   Result := SynCommons.StringToUTF8(unicodeString);
end;

// UTF8ToString
//
function UTF8ToString(const utf8String : RawByteString) : String;
begin
   Result := SynCommons.UTF8ToString(utf8String);
end;

end.
