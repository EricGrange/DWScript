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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit UdwsWebUtilsTests;

interface

uses Classes, SysUtils, Math, dwsXPlatformTests, dwsWebUtils;

type

   TdwsWebUtilsTests = class (TTestCase)
      private

      protected

      published
         procedure ParseURLEncodedTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsWebUtilsTests ------------------
// ------------------

// ParseURLEncodedTest
//
procedure TdwsWebUtilsTests.ParseURLEncodedTest;
var
   decoded : TStringList;
begin
   decoded:=TStringList.Create;
   try

      WebUtils.ParseURLEncoded('hello=world', decoded);
      CheckEquals('hello=world', decoded.CommaText);

      WebUtils.ParseURLEncoded('hell%64=+world', decoded);
      CheckEquals('hello=world,"helld= world"', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseURLEncoded('a=b&c=d', decoded);
      CheckEquals('a=b,c=d', decoded.CommaText);

      decoded.Clear;
      WebUtils.ParseURLEncoded('a=b;c=d', decoded);
      CheckEquals('a=b,c=d', decoded.CommaText);

   finally
      decoded.Free;
   end;
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('UtilsTests', TdwsWebUtilsTests);

end.
