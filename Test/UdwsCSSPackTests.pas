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
unit UdwsCSSPackTests;

interface

uses Classes, SysUtils, Math, dwsXPlatformTests, dwsCSSPack;

type

   TdwsCssPackTests = class (TTestCase)
      private

      protected

      published
         procedure MediaQueryTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsCssPackTests ------------------
// ------------------

// MediaQueryTest
//
procedure TdwsCssPackTests.MediaQueryTest;
begin
   CheckEquals(
      '@media only screen and (max-width:600px){body{background-color:lightblue}}',
      TCSSPack.Compress(
         '@media only screen and (max-width: 600px) {'#13#10
         + #9'body {'#13#10
         +  #9#9'background-color: lightblue;'#13#10
         + #9'}'#13#10
         + '}'
      )
   );
end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('UtilsTests', TdwsCssPackTests);

end.
