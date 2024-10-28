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
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsPascalUtils;

{$I ../dws.inc}

interface

//: Encode a string to a static Pascal string
function StringToPascalString(const str : String; quoteChar : Char) : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// StringToPascalString
//
function StringToPascalString(const str : String; quoteChar : Char) : String;
var
   resultLength : Integer;
   iResult : Integer;
   pResult : PChar;

   procedure AppendChar(c : Char);
   begin
      pResult[iResult] := c;
      if iResult = resultLength then begin
         Inc(resultLength, resultLength shr 2);
         SetLength(Result, resultLength);
         pResult := PChar(Pointer(Result));
      end;
      Inc(iResult);
   end;

   procedure AppendControlChar(c : Integer);
   begin
      AppendChar('#');
      if c >= 10 then begin
         var n := c div 10;
         AppendChar(Char(Ord('0') + n));
         AppendChar(Char(Ord('0') + (c - 10*n)));
      end else AppendChar(Char(Ord('0') + c));
   end;

begin
   if str = '' then
      Exit(quoteChar + quoteChar);

   var srcLen := Length(str);
   resultLength := srcLen + (srcLen shr 4) + 2;
   SetLength(Result, resultLength);
   iResult := 0;
   pResult := PChar(Pointer(Result));

   var inString := False;
   var p := PChar(Pointer(str));
   for var i := 1 to srcLen do begin
      if Ord(p^) < 32 then begin
         // control character
         if inString then begin
            AppendChar(quoteChar);
            inString := False;
         end;
         AppendControlChar(Ord(p^));
      end else begin
         // normal character
         if not inString then begin
            AppendChar(quoteChar);
            inString := True;
         end;
         if p^ = quoteChar then
            AppendChar(quoteChar);
         AppendChar(p^);
      end;
      Inc(p);
   end;
   if inString then
      AppendChar(quoteChar);
   SetLength(Result, iResult);
end;

end.
