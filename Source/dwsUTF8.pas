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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SynCommons;

{$R-}

// StringToUTF8
//
function StringToUTF8(const unicodeString : String) : RawByteString;
type
   TBytes4 = array [0..3] of Byte;
   PBytes4 = ^TBytes4;
begin
   if unicodeString = '' then Exit('');

   var srcLen := Length(unicodeString);
   SetLength(Result, srcLen * 3);

   var pSrc := PWord(unicodeString);
   var pDest := PByte(Result);

   repeat

      // quick scan over two ASCII chars at once
      while (srcLen >= 2) and ((PCardinal(pSrc)^ and $ff80ff80) = 0) do begin
         var pair := PCardinal(pSrc)^;
         PWord(pDest)^ := Word((pair shr 8) or pair);
         Inc(pSrc, 2);
         Inc(pDest, 2);
         Dec(srcLen, 2);
      end;

      if srcLen = 0 then Break;

      // isolated ASCII char ?
      if pSrc^ < $7f then begin
         pDest^ := pSrc^;
         Inc(pSrc);
         Inc(pDest);
         Dec(srcLen);
         if srcLen = 0 then Break;
      end;

      // decode to ucs4
      var ucs4 : Cardinal;
      if (pSrc^ >= $d800) and (pSrc^ < $e000) then begin

         // surrogate pair
         if srcLen = 1 then begin
            // error, use replacement character
            pDest^ := $ef; Inc(pDest);
            pDest^ := $bf; Inc(pDest);
            pDest^ := $bd; Inc(pDest);
            Break;
         end;
         ucs4 := (pSrc^ - $d800) * $400;
         Inc(pSrc);
         ucs4 := ucs4 + (pSrc^ - $dc00) + $10000;
         Inc(pSrc);
         Dec(srcLen, 2);

      end else begin

         ucs4 := pSrc^;
         Inc(pSrc);
         Dec(srcLen);

      end;

      // output ucs4 as utf-8
      case ucs4 of
         0..$7ff: begin
            pDest^ := %1100_0000 or (ucs4 shr 6); Inc(pDest);
            pDest^ := %1000_0000 or (ucs4 and %11_1111); Inc(pDest);
         end;
         $800..$ffff : begin
            pDest^ := %1110_0000 or (ucs4 shr 12); Inc(pDest);
            pDest^ := %1000_0000 or ((ucs4 shr 6) and %11_1111); Inc(pDest);
            pDest^ := %1000_0000 or (ucs4 and %11_1111); Inc(pDest);
         end;
         $10000..$10FFFF: begin
            pDest^ := %1111_0000 or (ucs4 shr 18); Inc(pDest);
            pDest^ := %1000_0000 or ((ucs4 shr 12) and %11_1111); Inc(pDest);
            pDest^ := %1000_0000 or ((ucs4 shr 6) and %11_1111); Inc(pDest);
            pDest^ := %1000_0000 or (ucs4 and %11_1111); Inc(pDest);
         end;
      else
         // error since RFC 3629, use replacement character
         pDest^ := $ef; Inc(pDest);
         pDest^ := $bf; Inc(pDest);
         pDest^ := $bd; Inc(pDest);
      end;

   until srcLen <= 0;

   SetLength(Result, UIntPtr(pDest) - UIntPtr(Pointer(Result)));
end;

// UTF8ToString
//
function UTF8ToString(const utf8String : RawByteString) : String;
begin
   Result := SynCommons.UTF8ToString(utf8String);
end;

end.
