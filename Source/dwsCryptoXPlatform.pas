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
unit dwsCryptoXPlatform;

{$I dws.inc}

//
// This unit should concentrate all cryptographic cross-platform aspects,
// cross-Delphi versions, ifdefs and other conditionals
//
// no ifdefs in the main code.

{$WARN SYMBOL_PLATFORM OFF}

interface

function CryptographicRandom(nb : Integer) : RawByteString;
function CryptographicToken(bitStrength : Integer) : String;
function ProcessUniqueRandom : String;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   dwsXPlatform,
   Windows, wcrypt2;

var
   vProcessUniqueRandom : String;

procedure GenerateUniqueRandom;
var
   buf : String;
begin
   // 6 bits per character, 42 characters, 252 bits of random
   buf:=CryptographicToken(6*42);
   Pointer(buf):=InterlockedCompareExchangePointer(Pointer(vProcessUniqueRandom),
                                                   Pointer(buf), nil);
end;

function ProcessUniqueRandom : String;
begin
   if vProcessUniqueRandom='' then
      GenerateUniqueRandom;
   Result:=vProcessUniqueRandom;
end;

var
   hProv : THandle;
   hProvLock : TMultiReadSingleWrite;
   vXorShiftSeedMask : UInt64;

function CryptographicRandom(nb : Integer) : RawByteString;

   function RDTSC : UInt64;
   asm
      RDTSC;
   end;

   function XorShift(var seed : UInt64) : Cardinal; inline;
   var
      buf : UInt64;
   begin
      buf:=seed xor (seed shl 13);
      buf:=buf xor (buf shr 17);
      buf:=buf xor (buf shl 5);
      seed:=buf;
      Result:=seed and $FFFFFFFF;
   end;

var
   i : Integer;
   seed : UInt64;
   p : PCardinal;
begin
   if nb<=0 then Exit('');

   SetLength(Result, nb);

   hProvLock.BeginWrite;
   try
      if hProv=0 then begin
         if not CryptAcquireContext(@hProv, nil, MS_ENHANCED_PROV, PROV_RSA_FULL,
                                    CRYPT_VERIFYCONTEXT) then begin
            CryptAcquireContext(@hProv, nil, MS_ENHANCED_PROV, PROV_RSA_FULL,
                                CRYPT_NEWKEYSET + CRYPT_VERIFYCONTEXT);
         end;
         CryptGenRandom(hProv, SizeOf(vXorShiftSeedMask), @vXorShiftSeedMask);
      end;
      CryptGenRandom(hProv, nb, Pointer(Result));
   finally
      hProvLock.EndWrite;
   end;

   FillChar(Result[1], nb, 0);

   // further muddy things, in case Windows generator is later found vulnerable,
   // this will protect us from "generic" exploits
   seed:=RDTSC xor vXorShiftSeedMask;
   p:=PCardinal(Result);
   for i:=0 to (nb div 4)-1 do begin
      p^:=p^ xor XorShift(seed);
      Inc(p);
   end;
end;

function CryptographicToken(bitStrength : Integer) : String;
const
   // uri-safe base64 table (RFC 4648)
   cChars : AnsiString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
var
   i, n : Integer;
   rand : RawByteString;
begin
   if bitStrength<=0 then
      bitStrength:=120;
   // 6 bits per character
   n:=bitStrength div 6;
   if n*6<bitStrength then
      Inc(n);
   rand:=CryptographicRandom(n);
   SetLength(Result, n);
   for i:=1 to n do
      Result[i]:=Char(cChars[(Ord(rand[i]) and 63)+1]);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   hProvLock := TMultiReadSingleWrite.Create;

finalization

   hProvLock.Free;
   hProvLock:=nil;
   if hProv>0 then
      CryptReleaseContext(hProv, 0);

end.
