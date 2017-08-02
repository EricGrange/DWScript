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
unit dwsSHA512;

{$I dws.inc}
{$R-}

interface

uses
  SysUtils;

type
   TSHA512Digest = array [0..(512 div 8)-1] of Byte;

   TSHA512State = record
      private
         FBufferIndex : Integer;
         FMessageLength : UInt64;
         FHash : array [0..7] of UInt64;
         FBuffer : array [0..127] of Byte;

         procedure Compress;

      public
         procedure Init;
         procedure Update(data : Pointer; size : Integer);
         procedure FinalHash(var digest : TSHA512Digest);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cK : array [0..79] of UInt64 = (
      $428a2f98d728ae22, $7137449123ef65cd, $b5c0fbcfec4d3b2f, $e9b5dba58189dbbc, $3956c25bf348b538,
      $59f111f1b605d019, $923f82a4af194f9b, $ab1c5ed5da6d8118, $d807aa98a3030242, $12835b0145706fbe,
      $243185be4ee4b28c, $550c7dc3d5ffb4e2, $72be5d74f27b896f, $80deb1fe3b1696b1, $9bdc06a725c71235,
      $c19bf174cf692694, $e49b69c19ef14ad2, $efbe4786384f25e3, $0fc19dc68b8cd5b5, $240ca1cc77ac9c65,
      $2de92c6f592b0275, $4a7484aa6ea6e483, $5cb0a9dcbd41fbd4, $76f988da831153b5, $983e5152ee66dfab,
      $a831c66d2db43210, $b00327c898fb213f, $bf597fc7beef0ee4, $c6e00bf33da88fc2, $d5a79147930aa725,
      $06ca6351e003826f, $142929670a0e6e70, $27b70a8546d22ffc, $2e1b21385c26c926, $4d2c6dfc5ac42aed,
      $53380d139d95b3df, $650a73548baf63de, $766a0abb3c77b2a8, $81c2c92e47edaee6, $92722c851482353b,
      $a2bfe8a14cf10364, $a81a664bbc423001, $c24b8b70d0f89791, $c76c51a30654be30, $d192e819d6ef5218,
      $d69906245565a910, $f40e35855771202a, $106aa07032bbd1b8, $19a4c116b8d2d0c8, $1e376c085141ab53,
      $2748774cdf8eeb99, $34b0bcb5e19b48a8, $391c0cb3c5c95a63, $4ed8aa4ae3418acb, $5b9cca4f7763e373,
      $682e6ff3d6b2b8a3, $748f82ee5defb2fc, $78a5636f43172f60, $84c87814a1f0ab72, $8cc702081a6439ec,
      $90befffa23631e28, $a4506cebde82bde9, $bef9a3f7b2c67915, $c67178f2e372532b, $ca273eceea26619c,
      $d186b8c721c0c207, $eada7dd6cde0eb1e, $f57d4f7fee6ed178, $06f067aa72176fba, $0a637dc5a2c898a6,
      $113f9804bef90dae, $1b710b35131c471b, $28db77f523047d84, $32caab7b40c72493, $3c9ebe0a15c9bebc,
      $431d67c49c100d4c, $4cc5d4becb3e42b6, $597f299cfc657e2a, $5fcb6fab3ad6faec, $6c44198c4a475817
      );

procedure BSwap64(const a : PUInt64; var result : UInt64);
{$ifndef WIN32_ASM}
{$ifndef CPUX64}
   function BSwap32(a : Cardinal) : Cardinal; inline;
   begin
      Result :=   (a shl 24)
               or ((a and $FF00) shl 8)
               or ((a and $FF0000) shr 8)
               or (a shr 24);
   end;

begin
   Int64Rec(result).Lo := BSwap32(Int64Rec(a^).Hi);
   Int64Rec(result).Hi := BSwap32(Int64Rec(a^).Lo);
{$else CPUX64}
begin
   result :=   (a^ shl 56)
            or ((a^ and $FF00) shl 40)
            or ((a^ and $FF0000) shl 24)
            or ((a^ and $FF000000) shl 8)
            or ((a^ and $FF00000000) shr 8)
            or ((a^ and $FF0000000000) shr 24)
            or ((a^ and $FF000000000000) shr 40)
            or (a^ shr 56)
{$endif CPUX64}
{$else WIN32_ASM}
asm
   mov     ecx, a.Int64Rec.Lo
   bswap   ecx
   mov     result.Int64Rec.Hi, ecx
   mov     ecx, a.Int64Rec.Hi
   bswap   ecx
   mov     result.Int64Rec.Lo, ecx
{$endif WIN32_ASM}
end;

procedure TSHA512State.Compress;
var
   a, b, c, d, e, f, g, h : UInt64;
   temp1, temp2 : UInt64;
   i : Integer;
   w : array [0..79] of UInt64;
begin
   a := FHash[0];
   b := FHash[1];
   c := FHash[2];
   d := FHash[3];
   e := FHash[4];
   f := FHash[5];
   g := FHash[6];
   h := FHash[7];

   for i := 0 to 15 do
      BSwap64(PUInt64(@FBuffer[i*8]), w[i]);

   for i := 16 to 79 do begin
      w[i] := (((w[i-2] shr 19) or (w[i-2] shl 45)) xor ((w[i-2] shr 61) or (w[i-2] shl 3)) xor (w[i-2] shr 6))
            + w[i-7]
            + (((w[i-15] shr 1) or (w[i-15] shl 63)) xor ((w[i-15] shr 8) or (w[i-15] shl 56)) xor (w[i-15] shr 7))
            + w[i-16];
   end;

   for i := 0 to 79 do begin
      temp1 :=    h
               + (((e shr 14) or (e shl 50)) xor ((e shr 18) or (e shl 46)) xor ((e shr 41) or (e shl 23)))
               + ((e and f) xor (not e and g))
               + cK[i]
               + w[i];
      temp2 :=   (((a shr 28) or (a shl 36)) xor ((a shr 34) or (a shl 30)) xor ((a shr 39) or (a shl 25)))
               + ((a and b) xor (a and c) xor (b and c));
      h := g;
      g := f;
      f := e;
      e := d + temp1;
      d := c;
      c := b;
      b := a;
      a := temp1 + temp2;
   end;

   Inc(FHash[0], a);
   Inc(FHash[1], b);
   Inc(FHash[2], c);
   Inc(FHash[3], d);
   Inc(FHash[4], e);
   Inc(FHash[5], f);
   Inc(FHash[6], g);
   Inc(FHash[7], h);

   FBufferIndex := 0;
   FillChar(FBuffer, SizeOf(FBuffer), 0);
end;

procedure TSHA512State.Init;
begin
   FMessageLength := 0;
   FBufferIndex := 0;
   FillChar(FBuffer, SizeOf(FBuffer), 0);

   FHash[0] := $6a09e667f3bcc908;
   FHash[1] := $bb67ae8584caa73b;
   FHash[2] := $3c6ef372fe94f82b;
   FHash[3] := $a54ff53a5f1d36f1;
   FHash[4] := $510e527fade682d1;
   FHash[5] := $9b05688c2b3e6c1f;
   FHash[6] := $1f83d9abfb41bd6b;
   FHash[7] := $5be0cd19137e2179;
end;

procedure TSHA512State.Update(data : Pointer; size : Integer);
var
   n : Integer;
begin
   if size <= 0 then Exit;

   Inc(FMessageLength, size);

   repeat
      if SizeOf(FBuffer)-FBufferIndex <= size then begin
         n := SizeOf(FBuffer)-FBufferIndex;
         Move(data^, FBuffer[FBufferIndex], n);
         Dec(size, n);
         Inc(PByte(data), n);
         Compress;
      end else begin
         Move(data^, FBuffer[FBufferIndex], size);
         Inc(FBufferIndex, size);
         Break;
      end;
   until size <= 0;
end;

procedure TSHA512State.FinalHash(var digest : TSHA512Digest);
var
   i : Integer;
   buf : UInt64;
begin
   FBuffer[FBufferIndex] := $80;
   if FBufferIndex >= 112 then
      Compress;

   buf := FMessageLength shr 61;
   BSwap64(@buf, PUInt64(@FBuffer[112])^);
   buf := FMessageLength shl 3;
   BSwap64(@buf, PUInt64(@FBuffer[120])^);
   Compress;

   for i := 0 to 7 do
      BSwap64(@FHash[i], PUInt64(@digest[i*8])^);
end;

end.
