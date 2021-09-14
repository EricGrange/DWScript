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
unit dwsRipeMD160;

interface

uses SysUtils;

type
   TRipe160Digest = array [0..4] of LongWord;
   TRipe160Block = array [0..15] of LongWord;
   PRipe160Block = ^TRipe160Block;

procedure RipeMD160Init(var digest : TRipe160Digest);
procedure RipeMD160(var digest : TRipe160Digest; block : PRipe160Block);
procedure RipeMD160Final(var digest : TRipe160Digest; block : PRipe160Block;
                         remaining, totalSize : Integer);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R-}
{$Q-}

// RipeMD160Init
//
procedure RipeMD160Init(var digest : TRipe160Digest);
begin
   digest[0] := $67452301;
   digest[1] := $efcdab89;
   digest[2] := $98badcfe;
   digest[3] := $10325476;
   digest[4] := $c3d2e1f0;
end;

// RipeMD160
//
procedure RipeMD160(var digest : TRipe160Digest; block : PRipe160Block);
const
   RipeS1 = $5A827999;
   RipeS2 = $6ED9EBA1;
   RipeS3 = $8F1BBCDC;
   RipeS4 = $A953FD4E;
   RipeS5 = $50A28BE6;
   RipeS6 = $5C4DD124;
   RipeS7 = $6D703EF3;
   RipeS8 = $7A6D76E9;
var
   a1, b1, c1, d1, e1 : LongWord;
   a2, b2, c2, d2, e2 : LongWord;
   temp : LongWord;
begin
   a1 := digest[0];
   b1 := digest[1];
   c1 := digest[2];
   d1 := digest[3];
   e1 := digest[4];

   a2 := digest[0];
   b2 := digest[1];
   c2 := digest[2];
   d2 := digest[3];
   e2 := digest[4];

   Inc(a1, b1 xor c1 xor d1 + block[ 0]); a1 := a1 shl 11 or a1 shr 21 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 xor b1 xor c1 + block[ 1]); e1 := e1 shl 14 or e1 shr 18 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 xor a1 xor b1 + block[ 2]); d1 := d1 shl 15 or d1 shr 17 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 xor e1 xor a1 + block[ 3]); c1 := c1 shl 12 or c1 shr 20 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 xor d1 xor e1 + block[ 4]); b1 := b1 shl  5 or b1 shr 27 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 xor c1 xor d1 + block[ 5]); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 xor b1 xor c1 + block[ 6]); e1 := e1 shl  7 or e1 shr 25 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 xor a1 xor b1 + block[ 7]); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 xor e1 xor a1 + block[ 8]); c1 := c1 shl 11 or c1 shr 21 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 xor d1 xor e1 + block[ 9]); b1 := b1 shl 13 or b1 shr 19 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 xor c1 xor d1 + block[10]); a1 := a1 shl 14 or a1 shr 18 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 xor b1 xor c1 + block[11]); e1 := e1 shl 15 or e1 shr 17 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 xor a1 xor b1 + block[12]); d1 := d1 shl  6 or d1 shr 26 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 xor e1 xor a1 + block[13]); c1 := c1 shl  7 or c1 shr 25 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 xor d1 xor e1 + block[14]); b1 := b1 shl  9 or b1 shr 23 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 xor c1 xor d1 + block[15]); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;

   Inc(e1, a1 and b1 or not a1 and c1 + block[ 7] + RipeS1); e1 := e1 shl  7 or e1 shr 25 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and a1 or not e1 and b1 + block[ 4] + RipeS1); d1 := d1 shl  6 or d1 shr 26 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and e1 or not d1 and a1 + block[13] + RipeS1); c1 := c1 shl  8 or c1 shr 24 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and d1 or not c1 and e1 + block[ 1] + RipeS1); b1 := b1 shl 13 or b1 shr 19 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and c1 or not b1 and d1 + block[10] + RipeS1); a1 := a1 shl 11 or a1 shr 21 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and b1 or not a1 and c1 + block[ 6] + RipeS1); e1 := e1 shl  9 or e1 shr 23 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and a1 or not e1 and b1 + block[15] + RipeS1); d1 := d1 shl  7 or d1 shr 25 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and e1 or not d1 and a1 + block[ 3] + RipeS1); c1 := c1 shl 15 or c1 shr 17 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and d1 or not c1 and e1 + block[12] + RipeS1); b1 := b1 shl  7 or b1 shr 25 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and c1 or not b1 and d1 + block[ 0] + RipeS1); a1 := a1 shl 12 or a1 shr 20 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and b1 or not a1 and c1 + block[ 9] + RipeS1); e1 := e1 shl 15 or e1 shr 17 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and a1 or not e1 and b1 + block[ 5] + RipeS1); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and e1 or not d1 and a1 + block[ 2] + RipeS1); c1 := c1 shl 11 or c1 shr 21 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and d1 or not c1 and e1 + block[14] + RipeS1); b1 := b1 shl  7 or b1 shr 25 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and c1 or not b1 and d1 + block[11] + RipeS1); a1 := a1 shl 13 or a1 shr 19 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and b1 or not a1 and c1 + block[ 8] + RipeS1); e1 := e1 shl 12 or e1 shr 20 + d1; b1 := b1 shl 10 or b1 shr 22;

   Inc(d1, e1 or not a1 xor b1 + block[ 3] + RipeS2); d1 := d1 shl 11 or d1 shr 21 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 or not e1 xor a1 + block[10] + RipeS2); c1 := c1 shl 13 or c1 shr 19 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 or not d1 xor e1 + block[14] + RipeS2); b1 := b1 shl  6 or b1 shr 26 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 or not c1 xor d1 + block[ 4] + RipeS2); a1 := a1 shl  7 or a1 shr 25 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 or not b1 xor c1 + block[ 9] + RipeS2); e1 := e1 shl 14 or e1 shr 18 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 or not a1 xor b1 + block[15] + RipeS2); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 or not e1 xor a1 + block[ 8] + RipeS2); c1 := c1 shl 13 or c1 shr 19 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 or not d1 xor e1 + block[ 1] + RipeS2); b1 := b1 shl 15 or b1 shr 17 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 or not c1 xor d1 + block[ 2] + RipeS2); a1 := a1 shl 14 or a1 shr 18 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 or not b1 xor c1 + block[ 7] + RipeS2); e1 := e1 shl  8 or e1 shr 24 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 or not a1 xor b1 + block[ 0] + RipeS2); d1 := d1 shl 13 or d1 shr 19 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 or not e1 xor a1 + block[ 6] + RipeS2); c1 := c1 shl  6 or c1 shr 26 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 or not d1 xor e1 + block[13] + RipeS2); b1 := b1 shl  5 or b1 shr 27 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 or not c1 xor d1 + block[11] + RipeS2); a1 := a1 shl 12 or a1 shr 20 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 or not b1 xor c1 + block[ 5] + RipeS2); e1 := e1 shl  7 or e1 shr 25 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 or not a1 xor b1 + block[12] + RipeS2); d1 := d1 shl  5 or d1 shr 27 + c1; a1 := a1 shl 10 or a1 shr 22;

   Inc(c1, d1 and a1 or e1 and not a1 + block[ 1] + RipeS3); c1 := c1 shl 11 or c1 shr 21 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and e1 or d1 and not e1 + block[ 9] + RipeS3); b1 := b1 shl 12 or b1 shr 20 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and d1 or c1 and not d1 + block[11] + RipeS3); a1 := a1 shl 14 or a1 shr 18 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and c1 or b1 and not c1 + block[10] + RipeS3); e1 := e1 shl 15 or e1 shr 17 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and b1 or a1 and not b1 + block[ 0] + RipeS3); d1 := d1 shl 14 or d1 shr 18 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and a1 or e1 and not a1 + block[ 8] + RipeS3); c1 := c1 shl 15 or c1 shr 17 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and e1 or d1 and not e1 + block[12] + RipeS3); b1 := b1 shl  9 or b1 shr 23 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and d1 or c1 and not d1 + block[ 4] + RipeS3); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and c1 or b1 and not c1 + block[13] + RipeS3); e1 := e1 shl  9 or e1 shr 23 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and b1 or a1 and not b1 + block[ 3] + RipeS3); d1 := d1 shl 14 or d1 shr 18 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and a1 or e1 and not a1 + block[ 7] + RipeS3); c1 := c1 shl  5 or c1 shr 27 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and e1 or d1 and not e1 + block[15] + RipeS3); b1 := b1 shl  6 or b1 shr 26 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and d1 or c1 and not d1 + block[14] + RipeS3); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and c1 or b1 and not c1 + block[ 5] + RipeS3); e1 := e1 shl  6 or e1 shr 26 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and b1 or a1 and not b1 + block[ 6] + RipeS3); d1 := d1 shl  5 or d1 shr 27 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and a1 or e1 and not a1 + block[ 2] + RipeS3); c1 := c1 shl 12 or c1 shr 20 + b1; e1 := e1 shl 10 or e1 shr 22;

   Inc(b1, d1 or not e1 xor c1 + block[ 4] + RipeS4); b1 := b1 shl  9 or b1 shr 23 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, c1 or not d1 xor b1 + block[ 0] + RipeS4); a1 := a1 shl 15 or a1 shr 17 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, b1 or not c1 xor a1 + block[ 5] + RipeS4); e1 := e1 shl  5 or e1 shr 27 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, a1 or not b1 xor e1 + block[ 9] + RipeS4); d1 := d1 shl 11 or d1 shr 21 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, e1 or not a1 xor d1 + block[ 7] + RipeS4); c1 := c1 shl  6 or c1 shr 26 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, d1 or not e1 xor c1 + block[12] + RipeS4); b1 := b1 shl  8 or b1 shr 24 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, c1 or not d1 xor b1 + block[ 2] + RipeS4); a1 := a1 shl 13 or a1 shr 19 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, b1 or not c1 xor a1 + block[10] + RipeS4); e1 := e1 shl 12 or e1 shr 20 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, a1 or not b1 xor e1 + block[14] + RipeS4); d1 := d1 shl  5 or d1 shr 27 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, e1 or not a1 xor d1 + block[ 1] + RipeS4); c1 := c1 shl 12 or c1 shr 20 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, d1 or not e1 xor c1 + block[ 3] + RipeS4); b1 := b1 shl 13 or b1 shr 19 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, c1 or not d1 xor b1 + block[ 8] + RipeS4); a1 := a1 shl 14 or a1 shr 18 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, b1 or not c1 xor a1 + block[11] + RipeS4); e1 := e1 shl 11 or e1 shr 21 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, a1 or not b1 xor e1 + block[ 6] + RipeS4); d1 := d1 shl  8 or d1 shr 24 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, e1 or not a1 xor d1 + block[15] + RipeS4); c1 := c1 shl  5 or c1 shr 27 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, d1 or not e1 xor c1 + block[13] + RipeS4); b1 := b1 shl  6 or b1 shr 26 + a1; d1 := d1 shl 10 or d1 shr 22;

   temp := a1; a1 := a2; a2 := temp;
   temp := b1; b1 := b2; b2 := temp;
   temp := c1; c1 := c2; c2 := temp;
   temp := d1; d1 := d2; d2 := temp;
   temp := e1; e1 := e2; e2 := temp;

   Inc(a1, c1 or not d1 xor b1 + block[ 5] + RipeS5); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, b1 or not c1 xor a1 + block[14] + RipeS5); e1 := e1 shl  9 or e1 shr 23 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, a1 or not b1 xor e1 + block[ 7] + RipeS5); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, e1 or not a1 xor d1 + block[ 0] + RipeS5); c1 := c1 shl 11 or c1 shr 21 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, d1 or not e1 xor c1 + block[ 9] + RipeS5); b1 := b1 shl 13 or b1 shr 19 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, c1 or not d1 xor b1 + block[ 2] + RipeS5); a1 := a1 shl 15 or a1 shr 17 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, b1 or not c1 xor a1 + block[11] + RipeS5); e1 := e1 shl 15 or e1 shr 17 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, a1 or not b1 xor e1 + block[ 4] + RipeS5); d1 := d1 shl  5 or d1 shr 27 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, e1 or not a1 xor d1 + block[13] + RipeS5); c1 := c1 shl  7 or c1 shr 25 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, d1 or not e1 xor c1 + block[ 6] + RipeS5); b1 := b1 shl  7 or b1 shr 25 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, c1 or not d1 xor b1 + block[15] + RipeS5); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, b1 or not c1 xor a1 + block[ 8] + RipeS5); e1 := e1 shl 11 or e1 shr 21 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, a1 or not b1 xor e1 + block[ 1] + RipeS5); d1 := d1 shl 14 or d1 shr 18 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, e1 or not a1 xor d1 + block[10] + RipeS5); c1 := c1 shl 14 or c1 shr 18 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, d1 or not e1 xor c1 + block[ 3] + RipeS5); b1 := b1 shl 12 or b1 shr 20 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, c1 or not d1 xor b1 + block[12] + RipeS5); a1 := a1 shl  6 or a1 shr 26 + e1; c1 := c1 shl 10 or c1 shr 22;

   Inc(e1, a1 and c1 or b1 and not c1 + block[ 6] + RipeS6); e1 := e1 shl  9 or e1 shr 23 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and b1 or a1 and not b1 + block[11] + RipeS6); d1 := d1 shl 13 or d1 shr 19 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and a1 or e1 and not a1 + block[ 3] + RipeS6); c1 := c1 shl 15 or c1 shr 17 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and e1 or d1 and not e1 + block[ 7] + RipeS6); b1 := b1 shl  7 or b1 shr 25 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and d1 or c1 and not d1 + block[ 0] + RipeS6); a1 := a1 shl 12 or a1 shr 20 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and c1 or b1 and not c1 + block[13] + RipeS6); e1 := e1 shl  8 or e1 shr 24 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and b1 or a1 and not b1 + block[ 5] + RipeS6); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and a1 or e1 and not a1 + block[10] + RipeS6); c1 := c1 shl 11 or c1 shr 21 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and e1 or d1 and not e1 + block[14] + RipeS6); b1 := b1 shl  7 or b1 shr 25 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and d1 or c1 and not d1 + block[15] + RipeS6); a1 := a1 shl  7 or a1 shr 25 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and c1 or b1 and not c1 + block[ 8] + RipeS6); e1 := e1 shl 12 or e1 shr 20 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and b1 or a1 and not b1 + block[12] + RipeS6); d1 := d1 shl  7 or d1 shr 25 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and a1 or e1 and not a1 + block[ 4] + RipeS6); c1 := c1 shl  6 or c1 shr 26 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and e1 or d1 and not e1 + block[ 9] + RipeS6); b1 := b1 shl 15 or b1 shr 17 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and d1 or c1 and not d1 + block[ 1] + RipeS6); a1 := a1 shl 13 or a1 shr 19 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and c1 or b1 and not c1 + block[ 2] + RipeS6); e1 := e1 shl 11 or e1 shr 21 + d1; b1 := b1 shl 10 or b1 shr 22;

   Inc(d1, e1 or not a1 xor b1 + block[15] + RipeS7); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 or not e1 xor a1 + block[ 5] + RipeS7); c1 := c1 shl  7 or c1 shr 25 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 or not d1 xor e1 + block[ 1] + RipeS7); b1 := b1 shl 15 or b1 shr 17 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 or not c1 xor d1 + block[ 3] + RipeS7); a1 := a1 shl 11 or a1 shr 21 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 or not b1 xor c1 + block[ 7] + RipeS7); e1 := e1 shl  8 or e1 shr 24 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 or not a1 xor b1 + block[14] + RipeS7); d1 := d1 shl  6 or d1 shr 26 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 or not e1 xor a1 + block[ 6] + RipeS7); c1 := c1 shl  6 or c1 shr 26 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 or not d1 xor e1 + block[ 9] + RipeS7); b1 := b1 shl 14 or b1 shr 18 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 or not c1 xor d1 + block[11] + RipeS7); a1 := a1 shl 12 or a1 shr 20 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 or not b1 xor c1 + block[ 8] + RipeS7); e1 := e1 shl 13 or e1 shr 19 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 or not a1 xor b1 + block[12] + RipeS7); d1 := d1 shl  5 or d1 shr 27 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 or not e1 xor a1 + block[ 2] + RipeS7); c1 := c1 shl 14 or c1 shr 18 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 or not d1 xor e1 + block[10] + RipeS7); b1 := b1 shl 13 or b1 shr 19 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 or not c1 xor d1 + block[ 0] + RipeS7); a1 := a1 shl 13 or a1 shr 19 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 or not b1 xor c1 + block[ 4] + RipeS7); e1 := e1 shl  7 or e1 shr 25 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 or not a1 xor b1 + block[13] + RipeS7); d1 := d1 shl  5 or d1 shr 27 + c1; a1 := a1 shl 10 or a1 shr 22;

   Inc(c1, d1 and e1 or not d1 and a1 + block[ 8] + RipeS8); c1 := c1 shl 15 or c1 shr 17 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and d1 or not c1 and e1 + block[ 6] + RipeS8); b1 := b1 shl  5 or b1 shr 27 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and c1 or not b1 and d1 + block[ 4] + RipeS8); a1 := a1 shl  8 or a1 shr 24 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and b1 or not a1 and c1 + block[ 1] + RipeS8); e1 := e1 shl 11 or e1 shr 21 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and a1 or not e1 and b1 + block[ 3] + RipeS8); d1 := d1 shl 14 or d1 shr 18 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and e1 or not d1 and a1 + block[11] + RipeS8); c1 := c1 shl 14 or c1 shr 18 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and d1 or not c1 and e1 + block[15] + RipeS8); b1 := b1 shl  6 or b1 shr 26 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and c1 or not b1 and d1 + block[ 0] + RipeS8); a1 := a1 shl 14 or a1 shr 18 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and b1 or not a1 and c1 + block[ 5] + RipeS8); e1 := e1 shl  6 or e1 shr 26 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and a1 or not e1 and b1 + block[12] + RipeS8); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and e1 or not d1 and a1 + block[ 2] + RipeS8); c1 := c1 shl 12 or c1 shr 20 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 and d1 or not c1 and e1 + block[13] + RipeS8); b1 := b1 shl  9 or b1 shr 23 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 and c1 or not b1 and d1 + block[ 9] + RipeS8); a1 := a1 shl 12 or a1 shr 20 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 and b1 or not a1 and c1 + block[ 7] + RipeS8); e1 := e1 shl  5 or e1 shr 27 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 and a1 or not e1 and b1 + block[10] + RipeS8); d1 := d1 shl 15 or d1 shr 17 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 and e1 or not d1 and a1 + block[14] + RipeS8); c1 := c1 shl  8 or c1 shr 24 + b1; e1 := e1 shl 10 or e1 shr 22;

   Inc(b1, c1 xor d1 xor e1 + block[12]); b1 := b1 shl  8 or b1 shr 24 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 xor c1 xor d1 + block[15]); a1 := a1 shl  5 or a1 shr 27 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 xor b1 xor c1 + block[10]); e1 := e1 shl 12 or e1 shr 20 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 xor a1 xor b1 + block[ 4]); d1 := d1 shl  9 or d1 shr 23 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 xor e1 xor a1 + block[ 1]); c1 := c1 shl 12 or c1 shr 20 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 xor d1 xor e1 + block[ 5]); b1 := b1 shl  5 or b1 shr 27 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 xor c1 xor d1 + block[ 8]); a1 := a1 shl 14 or a1 shr 18 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 xor b1 xor c1 + block[ 7]); e1 := e1 shl  6 or e1 shr 26 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 xor a1 xor b1 + block[ 6]); d1 := d1 shl  8 or d1 shr 24 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 xor e1 xor a1 + block[ 2]); c1 := c1 shl 13 or c1 shr 19 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 xor d1 xor e1 + block[13]); b1 := b1 shl  6 or b1 shr 26 + a1; d1 := d1 shl 10 or d1 shr 22;
   Inc(a1, b1 xor c1 xor d1 + block[14]); a1 := a1 shl  5 or a1 shr 27 + e1; c1 := c1 shl 10 or c1 shr 22;
   Inc(e1, a1 xor b1 xor c1 + block[ 0]); e1 := e1 shl 15 or e1 shr 17 + d1; b1 := b1 shl 10 or b1 shr 22;
   Inc(d1, e1 xor a1 xor b1 + block[ 3]); d1 := d1 shl 13 or d1 shr 19 + c1; a1 := a1 shl 10 or a1 shr 22;
   Inc(c1, d1 xor e1 xor a1 + block[ 9]); c1 := c1 shl 11 or c1 shr 21 + b1; e1 := e1 shl 10 or e1 shr 22;
   Inc(b1, c1 xor d1 xor e1 + block[11]); b1 := b1 shl 11 or b1 shr 21 + a1; d1 := d1 shl 10 or d1 shr 22;

   Inc(d1, c2 + digest[1]);
   digest[1] := digest[2] + d2 + e1;
   digest[2] := digest[3] + e2 + a1;
   digest[3] := digest[4] + a2 + b1;
   digest[4] := digest[0] + b2 + c1;
   digest[0] := d1;
end;

// RipeMD160Final
//
procedure RipeMD160Final(var digest : TRipe160Digest; block : PRipe160Block;
                         remaining, totalSize : Integer);
var
   buffer : TRipe160Block;
begin
   System.Move(block^, buffer, remaining);
   PByteArray(@buffer)[remaining]:=$80;
   Inc(remaining);
   if remaining>SizeOf(buffer)-8 then begin
      FillChar(PByteArray(@buffer)[remaining], SizeOf(buffer)-remaining, 0);
      RipeMD160(digest, @buffer);
      remaining:=0;
   end;
   FillChar(PByteArray(@buffer)[remaining], SizeOf(buffer)-remaining-8, 0);
   buffer[14]:=totalSize shl 3;
   buffer[15]:=totalSize shr 29;
   RipeMD160(digest, @buffer);
end;

end.
