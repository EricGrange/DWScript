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
{    Several of the algorithms below have been developped              }
{    by Sebastiano Vigna and released as public domain,                }
{    see http://vigna.di.unimi.it/                                     }
{                                                                      }
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsRandom;

{$I ../dws.inc}

interface

type
   // Algorithm by David Blackman and Sebastiano Vigna
   TXoroShiro128Plus = record
         Seed : array [0..1] of UInt64;

         procedure SetSeed64(seed64 : UInt64);
         procedure Randomize;

         function Next : UInt64;
   end;

// Algorithm by by Sebastiano Vigna
function SplitMix64(var x : UInt64) : UInt64;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RDTSC
//
function RDTSC : UInt64;
asm
   RDTSC
end;

// RotL
//
function RotL(const x : UInt64; k : Integer) : UInt64; inline;
begin
   Result := (x shl k) or (x shr (64-k));
end;

// SplitMix64
//
function SplitMix64(var x : UInt64) : UInt64;
var
   z : UInt64;
begin
   Inc(x, UInt64($9E3779B97F4A7C15));
	z := (x xor (x shr 30)) * UInt64($BF58476D1CE4E5B9);
	z := (z xor (z shr 27)) * UInt64($94D049BB133111EB);
	Result := z xor (z shr 31);
end;

// SetSeed64
//
procedure TXoroShiro128Plus.SetSeed64(seed64 : UInt64);
begin
   Seed[0] := SplitMix64(seed64);
   Seed[1] := SplitMix64(seed64);
end;

// Randomize
//
procedure TXoroShiro128Plus.Randomize;
begin
   SetSeed64(RDTSC);
end;

// Next
//
function TXoroShiro128Plus.Next : UInt64;
{$ifdef WIN32_ASM}
asm
   mov   ecx, eax

   // s0 := Seed[0];
   movq  mm0, [ecx]
   // s1 := Seed[1];
   movq  mm1, [ecx+8]

   // Result := s0 + s1;
   mov   eax, [ecx]
   mov   edx, [ecx+4]
   add   eax, [ecx+8]
   adc   edx, [ecx+12]

   // s1 := s1 xor s0;
   pxor  mm1, mm0

   // Seed[0] := RotL(s0, 55) xor s1 xor (s1 shl 14);
   movq  mm2, mm0
   psllq mm0, 55
   psrlq mm2, 64-55
   por   mm0, mm2
   pxor  mm0, mm1
   movq  mm3, mm1
   psllq mm3, 14
   pxor  mm0, mm3
   movq  [ecx], mm0

   // Seed[1] := RotL(s1, 36);
   movq  mm4, mm1
   psllq mm1, 36
   psrlq mm4, 64-36
   pxor  mm1, mm4
   movq  [ecx+8], mm1

   emms
{$else}
var
   s0, s1 : UInt64;
begin
   s0 := Seed[0];
   s1 := Seed[1];
   Result := s0 + s1;

   s1 := s1 xor s0;
   Seed[0] := RotL(s0, 55) xor s1 xor (s1 shl 14);
   Seed[1] := RotL(s1, 36);
{$endif}
end;

end.
