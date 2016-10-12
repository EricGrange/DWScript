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
(*
   An Implementation of xxHash by Yann Collet

   https://github.com/Cyan4973/xxHash

   xxHash Library
   Copyright (c) 2012-2014, Yann Collet
   All rights reserved.

   Redistribution and use in source and binary forms, with or without modification,
   are permitted provided that the following conditions are met:

   * Redistributions of source code must retain the above copyright notice, this
     list of conditions and the following disclaimer.

   * Redistributions in binary form must reproduce the above copyright notice, this
     list of conditions and the following disclaimer in the documentation and/or
     other materials provided with the distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
   ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
   ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
   (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
   ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
unit dwsXXHash;

{$I dws.inc}

interface

type
   xxHash32 = record
      private
         // Work buffers should be the first fields, to preserve alignment
         FBuffer : array [0..3] of Cardinal;
         Fv : array [0..3] of Cardinal;
         FBufferSize : Cardinal;
         FSeed : Cardinal;
         FTotalLength : UInt64;

         class function FullLarge(data : Pointer; dataSize : Cardinal) : Cardinal; static;
         class function FullSmall(data : Pointer; dataSize : Cardinal) : Cardinal; static;
         class function DigestTail(data : Pointer; dataSize, partial : Cardinal) : Cardinal; static;

      public
         procedure Init(aSeed : Cardinal = 0);
         procedure Update(data : Pointer; dataSize : Cardinal);
         function Digest : Cardinal;

         class function Full(data : Pointer; dataSize : Cardinal) : Cardinal; static;
   end;
   PxxHash32 = ^xxHash32;

implementation

const
   cPRIME32_1 = Cardinal(2654435761);
   cPRIME32_2 = Cardinal(2246822519);
   cPRIME32_3 = Cardinal(3266489917);
   cPRIME32_4 = Cardinal(668265263);
   cPRIME32_5 = Cardinal(374761393);

type
   PCardinalArray = ^TCardinalArray;
   TCardinalArray = array[0..(MaxLongint div SizeOf(Cardinal))-1] of Cardinal;

// RotateLeft32
//
function RotateLeft32(value : Cardinal; count: Integer) : Cardinal; inline;
begin
   Result := (value shl count) or (value shr (32 - count));
end;

// Kernel
//
{$CODEALIGN 16}
function Kernel(v : PCardinalArray; ptrData, ptrDataLimit : NativeUInt) : NativeUInt;
{$ifdef WIN32_ASM}
asm
   // eax = v, edx = ptrData, ecx = ptrDataLimit
   push  ebx
   push  edi
   push  esi
   push  eax
   push  ecx   // ptrDataLimit -> [esp]

   mov   edi, [eax]
   mov   esi, [eax+4]
   mov   ecx, [eax+8]
   mov   ebx, [eax+12]

@@loop:

   mov   eax, [edx]
   imul  eax, cPRIME32_2
   lea   edi, [edi+eax]
   rol   edi, 13
   imul  edi, cPRIME32_1

   mov   eax, [edx+4]
   imul  eax, cPRIME32_2
   lea   esi, [esi+eax]
   rol   esi, 13
   imul  esi, cPRIME32_1

   mov   eax, [edx+8]
   imul  eax, cPRIME32_2
   lea   ecx, [ecx+eax]
   rol   ecx, 13
   imul  ecx, cPRIME32_1

   mov   eax, [edx+12]
   imul  eax, cPRIME32_2
   lea   ebx, [ebx+eax]
   rol   ebx, 13
   imul  ebx, cPRIME32_1

   lea   edx, [edx+16];
   cmp   edx, [esp]
   jng   @@loop

   mov   eax, [esp+4]

   mov   [eax], edi
   mov   [eax+4], esi
   mov   [eax+8], ecx
   mov   [eax+12], ebx

   pop   ecx
   pop   eax
   pop   esi
   pop   edi
   pop   ebx

   mov   eax, edx
{$else}
begin
   repeat
      v[0] := cPRIME32_1 * RotateLeft32(v[0] + cPRIME32_2 * PCardinalArray(ptrData)[0], 13);
      v[1] := cPRIME32_1 * RotateLeft32(v[1] + cPRIME32_2 * PCardinalArray(ptrData)[1], 13);
      v[2] := cPRIME32_1 * RotateLeft32(v[2] + cPRIME32_2 * PCardinalArray(ptrData)[2], 13);
      v[3] := cPRIME32_1 * RotateLeft32(v[3] + cPRIME32_2 * PCardinalArray(ptrData)[3], 13);
      Inc(ptrData, 16);
   until ptrData > ptrDataLimit;
   Result := ptrData;
{$endif}
end;

// MixKernel
//
{$CODEALIGN 16}
{$ifdef WIN32_ASM}
function MixKernel(v : PCardinalArray) : Cardinal;
asm
   mov   edx, [eax]
   rol   edx, 1
   mov   ecx, [eax+4]
   rol   ecx, 7
   lea   edx, [edx+ecx]
   mov   ecx, [eax+8]
   rol   ecx, 12
   lea   edx, [edx+ecx]
   mov   ecx, [eax+12]
   rol   ecx, 18
   lea   eax, [edx+ecx]
end;
{$else}
function MixKernel(v : PCardinalArray) : Cardinal; inline;
begin
   Result := RotateLeft32(v[0],  1) + RotateLeft32(v[1],  7)
           + RotateLeft32(v[2], 12) + RotateLeft32(v[3], 18);
end;
{$endif}

procedure xxHash32.Init(aSeed : Cardinal = 0);
begin
   FSeed := aSeed;
   Fv[0] := aSeed + cPRIME32_1 + cPRIME32_2;
   Fv[1] := aSeed + cPRIME32_2;
   Fv[2] := aSeed;
   Fv[3] := aSeed - cPRIME32_1;
   FTotalLength := 0;
   FBufferSize := 0;
end;

// Update
//
procedure xxHash32.Update(data : Pointer; dataSize : Cardinal);
var
   ptrData, ptrDataEnd : NativeUInt;
begin
   ptrData := NativeUInt(data);

   Inc(FTotalLength, dataSize);

   if FBufferSize + dataSize < 16 then begin
      // accumulate to buffer
      Move(Pointer(ptrData)^, PByte(@FBuffer)[FBufferSize], dataSize);
      FBufferSize := FBufferSize + dataSize;
      Exit;
   end;

   ptrDataEnd := ptrData + UInt32(dataSize);

   if FBufferSize > 0 then begin
      // flush buffer
      Move(Pointer(ptrData)^, PByte(@FBuffer)[FBufferSize], 16-FBufferSize);

      Fv[0] := cPRIME32_1 * RotateLeft32(Fv[0] + cPRIME32_2 * FBuffer[0], 13);
      Fv[1] := cPRIME32_1 * RotateLeft32(Fv[1] + cPRIME32_2 * FBuffer[1], 13);
      Fv[2] := cPRIME32_1 * RotateLeft32(Fv[2] + cPRIME32_2 * FBuffer[2], 13);
      Fv[3] := cPRIME32_1 * RotateLeft32(Fv[3] + cPRIME32_2 * FBuffer[3], 13);

      ptrData := ptrData + 16-FBufferSize;
      FBufferSize := 0;
   end;

   if ptrData <= ptrDataEnd-16 then begin
      // hash 16 bytes blocks
      ptrData := Kernel(@Fv, ptrData, ptrDataEnd-16);
   end;

   if ptrData < ptrDataEnd then begin
      // place remainder in buffer
      FBufferSize := ptrDataEnd-ptrData;
      Move(Pointer(ptrData)^, FBuffer, ptrDataEnd-ptrData);
   end;
end;

// Digest
//
function xxHash32.Digest : Cardinal;
begin
   if FTotalLength >= 16 then begin
      Result := MixKernel(@Fv)
   end else begin
      Result := FSeed + cPRIME32_5;
   end;
   Inc(Result, FTotalLength);

   Result := DigestTail(@FBuffer, FBufferSize, Result);
end;

// FullLarge
//
class function xxHash32.FullLarge(data : Pointer; dataSize : Cardinal) : Cardinal;
var
   ptrData : NativeUInt;
   v : array [0..3] of Cardinal;
begin
   ptrData := NativeUInt(data);

   v[0] := cPRIME32_1 + cPRIME32_2;
   v[1] := cPRIME32_2;
   v[2] := 0;
   v[3] := Cardinal(0-cPRIME32_1);

   ptrData := Kernel(@v, ptrData, ptrData+dataSize-16);
   Result := DigestTail(Pointer(ptrData), dataSize and 15, dataSize + MixKernel(@v));
end;

// FullSmall
//
class function xxHash32.FullSmall(data : Pointer; dataSize : Cardinal) : Cardinal;
begin
   Result := DigestTail(data, dataSize, dataSize + 374761393);
end;

// DigestTail
//
{$CODEALIGN 16}
class function xxHash32.DigestTail(data : Pointer; dataSize, partial : Cardinal) : Cardinal;
{$ifdef WIN32_ASM}
asm
   // eax = data, edx = dataSize, ecx = result
   push  ebx

   cmp   edx, 4
   jb    @@sizebelow4

@@sizeabove4:
   mov   ebx, [eax]
   imul  ebx, cPRIME32_3
   lea   ecx, [ecx + ebx]
   rol   ecx, 17
   imul  ecx, cPRIME32_4
   lea   eax, [eax + 4]
   lea   edx, [edx - 4]
   cmp   edx, 4
   jge   @@sizeabove4

@@sizebelow4:
   test  edx, edx
   jz    @@wrapup

@@sizeabove0:
   movzx ebx, [eax]
   imul  ebx, cPRIME32_5
   lea   ecx, [ecx + ebx]
   rol   ecx, 11
   imul  ecx, cPRIME32_1
   lea   eax, [eax + 1]
   dec   edx
   jnz   @@sizeabove0

@@wrapup:
   mov   edx, ecx
   shr   ecx, 15
   xor   edx, ecx
   imul  ecx, edx, cPRIME32_2
   mov   edx, ecx
   shr   ecx, 13
   xor   edx, ecx
   imul  ecx, edx, cPRIME32_3
   mov   eax, ecx
   shr   ecx, 16
   xor   eax, ecx

   pop   ebx
{$else}
var
   i : Integer;
   ptrData : NativeUInt;
begin
   ptrData := NativeUInt(data);
   Result := partial;

   while dataSize >= 4 do begin
      Result := Result + PCardinal(ptrData)^ * cPRIME32_3;
      Result := RotateLeft32(Result, 17) * cPRIME32_4;
      Inc(ptrData, 4);
      Dec(dataSize, 4);
   end;

   for i := 1 to dataSize do begin
      Result := Result + PByte(ptrData)^ * cPRIME32_5;
      Result := RotateLeft32(Result, 11) * cPRIME32_1;
      Inc(ptrData);
   end;

   Result := (Result xor (Result shr 15)) * cPRIME32_2;
   Result := (Result xor (Result shr 13)) * cPRIME32_3;
   Result := (Result xor (Result shr 16));
{$endif}
end;

// Full
//
class function xxHash32.Full(data : Pointer; dataSize : Cardinal) : Cardinal;
var
   ptrData : NativeUInt;
   v : array [0..3] of Cardinal;
begin
   if dataSize >= 16 then begin

      v[0] := cPRIME32_1 + cPRIME32_2;
      v[1] := cPRIME32_2;
      v[2] := 0;
      v[3] := Cardinal(0-cPRIME32_1);

      ptrData := Kernel(@v, NativeUInt(data), NativeUInt(data)+dataSize-16);
      Result := DigestTail(Pointer(ptrData), dataSize and 15, dataSize + MixKernel(@v));

   end else begin

      Result := DigestTail(data, dataSize, dataSize + 374761393);

   end;
end;

end.
