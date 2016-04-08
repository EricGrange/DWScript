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
   An Implementation of Secure Hash Algorithm 3 (Keccak)
   by Guido Bertoni, Joan Daemen, Michaël Peeters and Gilles Van Assche

   Based on http://www.wolfgang-ehrhardt.de/crchash_en.html

    (C) Copyright 2012-2014 Wolfgang Ehrhardt

    This software is provided 'as-is', without any express or implied warranty.
    In no event will the authors be held liable for any damages arising from
    the use of this software.

    Permission is granted to anyone to use this software for any purpose,
    including commercial applications, and to alter it and redistribute it
    freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
       claim that you wrote the original software. If you use this software in
       a product, an acknowledgment in the product documentation would be
       appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
       misrepresented as being the original software.

    3. This notice may not be removed or altered from any source distribution.
*)
unit dwsSHA3;

{$I dws.inc}
{$R-}

interface

uses SysUtils;

type
   ESHA3Exception = class(Exception);

const
   cKeccakPermutationSize        = 1600;
   cKeccakMaximumRate            = 1536;
   cKeccakPermutationSizeInBytes = cKeccakPermutationSize div 8;
   cKeccakMaximumRateInBytes     = cKeccakMaximumRate div 8;

type
   TState_B = packed array[0..cKeccakPermutationSizeInBytes-1] of Byte;
   TState_L = packed array[0..(cKeccakPermutationSizeInBytes) div 4 - 1] of Integer;
   TKDQueue = packed array[0..cKeccakMaximumRateInBytes-1] of Byte;

   TSHA3_Algo = (SHA3_224, SHA3_256, SHA3_384, SHA3_512, SHAKE_128, SHAKE_256);

   TSpongeState = record
      StateB: TState_B;
      DataQueue: TKDQueue;
      Rate: Integer;
      Capacity: Integer;
      BitsInQueue: Integer;
      FixedOutputLength: Integer;
      BitsAvailableForSqueezing: Integer;
      Squeezing: Integer;

      procedure Init(algo: TSHA3_Algo); inline;
      procedure Update(msg: Pointer; len: Integer); inline;
      procedure FinalHash(digest : Pointer); inline;

      procedure UpdateString(const s : String); inline;
      procedure UpdateInt64(const i : Int64); inline;
      procedure UpdateInteger(const i : Integer); inline;
   end;

   THashState = TSpongeState;   {Hash state context}

   TKeccakMaxDigest = packed array[0..63] of Byte;  {Keccak-512 digest}

   TSHA3_256_Hash = array [0..31] of Byte;
   PSHA3_256_Hash = ^TSHA3_256_Hash;

   TSHA3State = TSpongeState;   {Hash state context}

const
   cSHA3_256_EmptyString : TSHA3_256_Hash = (
      $a7, $ff, $c6, $f8, $bf, $1e, $d7, $66, $51, $c1, $47, $56, $a0, $61, $d6, $62,
      $f5, $80, $ff, $4d, $e4, $3b, $49, $fa, $82, $d8, $0a, $4b, $80, $f8, $43, $4a
      );

procedure SHA3_Init(var state: TSHA3State; algo: TSHA3_Algo);
procedure SHA3_Update(var state: TSHA3State; msg: Pointer; len: Integer);
procedure SHA3_FinalHash(var state: THashState; digest: Pointer);
function  SHA3_DigestToString(digest : Pointer; size : Integer) : String;

function HashSHA3_256(const buf : RawByteString) : String; overload;
function HashSHA3_256(p : Pointer; len : Integer) : String; overload;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cKeccakNumberOfRounds = 24;

type
   TBABytes = array[0..MaxInt-1] of Byte;
   PBA = ^TBABytes;
   TUInt64Array = array [0..MaxInt div SizeOf(UInt64)-1] of UInt64;
   PUInt64Array = ^TUInt64Array;

{$define USE_LOCALA}  {With FPC64/WIN64 about 20% faster}

const
   cRoundConstants : array[0..23] of UInt64 = (
      UInt64($0000000000000001), UInt64($0000000000008082),
      UInt64($800000000000808A), UInt64($8000000080008000),
      UInt64($000000000000808B), UInt64($0000000080000001),
      UInt64($8000000080008081), UInt64($8000000000008009),
      UInt64($000000000000008A), UInt64($0000000000000088),
      UInt64($0000000080008009), UInt64($000000008000000A),
      UInt64($000000008000808B), UInt64($800000000000008B),
      UInt64($8000000000008089), UInt64($8000000000008003),
      UInt64($8000000000008002), UInt64($8000000000000080),
      UInt64($000000000000800A), UInt64($800000008000000A),
      UInt64($8000000080008081), UInt64($8000000000008080),
      UInt64($0000000080000001), UInt64($8000000080008008)
   );

function RotL(const x: UInt64; c: Integer): UInt64; inline;
begin
   Result := (x shl c) or (x shr (64-c));
end;

function RotL1(var x: UInt64): UInt64;
{$ifdef WIN32_ASM}
asm
   mov   edx, [eax+4]
   mov   eax, [eax]
   add   eax, eax
   adc   edx, edx
   adc   al, 0
{$else}
   Result := (x shl 1) or (x shr (64-1));
{$endif}
end;

procedure KeccakPermutation(var state: TState_L);
var
   A: PUInt64Array;
   B: array[0..24] of UInt64;
   C0, C1, C2, C3, C4, D0, D1, D2, D3, D4: UInt64;
   i: Integer;
begin
   A := PUInt64Array(@state);
   for i:=0 to 23 do begin
      C0 := A[00] xor A[05] xor A[10] xor A[15] xor A[20];
      C1 := A[01] xor A[06] xor A[11] xor A[16] xor A[21];
      C2 := A[02] xor A[07] xor A[12] xor A[17] xor A[22];
      C3 := A[03] xor A[08] xor A[13] xor A[18] xor A[23];
      C4 := A[04] xor A[09] xor A[14] xor A[19] xor A[24];

      D0 := RotL1(C0) xor C3;
      D1 := RotL1(C1) xor C4;
      D2 := RotL1(C2) xor C0;
      D3 := RotL1(C3) xor C1;
      D4 := RotL1(C4) xor C2;

      B[00] := A[00] xor D1;
      B[01] := RotL(A[06] xor D2, 44);
      B[02] := RotL(A[12] xor D3, 43);
      B[03] := RotL(A[18] xor D4, 21);
      B[04] := RotL(A[24] xor D0, 14);
      B[05] := RotL(A[03] xor D4, 28);
      B[06] := RotL(A[09] xor D0, 20);
      B[07] := RotL(A[10] xor D1, 3);
      B[08] := RotL(A[16] xor D2, 45);
      B[09] := RotL(A[22] xor D3, 61);
      B[10] := RotL(A[01] xor D2, 1);
      B[11] := RotL(A[07] xor D3, 6);
      B[12] := RotL(A[13] xor D4, 25);
      B[13] := RotL(A[19] xor D0, 8);
      B[14] := RotL(A[20] xor D1, 18);
      B[15] := RotL(A[04] xor D0, 27);
      B[16] := RotL(A[05] xor D1, 36);
      B[17] := RotL(A[11] xor D2, 10);
      B[18] := RotL(A[17] xor D3, 15);
      B[19] := RotL(A[23] xor D4, 56);
      B[20] := RotL(A[02] xor D3, 62);
      B[21] := RotL(A[08] xor D4, 55);
      B[22] := RotL(A[14] xor D0, 39);
      B[23] := RotL(A[15] xor D1, 41);
      B[24] := RotL(A[21] xor D2, 2);

      A[00] := B[00] xor ((not B[01]) and B[02]);
      A[01] := B[01] xor ((not B[02]) and B[03]);
      A[02] := B[02] xor ((not B[03]) and B[04]);
      A[03] := B[03] xor ((not B[04]) and B[00]);
      A[04] := B[04] xor ((not B[00]) and B[01]);
      A[05] := B[05] xor ((not B[06]) and B[07]);
      A[06] := B[06] xor ((not B[07]) and B[08]);
      A[07] := B[07] xor ((not B[08]) and B[09]);
      A[08] := B[08] xor ((not B[09]) and B[05]);
      A[09] := B[09] xor ((not B[05]) and B[06]);
      A[10] := B[10] xor ((not B[11]) and B[12]);
      A[11] := B[11] xor ((not B[12]) and B[13]);
      A[12] := B[12] xor ((not B[13]) and B[14]);
      A[13] := B[13] xor ((not B[14]) and B[10]);
      A[14] := B[14] xor ((not B[10]) and B[11]);
      A[15] := B[15] xor ((not B[16]) and B[17]);
      A[16] := B[16] xor ((not B[17]) and B[18]);
      A[17] := B[17] xor ((not B[18]) and B[19]);
      A[18] := B[18] xor ((not B[19]) and B[15]);
      A[19] := B[19] xor ((not B[15]) and B[16]);
      A[20] := B[20] xor ((not B[21]) and B[22]);
      A[21] := B[21] xor ((not B[22]) and B[23]);
      A[22] := B[22] xor ((not B[23]) and B[24]);
      A[23] := B[23] xor ((not B[24]) and B[20]);
      A[24] := B[24] xor ((not B[20]) and B[21]);
      A[00] := A[00] xor cRoundConstants[i];
   end;
end;

procedure ExtractFromState(outp: Pointer; const state: TState_L; laneCount: Integer);
var
   pI, pS: PUInt64;
   i: Integer;
begin
   pI := outp;
   pS := @state[0];
   for i:=laneCount-1 downto 0 do begin
      pI^ := pS^;
      Inc(pI);
      Inc(pS);
   end;
end;

procedure XORIntoState(var state: TState_L; pI: PUInt64; laneCount: Integer);
var
   pS: PUInt64;
   i: Integer;
begin
   pS := @state[0];
   for i:=laneCount-1 downto 0 do begin
      pS^ := pS^ xor pI^;
      Inc(pI);
      Inc(pS);
   end;
end;

procedure KeccakAbsorb(var state: TState_B; data: PUInt64; laneCount: Integer);
begin
   XORIntoState(TState_L(state), data, laneCount);
   KeccakPermutation(TState_L(state));
end;

procedure InitSponge(var state: TSpongeState; Rate, Capacity: Integer);
begin
   if Rate+Capacity <> 1600 then
      raise ESHA3Exception.Create('Rate+Capacity should be 1600');
   if (Rate <= 0) or (Rate >= 1600) or ((Rate and 63) <> 0) then
      raise ESHA3Exception.Create('Invalid Rate');

   state.Rate := Rate;
   state.Capacity := Capacity;
   state.FixedOutputLength := 0;
   FillChar(state.StateB, SizeOf(state.StateB), 0);
   FillChar(state.DataQueue, cKeccakMaximumRateInBytes,0);
   state.BitsInQueue := 0;
   state.Squeezing := 0;
   state.BitsAvailableForSqueezing := 0;
end;

procedure AbsorbQueue(var state: TSpongeState);
begin
   {state.BitsInQueue is assumed to be equal to state.Rate}
   KeccakAbsorb(state.StateB, @state.DataQueue, state.Rate div 64);
   state.BitsInQueue := 0;
end;

procedure Absorb(var state: TSpongeState; data: PBA; databitlen: Integer);
var
   i, j, wholeBlocks, partialBlock: Integer;
   partialByte: Integer;
   curData: PUInt64;
begin
   if state.BitsInQueue and 7 <> 0 then
      raise ESHA3Exception.Create('Only the last call may contain a partial byte');
   if state.Squeezing<>0 then
      raise ESHA3Exception.Create('Too late for additional input');
   i := 0;
   while i < databitlen do begin
      if ((state.BitsInQueue=0) and (databitlen >= state.Rate) and (i <= (databitlen-state.Rate))) then begin
         wholeBlocks := (databitlen-i) div state.Rate;
         curData := @data^[i div 8];
         j := 0;
         while j<wholeBlocks do begin
            KeccakAbsorb(state.StateB, curData, state.Rate div 64);
            Inc(j);
            Inc(pByte(curData), state.Rate div 8);
         end;
         Inc(i, wholeBlocks*state.Rate);
      end else begin
         partialBlock := databitlen - i;
         if partialBlock+state.BitsInQueue > state.Rate then begin
            partialBlock := state.Rate - state.BitsInQueue;
         end;
         partialByte := partialBlock and 7;
         dec(partialBlock, partialByte);
         move(data^[i div 8], state.DataQueue[state.BitsInQueue div 8], partialBlock div 8);
         Inc(state.BitsInQueue, partialBlock);
         Inc(i, partialBlock);
         if state.BitsInQueue=state.Rate then
            AbsorbQueue(state);
         if partialByte > 0 then begin
            state.DataQueue[state.BitsInQueue div 8] := data^[i div 8] and ((1 shl partialByte)-1);
            Inc(state.BitsInQueue, partialByte);
            Inc(i, partialByte);
         end;
      end;
   end;
end;

procedure PadAndSwitchToSqueezingPhase(var state: TSpongeState);
var
   i: Integer;
begin
   {Note: the bits are numbered from 0=LSB to 7=MSB}
   if (state.BitsInQueue + 1 = state.Rate) then begin
      i := state.BitsInQueue div 8;
      state.DataQueue[i] := state.DataQueue[i] or (1 shl (state.BitsInQueue and 7));
      AbsorbQueue(state);
      FillChar(state.DataQueue, state.Rate div 8, 0);
   end else begin
      i := state.BitsInQueue div 8;
      FillChar(state.DataQueue[(state.BitsInQueue+7) div 8], state.Rate div 8 - (state.BitsInQueue+7) div 8,0);
      state.DataQueue[i] := state.DataQueue[i] or (1 shl (state.BitsInQueue and 7));
   end;
   i := (state.Rate-1) div 8;
   state.DataQueue[i] := state.DataQueue[i] or (1 shl ((state.Rate-1) and 7));
   AbsorbQueue(state);
   ExtractFromState(@state.DataQueue, TState_L(state.StateB), state.Rate div 64);
   state.BitsAvailableForSqueezing := state.Rate;
   state.Squeezing := 1;
end;

procedure Squeeze(var state: THashState; output: PBA; outputLength: Integer);
var
   i: Integer;
   partialBlock: Integer;
begin
   if state.Squeezing=0 then
      PadAndSwitchToSqueezingPhase(state);
   if outputLength and 7 <> 0 then
      raise ESHA3Exception.Create('Only multiple of 8 bits are allowed for output digest length');
   i := 0;
   while i < outputLength do begin
      if state.BitsAvailableForSqueezing=0 then begin
         KeccakPermutation(TState_L(state.StateB));
         ExtractFromState(@state.DataQueue, TState_L(state.StateB), state.Rate div 64);
         state.BitsAvailableForSqueezing := state.Rate;
      end;
      partialBlock := state.BitsAvailableForSqueezing;
      if partialBlock > outputLength - i then
         partialBlock := outputLength - i;
      move(state.DataQueue[(state.Rate - state.BitsAvailableForSqueezing) div 8], output^[i div 8], partialBlock div 8);
      dec(state.BitsAvailableForSqueezing, partialBlock);
      Inc(i,partialBlock);
   end;
end;

procedure Update(var state: THashState; data: PBA; databitlen: Integer);
var
   lastByte: Byte;
begin
   if databitlen and 7 = 0 then
      Absorb(state, data, databitlen)
   else begin
      Absorb(state, data, databitlen - (databitlen and 7));
      {Align the last partial Byte to the least significant bits}
      lastByte := data^[databitlen div 8] shr (8 - (databitlen and 7));
      Absorb(state, @lastByte, databitlen and 7);
   end;
end;

procedure SHA3_FinalBit_LSB(var state: THashState; bits: Byte; bitlen: Integer; hashval: Pointer; numbits: Integer);
var
   ll : Integer;
   lw : word;
begin
   bitlen := bitlen and 7;
   if bitlen=0 then begin
      lw := 0
   end else begin
      lw := bits and Pred(word(1) shl bitlen);
   end;

   // 'append' (in LSB language) the domain separation bits
   if state.FixedOutputLength=0 then begin
      // SHAKE: append four bits 1111
      lw := lw or (word($F) shl bitlen);
      ll := bitlen+4;
   end else begin
      // SHA3: append two bits 01
      lw := lw or (word($2) shl bitlen);
      ll := bitlen+2;
   end;

   // update state with final bits
   if ll<9 then begin
      {0..8 bits, one call to update}
      lw := lw shl (8-ll);
      Update(state, @lw, ll);
      {squeeze the digits from the sponge}
      Squeeze(state, hashval, numbits);
   end else begin
      {More than 8 bits, first a regular update with low Byte}
      Update(state, @lw, 8);
      {Finally update remaining last bits}
      dec(ll,8);
      lw := lw shr ll;
      Update(state, @lw, ll);
      Squeeze(state, hashval, numbits);
   end;
end;

procedure SHA3_Init(var state: TSHA3State; algo: TSHA3_Algo);
const
   cOutputLength : array[TSHA3_Algo] of word =  (224, 256, 384, 512, 0, 0);
begin
   case algo of
      SHA3_224: InitSponge(state, 1152,  448);
      SHA3_256: InitSponge(state, 1088,  512);
      SHA3_384: InitSponge(state,  832,  768);
      SHA3_512: InitSponge(state,  576, 1024);
      SHAKE_128: InitSponge(state, 1344,  256);
      SHAKE_256: InitSponge(state, 1088,  512);
   else
      raise ESHA3Exception.Create('Invalid algorithm');
   end;
   state.FixedOutputLength := cOutputLength[algo];
end;

procedure SHA3_Update(var state: TSHA3State; msg: Pointer; len: Integer);
begin
   Absorb(state, msg, len*8);
end;

procedure SHA3_FinalHash(var state: THashState; digest: Pointer);
begin
   if state.FixedOutputLength=0 then
      raise ESHA3Exception.Create('Wrong final');
   SHA3_FinalBit_LSB(state, 0, 0, digest, state.FixedOutputLength);
end;

function SHA3_DigestToString(digest : Pointer; size : Integer) : String;
const
   cHex : String = '0123456789abcdef';
var
   i : Integer;
   pResult : PChar;
   pSrc : PByteArray;
begin
   SetLength(Result, size*2);
   pSrc:=digest;
   pResult:=Pointer(Result);
   for i:=0 to size-1 do begin
      pResult[0]:=cHex[(pSrc[i] shr 4)+1];
      pResult[1]:=cHex[(pSrc[i] and 15)+1];
      Inc(pResult, 2);
   end;
end;

procedure TSpongeState.Init(algo : TSHA3_Algo);
begin
   SHA3_Init(Self, algo);
end;

procedure TSpongeState.Update(msg : Pointer; len : Integer);
begin
   SHA3_Update(Self, msg, len);
end;

procedure TSpongeState.FinalHash(digest : Pointer);
begin
   SHA3_FinalHash(Self, digest);
end;

procedure TSpongeState.UpdateString(const s : String);
begin
   Update(Pointer(s), Length(s)*SizeOf(Char));
end;

procedure TSpongeState.UpdateInt64(const i : Int64);
begin
   Update(@i, SizeOf(i));
end;

procedure TSpongeState.UpdateInteger(const i : Integer);
begin
   Update(@i, SizeOf(i));
end;

function HashSHA3_256(const buf : RawByteString) : String;
begin
   Result:=HashSHA3_256(Pointer(buf), Length(buf));
end;

function HashSHA3_256(p : Pointer; len : Integer) : String;
var
   sponge : TSHA3State;
   hash : TSHA3_256_Hash;
begin
   SHA3_Init(sponge, SHA3_256);
   SHA3_Update(sponge, p, len);
   SHA3_FinalHash(sponge, @hash);

   Result := SHA3_DigestToString(@hash, SizeOf(hash));
end;

end.

