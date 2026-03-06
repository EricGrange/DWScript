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
{    The Initial Developer of the Original Code is Eric Grange.        }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsKernelCompilerBackend.JIT;

interface

{$POINTERMATH ON}

uses
   System.Classes, System.SysUtils, System.Generics.Collections, System.Math,
   Winapi.Windows,
   dwsUtils, dwsKernelCompilerCommon, dwsXPlatform,
   dwsJITx86Intrinsics, dwsJITFixups, dwsJITx86_64;

type
   TFixupLocationHelper = class
      Stream : TStream;
      constructor Create(aStream : TStream);
      function GetLocation : Integer;
   end;

   TKCLMicroKernel = procedure(pIn, pOut, pWeights, pBias: PDouble; totalPixels: NativeInt);

   TKCLCompiledKernel = class
   public
      Code : Pointer;
      Size : NativeInt;
      Weights : TDoubleDynArray; // Transposed weights
      constructor Create(ACode : Pointer; ASize : NativeInt);
      destructor Destroy; override;
   end;

   TKCLOptimizationData = class
   public
      CompiledKernels : TDictionary<TKCLNode, TKCLCompiledKernel>;
      constructor Create;
      destructor Destroy; override;
   end;

   TKCLWin64JITBackend = class
   public
      class function Compile(AKernel : TKCLKernel; ANode : TKCLNode; inC, outC : Integer) : TKCLCompiledKernel;
      class function Execute(AKernel : TKCLKernel; ANode : TKCLNode; pIn, pOut, pWeights, pBias: PDouble; totalPixels: NativeInt; inC, outC : Integer) : Boolean;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TFixupLocationHelper ------------------
// ------------------

// Create
//
constructor TFixupLocationHelper.Create(aStream : TStream);
begin
   Stream := aStream;
end;

// GetLocation
//
function TFixupLocationHelper.GetLocation : Integer;
begin
   Result := Stream.Position;
end;

// ------------------
// ------------------ TKCLCompiledKernel ------------------
// ------------------

// Create
//
constructor TKCLCompiledKernel.Create(ACode : Pointer; ASize : NativeInt);
begin
   Code := ACode; Size := ASize;
end;

// Destroy
//
destructor TKCLCompiledKernel.Destroy;
begin
   if Code <> nil then VirtualFree(Code, 0, MEM_RELEASE);
   inherited;
end;

// ------------------
// ------------------ TKCLOptimizationData ------------------
// ------------------

// Create
//
constructor TKCLOptimizationData.Create;
begin
   CompiledKernels := TDictionary<TKCLNode, TKCLCompiledKernel>.Create;
 end;

// Destroy
//
destructor TKCLOptimizationData.Destroy;
var compiled : TKCLCompiledKernel;
begin
   for compiled in CompiledKernels.Values do compiled.Free;
   CompiledKernels.Free;
   inherited;
end;

// ------------------
// ------------------ TKCLWin64JITBackend ------------------
// ------------------

// Compile
//
class function TKCLWin64JITBackend.Compile(AKernel : TKCLKernel; ANode : TKCLNode; inC, outC : Integer) : TKCLCompiledKernel;
var
   j : Tx86_64_WriteOnlyStream;
   ptr : Pointer;
   jmpTailPatch, jmpEpiloguePatch, pixel4LoopPos, tailLoopPos, kLoopPos, kLoop1Pos : Integer;
   i, p, tileCount, tile, curTileSize, cOffset, ymmIdx : Integer;
   nChan : Integer;
   accReg, inReg : Integer;
   pOutRegs : array[0..3] of TgpRegister64;
   b : TBytes;
   allocSize : Integer;
begin
   j := Tx86_64_WriteOnlyStream.Create;
   try
      // Prologue
      j._push_reg(gprRBP); j._mov_reg_reg(gprRBP, gprRSP);
      j._push_reg(gprRBX); j._push_reg(gprRDI); j._push_reg(gprR12); j._push_reg(gprR13); j._push_reg(gprR14); j._push_reg(gprR15);
      j._sub_reg_imm(gprRSP, 160);
      for i := 0 to 9 do j._vmovups_ptr_reg_reg(gprRSP, i * 16, TxmmRegister(i + 6)); // VMOVUPS [rsp + i*16], xmm(i+6)

      j._mov_reg_qword_ptr_reg(gprRDI, gprRBP, 48); // mov rdi, [rbp+48] // totalPixels

      // 4-Pixel Loop
      pixel4LoopPos := j.Size;
      j._cmp_reg_imm(gprRDI, 4);
      j._jump(flagsB, $10000); jmpTailPatch := j.Size - 4; // Forward jump dummy

      j._mov_reg_reg(gprR13, gprRCX);
      j._add_reg_imm(gprR13, inC * 8);
      j._mov_reg_reg(gprR14, gprR13);
      j._add_reg_imm(gprR14, inC * 8);
      j._mov_reg_reg(gprR15, gprR14);
      j._add_reg_imm(gprR15, inC * 8);

      j._mov_reg_reg(gprR10, gprRDX);
      j._add_reg_imm(gprR10, outC * 8);
      j._mov_reg_reg(gprR11, gprR10);
      j._add_reg_imm(gprR11, outC * 8);
      j._mov_reg_reg(gprR12, gprR11);
      j._add_reg_imm(gprR12, outC * 8);

      tileCount := (outC + 7) div 8;
      for tile := 0 to tileCount - 1 do begin
         curTileSize := Min(8, outC - tile * 8);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(4, curTileSize - cOffset); if nChan = 3 then nChan := 2;
            if nChan = 4 then j._vmovupd_ptr_reg(TymmRegister(ymmIdx), gprR9, tile*64 + cOffset*8)
            else if nChan >= 2 then j._vmovups_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*64 + cOffset*8)
            else j._vmovsd_reg_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*64 + cOffset*8);
            for p := 1 to 3 do begin
               if nChan = 4 then j._vmovapd_reg_reg(TymmRegister(ymmIdx + p*2), TymmRegister(ymmIdx))
               else j._vmovapd_reg_reg(TymmRegister(ymmIdx + p*2), TymmRegister(ymmIdx)); // DWScript uses ymm overload automatically
            end;
            cOffset := cOffset + nChan; ymmIdx := ymmIdx + 1;
         end;

         j._mov_reg_reg(gprRBX, gprR8);
         if tile > 0 then j._add_reg_imm(gprRBX, tile * 8 * inC * 8);
         j._xor_reg_reg(gprRAX, gprRAX);

         kLoopPos := j.Size;

         j._vbroadcastsd_ptr_indexed(ymm8, gprRCX, gprRAX, 8, 0);
         j._vbroadcastsd_ptr_indexed(ymm9, gprR13, gprRAX, 8, 0);
         j._vbroadcastsd_ptr_indexed(ymm10, gprR14, gprRAX, 8, 0);
         j._vbroadcastsd_ptr_indexed(ymm11, gprR15, gprRAX, 8, 0);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(4, curTileSize - cOffset); if nChan = 3 then nChan := 2;
            for p := 0 to 3 do begin
               accReg := ymmIdx + p*2; inReg := 8 + p;
               if nChan = 4 then j._vfmadd231pd_ptr_reg(TymmRegister(accReg), TymmRegister(inReg), gprRBX, cOffset*8)
               else if nChan >= 2 then j._vfmadd231pd_ptr_reg(TxmmRegister(accReg), TxmmRegister(inReg), gprRBX, cOffset*8)
               else j._vfmadd231sd_ptr_reg(TxmmRegister(accReg), TxmmRegister(inReg), gprRBX, cOffset*8);
            end;
            cOffset := cOffset + nChan; ymmIdx := ymmIdx + 1;
         end;

         j._add_reg_imm(gprRBX, 64);
         j._add_reg_imm(gprRAX, 1);
         j._cmp_reg_imm(gprRAX, inC);
         j._jump(flagsB, kLoopPos - j.Size);

         pOutRegs[0] := gprRDX; pOutRegs[1] := gprR10; pOutRegs[2] := gprR11; pOutRegs[3] := gprR12;
         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(4, curTileSize - cOffset); if nChan = 3 then nChan := 2;
            for p := 0 to 3 do begin
               accReg := ymmIdx + p*2;
               if nChan = 4 then j._vmovupd_ptr_reg_reg(pOutRegs[p], tile*64 + cOffset*8, TymmRegister(accReg))
               else if nChan >= 2 then j._vmovups_ptr_reg_reg(pOutRegs[p], tile*64 + cOffset*8, TxmmRegister(accReg))
               else j._vmovsd_ptr_reg_reg(pOutRegs[p], tile*64 + cOffset*8, TxmmRegister(accReg));
            end;
            cOffset := cOffset + nChan; ymmIdx := ymmIdx + 1;
         end;
      end;

      j._add_reg_imm(gprRCX, inC * 8 * 4);
      j._add_reg_imm(gprRDX, outC * 8 * 4);
      j._sub_reg_imm(gprRDI, 4);
      j._jump(flagsNE, pixel4LoopPos - j.Size);

      // Tail Pixel Loop (1 pixel)
      var curTailPos := j.Size;

      j._test_reg_reg(gprRDI, gprRDI);
      j._jump(flagsE, $10000); jmpEpiloguePatch := j.Size - 4;

      tailLoopPos := j.Size;
      for tile := 0 to tileCount - 1 do begin
         curTileSize := Min(8, outC - tile * 8);
         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(4, curTileSize - cOffset); if nChan = 3 then nChan := 2;
            if nChan = 4 then j._vmovupd_ptr_reg(TymmRegister(ymmIdx), gprR9, tile*64 + cOffset*8)
            else if nChan >= 2 then j._vmovups_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*64 + cOffset*8)
            else j._vmovsd_reg_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*64 + cOffset*8);
            cOffset := cOffset + nChan; ymmIdx := ymmIdx + 1;
         end;

         j._mov_reg_reg(gprRBX, gprR8);
         if tile > 0 then j._add_reg_imm(gprRBX, tile * 8 * inC * 8);
         j._xor_reg_reg(gprRAX, gprRAX);

         kLoop1Pos := j.Size;
         j._vbroadcastsd_ptr_indexed(ymm8, gprRCX, gprRAX, 8, 0);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(4, curTileSize - cOffset); if nChan = 3 then nChan := 2;
            if nChan = 4 then j._vfmadd231pd_ptr_reg(TymmRegister(ymmIdx), TymmRegister(8), gprRBX, cOffset*8)
            else if nChan >= 2 then j._vfmadd231pd_ptr_reg(TxmmRegister(ymmIdx), TxmmRegister(8), gprRBX, cOffset*8)
            else j._vfmadd231sd_ptr_reg(TxmmRegister(ymmIdx), TxmmRegister(8), gprRBX, cOffset*8);
            cOffset := cOffset + nChan; ymmIdx := ymmIdx + 1;
         end;

         j._add_reg_imm(gprRBX, 64);
         j._add_reg_imm(gprRAX, 1);
         j._cmp_reg_imm(gprRAX, inC);
         j._jump(flagsB, kLoop1Pos - j.Size);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(4, curTileSize - cOffset); if nChan = 3 then nChan := 2;
            if nChan = 4 then j._vmovupd_ptr_reg_reg(gprRDX, tile*64 + cOffset*8, TymmRegister(ymmIdx))
            else if nChan >= 2 then j._vmovups_ptr_reg_reg(gprRDX, tile*64 + cOffset*8, TxmmRegister(ymmIdx))
            else j._vmovsd_ptr_reg_reg(gprRDX, tile*64 + cOffset*8, TxmmRegister(ymmIdx));
            cOffset := cOffset + nChan; ymmIdx := ymmIdx + 1;
         end;
      end;
      j._add_reg_imm(gprRCX, inC * 8); j._add_reg_imm(gprRDX, outC * 8);
      j._sub_reg_imm(gprRDI, 1);
      j._jump(flagsNE, tailLoopPos - j.Size);

      // Epilogue
      var curEpiloguePos := j.Size;

      j._vzeroupper;

      for i := 0 to 9 do j._vmovups_ptr_reg(TxmmRegister(i + 6), gprRSP, i * 16); // VMOVUPS xmm(i+6), [rsp + i*16]
      j._add_reg_imm(gprRSP, 160);
      j._pop_reg(gprR15); j._pop_reg(gprR14); j._pop_reg(gprR13); j._pop_reg(gprR12); j._pop_reg(gprRDI); j._pop_reg(gprRBX);
      j._pop_reg(gprRBP); j._ret;

      b := j.ToBytes;

      // Patch forward jumps
      PInteger(@b[jmpTailPatch])^ := curTailPos - (jmpTailPatch + 4);
      PInteger(@b[jmpEpiloguePatch])^ := curEpiloguePos - (jmpEpiloguePatch + 4);

      allocSize := (Length(b) + 4095) and not 4095;
      ptr := VirtualAlloc(nil, allocSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      if ptr = nil then raise Exception.Create('Failed to allocate JIT memory');
      Move(b[0], ptr^, Length(b));
      FlushInstructionCache(GetCurrentProcess, ptr, Length(b));
      Result := TKCLCompiledKernel.Create(ptr, allocSize);
   finally j.Free; end;
end;

// Execute
//
class function TKCLWin64JITBackend.Execute(AKernel : TKCLKernel; ANode : TKCLNode; pIn, pOut, pWeights, pBias: PDouble; totalPixels: NativeInt; inC, outC : Integer) : Boolean;
begin
   Result := False;
   if not (cpuFMA in Win64CPUFeatures) or not (cpuAVX in Win64CPUFeatures) then Exit;

   TMonitor.Enter(AKernel);
   try
      var optData := TKCLOptimizationData(AKernel.OptimizationData);
      if optData = nil then begin
         optData := TKCLOptimizationData.Create; AKernel.OptimizationData := optData;
      end;
      var compiled : TKCLCompiledKernel;
      if not optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         compiled := Compile(AKernel, ANode, inC, outC);
         if compiled <> nil then begin
            SetLength(compiled.Weights, ((outC + 7) div 8) * 8 * inC);
            for var t := 0 to ((outC + 7) div 8) - 1 do begin
               for var k := 0 to inC - 1 do begin
                  for var c := 0 to 7 do begin
                     var outChan := t * 8 + c;
                     var val : Double := 0;
                     if outChan < outC then val := pWeights[outChan + k * outC];
                     compiled.Weights[(t * inC * 8) + (k * 8) + c] := val;
                  end;
               end;
            end;
            optData.CompiledKernels.Add(ANode, compiled);
         end;
      end;
      if compiled <> nil then begin
         var micro := TKCLMicroKernel(compiled.Code);
         micro(pIn, pOut, @compiled.Weights[0], pBias, totalPixels);
         Result := True;
      end;
   finally TMonitor.Exit(AKernel); end;
end;

end.
