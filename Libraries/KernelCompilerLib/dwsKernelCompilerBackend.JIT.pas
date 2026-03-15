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

{$I dws.inc}

{$IFNDEF WIN64_ASM}
   {$ERROR KCL JIT backend is only supported on Win64}
{$ENDIF}

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

   TKCLMicroKernel = procedure(pIn, pOut, pWeights, pBias, pResidual, pIntermediate: Pointer; totalPixels: NativeInt);

   TKCLActivation = (actNone, actReLU, actReLU6, actHardSwish);

   TKCLFusionInfo = record
      Activation : TKCLActivation;
      ActivationNode : TKCLNode;
      AddNode : TKCLNode;
      ResidualInput : TKCLNode; // the non-conv input of the Add
      HasMultipleConsumers : Boolean;
   end;

   TKCLCompiledKernel = class
   public
      Code : Pointer;
      Size : NativeInt; // VirtualAlloc size
      CodeSize : Integer; // Actual code bytes
      Weights : TSingleDynArray;
      Bias : TSingleDynArray;
      Activation : TKCLActivation;
      constructor Create(ACode : Pointer; ASize : NativeInt; ACodeSize : Integer);
      destructor Destroy; override;
   end;

   // 32-byte aligned workspace allocator for intra-layer temporaries.
   // Allocates a single large chunk and sub-allocates from it, avoiding
   // per-node dynamic array overhead and ensuring AVX alignment.
   TKCLAlignedWorkspace = class
   private
      FBasePtr : Pointer;    // Original allocation (unaligned)
      FAlignedPtr : PByte;   // 32-byte aligned start
      FCapacity : NativeInt; // Total usable bytes
      FOffset : NativeInt;   // Current allocation offset
   public
      constructor Create(ATotalBytes : NativeInt);
      destructor Destroy; override;
      function Allocate(ABytes : NativeInt) : Pointer;
      procedure Reset;
      property Capacity : NativeInt read FCapacity;
   end;

   TKCLBatchEntry = record
      Code : Pointer;
      pIn, pOut, pIn2, pParams, pIntermediate : Pointer;
      Count : NativeInt;
   end;
   TKCLBatchEntries = TArray<TKCLBatchEntry>;

   TKCLOptimizationData = class
   public
      CompiledKernels : TDictionary<TKCLNode, TKCLCompiledKernel>;
      FusionMap : TDictionary<TKCLNode, TKCLFusionInfo>;
      Workspace : TKCLAlignedWorkspace;
      Prepared : Boolean;
      constructor Create;
      destructor Destroy; override;
   end;

   TKCLWin64JITBackend = class
   private
      class procedure EmitPrologue(j : Tx86_64_WriteOnlyStream); static;
      class procedure EmitEpilogue(j : Tx86_64_WriteOnlyStream); static;
      class procedure EmitActivationSetup(j : Tx86_64_WriteOnlyStream; act : TKCLActivation); static;
      class procedure EmitActivationApply(j : Tx86_64_WriteOnlyStream; act : TKCLActivation; accReg : Integer); static;
      class procedure EmitVBroadcastSSIndexed(j : Tx86_64_WriteOnlyStream; destYMM : Integer; baseGPR : TgpRegister64); static;
      class function TransposeWeightsPointwise(pWeights : PDouble; inChannels, outChannels : Integer) : TSingleDynArray;
      class function TransposeWeightsKxK(pWeights : PDouble; inChannels, outChannels, K : Integer) : TSingleDynArray;
   public
      class function Compile(AKernel : TKCLKernel; ANode : TKCLNode;
         inChannels, outChannels : Integer; act : TKCLActivation = actNone; hasResidual : Boolean = False; residualBroadcast : Boolean = False; hasIntermediateOutput : Boolean = False) : TKCLCompiledKernel;
      class function CompileKxK(AKernel : TKCLKernel; ANode : TKCLNode;
         inChannels, outChannels, K, inW, stride : Integer; act : TKCLActivation = actNone; hasResidual : Boolean = False; residualBroadcast : Boolean = False; hasIntermediateOutput : Boolean = False) : TKCLCompiledKernel;
      class function CompileMap(AKernel : TKCLKernel; ANode : TKCLMapNode; act : TKCLActivation = actNone; hasIntermediateOutput: Boolean = False) : TKCLCompiledKernel;

      class function Execute(AKernel : TKCLKernel; ANode : TKCLNode;
         pIn, pOut, pWeights, pBias, pResidual, pIntermediate: Pointer; totalPixels: NativeInt;
         inChannels, outChannels : Integer; act : TKCLActivation = actNone;
         hasResidual : Boolean = False; residualBroadcast : Boolean = False) : Boolean;
      class function ExecuteKxK(AKernel : TKCLKernel; ANode : TKCLNode;
         pIn, pOut, pWeights, pBias, pResidual, pIntermediate: Pointer; totalPixels: NativeInt;
         inChannels, outChannels, K, inW, stride : Integer; act : TKCLActivation = actNone;
         hasResidual : Boolean = False; residualBroadcast : Boolean = False) : Boolean;
      class function ExecuteMap(AKernel : TKCLKernel; ANode : TKCLMapNode;
         pIn, pOut, pIn2, pParams, pIntermediate: Pointer; totalElements: NativeInt; act : TKCLActivation = actNone) : Boolean;

      class procedure PrepareGraph(AKernel : TKCLKernel; ASortedNodes : TList<TKCLNode>;
         ANodeToBufferIdx : TDictionary<TKCLNode, Integer>;
         ANodeDims : TArray<TKCLDimensions>;
         AFusionMap : TDictionary<TKCLNode, TKCLFusionInfo>);

      class procedure FlushBatch(const ABatch : TKCLBatchEntries; ACount : Integer);
      class function DumpGraph(AKernel : TKCLKernel) : string;
      class function HexDump(const ABytes : TBytes) : string;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

constructor TFixupLocationHelper.Create(aStream : TStream);
begin Stream := aStream; end;

function TFixupLocationHelper.GetLocation : Integer;
begin Result := Stream.Position; end;

// ------------------
// TKCLAlignedWorkspace
// ------------------

constructor TKCLAlignedWorkspace.Create(ATotalBytes : NativeInt);
begin
   // Allocate with 31 bytes extra for alignment
   FBasePtr := GetMemory(ATotalBytes + 31);
   FAlignedPtr := PByte((NativeUInt(FBasePtr) + 31) and not NativeUInt(31));
   FCapacity := ATotalBytes;
   FOffset := 0;
end;

destructor TKCLAlignedWorkspace.Destroy;
begin
   if FBasePtr <> nil then FreeMemory(FBasePtr);
   inherited;
end;

function TKCLAlignedWorkspace.Allocate(ABytes : NativeInt) : Pointer;
begin
   // Round up to 32-byte alignment
   ABytes := (ABytes + 31) and not NativeInt(31);
   if FOffset + ABytes > FCapacity then
      raise Exception.Create('KCL: Aligned workspace exhausted');
   Result := FAlignedPtr + FOffset;
   Inc(FOffset, ABytes);
end;

procedure TKCLAlignedWorkspace.Reset;
begin
   FOffset := 0;
end;

constructor TKCLCompiledKernel.Create(ACode : Pointer; ASize : NativeInt; ACodeSize : Integer);
begin Code := ACode; Size := ASize; CodeSize := ACodeSize; Activation := actNone; end;

destructor TKCLCompiledKernel.Destroy;
begin
   if Code <> nil then VirtualFree(Code, 0, MEM_RELEASE);
   inherited;
end;

constructor TKCLOptimizationData.Create;
begin
   CompiledKernels := TDictionary<TKCLNode, TKCLCompiledKernel>.Create;
   FusionMap := TDictionary<TKCLNode, TKCLFusionInfo>.Create;
end;

destructor TKCLOptimizationData.Destroy;
begin
   for var compiled in CompiledKernels.Values do compiled.Free;
   CompiledKernels.Free;
   FusionMap.Free;
   Workspace.Free;
   inherited;
end;

// ------------------
// TKCLWin64JITBackend — helpers
// ------------------

// EmitPrologue
//
class procedure TKCLWin64JITBackend.EmitPrologue(j : Tx86_64_WriteOnlyStream);
begin
   j._push_reg(gprRBP); j._mov_reg_reg(gprRBP, gprRSP);
   j._push_reg(gprRSI);
   j._push_reg(gprRBX); j._push_reg(gprRDI); j._push_reg(gprR12);
   j._push_reg(gprR13); j._push_reg(gprR14); j._push_reg(gprR15);
   j._sub_reg_imm(gprRSP, 168);
   for var i := 0 to 9 do
      j._vmovups_ptr_reg_reg(gprRSP, i * 16, TxmmRegister(i + 6));
end;

// EmitEpilogue
//
class procedure TKCLWin64JITBackend.EmitEpilogue(j : Tx86_64_WriteOnlyStream);
begin
   j._vzeroupper;
   for var i := 0 to 9 do
      j._vmovups_ptr_reg(TxmmRegister(i + 6), gprRSP, i * 16);
   j._add_reg_imm(gprRSP, 168);
   j._pop_reg(gprR15); j._pop_reg(gprR14); j._pop_reg(gprR13);
   j._pop_reg(gprR12); j._pop_reg(gprRDI); j._pop_reg(gprRBX);
   j._pop_reg(gprRSI);
   j._pop_reg(gprRBP); j._ret;
end;

// Load activation constants into dedicated registers:
//   ymm12 = 0.0  (ReLU, ReLU6, HardSwish)
//   ymm13 = 6.0  (ReLU6, HardSwish)
//   ymm14 = 3.0  (HardSwish)
//   ymm15 = 1/6  (HardSwish)
// Clobbers rax temporarily.
class procedure TKCLWin64JITBackend.EmitActivationSetup(j : Tx86_64_WriteOnlyStream; act : TKCLActivation);
begin
   case act of
      actReLU: begin
         j._vxorps(ymm12);
      end;
      actReLU6: begin
         j._vxorps(ymm12);
         j._mov_reg_imm(gprRAX, $40C00000); // FP32 6.0
         j._push_reg(gprRAX);
         j._vbroadcastss_ptr_reg(ymm13, gprRSP, 0);
         j._pop_reg(gprRAX);
      end;
      actHardSwish: begin
         j._vxorps(ymm12);
         j._mov_reg_imm(gprRAX, $40C00000); // 6.0
         j._push_reg(gprRAX);
         j._vbroadcastss_ptr_reg(ymm13, gprRSP, 0);
         j._pop_reg(gprRAX);
         j._mov_reg_imm(gprRAX, $40400000); // 3.0
         j._push_reg(gprRAX);
         j._vbroadcastss_ptr_reg(ymm14, gprRSP, 0);
         j._pop_reg(gprRAX);
         j._mov_reg_imm(gprRAX, $3E2AAAAB); // 1/6.0
         j._push_reg(gprRAX);
         j._vbroadcastss_ptr_reg(ymm15, gprRSP, 0);
         j._pop_reg(gprRAX);
      end;
   end;
end;

// Apply activation to a single accumulator register (ymm-width).
class procedure TKCLWin64JITBackend.EmitActivationApply(j : Tx86_64_WriteOnlyStream;
   act : TKCLActivation; accReg : Integer);
begin
   case act of
      actReLU:
         j._v_op_ps(xmm_maxps, TymmRegister(accReg), TymmRegister(accReg), ymm12);
      actReLU6: begin
         j._v_op_ps(xmm_maxps, TymmRegister(accReg), TymmRegister(accReg), ymm12);
         j._v_op_ps(xmm_minps, TymmRegister(accReg), TymmRegister(accReg), ymm13);
      end;
      actHardSwish: begin
         j._vaddps(ymm8, TymmRegister(accReg), ymm14);
         j._v_op_ps(xmm_maxps, ymm8, ymm8, ymm12);
         j._v_op_ps(xmm_minps, ymm8, ymm8, ymm13);
         j._vmulps(TymmRegister(accReg), TymmRegister(accReg), ymm8);
         j._vmulps(TymmRegister(accReg), TymmRegister(accReg), ymm15);
      end;
   end;
end;

class procedure TKCLWin64JITBackend.EmitVBroadcastSSIndexed(j : Tx86_64_WriteOnlyStream; destYMM : Integer; baseGPR : TgpRegister64);
begin
   // vbroadcastss ymm(destYMM), [baseGPR + rsi*4]
   // Map 2, L=1, W=0, pp=00
   j.WriteBytes([
      $C4,
      $E2 - Ord(destYMM >= 8)*$80 - $40*0 - Ord(baseGPR >= gprR8)*$20, // index is rsi which is < r8
      $7D,
      $18
   ]);
   // ModRM: mod=00 (ptr), reg=(destYMM and 7), rm=100 (SIB)
   // SIB: scale=10 (4), index=110 (rsi), base=(baseGPR and 7)
   j.WriteByte($04 + (destYMM and 7)*8);
   j.WriteByte($B0 + (Ord(baseGPR) and 7));
end;

// Transpose weights to [tile][inChannels][8] layout for pointwise (1x1) conv
class function TKCLWin64JITBackend.TransposeWeightsPointwise(
   pWeights : PDouble; inChannels, outChannels : Integer) : TSingleDynArray;
begin
   var tileCount := (outChannels + 7) div 8;
   SetLength(Result, tileCount * 8 * inChannels);
   for var t := 0 to tileCount - 1 do
      for var k := 0 to inChannels - 1 do
         for var c := 0 to 7 do begin
            var outChan := t * 8 + c;
            if outChan < outChannels then
               Result[(t * inChannels * 8) + (k * 8) + c] := pWeights[outChan + k * outChannels]
            else
               Result[(t * inChannels * 8) + (k * 8) + c] := 0;
         end;
end;

// Transpose weights to [K^2][tile][inChannels][8] layout for k*k conv
class function TKCLWin64JITBackend.TransposeWeightsKxK(
   pWeights : PDouble; inChannels, outChannels, K : Integer) : TSingleDynArray;
begin
   var tileCount := (outChannels + 7) div 8;
   var kSq := K * K;
   var tileStride := tileCount * inChannels * 8;
   SetLength(Result, kSq * tileStride);
   for var kp := 0 to kSq - 1 do
      for var t := 0 to tileCount - 1 do
         for var ic := 0 to inChannels - 1 do
            for var c := 0 to 7 do begin
               var outChan := t * 8 + c;
               if outChan < outChannels then
                  Result[kp * tileStride + (t * inChannels * 8) + (ic * 8) + c] :=
                     pWeights[(kp * inChannels + ic) * outChannels + outChan]
               else
                  Result[kp * tileStride + (t * inChannels * 8) + (ic * 8) + c] := 0;
            end;
end;

// ------------------
// Compile — pointwise (1x1) conv micro-kernel
// ------------------
// ABI: procedure(pIn:rcx, pOut:rdx, pWeights:r8, pBias:r9; totalPixels:[rbp+48])
// Internal FP32 engine: 8 elements/YMM (32 bytes)
class function TKCLWin64JITBackend.Compile(AKernel : TKCLKernel; ANode : TKCLNode;
   inChannels, outChannels : Integer; act : TKCLActivation; hasResidual : Boolean; residualBroadcast : Boolean; hasIntermediateOutput: Boolean) : TKCLCompiledKernel;
var
   j : Tx86_64_WriteOnlyStream;
   ptr : Pointer;
   jmpTailPatch, jmpEpiloguePatch : Integer;
   pixel4LoopPos, tailLoopPos, kLoopPos, kLoop1Pos : Integer;
   p, tileCount, tile, curTileSize, cOffset, ymmIdx : Integer;
   nChan, accReg, inReg : Integer;
   b : TBytes;
   allocSize : Integer;
begin
   j := Tx86_64_WriteOnlyStream.Create;
   try
      EmitPrologue(j);
      j._mov_reg_qword_ptr_reg(gprRDI, gprRBP, 64);
      if hasResidual then j._mov_reg_qword_ptr_reg(gprRSI, gprRBP, 48);
      tileCount := (outChannels + 7) div 8;

      if act <> actNone then
         EmitActivationSetup(j, act);

      // === 4-pixel main loop ===
      pixel4LoopPos := j.Size;
      j._cmp_reg_imm(gprRDI, 4);
      j._jump(flagsB, $10000); jmpTailPatch := j.Size - 4;

      // Use r10, r11, rax for extra pixel pointers (r10, r11 are scratch in Win64)
      j._mov_reg_reg(gprR10, gprRCX); j._add_reg_imm(gprR10, inChannels * 4);
      j._mov_reg_reg(gprR11, gprR10); j._add_reg_imm(gprR11, inChannels * 4);
      j._mov_reg_reg(gprRAX, gprR11); j._add_reg_imm(gprRAX, inChannels * 4);
      j._push_reg(gprRAX); // Pointer to 4th pixel on stack

      for tile := 0 to tileCount - 1 do begin
         curTileSize := Min(8, outChannels - tile * 8);

         // Load bias (FP32)
         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            for p := 0 to 3 do
               j._vxorps(TymmRegister(ymmIdx + p*2));

            if nChan > 4 then j._vmovups_ptr_reg(TymmRegister(ymmIdx), gprR9, tile*32 + cOffset*4)
            else j._vmovups_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*32 + cOffset*4);
            for p := 1 to 3 do
               j._vmovaps(TymmRegister(ymmIdx + p*2), TymmRegister(ymmIdx));
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;

         j._mov_reg_reg(gprRBX, gprR8);
         if tile > 0 then j._add_reg_imm(gprRBX, tile * 8 * inChannels * 4);
         j._xor_reg_reg(gprRSI, gprRSI); // RSI is loop counter (preserved, but we pushed it)
         
         kLoopPos := j.Size;

         j._vxorps(ymm8); j._vxorps(ymm9); j._vxorps(ymm10); j._vxorps(ymm11);
         EmitVBroadcastSSIndexed(j, 8, gprRCX);
         EmitVBroadcastSSIndexed(j, 9, gprR10);
         EmitVBroadcastSSIndexed(j, 10, gprR11);
         j._mov_reg_qword_ptr_reg(gprRAX, gprRSP, 0); // 4th pixel ptr
         EmitVBroadcastSSIndexed(j, 11, gprRAX);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            for p := 0 to 3 do begin
               accReg := ymmIdx + p*2; inReg := 8 + p;
               j._vfmadd231ps_ptr_reg(TymmRegister(accReg), TymmRegister(inReg), gprRBX, cOffset*4);
            end;
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;

         j._add_reg_imm(gprRBX, 32);
         j._add_reg_imm(gprRSI, 1);
         j._cmp_reg_imm(gprRSI, inChannels);
         j._jump(flagsB, kLoopPos - j.Size);

         // Activation + store for 4 pixels
         // Need rax for pixel 4 ptr again
         j._mov_reg_qword_ptr_reg(gprRSI, gprRSP, 0); // 4th pixel ptr
         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            for p := 0 to 3 do begin
               accReg := ymmIdx + p*2;
               var pOut : TgpRegister64 := gprRDX;
               case p of
                  0: pOut := gprRDX;
                  1: begin j._mov_reg_reg(gprRAX, gprRDX); j._add_reg_imm(gprRAX, outChannels * 4); pOut := gprRAX; end;
                  2: begin j._mov_reg_reg(gprRAX, gprRDX); j._add_reg_imm(gprRAX, outChannels * 8); pOut := gprRAX; end;
                  3: begin j._mov_reg_reg(gprRAX, gprRDX); j._add_reg_imm(gprRAX, outChannels * 12); pOut := gprRAX; end;
               end;

               if hasIntermediateOutput then begin
                  j._push_reg(gprRAX);
                  j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
                  if p > 0 then j._add_reg_imm(gprRAX, p * outChannels * 4);
                  if nChan = 8 then j._vmovups_ptr_reg_reg(gprRAX, tile*32 + cOffset*4, TymmRegister(accReg))
                  else begin
                     for var c := 0 to nChan - 1 do begin
                        if c = 0 then j._vmovss_ptr_reg_reg(gprRAX, tile*32 + cOffset*4, TxmmRegister(accReg))
                        else if c < 4 then j._vextractps_ptr_reg_imm(gprRAX, tile*32 + cOffset*4 + c*4, TxmmRegister(accReg), c)
                        else if c = 4 then begin
                           j._vextract128_high(xmm15, TymmRegister(accReg));
                           j._vmovss_ptr_reg_reg(gprRAX, tile*32 + cOffset*4 + c*4, xmm15);
                        end else begin
                           j._vextract128_high(xmm15, TymmRegister(accReg));
                           j._vextractps_ptr_reg_imm(gprRAX, tile*32 + cOffset*4 + c*4, xmm15, c - 4);
                        end;
                     end;
                  end;
                  j._pop_reg(gprRAX);
               end;
               if hasResidual then begin
                  j._push_reg(gprRAX);
                  j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 48);
                  if (not residualBroadcast) and (p > 0) then j._add_reg_imm(gprRAX, p * outChannels * 4);
                  if nChan > 4 then j._vmovups_ptr_reg(ymm7, gprRAX, tile*32 + cOffset*4)
                  else j._vmovups_ptr_reg(xmm7, gprRAX, tile*32 + cOffset*4);
                  j._vaddps(TymmRegister(accReg), TymmRegister(accReg), ymm7);
                  j._pop_reg(gprRAX);
               end;
               if act <> actNone then EmitActivationApply(j, act, accReg);
               if nChan = 8 then j._vmovups_ptr_reg_reg(pOut, tile*32 + cOffset*4, TymmRegister(accReg))
               else begin
                  for var c := 0 to nChan - 1 do begin
                     if c = 0 then j._vmovss_ptr_reg_reg(pOut, tile*32 + cOffset*4, TxmmRegister(accReg))
                     else if c < 4 then j._vextractps_ptr_reg_imm(pOut, tile*32 + cOffset*4 + c*4, TxmmRegister(accReg), c)
                     else if c = 4 then begin
                        j._vextract128_high(xmm15, TymmRegister(accReg));
                        j._vmovss_ptr_reg_reg(pOut, tile*32 + cOffset*4 + c*4, xmm15);
                     end else begin
                        j._vextract128_high(xmm15, TymmRegister(accReg));
                        j._vextractps_ptr_reg_imm(pOut, tile*32 + cOffset*4 + c*4, xmm15, c - 4);
                     end;
                  end;
               end;
            end;
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;
      end;

      j._pop_reg(gprRAX); // Pop 4th pixel ptr
      j._add_reg_imm(gprRCX, inChannels * 4 * 4);
      j._add_reg_imm(gprRDX, outChannels * 4 * 4);
      if hasIntermediateOutput then begin
         j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
         j._add_reg_imm(gprRAX, outChannels * 4 * 4);
         j._mov_qword_ptr_reg_reg(gprRBP, 56, gprRAX);
      end;
      if hasResidual and not residualBroadcast then begin
         j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 48);
         j._add_reg_imm(gprRAX, outChannels * 4 * 4);
         j._mov_qword_ptr_reg_reg(gprRBP, 48, gprRAX);
      end;
      j._sub_reg_imm(gprRDI, 4);
      j._cmp_reg_imm(gprRDI, 4);
      j._jump(flagsAE, pixel4LoopPos - j.Size);

      // === Tail loop (1 pixel) ===
      var curTailPos := j.Size;
      j._test_reg_reg(gprRDI, gprRDI);
      j._jump(flagsE, $10000); jmpEpiloguePatch := j.Size - 4;

      tailLoopPos := j.Size;
      for tile := 0 to tileCount - 1 do begin
         curTileSize := Min(8, outChannels - tile * 8);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            if nChan > 4 then j._vmovups_ptr_reg(TymmRegister(ymmIdx), gprR9, tile*32 + cOffset*4)
            else j._vmovups_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*32 + cOffset*4);
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;

         j._mov_reg_reg(gprRBX, gprR8);
         if tile > 0 then j._add_reg_imm(gprRBX, tile * 8 * inChannels * 4);
         j._xor_reg_reg(gprRAX, gprRAX);
         kLoop1Pos := j.Size;
         j._vbroadcastss_ptr_indexed(ymm8, gprRCX, gprRAX, 4, 0);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            if nChan = 8 then j._vfmadd231ps_ptr_reg(TymmRegister(ymmIdx), ymm8, gprRBX, cOffset*4)
            else j._vfmadd231ps_ptr_reg(TymmRegister(ymmIdx), TymmRegister(8), gprRBX, cOffset*4);
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;

         j._add_reg_imm(gprRBX, 32);
         j._add_reg_imm(gprRAX, 1);
         j._cmp_reg_imm(gprRAX, inChannels);
         j._jump(flagsB, kLoop1Pos - j.Size);

         // Activation + store for 1 pixel
         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            if hasIntermediateOutput then begin
               j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
               if nChan = 8 then j._vmovups_ptr_reg_reg(gprRAX, tile*32 + cOffset*4, TymmRegister(ymmIdx))
               else begin
                  for var c := 0 to nChan - 1 do begin
                     if c = 0 then j._vmovss_ptr_reg_reg(gprRAX, tile*32 + cOffset*4, TxmmRegister(ymmIdx))
                     else if c < 4 then j._vextractps_ptr_reg_imm(gprRAX, tile*32 + cOffset*4 + c*4, TxmmRegister(ymmIdx), c)
                     else if c = 4 then begin
                        j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                        j._vmovss_ptr_reg_reg(gprRAX, tile*32 + cOffset*4 + c*4, xmm15);
                     end else begin
                        j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                        j._vextractps_ptr_reg_imm(gprRAX, tile*32 + cOffset*4 + c*4, xmm15, c - 4);
                     end;
                  end;
               end;
            end;
            if hasResidual then begin
               if nChan > 4 then j._vmovups_ptr_reg(ymm8, gprRSI, tile*32 + cOffset*4)
               else j._vmovups_ptr_reg(xmm8, gprRSI, tile*32 + cOffset*4);
               j._vaddps(TymmRegister(ymmIdx), TymmRegister(ymmIdx), ymm8);
            end;
            if act <> actNone then EmitActivationApply(j, act, ymmIdx);
            if nChan = 8 then j._vmovups_ptr_reg_reg(gprRDX, tile*32 + cOffset*4, TymmRegister(ymmIdx))
            else begin
               for var c := 0 to nChan - 1 do begin
                  if c = 0 then j._vmovss_ptr_reg_reg(gprRDX, tile*32 + cOffset*4, TxmmRegister(ymmIdx))
                  else if c < 4 then j._vextractps_ptr_reg_imm(gprRDX, tile*32 + cOffset*4 + c*4, TxmmRegister(ymmIdx), c)
                  else if c = 4 then begin
                     j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                     j._vmovss_ptr_reg_reg(gprRDX, tile*32 + cOffset*4 + c*4, xmm15);
                  end else begin
                     j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                     j._vextractps_ptr_reg_imm(gprRDX, tile*32 + cOffset*4 + c*4, xmm15, c - 4);
                  end;
               end;
            end;
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;
      end;

      j._add_reg_imm(gprRCX, inChannels * 4);
      j._add_reg_imm(gprRDX, outChannels * 4);
      if hasResidual and not residualBroadcast then
         j._add_reg_imm(gprRSI, outChannels * 4);
      if hasIntermediateOutput then begin
         j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
         j._add_reg_imm(gprRAX, outChannels * 4);
         j._mov_qword_ptr_reg_reg(gprRBP, 56, gprRAX);
      end;
      j._sub_reg_imm(gprRDI, 1);
      j._jump(flagsNE, tailLoopPos - j.Size);

      var curEpiloguePos := j.Size;
      EmitEpilogue(j);

      b := j.ToBytes;
      PInteger(@b[jmpTailPatch])^ := curTailPos - (jmpTailPatch + 4);
      PInteger(@b[jmpEpiloguePatch])^ := curEpiloguePos - (jmpEpiloguePatch + 4);

      allocSize := (Length(b) + 4095) and not 4095;
      ptr := VirtualAlloc(nil, allocSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      Move(b[0], ptr^, Length(b));
      FlushInstructionCache(GetCurrentProcess, ptr, Length(b));
      Result := TKCLCompiledKernel.Create(ptr, allocSize, Length(b));
      Result.Activation := act;
   finally j.Free; end;
end;

// ------------------
// CompileKxK — k*k conv micro-kernel
// ------------------
class function TKCLWin64JITBackend.CompileKxK(AKernel : TKCLKernel; ANode : TKCLNode;
   inChannels, outChannels, K, inW, stride : Integer; act : TKCLActivation; hasResidual : Boolean; residualBroadcast : Boolean; hasIntermediateOutput: Boolean) : TKCLCompiledKernel;
var
   j : Tx86_64_WriteOnlyStream;
   ptr : Pointer;
   jmpEpiloguePatch, pixelLoopPos, kLoopPos : Integer;
   tileCount, tile, curTileSize, cOffset, ymmIdx, nChan : Integer;
   kp, ky, kx, inputOff : Integer;
   inRowStride, tileStride : Integer;
   woff : NativeInt;
   b : TBytes;
   allocSize : Integer;
begin
   j := Tx86_64_WriteOnlyStream.Create;
   try
      EmitPrologue(j);
      j._mov_reg_qword_ptr_reg(gprRDI, gprRBP, 64);
      if hasResidual then j._mov_reg_qword_ptr_reg(gprRSI, gprRBP, 48);

      tileCount := (outChannels + 7) div 8;
      inRowStride := inW * inChannels * 4;
      tileStride := tileCount * inChannels * 8;

      if act <> actNone then
         EmitActivationSetup(j, act);

      j._test_reg_reg(gprRDI, gprRDI);
      j._jump(flagsE, $10000); jmpEpiloguePatch := j.Size - 4;
      pixelLoopPos := j.Size;

      for tile := 0 to tileCount - 1 do begin
         curTileSize := Min(8, outChannels - tile * 8);

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            if nChan > 4 then j._vmovups_ptr_reg(TymmRegister(ymmIdx), gprR9, tile*32 + cOffset*4)
            else j._vmovups_ptr_reg(TxmmRegister(ymmIdx), gprR9, tile*32 + cOffset*4);
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;

         for kp := 0 to K * K - 1 do begin
            ky := kp div K; kx := kp mod K;
            inputOff := ky * inRowStride + kx * inChannels * 4;
            j._mov_reg_reg(gprR13, gprRCX);
            if inputOff > 0 then j._add_reg_imm(gprR13, inputOff);

            woff := NativeInt(kp) * tileStride * 4 + NativeInt(tile) * inChannels * 32;
            j._mov_reg_reg(gprRBX, gprR8);
            if woff > 0 then j._add_reg_imm(gprRBX, woff);

            j._xor_reg_reg(gprRAX, gprRAX);
            kLoopPos := j.Size;
            j._vbroadcastss_ptr_indexed(ymm8, gprR13, gprRAX, 4, 0);

            cOffset := 0; ymmIdx := 0;
            while cOffset < curTileSize do begin
               nChan := Min(8, curTileSize - cOffset);
               if nChan = 8 then j._vfmadd231ps_ptr_reg(TymmRegister(ymmIdx), ymm8, gprRBX, cOffset*4)
               else if nChan >= 4 then j._vfmadd231ps_ptr_reg(TxmmRegister(ymmIdx), TxmmRegister(8), gprRBX, cOffset*4)
               else j._vfmadd231ss_ptr_reg(TxmmRegister(ymmIdx), TxmmRegister(8), gprRBX, cOffset*4);
               Inc(cOffset, nChan);
               Inc(ymmIdx);
            end;

            j._add_reg_imm(gprRBX, 32);
            j._add_reg_imm(gprRAX, 1);
            j._cmp_reg_imm(gprRAX, inChannels);
            j._jump(flagsB, kLoopPos - j.Size);
         end;

         cOffset := 0; ymmIdx := 0;
         while cOffset < curTileSize do begin
            nChan := Min(8, curTileSize - cOffset);
            if hasIntermediateOutput then begin
               j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
               if nChan = 8 then j._vmovups_ptr_reg_reg(gprRAX, tile*32 + cOffset*4, TymmRegister(ymmIdx))
               else begin
                  for var c := 0 to nChan - 1 do begin
                     if c = 0 then j._vmovss_ptr_reg_reg(gprRAX, tile*32 + cOffset*4, TxmmRegister(ymmIdx))
                     else if c < 4 then j._vextractps_ptr_reg_imm(gprRAX, tile*32 + cOffset*4 + c*4, TxmmRegister(ymmIdx), c)
                     else if c = 4 then begin
                        j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                        j._vmovss_ptr_reg_reg(gprRAX, tile*32 + cOffset*4 + c*4, xmm15);
                     end else begin
                        j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                        j._vextractps_ptr_reg_imm(gprRAX, tile*32 + cOffset*4 + c*4, xmm15, c - 4);
                     end;
                  end;
               end;
            end;
            if hasResidual then begin
               if nChan > 4 then j._vmovups_ptr_reg(ymm8, gprRSI, tile*32 + cOffset*4)
               else j._vmovups_ptr_reg(xmm8, gprRSI, tile*32 + cOffset*4);
               j._vaddps(TymmRegister(ymmIdx), TymmRegister(ymmIdx), ymm8);
            end;
            if act <> actNone then EmitActivationApply(j, act, ymmIdx);
            if nChan = 8 then j._vmovups_ptr_reg_reg(gprRDX, tile*32 + cOffset*4, TymmRegister(ymmIdx))
            else begin
               for var c := 0 to nChan - 1 do begin
                  if c = 0 then j._vmovss_ptr_reg_reg(gprRDX, tile*32 + cOffset*4, TxmmRegister(ymmIdx))
                  else if c < 4 then j._vextractps_ptr_reg_imm(gprRDX, tile*32 + cOffset*4 + c*4, TxmmRegister(ymmIdx), c)
                  else if c = 4 then begin
                     j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                     j._vmovss_ptr_reg_reg(gprRDX, tile*32 + cOffset*4 + c*4, xmm15);
                  end else begin
                     j._vextract128_high(xmm15, TymmRegister(ymmIdx));
                     j._vextractps_ptr_reg_imm(gprRDX, tile*32 + cOffset*4 + c*4, xmm15, c - 4);
                  end;
               end;
            end;
            Inc(cOffset, nChan);
            Inc(ymmIdx);
         end;
      end;

      j._add_reg_imm(gprRCX, stride * inChannels * 4);
      j._add_reg_imm(gprRDX, outChannels * 4);
      if hasResidual and not residualBroadcast then
         j._add_reg_imm(gprRSI, outChannels * 4);
      if hasIntermediateOutput then begin
         j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
         j._add_reg_imm(gprRAX, outChannels * 4);
         j._mov_qword_ptr_reg_reg(gprRBP, 56, gprRAX);
      end;
      j._sub_reg_imm(gprRDI, 1);
      j._jump(flagsNE, pixelLoopPos - j.Size);

      var curEpiloguePos := j.Size;
      EmitEpilogue(j);

      b := j.ToBytes;
      PInteger(@b[jmpEpiloguePatch])^ := curEpiloguePos - (jmpEpiloguePatch + 4);

      allocSize := (Length(b) + 4095) and not 4095;
      ptr := VirtualAlloc(nil, allocSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      Move(b[0], ptr^, Length(b));
      FlushInstructionCache(GetCurrentProcess, ptr, Length(b));
      Result := TKCLCompiledKernel.Create(ptr, allocSize, Length(b));
      Result.Activation := act;
   finally j.Free; end;
end;

// ------------------
// Execute / ExecuteKxK — wrappers
// ------------------
class function TKCLWin64JITBackend.Execute(AKernel : TKCLKernel; ANode : TKCLNode;
   pIn, pOut, pWeights, pBias, pResidual, pIntermediate: Pointer; totalPixels: NativeInt;
   inChannels, outChannels : Integer; act : TKCLActivation;
   hasResidual, residualBroadcast : Boolean) : Boolean;
var
   optData : TKCLOptimizationData;
   compiled : TKCLCompiledKernel;
begin
   Result := False;
   if not (cpuFMA in Win64CPUFeatures) or not (cpuAVX in Win64CPUFeatures) then Exit;
   optData := TKCLOptimizationData(AKernel.OptimizationData);
   if (optData <> nil) and optData.Prepared then begin
      if optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         TKCLMicroKernel(compiled.Code)(pIn, pOut, @compiled.Weights[0], @compiled.Bias[0], pResidual, pIntermediate, totalPixels);
         Result := True;
      end;
      Exit;
   end;
   TMonitor.Enter(AKernel);
   try
      optData := TKCLOptimizationData(AKernel.OptimizationData);
      if optData = nil then begin optData := TKCLOptimizationData.Create; AKernel.OptimizationData := optData; end;
      if not optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         compiled := Compile(AKernel, ANode, inChannels, outChannels, act, hasResidual, residualBroadcast, pIntermediate <> nil);
         if compiled <> nil then begin
            compiled.Weights := TransposeWeightsPointwise(PDouble(pWeights), inChannels, outChannels);
            // Convert bias to Single, padded to 8 for SIMD safety
            var biasCount := (outChannels + 7) and not 7;
            SetLength(compiled.Bias, biasCount);
            for var i := 0 to outChannels - 1 do compiled.Bias[i] := PDouble(pBias)[i];
            for var i := outChannels to biasCount - 1 do compiled.Bias[i] := 0;
            optData.CompiledKernels.Add(ANode, compiled);
         end;
      end;
      if compiled <> nil then begin
         TKCLMicroKernel(compiled.Code)(pIn, pOut, @compiled.Weights[0], @compiled.Bias[0], pResidual, pIntermediate, totalPixels);
         Result := True;
      end;
   finally TMonitor.Exit(AKernel); end;
end;

class function TKCLWin64JITBackend.ExecuteKxK(AKernel : TKCLKernel; ANode : TKCLNode;
   pIn, pOut, pWeights, pBias, pResidual, pIntermediate: Pointer; totalPixels: NativeInt;
   inChannels, outChannels, K, inW, stride : Integer; act : TKCLActivation;
   hasResidual, residualBroadcast : Boolean) : Boolean;
var
   optData : TKCLOptimizationData;
   compiled : TKCLCompiledKernel;
begin
   Result := False;
   if not (cpuFMA in Win64CPUFeatures) or not (cpuAVX in Win64CPUFeatures) then Exit;
   optData := TKCLOptimizationData(AKernel.OptimizationData);
   if (optData <> nil) and optData.Prepared then begin
      if optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         TKCLMicroKernel(compiled.Code)(pIn, pOut, @compiled.Weights[0], @compiled.Bias[0], pResidual, pIntermediate, totalPixels);
         Result := True;
      end;
      Exit;
   end;
   TMonitor.Enter(AKernel);
   try
      optData := TKCLOptimizationData(AKernel.OptimizationData);
      if optData = nil then begin optData := TKCLOptimizationData.Create; AKernel.OptimizationData := optData; end;
      if not optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         compiled := CompileKxK(AKernel, ANode, inChannels, outChannels, K, inW, stride, act, hasResidual, residualBroadcast, pIntermediate <> nil);
         if compiled <> nil then begin
            compiled.Weights := TransposeWeightsKxK(PDouble(pWeights), inChannels, outChannels, K);
            // Convert bias to Single, padded to 8 for SIMD safety
            var biasCount := (outChannels + 7) and not 7;
            SetLength(compiled.Bias, biasCount);
            for var i := 0 to outChannels - 1 do compiled.Bias[i] := PDouble(pBias)[i];
            for var i := outChannels to biasCount - 1 do compiled.Bias[i] := 0;
            optData.CompiledKernels.Add(ANode, compiled);
         end;
      end;
      if compiled <> nil then begin
         TKCLMicroKernel(compiled.Code)(pIn, pOut, @compiled.Weights[0], @compiled.Bias[0], pResidual, pIntermediate, totalPixels);
         Result := True;
      end;
   finally TMonitor.Exit(AKernel); end;
end;

// ------------------
// CompileMap — pointwise Map operations (Add, Sub, Mul, ReLU, ReLU6, HardSwish, Dequantize)
// ------------------
class function TKCLWin64JITBackend.CompileMap(AKernel : TKCLKernel; ANode : TKCLMapNode; act : TKCLActivation = actNone; hasIntermediateOutput: Boolean = False) : TKCLCompiledKernel;
var
   j : Tx86_64_WriteOnlyStream;
   ptr : Pointer;
   b : TBytes;
   allocSize : Integer;
   jmpTailPatch, jmpEpiloguePatch, jmpTailDonePatch, pixel8LoopPos, tailLoopPos : Integer;
   curTailPos, curEpiloguePos : Integer;
   isUnary, hasPIn2 : Boolean;
begin
   isUnary := (ANode is TKCLReLUNode) or (ANode is TKCLReLU6Node) or (ANode is TKCLHardSwishNode);
   hasPIn2 := (ANode is TKCLAddNode) or (ANode is TKCLSubNode) or (ANode is TKCLMulNode);
   j := Tx86_64_WriteOnlyStream.Create;
   try
      EmitPrologue(j);
      j._mov_reg_qword_ptr_reg(gprRDI, gprRBP, 64);
      j._test_reg_reg(gprRDI, gprRDI);
      j._jump(flagsE, $10000); jmpEpiloguePatch := j.Size - 4;

      if isUnary then begin
         j._vxorps(ymm12);
         if (ANode is TKCLReLU6Node) or (ANode is TKCLHardSwishNode) then begin
            j._mov_reg_imm(gprRAX, $40C00000); // 6.0
            j._push_reg(gprRAX);
            j._vbroadcastss_ptr_reg(ymm13, gprRSP, 0);
            j._pop_reg(gprRAX);
         end;
         if ANode is TKCLHardSwishNode then begin
            j._mov_reg_imm(gprRAX, $40400000); // 3.0
            j._push_reg(gprRAX);
            j._vbroadcastss_ptr_reg(ymm14, gprRSP, 0);
            j._pop_reg(gprRAX);
            j._mov_reg_imm(gprRAX, $3E2AAAAB); // 1/6.0
            j._push_reg(gprRAX);
            j._vbroadcastss_ptr_reg(ymm15, gprRSP, 0);
            j._pop_reg(gprRAX);
         end;
      end else if act <> actNone then begin
         EmitActivationSetup(j, act);
      end else if ANode is TKCLDequantizeNode then begin
         j._vmovss_reg_ptr_reg(xmm12, gprR9, 0); j._vbroadcastss(ymm12, xmm12);
         j._vmovss_reg_ptr_reg(xmm13, gprR9, 4); j._vbroadcastss(ymm13, xmm13);
      end;

      j._cmp_reg_imm(gprRDI, 8);
      j._jump(flagsB, $10000); jmpTailPatch := j.Size - 4;

      pixel8LoopPos := j.Size;
      j._vmovups_ptr_reg(ymm0, gprRCX, 0);
      if ANode is TKCLAddNode then begin
         j._vmovups_ptr_reg(ymm1, gprR8, 0);
         j._vaddps(ymm0, ymm0, ymm1);
      end else if ANode is TKCLSubNode then begin
         j._vmovups_ptr_reg(ymm1, gprR8, 0);
         j._vsubps(ymm0, ymm0, ymm1);
      end else if ANode is TKCLMulNode then begin
         j._vmovups_ptr_reg(ymm1, gprR8, 0);
         j._vmulps(ymm0, ymm0, ymm1);
      end else if ANode is TKCLReLUNode then begin
         j._v_op_ps(xmm_maxps, ymm0, ymm0, ymm12);
      end else if ANode is TKCLReLU6Node then begin
         j._v_op_ps(xmm_maxps, ymm0, ymm0, ymm12);
         j._v_op_ps(xmm_minps, ymm0, ymm0, ymm13);
      end else if ANode is TKCLHardSwishNode then begin
         j._vaddps(ymm1, ymm0, ymm14);
         j._v_op_ps(xmm_maxps, ymm1, ymm1, ymm12);
         j._v_op_ps(xmm_minps, ymm1, ymm1, ymm13);
         j._vmulps(ymm0, ymm0, ymm1);
         j._vmulps(ymm0, ymm0, ymm15);
      end else if ANode is TKCLDequantizeNode then begin
         j._vsubps(ymm0, ymm0, ymm13);
         j._vmulps(ymm0, ymm0, ymm12);
      end;
      if hasIntermediateOutput then begin
         j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
         j._vmovups_ptr_reg_reg(gprRAX, 0, ymm0);
         j._add_reg_imm(gprRAX, 32);
         j._mov_qword_ptr_reg_reg(gprRBP, 56, gprRAX);
      end;
      if act <> actNone then EmitActivationApply(j, act, 0);
      j._vmovups_ptr_reg_reg(gprRDX, 0, ymm0);
      j._add_reg_imm(gprRCX, 32); j._add_reg_imm(gprRDX, 32);
      if hasPIn2 then j._add_reg_imm(gprR8, 32);
      j._sub_reg_imm(gprRDI, 8);
      j._cmp_reg_imm(gprRDI, 8);
      j._jump(flagsAE, pixel8LoopPos - j.Size);

      curTailPos := j.Size;
      j._test_reg_reg(gprRDI, gprRDI);
      j._jump(flagsE, $10000); jmpTailDonePatch := j.Size - 4;
      tailLoopPos := j.Size;
      j._vmovss_reg_ptr_reg(xmm0, gprRCX, 0);
      if ANode is TKCLAddNode then begin
         j._vmovss_reg_ptr_reg(xmm1, gprR8, 0);
         j._vaddps(ymm0, ymm0, ymm1);
      end else if ANode is TKCLSubNode then begin
         j._vmovss_reg_ptr_reg(xmm1, gprR8, 0);
         j._vsubps(ymm0, ymm0, ymm1);
      end else if ANode is TKCLMulNode then begin
         j._vmovss_reg_ptr_reg(xmm1, gprR8, 0);
         j._vmulps(ymm0, ymm0, ymm1);
      end else if ANode is TKCLReLUNode then begin
         j._v_op_ps(xmm_maxps, ymm0, ymm0, ymm12);
      end else if ANode is TKCLReLU6Node then begin
         j._v_op_ps(xmm_maxps, ymm0, ymm0, ymm12);
         j._v_op_ps(xmm_minps, ymm0, ymm0, ymm13);
      end else if ANode is TKCLHardSwishNode then begin
         j._vaddps(ymm1, ymm0, ymm14);
         j._v_op_ps(xmm_maxps, ymm1, ymm1, ymm12);
         j._v_op_ps(xmm_minps, ymm1, ymm1, ymm13);
         j._vmulps(ymm0, ymm0, ymm1);
         j._vmulps(ymm0, ymm0, ymm15);
      end else if ANode is TKCLDequantizeNode then begin
         j._vsubps(ymm0, ymm0, ymm13);
         j._vmulps(ymm0, ymm0, ymm12);
      end;
      if hasIntermediateOutput then begin
         j._mov_reg_qword_ptr_reg(gprRAX, gprRBP, 56);
         j._vmovss_ptr_reg_reg(gprRAX, 0, xmm0);
         j._add_reg_imm(gprRAX, 4);
         j._mov_qword_ptr_reg_reg(gprRBP, 56, gprRAX);
      end;
      if act <> actNone then EmitActivationApply(j, act, 0);
      j._vmovss_ptr_reg_reg(gprRDX, 0, xmm0);
      j._add_reg_imm(gprRCX, 4); j._add_reg_imm(gprRDX, 4);
      if hasPIn2 then j._add_reg_imm(gprR8, 4);
      j._sub_reg_imm(gprRDI, 1);
      j._jump(flagsNE, tailLoopPos - j.Size);

      curEpiloguePos := j.Size;
      EmitEpilogue(j);
      b := j.ToBytes;
      PInteger(@b[jmpTailPatch])^ := curTailPos - (jmpTailPatch + 4);
      PInteger(@b[jmpEpiloguePatch])^ := curEpiloguePos - (jmpEpiloguePatch + 4);
      PInteger(@b[jmpTailDonePatch])^ := curEpiloguePos - (jmpTailDonePatch + 4);
      allocSize := (Length(b) + 4095) and not 4095;
      ptr := VirtualAlloc(nil, allocSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      Move(b[0], ptr^, Length(b));
      FlushInstructionCache(GetCurrentProcess, ptr, Length(b));
      Result := TKCLCompiledKernel.Create(ptr, allocSize, Length(b));
      Result.Activation := act;
   finally j.Free; end;
end;

class function TKCLWin64JITBackend.ExecuteMap(AKernel : TKCLKernel; ANode : TKCLMapNode;
   pIn, pOut, pIn2, pParams, pIntermediate: Pointer; totalElements: NativeInt; act : TKCLActivation = actNone) : Boolean;
var
   optData : TKCLOptimizationData;
   compiled : TKCLCompiledKernel;
begin
   Result := False;
   if not (cpuAVX in Win64CPUFeatures) then Exit;
   optData := TKCLOptimizationData(AKernel.OptimizationData);
   if (optData <> nil) and optData.Prepared then begin
      if optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         TKCLMicroKernel(compiled.Code)(pIn, pOut, pIn2, pParams, nil, pIntermediate, totalElements);
         Result := True;
      end;
      Exit;
   end;
   TMonitor.Enter(AKernel);
   try
      optData := TKCLOptimizationData(AKernel.OptimizationData);
      if optData = nil then begin optData := TKCLOptimizationData.Create; AKernel.OptimizationData := optData; end;
      if not optData.CompiledKernels.TryGetValue(ANode, compiled) then begin
         if (ANode is TKCLAddNode) or (ANode is TKCLSubNode) or (ANode is TKCLMulNode)
            or (ANode is TKCLReLUNode) or (ANode is TKCLReLU6Node) or (ANode is TKCLHardSwishNode)
            or (ANode is TKCLDequantizeNode) then begin
            compiled := CompileMap(AKernel, ANode, act, pIntermediate <> nil);
            if compiled <> nil then optData.CompiledKernels.Add(ANode, compiled);
         end else Exit;
      end;
      if compiled <> nil then begin
         TKCLMicroKernel(compiled.Code)(pIn, pOut, pIn2, pParams, nil, pIntermediate, totalElements);
         Result := True;
      end;
   finally TMonitor.Exit(AKernel); end;
end;

class procedure TKCLWin64JITBackend.PrepareGraph(AKernel : TKCLKernel; ASortedNodes : TList<TKCLNode>;
   ANodeToBufferIdx : TDictionary<TKCLNode, Integer>; ANodeDims : TArray<TKCLDimensions>;
   AFusionMap : TDictionary<TKCLNode, TKCLFusionInfo>);
var
   optData : TKCLOptimizationData;
   node : TKCLNode;
   compiled : TKCLCompiledKernel;
   convNode : TKCLConv2DNode;
   mapNode : TKCLMapNode;
   fusion : TKCLFusionInfo;
   act : TKCLActivation;
   inIdx, inChannels, outChannels, inH, inW, outH, outW : Integer;
   pad_h, pad_top, pad_w, pad_left, hFirst, hLast, wFirst, wLast : Integer;
   hasFusion, hasResidual, residualBroadcast, hasIntermediateOutput : Boolean;
begin
   if not (cpuFMA in Win64CPUFeatures) or not (cpuAVX in Win64CPUFeatures) then Exit;
   TMonitor.Enter(AKernel);
   try
      optData := TKCLOptimizationData(AKernel.OptimizationData);
      if optData = nil then begin optData := TKCLOptimizationData.Create; AKernel.OptimizationData := optData; end;
      if optData.Prepared then Exit;
      optData.FusionMap.Clear;
      if AFusionMap <> nil then for var pair in AFusionMap do optData.FusionMap.Add(pair.Key, pair.Value);
      for var n := 0 to ASortedNodes.Count - 1 do begin
         node := ASortedNodes[n];
         if node is TKCLConv2DNode then begin
            convNode := TKCLConv2DNode(node);
            if optData.CompiledKernels.ContainsKey(node) then Continue;
            inIdx := ANodeToBufferIdx[convNode.Inputs[0]];
            inChannels := ANodeDims[inIdx][High(ANodeDims[inIdx])];
            outChannels := Length(convNode.Bias);
            act := actNone; hasResidual := False; residualBroadcast := False; hasIntermediateOutput := False;
            hasFusion := (AFusionMap <> nil) and AFusionMap.TryGetValue(node, fusion);
            if hasFusion then begin
               act := fusion.Activation; hasIntermediateOutput := fusion.HasMultipleConsumers;
               hasResidual := fusion.ResidualInput <> nil;
               if hasResidual then begin
                  var resDims := ANodeDims[ANodeToBufferIdx[fusion.ResidualInput]];
                  var convDims := ANodeDims[n];
                  var resTotal : NativeInt := 1; for var d := 0 to High(resDims) do resTotal := resTotal * resDims[d];
                  var outTotal : NativeInt := 1; for var d := 0 to High(convDims) do outTotal := outTotal * convDims[d];
                  residualBroadcast := resTotal < outTotal;
               end;
            end;
            if (convNode.KernelSize = 1) and (convNode.Stride = 1) then begin
               compiled := Compile(AKernel, node, inChannels, outChannels, act, hasResidual, residualBroadcast, hasIntermediateOutput);
               if compiled <> nil then begin
                  compiled.Weights := TransposeWeightsPointwise(PDouble(convNode.Weights), inChannels, outChannels);
                  var biasCount := (outChannels + 7) and not 7;
                  SetLength(compiled.Bias, biasCount);
                  for var i := 0 to outChannels - 1 do compiled.Bias[i] := convNode.Bias[i];
                  for var i := outChannels to biasCount - 1 do compiled.Bias[i] := 0;
                  optData.CompiledKernels.Add(node, compiled);
               end;
            end else if (convNode.KernelSize > 1) and (Length(ANodeDims[inIdx]) >= 3) then begin
               inH := ANodeDims[inIdx][High(ANodeDims[inIdx])-2]; inW := ANodeDims[inIdx][High(ANodeDims[inIdx])-1];
               outH := (inH + convNode.Stride - 1) div convNode.Stride; outW := (inW + convNode.Stride - 1) div convNode.Stride;
               pad_h := Max(0, (outH - 1) * convNode.Stride + convNode.KernelSize - inH); pad_top := pad_h div 2;
               pad_w := Max(0, (outW - 1) * convNode.Stride + convNode.KernelSize - inW); pad_left := pad_w div 2;
               hFirst := (pad_top + convNode.Stride - 1) div convNode.Stride; hLast := (inH + pad_top - convNode.KernelSize) div convNode.Stride;
               wFirst := (pad_left + convNode.Stride - 1) div convNode.Stride; wLast := (inW + pad_left - convNode.KernelSize) div convNode.Stride;
               if (hFirst <= hLast) and (wFirst <= wLast) then begin
                  compiled := CompileKxK(AKernel, node, inChannels, outChannels, convNode.KernelSize, inW, convNode.Stride, act, hasResidual, residualBroadcast, hasIntermediateOutput);
                  if compiled <> nil then begin
                     compiled.Weights := TransposeWeightsKxK(PDouble(convNode.Weights), inChannels, outChannels, convNode.KernelSize);
                     var biasCount := (outChannels + 7) and not 7;
                     SetLength(compiled.Bias, biasCount);
                     for var i := 0 to outChannels - 1 do compiled.Bias[i] := convNode.Bias[i];
                     for var i := outChannels to biasCount - 1 do compiled.Bias[i] := 0;
                     optData.CompiledKernels.Add(node, compiled);
                  end;
               end;
            end;
         end else if node is TKCLMapNode then begin
            mapNode := TKCLMapNode(node);
            if optData.CompiledKernels.ContainsKey(node) then Continue;
            if (mapNode is TKCLAddNode) or (mapNode is TKCLSubNode) or (mapNode is TKCLMulNode)
               or (mapNode is TKCLReLUNode) or (mapNode is TKCLReLU6Node) or (mapNode is TKCLHardSwishNode)
               or (mapNode is TKCLDequantizeNode) then begin
               act := actNone; hasIntermediateOutput := False;
               if (AFusionMap <> nil) and AFusionMap.TryGetValue(node, fusion) then begin
                  act := fusion.Activation; hasIntermediateOutput := fusion.HasMultipleConsumers;
               end;
               compiled := CompileMap(AKernel, mapNode, act, hasIntermediateOutput);
               if compiled <> nil then optData.CompiledKernels.Add(node, compiled);
            end;
         end;
      end;
      optData.Prepared := True;
   finally TMonitor.Exit(AKernel); end;
end;

class procedure TKCLWin64JITBackend.FlushBatch(const ABatch : TKCLBatchEntries; ACount : Integer);
begin
   for var i := 0 to ACount - 1 do
      TKCLMicroKernel(ABatch[i].Code)(ABatch[i].pIn, ABatch[i].pOut, ABatch[i].pIn2, ABatch[i].pParams, nil, ABatch[i].pIntermediate, ABatch[i].Count);
end;

class function TKCLWin64JITBackend.HexDump(const ABytes : TBytes) : string;
begin
   Result := '';
   for var i := 0 to High(ABytes) do begin
      if i > 0 then begin
         if (i mod 16) = 0 then Result := Result + sLineBreak
         else if (i mod 8) = 0 then Result := Result + '  '
         else Result := Result + ' ';
      end;
      Result := Result + IntToHex(ABytes[i], 2);
   end;
end;

class function TKCLWin64JITBackend.DumpGraph(AKernel : TKCLKernel) : string;
var
   optData : TKCLOptimizationData;
   node : TKCLNode;
   compiled : TKCLCompiledKernel;
   fusion : TKCLFusionInfo;
   s, fStr : string;
begin
   Result := '--- KCL Graph Dump ---' + #13#10;
   optData := TKCLOptimizationData(AKernel.OptimizationData);
   for var i := 0 to AKernel.Nodes.Count - 1 do begin
      node := AKernel.Nodes[i];
      s := Format('[%d] %s (%s)', [i, node.Name, node.ClassName]);
      if Length(node.Inputs) > 0 then begin
         s := s + ' Inputs:';
         for var j := 0 to High(node.Inputs) do begin
            var inIdx := AKernel.Nodes.IndexOf(node.Inputs[j]);
            s := s + ' [' + IntToStr(inIdx) + ']';
         end;
      end;
      if (optData <> nil) and optData.CompiledKernels.TryGetValue(node, compiled) then begin
         s := s + ' [JIT:' + IntToStr(compiled.CodeSize) + ' bytes]';
         if optData.FusionMap.TryGetValue(node, fusion) then begin
            fStr := ' [FUSED: ';
            case fusion.Activation of
               actReLU: fStr := fStr + 'ReLU';
               actReLU6: fStr := fStr + 'ReLU6';
               actHardSwish: fStr := fStr + 'HardSwish';
               actNone: fStr := fStr + 'None';
            end;
            if fusion.AddNode <> nil then fStr := fStr + ' + Residual';
            if fusion.HasMultipleConsumers then fStr := fStr + ' (Multi-Out)';
            fStr := fStr + ']';
            s := s + fStr;
         end;
         s := s + #13#10 + 'Code:' + #13#10;
         var bCode : TBytes; SetLength(bCode, compiled.CodeSize);
         System.Move(compiled.Code^, bCode[0], compiled.CodeSize);
         s := s + HexDump(bCode) + #13#10;
      end;
      Result := Result + s + #13#10;
   end;
   Result := Result + '----------------------' + #13#10;
end;

end.
