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
unit dwsKernelCompilerBackend.SSE2;

// Note: The SSE2 backend is designed as a fast, highly-optimized fallback
// for machines without advanced instruction sets (AVX/AVX2/FMA).
// For maximum performance on modern hardware, use the JIT backend.

{$I dws.inc}
{$POINTERMATH ON}

{$IFNDEF WIN64_ASM}
   {$ERROR KCL SSE2 backend is only supported on Win64}
{$ENDIF}

interface

uses
   System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
   dwsUtils, dwsKernelCompilerCommon, dwsKernelCompilerBackend.JIT;

type

   TKCLSSE2Backend = class
   protected
      // Execution state
      FKernel : TKCLKernel;
      FBuffers : TArray<TKCLStridedBufferDescriptor>;
      FNodeDims : TArray<TKCLDimensions>;
      FNodeTotalElements : TArray<NativeInt>;
      FNodeBuffers : TArray<TDoubleDynArray>;
      FNodeToBufferIdx : TDictionary<TKCLNode, Integer>;
      FSortedNodes : TList<TKCLNode>;
      FVisiting : TDictionary<TKCLNode, Boolean>;
      FVisited : TDictionary<TKCLNode, Boolean>;

      // Fusion state
      FFusionMap : TDictionary<TKCLNode, TKCLFusionInfo>;
      FFusedNodes : TDictionary<TKCLNode, Boolean>;

      function GetValue(const ABuf : TKCLStridedBufferDescriptor; const AIdx : TKCLDimensions) : Double;
      procedure SetValue(const ABuf : TKCLStridedBufferDescriptor; const AIdx : TKCLDimensions; AValue : Double);

      function FlatIndex(const AIdx : TKCLDimensions; const ADims : TKCLDimensions) : Integer;
      procedure IndexFromFlat(AFlat : Integer; const ADims : TKCLDimensions; var AIdx : TKCLDimensions);
      function IsContiguous(const ABuf : TKCLStridedBufferDescriptor) : Boolean;

      procedure VisitNode(ANode : TKCLNode);
      procedure VerifyOutputs;
      procedure AnalyzeFusion;

      procedure ProcessConstant(n : Integer; node : TKCLConstantNode);
      procedure ProcessInput(n : Integer; node : TKCLInputNode);
      procedure ProcessConcat(n : Integer; node : TKCLConcatNode); virtual;
      procedure ProcessMap(n : Integer; node : TKCLMapNode); virtual;
      procedure ProcessConv2D(n : Integer; node : TKCLConv2DNode); virtual;
      procedure ProcessConv2DSSE2(n : Integer; node : TKCLConv2DNode;
         act : TKCLActivation = actNone; pResidual : PDouble = nil);
      procedure ProcessConv2DTranspose(n : Integer; node : TKCLConv2DTransposeNode); virtual;
      procedure ProcessDepthwiseConv2D(n : Integer; node : TKCLDepthwiseConv2DNode); virtual;
      procedure ProcessSoftMax(n : Integer; node : TKCLSoftMaxNode); virtual;
      procedure ProcessResizeBilinear(n : Integer; node : TKCLResizeBilinearNode); virtual;
      procedure ProcessMaxPool2D(n : Integer; node : TKCLMaxPool2DNode); virtual;
      procedure ProcessGlobalAvgPool(n : Integer; node : TKCLGlobalAvgPoolNode); virtual;
      procedure ProcessMapFallback(n : Integer; node : TKCLMapNode);

      procedure ApplyActivationInPlace(pBuf : PDouble; count : NativeInt; act : TKCLActivation);

      procedure PrepareNodes; virtual;
      procedure FinalizeNodes; virtual;

   public
      constructor Create;
      destructor Destroy; override;
      procedure Execute(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
   end;

   TKCLJITBackend = class(TKCLSSE2Backend)
   private
      FBatch : TKCLBatchEntries;
      FBatchCount : Integer;
      procedure FlushMapBatch;
      procedure EnqueueMapBatch(compiled : TKCLCompiledKernel;
         p1, pRes, p2, pParams : PDouble; total : NativeInt);
   protected
      procedure PrepareNodes; override;
      procedure FinalizeNodes; override;
      procedure ProcessMap(n : Integer; node : TKCLMapNode); override;
      procedure ProcessConv2D(n : Integer; node : TKCLConv2DNode); override;
      procedure ProcessConv2DTranspose(n : Integer; node : TKCLConv2DTransposeNode); override;
      procedure ProcessConcat(n : Integer; node : TKCLConcatNode); override;
      procedure ProcessDepthwiseConv2D(n : Integer; node : TKCLDepthwiseConv2DNode); override;
      procedure ProcessSoftMax(n : Integer; node : TKCLSoftMaxNode); override;
      procedure ProcessResizeBilinear(n : Integer; node : TKCLResizeBilinearNode); override;
      procedure ProcessMaxPool2D(n : Integer; node : TKCLMaxPool2DNode); override;
      procedure ProcessGlobalAvgPool(n : Integer; node : TKCLGlobalAvgPoolNode); override;
   end;

procedure SSE2_CvtPS2PDContiguous(pSrc : PSingle; pDst : PDouble; count : NativeInt);
procedure SSE2_CvtPD2PSContiguous(pSrc : PDouble; pDst : PSingle; count : NativeInt);
function SSE2_DotProduct(p1, p2 : PDouble; count : NativeInt) : Double;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   PInt8 = ^ShortInt;

const
   G_EXP_LN2_INV : array [0..1] of Double = (1.44269504088896340, 1.44269504088896340);
   G_EXP_LN2_HI  : array [0..1] of Double = (0.693147180369123816, 0.693147180369123816);
   G_EXP_LN2_LO  : array [0..1] of Double = (1.9082149292705877e-10, 1.9082149292705877e-10);
   G_EXP_P0 : array [0..1] of Double = (1.0, 1.0);
   G_EXP_P1 : array [0..1] of Double = (1.0, 1.0);
   G_EXP_P2 : array [0..1] of Double = (0.5, 0.5);
   G_EXP_P3 : array [0..1] of Double = (0.16666666666666666, 0.16666666666666666);
   G_EXP_P4 : array [0..1] of Double = (0.041666666666666664, 0.041666666666666664);
   G_EXP_P5 : array [0..1] of Double = (0.008333333333333333, 0.008333333333333333);
   G_EXP_P6 : array [0..1] of Double = (0.0013888888888888889, 0.0013888888888888889);
   G_EXP_MIN_CLAMP : array [0..1] of Double = (-708.0, -708.0);
   G_EXP_MAX_CLAMP : array [0..1] of Double = (709.0, 709.0);

// ------------------
// ------------------ SSE2 ASM Helpers ------------------
// ------------------

// SSE2_Add
//
procedure SSE2_Add(p1, p2, pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; addpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; addsd xmm0, [rdx]; movsd [r8], xmm0
@done:
end;

// SSE2_Sub
//
procedure SSE2_Sub(p1, p2, pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; subpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; subsd xmm0, [rdx]; movsd [r8], xmm0
@done:
end;

// SSE2_Mul
//
procedure SSE2_Mul(p1, p2, pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; mulpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; mulsd xmm0, [rdx]; movsd [r8], xmm0
@done:
end;

// SSE2_Div
//
procedure SSE2_Div(p1, p2, pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; divpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; divsd xmm0, [rdx]; movsd [r8], xmm0
@done:
end;

// SSE2_ReLU
//
procedure SSE2_ReLU(p1, pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done
   xorpd xmm1, xmm1
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; maxpd xmm0, xmm1; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   movsd xmm0, [rcx]; maxsd xmm0, xmm1; movsd [rdx], xmm0
@done:
end;

// SSE2_AddScaled
//
procedure SSE2_AddScaled(pSrc : PDouble; scalar : Double; pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   movsd xmm2, xmm1; unpcklpd xmm2, xmm2
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; mulpd xmm0, xmm2; movupd xmm3, [r8]; addpd xmm3, xmm0; movupd [r8], xmm3
   add rcx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; mulsd xmm0, xmm2; addsd xmm0, [r8]; movsd [r8], xmm0
@done:
end;

// SSE2_MulAdd
//
procedure SSE2_MulAdd(pSrc, pWeights, pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; mulpd xmm0, xmm1; movupd xmm2, [r8]; addpd xmm2, xmm0; movupd [r8], xmm2
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; mulsd xmm0, [rdx]; addsd xmm0, [r8]; movsd [r8], xmm0
@done:
end;

// SSE2_Max
//
procedure SSE2_Max(pSrc, pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; maxpd xmm0, [rdx]; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   movsd xmm0, [rcx]; maxsd xmm0, [rdx]; movsd [rdx], xmm0
@done:
end;

// SSE2_Copy
//
procedure SSE2_Copy(pSrc, pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   movsd xmm0, [rcx]; movsd [rdx], xmm0
@done:
end;

// SSE2_CvtPS2PDContiguous
//
procedure SSE2_CvtPS2PDContiguous(pSrc : PSingle; pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   cvtps2pd xmm0, qword ptr [rcx]; movupd [rdx], xmm0
   add rcx, 8; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   cvtss2sd xmm0, dword ptr [rcx]; movsd [rdx], xmm0
@done:
end;

// SSE2_CvtPD2PSContiguous
//
procedure SSE2_CvtPD2PSContiguous(pSrc : PDouble; pDst : PSingle; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   cvtpd2ps xmm0, [rcx]; movlps [rdx], xmm0
   add rcx, 16; add rdx, 8; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   cvtsd2ss xmm0, [rcx]; movss dword ptr [rdx], xmm0
@done:
end;

// SSE2_ReLU6
//
procedure SSE2_ReLU6(p1, pRes : PDouble; count : NativeInt);
const
   c0 : array [0..1] of Double = (0.0, 0.0);
   c6 : array [0..1] of Double = (6.0, 6.0);
asm
   .noframe
   test r8, r8; jle @done
   movupd xmm1, [c0]; movupd xmm2, [c6]
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; maxpd xmm0, xmm1; minpd xmm0, xmm2; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   movsd xmm0, [rcx]; maxsd xmm0, xmm1; minsd xmm0, xmm2; movsd [rdx], xmm0
@done:
end;

// SSE2_HardSigmoid
//
procedure SSE2_HardSigmoid(p1, pRes : PDouble; count : NativeInt);
const
   c0 : array [0..1] of Double = (0.0, 0.0);
   c1 : array [0..1] of Double = (1.0, 1.0);
   c3 : array [0..1] of Double = (3.0, 3.0);
   cInv6 : array [0..1] of Double = (0.16666666666666666, 0.16666666666666666);
asm
   .noframe
   test r8, r8; jle @done
   movupd xmm1, [c0]; movupd xmm2, [c1]; movupd xmm3, [c3]; movupd xmm4, [cInv6]
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; addpd xmm0, xmm3; mulpd xmm0, xmm4; maxpd xmm0, xmm1; minpd xmm0, xmm2; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   movsd xmm0, [rcx]; addsd xmm0, xmm3; mulsd xmm0, xmm4; maxsd xmm0, xmm1; minsd xmm0, xmm2; movsd [rdx], xmm0
@done:
end;

// SSE2_HardSwish
//
procedure SSE2_HardSwish(p1, pRes : PDouble; count : NativeInt);
const
   c0 : array [0..1] of Double = (0.0, 0.0);
   c3 : array [0..1] of Double = (3.0, 3.0);
   c6 : array [0..1] of Double = (6.0, 6.0);
   cInv6 : array [0..1] of Double = (0.16666666666666666, 0.16666666666666666);
asm
   .noframe
   test r8, r8; jle @done
   movupd xmm1, [c0]; movupd xmm2, [c3]; movupd xmm3, [c6]; movupd xmm4, [cInv6]
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movapd xmm5, xmm0; addpd xmm0, xmm2; maxpd xmm0, xmm1; minpd xmm0, xmm3; mulpd xmm0, xmm5; mulpd xmm0, xmm4; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done
   movsd xmm0, [rcx]; movapd xmm5, xmm0; addsd xmm0, xmm2; maxsd xmm0, xmm1; minsd xmm0, xmm3; mulsd xmm0, xmm5; mulsd xmm0, xmm4; movsd [rdx], xmm0
@done:
end;

// SSE2_Exp
//
procedure SSE2_Exp(p1, pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done
   push rsi; lea r9, [G_EXP_LN2_INV]; lea r10, [G_EXP_LN2_HI]; lea r11, [G_EXP_LN2_LO]
   mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; lea rsi, [G_EXP_MIN_CLAMP]; movupd xmm1, [rsi]; maxpd xmm0, xmm1
   lea rsi, [G_EXP_MAX_CLAMP]; movupd xmm1, [rsi]; minpd xmm0, xmm1
   movupd xmm1, [r9]; mulpd xmm1, xmm0; cvtpd2dq xmm2, xmm1; cvtdq2pd xmm1, xmm2
   movapd xmm3, xmm1; movupd xmm4, [r10]; mulpd xmm3, xmm4; subpd xmm0, xmm3
   movapd xmm3, xmm1; movupd xmm4, [r11]; mulpd xmm3, xmm4; subpd xmm0, xmm3 
   lea rsi, [G_EXP_P6]; movupd xmm3, [rsi]; mulpd xmm3, xmm0
   lea rsi, [G_EXP_P5]; movupd xmm4, [rsi]; addpd xmm3, xmm4; mulpd xmm3, xmm0
   lea rsi, [G_EXP_P4]; movupd xmm4, [rsi]; addpd xmm3, xmm4; mulpd xmm3, xmm0
   lea rsi, [G_EXP_P3]; movupd xmm4, [rsi]; addpd xmm3, xmm4; mulpd xmm3, xmm0
   lea rsi, [G_EXP_P2]; movupd xmm4, [rsi]; addpd xmm3, xmm4; mulpd xmm3, xmm0
   lea rsi, [G_EXP_P1]; movupd xmm4, [rsi]; addpd xmm3, xmm4; mulpd xmm3, xmm0
   lea rsi, [G_EXP_P0]; movupd xmm4, [rsi]; addpd xmm3, xmm4
   movd r9d, xmm2; psrldq xmm2, 4; movd r10d, xmm2
   movsxd rsi, r9d; add rsi, 1023; shl rsi, 52; movq xmm1, rsi
   movsxd rsi, r10d; add rsi, 1023; shl rsi, 52; movq xmm4, rsi; punpcklqdq xmm1, xmm4; mulpd xmm3, xmm1
   movupd [rdx], xmm3
   lea r9, [G_EXP_LN2_INV]; lea r10, [G_EXP_LN2_HI]; lea r11, [G_EXP_LN2_LO]
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @finish
   movsd xmm0, [rcx]; lea rsi, [G_EXP_MIN_CLAMP]; movsd xmm1, [rsi]; maxsd xmm0, xmm1
   lea rsi, [G_EXP_MAX_CLAMP]; movsd xmm1, [rsi]; minsd xmm0, xmm1
   movsd xmm1, [r9]; mulsd xmm1, xmm0; cvtsd2si rax, xmm1; cvtsi2sd xmm1, rax
   movsd xmm3, xmm1; mulsd xmm3, [r10]; subsd xmm0, xmm3
   movsd xmm3, xmm1; mulsd xmm3, [r11]; subsd xmm0, xmm3
   lea rsi, [G_EXP_P6]; movsd xmm3, [rsi]; mulsd xmm3, xmm0
   lea rsi, [G_EXP_P5]; movsd xmm4, [rsi]; addsd xmm3, xmm4; mulsd xmm3, xmm0
   lea rsi, [G_EXP_P4]; movsd xmm4, [rsi]; addsd xmm3, xmm4; mulsd xmm3, xmm0
   lea rsi, [G_EXP_P3]; movsd xmm4, [rsi]; addsd xmm3, xmm4; mulsd xmm3, xmm0
   lea rsi, [G_EXP_P2]; movsd xmm4, [rsi]; addsd xmm3, xmm4; mulsd xmm3, xmm0
   lea rsi, [G_EXP_P1]; movsd xmm4, [rsi]; addsd xmm3, xmm4; mulsd xmm3, xmm0
   lea rsi, [G_EXP_P0]; movsd xmm4, [rsi]; addsd xmm3, xmm4
   add rax, 1023; shl rax, 52; movq xmm1, rax; mulsd xmm3, xmm1; movsd [rdx], xmm3
@finish:
   pop rsi
@done:
end;

// SSE2_Sigmoid
//
procedure SSE2_Sigmoid(p1, pRes : PDouble; count : NativeInt);
var i : NativeInt;
begin
   if count <= 0 then Exit;
   for i := 0 to count - 1 do pRes[i] := -p1[i];
   SSE2_Exp(pRes, pRes, count);
   for i := 0 to count - 1 do pRes[i] := 1.0 / (1.0 + pRes[i]);
end;

// SSE2_MulScaled
//
procedure SSE2_MulScaled(p1 : PDouble; scale : Double; pRes : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done
   unpcklpd xmm1, xmm1
   mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; mulpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done
   movsd xmm0, [rcx]; mulsd xmm0, xmm1; movsd [r8], xmm0
@done:
end;

// SSE2_BilinearInterpolate
//
procedure SSE2_BilinearInterpolate(p00, p01, p10, p11, pRes : PDouble; fw, fh : Double; count : NativeInt);
asm
   .noframe
   mov rax, [rsp+40] // pRes
   mov r10, [rsp+64] // count
   test r10, r10; jle @done
   movsd xmm4, [rsp+48] // fw
   unpcklpd xmm4, xmm4
   movsd xmm5, [rsp+56] // fh
   unpcklpd xmm5, xmm5
   
   mov r11, r10; shr r11, 1; jz @tail
@loop:
   movupd xmm0, [rcx]    // p00
   movupd xmm1, [rdx]    // p01
   subpd xmm1, xmm0      // p01-p00
   mulpd xmm1, xmm4      // (p01-p00)*fw
   addpd xmm0, xmm1      // v0 = p00 + (p01-p00)*fw
   
   movupd xmm2, [r8]     // p10
   movupd xmm3, [r9]     // p11
   subpd xmm3, xmm2      // p11-p10
   mulpd xmm3, xmm4      // (p11-p10)*fw
   addpd xmm2, xmm3      // v1 = p10 + (p11-p10)*fw
   
   subpd xmm2, xmm0      // v1-v0
   mulpd xmm2, xmm5      // (v1-v0)*fh
   addpd xmm0, xmm2      // val = v0 + (v1-v0)*fh
   
   movupd [rax], xmm0
   
   add rcx, 16; add rdx, 16; add r8, 16; add r9, 16; add rax, 16; dec r11; jnz @loop
@tail:
   and r10, 1; jz @done
   movsd xmm0, [rcx]
   movsd xmm1, [rdx]
   subsd xmm1, xmm0
   mulsd xmm1, xmm4
   addsd xmm0, xmm1
   
   movsd xmm2, [r8]
   movsd xmm3, [r9]
   subsd xmm3, xmm2
   mulsd xmm3, xmm4
   addsd xmm2, xmm3
   
   subsd xmm2, xmm0
   mulsd xmm2, xmm5
   addsd xmm0, xmm2
   
   movsd [rax], xmm0
@done:
end;

// SSE2_MaxVector
//
procedure SSE2_MaxVector(pSrc : PDouble; count : NativeInt; var maxV : Double);
asm
   .noframe
   test rdx, rdx; jle @done
   movsd xmm0, [rcx]; unpcklpd xmm0, xmm0
   mov rax, rdx; shr rax, 1; jz @tail
@loop:
   movupd xmm1, [rcx]; maxpd xmm0, xmm1
   add rcx, 16; dec rax; jnz @loop
@tail:
   movhlps xmm1, xmm0; maxsd xmm0, xmm1
   and rdx, 1; jz @finish
   maxsd xmm0, [rcx]
@finish:
   movsd [r8], xmm0
@done:
end;

// SSE2_SumVector
//
procedure SSE2_SumVector(pSrc : PDouble; count : NativeInt; var sum : Double);
asm
   .noframe
   xorpd xmm0, xmm0; test rdx, rdx; jle @done
   mov rax, rdx; shr rax, 1; jz @tail
@loop:
   movupd xmm1, [rcx]; addpd xmm0, xmm1
   add rcx, 16; dec rax; jnz @loop
@tail:
   movhlps xmm1, xmm0; addsd xmm0, xmm1
   and rdx, 1; jz @finish
   addsd xmm0, [rcx]
@finish:
   movsd [r8], xmm0
@done:
end;

function SSE2_DotProduct(p1, p2 : PDouble; count : NativeInt) : Double;
asm
   .noframe
   xorpd xmm0, xmm0
   test r8, r8; jle @done
   mov rax, r8; shr rax, 1; jz @tail
 @loop:
   movupd xmm1, [rcx]; movupd xmm2, [rdx]; mulpd xmm1, xmm2; addpd xmm0, xmm1
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
 @tail:
   and r8, 1; jz @reduce
   movsd xmm1, [rcx]; mulsd xmm1, [rdx]; addsd xmm0, xmm1
 @reduce:
   movhlps xmm1, xmm0; addsd xmm0, xmm1
 @done:
end;

// ------------------
// ------------------ TKCLSSE2Backend ------------------
// ------------------

// Create
//
constructor TKCLSSE2Backend.Create;
begin
   inherited Create;
end;

// Destroy
//
destructor TKCLSSE2Backend.Destroy;
begin
   inherited;
end;

// GetValue
//
function TKCLSSE2Backend.GetValue(const ABuf : TKCLStridedBufferDescriptor; const AIdx : TKCLDimensions) : Double;
var
   idx : Integer; offset : NativeInt; p : PByte;
begin
   offset := 0;
   for idx := 0 to High(AIdx) do offset := offset + AIdx[idx] * ABuf.Strides[idx];
   p := PByte(ABuf.BasePointer) + offset;
   case ABuf.DataType of
      dtInt8 : Result := PInt8(p)^;
      dtFloat16 : Result := HalfToFloat(PHalfFloat(p)^);
      dtFloat32 : Result := PSingle(p)^;
   else Result := 0; end;
end;

// SetValue
//
procedure TKCLSSE2Backend.SetValue(const ABuf : TKCLStridedBufferDescriptor; const AIdx : TKCLDimensions; AValue : Double);
var
   idx : Integer; offset : NativeInt; p : PByte;
begin
   offset := 0;
   for idx := 0 to High(AIdx) do offset := offset + AIdx[idx] * ABuf.Strides[idx];
   p := PByte(ABuf.BasePointer) + offset;
   case ABuf.DataType of
      dtInt8 : PInt8(p)^ := Max(-128, Min(127, Round(AValue)));
      dtFloat16 : PHalfFloat(p)^ := FloatToHalf(AValue);
      dtFloat32 : PSingle(p)^ := AValue;
   end;
end;

// FlatIndex
//
function TKCLSSE2Backend.FlatIndex(const AIdx, ADims : TKCLDimensions) : Integer;
var idx, mult : Integer;
begin
   Result := 0; mult := 1;
   for idx := High(AIdx) downto 0 do begin Result := Result + AIdx[idx] * mult; mult := mult * ADims[idx]; end;
end;

// IndexFromFlat
//
procedure TKCLSSE2Backend.IndexFromFlat(AFlat : Integer; const ADims : TKCLDimensions; var AIdx : TKCLDimensions);
var idx : Integer;
begin
   if Length(AIdx) <> Length(ADims) then SetLength(AIdx, Length(ADims));
   for idx := High(AIdx) downto 0 do begin
      if ADims[idx] > 0 then begin AIdx[idx] := AFlat mod ADims[idx]; AFlat := AFlat div ADims[idx]; end else AIdx[idx] := 0;
   end;
end;

// IsContiguous
//
function TKCLSSE2Backend.IsContiguous(const ABuf : TKCLStridedBufferDescriptor) : Boolean;
var idx : Integer; sz : NativeInt;
begin
   sz := 1;
   case ABuf.DataType of dtInt8 : sz := 1; dtFloat16 : sz := 2; dtFloat32 : sz := 4; end;
   for idx := High(ABuf.Dimensions) downto 0 do begin if ABuf.Strides[idx] <> sz then Exit(False); sz := sz * ABuf.Dimensions[idx]; end;
   Result := True;
end;

// VisitNode
//
procedure TKCLSSE2Backend.VisitNode(ANode : TKCLNode);
var idx : Integer;
begin
   if FVisited.ContainsKey(ANode) then Exit;
   if FVisiting.ContainsKey(ANode) then raise EdwsKCLException.Create('KCL: Cycle');
   FVisiting.Add(ANode, True);
   for idx := 0 to High(ANode.Inputs) do VisitNode(ANode.Inputs[idx]);
   FVisiting.Remove(ANode);
   FVisited.Add(ANode, True);
   FSortedNodes.Add(ANode);
end;

// AnalyzeFusion
//
procedure TKCLSSE2Backend.AnalyzeFusion;

   function NodeToActivation(node : TKCLNode) : TKCLActivation;
   begin
      if node is TKCLReLUNode then Result := actReLU
      else if node is TKCLReLU6Node then Result := actReLU6
      else if node is TKCLHardSwishNode then Result := actHardSwish
      else Result := actNone;
   end;

   function IsOutputNode(node : TKCLNode) : Boolean;
   begin
      for var i := 0 to High(FKernel.Outputs) do
         if FKernel.Outputs[i] = node then Exit(True);
      Result := False;
   end;

var
   consumerCount : TDictionary<TKCLNode, Integer>;
   consumers : TDictionary<TKCLNode, TKCLNode>;
   node, consumer, addConsumer : TKCLNode;
   fusion : TKCLFusionInfo;
   act : TKCLActivation;
   i, j : Integer;
begin
   FFusionMap := TDictionary<TKCLNode, TKCLFusionInfo>.Create;
   FFusedNodes := TDictionary<TKCLNode, Boolean>.Create;

   // Build consumer maps: count how many nodes consume each node,
   // and if exactly one, record which node that is
   consumerCount := TDictionary<TKCLNode, Integer>.Create;
   consumers := TDictionary<TKCLNode, TKCLNode>.Create;
   try
      for i := 0 to FSortedNodes.Count - 1 do begin
         node := FSortedNodes[i];
         for j := 0 to High(node.Inputs) do begin
            var inputNode := node.Inputs[j];
            var cnt : Integer;
            if consumerCount.TryGetValue(inputNode, cnt) then begin
               consumerCount[inputNode] := cnt + 1;
               consumers.Remove(inputNode); // more than one consumer
            end else begin
               consumerCount.Add(inputNode, 1);
               consumers.Add(inputNode, node);
            end;
         end;
      end;

      // Detect fusion patterns for Conv2D nodes
      for i := 0 to FSortedNodes.Count - 1 do begin
         node := FSortedNodes[i];
         if not (node is TKCLConv2DNode) then Continue;

         // Check if Conv2D has exactly one consumer
         if not consumers.TryGetValue(node, consumer) then Continue;

         // Pattern 1: Conv2D + Activation
         act := NodeToActivation(consumer);
         if act <> actNone then begin
            fusion.Activation := act;
            fusion.ActivationNode := consumer;
            fusion.AddNode := nil;
            fusion.ResidualInput := nil;
            FFusionMap.Add(node, fusion);
            FFusedNodes.Add(consumer, True);
            Continue;
         end;

         // Pattern 2: Conv2D + Add + Activation
         if consumer is TKCLAddNode then begin
            // Determine which Add input is the Conv2D and which is the residual
            var addNode := TKCLAddNode(consumer);
            var residualInput : TKCLNode;
            if addNode.Inputs[0] = node then
               residualInput := addNode.Inputs[1]
            else if addNode.Inputs[1] = node then
               residualInput := addNode.Inputs[0]
            else Continue;

            // Residual must be computed before the Conv2D in topological order
            var resPos := FSortedNodes.IndexOf(residualInput);
            if resPos >= i then Continue;

            // The Add must also be a single-use node (not an output itself)
            if not consumers.TryGetValue(addNode, addConsumer) then Continue;
            if IsOutputNode(addNode) then Continue;

            act := NodeToActivation(addConsumer);
            if act <> actNone then begin
               fusion.Activation := act;
               fusion.ActivationNode := addConsumer;
               fusion.AddNode := addNode;
               fusion.ResidualInput := residualInput;
               FFusionMap.Add(node, fusion);
               FFusedNodes.Add(addNode, True);
               FFusedNodes.Add(addConsumer, True);
            end;
         end;
      end;
   finally
      consumerCount.Free;
      consumers.Free;
   end;
end;

// ApplyActivationInPlace
//
procedure TKCLSSE2Backend.ApplyActivationInPlace(pBuf : PDouble; count : NativeInt; act : TKCLActivation);
begin
   case act of
      actReLU: SSE2_ReLU(pBuf, pBuf, count);
      actReLU6: SSE2_ReLU6(pBuf, pBuf, count);
      actHardSwish: SSE2_HardSwish(pBuf, pBuf, count);
   end;
end;

// PrepareNodes — virtual hook for subclasses to pre-compile before execution
//
procedure TKCLSSE2Backend.PrepareNodes;
begin
   // Base SSE2 backend: no preparation needed
end;

// FinalizeNodes — virtual hook for subclasses to finalize before output
//
procedure TKCLSSE2Backend.FinalizeNodes;
begin
   // Base SSE2 backend: no finalization needed
end;

// VerifyOutputs
//
procedure TKCLSSE2Backend.VerifyOutputs;
var
   j, i, outIdx : Integer;
   outNode : TKCLNode;
   outBuf : TKCLStridedBufferDescriptor;
begin
   for j := 0 to High(FKernel.Outputs) do begin
      outNode := FKernel.Outputs[j];
      outIdx := FNodeToBufferIdx[outNode];
      outBuf := FBuffers[Length(FKernel.Inputs) + j];
      if Length(FNodeDims[outIdx]) <> Length(outBuf.Dimensions) then
         EdwsKCLException.RaiseSpatialDomainLengthMismatch(Length(FNodeDims[outIdx]), Length(outBuf.Dimensions));
      for i := 0 to High(FNodeDims[outIdx]) do
         if FNodeDims[outIdx][i] <> outBuf.Dimensions[i] then
            EdwsKCLException.RaiseSpatialDomainSizeMismatch(i, FNodeDims[outIdx][i], outBuf.Dimensions[i]);
   end;
end;

// ProcessConstant
//
procedure TKCLSSE2Backend.ProcessConstant(n : Integer; node : TKCLConstantNode);
var i : Integer;
begin
   FNodeDims[n] := Copy(node.Dimensions);
   FNodeTotalElements[n] := 1;
   for i := 0 to High(FNodeDims[n]) do FNodeTotalElements[n] := FNodeTotalElements[n] * FNodeDims[n][i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);
   for i := 0 to FNodeTotalElements[n]-1 do FNodeBuffers[n][i] := node.Value;
end;

// ProcessInput
//
procedure TKCLSSE2Backend.ProcessInput(n : Integer; node : TKCLInputNode);
var
   dIn : TKCLStridedBufferDescriptor;
   i : Integer;
   lIdx : TKCLDimensions;
begin
   dIn := FBuffers[node.InputIndex];
   FNodeDims[n] := Copy(dIn.Dimensions);
   FNodeTotalElements[n] := 1;
   for i := 0 to High(FNodeDims[n]) do FNodeTotalElements[n] := FNodeTotalElements[n] * FNodeDims[n][i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);
   if (dIn.DataType = dtFloat32) and IsContiguous(dIn) then
      SSE2_CvtPS2PDContiguous(PSingle(dIn.BasePointer), Pointer(FNodeBuffers[n]), FNodeTotalElements[n])
   else begin
      SetLength(lIdx, Length(FNodeDims[n]));
      for i := 0 to FNodeTotalElements[n]-1 do begin IndexFromFlat(i, FNodeDims[n], lIdx); FNodeBuffers[n][i] := GetValue(dIn, lIdx); end;
   end;
end;

// ProcessConcat
//
procedure TKCLSSE2Backend.ProcessConcat(n : Integer; node : TKCLConcatNode);
var
   axis, i, k, inputIdx, inIdx, inChannels, outChannels, curDimOffset : Integer;
   dimsOut, dimsIn, lIdx, idxOut : TKCLDimensions;
   totalOther : NativeInt;
   pRes, pSrc : PDouble;
begin
   axis := node.Axis;
   dimsOut := Copy(FNodeDims[FNodeToBufferIdx[node.Inputs[0]]]);
   for k := 1 to High(node.Inputs) do dimsOut[axis] := dimsOut[axis] + FNodeDims[FNodeToBufferIdx[node.Inputs[k]]][axis];
   FNodeDims[n] := dimsOut;
   FNodeTotalElements[n] := 1;
   for i := 0 to High(FNodeDims[n]) do FNodeTotalElements[n] := FNodeTotalElements[n] * FNodeDims[n][i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);

   if axis = High(dimsOut) then begin
      totalOther := 1; for k := 0 to High(dimsOut)-1 do totalOther := totalOther * dimsOut[k];
      outChannels := dimsOut[High(dimsOut)];
      for k := 0 to totalOther - 1 do begin
         pRes := PDouble(Pointer(FNodeBuffers[n])) + (k * outChannels);
         curDimOffset := 0;
         for inputIdx := 0 to High(node.Inputs) do begin
            inIdx := FNodeToBufferIdx[node.Inputs[inputIdx]];
            inChannels := FNodeDims[inIdx][High(dimsOut)];
            pSrc := PDouble(Pointer(FNodeBuffers[inIdx])) + (k * inChannels);
            System.Move(pSrc^, (pRes + curDimOffset)^, inChannels * SizeOf(Double));
            curDimOffset := curDimOffset + inChannels;
         end;
      end;
   end else begin
      curDimOffset := 0;
      for inputIdx := 0 to High(node.Inputs) do begin
         inIdx := FNodeToBufferIdx[node.Inputs[inputIdx]];
         dimsIn := FNodeDims[inIdx]; SetLength(lIdx, Length(dimsIn));
         for i := 0 to FNodeTotalElements[inIdx]-1 do begin
            IndexFromFlat(i, dimsIn, lIdx); idxOut := Copy(lIdx); idxOut[axis] := idxOut[axis] + curDimOffset;
            FNodeBuffers[n][FlatIndex(idxOut, dimsOut)] := FNodeBuffers[inIdx][i];
         end;
         curDimOffset := curDimOffset + dimsIn[axis];
      end;
   end;
end;

// ProcessMap
//
procedure TKCLSSE2Backend.ProcessMap(n : Integer; node : TKCLMapNode);
var
   in1, in2, i : Integer;
   total : NativeInt;
   p1, p2, pRes : PDouble;
   args : TDoubleDynArray;
   dims : TKCLDimensions;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]];
   dims := Copy(FNodeDims[in1]);

   if (Length(node.Inputs) > 1) and not (node is TKCLConcatNode) then begin
      var in2Idx := FNodeToBufferIdx[node.Inputs[1]];
      var dims2 := FNodeDims[in2Idx];
      if Length(dims) < Length(dims2) then begin
         var oldDims := dims;
         SetLength(dims, Length(dims2));
         for i := 0 to High(dims) do dims[i] := 1;
         for i := 0 to High(oldDims) do dims[High(dims) - High(oldDims) + i] := oldDims[i];
      end;
      for i := 0 to High(dims) do begin
         var d2Idx := i - (Length(dims) - Length(dims2));
         var d2 := 1; if d2Idx >= 0 then d2 := dims2[d2Idx];
         if (dims[i] <> d2) and (dims[i] <> 1) and (d2 <> 1) then
            EdwsKCLException.RaiseBroadcastingMismatch(i, dims[i], d2);
         dims[i] := Max(dims[i], d2);
      end;
   end;

   FNodeDims[n] := dims;
   total := 1; for i := 0 to High(dims) do total := total * dims[i];
   FNodeTotalElements[n] := total;
   SetLength(FNodeBuffers[n], total);

   p1 := PDouble(Pointer(FNodeBuffers[in1])); pRes := PDouble(Pointer(FNodeBuffers[n]));

   if Length(node.Inputs) = 1 then begin
      if node is TKCLReLUNode then SSE2_ReLU(p1, pRes, total)
      else if node is TKCLSigmoidNode then SSE2_Sigmoid(p1, pRes, total)
      else if node is TKCLReLU6Node then SSE2_ReLU6(p1, pRes, total)
      else if node is TKCLHardSwishNode then SSE2_HardSwish(p1, pRes, total)
      else if node is TKCLHardSigmoidNode then SSE2_HardSigmoid(p1, pRes, total)
      else if node is TKCLExpNode then SSE2_Exp(p1, pRes, total)
      else begin
         SetLength(args, 2); args[1] := 0;
         for i := 0 to total-1 do begin args[0] := FNodeBuffers[in1][i]; pRes[i] := node.Eval(args); end;
      end;
   end else begin
      in2 := FNodeToBufferIdx[node.Inputs[1]];
      if FNodeTotalElements[in2] = total then begin
         p2 := PDouble(Pointer(FNodeBuffers[in2]));
         if node is TKCLAddNode then SSE2_Add(p1, p2, pRes, total)
         else if node is TKCLSubNode then SSE2_Sub(p1, p2, pRes, total)
         else if node is TKCLMulNode then SSE2_Mul(p1, p2, pRes, total)
         else if node is TKCLDivNode then SSE2_Div(p1, p2, pRes, total)
         else begin
            SetLength(args, 2);
            for i := 0 to total-1 do begin args[0] := FNodeBuffers[in1][i]; args[1] := FNodeBuffers[in2][i]; pRes[i] := node.Eval(args); end;
         end;
      end else ProcessMapFallback(n, node);
   end;
end;

// ProcessConv2D
//
procedure TKCLSSE2Backend.ProcessConv2D(n : Integer; node : TKCLConv2DNode);
var
   in1, i : Integer;
   dims : TKCLDimensions;
   fusion : TKCLFusionInfo;
   act : TKCLActivation;
   pResidual : PDouble;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]];
   dims := Copy(FNodeDims[in1]);
   if Length(dims) >= 3 then begin
      dims[High(dims)-2] := (dims[High(dims)-2] + node.Stride - 1) div node.Stride;
      dims[High(dims)-1] := (dims[High(dims)-1] + node.Stride - 1) div node.Stride;
      dims[High(dims)] := Length(node.Bias);
   end;
   FNodeDims[n] := dims;
   FNodeTotalElements[n] := 1; for i := 0 to High(dims) do FNodeTotalElements[n] := FNodeTotalElements[n] * dims[i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);

   act := actNone;
   pResidual := nil;
   if (FFusionMap <> nil) and FFusionMap.TryGetValue(node, fusion) then begin
      act := fusion.Activation;
      if fusion.ResidualInput <> nil then begin
         var resIdx := FNodeToBufferIdx[fusion.ResidualInput];
         pResidual := PDouble(Pointer(FNodeBuffers[resIdx]));
      end;
   end;

   ProcessConv2DSSE2(n, node, act, pResidual);

   // Alias fused nodes to Conv2D output
   if (FFusionMap <> nil) and FFusionMap.TryGetValue(node, fusion) then begin
      if fusion.AddNode <> nil then begin
         var addIdx := FNodeToBufferIdx[fusion.AddNode];
         FNodeDims[addIdx] := FNodeDims[n];
         FNodeTotalElements[addIdx] := FNodeTotalElements[n];
         FNodeBuffers[addIdx] := FNodeBuffers[n];
      end;
      if fusion.ActivationNode <> nil then begin
         var actIdx := FNodeToBufferIdx[fusion.ActivationNode];
         FNodeDims[actIdx] := FNodeDims[n];
         FNodeTotalElements[actIdx] := FNodeTotalElements[n];
         FNodeBuffers[actIdx] := FNodeBuffers[n];
      end;
   end;
end;

// ProcessConv2DSSE2
//
procedure TKCLSSE2Backend.ProcessConv2DSSE2(n: Integer; node: TKCLConv2DNode;
   act : TKCLActivation; pResidual : PDouble);
var
   in1, inChannels, outChannels, inH, inW, outH, outW, hO, wO, ny, nx, stepY, stepX, k, i : Integer;
   pInput, pRes, pW, pB, pROut, pWBase : PDouble;
   pad_h, pad_top, pad_w, pad_left, h_in_start, w_in_start : Integer;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]];
   inChannels := FNodeDims[in1][High(FNodeDims[in1])]; outChannels := Length(node.Bias);
   inH := FNodeDims[in1][High(FNodeDims[in1])-2]; inW := FNodeDims[in1][High(FNodeDims[in1])-1];
   outH := FNodeDims[n][High(FNodeDims[n])-2]; outW := FNodeDims[n][High(FNodeDims[n])-1];
   pInput := PDouble(Pointer(FNodeBuffers[in1])); pRes := PDouble(Pointer(FNodeBuffers[n]));
   pW := PDouble(node.Weights); pB := PDouble(node.Bias);

   if Length(node.Bias) <> outChannels then
      raise Exception.CreateFmt('Bias length mismatch: expected %d, got %d', [outChannels, Length(node.Bias)]);
   if Length(node.Weights) <> node.KernelSize * node.KernelSize * inChannels * outChannels then
      raise Exception.CreateFmt('Weights length mismatch: expected %d, got %d', [node.KernelSize * node.KernelSize * inChannels * outChannels, Length(node.Weights)]);

   for i := 0 to (FNodeTotalElements[n] div outChannels) - 1 do SSE2_Copy(pB, pRes + (i * outChannels), outChannels);
   pad_h := Max(0, (outH - 1) * node.Stride + node.KernelSize - inH); pad_top := pad_h div 2;
   pad_w := Max(0, (outW - 1) * node.Stride + node.KernelSize - inW); pad_left := pad_w div 2;

   for hO := 0 to outH - 1 do begin
      h_in_start := hO * node.Stride - pad_top;
      for wO := 0 to outW - 1 do begin
         w_in_start := wO * node.Stride - pad_left; pROut := pRes + (hO * outW + wO) * outChannels;
         for stepY := 0 to node.KernelSize - 1 do begin
            ny := h_in_start + stepY;
            if (ny >= 0) and (ny < inH) then begin
               for stepX := 0 to node.KernelSize - 1 do begin
                  nx := w_in_start + stepX;
                  if (nx >= 0) and (nx < inW) then begin
                     pWBase := pW + ((stepY * node.KernelSize + stepX) * inChannels * outChannels);
                     for k := 0 to inChannels - 1 do SSE2_AddScaled(pWBase + (k * outChannels), (pInput + (ny * inW * inChannels) + (nx * inChannels) + k)^, pROut, outChannels);
                  end;
               end;
            end;
         end;
      end;
   end;

   // Apply fused residual add
   if pResidual <> nil then
      SSE2_Add(pRes, pResidual, pRes, FNodeTotalElements[n]);

   // Apply fused activation
   if act <> actNone then
      ApplyActivationInPlace(pRes, FNodeTotalElements[n], act);
end;

// ProcessConv2DTranspose
//
procedure TKCLSSE2Backend.ProcessConv2DTranspose(n : Integer; node : TKCLConv2DTransposeNode);
var
   in1Idx, inChannels, outChannels, inH, inW, outH, outW : Integer;
   iy, ix, ky, kx, oy, ox, cIn, i : Integer;
   dimsIn, dimsOut : TKCLDimensions;
   pInput, pRes, pW, pB : PDouble;
   pad_h, pad_top, pad_w, pad_left : Integer;
   pInPixel, pWBase, pOutPixel : PDouble;
begin
   in1Idx := FNodeToBufferIdx[node.Inputs[0]];
   dimsIn := FNodeDims[in1Idx];
   dimsOut := Copy(dimsIn);
   if Length(dimsOut) >= 3 then begin
      dimsOut[High(dimsOut)-2] := dimsOut[High(dimsOut)-2] * node.Stride;
      dimsOut[High(dimsOut)-1] := dimsOut[High(dimsOut)-1] * node.Stride;
      dimsOut[High(dimsOut)] := Length(node.Bias);
   end;
   FNodeDims[n] := dimsOut;
   FNodeTotalElements[n] := 1;
   for i := 0 to High(dimsOut) do FNodeTotalElements[n] := FNodeTotalElements[n] * dimsOut[i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);

   if Length(dimsOut) < 3 then Exit;

   inChannels := dimsIn[High(dimsIn)]; outChannels := dimsOut[High(dimsOut)];
   inH := dimsIn[High(dimsIn)-2]; inW := dimsIn[High(dimsIn)-1];
   outH := dimsOut[High(dimsOut)-2]; outW := dimsOut[High(dimsOut)-1];
   pInput := PDouble(Pointer(FNodeBuffers[in1Idx]));
   pRes := PDouble(Pointer(FNodeBuffers[n]));
   pW := PDouble(node.Weights);
   pB := PDouble(node.Bias);

   pad_h := Max(0, (inH - 1) * node.Stride + node.KernelSize - outH);
   pad_top := pad_h div 2;
   pad_w := Max(0, (inW - 1) * node.Stride + node.KernelSize - outW);
   pad_left := pad_w div 2;

   for i := 0 to (FNodeTotalElements[n] div outChannels) - 1 do
      SSE2_Copy(pB, pRes + (i * outChannels), outChannels);

   // Scatter: each input pixel contributes to K*K output pixels
   for iy := 0 to inH - 1 do begin
      for ix := 0 to inW - 1 do begin
         pInPixel := pInput + (iy * inW + ix) * inChannels;
         for ky := 0 to node.KernelSize - 1 do begin
            oy := iy * node.Stride + ky - pad_top;
            if (oy < 0) or (oy >= outH) then Continue;
            for kx := 0 to node.KernelSize - 1 do begin
               ox := ix * node.Stride + kx - pad_left;
               if (ox < 0) or (ox >= outW) then Continue;

               pWBase := pW + (ky * node.KernelSize + kx) * inChannels * outChannels;
               pOutPixel := pRes + (oy * outW + ox) * outChannels;

               if outChannels = 1 then begin
                  pOutPixel^ := pOutPixel^ + SSE2_DotProduct(pInPixel, pWBase, inChannels);
               end else begin
                  for cIn := 0 to inChannels - 1 do
                     SSE2_AddScaled(pWBase + cIn * outChannels, pInPixel[cIn], pOutPixel, outChannels);
               end;
            end;
         end;
      end;
   end;
end;

// ProcessDepthwiseConv2D
//
procedure TKCLSSE2Backend.ProcessDepthwiseConv2D(n : Integer; node : TKCLDepthwiseConv2DNode);
var
   in1, inChannels, outH, outW, inH, inW, hO, wO, ny, nx, stepY, stepX, i : Integer;
   dims : TKCLDimensions;
   pInput, pRes, pW, pB, pROut : PDouble;
   pad_h, pad_top, pad_w, pad_left, h_in_start, w_in_start : Integer;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]];
   dims := Copy(FNodeDims[in1]);
   if Length(dims) >= 3 then begin
      dims[High(dims)-2] := (dims[High(dims)-2] + node.Stride - 1) div node.Stride;
      dims[High(dims)-1] := (dims[High(dims)-1] + node.Stride - 1) div node.Stride;
   end;
   FNodeDims[n] := dims;
   FNodeTotalElements[n] := 1; for i := 0 to High(dims) do FNodeTotalElements[n] := FNodeTotalElements[n] * dims[i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);
   inChannels := FNodeDims[in1][High(dims)]; outH := dims[High(dims)-2]; outW := dims[High(dims)-1];
   inH := FNodeDims[in1][High(dims)-2]; inW := FNodeDims[in1][High(dims)-1];
   pInput := PDouble(Pointer(FNodeBuffers[in1])); pRes := PDouble(Pointer(FNodeBuffers[n]));
   pW := PDouble(node.Weights); pB := PDouble(node.Bias);
   for i := 0 to (FNodeTotalElements[n] div inChannels) - 1 do SSE2_Copy(pB, pRes + (i * inChannels), inChannels);
   pad_h := Max(0, (outH - 1) * node.Stride + node.KernelSize - inH); pad_top := pad_h div 2;
   pad_w := Max(0, (outW - 1) * node.Stride + node.KernelSize - inW); pad_left := pad_w div 2;
   for hO := 0 to outH - 1 do begin
      h_in_start := hO * node.Stride - pad_top;
      for wO := 0 to outW - 1 do begin
         w_in_start := wO * node.Stride - pad_left; pROut := pRes + (hO * outW + wO) * inChannels;
         for stepY := 0 to node.KernelSize - 1 do begin
            ny := h_in_start + stepY;
            if (ny >= 0) and (ny < inH) then begin
               for stepX := 0 to node.KernelSize - 1 do begin
                  nx := w_in_start + stepX;
                  if (nx >= 0) and (nx < inW) then SSE2_MulAdd(pInput + (ny * inW * inChannels) + (nx * inChannels), pW + ((stepY * node.KernelSize + stepX) * inChannels), pROut, inChannels);
               end;
            end;
         end;
      end;
   end;
end;

// ProcessSoftMax
//
procedure TKCLSSE2Backend.ProcessSoftMax(n : Integer; node : TKCLSoftMaxNode);
var
   in1, axis, axisSize, k, loopA, i : Integer;
   dims : TKCLDimensions;
   totalOther : NativeInt;
   p1, pRes : PDouble;
   maxV, sumE, vVal, val : Double;
   baseI : TKCLDimensions;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]]; dims := Copy(FNodeDims[in1]);
   FNodeDims[n] := dims; FNodeTotalElements[n] := FNodeTotalElements[in1]; SetLength(FNodeBuffers[n], FNodeTotalElements[n]);
   axis := node.Axis; axisSize := dims[axis];
   totalOther := 1; for k := 0 to High(dims) do if k<>axis then totalOther := totalOther * dims[k];
   if axis = High(dims) then begin
      for i := 0 to totalOther - 1 do begin
         p1 := PDouble(Pointer(FNodeBuffers[in1])) + (i * axisSize); pRes := PDouble(Pointer(FNodeBuffers[n])) + (i * axisSize);
         SSE2_MaxVector(p1, axisSize, maxV);
         for loopA := 0 to axisSize - 1 do pRes[loopA] := p1[loopA] - maxV;
         SSE2_Exp(pRes, pRes, axisSize); SSE2_SumVector(pRes, axisSize, sumE);
         vVal := 1.0 / sumE; for loopA := 0 to axisSize-1 do pRes[loopA] := pRes[loopA] * vVal;
      end;
   end else begin
      SetLength(baseI, Length(dims));
      for i := 0 to totalOther-1 do begin
         var tempVal := i; 
         for k := High(dims) downto 0 do 
            if k <> axis then begin baseI[k] := tempVal mod dims[k]; tempVal := tempVal div dims[k]; end 
            else baseI[k] := 0;
         maxV := -1e30;
         for loopA := 0 to axisSize-1 do begin 
            baseI[axis] := loopA; val := FNodeBuffers[in1][FlatIndex(baseI, dims)]; 
            if val > maxV then maxV := val; 
         end;
         sumE := 0;
         for loopA := 0 to axisSize-1 do begin 
            baseI[axis] := loopA; sumE := sumE + Exp(FNodeBuffers[in1][FlatIndex(baseI, dims)] - maxV); 
         end;
         for loopA := 0 to axisSize-1 do begin 
            baseI[axis] := loopA; FNodeBuffers[n][FlatIndex(baseI, dims)] := Exp(FNodeBuffers[in1][FlatIndex(baseI, dims)] - maxV) / sumE; 
         end;
      end;
   end;
end;

// ProcessResizeBilinear
//
procedure TKCLSSE2Backend.ProcessResizeBilinear(n : Integer; node : TKCLResizeBilinearNode);
var
   i, in1, inChannels, inH, inW, outH, outW, k, hO, wO, h0, h1, w0, w1, rank : Integer;
   dims, dOutDims, lIdx : TKCLDimensions;
   pInput, pRes, pROut, pInBaseH0, pInBaseH1 : PDouble;
   scaleH, scaleW, h_in, w_in, fh, fw : Double;
   totalOther : NativeInt;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]]; dims := FNodeDims[in1];
   dOutDims := Copy(dims);
   rank := Length(dims);
   if rank >= 3 then begin
      dOutDims[rank-3] := node.TargetHeight;
      dOutDims[rank-2] := node.TargetWidth;
   end;
   FNodeDims[n] := dOutDims; FNodeTotalElements[n] := 1; for i := 0 to High(dOutDims) do FNodeTotalElements[n] := FNodeTotalElements[n] * dOutDims[i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);

   if rank >= 3 then begin
      inChannels := dims[rank-1]; inH := dims[rank-3]; inW := dims[rank-2];
      outH := dOutDims[rank-3]; outW := dOutDims[rank-2];
      pInput := PDouble(Pointer(FNodeBuffers[in1])); pRes := PDouble(Pointer(FNodeBuffers[n]));
      
      if node.AlignCorners and (outH > 1) then scaleH := (inH - 1) / (outH - 1) else scaleH := inH / outH;
      if node.AlignCorners and (outW > 1) then scaleW := (inW - 1) / (outW - 1) else scaleW := inW / outW;
      
      totalOther := 1; for k := 0 to rank - 4 do totalOther := totalOther * dOutDims[k];
      
      for k := 0 to totalOther - 1 do begin
         var pInBaseBatch := pInput + (k * inH * inW * inChannels); var pResBaseBatch := pRes + (k * outH * outW * inChannels);
         for hO := 0 to outH - 1 do begin
            if node.HalfPixelCenters then h_in := Max(0.0, (hO + 0.5) * scaleH - 0.5) else h_in := hO * scaleH;
            h0 := Floor(h_in); h1 := Min(h0 + 1, inH - 1); fh := h_in - h0;
            pInBaseH0 := pInBaseBatch + (h0 * inW * inChannels); pInBaseH1 := pInBaseBatch + (h1 * inW * inChannels);
            for wO := 0 to outW - 1 do begin
               if node.HalfPixelCenters then w_in := Max(0.0, (wO + 0.5) * scaleW - 0.5) else w_in := wO * scaleW;
               w0 := Floor(w_in); w1 := Min(w0 + 1, inW - 1); fw := w_in - w0;
               pROut := pResBaseBatch + (hO * outW + wO) * inChannels;
               SSE2_BilinearInterpolate(pInBaseH0 + (w0 * inChannels), pInBaseH0 + (w1 * inChannels), pInBaseH1 + (w0 * inChannels), pInBaseH1 + (w1 * inChannels), pROut, fw, fh, inChannels);
            end;
         end;
      end;
   end else begin
      SetLength(lIdx, Length(dOutDims));
      for i := 0 to FNodeTotalElements[n] - 1 do begin IndexFromFlat(i, dOutDims, lIdx); FNodeBuffers[n][i] := FNodeBuffers[in1][FlatIndex(lIdx, dims)]; end;
   end;
end;

// ProcessMaxPool2D
//
procedure TKCLSSE2Backend.ProcessMaxPool2D(n : Integer; node : TKCLMaxPool2DNode);
var
   in1, inChannels, outH, outW, inH, inW, hO, wO, ny, nx, stepY, stepX, i : Integer;
   dims : TKCLDimensions;
   pInput, pRes, pROut : PDouble;
   pad_h, pad_top, pad_w, pad_left, h_in_start, w_in_start : Integer;
begin
   in1 := FNodeToBufferIdx[node.Inputs[0]]; dims := Copy(FNodeDims[in1]);
   if Length(dims) >= 3 then begin
      dims[High(dims)-2] := (dims[High(dims)-2] + node.Stride - 1) div node.Stride;
      dims[High(dims)-1] := (dims[High(dims)-1] + node.Stride - 1) div node.Stride;
   end;
   FNodeDims[n] := dims; FNodeTotalElements[n] := 1; for i := 0 to High(dims) do FNodeTotalElements[n] := FNodeTotalElements[n] * dims[i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);
   inChannels := FNodeDims[in1][High(dims)]; inH := FNodeDims[in1][High(dims)-2]; inW := FNodeDims[in1][High(dims)-1];
   outH := dims[High(dims)-2]; outW := dims[High(dims)-1];
   pInput := PDouble(Pointer(FNodeBuffers[in1])); pRes := PDouble(Pointer(FNodeBuffers[n]));
   for i := 0 to FNodeTotalElements[n]-1 do FNodeBuffers[n][i] := -1e30;
   pad_h := Max(0, (outH - 1) * node.Stride + node.KernelSize - inH); pad_top := pad_h div 2;
   pad_w := Max(0, (outW - 1) * node.Stride + node.KernelSize - inW); pad_left := pad_w div 2;
   for hO := 0 to outH - 1 do begin
      h_in_start := hO * node.Stride - pad_top;
      for wO := 0 to outW - 1 do begin
         w_in_start := wO * node.Stride - pad_left; pROut := pRes + (hO * outW + wO) * inChannels;
         for stepY := 0 to node.KernelSize - 1 do begin
            ny := h_in_start + stepY;
            if (ny >= 0) and (ny < inH) then begin
               for stepX := 0 to node.KernelSize - 1 do begin
                  nx := w_in_start + stepX; if (nx >= 0) and (nx < inW) then SSE2_Max(pInput + (ny * inW + nx) * inChannels, pROut, inChannels);
               end;
            end;
         end;
      end;
   end;
end;

// ProcessGlobalAvgPool
//
procedure TKCLSSE2Backend.ProcessGlobalAvgPool(n : Integer; node : TKCLGlobalAvgPoolNode);
var i, ny, nx, inChannels, inH, inW : Integer; pInput, pRes : PDouble; vVal, sumVal : Double;
begin
   var in1 := FNodeToBufferIdx[node.Inputs[0]]; var dims := Copy(FNodeDims[in1]);
   if Length(dims) >= 3 then begin dims[High(dims)-2] := 1; dims[High(dims)-1] := 1; end;
   FNodeDims[n] := dims; FNodeTotalElements[n] := 1; for i := 0 to High(dims) do FNodeTotalElements[n] := FNodeTotalElements[n] * dims[i];
   SetLength(FNodeBuffers[n], FNodeTotalElements[n]);
   if Length(FNodeDims[in1]) >= 3 then begin
      inChannels := FNodeDims[in1][High(dims)]; inH := FNodeDims[in1][High(dims)-2]; inW := FNodeDims[in1][High(dims)-1];
      pInput := PDouble(Pointer(FNodeBuffers[in1])); pRes := PDouble(Pointer(FNodeBuffers[n]));
      for i := 0 to inChannels - 1 do pRes[i] := 0;
      for ny := 0 to inH - 1 do for nx := 0 to inW - 1 do SSE2_Add(pInput + (ny * inW + nx) * inChannels, pRes, pRes, inChannels);
      vVal := 1.0 / (inH * inW); for i := 0 to inChannels - 1 do pRes[i] := pRes[i] * vVal;
   end else begin
      sumVal := 0; 
      for i := 0 to FNodeTotalElements[in1]-1 do sumVal := sumVal + FNodeBuffers[in1][i];
      if FNodeTotalElements[in1] > 0 then sumVal := sumVal / FNodeTotalElements[in1];
      for i := 0 to FNodeTotalElements[n]-1 do FNodeBuffers[n][i] := sumVal;
   end;
end;

// ProcessMapFallback
//
procedure TKCLSSE2Backend.ProcessMapFallback(n : Integer; node : TKCLMapNode);
var
   dims, inDims, idxIn, lIdx : TKCLDimensions;
   args : TDoubleDynArray;
   i, k, j, inIdx, jOut : Integer;
begin
   dims := FNodeDims[n]; SetLength(lIdx, Length(dims));
   SetLength(args, Length(node.Inputs));
   for i := 0 to FNodeTotalElements[n]-1 do begin
      IndexFromFlat(i, dims, lIdx);
      for k := 0 to High(node.Inputs) do begin
         inIdx := FNodeToBufferIdx[node.Inputs[k]]; inDims := FNodeDims[inIdx];
         SetLength(idxIn, Length(inDims));
         for j := 0 to High(inDims) do begin
            jOut := j + (Length(dims) - Length(inDims));
            if inDims[j] = 1 then idxIn[j] := 0 else idxIn[j] := lIdx[jOut];
         end;
         args[k] := FNodeBuffers[inIdx][FlatIndex(idxIn, inDims)];
      end;
      FNodeBuffers[n][i] := node.Eval(args);
   end;
end;

// Execute
//
procedure TKCLSSE2Backend.Execute(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
var
   savedMask : TArithmeticExceptionMask;
   n, j, oIdx, i : Integer;
   node : TKCLNode;
   outBuf : TKCLStridedBufferDescriptor;
   dims, lIdx : TKCLDimensions;
begin
   FKernel := AKernel;
   SetLength(FBuffers, Length(ABuffers)); for n := 0 to High(ABuffers) do FBuffers[n] := ABuffers[n];
   if Length(FBuffers) = 0 then Exit;

   savedMask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
   try
      FSortedNodes := TList<TKCLNode>.Create;
      FVisiting := TDictionary<TKCLNode, Boolean>.Create;
      FVisited := TDictionary<TKCLNode, Boolean>.Create;
      FNodeToBufferIdx := TDictionary<TKCLNode, Integer>.Create;
      try
         for j := 0 to High(FKernel.Outputs) do VisitNode(FKernel.Outputs[j]);
         SetLength(FNodeDims, FSortedNodes.Count); SetLength(FNodeTotalElements, FSortedNodes.Count); SetLength(FNodeBuffers, FSortedNodes.Count);

         // Register buffer indices first so fusion analysis can reference them
         for n := 0 to FSortedNodes.Count - 1 do
            FNodeToBufferIdx.Add(FSortedNodes[n], n);

         // Analyze fusion opportunities
         AnalyzeFusion;

         // Pre-compile JIT kernels (no-op for SSE2-only backend)
         PrepareNodes;

         for n := 0 to FSortedNodes.Count - 1 do begin
            node := FSortedNodes[n];

            // Skip nodes that have been fused into another operation
            if (FFusedNodes <> nil) and FFusedNodes.ContainsKey(node) then
               Continue;

            if node is TKCLConstantNode then ProcessConstant(n, TKCLConstantNode(node))
            else if node is TKCLInputNode then ProcessInput(n, TKCLInputNode(node))
            else if node is TKCLConcatNode then ProcessConcat(n, TKCLConcatNode(node))
            else if node is TKCLConv2DNode then ProcessConv2D(n, TKCLConv2DNode(node))
            else if node is TKCLConv2DTransposeNode then ProcessConv2DTranspose(n, TKCLConv2DTransposeNode(node))
            else if node is TKCLDepthwiseConv2DNode then ProcessDepthwiseConv2D(n, TKCLDepthwiseConv2DNode(node))
            else if node is TKCLSoftMaxNode then ProcessSoftMax(n, TKCLSoftMaxNode(node))
            else if node is TKCLResizeBilinearNode then ProcessResizeBilinear(n, TKCLResizeBilinearNode(node))
            else if node is TKCLMaxPool2DNode then ProcessMaxPool2D(n, TKCLMaxPool2DNode(node))
            else if node is TKCLGlobalAvgPoolNode then ProcessGlobalAvgPool(n, TKCLGlobalAvgPoolNode(node))
            else if node is TKCLMapNode then ProcessMap(n, TKCLMapNode(node))
            else raise Exception.CreateFmt('Node %s is not supported by SSE2 backend', [node.ClassName]);
         end;

         FinalizeNodes;
         VerifyOutputs;

         for j := 0 to High(FKernel.Outputs) do begin
            oIdx := FNodeToBufferIdx[FKernel.Outputs[j]]; outBuf := FBuffers[Length(FKernel.Inputs) + j];
            if (outBuf.DataType = dtFloat32) and IsContiguous(outBuf) then
               SSE2_CvtPD2PSContiguous(PDouble(Pointer(FNodeBuffers[oIdx])), PSingle(outBuf.BasePointer), FNodeTotalElements[oIdx])
            else begin
               dims := FNodeDims[oIdx]; SetLength(lIdx, Length(dims));
               for i := 0 to FNodeTotalElements[oIdx]-1 do begin IndexFromFlat(i, dims, lIdx); SetValue(outBuf, lIdx, FNodeBuffers[oIdx][i]); end;
            end;
         end;
      finally
         FFusionMap.Free; FFusedNodes.Free;
         FFusionMap := nil; FFusedNodes := nil;
         FNodeToBufferIdx.Free; FSortedNodes.Free; FVisiting.Free; FVisited.Free;
      end;
   finally SetExceptionMask(savedMask); end;
end;

// ------------------
// ------------------ TKCLJITBackend ------------------
// ------------------

const
   cMapBatchCapacity = 32;

// FlushMapBatch — execute all enqueued map operations
//
procedure TKCLJITBackend.FlushMapBatch;
begin
   if FBatchCount > 0 then begin
      TKCLWin64JITBackend.FlushBatch(FBatch, FBatchCount);
      FBatchCount := 0;
   end;
end;

// EnqueueMapBatch — add a map operation to the batch
//
procedure TKCLJITBackend.EnqueueMapBatch(compiled : TKCLCompiledKernel;
   p1, pRes, p2, pParams : PDouble; total : NativeInt);
begin
   if FBatchCount >= cMapBatchCapacity then
      FlushMapBatch;
   if Length(FBatch) < cMapBatchCapacity then
      SetLength(FBatch, cMapBatchCapacity);
   FBatch[FBatchCount].Code := compiled.Code;
   FBatch[FBatchCount].pIn := p1;
   FBatch[FBatchCount].pOut := pRes;
   FBatch[FBatchCount].pIn2 := p2;
   FBatch[FBatchCount].pParams := pParams;
   FBatch[FBatchCount].Count := total;
   Inc(FBatchCount);
end;

// PrepareNodes — pre-compile all JIT kernels before execution
//
procedure TKCLJITBackend.PrepareNodes;
begin
   // Compute input dimensions for Conv2D nodes before pre-compilation
   // Input nodes must have their dims set first
   for var n := 0 to FSortedNodes.Count - 1 do begin
      var node := FSortedNodes[n];
      if node is TKCLInputNode then begin
         var dIn := FBuffers[TKCLInputNode(node).InputIndex];
         FNodeDims[n] := Copy(dIn.Dimensions);
      end else if node is TKCLConstantNode then begin
         FNodeDims[n] := Copy(TKCLConstantNode(node).Dimensions);
      end else if node is TKCLConv2DNode then begin
         var convNode := TKCLConv2DNode(node);
         var in1Idx := FNodeToBufferIdx[convNode.Inputs[0]];
         var dims := Copy(FNodeDims[in1Idx]);
         if Length(dims) >= 3 then begin
            dims[High(dims)-2] := (dims[High(dims)-2] + convNode.Stride - 1) div convNode.Stride;
            dims[High(dims)-1] := (dims[High(dims)-1] + convNode.Stride - 1) div convNode.Stride;
            dims[High(dims)] := Length(convNode.Bias);
         end;
         FNodeDims[n] := dims;
      end else if node is TKCLConv2DTransposeNode then begin
         var transNode := TKCLConv2DTransposeNode(node);
         var in1Idx := FNodeToBufferIdx[transNode.Inputs[0]];
         var dims := Copy(FNodeDims[in1Idx]);
         if Length(dims) >= 3 then begin
            dims[High(dims)-2] := dims[High(dims)-2] * transNode.Stride;
            dims[High(dims)-1] := dims[High(dims)-1] * transNode.Stride;
            dims[High(dims)] := Length(transNode.Bias);
         end;
         FNodeDims[n] := dims;
      end else if node is TKCLDepthwiseConv2DNode then begin
         var dwNode := TKCLDepthwiseConv2DNode(node);
         var in1Idx := FNodeToBufferIdx[dwNode.Inputs[0]];
         var dims := Copy(FNodeDims[in1Idx]);
         if Length(dims) >= 3 then begin
            dims[High(dims)-2] := (dims[High(dims)-2] + dwNode.Stride - 1) div dwNode.Stride;
            dims[High(dims)-1] := (dims[High(dims)-1] + dwNode.Stride - 1) div dwNode.Stride;
         end;
         FNodeDims[n] := dims;
      end else if node is TKCLMaxPool2DNode then begin
         var poolNode := TKCLMaxPool2DNode(node);
         var in1Idx := FNodeToBufferIdx[poolNode.Inputs[0]];
         var dims := Copy(FNodeDims[in1Idx]);
         if Length(dims) >= 3 then begin
            dims[High(dims)-2] := (dims[High(dims)-2] + poolNode.Stride - 1) div poolNode.Stride;
            dims[High(dims)-1] := (dims[High(dims)-1] + poolNode.Stride - 1) div poolNode.Stride;
         end;
         FNodeDims[n] := dims;
      end else if node is TKCLGlobalAvgPoolNode then begin
         var in1Idx := FNodeToBufferIdx[node.Inputs[0]];
         var dims := Copy(FNodeDims[in1Idx]);
         if Length(dims) >= 3 then begin
            dims[High(dims)-2] := 1;
            dims[High(dims)-1] := 1;
         end;
         FNodeDims[n] := dims;
      end else if node is TKCLSoftMaxNode then begin
         var in1Idx := FNodeToBufferIdx[node.Inputs[0]];
         FNodeDims[n] := Copy(FNodeDims[in1Idx]);
      end else if node is TKCLResizeBilinearNode then begin
         var rsNode := TKCLResizeBilinearNode(node);
         var in1Idx := FNodeToBufferIdx[rsNode.Inputs[0]];
         var dims := Copy(FNodeDims[in1Idx]);
         if Length(dims) >= 3 then begin
            dims[High(dims)-2] := rsNode.TargetHeight;
            dims[High(dims)-1] := rsNode.TargetWidth;
         end;
         FNodeDims[n] := dims;
      end else if node is TKCLConcatNode then begin
         var cNode := TKCLConcatNode(node);
         var axis := cNode.Axis;
         var dimsOut := Copy(FNodeDims[FNodeToBufferIdx[cNode.Inputs[0]]]);
         for var k := 1 to High(cNode.Inputs) do begin
            var inKIdx := FNodeToBufferIdx[cNode.Inputs[k]];
            dimsOut[axis] := dimsOut[axis] + FNodeDims[inKIdx][axis];
         end;
         FNodeDims[n] := dimsOut;
      end else if node is TKCLMapNode then begin
         // For map nodes, propagate dims from first input
         if Length(node.Inputs) > 0 then begin
            var in1Idx := FNodeToBufferIdx[node.Inputs[0]];
            FNodeDims[n] := Copy(FNodeDims[in1Idx]);
         end;
      end;
   end;
   TKCLWin64JITBackend.PrepareGraph(FKernel, FSortedNodes, FNodeToBufferIdx, FNodeDims, FFusionMap);

   // Pre-calculate workspace size for all intermediate node buffers
   var optData := TKCLOptimizationData(FKernel.OptimizationData);
   if (optData <> nil) and (optData.Workspace = nil) then begin
      var totalBytes : NativeInt := 0;
      for var n := 0 to FSortedNodes.Count - 1 do begin
         var node := FSortedNodes[n];
         if node is TKCLInputNode then Continue; // inputs use external buffers
         var dims := FNodeDims[n];
         var elemCount : NativeInt := 1;
         for var d := 0 to High(dims) do elemCount := elemCount * dims[d];
         if elemCount > 0 then
            totalBytes := totalBytes + ((elemCount * SizeOf(Double) + 31) and not NativeInt(31));
      end;
      if totalBytes > 0 then
         optData.Workspace := TKCLAlignedWorkspace.Create(totalBytes);
   end;
end;

// FinalizeNodes — flush remaining batch and reset workspace
//
procedure TKCLJITBackend.FinalizeNodes;
begin
   FlushMapBatch;
   var optData := TKCLOptimizationData(FKernel.OptimizationData);
   if (optData <> nil) and (optData.Workspace <> nil) then
      optData.Workspace.Reset;
end;

// ProcessConv2D
//
procedure TKCLJITBackend.ProcessConv2D(n: Integer; node: TKCLConv2DNode);
var
   in1Idx, inChannels, outChannels, inH, inW, outH, outW : Integer;
   total, hO, wO : Integer;
   pad_h, pad_top, pad_w, pad_left : Integer;
   hFirst, hLast, wFirst, wLast : Integer;
   pInput, pRes, pW, pB : PDouble;
   ny, nx : Integer;
   pROut, pWBase : PDouble;
   fusion : TKCLFusionInfo;
   act : TKCLActivation;
   pResidual : PDouble;
   hasFusion : Boolean;
begin
   // Flush pending map batch before stencil operation (synchronization point)
   FlushMapBatch;

   in1Idx := FNodeToBufferIdx[node.Inputs[0]];
   var dims := Copy(FNodeDims[in1Idx]);
   if Length(dims) >= 3 then begin
      dims[High(dims)-2] := (dims[High(dims)-2] + node.Stride - 1) div node.Stride;
      dims[High(dims)-1] := (dims[High(dims)-1] + node.Stride - 1) div node.Stride;
      dims[High(dims)] := Length(node.Bias);
   end;
   FNodeDims[n] := dims;
   total := 1; for var i := 0 to High(dims) do total := total * dims[i];
   FNodeTotalElements[n] := total;
   SetLength(FNodeBuffers[n], total);

   // Check fusion info
   act := actNone;
   pResidual := nil;
   hasFusion := (FFusionMap <> nil) and FFusionMap.TryGetValue(node, fusion);
   if hasFusion then begin
      act := fusion.Activation;
      if fusion.ResidualInput <> nil then begin
         var resIdx := FNodeToBufferIdx[fusion.ResidualInput];
         pResidual := PDouble(Pointer(FNodeBuffers[resIdx]));
      end;
   end;

   outChannels := Length(node.Bias);
   inChannels := FNodeDims[in1Idx][High(FNodeDims[in1Idx])];
   pInput := PDouble(Pointer(FNodeBuffers[in1Idx]));
   pRes := PDouble(Pointer(FNodeBuffers[n]));
   pW := PDouble(node.Weights);
   pB := PDouble(node.Bias);

   // Case 1: Pointwise (1x1, stride=1) -> batched pixel JIT
   if (node.KernelSize = 1) and (node.Stride = 1) then begin
      // When residual is present, do not fuse activation into JIT;
      // apply residual and activation after the convolution instead
      var jitAct := act;
      if pResidual <> nil then jitAct := actNone;
      if TKCLWin64JITBackend.Execute(FKernel, node, pInput, pRes, pW, pB,
            total div outChannels, inChannels, outChannels, jitAct) then begin
         if pResidual <> nil then begin
            SSE2_Add(pRes, pResidual, pRes, total);
            if act <> actNone then
               ApplyActivationInPlace(pRes, total, act);
         end;
         // Alias fused nodes
         if hasFusion then begin
            if fusion.AddNode <> nil then begin
               var addIdx := FNodeToBufferIdx[fusion.AddNode];
               FNodeDims[addIdx] := FNodeDims[n];
               FNodeTotalElements[addIdx] := FNodeTotalElements[n];
               FNodeBuffers[addIdx] := FNodeBuffers[n];
            end;
            if fusion.ActivationNode <> nil then begin
               var actIdx := FNodeToBufferIdx[fusion.ActivationNode];
               FNodeDims[actIdx] := FNodeDims[n];
               FNodeTotalElements[actIdx] := FNodeTotalElements[n];
               FNodeBuffers[actIdx] := FNodeBuffers[n];
            end;
         end;
         Exit;
      end;
   end

   // Case 2: k*k with JIT interior rows + SSE2 borders
   else if (node.KernelSize > 1) and (Length(dims) >= 3) then begin
      inH := FNodeDims[in1Idx][High(dims)-2]; inW := FNodeDims[in1Idx][High(dims)-1];
      outH := dims[High(dims)-2]; outW := dims[High(dims)-1];
      pad_h := Max(0, (outH - 1) * node.Stride + node.KernelSize - inH);
      pad_top := pad_h div 2;
      pad_w := Max(0, (outW - 1) * node.Stride + node.KernelSize - inW);
      pad_left := pad_w div 2;

      // Interior = region where all K*K inputs are within bounds
      hFirst := (pad_top + node.Stride - 1) div node.Stride;
      hLast := (inH + pad_top - node.KernelSize) div node.Stride;
      wFirst := (pad_left + node.Stride - 1) div node.Stride;
      wLast := (inW + pad_left - node.KernelSize) div node.Stride;

      if (hFirst <= hLast) and (wFirst <= wLast) then begin
         var interiorW := wLast - wFirst + 1;

         // Initialize all output with bias
         for var idx := 0 to (total div outChannels) - 1 do
            SSE2_Copy(pB, pRes + (idx * outChannels), outChannels);

         // Border pixels: SSE2 with bounds checking
         for hO := 0 to outH - 1 do begin
            var h_in_start := hO * node.Stride - pad_top;
            for wO := 0 to outW - 1 do begin
               if (hO >= hFirst) and (hO <= hLast) and (wO >= wFirst) and (wO <= wLast) then
                  Continue;
               pROut := pRes + (hO * outW + wO) * outChannels;
               for var stepY := 0 to node.KernelSize - 1 do begin
                  ny := h_in_start + stepY;
                  if (ny >= 0) and (ny < inH) then begin
                     for var stepX := 0 to node.KernelSize - 1 do begin
                        nx := (wO * node.Stride - pad_left) + stepX;
                        if (nx >= 0) and (nx < inW) then begin
                           pWBase := pW + ((stepY * node.KernelSize + stepX) * inChannels * outChannels);
                           for var k := 0 to inChannels - 1 do
                              SSE2_AddScaled(pWBase + (k * outChannels),
                                 (pInput + (ny * inW * inChannels) + (nx * inChannels) + k)^,
                                 pROut, outChannels);
                        end;
                     end;
                  end;
               end;
            end;
         end;

         // When residual is present, do not fuse activation into JIT;
         // apply both residual and activation after all conv pixels are computed
         var kxkAct := act;
         if pResidual <> nil then kxkAct := actNone;

         // Interior rows: JIT micro-kernel per row
         for hO := hFirst to hLast do begin
            var pInRow := pInput + ((hO * node.Stride - pad_top) * inW
                                   + (wFirst * node.Stride - pad_left)) * inChannels;
            var pOutRow := pRes + (hO * outW + wFirst) * outChannels;

            if not TKCLWin64JITBackend.ExecuteKxK(FKernel, node,
                  pInRow, pOutRow, pW, pB,
                  interiorW, inChannels, outChannels, node.KernelSize, inW, node.Stride, kxkAct) then begin
               // JIT unavailable - SSE2 fallback for interior
               for wO := 0 to interiorW - 1 do begin
                  var pIn1 := pInRow + wO * node.Stride * inChannels;
                  var pOut1 := pOutRow + wO * outChannels;
                  SSE2_Copy(pB, pOut1, outChannels);
                  for var ky := 0 to node.KernelSize - 1 do
                     for var kx := 0 to node.KernelSize - 1 do begin
                        var pInK := pIn1 + (ky * inW + kx) * inChannels;
                        var pWK := pW + ((ky * node.KernelSize + kx) * inChannels * outChannels);
                        for var cIn := 0 to inChannels - 1 do
                           SSE2_AddScaled(pWK + cIn * outChannels, pInK[cIn], pOut1, outChannels);
                     end;
               end;
            end;
         end;

         // Apply fused residual and activation to all pixels
         if pResidual <> nil then
            SSE2_Add(pRes, pResidual, pRes, total);
         if (pResidual <> nil) and (act <> actNone) then
            ApplyActivationInPlace(pRes, total, act)
         else if (pResidual = nil) and (act <> actNone) then begin
            // Activation was already applied to interior pixels by JIT;
            // apply only to border pixels
            for hO := 0 to outH - 1 do begin
               for wO := 0 to outW - 1 do begin
                  if (hO >= hFirst) and (hO <= hLast) and (wO >= wFirst) and (wO <= wLast) then
                     Continue;
                  var pixelOff := (hO * outW + wO) * outChannels;
                  ApplyActivationInPlace(pRes + pixelOff, outChannels, act);
               end;
            end;
         end;

         // Alias fused nodes
         if hasFusion then begin
            if fusion.AddNode <> nil then begin
               var addIdx := FNodeToBufferIdx[fusion.AddNode];
               FNodeDims[addIdx] := FNodeDims[n];
               FNodeTotalElements[addIdx] := FNodeTotalElements[n];
               FNodeBuffers[addIdx] := FNodeBuffers[n];
            end;
            if fusion.ActivationNode <> nil then begin
               var actIdx := FNodeToBufferIdx[fusion.ActivationNode];
               FNodeDims[actIdx] := FNodeDims[n];
               FNodeTotalElements[actIdx] := FNodeTotalElements[n];
               FNodeBuffers[actIdx] := FNodeBuffers[n];
            end;
         end;
         Exit;
      end;
   end;

   ProcessConv2DSSE2(n, node, act, pResidual);

   // Alias fused nodes
   if hasFusion then begin
      if fusion.AddNode <> nil then begin
         var addIdx := FNodeToBufferIdx[fusion.AddNode];
         FNodeDims[addIdx] := FNodeDims[n];
         FNodeTotalElements[addIdx] := FNodeTotalElements[n];
         FNodeBuffers[addIdx] := FNodeBuffers[n];
      end;
      if fusion.ActivationNode <> nil then begin
         var actIdx := FNodeToBufferIdx[fusion.ActivationNode];
         FNodeDims[actIdx] := FNodeDims[n];
         FNodeTotalElements[actIdx] := FNodeTotalElements[n];
         FNodeBuffers[actIdx] := FNodeBuffers[n];
      end;
   end;
end;

procedure TKCLJITBackend.ProcessConv2DTranspose(n : Integer; node : TKCLConv2DTransposeNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessConcat(n : Integer; node : TKCLConcatNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessDepthwiseConv2D(n : Integer; node : TKCLDepthwiseConv2DNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessSoftMax(n : Integer; node : TKCLSoftMaxNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessResizeBilinear(n : Integer; node : TKCLResizeBilinearNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessMaxPool2D(n : Integer; node : TKCLMaxPool2DNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessGlobalAvgPool(n : Integer; node : TKCLGlobalAvgPoolNode);
begin
   FlushMapBatch;
   inherited;
end;

procedure TKCLJITBackend.ProcessMap(n : Integer; node : TKCLMapNode);
var
   in1, in2 : Integer;
   total : NativeInt;
   p1, p2, pRes : PDouble;
   optData : TKCLOptimizationData;
   compiled : TKCLCompiledKernel;
   params : array[0..1] of Double;
begin
   // Try batch path: if kernel is pre-compiled, enqueue instead of full dispatch
   optData := TKCLOptimizationData(FKernel.OptimizationData);
   if (optData <> nil) and optData.Prepared and optData.CompiledKernels.TryGetValue(node, compiled) then begin
      if (node is TKCLAddNode) or (node is TKCLSubNode) or (node is TKCLMulNode) then begin
         in1 := FNodeToBufferIdx[node.Inputs[0]];
         in2 := FNodeToBufferIdx[node.Inputs[1]];
         if (FNodeTotalElements[in1] > 0) and (FNodeTotalElements[in1] = FNodeTotalElements[in2]) then begin
            total := FNodeTotalElements[in1];
            // Allocate output buffer if not yet allocated
            FNodeDims[n] := Copy(FNodeDims[in1]);
            FNodeTotalElements[n] := total;
            if Length(FNodeBuffers[n]) < total then SetLength(FNodeBuffers[n], total);
            p1 := PDouble(Pointer(FNodeBuffers[in1]));
            p2 := PDouble(Pointer(FNodeBuffers[in2]));
            pRes := PDouble(Pointer(FNodeBuffers[n]));
            EnqueueMapBatch(compiled, p1, pRes, p2, nil, total);
            Exit;
         end;
      end else if (node is TKCLReLUNode) or (node is TKCLReLU6Node) or (node is TKCLHardSwishNode) then begin
         in1 := FNodeToBufferIdx[node.Inputs[0]];
         if FNodeTotalElements[in1] > 0 then begin
            total := FNodeTotalElements[in1];
            FNodeDims[n] := Copy(FNodeDims[in1]);
            FNodeTotalElements[n] := total;
            if Length(FNodeBuffers[n]) < total then SetLength(FNodeBuffers[n], total);
            p1 := PDouble(Pointer(FNodeBuffers[in1]));
            pRes := PDouble(Pointer(FNodeBuffers[n]));
            EnqueueMapBatch(compiled, p1, pRes, nil, nil, total);
            Exit;
         end;
      end;
      // Note: Dequantize is not batched because it requires a pointer to params
      // (scale/zeroPoint) which would be a dangling stack reference in the batch.
      // It falls through to the individual dispatch path below.
   end;

   // Fall back to individual JIT dispatch
   if (node is TKCLAddNode) or (node is TKCLSubNode) or (node is TKCLMulNode) then begin
      in1 := FNodeToBufferIdx[node.Inputs[0]];
      in2 := FNodeToBufferIdx[node.Inputs[1]];
      if (FNodeTotalElements[in1] > 0) and (FNodeTotalElements[in1] = FNodeTotalElements[in2]) then begin
         total := FNodeTotalElements[in1];
         FNodeDims[n] := Copy(FNodeDims[in1]);
         FNodeTotalElements[n] := total;
         if Length(FNodeBuffers[n]) < total then SetLength(FNodeBuffers[n], total);
         p1 := PDouble(Pointer(FNodeBuffers[in1]));
         p2 := PDouble(Pointer(FNodeBuffers[in2]));
         pRes := PDouble(Pointer(FNodeBuffers[n]));
         if TKCLWin64JITBackend.ExecuteMap(FKernel, node, p1, pRes, p2, nil, total) then Exit;
      end;
   end else if (node is TKCLReLUNode) or (node is TKCLReLU6Node) or (node is TKCLHardSwishNode) then begin
      in1 := FNodeToBufferIdx[node.Inputs[0]];
      if FNodeTotalElements[in1] > 0 then begin
         total := FNodeTotalElements[in1];
         FNodeDims[n] := Copy(FNodeDims[in1]);
         FNodeTotalElements[n] := total;
         if Length(FNodeBuffers[n]) < total then SetLength(FNodeBuffers[n], total);
         p1 := PDouble(Pointer(FNodeBuffers[in1]));
         pRes := PDouble(Pointer(FNodeBuffers[n]));
         if TKCLWin64JITBackend.ExecuteMap(FKernel, node, p1, pRes, nil, nil, total) then Exit;
      end;
   end else if node is TKCLDequantizeNode then begin
      in1 := FNodeToBufferIdx[node.Inputs[0]];
      if FNodeTotalElements[in1] > 0 then begin
         total := FNodeTotalElements[in1];
         FNodeDims[n] := Copy(FNodeDims[in1]);
         FNodeTotalElements[n] := total;
         if Length(FNodeBuffers[n]) < total then SetLength(FNodeBuffers[n], total);
         p1 := PDouble(Pointer(FNodeBuffers[in1]));
         pRes := PDouble(Pointer(FNodeBuffers[n]));
         params[0] := TKCLDequantizeNode(node).Scale;
         params[1] := TKCLDequantizeNode(node).ZeroPoint;
         if TKCLWin64JITBackend.ExecuteMap(FKernel, node, p1, pRes, nil, @params[0], total) then Exit;
      end;
   end;
   FlushMapBatch;
   inherited ProcessMap(n, node);
end;

end.
