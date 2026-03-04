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

{$I dws.inc}
{$POINTERMATH ON}

interface

{$IFDEF WIN64_ASM}

uses
   System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
   dwsUtils, dwsKernelCompilerCommon;

type
   TKCLSSE2Backend = class
   private
      function GetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : TKCLDimensions) : Double;
      procedure SetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : TKCLDimensions; AValue : Double);
   public
      procedure Execute(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
   end;

{$ENDIF}

implementation

{$IFDEF WIN64_ASM}

type
   PInt8 = ^ShortInt;
   PDouble = ^Double;
   PSingle = ^Single;

// ------------------
// ------------------ SSE2 ASM Helpers (Win64) ------------------
// ------------------

procedure SSE2_Add(p1, p2, pr : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done; mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; addpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done; movsd xmm0, qword ptr [rcx]; addsd xmm0, qword ptr [rdx]; movsd qword ptr [r8], xmm0
@done:
end;

procedure SSE2_Sub(p1, p2, pr : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done; mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; subpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done; movsd xmm0, qword ptr [rcx]; subsd xmm0, qword ptr [rdx]; movsd qword ptr [r8], xmm0
@done:
end;

procedure SSE2_Mul(p1, p2, pr : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done; mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; mulpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done; movsd xmm0, qword ptr [rcx]; mulsd xmm0, qword ptr [rdx]; movsd qword ptr [r8], xmm0
@done:
end;

procedure SSE2_Div(p1, p2, pr : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done; mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; divpd xmm0, xmm1; movupd [r8], xmm0
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done; movsd xmm0, qword ptr [rcx]; divsd xmm0, qword ptr [rdx]; movsd qword ptr [r8], xmm0
@done:
end;

procedure SSE2_ReLU(p1, pr : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done; xorpd xmm1, xmm1; mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; maxpd xmm0, xmm1; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done; movsd xmm0, qword ptr [rcx]; maxsd xmm0, xmm1; movsd qword ptr [rdx], xmm0
@done:
end;

procedure SSE2_AddScaled(pSrc : PDouble; scalar : Double; pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done; movsd xmm2, xmm1; unpcklpd xmm2, xmm2; mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; mulpd xmm0, xmm2; movupd xmm3, [r8]; addpd xmm3, xmm0; movupd [r8], xmm3
   add rcx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done; movsd xmm0, qword ptr [rcx]; mulsd xmm0, xmm2; addsd xmm0, qword ptr [r8]; movsd qword ptr [r8], xmm0
@done:
end;

procedure SSE2_MulAdd(pSrc, pWeights, pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r9, r9; jle @done; mov rax, r9; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; mulpd xmm0, xmm1; movupd xmm2, [r8]; addpd xmm2, xmm0; movupd [r8], xmm2
   add rcx, 16; add rdx, 16; add r8, 16; dec rax; jnz @loop
@tail:
   and r9, 1; jz @done; movsd xmm0, qword ptr [rcx]; mulsd xmm0, qword ptr [rdx]; addsd xmm0, qword ptr [r8]; movsd qword ptr [r8], xmm0
@done:
end;

procedure SSE2_Max(p1, pr : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done; mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd xmm1, [rdx]; maxpd xmm0, xmm1; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done; movsd xmm0, qword ptr [rcx]; maxsd xmm0, qword ptr [rdx]; movsd qword ptr [rdx], xmm0
@done:
end;

procedure SSE2_Copy(pSrc, pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done; mov rax, r8; shr rax, 1; jz @tail
@loop:
   movupd xmm0, [rcx]; movupd [rdx], xmm0
   add rcx, 16; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done; movsd xmm0, qword ptr [rcx]; movsd qword ptr [rdx], xmm0
@done:
end;

procedure SSE2_CvtPS2PDContiguous(pSrc : PSingle; pDst : PDouble; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done; mov rax, r8; shr rax, 1; jz @tail
@loop:
   cvtps2pd xmm0, qword ptr [rcx]; movupd [rdx], xmm0
   add rcx, 8; add rdx, 16; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done; cvtss2sd xmm0, dword ptr [rcx]; movsd qword ptr [rdx], xmm0
@done:
end;

procedure SSE2_CvtPD2PSContiguous(pSrc : PDouble; pDst : PSingle; count : NativeInt);
asm
   .noframe
   test r8, r8; jle @done; mov rax, r8; shr rax, 1; jz @tail
@loop:
   cvtpd2ps xmm0, [rcx]; movlps [rdx], xmm0
   add rcx, 16; add rdx, 8; dec rax; jnz @loop
@tail:
   and r8, 1; jz @done; cvtsd2ss xmm0, qword ptr [rcx]; movss dword ptr [rdx], xmm0
@done:
end;

// ------------------
// ------------------ TKCLSSE2Backend ------------------
// ------------------

function TKCLSSE2Backend.GetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : TKCLDimensions) : Double;
var
   idx : Integer;
   offset : NativeInt;
   p : PByte;
begin
   offset := 0;
   for idx := 0 to High(AIndex) do
      offset := offset + AIndex[idx] * ABuffer.Strides[idx];
   p := PByte(ABuffer.BasePointer) + offset;
   case ABuffer.DataType of
      dtInt8 : Result := PInt8(p)^;
      dtFloat16 : Result := HalfToFloat(PHalfFloat(p)^);
      dtFloat32 : Result := PSingle(p)^;
   else Result := 0;
   end;
end;

procedure TKCLSSE2Backend.SetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : TKCLDimensions; AValue : Double);
var
   idx : Integer;
   offset : NativeInt;
   p : PByte;
begin
   offset := 0;
   for idx := 0 to High(AIndex) do
      offset := offset + AIndex[idx] * ABuffer.Strides[idx];
   p := PByte(ABuffer.BasePointer) + offset;
   case ABuffer.DataType of
      dtInt8 : PInt8(p)^ := Max(-128, Min(127, Round(AValue)));
      dtFloat16 : PHalfFloat(p)^ := FloatToHalf(AValue);
      dtFloat32 : PSingle(p)^ := AValue;
   end;
end;

procedure TKCLSSE2Backend.Execute(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
var
   nodeDims : array of TKCLDimensions;
   nodeTotalElements : array of NativeInt;
   nodeBuffers : array of TDoubleDynArray;
   nodeToBufferIdx : TDictionary<TKCLNode, Integer>;
   savedMask : TArithmeticExceptionMask;
   sortedNodes : TList<TKCLNode>;
   visiting : TDictionary<TKCLNode, Boolean>;
   visited : TDictionary<TKCLNode, Boolean>;
   
   n, i, j, k, loopA, stepX, stepY, hO, wO, hiC, wiC, inputCount, outputCount, nx, ny, inC, outC, inH, inW, outH, outW, kS, kH, in1, in2Idx, axisIdxLocal : Integer;
   axisIdxNative, axisSize, totalOther, totalElems, total, inKIdx, oIdx : NativeInt;
   h0, h1, w0, w1 : Integer;
   p1, p2, pr, pI, pW, pB, pWBase, pIBase, pIBaseY, pROut : PDouble;
   tempVal : NativeInt;
   sumVal, resVal, vVal, maxV, sumE, scaleH, scaleW, h_in, w_in, fh, fw, v0, v1 : Double;
   lIdx, idx1, idx2, baseI, c00, c01, c10, c11, dims, dims1, dims2, dOutDims, dimsOutLocal : TKCLDimensions;
   args : TDoubleDynArray;
   node : TKCLNode;
   cv : TKCLConv2DNode;
   dw : TKCLDepthwiseConv2DNode;
   mp : TKCLMaxPool2DNode;
   sm : TKCLSoftMaxNode;
   rs : TKCLResizeBilinearNode;
   dIn, dO_ : TKCLStridedBufferDescriptor;
   curDimOffset : NativeInt;

   function FlatIndex(const AIdx : TKCLDimensions; const ADims : TKCLDimensions) : Integer;
   var
      idx, mult : Integer;
   begin
      Result := 0; mult := 1;
      for idx := High(AIdx) downto 0 do begin
         Result := Result + AIdx[idx] * mult;
         mult := mult * ADims[idx];
      end;
   end;

   procedure IndexFromFlat(AFlat : Integer; const ADims : TKCLDimensions; var AIdx : TKCLDimensions);
   var
      idx : Integer;
   begin
      if Length(AIdx) <> Length(ADims) then SetLength(AIdx, Length(ADims));
      for idx := High(AIdx) downto 0 do begin
         if ADims[idx] > 0 then begin
            AIdx[idx] := AFlat mod ADims[idx];
            AFlat := AFlat div ADims[idx];
         end else AIdx[idx] := 0;
      end;
   end;

   function IsContiguous(const ABuf : TKCLStridedBufferDescriptor) : Boolean;
   var
      idx : Integer;
      sz : NativeInt;
   begin
      sz := 1;
      case ABuf.DataType of
         dtInt8 : sz := 1;
         dtFloat16 : sz := 2;
         dtFloat32 : sz := 4;
      end;
      for idx := High(ABuf.Dimensions) downto 0 do begin
         if ABuf.Strides[idx] <> sz then Exit(False);
         sz := sz * ABuf.Dimensions[idx];
      end;
      Result := True;
   end;

   procedure VisitNode(ANode : TKCLNode);
   var
      idx : Integer;
   begin
      if visited.ContainsKey(ANode) then Exit;
      if visiting.ContainsKey(ANode) then raise EdwsKCLException.Create('KCL: Cycle');
      visiting.Add(ANode, True);
      for idx := 0 to High(ANode.Inputs) do VisitNode(ANode.Inputs[idx]);
      visiting.Remove(ANode);
      visited.Add(ANode, True);
      sortedNodes.Add(ANode);
   end;

   procedure SSE2_SetBias(pDst, pBias : PDouble; totalElements, outC : NativeInt);
   var
      idx : NativeInt;
   begin
      if outC <= 0 then Exit;
      for idx := 0 to (totalElements div outC) - 1 do
         SSE2_Copy(pBias, pDst + (idx * outC), outC);
   end;

begin
   if Length(ABuffers) = 0 then Exit;
   inputCount := Length(AKernel.Inputs);
   outputCount := Length(AKernel.Outputs);

   savedMask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
   try
      sortedNodes := TList<TKCLNode>.Create;
      visiting := TDictionary<TKCLNode, Boolean>.Create;
      visited := TDictionary<TKCLNode, Boolean>.Create;
      try
         for j := 0 to High(AKernel.Outputs) do VisitNode(AKernel.Outputs[j]);
         SetLength(nodeDims, sortedNodes.Count);
         SetLength(nodeTotalElements, sortedNodes.Count);
         SetLength(nodeBuffers, sortedNodes.Count);
         nodeToBufferIdx := TDictionary<TKCLNode, Integer>.Create;
         try
            for n := 0 to sortedNodes.Count - 1 do begin
               node := sortedNodes[n];
               nodeToBufferIdx.Add(node, n);
               if node is TKCLConstantNode then nodeDims[n] := Copy(TKCLConstantNode(node).Dimensions)
               else if node is TKCLInputNode then nodeDims[n] := Copy(ABuffers[TKCLInputNode(node).InputIndex].Dimensions)
               else if node is TKCLMapNode then begin
                  dims := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);
                  if (Length(node.Inputs) > 1) and not (node is TKCLConcatNode) then begin
                     dims2 := nodeDims[nodeToBufferIdx[node.Inputs[1]]];
                     if Length(dims) < Length(dims2) then begin
                        lIdx := dims; SetLength(dims, Length(dims2));
                        for i := 0 to High(dims) do dims[i] := 1;
                        for i := 0 to High(lIdx) do dims[High(dims)-High(lIdx)+i] := lIdx[i];
                     end;
                     for i := 0 to High(dims) do begin
                        k := i - (Length(dims)-Length(dims2));
                        if k >= 0 then dims[i] := Max(dims[i], dims2[k]);
                     end;
                  end;
                  nodeDims[n] := dims;
                  if node is TKCLConcatNode then begin
                     axisIdxLocal := TKCLConcatNode(node).Axis;
                     for k := 1 to High(node.Inputs) do nodeDims[n][axisIdxLocal] := nodeDims[n][axisIdxLocal] + nodeDims[nodeToBufferIdx[node.Inputs[k]]][axisIdxLocal];
                  end;
               end else if node is TKCLConv2DNode then begin
                  dims := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);
                  cv := TKCLConv2DNode(node);
                  if Length(dims) >= 3 then begin
                     dims[High(dims)-2] := (dims[High(dims)-2] + cv.Stride - 1) div cv.Stride;
                     dims[High(dims)-1] := (dims[High(dims)-1] + cv.Stride - 1) div cv.Stride;
                     dims[High(dims)] := Length(cv.Bias);
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLDepthwiseConv2DNode then begin
                  dims := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);
                  dw := TKCLDepthwiseConv2DNode(node);
                  if Length(dims) >= 3 then begin
                     dims[High(dims)-2] := (dims[High(dims)-2] + dw.Stride - 1) div dw.Stride;
                     dims[High(dims)-1] := (dims[High(dims)-1] + dw.Stride - 1) div dw.Stride;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLResizeBilinearNode then begin
                  dims := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);
                  if Length(dims) >= 3 then begin
                     dims[High(dims)-2] := TKCLResizeBilinearNode(node).TargetHeight;
                     dims[High(dims)-1] := TKCLResizeBilinearNode(node).TargetWidth;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLMaxPool2DNode then begin
                  dims := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);
                  mp := TKCLMaxPool2DNode(node);
                  if Length(dims) >= 3 then begin
                     dims[High(dims)-2] := (dims[High(dims)-2] + mp.Stride - 1) div mp.Stride;
                     dims[High(dims)-1] := (dims[High(dims)-1] + mp.Stride - 1) div mp.Stride;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLGlobalAvgPoolNode then begin
                  dims := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);
                  if Length(dims) >= 3 then begin dims[High(dims)-2] := 1; dims[High(dims)-1] := 1; end;
                  nodeDims[n] := dims;
               end else nodeDims[n] := Copy(nodeDims[nodeToBufferIdx[node.Inputs[0]]]);

               totalElems := 1;
               for i := 0 to High(nodeDims[n]) do totalElems := totalElems * nodeDims[n][i];
               nodeTotalElements[n] := totalElems; SetLength(nodeBuffers[n], totalElems);
            end;

            // Output verification
            for j := 0 to High(AKernel.Outputs) do begin
               var outNode := AKernel.Outputs[j];
               var outIdx := nodeToBufferIdx[outNode];
               var outBuf := ABuffers[inputCount + j];
               if Length(nodeDims[outIdx]) <> Length(outBuf.Dimensions) then
                  raise EdwsKCLException.Create('KCL: Output spatial domain length mismatch.');
               for i := 0 to High(nodeDims[outIdx]) do
                  if nodeDims[outIdx][i] <> outBuf.Dimensions[i] then
                     raise EdwsKCLException.Create('KCL: Output spatial domain size mismatch.');
            end;

            for n := 0 to sortedNodes.Count - 1 do begin
               node := sortedNodes[n];
               total := nodeTotalElements[n]; if total = 0 then Continue;
               if node is TKCLConstantNode then begin
                  sumVal := TKCLConstantNode(node).Value; for i := 0 to total-1 do nodeBuffers[n][i] := sumVal;
               end else if node is TKCLInputNode then begin
                  dIn := ABuffers[TKCLInputNode(node).InputIndex];
                  if (dIn.DataType = dtFloat32) and IsContiguous(dIn) then
                     SSE2_CvtPS2PDContiguous(PSingle(dIn.BasePointer), Pointer(nodeBuffers[n]), total)
                  else begin
                     dims := nodeDims[n]; SetLength(lIdx, Length(dims));
                     for i := 0 to total-1 do begin IndexFromFlat(i, dims, lIdx); nodeBuffers[n][i] := GetValue(dIn, lIdx); end;
                  end;
               end else if node is TKCLConcatNode then begin
                  axisIdxLocal := TKCLConcatNode(node).Axis;
                  curDimOffset := 0; dimsOutLocal := nodeDims[n];
                  for k := 0 to High(node.Inputs) do begin
                     inKIdx := nodeToBufferIdx[node.Inputs[k]];
                     dims1 := nodeDims[inKIdx]; SetLength(idx1, Length(dims1));
                     for i := 0 to nodeTotalElements[inKIdx] - 1 do begin
                        IndexFromFlat(i, dims1, idx1);
                        dims2 := Copy(idx1); SetLength(dims2, Length(dimsOutLocal));
                        dims2[axisIdxLocal] := dims2[axisIdxLocal] + curDimOffset;
                        nodeBuffers[n][FlatIndex(dims2, dimsOutLocal)] := nodeBuffers[inKIdx][i];
                     end;
                     curDimOffset := curDimOffset + dims1[axisIdxLocal];
                  end;
               end else if node is TKCLMapNode then begin
                  in1 := nodeToBufferIdx[node.Inputs[0]];
                  in2Idx := -1; if Length(node.Inputs) > 1 then in2Idx := nodeToBufferIdx[node.Inputs[1]];
                  if (in2Idx = -1) or ((nodeTotalElements[in1] = total) and (nodeTotalElements[in2Idx] = total)) then begin
                     p1 := Pointer(nodeBuffers[in1]); pr := Pointer(nodeBuffers[n]);
                     if in2Idx = -1 then begin
                        if node is TKCLReLUNode then SSE2_ReLU(p1, pr, total)
                        else if node is TKCLSigmoidNode then for i := 0 to total-1 do pr[i] := 1.0 / (1.0 + Exp(-p1[i]))
                        else if node is TKCLReLU6Node then for i := 0 to total-1 do begin vVal := p1[i]; if vVal > 6.0 then vVal := 6.0 else if vVal < 0.0 then vVal := 0.0; pr[i] := vVal; end
                        else begin
                           SetLength(args, 2); args[1] := 0;
                           for i := 0 to total-1 do begin args[0] := p1[i]; pr[i] := TKCLMapNode(node).Eval(args); end;
                        end;
                     end else begin
                        p2 := Pointer(nodeBuffers[in2Idx]);
                        if node is TKCLAddNode then SSE2_Add(p1, p2, pr, total)
                        else if node is TKCLSubNode then SSE2_Sub(p1, p2, pr, total)
                        else if node is TKCLMulNode then SSE2_Mul(p1, p2, pr, total)
                        else if node is TKCLDivNode then SSE2_Div(p1, p2, pr, total)
                        else begin
                           SetLength(args, 2);
                           for i := 0 to total-1 do begin args[0] := p1[i]; args[1] := p2[i]; pr[i] := TKCLMapNode(node).Eval(args); end;
                        end;
                     end;
                  end else begin
                     dims := nodeDims[n]; in1 := nodeToBufferIdx[node.Inputs[0]]; dims1 := nodeDims[in1]; dims2 := nodeDims[in2Idx];
                     SetLength(lIdx, Length(dims)); SetLength(idx1, Length(dims1)); SetLength(idx2, Length(dims2));
                     SetLength(args, 2);
                     for i := 0 to total-1 do begin
                        IndexFromFlat(i, dims, lIdx);
                        for k := 0 to High(idx1) do begin j := k+(Length(dims)-Length(dims1)); if dims1[k]=1 then idx1[k]:=0 else idx1[k]:=lIdx[j]; end;
                        for k := 0 to High(idx2) do begin j := k+(Length(dims)-Length(dims2)); if dims2[k]=1 then idx2[k]:=0 else idx2[k]:=lIdx[j]; end;
                        args[0] := nodeBuffers[in1][FlatIndex(idx1, dims1)]; args[1] := nodeBuffers[in2Idx][FlatIndex(idx2, dims2)];
                        nodeBuffers[n][i] := TKCLMapNode(node).Eval(args);
                     end;
                  end;
               end else if node is TKCLConv2DNode then begin
                  cv := TKCLConv2DNode(node); in1 := nodeToBufferIdx[node.Inputs[0]];
                  dims := nodeDims[in1]; dOutDims := nodeDims[n];
                  inC := dims[High(dims)]; outC := dOutDims[High(dOutDims)];
                  inH := dims[High(dims)-2]; inW := dims[High(dims)-1];
                  outH := dOutDims[High(dOutDims)-2]; outW := dOutDims[High(dOutDims)-1];
                  kS := cv.KernelSize; kH := kS div 2;
                  pI := Pointer(nodeBuffers[in1]); pr := Pointer(nodeBuffers[n]);
                  pW := Pointer(cv.Weights); pB := Pointer(cv.Bias);
                  SSE2_SetBias(pr, pB, total, outC);
                  for hO := 0 to outH - 1 do begin
                     hiC := hO * cv.Stride;
                     for wO := 0 to outW - 1 do begin
                        wiC := wO * cv.Stride;
                        pROut := pr + (hO * outW + wO) * outC;
                        for stepY := -kH to kH do begin
                           ny := hiC + stepY;
                           if (ny >= 0) and (ny < inH) then begin
                              pIBaseY := pI + (ny * inW * inC);
                              for stepX := -kH to kH do begin
                                 nx := wiC + stepX;
                                 if (nx >= 0) and (nx < inW) then begin
                                    pWBase := pW + (((stepY + kH) * kS + (stepX + kH)) * inC * outC);
                                    pIBase := pIBaseY + (nx * inC);
                                    for k := 0 to inC - 1 do
                                       SSE2_AddScaled(pWBase + (k * outC), (pIBase + k)^, pROut, outC);
                                 end;
                              end;
                           end;
                        end;
                     end;
                  end;
               end else if node is TKCLDepthwiseConv2DNode then begin
                  dw := TKCLDepthwiseConv2DNode(node); in1 := nodeToBufferIdx[node.Inputs[0]];
                  dims := nodeDims[in1]; dOutDims := nodeDims[n];
                  inC := dims[High(dims)];
                  inH := dims[High(dims)-2]; inW := dims[High(dims)-1];
                  outH := dOutDims[High(dOutDims)-2]; outW := dOutDims[High(dOutDims)-1];
                  kS := dw.KernelSize; kH := kS div 2;
                  pI := Pointer(nodeBuffers[in1]); pr := Pointer(nodeBuffers[n]);
                  pW := Pointer(dw.Weights); pB := Pointer(dw.Bias);
                  SSE2_SetBias(pr, pB, total, inC);
                  for hO := 0 to outH - 1 do begin
                     hiC := hO * dw.Stride;
                     for wO := 0 to outW - 1 do begin
                        wiC := wO * dw.Stride;
                        pROut := pr + (hO * outW + wO) * inC;
                        for stepY := -kH to kH do begin
                           ny := hiC + stepY;
                           if (ny >= 0) and (ny < inH) then begin
                              pIBaseY := pI + (ny * inW * inC);
                              for stepX := -kH to kH do begin
                                 nx := wiC + stepX;
                                 if (nx >= 0) and (nx < inW) then begin
                                    pWBase := pW + (((stepY + kH) * kS + (stepX + kH)) * inC);
                                    pIBase := pIBaseY + (nx * inC);
                                    SSE2_MulAdd(pIBase, pWBase, pROut, inC);
                                 end;
                              end;
                           end;
                        end;
                     end;
                  end;
               end else if node is TKCLSoftMaxNode then begin
                  in1 := nodeToBufferIdx[node.Inputs[0]]; sm := TKCLSoftMaxNode(node);
                  dims := nodeDims[in1]; axisIdxNative := sm.Axis; axisSize := dims[axisIdxNative];
                  totalOther := 1; for k := 0 to High(dims) do if k<>axisIdxNative then totalOther := totalOther * dims[k];
                  SetLength(baseI, Length(dims));
                  for i := 0 to totalOther-1 do begin
                     tempVal := i; for k := High(dims) downto 0 do if k<>axisIdxNative then begin baseI[k] := tempVal mod dims[k]; tempVal := tempVal div dims[k]; end else baseI[k]:=0;
                     maxV := -1e30; for loopA := 0 to axisSize-1 do begin baseI[axisIdxNative]:=loopA; vVal := nodeBuffers[in1][FlatIndex(baseI, dims)]; if vVal > maxV then maxV := vVal; end;
                     sumE := 0; for loopA := 0 to axisSize-1 do begin baseI[axisIdxNative]:=loopA; sumE := sumE + Exp(nodeBuffers[in1][FlatIndex(baseI, dims)]-maxV); end;
                     for loopA := 0 to axisSize-1 do begin baseI[axisIdxNative]:=loopA; nodeBuffers[n][FlatIndex(baseI, dims)] := Exp(nodeBuffers[in1][FlatIndex(baseI, dims)]-maxV)/sumE; end;
                  end;
               end else if node is TKCLResizeBilinearNode then begin
                  rs := TKCLResizeBilinearNode(node); in1 := nodeToBufferIdx[node.Inputs[0]];
                  dims := nodeDims[in1];
                  dimsOutLocal := nodeDims[n]; SetLength(lIdx, Length(dimsOutLocal));
                  for i := 0 to total-1 do begin
                     IndexFromFlat(i, dimsOutLocal, lIdx); resVal := 0;
                     if Length(dimsOutLocal) >= 3 then begin
                        axisIdxLocal := High(dimsOutLocal)-2; stepX := High(dimsOutLocal)-1; scaleH := dims[axisIdxLocal]/dimsOutLocal[axisIdxLocal]; scaleW := dims[stepX]/dimsOutLocal[stepX];
                        h_in := lIdx[axisIdxLocal]*scaleH; w_in := lIdx[stepX]*scaleW; h0 := Floor(h_in); h1 := Min(h0+1, dims[axisIdxLocal]-1);
                        w0 := Floor(w_in); w1 := Min(w0+1, dims[stepX]-1); fh := h_in-h0; fw := w_in-w0;
                        SetLength(c00, Length(dims)); for k := 0 to High(dims) do c00[k] := lIdx[k]; c00[axisIdxLocal]:=h0; c00[stepX]:=w0;
                        SetLength(c01, Length(dims)); for k := 0 to High(dims) do c01[k] := lIdx[k]; c01[axisIdxLocal]:=h0; c01[stepX]:=w1;
                        SetLength(c10, Length(dims)); for k := 0 to High(dims) do c10[k] := lIdx[k]; c10[axisIdxLocal]:=h1; c10[stepX]:=w0;
                        SetLength(c11, Length(dims)); for k := 0 to High(dims) do c11[k] := lIdx[k]; c11[axisIdxLocal]:=h1; c11[stepX]:=w1;
                        v0 := nodeBuffers[in1][FlatIndex(c00, dims)]*(1-fw) + nodeBuffers[in1][FlatIndex(c01, dims)]*fw;
                        v1 := nodeBuffers[in1][FlatIndex(c10, dims)]*(1-fw) + nodeBuffers[in1][FlatIndex(c11, dims)]*fw;
                        resVal := v0*(1-fh) + v1*fh;
                     end;
                     nodeBuffers[n][i] := resVal;
                  end;
               end else if node is TKCLMaxPool2DNode then begin
                  mp := TKCLMaxPool2DNode(node); in1 := nodeToBufferIdx[node.Inputs[0]];
                  dims := nodeDims[in1]; dOutDims := nodeDims[n];
                  inC := dims[High(dims)];
                  inH := dims[High(dims)-2]; inW := dims[High(dims)-1];
                  outH := dOutDims[High(dOutDims)-2]; outW := dOutDims[High(dOutDims)-1];
                  kS := mp.KernelSize; kH := kS div 2;
                  pI := Pointer(nodeBuffers[in1]); pr := Pointer(nodeBuffers[n]);
                  for i := 0 to total - 1 do pr[i] := -1e30;
                  for hO := 0 to outH - 1 do begin
                     hiC := hO * mp.Stride;
                     for wO := 0 to outW - 1 do begin
                        wiC := wO * mp.Stride;
                        pROut := pr + (hO * outW + wO) * inC;
                        for stepY := -kH to kH do begin
                           ny := hiC + stepY;
                           if (ny >= 0) and (ny < inH) then begin
                              pIBaseY := pI + (ny * inW * inC);
                              for stepX := -kH to kH do begin
                                 nx := wiC + stepX;
                                 if (nx >= 0) and (nx < inW) then begin
                                    pIBase := pIBaseY + (nx * inC);
                                    SSE2_Max(pIBase, pROut, inC);
                                 end;
                              end;
                           end;
                        end;
                     end;
                  end;
               end else if node is TKCLGlobalAvgPoolNode then begin
                  in1 := nodeToBufferIdx[node.Inputs[0]];
                  dims := nodeDims[in1];
                  if Length(dims) >= 3 then begin
                     inC := dims[High(dims)];
                     inH := dims[High(dims)-2]; inW := dims[High(dims)-1];
                     pI := Pointer(nodeBuffers[in1]); pr := Pointer(nodeBuffers[n]);
                     for i := 0 to total - 1 do pr[i] := 0;
                     for ny := 0 to inH - 1 do begin
                        for nx := 0 to inW - 1 do begin
                           pIBase := pI + (ny * inW + nx) * inC;
                           SSE2_Add(pIBase, pr, pr, inC);
                        end;
                     end;
                     vVal := 1.0 / (inH * inW);
                     for i := 0 to total - 1 do pr[i] := pr[i] * vVal;
                  end else begin
                     sumVal := 0; for i := 0 to nodeTotalElements[in1]-1 do sumVal := sumVal + nodeBuffers[in1][i];
                     if nodeTotalElements[in1] > 0 then sumVal := sumVal / nodeTotalElements[in1];
                     for i := 0 to total-1 do nodeBuffers[n][i] := sumVal;
                  end;
               end;
            end;

            for j := 0 to High(AKernel.Outputs) do begin
               oIdx := nodeToBufferIdx[AKernel.Outputs[j]]; dO_ := ABuffers[inputCount+j];
               if (dO_.DataType = dtFloat32) and IsContiguous(dO_) then
                  SSE2_CvtPD2PSContiguous(Pointer(nodeBuffers[oIdx]), PSingle(dO_.BasePointer), nodeTotalElements[oIdx])
               else begin
                  dimsOutLocal := nodeDims[oIdx]; SetLength(lIdx, Length(dimsOutLocal));
                  for i := 0 to nodeTotalElements[oIdx]-1 do begin IndexFromFlat(i, dimsOutLocal, lIdx); SetValue(dO_, lIdx, nodeBuffers[oIdx][i]); end;
               end;
            end;
         finally nodeToBufferIdx.Free; end;
      finally sortedNodes.Free; visiting.Free; visited.Free; end;
   finally SetExceptionMask(savedMask); end;
end;

{$ENDIF}

end.
