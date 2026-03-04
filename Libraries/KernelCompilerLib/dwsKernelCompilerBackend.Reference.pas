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
unit dwsKernelCompilerBackend.Reference;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
   dwsUtils, dwsKernelCompilerCommon;

type
   PInt8 = ^ShortInt;

   TKCLReferenceBackend = class
   private
      function GetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : array of Integer) : Double;
      procedure SetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : array of Integer; AValue : Double);
   public
      procedure Execute(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------
// ------------------ TKCLReferenceBackend ------------------
// ------------------

// GetValue
//
function TKCLReferenceBackend.GetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : array of Integer) : Double;
begin
   var offset : NativeInt := 0;
   for var i := 0 to High(AIndex) do
      offset := offset + AIndex[i] * ABuffer.Strides[i];
      
   var size : NativeInt := 1;
   case ABuffer.DataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;
   
   if (offset < 0) or (offset + size > ABuffer.Capacity) then
      raise EdwsKCLException.Create('KCL: Buffer overflow during read.');

   var p := PByte(ABuffer.BasePointer) + offset;
   case ABuffer.DataType of
      dtInt8 : Result := PInt8(p)^;
      dtFloat16 : Result := HalfToFloat(PHalfFloat(p)^);
      dtFloat32 : Result := PSingle(p)^;
   else
      Result := 0;
   end;
end;

// SetValue
//
procedure TKCLReferenceBackend.SetValue(const ABuffer : TKCLStridedBufferDescriptor; const AIndex : array of Integer; AValue : Double);
begin
   var offset : NativeInt := 0;
   for var i := 0 to High(AIndex) do
      offset := offset + AIndex[i] * ABuffer.Strides[i];

   var size : NativeInt := 1;
   case ABuffer.DataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;
   
   if (offset < 0) or (offset + size > ABuffer.Capacity) then
      raise EdwsKCLException.Create('KCL: Buffer overflow during write.');

   var p := PByte(ABuffer.BasePointer) + offset;
   case ABuffer.DataType of
      dtInt8 : begin
         var intVal := Round(AValue);
         if intVal < -128 then intVal := -128
         else if intVal > 127 then intVal := 127;
         PInt8(p)^ := intVal;
      end;
      dtFloat16 : PHalfFloat(p)^ := FloatToHalf(AValue);
      dtFloat32 : PSingle(p)^ := AValue;
   else
      // Unknown data type
   end;
end;

// Execute
//
procedure TKCLReferenceBackend.Execute(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
var
   nodeDims : array of TKCLDimensions;
   nodeTotalElements : array of NativeInt;
   nodeBuffers : array of TDoubleDynArray;
   nodeToBufferIdx : TDictionary<TKCLNode, Integer>;
   savedMask : TArithmeticExceptionMask;
   sortedNodes : TList<TKCLNode>;
   visiting : TDictionary<TKCLNode, Boolean>;
   visited : TDictionary<TKCLNode, Boolean>;
   indices : TArray<Integer>;

   function FlatIndex(const AIdx : array of Integer; const ADims : TKCLDimensions) : Integer;
   begin
      Result := 0;
      var mult := 1;
      for var i := High(AIdx) downto 0 do begin
         Result := Result + AIdx[i] * mult;
         mult := mult * ADims[i];
      end;
   end;

   procedure IndexFromFlat(AFlat : Integer; const ADims : TKCLDimensions; var AIdx : array of Integer);
   begin
      for var i := High(AIdx) downto 0 do begin
         if ADims[i] > 0 then begin
            AIdx[i] := AFlat mod ADims[i];
            AFlat := AFlat div ADims[i];
         end else
            AIdx[i] := 0;
      end;
   end;

   procedure VisitNode(ANode : TKCLNode);
   begin
      if visited.ContainsKey(ANode) then Exit;
      if visiting.ContainsKey(ANode) then
         raise EdwsKCLException.Create('KCL: Hazard ordering violation (Cycle detected).');

      visiting.Add(ANode, True);

      for var i := 0 to High(ANode.Inputs) do
         VisitNode(ANode.Inputs[i]);

      visiting.Remove(ANode);
      visited.Add(ANode, True);
      sortedNodes.Add(ANode);
   end;

begin
   if Length(ABuffers) = 0 then Exit;
   
   var inputCount := Length(AKernel.Inputs);
   var outputCount := Length(AKernel.Outputs);

   if Length(ABuffers) < inputCount + outputCount then
      raise EdwsKCLException.CreateFmt('TKCLReferenceBackend.Execute: Expected at least %d buffers, but got %d', [inputCount + outputCount, Length(ABuffers)]);

   // Capacity validation for all buffers independent of DAG bounds
   for var b := 0 to High(ABuffers) do begin
      var totalElements : NativeInt := 1;
      for var i := 0 to High(ABuffers[b].Dimensions) do
         totalElements := totalElements * ABuffers[b].Dimensions[i];
         
      var maxOffset : NativeInt := 0;
      var minOffset : NativeInt := 0;
      if totalElements > 0 then begin
         for var d := 0 to High(ABuffers[b].Dimensions) do begin
            var extent := (ABuffers[b].Dimensions[d] - 1) * ABuffers[b].Strides[d];
            if extent > 0 then
               maxOffset := maxOffset + extent
            else
               minOffset := minOffset + extent;
         end;
      end;
      
      var size : NativeInt := 1;
      case ABuffers[b].DataType of
         dtInt8 : size := 1;
         dtFloat16 : size := 2;
         dtFloat32 : size := 4;
      end;
      
      if (minOffset < 0) or (maxOffset + size > ABuffers[b].Capacity) then
         raise EdwsKCLException.Create('KCL: Dynamic bounds exceed logical Capacity.');
   end;

   // Mask FPU Exceptions
   savedMask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
   try
      sortedNodes := TList<TKCLNode>.Create;
      visiting := TDictionary<TKCLNode, Boolean>.Create;
      visited := TDictionary<TKCLNode, Boolean>.Create;
      try
         for var j := 0 to High(AKernel.Outputs) do
            VisitNode(AKernel.Outputs[j]);

         SetLength(nodeDims, sortedNodes.Count);
         SetLength(nodeTotalElements, sortedNodes.Count);
         SetLength(nodeBuffers, sortedNodes.Count);
            
         nodeToBufferIdx := TDictionary<TKCLNode, Integer>.Create;
         try
            // 1. Calculate and propagate dimensions and total elements
            for var n := 0 to sortedNodes.Count - 1 do begin
               var node := sortedNodes[n];
               nodeToBufferIdx.Add(node, n);
               
               if node is TKCLConstantNode then begin
                  nodeDims[n] := Copy(TKCLConstantNode(node).Dimensions);
               end else if node is TKCLInputNode then begin
                  var inIdx := TKCLInputNode(node).InputIndex;
                  nodeDims[n] := Copy(ABuffers[inIdx].Dimensions);
               end else if node is TKCLMapNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims := Copy(nodeDims[in1Idx]);
                  if (Length(node.Inputs) > 1) and not (node is TKCLConcatNode) then begin
                     var in2Idx := nodeToBufferIdx[node.Inputs[1]];
                     var dims2 := nodeDims[in2Idx];
                     if Length(dims) < Length(dims2) then begin
                        var oldDims := dims;
                        SetLength(dims, Length(dims2));
                        for var i := 0 to High(dims) do dims[i] := 1;
                        for var i := 0 to High(oldDims) do dims[High(dims) - High(oldDims) + i] := oldDims[i];
                     end;
                     for var i := 0 to High(dims) do begin
                        var d2Idx := i - (Length(dims) - Length(dims2));
                        var d2 := 1; if d2Idx >= 0 then d2 := dims2[d2Idx];
                        if (dims[i] <> d2) and (dims[i] <> 1) and (d2 <> 1) then
                           raise EdwsKCLException.Create('KCL: Incompatible shapes for broadcasting.');
                        dims[i] := Max(dims[i], d2);
                     end;
                  end;
                  nodeDims[n] := dims;
                  if node is TKCLConcatNode then begin
                     var cNode := TKCLConcatNode(node);
                     var ax := cNode.Axis;
                     var newDim := nodeDims[n][ax];
                     for var k := 1 to High(node.Inputs) do begin
                        var inKIdx := nodeToBufferIdx[node.Inputs[k]];
                        newDim := newDim + nodeDims[inKIdx][ax];
                     end;
                     nodeDims[n][ax] := newDim;
                  end;
               end else if node is TKCLConv2DNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims := Copy(nodeDims[in1Idx]);
                  var cvNode := TKCLConv2DNode(node);
                  var st := cvNode.Stride;
                  if Length(dims) >= 3 then begin
                     var dimH := High(dims) - 2;
                     var dimW := High(dims) - 1;
                     var outC := Length(cvNode.Bias);
                     dims[dimH] := (dims[dimH] + st - 1) div st;
                     dims[dimW] := (dims[dimW] + st - 1) div st;
                     dims[High(dims)] := outC;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLDepthwiseConv2DNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims := Copy(nodeDims[in1Idx]);
                  var dwNode := TKCLDepthwiseConv2DNode(node);
                  var st := dwNode.Stride;
                  if Length(dims) >= 3 then begin
                     var dimH := High(dims) - 2;
                     var dimW := High(dims) - 1;
                     dims[dimH] := (dims[dimH] + st - 1) div st;
                     dims[dimW] := (dims[dimW] + st - 1) div st;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLResizeBilinearNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims := Copy(nodeDims[in1Idx]);
                  var rsNode := TKCLResizeBilinearNode(node);
                  if Length(dims) >= 3 then begin
                     var dimH := High(dims) - 2;
                     var dimW := High(dims) - 1;
                     dims[dimH] := rsNode.TargetHeight;
                     dims[dimW] := rsNode.TargetWidth;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLMaxPool2DNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims := Copy(nodeDims[in1Idx]);
                  var mpNode := TKCLMaxPool2DNode(node);
                  var st := mpNode.Stride;
                  if Length(dims) >= 3 then begin
                     var dimH := High(dims) - 2;
                     var dimW := High(dims) - 1;
                     dims[dimH] := (dims[dimH] + st - 1) div st;
                     dims[dimW] := (dims[dimW] + st - 1) div st;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLGlobalAvgPoolNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims := Copy(nodeDims[in1Idx]);
                  if Length(dims) >= 3 then begin
                     dims[High(dims) - 2] := 1;
                     dims[High(dims) - 1] := 1;
                  end;
                  nodeDims[n] := dims;
               end else if node is TKCLSoftMaxNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  nodeDims[n] := Copy(nodeDims[in1Idx]);
               end;

               var elems : NativeInt := 1;

               for var i := 0 to High(nodeDims[n]) do
                  elems := elems * nodeDims[n][i];
               nodeTotalElements[n] := elems;
               
               SetLength(nodeBuffers[n], elems);
               for var j := 0 to elems - 1 do
                  nodeBuffers[n][j] := 0.0;
            end;
            
            // 2. Output verification
            for var j := 0 to High(AKernel.Outputs) do begin
               var outNode := AKernel.Outputs[j];
               var outIdx := nodeToBufferIdx[outNode];
               var outBuf := ABuffers[inputCount + j];
               if Length(nodeDims[outIdx]) <> Length(outBuf.Dimensions) then
                  raise EdwsKCLException.Create('KCL: Output spatial domain length mismatch.');
               for var i := 0 to High(nodeDims[outIdx]) do
                  if nodeDims[outIdx][i] <> outBuf.Dimensions[i] then
                     raise EdwsKCLException.Create('KCL: Output spatial domain size mismatch.');
            end;

            // 3. Topological evaluation
            for var n := 0 to sortedNodes.Count - 1 do begin
               var node := sortedNodes[n];
               var dimsOut := nodeDims[n];
               SetLength(indices, Length(dimsOut));
               
               if node is TKCLConstantNode then begin
                  var val := TKCLConstantNode(node).Value;
                  for var i := 0 to nodeTotalElements[n] - 1 do
                     nodeBuffers[n][i] := val;
               end else if node is TKCLInputNode then begin
                  var inIdx := TKCLInputNode(node).InputIndex;
                  for var i := 0 to nodeTotalElements[n] - 1 do begin
                     IndexFromFlat(i, dimsOut, indices);
                     nodeBuffers[n][i] := GetValue(ABuffers[inIdx], indices);
                  end;
               end else if node is TKCLConcatNode then begin
                  var cNode := TKCLConcatNode(node);
                  var ax := cNode.Axis;
                  var curDimOffset := 0;
                  for var k := 0 to High(node.Inputs) do begin
                     var inKIdx := nodeToBufferIdx[node.Inputs[k]];
                     var inKDims := nodeDims[inKIdx];
                     var inKElems := nodeTotalElements[inKIdx];
                     var inKIndices : TArray<Integer>;
                     SetLength(inKIndices, Length(inKDims));
                     
                     for var i := 0 to inKElems - 1 do begin
                        IndexFromFlat(i, inKDims, inKIndices);
                        var outIdxs := Copy(inKIndices);
                        outIdxs[ax] := outIdxs[ax] + curDimOffset;
                        nodeBuffers[n][FlatIndex(outIdxs, dimsOut)] := nodeBuffers[inKIdx][i];
                     end;
                     curDimOffset := curDimOffset + inKDims[ax];
                  end;
               end else if node is TKCLMapNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dims1 := nodeDims[in1Idx];
                  var in2Idx := -1;
                  if Length(node.Inputs) > 1 then in2Idx := nodeToBufferIdx[node.Inputs[1]];

                  var sameShape := False;
                  if in2Idx >= 0 then begin
                     var dims2 := nodeDims[in2Idx];
                     if Length(dims1) = Length(dims2) then begin
                        sameShape := True;
                        for var k := 0 to High(dims1) do if dims1[k] <> dims2[k] then begin
                           sameShape := False;
                           break;
                        end;
                     end;
                  end else sameShape := True;

                  if sameShape then begin
                     // Fast Path: matching shapes
                     for var i := 0 to nodeTotalElements[n] - 1 do begin
                        var val1 := nodeBuffers[in1Idx][i];
                        var val2 : Double := 0;
                        if in2Idx >= 0 then val2 := nodeBuffers[in2Idx][i];
                        var evalArgs : TDoubleDynArray;
                        SetLength(evalArgs, 2);
                        evalArgs[0] := val1;
                        evalArgs[1] := val2;
                        nodeBuffers[n][i] := TKCLMapNode(node).Eval(evalArgs);
                     end;
                  end else begin
                     // Broadcasting Path
                     var dims2 := nodeDims[in2Idx];
                     var idx1, idx2 : array of Integer;
                     SetLength(idx1, Length(dims1));
                     SetLength(idx2, Length(dims2));

                     for var i := 0 to nodeTotalElements[n] - 1 do begin
                        IndexFromFlat(i, dimsOut, indices);

                        for var k := 0 to High(idx1) do begin
                           var outIdx := k + (Length(dimsOut) - Length(dims1));
                           if dims1[k] = 1 then idx1[k] := 0 else idx1[k] := indices[outIdx];
                        end;
                        for var k := 0 to High(idx2) do begin
                           var outIdx := k + (Length(dimsOut) - Length(dims2));
                           if dims2[k] = 1 then idx2[k] := 0 else idx2[k] := indices[outIdx];
                        end;

                        var val1 := nodeBuffers[in1Idx][FlatIndex(idx1, dims1)];
                        var val2 := nodeBuffers[in2Idx][FlatIndex(idx2, dims2)];

                        var evalArgs : TDoubleDynArray;
                        SetLength(evalArgs, 2);
                        evalArgs[0] := val1;
                        evalArgs[1] := val2;
                        nodeBuffers[n][i] := TKCLMapNode(node).Eval(evalArgs);
                     end;
                  end;
               end else if node is TKCLConv2DNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var cvNode := TKCLConv2DNode(node);
                  var kSize := cvNode.KernelSize;
                  var kHalf := kSize div 2;
                  var st := cvNode.Stride;
                  var dimsIn := nodeDims[in1Idx];
                  var inChannels := dimsIn[High(dimsIn)];
                  var outChannels := dimsOut[High(dimsOut)];
                  
                  for var i := 0 to nodeTotalElements[n] - 1 do begin
                     IndexFromFlat(i, dimsOut, indices);
                     var sum : Double := 0.0;
                     if Length(dimsOut) >= 3 then begin
                        var cOut := indices[High(dimsOut)];
                        var dimH := High(dimsOut) - 2;
                        var dimW := High(dimsOut) - 1;
                        var h_out := indices[dimH];
                        var w_out := indices[dimW];
                        var h_in_center := h_out * st;
                        var w_in_center := w_out * st;
                        
                        if cOut < Length(cvNode.Bias) then sum := cvNode.Bias[cOut];
                        
                        for var dy := -kHalf to kHalf do begin
                           for var dx := -kHalf to kHalf do begin
                              var ny := h_in_center + dy;
                              var nx := w_in_center + dx;
                              if (ny >= 0) and (ny < dimsIn[dimH]) and (nx >= 0) and (nx < dimsIn[dimW]) then begin
                                 for var cIn := 0 to inChannels - 1 do begin
                                    var nIndices := Copy(indices);
                                    nIndices[dimH] := ny;
                                    nIndices[dimW] := nx;
                                    nIndices[High(dimsOut)] := cIn;
                                    
                                    var wIdx := (((dy + kHalf) * kSize + (dx + kHalf)) * inChannels + cIn) * outChannels + cOut;
                                    if wIdx < Length(cvNode.Weights) then
                                       sum := sum + nodeBuffers[in1Idx][FlatIndex(nIndices, dimsIn)] * cvNode.Weights[wIdx];
                                 end;
                              end;
                           end;
                        end;
                     end;
                     nodeBuffers[n][i] := sum;
                  end;
               end else if node is TKCLDepthwiseConv2DNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dwNode := TKCLDepthwiseConv2DNode(node);
                  var kSize := dwNode.KernelSize;
                  var kHalf := kSize div 2;
                  var st := dwNode.Stride;
                  var dimsIn := nodeDims[in1Idx];
                  var channels := dimsIn[High(dimsIn)];
                  
                  for var i := 0 to nodeTotalElements[n] - 1 do begin
                     IndexFromFlat(i, dimsOut, indices);
                     var sum : Double := 0.0;
                     if Length(dimsOut) >= 3 then begin
                        var cIdx := indices[High(dimsOut)];
                        var dimH := High(dimsOut) - 2;
                        var dimW := High(dimsOut) - 1;
                        var h_out := indices[dimH];
                        var w_out := indices[dimW];
                        var h_in_center := h_out * st;
                        var w_in_center := w_out * st;
                        
                        if cIdx < Length(dwNode.Bias) then sum := dwNode.Bias[cIdx];
                        
                        for var dy := -kHalf to kHalf do begin
                           for var dx := -kHalf to kHalf do begin
                              var ny := h_in_center + dy;
                              var nx := w_in_center + dx;
                              if (ny >= 0) and (ny < dimsIn[dimH]) and (nx >= 0) and (nx < dimsIn[dimW]) then begin
                                 var nIndices := Copy(indices);
                                 nIndices[dimH] := ny;
                                 nIndices[dimW] := nx;
                                 var wIdx := ((dy + kHalf) * kSize + (dx + kHalf)) * channels + cIdx;
                                 if wIdx < Length(dwNode.Weights) then
                                    sum := sum + nodeBuffers[in1Idx][FlatIndex(nIndices, dimsIn)] * dwNode.Weights[wIdx];
                              end;
                           end;
                        end;
                     end;
                     nodeBuffers[n][i] := sum;
                  end;
               end else if node is TKCLResizeBilinearNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dimsIn := nodeDims[in1Idx];
                  
                  for var i := 0 to nodeTotalElements[n] - 1 do begin
                     IndexFromFlat(i, dimsOut, indices);
                     var val : Double := 0.0;
                     if Length(dimsOut) >= 3 then begin
                        var dimH := High(dimsOut) - 2;
                        var dimW := High(dimsOut) - 1;
                        var h_out := indices[dimH];
                        var w_out := indices[dimW];
                        
                        var scaleH := dimsIn[dimH] / dimsOut[dimH];
                        var scaleW := dimsIn[dimW] / dimsOut[dimW];
                        
                        var h_in := h_out * scaleH;
                        var w_in := w_out * scaleW;
                        
                        var h0 := Floor(h_in);
                        var h1 := h0 + 1;
                        if h1 >= dimsIn[dimH] then h1 := h0;
                        var w0 := Floor(w_in);
                        var w1 := w0 + 1;
                        if w1 >= dimsIn[dimW] then w1 := w0;
                        
                        var fh := h_in - h0;
                        var fw := w_in - w0;
                        
                        var idx00 := Copy(indices); idx00[dimH] := h0; idx00[dimW] := w0;
                        var idx01 := Copy(indices); idx01[dimH] := h0; idx01[dimW] := w1;
                        var idx10 := Copy(indices); idx10[dimH] := h1; idx10[dimW] := w0;
                        var idx11 := Copy(indices); idx11[dimH] := h1; idx11[dimW] := w1;
                        
                        var v00 := nodeBuffers[in1Idx][FlatIndex(idx00, dimsIn)];
                        var v01 := nodeBuffers[in1Idx][FlatIndex(idx01, dimsIn)];
                        var v10 := nodeBuffers[in1Idx][FlatIndex(idx10, dimsIn)];
                        var v11 := nodeBuffers[in1Idx][FlatIndex(idx11, dimsIn)];
                        
                        var v0 := v00 * (1 - fw) + v01 * fw;
                        var v1 := v10 * (1 - fw) + v11 * fw;
                        val := v0 * (1 - fh) + v1 * fh;
                     end;
                     nodeBuffers[n][i] := val;
                  end;
               end else if node is TKCLMaxPool2DNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var mpNode := TKCLMaxPool2DNode(node);
                  var kSize := mpNode.KernelSize;
                  var kHalf := kSize div 2;
                  var st := mpNode.Stride;
                  var dimsIn := nodeDims[in1Idx];
                  
                  for var i := 0 to nodeTotalElements[n] - 1 do begin
                     IndexFromFlat(i, dimsOut, indices);
                     var maxVal : Double := -1e30;
                     if Length(dimsOut) >= 3 then begin
                        var dimH := High(dimsOut) - 2;
                        var dimW := High(dimsOut) - 1;
                        var h_out := indices[dimH];
                        var w_out := indices[dimW];
                        var h_in_center := h_out * st;
                        var w_in_center := w_out * st;

                        for var dy := -kHalf to kHalf do begin
                           for var dx := -kHalf to kHalf do begin
                              var ny := h_in_center + dy;
                              var nx := w_in_center + dx;
                              if (ny >= 0) and (ny < dimsIn[dimH]) and (nx >= 0) and (nx < dimsIn[dimW]) then begin
                                 var nIndices := Copy(indices);
                                 nIndices[dimH] := ny;
                                 nIndices[dimW] := nx;
                                 var val := nodeBuffers[in1Idx][FlatIndex(nIndices, dimsIn)];
                                 if val > maxVal then maxVal := val;
                              end;
                           end;
                        end;
                     end;
                     nodeBuffers[n][i] := maxVal;
                  end;
               end else if node is TKCLSoftMaxNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var smNode := TKCLSoftMaxNode(node);
                  var axis := smNode.Axis;
                  var dimsIn := nodeDims[in1Idx];
                  var axisSize := dimsIn[axis];
                  
                  // For SoftMax, we iterate over all other dimensions
                  var totalOtherElements : NativeInt := 1;
                  for var k := 0 to High(dimsIn) do if k <> axis then totalOtherElements := totalOtherElements * dimsIn[k];
                  
                  for var i := 0 to totalOtherElements - 1 do begin
                     var baseIndices : array of Integer;
                     SetLength(baseIndices, Length(dimsIn));
                     
                     // Construct indices for all dimensions except axis
                     var tempIdx := i;
                     for var k := High(dimsIn) downto 0 do if k <> axis then begin
                        baseIndices[k] := tempIdx mod dimsIn[k];
                        tempIdx := tempIdx div dimsIn[k];
                     end else baseIndices[k] := 0;
                     
                     // 1. Find max for stability
                     var maxVal : Double := -1e30;
                     for var a := 0 to axisSize - 1 do begin
                        baseIndices[axis] := a;
                        var val := nodeBuffers[in1Idx][FlatIndex(baseIndices, dimsIn)];
                        if val > maxVal then maxVal := val;
                     end;
                     
                     // 2. Compute sum of exp
                     var sumExp : Double := 0.0;
                     for var a := 0 to axisSize - 1 do begin
                        baseIndices[axis] := a;
                        sumExp := sumExp + Exp(nodeBuffers[in1Idx][FlatIndex(baseIndices, dimsIn)] - maxVal);
                     end;
                     
                     // 3. Compute SoftMax
                     for var a := 0 to axisSize - 1 do begin
                        baseIndices[axis] := a;
                        nodeBuffers[n][FlatIndex(baseIndices, dimsIn)] := Exp(nodeBuffers[in1Idx][FlatIndex(baseIndices, dimsIn)] - maxVal) / sumExp;
                     end;
                  end;
               end else if node is TKCLGlobalAvgPoolNode then begin
                  var in1Idx := nodeToBufferIdx[node.Inputs[0]];
                  var dimsIn := nodeDims[in1Idx];
                  
                  if Length(dimsIn) >= 3 then begin
                     var dimH := High(dimsIn) - 2;
                     var dimW := High(dimsIn) - 1;
                     var inH := dimsIn[dimH];
                     var inW := dimsIn[dimW];
                     var count := inH * inW;
                     
                     for var i := 0 to nodeTotalElements[n] - 1 do begin
                        IndexFromFlat(i, dimsOut, indices);
                        var sum : Double := 0.0;
                        var inIdxs := Copy(indices);
                        for var y := 0 to inH - 1 do begin
                           for var x := 0 to inW - 1 do begin
                              inIdxs[dimH] := y;
                              inIdxs[dimW] := x;
                              sum := sum + nodeBuffers[in1Idx][FlatIndex(inIdxs, dimsIn)];
                           end;
                        end;
                        nodeBuffers[n][i] := sum / count;
                     end;
                  end else begin
                     // Fallback for rank < 3: average everything
                     var inElems := nodeTotalElements[in1Idx];
                     var sum : Double := 0.0;
                     for var i := 0 to inElems - 1 do
                        sum := sum + nodeBuffers[in1Idx][i];
                     var avg : Double := 0.0;
                     if inElems > 0 then
                        avg := sum / inElems;
                     for var i := 0 to nodeTotalElements[n] - 1 do
                        nodeBuffers[n][i] := avg;
                  end;
               end;
            end;
            
            // 4. Write outputs
            for var j := 0 to High(AKernel.Outputs) do begin
               var outNode := AKernel.Outputs[j];
               var outIdx := nodeToBufferIdx[outNode];
               var dimsOut := nodeDims[outIdx];
               SetLength(indices, Length(dimsOut));
               for var i := 0 to nodeTotalElements[outIdx] - 1 do begin
                  IndexFromFlat(i, dimsOut, indices);
                  SetValue(ABuffers[inputCount + j], indices, nodeBuffers[outIdx][i]);
               end;
            end;
         finally
            nodeToBufferIdx.Free;
         end;
      finally
         sortedNodes.Free;
         visiting.Free;
         visited.Free;
      end;
   finally
      SetExceptionMask(savedMask);
   end;
end;

end.