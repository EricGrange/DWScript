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
      dtInt8 : PInt8(p)^ := Round(AValue);
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
   dims : TKCLDimensions;
   totalElements : NativeInt;
   nodeBuffers : array of TArray<Double>;
   nodeToBufferIdx : TDictionary<TKCLNode, Integer>;
   savedMask : TArithmeticExceptionMask;
   indices : TArray<Integer>;

   function FlatIndex(const AIdx : array of Integer) : Integer;
   begin
      Result := 0;
      var mult := 1;
      for var i := High(AIdx) downto 0 do begin
         Result := Result + AIdx[i] * mult;
         mult := mult * dims[i];
      end;
   end;

   procedure IndexFromFlat(AFlat : Integer; var AIdx : array of Integer);
   begin
      for var i := High(AIdx) downto 0 do begin
         if dims[i] > 0 then begin
            AIdx[i] := AFlat mod dims[i];
            AFlat := AFlat div dims[i];
         end else
            AIdx[i] := 0;
      end;
   end;

begin
   if Length(ABuffers) = 0 then Exit;
   
   var inputCount := Length(AKernel.Inputs);
   var outputCount := Length(AKernel.Outputs);

   if Length(ABuffers) < inputCount + outputCount then
      raise EdwsKCLException.CreateFmt('TKCLReferenceBackend.Execute: Expected at least %d buffers, but got %d', [inputCount + outputCount, Length(ABuffers)]);

   dims := ABuffers[0].Dimensions;
   totalElements := 1;
   for var i := 0 to High(dims) do
      totalElements := totalElements * dims[i];
      
   // Domain validation
   for var b := 1 to High(ABuffers) do begin
      if Length(ABuffers[b].Dimensions) <> Length(dims) then
         raise EdwsKCLException.Create('KCL: Spatial domain mismatch (dimensions length).');
      for var i := 0 to High(dims) do
         if ABuffers[b].Dimensions[i] <> dims[i] then
            raise EdwsKCLException.Create('KCL: Spatial domain mismatch (dimension size).');
   end;

   SetLength(indices, Length(dims));

   // Mask FPU Exceptions
   savedMask := SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
   try
      SetLength(nodeBuffers, Length(AKernel.Nodes));
      for var i := 0 to High(AKernel.Nodes) do begin
         SetLength(nodeBuffers[i], totalElements);
         for var j := 0 to totalElements - 1 do
            nodeBuffers[i][j] := 0.0;
      end;
         
      nodeToBufferIdx := TDictionary<TKCLNode, Integer>.Create;
      try
         // Zero-initialize output buffers
         for var j := 0 to outputCount - 1 do begin
            for var i := 0 to totalElements - 1 do begin
               IndexFromFlat(i, indices);
               SetValue(ABuffers[inputCount + j], indices, 0.0);
            end;
         end;

         // Topological evaluation
         for var n := 0 to High(AKernel.Nodes) do begin
            var node := AKernel.Nodes[n];
            nodeToBufferIdx.Add(node, n);
            
            if node is TKCLInputNode then begin
               var inIdx := TKCLInputNode(node).InputIndex;
               for var i := 0 to totalElements - 1 do begin
                  IndexFromFlat(i, indices);
                  nodeBuffers[n][i] := GetValue(ABuffers[inIdx], indices);
               end;
            end else if node is TKCLMapNode then begin
               var in1Idx := nodeToBufferIdx[node.Inputs[0]];
               var in2Idx := -1;
               if Length(node.Inputs) > 1 then in2Idx := nodeToBufferIdx[node.Inputs[1]];
               
               for var i := 0 to totalElements - 1 do begin
                  var val1 := nodeBuffers[in1Idx][i];
                  var val2 : Double := 0;
                  if in2Idx >= 0 then val2 := nodeBuffers[in2Idx][i];
                  nodeBuffers[n][i] := TKCLMapNode(node).Eval([val1, val2]);
               end;
            end else if node is TKCLConv2DNode then begin
               var in1Idx := nodeToBufferIdx[node.Inputs[0]];
               for var i := 0 to totalElements - 1 do begin
                  IndexFromFlat(i, indices);
                  
                  var sum : Double := 0.0;
                  if Length(dims) >= 2 then begin
                     var dimH := High(dims) - 1;
                     if Length(dims) >= 3 then dimH := High(dims) - 2;
                     var dimW := dimH + 1;
                     
                     var h := indices[dimH];
                     var w := indices[dimW];
                     
                     for var dy := -1 to 1 do begin
                        for var dx := -1 to 1 do begin
                           var ny := h + dy;
                           var nx := w + dx;
                           if (ny >= 0) and (ny < dims[dimH]) and (nx >= 0) and (nx < dims[dimW]) then begin
                              var nIndices := Copy(indices);
                              nIndices[dimH] := ny;
                              nIndices[dimW] := nx;
                              sum := sum + nodeBuffers[in1Idx][FlatIndex(nIndices)];
                           end;
                        end;
                     end;
                  end else begin
                     sum := nodeBuffers[in1Idx][i];
                  end;
                  nodeBuffers[n][i] := sum;
               end;
            end else if node is TKCLGlobalAvgPoolNode then begin
               var in1Idx := nodeToBufferIdx[node.Inputs[0]];
               var sum : Double := 0.0;
               for var i := 0 to totalElements - 1 do
                  sum := sum + nodeBuffers[in1Idx][i];
               var avg : Double := 0.0;
               if totalElements > 0 then
                  avg := sum / totalElements;
               for var i := 0 to totalElements - 1 do
                  nodeBuffers[n][i] := avg;
            end;
         end;
         
         // Write outputs
         for var j := 0 to High(AKernel.Outputs) do begin
            var outNode := AKernel.Outputs[j];
            var outIdx := nodeToBufferIdx[outNode];
            for var i := 0 to totalElements - 1 do begin
               IndexFromFlat(i, indices);
               SetValue(ABuffers[inputCount + j], indices, nodeBuffers[outIdx][i]);
            end;
         end;
      finally
         nodeToBufferIdx.Free;
      end;
   finally
      SetExceptionMask(savedMask);
   end;
end;

end.
