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
unit dwsKernelCompiler;

{$I dws.inc}
{$POINTERMATH ON}

interface

uses
   System.Classes, System.SysUtils, System.Variants, System.Math,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsUnitSymbols, dwsComp,
   dwsMagicExprs, dwsExprList, dwsByteBuffer,
   dwsKernelCompilerCommon, dwsKernelCompilerBackend.Reference
{$IFDEF WIN64_ASM}
   , dwsKernelCompilerBackend.SSE2
{$ENDIF}
   ;

const
   SYS_KCL_DATATYPE = 'TKCLDataType';
   SYS_KCL_STRIDEDBUFFER = 'TKCLStridedBuffer';
   SYS_KCL_KERNEL = 'TKCLKernel';
   SYS_KCL_NODE = 'TKCLNode';
   SYS_KCL_KERNELCOMPILER = 'TKCLKernelCompiler';
   SYS_KCL_REFERENCECOMPILER = 'TKCLReferenceCompiler';
   SYS_KCL_JITCOMPILER = 'TKCLJITCompiler';

type
   TKCLStridedBufferWrapper = class
   private
      FDescriptor : TKCLStridedBufferDescriptor;
      FOriginalPointer : Pointer;
   public
      constructor Create(ADataType : TKCLDataType; const ADims : TKCLDimensions);
      destructor Destroy; override;
      property Descriptor : TKCLStridedBufferDescriptor read FDescriptor;
   end;

   TKCLKernelWrapper = class
   private
      FKernel : TKCLKernel;
   public
      constructor Create;
      destructor Destroy; override;
      property Kernel : TKCLKernel read FKernel;
   end;

   TKCLNodeWrapper = class
   private
      FNode : TKCLNode;
   public
      constructor Create(ANode : TKCLNode);
      destructor Destroy; override;
      property Node : TKCLNode read FNode;
   end;

   // Constructors
   TStridedBufferCreateMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelCreateMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   // Methods
   TStridedBufferSetDataMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TStridedBufferSetBulkDataMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TStridedBufferGetDataMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TStridedBufferGetBulkDataMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TStridedBufferSetArrayDataMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TStridedBufferGetArrayDataMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TStridedBufferPermuteMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddInputMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddAddMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddMulMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddSubMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddDequantizeMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddDivMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddSigmoidMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddHardSigmoidMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddExpMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddLogMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddPowerMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddConstantMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddReLUMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddReLU6Method = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddHardSwishMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddConv2DMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddDepthwiseConv2DMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddResizeBilinearMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddMaxPool2DMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddConcatMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddGlobalAvgPoolMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddSoftMaxMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddConv2DTransposeMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelMarkOutputMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelCompilerDispatchMethod = class(TInternalMagicProcedure)
   protected
      procedure ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor); virtual;
   public
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TReferenceCompilerDispatchMethod = class(TKernelCompilerDispatchMethod)
   protected
      procedure ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor); override;
   end;

   {$IFDEF WIN64_ASM}
   TJITCompilerDispatchMethod = class(TKernelCompilerDispatchMethod)
   protected
      procedure ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor); override;
   end;
   {$ENDIF}

procedure RegisterKernelCompilerSymbols(systemTable : TSystemSymbolTable; unitTable : TSymbolTable;
                                       cleanupEvent : TObjectDestroyEvent);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation

uses dwsDynamicArrays;

type
   PInt8 = ^ShortInt;

// ------------------
// ------------------ TKCLStridedBufferWrapper ------------------
// ------------------

// Create
//
constructor TKCLStridedBufferWrapper.Create(ADataType : TKCLDataType; const ADims : TKCLDimensions);
begin
   FDescriptor.DataType := ADataType;
   FDescriptor.Dimensions := ADims;
   SetLength(FDescriptor.Strides, Length(ADims));
   var size : NativeInt := 1;
   case ADataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;
   for var i := High(ADims) downto 0 do begin
      FDescriptor.Strides[i] := size;
      size := size * ADims[i];
   end;
   FDescriptor.Capacity := size + 16 + 15; 
   FOriginalPointer := GetMemory(FDescriptor.Capacity);
   FillChar(FOriginalPointer^, FDescriptor.Capacity, 0);
   FDescriptor.BasePointer := Pointer((NativeUInt(FOriginalPointer) + 15) and not NativeUInt(15));
end;

// Destroy
//
destructor TKCLStridedBufferWrapper.Destroy;
begin
   FreeMemory(FOriginalPointer);
   inherited;
end;

// ------------------
// ------------------ TStridedBufferCreateMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferCreateMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var dataType := TKCLDataType(info.ParamAsInteger[0]);
   var argDims := info.ParamAsScriptDynArray[1];
   var len := argDims.ArrayLength;
   var dims : TKCLDimensions;
   SetLength(dims, len);
   for var i := 0 to len - 1 do dims[i] := argDims.AsInteger[i];
   externalObject := TKCLStridedBufferWrapper.Create(dataType, dims);
end;

// ------------------
// ------------------ TStridedBufferSetDataMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferSetDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var argIndices := info.ParamAsScriptDynArray[0];
   var val := info.ParamAsFloat[1];
   var len := argIndices.ArrayLength;
   if len <> Length(wrapper.FDescriptor.Dimensions) then
      EdwsKCLException.RaiseIndicesCountMismatch(Length(wrapper.FDescriptor.Dimensions), len);

   var offset : NativeInt := 0;
   for var i := 0 to len - 1 do begin
      var idx := argIndices.AsInteger[i];
      if (idx < 0) or (idx >= wrapper.FDescriptor.Dimensions[i]) then
         raise EdwsKCLException.Create('KCL: Buffer overflow during write (index).');
      offset := offset + idx * wrapper.FDescriptor.Strides[i];
   end;

   var size : NativeInt := 1;
   case wrapper.FDescriptor.DataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;

   if (offset < 0) or (offset + size > wrapper.FDescriptor.Capacity) then
      raise EdwsKCLException.Create('KCL: Buffer overflow during write (capacity).');

   var p := PByte(wrapper.FDescriptor.BasePointer) + offset;
   case wrapper.FDescriptor.DataType of
      dtInt8 : begin
         var intVal := Round(val);
         if intVal < -128 then intVal := -128 else if intVal > 127 then intVal := 127;
         PInt8(p)^ := intVal;
      end;
      dtFloat16 : PHalfFloat(p)^ := FloatToHalf(val);
      dtFloat32 : PSingle(p)^ := val;
   end;
end;

// ------------------
// ------------------ TStridedBufferSetBulkDataMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferSetBulkDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var intf : IUnknown := info.ParamAsVariant[0];
   var buffer : IdwsByteBuffer;
   if (intf = nil) or (intf.QueryInterface(IdwsByteBuffer, buffer) <> 0) then
      raise EdwsKCLException.Create('KCL: Argument is not a ByteBuffer.');
   
   var bb := TdwsByteBuffer(buffer);
   var srcOffset := info.ParamAsInteger[1];
   var elementCount := info.ParamAsInteger[2];
   var dataType := TKCLDataType(info.ParamAsInteger[3]);
   
   var totalBytes : NativeInt := 0;
   case dataType of
      dtInt8 : totalBytes := elementCount;
      dtFloat16 : totalBytes := elementCount * 2;
      dtFloat32 : totalBytes := elementCount * 4;
   end;
   
   if (srcOffset < 0) or (srcOffset + totalBytes > bb.Count) then
      raise EdwsKCLException.Create('KCL: ByteBuffer overflow during bulk SetData.');

   var totalElements : NativeInt := 1;
   for var i := 0 to High(wrapper.FDescriptor.Dimensions) do
      totalElements := totalElements * wrapper.FDescriptor.Dimensions[i];
   
   if elementCount > totalElements then
      raise EdwsKCLException.Create('KCL: elementCount exceeds buffer capacity.');

   var pSrc := PByte(bb.DataPtr) + srcOffset;
   var pDest := wrapper.FDescriptor.BasePointer;

   if dataType = wrapper.FDescriptor.DataType then begin
      Move(pSrc^, pDest^, totalBytes);
   end else begin
      for var i := 0 to elementCount - 1 do begin
         var val : Double := 0;
         case dataType of
            dtInt8 : val := PInt8(PByte(pSrc) + i)^;
            dtFloat16 : val := HalfToFloat(PHalfFloat(PByte(pSrc) + i * 2)^);
            dtFloat32 : val := PSingle(PByte(pSrc) + i * 4)^;
         end;
         case wrapper.FDescriptor.DataType of
            dtInt8 : begin
               var intVal := Round(val);
               if intVal < -128 then intVal := -128 else if intVal > 127 then intVal := 127;
               PInt8(PByte(pDest) + i)^ := intVal;
            end;
            dtFloat16 : PHalfFloat(PByte(pDest) + i * 2)^ := FloatToHalf(val);
            dtFloat32 : PSingle(PByte(pDest) + i * 4)^ := val;
         end;
      end;
   end;
end;

// ------------------
// ------------------ TStridedBufferGetDataMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferGetDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var argIndices := info.ParamAsScriptDynArray[0];
   var len := argIndices.ArrayLength;
   if len <> Length(wrapper.FDescriptor.Dimensions) then
      EdwsKCLException.RaiseIndicesCountMismatch(Length(wrapper.FDescriptor.Dimensions), len);

   var offset : NativeInt := 0;
   for var i := 0 to len - 1 do begin
      var idx := argIndices.AsInteger[i];
      if (idx < 0) or (idx >= wrapper.FDescriptor.Dimensions[i]) then
         raise EdwsKCLException.Create('KCL: Buffer overflow during read (index).');
      offset := offset + idx * wrapper.FDescriptor.Strides[i];
   end;

   var size : NativeInt := 1;
   case wrapper.FDescriptor.DataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;

   if (offset < 0) or (offset + size > wrapper.FDescriptor.Capacity) then
      raise EdwsKCLException.Create('KCL: Buffer overflow during read (capacity).');

   var p := PByte(wrapper.FDescriptor.BasePointer) + offset;
   case wrapper.FDescriptor.DataType of
      dtInt8 : info.ResultAsFloat := PInt8(p)^;
      dtFloat16 : info.ResultAsFloat := HalfToFloat(PHalfFloat(p)^);
      dtFloat32 : info.ResultAsFloat := PSingle(p)^;
   else info.ResultAsFloat := 0; end;
end;

// ------------------
// ------------------ TStridedBufferGetBulkDataMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferGetBulkDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var intf : IUnknown := info.ParamAsVariant[0];
   var buffer : IdwsByteBuffer;
   if (intf = nil) or (intf.QueryInterface(IdwsByteBuffer, buffer) <> 0) then
      raise EdwsKCLException.Create('KCL: Argument is not a ByteBuffer.');
   
   var destOffset := info.ParamAsInteger[1];
   var elementCount := info.ParamAsInteger[2];
   var dataType := TKCLDataType(info.ParamAsInteger[3]);
   
   var totalBytes : NativeInt := 0;
   case dataType of
      dtInt8 : totalBytes := elementCount;
      dtFloat16 : totalBytes := elementCount * 2;
      dtFloat32 : totalBytes := elementCount * 4;
   end;
   
   var bb := TdwsByteBuffer(buffer);
   if (destOffset < 0) or (destOffset + totalBytes > bb.Count) then
      raise EdwsKCLException.Create('KCL: ByteBuffer overflow during bulk GetData.');

   var totalElements : NativeInt := 1;
   for var i := 0 to High(wrapper.FDescriptor.Dimensions) do
      totalElements := totalElements * wrapper.FDescriptor.Dimensions[i];
   
   if elementCount > totalElements then
      raise EdwsKCLException.Create('KCL: elementCount exceeds buffer capacity.');

   var pDest := PByte(bb.DataPtr) + destOffset;
   var pSrc := wrapper.FDescriptor.BasePointer;

   if dataType = wrapper.FDescriptor.DataType then begin
      Move(pSrc^, pDest^, totalBytes);
   end else begin
      for var i := 0 to elementCount - 1 do begin
         var val : Double := 0;
         case wrapper.FDescriptor.DataType of
            dtInt8 : val := PInt8(PByte(pSrc) + i)^;
            dtFloat16 : val := HalfToFloat(PHalfFloat(PByte(pSrc) + i * 2)^);
            dtFloat32 : val := PSingle(PByte(pSrc) + i * 4)^;
         end;
         case dataType of
            dtInt8 : begin
               var intVal := Round(val);
               if intVal < -128 then intVal := -128 else if intVal > 127 then intVal := 127;
               PInt8(PByte(pDest) + i)^ := intVal;
            end;
            dtFloat16 : PHalfFloat(PByte(pDest) + i * 2)^ := FloatToHalf(val);
            dtFloat32 : PSingle(PByte(pDest) + i * 4)^ := val;
         end;
      end;
   end;
end;

// IsContiguous
//
function IsContiguous(const ABuf : TKCLStridedBufferDescriptor) : Boolean;
var idx : Integer; sz : NativeInt;
begin
   sz := 1;
   case ABuf.DataType of dtInt8 : sz := 1; dtFloat16 : sz := 2; dtFloat32 : sz := 4; end;
   for idx := High(ABuf.Dimensions) downto 0 do begin if ABuf.Strides[idx] <> sz then Exit(False); sz := sz * ABuf.Dimensions[idx]; end;
   Result := True;
end;

// ------------------
// ------------------ TStridedBufferSetArrayDataMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferSetArrayDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var argSource := info.ParamAsScriptDynArray[0];
   var len := argSource.ArrayLength;
   var totalElems : NativeInt := 1; for var i := 0 to High(wrapper.FDescriptor.Dimensions) do totalElems := totalElems * wrapper.FDescriptor.Dimensions[i];
   if len <> totalElems then raise EdwsKCLException.Create('KCL: Buffer size mismatch for array SetData.');
   if wrapper.FDescriptor.DataType <> dtFloat32 then raise EdwsKCLException.Create('KCL: Bulk Pascal array I/O currently supports Float32 only.');
   if not IsContiguous(wrapper.FDescriptor) then raise EdwsKCLException.Create('KCL: Buffer must be contiguous for bulk array I/O.');
   var destPtr := PSingle(wrapper.FDescriptor.BasePointer);
   var selfObj := argSource.GetSelf;
   if selfObj is TScriptDynamicNativeFloatArray then begin
      var nbElems, stride : NativeInt;
      var srcPtr := TScriptDynamicNativeFloatArray(selfObj).AsPDouble(nbElems, stride);
      if stride = SizeOf(Double) then begin
{$IFDEF WIN64_ASM}
         SSE2_CvtPD2PSContiguous(srcPtr, destPtr, len);
{$ELSE}
         for var i := 0 to len - 1 do destPtr[i] := srcPtr[i];
{$ENDIF}
         Exit;
      end;
   end;
   for var i := 0 to len - 1 do destPtr[i] := argSource.AsFloat[i];
end;

// ------------------
// ------------------ TStridedBufferGetArrayDataMethod ------------------
// ------------------

// Execute
//
procedure TStridedBufferGetArrayDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var argDest := info.ParamAsScriptDynArray[0];
   var totalElems : NativeInt := 1; for var i := 0 to High(wrapper.FDescriptor.Dimensions) do totalElems := totalElems * wrapper.FDescriptor.Dimensions[i];
   argDest.ArrayLength := totalElems;
   if wrapper.FDescriptor.DataType <> dtFloat32 then raise EdwsKCLException.Create('KCL: Bulk Pascal array I/O currently supports Float32 only.');
   if not IsContiguous(wrapper.FDescriptor) then raise EdwsKCLException.Create('KCL: Buffer must be contiguous for bulk array I/O.');
   var srcPtr := PSingle(wrapper.FDescriptor.BasePointer);
   var selfObj := argDest.GetSelf;
   if selfObj is TScriptDynamicNativeFloatArray then begin
      var nbElems, stride : NativeInt;
      var destPtr := TScriptDynamicNativeFloatArray(selfObj).AsPDouble(nbElems, stride);
      if stride = SizeOf(Double) then begin
{$IFDEF WIN64_ASM}
         SSE2_CvtPS2PDContiguous(srcPtr, destPtr, totalElems);
{$ELSE}
         for var i := 0 to totalElems - 1 do destPtr[i] := srcPtr[i];
{$ENDIF}
         Exit;
      end;
   end;
   for var i := 0 to totalElems - 1 do argDest.AsFloat[i] := srcPtr[i];
end;

procedure TStridedBufferPermuteMethod.Execute(info : TProgramInfo; var externalObject : TObject);
var
   wrapper : TKCLStridedBufferWrapper;
   orderArr : IScriptDynArray;
   rank, i : Integer;
   order : TArray<Integer>;
   oldDims : TKCLDimensions;
   oldStrides : TArray<NativeInt>;
   newDims : TKCLDimensions;
   newStrides : TArray<NativeInt>;
   seen : TArray<Boolean>;
begin
   wrapper := TKCLStridedBufferWrapper(externalObject);
   orderArr := info.ParamAsScriptDynArray[0];
   rank := Length(wrapper.FDescriptor.Dimensions);

   if orderArr.ArrayLength <> rank then
      raise EdwsKCLException.CreateFmt('Permute: order length %d does not match rank %d',
         [orderArr.ArrayLength, rank]);

   SetLength(order, rank);
   SetLength(seen, rank);
   for i := 0 to rank - 1 do begin
      order[i] := orderArr.AsInteger[i];
      if (order[i] < 0) or (order[i] >= rank) then
         raise EdwsKCLException.CreateFmt('Permute: index %d out of range [0, %d)', [order[i], rank]);
      if seen[order[i]] then
         raise EdwsKCLException.CreateFmt('Permute: duplicate index %d', [order[i]]);
      seen[order[i]] := True;
   end;

   SetLength(oldDims, rank);
   SetLength(oldStrides, rank);
   for i := 0 to rank - 1 do begin
      oldDims[i] := wrapper.FDescriptor.Dimensions[i];
      oldStrides[i] := wrapper.FDescriptor.Strides[i];
   end;

   SetLength(newDims, rank);
   SetLength(newStrides, rank);
   for i := 0 to rank - 1 do begin
      newDims[i] := oldDims[order[i]];
      newStrides[i] := oldStrides[order[i]];
   end;

   wrapper.FDescriptor.Dimensions := newDims;
   wrapper.FDescriptor.Strides := newStrides;
end;

// ------------------
// ------------------ TKCLKernelWrapper ------------------
// ------------------

constructor TKCLKernelWrapper.Create; begin FKernel := TKCLKernel.Create; end;
destructor TKCLKernelWrapper.Destroy; begin FKernel.Free; inherited; end;

// ------------------
// ------------------ TKCLNodeWrapper ------------------
// ------------------

constructor TKCLNodeWrapper.Create(ANode : TKCLNode); begin FNode := ANode; end;
destructor TKCLNodeWrapper.Destroy; begin inherited; end;

// ------------------
// ------------------ TKernelCreateMethod ------------------
// ------------------

// Execute
//
procedure TKernelCreateMethod.Execute(info : TProgramInfo; var externalObject : TObject); begin externalObject := TKCLKernelWrapper.Create; end;

// ------------------
// ------------------ TKernelAddInputMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddInputMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var node := wrapper.FKernel.AddInput(info.ParamAsString[0]);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddAddMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddAddMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLAddNode.Create('add', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddMulMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddMulMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLMulNode.Create('mul', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddSubMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddSubMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLSubNode.Create('sub', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddDequantizeMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddDequantizeMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLDequantizeNode.Create([in1.FNode], info.ParamAsFloat[1], info.ParamAsFloat[2]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddDivMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddDivMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLDivNode.Create('div', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddSigmoidMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddSigmoidMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLSigmoidNode.Create('sigmoid', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddHardSigmoidMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddHardSigmoidMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLHardSigmoidNode.Create('hardsigmoid', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddExpMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddExpMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLExpNode.Create('exp', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddLogMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddLogMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLLogNode.Create('log', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddPowerMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddPowerMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLPowerNode.Create('power', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddConstantMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddConstantMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var argDims := info.ParamAsScriptDynArray[1];
   var dims : TKCLDimensions; SetLength(dims, argDims.ArrayLength);
   for var i := 0 to High(dims) do dims[i] := argDims.AsInteger[i];
   var node := TKCLConstantNode.Create('constant', info.ParamAsFloat[0], dims);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddReLUMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddReLUMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLReLUNode.Create('relu', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddReLU6Method ------------------
// ------------------

// Execute
//
procedure TKernelAddReLU6Method.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLReLU6Node.Create('relu6', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddHardSwishMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddHardSwishMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLHardSwishNode.Create('hardswish', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ExtractDoubleArray
//
procedure ExtractDoubleArray(arg : IScriptDynArray; var dest : TDoubleDynArray);
begin
   var len := arg.ArrayLength;
   SetLength(dest, len);
   if len = 0 then Exit;
   var selfObj := arg.GetSelf;
   if selfObj is TScriptDynamicNativeFloatArray then begin
      var nbElems, stride : NativeInt;
      var srcPtr := TScriptDynamicNativeFloatArray(selfObj).AsPDouble(nbElems, stride);
      if stride = SizeOf(Double) then begin
         //PrintLn('Using Move path for ' + IntToStr(len) + ' elements');
         Move(srcPtr^, dest[0], len * SizeOf(Double));
         Exit;
      end;
   end;
   for var i := 0 to len - 1 do dest[i] := arg.AsFloat[i];
end;

// ------------------
// ------------------ TKernelAddConv2DMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddConv2DMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var weights : TDoubleDynArray; ExtractDoubleArray(info.ParamAsScriptDynArray[1], weights);
   var bias : TDoubleDynArray; ExtractDoubleArray(info.ParamAsScriptDynArray[2], bias);
   var node := TKCLConv2DNode.Create('conv2d', [in1.FNode], weights, bias, info.ParamAsInteger[3], info.ParamAsInteger[4]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddDepthwiseConv2DMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddDepthwiseConv2DMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var weights : TDoubleDynArray; ExtractDoubleArray(info.ParamAsScriptDynArray[1], weights);
   var bias : TDoubleDynArray; ExtractDoubleArray(info.ParamAsScriptDynArray[2], bias);
   var node := TKCLDepthwiseConv2DNode.Create('dwconv2d', [in1.FNode], weights, bias, info.ParamAsInteger[3], info.ParamAsInteger[4]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddResizeBilinearMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddResizeBilinearMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLResizeBilinearNode.Create('resize_bilinear', [in1.FNode], info.ParamAsInteger[1], info.ParamAsInteger[2], info.ParamAsBoolean[3], info.ParamAsBoolean[4]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddMaxPool2DMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddMaxPool2DMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLMaxPool2DNode.Create('maxpool2d', [in1.FNode], info.ParamAsInteger[1], info.ParamAsInteger[2]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddConcatMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddConcatMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var argNodes := info.ParamAsScriptDynArray[0];
   var inputs : TKCLNodes; SetLength(inputs, argNodes.ArrayLength);
   for var i := 0 to High(inputs) do begin
      var objUnk : IUnknown; argNodes.EvalAsInterface(i, objUnk);
      var objObj : IScriptObj; objUnk.QueryInterface(IScriptObj, objObj);
      inputs[i] := TKCLNodeWrapper(objObj.ExternalObject).FNode;
   end;
   var node := TKCLConcatNode.Create('concat', inputs, info.ParamAsInteger[1]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddGlobalAvgPoolMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddGlobalAvgPoolMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLGlobalAvgPoolNode.Create('gap', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddSoftMaxMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddSoftMaxMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLSoftMaxNode.Create('softmax', [in1.FNode], info.ParamAsInteger[1]);
   wrapper.FKernel.AddNode(node);
   var classSym := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddConv2DTransposeMethod ------------------
// ------------------

// Execute
//
procedure TKernelAddConv2DTransposeMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var weights : TDoubleDynArray; ExtractDoubleArray(info.ParamAsScriptDynArray[1], weights);
   var bias : TDoubleDynArray; ExtractDoubleArray(info.ParamAsScriptDynArray[2], bias);
   var node := TKCLConv2DTransposeNode.Create('conv2d_transpose', [in1.FNode], weights, bias, info.ParamAsInteger[3], info.ParamAsInteger[4]);
   wrapper.FKernel.AddNode(node);
   var scriptObj := info.FindSymbolInUnits(SYS_KCL_NODE) as TClassSymbol;
   var scriptObjInst := TScriptObjInstance.Create(scriptObj, info.Execution as TdwsProgramExecution);
   scriptObjInst.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObjInst as IUnknown;
end;

// ------------------
// ------------------ TKernelMarkOutputMethod ------------------
// ------------------

// Execute
//
procedure TKernelMarkOutputMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var node := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   wrapper.FKernel.MarkOutput(node.FNode);
end;

// ------------------
// ------------------ TKernelCompilerDispatchMethod ------------------
// ------------------

// ExecuteDispatch
//
procedure TKernelCompilerDispatchMethod.ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
begin
{$IFDEF WIN64_ASM}
   var backend := TKCLJITBackend.Create;
{$ELSE}
   var backend := TKCLReferenceBackend.Create;
{$ENDIF}
   try backend.Execute(AKernel, ABuffers); finally backend.Free; end;
end;

// DoEvalProc
//
procedure TKernelCompilerDispatchMethod.DoEvalProc(const args : TExprBaseListExec);
begin
   var kernelObj : IScriptObj; args.ExprBase[0].EvalAsScriptObj(args.Exec, kernelObj);
   var kernel := TKCLKernelWrapper(kernelObj.ExternalObject);
   var argBuffers : IScriptDynArray; args.EvalAsDynArray(1, argBuffers);
   var count := argBuffers.ArrayLength;
   var buffers : array of TKCLStridedBufferDescriptor; SetLength(buffers, count);
   for var i := 0 to count - 1 do begin
      var bufUnk : IUnknown; argBuffers.EvalAsInterface(i, bufUnk);
      var bufObj : IScriptObj; bufUnk.QueryInterface(IScriptObj, bufObj);
      buffers[i] := TKCLStridedBufferWrapper(bufObj.ExternalObject).Descriptor;
   end;
   ExecuteDispatch(kernel.FKernel, buffers);
end;

// ------------------
// ------------------ TReferenceCompilerDispatchMethod ------------------
// ------------------

// ExecuteDispatch
//
procedure TReferenceCompilerDispatchMethod.ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
begin
   var backend := TKCLReferenceBackend.Create;
   try backend.Execute(AKernel, ABuffers); finally backend.Free; end;
end;

{$IFDEF WIN64_ASM}

// ------------------
// ------------------ TJITCompilerDispatchMethod ------------------
// ------------------

// ExecuteDispatch
//
procedure TJITCompilerDispatchMethod.ExecuteDispatch(AKernel : TKCLKernel; const ABuffers : array of TKCLStridedBufferDescriptor);
begin
   var backend := TKCLJITBackend.Create;
   try backend.Execute(AKernel, ABuffers); finally backend.Free; end;
end;
{$ENDIF}

// RegisterKernelCompilerSymbols
//
procedure RegisterKernelCompilerSymbols(systemTable : TSystemSymbolTable; unitTable : TSymbolTable; cleanupEvent : TObjectDestroyEvent);
begin
   var typDataType := TEnumerationSymbol.Create(SYS_KCL_DATATYPE, systemTable.TypInteger, enumScoped);
   unitTable.AddSymbol(typDataType);
   typDataType.AddElement(TElementSymbol.Create('Int8', typDataType, Ord(dtInt8), False));
   typDataType.AddElement(TElementSymbol.Create('Float16', typDataType, Ord(dtFloat16), False));
   typDataType.AddElement(TElementSymbol.Create('Float32', typDataType, Ord(dtFloat32), False));

   var clsStridedBuffer := TClassSymbol.Create(SYS_KCL_STRIDEDBUFFER, systemTable.TypTObject);
   clsStridedBuffer.OnObjectDestroy := cleanupEvent;
   unitTable.AddSymbol(clsStridedBuffer);
   var clsNode := TClassSymbol.Create(SYS_KCL_NODE, systemTable.TypTObject);
   clsNode.OnObjectDestroy := cleanupEvent;
   unitTable.AddSymbol(clsNode);
   var clsKernel := TClassSymbol.Create(SYS_KCL_KERNEL, systemTable.TypTObject);
   clsKernel.OnObjectDestroy := cleanupEvent;
   unitTable.AddSymbol(clsKernel);
   var clsKernelCompiler := TClassSymbol.Create(SYS_KCL_KERNELCOMPILER, systemTable.TypTObject);
   unitTable.AddSymbol(clsKernelCompiler);

   var clsReferenceCompiler := TClassSymbol.Create(SYS_KCL_REFERENCECOMPILER, clsKernelCompiler);
   unitTable.AddSymbol(clsReferenceCompiler);

   unitTable.AddSymbol(TDynamicArraySymbol.Create('array of ' + SYS_KCL_STRIDEDBUFFER, clsStridedBuffer, systemTable.TypInteger));
   unitTable.AddSymbol(TDynamicArraySymbol.Create('array of ' + SYS_KCL_NODE, clsNode, systemTable.TypInteger));

   TStridedBufferCreateMethod.Create(mkConstructor, [], 'Create', ['dataType', SYS_KCL_DATATYPE, 'dimensions', 'array of Integer'], '', clsStridedBuffer, cvPublic, unitTable);
   TStridedBufferSetDataMethod.Create(mkProcedure, [], 'SetData', ['indices', 'array of Integer', 'value', 'Float'], '', clsStridedBuffer, cvPublic, unitTable, True);
   TStridedBufferSetBulkDataMethod.Create(mkProcedure, [], 'SetData', ['source', 'ByteBuffer', 'srcOffset', 'Integer', 'elementCount', 'Integer', 'dataType', SYS_KCL_DATATYPE], '', clsStridedBuffer, cvPublic, unitTable, True);
   TStridedBufferSetArrayDataMethod.Create(mkProcedure, [], 'SetData', ['source', 'array of Float'], '', clsStridedBuffer, cvPublic, unitTable, True);
   TStridedBufferGetDataMethod.Create(mkFunction, [], 'GetData', ['indices', 'array of Integer'], 'Float', clsStridedBuffer, cvPublic, unitTable, True);
   TStridedBufferGetBulkDataMethod.Create(mkProcedure, [], 'GetData', ['dest', 'ByteBuffer', 'destOffset', 'Integer', 'elementCount', 'Integer', 'dataType', SYS_KCL_DATATYPE], '', clsStridedBuffer, cvPublic, unitTable, True);
   TStridedBufferGetArrayDataMethod.Create(mkProcedure, [], 'GetData', ['var dest', 'array of Float'], '', clsStridedBuffer, cvPublic, unitTable, True);
   TStridedBufferPermuteMethod.Create(mkProcedure, [], 'Permute', ['order', 'array of Integer'], '', clsStridedBuffer, cvPublic, unitTable, True);

   TKernelCreateMethod.Create(mkConstructor, [], 'Create', [], '', clsKernel, cvPublic, unitTable);
   TKernelAddInputMethod.Create(mkFunction, [], 'AddInput', ['name', 'String'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddAddMethod.Create(mkFunction, [], 'AddAdd', ['in1', SYS_KCL_NODE, 'in2', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddMulMethod.Create(mkFunction, [], 'AddMul', ['in1', SYS_KCL_NODE, 'in2', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddSubMethod.Create(mkFunction, [], 'AddSub', ['in1', SYS_KCL_NODE, 'in2', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddDequantizeMethod.Create(mkFunction, [], 'AddDequantize', ['input', SYS_KCL_NODE, 'scale', 'Float', 'zeroPoint', 'Float'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddDivMethod.Create(mkFunction, [], 'AddDiv', ['in1', SYS_KCL_NODE, 'in2', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddSigmoidMethod.Create(mkFunction, [], 'AddSigmoid', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddHardSigmoidMethod.Create(mkFunction, [], 'AddHardSigmoid', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddExpMethod.Create(mkFunction, [], 'AddExp', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddLogMethod.Create(mkFunction, [], 'AddLog', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddPowerMethod.Create(mkFunction, [], 'AddPower', ['base', SYS_KCL_NODE, 'exponent', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddConstantMethod.Create(mkFunction, [], 'AddConstant', ['value', 'Float', 'dimensions', 'array of Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddReLUMethod.Create(mkFunction, [], 'AddReLU', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddReLU6Method.Create(mkFunction, [], 'AddReLU6', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddHardSwishMethod.Create(mkFunction, [], 'AddHardSwish', ['in1', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddConv2DMethod.Create(mkFunction, [], 'AddConv2D', ['input', SYS_KCL_NODE, 'weights', 'array of Float', 'bias', 'array of Float', 'kernelSize', 'Integer', 'stride', 'Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddConv2DTransposeMethod.Create(mkFunction, [], 'AddConv2DTranspose', ['input', SYS_KCL_NODE, 'weights', 'array of Float', 'bias', 'array of Float', 'kernelSize', 'Integer', 'stride', 'Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddDepthwiseConv2DMethod.Create(mkFunction, [], 'AddDepthwiseConv2D', ['input', SYS_KCL_NODE, 'weights', 'array of Float', 'bias', 'array of Float', 'kernelSize', 'Integer', 'stride', 'Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddResizeBilinearMethod.Create(mkFunction, [], 'AddResizeBilinear', ['input', SYS_KCL_NODE, 'targetHeight', 'Integer', 'targetWidth', 'Integer', 'alignCorners=False', 'Boolean', 'halfPixelCenters=False', 'Boolean'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddMaxPool2DMethod.Create(mkFunction, [], 'AddMaxPool2D', ['input', SYS_KCL_NODE, 'kernelSize', 'Integer', 'stride', 'Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddConcatMethod.Create(mkFunction, [], 'AddConcat', ['inputs', 'array of ' + SYS_KCL_NODE, 'axis', 'Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddGlobalAvgPoolMethod.Create(mkFunction, [], 'AddGlobalAvgPool', ['input', SYS_KCL_NODE], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddSoftMaxMethod.Create(mkFunction, [], 'AddSoftMax', ['input', SYS_KCL_NODE, 'axis', 'Integer'], SYS_KCL_NODE, clsKernel, cvPublic, unitTable);
   TKernelMarkOutputMethod.Create(mkProcedure, [], 'MarkOutput', ['node', SYS_KCL_NODE], '', clsKernel, cvPublic, unitTable);

   TKernelCompilerDispatchMethod.Create(unitTable, 'Dispatch', ['kernel', SYS_KCL_KERNEL, 'buffers', 'array of ' + SYS_KCL_STRIDEDBUFFER], '', [iffStaticMethod], clsKernelCompiler);
   TReferenceCompilerDispatchMethod.Create(unitTable, 'Dispatch', ['kernel', SYS_KCL_KERNEL, 'buffers', 'array of ' + SYS_KCL_STRIDEDBUFFER], '', [iffStaticMethod], clsReferenceCompiler);

   {$IFDEF WIN64_ASM}
   var clsJIT := TClassSymbol.Create(SYS_KCL_JITCOMPILER, clsKernelCompiler);
   unitTable.AddSymbol(clsJIT);
   TJITCompilerDispatchMethod.Create(unitTable, 'Dispatch', ['kernel', SYS_KCL_KERNEL, 'buffers', 'array of ' + SYS_KCL_STRIDEDBUFFER], '', [iffStaticMethod], clsJIT);
   {$ENDIF}
end;

end.
