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

interface

uses
   System.Classes, System.SysUtils, System.Variants,
   dwsXPlatform, dwsUtils, dwsStrings,
   dwsFunctions, dwsSymbols, dwsExprs, dwsUnitSymbols, dwsComp,
   dwsMagicExprs, dwsExprList,
   dwsKernelCompilerCommon, dwsKernelCompilerBackend.Reference;

const
   SYS_DATATYPE = 'TDataType';
   SYS_STRIDEDBUFFER = 'TStridedBuffer';
   SYS_KERNEL = 'TKernel';
   SYS_NODE = 'TNode';
   SYS_KERNELCOMPILER = 'TKernelCompiler';

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

   TStridedBufferGetDataMethod = class(TInternalMethod)
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

   TKernelAddConv2DMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelAddGlobalAvgPoolMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelMarkOutputMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var externalObject : TObject); override;
   end;

   TKernelCompilerDispatchMethod = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

procedure RegisterKernelCompilerSymbols(systemTable : TSystemSymbolTable; unitTable : TSymbolTable;
                                       cleanupEvent : TObjectDestroyEvent);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
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
   // Row-major strides (inner-most is last)
   for var i := High(ADims) downto 0 do begin
      FDescriptor.Strides[i] := size;
      size := size * ADims[i];
   end;
   // Spec 3.1: 16-byte tail-padding. Also 16-byte alignment.
   FDescriptor.Capacity := size + 16 + 15; 
   FOriginalPointer := GetMemory(FDescriptor.Capacity);
   FillChar(FOriginalPointer^, FDescriptor.Capacity, 0);
   // Align to 16 bytes
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

procedure TStridedBufferCreateMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var dataType := TKCLDataType(info.ParamAsInteger[0]);
   var argDims : IScriptDynArray;
   argDims := info.ParamAsScriptDynArray[1];
   var len := argDims.ArrayLength;
   var dims : TKCLDimensions;
   SetLength(dims, len);
   for var i := 0 to len - 1 do
      dims[i] := argDims.AsInteger[i];
   externalObject := TKCLStridedBufferWrapper.Create(dataType, dims);
end;

// ------------------
// ------------------ TStridedBufferSetDataMethod ------------------
// ------------------

procedure TStridedBufferSetDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var argIndices : IScriptDynArray;
   argIndices := info.ParamAsScriptDynArray[0];
   var val := info.ParamAsFloat[1];
   
   var len := argIndices.ArrayLength;
   if len <> Length(wrapper.FDescriptor.Dimensions) then
      raise EdwsKCLException.Create('KCL: Indices count mismatch.');

   var offset : NativeInt := 0;
   for var i := 0 to len - 1 do begin
      var idx := argIndices.AsInteger[i];
      if (idx < 0) or (idx >= wrapper.FDescriptor.Dimensions[i]) then begin
         raise EdwsKCLException.Create('KCL: Buffer overflow during write (index).');
      end;
      offset := offset + idx * wrapper.FDescriptor.Strides[i];
   end;

   var size : NativeInt := 1;
   case wrapper.FDescriptor.DataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;

   if (offset < 0) or (offset + size > wrapper.FDescriptor.Capacity) then begin
      raise EdwsKCLException.Create('KCL: Buffer overflow during write (capacity).');
   end;

   var p := PByte(wrapper.FDescriptor.BasePointer) + offset;
   case wrapper.FDescriptor.DataType of
      dtInt8 : PInt8(p)^ := Round(val);
      dtFloat16 : PHalfFloat(p)^ := FloatToHalf(val);
      dtFloat32 : PSingle(p)^ := val;
   end;
end;

// ------------------
// ------------------ TStridedBufferGetDataMethod ------------------
// ------------------

procedure TStridedBufferGetDataMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLStridedBufferWrapper(externalObject);
   var argIndices : IScriptDynArray;
   argIndices := info.ParamAsScriptDynArray[0];
   
   var len := argIndices.ArrayLength;
   if len <> Length(wrapper.FDescriptor.Dimensions) then
      raise EdwsKCLException.Create('KCL: Indices count mismatch.');

   var offset : NativeInt := 0;
   for var i := 0 to len - 1 do begin
      var idx := argIndices.AsInteger[i];
      if (idx < 0) or (idx >= wrapper.FDescriptor.Dimensions[i]) then begin
         raise EdwsKCLException.Create('KCL: Buffer overflow during read (index).');
      end;
      offset := offset + idx * wrapper.FDescriptor.Strides[i];
   end;

   var size : NativeInt := 1;
   case wrapper.FDescriptor.DataType of
      dtInt8 : size := 1;
      dtFloat16 : size := 2;
      dtFloat32 : size := 4;
   end;

   if (offset < 0) or (offset + size > wrapper.FDescriptor.Capacity) then begin
      raise EdwsKCLException.Create('KCL: Buffer overflow during read (capacity).');
   end;

   var p := PByte(wrapper.FDescriptor.BasePointer) + offset;
   case wrapper.FDescriptor.DataType of
      dtInt8 : info.ResultAsFloat := PInt8(p)^;
      dtFloat16 : info.ResultAsFloat := HalfToFloat(PHalfFloat(p)^);
      dtFloat32 : info.ResultAsFloat := PSingle(p)^;
   else
      info.ResultAsFloat := 0;
   end;
end;

// ------------------
// ------------------ TKCLKernelWrapper ------------------
// ------------------

constructor TKCLKernelWrapper.Create;
begin
   FKernel := TKCLKernel.Create;
end;

destructor TKCLKernelWrapper.Destroy;
begin
   FKernel.Free;
   inherited;
end;

// ------------------
// ------------------ TKCLNodeWrapper ------------------
// ------------------

constructor TKCLNodeWrapper.Create(ANode : TKCLNode);
begin
   FNode := ANode;
end;

destructor TKCLNodeWrapper.Destroy;
begin
   // Node is owned by the Kernel, so we don't free it here
   inherited;
end;

// ------------------
// ------------------ TKernelCreateMethod ------------------
// ------------------

procedure TKernelCreateMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   externalObject := TKCLKernelWrapper.Create;
end;

// ------------------
// ------------------ TKernelAddInputMethod ------------------
// ------------------

procedure TKernelAddInputMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var node := wrapper.FKernel.AddInput(info.ParamAsString[0]);
   
   var classSym := info.FindSymbolInUnits(SYS_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddAddMethod ------------------
// ------------------

procedure TKernelAddAddMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLAddNode.Create('add', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   
   var classSym := info.FindSymbolInUnits(SYS_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddMulMethod ------------------
// ------------------

procedure TKernelAddMulMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var in2 := TKCLNodeWrapper(info.ParamAsScriptObj[1].ExternalObject);
   var node := TKCLMulNode.Create('mul', [in1.FNode, in2.FNode]);
   wrapper.FKernel.AddNode(node);
   
   var classSym := info.FindSymbolInUnits(SYS_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddConv2DMethod ------------------
// ------------------

procedure TKernelAddConv2DMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLConv2DNode.Create('conv2d', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   
   var classSym := info.FindSymbolInUnits(SYS_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelAddGlobalAvgPoolMethod ------------------
// ------------------

procedure TKernelAddGlobalAvgPoolMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var in1 := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   var node := TKCLGlobalAvgPoolNode.Create('gap', [in1.FNode]);
   wrapper.FKernel.AddNode(node);
   
   var classSym := info.FindSymbolInUnits(SYS_NODE) as TClassSymbol;
   var scriptObj := TScriptObjInstance.Create(classSym, info.Execution as TdwsProgramExecution);
   scriptObj.ExternalObject := TKCLNodeWrapper.Create(node);
   info.ResultAsVariant := scriptObj as IUnknown;
end;

// ------------------
// ------------------ TKernelMarkOutputMethod ------------------
// ------------------

procedure TKernelMarkOutputMethod.Execute(info : TProgramInfo; var externalObject : TObject);
begin
   var wrapper := TKCLKernelWrapper(externalObject);
   var node := TKCLNodeWrapper(info.ParamAsScriptObj[0].ExternalObject);
   wrapper.FKernel.MarkOutput(node.FNode);
end;

// ------------------
// ------------------ TKernelCompilerDispatchMethod ------------------
// ------------------

procedure TKernelCompilerDispatchMethod.DoEvalProc(const args : TExprBaseListExec);
begin
   var kernelObj : IScriptObj;
   args.ExprBase[0].EvalAsScriptObj(args.Exec, kernelObj);
   var kernel := TKCLKernelWrapper(kernelObj.ExternalObject);
   
   var argBuffers : IScriptDynArray;
   args.EvalAsDynArray(1, argBuffers);
   
   var count := argBuffers.ArrayLength;
   var buffers : array of TKCLStridedBufferDescriptor;
   SetLength(buffers, count);
   
   for var i := 0 to count - 1 do begin
      var bufUnk : IUnknown;
      argBuffers.EvalAsInterface(i, bufUnk);
      var bufObj : IScriptObj;
      if (bufUnk <> nil) and (bufUnk.QueryInterface(IScriptObj, bufObj) = 0) then
         buffers[i] := TKCLStridedBufferWrapper(bufObj.ExternalObject).Descriptor
      else
         raise EdwsKCLException.Create('KCL: Buffer array element is not a script object.');
   end;

   var backend := TKCLReferenceBackend.Create;
   try
      backend.Execute(kernel.FKernel, buffers);
   finally
      backend.Free;
   end;
end;

// RegisterKernelCompilerSymbols
//
procedure RegisterKernelCompilerSymbols(systemTable : TSystemSymbolTable; unitTable : TSymbolTable;
                                       cleanupEvent : TObjectDestroyEvent);
begin
   var typDataType := TEnumerationSymbol.Create(SYS_DATATYPE, systemTable.TypInteger, enumScoped);
   unitTable.AddSymbol(typDataType);
   typDataType.AddElement(TElementSymbol.Create('Int8', typDataType, Ord(dtInt8), False));
   typDataType.AddElement(TElementSymbol.Create('Float16', typDataType, Ord(dtFloat16), False));
   typDataType.AddElement(TElementSymbol.Create('Float32', typDataType, Ord(dtFloat32), False));

   var clsStridedBuffer := TClassSymbol.Create(SYS_STRIDEDBUFFER, systemTable.TypTObject);
   clsStridedBuffer.OnObjectDestroy := cleanupEvent;
   unitTable.AddSymbol(clsStridedBuffer);
   var clsNode := TClassSymbol.Create(SYS_NODE, systemTable.TypTObject);
   clsNode.OnObjectDestroy := cleanupEvent;
   unitTable.AddSymbol(clsNode);
   var clsKernel := TClassSymbol.Create(SYS_KERNEL, systemTable.TypTObject);
   clsKernel.OnObjectDestroy := cleanupEvent;
   unitTable.AddSymbol(clsKernel);
   var clsKernelCompiler := TClassSymbol.Create(SYS_KERNELCOMPILER, systemTable.TypTObject);
   unitTable.AddSymbol(clsKernelCompiler);

   // Register 'array of TStridedBuffer'
   unitTable.AddSymbol(TDynamicArraySymbol.Create('array of ' + SYS_STRIDEDBUFFER, clsStridedBuffer, systemTable.TypInteger));

   TStridedBufferCreateMethod.Create(mkConstructor, [], 'Create', ['dataType', SYS_DATATYPE, 'dimensions', 'array of Integer'], '', clsStridedBuffer, cvPublic, unitTable);
   TStridedBufferSetDataMethod.Create(mkProcedure, [], 'SetData', ['indices', 'array of Integer', 'value', 'Float'], '', clsStridedBuffer, cvPublic, unitTable);
   TStridedBufferGetDataMethod.Create(mkFunction, [], 'GetData', ['indices', 'array of Integer'], 'Float', clsStridedBuffer, cvPublic, unitTable);

   TKernelCreateMethod.Create(mkConstructor, [], 'Create', [], '', clsKernel, cvPublic, unitTable);
   TKernelAddInputMethod.Create(mkFunction, [], 'AddInput', ['name', 'String'], SYS_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddAddMethod.Create(mkFunction, [], 'AddAdd', ['in1', SYS_NODE, 'in2', SYS_NODE], SYS_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddMulMethod.Create(mkFunction, [], 'AddMul', ['in1', SYS_NODE, 'in2', SYS_NODE], SYS_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddConv2DMethod.Create(mkFunction, [], 'AddConv2D', ['input', SYS_NODE], SYS_NODE, clsKernel, cvPublic, unitTable);
   TKernelAddGlobalAvgPoolMethod.Create(mkFunction, [], 'AddGlobalAvgPool', ['input', SYS_NODE], SYS_NODE, clsKernel, cvPublic, unitTable);
   TKernelMarkOutputMethod.Create(mkProcedure, [], 'MarkOutput', ['node', SYS_NODE], '', clsKernel, cvPublic, unitTable);

   TKernelCompilerDispatchMethod.Create(unitTable, 'Dispatch', ['kernel', SYS_KERNEL, 'buffers', 'array of TStridedBuffer'], '', [iffStaticMethod], clsKernelCompiler);
end;

end.
