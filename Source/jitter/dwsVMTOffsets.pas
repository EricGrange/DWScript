{**************************************************************************}
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{     file, You can obtain one at http://mozilla.org/MPL/2.0/.             }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Copyright Eric Grange / Creative IT                                   }
{                                                                          }
{**************************************************************************}
unit dwsVMTOffsets;

{$I ../dws.inc}

interface

uses
   dwsExprs, dwsDataContext, dwsSymbols;

var
   vmt_Prepared : Boolean;

   vmt_IDataContext_GetSelf : Integer;
   vmt_IDataContext_AsPData : Integer;
   vmt_IDataContext_FData : Integer;
   vmt_IScriptObj_ExternalObject : Integer;
   vmt_TExprBase_EvalNoResult : Integer;
   vmt_TExprBase_EvalAsInteger : Integer;
   vmt_TExprBase_EvalAsFloat : Integer;
   vmt_TExprBase_EvalAsBoolean : Integer;
   vmt_TExprBase_EvalAsString: Integer;
   vmt_TExprBase_EvalAsScriptObj: Integer;
   vmt_TExprBase_EvalAsVariant: Integer;
   vmt_TExprBase_EvalAsDynArray: Integer;
   vmt_TExprBase_AssignValueAsFloat : Integer;
   vmt_TExprBase_AssignValueAsInteger : Integer;
   vmt_TExprBase_AssignValueAsBoolean : Integer;

//   vmt_ScriptDynamicArray_IScriptObj_To_FData : Integer;
   vmt_ScriptObjInstance_IScriptObj_To_FData : Integer;

   vmt_ScriptDynamicFloatArray_IScriptDynArray_To_DataPointer : Integer;
   vmt_ScriptDynamicIntegerArray_IScriptDynArray_To_DataPointer : Integer;
   vmt_ScriptDynamicInterfaceArray_IScriptDynArray_To_DataPointer : Integer;

   fld_TdwsExecution_Status : Integer;

{$IF Defined(WIN32)}
   func_ustr_clear: pointer;
   func_handle_finally: pointer;
   func_intf_clear: pointer;
   func_var_clr: pointer;
   func_dyn_array_clear: pointer;
   func_var_from_int: pointer;
{$IFEND}

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Variants, dwsDynamicArrays;

// PrepareVMTOffsets
//
procedure PrepareVMTOffsets;
asm
   mov vmt_Prepared, True
   mov vmt_IDataContext_GetSelf, VMTOFFSET IDataContext.GetSelf
   mov vmt_IDataContext_AsPData, VMTOFFSET IDataContext.AsPData
   mov vmt_IScriptObj_ExternalObject, VMTOFFSET IScriptObj.GetExternalObject
   mov vmt_TExprBase_EvalNoResult, VMTOFFSET TExprBase.EvalNoResult
   mov vmt_TExprBase_EvalAsInteger, VMTOFFSET TExprBase.EvalAsInteger
   mov vmt_TExprBase_EvalAsFloat, VMTOFFSET TExprBase.EvalAsFloat
   mov vmt_TExprBase_EvalAsBoolean, VMTOFFSET TExprBase.EvalAsBoolean
   mov vmt_TExprBase_EvalAsString, VMTOFFSET TExprBase.EvalAsString
   mov vmt_TExprBase_EvalAsScriptObj, VMTOFFSET TExprBase.EvalAsScriptObj
   mov vmt_TExprBase_EvalAsVariant, VMTOFFSET TExprBase.EvalAsVariant
   mov vmt_TExprBase_EvalAsDynArray,  VMTOFFSET TExprBase.EvalAsScriptDynArray
   mov vmt_TExprBase_AssignValueAsFloat, VMTOFFSET TExprBase.AssignValueAsFloat
   mov vmt_TExprBase_AssignValueAsInteger, VMTOFFSET TExprBase.AssignValueAsInteger
   mov vmt_TExprBase_AssignValueAsBoolean, VMTOFFSET TExprBase.AssignValueAsBoolean
{$IF Defined(WIN32)}
   mov func_ustr_clear, offset System.@UStrClr
   mov func_intf_clear, offset System.@IntfClear
   mov func_var_clr, offset variants.@VarClr
   mov func_dyn_array_clear, offset system.@DynArrayClear
   mov func_handle_finally, offset System.@HandleFinally
   mov func_var_from_int, offset Variants.@VarFromInt
{$IFEND}
end;

procedure PrepareDynArrayIDataContextToFDataOffset;
var
   soi : TScriptObjInstance;
   io : IScriptObj;
begin
   vmt_ScriptDynamicFloatArray_IScriptDynArray_To_DataPointer := TScriptDynamicNativeFloatArray.InterfaceToDataOffset;
   vmt_ScriptDynamicIntegerArray_IScriptDynArray_To_DataPointer := TScriptDynamicNativeIntegerArray.InterfaceToDataOffset;
   vmt_ScriptDynamicInterfaceArray_IScriptDynArray_To_DataPointer := TScriptDynamicNativeInterfaceArray.InterfaceToDataOffset;

   soi:=TScriptObjInstance.Create(nil);
   io:=IScriptObj(soi);

   vmt_ScriptObjInstance_IScriptObj_To_FData:=NativeInt(io.AsPData)-NativeInt(io);

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   PrepareVMTOffsets;
   PrepareDynArrayIDataContextToFDataOffset;

end.
