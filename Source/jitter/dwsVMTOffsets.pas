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

interface

uses
   dwsExprs, dwsDataContext, dwsSymbols;

var
   vmt_Prepared : Boolean;

   vmt_IDataContext_GetSelf : Integer;
   vmt_IDataContext_AsPVariant : Integer;
   vmt_IDataContext_AsPData : Integer;
   vmt_IDataContext_FData : Integer;
   vmt_TExprBase_EvalNoResult : Integer;
   vmt_TExprBase_EvalAsInteger : Integer;
   vmt_TExprBase_EvalAsFloat : Integer;
   vmt_TExprBase_EvalAsBoolean : Integer;
   vmt_TExprBase_AssignValueAsFloat : Integer;
   vmt_TExprBase_AssignValueAsInteger : Integer;

   vmt_ScriptDynamicArray_IScriptObj_To_FData : Integer;
   vmt_ScriptObjInstance_IScriptObj_To_FData : Integer;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// PrepareVMTOffsets
//
procedure PrepareVMTOffsets;
asm
   mov vmt_Prepared, True
   mov vmt_IDataContext_GetSelf, VMTOFFSET IDataContext.GetSelf
   mov vmt_IDataContext_AsPData, VMTOFFSET IDataContext.AsPData
   mov vmt_IDataContext_AsPVariant, VMTOFFSET IDataContext.AsPVariant
   mov vmt_TExprBase_EvalNoResult, VMTOFFSET TExprBase.EvalNoResult
   mov vmt_TExprBase_EvalAsInteger, VMTOFFSET TExprBase.EvalAsInteger
   mov vmt_TExprBase_EvalAsFloat, VMTOFFSET TExprBase.EvalAsFloat
   mov vmt_TExprBase_EvalAsBoolean, VMTOFFSET TExprBase.EvalAsBoolean
   mov vmt_TExprBase_AssignValueAsFloat, VMTOFFSET TExprBase.AssignValueAsFloat
   mov vmt_TExprBase_AssignValueAsInteger, VMTOFFSET TExprBase.AssignValueAsInteger
end;

procedure PrepareDynArrayIDataContextToFDataOffset;
var
   sda : TScriptDynamicArray;
   soi : TScriptObjInstance;
   i : IScriptObj;
begin
   sda:=TScriptDynamicArray.CreateNew(nil);
   i:=IScriptObj(sda);

   vmt_ScriptDynamicArray_IScriptObj_To_FData:=NativeInt(i.AsPData)-NativeInt(i);

   soi:=TScriptObjInstance.Create(nil);
   i:=IScriptObj(soi);

   vmt_ScriptObjInstance_IScriptObj_To_FData:=NativeInt(i.AsPData)-NativeInt(i);
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
