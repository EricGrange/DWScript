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
   dwsDataContext, dwsSymbols;

var
   vmt_Prepared : Boolean;

   vmt_IDataContext_AsPVariant : Integer;
   vmt_TExprBase_EvalNoResult : Integer;
   vmt_TExprBase_EvalAsInteger : Integer;
   vmt_TExprBase_EvalAsFloat : Integer;

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
   mov vmt_IDataContext_AsPVariant, VMTOFFSET IDataContext.AsPVariant
   mov vmt_TExprBase_EvalNoResult, VMTOFFSET TExprBase.EvalNoResult
   mov vmt_TExprBase_EvalAsInteger, VMTOFFSET TExprBase.EvalAsInteger
   mov vmt_TExprBase_EvalAsFloat, VMTOFFSET TExprBase.EvalAsFloat
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   PrepareVMTOffsets;

end.
