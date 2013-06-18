{**************************************************************************}
{                                                                          }
{    This file is release under dual licenses, MPL 2.0 and LGPL.           }
{                                                                          }
{    This Source Code Form is subject to the terms of the Mozilla Public   }
{    License, v. 2.0. If a copy of the MPL was not distributed with this   }
{    file, You can obtain one at http://mozilla.org/MPL/2.0/.              }
{                                                                          }
{    This program is free software: you can redistribute it and/or modify  }
{    it under the terms of the GNU Lesser General Public License as        }
{    published by the Free Software Foundation, either version 3 of the    }
{    License, or (at your option) any later version.                       }
{    Yu can obtain a copy of the LGPL at http://www.gnu.org/licenses/      }
{                                                                          }
{    Software distributed under the License is distributed on an           }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express           }
{    or implied. See the License for the specific language                 }
{    governing rights and limitations under the License.                   }
{                                                                          }
{    Original Copyright: Christian Budde                                   }
{                                                                          }
{**************************************************************************}
unit dwsLLVMClasses;

interface

uses
   dwsUtils, dwsLLVM;

type
   TLLVMContext = class (TRefCountedObject)
      private
         FLLVMContext: PLLVMContext;
      public
         constructor Create; overload;
         constructor Create(Context: PLLVMContext); overload;
         destructor Destroy; override;

         property Handle: PLLVMContext read FLLVMContext;
   end;

   TLLVMModule = class (TRefCountedObject)
      private
         FLLVMModule: PLLVMModule;
         FLLVMContext: TLLVMContext;
         function GetDataLayout: AnsiString;
         function GetTarget: AnsiString;
         function GetTypeByName(Name: AnsiString): PLLVMType;
         procedure SetDataLayout(const Value: AnsiString);
         procedure SetTarget(const Value: AnsiString);
      public
         constructor Create(const Name: AnsiString); overload;
         constructor Create(const Name: AnsiString; Context: TLLVMContext); overload;
         destructor Destroy; override;

         procedure DumpModule;
         function PrintModuleToFile(FileName: AnsiString): Boolean;
         procedure SetModuleInlineAsm(&Asm: AnsiString);
         function VerifyModule(Action: TLLVMVerifierFailureAction = LLVMReturnStatusAction): Boolean;

         function GetNamedMetadataNumOperands(const Name: AnsiString): Cardinal;
         procedure GetNamedMetadataOperands(const Name: AnsiString; var Dest: PLLVMValue);
         procedure AddNamedMetadataOperand(const Name: AnsiString; Value: PLLVMValue);
         function AddFunction(const Name: AnsiString; FunctionType: PLLVMType): PLLVMValue;
         function GetNamedFunction(const Name: AnsiString): PLLVMValue;

         procedure WriteBitcodeToFile(FileName: AnsiString);

         property Handle: PLLVMModule read FLLVMModule;
         property Context: TLLVMContext read FLLVMContext;
         property DataLayout: AnsiString read GetDataLayout write SetDataLayout;
         property Target: AnsiString read GetTarget write SetTarget;
         property TypeByName[Name: AnsiString]: PLLVMType read GetTypeByName;
   end;

   TLLVMBaseType = class (TRefCountedObject)
      private
         FLLVMType: PLLVMType;
         function GetTypeKind: TLLVMTypeKind;
         function GetIsSized: Boolean;
      public
         function GetTypeContext: PLLVMContext;

         property Handle: PLLVMType read FLLVMType;
         property Kind: TLLVMTypeKind read GetTypeKind;
         property IsSized: Boolean read GetIsSized;
   end;

   TLLVMSimpleBaseType = class (TLLVMBaseType)
      public
         constructor Create; overload; virtual; abstract;
         constructor Create(Context: TLLVMContext); overload; virtual; abstract;
   end;

   TLLVMIntBaseType = class (TLLVMSimpleBaseType);

   TLLVMInt1Type = class (TLLVMIntBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMInt8Type = class (TLLVMIntBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMInt16Type = class (TLLVMIntBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMInt32Type = class (TLLVMIntBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMInt64Type = class (TLLVMIntBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMIntType = class (TLLVMIntBaseType)
      public
         constructor Create(BitSize: Cardinal); reintroduce; overload;
         constructor Create(Context: TLLVMContext; BitSize: Cardinal); reintroduce; overload;
   end;

   TLLVMRealBaseType = class (TLLVMSimpleBaseType);

   TLLVMHalfType = class (TLLVMRealBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMFloatType = class (TLLVMRealBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMDoubleType = class (TLLVMRealBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMX86FP80Type = class (TLLVMRealBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMFP128Type = class (TLLVMRealBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMPPCFP128Type = class (TLLVMRealBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMFunctionType = class (TLLVMBaseType)
      private
         function GetIsFunctionVarArg: Boolean;
         function GetReturnType: PLLVMType;
         function GetParameterCount: Cardinal;
         function GetParameterType(Index: Cardinal): PLLVMType;
      public
         constructor Create(ReturnType: PLLVMType;
            ParamTypes: PLLVMTypePtrArray; ParamCount: Cardinal;
            IsVarArg: Boolean = False);

         property IsFunctionVarArg: Boolean read GetIsFunctionVarArg;
         property ReturnType: PLLVMType read GetReturnType;
         property ParameterCount: Cardinal read GetParameterCount;
         property ParameterType[Index: Cardinal]: PLLVMType read GetParameterType;
   end;

   TLLVMStructType = class (TLLVMBaseType)
      private
         function GetStructName: AnsiString;
         function GetItemCount: Cardinal;
         function GetIsOpaque: Boolean;
         function GetIsPacked: Boolean;
         function GetItemType(Index: Cardinal): PLLVMType;
      public
         constructor Create(ElementTypes: PLLVMTypePtrArray;
            ElementCount: Cardinal; &Packed: Boolean = True); overload;
         constructor Create(Context: TLLVMContext;
            ElementTypes: PLLVMTypePtrArray; ElementCount: Cardinal;
            &Packed: Boolean = True); overload;
         constructor Create(Context: TLLVMContext; Name: AnsiString); overload;

         procedure SetBody(ElementTypes: PLLVMTypePtrArray;
            ElementCount: Cardinal; &Packed: Boolean = True);

         property Name: AnsiString read GetStructName;
         property IsPacked: Boolean read GetIsPacked;
         property IsOpaque: Boolean read GetIsOpaque;
         property ItemCount: Cardinal read GetItemCount;
         property ItemType[Index: Cardinal]: PLLVMType read GetItemType;
   end;

   TLLVMArrayType = class (TLLVMBaseType)
      private
         function GetArrayLength: Cardinal;
      public
         constructor Create(ElementType: PLLVMType; ElementCount: Cardinal);

         property ArrayLength: Cardinal read GetArrayLength;
   end;

   TLLVMPointerType = class (TLLVMBaseType)
      private
         function GetPointerAddressSpace: Cardinal;
      public
         constructor Create(ElementType: PLLVMType; AddressSpace: Cardinal);

         property AddressSpace: Cardinal read GetPointerAddressSpace;
   end;

   TLLVMVectorType = class (TLLVMBaseType)
      private
         function GetVectorSize: Cardinal;
      public
         constructor Create(ElementType: PLLVMType; ElementCount: Cardinal);

         property VectorSize: Cardinal read GetVectorSize;
   end;

   TLLVMVoidType = class (TLLVMSimpleBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMLabelType = class (TLLVMSimpleBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMX86MMXType = class (TLLVMSimpleBaseType)
      public
         constructor Create; overload; override;
         constructor Create(Context: TLLVMContext); overload; override;
   end;

   TLLVMValue = class (TRefCountedObject)
      private
         FLLVMValue: PLLVMValue;
         function GetName: AnsiString;
         procedure SetName(const Value: AnsiString);

         function GetIsConstant: Boolean;
         function GetIsUndef: Boolean;
         function GetIsNull: Boolean;
      public
         constructor CreateNull(BaseType: TLLVMBaseType);
         constructor CreatePointerNull(BaseType: TLLVMPointerType);
         constructor CreateUndef(BaseType: TLLVMBaseType);

         function GetType: PLLVMType;
         procedure Dump;

         property Handle: PLLVMValue read FLLVMValue;
         property Name: AnsiString read GetName write SetName;

         property IsConstant: Boolean read GetIsConstant;
         property IsNull: Boolean read GetIsNull;
         property IsUndef: Boolean read GetIsUndef;
   end;

   TLLVMConstIntValue = class (TLLVMValue)
      public
         constructor CreateAllOnes(BaseType: TLLVMIntBaseType);
         constructor CreateInt(BaseType: TLLVMIntBaseType; N: UInt64;
            SignExtend: Boolean = True);
         constructor CreateIntOfArbitraryPrecisionInt(BaseType: TLLVMIntType;
            NumWords: Cardinal; Words: PUInt64);
         constructor CreateIntOfString(BaseType: TLLVMIntBaseType;
            const Text: AnsiString; Radix: Byte); overload;
         constructor CreateIntOfString(BaseType: TLLVMIntBaseType;
            const Text: AnsiString; TextLength: Cardinal; Radix: Byte); overload;

         function GetZExtValue: UInt64;
         function GetSExtValue: Int64;
   end;

   TLLVMConstRealValue = class (TLLVMValue)
      public
         constructor CreateReal(BaseType: TLLVMRealBaseType; N: Double);
         constructor CreateRealOfString(BaseType: TLLVMIntBaseType;
            const Text: AnsiString); overload;
         constructor CreateRealOfString(BaseType: TLLVMIntBaseType;
            const Text: AnsiString; TextLength: Cardinal); overload;
   end;

   TLLVMConstStringValue = class (TLLVMValue)
      public
         constructor Create(Text: AnsiString); overload;
         constructor Create(Context: TLLVMContext; Text: AnsiString); overload;
   end;

   TLLVMConstStructValue = class (TLLVMValue)
      public
         constructor Create(ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean); overload;
         constructor Create(Context: TLLVMContext; ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean); overload;
         constructor Create(Struct: TLLVMStructType; ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean); overload;
   end;

   TLLVMConstArrayValue = class (TLLVMValue)
      public
         constructor Create(ElementTy: PLLVMType; ConstantVals: PLLVMValuePtrArray; Length: Cardinal); overload;
   end;

   TLLVMConstVectorValue = class (TLLVMValue)
      public
         constructor Create(ScalarConstantVals: PLLVMValuePtrArray; Size: Cardinal); overload;
   end;

   TLLVMConstExpression = class (TLLVMValue);

   TLLVMConstNeg = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue);
   end;

   TLLVMConstNSWNeg = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue);
   end;

   TLLVMConstNUWNeg = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue);
   end;

   TLLVMConstFNeg = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue);
   end;

   TLLVMConstNot = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue);
   end;

   TLLVMConstAdd = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstNSWAdd = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstNUWAdd = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstFAdd = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstSub = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstNSWSub = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstNUWSub = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstFSub = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstMul = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstNSWMul = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstNUWMul = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstFMul = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstUDiv = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstSDiv = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstExactSDiv = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstFDiv = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstURem = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstSRem = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstFRem = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstAnd = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstOr = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstXor = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstICmp = class (TLLVMConstExpression)
      public
         constructor Create(Predicate: TLLVMIntPredicate; LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstFCmp = class (TLLVMConstExpression)
      public
         constructor Create(Predicate: TLLVMIntPredicate; LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstShl = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstLShr = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstAShr = class (TLLVMConstExpression)
      public
         constructor Create(LHSConstant: TLLVMValue; RHSConstant: TLLVMValue);
   end;

   TLLVMConstGEP = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ConstantIndices: PLLVMValuePtrArray; NumIndices: Cardinal);
   end;

   TLLVMConstInBoundsGEP = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ConstantIndices: PLLVMValuePtrArray; NumIndices: Cardinal);
   end;

   TLLVMConstTrunc = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstSExt = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstZExt = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstFPTrunc = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstFPExt = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstUIToFP = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstSIToFP = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstFPToUI = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstFPToSI = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstPtrToInt = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstIntToPtr = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstBitCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstZExtOrBitCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstSExtOrBitCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstTruncOrBitCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstPointerCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstIntCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType; isSigned: Boolean);
   end;

   TLLVMConstFPCast = class (TLLVMConstExpression)
      public
         constructor Create(ConstantVal: TLLVMValue; ToType: TLLVMBaseType);
   end;

   TLLVMConstSelect = class (TLLVMConstExpression)
      public
         constructor Create(ConstantCondition: TLLVMValue; ConstantIfTrue: TLLVMValue; ConstantIfFalse: TLLVMValue);
   end;

   TLLVMConstExtractElement = class (TLLVMConstExpression)
      public
         constructor Create(VectorConstant: TLLVMValue; IndexConstant: TLLVMValue);
   end;

   TLLVMConstInsertElement = class (TLLVMConstExpression)
      public
         constructor Create(VectorConstant: TLLVMValue; ElementValueConstant: TLLVMValue; IndexConstant: TLLVMValue);
   end;

   TLLVMConstShuffleVector = class (TLLVMConstExpression)
      public
         constructor Create(VectorAConstant: TLLVMValue; VectorBConstant: TLLVMValue; MaskConstant: TLLVMValue);
   end;

   TLLVMConstExtractValue = class (TLLVMConstExpression)
      public
         constructor Create(AggConstant: TLLVMValue; IdxList: PCardinal; NumIdx: Cardinal);
   end;

   TLLVMConstInsertValue = class (TLLVMConstExpression)
      public
         constructor Create(AggConstant: TLLVMValue; ElementValueConstant: TLLVMValue; IdxList: PCardinal; NumIdx: Cardinal);
   end;

   TLLVMConstInlineAsm = class (TLLVMConstExpression)
      public
         constructor Create(BaseType: TLLVMBaseType; const AsmString,
            Constraints: AnsiString; HasSideEffects: Boolean; IsAlignStack: Boolean);
   end;

   TLLVMBlockAddress = class (TLLVMConstExpression)
      public
         constructor Create(F: TLLVMValue; BB: PLLVMBasicBlock);
   end;

   TLLVMBasicBlock = class;

   TLLVMExpression = class (TLLVMValue)
      public
         function GetInstructionParent: TLLVMBasicBlock;
   end;

   TLLVMBuilder = class;

   TLLVMPhiValue = class (TLLVMExpression)
      public
         constructor Create(Builder: TLLVMBuilder; LLVMType: TLLVMBaseType; Name: AnsiString);
         procedure AddIncoming(IncomingValue: TLLVMValue; IncomingBlock: TLLVMBasicBlock);
   end;

   TLLVMBuilder = class (TRefCountedObject)
      private
         FLLVMBuilder: PLLVMBuilder;
      public
         constructor Create; overload;
         constructor Create(Context: TLLVMContext); overload;
         destructor Destroy; override;

         procedure ClearInsertionPosition;

         procedure PositionBuilder(Block: PLLVMBasicBlock; Instr: TLLVMValue);
         procedure PositionBuilderBefore(Instr: TLLVMValue);
         procedure PositionBuilderAtEnd(Block: PLLVMBasicBlock);
         function GetInsertBlock: PLLVMBasicBlock;
         procedure InsertIntoBuilder(Instr: TLLVMValue);
         procedure InsertIntoBuilderWithName(Instr: TLLVMValue; const Name: AnsiString);

         { Metadata }
         procedure SetCurrentDebugLocation(L: TLLVMValue);
         function GetCurrentDebugLocation: PLLVMValue;
         procedure SetInstDebugLocation(Instr: TLLVMValue);

         { Terminators }
         function BuildRetVoid: PLLVMValue;
         function BuildRet(V: TLLVMValue): PLLVMValue;
         function BuildAggregateRet(var RetVals: PLLVMValuePtrArray; N: Cardinal): PLLVMValue;
         function BuildBr(Dest: PLLVMBasicBlock): PLLVMValue;
         function BuildCondBr(&If: TLLVMValue; &Then: PLLVMBasicBlock; &Else: PLLVMBasicBlock): PLLVMValue;
         function BuildSwitch(V: TLLVMValue; &Else: PLLVMBasicBlock; NumCases: Cardinal): PLLVMValue;
         function BuildIndirectBr(Addr: TLLVMValue; NumDests: Cardinal): PLLVMValue;
         function BuildInvoke(Fn: TLLVMValue; var Args: PLLVMValuePtrArray; NumArgs: Cardinal; &Then: PLLVMBasicBlock; Catch: PLLVMBasicBlock; const Name: AnsiString): PLLVMValue;

         function BuildLandingPad(Ty: TLLVMBaseType; PersFn: TLLVMValue; NumClauses: Cardinal; const Name: AnsiString): PLLVMValue;
         function BuildResume(Exn: TLLVMValue): PLLVMValue;
         function BuildUnreachable: PLLVMValue;

         { Arithmetic }
         function BuildAdd(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNSWAdd(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNUWAdd(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFAdd(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildSub(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNSWSub(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNUWSub(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFSub(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildMul(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNSWMul(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNUWMul(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFMul(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildUDiv(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildSDiv(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildExactSDiv(LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFDiv(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildURem(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildSRem(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFRem(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildShl(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildLShr(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildAShr(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildAnd(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildOr(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildXor(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildBinOp(B: PLLVMBuilder; Op: TLLVMOpcode; LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNeg(V: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNSWNeg(V: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNUWNeg(V: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFNeg(V: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildNot(V: TLLVMValue; const Name: AnsiString): PLLVMValue;

         { Memory }
         function BuildMalloc(Ty: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildArrayMalloc(Ty: TLLVMBaseType; Val: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildAlloca(Ty: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildArrayAlloca(Ty: TLLVMBaseType; Val: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFree(PointerVal: TLLVMValue): PLLVMValue;
         function BuildLoad(PointerVal: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildStore(Val: TLLVMValue; Ptr: TLLVMValue): PLLVMValue;
         function BuildGEP(Pointer: TLLVMValue; var Indices: PLLVMValuePtrArray; NumIndices: Cardinal; const Name: AnsiString): PLLVMValue;
         function BuildInBoundsGEP(Pointer: TLLVMValue; var Indices: PLLVMValuePtrArray; NumIndices: Cardinal; const Name: AnsiString): PLLVMValue;
         function BuildStructGEP(Pointer: TLLVMValue; Idx: Cardinal; const Name: AnsiString): PLLVMValue;
         function BuildGlobalString(Str: AnsiString; const Name: AnsiString): PLLVMValue;
         function BuildGlobalStringPtr(Str: AnsiString; const Name: AnsiString): PLLVMValue;

         { Casts }
         function BuildTrunc(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildZExt(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildSExt(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildFPToUI(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildFPToSI(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildUIToFP(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildSIToFP(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildFPTrunc(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildFPExt(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildPtrToInt(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildIntToPtr(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildBitCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildZExtOrBitCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildSExtOrBitCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildTruncOrBitCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildPointerCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildIntCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildFPCast(Val: TLLVMValue; DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;

         { Comparisons }
         function BuildICmp(Op: TLLVMIntPredicate; LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildFCmp(Op: TLLVMRealPredicate; LHS: TLLVMValue; RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;

         { Miscellaneous instructions }
         function BuildPhi(Ty: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildCall(Fn: TLLVMValue; var Args: PLLVMValuePtrArray; NumArgs: Cardinal; const Name: AnsiString): PLLVMValue;
         function BuildSelect(&If, &Then, &Else: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildVAArg(List: TLLVMValue; Ty: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
         function BuildExtractElement(VecVal: TLLVMValue; Index: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildInsertElement(VecVal, EltVal, Index: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildShuffleVector(V1, V2: TLLVMValue; Mask: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildExtractValue(AggVal: TLLVMValue; Index: Cardinal; const Name: AnsiString): PLLVMValue;
         function BuildInsertValue(AggVal, EltVal: TLLVMValue; Index: Cardinal; const Name: AnsiString): PLLVMValue;

         function BuildIsNull(Val: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildIsNotNull(Val: TLLVMValue; const Name: AnsiString): PLLVMValue;
         function BuildPtrDiff(LHS, RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;

         property Handle: PLLVMBuilder read FLLVMBuilder;
   end;

   TLLVMBasicBlock = class (TRefCountedObject)
      private
         FLLVMBasicBlock: PLLVMBasicBlock;
      public
         constructor AppendInContext(Context: TLLVMContext; Func: PLLVMValue; const Name: AnsiString);
         constructor Append(Func: PLLVMValue; const Name: AnsiString);
         constructor InsertInContext(Context: TLLVMContext; Successor: TLLVMBasicBlock; const Name: AnsiString);
         constructor Insert(Successor: TLLVMBasicBlock; const Name: AnsiString);
         procedure RemoveFromParent;
         procedure MoveAfter(MovePos: TLLVMBasicBlock);
         procedure MoveBefore(MovePos: TLLVMBasicBlock);
         function GetFirstInstruction: PLLVMValue;
         function GetLastInstruction: PLLVMValue;
         function GetTerminator: PLLVMValue;

         property Handle: PLLVMBasicBlock read FLLVMBasicBlock;
   end;

   TLLVMModuleProvider = class (TRefCountedObject)
      private
         FLLVMModuleProvider: PLLVMModuleProvider;
      public
         constructor Create(Module: TLLVMModule);
         destructor Destroy; override;

         property Handle: PLLVMModuleProvider read FLLVMModuleProvider;
   end;

   TLLVMMemoryBuffer = class (TRefCountedObject)
      private
         FLLVMMemoryBuffer: PLLVMMemoryBuffer;
      public
         constructor Create(Path: AnsiString); overload;
         constructor Create; overload;
         destructor Destroy; override;

         property Handle: PLLVMMemoryBuffer read FLLVMMemoryBuffer;
   end;

   TLLVMTargetData = class;

   TLLVMPassManagerBase = class abstract (TRefCountedObject)
      protected
         FLLVMPassManager: PLLVMPassManager;
      public
         destructor Destroy; override;

         procedure AddTargetData(TargetData: TLLVMTargetData);
         procedure AddTargetLibraryInfo(TargetLibraryInfo: PLLVMTargetLibraryInfo);
         property Handle: PLLVMPassManager read FLLVMPassManager;
   end;

   TLLVMPassManager = class sealed (TLLVMPassManagerBase)
      public
         constructor Create; overload;

         function RunPassManager(Module: TLLVMModule): Boolean;
   end;

   TLLVMFunctionPassManager = class sealed (TLLVMPassManagerBase)
      public
         constructor Create(Module: TLLVMModule); overload;
         constructor Create(ModuleProvider: TLLVMModuleProvider); overload;

         function RunFunctionPassManager(Value: TLLVMValue): Boolean;
         function InitializeFunctionPassManager: Boolean;
         function FinalizeFunctionPassManager: Boolean;
   end;

   TLLVMPassManagerBuilder = class abstract (TRefCountedObject)
      private
         FLLVMPassManagerBuilder: PLLVMPassManagerBuilder;
         procedure SetOptLevel(OptLevel: TLLVMCodeGenOptLevel);
         procedure SetSizeLevel(SizeLevel: Cardinal);
         procedure SetDisableUnitAtATime(Value: Boolean);
         procedure SetDisableUnrollLoops(Value: Boolean);
         procedure SetDisableSimplifyLibCalls(Value: Boolean);
      public
         constructor Create;
         destructor Destroy; override;

         procedure UseInlinerWithThreshold(Threshold: Cardinal);
         procedure PopulateFunctionPassManager(PM: TLLVMFunctionPassManager);
         procedure PopulateModulePassManager(PM: TLLVMPassManager);
//         procedure PopulateLTOPassManager(PM: TLLVMPassManager; Internalize, RunInliner: Boolean);

         property OptLevel: TLLVMCodeGenOptLevel write SetOptLevel;
         property SizeLevel: Cardinal write SetSizeLevel;
         property DisableUnitAtATime: Boolean write SetDisableUnitAtATime;
         property DisableUnrollLoops: Boolean write SetDisableUnrollLoops;
         property DisableSimplifyLibCalls: Boolean write SetDisableSimplifyLibCalls;
         property Handle: PLLVMPassManagerBuilder read FLLVMPassManagerBuilder;
   end;

   TLLVMTargetData = class (TRefCountedObject)
      private
         FLLVMTargetData: PLLVMTargetData;
         function GetByteOrder: TLLVMByteOrdering;
         function GetPointerSize: Cardinal;
      public
         constructor Create(StringRep: AnsiString);
         destructor Destroy; override;

         function CopyStringRepOfTargetData: AnsiString;
         function PointerSizeForAS(AddressSpace: Cardinal): Cardinal;
         function SizeOfTypeInBits(&Type: TLLVMBaseType): UInt64;
         function StoreSizeOfType(&Type: TLLVMBaseType): UInt64;

         function IntPtrType: PLLVMType;
         function IntPtrTypeForAS(AddressSpace: Cardinal): PLLVMType;

         function ABISizeOfType(&Type: TLLVMBaseType): UInt64;
         function ABIAlignmentOfType(&Type: TLLVMBaseType): Cardinal;
         function CallFrameAlignmentOfType(&Type: TLLVMBaseType): Cardinal;
         function PreferredAlignmentOfType(&Type: TLLVMBaseType): Cardinal;
         function PreferredAlignmentOfGlobal(GlobalVar: TLLVMValue): Cardinal;
         function ElementAtOffset(StructType: TLLVMStructType; Offset: UInt64): Cardinal;
         function OffsetOfElement(StructType: TLLVMStructType; Element: Cardinal): UInt64;

         property ByteOrder: TLLVMByteOrdering read GetByteOrder;
         property PointerSize: Cardinal read GetPointerSize;
         property Handle: PLLVMTargetData read FLLVMTargetData;
   end;

   TLLVMSectionIterator = class;
   TLLVMSymbolIterator = class;
   TLLVMRelocationIterator = class;

   TLLVMObjectFile = class (TRefCountedObject)
      private
         FLLVMObjectFile: PLLVMObjectFile;
      public
         constructor Create(MemBuf: TLLVMMemoryBuffer);
         destructor Destroy; override;

         function IsSectionIteratorAtEnd(SectionIterator: TLLVMSectionIterator): Boolean;
         function IsSymbolIteratorAtEnd(SymbolIterator: TLLVMSymbolIterator): Boolean;

         property Handle: PLLVMObjectFile read FLLVMObjectFile;
   end;

   TLLVMSectionIterator = class (TRefCountedObject)
      private
         FLLVMSectionIterator: PLLVMSectionIterator;
         function GetSectionName: AnsiString;
         function GetSectionSize: UInt64;
         function GetSectionContents: AnsiString;
         function GetSectionAddress: UInt64;
      public
         constructor Create(ObjectFile: TLLVMObjectFile);
         destructor Destroy; override;

         procedure MoveToContainingSection(Sym: TLLVMSymbolIterator);
         procedure MoveToNextSection;

         function GetSectionContainsSymbol(Sym: TLLVMSymbolIterator): Boolean;
         function IsRelocationIteratorAtEnd(RI: TLLVMRelocationIterator): Boolean;

         property Handle: PLLVMSectionIterator read FLLVMSectionIterator;
         property SectionName: AnsiString read GetSectionName;
         property SectionSize: UInt64 read GetSectionSize;
         property SectionContents: AnsiString read GetSectionContents;
         property SectionAddress: UInt64 read GetSectionAddress;
   end;

   TLLVMSymbolIterator = class (TRefCountedObject)
      private
         FLLVMSymbolIterator: PLLVMSymbolIterator;
         function GetSymbolName: PAnsiChar;
         function GetSymbolAddress: UInt64;
         function GetSymbolFileOffset: UInt64;
         function GetSymbolSize: UInt64;
      public
         constructor Create(ObjectFile: TLLVMObjectFile);
(*
         constructor Create(
         GetRelocationSymbol: TLLVMSymbolIterator;
*)

         destructor Destroy; override;

         procedure MoveToNextSection;

         property Handle: PLLVMSymbolIterator read FLLVMSymbolIterator;
         property SymbolName: PAnsiChar read GetSymbolName;
         property SymbolAddress: UInt64 read GetSymbolAddress;
         property SymbolFileOffset: UInt64 read GetSymbolFileOffset;
         property SymbolSize: UInt64 read GetSymbolSize;
   end;

   TLLVMRelocationIterator = class (TRefCountedObject)
      private
         FLLVMRelocationIterator: PLLVMRelocationIterator;
         function GetRelocationAddress: UInt64;
         function GetRelocationOffset: UInt64;
         function GetRelocationType: UInt64;

         function GetRelocationTypeName: AnsiString;
         function GetRelocationValueString: AnsiString;
      public
         constructor Create(SectionIterator: TLLVMSectionIterator);
         destructor Destroy; override;

         procedure MoveToNextRelocation;

         property Handle: PLLVMRelocationIterator read FLLVMRelocationIterator;
         property RelocationAddress: UInt64 read GetRelocationAddress;
         property RelocationOffset: UInt64 read GetRelocationOffset;
         property RelocationType: UInt64 read GetRelocationType;
         property RelocationTypeName: AnsiString read GetRelocationTypeName;
         property RelocationValueString: AnsiString read GetRelocationValueString;
   end;

   TLTOModule = class (TRefCountedObject)
      private
         FLTOModule: PLTOModule;
         procedure SetTargetTriple(const Value: AnsiString);
         function GetTargetTriple: AnsiString;
         function GetNumSymbols: Integer;
         function GetSymbolAttribute(Index: Integer): TLTOSymbolAttributes;
         function GetSymbolName(Index: Integer): AnsiString;
      public
         constructor Create(Path: AnsiString); overload; virtual;
         constructor Create(Mem: Pointer; Length: NativeUInt); overload; virtual;
         destructor Destroy; override;

         property Handle: PLTOModule read FLTOModule;
         property NumSymbols: Integer read GetNumSymbols;
         property SymbolAttribute[Index: Integer]: TLTOSymbolAttributes read GetSymbolAttribute;
         property SymbolName[Index: Integer]: AnsiString read GetSymbolName;
         property TargetTriple: AnsiString read GetTargetTriple write SetTargetTriple;
   end;


   TLTOCodeGenerator = class (TRefCountedObject)
      private
         FLTOCodeGenerator: PLTOCodeGenerator;
         procedure SetDebugModel(const Value: TLTODebugModel);
         procedure SetCodegenModel(const Value: TLTOCodegenModel);
         procedure SetCPU(const Value: AnsiString);
         procedure SetDebugOptions(const Value: AnsiString);
    procedure SetAssemblerPath(const Value: AnsiString);
      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure AddModule(Module: TLTOModule);
//          procedure SetAssemblerArgs(var args: PAnsiChar; nargs: Integer);
         procedure AddMustPreserveSymbol(Symbol: AnsiString);

         function WriteMergedModules(Path: AnsiString): Boolean;
         function Compile(var Length: NativeUInt): Pointer;
         function CompileToFile(out name: AnsiString): Boolean;

         property AssemblerPath: AnsiString write SetAssemblerPath;
         property CPU: AnsiString write SetCPU;
         property DebugModel: TLTODebugModel write SetDebugModel;
         property DebugOptions: AnsiString write SetDebugOptions;
         property PicModel: TLTOCodegenModel write SetCodegenModel;
   end;



implementation

uses
   SysUtils;

{ TLLVMContext }

constructor TLLVMContext.Create(Context: PLLVMContext);
begin
   FLLVMContext := Context;
end;

constructor TLLVMContext.Create;
begin
   Create(LLVMContextCreate);
end;

destructor TLLVMContext.Destroy;
begin
   LLVMContextDispose(FLLVMContext);
end;


{ TLLVMModule }

constructor TLLVMModule.Create(const Name: AnsiString);
begin
   FLLVMModule := LLVMModuleCreateWithName(PAnsiChar(Name));
   FLLVMContext := TLLVMContext.Create(LLVMGetModuleContext(FLLVMModule));
end;

constructor TLLVMModule.Create(const Name: AnsiString; Context: TLLVMContext);
begin
   FLLVMModule := LLVMModuleCreateWithNameInContext(PAnsiChar(Name), Context.Handle);
   FLLVMContext := Context;

   Assert(LLVMGetModuleContext(FLLVMModule) = Context.Handle);
end;

destructor TLLVMModule.Destroy;
begin
   LLVMDisposeModule(FLLVMModule);
//   FreeAndNil(FLLVMContext);
end;

function TLLVMModule.AddFunction(const Name: AnsiString;
  FunctionType: PLLVMType): PLLVMValue;
begin
   Result := LLVMAddFunction(FLLVMModule, PAnsiChar(Name), FunctionType);
end;

procedure TLLVMModule.AddNamedMetadataOperand(const Name: AnsiString;
  Value: PLLVMValue);
begin
   LLVMAddNamedMetadataOperand(FLLVMModule, PAnsiChar(Name), Value);
end;

procedure TLLVMModule.DumpModule;
begin
   LLVMDumpModule(FLLVMModule);
end;

function TLLVMModule.GetDataLayout: AnsiString;
begin
   Result := LLVMGetDataLayout(FLLVMModule);
end;

function TLLVMModule.GetNamedFunction(const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMGetNamedFunction(FLLVMModule, PAnsiChar(Name));
end;

function TLLVMModule.GetNamedMetadataNumOperands(
  const Name: AnsiString): Cardinal;
begin
   Result := LLVMGetNamedMetadataNumOperands(FLLVMModule, PAnsiChar(Name));
end;

procedure TLLVMModule.GetNamedMetadataOperands(const Name: AnsiString;
  var Dest: PLLVMValue);
begin
   LLVMGetNamedMetadataOperands(FLLVMModule, PAnsiChar(Name), Dest);
end;

function TLLVMModule.GetTarget: AnsiString;
begin
   Result := LLVMGetTarget(FLLVMModule);
end;

function TLLVMModule.GetTypeByName(Name: AnsiString): PLLVMType;
begin
   Result := LLVMGetTypeByName(FLLVMModule, PAnsiChar(Name));
end;

function TLLVMModule.PrintModuleToFile(FileName: AnsiString): Boolean;
var
   ErrorMessage: PAnsiChar;
begin
   ErrorMessage := nil;
   Result := LLVMPrintModuleToFile(FLLVMModule, PAnsiChar(FileName), ErrorMessage);
   try
      if Result and Assigned(ErrorMessage) then
         raise Exception.Create(string(AnsiString(ErrorMessage)));
   finally
      LLVMDisposeMessage(ErrorMessage);
   end;
end;

procedure TLLVMModule.SetDataLayout(const Value: AnsiString);
begin
   LLVMSetDataLayout(FLLVMModule, PAnsiChar(Value));
end;

procedure TLLVMModule.SetModuleInlineAsm(&Asm: AnsiString);
begin
   LLVMSetModuleInlineAsm(FLLVMModule, PAnsiChar(&Asm));
end;

procedure TLLVMModule.SetTarget(const Value: AnsiString);
begin
   LLVMSetTarget(FLLVMModule, PAnsiChar(Value));
end;

function TLLVMModule.VerifyModule(Action: TLLVMVerifierFailureAction = LLVMReturnStatusAction): Boolean;
var
   ErrorMessage: PAnsiChar;
begin
   ErrorMessage := nil;
   Result := not LLVMVerifyModule(FLLVMModule, Action, ErrorMessage);

   if Action = LLVMAbortProcessAction then
      raise Exception.Create(string(AnsiString(ErrorMessage)));
end;

procedure TLLVMModule.WriteBitcodeToFile(FileName: AnsiString);
begin
   LLVMWriteBitcodeToFile(FLLVMModule, PAnsiChar(FileName));
end;


{ TLLVMBaseType }

function TLLVMBaseType.GetIsSized: Boolean;
begin
   Result := LLVMTypeIsSized(FLLVMType);
end;

function TLLVMBaseType.GetTypeContext: PLLVMContext;
begin
   Result := LLVMGetTypeContext(FLLVMType);
end;

function TLLVMBaseType.GetTypeKind: TLLVMTypeKind;
begin
   Result := LLVMGetTypeKind(FLLVMType);
end;


{ TLLVMInt1Type }

constructor TLLVMInt1Type.Create;
begin
   inherited;
   FLLVMType := LLVMInt1Type;
end;

constructor TLLVMInt1Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMInt1TypeInContext(Context.Handle);
end;

procedure TLLVMBuilder.SetCurrentDebugLocation(L: TLLVMValue);
begin
   LLVMSetCurrentDebugLocation(FLLVMBuilder, L.Handle);
end;


{ TLLVMInt8Type }

constructor TLLVMInt8Type.Create;
begin
   inherited;
   FLLVMType := LLVMInt8Type;
end;

constructor TLLVMInt8Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMInt8TypeInContext(Context.Handle);
end;


{ TLLVMInt16Type }

constructor TLLVMInt16Type.Create;
begin
   inherited;
   FLLVMType := LLVMInt16Type;
end;

constructor TLLVMInt16Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMInt16TypeInContext(Context.Handle);
end;


{ TLLVMInt32Type }

constructor TLLVMInt32Type.Create;
begin
   inherited;
   FLLVMType := LLVMInt32Type;
end;

constructor TLLVMInt32Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMInt32TypeInContext(Context.Handle);
end;


{ TLLVMInt64Type }

constructor TLLVMInt64Type.Create;
begin
   inherited;
   FLLVMType := LLVMInt64Type;
end;

constructor TLLVMInt64Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMInt64TypeInContext(Context.Handle);
end;


{ TLLVMIntType }

constructor TLLVMIntType.Create(BitSize: Cardinal);
begin
   inherited;
   FLLVMType := LLVMIntType(BitSize);
end;

constructor TLLVMIntType.Create(Context: TLLVMContext; BitSize: Cardinal);
begin
   inherited;
   FLLVMType := LLVMIntTypeInContext(Context.Handle, BitSize);
end;

{ TLLVMHalfType }

constructor TLLVMHalfType.Create;
begin
   inherited;
   FLLVMType := LLVMHalfType;
end;

constructor TLLVMHalfType.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMHalfTypeInContext(Context.Handle);
end;


{ TLLVMFloatType }

constructor TLLVMFloatType.Create;
begin
   inherited;
   FLLVMType := LLVMFloatType;
end;

constructor TLLVMFloatType.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMFloatTypeInContext(Context.Handle);
end;


{ TLLVMDoubleType }

constructor TLLVMDoubleType.Create;
begin
   inherited;
   FLLVMType := LLVMDoubleType;
end;

constructor TLLVMDoubleType.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMDoubleTypeInContext(Context.Handle);
end;


{ TLLVMX86FP80Type }

constructor TLLVMX86FP80Type.Create;
begin
   inherited;
   FLLVMType := LLVMX86FP80Type;
end;

constructor TLLVMX86FP80Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMX86FP80TypeInContext(Context.Handle);
end;


{ TLLVMFP128Type }

constructor TLLVMFP128Type.Create;
begin
   inherited;
   FLLVMType := LLVMFP128Type;
end;

constructor TLLVMFP128Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMFP128TypeInContext(Context.Handle);
end;


{ TLLVMPPCFP128Type }

constructor TLLVMPPCFP128Type.Create;
begin
   inherited;
   FLLVMType := LLVMPPCFP128Type;
end;

constructor TLLVMPPCFP128Type.Create(Context: TLLVMContext);
begin
   inherited;
   FLLVMType := LLVMPPCFP128TypeInContext(Context.Handle);
end;


{ TLLVMFunctionType }

constructor TLLVMFunctionType.Create(ReturnType: PLLVMType;
  ParamTypes: PLLVMTypePtrArray; ParamCount: Cardinal; IsVarArg: Boolean = False);
begin
   FLLVMType := LLVMFunctionType(ReturnType, ParamTypes, ParamCount, IsVarArg);
end;

function TLLVMFunctionType.GetIsFunctionVarArg: Boolean;
begin
   Result := LLVMIsFunctionVarArg(FLLVMType);
end;

function TLLVMFunctionType.GetParameterCount: Cardinal;
begin
   Result := LLVMCountParamTypes(FLLVMType);
end;

function TLLVMFunctionType.GetParameterType(Index: Cardinal): PLLVMType;
var
   Dest: PLLVMType;
begin
   LLVMGetParamTypes(FLLVMType, Dest);
   Result := PLLVMTypePtrArray(Dest)[Index];
end;

function TLLVMFunctionType.GetReturnType: PLLVMType;
begin
   Result := LLVMGetReturnType(FLLVMType);
end;


{ TLLVMStructType }

constructor TLLVMStructType.Create(ElementTypes: PLLVMTypePtrArray;
   ElementCount: Cardinal; &Packed: Boolean = True);
begin
   FLLVMType := LLVMStructType(ElementTypes, ElementCount, &Packed);
end;

constructor TLLVMStructType.Create(Context: TLLVMContext;
   ElementTypes: PLLVMTypePtrArray; ElementCount: Cardinal;
   &Packed: Boolean = True);
begin
   FLLVMType := LLVMStructTypeInContext(Context.Handle, ElementTypes,
      ElementCount, &Packed);
end;

constructor TLLVMStructType.Create(Context: TLLVMContext; Name: AnsiString);
begin
   FLLVMType := LLVMStructCreateNamed(Context.Handle, PAnsiChar(Name));
end;

function TLLVMStructType.GetStructName: AnsiString;
begin
   Result := LLVMGetStructName(FLLVMType);
end;

procedure TLLVMStructType.SetBody(ElementTypes: PLLVMTypePtrArray;
  ElementCount: Cardinal; &Packed: Boolean);
begin
   LLVMStructSetBody(FLLVMType, ElementTypes, ElementCount, &Packed);
end;

function TLLVMStructType.GetIsOpaque: Boolean;
begin
   Result := LLVMIsOpaqueStruct(FLLVMType);
end;

function TLLVMStructType.GetIsPacked: Boolean;
begin
   Result := LLVMIsPackedStruct(FLLVMType);
end;

function TLLVMStructType.GetItemCount: Cardinal;
begin
   Result := LLVMCountStructElementTypes(FLLVMType);
end;

function TLLVMStructType.GetItemType(Index: Cardinal): PLLVMType;
var
   Dest: PLLVMType;
begin
   LLVMGetStructElementTypes(FLLVMType, Dest);
   Result := PLLVMTypePtrArray(Dest)[Index];
end;


{ TLLVMArrayType }

constructor TLLVMArrayType.Create(ElementType: PLLVMType;
  ElementCount: Cardinal);
begin
   FLLVMType := LLVMArrayType(ElementType, ElementCount);
end;

function TLLVMArrayType.GetArrayLength: Cardinal;
begin
   Result := LLVMGetArrayLength(FLLVMType);
end;


{ TLLVMPointerType }

constructor TLLVMPointerType.Create(ElementType: PLLVMType;
  AddressSpace: Cardinal);
begin
   FLLVMType := LLVMPointerType(ElementType, AddressSpace);
end;

function TLLVMPointerType.GetPointerAddressSpace: Cardinal;
begin
   Result := LLVMGetPointerAddressSpace(FLLVMType);
end;


{ TLLVMVectorType }

constructor TLLVMVectorType.Create(ElementType: PLLVMType;
  ElementCount: Cardinal);
begin
   FLLVMType := LLVMVectorType(ElementType, ElementCount);
end;

function TLLVMVectorType.GetVectorSize: Cardinal;
begin
   Result := LLVMGetVectorSize(FLLVMType);
end;


{ TLLVMVoidType }

constructor TLLVMVoidType.Create;
begin
   FLLVMType := LLVMVoidType;
end;

constructor TLLVMVoidType.Create(Context: TLLVMContext);
begin
   FLLVMType := LLVMVoidTypeInContext(Context.Handle);
end;


{ TLLVMLabelType }

constructor TLLVMLabelType.Create;
begin
   FLLVMType := LLVMLabelType;
end;

constructor TLLVMLabelType.Create(Context: TLLVMContext);
begin
   FLLVMType := LLVMLabelTypeInContext(Context.Handle);
end;


{ TLLVMX86MMXType }

constructor TLLVMX86MMXType.Create;
begin
   FLLVMType := LLVMX86MMXType;
end;

constructor TLLVMX86MMXType.Create(Context: TLLVMContext);
begin
   FLLVMType := LLVMX86MMXTypeInContext(Context.Handle);
end;


{ TLLVMValue }

constructor TLLVMValue.CreateNull(BaseType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstNull(BaseType.Handle);
end;

constructor TLLVMValue.CreatePointerNull(BaseType: TLLVMPointerType);
begin
   FLLVMValue := LLVMConstPointerNull(BaseType.Handle);
end;

constructor TLLVMValue.CreateUndef(BaseType: TLLVMBaseType);
begin
   FLLVMValue := LLVMGetUndef(BaseType.Handle);
end;

function TLLVMValue.GetName: AnsiString;
begin
   Result := LLVMGetValueName(FLLVMValue);
end;

procedure TLLVMValue.SetName(const Value: AnsiString);
begin
   LLVMSetValueName(FLLVMValue, PAnsiChar(Value));
end;

procedure TLLVMValue.Dump;
begin
   LLVMDumpValue(FLLVMValue);
end;

function TLLVMValue.GetType: PLLVMType;
begin
   Result := LLVMTypeOf(FLLVMValue);
end;

function TLLVMValue.GetIsConstant: Boolean;
begin
   Result := LLVMIsConstant(FLLVMValue);
end;

function TLLVMValue.GetIsNull: Boolean;
begin
   Result := LLVMIsNull(FLLVMValue);
end;

function TLLVMValue.GetIsUndef: Boolean;
begin
   Result := LLVMIsUndef(FLLVMValue);
end;


{ TLLVMConstIntValue }

constructor TLLVMConstIntValue.CreateAllOnes(BaseType: TLLVMIntBaseType);
begin
   FLLVMValue := LLVMConstAllOnes(BaseType.Handle);
end;

constructor TLLVMConstIntValue.CreateInt(BaseType: TLLVMIntBaseType; N: UInt64;
   SignExtend: Boolean = True);
begin
   FLLVMValue := LLVMConstInt(BaseType.Handle, N, SignExtend);
end;

constructor TLLVMConstIntValue.CreateIntOfArbitraryPrecisionInt(
   BaseType: TLLVMIntType; NumWords: Cardinal; Words: PUInt64);
begin
   FLLVMValue := LLVMConstIntOfArbitraryPrecision(BaseType.Handle, NumWords, Words);
end;

constructor TLLVMConstIntValue.CreateIntOfString(BaseType: TLLVMIntBaseType;
   const Text: AnsiString; Radix: Byte);
begin
   FLLVMValue := LLVMConstIntOfString(BaseType.Handle, PAnsiChar(Text), Radix);
end;

constructor TLLVMConstIntValue.CreateIntOfString(BaseType: TLLVMIntBaseType;
   const Text: AnsiString; TextLength: Cardinal; Radix: Byte);
begin
   FLLVMValue := LLVMConstIntOfStringAndSize(BaseType.Handle, PAnsiChar(Text),
      TextLength, Radix);
end;

function TLLVMConstIntValue.GetZExtValue: UInt64;
begin
   Result := LLVMConstIntGetZExtValue(FLLVMValue);
end;

function TLLVMConstIntValue.GetSExtValue: Int64;
begin
   Result := LLVMConstIntGetSExtValue(FLLVMValue);
end;


{ TLLVMConstRealValue }

constructor TLLVMConstRealValue.CreateReal(BaseType: TLLVMRealBaseType; N: Double);
begin
   FLLVMValue := LLVMConstReal(BaseType.Handle, N);
end;

constructor TLLVMConstRealValue.CreateRealOfString(BaseType: TLLVMIntBaseType;
   const Text: AnsiString);
begin
   FLLVMValue := LLVMConstRealOfString(BaseType.Handle, PAnsiChar(Text));
end;

constructor TLLVMConstRealValue.CreateRealOfString(BaseType: TLLVMIntBaseType;
   const Text: AnsiString; TextLength: Cardinal);
begin
   FLLVMValue := LLVMConstRealOfStringAndSize(BaseType.Handle, PAnsiChar(Text),
      TextLength);
end;

{ TLLVMConstStringValue }

constructor TLLVMConstStringValue.Create(Text: AnsiString);
begin
   FLLVMValue := LLVMConstString(PAnsiChar(Text), Length(Text), False);
end;

constructor TLLVMConstStringValue.Create(Context: TLLVMContext;
   Text: AnsiString);
begin
   FLLVMValue := LLVMConstStringInContext(Context.Handle, PAnsiChar(Text),
      Length(Text), False);
end;


{ TLLVMConstStructValue }

constructor TLLVMConstStructValue.Create(ConstantVals: PLLVMValuePtrArray;
   Count: Cardinal; &Packed: Boolean);
begin
   FLLVMValue := LLVMConstStruct(ConstantVals, Count, &Packed);
end;

constructor TLLVMConstStructValue.Create(Context: TLLVMContext;
   ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean);
begin
   FLLVMValue := LLVMConstStructInContext(Context.Handle, ConstantVals, Count,
      &Packed);
end;

constructor TLLVMConstStructValue.Create(Struct: TLLVMStructType;
   ConstantVals: PLLVMValuePtrArray; Count: Cardinal; &Packed: Boolean);
begin
   FLLVMValue := LLVMConstNamedStruct(Struct.Handle, ConstantVals, Count);
end;


{ TLLVMConstArrayValue }

constructor TLLVMConstArrayValue.Create(ElementTy: PLLVMType;
  ConstantVals: PLLVMValuePtrArray; Length: Cardinal);
begin
   FLLVMValue := LLVMConstArray(ElementTy, ConstantVals, Length);
end;


{ TLLVMConstVectorValue }

constructor TLLVMConstVectorValue.Create(ScalarConstantVals: PLLVMValuePtrArray;
  Size: Cardinal);
begin
   FLLVMValue := LLVMConstVector(ScalarConstantVals, Size);
end;


{ TLLVMConstNeg }

constructor TLLVMConstNeg.Create(ConstantVal: TLLVMValue);
begin
   FLLVMValue := LLVMConstNeg(ConstantVal.Handle);
end;


{ TLLVMConstNSWNeg }

constructor TLLVMConstNSWNeg.Create(ConstantVal: TLLVMValue);
begin
   FLLVMValue := LLVMConstNSWNeg(ConstantVal.Handle);
end;


{ TLLVMConstNUWNeg }

constructor TLLVMConstNUWNeg.Create(ConstantVal: TLLVMValue);
begin
   FLLVMValue := LLVMConstNUWNeg(ConstantVal.Handle);
end;


{ TLLVMConstFNeg }

constructor TLLVMConstFNeg.Create(ConstantVal: TLLVMValue);
begin
   FLLVMValue := LLVMConstFNeg(ConstantVal.Handle);
end;


{ TLLVMConstNot }

constructor TLLVMConstNot.Create(ConstantVal: TLLVMValue);
begin
   FLLVMValue := LLVMConstNot(ConstantVal.Handle);
end;


{ TLLVMConstAdd }

constructor TLLVMConstAdd.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstAdd(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstNSWAdd }

constructor TLLVMConstNSWAdd.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstNSWAdd(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstNUWAdd }

constructor TLLVMConstNUWAdd.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstNUWAdd(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstFAdd }

constructor TLLVMConstFAdd.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstFAdd(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstSub }

constructor TLLVMConstSub.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstSub(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstNSWSub }

constructor TLLVMConstNSWSub.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstNSWSub(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstNUWSub }

constructor TLLVMConstNUWSub.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstNUWSub(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstFSub }

constructor TLLVMConstFSub.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstFSub(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstMul }

constructor TLLVMConstMul.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstMul(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstNSWMul }

constructor TLLVMConstNSWMul.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstNSWMul(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstNUWMul }

constructor TLLVMConstNUWMul.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstNUWMul(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstFMul }

constructor TLLVMConstFMul.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstFMul(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstUDiv }

constructor TLLVMConstUDiv.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstUDiv(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstSDiv }

constructor TLLVMConstSDiv.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstSDiv(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstExactSDiv }

constructor TLLVMConstExactSDiv.Create(LHSConstant,
  RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstSDiv(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstFDiv }

constructor TLLVMConstFDiv.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstFDiv(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstURem }

constructor TLLVMConstURem.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstURem(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstSRem }

constructor TLLVMConstSRem.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstSRem(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstFRem }

constructor TLLVMConstFRem.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstFRem(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstAnd }

constructor TLLVMConstAnd.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstAnd(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstOr }

constructor TLLVMConstOr.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstOr(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstXor }

constructor TLLVMConstXor.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstXor(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstICmp }

constructor TLLVMConstICmp.Create(Predicate: TLLVMIntPredicate; LHSConstant,
  RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstICmp(Predicate, LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstFCmp }

constructor TLLVMConstFCmp.Create(Predicate: TLLVMIntPredicate; LHSConstant,
  RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstFCmp(Predicate, LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstShl }

constructor TLLVMConstShl.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstShl(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstLShr }

constructor TLLVMConstLShr.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstLShr(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstAShr }

constructor TLLVMConstAShr.Create(LHSConstant, RHSConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstAShr(LHSConstant.Handle, RHSConstant.Handle);
end;


{ TLLVMConstGEP }

constructor TLLVMConstGEP.Create(ConstantVal: TLLVMValue;
   ConstantIndices: PLLVMValuePtrArray; NumIndices: Cardinal);
begin
   FLLVMValue := LLVMConstGEP(ConstantVal.Handle, ConstantIndices, NumIndices);
end;


{ TLLVMConstInBoundsGEP }

constructor TLLVMConstInBoundsGEP.Create(ConstantVal: TLLVMValue;
   ConstantIndices: PLLVMValuePtrArray; NumIndices: Cardinal);
begin
   FLLVMValue := LLVMConstInBoundsGEP(ConstantVal.Handle, ConstantIndices,
      NumIndices);
end;


{ TLLVMConstTrunc }

constructor TLLVMConstTrunc.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstTrunc(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstSExt }

constructor TLLVMConstSExt.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstSExt(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstZExt }

constructor TLLVMConstZExt.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstZExt(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstFPTrunc }

constructor TLLVMConstFPTrunc.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstFPTrunc(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstFPExt }

constructor TLLVMConstFPExt.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstFPExt(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstUIToFP }

constructor TLLVMConstUIToFP.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstUIToFP(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstSIToFP }

constructor TLLVMConstSIToFP.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstSIToFP(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstFPToUI }

constructor TLLVMConstFPToUI.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstFPToUI(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstFPToSI }

constructor TLLVMConstFPToSI.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstFPToSI(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstPtrToInt }

constructor TLLVMConstPtrToInt.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstPtrToInt(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstIntToPtr }

constructor TLLVMConstIntToPtr.Create(ConstantVal: TLLVMValue;
  ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstIntToPtr(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstBitCast }

constructor TLLVMConstBitCast.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstBitCast(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstZExtOrBitCast }

constructor TLLVMConstZExtOrBitCast.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstZExtOrBitCast(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstSExtOrBitCast }

constructor TLLVMConstSExtOrBitCast.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstSExtOrBitCast(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstTruncOrBitCast }

constructor TLLVMConstTruncOrBitCast.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstTruncOrBitCast(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstPointerCast }

constructor TLLVMConstPointerCast.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstPointerCast(ConstantVal.Handle, ToType.Handle);
end;


{ TLLVMConstIntCast }

constructor TLLVMConstIntCast.Create(ConstantVal: TLLVMValue;
  ToType: TLLVMBaseType; isSigned: Boolean);
begin
   FLLVMValue := LLVMConstIntCast(ConstantVal.Handle, ToType.Handle, isSigned);
end;

{ TLLVMConstFPCast }

constructor TLLVMConstFPCast.Create(ConstantVal: TLLVMValue;
   ToType: TLLVMBaseType);
begin
   FLLVMValue := LLVMConstFPCast(ConstantVal.Handle, ToType.Handle);

end;


{ TLLVMConstSelect }

constructor TLLVMConstSelect.Create(ConstantCondition, ConstantIfTrue,
  ConstantIfFalse: TLLVMValue);
begin
   FLLVMValue := LLVMConstSelect(ConstantCondition.Handle,
      ConstantIfTrue.Handle, ConstantIfFalse.Handle);
end;


{ TLLVMConstExtractElement }

constructor TLLVMConstExtractElement.Create(VectorConstant,
  IndexConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstExtractElement(VectorConstant.Handle,
      IndexConstant.Handle);
end;


{ TLLVMConstInsertElement }

constructor TLLVMConstInsertElement.Create(VectorConstant, ElementValueConstant,
  IndexConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstInsertElement(VectorConstant.Handle,
      ElementValueConstant.Handle, IndexConstant.Handle);
end;


{ TLLVMConstShuffleVector }

constructor TLLVMConstShuffleVector.Create(VectorAConstant, VectorBConstant,
  MaskConstant: TLLVMValue);
begin
   FLLVMValue := LLVMConstShuffleVector(VectorAConstant.Handle,
      VectorBConstant.Handle, MaskConstant.Handle);
end;


{ TLLVMConstExtractValue }

constructor TLLVMConstExtractValue.Create(AggConstant: TLLVMValue;
  IdxList: PCardinal; NumIdx: Cardinal);
begin
   FLLVMValue := LLVMConstExtractValue(AggConstant.Handle, IdxList, NumIdx);
end;


{ TLLVMConstInsertValue }

constructor TLLVMConstInsertValue.Create(AggConstant,
  ElementValueConstant: TLLVMValue; IdxList: PCardinal; NumIdx: Cardinal);
begin
   FLLVMValue := LLVMConstInsertValue(AggConstant.Handle,
      ElementValueConstant.Handle, IdxList, NumIdx);
end;


{ TLLVMConstInlineAsm }

constructor TLLVMConstInlineAsm.Create(BaseType: TLLVMBaseType; const AsmString,
  Constraints: AnsiString; HasSideEffects, IsAlignStack: Boolean);
begin
   FLLVMValue := LLVMConstInlineAsm(BaseType.Handle, PAnsiChar(AsmString),
      PAnsiChar(AsmString), HasSideEffects, IsAlignStack);
end;


{ TLLVMBlockAddress }

constructor TLLVMBlockAddress.Create(F: TLLVMValue; BB: PLLVMBasicBlock);
begin
   FLLVMValue := LLVMBlockAddress(F.Handle, BB);
end;


{ TLLVMExpression }

function TLLVMExpression.GetInstructionParent: TLLVMBasicBlock;
begin
   LLVMGetInstructionParent(FLLVMValue); // HV: Note warning here! (Result not set)
end;


{ TLLVMBasicBlock }

constructor TLLVMBasicBlock.Append(Func: PLLVMValue; const Name: AnsiString);
begin
   LLVMAppendBasicBlock(Func, PAnsiChar(Name));
end;

constructor TLLVMBasicBlock.AppendInContext(Context: TLLVMContext; Func: PLLVMValue; const Name: AnsiString);
begin
   LLVMAppendBasicBlockInContext(Context.Handle, Func, PAnsiChar(Name));
end;

function TLLVMBasicBlock.GetFirstInstruction: PLLVMValue;
begin
   Result := LLVMGetFirstInstruction(FLLVMBasicBlock);
end;

function TLLVMBasicBlock.GetTerminator: PLLVMValue;
begin
   Result := LLVMGetBasicBlockTerminator(FLLVMBasicBlock);
end;

constructor TLLVMBasicBlock.Insert(Successor: TLLVMBasicBlock;
  const Name: AnsiString);
begin
   LLVMInsertBasicBlock(Successor.Handle, PAnsiChar(Name));
end;

constructor TLLVMBasicBlock.InsertInContext(Context: TLLVMContext;
  Successor: TLLVMBasicBlock; const Name: AnsiString);
begin
   LLVMInsertBasicBlockInContext(Context.Handle, Successor.Handle,
      PAnsiChar(Name));
end;

function TLLVMBasicBlock.GetLastInstruction: PLLVMValue;
begin
   Result := LLVMGetLastInstruction(FLLVMBasicBlock);
end;

procedure TLLVMBasicBlock.MoveAfter(MovePos: TLLVMBasicBlock);
begin
   LLVMMoveBasicBlockAfter(FLLVMBasicBlock, MovePos.Handle);
end;

procedure TLLVMBasicBlock.MoveBefore(MovePos: TLLVMBasicBlock);
begin
   LLVMMoveBasicBlockBefore(FLLVMBasicBlock, MovePos.Handle);
end;

procedure TLLVMBasicBlock.RemoveFromParent;
begin
   LLVMRemoveBasicBlockFromParent(FLLVMBasicBlock);
end;


{ TLLVMPhiValue }

constructor TLLVMPhiValue.Create(Builder: TLLVMBuilder; LLVMType: TLLVMBaseType;
   Name: AnsiString);
begin
   FLLVMValue := LLVMBuildPhi(Builder.Handle, LLVMType.Handle, PAnsiChar(Name));
end;

procedure TLLVMPhiValue.AddIncoming(IncomingValue: TLLVMValue;
   IncomingBlock: TLLVMBasicBlock);
begin
   LLVMAddIncoming(FLLVMValue, @IncomingValue.Handle, @IncomingBlock.Handle, 1);
end;


{ TLLVMBuilder }

constructor TLLVMBuilder.Create;
begin
  FLLVMBuilder := LLVMCreateBuilder;
end;

constructor TLLVMBuilder.Create(Context: TLLVMContext);
begin
  FLLVMBuilder := LLVMCreateBuilderInContext(Context.Handle);
end;

destructor TLLVMBuilder.Destroy;
begin
   LLVMDisposeBuilder(FLLVMBuilder);
   inherited;
end;

function TLLVMBuilder.BuildAdd(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildAdd(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildAggregateRet(var RetVals: PLLVMValuePtrArray;
  N: Cardinal): PLLVMValue;
begin
   Result := LLVMBuildAggregateRet(FLLVMBuilder, RetVals, N);
end;

function TLLVMBuilder.BuildAlloca(Ty: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildAlloca(FLLVMBuilder, Ty.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildAnd(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildAnd(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildArrayAlloca(Ty: TLLVMBaseType; Val: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildArrayAlloca(FLLVMBuilder, Ty.Handle, Val.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildArrayMalloc(Ty: TLLVMBaseType; Val: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildArrayMalloc(FLLVMBuilder, Ty.Handle, Val.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildAShr(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildAShr(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildBinOp(B: PLLVMBuilder; Op: TLLVMOpcode; LHS,
  RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildBinOp(FLLVMBuilder, Op, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildBitCast(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildBitCast(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildBr(Dest: PLLVMBasicBlock): PLLVMValue;
begin
   Result := LLVMBuildBr(FLLVMBuilder, Dest);
end;

function TLLVMBuilder.BuildCall(Fn: TLLVMValue; var Args: PLLVMValuePtrArray;
  NumArgs: Cardinal; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildCall(FLLVMBuilder, Fn.Handle, Args, NumArgs, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildCast(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildCast(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildCondBr(&If: TLLVMValue; &Then,
  &Else: PLLVMBasicBlock): PLLVMValue;
begin
   Result := LLVMBuildCondBr(&FLLVMBuilder, &If.Handle, &Then, &Else);
end;

function TLLVMBuilder.BuildExactSDiv(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildExactSDiv(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildExtractElement(VecVal, Index: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildExtractElement(FLLVMBuilder, VecVal.Handle, Index.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildExtractValue(AggVal: TLLVMValue;
  Index: Cardinal; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildExtractValue(FLLVMBuilder, AggVal.Handle, Index,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFAdd(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFAdd(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFCmp(Op: TLLVMRealPredicate; LHS,
  RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFCmp(FLLVMBuilder, Op, LHS.Handle, RHS.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFDiv(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFDiv(FLLVMBuilder, LHS.Handle, RHS.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFMul(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFMul(FLLVMBuilder, LHS.Handle, RHS.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFNeg(V: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFNeg(FLLVMBuilder, V.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFPCast(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFPCast(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFPExt(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFPExt(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFPToSI(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFPToSI(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFPToUI(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFPToUI(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFPTrunc(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFPTrunc(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFree(PointerVal: TLLVMValue): PLLVMValue;
begin
   Result := LLVMBuildFree(FLLVMBuilder, PointerVal.Handle);
end;

function TLLVMBuilder.BuildFRem(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFRem(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildFSub(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildFSub(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildGEP(Pointer: TLLVMValue;
  var Indices: PLLVMValuePtrArray; NumIndices: Cardinal;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildGEP(FLLVMBuilder, Pointer.Handle, Indices, NumIndices,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildGlobalString(Str: AnsiString;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildGlobalString(FLLVMBuilder, PAnsiChar(Str),
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildGlobalStringPtr(Str: AnsiString;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildGlobalStringPtr(FLLVMBuilder, PAnsiChar(Str), PAnsiChar(Name));
end;

function TLLVMBuilder.BuildICmp(Op: TLLVMIntPredicate; LHS,
  RHS: TLLVMValue; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildICmp(FLLVMBuilder, Op, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildInBoundsGEP(Pointer: TLLVMValue;
  var Indices: PLLVMValuePtrArray; NumIndices: Cardinal;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildInBoundsGEP(FLLVMBuilder, Pointer.Handle, Indices,
      NumIndices, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildIndirectBr(Addr: TLLVMValue;
  NumDests: Cardinal): PLLVMValue;
begin
   Result := LLVMBuildIndirectBr(FLLVMBuilder, Addr.Handle, NumDests);
end;

function TLLVMBuilder.BuildInsertElement(VecVal, EltVal, Index: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildInsertElement(FLLVMBuilder, VecVal.Handle, EltVal.Handle,
      Index.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildInsertValue(AggVal, EltVal: TLLVMValue;
  Index: Cardinal; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildInsertValue(FLLVMBuilder, AggVal.Handle, EltVal.Handle,
      Index, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildIntCast(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildIntCast(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildIntToPtr(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildIntToPtr(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildInvoke(Fn: TLLVMValue;
  var Args: PLLVMValuePtrArray; NumArgs: Cardinal; &Then, Catch: PLLVMBasicBlock;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildInvoke(FLLVMBuilder, Fn.Handle, Args, NumArgs, &Then,
      Catch, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildIsNotNull(Val: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildIsNotNull(FLLVMBuilder, Val.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildIsNull(Val: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildIsNull(FLLVMBuilder, Val.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildLandingPad(Ty: TLLVMBaseType; PersFn: TLLVMValue;
  NumClauses: Cardinal; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildLandingPad(FLLVMBuilder, Ty.Handle, PersFn.Handle,
      NumClauses, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildLoad(PointerVal: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildLoad(FLLVMBuilder, PointerVal.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildLShr(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildLShr(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildMalloc(Ty: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildMalloc(FLLVMBuilder, Ty.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildMul(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildMul(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNeg(V: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNeg(FLLVMBuilder, V.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNot(V: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNot(FLLVMBuilder, V.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNSWAdd(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNSWAdd(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNSWMul(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNSWMul(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNSWNeg(V: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNSWNeg(FLLVMBuilder, V.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNSWSub(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNSWSub(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNUWAdd(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNUWAdd(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNUWMul(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNUWMul(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNUWNeg(V: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNUWNeg(FLLVMBuilder, V.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildNUWSub(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildNUWSub(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildOr(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildOr(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildPhi(Ty: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildPhi(FLLVMBuilder, Ty.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildPointerCast(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildPointerCast(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildPtrDiff(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildPtrDiff(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildPtrToInt(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildPtrToInt(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildResume(Exn: TLLVMValue): PLLVMValue;
begin
   Result := LLVMBuildResume(FLLVMBuilder, Exn.Handle);
end;

function TLLVMBuilder.BuildRet(V: TLLVMValue): PLLVMValue;
begin
   Result := LLVMBuildRet(FLLVMBuilder, V.Handle);
end;

function TLLVMBuilder.BuildRetVoid: PLLVMValue;
begin
   Result := LLVMBuildRetVoid(FLLVMBuilder);
end;

function TLLVMBuilder.BuildSDiv(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSDiv(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSelect(&If, &Then, &Else: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSelect(&FLLVMBuilder, &If.Handle, &Then.Handle,
      &Else.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSExt(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSExt(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSExtOrBitCast(Val: TLLVMValue;
  DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSExtOrBitCast(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildShl(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildShl(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildShuffleVector(V1, V2, Mask: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildShuffleVector(FLLVMBuilder, V1.Handle, V2.Handle,
      Mask.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSIToFP(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSIToFP(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSRem(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSRem(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildStore(Val, Ptr: TLLVMValue): PLLVMValue;
begin
   Result := LLVMBuildStore(FLLVMBuilder, Val.Handle, Ptr.Handle);
end;

function TLLVMBuilder.BuildStructGEP(Pointer: TLLVMValue; Idx: Cardinal;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildStructGEP(FLLVMBuilder, Pointer.Handle, Idx, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSub(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildSub(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildSwitch(V: TLLVMValue; &Else: PLLVMBasicBlock;
  NumCases: Cardinal): PLLVMValue;
begin
   Result := LLVMBuildSwitch(FLLVMBuilder, V.Handle, &Else, NumCases);
end;

function TLLVMBuilder.BuildTrunc(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildTrunc(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildTruncOrBitCast(Val: TLLVMValue;
  DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildTruncOrBitCast(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildUDiv(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildUDiv(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildUIToFP(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildUIToFP(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

function TLLVMBuilder.BuildUnreachable: PLLVMValue;
begin
   Result := LLVMBuildUnreachable(FLLVMBuilder);
end;

function TLLVMBuilder.BuildURem(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildURem(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildVAArg(List: TLLVMValue; Ty: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildVAArg(FLLVMBuilder, List.Handle, Ty.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildXor(LHS, RHS: TLLVMValue;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildXor(FLLVMBuilder, LHS.Handle, RHS.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildZExt(Val: TLLVMValue; DestTy: TLLVMBaseType;
  const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildZExt(FLLVMBuilder, Val.Handle, DestTy.Handle, PAnsiChar(Name));
end;

function TLLVMBuilder.BuildZExtOrBitCast(Val: TLLVMValue;
  DestTy: TLLVMBaseType; const Name: AnsiString): PLLVMValue;
begin
   Result := LLVMBuildZExtOrBitCast(FLLVMBuilder, Val.Handle, DestTy.Handle,
      PAnsiChar(Name));
end;

procedure TLLVMBuilder.ClearInsertionPosition;
begin
   LLVMClearInsertionPosition(FLLVMBuilder);
end;

function TLLVMBuilder.GetCurrentDebugLocation: PLLVMValue;
begin
   Result := LLVMGetCurrentDebugLocation(FLLVMBuilder);
end;

function TLLVMBuilder.GetInsertBlock: PLLVMBasicBlock;
begin
   Result := LLVMGetInsertBlock(FLLVMBuilder)
end;

procedure TLLVMBuilder.InsertIntoBuilder(Instr: TLLVMValue);
begin
   LLVMInsertIntoBuilder(FLLVMBuilder, Instr.Handle);
end;

procedure TLLVMBuilder.InsertIntoBuilderWithName(Instr: TLLVMValue;
  const Name: AnsiString);
begin
   LLVMInsertIntoBuilderWithName(FLLVMBuilder, Instr.Handle, PAnsiChar(Name));
end;

procedure TLLVMBuilder.PositionBuilder(Block: PLLVMBasicBlock;
  Instr: TLLVMValue);
begin
   LLVMPositionBuilder(FLLVMBuilder, Block, Instr.Handle);
end;

procedure TLLVMBuilder.PositionBuilderAtEnd(Block: PLLVMBasicBlock);
begin
   LLVMPositionBuilderAtEnd(FLLVMBuilder, Block);
end;

procedure TLLVMBuilder.PositionBuilderBefore(Instr: TLLVMValue);
begin
   LLVMPositionBuilderBefore(FLLVMBuilder, Instr.Handle);
end;

procedure TLLVMBuilder.SetInstDebugLocation(Instr: TLLVMValue);
begin
   LLVMSetInstDebugLocation(FLLVMBuilder, Instr.Handle);
end;


{ TLLVMMemoryBuffer }

constructor TLLVMMemoryBuffer.Create(Path: AnsiString);
var
   OutMessage: PAnsiChar;
begin
   FLLVMMemoryBuffer := nil;

   if not LLVMCreateMemoryBufferWithContentsOfFile(PAnsiChar(Path),
      FLLVMMemoryBuffer, OutMessage) and Assigned(OutMessage) then
      raise Exception.Create(string(AnsiString(OutMessage)));
end;

constructor TLLVMMemoryBuffer.Create;
var
   OutMessage: PAnsiChar;
begin
   FLLVMMemoryBuffer := nil;

   if not LLVMCreateMemoryBufferWithSTDIN(FLLVMMemoryBuffer, OutMessage) then
      raise Exception.Create(string(AnsiString(OutMessage)));
end;

destructor TLLVMMemoryBuffer.Destroy;
begin
   if Assigned(FLLVMMemoryBuffer) then
      LLVMDisposeMemoryBuffer(FLLVMMemoryBuffer);

   inherited;
end;


{ TLLVMModuleProvider }

constructor TLLVMModuleProvider.Create(Module: TLLVMModule);
begin
   FLLVMModuleProvider := LLVMCreateModuleProviderForExistingModule(Module.Handle);
end;

destructor TLLVMModuleProvider.Destroy;
begin
   LLVMDisposeModuleProvider(FLLVMModuleProvider);
   inherited;
end;


{ TLLVMPassManagerBase }

destructor TLLVMPassManagerBase.Destroy;
begin
  LLVMDisposePassManager(FLLVMPassManager);
  inherited;
end;

procedure TLLVMPassManagerBase.AddTargetData(TargetData: TLLVMTargetData);
begin
   LLVMAddTargetData(TargetData.Handle, FLLVMPassManager);
end;

procedure TLLVMPassManagerBase.AddTargetLibraryInfo(
   TargetLibraryInfo: PLLVMTargetLibraryInfo);
begin
   LLVMAddTargetLibraryInfo(TargetLibraryInfo, FLLVMPassManager);
end;


{ TLLVMPassManager }

constructor TLLVMPassManager.Create;
begin
   FLLVMPassManager := LLVMCreatePassManager;
end;

function TLLVMPassManager.RunPassManager(Module: TLLVMModule): Boolean;
begin
   Result := LLVMRunPassManager(FLLVMPassManager, Module.Handle);
end;


{ TLLVMFunctionPassManager }

constructor TLLVMFunctionPassManager.Create(ModuleProvider: TLLVMModuleProvider);
begin
   FLLVMPassManager := LLVMCreateFunctionPassManager(ModuleProvider.Handle);
end;

constructor TLLVMFunctionPassManager.Create(Module: TLLVMModule);
begin
   FLLVMPassManager := LLVMCreateFunctionPassManagerForModule(Module.Handle);
end;

function TLLVMFunctionPassManager.FinalizeFunctionPassManager: Boolean;
begin
   Result := LLVMFinalizeFunctionPassManager(FLLVMPassManager);
end;

function TLLVMFunctionPassManager.InitializeFunctionPassManager: Boolean;
begin
   Result :=  LLVMInitializeFunctionPassManager(FLLVMPassManager);
end;

function TLLVMFunctionPassManager.RunFunctionPassManager(
  Value: TLLVMValue): Boolean;
begin
   Result := LLVMRunFunctionPassManager(FLLVMPassManager, Value.Handle);
end;


{ TLLVMPassManagerBuilder }

constructor TLLVMPassManagerBuilder.Create;
begin
   FLLVMPassManagerBuilder := LLVMPassManagerBuilderCreate;
end;

destructor TLLVMPassManagerBuilder.Destroy;
begin
  LLVMPassManagerBuilderDispose(FLLVMPassManagerBuilder);
  inherited;
end;

procedure TLLVMPassManagerBuilder.PopulateFunctionPassManager(
  PM: TLLVMFunctionPassManager);
begin
   LLVMPassManagerBuilderPopulateFunctionPassManager(FLLVMPassManagerBuilder, PM.Handle);
end;

procedure TLLVMPassManagerBuilder.PopulateModulePassManager(
  PM: TLLVMPassManager);
begin
   LLVMPassManagerBuilderPopulateModulePassManager(FLLVMPassManagerBuilder, PM.Handle);
end;

procedure TLLVMPassManagerBuilder.SetDisableSimplifyLibCalls(Value: Boolean);
begin
   LLVMPassManagerBuilderSetDisableSimplifyLibCalls(FLLVMPassManagerBuilder, Value);
end;

procedure TLLVMPassManagerBuilder.SetDisableUnitAtATime(Value: Boolean);
begin
   LLVMPassManagerBuilderSetDisableUnitAtATime(FLLVMPassManagerBuilder, Value);
end;

procedure TLLVMPassManagerBuilder.SetDisableUnrollLoops(Value: Boolean);
begin
   LLVMPassManagerBuilderSetDisableUnrollLoops(FLLVMPassManagerBuilder, Value);
end;

procedure TLLVMPassManagerBuilder.SetOptLevel(OptLevel: TLLVMCodeGenOptLevel);
begin
   LLVMPassManagerBuilderSetOptLevel(FLLVMPassManagerBuilder, OptLevel);
end;

procedure TLLVMPassManagerBuilder.SetSizeLevel(SizeLevel: Cardinal);
begin
   LLVMPassManagerBuilderSetSizeLevel(FLLVMPassManagerBuilder, SizeLevel);
end;

procedure TLLVMPassManagerBuilder.UseInlinerWithThreshold(Threshold: Cardinal);
begin
   LLVMPassManagerBuilderUseInlinerWithThreshold(FLLVMPassManagerBuilder, Threshold);
end;


{ TLLVMTargetData }

constructor TLLVMTargetData.Create(StringRep: AnsiString);
begin
   FLLVMTargetData := LLVMCreateTargetData(PAnsiChar(StringRep));
end;

destructor TLLVMTargetData.Destroy;
begin
   LLVMDisposeTargetData(FLLVMTargetData);
   inherited;
end;

function TLLVMTargetData.ABIAlignmentOfType(&Type: TLLVMBaseType): Cardinal;
begin
   Result := LLVMABIAlignmentOfType(FLLVMTargetData, &Type.Handle);
end;

function TLLVMTargetData.ABISizeOfType(&Type: TLLVMBaseType): UInt64;
begin
   Result := LLVMABISizeOfType(FLLVMTargetData, &Type.Handle);
end;

function TLLVMTargetData.CallFrameAlignmentOfType(
  &Type: TLLVMBaseType): Cardinal;
begin
   Result := LLVMCallFrameAlignmentOfType(FLLVMTargetData, &Type.Handle);
end;

function TLLVMTargetData.ElementAtOffset(StructType: TLLVMStructType;
  Offset: UInt64): Cardinal;
begin
   Result := LLVMElementAtOffset(FLLVMTargetData, StructType.Handle,
      Offset);
end;

function TLLVMTargetData.CopyStringRepOfTargetData: AnsiString;
begin
   Result := LLVMCopyStringRepOfTargetData(FLLVMTargetData);
end;

function TLLVMTargetData.GetByteOrder: TLLVMByteOrdering;
begin
   Result := LLVMByteOrder(FLLVMTargetData);
end;

function TLLVMTargetData.IntPtrType: PLLVMType;
begin
   Result := LLVMIntPtrType(FLLVMTargetData);
end;

function TLLVMTargetData.IntPtrTypeForAS(AddressSpace: Cardinal): PLLVMType;
begin
   Result := LLVMIntPtrTypeForAS(FLLVMTargetData, AddressSpace);
end;

function TLLVMTargetData.OffsetOfElement(StructType: TLLVMStructType;
  Element: Cardinal): UInt64;
begin
   Result := LLVMOffsetOfElement(FLLVMTargetData, StructType.Handle,
      Element);
end;

function TLLVMTargetData.GetPointerSize: Cardinal;
begin
   Result := LLVMPointerSize(FLLVMTargetData);
end;

function TLLVMTargetData.PointerSizeForAS(AddressSpace: Cardinal): Cardinal;
begin
   Result := LLVMPointerSizeForAS(FLLVMTargetData, AddressSpace);
end;

function TLLVMTargetData.PreferredAlignmentOfGlobal(
  GlobalVar: TLLVMValue): Cardinal;
begin
   Result := LLVMPreferredAlignmentOfGlobal(FLLVMTargetData, GlobalVar.Handle);
end;

function TLLVMTargetData.PreferredAlignmentOfType(
  &Type: TLLVMBaseType): Cardinal;
begin
   Result := LLVMPreferredAlignmentOfType(FLLVMTargetData, &Type.Handle);
end;

function TLLVMTargetData.SizeOfTypeInBits(&Type: TLLVMBaseType): UInt64;
begin
   Result := LLVMSizeOfTypeInBits(FLLVMTargetData, &Type.Handle);
end;

function TLLVMTargetData.StoreSizeOfType(&Type: TLLVMBaseType): UInt64;
begin
   Result := LLVMStoreSizeOfType(FLLVMTargetData, &Type.Handle);
end;


{ TLLVMObjectFile }

constructor TLLVMObjectFile.Create(MemBuf: TLLVMMemoryBuffer);
begin
   FLLVMObjectFile := LLVMCreateObjectFile(MemBuf.Handle)
end;

destructor TLLVMObjectFile.Destroy;
begin
  LLVMDisposeObjectFile(FLLVMObjectFile);
  inherited;
end;

function TLLVMObjectFile.IsSectionIteratorAtEnd(
  SectionIterator: TLLVMSectionIterator): Boolean;
begin
   Result := LLVMIsSectionIteratorAtEnd(FLLVMObjectFile, SectionIterator.Handle);
end;

function TLLVMObjectFile.IsSymbolIteratorAtEnd(
  SymbolIterator: TLLVMSymbolIterator): Boolean;
begin
   Result := LLVMIsSymbolIteratorAtEnd(FLLVMObjectFile, SymbolIterator.Handle);
end;


{ TLLVMSectionIterator }

constructor TLLVMSectionIterator.Create(ObjectFile: TLLVMObjectFile);
begin
   FLLVMSectionIterator := LLVMGetSections(ObjectFile.Handle);
end;

destructor TLLVMSectionIterator.Destroy;
begin
   LLVMDisposeSectionIterator(FLLVMSectionIterator);
   inherited;
end;

function TLLVMSectionIterator.GetSectionAddress: UInt64;
begin
   Result := LLVMGetSectionAddress(FLLVMSectionIterator);
end;

function TLLVMSectionIterator.GetSectionContainsSymbol(
  Sym: TLLVMSymbolIterator): Boolean;
begin
   Result := LLVMGetSectionContainsSymbol(FLLVMSectionIterator, Sym.Handle);
end;

function TLLVMSectionIterator.GetSectionContents: AnsiString;
begin
   Result := LLVMGetSectionContents(FLLVMSectionIterator);
end;

function TLLVMSectionIterator.GetSectionName: AnsiString;
begin
   Result := LLVMGetSectionName(FLLVMSectionIterator);
end;

function TLLVMSectionIterator.GetSectionSize: UInt64;
begin
   Result := LLVMGetSectionSize(FLLVMSectionIterator);
end;

function TLLVMSectionIterator.IsRelocationIteratorAtEnd(
  RI: TLLVMRelocationIterator): Boolean;
begin
   Result := LLVMIsRelocationIteratorAtEnd(FLLVMSectionIterator, RI.Handle);
end;

procedure TLLVMSectionIterator.MoveToContainingSection(
  Sym: TLLVMSymbolIterator);
begin
   LLVMMoveToContainingSection(FLLVMSectionIterator, Sym.Handle);
end;

procedure TLLVMSectionIterator.MoveToNextSection;
begin
   LLVMMoveToNextSection(FLLVMSectionIterator);
end;


{ TLLVMSymbolIterator }

constructor TLLVMSymbolIterator.Create(ObjectFile: TLLVMObjectFile);
begin
   FLLVMSymbolIterator := LLVMGetSymbols(ObjectFile.Handle);
end;

destructor TLLVMSymbolIterator.Destroy;
begin
   LLVMDisposeSymbolIterator(FLLVMSymbolIterator);
   inherited;
end;

function TLLVMSymbolIterator.GetSymbolAddress: UInt64;
begin
   Result := LLVMGetSymbolAddress(FLLVMSymbolIterator);
end;

function TLLVMSymbolIterator.GetSymbolFileOffset: UInt64;
begin
   Result := LLVMGetSymbolFileOffset(FLLVMSymbolIterator);
end;

function TLLVMSymbolIterator.GetSymbolName: PAnsiChar;
begin
   Result := LLVMGetSymbolName(FLLVMSymbolIterator);
end;

function TLLVMSymbolIterator.GetSymbolSize: UInt64;
begin
   Result := LLVMGetSymbolSize(FLLVMSymbolIterator);
end;

procedure TLLVMSymbolIterator.MoveToNextSection;
begin
   LLVMMoveToNextSymbol(FLLVMSymbolIterator);
end;


{ TLLVMRelocationIterator }

constructor TLLVMRelocationIterator.Create(
   SectionIterator: TLLVMSectionIterator);
begin
   FLLVMRelocationIterator := LLVMGetRelocations(SectionIterator.Handle);
end;

destructor TLLVMRelocationIterator.Destroy;
begin
   LLVMDisposeRelocationIterator(FLLVMRelocationIterator);
   inherited;
end;

function TLLVMRelocationIterator.GetRelocationAddress: UInt64;
begin
   Result := LLVMGetRelocationAddress(FLLVMRelocationIterator);
end;

function TLLVMRelocationIterator.GetRelocationOffset: UInt64;
begin
   Result := LLVMGetRelocationOffset(FLLVMRelocationIterator);
end;

function TLLVMRelocationIterator.GetRelocationType: UInt64;
begin
   Result := LLVMGetRelocationType(FLLVMRelocationIterator);
end;

function TLLVMRelocationIterator.GetRelocationTypeName: AnsiString;
var
   RelocationTypeName: PAnsiChar;
begin
   RelocationTypeName := LLVMGetRelocationTypeName(FLLVMRelocationIterator);
   Result := RelocationTypeName;

   StrDispose(RelocationTypeName);
end;

function TLLVMRelocationIterator.GetRelocationValueString: AnsiString;
var
   RelocationValueString: PAnsiChar;
begin
   RelocationValueString := LLVMGetRelocationValueString(FLLVMRelocationIterator);
   Result := RelocationValueString;

   StrDispose(RelocationValueString);
end;

procedure TLLVMRelocationIterator.MoveToNextRelocation;
begin
   LLVMMoveToNextRelocation(FLLVMRelocationIterator);
end;


{ TLTOModule }

constructor TLTOModule.Create(Path: AnsiString);
begin
   FLTOModule := LTOModuleCreate(PAnsiChar(Path));
end;

constructor TLTOModule.Create(Mem: Pointer; Length: NativeUInt);
begin
   FLTOModule := LTOModuleCreateFromMemory(Mem, Length);
end;

destructor TLTOModule.Destroy;
begin
   LTOModuleDispose(FLTOModule);
   inherited;
end;

function TLTOModule.GetNumSymbols: Integer;
begin
   Result := LTOModuleGetNumSymbols(FLTOModule);
end;

function TLTOModule.GetSymbolAttribute(Index: Integer): TLTOSymbolAttributes;
begin
   Result := LTOModuleGetSymbolAttribute(FLTOModule, Index);
end;

function TLTOModule.GetSymbolName(Index: Integer): AnsiString;
begin
   Result := LTOModuleGetSymbolName(FLTOModule, Index);
end;

function TLTOModule.GetTargetTriple: AnsiString;
begin
   Result := LTOModuleGetTargetTriple(FLTOModule);
end;

procedure TLTOModule.SetTargetTriple(const Value: AnsiString);
begin
  LTOModuleSetTargetTriple(FLTOModule, PAnsiChar(Value));
end;


{ TLTOCodeGenerator }

constructor TLTOCodeGenerator.Create;
begin
   FLTOCodeGenerator := LTOCodegenCreate;
end;

destructor TLTOCodeGenerator.Destroy;
begin
   LTOCodegenDispose(FLTOCodeGenerator);
end;

procedure TLTOCodeGenerator.AddMustPreserveSymbol(Symbol: AnsiString);
begin
   LTOCodegenAddMustPreserveSymbol(FLTOCodeGenerator, PAnsiChar(Symbol));
end;

function TLTOCodeGenerator.Compile(var Length: NativeUInt): Pointer;
begin
   Result := LTOCodegenCompile(FLTOCodeGenerator, @Length);
end;

function TLTOCodeGenerator.CompileToFile(out name: AnsiString): Boolean;
var
   ObjectFileName: PAnsiChar;
begin
   Result := LTOCodegenCompileToFile(FLTOCodeGenerator, ObjectFileName);
   Name := ObjectFileName;
end;

procedure TLTOCodeGenerator.SetAssemblerPath(const Value: AnsiString);
begin
   LTOCodegenSetAssemblerPath(FLTOCodeGenerator, PAnsiChar(Value));
end;

procedure TLTOCodeGenerator.SetCodegenModel(const Value: TLTOCodegenModel);
begin
   LTOCodegenSetPicModel(FLTOCodeGenerator, Value);
end;

procedure TLTOCodeGenerator.SetCPU(const Value: AnsiString);
begin
   LTOCodegenSetCpu(FLTOCodeGenerator, PAnsiChar(Value));
end;

procedure TLTOCodeGenerator.SetDebugModel(const Value: TLTODebugModel);
begin
  LTOCodegenSetDebugModel(FLTOCodeGenerator, Value);
end;

procedure TLTOCodeGenerator.SetDebugOptions(const Value: AnsiString);
begin
   LTOCodegenDebugOptions(FLTOCodeGenerator, PAnsiChar(Value));
end;

function TLTOCodeGenerator.WriteMergedModules(Path: AnsiString): Boolean;
begin
   Result := LTOCodegenWriteMergedModules(FLTOCodeGenerator, PAnsiChar(Path));
end;

procedure TLTOCodeGenerator.AddModule(Module: TLTOModule);
begin
   LTOCodegenAddModule(FLTOCodeGenerator, Module.Handle);
end;

end.
