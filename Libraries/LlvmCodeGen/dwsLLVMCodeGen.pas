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
unit dwsLLVMCodeGen;

interface

{-$DEFINE UseExplicitContext} // <-- this is yet buggy!

uses
   Classes, SysUtils, Variants, Math, Contnrs,
   dwsUtils, dwsSymbols, dwsCodeGen, dwsCoreExprs,
   dwsDataContext, dwsExprs, dwsRelExprs, dwsMagicExprs, dwsStrings,
   dwsMethodExprs, dwsConnectorExprs, dwsConvExprs, dwsFunctions,
   dwsGlobalVarsFunctions, dwsErrors, dwsRTTIFunctions, dwsConstExprs,
   dwsLLVM, dwsLLVMClasses;

type
   {$IFNDEF LLVM_TAG}
   TdwsExpressionLLVMValuePair = record
      Expression: TExprBase;
      LLVMValue: PLLVMValue;
   end;

   TdwsExpressionLLVMValueDictionary = class
      private
         FPairs: TSimpleList<TdwsExpressionLLVMValuePair>;
         function FindPairIndexForObject(Expression: TExprBase): Integer;
         function FindPairIndexForLLVMValue(LLVMValue: PLLVMValue): Integer;
         function GetCount: Cardinal;
      public
         constructor Create;
         destructor Destroy; override;

         procedure LinkLLVMValueToObject(LLVMValue: PLLVMValue; Expression: TExprBase);
         procedure RemoveObject(Expression: TExprBase);
         procedure RemoveLLVMValue(LLVMValue: PLLVMValue);
         procedure Clear;

         function GetLLVMValue(Expression: TExprBase): PLLVMValue;

         function FindPairForObject(Expression: TExprBase): PLLVMValue;
         function FindPairForLLVMValue(LLVMValue: PLLVMValue): TExprBase;

         property Count: Cardinal read GetCount;
   end;

   TdwsDataSymbolLLVMValuePair = record
      DataSymbol: TDataSymbol;
      LLVMValue: PLLVMValue;
   end;

   TdwsDataSymbolLLVMValueDictionary = class
      private
         FPairs: TSimpleList<TdwsDataSymbolLLVMValuePair>;
         function FindPairIndexForDataSymbol(DataSymbol: TDataSymbol): Integer;
         function FindPairIndexForLLVMValue(LLVMValue: PLLVMValue): Integer;
         function GetCount: Cardinal;
      public
         constructor Create;
         destructor Destroy; override;

         procedure LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue; DataSymbol: TDataSymbol);
         procedure RemoveDataSymbol(DataSymbol: TDataSymbol);
         procedure RemoveLLVMValue(LLVMValue: PLLVMValue);
         procedure Clear;

         function GetLLVMValue(DataSymbol: TDataSymbol): PLLVMValue;

         function FindPairForDataSymbol(DataSymbol: TDataSymbol): PLLVMValue;
         function FindPairForLLVMValue(LLVMValue: PLLVMValue): TDataSymbol;

         property Count: Cardinal read GetCount;
   end;

   TdwsFunctionSymbolLLVMValuePair = record
      FunctionSymbol: TFuncSymbol;
      LLVMValue: PLLVMValue;
   end;

   TdwsFunctionSymbolLLVMValueDictionary = class
      private
         FPairs: TSimpleList<TdwsFunctionSymbolLLVMValuePair>;
         function FindPairIndexForFunctionSymbol(FunctionSymbol: TFuncSymbol): Integer;
         function FindPairIndexForLLVMValue(LLVMValue: PLLVMValue): Integer;
         function GetCount: Cardinal;
      public
         constructor Create;
         destructor Destroy; override;

         procedure LinkLLVMValueToFunctionSymbol(LLVMValue: PLLVMValue; FunctionSymbol: TFuncSymbol);
         procedure RemoveFunctionSymbol(FunctionSymbol: TFuncSymbol);
         procedure RemoveLLVMValue(LLVMValue: PLLVMValue);
         procedure Clear;

         function GetLLVMValue(FunctionSymbol: TFuncSymbol): PLLVMValue;

         function FindPairForFunctionSymbol(FunctionSymbol: TFuncSymbol): PLLVMValue;
         function FindPairForLLVMValue(LLVMValue: PLLVMValue): TFuncSymbol;

         property Count: Cardinal read GetCount;
   end;

   TdwsTypeSymbolLLVMTypePair = record
      TypeSymbol: TTypeSymbol;
      LLVMType: PLLVMType;
   end;

   TdwsTypeSymbolLLVMTypeDictionary = class
      private
         FPairs: TSimpleList<TdwsTypeSymbolLLVMTypePair>;
         function FindPairIndexForTypeSymbol(TypeSymbol: TTypeSymbol): Integer;
         function FindPairIndexForLLVMType(LLVMType: PLLVMType): Integer;
         function GetCount: Cardinal;
      public
         constructor Create;
         destructor Destroy; override;

         procedure LinkLLVMTypeToTypeSymbol(LLVMType: PLLVMType; TypeSymbol: TTypeSymbol);
         procedure RemoveTypeSymbol(TypeSymbol: TTypeSymbol);
         procedure RemoveLLVMType(LLVMType: PLLVMType);
         procedure Clear;

         function GetLLVMType(TypeSymbol: TTypeSymbol): PLLVMType;

         function FindPairForTypeSymbol(TypeSymbol: TTypeSymbol): PLLVMType;
         function FindPairForLLVMType(LLVMType: PLLVMType): TTypeSymbol;

         property Count: Cardinal read GetCount;
   end;
   {$ENDIF}

   TdwsLLVMBasicType = (btBool, btInt, btFloat);

   TdwsLLVMOptimizations = (loNone, loLess, loDefault, loAggressive, loCustom);

   TdwsLLVMCustomPass = (
      cpAggressiveDCEPass,
      cpCFGSimplificationPass,
      cpDeadStoreEliminationPass,
      cpGVNPass,
      cpIndVarSimplifyPass,
      cpInstructionCombiningPass,
      cpJumpThreadingPass,
      cpLICMPass,
      cpLoopDeletionPass,
      cpLoopIdiomPass,
      cpLoopRotatePass,
      cpLoopUnrollPass,
      cpLoopUnswitchPass,
      cpMemCpyOptPass,
      cpPromoteMemoryToRegisterPass,
      cpReassociatePass,
      cpSCCPPass,
      cpScalarReplAggregatesPass,
      cpScalarReplAggregatesPassSSA,
      cpSimplifyLibCallsPass,
      cpTailCallEliminationPass,
      cpConstantPropagationPass,
      cpDemoteMemoryToRegisterPass,
      cpVerifierPass,
      cpCorrelatedValuePropagationPass,
      cpEarlyCSEPass,
      cpLowerExpectIntrinsicPass,
      cpTypeBasedAliasAnalysisPass,
      cpBasicAliasAnalysisPass,
      cpBBVectorizePass,
      cpLoopVectorizePass,
      cpArgumentPromotionPass,
      cpConstantMergePass,
      cpDeadArgEliminationPass,
      cpFunctionAttrsPass,
      cpFunctionInliningPass,
      cpAlwaysInlinerPass,
      cpGlobalDCEPass,
      cpGlobalOptimizerPass,
      cpIPConstantPropagationPass,
      cpPruneEHPass,
      cpIPSCCPPass,
      cpStripDeadPrototypesPass,
      cpStripSymbolsPass
   );
   TdwsLLVMCustomPasses = set of TdwsLLVMCustomPass;

   TdwsLLVMCodeGen = class (TdwsCodeGen)
      private
         FModuleName: AnsiString;
         FContext: TLLVMContext;
         FModule: TLLVMModule;
         FBuilder: TLLVMBuilder;

         FBoolType: TLLVMInt1Type;
         FIntType: TLLVMInt64Type;
         FFloatType: TLLVMDoubleType;
         FTargetData: TLLVMTargetData;
         FAggregateLevel: Integer;

         {$IFNDEF LLVM_TAG}
         FdwsLLVMDict: TdwsExpressionLLVMValueDictionary;
         FdwsLLVMDataSymbolDict: TdwsDataSymbolLLVMValueDictionary;
         FdwsLLVMFuncSymbolDict: TdwsFunctionSymbolLLVMValueDictionary;
         FdwsLLVMTypeSymbolDict: TdwsTypeSymbolLLVMTypeDictionary;
         {$ENDIF}

         FVerifyModule: Boolean;
         FOptimizations: TdwsLLVMOptimizations;
         FCustomPasses: TdwsLLVMCustomPasses;

         function TypeSymbolToLLVMType(TypeSymbol: TTypeSymbol): PLLVMType;
      protected
         procedure DoCompileFuncSymbol(func: TFuncSymbol;
            deAnonymize: Boolean = False); override;
      public
         constructor Create; override;
         destructor Destroy; override;

         procedure BeginProgramSession(const prog: IdwsProgram); override;
         procedure EndProgramSession; override;

         procedure CompileProgramBody(expr: TProgramExpr); override;

         procedure Clear; override;

         procedure EmitToFile(FileName: AnsiString; CodeGenFileType: TLLVMCodeGenFileType);
         procedure PrintToFile(FileName: AnsiString);
         procedure SaveByteCodeToFile(FileName: AnsiString);
         procedure Dump;

         procedure WriteLiteralString(const s: string); override;
         procedure WriteFloat(const v: Double; const fmt: TFormatSettings); override;

         property Module: TLLVMModule read FModule;
         property ModuleName: AnsiString read FModuleName write FModuleName;

         property Optimizations: TdwsLLVMOptimizations read FOptimizations write FOptimizations default loNone;
         property CustomOptimizationPasses: TdwsLLVMCustomPasses read FCustomPasses write FCustomPasses;
         property VerifyModule: Boolean read FVerifyModule write FVerifyModule default False;
   end;

   TLLVMExprCodeGen = class (TdwsExprCodeGen);

   TLLVMBlockInitExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMBlockExprBase = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMBlockExpr = class (TLLVMBlockExprBase)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMNullExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMAssignExpr = class (TLLVMExprCodeGen)
      protected
         function GetLLVMValue(codeGen : TdwsLLVMCodeGen; e: TAssignExpr): PLLVMValue; virtual;
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMAssignConstVarExpr = class (TLLVMAssignExpr)
      protected
         FBasicType: TdwsLLVMBasicType;
         function GetLLVMValue(codeGen : TdwsLLVMCodeGen; e: TAssignExpr): PLLVMValue; override;
      public
         constructor Create(BasicType: TdwsLLVMBasicType);
   end;

   TLLVMConstExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMConstIntExpr  = class (TLLVMConstExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMConstFloatExpr = class (TLLVMConstExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMVarExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMRecordExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMStaticArrayExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMOpExpr = class (TLLVMExprCodeGen)
   end;

   TLLVMOpAssignExpr = class (TLLVMAssignExpr)
      private
         FOp: TLLVMOpcode;
      protected
         function GetLLVMValue(codeGen : TdwsLLVMCodeGen; e: TAssignExpr): PLLVMValue; override;
      public
         constructor Create(Op: TLLVMOpcode);
   end;

   TLLVMIncDecVarFuncExpr = class (TLLVMExprCodeGen)
      private
         FOp: TLLVMOpcode;
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
      public
         constructor Create(Op: TLLVMOpcode);
   end;

   TLLVMBinOpExpr = class (TLLVMOpExpr)
      protected
         FOp: TLLVMOpcode;
         FIsFloatOp: Boolean;
      public
         constructor Create(Op: TLLVMOpcode);
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMMultIntPow2Expr = class (TLLVMOpExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMNotExpr = class (TLLVMExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMUnaryIntFloatOpExpr = class (TLLVMExprCodeGen)
      protected
         FIsInteger: Boolean;
      public
         constructor Create(IsInteger: Boolean = True);
   end;

   TLLVMSqrExpr = class (TLLVMUnaryIntFloatOpExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TLLVMNegExpr = class (TLLVMUnaryIntFloatOpExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMAbsIntExpr = class (TLLVMExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMConvFloatExpr = class (TLLVMExprCodeGen)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMRelExpr = class(TdwsExprCodeGen);
   TLLVMRelIntZeroExpr = class(TLLVMRelExpr)
      private
         FIsZero: Boolean;
      public
         constructor Create(Value: Boolean);
         procedure CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr); override;
   end;
   TLLVMRelIntExpr = class(TLLVMRelExpr)
      private
         FPredicate: TLLVMIntPredicate;
      public
         constructor Create(Predicate: TLLVMIntPredicate);
         procedure CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr); override;
   end;
   TLLVMRelFloatExpr = class(TLLVMRelExpr)
      private
         FPredicate: TLLVMRealPredicate;
      public
         constructor Create(Predicate: TLLVMRealPredicate);
         procedure CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr); override;
   end;

   TLLVMIfThenElseValueExpr = class (TLLVMExprCodeGen)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMIfExpr = class (TLLVMExprCodeGen)
      procedure CodeGenCondition(codeGen : TdwsCodeGen; condExpr : TTypedExpr; out SwapBlocks: Boolean);
   end;
   TLLVMIfThenExpr = class (TLLVMIfExpr)
      function SubExprIsSafeStatement(sub : TExprBase) : Boolean;
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMIfThenElseExpr = class (TLLVMIfExpr)
      function SubExprIsSafeStatement(sub : TExprBase) : Boolean;
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMCaseExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMForExpr = class (TLLVMExprCodeGen)
      private
         FReverse: Boolean;
      public
         constructor Create(Reverse: Boolean);
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMLoopExpr = class (TLLVMExprCodeGen)
   end;
   TLLVMRepeatExpr = class (TLLVMLoopExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMWhileExpr = class (TLLVMLoopExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMExitExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMExitValueExpr = class (TLLVMExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMFuncBaseExpr = class (TLLVMExprCodeGen)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMMagicFuncExpr = class (TLLVMFuncBaseExpr)
      private
         FMagicCodeGens : TStringList;
      public
         constructor Create;
         destructor Destroy; override;
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMPrintLnExpr = class (TLLVMFuncBaseExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMBaseFloatFuncExpr = class (TLLVMFuncBaseExpr)
      private
         FIntrisicName: AnsiString;
      public
         constructor Create(IntrisicName: AnsiString);
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMBaseFloatIntFuncExpr = class (TLLVMBaseFloatFuncExpr)
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TLLVMAbsFloatExpr = class (TLLVMFuncBaseExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TLLVMTanFloatExpr = class (TLLVMFuncBaseExpr)
      procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;

   TLLVMPowFloatExpr = class (TLLVMFuncBaseExpr)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;
   TLLVMMinMaxIntExpr = class (TLLVMFuncBaseExpr)
      private
         FPredicate: TLLVMIntPredicate;
      public
         constructor Create(isMax: Boolean);
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;
   TLLVMMinMaxFloatExpr = class (TLLVMFuncBaseExpr)
      private
         FPredicate: TLLVMRealPredicate;
      public
         constructor Create(isMax: Boolean);
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   AnsiStrings;

resourcestring
   RStrNotSupported = 'Not supported';
   RStrFunctionAlreadyExists = 'Function already exists!';
   RStrNotImplemented = 'Not implemented yet!';
   RStrValueIsUndefined = 'Value is undefined: %s';
   RStrVarNotFound = 'Var not found at StackAddr %d';
   RStrWrongNumberOfArgs = 'Wrong number of arguments';

type
   TCodeGenHelper = class helper for TdwsCodeGen
      private
         function GetBoolType: TLLVMInt1Type; inline;
         function GetBuilder: TLLVMBuilder; inline;
         function GetContext: TLLVMContext; inline;
         function GetFloatType: TLLVMDoubleType; inline;
         function GetIntType: TLLVMInt64Type; inline;
         function GetModule: TLLVMModule; inline;
         function GetAggregateLevel: Integer; inline;
         procedure SetAggregateLevel(const Value: Integer); inline;
      public
         function TypeSymbolToLLVMType(TypeSymbol: TTypeSymbol): PLLVMType; inline;
         function DataSymbolToLLVMValue(DataSymbol: TDataSymbol): PLLVMValue; inline;
         function FunctionSymbolToLLVMValue(FuncSymbol: TFuncSymbol): PLLVMValue; inline;
         function ExpressionToLLVMValue(Expression: TExprBase): PLLVMValue; inline;
         procedure LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue; DataSymbol: TDataSymbol); inline;
         procedure LinkLLVMValueToFunctionSymbol(LLVMValue: PLLVMValue; FuncSymbol: TFuncSymbol); inline;
         procedure LinkLLVMValueToExpression(LLVMValue: PLLVMValue; Expression: TExprBase); inline;

         property Context: TLLVMContext read GetContext;
         property Module: TLLVMModule read GetModule;
         property Builder: TLLVMBuilder read GetBuilder;

         property AggregateLevel: Integer read GetAggregateLevel write SetAggregateLevel;
         property BoolType: TLLVMInt1Type read GetBoolType;
         property IntType: TLLVMInt64Type read GetIntType;
         property FloatType: TLLVMDoubleType read GetFloatType;
   end;


// ------------------
// ------------------ TdwsExpressionLLVMValueDictionary ------------------
// ------------------

function TCodeGenHelper.GetAggregateLevel: Integer;
begin
   Result := TdwsLLVMCodeGen(Self).FAggregateLevel;
end;

function TCodeGenHelper.GetBoolType: TLLVMInt1Type;
begin
   Result := TdwsLLVMCodeGen(Self).FBoolType;
end;

function TCodeGenHelper.GetBuilder: TLLVMBuilder;
begin
   Result := TdwsLLVMCodeGen(Self).FBuilder;
end;

function TCodeGenHelper.GetContext: TLLVMContext;
begin
   Result := TdwsLLVMCodeGen(Self).FContext;
end;

function TCodeGenHelper.GetFloatType: TLLVMDoubleType;
begin
   Result := TdwsLLVMCodeGen(Self).FFloatType;
end;

function TCodeGenHelper.GetIntType: TLLVMInt64Type;
begin
   Result := TdwsLLVMCodeGen(Self).FIntType;
end;

function TCodeGenHelper.GetModule: TLLVMModule;
begin
   Result := TdwsLLVMCodeGen(Self).FModule;
end;

procedure TCodeGenHelper.LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue;
  DataSymbol: TDataSymbol);
begin
{$IFDEF LLVM_TAG}
   DataSymbol.LLVMValue := LLVMValue;
{$ELSE}
   TdwsLLVMCodeGen(Self).FdwsLLVMDataSymbolDict.LinkLLVMValueToDataSymbol(LLVMValue,
      DataSymbol);
{$ENDIF}
end;

procedure TCodeGenHelper.LinkLLVMValueToFunctionSymbol(LLVMValue: PLLVMValue;
  FuncSymbol: TFuncSymbol);
begin
{$IFDEF LLVM_TAG}
   FuncSymbol.LLVMValue := LLVMValue;
{$ELSE}
   TdwsLLVMCodeGen(Self).FdwsLLVMFuncSymbolDict.LinkLLVMValueToFunctionSymbol(
      LLVMValue, FuncSymbol);
{$ENDIF}
end;

procedure TCodeGenHelper.SetAggregateLevel(const Value: Integer);
begin
   TdwsLLVMCodeGen(Self).FAggregateLevel := Value;
end;

procedure TCodeGenHelper.LinkLLVMValueToExpression(LLVMValue: PLLVMValue;
  Expression: TExprBase);
begin
{$IFDEF LLVM_TAG}
   Expression.LLVMValue := LLVMValue;
{$ELSE}
   TdwsLLVMCodeGen(Self).FdwsLLVMDict.LinkLLVMValueToObject(LLVMValue,
      Expression);
{$ENDIF}
end;

function TCodeGenHelper.DataSymbolToLLVMValue(DataSymbol: TDataSymbol): PLLVMValue;
begin
{$IFDEF LLVM_TAG}
   Result := Expression.LLVMValue;
{$ELSE}
   Result := TdwsLLVMCodeGen(Self).FdwsLLVMDataSymbolDict.GetLLVMValue(DataSymbol);
{$ENDIF}
end;

function TCodeGenHelper.FunctionSymbolToLLVMValue(FuncSymbol: TFuncSymbol): PLLVMValue;
begin
{$IFDEF LLVM_TAG}
   Result := Expression.LLVMValue;
{$ELSE}
   Result := TdwsLLVMCodeGen(Self).FdwsLLVMFuncSymbolDict.GetLLVMValue(FuncSymbol);
{$ENDIF}
end;

function TCodeGenHelper.ExpressionToLLVMValue(Expression: TExprBase): PLLVMValue;
begin
{$IFDEF LLVM_TAG}
   Result := Expression.LLVMValue;
{$ELSE}
   Result := TdwsLLVMCodeGen(Self).FdwsLLVMDict.GetLLVMValue(Expression);
{$ENDIF}
end;

function TCodeGenHelper.TypeSymbolToLLVMType(
  TypeSymbol: TTypeSymbol): PLLVMType;
begin
   Result := TdwsLLVMCodeGen(Self).TypeSymbolToLLVMType(TypeSymbol);
end;


{$IFNDEF LLVM_TAG}

// ------------------
// ------------------ TdwsExpressionLLVMValueDictionary ------------------
// ------------------

constructor TdwsExpressionLLVMValueDictionary.Create;
begin
   FPairs := TSimpleList<TdwsExpressionLLVMValuePair>.Create;
end;

destructor TdwsExpressionLLVMValueDictionary.Destroy;
begin
   FPairs.Free;
   inherited;
end;

procedure TdwsExpressionLLVMValueDictionary.Clear;
begin
   FPairs.Clear;
end;

function TdwsExpressionLLVMValueDictionary.FindPairForObject(
   Expression: TExprBase): PLLVMValue;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].Expression = Expression then
         Exit(FPairs[pairIndex].LLVMValue);
end;

function TdwsExpressionLLVMValueDictionary.FindPairForLLVMValue(
   LLVMValue: PLLVMValue): TExprBase;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMValue = LLVMValue then
         Exit(FPairs[pairIndex].Expression);
end;

function TdwsExpressionLLVMValueDictionary.FindPairIndexForObject(
  Expression: TExprBase): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].Expression = Expression then
         Exit(pairIndex);
end;

function TdwsExpressionLLVMValueDictionary.FindPairIndexForLLVMValue(
  LLVMValue: PLLVMValue): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMValue = LLVMValue then
         Exit(pairIndex);
end;

function TdwsExpressionLLVMValueDictionary.GetCount: Cardinal;
begin
   Result := FPairs.Count;
end;

function TdwsExpressionLLVMValueDictionary.GetLLVMValue(
  Expression: TExprBase): PLLVMValue;
var
   pairIndex : Integer;
begin
   Result := nil;
   pairIndex := FindPairIndexForObject(Expression);
   if pairIndex >= 0 then
      Result := FPairs.Items[pairIndex].LLVMValue;
end;

procedure TdwsExpressionLLVMValueDictionary.LinkLLVMValueToObject(LLVMValue: PLLVMValue;
   Expression: TExprBase);
var
   objIndex : Integer;
   pair : TdwsExpressionLLVMValuePair;
begin
   objIndex := FindPairIndexForObject(Expression);
   if objIndex >= 0 then
   begin
      pair := FPairs[objIndex];
      Assert(pair.Expression = Expression);
      pair.LLVMValue := LLVMValue;
      FPairs[objIndex] := pair;
      Exit;
   end;

   pair.Expression := Expression;
   pair.LLVMValue := LLVMValue;
   FPairs.Add(pair);
end;

procedure TdwsExpressionLLVMValueDictionary.RemoveObject(Expression: TExprBase);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForObject(Expression);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;

procedure TdwsExpressionLLVMValueDictionary.RemoveLLVMValue(
   LLVMValue: PLLVMValue);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForLLVMValue(LLVMValue);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;


// ------------------
// ------------------ TdwsDataSymbolLLVMValueDictionary ------------------
// ------------------

constructor TdwsDataSymbolLLVMValueDictionary.Create;
begin
   FPairs := TSimpleList<TdwsDataSymbolLLVMValuePair>.Create;
end;

destructor TdwsDataSymbolLLVMValueDictionary.Destroy;
begin
   FPairs.Free;
   inherited;
end;

procedure TdwsDataSymbolLLVMValueDictionary.Clear;
begin
   FPairs.Clear;
end;

function TdwsDataSymbolLLVMValueDictionary.FindPairForDataSymbol(
   DataSymbol: TDataSymbol): PLLVMValue;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].DataSymbol = DataSymbol then
         Exit(FPairs[pairIndex].LLVMValue);
end;

function TdwsDataSymbolLLVMValueDictionary.FindPairForLLVMValue(
   LLVMValue: PLLVMValue): TDataSymbol;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMValue = LLVMValue then
         Exit(FPairs[pairIndex].DataSymbol);
end;

function TdwsDataSymbolLLVMValueDictionary.FindPairIndexForDataSymbol(
  DataSymbol: TDataSymbol): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].DataSymbol = DataSymbol then
         Exit(pairIndex);
end;

function TdwsDataSymbolLLVMValueDictionary.FindPairIndexForLLVMValue(
  LLVMValue: PLLVMValue): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMValue = LLVMValue then
         Exit(pairIndex);
end;

function TdwsDataSymbolLLVMValueDictionary.GetCount: Cardinal;
begin
   Result := FPairs.Count;
end;

function TdwsDataSymbolLLVMValueDictionary.GetLLVMValue(
  DataSymbol: TDataSymbol): PLLVMValue;
var
   pairIndex : Integer;
begin
   Result := nil;
   pairIndex := FindPairIndexForDataSymbol(DataSymbol);
   if pairIndex >= 0 then
      Result := FPairs.Items[pairIndex].LLVMValue;
end;

procedure TdwsDataSymbolLLVMValueDictionary.LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue;
   DataSymbol: TDataSymbol);
var
   objIndex : Integer;
   pair : TdwsDataSymbolLLVMValuePair;
begin
   objIndex := FindPairIndexForDataSymbol(DataSymbol);
   if objIndex >= 0 then
   begin
      pair := FPairs[objIndex];
      Assert(pair.DataSymbol = DataSymbol);
      pair.LLVMValue := LLVMValue;
      FPairs[objIndex] := pair;
      Exit;
   end;

   pair.DataSymbol := DataSymbol;
   pair.LLVMValue := LLVMValue;
   FPairs.Add(pair);
end;

procedure TdwsDataSymbolLLVMValueDictionary.RemoveDataSymbol(DataSymbol: TDataSymbol);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForDataSymbol(DataSymbol);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;

procedure TdwsDataSymbolLLVMValueDictionary.RemoveLLVMValue(
   LLVMValue: PLLVMValue);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForLLVMValue(LLVMValue);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;


// ------------------
// ------------------ TdwsFunctionSymbolLLVMValueDictionary ------------------
// ------------------

constructor TdwsFunctionSymbolLLVMValueDictionary.Create;
begin
   FPairs := TSimpleList<TdwsFunctionSymbolLLVMValuePair>.Create;
end;

destructor TdwsFunctionSymbolLLVMValueDictionary.Destroy;
begin
   FPairs.Free;
   inherited;
end;

procedure TdwsFunctionSymbolLLVMValueDictionary.Clear;
begin
   FPairs.Clear;
end;

function TdwsFunctionSymbolLLVMValueDictionary.FindPairForFunctionSymbol(
   FunctionSymbol: TFuncSymbol): PLLVMValue;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].FunctionSymbol = FunctionSymbol then
         Exit(FPairs[pairIndex].LLVMValue);
end;

function TdwsFunctionSymbolLLVMValueDictionary.FindPairForLLVMValue(
   LLVMValue: PLLVMValue): TFuncSymbol;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMValue = LLVMValue then
         Exit(FPairs[pairIndex].FunctionSymbol);
end;

function TdwsFunctionSymbolLLVMValueDictionary.FindPairIndexForFunctionSymbol(
  FunctionSymbol: TFuncSymbol): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].FunctionSymbol = FunctionSymbol then
         Exit(pairIndex);
end;

function TdwsFunctionSymbolLLVMValueDictionary.FindPairIndexForLLVMValue(
  LLVMValue: PLLVMValue): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMValue = LLVMValue then
         Exit(pairIndex);
end;

function TdwsFunctionSymbolLLVMValueDictionary.GetCount: Cardinal;
begin
   Result := FPairs.Count;
end;

function TdwsFunctionSymbolLLVMValueDictionary.GetLLVMValue(
  FunctionSymbol: TFuncSymbol): PLLVMValue;
var
   pairIndex : Integer;
begin
   Result := nil;
   pairIndex := FindPairIndexForFunctionSymbol(FunctionSymbol);
   if pairIndex >= 0 then
      Result := FPairs.Items[pairIndex].LLVMValue;
end;

procedure TdwsFunctionSymbolLLVMValueDictionary.LinkLLVMValueToFunctionSymbol(LLVMValue: PLLVMValue;
   FunctionSymbol: TFuncSymbol);
var
   objIndex : Integer;
   pair : TdwsFunctionSymbolLLVMValuePair;
begin
   objIndex := FindPairIndexForFunctionSymbol(FunctionSymbol);
   if objIndex >= 0 then
   begin
      pair := FPairs[objIndex];
      Assert(pair.FunctionSymbol = FunctionSymbol);
      pair.LLVMValue := LLVMValue;
      FPairs[objIndex] := pair;
      Exit;
   end;

   pair.FunctionSymbol := FunctionSymbol;
   pair.LLVMValue := LLVMValue;
   FPairs.Add(pair);
end;

procedure TdwsFunctionSymbolLLVMValueDictionary.RemoveFunctionSymbol(FunctionSymbol: TFuncSymbol);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForFunctionSymbol(FunctionSymbol);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;

procedure TdwsFunctionSymbolLLVMValueDictionary.RemoveLLVMValue(
   LLVMValue: PLLVMValue);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForLLVMValue(LLVMValue);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;


// ------------------
// ------------------ TdwsTypeSymbolLLVMTypeDictionary ------------------
// ------------------

constructor TdwsTypeSymbolLLVMTypeDictionary.Create;
begin
   FPairs := TSimpleList<TdwsTypeSymbolLLVMTypePair>.Create;
end;

destructor TdwsTypeSymbolLLVMTypeDictionary.Destroy;
begin
   FPairs.Free;
   inherited;
end;

procedure TdwsTypeSymbolLLVMTypeDictionary.Clear;
begin
   FPairs.Clear;
end;

function TdwsTypeSymbolLLVMTypeDictionary.FindPairForTypeSymbol(
   TypeSymbol: TTypeSymbol): PLLVMType;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].TypeSymbol = TypeSymbol then
         Exit(FPairs[pairIndex].LLVMType);
end;

function TdwsTypeSymbolLLVMTypeDictionary.FindPairForLLVMType(
   LLVMType: PLLVMType): TTypeSymbol;
var
   pairIndex : Integer;
begin
   Result := nil;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMType = LLVMType then
         Exit(FPairs[pairIndex].TypeSymbol);
end;

function TdwsTypeSymbolLLVMTypeDictionary.FindPairIndexForTypeSymbol(
  TypeSymbol: TTypeSymbol): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].TypeSymbol = TypeSymbol then
         Exit(pairIndex);
end;

function TdwsTypeSymbolLLVMTypeDictionary.FindPairIndexForLLVMType(
  LLVMType: PLLVMType): Integer;
var
   pairIndex : Integer;
begin
   Result := -1;
   for pairIndex := 0 to FPairs.Count - 1 do
      if FPairs[pairIndex].LLVMType = LLVMType then
         Exit(pairIndex);
end;

function TdwsTypeSymbolLLVMTypeDictionary.GetCount: Cardinal;
begin
   Result := FPairs.Count;
end;

function TdwsTypeSymbolLLVMTypeDictionary.GetLLVMType(
  TypeSymbol: TTypeSymbol): PLLVMType;
var
   pairIndex : Integer;
begin
   Result := nil;
   pairIndex := FindPairIndexForTypeSymbol(TypeSymbol);
   if pairIndex >= 0 then
      Result := FPairs.Items[pairIndex].LLVMType;
end;

procedure TdwsTypeSymbolLLVMTypeDictionary.LinkLLVMTypeToTypeSymbol(LLVMType: PLLVMType;
   TypeSymbol: TTypeSymbol);
var
   objIndex : Integer;
   pair : TdwsTypeSymbolLLVMTypePair;
begin
   objIndex := FindPairIndexForTypeSymbol(TypeSymbol);
   if objIndex >= 0 then
   begin
      pair := FPairs[objIndex];
      Assert(pair.TypeSymbol = TypeSymbol);
      pair.LLVMType := LLVMType;
      FPairs[objIndex] := pair;
      Exit;
   end;

   pair.TypeSymbol := TypeSymbol;
   pair.LLVMType := LLVMType;
   FPairs.Add(pair);
end;

procedure TdwsTypeSymbolLLVMTypeDictionary.RemoveTypeSymbol(TypeSymbol: TTypeSymbol);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForTypeSymbol(TypeSymbol);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;

procedure TdwsTypeSymbolLLVMTypeDictionary.RemoveLLVMType(
   LLVMType: PLLVMType);
var
   pairIndex : Integer;
begin
   pairIndex := FindPairIndexForLLVMType(LLVMType);

   if pairIndex >= 0 then
      FPairs.Extract(pairIndex);
end;
{$ENDIF}


// ------------------
// ------------------ TdwsLLVMCodeGen ------------------
// ------------------

// Create
//
constructor TdwsLLVMCodeGen.Create;
begin
   inherited;

   RegisterCodeGen(TBlockInitExpr, TLLVMBlockInitExpr.Create);

   RegisterCodeGen(TBlockExpr, TLLVMBlockExpr.Create);
   RegisterCodeGen(TBlockExprNoTable, TLLVMBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable2, TLLVMBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable3, TLLVMBlockExprBase.Create);
   RegisterCodeGen(TBlockExprNoTable4, TLLVMBlockExprBase.Create);

   RegisterCodeGen(TNullExpr, TLLVMNullExpr.Create);

   RegisterCodeGen(TAssignExpr, TLLVMAssignExpr.Create);
   RegisterCodeGen(TAssignDataExpr, TLLVMAssignExpr.Create);
   RegisterCodeGen(TAssignConstToBoolVarExpr, TLLVMAssignConstVarExpr.Create(btBool));
   RegisterCodeGen(TAssignConstToIntegerVarExpr, TLLVMAssignConstVarExpr.Create(btInt));
   RegisterCodeGen(TAssignConstToFloatVarExpr, TLLVMAssignConstVarExpr.Create(btFloat));

   RegisterCodeGen(TVarExpr, TLLVMVarExpr.Create);
   RegisterCodeGen(TVarParamExpr, TLLVMVarExpr.Create);

   RegisterCodeGen(TBoolVarExpr, TLLVMVarExpr.Create);
   RegisterCodeGen(TIntVarExpr, TLLVMVarExpr.Create);
   RegisterCodeGen(TFloatVarExpr, TLLVMVarExpr.Create);

   RegisterCodeGen(TRecordExpr, TLLVMRecordExpr.Create);
   RegisterCodeGen(TRecordVarExpr, TLLVMRecordExpr.Create);
   RegisterCodeGen(TStaticArrayExpr, TLLVMStaticArrayExpr.Create);

   RegisterCodeGen(TConstIntExpr, TLLVMConstIntExpr.Create);
   RegisterCodeGen(TConstFloatExpr, TLLVMConstFloatExpr.Create);

   RegisterCodeGen(TAddIntExpr, TLLVMBinOpExpr.Create(LLVMAdd));
   RegisterCodeGen(TAddFloatExpr, TLLVMBinOpExpr.Create(LLVMFAdd));
   RegisterCodeGen(TSubIntExpr, TLLVMBinOpExpr.Create(LLVMSub));
   RegisterCodeGen(TSubFloatExpr, TLLVMBinOpExpr.Create(LLVMFSub));
   RegisterCodeGen(TMultIntExpr, TLLVMBinOpExpr.Create(LLVMMul));
   RegisterCodeGen(TMultIntPow2Expr, TLLVMMultIntPow2Expr.Create);
   RegisterCodeGen(TMultFloatExpr, TLLVMBinOpExpr.Create(LLVMFMul));
   RegisterCodeGen(TDivExpr, TLLVMBinOpExpr.Create(LLVMSDiv));
   RegisterCodeGen(TDivideExpr, TLLVMBinOpExpr.Create(LLVMFDiv));
   RegisterCodeGen(TModExpr, TLLVMBinOpExpr.Create(LLVMSRem));

   RegisterCodeGen(TSarExpr, TLLVMBinOpExpr.Create(LLVMAShr));
   RegisterCodeGen(TShrExpr, TLLVMBinOpExpr.Create(LLVMLShr));
   RegisterCodeGen(TShlExpr, TLLVMBinOpExpr.Create(LLVMShl));
   RegisterCodeGen(TBoolAndExpr, TLLVMBinOpExpr.Create(LLVMAnd));
   RegisterCodeGen(TBoolOrExpr, TLLVMBinOpExpr.Create(LLVMOr));
   RegisterCodeGen(TBoolXorExpr, TLLVMBinOpExpr.Create(LLVMXor));
   RegisterCodeGen(TIntAndExpr, TLLVMBinOpExpr.Create(LLVMAnd));
   RegisterCodeGen(TIntOrExpr, TLLVMBinOpExpr.Create(LLVMOr));
   RegisterCodeGen(TIntXorExpr, TLLVMBinOpExpr.Create(LLVMXor));

   RegisterCodeGen(TNotBoolExpr, TLLVMNotExpr.Create);
   RegisterCodeGen(TNotIntExpr, TLLVMNotExpr.Create);
   RegisterCodeGen(TSqrIntExpr, TLLVMSqrExpr.Create(True));
   RegisterCodeGen(TSqrFloatExpr, TLLVMSqrExpr.Create(False));
   RegisterCodeGen(TNegIntExpr, TLLVMNegExpr.Create(True));
   RegisterCodeGen(TNegFloatExpr, TLLVMNegExpr.Create(False));
   RegisterCodeGen(TAbsIntExpr, TLLVMAbsIntExpr.Create);
   RegisterCodeGen(TAbsFloatExpr, TLLVMAbsFloatExpr.Create);

   RegisterCodeGen(TIncIntVarExpr, TLLVMOpAssignExpr.Create(LLVMAdd));
   RegisterCodeGen(TDecIntVarExpr, TLLVMOpAssignExpr.Create(LLVMSub));

   RegisterCodeGen(TIncVarFuncExpr, TLLVMIncDecVarFuncExpr.Create(LLVMAdd));
   RegisterCodeGen(TDecVarFuncExpr, TLLVMIncDecVarFuncExpr.Create(LLVMSub));

   RegisterCodeGen(TPlusAssignIntExpr, TLLVMOpAssignExpr.Create(LLVMAdd));
   RegisterCodeGen(TPlusAssignFloatExpr, TLLVMOpAssignExpr.Create(LLVMFAdd));
   RegisterCodeGen(TMinusAssignIntExpr, TLLVMOpAssignExpr.Create(LLVMSub));
   RegisterCodeGen(TMinusAssignFloatExpr, TLLVMOpAssignExpr.Create(LLVMFSub));
   RegisterCodeGen(TMultAssignIntExpr, TLLVMOpAssignExpr.Create(LLVMMul));
   RegisterCodeGen(TMultAssignFloatExpr, TLLVMOpAssignExpr.Create(LLVMFMul));
   RegisterCodeGen(TDivideAssignExpr, TLLVMOpAssignExpr.Create(LLVMFDiv));

   RegisterCodeGen(TConvFloatExpr, TLLVMConvFloatExpr.Create);

   RegisterCodeGen(TRelIntIsZeroExpr, TLLVMRelIntZeroExpr.Create(True));
   RegisterCodeGen(TRelIntIsNotZeroExpr, TLLVMRelIntZeroExpr.Create(False));
   RegisterCodeGen(TRelEqualIntExpr, TLLVMRelIntExpr.Create(LLVMIntEQ));
   RegisterCodeGen(TRelLessIntExpr, TLLVMRelIntExpr.Create(LLVMIntSLT));
   RegisterCodeGen(TRelLessEqualIntExpr, TLLVMRelIntExpr.Create(LLVMIntSLE));
   RegisterCodeGen(TRelGreaterIntExpr, TLLVMRelIntExpr.Create(LLVMIntSGT));
   RegisterCodeGen(TRelGreaterEqualIntExpr, TLLVMRelIntExpr.Create(LLVMIntSGE));

   RegisterCodeGen(TRelEqualFloatExpr, TLLVMRelFloatExpr.Create(LLVMRealOEQ));
   RegisterCodeGen(TRelLessFloatExpr, TLLVMRelFloatExpr.Create(LLVMRealOLT));
   RegisterCodeGen(TRelLessEqualFloatExpr, TLLVMRelFloatExpr.Create(LLVMRealOLE));
   RegisterCodeGen(TRelGreaterFloatExpr, TLLVMRelFloatExpr.Create(LLVMRealOGT));
   RegisterCodeGen(TRelGreaterEqualFloatExpr, TLLVMRelFloatExpr.Create(LLVMRealOGE));

   RegisterCodeGen(TIfThenElseValueExpr, TLLVMIfThenElseValueExpr.Create);
   RegisterCodeGen(TIfThenExpr, TLLVMIfThenExpr.Create);
   RegisterCodeGen(TIfThenElseExpr, TLLVMIfThenElseExpr.Create);
   RegisterCodeGen(TCaseExpr, TLLVMCaseExpr.Create);

   RegisterCodeGen(TForUpwardExpr, TLLVMForExpr.Create(False));
   RegisterCodeGen(TForDownwardExpr, TLLVMForExpr.Create(True));
   RegisterCodeGen(TForUpwardStepExpr, TLLVMForExpr.Create(False));
   RegisterCodeGen(TForDownwardStepExpr, TLLVMForExpr.Create(True));
   RegisterCodeGen(TRepeatExpr, TLLVMRepeatExpr.Create);
   RegisterCodeGen(TWhileExpr, TLLVMWhileExpr.Create);

   RegisterCodeGen(TFuncExpr, TLLVMFuncBaseExpr.Create);

   RegisterCodeGen(TExitExpr, TLLVMExitExpr.Create);
   RegisterCodeGen(TExitValueExpr, TLLVMExitValueExpr.Create);

   RegisterCodeGen(TMagicIntFuncExpr, TLLVMMagicFuncExpr.Create);
   RegisterCodeGen(TMagicStringFuncExpr, TLLVMMagicFuncExpr.Create);
   RegisterCodeGen(TMagicFloatFuncExpr, TLLVMMagicFuncExpr.Create);
   RegisterCodeGen(TMagicBoolFuncExpr, TLLVMMagicFuncExpr.Create);
   RegisterCodeGen(TMagicVariantFuncExpr, TLLVMMagicFuncExpr.Create);
   RegisterCodeGen(TMagicProcedureExpr, TLLVMMagicFuncExpr.Create);

   // create llvm context (valid for this TdwsLLVMCodeGen instance)
   {$IFDEF UseExplicitContext}
   FContext := TLLVMContext.Create;
   {$ENDIF}

   FVerifyModule := False;
   FOptimizations := loNone;

{$IFNDEF LLVM_TAG}
   FdwsLLVMDict := TdwsExpressionLLVMValueDictionary.Create;
   FdwsLLVMDataSymbolDict := TdwsDataSymbolLLVMValueDictionary.Create;
   FdwsLLVMFuncSymbolDict := TdwsFunctionSymbolLLVMValueDictionary.Create;
   FdwsLLVMTypeSymbolDict := TdwsTypeSymbolLLVMTypeDictionary.Create;
{$ENDIF}
end;

// Destroy
//
destructor TdwsLLVMCodeGen.Destroy;
begin
{$IFNDEF LLVM_TAG}
   FreeAndNil(FdwsLLVMTypeSymbolDict);
   FreeAndNil(FdwsLLVMFuncSymbolDict);
   FreeAndNil(FdwsLLVMDataSymbolDict);
   FreeAndNil(FdwsLLVMDict);
{$ENDIF}

   if Assigned(FModule) then
      FModule.Free;

   if Assigned(FTargetData) then
      FTargetData.Free;

   {$IFDEF UseExplicitContext}
   FContext.Free;
   {$ENDIF}

   inherited;
end;

procedure TdwsLLVMCodeGen.BeginProgramSession(const prog: IdwsProgram);
begin
   inherited;

   if Assigned(FTargetData) then
      FTargetData.Free;

   // eventually remove a previously create module (<-- can be removed later)
   if Assigned(FModule) then
      FModule.Free;

   {$IFDEF UseExplicitContext}
   FModule := TLLVMModule.Create(FModuleName, FContext); // <-- this is yet buggy
   {$ELSE}
   FModule := TLLVMModule.Create(FModuleName);
   FContext := FModule.Context;
   {$ENDIF}

   // still use hard coded target and data layout
   FModule.Target := 'i686-pc-win32';
   FModule.DataLayout := 'e-p:32:32:32-S32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f16:16:16-f32:32:32-f64:64:64-f128:128:128-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32';

   FTargetData := TLLVMTargetData.Create(FModule.DataLayout);

   // create standard types
   FBoolType := TLLVMInt1Type.Create(FContext);
   FIntType := TLLVMInt64Type.Create(FContext);
   FFloatType := TLLVMDoubleType.Create(FContext);

{$IFNDEF LLVM_TAG}
   FdwsLLVMDict.Clear;
   FdwsLLVMDataSymbolDict.Clear;
   FdwsLLVMFuncSymbolDict.Clear;
{$ENDIF}
end;

procedure TdwsLLVMCodeGen.EndProgramSession;
var
   PassManagerBuilder: TLLVMPassManagerBuilder;
   PassManager: TLLVMPassManager;
begin
   FreeAndNil(FBoolType);
   FreeAndNil(FIntType);
   FreeAndNil(FFloatType);

   if VerifyModule then
      FModule.VerifyModule(LLVMPrintMessageAction);

   if FOptimizations <> loNone then
   begin
      PassManager := TLLVMPassManager.Create;
      try
         PassManager.AddTargetData(FTargetData);

         case FOptimizations of
            loLess:
               begin
                  PassManagerBuilder := TLLVMPassManagerBuilder.Create;
                  try
                     PassManagerBuilder.OptLevel := LLVMCodeGenLevelLess;
                     PassManagerBuilder.PopulateModulePassManager(PassManager);
                  finally
                     FreeAndNil(PassManagerBuilder);
                  end;
               end;
            loDefault:
               begin
                  PassManagerBuilder := TLLVMPassManagerBuilder.Create;
                  try
                     PassManagerBuilder.OptLevel := LLVMCodeGenLevelDefault;
                     PassManagerBuilder.PopulateModulePassManager(PassManager);
                  finally
                     FreeAndNil(PassManagerBuilder);
                  end;
               end;
            loAggressive:
               begin
                  PassManagerBuilder := TLLVMPassManagerBuilder.Create;
                  try
                     PassManagerBuilder.OptLevel := LLVMCodeGenLevelAggressive;
                     PassManagerBuilder.PopulateModulePassManager(PassManager);
                  finally
                     FreeAndNil(PassManagerBuilder);
                  end;
               end;
            loCustom:
               begin
                  if cpAggressiveDCEPass in FCustomPasses then
                     LLVMAddAggressiveDCEPass(PassManager.Handle);
                  if cpCFGSimplificationPass in FCustomPasses then
                     LLVMAddCFGSimplificationPass(PassManager.Handle);
                  if cpDeadStoreEliminationPass in FCustomPasses then
                     LLVMAddDeadStoreEliminationPass(PassManager.Handle);
                  if cpGVNPass in FCustomPasses then
                     LLVMAddGVNPass(PassManager.Handle);
                  if cpIndVarSimplifyPass in FCustomPasses then
                     LLVMAddIndVarSimplifyPass(PassManager.Handle);
                  if cpInstructionCombiningPass in FCustomPasses then
                     LLVMAddInstructionCombiningPass(PassManager.Handle);
                  if cpJumpThreadingPass in FCustomPasses then
                     LLVMAddJumpThreadingPass(PassManager.Handle);
                  if cpLICMPass in FCustomPasses then
                     LLVMAddLICMPass(PassManager.Handle);
                  if cpLoopDeletionPass in FCustomPasses then
                     LLVMAddLoopDeletionPass(PassManager.Handle);
                  if cpLoopIdiomPass in FCustomPasses then
                     LLVMAddLoopIdiomPass(PassManager.Handle);
                  if cpLoopRotatePass in FCustomPasses then
                     LLVMAddLoopRotatePass(PassManager.Handle);
                  if cpLoopUnrollPass in FCustomPasses then
                     LLVMAddLoopUnrollPass(PassManager.Handle);
                  if cpLoopUnswitchPass in FCustomPasses then
                     LLVMAddLoopUnswitchPass(PassManager.Handle);
                  if cpMemCpyOptPass in FCustomPasses then
                     LLVMAddMemCpyOptPass(PassManager.Handle);
                  if cpPromoteMemoryToRegisterPass in FCustomPasses then
                     LLVMAddPromoteMemoryToRegisterPass(PassManager.Handle);
                  if cpReassociatePass in FCustomPasses then
                     LLVMAddReassociatePass(PassManager.Handle);
                  if cpSCCPPass in FCustomPasses then
                     LLVMAddSCCPPass(PassManager.Handle);
                  if cpScalarReplAggregatesPass in FCustomPasses then
                     LLVMAddScalarReplAggregatesPass(PassManager.Handle);
                  if cpScalarReplAggregatesPassSSA in FCustomPasses then
                     LLVMAddScalarReplAggregatesPassSSA(PassManager.Handle);
                  if cpSimplifyLibCallsPass in FCustomPasses then
                     LLVMAddSimplifyLibCallsPass(PassManager.Handle);
                  if cpTailCallEliminationPass in FCustomPasses then
                     LLVMAddTailCallEliminationPass(PassManager.Handle);
                  if cpConstantPropagationPass in FCustomPasses then
                     LLVMAddConstantPropagationPass(PassManager.Handle);
                  if cpDemoteMemoryToRegisterPass in FCustomPasses then
                     LLVMAddDemoteMemoryToRegisterPass(PassManager.Handle);
                  if cpVerifierPass in FCustomPasses then
                     LLVMAddVerifierPass(PassManager.Handle);
                  if cpCorrelatedValuePropagationPass in FCustomPasses then
                     LLVMAddCorrelatedValuePropagationPass(PassManager.Handle);
                  if cpEarlyCSEPass in FCustomPasses then
                     LLVMAddEarlyCSEPass(PassManager.Handle);
                  if cpLowerExpectIntrinsicPass in FCustomPasses then
                     LLVMAddLowerExpectIntrinsicPass(PassManager.Handle);
                  if cpTypeBasedAliasAnalysisPass in FCustomPasses then
                     LLVMAddTypeBasedAliasAnalysisPass(PassManager.Handle);
                  if cpBasicAliasAnalysisPass in FCustomPasses then
                     LLVMAddBasicAliasAnalysisPass(PassManager.Handle);

                  if cpBBVectorizePass in FCustomPasses then
                     LLVMAddBBVectorizePass(PassManager.Handle);
                  if cpLoopVectorizePass in FCustomPasses then
                     LLVMAddLoopVectorizePass(PassManager.Handle);
                  if cpArgumentPromotionPass in FCustomPasses then
                     LLVMAddArgumentPromotionPass(PassManager.Handle);

                  if cpConstantMergePass in FCustomPasses then
                     LLVMAddConstantMergePass(PassManager.Handle);
                  if cpDeadArgEliminationPass in FCustomPasses then
                     LLVMAddDeadArgEliminationPass(PassManager.Handle);
                  if cpFunctionAttrsPass in FCustomPasses then
                     LLVMAddFunctionAttrsPass(PassManager.Handle);
                  if cpFunctionInliningPass in FCustomPasses then
                     LLVMAddFunctionInliningPass(PassManager.Handle);
                  if cpAlwaysInlinerPass in FCustomPasses then
                     LLVMAddAlwaysInlinerPass(PassManager.Handle);
                  if cpGlobalDCEPass in FCustomPasses then
                     LLVMAddGlobalDCEPass(PassManager.Handle);
                  if cpGlobalOptimizerPass in FCustomPasses then
                     LLVMAddGlobalOptimizerPass(PassManager.Handle);
                  if cpIPConstantPropagationPass in FCustomPasses then
                     LLVMAddIPConstantPropagationPass(PassManager.Handle);
                  if cpPruneEHPass in FCustomPasses then
                     LLVMAddPruneEHPass(PassManager.Handle);
                  if cpIPSCCPPass in FCustomPasses then
                     LLVMAddIPSCCPPass(PassManager.Handle);
                  if cpStripDeadPrototypesPass in FCustomPasses then
                     LLVMAddStripDeadPrototypesPass(PassManager.Handle);
                  if cpStripSymbolsPass in FCustomPasses then
                     LLVMAddStripSymbolsPass(PassManager.Handle);
               end;
         end;
         PassManager.RunPassManager(FModule);
      finally
         PassManager.Free;
      end;
   end;


   begin
   end;

   // FModule.Free;  // <-- may be used once everything is working

   inherited;
end;

// Dump
//
procedure TdwsLLVMCodeGen.Dump;
begin
   if Assigned(FModule) then
      FModule.DumpModule;
end;

// EmitToFile
//
procedure TdwsLLVMCodeGen.EmitToFile(FileName: AnsiString;
   CodeGenFileType: TLLVMCodeGenFileType);
var
   CPU: AnsiString;
   Triple: AnsiString;
   Features: AnsiString;
   Target: PLLVMTarget;
   TargetMachine: PLLVMTargetMachine;
   ErrorMessage: PAnsiChar;
begin
   Target := LLVMGetFirstTarget;
   repeat
      CPU := LLVMGetTargetName(Target);
      if CPU = 'x86' then
         Break;
      Target := LLVMGetNextTarget(Target);
   until Target = nil;

   if not Assigned(Target) then
      Exit;

   Triple := FModule.Target;
   Features := '+sse2';

(*
      +cmov (Enable conditional move instructions)
      +popcnt (Support POPCNT instruction)
      +mmx (Enable MMX instructions)
      +sse (Enable SSE instructions)
      +sse2 (Enable SSE2 instructions)
      +sse3 (Enable SSE3 instructions)
      +ssse3 (Enable SSSE3 instructions)
      +sse41 (Enable SSE 4.1 instructions)
      +sse42 (Enable SSE 4.2 instructions)
      +3dnow (Enable 3DNow! instructions)
      +3dnowa (Enable 3DNow! Athlon instructions)
      +64bit (Support 64-bit instructions)
      +cmpxchg16b (64-bit with cmpxchg16b)
      +slow-bt-mem (Bit testing of memory is slow)
      +fast-unaligned-mem (IsUAMemFast)
      +sse4a (Support SSE 4a instructions)
      +avx (Enable AVX instructions)
      +avx2 (Enable AVX2 instructions)
      +pclmul (Enable packed carry-less multiplication instructions)
      +fma (Enable three-operand fused multiple-add)
      +fma4 (Enable four-operand fused multiple-add)
      +xop (Enable XOP instructions)
      +vector-unaligned-mem (HasVectorUAMem)
      +aes (Enable AES instructions)
      +movbe (Support MOVBE instruction)
      +rdrand (Support RDRAND instruction)
      +f16c (Support 16-bit floating point conversion instructions)
      +fsgsbase (Support FS/GS Base instructions)
      +lzcnt (Support LZCNT instruction)
      +bmi (Support BMI instructions)
      +bmi2 (Support BMI2 instructions)
      +rtm (Support RTM instructions)
      +hle (Support HLE)
      +adx (Support ADX instructions)
      +prfchw (Support PRFCHW instructions)
      +rdseed (Support RDSEED instruction)
      +lea-sp (Use LEA for adjusting the stack pointer)
      +idiv-to-divb (HasSlowDivide)
      +pad-short-functions (PadShortFunctions)
      +call-reg-indirect (CallRegIndirect)
      +lea-uses-ag (LEA instruction needs inputs at AG stage)

   CPU := 'generic';
      { Alternatives: generic, i386, i486, i586, pentium, pentium-mmx, i686,
            pentiumpro, pentium2, pentium3, pentium3m, pentium-m, pentium4,
            pentium4m, x86-64, yonah, prescott, nocona, core2, penryn, atom,
            corei7, nehalem, westmere, corei7-avx, core-avx-i, core-avx2, k6,
            k6-2, k6-3, athlon, athlon-tbird, athlon-4, athlon-xp, athlon-mp,
            k8, opteron, athlon64, athlon-fx, k8-sse3, opteron-sse3,
            athlon64-sse3, amdfam10, btver1, btver2, bdver1, bdver2, geode,
            winchip-c6, winchip2, c3, c3-2
      }
*)

   ErrorMessage := nil;
   TargetMachine := LLVMCreateTargetMachine(Target, PAnsiChar(Triple),
      PAnsiChar(CPU), PAnsiChar(Features), LLVMCodeGenLevelAggressive,
      LLVMRelocDefault, LLVMCodeModelDefault);

   try
      LLVMTargetMachineEmitToFile(TargetMachine, FModule.Handle,
         PAnsiChar(FileName), CodeGenFileType, ErrorMessage);
   finally
     LLVMDisposeTargetMachine(TargetMachine);
   end;

   if Assigned(ErrorMessage) then
   try
      raise Exception.Create(string(AnsiString(ErrorMessage)));
   finally
      LLVMDisposeMessage(ErrorMessage);
   end;
end;

// PrintToFile
//
procedure TdwsLLVMCodeGen.PrintToFile(FileName: AnsiString);
begin
   if Assigned(FModule) then
      FModule.PrintModuleToFile(FileName);
end;

// SaveByteCodeToFile
//
procedure TdwsLLVMCodeGen.SaveByteCodeToFile(FileName: AnsiString);
begin
   FModule.WriteBitcodeToFile(FileName);
end;

// WriteFloat
//
procedure TdwsLLVMCodeGen.WriteFloat(const v: Double;
  const fmt: TFormatSettings);
begin
   raise ECodeGenException.Create(RStrNotSupported);
end;

// WriteLiteralString
//
procedure TdwsLLVMCodeGen.WriteLiteralString(const s: string);
begin
   raise ECodeGenException.Create(RStrNotSupported);
end;

// Clear
//
procedure TdwsLLVMCodeGen.Clear;
begin
   inherited;
end;

// TypeSymbolToLLVMType
//
function TdwsLLVMCodeGen.TypeSymbolToLLVMType(
   TypeSymbol: TTypeSymbol): PLLVMType;
var
  index: Integer;
  recSym: TRecordSymbol;
  classSym: TClassSymbol;
  arrSym: TStaticArraySymbol;
  arrayType: TLLVMArrayType;
  structType: TLLVMStructType;
  name: AnsiString;
  ElementTypes: array of PLLVMType;
begin
   // convert DWS types to LLVM types
   if TypeSymbol is TBaseBooleanSymbol then
   begin
      Result := FBoolType.Handle;
   end else
   if TypeSymbol is TBaseIntegerSymbol then
   begin
      Result := FIntType.Handle;
   end else
   if TypeSymbol is TBaseFloatSymbol then
   begin
      Result := FFloatType.Handle;
   end
   else
   if TypeSymbol is TStaticArraySymbol then
   begin
      arrSym := TStaticArraySymbol(TypeSymbol);
      Result := FdwsLLVMTypeSymbolDict.GetLLVMType(arrSym);
      if not Assigned(Result) then
      begin
         name := AnsiString(arrSym.Name);
         arrayType := TLLVMArrayType.Create(TypeSymbolToLLVMType(arrSym.Typ), arrSym.ElementCount);
         Result := arrayType.Handle;
         FdwsLLVMTypeSymbolDict.LinkLLVMTypeToTypeSymbol(Result, arrSym);
      end;
   end
   else
   if TypeSymbol is TRecordSymbol then
   begin
      recSym := TRecordSymbol(TypeSymbol);
      Result := FdwsLLVMTypeSymbolDict.GetLLVMType(recSym);
      if not Assigned(Result) then
      begin
         name := AnsiString(recSym.Name);
         structType := TLLVMStructType.Create(FContext, PAnsiChar(name));
         SetLength(ElementTypes, recSym.Members.Count);
         for index := 0 to Length(ElementTypes) - 1 do
           ElementTypes[index] := TypeSymbolToLLVMType(recSym.Members[index].Typ);

         structType.SetBody(@ElementTypes[0], Length(ElementTypes));
         Result := structType.Handle;
         FdwsLLVMTypeSymbolDict.LinkLLVMTypeToTypeSymbol(Result, recSym);
      end;
   end
   else
   if TypeSymbol is TClassSymbol then
   begin
      // yet todo
      classSym := TClassSymbol(TypeSymbol);
      Result := FdwsLLVMTypeSymbolDict.GetLLVMType(classSym);
      if not Assigned(Result) then
      begin
         name := AnsiString(classSym.Name);
         structType := TLLVMStructType.Create(FContext, PAnsiChar(name));
         SetLength(ElementTypes, classSym.Members.Count);
         for index := 0 to Length(ElementTypes) - 1 do
           ElementTypes[index] := TypeSymbolToLLVMType(classSym.Members[index].Typ);

         structType.SetBody(@ElementTypes[0], Length(ElementTypes));
         Result := structType.Handle;
         FdwsLLVMTypeSymbolDict.LinkLLVMTypeToTypeSymbol(Result, classSym);
      end;
   end
   else
      Result := nil;
end;

// CompileProgramBody
//
procedure TdwsLLVMCodeGen.CompileProgramBody(expr: TProgramExpr);
var
   typeInt32 : PLLVMType;
   typeMain : PLLVMType;
   builder : TLLVMBuilder;
   blockAlloc : PLLVMBasicBlock;
   blockEntry : PLLVMBasicBlock;
   blockReturn : PLLVMBasicBlock;
   valFunc : PLLVMValue;
   valBranch : PLLVMValue;
   valReturn : PLLVMValue;
   nameFunc : AnsiString;
begin
   // setup main function type
   typeInt32 := LLVMInt32TypeInContext(FContext.Handle);
   typeMain := LLVMFunctionType(typeInt32, nil, 0, False);

   // setup function name for body -> 'main', performs no checks if already taken!
   nameFunc := 'main';

   // check if main function already exists, if not add function
   valFunc := LLVMGetNamedFunction(FModule.Handle, PAnsiChar(nameFunc));
   if not Assigned(valFunc) then
   begin
      valFunc := LLVMAddFunction(FModule.Handle, PAnsiChar(nameFunc), typeMain);
      LLVMSetLinkage(valFunc, LLVMExternalLinkage);
      LLVMSetFunctionCallConv(valFunc, Ord(LLVMCCallConv));
   end;

   // build blocks
   blockAlloc := LLVMAppendBasicBlockInContext(FContext.Handle, valFunc, 'alloc');
   blockEntry := LLVMAppendBasicBlockInContext(FContext.Handle, valFunc, 'entry');
   blockReturn := LLVMAppendBasicBlockInContext(FContext.Handle, valFunc, 'result');

   // create builder (essential to define function bodies)
   builder := TLLVMBuilder.Create;
   try
      // build empty alloc block
      builder.PositionBuilderAtEnd(blockAlloc);
      LLVMBuildBr(builder.Handle, blockEntry);

      // locate entry block
      builder.PositionBuilderAtEnd(blockEntry);
      FBuilder := builder;

      inherited;

      // branch to return result section
      valBranch := LLVMBuildBr(builder.Handle, blockReturn);
      LLVMMoveBasicBlockAfter(blockReturn, LLVMGetInstructionParent(valBranch));
      builder.PositionBuilderAtEnd(blockReturn);

      // return result (= zero)
      valReturn := LLVMConstNull(typeInt32);
      LLVMBuildRet(builder.Handle, valReturn);
   finally
      builder.Free;
   end;
end;

// DoCompileFuncSymbol
//
procedure TdwsLLVMCodeGen.DoCompileFuncSymbol(func: TFuncSymbol;
  deAnonymize: Boolean);
var
   index : Integer;
   argCount : Cardinal;
   params : TParamsSymbolTable;
   retType : PLLVMType;
   argTypes : array of PLLVMType;
   typeFunc : PLLVMType;
   builder : TLLVMBuilder;
   blockAlloc : PLLVMBasicBlock;
   blockEntry : PLLVMBasicBlock;
   blockReturn : PLLVMBasicBlock;
   valFunc : PLLVMValue;
   argValue : PLLVMValue;
   argValues : array of PLLVMValue;
   valReturn : PLLVMValue;
   valBranch : PLLVMValue;
   nameFunc : AnsiString;
   argName : AnsiString;
begin
   // get function name
   nameFunc := AnsiString(func.Name);

   // get result / return type
   if Assigned(func.Result) then
      retType := TypeSymbolToLLVMType(func.Result.Typ)
   else
      retType := LLVMVoidTypeInContext(FContext.Handle);

   // get parameter / argument count
   argCount := func.Params.Count;

   // shortcut to func.Params
   params := func.Params;

   // get parameter / argument types
   SetLength(argTypes, argCount);
   for index := 0 to argCount - 1 do
   begin
      argTypes[index] := TypeSymbolToLLVMType(Params.Symbols[index].Typ);
      Assert(Assigned(argTypes[index]));

      // eventually use pointer in case of a 'by reference' parameter
      if params.Symbols[index] is TByRefParamSymbol then
         argTypes[index] := LLVMPointerType(argTypes[index], 0);
   end;

   // define function type and make sure no function with that name already exists
   typeFunc := LLVMFunctionType(retType, @argTypes[0], argCount, False);
   valFunc := LLVMGetNamedFunction(Module.Handle, PAnsiChar(nameFunc));
   if Assigned(valFunc) then
      raise ECodeGenException.Create(RStrFunctionAlreadyExists);

   // add function
   valFunc := LLVMAddFunction(Module.Handle, PAnsiChar(nameFunc), typeFunc);
   LinkLLVMValueToFunctionSymbol(valFunc, func);

   SetLength(argValues, argCount);
   if argCount > 0 then
   begin
      argValues[0] := LLVMGetFirstParam(valFunc);
      LinkLLVMValueToDataSymbol(argValues[0], params.Symbols[0]);
      argName := 'param_' + AnsiString(params.Symbols[0].Name);
      LLVMSetValueName(argValues[0], PAnsiChar(argName));

      for index := 1 to argCount - 1 do
      begin
         argValues[index] := LLVMGetNextParam(argValues[index - 1]);
         argName := 'param_' + AnsiString(params.Symbols[index].Name);
         LinkLLVMValueToDataSymbol(argValues[index], params.Symbols[index]);
         LLVMSetValueName(argValues[index], PAnsiChar(argName));
      end;
   end;

   // define entry basic block (first basic block in a function
   blockAlloc := LLVMAppendBasicBlockInContext(FContext.Handle, valFunc, 'alloc');
   blockEntry := LLVMAppendBasicBlockInContext(FContext.Handle, valFunc, 'entry');
   blockReturn := nil;
   if Assigned(func.Result) then
      blockReturn := LLVMAppendBasicBlockInContext(FContext.Handle, valFunc, 'result');

   // create builder (essential to define function bodies)
   builder := TLLVMBuilder.Create;
   try
      // build empty alloc block
      builder.PositionBuilderAtEnd(blockAlloc);
      valBranch := LLVMBuildBr(builder.Handle, blockEntry);

      // locate entry block
      builder.PositionBuilderAtEnd(blockEntry);
      FBuilder := builder;

      // allocate memory for parameters / arguments and use this instead
      for index := 0 to argCount - 1 do
      begin
         argName := AnsiString(params.Symbols[index].Name);
         LLVMPositionBuilder(builder.Handle, blockAlloc, valBranch);
         builder.PositionBuilderAtEnd(blockEntry);

         if (params.Symbols[index] is TByRefParamSymbol) then
            Assert(LLVMGetTypeKind(LLVMTypeOf(argValues[index])) = LLVMPointerTypeKind)
         else
         begin
            // allocate memory for parameters
            LLVMPositionBuilder(builder.Handle, blockAlloc, valBranch);
            argValue := LLVMBuildAlloca(FBuilder.Handle, argTypes[index], PAnsiChar(argName));
            builder.PositionBuilderAtEnd(blockEntry);

            // copy parameters to memory
            LLVMBuildStore(FBuilder.Handle, argValues[index], argValue);
            argValues[index] := argValue;
            LinkLLVMValueToDataSymbol(argValues[index], params.Symbols[index]);
         end;
      end;

      // allocate memory for result / return value and use this instead
      if Assigned(func.Result) then
      begin
         argName := AnsiString(func.Result.Name);
         LLVMPositionBuilder(builder.Handle, blockAlloc, valBranch);
         argValue := LLVMBuildAlloca(FBuilder.Handle, retType, PAnsiChar(argName));
         builder.PositionBuilderAtEnd(blockEntry);
         LinkLLVMValueToDataSymbol(argValue, func.Result);
      end;

      // actually compile the function
      inherited;

      // check for result / return value and build return
      if Assigned(func.Result) then
      begin
         // branch to result block
         valBranch := LLVMBuildBr(builder.Handle, blockReturn);
         LLVMMoveBasicBlockAfter(blockReturn, LLVMGetInstructionParent(valBranch));
         builder.PositionBuilderAtEnd(blockReturn);

        // get result pointer
         valReturn := DataSymbolToLLVMValue(func.Result);

         // get result value (with meaningful name
         argName := AnsiString(func.Result.Name);
         valReturn := LLVMBuildLoad(FBuilder.Handle, valReturn, PAnsiChar(argName));

         // actually return result value
         LLVMBuildRet(builder.Handle, valReturn);
      end
      else
         LLVMBuildRetVoid(builder.Handle);
   finally
      builder.Free;
   end;
end;


// ------------------
// ------------------ TLLVMNullExpr ------------------
// ------------------

procedure TLLVMNullExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
begin
  // do nothing
end;


// ------------------
// ------------------ TLLVMBlockInitExpr ------------------
// ------------------

procedure TLLVMBlockInitExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
(*
var
   i : Integer;
   blockInit : TBlockExprBase;
*)
begin
//   blockInit := TBlockExprBase(expr);

   if expr is TTypedExpr then
      CodeGenNoWrap(codeGen, TTypedExpr(expr));

   // nothing here yet
end;


// ------------------
// ------------------ TLLVMBlockExprBase ------------------
// ------------------

procedure TLLVMBlockExprBase.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   i : Integer;
   block : TBlockExprNoTable;
   sub : TExprBase;
begin
   block := TBlockExprNoTable(expr);
   for i := 0 to block.SubExprCount-1 do
   begin
      sub := block.SubExpr[i];
      codeGen.Compile(sub);
   end;
end;


// ------------------
// ------------------ TLLVMBlockExpr ------------------
// ------------------

procedure TLLVMBlockExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   block : TBlockExpr;
begin
   block := TBlockExpr(expr);
   codeGen.CompileSymbolTable(block.Table);
   inherited;
end;


// ------------------
// ------------------ TLLVMAssignExpr ------------------
// ------------------

function TLLVMAssignExpr.GetLLVMValue(codeGen: TdwsLLVMCodeGen;
  e: TAssignExpr): PLLVMValue;
begin
   codeGen.CompileNoWrap(e.Right);
   Result := codeGen.ExpressionToLLVMValue(e.Right);
end;

procedure TLLVMAssignExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TAssignExpr;
   eVar : TVarExpr;
   eDest : TDataExpr;
   eArr : TStaticArrayExpr;
   eRec : TRecordExpr;
   sym : TDataSymbol;
   index : Integer;
   valExpr, valSym, valRec, valTerm : PLLVMValue;
   valIndexList: TSimpleList<PLLVMValue>;
   valIndices: array of PLLVMValue;
   valExprType : PLLVMType;
   allocBB , currentBB : PLLVMBasicBlock;
   nameVar : AnsiString;
begin
   e := TAssignExpr(expr);
   cg := TdwsLLVMCodeGen(codeGen);

   if not (e.Left is TDataExpr) then
      raise ECodeGenException.Create(RStrNotSupported);

   valExpr := GetLLVMValue(cg, e);

   eDest := TDataExpr(e.Left);
   while eDest is TRecordExpr do
   begin
      eRec := TRecordExpr(eDest);

      // get LLVMValue for expression
      valRec := codeGen.ExpressionToLLVMValue(eRec.BaseExpr);
      LLVMBuildInsertValue(cg.Builder.Handle, valRec, valExpr, eRec.MemberOffset, '');

      eDest := eRec.BaseExpr;
      valExpr := valRec;
   end;

   if eDest is TStaticArrayExpr then
   begin
      eArr := TStaticArrayExpr(eDest);

      valIndexList := TSimpleList<PLLVMValue>.Create;
      try
         repeat
            if eDest is TStaticArrayExpr then
               eArr := TStaticArrayExpr(eDest);

            codeGen.CompileNoWrap(eArr.IndexExpr);
            valIndexList.Add(codeGen.ExpressionToLLVMValue(eArr.IndexExpr));

            eDest := eArr.BaseExpr;
         until (eDest is TVarExpr);

         SetLength(valIndices, valIndexList.Count + 1);
         valIndices[0] := LLVMConstNull(codeGen.IntType.Handle);
         for Index := 1 to valIndexList.Count do
            valIndices[Index] := valIndexList.Items[Index - 1];

      finally
         valIndexList.Free;
      end;
   end;

   // check if left expression is a TVarExpr (trivial case)
   if eDest is TVarExpr then
   begin
      // get TVarExpr and TDataSymbol
      eVar := TVarExpr(eDest);
      sym := eVar.DataSym;
      if not Assigned(sym) then
         raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [eVar.StackAddr]);

      // get LLVMValue for expression
      nameVar := AnsiString(sym.Name);

      // get LLVMValue pointer
      valSym := codeGen.DataSymbolToLLVMValue(sym);
      if not Assigned(valSym) then
      begin
         // get current basic block
         currentBB := cg.Builder.GetInsertBlock;

         // get entry for current basic block
         allocBB := LLVMGetEntryBasicBlock(LLVMGetBasicBlockParent(currentBB));

         valTerm := LLVMGetBasicBlockTerminator(allocBB);
         if Assigned(valTerm) then
            LLVMPositionBuilder(cg.Builder.Handle, allocBB, valTerm)
         else
            cg.Builder.PositionBuilderAtEnd(allocBB);

         // allocate memory for assign
         valExprType := LLVMTypeOf(valExpr);
         valSym := LLVMBuildAlloca(cg.Builder.Handle, valExprType, PAnsiChar(nameVar));
         cg.Builder.PositionBuilderAtEnd(currentBB);
         codeGen.LinkLLVMValueToDataSymbol(valSym, sym);
      end;

      if Length(valIndices) > 0 then
      begin
         valSym := LLVMBuildGEP(TdwsLLVMCodeGen(codeGen).Builder.Handle,
            valSym, @valIndices[0], Length(valIndices), PAnsiChar(nameVar));
      end;

      LLVMBuildStore(cg.Builder.Handle, valExpr, valSym);
   end
   else
      raise ECodeGenException.Create(RStrNotSupported);
end;


// ------------------
// ------------------ TLLVMAssignConstVarExpr ------------------
// ------------------

constructor TLLVMAssignConstVarExpr.Create(BasicType: TdwsLLVMBasicType);
begin
   FBasicType := BasicType;
end;

function TLLVMAssignConstVarExpr.GetLLVMValue(codeGen: TdwsLLVMCodeGen;
   e: TAssignExpr): PLLVMValue;
begin
   case FBasicType of
      btBool:
         Result := LLVMConstInt(codeGen.BoolType.Handle,
            Integer(TAssignConstToBoolVarExpr(e).Right), True);
      btInt:
         Result := LLVMConstInt(codeGen.IntType.Handle,
            TAssignConstToIntegerVarExpr(e).Right, True);
      btFloat:
         Result := LLVMConstReal(codeGen.FloatType.Handle,
            TAssignConstToFloatVarExpr(e).Right);
      else
         raise ECodeGenException.Create(RStrNotSupported);
   end;
end;


// ------------------
// ------------------ TLLVMConstExpr ------------------
// ------------------

procedure TLLVMConstExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TConstExpr;
begin
   e := TConstExpr(expr);
   CodeGenNoWrap(codeGen, e);
end;


// ------------------
// ------------------ TLLVMConstIntExpr ------------------
// ------------------

procedure TLLVMConstIntExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   e : TConstIntExpr;
   valOut : PLLVMValue;
begin
   e := TConstIntExpr(expr);

   valOut := LLVMConstInt(codeGen.IntType.Handle, e.Value, True);
   codeGen.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMConstFloatExpr ------------------
// ------------------

procedure TLLVMConstFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   e : TConstFloatExpr;
   typeFloat : PLLVMType;
   valOutput : PLLVMValue;
begin
   e := TConstFloatExpr(expr);

   typeFloat := LLVMDoubleTypeInContext(codeGen.Context.Handle);
   valOutput := LLVMConstReal(typeFloat, e.Value);
   codeGen.LinkLLVMValueToExpression(valOutput, expr);
end;


// ------------------
// ------------------ TLLVMVarExpr ------------------
// ------------------

procedure TLLVMVarExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TVarExpr;
   sym : TDataSymbol;
   valOut : PLLVMValue;
   typeOut : PLLVMType;
   nameSym : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TVarExpr(expr);

   sym := e.DataSym;
   if not Assigned(sym) then
      raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [e.StackAddr]);

   // load value from symbol and link to expression
   valOut := cg.DataSymbolToLLVMValue(sym);

   if not Assigned(valOut) then
      raise ECodeGenException.CreateFmt(RStrValueIsUndefined, [sym.Name]);

   nameSym := AnsiString(sym.Name);

   // get LLVMValue type and check if type is an integer (used as internal counter)
   typeOut := LLVMTypeOf(valOut);
   if (LLVMGetTypeKind(typeOut) = LLVMIntegerTypeKind) then
      valOut := LLVMBuildSExt(cg.Builder.Handle, valOut, cg.IntType.Handle, PAnsiChar(nameSym))
   else
   begin
      if (e.BaseType is TBaseSymbol) or (e.BaseType is TRecordSymbol) then
         valOut := LLVMBuildLoad(cg.Builder.Handle, valOut, PAnsiChar(nameSym));
   end;
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMRecordExpr ------------------
// ------------------

procedure TLLVMRecordExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TRecordExpr;
   valOut : PLLVMValue;
   name: AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TRecordExpr(expr);

   codeGen.Compile(e.BaseExpr);

   valOut := cg.ExpressionToLLVMValue(e.BaseExpr);

   name := LLVMGetValueName(valOut) + '.' + AnsiString(e.FieldSymbol.Name);

   valOut := LLVMBuildExtractValue(cg.Builder.Handle, valOut,
      e.MemberOffset, PAnsiChar(name));

   codeGen.LinkLLVMValueToExpression(valOut, e);
end;


// ------------------
// ------------------ TLLVMStaticArrayExpr ------------------
// ------------------

procedure TLLVMStaticArrayExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TStaticArrayExpr;
   valIndices : array [0..1] of PLLVMValue;
   valOut, valArrayPtr: PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TStaticArrayExpr(expr);

   cg.CompileNoWrap(e.IndexExpr);

   valIndices[0] := LLVMConstNull(cg.IntType.Handle);
   valIndices[1] := cg.ExpressionToLLVMValue(e.IndexExpr);

   // compile base expression to LLVM pointer
   cg.CompileNoWrap(e.BaseExpr);
   valArrayPtr := cg.ExpressionToLLVMValue(e.BaseExpr);

   // calculate actual address
   valOut := LLVMBuildGEP(cg.Builder.Handle, valArrayPtr,
      @valIndices[0], 2, '');

   // eventually dereferentiate
   if (e.Typ is TBaseSymbol) or (e.BaseType is TRecordSymbol) then
      valOut := LLVMBuildLoad(cg.Builder.Handle, valOut, '');

   // assign output value to expression
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMBinOpExpr ------------------
// ------------------

constructor TLLVMBinOpExpr.Create(Op: TLLVMOpcode);
begin
   FOp := Op;
   FIsFloatOp := Op in [LLVMFAdd, LLVMFSub, LLVMFMul, LLVMFDiv, LLVMFRem];
end;

procedure TLLVMBinOpExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TBinaryOpExpr;

   procedure AdaptValue(var val: PLLVMValue);

      function TypeIsFloat(typeKind: TLLVMTypeKind): Boolean;
      begin
         // check if type kind is among any floating point typs
         Result := (typeKind in [LLVMHalfTypeKind, LLVMFloatTypeKind,
            LLVMDoubleTypeKind, LLVMX86_FP80TypeKind, LLVMFP128TypeKind,
            LLVMPPC_FP128TypeKind]);
      end;

   var
      nameVal : AnsiString;
   begin
      // eventually convert integer to float
      if FIsFloatOp <> TypeIsFloat(LLVMGetTypeKind(LLVMTypeOf(val))) then
      begin
         nameVal := LLVMGetValueName(val);
         val := LLVMBuildSIToFP(cg.Builder.Handle, val, cg.FloatType.Handle,
            PAnsiChar('conv_' + nameVal));
      end;
   end;

var
   valL, valR, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TBinaryOpExpr(expr);

   // compile, get and adapt left expression
   cg.CompileNoWrap(e.Left);
   valL := cg.ExpressionToLLVMValue(e.Left);
   AdaptValue(valL);

   // compile, get and adapt right expression
   cg.CompileNoWrap(e.Right);
   valR := cg.ExpressionToLLVMValue(e.Right);
   AdaptValue(valR);

   // some sanity checks
   Assert(Assigned(valL));
   Assert(Assigned(valR));

   // perform binary operation
   valOut := LLVMBuildBinOp(cg.Builder.Handle, FOp, valL, valR, '');
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMMultIntPow2Expr ------------------
// ------------------

procedure TLLVMMultIntPow2Expr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TMultIntPow2Expr;
   valL, valR, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMultIntPow2Expr(expr);

   // compile and get expression
   codeGen.CompileNoWrap(e.Expr);
   valL := cg.ExpressionToLLVMValue(e.Expr);

   // build shift argument
   valR := LLVMConstInt(cg.IntType.Handle, 1 + e.Shift, True);

   // some sanity checks
   Assert(Assigned(valL));
   Assert(Assigned(valR));

   // perform shift
   valOut := LLVMBuildShl(cg.Builder.Handle, valL, valR, '');
   codeGen.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMOpAssignExpr ------------------
// ------------------

constructor TLLVMOpAssignExpr.Create(Op: TLLVMOpcode);
begin
   FOp := Op;
end;

function TLLVMOpAssignExpr.GetLLVMValue(codeGen: TdwsLLVMCodeGen;
  e: TAssignExpr): PLLVMValue;
var
   cg : TdwsLLVMCodeGen;
   valL, valR : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);

   // compile and get left expression
   cg.CompileNoWrap(e.Left);
   valL := cg.ExpressionToLLVMValue(e.Left);

   // compile and get right expression
   cg.CompileNoWrap(e.Right);
   valR := cg.ExpressionToLLVMValue(e.Right);

   // some sanity checks
   Assert(Assigned(valL));
   Assert(Assigned(valR));

   // perform binary operation
   Result := LLVMBuildBinOp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, FOp, valL, valR, '');
end;


// ------------------
// ------------------ TLLVMIncDecVarFuncExpr ------------------
// ------------------

constructor TLLVMIncDecVarFuncExpr.Create(Op: TLLVMOpcode);
begin
   FOp := Op;
end;

procedure TLLVMIncDecVarFuncExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TMagicIteratorFuncExpr;
   valSym, valL, valR, vExpr : PLLVMValue;
   eVar : TVarExpr;
   namVal : AnsiString;
   sym : TDataSymbol;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMagicIteratorFuncExpr(expr);

   if e.SubExpr[0] is TVarExpr then
   begin
      eVar := TVarExpr(e.SubExpr[0]);

      sym := eVar.DataSym;

      if not Assigned(sym) then
         raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [eVar.StackAddr]);

      // retrieve value from symbol and check if present
      valSym := cg.DataSymbolToLLVMValue(sym);
      if not Assigned(valSym) then
         raise ECodeGenException.CreateFmt(RStrValueIsUndefined, [sym.Name]);

      // eventually rename value
      namVal := LLVMGetValueName(valSym);
      valL := LLVMBuildLoad(cg.Builder.Handle, valSym, '');

      // compile expression
      cg.CompileNoWrap(TTypedExpr(e.SubExpr[1]));
      valR := cg.ExpressionToLLVMValue(e.SubExpr[1]);

      // some sanity checks
      Assert(Assigned(valL));
      Assert(Assigned(valR));

      // perform binary operation
      vExpr := LLVMBuildBinOp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, FOp, valL, valR, '');

      // store value (implicit assign)
      LLVMBuildStore(codeGen.Builder.Handle, vExpr, valSym);
   end
   else
      raise ECodeGenException.Create(RStrNotSupported);
end;


// ------------------
// ------------------ TLLVMNotExpr ------------------
// ------------------

procedure TLLVMNotExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TUnaryOpExpr;
   valIn, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TUnaryOpExpr(expr);

   // compile input value
   cg.CompileNoWrap(e.Expr);
   valIn := TdwsLLVMCodeGen(cg).ExpressionToLLVMValue(e.Expr);

   // build output value
   valOut := LLVMBuildNot(TdwsLLVMCodeGen(cg).FBuilder.Handle, valIn, '');
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMUnaryIntFloatOpExpr ------------------
// ------------------

constructor TLLVMUnaryIntFloatOpExpr.Create(IsInteger: Boolean);
begin
   FIsInteger := IsInteger;
end;


// ------------------
// ------------------ TLLVMSqrExpr ------------------
// ------------------

procedure TLLVMSqrExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TUnaryOpExpr;
   valIn, valOut : PLLVMValue;
const
   CMulOps: array [Boolean] of TLLVMOpcode = (LLVMFMul, LLVMMul);
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TUnaryOpExpr(expr);

   // compile input value
   cg.CompileNoWrap(e.Expr);
   valIn := cg.ExpressionToLLVMValue(e.Expr);

   // build output value
   valOut := LLVMBuildBinOp(cg.Builder.Handle, CMulOps[FIsInteger], valIn, valIn, '');
   codeGen.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMNegExpr ------------------
// ------------------

procedure TLLVMNegExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TUnaryOpExpr;
   valIn, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TUnaryOpExpr(expr);

   // compile input value
   cg.CompileNoWrap(e.Expr);
   valIn := cg.ExpressionToLLVMValue(e.Expr);

   // build output value
   if FIsInteger then
      valOut := LLVMBuildNeg(cg.Builder.Handle, valIn, '')
   else
      valOut := LLVMBuildFNeg(cg.Builder.Handle, valIn, '');
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMAbsIntExpr ------------------
// ------------------

procedure TLLVMAbsIntExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TUnaryOpExpr;
   valInput, valOutput, valPhi, valCond : PLLVMValue;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TUnaryOpExpr(expr);

   // compile input
   codeGen.CompileNoWrap(e.Expr);
   valInput := cg.ExpressionToLLVMValue(e.Expr);

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // insert if valInput < 0 then
   valCond := LLVMBuildICmp(cg.Builder.Handle, LLVMIntSLT, valInput,
      LLVMConstNull(cg.IntType.Handle), '');
   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

   // negate input value
   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   valOutput := LLVMBuildNeg(cg.Builder.Handle, valInput, '');
   LLVMBuildBr(cg.Builder.Handle, bbList[2]);

   // build phi value (either input or output)
   cg.Builder.PositionBuilderAtEnd(bbList[2]);
   valPhi := LLVMBuildPhi(cg.Builder.Handle, codeGen.IntType.Handle, 'phi');
   LLVMAddIncoming(valPhi, @valInput, @bbList[0], 1);
   LLVMAddIncoming(valPhi, @valOutput, @bbList[1], 1);
   codeGen.LinkLLVMValueToExpression(valPhi, expr);
end;


// ------------------
// ------------------ TLLVMConvFloatExpr ------------------
// ------------------

procedure TLLVMConvFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TConvFloatExpr;
   valE, valO : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TConvFloatExpr(expr);

   cg.CompileNoWrap(e.Expr);
   valE := cg.ExpressionToLLVMValue(e.Expr);
   valO := LLVMBuildSIToFP(cg.Builder.Handle, valE, cg.FloatType.Handle, '');
   cg.LinkLLVMValueToExpression(valO, expr);
end;


// ------------------
// ------------------ TLLVMRelExpr ------------------
// ------------------


// ------------------
// ------------------ TLLVMRelIntIsZeroExpr ------------------
// ------------------

constructor TLLVMRelIntZeroExpr.Create(Value: Boolean);
begin
   FIsZero := Value;
end;

procedure TLLVMRelIntZeroExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TRelIntIsZeroExpr;
   valL, valR, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TRelIntIsZeroExpr(expr);

   // compile relation expression
   cg.CompileNoWrap(e.Expr);
   valL := cg.ExpressionToLLVMValue(e.Expr);
   valR := LLVMConstNull(codeGen.IntType.Handle);

   // some sanity checks
   Assert(Assigned(valL));
   Assert(Assigned(valR));
   Assert(LLVMGetTypeKind(LLVMTypeOf(valL)) = LLVMIntegerTypeKind);

   // build out value
   valOut := LLVMBuildICmp(cg.Builder.Handle, LLVMIntEQ, valL, valR, '');
   valOut := LLVMBuildNot(cg.Builder.Handle, valOut, '');
   codeGen.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMRelIntExpr ------------------
// ------------------

constructor TLLVMRelIntExpr.Create(Predicate: TLLVMIntPredicate);
begin
   FPredicate := Predicate;
end;

procedure TLLVMRelIntExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TIntegerRelOpExpr;
   valL, valR, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TIntegerRelOpExpr(expr);

   // compile left expression
   cg.CompileNoWrap(e.Left);
   valL := cg.ExpressionToLLVMValue(e.Left);

   // compile right expression
   cg.CompileNoWrap(e.Right);
   valR := cg.ExpressionToLLVMValue(e.Right);

   // some sanity checks
   Assert(Assigned(valL));
   Assert(Assigned(valR));
   Assert(LLVMGetTypeKind(LLVMTypeOf(valL)) = LLVMGetTypeKind(LLVMTypeOf(valR)));

   // build out value
   valOut := LLVMBuildICmp(cg.Builder.Handle, FPredicate, valL, valR, '');
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMRelFloatExpr ------------------
// ------------------

constructor TLLVMRelFloatExpr.Create(Predicate: TLLVMRealPredicate);
begin
   FPredicate := Predicate;
end;

procedure TLLVMRelFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TFloatRelOpExpr;
   valL, valR, valOut : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TFloatRelOpExpr(expr);

   // compile left expression
   cg.CompileNoWrap(e.Left);
   valL := cg.ExpressionToLLVMValue(e.Left);

   // compile right expression
   cg.CompileNoWrap(e.Right);
   valR := cg.ExpressionToLLVMValue(e.Right);

   // some sanity checks
   Assert(Assigned(valL));
   Assert(Assigned(valR));

   // build out value
   valOut := LLVMBuildFCmp(cg.Builder.Handle, FPredicate, valL, valR, '');
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMIfThenElseValueExpr ------------------
// ------------------

procedure TLLVMIfThenElseValueExpr.CodeGen(codeGen: TdwsCodeGen;
  expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TIfThenElseValueExpr;
   bbList : array [0 .. 3] of PLLVMBasicBlock;
   valCond, valTrue, valFalse, valPhi : PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TIfThenElseValueExpr(expr);

   // generate code for condition
   cg.CompileNoWrap(e.CondExpr);
   valCond := cg.ExpressionToLLVMValue(e.CondExpr);

   if (e.TrueExpr is TConstExpr) and (e.FalseExpr is TConstExpr) then
   begin
      cg.CompileNoWrap(e.TrueExpr);
      valTrue :=cg.ExpressionToLLVMValue(e.TrueExpr);
      cg.CompileNoWrap(e.FalseExpr);
      valFalse :=cg.ExpressionToLLVMValue(e.FalseExpr);
      valPhi := LLVMBuildSelect(cg.Builder.Handle, valCond, valTrue, valFalse, '');
      codeGen.LinkLLVMValueToExpression(valPhi, expr);
   end
   else
   begin
      // get, build and position basic blocks
      bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
      bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
         bbList[0], 'then');
      LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
      bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
         bbList[1], 'else');
      LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);
      bbList[3] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
         bbList[2], 'cont');
      LLVMMoveBasicBlockAfter(bbList[3], bbList[2]);

      // build conditional branch (eventually swap blocks in case of a not condition)
      LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

      // locate & compile 'then' block
      cg.Builder.PositionBuilderAtEnd(bbList[1]);
      codeGen.Compile(e.TrueExpr);
      valTrue :=cg.ExpressionToLLVMValue(e.TrueExpr);
      LLVMBuildBr(cg.Builder.Handle, bbList[3]);

      // locate & compile 'else' block
      cg.Builder.PositionBuilderAtEnd(bbList[2]);
      codeGen.Compile(e.FalseExpr);
      valFalse := cg.ExpressionToLLVMValue(e.FalseExpr);
      LLVMBuildBr(cg.Builder.Handle, bbList[3]);

      // locate final block
      cg.Builder.PositionBuilderAtEnd(bbList[3]);
      Assert(LLVMTypeOf(valTrue) = LLVMTypeOf(valFalse));
      valPhi := LLVMBuildPhi(cg.Builder.Handle, LLVMTypeOf(valTrue), '');
      LLVMAddIncoming(valPhi, @valTrue, @bbList[1], 1);
      LLVMAddIncoming(valPhi, @valFalse, @bbList[2], 1);
      codeGen.LinkLLVMValueToExpression(valPhi, expr);
   end;
end;


// ------------------
// ------------------ TLLVMIfExpr ------------------
// ------------------

procedure TLLVMIfExpr.CodeGenCondition(codeGen: TdwsCodeGen;
  condExpr: TTypedExpr; out SwapBlocks: Boolean);
var
   cg : TdwsLLVMCodeGen;
   valOut: PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);

   if condExpr is TNotBoolExpr then
   begin
      SwapBlocks := True;
      cg.CompileNoWrap(TNotBoolExpr(condExpr).Expr);
      valOut := cg.ExpressionToLLVMValue(TNotBoolExpr(condExpr).Expr);
      cg.LinkLLVMValueToExpression(valOut, condExpr);
   end
   else
   begin
      SwapBlocks := False;
      cg.CompileNoWrap(condExpr);
   end;
end;


// ------------------
// ------------------ TLLVMIfThenExpr ------------------
// ------------------

procedure TLLVMIfThenExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TIfThenExpr;

   bbList : array [0 .. 2] of PLLVMBasicBlock;
   valCond : PLLVMValue;
   SwapBlocks : Boolean;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TIfThenExpr(expr);

   // generate code for condition
   CodeGenCondition(cg, e.CondExpr, SwapBlocks);
   valCond := cg.ExpressionToLLVMValue(e.CondExpr);

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0],
      'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1],
      'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   if SwapBlocks then
      LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[2], bbList[1])
   else
      LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

   cg.Builder.PositionBuilderAtEnd(bbList[1]);

   cg.Compile(e.ThenExpr);
   if not e.ThenExpr.InterruptsFlow then
      LLVMBuildBr(cg.Builder.Handle, bbList[2]);

   cg.Builder.PositionBuilderAtEnd(bbList[2]);
end;

function TLLVMIfThenExpr.SubExprIsSafeStatement(sub: TExprBase): Boolean;
begin
   Result := (sub is TFuncExprBase)
      or (sub is TNoResultWrapperExpr)
      or (sub is TAssignExpr)
      or (sub is TFlowControlExpr);
end;


// ------------------
// ------------------ TLLVMIfThenElseExpr ------------------
// ------------------

procedure TLLVMIfThenElseExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TIfThenElseExpr;
   bbList : array [0 .. 3] of PLLVMBasicBlock;
   valCond : PLLVMValue;
   SwapBlocks : Boolean;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TIfThenElseExpr(expr);

   // generate code for condition
   CodeGenCondition(cg, e.CondExpr, SwapBlocks);
   valCond := cg.ExpressionToLLVMValue(e.CondExpr);

   // get, build and position basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'else');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);
   bbList[3] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[2], 'cont');
   LLVMMoveBasicBlockAfter(bbList[3], bbList[2]);

   // build conditional branch (eventually swap blocks in case of a not condition)
   if SwapBlocks then
      LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[2], bbList[1])
   else
      LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

   // locate & compile 'then' block
   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   cg.Compile(e.ThenExpr);
   if not e.ThenExpr.InterruptsFlow then
      LLVMBuildBr(cg.Builder.Handle, bbList[3]);

   // locate & compile 'else' block
   cg.Builder.PositionBuilderAtEnd(bbList[2]);
   cg.Compile(e.ElseExpr);
   if not e.ElseExpr.InterruptsFlow then
      LLVMBuildBr(cg.Builder.Handle, bbList[3]);

   // locate final block
   cg.Builder.PositionBuilderAtEnd(bbList[3]);
end;

function TLLVMIfThenElseExpr.SubExprIsSafeStatement(sub: TExprBase): Boolean;
begin
   Result := (sub is TFuncExprBase) or (sub is TNoResultWrapperExpr)
      or (sub is TAssignExpr) or (sub is TFlowControlExpr);
end;


// ------------------
// ------------------ TLLVMCaseExpr ------------------
// ------------------

procedure TLLVMCaseExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TCaseExpr;
   caseIndex : Integer;
   rangeIndex : Integer;
   caseCond : TCaseCondition;
   valValue, valSwitch : PLLVMValue;
   valCaseVal : PLLVMValue;
   nameCase : AnsiString;
   caseValue : array [0 .. 1] of Int64;
   bbCase : PLLVMBasicBlock;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TCaseExpr(expr);

   cg.CompileNoWrap(e.ValueExpr);
   valValue := cg.ExpressionToLLVMValue(e.ValueExpr);

   // get, build and position basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'else');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // build switch instruction
   valSwitch := LLVMBuildSwitch(cg.Builder.Handle, valValue, bbList[1],
      e.CaseConditions.Count);

   for caseIndex := 0 to e.CaseConditions.Count - 1 do
   begin
      caseCond := TCaseCondition(e.CaseConditions.List[caseIndex]);
      if caseCond is TCompareCaseCondition then
      begin
         if TCompareCaseCondition(caseCond).CompareExpr is TConstIntExpr then
         begin
            // transfer case value as LLVM value
            caseValue[0] :=  TConstIntExpr(TCompareCaseCondition(caseCond).CompareExpr).Value;
            valCaseVal := LLVMConstInt(cg.IntType.Handle, caseValue[0], True);
         end
         else
            raise ECodeGenException.Create(RStrNotSupported);

         // get proper label
         if caseValue[0] < 0 then
            nameCase := 'case__' + AnsiString(IntToStr(-caseValue[0]))
         else
            nameCase := 'case_' + AnsiString(IntToStr(caseValue[0]));

         // insert new case right before 'else' block and add!
         bbCase := LLVMInsertBasicBlockInContext(cg.Context.Handle,
            bbList[1], PAnsiChar(nameCase));
         LLVMAddCase(valSwitch, valCaseVal, bbCase);

         // locate current case
         cg.Builder.PositionBuilderAtEnd(bbCase);

         // compile case
         cg.Compile(TCompareCaseCondition(caseCond).TrueExpr);

         // jump to 'cont' branch (= end of case expression)
         LLVMBuildBr(cg.Builder.Handle, bbList[2]);
      end
      else
      if caseCond is TRangeCaseCondition then
      begin
         // get transfer values
         if TRangeCaseCondition(caseCond).FromExpr is TConstIntExpr then
            caseValue[0] :=  TConstIntExpr(TRangeCaseCondition(caseCond).FromExpr).Value
         else
            raise ECodeGenException.Create(RStrNotSupported);

         if TRangeCaseCondition(caseCond).ToExpr is TConstIntExpr then
            caseValue[1] :=  TConstIntExpr(TRangeCaseCondition(caseCond).ToExpr).Value
         else
            raise ECodeGenException.Create(RStrNotSupported);

         // get proper label (not that meaningful yet)
         nameCase := 'range';

         // insert new case right before 'else' block
         bbCase := LLVMInsertBasicBlockInContext(cg.Context.Handle,
            bbList[1], PAnsiChar(nameCase));

         for rangeIndex := caseValue[0] to caseValue[1] do
         begin
            valCaseVal := LLVMConstInt(cg.IntType.Handle, rangeIndex, True);
            LLVMAddCase(valSwitch, valCaseVal, bbCase);
         end;

         // locate current case
         cg.Builder.PositionBuilderAtEnd(bbCase);

         // compile case
         cg.Compile(TCompareCaseCondition(caseCond).TrueExpr);

         // jump to 'cont' branch (= end of case expression)
         LLVMBuildBr(cg.Builder.Handle, bbList[2]);
      end
      else
         raise ECodeGenException.Create(RStrNotSupported);
   end;

   // locate 'else' branch
   cg.Builder.PositionBuilderAtEnd(bbList[1]);

   // compile else
   cg.Compile(e.ElseExpr);

   // jump to & locate 'cont' branch (= end of case expression)
   LLVMBuildBr(cg.Builder.Handle, bbList[2]);
   cg.Builder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMExitExpr ------------------
// ------------------

procedure TLLVMExitExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   valFunc : PLLVMValue;
   valBlock : PLLVMValue;
   BB : PLLVMBasicBlock;
begin
   valFunc := LLVMGetBasicBlockParent(codeGen.Builder.GetInsertBlock);

   // locate result basic block and branch if found, then exit
   BB := LLVMGetFirstBasicBlock(valFunc);
   while Assigned(BB) do
   begin
      valBlock := LLVMBasicBlockAsValue(BB);
      if LLVMGetValueName(valBlock) = 'result' then
      begin
         LLVMBuildBr(codeGen.Builder.Handle, BB);
         Exit;
      end;
      BB := LLVMGetNextBasicBlock(BB);
   end;

   // insert plain return
   LLVMBuildRetVoid(codeGen.Builder.Handle);
end;


// ------------------
// ------------------ TLLVMExitValueExpr ------------------
// ------------------

procedure TLLVMExitValueExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TExitValueExpr;
   valOut : PLLVMValue;
begin
   e := TExitValueExpr(expr);
   codeGen.Compile(e.AssignExpr);
   valOut := codeGen.ExpressionToLLVMValue(e.AssignExpr);
   LLVMBuildRet(codeGen.Builder.Handle, valOut);
end;


// ------------------
// ------------------ TLLVMForExpr ------------------
// ------------------

constructor TLLVMForExpr.Create(Reverse: Boolean);
begin
   FReverse := Reverse;
end;

procedure TLLVMForExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TForExpr;

   useInt32 : Boolean;
   bounds : array [0 .. 1] of Int64;
   indexType : PLLVMType;
   valPhi, valNext, valFrom, valTo, valStep, valCond : PLLVMValue;
   loopVarSym : TDataSymbol;
   nameVal : AnsiString;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
   bbNext : PLLVMBasicBlock;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TForExpr(expr);

   // check if we can use int32 instead of int64
   useInt32 := (e.FromExpr is TConstIntExpr) and (e.ToExpr is TConstIntExpr);
   if useInt32 then
   begin
      // check if we can really use int32 instead of int64
      bounds[0] := TConstIntExpr(e.FromExpr).Value;
      bounds[1] := TConstIntExpr(e.ToExpr).Value;
      if FReverse then
         useInt32 := (bounds[0] <= High(Integer)) and (bounds[1] >= Low(Integer))
      else
         useInt32 := (bounds[0] >= Low(Integer)) and (bounds[1] <= High(Integer));

      // additional check for step loop
      if useInt32 and (expr is TForStepExpr) then
         useInt32 := (TForStepExpr(expr).StepExpr is TConstIntExpr) and
            (TConstIntExpr(TForStepExpr(expr).StepExpr).Value < High(Integer));
   end;

   if useInt32 then
   begin
      // simply transfer constant values to int32 LLVM values
      indexType := LLVMInt32TypeInContext(cg.Context.Handle);
      valFrom := LLVMConstInt(indexType, bounds[0], True);
      valTo := LLVMConstInt(indexType, bounds[1], True);
   end
   else
   begin
      // use native int type
      indexType := cg.IntType.Handle;

      // calculate 'from' and 'to'
      cg.CompileNoWrap(e.FromExpr);
      valFrom := cg.ExpressionToLLVMValue(e.FromExpr);
      cg.CompileNoWrap(e.ToExpr);
      valTo := cg.ExpressionToLLVMValue(e.ToExpr);
   end;

   // check if for loop uses a dedicated step
   if expr is TForStepExpr then
   begin
      // either transfer constant value or compile step value
      if useInt32 then
         valStep := LLVMConstInt(indexType, TConstIntExpr(TForStepExpr(expr).StepExpr).Value, True)
      else
      begin
         cg.CompileNoWrap(TForStepExpr(expr).StepExpr);
         valStep := cg.ExpressionToLLVMValue(TForStepExpr(expr).StepExpr);
      end;
   end
   else
      valStep := LLVMConstInt(indexType, 1, True);

   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'for_loop');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'loop_done');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // perform comparison in
   valCond := LLVMBuildICmp(cg.Builder.Handle, LLVMIntSLE, valFrom, valTo, 'loopcond');

   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

   loopVarSym := e.VarExpr.DataSym;
   nameVal := AnsiString(loopVarSym.Name);
   if not Assigned(loopVarSym) then
      raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [e.VarExpr.StackAddr]);

   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   valPhi := LLVMBuildPhi(cg.Builder.Handle, indexType, PAnsiChar(nameVal));
   cg.LinkLLVMValueToDataSymbol(valPhi, loopVarSym);

   LLVMAddIncoming(valPhi, @valFrom, @bbList[0], 1);

   // compile inner program
   cg.Compile(e.DoExpr);

   // advance index
   if FReverse then
      valNext := LLVMBuildSub(cg.Builder.Handle, valPhi, valStep, PAnsiChar(nameVal))
   else
      valNext := LLVMBuildAdd(cg.Builder.Handle, valPhi, valStep, PAnsiChar(nameVal));
   bbNext := LLVMGetInstructionParent(valNext);
   LLVMAddIncoming(valPhi, @valNext, @bbNext, 1);
   cg.LinkLLVMValueToDataSymbol(valNext, e.VarExpr.DataSym);

   // build conditional branch (the actual looping)
   valCond := LLVMBuildICmp(cg.Builder.Handle, LLVMIntEQ, valPhi, valTo, 'loopcond');
   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[2], bbList[1]);

   LLVMPositionBuilderBefore(cg.Builder.Handle, valCond);
   cg.Builder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMLoopExpr ------------------
// ------------------


// ------------------
// ------------------ TLLVMRepeatExpr ------------------
// ------------------

procedure TLLVMRepeatExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TRepeatExpr;
   valCond : PLLVMValue;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TRepeatExpr(expr);

   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'repeat_loop');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'loop_done');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildBr(cg.Builder.Handle, bbList[1]);
   cg.Builder.PositionBuilderAtEnd(bbList[1]);

   // compile inner program
   cg.Compile(e.LoopExpr);

   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   cg.CompileNoWrap(e.CondExpr);
   valCond := cg.ExpressionToLLVMValue(e.CondExpr);

   LLVMSetValueName(valCond, 'repeat_cond');

   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[2], bbList[1]);

   cg.Builder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMWhileExpr ------------------
// ------------------

procedure TLLVMWhileExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TWhileExpr;

   valCond : PLLVMValue;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
begin
   // TODO !!!

   cg := TdwsLLVMCodeGen(codeGen);
   e := TWhileExpr(expr);

   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'repeat_loop');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'loop_done');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildBr(cg.Builder.Handle, bbList[1]);
   cg.Builder.PositionBuilderAtEnd(bbList[1]);

   // compile inner program
   cg.Compile(e.LoopExpr);

   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   cg.CompileNoWrap(e.CondExpr);
   valCond := cg.ExpressionToLLVMValue(e.CondExpr);

   LLVMSetValueName(valCond, 'while_cond');

   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[2], bbList[1]);

   cg.Builder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMFuncBaseExpr ------------------
// ------------------

procedure TLLVMFuncBaseExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TFuncExprBase;
   index : Integer;
   funcSym : TFuncSymbol;
   params : TParamsSymbolTable;
   paramExpr : TTypedExpr;

   argCount : Integer;
   argTypes : PLLVMTypePtrArray;
   argValues : PLLVMValuePtrArray;

   valFunc : PLLVMValue;
   valOut : PLLVMValue;

   nameFunc : AnsiString;
   typeFunc : PLLVMType;
   typeRet : PLLVMType;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TFuncExprBase(expr);
   funcSym := e.FuncSym;
   params := e.FuncSym.Params;

   valFunc := cg.FunctionSymbolToLLVMValue(e.FuncSym);

   argCount := e.Args.Count;
   Assert(e.Args.Count = params.Count);

   // function not found -> probably external? -> workaround solution (todo)
   if not Assigned(valFunc) then
   begin
      nameFunc := AnsiString(funcSym.Name);
      if Assigned(funcSym.Result) then
         typeRet := cg.TypeSymbolToLLVMType(funcSym.Result.Typ)
      else
         typeRet := LLVMVoidTypeInContext(cg.Context.Handle);

      GetMem(argTypes, argCount * SizeOf(PLLVMType));
      for index := 0 to argCount - 1 do
         argTypes^[index] := cg.TypeSymbolToLLVMType(params.Symbols[index].Typ);

      typeFunc := LLVMFunctionType(typeRet, argTypes, argCount, False);

      valFunc := LLVMGetNamedFunction(cg.Module.Handle, PAnsiChar(nameFunc));
      if not Assigned(valFunc) then
      begin
         valFunc := LLVMAddFunction(cg.Module.Handle, PAnsiChar(nameFunc),
            typeFunc);

         LLVMSetLinkage(valFunc, LLVMExternalLinkage);
         LLVMSetFunctionCallConv(valFunc, Ord(LLVMCCallConv));
      end;
   end;

   GetMem(argValues, argCount * SizeOf(PLLVMValue));
   for index := 0 to argCount - 1 do
   begin
      paramExpr := e.Args.ExprBase[index] as TTypedExpr;
      cg.CompileNoWrap(paramExpr);
      argValues[index] := cg.ExpressionToLLVMValue(paramExpr);
   end;

   valOut := LLVMBuildCall(cg.Builder.Handle, valFunc, argValues, argCount, '');
   cg.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMMagicFuncExpr ------------------
// ------------------

constructor TLLVMMagicFuncExpr.Create;
begin
   FMagicCodeGens := TStringList.Create;
   FMagicCodeGens.CaseSensitive := False;
   FMagicCodeGens.Sorted := True;
   FMagicCodeGens.Duplicates := dupError;

   FMagicCodeGens.AddObject('PrintLn', TLLVMPrintLnExpr.Create);
   FMagicCodeGens.AddObject('Sqrt', TLLVMBaseFloatFuncExpr.Create('sqrt'));
   FMagicCodeGens.AddObject('Sin', TLLVMBaseFloatFuncExpr.Create('sin'));
   FMagicCodeGens.AddObject('Cos', TLLVMBaseFloatFuncExpr.Create('cos'));
   FMagicCodeGens.AddObject('Tan', TLLVMTanFloatExpr.Create);
   FMagicCodeGens.AddObject('Power', TLLVMPowFloatExpr.Create);
   FMagicCodeGens.AddObject('Exp', TLLVMBaseFloatFuncExpr.Create('exp'));
   FMagicCodeGens.AddObject('Ln', TLLVMBaseFloatFuncExpr.Create('log'));
   FMagicCodeGens.AddObject('Log10', TLLVMBaseFloatFuncExpr.Create('log10'));
   FMagicCodeGens.AddObject('Log2', TLLVMBaseFloatFuncExpr.Create('log2'));

   FMagicCodeGens.AddObject('Floor', TLLVMBaseFloatIntFuncExpr.Create('floor'));
   FMagicCodeGens.AddObject('Ceil', TLLVMBaseFloatIntFuncExpr.Create('ceil'));
   FMagicCodeGens.AddObject('Trunc', TLLVMBaseFloatIntFuncExpr.Create('trunc'));
   FMagicCodeGens.AddObject('Round', TLLVMBaseFloatIntFuncExpr.Create('rint'));

   FMagicCodeGens.AddObject('Max$_Float_Float_', TLLVMMinMaxFloatExpr.Create(True));
   FMagicCodeGens.AddObject('Max$_Integer_Integer_', TLLVMMinMaxIntExpr.Create(True));
   FMagicCodeGens.AddObject('Min$_Float_Float_', TLLVMMinMaxFloatExpr.Create(False));
   FMagicCodeGens.AddObject('Min$_Integer_Integer_', TLLVMMinMaxIntExpr.Create(False));
end;

destructor TLLVMMagicFuncExpr.Destroy;
var
   i : Integer;
begin
   inherited;
   for i := 0 to FMagicCodeGens.Count-1 do
      FMagicCodeGens.Objects[i].Free;
   FMagicCodeGens.Free;
end;

procedure TLLVMMagicFuncExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);

   function GetSignature(funcSym : TFuncSymbol) : String;
   var
      i : Integer;
   begin
      Result := funcSym.QualifiedName+'$_';
      for i := 0 to funcSym.Params.Count-1 do
         Result := Result+funcSym.GetParamType(i).Name+'_';
   end;

var
   e : TMagicFuncExpr;
   name : String;
   i : Integer;
begin
   e := TMagicFuncExpr(expr);
   if e.FuncSym.IsOverloaded then
      name := GetSignature(e.FuncSym)
   else
      name := e.FuncSym.QualifiedName;

   if cgoNoInlineMagics in codeGen.Options then
      i := -1
   else i := FMagicCodeGens.IndexOf(name);

   if i >= 0 then
      TdwsExprCodeGen(FMagicCodeGens.Objects[i]).CodeGen(codeGen, expr)
   else
   begin
      codeGen.Dependencies.Add(name);
      inherited;
   end;
end;


// ------------------
// ------------------ TLLVMPrintLnExpr ------------------
// ------------------

procedure TLLVMPrintLnExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TMagicFuncExpr;
   a : TExprBase;
   c : TConstStringExpr;
   name : AnsiString;
   typeAnsiChar : TLLVMInt8Type;
   typePAnsiChar : TLLVMPointerType;
   typePuts : PLLVMType;
   valParam, valFuncPuts, valG, valStr, valOut, valGEP : PLLVMValue;
   valPtrArray : array [0 .. 1] of PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMagicFuncExpr(expr);

   typeAnsiChar := TLLVMInt8Type.Create(cg.Context);
   typePAnsiChar := TLLVMPointerType.Create(typeAnsiChar.Handle, 0);
   typePuts := LLVMFunctionType(cg.IntType.Handle, @typePAnsiChar.Handle, 1, False);

   valFuncPuts := LLVMGetNamedFunction(cg.Module.Handle, 'puts');
   if not Assigned(valFuncPuts) then
   begin
      valFuncPuts := LLVMAddFunction(cg.Module.Handle, 'puts', typePuts);

      valParam := LLVMGetParam(valFuncPuts, 0);
      LLVMAddAttribute(valParam, LLVMNoCaptureAttribute);

      LLVMSetLinkage(valFuncPuts, LLVMExternalLinkage);
      LLVMSetFunctionCallConv(valFuncPuts, Ord(LLVMCCallConv));
   end;
   LLVMAddFunctionAttr(valFuncPuts, LLVMNoUnwindAttribute);

   a := e.Args[0];

   if a is TConstStringExpr then
   begin
      c := TConstStringExpr(a);
      name := AnsiString(c.Value);
      name := name + #0;

      // global variable declaration and initialization
      valStr := LLVMConstStringInContext(
        cg.Context.Handle, PAnsiChar(name), Length(name), True);
      valG := LLVMAddGlobal(cg.Module.Handle,
         LLVMArrayType(typeAnsiChar.Handle, Length(name)), 'printf_static');
      LLVMSetLinkage(valG, LLVMInternalLinkage);
      LLVMSetGlobalConstant(valG, True);
      LLVMSetInitializer(valG, valStr);

      valPtrArray[0] := LLVMConstNull(cg.IntType.Handle);
      valPtrArray[1] := valPtrArray[0];
      valGEP := LLVMBuildGEP(cg.Builder.Handle, valG, @valPtrArray[0], 2, 'cast');

      valOut := LLVMBuildCall(cg.Builder.Handle, valFuncPuts, @valGEP, 1, 'puts_result');
      cg.LinkLLVMValueToExpression(valOut, expr);
      LLVMSetInstructionCallConv(valOut, Ord(LLVMCCallConv));
   end
   else
   begin
      if a is TTypedExpr then
         cg.Compile(a);
      valOut := cg.ExpressionToLLVMValue(a);
      if LLVMGetTypeKind(LLVMTypeOf(valOut)) = LLVMIntegerTypeKind then
      begin
         // convert integer to string here
         // probably compare code with:
         //   https://bitbucket.org/carli/gwscript/src/tip/libs/strings.ll

      end;
   end;
end;


// ------------------
// ------------------ TLLVMBaseFloatFuncExpr ------------------
// ------------------

constructor TLLVMBaseFloatFuncExpr.Create(IntrisicName: AnsiString);
begin
   FIntrisicName := IntrisicName;
end;

procedure TLLVMBaseFloatFuncExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TMagicFuncExpr;
   valI, valO, valParam, valFunc : PLLVMValue;
   typeFunc : PLLVMType;
   nameVal : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMagicFuncExpr(expr);

   typeFunc := LLVMFunctionType(cg.FloatType.Handle, @CodeGen.FloatType.Handle, 1, False);

   // check if intrinsics declaration is present, otherwise declare
   nameVal := 'llvm.' + FIntrisicName + '.f64';
   valFunc := LLVMGetNamedFunction(cg.Module.Handle, PAnsiChar(nameVal));
   if not Assigned(valFunc) then
   begin
      valFunc := LLVMAddFunction(cg.Module.Handle, PAnsiChar(nameVal), typeFunc);
      valParam := LLVMGetParam(valFunc, 0);
      nameVal := 'Val';
      LLVMSetValueName(valParam, PAnsiChar(nameVal));
   end;

   // compile and get input
   cg.Compile(e.Args[0]);
   valI := cg.ExpressionToLLVMValue(e.Args[0]);

   // get value name
   nameVal := LLVMGetValueName(valI);
   nameVal := FIntrisicName + '_' + nameVal;

   // build output
   valO := LLVMBuildCall(cg.Builder.Handle, valFunc, @valI, 1, PAnsiChar(nameVal));
   codeGen.LinkLLVMValueToExpression(valO, expr);
end;


// ------------------
// ------------------ TLLVMBaseFloatIntFuncExpr ------------------
// ------------------

procedure TLLVMBaseFloatIntFuncExpr.CodeGen(codeGen: TdwsCodeGen;
  expr: TExprBase);
var
   valIn, valOut : PLLVMValue;
   nameVal : AnsiString;
begin
   inherited;

   // get in value and name
   valIn := codeGen.ExpressionToLLVMValue(expr);
   nameVal := LLVMGetValueName(valIn);
   nameVal := 'conv_' + nameVal;

   // build out value
   valOut := LLVMBuildFPToSI(codeGen.Builder.Handle, valIn, codeGen.IntType.Handle, PAnsiChar(nameVal));
   codeGen.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMAbsFloatExpr ------------------
// ------------------

procedure TLLVMAbsFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TAbsFloatExpr;
   valI, valO, valParam, valFunc : PLLVMValue;
   typeFunc : PLLVMType;
   nameVal : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TAbsFloatExpr(expr);

   typeFunc := LLVMFunctionType(cg.FloatType.Handle, @cg.FloatType.Handle, 1, False);

   // check if fabs intrinsics declaration is present, otherwise declare
   nameVal := 'llvm.fabs.f64';
   valFunc := LLVMGetNamedFunction(cg.Module.Handle, PAnsiChar(nameVal));
   if not Assigned(valFunc) then
   begin
      valFunc := LLVMAddFunction(cg.Module.Handle, PAnsiChar(nameVal), typeFunc);
      valParam := LLVMGetParam(valFunc, 0);
      nameVal := 'Val';
      LLVMSetValueName(valParam, PAnsiChar(nameVal));
   end;

   // compile and get input
   cg.Compile(e.Expr);
   valI := codeGen.ExpressionToLLVMValue(e.Expr);

   // get value name
   nameVal := LLVMGetValueName(valI);
   nameVal := 'fabs_' + nameVal;

   // build output
   valO := LLVMBuildCall(cg.Builder.Handle, valFunc, @valI, 1, PAnsiChar(nameVal));
   codeGen.LinkLLVMValueToExpression(valO, expr);
end;


// ------------------
// ------------------ TLLVMTanFloatExpr ------------------
// ------------------

procedure TLLVMTanFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   cg : TdwsLLVMCodeGen;
   e : TAbsFloatExpr;
   valI, valO, valL, valR, valParam, valFunc : PLLVMValue;
   typeFunc : PLLVMType;
   nameVal, nameInstr : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TAbsFloatExpr(expr);

   typeFunc := LLVMFunctionType(cg.FloatType.Handle, @cg.FloatType.Handle, 1, False);

   // check if sin intrinsics is present, otherwise declare
   nameVal := 'llvm.sin.f64';
   valFunc := LLVMGetNamedFunction(cg.Module.Handle, PAnsiChar(nameVal));
   if not Assigned(valFunc) then
   begin
      valFunc := LLVMAddFunction(cg.Module.Handle, PAnsiChar(nameVal), typeFunc);
      valParam := LLVMGetParam(valFunc, 0);
      nameVal := 'Val';
      LLVMSetValueName(valParam, PAnsiChar(nameVal));
   end;

   // check if cos intrinsics is present, otherwise declare
   nameVal := 'llvm.cos.f64';
   valFunc := LLVMGetNamedFunction(cg.Module.Handle, PAnsiChar(nameVal));
   if not Assigned(valFunc) then
   begin
      valFunc := LLVMAddFunction(cg.Module.Handle, PAnsiChar(nameVal), typeFunc);
      valParam := LLVMGetParam(valFunc, 0);
      nameVal := 'Val';
      LLVMSetValueName(valParam, PAnsiChar(nameVal));
   end;

   // compile and get input
   cg.Compile(e.Expr);
   valI := codeGen.ExpressionToLLVMValue(e.Expr);
   nameVal := LLVMGetValueName(valI);

   nameInstr := 'sin_' + nameVal;
   valL := LLVMBuildCall(cg.Builder.Handle, valFunc, @valI, 1, PAnsiChar(nameInstr));

   nameInstr := 'cos_' + nameVal;
   valR := LLVMBuildCall(cg.Builder.Handle, valFunc, @valI, 1, PAnsiChar(nameInstr));

   // build tan by calculating sin / cos (not the best idea)
   nameInstr := 'tan_' + nameVal;
   valO := LLVMBuildFDiv(cg.Builder.Handle, valL, valR, PAnsiChar(nameInstr));
   codeGen.LinkLLVMValueToExpression(valO, expr);
end;


// ------------------
// ------------------ TLLVMPowFloatExpr ------------------
// ------------------

procedure TLLVMPowFloatExpr.CodeGen(codeGen: TdwsCodeGen;
  expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TMagicFuncExpr;
   typeIn : array [0 .. 1] of PLLVMType;
   valIn : array [0 .. 1] of PLLVMValue;
   valOut, valParam, valFunc : PLLVMValue;
   typeFunc : PLLVMType;
   nameVal : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMagicFuncExpr(expr);

   typeIn[0] := cg.FloatType.Handle;
   typeIn[1] := cg.FloatType.Handle;
   typeFunc := LLVMFunctionType(cg.FloatType.Handle, @typeIn, 2, False);

   // check if pow intrinsics is present, otherwise declare
   nameVal := 'llvm.pow.f64';
   valFunc := LLVMGetNamedFunction(cg.Module.Handle, PAnsiChar(nameVal));
   if not Assigned(valFunc) then
   begin
      valFunc := LLVMAddFunction(cg.Module.Handle, PAnsiChar(nameVal), typeFunc);
      valParam := LLVMGetParam(valFunc, 0);
      nameVal := 'Val';
      LLVMSetValueName(valParam, PAnsiChar(nameVal));
   end;

   // compile value
   cg.Compile(e.Args[0]);
   valIn[0] := cg.ExpressionToLLVMValue(e.Args[0]);

   // compile power
   cg.Compile(e.Args[1]);
   valIn[1] := cg.ExpressionToLLVMValue(e.Args[1]);

   nameVal := LLVMGetValueName(valIn[0]);
   nameVal := 'pow' + nameVal;
   valOut := LLVMBuildCall(cg.Builder.Handle, valFunc, @valIn, 2, PAnsiChar(nameVal));
   codeGen.LinkLLVMValueToExpression(valOut, expr);
end;


// ------------------
// ------------------ TLLVMMinMaxIntExpr ------------------
// ------------------

constructor TLLVMMinMaxIntExpr.Create(isMax: Boolean);
begin
   if isMax then
      FPredicate := LLVMIntSGT
   else
      FPredicate := LLVMIntSLT
end;

procedure TLLVMMinMaxIntExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TMagicFuncExpr;
   valA, valB, valNull, valPhi, valCond : PLLVMValue;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
   nameVal : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMagicFuncExpr(expr);

   if e.Args.Count <> 2 then
      raise ECodeGenException.Create(RStrWrongNumberOfArgs);

   // compile value
   cg.Compile(e.Args[0]);
   valA := cg.ExpressionToLLVMValue(e.Args[0]);

   cg.Compile(e.Args[1]);
   valB := cg.ExpressionToLLVMValue(e.Args[1]);

   if not (LLVMGetTypeKind(LLVMTypeOf(valB)) = LLVMIntegerTypeKind) then
      raise ECodeGenException.Create('Type mismatch');

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // build condition and branch
   valCond := LLVMBuildICmp(cg.Builder.Handle, FPredicate, valA, valB, '');
   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

   // goto then block and build value
   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   nameVal := LLVMGetValueName(valB);
   valNull := LLVMConstNull(cg.IntType.Handle);
   valB := LLVMBuildAdd(cg.Builder.Handle, valB, valNull, PAnsiChar(nameVal));
   LLVMBuildBr(cg.Builder.Handle, bbList[2]);

   // goto phi block and build value
   cg.Builder.PositionBuilderAtEnd(bbList[2]);
   valPhi := LLVMBuildPhi(cg.Builder.Handle, cg.IntType.Handle, 'phi');
   LLVMAddIncoming(valPhi, @valA, @bbList[0], 1);
   LLVMAddIncoming(valPhi, @valB, @bbList[1], 1);
   cg.LinkLLVMValueToExpression(valPhi, expr);
end;

procedure TLLVMMinMaxIntExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
   expr: TTypedExpr);
begin
   Self.CodeGen(codeGen, expr);
end;


// ------------------
// ------------------ TLLVMMinMaxFloatExpr ------------------
// ------------------

constructor TLLVMMinMaxFloatExpr.Create(isMax: Boolean);
begin
   if isMax then
      FPredicate := LLVMRealOGT
   else
      FPredicate := LLVMRealOLT;
end;

procedure TLLVMMinMaxFloatExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   cg : TdwsLLVMCodeGen;
   e : TMagicFuncExpr;
   valA, valB, valNull, valPhi, valCond : PLLVMValue;
   bbList : array [0 .. 2] of PLLVMBasicBlock;
   nameVal : AnsiString;
begin
   cg := TdwsLLVMCodeGen(codeGen);
   e := TMagicFuncExpr(expr);

   if e.Args.Count <> 2 then
      raise ECodeGenException.Create(RStrWrongNumberOfArgs);

   // compile value
   cg.Compile(e.Args[0]);
   valA := cg.ExpressionToLLVMValue(e.Args[0]);

   cg.Compile(e.Args[1]);
   valB := cg.ExpressionToLLVMValue(e.Args[1]);

   if not (LLVMGetTypeKind(LLVMTypeOf(valB)) = LLVMFloatTypeKind) then
      raise ECodeGenException.Create('Type mismatch');

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(cg.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // build condition and branch
   valCond := LLVMBuildFCmp(cg.Builder.Handle, FPredicate, valA, valB, '');
   LLVMBuildCondBr(cg.Builder.Handle, valCond, bbList[1], bbList[2]);

   // goto then block and build value
   cg.Builder.PositionBuilderAtEnd(bbList[1]);
   nameVal := LLVMGetValueName(valB);
   valNull := LLVMConstNull(cg.FloatType.Handle);
   valB := LLVMBuildFAdd(cg.Builder.Handle, valB, valNull, PAnsiChar(nameVal));
   LLVMBuildBr(cg.Builder.Handle, bbList[2]);

   // goto phi block and build value
   cg.Builder.PositionBuilderAtEnd(bbList[2]);
   valPhi := LLVMBuildPhi(cg.Builder.Handle, cg.FloatType.Handle, 'phi');
   LLVMAddIncoming(valPhi, @valA, @bbList[0], 1);
   LLVMAddIncoming(valPhi, @valB, @bbList[1], 1);
   cg.LinkLLVMValueToExpression(valPhi, expr);
end;

procedure TLLVMMinMaxFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
begin
   Self.CodeGen(codeGen, expr);
end;

end.
