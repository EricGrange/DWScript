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

         {$IFNDEF LLVM_TAG}
         FdwsLLVMDict: TdwsExpressionLLVMValueDictionary;
         FdwsLLVMDataSymbolDict: TdwsDataSymbolLLVMValueDictionary;
         {$ENDIF}

         FVerifyModule: Boolean;
         FOptimizations: TdwsLLVMOptimizations;
         FCustomPasses: TdwsLLVMCustomPasses;

         function TypeSymbolToLLVMType(TypeSymbol: TTypeSymbol): PLLVMType;
      protected
         procedure DoCompileFuncSymbol(func: TFuncSymbol;
            deAnonymize: Boolean = False); override;
         procedure LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue;
            DataSymbol: TDataSymbol); inline;
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

   TLLVMVarParamExpr = class (TLLVMVarExpr)
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
      public
         function TypeSymbolToLLVMType(TypeSymbol: TTypeSymbol): PLLVMType; inline;
{$IFNDEF LLVM_TAG}
         function DataSymbolToLLVMValue(DataSymbol: TDataSymbol): PLLVMValue;
         function ExpressionToLLVMValue(Expression: TExprBase): PLLVMValue;
         procedure LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue; DataSymbol: TDataSymbol);
         procedure LinkLLVMValueToExpression(LLVMValue: PLLVMValue; Expression: TExprBase);
{$ENDIF}
         property BoolType: TLLVMInt1Type read GetBoolType;
         property IntType: TLLVMInt64Type read GetIntType;
         property FloatType: TLLVMDoubleType read GetFloatType;

         property Builder: TLLVMBuilder read GetBuilder;

         property Context: TLLVMContext read GetContext;
         property Module: TLLVMModule read GetModule;
   end;


// ------------------
// ------------------ TdwsExpressionLLVMValueDictionary ------------------
// ------------------

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

{$IFNDEF LLVM_TAG}
procedure TCodeGenHelper.LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue;
  DataSymbol: TDataSymbol);
begin
   TdwsLLVMCodeGen(Self).FdwsLLVMDataSymbolDict.LinkLLVMValueToDataSymbol(LLVMValue,
      DataSymbol);
end;

procedure TCodeGenHelper.LinkLLVMValueToExpression(LLVMValue: PLLVMValue;
  Expression: TExprBase);
begin
   TdwsLLVMCodeGen(Self).FdwsLLVMDict.LinkLLVMValueToObject(LLVMValue,
      Expression);
end;

function TCodeGenHelper.DataSymbolToLLVMValue(DataSymbol: TDataSymbol): PLLVMValue;
begin
   Result := TdwsLLVMCodeGen(Self).FdwsLLVMDataSymbolDict.GetLLVMValue(DataSymbol);
end;

function TCodeGenHelper.ExpressionToLLVMValue(Expression: TExprBase): PLLVMValue;
begin
   Result := TdwsLLVMCodeGen(Self).FdwsLLVMDict.GetLLVMValue(Expression)
end;

function TCodeGenHelper.TypeSymbolToLLVMType(
  TypeSymbol: TTypeSymbol): PLLVMType;
begin
   Result := TdwsLLVMCodeGen(Self).TypeSymbolToLLVMType(TypeSymbol);
end;


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
   RegisterCodeGen(TAssignConstToBoolVarExpr, TLLVMAssignConstVarExpr.Create(btBool));
   RegisterCodeGen(TAssignConstToIntegerVarExpr, TLLVMAssignConstVarExpr.Create(btInt));
   RegisterCodeGen(TAssignConstToFloatVarExpr, TLLVMAssignConstVarExpr.Create(btFloat));

   RegisterCodeGen(TVarExpr, TLLVMVarExpr.Create);
   RegisterCodeGen(TVarParamExpr, TLLVMVarParamExpr.Create);

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
{$ENDIF}
end;

// Destroy
//
destructor TdwsLLVMCodeGen.Destroy;
begin
{$IFNDEF LLVM_TAG}
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

// LinkLLVMValueToDataSymbol
//
procedure TdwsLLVMCodeGen.LinkLLVMValueToDataSymbol(LLVMValue: PLLVMValue;
   DataSymbol: TDataSymbol);
begin
{$IFDEF LLVM_TAG}
      DataSymbol.LLVMValue := LLVMValue;
{$ELSE}
      FdwsLLVMDataSymbolDict.LinkLLVMValueToDataSymbol(LLVMValue, DataSymbol);
{$ENDIF}
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
  arrSym: TStaticArraySymbol;
  arrayType: TLLVMArrayType;
  structType: TLLVMStructType;
  str: AnsiString;
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
      str := AnsiString(arrSym.Name);
      arrayType := TLLVMArrayType.Create(TypeSymbolToLLVMType(arrSym.Typ), arrSym.ElementCount);
      Result := arrayType.Handle;
   end
   else
   if TypeSymbol is TRecordSymbol then
   begin
      recSym := TRecordSymbol(TypeSymbol);
      str := AnsiString(recSym.Name);
      structType := TLLVMStructType.Create(FContext, PAnsiChar(str));
      SetLength(ElementTypes, recSym.Members.Count);
      for index := 0 to Length(ElementTypes) - 1 do
        ElementTypes[index] := TypeSymbolToLLVMType(recSym.Members[index].Typ);

      structType.SetBody(@ElementTypes[0], Length(ElementTypes));
      Result := structType.Handle;
   end
   else
      Result := nil;
end;

// CompileProgramBody
//
procedure TdwsLLVMCodeGen.CompileProgramBody(expr: TProgramExpr);
var
   int32Type : PLLVMType;
   typeMain : PLLVMType;
   funcName: AnsiString;
   funcMain : PLLVMValue;
   allocBlock: PLLVMBasicBlock;
   entryBlock: PLLVMBasicBlock;
   resultBlock: PLLVMBasicBlock;
   builder: TLLVMBuilder;
   brValue: PLLVMValue;
   retValue: PLLVMValue;
begin
   // setup main function type
   int32Type := LLVMInt32TypeInContext(FContext.Handle);
   typeMain := LLVMFunctionType(int32Type, nil, 0, False);

   // setup function name for body -> 'main', performs no checks if already taken!
   funcName := 'main';

   // check if main function already exists, if not add function
   funcMain := LLVMGetNamedFunction(FModule.Handle, PAnsiChar(funcName));
   if not Assigned(funcMain) then
   begin
      funcMain := LLVMAddFunction(FModule.Handle, PAnsiChar(funcName), typeMain);
      LLVMSetLinkage(funcMain, LLVMExternalLinkage);
      LLVMSetFunctionCallConv(funcMain, Ord(LLVMCCallConv));
   end;

   // build blocks
   allocBlock := LLVMAppendBasicBlockInContext(FContext.Handle, funcMain, 'alloc');
   entryBlock := LLVMAppendBasicBlockInContext(FContext.Handle, funcMain, 'entry');
   resultBlock := LLVMAppendBasicBlockInContext(FContext.Handle, funcMain, 'result');

   // create builder (essential to define function bodies)
   builder := TLLVMBuilder.Create;
   try
      // build empty alloc block
      builder.PositionBuilderAtEnd(allocBlock);
      LLVMBuildBr(builder.Handle, entryBlock);

      // locate entry block
      builder.PositionBuilderAtEnd(entryBlock);
      FBuilder := builder;

      inherited;

      // branch to return result section
      brValue := LLVMBuildBr(builder.Handle, resultBlock);
      LLVMMoveBasicBlockAfter(resultBlock, LLVMGetInstructionParent(brValue));
      builder.PositionBuilderAtEnd(resultBlock);

      // return result (= zero)
      retValue := LLVMConstNull(int32Type);
      LLVMBuildRet(builder.Handle, retValue);
   finally
      builder.Free;
   end;
end;

// DoCompileFuncSymbol
//
procedure TdwsLLVMCodeGen.DoCompileFuncSymbol(func: TFuncSymbol;
  deAnonymize: Boolean);
var
   retType: PLLVMType;
   params: TParamsSymbolTable;
   argTypes: array of PLLVMType;
   argCount: Cardinal;
   funcType: PLLVMType;
   funcName: AnsiString;
   funcVal: PLLVMValue;
   argValue: PLLVMValue;
   argValues: array of PLLVMValue;
   argName: AnsiString;
   retValue: PLLVMValue;
   brValue: PLLVMValue;
   index: Integer;
   allocBranch: PLLVMValue;
   allocBlock: PLLVMBasicBlock;
   entryBlock: PLLVMBasicBlock;
   resultBlock: PLLVMBasicBlock;
   builder: TLLVMBuilder;
begin
   // get function name
   funcName := AnsiString(func.Name);

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
      argTypes[index] := TypeSymbolToLLVMType(Params.Symbols[index].Typ);

   // define function type and make sure no function with that name already exists
   funcType := LLVMFunctionType(retType, @argTypes[0], argCount, False);
   funcVal := LLVMGetNamedFunction(Module.Handle, PAnsiChar(funcName));
   if Assigned(funcVal) then
      raise ECodeGenException.Create(RStrFunctionAlreadyExists);

   // add function
   funcVal := LLVMAddFunction(Module.Handle, PAnsiChar(funcName), funcType);

   // link function
//   LinkLLVMValueToObject(funcVal, func);

   SetLength(argValues, argCount);
   if argCount > 0 then
   begin
      argValues[0] := LLVMGetFirstParam(funcVal);
      LinkLLVMValueToDataSymbol(argValues[0], params.Symbols[0]);
      argName := AnsiString(params.Symbols[0].Name);
      LLVMSetValueName(argValues[0], PAnsiChar(argName));

      for index := 1 to argCount - 1 do
      begin
         argValues[index] := LLVMGetNextParam(argValues[index - 1]);
         argName := AnsiString(params.Symbols[index].Name);
         LinkLLVMValueToDataSymbol(argValues[index], params.Symbols[index]);
         LLVMSetValueName(argValues[index], PAnsiChar(argName));
      end;
   end;

   // define entry basic block (first basic block in a function
   allocBlock := LLVMAppendBasicBlockInContext(FContext.Handle, funcVal, 'alloc');
   entryBlock := LLVMAppendBasicBlockInContext(FContext.Handle, funcVal, 'entry');
   resultBlock := nil;
   if Assigned(func.Result) then
      resultBlock := LLVMAppendBasicBlockInContext(FContext.Handle, funcVal, 'result');

   // create builder (essential to define function bodies)
   builder := TLLVMBuilder.Create;
   try
      // build empty alloc block
      builder.PositionBuilderAtEnd(allocBlock);
      allocBranch := LLVMBuildBr(builder.Handle, entryBlock);

      // locate entry block
      builder.PositionBuilderAtEnd(entryBlock);
      FBuilder := builder;

      // allocate memory for parameters / arguments and use this instead
      for index := 0 to argCount - 1 do
      begin
         argName := AnsiString(params.Symbols[index].Name);
         LLVMPositionBuilder(builder.Handle, allocBlock, allocBranch);
         argValue := LLVMBuildAlloca(FBuilder.Handle, argTypes[index], PAnsiChar(argName));
         builder.PositionBuilderAtEnd(entryBlock);
         LLVMBuildStore(FBuilder.Handle, argValues[index], argValue);
         argValues[index] := argValue;
         LinkLLVMValueToDataSymbol(argValues[index], params.Symbols[index]);
      end;

      // allocate memory for result / return value and use this instead
      if Assigned(func.Result) then
      begin
         argName := AnsiString(func.Result.Name);
         LLVMPositionBuilder(builder.Handle, allocBlock, allocBranch);
         argValue := LLVMBuildAlloca(FBuilder.Handle, retType, PAnsiChar(argName));
         builder.PositionBuilderAtEnd(entryBlock);
         LinkLLVMValueToDataSymbol(argValue, func.Result);
      end;

      // actually compile the function
      inherited;

      // check for result / return value and build return
      if Assigned(func.Result) then
      begin
         // branch to result block
         brValue := LLVMBuildBr(builder.Handle, resultBlock);
         LLVMMoveBasicBlockAfter(resultBlock, LLVMGetInstructionParent(brValue));
         builder.PositionBuilderAtEnd(resultBlock);

        // get result pointer
{$IFDEF LLVM_TAG}
         retValue := func.Result.LLVMValue;
{$ELSE}
         retValue := DataSymbolToLLVMValue(func.Result);
{$ENDIF}

         // get result value (with meaningful name
         argName := AnsiString(func.Result.Name);
         retValue := LLVMBuildLoad(FBuilder.Handle, retValue, PAnsiChar(argName));

         // actually return result value
         LLVMBuildRet(builder.Handle, retValue);
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

   {$IFDEF LLVM_TAG}
   Result := e.Right.LLVMValue;
   {$ELSE}
   Result := codeGen.ExpressionToLLVMValue(e.Right);
   {$ENDIF}
end;

procedure TLLVMAssignExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TAssignExpr;
   eDest: TDataExpr;
   cg : TdwsLLVMCodeGen;
   vExpr, vSym, vRec: PLLVMValue;
   vExprType: PLLVMType;
   sym: TDataSymbol;
   eRec: TRecordExpr;
   varExpr: TVarExpr;
   varName: AnsiString;
   entryBB, currentBB: PLLVMBasicBlock;
   term: PLLVMValue;
begin
   e := TAssignExpr(expr);
   cg := TdwsLLVMCodeGen(codeGen);

   if not (e.Left is TDataExpr) then
      raise ECodeGenException.Create(RStrNotSupported);

   vExpr := GetLLVMValue(cg, e);

   eDest := TDataExpr(e.Left);
   while eDest is TRecordExpr do
   begin
      eRec := TRecordExpr(eDest);

      // get LLVMValue for expression
      vRec := codeGen.ExpressionToLLVMValue(eRec.BaseExpr);
      LLVMBuildInsertValue(cg.Builder.Handle, vRec, vExpr, eRec.MemberOffset, '');

      eDest := eRec.BaseExpr;
      vExpr := vRec;
   end;

   // check if left expression is a TVarExpr (trivial case)
   if eDest is TVarExpr then
   begin
      // get TVarExpr and TDataSymbol
      varExpr := TVarExpr(eDest);
      sym := varExpr.DataSym;
      if not Assigned(sym) then
         raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [varExpr.StackAddr]);

      // get LLVMValue for expression
      varName := AnsiString(sym.Name);

      // get LLVMValue pointer
      vSym := codeGen.DataSymbolToLLVMValue(sym);
      if not Assigned(vSym) then
      begin
         // get current basic block
         currentBB := cg.Builder.GetInsertBlock;

         // get entry for current basic block
         entryBB := LLVMGetEntryBasicBlock(LLVMGetBasicBlockParent(currentBB));

         term := LLVMGetBasicBlockTerminator(entryBB);
         if Assigned(term) then
            LLVMPositionBuilder(cg.FBuilder.Handle, entryBB, term)
         else
            cg.FBuilder.PositionBuilderAtEnd(entryBB);

         // allocate memory for assign
         vExprType := LLVMTypeOf(vExpr);
         vSym := LLVMBuildAlloca(cg.Builder.Handle, vExprType, PAnsiChar(varName));
         cg.Builder.PositionBuilderAtEnd(currentBB);
         codeGen.LinkLLVMValueToDataSymbol(vSym, sym);
      end;

      LLVMBuildStore(cg.Builder.Handle, vExpr, vSym);
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
   vO : PLLVMValue;
begin
   e := TConstIntExpr(expr);

   vO := LLVMConstInt(codeGen.IntType.Handle, e.Value, True);
   codeGen.LinkLLVMValueToExpression(vO, expr);
end;


// ------------------
// ------------------ TLLVMConstFloatExpr ------------------
// ------------------

procedure TLLVMConstFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   e : TConstFloatExpr;
   floatType: PLLVMType;
   vO : PLLVMValue;
begin
   e := TConstFloatExpr(expr);

   floatType := LLVMDoubleTypeInContext(codeGen.Context.Handle);
   vO := LLVMConstReal(floatType, e.Value);
   codeGen.LinkLLVMValueToExpression(vO, expr);
end;


// ------------------
// ------------------ TLLVMVarExpr ------------------
// ------------------

procedure TLLVMVarExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   sym : TDataSymbol;
   varExpr : TVarExpr;
   vO : PLLVMValue;
   vOType : PLLVMType;
   symName: AnsiString;
begin
   varExpr := TVarExpr(expr);

   sym := varExpr.DataSym;
   if not Assigned(sym) then
      raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [varExpr.StackAddr]);

{$IFDEF LLVM_TAG}
{$ELSE}
   // load value from symbol and link to expression
   vO := codeGen.DataSymbolToLLVMValue(sym);

   if not Assigned(vO) then
      raise ECodeGenException.CreateFmt(RStrValueIsUndefined, [sym.Name]);

   symName := AnsiString(sym.Name);

   // get LLVMValue type and check if type is an integer (used as internal counter)
   vOType := LLVMTypeOf(vO);
   if (LLVMGetTypeKind(vOType) = LLVMIntegerTypeKind) then
      vO := LLVMBuildSExt(codeGen.Builder.Handle, vO, codeGen.IntType.Handle, PAnsiChar(symName))
   else
      vO := LLVMBuildLoad(codeGen.Builder.Handle, vO, PAnsiChar(symName));
   codeGen.LinkLLVMValueToExpression(vO, expr);
{$ENDIF}
end;


// ------------------
// ------------------ TLLVMRecordExpr ------------------
// ------------------

procedure TLLVMRecordExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TRecordExpr;
   vO : PLLVMValue;
   name: AnsiString;
begin
   e := TRecordExpr(expr);

   codeGen.Compile(e.BaseExpr);

{$IFDEF LLVM_TAG}
{$ELSE}
   vO := codeGen.ExpressionToLLVMValue(e.BaseExpr);

   name := LLVMGetValueName(vO) + '.' + AnsiString(e.FieldSymbol.Name);

   vO := LLVMBuildExtractValue(TdwsLLVMCodeGen(codeGen).Builder.Handle, vO,
      e.MemberOffset, PAnsiChar(name));

   codeGen.LinkLLVMValueToExpression(vO, e);
{$ENDIF}
end;


// ------------------
// ------------------ TLLVMStaticArrayExpr ------------------
// ------------------

procedure TLLVMStaticArrayExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TStaticArrayExpr;
   sym : TDataSymbol;
   eVar : TVarExpr;
   symName: AnsiString;
   vIndices: array [0..1] of PLLVMValue;
   vO, vArrayPtr: PLLVMValue;
begin
   e:=TStaticArrayExpr(expr);

   codeGen.CompileNoWrap(e.IndexExpr);

   if (e.BaseExpr is TVarExpr) then
   begin
      eVar := TVarExpr(e.BaseExpr);

      sym := eVar.DataSym;
      symName := AnsiString(sym.Name);
      if not Assigned(sym) then
         raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [eVar.StackAddr]);
      {$IFDEF LLVM_TAG}
      {$ELSE}

      vIndices[0] := LLVMConstNull(codeGen.IntType.Handle);
      vIndices[1] := codeGen.ExpressionToLLVMValue(e.IndexExpr);

      // load value from symbol and link to expression
      vArrayPtr := codeGen.DataSymbolToLLVMValue(sym);

      vO := LLVMBuildGEP(TdwsLLVMCodeGen(codeGen).Builder.Handle, vArrayPtr,
         @vIndices[0], 2, PAnsiChar(symName));

      vO := LLVMBuildLoad(codeGen.Builder.Handle, vO, PAnsiChar(symName));

      if not Assigned(vArrayPtr) then
         raise ECodeGenException.CreateFmt(RStrValueIsUndefined, [sym.Name]);

      codeGen.LinkLLVMValueToExpression(vO, expr);
      {$ENDIF}
   end
   else
   begin
      raise ECodeGenException.Create(RStrNotImplemented);

      codeGen.Compile(e.BaseExpr);
   end
end;


// ------------------
// ------------------ TLLVMVarParamExpr ------------------
// ------------------

procedure TLLVMVarParamExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   sym : TDataSymbol;
   varExpr : TVarExpr;
   vO : PLLVMValue;
begin
   varExpr := TVarExpr(expr);

   sym := varExpr.DataSym;
   if not Assigned(sym) then
      raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [varExpr.StackAddr]);

{$IFDEF LLVM_TAG}
{$ELSE}
   // synchronize symbol with expression
   vO := codeGen.DataSymbolToLLVMValue(sym);
   if Assigned(vO) then
   begin
      if not Assigned(codeGen.ExpressionToLLVMValue(expr)) then
         codeGen.LinkLLVMValueToExpression(vO, expr)
      else
      begin
         vO := codeGen.ExpressionToLLVMValue(expr);
         if Assigned(vO) then
            codeGen.LinkLLVMValueToDataSymbol(vO, sym)
      end;
   end
   else
   begin
      vO := codeGen.ExpressionToLLVMValue(expr);
      if Assigned(vO) then
         codeGen.LinkLLVMValueToDataSymbol(vO, sym)
   end;
{$ENDIF}
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
   vL, vR, vO: PLLVMValue;
   valName: AnsiString;
   e : TBinaryOpExpr;
   tk : TLLVMTypeKind;
begin
   e := TBinaryOpExpr(expr);

   codeGen.CompileNoWrap(e.Left);
   vL := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Left);

   tk := LLVMGetTypeKind(LLVMTypeOf(vL));

   if FIsFloatOp <> (tk in [LLVMHalfTypeKind,
      LLVMFloatTypeKind, LLVMDoubleTypeKind, LLVMX86_FP80TypeKind,
      LLVMFP128TypeKind, LLVMPPC_FP128TypeKind]) then
   begin
      valName := LLVMGetValueName(vL);
      vL := LLVMBuildSIToFP(codeGen.Builder.Handle, vL, codeGen.FloatType.Handle,
         PAnsiChar('conv_' + valName));
   end;

   codeGen.CompileNoWrap(e.Right);
   vR := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Right);

   if FIsFloatOp <> (LLVMGetTypeKind(LLVMTypeOf(vR)) in [LLVMHalfTypeKind,
      LLVMFloatTypeKind, LLVMDoubleTypeKind, LLVMX86_FP80TypeKind,
      LLVMFP128TypeKind, LLVMPPC_FP128TypeKind]) then
   begin
      valName := LLVMGetValueName(vR);
      vR := LLVMBuildSIToFP(codeGen.Builder.Handle, vR, codeGen.FloatType.Handle,
         PAnsiChar('conv_' + valName));
   end;

   vO := LLVMBuildBinOp(codeGen.Builder.Handle, FOp, vL, vR, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
end;


// ------------------
// ------------------ TLLVMMultIntPow2Expr ------------------
// ------------------

procedure TLLVMMultIntPow2Expr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
var
   vL, vR, vO: PLLVMValue;
   e : TMultIntPow2Expr;
begin
   e := TMultIntPow2Expr(expr);

   codeGen.CompileNoWrap(e.Expr);
   vL := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Expr);

   vR := LLVMConstInt(codeGen.IntType.Handle, 1 + e.Shift, True);

   vO := LLVMBuildShl(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, vL, vR, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   vL, vR: PLLVMValue;
begin
   codeGen.CompileNoWrap(e.Left);
{$IFDEF LLVM_TAG}
   vL := e.Left.LLVMValue;
{$ELSE}
   vL := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Left);
{$ENDIF}

   codeGen.CompileNoWrap(e.Right);
{$IFDEF LLVM_TAG}
   vR := e.Right.LLVMValue;
{$ELSE}
   vR := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Right);
{$ENDIF}
   Assert(Assigned(vR));

   Result := LLVMBuildBinOp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, FOp, vL, vR, '');
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
   vSym, vL, vR, vExpr: PLLVMValue;
   e : TMagicIteratorFuncExpr;
   VarExpr: TVarExpr;
   valName: AnsiString;
   sym: TDataSymbol;
begin
   e := TMagicIteratorFuncExpr(expr);

   if e.SubExpr[0] is TVarExpr then
   begin
      varExpr := TVarExpr(e.SubExpr[0]);

      sym := varExpr.DataSym;

      if not Assigned(sym) then
         raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [varExpr.StackAddr]);

      {$IFDEF LLVM_TAG}
      {$ELSE}
      // load value from symbol and link to expression
      vSym := codeGen.DataSymbolToLLVMValue(sym);

      if not Assigned(vSym) then
         raise ECodeGenException.CreateFmt(RStrValueIsUndefined, [sym.Name]);
      {$ENDIF}

      // eventually rename value
      valName := LLVMGetValueName(vSym);
      vL := LLVMBuildLoad(codeGen.Builder.Handle, vSym, '');

      codeGen.CompileNoWrap(TTypedExpr(e.SubExpr[1]));
      {$IFDEF LLVM_TAG}
      vR := e.SubExpr[1].LLVMValue;
      {$ELSE}
      vR := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.SubExpr[1]);
      {$ENDIF}
      Assert(Assigned(vR));

      vExpr := LLVMBuildBinOp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, FOp, vL, vR, '');

      LLVMBuildStore(codeGen.Builder.Handle, vExpr, vSym);
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
   vE, vO: PLLVMValue;
   unaryOpExpr: TUnaryOpExpr;
begin
   unaryOpExpr := TUnaryOpExpr(expr);

   codeGen.CompileNoWrap(unaryOpExpr.Expr);
   vE := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(unaryOpExpr.Expr);
   vO := LLVMBuildNot(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, vE, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   vE, vO: PLLVMValue;
   unaryOpExpr: TUnaryOpExpr;
   e : TVarExpr;
const
   CMulOps: array [Boolean] of TLLVMOpcode = (LLVMFMul, LLVMMul);
begin
   unaryOpExpr := TUnaryOpExpr(expr);

   if unaryOpExpr.Expr is TVarExpr then
   begin
      e := TVarExpr(unaryOpExpr.Expr);

      codeGen.CompileNoWrap(e);
      vE := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e);

      vO := LLVMBuildBinOp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle,
         CMulOps[FIsInteger], vE, vE, '');

      codeGen.LinkLLVMValueToExpression(vO, expr);
   end
   else
      raise ECodeGenException.Create(RStrNotSupported);
end;


// ------------------
// ------------------ TLLVMNegExpr ------------------
// ------------------

procedure TLLVMNegExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   vE, vO: PLLVMValue;
   unaryOpExpr: TUnaryOpExpr;
   e : TVarExpr;
begin
   unaryOpExpr := TUnaryOpExpr(expr);

   if unaryOpExpr.Expr is TVarExpr then
   begin
      e := TVarExpr(unaryOpExpr.Expr);

      codeGen.CompileNoWrap(e);
      vE := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e);

      if FIsInteger then
         vO := LLVMBuildNeg(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, vE, '')
      else
         vO := LLVMBuildFNeg(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, vE, '');

      codeGen.LinkLLVMValueToExpression(vO, expr);
   end
   else
      raise ECodeGenException.Create(RStrNotSupported);
end;


// ------------------
// ------------------ TLLVMAbsIntExpr ------------------
// ------------------

procedure TLLVMAbsIntExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   valInput, valNull, valOutput, valPhi, valCond: PLLVMValue;
   unaryOpExpr: TUnaryOpExpr;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
begin
   unaryOpExpr := TUnaryOpExpr(expr);

   codeGen.CompileNoWrap(unaryOpExpr.Expr);
   valInput := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(unaryOpExpr.Expr);
   valNull := LLVMConstNull(codeGen.IntType.Handle);

   valCond := LLVMBuildICmp(codeGen.Builder.Handle, LLVMIntSLT, valInput, valNull, '');

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(codeGen.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildCondBr(codeGen.Builder.Handle, valCond, bbList[1], bbList[2]);

   codeGen.Builder.PositionBuilderAtEnd(bbList[1]);
   valOutput := LLVMBuildNeg(codeGen.Builder.Handle, valInput, '');
   LLVMBuildBr(codeGen.Builder.Handle, bbList[2]);

   codeGen.Builder.PositionBuilderAtEnd(bbList[2]);

   valPhi := LLVMBuildPhi(codeGen.Builder.Handle, codeGen.IntType.Handle, 'phi');
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
   vE, vO: PLLVMValue;
   convFloat: TConvFloatExpr;
begin
   convFloat := TConvFloatExpr(expr);

   codeGen.CompileNoWrap(convFloat.Expr);
   vE := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(convFloat.Expr);
   vO := LLVMBuildSIToFP(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, vE, TdwsLLVMCodeGen(codeGen).FFloatType.Handle, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   e: TRelIntIsZeroExpr;
   vL, vR, vO: PLLVMValue;
begin
   e := TRelIntIsZeroExpr(expr);

   codeGen.CompileNoWrap(e.Expr);
   vL := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Expr);
   vR := LLVMConstNull(codeGen.IntType.Handle);

   Assert(LLVMGetTypeKind(LLVMTypeOf(vL)) = LLVMGetTypeKind(LLVMTypeOf(vR)));

   vO := LLVMBuildICmp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, LLVMIntEQ, vL, vR, '');
   vO := LLVMBuildNot(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, vO, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   e: TIntegerRelOpExpr;
   vL, vR, vO: PLLVMValue;
begin
   e := TIntegerRelOpExpr(expr);

   codeGen.CompileNoWrap(e.Left);
   vL := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Left);

   codeGen.CompileNoWrap(e.Right);
   vR := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Right);

   Assert(LLVMGetTypeKind(LLVMTypeOf(vL)) = LLVMGetTypeKind(LLVMTypeOf(vR)));

   vO := LLVMBuildICmp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, FPredicate, vL, vR, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   e: TFloatRelOpExpr;
   vL, vR, vO: PLLVMValue;
begin
   e := TFloatRelOpExpr(expr);

   codeGen.CompileNoWrap(e.Left);
   vL := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Left);

   codeGen.CompileNoWrap(e.Right);
   vR := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Right);

   vO := LLVMBuildFCmp(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, FPredicate, vL, vR, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
end;


// ------------------
// ------------------ TLLVMIfThenElseValueExpr ------------------
// ------------------

procedure TLLVMIfThenElseValueExpr.CodeGen(codeGen: TdwsCodeGen;
  expr: TExprBase);
var
   e: TIfThenElseValueExpr;
   cg: TdwsLLVMCodeGen;
   bbList: array [0 .. 3] of PLLVMBasicBlock;
   vCond, vTrue, vFalse, vPhi: PLLVMValue;
begin
   cg := TdwsLLVMCodeGen(codeGen);;
   e := TIfThenElseValueExpr(expr);

   // generate code for condition
   cg.CompileNoWrap(e.CondExpr);
   vCond := codeGen.ExpressionToLLVMValue(e.CondExpr);

   // get, build and position basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.FBuilder.Handle);
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
   LLVMBuildCondBr(cg.FBuilder.Handle, vCond, bbList[1], bbList[2]);

   // locate & compile 'then' block
   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);
   codeGen.Compile(e.TrueExpr);
   vTrue :=TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.TrueExpr);
   LLVMBuildBr(cg.FBuilder.Handle, bbList[3]);

   // locate & compile 'else' block
   cg.FBuilder.PositionBuilderAtEnd(bbList[2]);
   codeGen.Compile(e.FalseExpr);
   vFalse := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.FalseExpr);
   LLVMBuildBr(cg.FBuilder.Handle, bbList[3]);

   // locate final block
   cg.FBuilder.PositionBuilderAtEnd(bbList[3]);
   Assert(LLVMTypeOf(vTrue) = LLVMTypeOf(vFalse));
   vPhi := LLVMBuildPhi(cg.FBuilder.Handle, LLVMTypeOf(vTrue), '');
   LLVMAddIncoming(vPhi, @vTrue, @bbList[1], 1);
   LLVMAddIncoming(vPhi, @vFalse, @bbList[2], 1);
   codeGen.LinkLLVMValueToExpression(vPhi, expr);
end;


// ------------------
// ------------------ TLLVMIfExpr ------------------
// ------------------

procedure TLLVMIfExpr.CodeGenCondition(codeGen: TdwsCodeGen;
  condExpr: TTypedExpr; out SwapBlocks: Boolean);
var
   vO: PLLVMValue;
begin
   if (condExpr is TNotBoolExpr) then
   begin
      SwapBlocks := True;
      codeGen.CompileNoWrap(TNotBoolExpr(condExpr).Expr);
      vO := codeGen.ExpressionToLLVMValue(TNotBoolExpr(condExpr).Expr);
      codeGen.LinkLLVMValueToExpression(vO, condExpr);
   end
   else
   begin
      SwapBlocks := False;
      codeGen.CompileNoWrap(condExpr);
   end;
end;


// ------------------
// ------------------ TLLVMIfThenExpr ------------------
// ------------------

procedure TLLVMIfThenExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TIfThenExpr;
   cg: TdwsLLVMCodeGen;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
   vCond: PLLVMValue;
   SwapBlocks: Boolean;
begin
   cg := TdwsLLVMCodeGen(codeGen);;
   e := TIfThenExpr(expr);

   // generate code for condition
   CodeGenCondition(codeGen, e.CondExpr, SwapBlocks);
   vCond := codeGen.ExpressionToLLVMValue(e.CondExpr);

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.FBuilder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   if SwapBlocks then
      LLVMBuildCondBr(cg.FBuilder.Handle, vCond, bbList[2], bbList[1])
   else
      LLVMBuildCondBr(cg.FBuilder.Handle, vCond, bbList[1], bbList[2]);

   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);

   codeGen.Compile(e.ThenExpr);
   if not e.ThenExpr.InterruptsFlow then
      LLVMBuildBr(cg.FBuilder.Handle, bbList[2]);

   cg.FBuilder.PositionBuilderAtEnd(bbList[2]);
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
   e : TIfThenElseExpr;
   cg: TdwsLLVMCodeGen;
   bbList: array [0 .. 3] of PLLVMBasicBlock;
   vCond: PLLVMValue;
   SwapBlocks: Boolean;
begin
   cg := TdwsLLVMCodeGen(codeGen);;
   e := TIfThenElseExpr(expr);

   // generate code for condition
   CodeGenCondition(codeGen, e.CondExpr, SwapBlocks);
   vCond := codeGen.ExpressionToLLVMValue(e.CondExpr);

   // get, build and position basic blocks
   bbList[0] := LLVMGetInsertBlock(cg.FBuilder.Handle);
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
   if SwapBlocks then
      LLVMBuildCondBr(cg.FBuilder.Handle, vCond, bbList[2], bbList[1])
   else
      LLVMBuildCondBr(cg.FBuilder.Handle, vCond, bbList[1], bbList[2]);

   // locate & compile 'then' block
   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);
   codeGen.Compile(e.ThenExpr);
   if not e.ThenExpr.InterruptsFlow then
      LLVMBuildBr(cg.FBuilder.Handle, bbList[3]);

   // locate & compile 'else' block
   cg.FBuilder.PositionBuilderAtEnd(bbList[2]);
   codeGen.Compile(e.ElseExpr);
   if not e.ElseExpr.InterruptsFlow then
      LLVMBuildBr(cg.FBuilder.Handle, bbList[3]);

   // locate final block
   cg.FBuilder.PositionBuilderAtEnd(bbList[3]);
end;

function TLLVMIfThenElseExpr.SubExprIsSafeStatement(sub: TExprBase): Boolean;
begin
   Result :=    (sub is TFuncExprBase)
           or (sub is TNoResultWrapperExpr)
           or (sub is TAssignExpr)
           or (sub is TFlowControlExpr);
end;


// ------------------
// ------------------ TLLVMCaseExpr ------------------
// ------------------

procedure TLLVMCaseExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e: TCaseExpr;
   caseIndex: Integer;
   rangeIndex: Integer;
   cc : TCaseCondition;
   vVal, vSwitch: PLLVMValue;
   vCaseVal: PLLVMValue;
   caseName: AnsiString;
   caseValue: array [0 .. 1] of Int64;
   bbCase: PLLVMBasicBlock;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
begin
   e := TCaseExpr(expr);
   codeGen.CompileNoWrap(e.ValueExpr);
   vVal := codeGen.ExpressionToLLVMValue(e.ValueExpr);

   // get, build and position basic blocks
   bbList[0] := LLVMGetInsertBlock(codeGen.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[0], 'else');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // build switch instruction
   vSwitch := LLVMBuildSwitch(codeGen.Builder.Handle, vVal, bbList[1],
      e.CaseConditions.Count);

   for caseIndex := 0 to e.CaseConditions.Count - 1 do
   begin
      cc := TCaseCondition(e.CaseConditions.List[caseIndex]);
      if cc is TCompareCaseCondition then
      begin
         if TCompareCaseCondition(cc).CompareExpr is TConstIntExpr then
         begin
            // transfer case value as LLVM value
            caseValue[0] :=  TConstIntExpr(TCompareCaseCondition(cc).CompareExpr).Value;
            vCaseVal := LLVMConstInt(codeGen.IntType.Handle, caseValue[0], True);
         end
         else
            raise ECodeGenException.Create(RStrNotSupported);

         // get proper label
         if caseValue[0] < 0 then
            caseName := 'case__' + AnsiString(IntToStr(-caseValue[0]))
         else
            caseName := 'case_' + AnsiString(IntToStr(caseValue[0]));

         // insert new case right before 'else' block and add!
         bbCase := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
            bbList[1], PAnsiChar(caseName));
         LLVMAddCase(vSwitch, vCaseVal, bbCase);

         // locate current case
         codeGen.Builder.PositionBuilderAtEnd(bbCase);

         // compile case
         codeGen.Compile(TCompareCaseCondition(cc).TrueExpr);

         // jump to 'cont' branch (= end of case expression)
         LLVMBuildBr(codeGen.Builder.Handle, bbList[2]);
      end
      else
      if cc is TRangeCaseCondition then
      begin
         // get transfer values
         if TRangeCaseCondition(cc).FromExpr is TConstIntExpr then
            caseValue[0] :=  TConstIntExpr(TRangeCaseCondition(cc).FromExpr).Value
         else
            raise ECodeGenException.Create(RStrNotSupported);

         if TRangeCaseCondition(cc).ToExpr is TConstIntExpr then
            caseValue[1] :=  TConstIntExpr(TRangeCaseCondition(cc).ToExpr).Value
         else
            raise ECodeGenException.Create(RStrNotSupported);

         // get proper label (not that meaningful yet)
         caseName := 'range';

         // insert new case right before 'else' block
         bbCase := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
            bbList[1], PAnsiChar(caseName));

         for rangeIndex := caseValue[0] to caseValue[1] do
         begin
            vCaseVal := LLVMConstInt(codeGen.IntType.Handle, rangeIndex, True);
            LLVMAddCase(vSwitch, vCaseVal, bbCase);
         end;

         // locate current case
         codeGen.Builder.PositionBuilderAtEnd(bbCase);

         // compile case
         codeGen.Compile(TCompareCaseCondition(cc).TrueExpr);

         // jump to 'cont' branch (= end of case expression)
         LLVMBuildBr(codeGen.Builder.Handle, bbList[2]);
      end
      else
         raise ECodeGenException.Create(RStrNotSupported);
   end;

   // locate 'else' branch
   codeGen.Builder.PositionBuilderAtEnd(bbList[1]);

   // compile else
   codeGen.Compile(e.ElseExpr);

   // jump to & locate 'cont' branch (= end of case expression)
   LLVMBuildBr(codeGen.Builder.Handle, bbList[2]);
   codeGen.Builder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMExitExpr ------------------
// ------------------

procedure TLLVMExitExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   Fn: PLLVMValue;
   BB: PLLVMBasicBlock;
   vB: PLLVMValue;
begin
   Fn := LLVMGetBasicBlockParent(codeGen.Builder.GetInsertBlock);

   // locate result basic block and branch if found, then exit
   BB := LLVMGetFirstBasicBlock(Fn);
   while Assigned(BB) do
   begin
      vB := LLVMBasicBlockAsValue(BB);
      if LLVMGetValueName(vB) = 'result' then
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
   e: TExitValueExpr;
   vO: PLLVMValue;
begin
   e := TExitValueExpr(expr);
   codeGen.Compile(e.AssignExpr);
   vO := codeGen.ExpressionToLLVMValue(e.AssignExpr);
   LLVMBuildRet(codeGen.Builder.Handle, vO);
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
   e: TForExpr;
   cg: TdwsLLVMCodeGen;

   useInt32: Boolean;
   bounds: array [0 .. 1] of Int64;
   indexType: PLLVMType;
   vPhi, vNext, vFrom, vTo, vStep : PLLVMValue;
   loopVarSym: TDataSymbol;
   valName: AnsiString;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
   bbNext: PLLVMBasicBlock;
   CondValue: PLLVMValue;
begin
   e := TForExpr(expr);
   cg := TdwsLLVMCodeGen(codeGen);

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
      vFrom := LLVMConstInt(indexType, bounds[0], True);
      vTo := LLVMConstInt(indexType, bounds[1], True);
   end
   else
   begin
      // use native int type
      indexType := cg.IntType.Handle;

      // calculate 'from' and 'to'
      cg.CompileNoWrap(e.FromExpr);
      vFrom := CodeGen.ExpressionToLLVMValue(e.FromExpr);
      cg.CompileNoWrap(e.ToExpr);
      vTo := CodeGen.ExpressionToLLVMValue(e.ToExpr);
   end;

   // check if for loop uses a dedicated step
   if expr is TForStepExpr then
   begin
      // either transfer constant value or compile step value
      if useInt32 then
         vStep := LLVMConstInt(indexType, TConstIntExpr(TForStepExpr(expr).StepExpr).Value, True)
      else
      begin
         cg.CompileNoWrap(TForStepExpr(expr).StepExpr);
         vStep := CodeGen.ExpressionToLLVMValue(TForStepExpr(expr).StepExpr);
      end;
   end
   else
      vStep := LLVMConstInt(indexType, 1, True);

   bbList[0] := LLVMGetInsertBlock(cg.FBuilder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[0], 'for_loop');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[1], 'loop_done');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   // perform comparison in
   CondValue := LLVMBuildICmp(cg.Builder.Handle, LLVMIntSLE, vFrom, vTo, 'loopcond');

   LLVMBuildCondBr(cg.Builder.Handle, CondValue, bbList[1], bbList[2]);

   loopVarSym := e.VarExpr.DataSym;
   valName := AnsiString(loopVarSym.Name);
   if not Assigned(loopVarSym) then
      raise ECodeGenUnsupportedSymbol.CreateFmt(RStrVarNotFound, [e.VarExpr.StackAddr]);

   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);
   vPhi := LLVMBuildPhi(cg.FBuilder.Handle, indexType, PAnsiChar(valName));
   codeGen.LinkLLVMValueToDataSymbol(vPhi, loopVarSym);

   LLVMAddIncoming(vPhi, @vFrom, @bbList[0], 1);

   codeGen.Compile(e.DoExpr);

   // advance index
   if FReverse then
      vNext := LLVMBuildSub(cg.Builder.Handle, vPhi, vStep, PAnsiChar(valName))
   else
      vNext := LLVMBuildAdd(cg.Builder.Handle, vPhi, vStep, PAnsiChar(valName));
   bbNext := LLVMGetInstructionParent(vNext);
   LLVMAddIncoming(vPhi, @vNext, @bbNext, 1);
   cg.LinkLLVMValueToDataSymbol(vNext, e.VarExpr.DataSym);

   CondValue := LLVMBuildICmp(cg.Builder.Handle, LLVMIntEQ, vPhi, vTo, 'loopcond');
   LLVMBuildCondBr(cg.Builder.Handle, CondValue, bbList[2], bbList[1]);

   LLVMPositionBuilderBefore(cg.FBuilder.Handle, CondValue);

   cg.FBuilder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMLoopExpr ------------------
// ------------------


// ------------------
// ------------------ TLLVMRepeatExpr ------------------
// ------------------

procedure TLLVMRepeatExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e: TRepeatExpr;
   cg: TdwsLLVMCodeGen;

   vCond: PLLVMValue;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
begin
   e := TRepeatExpr(expr);
   cg := TdwsLLVMCodeGen(codeGen);

   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[0], 'repeat_loop');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[1], 'loop_done');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildBr(cg.FBuilder.Handle, bbList[1]);
   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);

   codeGen.Compile(e.LoopExpr);

   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);
   codeGen.CompileNoWrap(e.CondExpr);
   vCond := cg.ExpressionToLLVMValue(e.CondExpr);

   LLVMSetValueName(vCond, 'repeat_cond');

   LLVMBuildCondBr(cg.Builder.Handle, vCond, bbList[2], bbList[1]);

   cg.FBuilder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMWhileExpr ------------------
// ------------------

procedure TLLVMWhileExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e: TWhileExpr;
   cg: TdwsLLVMCodeGen;

   vCond: PLLVMValue;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
begin
   e := TWhileExpr(expr);
   cg := TdwsLLVMCodeGen(codeGen);

   bbList[0] := LLVMGetInsertBlock(cg.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[0], 'repeat_loop');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle,
     bbList[1], 'loop_done');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildBr(cg.FBuilder.Handle, bbList[1]);
   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);

   codeGen.Compile(e.LoopExpr);

   cg.FBuilder.PositionBuilderAtEnd(bbList[1]);
   codeGen.CompileNoWrap(e.CondExpr);
   vCond := cg.ExpressionToLLVMValue(e.CondExpr);

   LLVMSetValueName(vCond, 'repeat_cond');

   LLVMBuildCondBr(cg.Builder.Handle, vCond, bbList[2], bbList[1]);

   cg.FBuilder.PositionBuilderAtEnd(bbList[2]);
end;


// ------------------
// ------------------ TLLVMFuncBaseExpr ------------------
// ------------------

procedure TLLVMFuncBaseExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   e : TFuncExprBase;
   i : Integer;
   funcSym : TFuncSymbol;
   paramExpr : TTypedExpr;

   argumentCount: Integer;
   argumentTypes: PLLVMTypePtrArray;
   argumentValues: PLLVMValuePtrArray;

   Fn: PLLVMValue;
   vO: PLLVMValue;

   functionName: AnsiString;
   functionType: PLLVMType;
   returnType: PLLVMType;
   Index: Integer;
begin
   e := TFuncExprBase(expr);
   funcSym := e.FuncSym;

//   Fn := TdwsLLVMCodeGen(codeGen).ObjectToLLVMValue(e.FuncSym);

   ArgumentCount := e.Args.Count;
   Assert(e.Args.Count = e.FuncSym.Params.Count);

   // function not found -> probably external? -> workaround solution (todo)
   if not Assigned(Fn) then
   begin
      functionName := AnsiString(funcSym.Name);
      if Assigned(funcSym.Result) then
         returnType := TdwsLLVMCodeGen(codeGen).TypeSymbolToLLVMType(funcSym.Result.Typ)
      else
         returnType := LLVMVoidTypeInContext(codeGen.Context.Handle);

      GetMem(argumentTypes, argumentCount * SizeOf(PLLVMType));
      for Index := 0 to argumentCount - 1 do
         argumentTypes^[Index] := TdwsLLVMCodeGen(codeGen).TypeSymbolToLLVMType(funcSym.Params.Symbols[Index].Typ);

      functionType := LLVMFunctionType(returnType, argumentTypes, argumentCount, False);

      Fn := LLVMGetNamedFunction(codeGen.Module.Handle, PAnsiChar(functionName));
      if not Assigned(Fn) then
      begin
         Fn := LLVMAddFunction(codeGen.Module.Handle, PAnsiChar(functionName),
            functionType);

         LLVMSetLinkage(Fn, LLVMExternalLinkage);
         LLVMSetFunctionCallConv(Fn, Ord(LLVMCCallConv));
      end;
   end;

   GetMem(argumentValues, ArgumentCount * SizeOf(PLLVMValue));
   for i := 0 to ArgumentCount-1 do
   begin
      paramExpr := e.Args.ExprBase[i] as TTypedExpr;
      codeGen.CompileNoWrap(paramExpr);
      argumentValues[i] := codeGen.ExpressionToLLVMValue(paramExpr);
   end;

   vO := LLVMBuildCall(TdwsLLVMCodeGen(codeGen).FBuilder.Handle, Fn, argumentValues, argumentCount, '');
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   else name := e.FuncSym.QualifiedName;
   if cgoNoInlineMagics in codeGen.Options then
      i := -1
   else i := FMagicCodeGens.IndexOf(name);
   if i>=0 then
      TdwsExprCodeGen(FMagicCodeGens.Objects[i]).CodeGen(codeGen, expr)
   else begin
      codeGen.Dependencies.Add(name);
      inherited;
   end;
end;


// ------------------
// ------------------ TLLVMPrintLnExpr ------------------
// ------------------

procedure TLLVMPrintLnExpr.CodeGen(codeGen: TdwsCodeGen; expr: TExprBase);
var
   AnsiCharType: TLLVMInt8Type;
   PAnsiCharType: TLLVMPointerType;
   Param, FuncPuts: PLLVMValue;

   TypePuts: PLLVMType;

   e : TMagicFuncExpr;
   a : TExprBase;
   c : TConstStringExpr;
   str: AnsiString;
   vG: PLLVMValue;
   vStr: PLLVMValue;
   vO: PLLVMValue;
   vGEP: PLLVMValue;
   vPtrArray: array [0 .. 1] of PLLVMValue;
begin
   AnsiCharType := TLLVMInt8Type.Create(CodeGen.Context);
   PAnsiCharType := TLLVMPointerType.Create(AnsiCharType.Handle, 0);

   TypePuts := LLVMFunctionType(codeGen.IntType.Handle, @PAnsiCharType.Handle, 1, False);

   FuncPuts := LLVMGetNamedFunction(CodeGen.Module.Handle, 'puts');
   if not Assigned(FuncPuts) then
   begin
      FuncPuts := LLVMAddFunction(CodeGen.Module.Handle, 'puts', TypePuts);

      Param := LLVMGetParam(FuncPuts, 0);
      LLVMAddAttribute(Param, LLVMNoCaptureAttribute);

      LLVMSetLinkage(FuncPuts, LLVMExternalLinkage);
      LLVMSetFunctionCallConv(FuncPuts, Ord(LLVMCCallConv));
   end;
   LLVMAddFunctionAttr(FuncPuts, LLVMNoUnwindAttribute);

   e := TMagicFuncExpr(expr);
   a := e.Args[0];

   if a is TConstStringExpr then
   begin
      c := TConstStringExpr(a);
      str := AnsiString(c.Value);
      str := str + #0;

      // global variable declaration and initialization
      vStr := LLVMConstStringInContext(
        codeGen.Context.Handle, PAnsiChar(str), Length(str), True);
      vG := LLVMAddGlobal(codeGen.Module.Handle,
         LLVMArrayType(AnsiCharType.Handle, Length(str)), 'printf_static');
      LLVMSetLinkage(vG, LLVMInternalLinkage);
      LLVMSetGlobalConstant(vG, True);
      LLVMSetInitializer(vG, vStr);

      vPtrArray[0] := LLVMConstNull(codeGen.IntType.Handle);
      vPtrArray[1] := vPtrArray[0];
      vGEP := LLVMBuildGEP(codeGen.Builder.Handle, vG, @vPtrArray[0], 2, 'cast');

      vO := LLVMBuildCall(codeGen.Builder.Handle, FuncPuts, @vGEP, 1, 'puts_result');
      codeGen.LinkLLVMValueToExpression(vO, expr);
      LLVMSetInstructionCallConv(vO, Ord(LLVMCCallConv));
   end
   else
   begin
      if a is TTypedExpr then
         codeGen.Compile(a);
      vO := codeGen.ExpressionToLLVMValue(a);
      if LLVMGetTypeKind(LLVMTypeOf(vO)) = LLVMIntegerTypeKind then
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
   e : TMagicFuncExpr;
   vI, vO, Param, FuncVal: PLLVMValue;
   FnType: PLLVMType;
   str: AnsiString;
begin
   FnType := LLVMFunctionType(codeGen.FloatType.Handle, @CodeGen.FloatType.Handle, 1, False);

   str := 'llvm.' + FIntrisicName + '.f64';
   FuncVal := LLVMGetNamedFunction(CodeGen.Module.Handle, PAnsiChar(str));
   if not Assigned(FuncVal) then
   begin
      FuncVal := LLVMAddFunction(CodeGen.Module.Handle, PAnsiChar(str), FnType);
      Param := LLVMGetParam(FuncVal, 0);
      str := 'Val';
      LLVMSetValueName(Param, PAnsiChar(str));
   end;

   e := TMagicFuncExpr(expr);
   codeGen.Compile(e.Args[0]);
   vI := codeGen.ExpressionToLLVMValue(e.Args[0]);
   str := LLVMGetValueName(vI);
   str := FIntrisicName + '_' + str;
   vO := LLVMBuildCall(codeGen.Builder.Handle, FuncVal, @vI, 1, PAnsiChar(str));
   codeGen.LinkLLVMValueToExpression(vO, expr);
end;


// ------------------
// ------------------ TLLVMBaseFloatIntFuncExpr ------------------
// ------------------

procedure TLLVMBaseFloatIntFuncExpr.CodeGen(codeGen: TdwsCodeGen;
  expr: TExprBase);
var
   vVal: PLLVMValue;
   str: AnsiString;
begin
   inherited;

   vVal := codeGen.ExpressionToLLVMValue(expr);
   str := LLVMGetValueName(vVal);
   str := 'conv_' + str;
   vVal := LLVMBuildFPToSI(codeGen.Builder.Handle, vVal, codeGen.IntType.Handle, PAnsiChar(str));
   codeGen.LinkLLVMValueToExpression(vVal, expr);
end;


// ------------------
// ------------------ TLLVMAbsFloatExpr ------------------
// ------------------

procedure TLLVMAbsFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen; expr: TTypedExpr);
var
   e : TAbsFloatExpr;
   vI, vO, Param, FuncVal: PLLVMValue;
   FnType: PLLVMType;
   str: AnsiString;
begin
   FnType := LLVMFunctionType(codeGen.FloatType.Handle, @CodeGen.FloatType.Handle, 1, False);

   str := 'llvm.fabs.f64';
   FuncVal := LLVMGetNamedFunction(CodeGen.Module.Handle, PAnsiChar(str));
   if not Assigned(FuncVal) then
   begin
      FuncVal := LLVMAddFunction(CodeGen.Module.Handle, PAnsiChar(str), FnType);
      Param := LLVMGetParam(FuncVal, 0);
      str := 'Val';
      LLVMSetValueName(Param, PAnsiChar(str));
   end;

   e := TAbsFloatExpr(expr);
   codeGen.Compile(e.Expr);
   vI := codeGen.ExpressionToLLVMValue(e.Expr);
   str := LLVMGetValueName(vI);
   str := 'fabs_' + str;
   vO := LLVMBuildCall(codeGen.Builder.Handle, FuncVal, @vI, 1, PAnsiChar(str));
   codeGen.LinkLLVMValueToExpression(vO, expr);
end;


// ------------------
// ------------------ TLLVMPowFloatExpr ------------------
// ------------------

procedure TLLVMPowFloatExpr.CodeGen(codeGen: TdwsCodeGen;
  expr: TExprBase);
var
   e : TMagicFuncExpr;
   tI: array [0 .. 1] of PLLVMType;
   vI: array [0 .. 1] of PLLVMValue;
   vO, Param, FuncVal: PLLVMValue;
   FnType: PLLVMType;
   str: AnsiString;
begin
   tI[0] := CodeGen.FloatType.Handle;
   tI[1] := CodeGen.FloatType.Handle;
   FnType := LLVMFunctionType(codeGen.FloatType.Handle, @tI, 2, False);

   str := 'llvm.pow.f64';
   FuncVal := LLVMGetNamedFunction(CodeGen.Module.Handle, PAnsiChar(str));
   if not Assigned(FuncVal) then
   begin
      FuncVal := LLVMAddFunction(CodeGen.Module.Handle, PAnsiChar(str), FnType);
      Param := LLVMGetParam(FuncVal, 0);
      str := 'Val';
      LLVMSetValueName(Param, PAnsiChar(str));
   end;

   e := TMagicFuncExpr(expr);

   // compile value
   codeGen.Compile(e.Args[0]);
   vI[0] := codeGen.ExpressionToLLVMValue(e.Args[0]);

   // compile power
   codeGen.Compile(e.Args[1]);
   vI[1] := codeGen.ExpressionToLLVMValue(e.Args[1]);

   str := LLVMGetValueName(vI[0]);
   str := 'pow' + str;
   vO := LLVMBuildCall(codeGen.Builder.Handle, FuncVal, @vI, 2, PAnsiChar(str));
   codeGen.LinkLLVMValueToExpression(vO, expr);
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
   e: TMagicFuncExpr;
   valA, valB, valNull, valPhi, valCond: PLLVMValue;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
   str: AnsiString;
begin
   e := TMagicFuncExpr(expr);

   if e.Args.Count <> 2 then
      raise ECodeGenException.Create(RStrWrongNumberOfArgs);

   // compile value
   codeGen.Compile(e.Args[0]);
   valA := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Args[0]);

   codeGen.Compile(e.Args[1]);
   valB := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Args[1]);

   if not (LLVMGetTypeKind(LLVMTypeOf(valB)) = LLVMIntegerTypeKind) then
      raise ECodeGenException.Create('Type mismatch');

   valCond := LLVMBuildICmp(codeGen.Builder.Handle, FPredicate, valA, valB, '');
   valNull := LLVMConstNull(codeGen.IntType.Handle);

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(codeGen.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildCondBr(codeGen.Builder.Handle, valCond, bbList[1], bbList[2]);

   codeGen.Builder.PositionBuilderAtEnd(bbList[1]);
   str := LLVMGetValueName(valB);
   valB := LLVMBuildAdd(codeGen.Builder.Handle, valB, valNull, PAnsiChar(str));
   LLVMBuildBr(codeGen.Builder.Handle, bbList[2]);

   codeGen.Builder.PositionBuilderAtEnd(bbList[2]);

   // build phi value
   valPhi := LLVMBuildPhi(codeGen.Builder.Handle, codeGen.IntType.Handle, 'phi');
   LLVMAddIncoming(valPhi, @valA, @bbList[0], 1);
   LLVMAddIncoming(valPhi, @valB, @bbList[1], 1);

   codeGen.LinkLLVMValueToExpression(valPhi, expr);
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
   e: TMagicFuncExpr;
   valA, valB, valNull, valPhi, valCond: PLLVMValue;
   bbList: array [0 .. 2] of PLLVMBasicBlock;
   str: AnsiString;
begin
   e := TMagicFuncExpr(expr);

   if e.Args.Count <> 2 then
      raise ECodeGenException.Create(RStrWrongNumberOfArgs);

   // compile value
   codeGen.Compile(e.Args[0]);
   valA := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Args[0]);

   codeGen.Compile(e.Args[1]);
   valB := TdwsLLVMCodeGen(codeGen).ExpressionToLLVMValue(e.Args[1]);

   valCond := LLVMBuildFCmp(codeGen.Builder.Handle, FPredicate, valA, valB, '');
   valNull := LLVMConstNull(codeGen.FloatType.Handle);

   // build basic blocks
   bbList[0] := LLVMGetInsertBlock(codeGen.Builder.Handle);
   bbList[1] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle, bbList[0], 'then');
   LLVMMoveBasicBlockAfter(bbList[1], bbList[0]);
   bbList[2] := LLVMInsertBasicBlockInContext(codeGen.Context.Handle, bbList[1], 'cont');
   LLVMMoveBasicBlockAfter(bbList[2], bbList[1]);

   LLVMBuildCondBr(codeGen.Builder.Handle, valCond, bbList[1], bbList[2]);

   codeGen.Builder.PositionBuilderAtEnd(bbList[1]);
   str := LLVMGetValueName(valB);
   valB := LLVMBuildFAdd(codeGen.Builder.Handle, valB, valNull, PAnsiChar(str));
   LLVMBuildBr(codeGen.Builder.Handle, bbList[2]);

   codeGen.Builder.PositionBuilderAtEnd(bbList[2]);

   // build phi value
   valPhi := LLVMBuildPhi(codeGen.Builder.Handle, codeGen.FloatType.Handle, 'phi');
   LLVMAddIncoming(valPhi, @valA, @bbList[0], 1);
   LLVMAddIncoming(valPhi, @valB, @bbList[1], 1);

   codeGen.LinkLLVMValueToExpression(valPhi, expr);
end;

procedure TLLVMMinMaxFloatExpr.CodeGenNoWrap(codeGen: TdwsCodeGen;
  expr: TTypedExpr);
begin
   Self.CodeGen(codeGen, expr);
end;

end.
