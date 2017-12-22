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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsMagicExprs;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsErrors, dwsStrings, dwsScriptSource, dwsCompilerContext,
   dwsSymbols, dwsExprList, dwsStack, dwsDataContext,
   dwsExprs, dwsFunctions;

type

   TMagicFuncExpr = class;
   TMagicFuncExprClass = class of TMagicFuncExpr;

   TMagicFuncDoEvalEvent = procedure(const args : TExprBaseListExec; var result : Variant) of object;
   TMagicProcedureDoEvalEvent = procedure(const args : TExprBaseListExec) of object;
   TMagicFuncDoEvalDataEvent = procedure(const args : TExprBaseListExec; var result : IDataContext) of object;
   TMagicFuncDoEvalAsInterfaceEvent = procedure(const args : TExprBaseListExec; var Result : IUnknown) of object;
   TMagicFuncDoEvalAsIntegerEvent = function(const args : TExprBaseListExec) : Int64 of object;
   TMagicFuncDoEvalAsBooleanEvent = function(const args : TExprBaseListExec) : Boolean of object;
   TMagicFuncDoEvalAsFloatEvent = procedure(const args : TExprBaseListExec; var Result : Double) of object;
   TMagicFuncDoEvalAsStringEvent = procedure(const args : TExprBaseListExec; var Result : String) of object;

   // TInternalMagicFunction
   //
   TInternalMagicFunction = class (TInternalFunction)
      public
         constructor Create(table: TSymbolTable; const funcName: String;
                            const params : TParamArray; const funcType: String;
                            const flags : TInternalFunctionFlags;
                            compositeSymbol : TCompositeTypeSymbol;
                            const helperName : String); override;
         function MagicFuncExprClass : TMagicFuncExprClass; virtual; abstract;
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override; final;
   end;

   // TInternalMagicProcedure
   //
   TInternalMagicProcedure = class(TInternalMagicFunction)
      public
         procedure DoEvalProc(const args : TExprBaseListExec); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;

   // TInternalMagicDataFunction
   //
   TInternalMagicDataFunction = class(TInternalMagicFunction)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicDataFunctionClass = class of TInternalMagicDataFunction;

   // TInternalMagicVariantFunction
   //
   TInternalMagicVariantFunction = class(TInternalMagicFunction)
      public
         procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicVariantFunctionClass = class of TInternalMagicVariantFunction;

   // TInternalMagicInterfaceFunction
   //
   TInternalMagicInterfaceFunction = class(TInternalMagicFunction)
      public
         procedure DoEvalAsInterface(const args : TExprBaseListExec; var result : IUnknown); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicInterfaceFunctionClass = class of TInternalMagicInterfaceFunction;

   // TInternalMagicIntFunction
   //
   TInternalMagicIntFunction = class(TInternalMagicFunction)
      public
         function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicIntFunctionClass = class of TInternalMagicIntFunction;

   // TInternalMagicBoolFunction
   //
   TInternalMagicBoolFunction = class(TInternalMagicFunction)
      public
         function DoEvalAsBoolean(const args : TExprBaseListExec) : Boolean; virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicBoolFunctionClass = class of TInternalMagicBoolFunction;

   // TInternalMagicFloatFunction
   //
   TInternalMagicFloatFunction = class(TInternalMagicFunction)
      public
         procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicFloatFunctionClass = class of TInternalMagicFloatFunction;

   // TInternalMagicStringFunction
   //
   TInternalMagicStringFunction = class(TInternalMagicFunction)
      public
         procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicStringFunctionClass = class of TInternalMagicStringFunction;

   // TMagicFuncSymbol
   //
   TMagicFuncSymbol = class sealed (TFuncSymbol)
      private
         FInternalFunction : TInternalMagicFunction;

      public
         destructor Destroy; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         function IsType : Boolean; override;

         property InternalFunction : TInternalMagicFunction read FInternalFunction write FInternalFunction;
   end;

   // TMagicMethodSymbol
   //
   TMagicMethodSymbol = class(TMethodSymbol)
      private
         FInternalFunction : TInternalFunction;

      public
         destructor Destroy; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         function IsType : Boolean; override;

         property InternalFunction : TInternalFunction read FInternalFunction write FInternalFunction;
   end;

   // TMagicStaticMethodSymbol
   //
   TMagicStaticMethodSymbol = class(TMagicMethodSymbol)
      protected
         function GetInternalFunction : TInternalMagicFunction;
         procedure SetInternalFunction(const val : TInternalMagicFunction);

      public
         property InternalFunction : TInternalMagicFunction read GetInternalFunction write SetInternalFunction;
   end;

   // TMagicFuncExpr
   //
   TMagicFuncExpr = class(TFuncExprBase)
      public
         class function CreateMagicFuncExpr(context : TdwsCompilerContext;
                           const scriptPos : TScriptPos; magicFuncSym : TMagicFuncSymbol) : TMagicFuncExpr;

         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); virtual;

         function ExpectedArg : TParamSymbol; override;

         function IsWritable : Boolean; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         procedure CompileTimeCheck(context : TdwsCompilerContext); override;
   end;

   // TMagicVariantFuncExpr
   //
   TMagicVariantFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalEvent;
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // TMagicInterfaceFuncExpr
   //
   TMagicInterfaceFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsInterfaceEvent;
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsInterface(exec : TdwsExecution; var Result : IUnknown); override;
   end;

   // TMagicProcedureExpr
   //
   TMagicProcedureExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicProcedureDoEvalEvent;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // TMagicDataFuncExpr
   //
   TMagicDataFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalDataEvent;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
   end;

   // TMagicIntFuncExpr
   //
   TMagicIntFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsIntegerEvent;
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // TMagicStringFuncExpr
   //
   TMagicStringFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsStringEvent;
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsString(exec : TdwsExecution; var result : String); override;
   end;

   // TMagicFloatFuncExpr
   //
   TMagicFloatFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsFloatEvent;
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // TMagicBoolFuncExpr
   //
   TMagicBoolFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsBooleanEvent;
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            func : TFuncSymbol; internalFunc : TInternalMagicFunction); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // Inc/Dec/Succ/Pred
   TMagicIteratorFuncExpr = class(TMagicFuncExpr)
      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                            left, right : TTypedExpr); reintroduce;
         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // result = Inc(left, right)
   TIncVarFuncExpr = class(TMagicIteratorFuncExpr)
      protected
         function DoInc(exec : TdwsExecution) : PVarData;
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // result = Dec(left, right)
   TDecVarFuncExpr = class(TMagicIteratorFuncExpr)
      protected
         function DoDec(exec : TdwsExecution) : PVarData;
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // result = Succ(left, right)
   TSuccFuncExpr = class(TMagicIteratorFuncExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // result = Pred(left, right)
   TPredFuncExpr = class(TMagicIteratorFuncExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

procedure RegisterInternalInterfaceFunction(InternalFunctionClass: TInternalMagicInterfaceFunctionClass;
      const FuncName: String; const FuncParams: array of String; const funcType : String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsCompilerUtils;

// RegisterInternalInterfaceFunction
//
procedure RegisterInternalInterfaceFunction(InternalFunctionClass: TInternalMagicInterfaceFunctionClass;
      const FuncName: String; const FuncParams: array of String; const funcType : String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, funcType, flags, helperName);
end;

// RegisterInternalIntFunction
//
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, SYS_INTEGER, flags, helperName);
end;

// RegisterInternalBoolFunction
//
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, SYS_BOOLEAN, flags, helperName);
end;

// RegisterInternalFloatFunction
//
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, SYS_FLOAT, flags, helperName);
end;

// RegisterInternalStringFunction
//
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: String; const FuncParams: array of String;
      const flags : TInternalFunctionFlags = []; const helperName : String = '');
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, SYS_STRING, flags, helperName);
end;

// ------------------
// ------------------ TMagicFuncSymbol ------------------
// ------------------

procedure TMagicFuncSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   FInternalParams.Initialize(msgs);
end;

// IsType
//
function TMagicFuncSymbol.IsType : Boolean;
begin
   Result:=False;
end;

// Destroy
//
destructor TMagicFuncSymbol.Destroy;
begin
   FInternalFunction.Free;
   FInternalFunction:=nil;
   inherited;
end;

// ------------------
// ------------------ TMagicMethodSymbol ------------------
// ------------------

// Initialize
//
procedure TMagicMethodSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   FInternalParams.Initialize(msgs);
end;

// IsType
//
function TMagicMethodSymbol.IsType : Boolean;
begin
   Result:=False;
end;

// Destroy
//
destructor TMagicMethodSymbol.Destroy;
begin
   FInternalFunction.Free;
   FInternalFunction:=nil;
   inherited;
end;

// ------------------
// ------------------ TInternalMagicFunction ------------------
// ------------------

// Create
//
constructor TInternalMagicFunction.Create(table : TSymbolTable;
      const funcName : String; const params : TParamArray; const funcType : String;
      const flags : TInternalFunctionFlags;
      compositeSymbol : TCompositeTypeSymbol;
      const helperName : String);
var
   sym : TMagicFuncSymbol;
   ssym : TMagicStaticMethodSymbol;
begin
   if iffStaticMethod in flags then begin
      ssym:=TMagicStaticMethodSymbol.Generate(table, mkClassMethod, [maStatic],
                                              funcName, params, funcType,
                                              compositeSymbol,
                                              cvPublic, (iffOverloaded in flags));
      ssym.InternalFunction:=Self;
      ssym.IsStateless:=(iffStateLess in flags);
      ssym.IsExternal:=True;
      compositeSymbol.AddMethod(ssym);
      Assert(helperName=''); // unsupported
      self.FuncSymbol := ssym;
   end else begin
      sym:=TMagicFuncSymbol.Generate(table, funcName, params, funcType);
      sym.params.AddParent(table);
      sym.InternalFunction:=Self;
      sym.IsStateless:=(iffStateLess in flags);
      sym.IsOverloaded:=(iffOverloaded in flags);
      table.AddSymbol(sym);
      self.FuncSymbol := sym;
      if helperName<>'' then
         CompilerUtils.AddProcHelper(helperName, table, sym, nil);
   end;
end;

// Call
//
procedure TInternalMagicFunction.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
begin
   Assert(False);
end;

// ------------------
// ------------------ TMagicFuncExpr ------------------
// ------------------

// CreateMagicFuncExpr
//
class function TMagicFuncExpr.CreateMagicFuncExpr(context : TdwsCompilerContext;
         const scriptPos : TScriptPos; magicFuncSym : TMagicFuncSymbol) : TMagicFuncExpr;
var
   internalFunc : TInternalMagicFunction;
begin
   internalFunc:=magicFuncSym.InternalFunction;
   Result:=internalFunc.MagicFuncExprClass.Create(context, scriptPos, magicFuncSym, internalFunc);
end;

// Create
//
constructor TMagicFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                  func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(scriptPos, func);
end;

// ExpectedArg
//
function TMagicFuncExpr.ExpectedArg : TParamSymbol;
begin
   if FArgs.Count<FuncSym.Params.Count then
      Result:=(FuncSym.Params[FArgs.Count] as TParamSymbol)
   else Result:=nil;
end;

// IsWritable
//
function TMagicFuncExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// GetDataPtr
//
procedure TMagicFuncExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   exec.DataContext_CreateBase(FResultAddr, Result);
end;

// CompileTimeCheck
//
procedure TMagicFuncExpr.CompileTimeCheck(context : TdwsCompilerContext);
begin
   TMagicFuncSymbol(FuncSym).InternalFunction.CompileTimeCheck(context, Self);
end;

// ------------------
// ------------------ TMagicVariantFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicVariantFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                         func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicVariantFunction).DoEvalAsVariant;
end;

// EvalAsVariant
//
procedure TMagicVariantFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      FOnEval(execRec, Result);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicInterfaceFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicInterfaceFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                           func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicInterfaceFunction).DoEvalAsInterface;
end;

// EvalAsVariant
//
procedure TMagicInterfaceFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   intf : IUnknown;
begin
   EvalAsInterface(exec, intf);
   VarCopySafe(Result, intf);
end;

// EvalAsInterface
//
procedure TMagicInterfaceFuncExpr.EvalAsInterface(exec : TdwsExecution; var Result : IUnknown);
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      FOnEval(execRec, Result);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicDataFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicDataFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                      func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicDataFunction).DoEval;
   SetResultAddr(context.Prog as TdwsProgram, nil);
end;

// EvalNoResult
//
procedure TMagicDataFuncExpr.EvalNoResult(exec : TdwsExecution);
var
   buf : IDataContext;
begin
   GetDataPtr(exec, buf);
end;

// EvalAsVariant
//
procedure TMagicDataFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   buf : IDataContext;
begin
   GetDataPtr(exec, buf);
   buf.EvalAsVariant(0, Result);
end;

// GetDataPtr
//
procedure TMagicDataFuncExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      exec.DataContext_CreateBase(FResultAddr, Result);
      FOnEval(execRec, Result);
   except
      RaiseScriptError(exec);
      raise;
   end;
end;

// ------------------
// ------------------ TMagicIntFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicIntFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                     func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicIntFunction).DoEvalAsInteger;
end;

// EvalNoResult
//
procedure TMagicIntFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsInteger(exec);
end;

// EvalAsVariant
//
procedure TMagicIntFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, EvalAsInteger(exec));
end;

// EvalAsInteger
//
function TMagicIntFuncExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      Result:=FOnEval(execRec);
   except
      on E: EScriptException do
         raise
   else
      Result:=0;
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicStringFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicStringFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                        func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval := (internalFunc as TInternalMagicStringFunction).DoEvalAsString;
end;

// EvalNoResult
//
procedure TMagicStringFuncExpr.EvalNoResult(exec : TdwsExecution);
var
   buf : String;
begin
   EvalAsString(exec, buf);
end;

// EvalAsVariant
//
procedure TMagicStringFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   buf : String;
begin
   EvalAsString(exec, buf);
   VarCopySafe(Result, buf);
end;

// EvalAsString
//
procedure TMagicStringFuncExpr.EvalAsString(exec : TdwsExecution; var result : String);
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      FOnEval(execRec, Result);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicFloatFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicFloatFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                       func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicFloatFunction).DoEvalAsFloat;
end;

// EvalNoResult
//
procedure TMagicFloatFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsFloat(exec);
end;

// EvalAsVariant
//
procedure TMagicFloatFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, EvalAsFloat(exec));
end;

// EvalAsFloat
//
function TMagicFloatFuncExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      FOnEval(execRec, Result);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicBoolFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicBoolFuncExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                      func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicBoolFunction).DoEvalAsBoolean;
end;

// EvalNoResult
//
procedure TMagicBoolFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsBoolean(exec);
end;

// EvalAsVariant
//
procedure TMagicBoolFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, EvalAsBoolean(exec));
end;

// EvalAsBoolean
//
function TMagicBoolFuncExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      Result:=FOnEval(execRec);
   except
      Result:=False;
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicProcedureExpr ------------------
// ------------------

// Create
//
constructor TMagicProcedureExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                       func : TFuncSymbol; internalFunc : TInternalMagicFunction);
begin
   inherited Create(context, scriptPos, func, internalFunc);
   FOnEval:=(internalFunc as TInternalMagicProcedure).DoEvalProc;
end;

// EvalNoResult
//
procedure TMagicProcedureExpr.EvalNoResult(exec : TdwsExecution);
var
   execRec : TExprBaseListExec;
begin
   execRec.ListRec:=FArgs;
   execRec.Exec:=exec;
   execRec.Expr:=Self;
   try
      FOnEval(execRec);
   except
      on E : EScriptError do
         raise
      else RaiseScriptError(exec);
   end;
end;

// EvalAsVariant
//
procedure TMagicProcedureExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   EvalNoResult(exec);
end;

// ------------------
// ------------------ TMagicIteratorFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicIteratorFuncExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                          left, right : TTypedExpr);
begin
   inherited Create(context, aScriptPos, nil, nil);
   FTyp:=left.Typ;
   AddArg(left);
   AddArg(right);
end;

// EvalNoResult
//
procedure TMagicIteratorFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsInteger(exec);
end;

// EvalAsVariant
//
procedure TMagicIteratorFuncExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, EvalAsInteger(exec));
end;

// ------------------
// ------------------ TIncVarFuncExpr ------------------
// ------------------

// DoInc
//
function TIncVarFuncExpr.DoInc(exec : TdwsExecution) : PVarData;
var
   left : TDataExpr;
begin
   left:=TDataExpr(FArgs.ExprBase[0]);
   Result:=PVarData(left.DataPtr[exec].AsPVariant(0));
   Assert(Result.VType=varInt64);
   Inc(Result.VInt64, FArgs.ExprBase[1].EvalAsInteger(exec));
end;

// EvalNoResult
//
procedure TIncVarFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   DoInc(exec);
end;

// EvalAsInteger
//
function TIncVarFuncExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=DoInc(exec).VInt64;
end;

// ------------------
// ------------------ TDecVarFuncExpr ------------------
// ------------------

// DoDec
//
function TDecVarFuncExpr.DoDec(exec : TdwsExecution) : PVarData;
var
   left : TDataExpr;
begin
   left:=TDataExpr(FArgs.ExprBase[0]);
   Result:=PVarData(left.DataPtr[exec].AsPVariant(0));
   Assert(Result.VType=varInt64);
   Dec(Result.VInt64, FArgs.ExprBase[1].EvalAsInteger(exec));
end;

// EvalNoResult
//
procedure TDecVarFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   DoDec(exec);
end;

// EvalAsInteger
//
function TDecVarFuncExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=DoDec(exec).VInt64;
end;

// ------------------
// ------------------ TSuccFuncExpr ------------------
// ------------------

// EvalAsInteger
//
function TSuccFuncExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FArgs.ExprBase[0].EvalAsInteger(exec)+FArgs.ExprBase[1].EvalAsInteger(exec);
end;

// ------------------
// ------------------ TPredFuncExpr ------------------
// ------------------

// EvalAsInteger
//
function TPredFuncExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FArgs.ExprBase[0].EvalAsInteger(exec)-FArgs.ExprBase[1].EvalAsInteger(exec);
end;

// ------------------
// ------------------ TInternalMagicProcedure ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicProcedure.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicProcedureExpr;
end;

// ------------------
// ------------------ TInternalMagicDataFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicDataFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicDataFuncExpr;
end;

// ------------------
// ------------------ TInternalMagicVariantFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicVariantFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicVariantFuncExpr;
end;

// ------------------
// ------------------ TInternalMagicInterfaceFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicInterfaceFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicInterfaceFuncExpr;
end;

// ------------------
// ------------------ TInternalMagicIntFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicIntFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicIntFuncExpr;
end;

// ------------------
// ------------------ TInternalMagicBoolFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicBoolFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicBoolFuncExpr;
end;

// ------------------
// ------------------ TInternalMagicFloatFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicFloatFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicFloatFuncExpr;
end;

// ------------------
// ------------------ TInternalMagicStringFunction ------------------
// ------------------

// MagicFuncExprClass
//
function TInternalMagicStringFunction.MagicFuncExprClass : TMagicFuncExprClass;
begin
   Result:=TMagicStringFuncExpr;
end;

// ------------------
// ------------------ TMagicStaticMethodSymbol ------------------
// ------------------

// GetInternalFunction
//
function TMagicStaticMethodSymbol.GetInternalFunction : TInternalMagicFunction;
begin
   Result:=(inherited InternalFunction) as TInternalMagicFunction;
end;

// SetInternalFunction
//
procedure TMagicStaticMethodSymbol.SetInternalFunction(const val : TInternalMagicFunction);
begin
   inherited InternalFunction:=val;
end;

end.
