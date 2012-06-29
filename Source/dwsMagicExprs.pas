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

uses Classes, SysUtils, dwsExprs, dwsSymbols, dwsStack, dwsErrors, dwsFunctions,
   dwsUtils, dwsCoreExprs;

type

   TMagicFuncExpr = class;
   TMagicFuncExprClass = class of TMagicFuncExpr;

   TMagicFuncDoEvalEvent = function(args : TExprBaseList) : Variant of object;
   TMagicProcedureDoEvalEvent = procedure(args : TExprBaseList) of object;
   TMagicFuncDoEvalDataEvent = procedure(args : TExprBaseList; var result : TDataPtr) of object;
   TMagicFuncDoEvalAsIntegerEvent = function(args : TExprBaseList) : Int64 of object;
   TMagicFuncDoEvalAsBooleanEvent = function(args : TExprBaseList) : Boolean of object;
   TMagicFuncDoEvalAsFloatEvent = procedure(args : TExprBaseList; var Result : Double) of object;
   TMagicFuncDoEvalAsStringEvent = procedure(args : TExprBaseList; var Result : UnicodeString) of object;

   // TInternalMagicFunction
   //
   TInternalMagicFunction = class(TInternalFunction)
      public
         constructor Create(table: TSymbolTable; const funcName: UnicodeString;
                            const params : TParamArray; const funcType: UnicodeString;
                            const flags : TInternalFunctionFlags = []); override;
         function MagicFuncExprClass : TMagicFuncExprClass; virtual; abstract;
   end;

   // TInternalMagicProcedure
   //
   TInternalMagicProcedure = class(TInternalMagicFunction)
      public
         procedure DoEvalProc(args : TExprBaseList); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;

   // TInternalMagicDataFunction
   //
   TInternalMagicDataFunction = class(TInternalMagicFunction)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicDataFunctionClass = class of TInternalMagicDataFunction;

   // TInternalMagicVariantFunction
   //
   TInternalMagicVariantFunction = class(TInternalMagicFunction)
      public
         function DoEvalAsVariant(args : TExprBaseList) : Variant; virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicVariantFunctionClass = class of TInternalMagicVariantFunction;

   // TInternalMagicIntFunction
   //
   TInternalMagicIntFunction = class(TInternalMagicFunction)
      public
         function DoEvalAsInteger(args : TExprBaseList) : Int64; virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicIntFunctionClass = class of TInternalMagicIntFunction;

   // TInternalMagicBoolFunction
   //
   TInternalMagicBoolFunction = class(TInternalMagicFunction)
      public
         function DoEvalAsBoolean(args : TExprBaseList) : Boolean; virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicBoolFunctionClass = class of TInternalMagicBoolFunction;

   // TInternalMagicFloatFunction
   //
   TInternalMagicFloatFunction = class(TInternalMagicFunction)
      public
         procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicFloatFunctionClass = class of TInternalMagicFloatFunction;

   // TInternalMagicStringFunction
   //
   TInternalMagicStringFunction = class(TInternalMagicFunction)
      public
         procedure DoEvalAsString(args : TExprBaseList; var Result : UnicodeString); virtual; abstract;
         function MagicFuncExprClass : TMagicFuncExprClass; override;
   end;
   TInternalMagicStringFunctionClass = class of TInternalMagicStringFunction;

   // TMagicFuncSymbol
   //
   TMagicFuncSymbol = class(TFuncSymbol)
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

   // TMagicFuncExpr
   //
   TMagicFuncExpr = class(TFuncExprBase)
      protected
         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;

      public
         class function CreateMagicFuncExpr(prog : TdwsProgram;
                           const pos : TScriptPos; magicFuncSym : TMagicFuncSymbol) : TMagicFuncExpr;

         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); virtual;

         function ExpectedArg : TParamSymbol; override;

         function IsWritable : Boolean; override;
   end;

   // TMagicVariantFuncExpr
   //
   TMagicVariantFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalEvent;
      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // TMagicProcedureExpr
   //
   TMagicProcedureExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicProcedureDoEvalEvent;
      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // TMagicDataFuncExpr
   //
   TMagicDataFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalDataEvent;

      protected
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // TMagicIntFuncExpr
   //
   TMagicIntFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsIntegerEvent;
      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // TMagicStringFuncExpr
   //
   TMagicStringFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsStringEvent;
      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); override;
   end;

   // TMagicFloatFuncExpr
   //
   TMagicFloatFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsFloatEvent;
      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // TMagicBoolFuncExpr
   //
   TMagicBoolFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsBooleanEvent;
      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // Inc/Dec/Succ/Pred
   TMagicIteratorFuncExpr = class(TMagicFuncExpr)
      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos;
                            left, right : TTypedExpr); reintroduce;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
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

procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterInternalIntFunction
//
procedure RegisterInternalIntFunction(InternalFunctionClass: TInternalMagicIntFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Integer', flags);
end;

// RegisterInternalBoolFunction
//
procedure RegisterInternalBoolFunction(InternalFunctionClass: TInternalMagicBoolFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Boolean', flags);
end;

// RegisterInternalFloatFunction
//
procedure RegisterInternalFloatFunction(InternalFunctionClass: TInternalMagicFloatFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'Float', flags);
end;

// RegisterInternalStringFunction
//
procedure RegisterInternalStringFunction(InternalFunctionClass: TInternalMagicStringFunctionClass;
      const FuncName: UnicodeString; const FuncParams: array of UnicodeString; const flags : TInternalFunctionFlags = []);
begin
   RegisterInternalFunction(InternalFunctionClass, FuncName, FuncParams, 'String', flags);
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
      const funcName : UnicodeString; const params : TParamArray; const funcType : UnicodeString;
      const flags : TInternalFunctionFlags = []);
var
  sym : TMagicFuncSymbol;
begin
  sym:=TMagicFuncSymbol.Generate(table, funcName, params, funcType);
  sym.params.AddParent(table);
  sym.InternalFunction:=Self;
  sym.IsStateless:=(iffStateLess in flags);
  sym.IsOverloaded:=(iffOverloaded in flags);
  table.AddSymbol(sym);
end;

// ------------------
// ------------------ TMagicFuncExpr ------------------
// ------------------

// CreateMagicFuncExpr
//
class function TMagicFuncExpr.CreateMagicFuncExpr(prog : TdwsProgram;
         const pos : TScriptPos; magicFuncSym : TMagicFuncSymbol) : TMagicFuncExpr;
begin
   Result:=magicFuncSym.InternalFunction.MagicFuncExprClass.Create(prog, pos, magicFuncSym);
end;

// Create
//
constructor TMagicFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
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

// GetData
//
function TMagicFuncExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result:=exec.Stack.Data;
end;

// GetAddr
//
function TMagicFuncExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=exec.Stack.BasePointer+FResultAddr;
end;

// ------------------
// ------------------ TMagicVariantFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicVariantFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicVariantFunction(func.InternalFunction).DoEvalAsVariant;
end;

// Eval
//
function TMagicVariantFuncExpr.Eval(exec : TdwsExecution) : Variant;
var
   execRec : TExprBaseListExec;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      Result:=FOnEval(@execRec);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicDataFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicDataFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicDataFunction(Func.InternalFunction).DoEval;
   SetResultAddr(prog, nil);
end;

// EvalNoResult
//
procedure TMagicDataFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   GetData(exec);
end;

// Eval
//
function TMagicDataFuncExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=Data[exec][Addr[exec]];
end;

// GetData
//
function TMagicDataFuncExpr.GetData(exec : TdwsExecution) : TData;
var
   execRec : TExprBaseListExec;
   dataPtr : TDataPtr;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      Result:=exec.Stack.Data;
      dataPtr:=TDataPtr.Create(exec.Stack.Data, exec.Stack.BasePointer+FResultAddr);
      FOnEval(@execRec, dataPtr);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicIntFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicIntFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicIntFunction(Func.InternalFunction).DoEvalAsInteger;
end;

// EvalNoResult
//
procedure TMagicIntFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsInteger(exec);
end;

// Eval
//
function TMagicIntFuncExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsInteger(exec);
end;

// EvalAsInteger
//
function TMagicIntFuncExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   execRec : TExprBaseListExec;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      Result:=FOnEval(@execRec);
   except
      Result:=0;
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicStringFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicStringFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicStringFunction(Func.InternalFunction).DoEvalAsString;
end;

// EvalNoResult
//
procedure TMagicStringFuncExpr.EvalNoResult(exec : TdwsExecution);
var
   buf : UnicodeString;
begin
   EvalAsString(exec, buf);
end;

// Eval
//
function TMagicStringFuncExpr.Eval(exec : TdwsExecution) : Variant;
var
   buf : UnicodeString;
begin
   EvalAsString(exec, buf);
   Result:=buf;
end;

// EvalAsString
//
procedure TMagicStringFuncExpr.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
var
   execRec : TExprBaseListExec;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      FOnEval(@execRec, Result);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicFloatFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicFloatFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicFloatFunction(Func.InternalFunction).DoEvalAsFloat;
end;

// EvalNoResult
//
procedure TMagicFloatFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsFloat(exec);
end;

// Eval
//
function TMagicFloatFuncExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsFloat(exec);
end;

// EvalAsFloat
//
function TMagicFloatFuncExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   execRec : TExprBaseListExec;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      FOnEval(@execRec, Result);
   except
      RaiseScriptError(exec);
   end;
end;

// ------------------
// ------------------ TMagicBoolFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicBoolFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicBoolFunction(Func.InternalFunction).DoEvalAsBoolean;
end;

// EvalNoResult
//
procedure TMagicBoolFuncExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsBoolean(exec);
end;

// Eval
//
function TMagicBoolFuncExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsBoolean(exec);
end;

// EvalAsBoolean
//
function TMagicBoolFuncExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   execRec : TExprBaseListExec;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      Result:=FOnEval(@execRec);
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
constructor TMagicProcedureExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TMagicFuncSymbol);
begin
   inherited Create(prog, scriptPos, func);
   FOnEval:=TInternalMagicProcedure(Func.InternalFunction).DoEvalProc;
end;

// EvalNoResult
//
procedure TMagicProcedureExpr.EvalNoResult(exec : TdwsExecution);
var
   execRec : TExprBaseListExec;
begin
   execRec.List:=@FArgs;
   execRec.Exec:=exec;
   try
      FOnEval(@execRec);
   except
      RaiseScriptError(exec);
   end;
end;

// Eval
//
function TMagicProcedureExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalNoResult(exec);
end;

// ------------------
// ------------------ TMagicIteratorFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicIteratorFuncExpr.Create(prog : TdwsProgram; const pos : TScriptPos;
                                          left, right : TTypedExpr);
begin
   inherited Create(prog, pos, nil);
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

// Eval
//
function TMagicIteratorFuncExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsInteger(exec);
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
   Result:=@left.Data[exec][left.Addr[exec]];
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
   Result:=@left.Data[exec][left.Addr[exec]];
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

end.
