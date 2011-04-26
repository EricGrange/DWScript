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
{$I dws.inc}
unit dwsMagicExprs;

interface

uses Classes, SysUtils, dwsExprs, dwsSymbols, dwsStack, dwsErrors, dwsFunctions, dwsUtils;

type

   // TMagicFuncExpr
   //
   TMagicFuncExpr = class(TFuncExprBase)
      protected
         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;

      public
         class function CreateMagicFuncExpr(prog : TdwsProgram;
                           const pos : TScriptPos; magicFuncSym : TMagicFuncSymbol) : TMagicFuncExpr;

         function AddArg(Arg: TTypedExpr) : TSymbol; override;
         function ExpectedArgType : TTypeSymbol; override;
         function IsWritable : Boolean; override;
   end;

   // TMagicVariantFuncExpr
   //
   TMagicVariantFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // TMagicProcedureExpr
   //
   TMagicProcedureExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicProcedureDoEvalEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // TMagicIntFuncExpr
   //
   TMagicIntFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsIntegerEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
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
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
   end;

   // TMagicFloatFuncExpr
   //
   TMagicFloatFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsFloatEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TMagicFuncExpr ------------------
// ------------------

// CreateMagicFuncExpr
//
class function TMagicFuncExpr.CreateMagicFuncExpr(prog : TdwsProgram;
         const pos : TScriptPos; magicFuncSym : TMagicFuncSymbol) : TMagicFuncExpr;
var
   internalFunc : TObject;
begin
   internalFunc:=magicFuncSym.InternalFunction;
   if internalFunc.InheritsFrom(TInternalMagicIntFunction) then
      Result:=TMagicIntFuncExpr.Create(prog, pos, magicFuncSym)
   else if internalFunc.InheritsFrom(TInternalMagicFloatFunction) then
      Result:=TMagicFloatFuncExpr.Create(prog, pos, magicFuncSym)
   else if internalFunc.InheritsFrom(TInternalMagicStringFunction) then
      Result:=TMagicStringFuncExpr.Create(prog, pos, magicFuncSym)
   else if internalFunc.InheritsFrom(TInternalMagicProcedure) then
      Result:=TMagicProcedureExpr.Create(prog, pos, magicFuncSym)
   else Result:=TMagicVariantFuncExpr.Create(prog, pos, magicFuncSym);
end;

// AddArg
//
function TMagicFuncExpr.AddArg(arg: TTypedExpr) : TSymbol;
begin
   if FArgs.Count<FFunc.Params.Count then begin
      Result:=FFunc.Params[FArgs.Count];
   end else Result:=nil;
   FArgs.Add(arg);
end;

// ExpectedArgType
//
function TMagicFuncExpr.ExpectedArgType : TTypeSymbol;
begin
   if FArgs.Count<FFunc.Params.Count then
      Result:=FFunc.Params[FArgs.Count].Typ
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
   exec.Stack.Data[exec.Stack.BasePointer]:=Eval(exec);
   Result:=exec.Stack.Data;
end;

// GetAddr
//
function TMagicFuncExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=exec.Stack.BasePointer;
end;

// ------------------
// ------------------ TMagicVariantFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicVariantFuncExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
begin
   inherited Create(Prog, Pos, Func);
   FOnEval:=TInternalMagicFunction(Func.InternalFunction).DoEval;
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
// ------------------ TMagicIntFuncExpr ------------------
// ------------------

// Create
//
constructor TMagicIntFuncExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
begin
   inherited Create(Prog, Pos, Func);
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
constructor TMagicStringFuncExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
begin
   inherited Create(Prog, Pos, Func);
   FOnEval:=TInternalMagicStringFunction(Func.InternalFunction).DoEvalAsString;
end;

// EvalNoResult
//
procedure TMagicStringFuncExpr.EvalNoResult(exec : TdwsExecution);
var
   buf : String;
begin
   EvalAsString(exec, buf);
end;

// Eval
//
function TMagicStringFuncExpr.Eval(exec : TdwsExecution) : Variant;
var
   buf : String;
begin
   EvalAsString(exec, buf);
   Result:=buf;
end;

// EvalAsString
//
procedure TMagicStringFuncExpr.EvalAsString(exec : TdwsExecution; var Result : String);
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
constructor TMagicFloatFuncExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
begin
   inherited Create(Prog, Pos, Func);
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
// ------------------ TMagicProcedureExpr ------------------
// ------------------

// Create
//
constructor TMagicProcedureExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
begin
   inherited Create(Prog, Pos, Func);
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

end.
