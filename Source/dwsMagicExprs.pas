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

uses dwsExprs, dwsSymbols, dwsStack, dwsErrors, dwsFunctions, dwsUtils;

type

   // TMagicFuncExpr
   //
   TMagicFuncExpr = class(TFuncExprBase)
      protected
         function GetData: TData; override;
         function GetAddr: Integer; override;
      public
         function AddArg(Arg: TNoPosExpr) : TSymbol; override;
         function IsWritable : Boolean; override;
   end;

   // TMagicVariantFuncExpr
   //
   TMagicVariantFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         function Eval: Variant; override;
   end;

   // TMagicProcedureExpr
   //
   TMagicProcedureExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicProcedureDoEvalEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         function Eval: Variant; override;
   end;

   // TMagicIntFuncExpr
   //
   TMagicIntFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsIntegerEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         function Eval : Variant; override;
         function EvalAsInteger : Int64; override;
   end;

   // TMagicStringFuncExpr
   //
   TMagicStringFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsStringEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         function Eval : Variant; override;
         procedure EvalAsString(var Result : String); override;
   end;

   // TMagicFloatFuncExpr
   //
   TMagicFloatFuncExpr = class(TMagicFuncExpr)
      private
         FOnEval : TMagicFuncDoEvalAsFloatEvent;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMagicFuncSymbol);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         function Eval: Variant; override;
         procedure EvalAsFloat(var Result : Double); override;
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

// AddArg
//
function TMagicFuncExpr.AddArg(Arg: TNoPosExpr) : TSymbol;
begin
   if FArgs.Count<FFunc.Params.Count then
      Result:=FFunc.Params[FArgs.Count]
   else Result:=nil;

   FArgs.Add(Arg);
end;

// IsWritable
//
function TMagicFuncExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// GetData
//
function TMagicFuncExpr.GetData: TData;
begin
   Prog.Stack.Data[FProg.Stack.BasePointer]:=Eval;
   Result:=Prog.Stack.Data;
end;

// GetAddr
//
function TMagicFuncExpr.GetAddr: Integer;
begin
   Result:=FProg.Stack.BasePointer;
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
function TMagicVariantFuncExpr.Eval: Variant;
begin
   try
      Result:=FOnEval(@FArgs);
   except
      FProg.Msgs.SetLastScriptError(Pos);
      raise;
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
procedure TMagicIntFuncExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   EvalAsInteger;
end;

// Eval
//
function TMagicIntFuncExpr.Eval: Variant;
begin
   Result:=EvalAsInteger;
end;

// EvalAsInteger
//
function TMagicIntFuncExpr.EvalAsInteger : Int64;
begin
   try
      Result:=FOnEval(@FArgs);
   except
      FProg.Msgs.SetLastScriptError(Pos);
      raise;
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
procedure TMagicStringFuncExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   buf : String;
begin
   EvalAsString(buf);
end;

// Eval
//
function TMagicStringFuncExpr.Eval: Variant;
var
   buf : String;
begin
   EvalAsString(buf);
   Result:=buf;
end;

// EvalAsString
//
procedure TMagicStringFuncExpr.EvalAsString(var Result : String);
begin
   try
      FOnEval(@FArgs, Result);
   except
      FProg.Msgs.SetLastScriptError(Pos);
      raise;
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
procedure TMagicFloatFuncExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   buf : Double;
begin
   EvalAsFloat(buf);
end;

// Eval
//
function TMagicFloatFuncExpr.Eval : Variant;
var
   buf : Double;
begin
   EvalAsFloat(buf);
   Result:=buf;
end;

// EvalAsFloat
//
procedure TMagicFloatFuncExpr.EvalAsFloat(var Result : Double);
begin
   try
      FOnEval(@FArgs, Result);
   except
      FProg.Msgs.SetLastScriptError(Pos);
      raise;
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
procedure TMagicProcedureExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   try
      FOnEval(@FArgs);
   except
      FProg.Msgs.SetLastScriptError(Pos);
      raise;
   end;
end;

// Eval
//
function TMagicProcedureExpr.Eval: Variant;
var
   status : TExecutionStatusResult;
begin
   status:=esrNone;
   EvalNoResult(status);
   Assert(status=esrNone);
end;

end.
