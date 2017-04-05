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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsResultFunctions;

interface

{$I dws.inc}

uses
   dwsExprs, dwsExprList, dwsMagicExprs, dwsSymbols, dwsStrings,
   dwsFunctions, dwsCoreExprs, dwsConstExprs, dwsUtils;

type

   TPrintFunction = class(TInternalMagicProcedure)
      public
         function DoPrint(const args : TExprBaseListExec) : TdwsResult;// inline;
         procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TPrintLnFunction = class(TPrintFunction)
      public
         procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

procedure RegisterStandardResultFunctions(table : TSymbolTable);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterStandardResultFunctions
//
procedure RegisterStandardResultFunctions(table : TSymbolTable);
begin
   TPrintFunction.Create(table, 'Print', ['s', SYS_VARIANT], '', []);
   TPrintLnFunction.Create(table, 'PrintLn', ['s', SYS_VARIANT], '', []);
end;

// ------------------
// ------------------ TPrintFunction ------------------
// ------------------

// DoPrint
//
function TPrintFunction.DoPrint(const args : TExprBaseListExec) : TdwsResult;
var
   exprBase : TExprBase;

   procedure EvalString(Result : TdwsResult);
   var
      buf : UnicodeString;
   begin
      exprBase.EvalAsString(args.Exec, buf);
      Result:=TdwsProgramExecution(args.Exec).Result;
      Result.AddString(buf);
   end;

var
   exprBaseClass : TClass;
begin
   exprBase:=args.ExprBase[0];
   exprBaseClass:=exprBase.ClassType;
   Result:=TdwsProgramExecution(args.Exec).Result;
   if exprBaseClass=TConstStringExpr then begin
      Result.AddString(TConstStringExpr(exprBase).Value);
   end else if exprBaseClass=TIntVarExpr then begin
      Result.AddString(exprBase.EvalAsInteger(args.Exec));
   end else begin
      EvalString(Result);
   end;
end;

// DoEvalProc
//
procedure TPrintFunction.DoEvalProc(const args : TExprBaseListExec);
begin
   DoPrint(args);
end;

// ------------------
// ------------------ TPrintLnFunction ------------------
// ------------------

// DoEvalProc
//
procedure TPrintLnFunction.DoEvalProc(const args : TExprBaseListExec);
var
   result : TdwsResult;
begin
   result:=DoPrint(args);
   result.AddCRLF;
end;

end.
