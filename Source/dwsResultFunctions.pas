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

uses dwsExprs, dwsExprList, dwsMagicExprs, dwsSymbols;

type

   TPrintFunction = class(TInternalMagicProcedure)
      public
         function DoPrint(const args : TExprBaseListExec) : TdwsResult; inline;
         procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TPrintLnFunction = class(TPrintFunction)
      public
         procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TPrintFunction ------------------
// ------------------

// DoPrint
//
function TPrintFunction.DoPrint(const args : TExprBaseListExec) : TdwsResult;
var
   buf : UnicodeString;
{$IFDEF DELPHI_2010_MINUS}
   ExprBaseListRec: TExprBaseListRec;
begin
   ExprBaseListRec := args.List^;
   ExprBaseListRec.ExprBase[0].EvalAsString(args.Exec, buf);
{$ELSE}
begin
   args.List.ExprBase[0].EvalAsString(args.Exec, buf);
{$ENDIF}
   Result:=(args.Exec as TdwsProgramExecution).Result;
   Result.AddString(buf);
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
