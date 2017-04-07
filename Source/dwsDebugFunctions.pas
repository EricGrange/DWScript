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
unit dwsDebugFunctions;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsFunctions, dwsExprs, dwsSymbols, dwsUtils, dwsExprList, dwsStrings, dwsScriptSource,
   dwsMagicExprs, dwsUnitSymbols, dwsXPlatform, dwsErrors, dwsDataContext, dwsInfo;

type
   TOutputDebugStringFunc = class(TInternalMagicProcedure)
      procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TCurrentSourceCodeLocation = class(TInternalMagicDataFunction)
      procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;
   TCallerSourceCodeLocation = class(TInternalMagicDataFunction)
      procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

const
   SYS_TSOURCECODELOCATION = 'TSourceCodeLocation';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// RegisterComplexType
//
procedure RegisterDebugTypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                             unitTable : TSymbolTable);
var
   typLocation : TRecordSymbol;
begin
   if systemTable.FindLocal(SYS_TSOURCECODELOCATION)<>nil then exit;

   typLocation:=TRecordSymbol.Create(SYS_TSOURCECODELOCATION, nil);
   typLocation.AddField(TFieldSymbol.Create('Name', systemTable.TypString, cvPublic));
   typLocation.AddField(TFieldSymbol.Create('File', systemTable.TypString, cvPublic));
   typLocation.AddField(TFieldSymbol.Create('Line', systemTable.TypInteger, cvPublic));

   systemTable.AddSymbol(typLocation);
end;

// DoEvalProc
//
procedure TOutputDebugStringFunc.DoEvalProc(const args : TExprBaseListExec);
var
   exec : TdwsExecution;
begin
   exec:=args.Exec;
   if exec.Debugger<>nil then
      exec.Debugger.DebugMessage(args.AsString[0])
   else OutputDebugString(args.AsString[0]);
end;

// ------------------
// ------------------ TCurrentSourceCodeLocation ------------------
// ------------------

// DoEval
//
procedure TCurrentSourceCodeLocation.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   expr : TExprBase;
   sp : TScriptPos;
   prog : TObject;
begin
   expr := args.Expr as TExprBase;
   if expr<>nil then begin
      sp := expr.ScriptPos;
      prog := (args.Exec as TdwsProgramExecution).CurrentProg;
      if prog.ClassType = TdwsProcedure then
         result.AsString[0] := TdwsProcedure(prog).Func.QualifiedName
      else result.AsString[0] := '';
      result.AsString[1] := sp.SourceName;
      result.AsInteger[2] := sp.Line;
   end else begin
      result.AsString[0] := '';
      result.AsString[1] := '';
      result.AsInteger[2] := 0;
   end;
end;

// ------------------
// ------------------ TCallerSourceCodeLocation ------------------
// ------------------

// DoEval
//
procedure TCallerSourceCodeLocation.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   expr : TExprBase;
   sp : TScriptPos;
   prog : TObject;
begin
   expr := args.Exec.CallStackLastExpr;
   if expr<>nil then begin
      sp := expr.ScriptPos;
      prog := args.Exec.CallStackLastProg;
      if prog.ClassType = TdwsProcedure then
         result.AsString[0] := TdwsProcedure(prog).Func.QualifiedName
      else result.AsString[0] := '';
      result.AsString[1] := sp.SourceName;
      result.AsInteger[2] := sp.Line;
   end else begin
      result.AsString[0] := '';
      result.AsString[1] := '';
      result.AsInteger[2] := 0;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterDebugTypes);

   RegisterInternalProcedure(TOutputDebugStringFunc, 'OutputDebugString', ['m', SYS_STRING]);

   RegisterInternalFunction(TCurrentSourceCodeLocation, 'CurrentSourceCodeLocation', [], SYS_TSOURCECODELOCATION, []);
   RegisterInternalFunction(TCallerSourceCodeLocation, 'CallerSourceCodeLocation', [], SYS_TSOURCECODELOCATION, []);

end.

