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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsJSCodeGen;

interface

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsCodeGen, dwsCoreExprs,
   dwsExprs, dwsRelExprs;

type

   TdwsJSCodeGen = class (TdwsCodeGen)
      private

      protected

      public
         constructor Create; override;
   end;

   TJSBlockInitExpr = class (TdwsExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSBlockExprNoTable = class (TdwsExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSAssignConstToIntegerVarExpr = class (TdwsExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSConstIntExpr = class (TdwsExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

   TJSVarExpr = class (TdwsExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsJSCodeGen ------------------
// ------------------

// Create
//
constructor TdwsJSCodeGen.Create;
begin
   inherited;

   RegisterCodeGen(TBlockInitExpr, TJSBlockInitExpr.Create);

   RegisterCodeGen(TBlockExprNoTable,  TJSBlockExprNoTable.Create);
   RegisterCodeGen(TBlockExprNoTable2, TJSBlockExprNoTable.Create);
   RegisterCodeGen(TBlockExprNoTable3, TJSBlockExprNoTable.Create);
   RegisterCodeGen(TBlockExprNoTable4, TJSBlockExprNoTable.Create);

   RegisterCodeGen(TConstIntExpr,      TJSConstIntExpr.Create);

   RegisterCodeGen(TAssignConstToIntegerVarExpr, TJSAssignConstToIntegerVarExpr.Create);

   RegisterCodeGen(TIntVarExpr,        TJSVarExpr.Create);

   RegisterCodeGen(TAddIntExpr,
      TdwsExprGenericCodeGen.Create(['((', 0, ')+(', 1, '))']));

   RegisterCodeGen(TRelEqualIntExpr,
      TdwsExprGenericCodeGen.Create(['((', 0, ')==(', 1, '))']));

   RegisterCodeGen(TIfThenExpr,
      TdwsExprGenericCodeGen.Create(['if (', 0, ') {', 1, '};']));
   RegisterCodeGen(TIfThenElseExpr,
      TdwsExprGenericCodeGen.Create(['if (', 0, ') {', 1, '} else {', 2, '};']));
end;

// ------------------
// ------------------ TJSBlockInitExpr ------------------
// ------------------

// CodeGen
//
procedure TJSBlockInitExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   blockInit : TBlockInitExpr;
begin
   blockInit:=TBlockInitExpr(expr);
   for i:=0 to blockInit.SubExprCount-1 do begin
      codeGen.Output.WriteString('var ');
      codeGen.Compile(blockInit.SubExpr[i]);
      codeGen.Output.WriteString(#13#10);
   end;
end;

// ------------------
// ------------------ TJSBlockExprNoTable ------------------
// ------------------

// CodeGen
//
procedure TJSBlockExprNoTable.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   blockInit : TBlockInitExpr;
begin
   blockInit:=TBlockInitExpr(expr);
   for i:=0 to blockInit.SubExprCount-1 do begin
      codeGen.Compile(blockInit.SubExpr[i]);
      codeGen.Output.WriteString(#13#10);
   end;
end;

// ------------------
// ------------------ TJSVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   varExpr : TVarExpr;
   sym : TDataSymbol;
begin
   varExpr:=TVarExpr(expr);
   sym:=codeGen.FindSymbolAtStackAddr(varExpr.StackAddr);
   codeGen.Output.WriteString(sym.Name);
end;

// ------------------
// ------------------ TJSAssignConstToIntegerVarExpr ------------------
// ------------------

// CodeGen
//
procedure TJSAssignConstToIntegerVarExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TAssignConstToIntegerVarExpr;
begin
   e:=TAssignConstToIntegerVarExpr(expr);
   codeGen.Compile(e.Left);
   codeGen.Output.WriteString('=');
   codeGen.Output.WriteString(IntToStr(e.Right));
   codeGen.Output.WriteString(';');
end;

// ------------------
// ------------------ TJSConstIntExpr ------------------
// ------------------

// CodeGen
//
procedure TJSConstIntExpr.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   e : TConstIntExpr;
begin
   e:=TConstIntExpr(expr);
   codeGen.Output.WriteString(IntToStr(e.Value));
end;

end.
