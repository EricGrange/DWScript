{**********************************************************************}
{                                                                      }
{    The contents of this file are subject to the GNU General          }
{    Public License version 3 (the "License") as published by          }
{    the Free Software Foundation. You may obtain a copy of            }
{    the License at https://www.gnu.org/licenses/gpl-3.0.txt           }
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
unit dwsJSBigInteger;

{$I dws.inc}

interface

uses
   SysUtils, dwsBigIntegerFunctions.GMP,
   dwsUtils, dwsDataContext, dwsJSON, dwsSymbols, dwsExprs, dwsConstExprs,
   dwsCodeGen, dwsCodeGenWriters, dwsJSCodeGen;

type

   TJSBaseBigIntegerSymbol = class (TdwsCodeGenSymbolWriter)
      constructor Create; reintroduce;
      procedure WriteDefaultValue(sym : TSymbol); override;
      procedure WriteValue(sym : TSymbol; const dataPtr : IDataContext); override;
      class procedure WriteBigInteger(stream : TWriteOnlyBlockStream; const bi : IdwsBigInteger); static;
   end;

   TJSBigIntegerBinOpExpr = class (TJSBinOpExpr)
      protected
         procedure WriteLeftOperand(codeGen : TdwsCodeGen; leftExpr : TTypedExpr); override;
         procedure WriteRightOperand(codeGen : TdwsCodeGen; rightExpr : TTypedExpr); override;
   end;

   TJSBigIntegerAddOpExpr = class (TJSBigIntegerBinOpExpr)
      protected
         function WriteOperator(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean; override;
      public
         constructor Create;
   end;

   TJSBigIntegerSubOpExpr = class (TJSBigIntegerBinOpExpr)
      protected
         function WriteOperator(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean; override;
      public
         constructor Create;
   end;

   TJSBigIntegerCompoundExpr = class (TJSCompoundExpr)
      procedure CodeGenRightExpr(codeGen : TdwsCodeGen; rightExpr : TTypedExpr); override;
   end;

   TJSConvFloatToBigInteger = class (TJSExprCodeGen)
      procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

implementation

// ------------------
// ------------------ TJSBaseBigIntegerSymbol ------------------
// ------------------

// Create
//
constructor TJSBaseBigIntegerSymbol.Create;
begin
   inherited Create(TBaseBigIntegerSymbol);
end;

// WriteDefaultValue
//
procedure TJSBaseBigIntegerSymbol.WriteDefaultValue(sym : TSymbol);
begin
   CodeGen.Output.WriteString('0n');
end;

// WriteValue
//
procedure TJSBaseBigIntegerSymbol.WriteValue(sym : TSymbol; const dataPtr : IDataContext);
var
   intf : IInterface;
begin
   dataPtr.EvalAsInterface(0, intf);
   TJSBaseBigIntegerSymbol.WriteBigInteger(CodeGen.Output, intf as IdwsBigInteger);
end;

// WriteBigInteger
//
class procedure TJSBaseBigIntegerSymbol.WriteBigInteger(stream : TWriteOnlyBlockStream; const bi : IdwsBigInteger);
begin
   if bi = nil then
      stream.WriteString('0n')
   else stream.WriteString(bi.ToStringBase(10)+'n');
end;

// ------------------
// ------------------ TJSBigIntegerBinOpExpr ------------------
// ------------------

// WriteLeftOperand
//
procedure TJSBigIntegerBinOpExpr.WriteLeftOperand(codeGen : TdwsCodeGen; leftExpr : TTypedExpr);
begin
   if leftExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then
      inherited WriteLeftOperand(codeGen, leftExpr)
   else begin
      codeGen.WriteString('BigInt(');
      codeGen.CompileNoWrap(leftExpr);
      codeGen.WriteString(')');
   end;
end;

// WriteRightOperand
//
procedure TJSBigIntegerBinOpExpr.WriteRightOperand(codeGen : TdwsCodeGen; rightExpr : TTypedExpr);
begin
   if rightExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then
      inherited WriteRightOperand(codeGen, rightExpr)
   else begin
      codeGen.WriteString('BigInt(');
      codeGen.CompileNoWrap(rightExpr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSBigIntegerAddOpExpr ------------------
// ------------------

// Create
//
constructor TJSBigIntegerAddOpExpr.Create;
begin
   inherited Create('+', 13, [associativeLeft, associativeRight]);
end;

// WriteOperator
//
function TJSBigIntegerAddOpExpr.WriteOperator(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean;
var
   bi : IInterface;
begin
   Result := False;
   if (rightExpr is TConstExpr) and (rightExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol)) then begin
      rightExpr.EvalAsInterface(nil, bi);
      // right operand will write a minus
      if (bi as IdwsBigInteger).Sign < 0 then Exit;
   end;
   codeGen.WriteString(FOp);
end;

// ------------------
// ------------------ TJSBigIntegerSubOpExpr ------------------
// ------------------

// Create
//
constructor TJSBigIntegerSubOpExpr.Create;
begin
   inherited Create('-', 13, [associativeLeft]);
end;

// WriteOperator
//
function TJSBigIntegerSubOpExpr.WriteOperator(codeGen : TdwsCodeGen; rightExpr : TTypedExpr) : Boolean;
var
   bi : IInterface;
begin
   if (rightExpr is TConstExpr) and (rightExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol)) then begin
      // right operand could write a minus
      rightExpr.EvalAsInterface(nil, bi);
      if (bi as IdwsBigInteger).Sign < 0 then begin
         codeGen.WriteString('+');
         TJSBaseBigIntegerSymbol.WriteBigInteger(codeGen.Output, (bi as IdwsBigInteger).ToNeg);
         Exit(True);
      end;
   end;
   codeGen.WriteString(FOp);
   Result := False;
end;

// ------------------
// ------------------ TJSBigIntegerCompoundExpr ------------------
// ------------------

// CodeGenRightExpr
//
procedure TJSBigIntegerCompoundExpr.CodeGenRightExpr(codeGen : TdwsCodeGen; rightExpr : TTypedExpr);
begin
   if rightExpr.Typ.UnAliasedTypeIs(TBaseBigIntegerSymbol) then
      codeGen.CompileNoWrap(rightExpr)
   else begin
      codeGen.WriteString('BigInt(');
      codeGen.CompileNoWrap(rightExpr);
      codeGen.WriteString(')');
   end;
end;

// ------------------
// ------------------ TJSConvFloatToBigInteger ------------------
// ------------------

// CodeGen
//
procedure TJSConvFloatToBigInteger.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   bi : IInterface;
begin
   if expr.IsConstant then begin
      expr.EvalAsInterface(nil, bi);
      TJSBaseBigIntegerSymbol.WriteBigInteger(codeGen.Output, bi as IdwsBigInteger);
   end else begin
      codeGen.WriteString('BigInt(Math.trunc(');
      codeGen.CompileNoWrap(expr as TTypedExpr);
      codeGen.WriteString('))');
   end;
end;

end.
