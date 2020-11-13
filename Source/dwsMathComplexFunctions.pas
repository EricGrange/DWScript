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
unit dwsMathComplexFunctions;

{$I dws.inc}

interface

uses
   SysUtils,
   dwsUtils, dwsXPlatform, dwsCompilerContext, dwsScriptSource,
   dwsFunctions, dwsSymbols, dwsExprs, dwsStrings, dwsOperators, dwsExprList,
   dwsTokenTypes, dwsMagicExprs, dwsUnitSymbols, dwsDataContext;

type
   TComplexMakeExpr = class(TInternalMagicDataFunction)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexToStrExpr = class(TInternalMagicStringFunction)
      public
         procedure DoEvalAsString(const args : TExprBaseListExec; var result : String); override;
   end;

   TComplexToRealExpr = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TComplexToImagExpr = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TComplexAbsExpr = class(TInternalMagicFloatFunction)
      procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TComplexOpExpr = class(TInternalMagicDataFunction);

   TComplexNegOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexConjugateOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexAddOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexSubOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexMultOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexDivOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TComplexExpOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

const
   SYS_COMPLEX = 'TComplex';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses Math;

// RegisterComplexType
//
procedure RegisterComplexType(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                              unitTable : TSymbolTable);
var
   typComplex : TRecordSymbol;
begin
   if systemTable.FindLocal(SYS_COMPLEX)<>nil then exit;

   typComplex:=TRecordSymbol.Create(SYS_COMPLEX, nil);
   typComplex.AddField(TFieldSymbol.Create('Re', systemTable.TypFloat, cvPublic));
   typComplex.AddField(TFieldSymbol.Create('Im', systemTable.TypFloat, cvPublic));

   systemTable.AddSymbol(typComplex);
end;

// RegisterComplexOperators
//
procedure RegisterComplexOperators(systemTable : TSystemSymbolTable;
                                   unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
begin
   typComplex:=systemTable.FindTypeSymbol(SYS_COMPLEX, cvMagic) as TRecordSymbol;

   if operators.HasOperatorFor(ttPROPERTY, typComplex, typComplex) then Exit;

   operators.RegisterOperator(ttPLUS, unitTable.FindSymbol('ComplexAdd', cvMagic) as TFuncSymbol, typComplex, typComplex);
   operators.RegisterOperator(ttMINUS, unitTable.FindSymbol('ComplexSub', cvMagic) as TFuncSymbol, typComplex, typComplex);
   operators.RegisterOperator(ttTIMES, unitTable.FindSymbol('ComplexMult', cvMagic) as TFuncSymbol, typComplex, typComplex);
   operators.RegisterOperator(ttDIVIDE, unitTable.FindSymbol('ComplexDiv', cvMagic) as TFuncSymbol, typComplex, typComplex);
end;

// ------------------
// ------------------ TComplexMakeExpr ------------------
// ------------------

// DoEval
//
procedure TComplexMakeExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
begin
   result.AsFloat[0]:=args.AsFloat[0];
   result.AsFloat[1]:=args.AsFloat[1];
end;

// ------------------
// ------------------ TComplexToStrExpr ------------------
// ------------------

// DoEvalAsString
//
procedure TComplexToStrExpr.DoEvalAsString(const args : TExprBaseListExec; var result : String);
var
   cmplxData : IDataContext;
   r, i : Double;
begin
   cmplxData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   r:=cmplxData.AsFloat[0];
   i:=cmplxData.AsFloat[1];
   if i>0 then
      Result := Format('%f + %fi', [r, i])
   else if i<0 then
      Result := Format('%f - %fi', [r, Abs(i)])
   else Result := Format('%f', [r]);
end;

// ------------------
// ------------------ TComplexToRealExpr ------------------
// ------------------

// DoEvalAsFloat
//
procedure TComplexToRealExpr.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := TDataExpr(args.ExprBase[0]).DataPtr[args.Exec].AsFloat[0];
end;

// ------------------
// ------------------ TComplexToImagExpr ------------------
// ------------------

// DoEvalAsFloat
//
procedure TComplexToImagExpr.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
begin
   Result := TDataExpr(args.ExprBase[0]).DataPtr[args.Exec].AsFloat[1];
end;

// ------------------
// ------------------ TComplexAbsExpr ------------------
// ------------------

// DoEvalAsFloat
//
procedure TComplexAbsExpr.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   v : IDataContext;
begin
   v := TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];

   Result := Sqrt(Sqr(v.AsFloat[0])+Sqr(v.AsFloat[1]));
end;

// ------------------
// ------------------ TComplexNegOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexNegOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   v : IDataContext;
begin
   v:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];

   result.AsFloat[0]:=-v.AsFloat[0];
   result.AsFloat[1]:=-v.AsFloat[1];
end;

// ------------------
// ------------------ TComplexConjugateOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexConjugateOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   v : IDataContext;
begin
   v := TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];

   result.AsFloat[0] := v.AsFloat[0];
   result.AsFloat[1] := -v.AsFloat[1];
end;

// ------------------
// ------------------ TComplexAddOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexAddOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : IDataContext;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]+rightData[0];
   result[1]:=leftData[1]+rightData[1];
end;

// ------------------
// ------------------ TComplexSubOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexSubOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : IDataContext;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]-rightData[0];
   result[1]:=leftData[1]-rightData[1];
end;

// ------------------
// ------------------ TComplexMultOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexMultOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : IDataContext;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]*rightData[0]-leftData[1]*rightData[1];
   result[1]:=leftData[1]*rightData[0]+leftData[0]*rightData[1];
end;

// ------------------
// ------------------ TComplexDivOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexDivOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : IDataContext;
   d : Double;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   d:=Sqr(rightData[0])+Sqr(rightData[1]);
   result[0]:=(leftData[0]*rightData[0]+leftData[1]*rightData[1])/d;
   result[1]:=(leftData[1]*rightData[0]-leftData[0]*rightData[1])/d;
end;

// ------------------
// ------------------ TComplexExpOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexExpOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   v : IDataContext;
   theta, c, s : Double;
   buf : array [0..1] of Double;
begin
   v := TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];

   buf[0] := Exp(v.AsFloat[0]);
   buf[1] := v.AsFloat[1];
   theta := buf[0];
   SinCos(buf[1], c, s);

   result.AsFloat[0] := theta * c;
   result.AsFloat[1] := theta * s;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterComplexType);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterComplexOperators);

   RegisterInternalFloatFunction(TComplexAbsExpr,  'Abs',  ['v', SYS_COMPLEX], [iffOverloaded, iffStateLess]);

   RegisterInternalFunction(TComplexMakeExpr, 'Complex', ['real', SYS_FLOAT, 'imaginary', SYS_FLOAT], SYS_COMPLEX, [iffStateLess]);
   RegisterInternalStringFunction(TComplexToStrExpr, 'ComplexToStr', ['&c', SYS_COMPLEX], [iffStateLess], 'ToString');

   RegisterInternalFloatFunction(TComplexToRealExpr, 'ComplexReal', ['&c', SYS_COMPLEX], [iffStateLess], 'Real');
   RegisterInternalFloatFunction(TComplexToImagExpr, 'ComplexImag', ['&c', SYS_COMPLEX], [iffStateLess], 'Imag');

   RegisterInternalFunction(TComplexNegOpExpr,  'ComplexNeg',  ['&v', SYS_COMPLEX], SYS_COMPLEX, [ iffStateLess ], 'Negate');
   RegisterInternalFunction(TComplexConjugateOpExpr,  'ComplexConjugate',  ['&v', SYS_COMPLEX], SYS_COMPLEX, [iffStateLess], 'Conjugate');

   RegisterInternalFunction(TComplexAddOpExpr,  'ComplexAdd',  ['&left', SYS_COMPLEX, '&right', SYS_COMPLEX], SYS_COMPLEX, [iffStateLess], 'Add');
   RegisterInternalFunction(TComplexSubOpExpr,  'ComplexSub',  ['&left', SYS_COMPLEX, '&right', SYS_COMPLEX], SYS_COMPLEX, [iffStateLess], 'Sub');
   RegisterInternalFunction(TComplexMultOpExpr, 'ComplexMult', ['&left', SYS_COMPLEX, '&right', SYS_COMPLEX], SYS_COMPLEX, [iffStateLess], 'Mult');
   RegisterInternalFunction(TComplexDivOpExpr,  'ComplexDiv',  ['&left', SYS_COMPLEX, '&right', SYS_COMPLEX], SYS_COMPLEX, [iffStateLess], 'Div');

   RegisterInternalFunction(TComplexExpOpExpr,  'ComplexExp',  ['&v', SYS_COMPLEX], SYS_COMPLEX, [iffStateLess], 'Exp');

end.
