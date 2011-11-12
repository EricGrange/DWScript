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

uses dwsFunctions, dwsSymbols, dwsExprs, dwsStrings, dwsOperators, dwsStack,
   dwsTokenizer, SysUtils, dwsUtils;

type
   TComplexMakeExpr = class(TInternalMagicDataFunction)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexToStrExpr = class(TInternalMagicStringFunction)
      public
         procedure DoEvalAsString(args : TExprBaseList; var Result : UnicodeString); override;
   end;

   TComplexOpExpr = class(TInternalMagicDataFunction);

   TComplexNegOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexAddOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexSubOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexMultOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
   end;

   TComplexDivOpExpr = class(TComplexOpExpr)
      public
         procedure DoEval(args : TExprBaseList; var result : TDataPtr); override;
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

// RegisterComplexType
//
procedure RegisterComplexType(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                              unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
begin
   typComplex:=TRecordSymbol.Create(SYS_COMPLEX, nil);
   typComplex.AddField(TFieldSymbol.Create('Re', systemTable.TypFloat, cvPublic));
   typComplex.AddField(TFieldSymbol.Create('Im', systemTable.TypFloat, cvPublic));

   systemTable.AddSymbol(typComplex);
end;

// RegisterComplexOperators
//
procedure RegisterComplexOperators(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                   unitTable : TSymbolTable; operators : TOperators);
var
   typComplex : TRecordSymbol;
begin
   typComplex:=systemTable.FindTypeSymbol(SYS_COMPLEX, cvMagic) as TRecordSymbol;

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
procedure TComplexMakeExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
begin
   result[0]:=args.AsFloat[0];
   result[1]:=args.AsFloat[1];
end;

// ------------------
// ------------------ TComplexToStrExpr ------------------
// ------------------

// DoEvalAsString
//
procedure TComplexToStrExpr.DoEvalAsString(args : TExprBaseList; var Result : UnicodeString);
var
   cmplxData : TDataPtr;
   r, i : Double;
begin
   cmplxData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   r:=cmplxData[0];
   i:=cmplxData[1];
   if i>0 then
      Result:=Format('%f + %fi', [r, i])
   else if i<0 then
      Result:=Format('%f - %fi', [r, Abs(i)])
   else Result:=Format('%f', [r]);
end;

// ------------------
// ------------------ TComplexNegOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexNegOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   v : TDataPtr;
begin
   v:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];

   result[0]:=-v[0];
   result[1]:=-v[1];
end;

// ------------------
// ------------------ TComplexAddOpExpr ------------------
// ------------------

// DoEval
//
procedure TComplexAddOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
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
procedure TComplexSubOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
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
procedure TComplexMultOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
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
procedure TComplexDivOpExpr.DoEval(args : TExprBaseList; var result : TDataPtr);
var
   leftData, rightData : TDataPtr;
   d : Double;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   d:=Sqr(rightData[0])+Sqr(rightData[1]);
   result[0]:=(leftData[0]*rightData[0]+leftData[1]*rightData[1])/d;
   result[1]:=(leftData[1]*rightData[0]-leftData[0]*rightData[1])/d;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddPreInitProc(RegisterComplexType);
   dwsInternalUnit.AddPostInitProc(RegisterComplexOperators);

   RegisterInternalFunction(TComplexMakeExpr, 'Complex', ['real', SYS_FLOAT, 'imaginary', SYS_FLOAT], SYS_COMPLEX, True);
   RegisterInternalStringFunction(TComplexToStrExpr, 'ComplexToStr', ['c', SYS_COMPLEX], True);

   RegisterInternalFunction(TComplexNegOpExpr,  'ComplexNeg',  ['v', SYS_COMPLEX], SYS_COMPLEX, True);
   RegisterInternalFunction(TComplexAddOpExpr,  'ComplexAdd',  ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);
   RegisterInternalFunction(TComplexSubOpExpr,  'ComplexSub',  ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);
   RegisterInternalFunction(TComplexMultOpExpr, 'ComplexMult', ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);
   RegisterInternalFunction(TComplexDivOpExpr,  'ComplexDiv',  ['left', SYS_COMPLEX, 'right', SYS_COMPLEX], SYS_COMPLEX, True);

end.
