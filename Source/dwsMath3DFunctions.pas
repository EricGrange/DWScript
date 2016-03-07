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
unit dwsMath3DFunctions;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsStrings, dwsUtils, dwsXPlatform,
   dwsFunctions, dwsExprs, dwsSymbols, dwsOperators, dwsDataContext,
   dwsTokenizer, dwsMagicExprs, dwsUnitSymbols, dwsExprList;

type
   TVectorMakeExpr = class(TInternalMagicDataFunction)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TVectorToStrExpr = class(TInternalMagicStringFunction)
      public
         procedure DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString); override;
   end;

   TVectorOpExpr = class(TInternalMagicDataFunction);

   TVectorAddOpExpr = class(TVectorOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TVectorSubOpExpr = class(TVectorOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TVectorScaleLeftOpExpr = class(TVectorOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TVectorScaleRightOpExpr = class(TVectorOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TVectorCrossProductOpExpr = class(TVectorOpExpr)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   TVectorDotProductOpExpr = class(TInternalMagicFloatFunction)
      public
         procedure DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double); override;
   end;

   TVectorNormalizeExpr = class(TInternalMagicDataFunction)
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

const
   SYS_VECTOR = 'TVector';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const cZero : Double = 0;

// RegisterMath3DTypes
//
procedure RegisterMath3DTypes(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                              unitTable : TSymbolTable);
var
   typVector : TRecordSymbol;
//   methSym : TMagicMethodSymbol;
begin
   if systemTable.FindLocal(SYS_VECTOR)<>nil then Exit;

   typVector:=TRecordSymbol.Create(SYS_VECTOR, nil);
   typVector.AddField(TFieldSymbol.Create('X', systemTable.TypFloat, cvPublic));
   typVector.AddField(TFieldSymbol.Create('Y', systemTable.TypFloat, cvPublic));
   typVector.AddField(TFieldSymbol.Create('Z', systemTable.TypFloat, cvPublic));
   typVector.AddField(TFieldSymbol.Create('W', systemTable.TypFloat, cvPublic));
   typVector.IsFullyDefined:=True;

(*   methSym:=TMagicMethodSymbol.Create('AsString', fkFunction, typVector, cvPublic, False);
   typVector.AddMethod(methSym);
   methSym.Typ:= *)

   systemTable.AddSymbol(typVector);
end;

// RegisterMath3DOperators
//
procedure RegisterMath3DOperators(systemTable : TSystemSymbolTable;
                                  unitTable : TSymbolTable; operators : TOperators);
var
   typVector : TRecordSymbol;
   sym : TSymbol;
begin
   typVector:=systemTable.FindTypeSymbol(SYS_VECTOR, cvMagic) as TRecordSymbol;

   if operators.HasOperatorFor(ttPLUS, typVector, typVector) then Exit;

   operators.RegisterOperator(ttPLUS, unitTable.FindSymbol('VectorAdd', cvMagic) as TFuncSymbol, typVector, typVector);
   operators.RegisterOperator(ttMINUS, unitTable.FindSymbol('VectorSub', cvMagic) as TFuncSymbol, typVector, typVector);

   for sym in (unitTable as TLinkedSymbolTable).ParentSymbolTable do begin
      if sym.Name<>'VectorScale' then continue;
      if (sym as TFuncSymbol).Params[0].Typ.IsOfType(systemTable.TypFloat) then
         operators.RegisterOperator(ttTIMES, sym as TFuncSymbol, systemTable.TypFloat, typVector)
      else operators.RegisterOperator(ttTIMES, sym as TFuncSymbol, typVector, systemTable.TypFloat);
   end;

   operators.RegisterOperator(ttCARET, unitTable.FindSymbol('VectorCrossProduct', cvMagic) as TFuncSymbol, typVector, typVector);
   operators.RegisterOperator(ttTIMES, unitTable.FindSymbol('VectorDotProduct', cvMagic) as TFuncSymbol, typVector, typVector);
end;

// ------------------
// ------------------ TVectorMakeExpr ------------------
// ------------------

// DoEval
//
procedure TVectorMakeExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
begin
   result.AsFloat[0] := args.AsFloat[0];
   result.AsFloat[1] := args.AsFloat[1];
   result.AsFloat[2] := args.AsFloat[2];
   result.AsFloat[3] := args.AsFloat[3];
end;

// ------------------
// ------------------ TVectorToStrExpr ------------------
// ------------------

// DoEvalAsString
//
procedure TVectorToStrExpr.DoEvalAsString(const args : TExprBaseListExec; var Result : UnicodeString);
var
   vectorData : IDataContext;
begin
   vectorData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   Result:=UnicodeFormat('[%f %f %f %f]',
                         [Double(vectorData[0]), Double(vectorData[1]),
                          Double(vectorData[2]), Double(vectorData[3])]);
end;

// ------------------
// ------------------ TVectorAddOpExpr ------------------
// ------------------

// DoEval
//
procedure TVectorAddOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : PVarDataArray;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec].AsPVarDataArray;
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec].AsPVarDataArray;

   result.AsFloat[0] := leftData[0].VDouble+rightData[0].VDouble;
   result.AsFloat[1] := leftData[1].VDouble+rightData[1].VDouble;
   result.AsFloat[2] := leftData[2].VDouble+rightData[2].VDouble;
   result.AsFloat[3] := leftData[3].VDouble+rightData[3].VDouble;
end;

// ------------------
// ------------------ TVectorScaleLeftOpExpr ------------------
// ------------------

// DoEval
//
procedure TVectorScaleLeftOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   f : Double;
   vec : PVarDataArray;
begin
   f := args.AsFloat[0];
   vec := TDataExpr(args.ExprBase[1]).DataPtr[args.Exec].AsPVarDataArray;

   result.AsFloat[0] := vec[0].VDouble*f;
   result.AsFloat[1] := vec[1].VDouble*f;
   result.AsFloat[2] := vec[2].VDouble*f;
   result.AsFloat[3] := vec[3].VDouble*f;
end;

// ------------------
// ------------------ TVectorScaleRightOpExpr ------------------
// ------------------

// DoEval
//
procedure TVectorScaleRightOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   f : Double;
   vec : PVarDataArray;
begin
   f := args.AsFloat[1];
   vec := TDataExpr(args.ExprBase[0]).DataPtr[args.Exec].AsPVarDataArray;

   result.AsFloat[0] := vec[0].VDouble*f;
   result.AsFloat[1] := vec[1].VDouble*f;
   result.AsFloat[2] := vec[2].VDouble*f;
   result.AsFloat[3] := vec[3].VDouble*f;
end;

// ------------------
// ------------------ TVectorSubOpExpr ------------------
// ------------------

// DoEval
//
procedure TVectorSubOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : IDataContext;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[0]-rightData[0];
   result[1]:=leftData[1]-rightData[1];
   result[2]:=leftData[2]-rightData[2];
   result[3]:=leftData[3]-rightData[3];
end;

// ------------------
// ------------------ TVectorCrossProductOpExpr ------------------
// ------------------

// DoEval
//
procedure TVectorCrossProductOpExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   leftData, rightData : IDataContext;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   result[0]:=leftData[1]*rightData[2]-leftData[2]*rightData[1];
   result[1]:=leftData[0]*rightData[2]-leftData[2]*rightData[0];
   result[2]:=leftData[0]*rightData[1]-leftData[1]*rightData[0];
   result[3]:=cZero;
end;

// ------------------
// ------------------ TVectorDotProductOpExpr ------------------
// ------------------

// DoEvalAsFloat
//
procedure TVectorDotProductOpExpr.DoEvalAsFloat(const args : TExprBaseListExec; var Result : Double);
var
   leftData, rightData : IDataContext;
begin
   leftData:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];
   rightData:=TDataExpr(args.ExprBase[1]).DataPtr[args.Exec];

   Result:= leftData[0]*rightData[0]
           +leftData[1]*rightData[1]
           +leftData[2]*rightData[2];
end;

// ------------------
// ------------------ TVectorNormalizeExpr ------------------
// ------------------

// DoEval
//
procedure TVectorNormalizeExpr.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   n, invN : Double;
   v : IDataContext;
begin
   v:=TDataExpr(args.ExprBase[0]).DataPtr[args.Exec];

   n:=Sqr(v[0])+Sqr(v[1])+Sqr(v[2]);
   if n>0 then
      invN:=1/Sqrt(n)
   else invN:=cZero;
   Result[0] := v[0]*invN;
   Result[1] := v[1]*invN;
   Result[2] := v[2]*invN;
   Result[3] := cZero;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   dwsInternalUnit.AddSymbolsRegistrationProc(RegisterMath3DTypes);
   dwsInternalUnit.AddOperatorsRegistrationProc(RegisterMath3DOperators);

   RegisterInternalFunction(TVectorMakeExpr, 'Vector', ['x', SYS_FLOAT, 'y', SYS_FLOAT, 'z', SYS_FLOAT, 'w=0', SYS_FLOAT], SYS_VECTOR, [iffStateLess]);
   RegisterInternalStringFunction(TVectorToStrExpr, 'VectorToStr', ['c', SYS_VECTOR], [iffStateLess]);

   RegisterInternalFunction(TVectorAddOpExpr,  'VectorAdd',  ['left', SYS_VECTOR, 'right', SYS_VECTOR], SYS_VECTOR, [iffStateLess]);
   RegisterInternalFunction(TVectorSubOpExpr,  'VectorSub',  ['left', SYS_VECTOR, 'right', SYS_VECTOR], SYS_VECTOR, [iffStateLess]);
   RegisterInternalFunction(TVectorScaleLeftOpExpr,  'VectorScale',  ['left', SYS_FLOAT, 'right', SYS_VECTOR], SYS_VECTOR, [iffStateLess, iffOverloaded]);
   RegisterInternalFunction(TVectorScaleRightOpExpr,  'VectorScale',  ['left', SYS_VECTOR, 'right', SYS_FLOAT], SYS_VECTOR, [iffStateLess, iffOverloaded]);
   RegisterInternalFunction(TVectorCrossProductOpExpr,  'VectorCrossProduct',  ['left', SYS_VECTOR, 'right', SYS_VECTOR], SYS_VECTOR, [iffStateLess]);
   RegisterInternalFloatFunction(TVectorDotProductOpExpr,  'VectorDotProduct',  ['left', SYS_VECTOR, 'right', SYS_VECTOR], [iffStateLess]);
   RegisterInternalFunction(TVectorNormalizeExpr,  'VectorNormalize',  ['v', SYS_VECTOR], SYS_VECTOR, [iffStateLess]);

end.
