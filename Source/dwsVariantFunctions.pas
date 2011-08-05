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
unit dwsVariantFunctions;

interface

uses Classes, Variants, SysUtils, dwsFunctions, dwsExprs, dwsSymbols, dwsUtils;

type
  TVarClearFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TVarIsNullFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
  end;

  TVarIsEmptyFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
  end;

  TVarTypeFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TVarAsTypeFunc = class(TInternalFunction)
    procedure Execute(info : TProgramInfo); override;
  end;

  TVarToStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

implementation

const // type constants
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';
  cVariant = 'Variant';

{ TVarClearFunc }

procedure TVarClearFunc.Execute(info : TProgramInfo);
begin
  Info.ValueAsVariant['v'] := Unassigned;
end;

{ TVarIsNullFunc }

function TVarIsNullFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
var
   v : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v);
   Result:=VarIsNull(v);
end;

{ TVarIsEmptyFunc }

function TVarIsEmptyFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
var
   v : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v);
   Result:=VarIsEmpty(v);
end;

{ TVarTypeFunc }

function TVarTypeFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
var
   v : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v);
   Result:=VarType(v);
end;

{ TVarAsTypeFunc }

procedure TVarAsTypeFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := VarAsType(Info.ValueAsVariant['v'], Info.ValueAsInteger['VarType']);
end;

{ TVarToStrFunc }

// DoEvalAsString
//
procedure TVarToStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   v : Variant;
begin
   args.ExprBase[0].EvalAsVariant(args.Exec, v);
   Result:=VarToStr(v);
end;

{ InitVariants }

procedure InitVariants(SystemTable, UnitSyms, UnitTable : TSymbolTable);
type
   TVarTypeRec = packed record n : String; v : Word; end;
const
   cVarTypes : array [0..24] of TVarTypeRec = (
      (n:'Empty'; v:varEmpty),         (n:'Null'; v:varNull),
      (n:'Smallint'; v:varSmallint),   (n:'Integer'; v:varInteger),
      (n:'Single'; v:varSingle),       (n:'Double'; v:varDouble),
      (n:'Currency'; v:varCurrency),   (n:'Date'; v:varDate),
      (n:'OleStr'; v:varOleStr),       (n:'Dispatch'; v:varDispatch),
      (n:'Error'; v:varError),         (n:'Boolean'; v:varBoolean),
      (n:'Variant'; v:varVariant),     (n:'Unknown'; v:varUnknown),
      (n:'ShortInt'; v:varShortInt),   (n:'Byte'; v:varByte),
      (n:'Word'; v:varWord),           (n:'LongWord'; v:varLongWord),
      (n:'Int64'; v:varInt64),         (n:'StrArg'; v:varStrArg),
      (n:'String'; v:varUString),      (n:'Any'; v:varAny),
      (n:'TypeMask'; v:varTypeMask),   (n:'Array'; v:varArray),
      (n:'ByRef'; v:varByRef) );
var
   i : Integer;
   T, E : TTypeSymbol;
begin
   T := SystemTable.FindSymbol('Integer', cvMagic) as TTypeSymbol;
   E := TEnumerationSymbol.Create('TVarType', T);
   UnitTable.AddSymbol(E);
   for i:=Low(cVarTypes) to High(cVarTypes) do
      UnitTable.AddSymbol(TElementSymbol.Create('var'+cVarTypes[i].n, E, cVarTypes[i].v, True));
end;

initialization

   RegisterInternalPreInitProc(@InitVariants);

   RegisterInternalFunction(TVarClearFunc, 'VarClear', ['@v', cVariant], '');
   RegisterInternalBoolFunction(TVarIsNullFunc, 'VarIsNull', ['v', cVariant]);
   RegisterInternalBoolFunction(TVarIsEmptyFunc, 'VarIsEmpty', ['v', cVariant]);
   RegisterInternalFunction(TVarTypeFunc, 'VarType', ['v', cVariant], 'TVarType');
   RegisterInternalFunction(TVarAsTypeFunc, 'VarAsType', ['v', cVariant, 'VarType', 'TVarType'], cVariant);
   RegisterInternalStringFunction(TVarToStrFunc, 'VarToStr', ['v', cVariant]);

end.

