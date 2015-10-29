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
unit dwsSystemOperators;

{$I dws.inc}

interface

uses
   dwsSymbols, dwsTokenizer, dwsExprs, dwsCoreExprs, dwsRelExprs,
   dwsOperators, dwsUnitSymbols;

type

   TSystemOperators = class (TOperators)
      public
         constructor Create(systemTable : TSystemSymbolTable);
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSystemOperators ------------------
// ------------------

// Create
//
constructor TSystemOperators.Create(systemTable : TSystemSymbolTable);
var
   typInteger : TTypeSymbol;
   typFloat : TTypeSymbol;
   typBoolean : TTypeSymbol;
   typString : TTypeSymbol;
   typVariant : TTypeSymbol;
   typClassOf : TTypeSymbol;

   procedure RegisterRelOp(aToken : TTokenType; intExpr, floatExpr, strExpr, varExpr : TRelOpExprClass);
   begin
      RegisterOperator(aToken,   intExpr,    typInteger, typInteger);
      RegisterOperator(aToken,   floatExpr,  typFloat,   typFloat);
      RegisterOperator(aToken,   floatExpr,  typFloat,   typInteger);
      RegisterOperator(aToken,   floatExpr,  typInteger, typFloat);
      RegisterOperator(aToken,   strExpr,    typString,  typString);
      RegisterOperator(aToken,   strExpr,    typString,  typVariant);
      RegisterOperator(aToken,   strExpr,    typVariant, typString);
      RegisterOperator(aToken,   varExpr,    typVariant, typVariant);
      RegisterOperator(aToken,   varExpr,    typFloat,   typVariant);
      RegisterOperator(aToken,   varExpr,    typInteger, typVariant);
      RegisterOperator(aToken,   varExpr,    typVariant, typInteger);
      RegisterOperator(aToken,   varExpr,    typVariant, typFloat);
   end;

begin
   inherited Create;

   typInteger:=systemTable.TypInteger;
   typFloat:=systemTable.TypFloat;
   typBoolean:=systemTable.TypBoolean;
   typString:=systemTable.TypString;
   typVariant:=systemTable.TypVariant;
   typClassOf:=systemTable.TypClass;

   // computation operators

   RegisterOperator(ttPLUS,   TAddIntExpr,      typInteger,  typInteger);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typString,   typString);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typString,   typVariant);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typVariant,  typString);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typVariant,  typFloat);
   RegisterOperator(ttPLUS,   TAddVariantExpr,  typInteger,  typVariant);
   RegisterOperator(ttPLUS,   TAddVariantExpr,  typVariant,  typInteger);
   RegisterOperator(ttPLUS,   TAddVariantExpr,  typVariant,  typVariant);

   RegisterOperator(ttMINUS,  TSubIntExpr,      typInteger,  typInteger);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typVariant,  typFloat);
   RegisterOperator(ttMINUS,  TSubVariantExpr,  typInteger,  typVariant);
   RegisterOperator(ttMINUS,  TSubVariantExpr,  typVariant,  typInteger);
   RegisterOperator(ttMINUS,  TSubVariantExpr,  typVariant,  typVariant);

   RegisterOperator(ttTIMES,  TMultIntExpr,     typInteger,  typInteger);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typInteger,  typFloat);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typInteger);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typFloat);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typVariant);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typVariant,  typFloat);
   RegisterOperator(ttTIMES,  TMultVariantExpr, typInteger,  typVariant);
   RegisterOperator(ttTIMES,  TMultVariantExpr, typVariant,  typInteger);
   RegisterOperator(ttTIMES,  TMultVariantExpr, typVariant,  typVariant);

   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typVariant);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typVariant);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typVariant);

   RegisterOperator(ttDIV,    TDivExpr,         typInteger,  typInteger);
   RegisterOperator(ttDIV,    TDivExpr,         typInteger,  typVariant);
   RegisterOperator(ttDIV,    TDivExpr,         typVariant,  typInteger);
   RegisterOperator(ttDIV,    TDivExpr,         typVariant,  typVariant);

   RegisterOperator(ttMOD,    TModExpr,         typInteger,  typInteger);
   RegisterOperator(ttMOD,    TModExpr,         typInteger,  typVariant);
   RegisterOperator(ttMOD,    TModExpr,         typVariant,  typInteger);
   RegisterOperator(ttMOD,    TModExpr,         typVariant,  typVariant);

   RegisterOperator(ttMOD,    TModFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttMOD,    TModFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttMOD,    TModFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttMOD,    TModFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttMOD,    TModFloatExpr,    typVariant,  typFloat);

   RegisterOperator(ttOR,     TBoolOrExpr,      typBoolean,  typBoolean);
   RegisterOperator(ttOR,     TBoolOrExpr,      typBoolean,  typVariant);
   RegisterOperator(ttOR,     TBoolOrExpr,      typVariant,  typBoolean);
   RegisterOperator(ttOR,     TIntOrExpr,       typInteger,  typInteger);
   RegisterOperator(ttOR,     TIntOrExpr,       typInteger,  typVariant);
   RegisterOperator(ttOR,     TIntOrExpr,       typVariant,  typInteger);
   RegisterOperator(ttOR,     TVariantOrExpr,   typVariant,  typVariant);

   RegisterOperator(ttAND,    TBoolAndExpr,     typBoolean,  typBoolean);
   RegisterOperator(ttAND,    TBoolAndExpr,     typBoolean,  typVariant);
   RegisterOperator(ttAND,    TBoolAndExpr,     typVariant,  typBoolean);
   RegisterOperator(ttAND,    TIntAndExpr,      typInteger,  typInteger);
   RegisterOperator(ttAND,    TIntAndExpr,      typInteger,  typVariant);
   RegisterOperator(ttAND,    TIntAndExpr,      typVariant,  typInteger);
   RegisterOperator(ttAND,    TVariantAndExpr,  typVariant,  typVariant);

   RegisterOperator(ttXOR,    TBoolXorExpr,     typBoolean,  typBoolean);
   RegisterOperator(ttXOR,    TBoolXorExpr,     typBoolean,  typVariant);
   RegisterOperator(ttXOR,    TBoolXorExpr,     typVariant,  typBoolean);
   RegisterOperator(ttXOR,    TIntXorExpr,      typInteger,  typInteger);
   RegisterOperator(ttXOR,    TIntXorExpr,      typInteger,  typVariant);
   RegisterOperator(ttXOR,    TIntXorExpr,      typVariant,  typInteger);
   RegisterOperator(ttXOR,    TVariantXorExpr,  typVariant,  typVariant);

   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typBoolean,  typBoolean);
   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typVariant,  typBoolean);
   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typBoolean,  typVariant);
   RegisterOperator(ttIMPLIES,TBoolImpliesExpr, typVariant,  typVariant);

   RegisterOperator(ttSHL,    TShlExpr,         typInteger,  typInteger);
   RegisterOperator(ttSHL,    TShlExpr,         typInteger,  typVariant);
   RegisterOperator(ttSHL,    TShlExpr,         typVariant,  typInteger);
   RegisterOperator(ttSHL,    TShlExpr,         typVariant,  typVariant);

   RegisterOperator(ttSHR,    TShrExpr,         typInteger,  typInteger);
   RegisterOperator(ttSHR,    TShrExpr,         typInteger,  typVariant);
   RegisterOperator(ttSHR,    TShrExpr,         typVariant,  typInteger);
   RegisterOperator(ttSHR,    TShrExpr,         typVariant,  typVariant);

   RegisterOperator(ttSAR,    TSarExpr,         typInteger,  typInteger);
   RegisterOperator(ttSAR,    TSarExpr,         typInteger,  typVariant);
   RegisterOperator(ttSAR,    TSarExpr,         typVariant,  typInteger);
   RegisterOperator(ttSAR,    TSarExpr,         typVariant,  typVariant);

   // comparison operators

   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typBoolean, typBoolean);
   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typVariant, typBoolean);
   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typBoolean, typVariant);

   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typBoolean, typBoolean);
   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typBoolean, typVariant);
   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typVariant, typBoolean);

   RegisterRelOp(ttEQ,     TRelEqualIntExpr, TRelEqualFloatExpr, TRelEqualStringExpr, TRelEqualVariantExpr);
   RegisterRelOp(ttNOTEQ,  TRelNotEqualIntExpr, TRelNotEqualFloatExpr, TRelNotEqualStringExpr, TRelNotEqualVariantExpr);
   RegisterRelOp(ttLESS,   TRelLessIntExpr, TRelLessFloatExpr, TRelLessStringExpr, TRelLessVariantExpr);
   RegisterRelOp(ttLESSEQ, TRelLessEqualIntExpr, TRelLessEqualFloatExpr, TRelLessEqualStringExpr, TRelLessEqualVariantExpr);
   RegisterRelOp(ttGTR,    TRelGreaterIntExpr, TRelGreaterFloatExpr, TRelGreaterStringExpr, TRelGreaterVariantExpr);
   RegisterRelOp(ttGTREQ,  TRelGreaterEqualIntExpr, TRelGreaterEqualFloatExpr, TRelGreaterEqualStringExpr, TRelGreaterEqualVariantExpr);

   RegisterOperator(ttEQ,     TRelEqualMetaExpr,      typClassOf, typClassOf);
   RegisterOperator(ttNOTEQ,  TRelNotEqualMetaExpr,   typClassOf, typClassOf);

   RegisterOperator(ttIN,     TStringInStringExpr,    typString,  typString);

   // combined assignment operator

   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignIntExpr,     typInteger,    typInteger);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typInteger);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typInteger,    typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typVariant,    typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typString,     typString);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typString,     typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typVariant,    typString);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typVariant,    typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typInteger,    typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typVariant,    typInteger);

   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignIntExpr,    typInteger,    typInteger);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typInteger);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typInteger,    typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typVariant,    typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typInteger,    typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typVariant,    typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typVariant,    typInteger);

   RegisterOperator(ttTIMES_ASSIGN, TMultAssignIntExpr,     typInteger,     typInteger);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typInteger);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typInteger,     typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typVariant,     typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typInteger,     typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typVariant,     typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typVariant,     typInteger);

   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typInteger);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typInteger);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typInteger);
end;

end.
