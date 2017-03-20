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
unit dwsGenericExprs;

interface

uses
   dwsTokenizer, dwsScriptSource,
   dwsExprs, dwsSymbols, dwsGenericSymbols, dwsCompilerContext, dwsStrings;

type

   // left "op" right
   TGenericBinaryOpExpr = class(TBinaryOpExpr)
      private
         FOp : TTokenType;
         FConstraint : TGenericConstraintBinaryOp;  // referred
         FScriptPos : TScriptPos;

      protected

      public
         constructor Create(const aScriptPos : TScriptPos; aGeneric : TGenericSymbol;
                            const anOp : TTokenType; aLeft, aRight : TTypedExpr); reintroduce;

         function SpecializeTypedExpr(const context : ISpecializationContext) : TTypedExpr; override;

         function ScriptPos : TScriptPos; override;

         property Op : TTokenType read FOp;
         property Constraint : TGenericConstraintBinaryOp read FConstraint;
   end;

implementation

uses dwsSpecializationContext, dwsCompilerUtils;

// ------------------
// ------------------ TGenericBinaryOpExpr ------------------
// ------------------

// Create
//
constructor TGenericBinaryOpExpr.Create(const aScriptPos : TScriptPos; aGeneric : TGenericSymbol;
                                        const anOp : TTokenType; aLeft, aRight : TTypedExpr);
begin
   inherited Create(nil, aScriptPos, aLeft, aRight);
   FOp := anOp;
   FScriptPos := aScriptPos;
   FConstraint := aGeneric.ConstraintForBinaryOp(anOp, aLeft.Typ, aRight.Typ);
   Typ := FConstraint.ResultType;
end;

// SpecializeTypedExpr
//
function TGenericBinaryOpExpr.SpecializeTypedExpr(const context : ISpecializationContext) : TTypedExpr;
var
   leftSpecialized, rightSpecialized : TTypedExpr;
   leftTyp, rightTyp : TTypeSymbol;
begin
   leftSpecialized := Left.SpecializeTypedExpr(context);
   rightSpecialized := Right.SpecializeTypedExpr(context);

   Result := CreateTypedOperatorExpr(
      CompilerContextFromSpecialization(context),
      FOp, FScriptPos,
      leftSpecialized, rightSpecialized
   );
   if Result = nil then begin
      if leftSpecialized <> nil then
         leftTyp := leftSpecialized.Typ
      else leftTyp := nil;
      if rightSpecialized <> nil then
         rightTyp := rightSpecialized.Typ
      else rightTyp := nil;
      context.AddCompilerErrorFmt(CPE_NoAvailableBinaryOpSpecialization,
                                  [cTokenStrings[FOp], leftTyp.Caption, rightTyp.Caption]);
      leftSpecialized.Free;
      rightSpecialized.Free;
   end;
end;

// ScriptPos
//
function TGenericBinaryOpExpr.ScriptPos : TScriptPos;
begin
   Result := FScriptPos;
end;

end.
