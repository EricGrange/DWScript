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
   dwsTokenTypes, dwsScriptSource,
   dwsSymbols, dwsGenericSymbols, dwsCompilerContext, dwsStrings,
   dwsExprs, dwsCoreExprs;

type

   // left "op" right
   TGenericBinaryOpExpr = class(TBinaryOpExpr)
      private
         FConstraint : TGenericConstraintBinaryOp;  // referred

      protected

      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                            const anOp : TTokenType; aLeft, aRight : TTypedExpr); override;

         function SpecializeTypedExpr(const context : ISpecializationContext) : TTypedExpr; override;

         function IsGeneric : Boolean; override;

         property Constraint : TGenericConstraintBinaryOp read FConstraint;
   end;

   // left "assign op" right
   TGenericAssignExpr = class(TAssignExpr)
      private
         FToken : TTokenType;

      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos: TScriptPos;
                            token : TTokenType; left : TDataExpr; right : TTypedExpr); reintroduce;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure TypeCheckAssign(context : TdwsCompilerContext); override;

         function Token : TTokenType; override;
   end;

implementation

uses dwsSpecializationContext, dwsCompilerUtils;

// ------------------
// ------------------ TGenericBinaryOpExpr ------------------
// ------------------

// Create
//
constructor TGenericBinaryOpExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos;
                                        const anOp : TTokenType; aLeft, aRight : TTypedExpr);
var
   generic : TGenericSymbol;
begin
   inherited Create(context, aScriptPos, anOp, aLeft, aRight);
   generic := TGenericSymbol.GenericFor(aLeft.Typ, aRight.Typ);
   FConstraint := generic.ConstraintForBinaryOp(anOp, aLeft.Typ, aRight.Typ);
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

// IsGeneric
//
function TGenericBinaryOpExpr.IsGeneric : Boolean;
begin
   Result := True;
end;

// ------------------
// ------------------ TGenericAssignExpr ------------------
// ------------------

// Create
//
constructor TGenericAssignExpr.Create(
      context : TdwsCompilerContext; const aScriptPos: TScriptPos;
      token : TTokenType;
      left : TDataExpr; right : TTypedExpr);
begin
   FToken := token;
   inherited Create(context, aScriptPos, left, right);
end;

// EvalNoResult
//
procedure TGenericAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   Assert(False);
end;

// TypeCheckAssign
//
procedure TGenericAssignExpr.TypeCheckAssign(context : TdwsCompilerContext);
begin
   // TODO check vs constraints
end;

// Token
//
function TGenericAssignExpr.Token : TTokenType;
begin
   Result := FToken;
end;

end.
