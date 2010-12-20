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
unit dwsRelExprs;

interface

uses dwsExprs, dwsErrors, dwsStrings;

type

   TRelOpExpr = class(TBinaryOpExpr)
      constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); override;
      function Eval: Variant; override;
      procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;
   TRelOpExprClass = class of TRelOpExpr;

   TRelEqualExpr = class(TRelOpExpr) end;
   TRelNotEqualExpr = class(TRelOpExpr) end;
   TRelLessExpr = class(TRelOpExpr) end;
   TRelLessEqualExpr = class(TRelOpExpr) end;
   TRelGreaterExpr = class(TRelOpExpr) end;
   TRelGreaterEqualExpr = class(TRelOpExpr) end;

   // boolean rel ops

   TRelEqualBoolExpr = class(TRelEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelNotEqualBoolExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;

   // integer rel ops

   TRelEqualIntExpr = class(TRelEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelNotEqualIntExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessIntExpr = class(TRelLessExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessEqualIntExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterIntExpr = class(TRelGreaterExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterEqualIntExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;

   // float rel ops

   TRelEqualFloatExpr = class(TRelEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelNotEqualFloatExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessFloatExpr = class(TRelLessExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessEqualFloatExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterFloatExpr = class(TRelGreaterExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterEqualFloatExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;

   // string rel ops

   TRelEqualStringExpr = class(TRelEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelNotEqualStringExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessStringExpr = class(TRelLessExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessEqualStringExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterStringExpr = class(TRelGreaterExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterEqualStringExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;

   // variant rel ops

   TRelEqualVariantExpr = class(TRelEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelNotEqualVariantExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessVariantExpr = class(TRelLessExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelLessEqualVariantExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterVariantExpr = class(TRelGreaterExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelGreaterEqualVariantExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean: Boolean; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TRelOpExpr ------------------
// ------------------

// Create
//
constructor TRelOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited;
   FTyp:=Prog.TypBoolean;
end;

// Eval
//
function TRelOpExpr.Eval: Variant;
begin
   Result:=EvalAsBoolean;
end;

// TypeCheckNoPos
//
procedure TRelOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if not (FLeft.Typ.IsCompatible(FRight.Typ)) then
      Prog.Msgs.AddCompilerStop(aPOs, CPE_InvalidOperands);
end;

// ------------------
// ------------------ TRelEqualBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualBoolExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsBoolean=FRight.EvalAsBoolean);
end;

// ------------------
// ------------------ TRelNotEqualBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualBoolExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsBoolean<>FRight.EvalAsBoolean);
end;

// ------------------
// ------------------ TRelEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualIntExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsInteger=FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TRelNotEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualIntExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsInteger<>FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TRelLessIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessIntExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsInteger<FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TRelLessEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualIntExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsInteger<=FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TRelGreaterIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterIntExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsInteger>FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TRelGreaterEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualIntExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.EvalAsInteger>=FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TRelEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualFloatExpr.EvalAsBoolean: Boolean;
var
   a, b : Double;
begin
   FLeft.EvalAsFloat(a);
   FRight.EvalAsFloat(b);
   Result:=(a=b);
end;

// ------------------
// ------------------ TRelNotEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualFloatExpr.EvalAsBoolean: Boolean;
var
   a, b : Double;
begin
   FLeft.EvalAsFloat(a);
   FRight.EvalAsFloat(b);
   Result:=(a<>b);
end;

// ------------------
// ------------------ TRelLessFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessFloatExpr.EvalAsBoolean: Boolean;
var
   a, b : Double;
begin
   FLeft.EvalAsFloat(a);
   FRight.EvalAsFloat(b);
   Result:=(a<b);
end;

// ------------------
// ------------------ TRelLessEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualFloatExpr.EvalAsBoolean: Boolean;
var
   a, b : Double;
begin
   FLeft.EvalAsFloat(a);
   FRight.EvalAsFloat(b);
   Result:=(a<=b);
end;

// ------------------
// ------------------ TRelGreaterFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterFloatExpr.EvalAsBoolean: Boolean;
var
   a, b : Double;
begin
   FLeft.EvalAsFloat(a);
   FRight.EvalAsFloat(b);
   Result:=(a>b);
end;

// ------------------
// ------------------ TRelGreaterEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualFloatExpr.EvalAsBoolean: Boolean;
var
   a, b : Double;
begin
   FLeft.EvalAsFloat(a);
   FRight.EvalAsFloat(b);
   Result:=(a>=b);
end;

// ------------------
// ------------------ TRelEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualStringExpr.EvalAsBoolean: Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(a);
   FRight.EvalAsString(b);
   Result:=(a=b);
end;

// ------------------
// ------------------ TRelNotEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualStringExpr.EvalAsBoolean: Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(a);
   FRight.EvalAsString(b);
   Result:=(a<>b);
end;

// ------------------
// ------------------ TRelLessStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessStringExpr.EvalAsBoolean: Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(a);
   FRight.EvalAsString(b);
   Result:=(a<b);
end;

// ------------------
// ------------------ TRelLessEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualStringExpr.EvalAsBoolean: Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(a);
   FRight.EvalAsString(b);
   Result:=(a<=b);
end;

// ------------------
// ------------------ TRelGreaterStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterStringExpr.EvalAsBoolean: Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(a);
   FRight.EvalAsString(b);
   Result:=(a>b);
end;

// ------------------
// ------------------ TRelGreaterEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualStringExpr.EvalAsBoolean: Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(a);
   FRight.EvalAsString(b);
   Result:=(a>=b);
end;

// ------------------
// ------------------ TRelEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualVariantExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.Eval=FRight.Eval);
end;

// ------------------
// ------------------ TRelNotEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualVariantExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.Eval<>FRight.Eval);
end;

// ------------------
// ------------------ TRelLessVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessVariantExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.Eval<FRight.Eval);
end;

// ------------------
// ------------------ TRelLessEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualVariantExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.Eval<=FRight.Eval);
end;

// ------------------
// ------------------ TRelGreaterVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterVariantExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.Eval>FRight.Eval);
end;

// ------------------
// ------------------ TRelGreaterEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualVariantExpr.EvalAsBoolean: Boolean;
begin
   Result:=(FLeft.Eval>=FRight.Eval);
end;

end.
