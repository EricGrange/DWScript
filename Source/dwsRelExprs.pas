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

uses dwsExprs, dwsSymbols;

type

   TRelOpExpr = class(TBinaryOpExpr)
      constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); override;
      function Eval(exec : TdwsExecution) : Variant; override;
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
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelNotEqualBoolExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // integer rel ops

   TRelEqualIntExpr = class(TRelEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelNotEqualIntExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessIntExpr = class(TRelLessExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessEqualIntExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterIntExpr = class(TRelGreaterExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterEqualIntExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // float rel ops

   TRelEqualFloatExpr = class(TRelEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelNotEqualFloatExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessFloatExpr = class(TRelLessExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessEqualFloatExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterFloatExpr = class(TRelGreaterExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterEqualFloatExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // string rel ops

   TRelEqualStringExpr = class(TRelEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelNotEqualStringExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessStringExpr = class(TRelLessExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessEqualStringExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterStringExpr = class(TRelGreaterExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterEqualStringExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // variant rel ops

   TRelEqualVariantExpr = class(TRelEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelNotEqualVariantExpr = class(TRelNotEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessVariantExpr = class(TRelLessExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelLessEqualVariantExpr = class(TRelLessEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterVariantExpr = class(TRelGreaterExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TRelGreaterEqualVariantExpr = class(TRelGreaterEqualExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // metaclass rel ops

   TRelEqualMetaExpr = class(TRelEqualIntExpr)
   end;
   TRelNotEqualMetaExpr = class(TRelNotEqualIntExpr)
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
constructor TRelOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypBoolean;
end;

// Eval
//
function TRelOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TRelEqualBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsBoolean(exec)=FRight.EvalAsBoolean(exec));
end;

// ------------------
// ------------------ TRelNotEqualBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsBoolean(exec)<>FRight.EvalAsBoolean(exec));
end;

// ------------------
// ------------------ TRelEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualIntExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsInteger(exec)=FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TRelNotEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualIntExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsInteger(exec)<>FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TRelLessIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessIntExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsInteger(exec)<FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TRelLessEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualIntExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsInteger(exec)<=FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TRelGreaterIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterIntExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsInteger(exec)>FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TRelGreaterEqualIntExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualIntExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsInteger(exec)>=FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TRelEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualFloatExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsFloat(exec)=FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TRelNotEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualFloatExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsFloat(exec)<>FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TRelLessFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessFloatExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsFloat(exec)<FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TRelLessEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualFloatExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsFloat(exec)<=FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TRelGreaterFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterFloatExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsFloat(exec)>FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TRelGreaterEqualFloatExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualFloatExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FLeft.EvalAsFloat(exec)>=FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TRelEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualStringExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(exec, a);
   FRight.EvalAsString(exec, b);
   Result:=(a=b);
end;

// ------------------
// ------------------ TRelNotEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualStringExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(exec, a);
   FRight.EvalAsString(exec, b);
   Result:=(a<>b);
end;

// ------------------
// ------------------ TRelLessStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessStringExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(exec, a);
   FRight.EvalAsString(exec, b);
   Result:=(a<b);
end;

// ------------------
// ------------------ TRelLessEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualStringExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(exec, a);
   FRight.EvalAsString(exec, b);
   Result:=(a<=b);
end;

// ------------------
// ------------------ TRelGreaterStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterStringExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(exec, a);
   FRight.EvalAsString(exec, b);
   Result:=(a>b);
end;

// ------------------
// ------------------ TRelGreaterEqualStringExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualStringExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   a, b : String;
begin
   FLeft.EvalAsString(exec, a);
   FRight.EvalAsString(exec, b);
   Result:=(a>=b);
end;

// ------------------
// ------------------ TRelEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelEqualVariantExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=(lv=rv);
end;

// ------------------
// ------------------ TRelNotEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelNotEqualVariantExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=(lv<>rv);
end;

// ------------------
// ------------------ TRelLessVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessVariantExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=(lv<rv);
end;

// ------------------
// ------------------ TRelLessEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelLessEqualVariantExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=(lv<=rv);
end;

// ------------------
// ------------------ TRelGreaterVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterVariantExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=(lv>rv);
end;

// ------------------
// ------------------ TRelGreaterEqualVariantExpr ------------------
// ------------------

// EvalAsBoolean
//
function TRelGreaterEqualVariantExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=(lv>=rv);
end;

end.
