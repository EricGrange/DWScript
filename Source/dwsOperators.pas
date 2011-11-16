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
unit dwsOperators;

{$I dws.inc}

interface

uses dwsSymbols, dwsTokenizer, dwsStrings, dwsExprs, dwsCoreExprs, dwsErrors;

type

   TRegisteredOperator = record
      BinExprClass : TBinaryOpExprClass;
      AssignExprClass : TAssignExprClass;
      FuncSym : TFuncSymbol;
      LeftType : TTypeSymbol;
      RighType : TTypeSymbol;
   end;
   PRegisteredOperator = ^TRegisteredOperator;

   // lists of  operators and their expression classes
   // used for operator overloading
   TOperators = class
      private
         FCount : Integer;
         FItems : array [TTokenType] of array of TRegisteredOperator;
         FParent, FChild : TOperators;

         function AddOperator(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : Integer;

      public
         constructor Create(aParent : TOperators);
         destructor Destroy; override;

         function RegisterOperator(sym : TOperatorSymbol) : Boolean; overload;

         procedure RegisterOperator(aToken : TTokenType; aExprClass : TBinaryOpExprClass;
                                    aLeftType, aRightType : TTypeSymbol); overload;
         procedure RegisterOperator(aToken : TTokenType; aExprClass : TAssignExprClass;
                                    aLeftType, aRightType : TTypeSymbol); overload;
         procedure RegisterOperator(aToken : TTokenType; funcSymbol : TFuncSymbol;
                                    aLeftType, aRightType : TTypeSymbol); overload;

         function OperatorFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TOperators ------------------
// ------------------

// Create
//
constructor TOperators.Create(aParent : TOperators);
begin
   if aParent<>nil then begin
      FParent:=aParent;
      aParent.FChild:=Self;
   end;
end;

// Destroy
//
destructor TOperators.Destroy;
begin
   FChild.Free;
   inherited;
end;

// AddOperator
//
function TOperators.AddOperator(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : Integer;
begin
   Result:=Length(FItems[aToken]);
   SetLength(FItems[aToken], Result+1);
   with FItems[aToken][Result] do begin
      LeftType:=aLeftType;
      RighType:=aRightType;
   end;
   Inc(FCount);
end;

// RegisterOperator
//
function TOperators.RegisterOperator(sym : TOperatorSymbol) : Boolean;
begin
   Result:=(OperatorFor(sym.Token, sym.Params[0], sym.Params[1])=nil);
   RegisterOperator(sym.Token, sym.UsesSym, sym.Params[0], sym.Params[1]);
end;

// RegisterOperator
//
procedure TOperators.RegisterOperator(aToken : TTokenType; aExprClass : TBinaryOpExprClass;
                                      aLeftType, aRightType : TTypeSymbol);
var
   n : Integer;
begin
   n:=AddOperator(aToken, aLeftType, aRightType);
   FItems[aToken][n].BinExprClass:=aExprClass;
end;

// RegisterOperator
//
procedure TOperators.RegisterOperator(aToken : TTokenType; aExprClass : TAssignExprClass;
                                      aLeftType, aRightType : TTypeSymbol);
var
   n : Integer;
begin
   n:=AddOperator(aToken, aLeftType, aRightType);
   FItems[aToken][n].AssignExprClass:=aExprClass;
end;

// RegisterOperator
//
procedure TOperators.RegisterOperator(aToken : TTokenType; funcSymbol : TFuncSymbol;
                                      aLeftType, aRightType : TTypeSymbol);
var
   n : Integer;
begin
   n:=AddOperator(aToken, aLeftType, aRightType);
   FItems[aToken][n].FuncSym:=funcSymbol;
end;

// OperatorFor
//
function TOperators.OperatorFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
var
   i : Integer;
   p : PRegisteredOperator;
begin
   p:=nil;
   if (FCount>0) and (aLeftType<>nil) and (aRightType<>nil) then begin
      for i:=0 to High(FItems[aToken]) do begin
         Result:=@FItems[aToken][i];
         if (aLeftType=Result.LeftType) and (aRightType=Result.RighType) then
            Exit
         else if aLeftType.IsOfType(Result.LeftType) and aRightType.IsOfType(Result.RighType) then
            p:=Result;
      end;
   end;
   if p=nil then
      if FParent<>nil then
         Result:=FParent.OperatorFor(aToken, aLeftType, aRightType)
      else Result:=nil
   else Result:=p;
end;

end.
