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

uses dwsSymbols, dwsTokenizer, dwsExprs, dwsCoreExprs;

type

   TRegisteredOperator = record
      OperatorSym : TOperatorSymbol;
      LeftType : TTypeSymbol;
      RighType : TTypeSymbol;
      Owned : Boolean;
   end;
   PRegisteredOperator = ^TRegisteredOperator;

   // lists of  operators and their expression classes
   // used for operator overloading
   TOperators = class
      private
         FCount : Integer;
         FItems : array [TTokenType] of array of TRegisteredOperator;

         function AddOperator(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;

      public
         destructor Destroy; override;

         procedure RegisterOperator(sym : TOperatorSymbol); overload;

         function RegisterOperator(aToken : TTokenType; aExprClass : TBinaryOpExprClass;
                                   aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator; overload;
         function RegisterOperator(aToken : TTokenType; aExprClass : TAssignExprClass;
                                   aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator; overload;
         function RegisterOperator(aToken : TTokenType; funcSymbol : TFuncSymbol;
                                   aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator; overload;

         function EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                        const callback : TOperatorSymbolEnumerationCallback) : Boolean;
         function EnumerateOperatorSymbols(const callback : TOperatorSymbolEnumerationCallback) : Boolean;
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

// Destroy
//
destructor TOperators.Destroy;
var
   tt : TTokenType;
   i : Integer;
begin
   for tt:=Low(TTokenType) to High(TTokenType) do begin
      for i:=0 to High(FItems[tt]) do
         if FItems[tt][i].Owned then
            FItems[tt][i].OperatorSym.Free;
   end;
   inherited;
end;

// AddOperator
//
function TOperators.AddOperator(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
var
   n : Integer;
begin
   n:=Length(FItems[aToken]);
   SetLength(FItems[aToken], n+1);
   Result:=@FItems[aToken][n];
   Result.LeftType:=aLeftType;
   Result.RighType:=aRightType;
   Inc(FCount);
end;

// RegisterOperator
//
procedure TOperators.RegisterOperator(sym : TOperatorSymbol);
var
   p : PRegisteredOperator;
begin
   p:=AddOperator(sym.Token, sym.Params[0], sym.Params[1]);
   p.OperatorSym:=sym;
   p.Owned:=False;
end;

// RegisterOperator
//
function TOperators.RegisterOperator(aToken : TTokenType; aExprClass : TBinaryOpExprClass;
                                     aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
begin
   Result:=AddOperator(aToken, aLeftType, aRightType);
   Result.OperatorSym:=TOperatorSymbol.Create(aToken);
   Result.OperatorSym.BinExprClass:=aExprClass;
   Result.OperatorSym.AddParam(aLeftType);
   Result.OperatorSym.AddParam(aRightType);
   Result.Owned:=True;
end;

// RegisterOperator
//
function TOperators.RegisterOperator(aToken : TTokenType; aExprClass : TAssignExprClass;
                                     aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
begin
   Result:=AddOperator(aToken, aLeftType, aRightType);
   Result.OperatorSym:=TOperatorSymbol.Create(aToken);
   Result.OperatorSym.AssignExprClass:=aExprClass;
   Result.OperatorSym.AddParam(aLeftType);
   Result.OperatorSym.AddParam(aRightType);
   Result.Owned:=True;
end;

// RegisterOperator
//
function TOperators.RegisterOperator(aToken : TTokenType; funcSymbol : TFuncSymbol;
                                     aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
begin
   Result:=AddOperator(aToken, aLeftType, aRightType);
   Result.OperatorSym:=TOperatorSymbol.Create(aToken);
   Result.OperatorSym.UsesSym:=funcSymbol;
   Result.OperatorSym.AddParam(aLeftType);
   Result.OperatorSym.AddParam(aRightType);
   Result.Owned:=True;
end;

// EnumerateOperatorsFor
//
function TOperators.EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                          const callback : TOperatorSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   p : PRegisteredOperator;
begin
   for i:=0 to High(FItems[aToken]) do begin
      p:=@FItems[aToken][i];
      if     ((aLeftType=p.LeftType) or aLeftType.IsOfType(p.LeftType))
         and ((aRightType=p.RighType) or aRightType.IsOfType(p.RighType)) then begin
         if callback(p.OperatorSym) then Exit(True);
      end;
   end;
   Result:=False;
end;

// EnumerateOperatorSymbols
//
function TOperators.EnumerateOperatorSymbols(const callback : TOperatorSymbolEnumerationCallback) : Boolean;
var
   tt : TTokenType;
   i : Integer;
   p : PRegisteredOperator;
begin
   for tt:=Low(FItems) to High(FItems) do begin
      for i:=0 to High(FItems[tt]) do begin
         p:=@FItems[tt][i];
         if callback(p.OperatorSym) then Exit(True);
      end;
   end;
   Result:=False;
end;

end.
