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

uses dwsSymbols, dwsTokenTypes, dwsExprs, dwsCoreExprs, dwsCompilerContext, dwsScriptSource;

type

   TRegisteredOperator = record
      OperatorSym : TOperatorSymbol;
      LeftType : TTypeSymbol;
      RighType : TTypeSymbol;
      Owned : Boolean;
   end;
   PRegisteredOperator = ^TRegisteredOperator;

   TRegisteredCaster = record
      CastType : TTypeSymbol;
      OperandType : TTypeSymbol;
      ExprClass : TTypedExprClass;
      Implicit : Boolean;
   end;
   PRegisteredCaster = ^TRegisteredCaster;

   TAsCasterExprCreator = reference to function (
      compilerContext : TdwsCompilerContext;
      operand : TTypedExpr; castType : TTypeSymbol;
      const scriptPos : TScriptPos
      ) : TTypedExpr;

   // lists of  operators and their expression classes
   // used for operator overloading
   TOperators = class
      private
         FCount : Integer;
         FOperators : array [ TTokenType ] of array of TRegisteredOperator;
         FCasters : array [0..15] of array of TRegisteredCaster;
         FAsCasters : array of TAsCasterExprCreator;

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

         function RegisterUnaryOperator(aToken : TTokenType; aExprClass : TUnaryOpExprClass;
                                        aType : TTypeSymbol) : PRegisteredOperator; overload;

         function EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                        const callback : TOperatorSymbolEnumerationCallback) : Boolean;
         function EnumerateOperatorSymbols(const callback : TOperatorSymbolEnumerationCallback) : Boolean;

         function EnumerateUnaryOperatorsFor(aToken : TTokenType; aType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean;

         function FindUnaryOperatorFor(aToken : TTokenType; aType : TTypeSymbol) : TOperatorSymbol;

         // strict check
         function HasOperatorFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : Boolean;

         procedure RegisterCaster(aCastType, aOperandType : TTypeSymbol;
                                  exprClass : TTypedExprClass; implicit : Boolean = False);

         function FindCaster(aCastType, aOperandType : TTypeSymbol) : TTypedExprClass;
         function FindImplicitCaster(aCastType, aOperandType : TTypeSymbol) : TTypedExprClass;

         procedure RegisterAsCaster(const asCaster : TAsCasterExprCreator);

         function CreateAsCastExpr(compilerContext : TdwsCompilerContext;
                                   operand : TTypedExpr; castType : TTypeSymbol;
                                   const scriptPos : TScriptPos) : TTypedExpr;
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
      for i:=0 to High(FOperators[tt]) do
         if FOperators[tt][i].Owned then
            FOperators[tt][i].OperatorSym.Free;
   end;
   inherited;
end;

// AddOperator
//
function TOperators.AddOperator(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : PRegisteredOperator;
var
   n : Integer;
begin
   n:=Length(FOperators[aToken]);
   SetLength(FOperators[aToken], n+1);
   Result:=@FOperators[aToken][n];
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
   Assert(Assigned(aExprClass));
   Result:=AddOperator(aToken, aLeftType, aRightType);
   Result.OperatorSym:=TOperatorSymbol.Create(aToken);
   Result.OperatorSym.OperatorExprClass:=aExprClass;
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

// RegisterUnaryOperator
//
function TOperators.RegisterUnaryOperator(aToken : TTokenType; aExprClass : TUnaryOpExprClass;
                                          aType : TTypeSymbol) : PRegisteredOperator;
begin
   Result:=AddOperator(aToken, nil, aType);
   Result.OperatorSym:=TOperatorSymbol.Create(aToken);
   Result.OperatorSym.OperatorExprClass:=aExprClass;
   Result.OperatorSym.AddParam(nil);
   Result.OperatorSym.AddParam(aType);
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
   for i:=0 to High(FOperators[aToken]) do begin
      p:=@FOperators[aToken][i];
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
   for tt:=Low(FOperators) to High(FOperators) do begin
      for i:=0 to High(FOperators[tt]) do begin
         p:=@FOperators[tt][i];
         if callback(p.OperatorSym) then Exit(True);
      end;
   end;
   Result:=False;
end;

// EnumerateUnaryOperatorsFor
//
function TOperators.EnumerateUnaryOperatorsFor(aToken : TTokenType; aType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   p : PRegisteredOperator;
begin
   for i:=0 to High(FOperators[aToken]) do begin
      p:=@FOperators[aToken][i];
      if     (p.LeftType=nil)
         and ((aType=p.RighType) or aType.IsOfType(p.RighType)) then begin
         if callback(p.OperatorSym) then Exit(True);
      end;
   end;
   Result:=False;
end;

// FindUnaryOperatorFor
//
function TOperators.FindUnaryOperatorFor(aToken : TTokenType; aType : TTypeSymbol) : TOperatorSymbol;
var
   i : Integer;
   p : PRegisteredOperator;
begin
   for i:=0 to High(FOperators[aToken]) do begin
      p:=@FOperators[aToken][i];
      if     (p.LeftType=nil)
         and ((aType=p.RighType) or aType.IsOfType(p.RighType)) then begin
         Exit(p.OperatorSym);
      end;
   end;
   Result:=nil;
end;

// HasOperatorFor
//
function TOperators.HasOperatorFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol) : Boolean;
var
   i : Integer;
   p : PRegisteredOperator;
begin
   for i:=0 to High(FOperators[aToken]) do begin
      p:=@FOperators[aToken][i];
      if (aLeftType=p.LeftType) and (aRightType=p.RighType) then Exit(True);
   end;
   Result:=False;
end;

// RegisterCaster
//
procedure TOperators.RegisterCaster(aCastType, aOperandType : TTypeSymbol;
                                    exprClass : TTypedExprClass; implicit : Boolean = False);
var
   h, n : Integer;
   p : PRegisteredCaster;
begin
   h := (NativeUInt(aCastType) shr 4) and 15;
   n := Length(FCasters[h]);
   SetLength(FCasters[h], n+1);
   p := @FCasters[h][n];

   p.CastType := aCastType;
   p.OperandType := aOperandType;
   p.ExprClass := exprClass;
   p.Implicit := implicit;
end;

// FindCaster
//
function TOperators.FindCaster(aCastType, aOperandType : TTypeSymbol) : TTypedExprClass;
var
   h, i : Integer;
   p : PRegisteredCaster;
begin
   h := (NativeUInt(aCastType) shr 4) and 15;
   for i := 0 to High(FCasters[h]) do begin
      p := @FCasters[h][i];
      if (p.CastType=aCastType) and (p.OperandType=aOperandType) then
         Exit(p.ExprClass);
   end;
   Result := nil;
end;

// FindImplicitCaster
//
function TOperators.FindImplicitCaster(aCastType, aOperandType : TTypeSymbol) : TTypedExprClass;
var
   h, i : Integer;
   p : PRegisteredCaster;
begin
   h := (NativeUInt(aCastType) shr 4) and 15;
   for i := 0 to High(FCasters[h]) do begin
      p := @FCasters[h][i];
      if p.Implicit and (p.CastType=aCastType) and (p.OperandType=aOperandType) then
         Exit(p.ExprClass);
   end;
   Result := nil;
end;

// RegisterAsCaster
//
procedure TOperators.RegisterAsCaster(const asCaster : TAsCasterExprCreator);
begin
   var n := Length(FAsCasters);
   SetLength(FAsCasters, n+1);
   FAsCasters[n] := asCaster;
end;

// CreateAsCastExpr
//
function TOperators.CreateAsCastExpr(
   compilerContext : TdwsCompilerContext; operand : TTypedExpr; castType : TTypeSymbol;
   const scriptPos : TScriptPos) : TTypedExpr;
begin
   for var i := 0 to High(FAsCasters) do begin
      Result := FAsCasters[i](compilerContext, operand, castType, scriptPos);
      if Result <> nil then Exit;
   end;
   Result := nil;
end;

end.
