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
unit dwsArrayIndexOfExprs;

{$I dws.inc}

interface

uses
   SysUtils,
   dwsUtils, dwsDataContext, dwsExprs, dwsCompilerContext, dwsSymbols, dwsScriptSource,
   dwsXPlatform, dwsDynamicArrays, dwsArrayExprs;

type

   TArrayIndexOfExpr = class;
   TArrayIndexOfExprClass = class of TArrayIndexOfExpr;

   // Find element in a dynamic array (shallow comparison)
   TArrayIndexOfExpr = class abstract (TArrayTypedExpr)
      private
         FItemExpr : TTypedExpr;
         FFromIndexExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            aBase : TTypedExpr; aItem : TTypedExpr; aFromIndex : TTypedExpr); reintroduce; virtual;
         destructor Destroy; override;

         class function ArrayIndexOfExprClass(arraySym : TArraySymbol) : TArrayIndexOfExprClass; static;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;

         property ItemExpr : TTypedExpr read FItemExpr;
         property FromIndexExpr : TTypedExpr read FFromIndexExpr;
   end;

   TDynamicArrayIndexOfExpr = class abstract (TArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; virtual; abstract;

      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TDynamicArrayIndexOfDataExpr = class(TDynamicArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; override;
   end;

   TDynamicArrayIndexOfVariantExpr = class(TDynamicArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; override;
   end;

   TDynamicArrayIndexOfFuncPtrExpr = class(TDynamicArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; override;
   end;

   TDynamicArrayIndexOfIntegerExpr = class(TDynamicArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; override;
   end;

   TDynamicArrayIndexOfStringExpr = class(TDynamicArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; override;
   end;

   TDynamicArrayIndexOfFloatExpr = class(TDynamicArrayIndexOfExpr)
      protected
         function DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer; override;
   end;

   TStaticArrayIndexOfExpr = class(TArrayIndexOfExpr)
      private
         FForceZeroBased : Boolean;

      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;

         property ForceZeroBased : Boolean read FForceZeroBased write FForceZeroBased;
   end;

   // Remove an element in a dynamic array (shallow comparison)
   TArrayRemoveExpr = class(TArrayTypedExpr)
      private
         FIndexOf : TDynamicArrayIndexOfExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetItemExpr : TTypedExpr; inline;
         function GetFromIndexExpr : TTypedExpr; inline;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            aBase : TTypedExpr; aItem : TTypedExpr; aFromIndex : TTypedExpr);
         destructor Destroy; override;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;

         property ItemExpr : TTypedExpr read GetItemExpr;
         property FromIndexExpr : TTypedExpr read GetFromIndexExpr;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TArrayIndexOfExpr ------------------
// ------------------

// Create
//
constructor TArrayIndexOfExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                            aBase : TTypedExpr; aItem : TTypedExpr; aFromIndex : TTypedExpr);
begin
   inherited Create(context, scriptPos, aBase);
   FItemExpr:=aItem;
   FFromIndexExpr:=aFromIndex;
   Typ:=context.TypInteger;
end;

// Destroy
//
destructor TArrayIndexOfExpr.Destroy;
begin
   inherited;
   FItemExpr.Free;
   FFromIndexExpr.Free;
end;

// ArrayIndexOfExprClass
//
class function TArrayIndexOfExpr.ArrayIndexOfExprClass(arraySym : TArraySymbol) : TArrayIndexOfExprClass;
var
   elemTyp : TTypeSymbol;
   elemTypClass : TClass;
begin
   if arraySym.ClassType = TDynamicArraySymbol then begin
      elemTyp := arraySym.Typ.UnAliasedType;
      if elemTyp.Size > 1 then
         Result := TDynamicArrayIndexOfDataExpr
      else begin
         elemTypClass := elemTyp.ClassType;
         if elemTypClass = TBaseIntegerSymbol then
            Result := TDynamicArrayIndexOfIntegerExpr
         else if elemTypClass = TBaseStringSymbol then
            Result := TDynamicArrayIndexOfStringExpr
         else if elemTyp.ClassType = TBaseFloatSymbol then
            Result := TDynamicArrayIndexOfFloatExpr
         else if elemTyp.AsFuncSymbol <> nil then
            Result := TDynamicArrayIndexOfFuncPtrExpr
         else Result := TDynamicArrayIndexOfVariantExpr;
      end;
   end else Result := TStaticArrayIndexOfExpr;
end;

// EvalAsVariant
//
procedure TArrayIndexOfExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, EvalAsInteger(exec));
end;

// GetSubExpr
//
function TArrayIndexOfExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result := BaseExpr;
      1 : Result := ItemExpr;
   else
      Result := FromIndexExpr;
   end;
end;

// GetSubExprCount
//
function TArrayIndexOfExpr.GetSubExprCount : Integer;
begin
   Result:=3
end;

// ------------------
// ------------------ TDynamicArrayIndexOfExpr ------------------
// ------------------

// EvalAsInteger
//
function TDynamicArrayIndexOfExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptDynArray;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   Result := DoEval(exec, base);
end;

// ------------------
// ------------------ TDynamicArrayIndexOfDataExpr ------------------
// ------------------

// DoEval
//
function TDynamicArrayIndexOfDataExpr.DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer;
var
   dyn : TScriptDynamicDataArray;
   fromIndex : Integer;
begin
   dyn := (base.GetSelf as TScriptDynamicDataArray);
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   Result:=dyn.IndexOfData(TDataExpr(FItemExpr).DataPtr[exec], fromIndex, dyn.ArrayLength-1, FItemExpr.Typ.Size)
end;

// ------------------
// ------------------ TDynamicArrayIndexOfVariantExpr ------------------
// ------------------

// DoEval
//
function TDynamicArrayIndexOfVariantExpr.DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer;
var
   fromIndex : Integer;
   v : Variant;
begin
   if FFromIndexExpr <> nil then
      fromIndex := FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex := 0;
   FItemExpr.EvalAsVariant(exec, v);
   Result := base.IndexOfValue(v, fromIndex);
end;

// ------------------
// ------------------ TDynamicArrayIndexOfFuncPtrExpr ------------------
// ------------------

// DoEval
//
function TDynamicArrayIndexOfFuncPtrExpr.DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer;
var
   fromIndex : Integer;
   v : Variant;
begin
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   FItemExpr.EvalAsVariant(exec, v);
   Result := base.IndexOfFuncPtr(v, fromIndex)
end;

// ------------------
// ------------------ TDynamicArrayIndexOfIntegerExpr ------------------
// ------------------

// DoEval
//
function TDynamicArrayIndexOfIntegerExpr.DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer;
var
   fromIndex : Integer;
begin
   if FFromIndexExpr <> nil then
      fromIndex := FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex := 0;
   Result := base.IndexOfInteger(FItemExpr.EvalAsInteger(exec), fromIndex);
end;

// ------------------
// ------------------ TDynamicArrayIndexOfStringExpr ------------------
// ------------------

// DoEval
//
function TDynamicArrayIndexOfStringExpr.DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer;
var
   fromIndex : Integer;
   v : String;
begin
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   FItemExpr.EvalAsString(exec, v);
   Result := base.IndexOfString(v, fromIndex);
end;

// ------------------
// ------------------ TDynamicArrayIndexOfFloatExpr ------------------
// ------------------

// DoEval
//
function TDynamicArrayIndexOfFloatExpr.DoEval(exec : TdwsExecution; const base : IScriptDynArray) : Integer;
var
   fromIndex : Integer;
   v : Double;
begin
   if FFromIndexExpr <> nil then
      fromIndex := FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex := 0;
   v := FItemExpr.EvalAsFloat(exec);
   Result := base.IndexOfFloat(v, fromIndex);
end;

// ------------------
// ------------------ TStaticArrayIndexOfExpr ------------------
// ------------------

// EvalAsInteger
//
function TStaticArrayIndexOfExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   fromIndex : Integer;
   arrayDC : IDataContext;
   dc : TDataContext;
   value : Variant;
   arrayTyp : TStaticArraySymbol;
begin
   arrayTyp := BaseExpr.Typ as TStaticArraySymbol;
   if FFromIndexExpr<>nil then begin
      fromIndex := FFromIndexExpr.EvalAsInteger(exec);
      if fromIndex < arrayTyp.LowBound then
         fromIndex := arrayTyp.LowBound;
   end else fromIndex := arrayTyp.LowBound;

   arrayDC := (BaseExpr as TDataExpr).DataPtr[exec];
   dc := (arrayDC.GetSelf as TDataContext);

   if FItemExpr.Typ.Size = 1 then begin
      FItemExpr.EvalAsVariant(exec, value);
      Result := dc.IndexOfValue(value, fromIndex - arrayTyp.LowBound, arrayTyp.ElementCount - 1);
   end else begin
      Result := TDataContext(arrayDC.GetSelf).IndexOfData(
         TDataExpr(FItemExpr).DataPtr[exec],
         fromIndex - arrayTyp.LowBound, arrayTyp.ElementCount - 1,
         FItemExpr.Typ.Size
      );
   end;
   if not ForceZeroBased then
      if (Result >= 0) or (arrayTyp.LowBound < 0) then
         Result := Result + arrayTyp.LowBound;
end;

// ------------------
// ------------------ TArrayRemoveExpr ------------------
// ------------------

// Create
//
constructor TArrayRemoveExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                    aBase : TTypedExpr; aItem : TTypedExpr; aFromIndex : TTypedExpr);
var
   indexOfClass : TArrayIndexOfExprClass;
begin
   inherited Create(context, scriptPos, aBase);
   Typ := context.TypInteger;
   indexOfClass := TArrayIndexOfExpr.ArrayIndexOfExprClass(aBase.Typ as TDynamicArraySymbol);
   FIndexOf := indexOfClass.Create(context, scriptPos, aBase, aItem, aFromIndex) as TDynamicArrayIndexOfExpr;
   aBase.IncRefCount;
end;

// Destroy
//
destructor TArrayRemoveExpr.Destroy;
begin
   inherited;
   FIndexOf.Free;
end;

// EvalAsVariant
//
procedure TArrayRemoveExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   VarCopySafe(Result, EvalAsInteger(exec));
end;

// GetSubExpr
//
function TArrayRemoveExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result := FIndexOf.GetSubExpr(i);
end;

// GetSubExprCount
//
function TArrayRemoveExpr.GetSubExprCount : Integer;
begin
   Result := FIndexOf.SubExprCount;
end;

// GetItemExpr
//
function TArrayRemoveExpr.GetItemExpr : TTypedExpr;
begin
   Result := FIndexOf.ItemExpr;
end;

// GetFromIndexExpr
//
function TArrayRemoveExpr.GetFromIndexExpr : TTypedExpr;
begin
   Result := FIndexOf.FromIndexExpr;
end;

// EvalAsInteger
//
function TArrayRemoveExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   index : Integer;
   base : IScriptDynArray;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   index := FIndexOf.DoEval(exec, base);
   if index >= 0 then
      base.Delete(index, 1);
   Result := index;
end;

end.
