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
unit dwsUnifiedConstants;

{$I dws.inc}

interface

uses dwsXPlatform, dwsConstExprs, dwsUnitSymbols, dwsUtils;

type

   TStandardIntegersConstIntExprArray = array [-1..2] of TConstIntExpr;

   // TUnifiedConstants
   //
   TUnifiedConstants = class
      private
         FEmptyString : TConstStringExpr;
         FIntegers : TStandardIntegersConstIntExprArray;
         FZeroFloat : TConstFloatExpr;
         FTrue, FFalse : TConstBooleanExpr;
         FNil : TConstNilExpr;

      public
         constructor Create(systemTable : TSystemSymbolTable);
         destructor Destroy; override;

         property EmptyString : TConstStringExpr read FEmptyString;
         property Integers : TStandardIntegersConstIntExprArray read FIntegers;
         property ZeroFloat : TConstFloatExpr read FZeroFloat;
         property TrueConst : TConstBooleanExpr read FTrue;
         property FalseConst : TConstBooleanExpr read FFalse;
         property NilConst : TConstNilExpr read FNil;

         function CreateEmptyString : TConstStringExpr;
         function CreateString(const s : String) : TConstStringExpr;
         function CreateInteger(const i : Int64) : TConstIntExpr;
         function CreateFloat(const f : Double) : TConstFloatExpr;
         function CreateNil : TConstNilExpr;
         function CreateBoolean(v : Boolean) : TConstBooleanExpr;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsScriptSource;

// ------------------
// ------------------ TUnifiedConstants ------------------
// ------------------

// Precharge
//
constructor TUnifiedConstants.Create(systemTable : TSystemSymbolTable);
const
   cZeroFloat : Double = 0;
   cNilIntf : IUnknown = nil;
var
   i : Integer;
begin
   inherited Create;
   // no lock is required here
   FEmptyString := TConstStringExpr.Create(cNullPos, systemTable.TypString, '');
   for i:=Low(FIntegers) to High(FIntegers) do
      FIntegers[i] := TConstIntExpr.Create(cNullPos, systemTable.TypInteger, Int64(i));
   FZeroFloat := TConstFloatExpr.Create(cNullPos, systemTable.TypFloat, cZeroFloat);
   FTrue := TConstBooleanExpr.Create(cNullPos, systemTable.TypBoolean, True);
   FFalse := TConstBooleanExpr.Create(cNullPos, systemTable.TypBoolean, False);
   FNil := TConstNilExpr.Create(cNullPos, systemTable.TypNil);
end;

// Destroy
//
destructor TUnifiedConstants.Destroy;
var
   i : Integer;
begin
   FEmptyString.Free;
   for i:=Low(FIntegers) to High(FIntegers) do begin
      Assert(FIntegers[i].RefCount=0);
      FIntegers[i].Free;
   end;
   FZeroFloat.Free;
   FTrue.Free;
   FFalse.Free;
   FNil.Free;
   inherited;
end;

// CreateEmptyString
//
function TUnifiedConstants.CreateEmptyString : TConstStringExpr;
begin
   Result := FEmptyString;
   Result.IncRefCount;
end;

// CreateString
//
function TUnifiedConstants.CreateString(const s : String) : TConstStringExpr;
begin
   if s = '' then
      Result := CreateEmptyString
   else Result := TConstStringExpr.Create(cNullPos, FEmptyString.Typ, s);
end;

// CreateInteger
//
function TUnifiedConstants.CreateInteger(const i : Int64) : TConstIntExpr;
begin
   // can't use a "case of" or range here because of compiler bug (will do a 32bit comparison)
   if (i >= Low(FIntegers)) and (i <= High(FIntegers)) then begin
      Result := FIntegers[i];
      Result.IncRefCount;
   end else begin
      Result := TConstIntExpr.Create(cNullPos, FIntegers[0].Typ, i);
   end;
end;

// CreateFloat
//
function TUnifiedConstants.CreateFloat(const f : Double) : TConstFloatExpr;
begin
   // compare vs zero, but STRICT, without matching NAN and other stuff
   if PUInt64(@f)^ = 0 then begin
      Result := FZeroFloat;
      Result.IncRefCount;
   end else begin
      Result := TConstFloatExpr.Create(cNullPos, FZeroFloat.Typ, f);
   end;
end;

// CreateNil
//
function TUnifiedConstants.CreateNil : TConstNilExpr;
begin
   Result := FNil;
   Result.IncRefCount;
end;

// CreateBoolean
//
function TUnifiedConstants.CreateBoolean(v : Boolean) : TConstBooleanExpr;
begin
   if v then
      Result := FTrue
   else Result := FFalse;
   Result.IncRefCount;
end;

end.
