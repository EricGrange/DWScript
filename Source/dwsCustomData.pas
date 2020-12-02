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
unit dwsCustomData;

{$I dws.inc}

interface

uses
   SysUtils, Variants,
   dwsUtils;

type

   TdwsCustomState = record
      Key : TGUID;
      Value : Variant;
   end;

   TdwsCustomStates = class (TSimpleHash<TdwsCustomState>)
      protected
         function SameItem(const item1, item2 : TdwsCustomState) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsCustomState) : Cardinal; override;

         function AddClonedState(const item : TdwsCustomState) : TSimpleHashAction;

         function GetState(const index : TGUID) : Variant; inline;
         procedure SetState(const index : TGUID; const v : Variant);

      public
         property States[const index : TGUID] : Variant read GetState write SetState; default;

         procedure VariantState(const index : TGUID; var result : Variant);

         function IntegerStateDef(const index : TGUID; const default : Integer) : Integer;
         function StringStateDef(const index : TGUID; const default : String) : String;

         function Clone : TdwsCustomStates;
   end;

   TdwsCustomInterface = record
      Key : TGUID;
      Value : IInterface;
   end;

   TdwsCustomInterfaces = class (TSimpleHash<TdwsCustomInterface>)
      protected
         function SameItem(const item1, item2 : TdwsCustomInterface) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsCustomInterface) : Cardinal; override;

         function  GetInterface(const index : TGUID) : IInterface; inline;
         procedure SetInterface(const index : TGUID; const intf : IInterface); inline;

      public
         property Interfaces[const index : TGUID] : IInterface read GetInterface write SetInterface; default;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// GUIDToHash
//
function GUIDToHash(const guid : TGUID) : Cardinal; inline;
type
   TCardinal4 = array [0..3] of Cardinal;
begin
   Result := TCardinal4(guid)[0] xor TCardinal4(guid)[1] xor TCardinal4(guid)[2] xor TCardinal4(guid)[3];
   if Result = 0 then Result := 1;
end;

// ------------------
// ------------------ TdwsCustomStates ------------------
// ------------------

// SameItem
//
function TdwsCustomStates.SameItem(const item1, item2 : TdwsCustomState) : Boolean;
begin
   Result := IsEqualGUID(item1.Key, item2.Key);
end;

// GetItemHashCode
//
function TdwsCustomStates.GetItemHashCode(const item1 : TdwsCustomState) : Cardinal;
begin
   Result := GUIDToHash(item1.Key);
end;

// GetState
//
function TdwsCustomStates.GetState(const index : TGUID) : Variant;
begin
   VariantState(index, Result);
end;

// SetState
//
procedure TdwsCustomStates.SetState(const index : TGUID; const v : Variant);
var
   s : TdwsCustomState;
begin
   s.Key:=index;
   s.Value:=v;
   Replace(s);
end;

// VariantState
//
procedure TdwsCustomStates.VariantState(const index : TGUID; var result : Variant);
var
   s : TdwsCustomState;
begin
   s.Key := index;
   if Match(s) then
      VarCopySafe(result, s.Value)
   else VarClearSafe(result);
end;

// IntegerStateDef
//
function TdwsCustomStates.IntegerStateDef(const index : TGUID; const default : Integer) : Integer;
var
   s : TdwsCustomState;
begin
   s.Key:=index;
   if Match(s) and VariantIsOrdinal(s.Value) then
      Result:=s.Value
   else Result:=default;
end;

// StringStateDef
//
function TdwsCustomStates.StringStateDef(const index : TGUID; const default : String) : String;
var
   s : TdwsCustomState;
begin
   s.Key:=index;
   if Match(s) and VariantIsString(s.Value) then
      VariantToString(s.Value, Result)
   else Result:=default;
end;

// AddClonedState
//
function TdwsCustomStates.AddClonedState(const item : TdwsCustomState) : TSimpleHashAction;
begin
   SetState(item.Key, item.Value);
   Result := shaNone;
end;

// Clone
//
function TdwsCustomStates.Clone : TdwsCustomStates;
begin
   Result := TdwsCustomStates.Create;
   Self.Enumerate(Result.AddClonedState);
end;

// ------------------
// ------------------ TdwsCustomInterfaces ------------------
// ------------------

// SameItem
//
function TdwsCustomInterfaces.SameItem(const item1, item2 : TdwsCustomInterface) : Boolean;
begin
   Result := IsEqualGUID(item1.Key, item2.Key);
end;

// GetItemHashCode
//
function TdwsCustomInterfaces.GetItemHashCode(const item1 : TdwsCustomInterface) : Cardinal;
begin
   Result := GUIDToHash(item1.Key);
end;

// GetInterface
//
function TdwsCustomInterfaces.GetInterface(const index : TGUID) : IInterface;
var
   s : TdwsCustomInterface;
begin
   s.Key:=index;
   if Match(s) then
      Result:=s.Value
   else Result:=nil;
end;

// SetInterface
//
procedure TdwsCustomInterfaces.SetInterface(const index : TGUID; const intf : IInterface);
var
   s : TdwsCustomInterface;
begin
   s.Key := index;
   s.Value := intf;
   Replace(s);
end;

end.
