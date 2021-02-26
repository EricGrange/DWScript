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
unit dwsDynamicArrays;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsSymbols, dwsUtils, dwsDataContext, dwsJSON;

type
   TScriptDynamicArray = class abstract (TDataContext, IScriptDynArray)//(TInterfacedSelfObject, IScriptDynArray)//
      private
         FElementTyp : TTypeSymbol;
         FElementSize : Integer;
         FArrayLength : Integer;

      protected
         function GetElementSize : Integer;
         function GetElementType : TTypeSymbol;

         procedure SetArrayLength(n : Integer);
         function GetArrayLength : Integer;

         procedure SetAsVariant(index : Integer; const v : Variant);
         procedure EvalAsVariant(index : Integer; var result : Variant);

         function GetAsInteger(index : Integer) : Int64;
         procedure SetAsInteger(index : Integer; const v : Int64);

         function GetAsFloat(index : Integer) : Double;
         procedure SetAsFloat(index : Integer; const v : Double);

         function GetAsBoolean(index : Integer) : Boolean;
         procedure SetAsBoolean(index : Integer; const v : Boolean);

         procedure SetAsString(index : Integer; const v : String);
         procedure EvalAsString(index : Integer; var result : String);
         function  GetAsString(index : Integer) : String; inline;

         procedure SetAsInterface(index : Integer; const v : IUnknown);
         procedure EvalAsInterface(index : Integer; var result : IUnknown);

      public
         constructor Create(elemTyp : TTypeSymbol);

         function BoundsCheckPassed(index : Integer) : Boolean;

         procedure Delete(index, count : Integer);
         procedure Insert(index : Integer);
         procedure Swap(i1, i2 : Integer); virtual; abstract;
         procedure Reverse;
         procedure NaturalSort; virtual;

         function IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
         function IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
         function IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
         function IndexOfString(const item : String; fromIndex : Integer) : Integer;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         procedure Copy(src : TScriptDynamicArray; index, count : Integer);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);
         procedure MoveItem(srcIndex, dstIndex : Integer);

         function ToString : String; override;
         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure ReplaceData(const newData : TData); override;

         procedure AddStrings(sl : TStrings);

         function AsPDouble(var nbElements, stride : Integer) : PDouble; virtual;

         function HashCode(addr : Integer; size : Integer) : Cardinal; virtual;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ElementSize : Integer read FElementSize;
         property ArrayLength : Integer read FArrayLength write SetArrayLength;

         function VarType(addr : Integer) : TVarType; reintroduce; virtual;
         function IsEmpty(addr : Integer) : Boolean; virtual;

         property AsInteger[index : Integer] : Int64 read GetAsInteger write SetAsInteger;
         property AsFloat[index : Integer] : Double read GetAsFloat write SetAsFloat;
         property AsString[index : Integer] : String read GetAsString write SetAsString;
   end;

   TScriptDynamicDataArray = class (TScriptDynamicArray)
      public
         procedure Swap(i1, i2 : Integer); override;
   end;

   TScriptDynamicValueArray = class (TScriptDynamicArray)
      public
         procedure ReplaceData(const newData : TData); override;

         procedure Swap(i1, i2 : Integer); override;

         function CompareString(i1, i2 : Integer) : Integer;
         function CompareInteger(i1, i2 : Integer) : Integer;
         function CompareFloat(i1, i2 : Integer) : Integer;
   end;

   TScriptDynamicNativeArray = class abstract (TInterfacedSelfObject)
      private
         FElementTyp : TTypeSymbol;

      protected
         FArrayLength : Integer;

         function GetElementSize : Integer;
         function GetElementType : TTypeSymbol;

         function GetArrayLength : Integer;

      public
         constructor Create(elemTyp : TTypeSymbol); virtual;

         function ToString : String; override;

         function BoundsCheckPassed(index : Integer) : Boolean;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ArrayLength : Integer read FArrayLength;
   end;

   TScriptDynamicNativeIntegerArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FData : TInt64DynArray;

      public
         procedure SetArrayLength(n : Integer);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure Insert(index : Integer);
         procedure Delete(index, count : Integer);
         procedure MoveItem(source, destination : Integer);
         procedure Swap(index1, index2 : Integer);

         function IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
         function IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
         function IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
         function IndexOfString(const item : String; fromIndex : Integer) : Integer;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         procedure WriteData(const src : TData; srcAddr, size : Integer);
         procedure ReplaceData(const v : TData);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);

         procedure Reverse;
         function  Compare(index1, index2 : Integer) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function AsPDouble(var nbElements, stride : Integer) : PDouble;

         function GetAsFloat(index : Integer) : Double;
         procedure SetAsFloat(index : Integer; const v : Double);

         function GetAsInteger(index : Integer) : Int64;
         procedure SetAsInteger(index : Integer; const v : Int64);

         function GetAsBoolean(index : Integer) : Boolean;
         procedure SetAsBoolean(index : Integer; const v : Boolean);

         procedure SetAsVariant(index : Integer; const v : Variant);
         procedure EvalAsVariant(index : Integer; var result : Variant);

         procedure SetAsString(index : Integer; const v : String);
         procedure EvalAsString(index : Integer; var result : String);

         procedure SetAsInterface(index : Integer; const v : IUnknown);
         procedure EvalAsInterface(index : Integer; var result : IUnknown);

         function IsEmpty(addr : Integer) : Boolean;
         function VarType(addr : Integer) : TVarType;

         function HashCode(addr : Integer; size : Integer) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   TScriptDynamicNativeFloatArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FData : TDoubleDynArray;

      public
         procedure SetArrayLength(n : Integer);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure Insert(index : Integer);
         procedure Delete(index, count : Integer);
         procedure MoveItem(source, destination : Integer);
         procedure Swap(index1, index2 : Integer);

         function IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
         function IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
         function IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
         function IndexOfString(const item : String; fromIndex : Integer) : Integer;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         procedure WriteData(const src : TData; srcAddr, size : Integer);
         procedure ReplaceData(const v : TData);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);

         procedure Reverse;
         function  Compare(index1, index2 : Integer) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function AsPDouble(var nbElements, stride : Integer) : PDouble;

         function GetAsFloat(index : Integer) : Double;
         procedure SetAsFloat(index : Integer; const v : Double);

         function GetAsInteger(index : Integer) : Int64;
         procedure SetAsInteger(index : Integer; const v : Int64);

         function GetAsBoolean(index : Integer) : Boolean;
         procedure SetAsBoolean(index : Integer; const v : Boolean);

         procedure SetAsVariant(index : Integer; const v : Variant);
         procedure EvalAsVariant(index : Integer; var result : Variant);

         procedure SetAsString(index : Integer; const v : String);
         procedure EvalAsString(index : Integer; var result : String);

         procedure SetAsInterface(index : Integer; const v : IUnknown);
         procedure EvalAsInterface(index : Integer; var result : IUnknown);

         function IsEmpty(addr : Integer) : Boolean;
         function VarType(addr : Integer) : TVarType;

         function HashCode(addr : Integer; size : Integer) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   TScriptDynamicNativeStringArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FData : TStringDynArray;

      public
         procedure SetArrayLength(n : Integer);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure Insert(index : Integer);
         procedure Delete(index, count : Integer);
         procedure MoveItem(source, destination : Integer);
         procedure Swap(index1, index2 : Integer);

         function IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
         function IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
         function IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
         function IndexOfString(const item : String; fromIndex : Integer) : Integer;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         procedure WriteData(const src : TData; srcAddr, size : Integer);
         procedure ReplaceData(const v : TData);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);

         procedure Reverse;
         function  Compare(index1, index2 : Integer) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function AsPDouble(var nbElements, stride : Integer) : PDouble;

         function GetAsFloat(index : Integer) : Double;
         procedure SetAsFloat(index : Integer; const v : Double);

         function GetAsInteger(index : Integer) : Int64;
         procedure SetAsInteger(index : Integer; const v : Int64);

         function GetAsBoolean(index : Integer) : Boolean;
         procedure SetAsBoolean(index : Integer; const v : Boolean);

         procedure SetAsVariant(index : Integer; const v : Variant);
         procedure EvalAsVariant(index : Integer; var result : Variant);

         procedure SetAsString(index : Integer; const v : String);
         procedure EvalAsString(index : Integer; var result : String);

         procedure SetAsInterface(index : Integer; const v : IUnknown);
         procedure EvalAsInterface(index : Integer; var result : IUnknown);

         function IsEmpty(addr : Integer) : Boolean;
         function VarType(addr : Integer) : TVarType;

         function HashCode(addr : Integer; size : Integer) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   TScriptDynamicNativeBooleanArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FBits : TBits;

      public
         constructor Create(elemTyp : TTypeSymbol); override;
         destructor Destroy; override;

         procedure SetArrayLength(n : Integer);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure Insert(index : Integer);
         procedure Delete(index, count : Integer);
         procedure MoveItem(source, destination : Integer);
         procedure Swap(index1, index2 : Integer);

         function IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
         function IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
         function IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
         function IndexOfString(const item : String; fromIndex : Integer) : Integer;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         procedure WriteData(const src : TData; srcAddr, size : Integer);
         procedure ReplaceData(const v : TData);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);

         procedure Reverse;
         function  Compare(index1, index2 : Integer) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function AsPDouble(var nbElements, stride : Integer) : PDouble;

         function GetAsFloat(index : Integer) : Double;
         procedure SetAsFloat(index : Integer; const v : Double);

         function GetAsInteger(index : Integer) : Int64;
         procedure SetAsInteger(index : Integer; const v : Int64);

         function GetAsBoolean(index : Integer) : Boolean;
         procedure SetAsBoolean(index : Integer; const v : Boolean);

         procedure SetAsVariant(index : Integer; const v : Variant);
         procedure EvalAsVariant(index : Integer; var result : Variant);

         procedure SetAsString(index : Integer; const v : String);
         procedure EvalAsString(index : Integer; var result : String);

         procedure SetAsInterface(index : Integer; const v : IUnknown);
         procedure EvalAsInterface(index : Integer; var result : IUnknown);

         function IsEmpty(addr : Integer) : Boolean;
         function VarType(addr : Integer) : TVarType;

         function HashCode(addr : Integer; size : Integer) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

function CreateNewDynamicArray(elemTyp : TTypeSymbol) : IScriptDynArray;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs, dwsXXHash;

(*
// BoundsCheckFailed
//
procedure BoundsCheckFailed(exec : TdwsExecution; index : Integer);
begin
   if index<0 then
      RaiseLowerExceeded(exec, index)
   else RaiseUpperExceeded(exec, index);
end;

// BoundsCheck
//
procedure BoundsCheck(exec : TdwsExecution; aLength, index : Integer); inline;
begin
   if Cardinal(index)>=Cardinal(aLength) then
      BoundsCheckFailed(exec, index);
end;
*)

// DynamicArrayAddStrings
//
procedure DynamicArrayAddStrings(const dyn : IScriptDynArray; sl : TStrings);
var
   i, n : Integer;
begin
   n := dyn.ArrayLength;
   dyn.ArrayLength := n + sl.Count;
   for i := 0 to sl.Count-1 do
      dyn.AsString[i+n] := sl[i];
end;

// CreateNewDynamicArray
//
function CreateNewDynamicArray(elemTyp : TTypeSymbol) : IScriptDynArray;
var
   size : Integer;
   elemTypClass : TClass;
begin
   if elemTyp<>nil then
      size := elemTyp.Size
   else size := 0;
   if size = 1 then begin
      elemTypClass := elemTyp.UnAliasedType.ClassType;
      if elemTypClass = TBaseStringSymbol then
         Result := TScriptDynamicNativeStringArray.Create(elemTyp)
      else if elemTypClass = TBaseFloatSymbol then
         Result := TScriptDynamicNativeFloatArray.Create(elemTyp)
      else if elemTypClass = TBaseIntegerSymbol then
         Result := TScriptDynamicNativeIntegerArray.Create(elemTyp)
      else if elemTypClass = TBaseBooleanSymbol then
         Result := TScriptDynamicNativeBooleanArray.Create(elemTyp)
      else Result := TScriptDynamicValueArray.Create(elemTyp)
   end else Result := TScriptDynamicDataArray.Create(elemTyp);
end;

// ------------------
// ------------------ TScriptDynamicArray ------------------
// ------------------

// Create
//
constructor TScriptDynamicArray.Create(elemTyp : TTypeSymbol);
begin
   inherited Create;
   FElementTyp := elemTyp;
   if elemTyp <> nil then
      FElementSize := elemTyp.Size;
end;

// SetArrayLength
//
procedure TScriptDynamicArray.SetArrayLength(n : Integer);
var
   i : Integer;
   p : PData;
begin
   SetDataLength(n*ElementSize);
   p:=AsPData;
   for i:=FArrayLength to n-1 do
      FElementTyp.InitData(p^, i*ElementSize);
   FArrayLength:=n;
end;

// GetArrayLength
//
function TScriptDynamicArray.GetArrayLength : Integer;
begin
   Result:=FArrayLength;
end;

// SetAsVariant
//
procedure TScriptDynamicArray.SetAsVariant(index : Integer; const v : Variant);
begin
   inherited AsVariant[index] := v;
end;

// EvalAsVariant
//
procedure TScriptDynamicArray.EvalAsVariant(index : Integer; var result : Variant);
begin
   inherited EvalAsVariant(index, result);
end;

// BoundsCheckPassed
//
function TScriptDynamicArray.BoundsCheckPassed(index : Integer) : Boolean;
begin
   Result := Cardinal(index) < Cardinal(FArrayLength);
end;

// GetAsInteger
//
function TScriptDynamicArray.GetAsInteger(index : Integer) : Int64;
begin
   Result := inherited AsInteger[index];
end;

// SetAsInteger
//
procedure TScriptDynamicArray.SetAsInteger(index : Integer; const v : Int64);
begin
   inherited AsInteger[index] := v;
end;

// GetAsFloat
//
function TScriptDynamicArray.GetAsFloat(index : Integer) : Double;
begin
   Result := inherited AsFloat[index];
end;

// SetAsFloat
//
procedure TScriptDynamicArray.SetAsFloat(index : Integer; const v : Double);
begin
   inherited AsFloat[index] := v;
end;

// GetAsBoolean
//
function TScriptDynamicArray.GetAsBoolean(index : Integer) : Boolean;
begin
   Result := inherited AsBoolean[index];
end;

// SetAsBoolean
//
procedure TScriptDynamicArray.SetAsBoolean(index : Integer; const v : Boolean);
begin
   inherited AsBoolean[index] := v;
end;

// SetAsString
//
procedure TScriptDynamicArray.SetAsString(index : Integer; const v : String);
begin
   inherited AsString[index] := v;
end;

// EvalAsString
//
procedure TScriptDynamicArray.EvalAsString(index : Integer; var result : String);
begin
   inherited EvalAsString(index, result);
end;

// GetAsString
//
function TScriptDynamicArray.GetAsString(index : Integer) : String;
begin
   EvalAsString(index, Result);
end;

// SetAsInterface
//
procedure TScriptDynamicArray.SetAsInterface(index : Integer; const v : IUnknown);
begin
   inherited AsInterface[index] := v;
end;

// EvalAsInterface
//
procedure TScriptDynamicArray.EvalAsInterface(index : Integer; var result : IUnknown);
begin
   inherited EvalAsInterface(index, result);
end;

// ReplaceData
//
procedure TScriptDynamicArray.ReplaceData(const newData : TData);
begin
   inherited;
   FArrayLength:=System.Length(newData) div ElementSize;
end;

// AddStrings
//
procedure TScriptDynamicArray.AddStrings(sl : TStrings);
begin
   DynamicArrayAddStrings(Self, sl);
end;

// AsPDouble
//
function TScriptDynamicArray.AsPDouble(var nbElements, stride : Integer) : PDouble;
begin
   Assert(False);
   Result := nil;
end;

// HashCode
//
function TScriptDynamicArray.HashCode(addr : Integer; size : Integer) : Cardinal;
begin
   Result := DWSHashCode(@DirectData[addr], size);
end;

// VarType
//
function TScriptDynamicArray.VarType(addr : Integer) : TVarType;
begin
   Result := inherited VarType(addr);
end;

// IsEmpty
//
function TScriptDynamicArray.IsEmpty(addr : Integer) : Boolean;
begin
   Result := inherited IsEmpty(addr);
end;

// Insert
//
procedure TScriptDynamicArray.Insert(index : Integer);
var
   n : Integer;
   p : PData;
begin
   Inc(FArrayLength);
   SetDataLength(FArrayLength*ElementSize);
   n:=(FArrayLength-index-1)*ElementSize*SizeOf(Variant);
   p := AsPData;
   if n>0 then
      Move(p^[index*ElementSize], p^[(index+1)*ElementSize], n);
   FillChar(p^[index*ElementSize], ElementSize*SizeOf(Variant), 0);
   FElementTyp.InitData(p^, index*ElementSize);
end;

// Delete
//
procedure TScriptDynamicArray.Delete(index, count : Integer);
var
   i, d : Integer;
   p : PData;
begin
   if count<=0 then Exit;
   Dec(FArrayLength, count);
   index:=index*ElementSize;
   count:=count*ElementSize;
   for i:=index to index+count-1 do
      VarClearSafe(DirectData[i]);
   d:=(FArrayLength-1)*ElementSize+count-index;
   p := AsPData;
   if d>0 then
      System.Move(p^[index+count], p^[index], d*SizeOf(Variant));
   System.FillChar(p^[FArrayLength*ElementSize], count*SizeOf(Variant), 0);
   SetDataLength(FArrayLength*ElementSize);
end;

// Reverse
//
procedure TScriptDynamicArray.Reverse;
var
   t, b : Integer;
begin
   t:=ArrayLength-1;
   b:=0;
   while t>b do begin
      Swap(t, b);
      Dec(t);
      Inc(b);
   end;
end;

// NaturalSort
//
procedure TScriptDynamicArray.NaturalSort;
begin
   Assert(False);
end;

// IndexOfValue
//
function TScriptDynamicArray.IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1)
end;

// IndexOfInteger
//
function TScriptDynamicArray.IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfFloat
//
function TScriptDynamicArray.IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfString
//
function TScriptDynamicArray.IndexOfString(const item : String; fromIndex : Integer) : Integer;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfFuncPtr
//
function TScriptDynamicArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
var
   i : Integer;
   itemFunc : IFuncPointer;
   p : PVarData;
begin
   itemFunc := IFuncPointer(IUnknown(item));
   if itemFunc = nil then begin
      for i := fromIndex to ArrayLength-1 do begin
         p := PVarData(@DirectData[i]);
         if (p.VType=varUnknown) and (p.VUnknown=nil) then
            Exit(i);
      end;
   end else begin
      for i:=fromIndex to ArrayLength-1 do
         if itemFunc.SameFunc(DirectData[i]) then
            Exit(i);
   end;
   Result:=-1;
end;

// Copy
//
procedure TScriptDynamicArray.Copy(src : TScriptDynamicArray; index, count : Integer);
begin
   ArrayLength := count;
   WriteData(src, index*ElementSize, count*ElementSize);
end;

// Concat
//
procedure TScriptDynamicArray.Concat(const src : IScriptDynArray; index, size : Integer);
var
   n : Integer;
   srcDyn : TScriptDynamicArray;
begin
   Assert(src.GetSelf.ClassType = Self.ClassType);
   Assert(index >= 0);
   srcDyn := TScriptDynamicArray(src.GetSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := ArrayLength;
      FArrayLength := n + size;
      SetDataLength(FArrayLength*ElementSize);
      srcDyn.CopyData(index*ElementSize, DirectData, n*ElementSize, size*ElementSize);
   end;
end;

// MoveItem
//
procedure TScriptDynamicArray.MoveItem(srcIndex, dstIndex : Integer);
begin
   MoveData(srcIndex*ElementSize, dstIndex*ElementSize, ElementSize);
end;

// ToString
//
function TScriptDynamicArray.ToString : String;
begin
   Result := 'array of '+FElementTyp.Name;
end;

// ToStringArray
//
function TScriptDynamicArray.ToStringArray : TStringDynArray;
var
   i : Integer;
begin
   Assert(FElementTyp.BaseType.ClassType=TBaseStringSymbol);

   System.SetLength(Result, ArrayLength);
   for i:=0 to ArrayLength-1 do
      EvalAsString(i, Result[i]);
end;

// ToInt64Array
//
function TScriptDynamicArray.ToInt64Array : TInt64DynArray;
var
   i : Integer;
begin
   Assert(FElementTyp.BaseType.ClassType=TBaseIntegerSymbol);

   System.SetLength(Result, ArrayLength);
   for i:=0 to ArrayLength-1 do
      Result[i]:=AsInteger[i];
end;

// ToData
//
function TScriptDynamicArray.ToData : TData;
var
   i, j, p : Integer;
begin
   System.SetLength(Result, ArrayLength*ElementSize);
   p := 0;
   for i := 0 to ArrayLength-1 do begin
      for j := 0 to ElementSize-1 do begin
         EvalAsVariant(i, Result[p]);
         Inc(p);
      end;
   end;
end;

// GetElementSize
//
function TScriptDynamicArray.GetElementSize : Integer;
begin
   Result:=FElementSize;
end;

// GetElementType
//
function TScriptDynamicArray.GetElementType : TTypeSymbol;
begin
   Result := FElementTyp;
end;

// ------------------
// ------------------ TScriptDynamicValueArray ------------------
// ------------------

// ReplaceData
//
procedure TScriptDynamicValueArray.ReplaceData(const newData : TData);
var
   i, n : Integer;
begin
   n := Length(newData);
   ArrayLength := n;
   for i := 0 to n-1 do
      SetAsVariant(i, newData[i]);
end;

// Swap
//
procedure TScriptDynamicValueArray.Swap(i1, i2 : Integer);
var
   elem1, elem2 : PVarData;
   buf : TVarData;
begin
   elem1:=@DirectData[i1];
   elem2:=@DirectData[i2];
   buf.VType:=elem1^.VType;
   buf.VInt64:=elem1^.VInt64;
   elem1^.VType:=elem2^.VType;
   elem1^.VInt64:=elem2^.VInt64;
   elem2^.VType:=buf.VType;
   elem2^.VInt64:=buf.VInt64;
end;

// CompareString
//
function TScriptDynamicValueArray.CompareString(i1, i2 : Integer) : Integer;
var
   p : PVarDataArray;
   v1, v2 : PVarData;
begin
   p:=@DirectData[0];
   v1:=@p[i1];
   v2:=@p[i2];
   {$ifdef FPC}
   Assert((v1.VType=varString) and (v2.VType=varString));
   Result:=UnicodeCompareStr(String(v1.VString), String(v2.VString));
   {$else}
   Assert((v1.VType=varUString) and (v2.VType=varUString));
   Result:=UnicodeCompareStr(String(v1.VUString), String(v2.VUString));
   {$endif}
end;

// CompareInteger
//
function TScriptDynamicValueArray.CompareInteger(i1, i2 : Integer) : Integer;
var
   p : PVarDataArray;
   v1, v2 : PVarData;
begin
   p:=@DirectData[0];
   v1:=@p[i1];
   v2:=@p[i2];
   if (v1.VType=varInt64) and (v2.VType=varInt64) then begin
   end else
      Assert((v1.VType=varInt64) and (v2.VType=varInt64));
   if v1.VInt64<v2.VInt64 then
      Result:=-1
   else Result:=Ord(v1.VInt64>v2.VInt64);
end;

// CompareFloat
//
function TScriptDynamicValueArray.CompareFloat(i1, i2 : Integer) : Integer;
var
   p : PVarDataArray;
   v1, v2 : PVarData;
begin
   p:=@DirectData[0];
   v1:=@p[i1];
   v2:=@p[i2];
   Assert((v1.VType=varDouble) and (v2.VType=varDouble));
   if v1.VDouble<v2.VDouble then
      Result:=-1
   else Result:=Ord(v1.VDouble>v2.VDouble);
end;

// ------------------
// ------------------ TScriptDynamicDataArray ------------------
// ------------------

// Swap
//
procedure TScriptDynamicDataArray.Swap(i1, i2 : Integer);
var
   i : Integer;
   elem1, elem2 : PVarData;
   buf : TVarData;
begin
   elem1:=@DirectData[i1*ElementSize];
   elem2:=@DirectData[i2*ElementSize];
   for i:=1 to ElementSize do begin
      buf:=elem1^;
      elem1^:=elem2^;
      elem2^:=buf;
      Inc(elem1);
      Inc(elem2);
   end;
end;

// ------------------
// ------------------ TScriptDynamicNativeArray ------------------
// ------------------

// Create
//
constructor TScriptDynamicNativeArray.Create(elemTyp : TTypeSymbol);
begin
   inherited Create;
   FElementTyp := elemTyp;
   Assert(elemTyp.Size = 1);
end;

// ToString
//
function TScriptDynamicNativeArray.ToString : String;
begin
   Result := 'array of ' + ElementTyp.Name;
end;

// BoundsCheckPassed
//
function TScriptDynamicNativeArray.BoundsCheckPassed(index : Integer) : Boolean;
begin
   Result := Cardinal(index) < Cardinal(FArrayLength);
end;

// GetElementSize
//
function TScriptDynamicNativeArray.GetElementSize : Integer;
begin
   Result := 1;
end;

// GetElementType
//
function TScriptDynamicNativeArray.GetElementType : TTypeSymbol;
begin
   Result := FElementTyp;
end;

// GetArrayLength
//
function TScriptDynamicNativeArray.GetArrayLength : Integer;
begin
   Result := FArrayLength;
end;

// ------------------
// ------------------ TScriptDynamicNativeIntegerArray ------------------
// ------------------

// SetArrayLength
//
procedure TScriptDynamicNativeIntegerArray.SetArrayLength(n : Integer);
begin
   SetLength(FData, n);
   if n > FArrayLength then
      System.FillChar(FData[FArrayLength], (n-FArrayLength)*SizeOf(Int64), 0);
   FArrayLength := n;
end;

// ToStringArray
//
function TScriptDynamicNativeIntegerArray.ToStringArray : TStringDynArray;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := IntToStr(FData[i]);
end;

// ToInt64Array
//
function TScriptDynamicNativeIntegerArray.ToInt64Array : TInt64DynArray;
begin
   Result := Copy(FData);
end;

// ToData
//
function TScriptDynamicNativeIntegerArray.ToData : TData;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      VarCopySafe(Result[i], FData[i]);
end;

// Insert
//
procedure TScriptDynamicNativeIntegerArray.Insert(index : Integer);
begin
   System.Insert(0, FData, index);
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeIntegerArray.Delete(index, count : Integer);
begin
   System.Delete(FData, index, count);
   Dec(FArrayLength);
end;

// MoveItem
//
procedure TScriptDynamicNativeIntegerArray.MoveItem(source, destination : Integer);
var
   buf : Int64;
begin
   if source = destination then Exit;

   buf := FData[source];
   if source < destination then
      System.Move(FData[source+1], FData[source], SizeOf(Int64)*(destination-source))
   else System.Move(FData[destination], FData[destination+1], SizeOf(Int64)*(source-destination));
   FData[destination] := buf;
end;

// Swap
//
procedure TScriptDynamicNativeIntegerArray.Swap(index1, index2 : Integer);
var
   buf : Int64;
begin
   buf := FData[index1];
   FData[index1] := FData[index2];
   FData[index2] := buf;
end;

// IndexOfValue
//
function TScriptDynamicNativeIntegerArray.IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := IndexOfInteger(VariantToInt64(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeIntegerArray.IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
var
   i : Integer;
begin
   if fromIndex < 0 then
      fromIndex := 0;
   for i := fromIndex to FArrayLength-1 do begin
      if FData[i] = item then
         Exit(i);
   end;
   Result := -1;
end;

// IndexOfFloat
//
function TScriptDynamicNativeIntegerArray.IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
var
   i : Int64;
begin
   i := Round(item);
   if i = item then
      Result := IndexOfInteger(i, fromIndex)
   else Result := -1;
end;

// IndexOfString
//
function TScriptDynamicNativeIntegerArray.IndexOfString(const item : String; fromIndex : Integer) : Integer;
begin
   Result := IndexOfInteger(StrToInt64(item), fromIndex);
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeIntegerArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeIntegerArray.WriteData(const src : TData; srcAddr, size : Integer);
var
   i : Integer;
begin
   for i := 0 to size-1 do
      VariantToInt64(src[i + srcAddr], FData[i]);
end;

// ReplaceData
//
procedure TScriptDynamicNativeIntegerArray.ReplaceData(const v : TData);
begin
   FArrayLength := Length(v);
   SetLength(FData, FArrayLength);
   WriteData(v, 0, FArrayLength);
end;

// Concat
//
procedure TScriptDynamicNativeIntegerArray.Concat(const src : IScriptDynArray; index, size : Integer);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeIntegerArray;
   n : Integer;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeIntegerArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeIntegerArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      FArrayLength := n + size;
      SetLength(FData, FArrayLength);
      System.Move(srcDyn.FData[index], FData[n], size*SizeOf(Int64));
   end;
end;

// Reverse
//
procedure TScriptDynamicNativeIntegerArray.Reverse;
var
   pLow, pHigh : PInt64;
   t : Int64;
begin
   if FArrayLength <= 1 then Exit;

   pLow := @FData[0];
   pHigh := @FData[FArrayLength-1];
   while NativeUInt(pHigh) > NativeUInt(pLow) do begin
      t := pLow^;
      pLow^ := pHigh^;
      pHigh^ := t;
      Inc(pLow);
      Dec(pHigh);
   end;
end;

// Compare
//
function TScriptDynamicNativeIntegerArray.Compare(index1, index2 : Integer) : Integer;
var
   n1, n2 : Int64;
begin
   n1 := FData[index1];
   n2 := FData[index2];
   if n1 < n2 then
      Result := -1
   else Result := Ord(n1 > n2);
end;

// NaturalSort
//
procedure TScriptDynamicNativeIntegerArray.NaturalSort;
var
   qs : TQuickSort;
begin
   qs.CompareMethod := Self.Compare;
   qs.SwapMethod := Self.Swap;
   qs.Sort(0, FArrayLength-1);
end;

// AddStrings
//
procedure TScriptDynamicNativeIntegerArray.AddStrings(sl : TStrings);
begin
   DynamicArrayAddStrings(Self, sl);
end;

// AsPDouble
//
function TScriptDynamicNativeIntegerArray.AsPDouble(var nbElements, stride : Integer) : PDouble;
begin
   Assert(False);
   Result := nil;
end;

// GetAsFloat
//
function TScriptDynamicNativeIntegerArray.GetAsFloat(index : Integer) : Double;
begin
   Result := FData[index];
end;

// SetAsFloat
//
procedure TScriptDynamicNativeIntegerArray.SetAsFloat(index : Integer; const v : Double);
begin
   FData[index] := Round(v);
end;

// GetAsInteger
//
function TScriptDynamicNativeIntegerArray.GetAsInteger(index : Integer) : Int64;
begin
   Result := FData[index];
end;

// SetAsInteger
//
procedure TScriptDynamicNativeIntegerArray.SetAsInteger(index : Integer; const v : Int64);
begin
   FData[index] := v;
end;

// GetAsBoolean
//
function TScriptDynamicNativeIntegerArray.GetAsBoolean(index : Integer) : Boolean;
begin
   Result := FData[index] <> 0;
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeIntegerArray.SetAsBoolean(index : Integer; const v : Boolean);
begin
   FData[index] := Ord(v);
end;

// SetAsVariant
//
procedure TScriptDynamicNativeIntegerArray.SetAsVariant(index : Integer; const v : Variant);
begin
   FData[index] := VariantToInt64(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeIntegerArray.EvalAsVariant(index : Integer; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeIntegerArray.SetAsString(index : Integer; const v : String);
begin
   FData[index] := StrToInt64(v);
end;

// EvalAsString
//
procedure TScriptDynamicNativeIntegerArray.EvalAsString(index : Integer; var result : String);
begin
   result := IntToStr(FData[index]);
end;

// SetAsInterface
//
procedure TScriptDynamicNativeIntegerArray.SetAsInterface(index : Integer; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeIntegerArray.EvalAsInterface(index : Integer; var result : IUnknown);
begin
   Assert(False);
end;

// IsEmpty
//
function TScriptDynamicNativeIntegerArray.IsEmpty(addr : Integer) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeIntegerArray.VarType(addr : Integer) : TVarType;
begin
   Result := vtInt64;
end;

// HashCode
//
function TScriptDynamicNativeIntegerArray.HashCode(addr : Integer; size : Integer) : Cardinal;
var
   i : Integer;
begin
   Result := cFNV_basis;
   for i := 0 to FArrayLength-1 do
      Result := (Result xor SimpleInt64Hash(FData[i])) * cFNV_prime;
   if Result = 0 then
      Result := cFNV_basis;
end;

// WriteToJSON
//
procedure TScriptDynamicNativeIntegerArray.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteInteger(FData[i]);
   writer.EndArray;
end;

// ------------------
// ------------------ TScriptDynamicNativeFloatArray ------------------
// ------------------

// SetArrayLength
//
procedure TScriptDynamicNativeFloatArray.SetArrayLength(n : Integer);
begin
   SetLength(FData, n);
   if n > FArrayLength then
      System.FillChar(FData[FArrayLength], (n-FArrayLength)*SizeOf(Double), 0);
   FArrayLength := n;
end;

// ToStringArray
//
function TScriptDynamicNativeFloatArray.ToStringArray : TStringDynArray;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      FastFloatToStr(FData[i], Result[i], FormatSettings);
end;

// ToInt64Array
//
function TScriptDynamicNativeFloatArray.ToInt64Array : TInt64DynArray;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := Round(FData[i]);
end;

// ToData
//
function TScriptDynamicNativeFloatArray.ToData : TData;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      VarCopySafe(Result[i], FData[i]);
end;

// Insert
//
procedure TScriptDynamicNativeFloatArray.Insert(index : Integer);
begin
   System.Insert(0, FData, index);
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeFloatArray.Delete(index, count : Integer);
begin
   System.Delete(FData, index, count);
   Dec(FArrayLength);
end;

// MoveItem
//
procedure TScriptDynamicNativeFloatArray.MoveItem(source, destination : Integer);
var
   buf : Double;
begin
   if source = destination then Exit;

   buf := FData[source];
   if source < destination then
      System.Move(FData[source+1], FData[source], SizeOf(Double)*(destination-source))
   else System.Move(FData[destination], FData[destination+1], SizeOf(Double)*(source-destination));
   FData[destination] := buf;
end;

// Swap
//
procedure TScriptDynamicNativeFloatArray.Swap(index1, index2 : Integer);
var
   buf : Double;
begin
   buf := FData[index1];
   FData[index1] := FData[index2];
   FData[index2] := buf;
end;

// IndexOfValue
//
function TScriptDynamicNativeFloatArray.IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := IndexOfFloat(VariantToInt64(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeFloatArray.IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
begin
   Result := IndexOfFloat(item, fromIndex);
end;

// IndexOfFloat
//
function TScriptDynamicNativeFloatArray.IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
var
   i : Integer;
begin
   if fromIndex < 0 then
      fromIndex := 0;
   for i := fromIndex to FArrayLength-1 do begin
      if FData[i] = item then
         Exit(i);
   end;
   Result := -1;
end;

// IndexOfString
//
function TScriptDynamicNativeFloatArray.IndexOfString(const item : String; fromIndex : Integer) : Integer;
begin
   Result := IndexOfFloat(StrToFloat(item), fromIndex);
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeFloatArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeFloatArray.WriteData(const src : TData; srcAddr, size : Integer);
var
   i : Integer;
begin
   for i := 0 to size-1 do
      FData[i] := VariantToFloat(src[i + srcAddr]);
end;

// ReplaceData
//
procedure TScriptDynamicNativeFloatArray.ReplaceData(const v : TData);
begin
   FArrayLength := Length(v);
   SetLength(FData, FArrayLength);
   WriteData(v, 0, FArrayLength);
end;

// Concat
//
procedure TScriptDynamicNativeFloatArray.Concat(const src : IScriptDynArray; index, size : Integer);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeFloatArray;
   n : Integer;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeFloatArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeFloatArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      FArrayLength := n + size;
      SetLength(FData, FArrayLength);
      System.Move(srcDyn.FData[index], FData[n], size*SizeOf(Double));
   end;
end;

// Reverse
//
procedure TScriptDynamicNativeFloatArray.Reverse;
var
   pLow, pHigh : PDouble;
   t : Double;
begin
   if FArrayLength <= 1 then Exit;

   pLow := @FData[0];
   pHigh := @FData[FArrayLength-1];
   while NativeUInt(pHigh) > NativeUInt(pLow) do begin
      t := pLow^;
      pLow^ := pHigh^;
      pHigh^ := t;
      Inc(pLow);
      Dec(pHigh);
   end;
end;

// Compare
//
function TScriptDynamicNativeFloatArray.Compare(index1, index2 : Integer) : Integer;
var
   n1, n2 : Double;
begin
   n1 := FData[index1];
   n2 := FData[index2];
   if n1 < n2 then
      Result := -1
   else Result := Ord(n1 > n2);
end;

// NaturalSort
//
procedure TScriptDynamicNativeFloatArray.NaturalSort;
var
   qs : TQuickSort;
begin
   qs.CompareMethod := Self.Compare;
   qs.SwapMethod := Self.Swap;
   qs.Sort(0, FArrayLength-1);
end;

// AddStrings
//
procedure TScriptDynamicNativeFloatArray.AddStrings(sl : TStrings);
begin
   DynamicArrayAddStrings(Self, sl);
end;

// AsPDouble
//
function TScriptDynamicNativeFloatArray.AsPDouble(var nbElements, stride : Integer) : PDouble;
begin
   Result := Pointer(FData);
   nbElements := FArrayLength;
   stride := SizeOf(Double);
end;

// GetAsFloat
//
function TScriptDynamicNativeFloatArray.GetAsFloat(index : Integer) : Double;
begin
   Result := FData[index];
end;

// SetAsFloat
//
procedure TScriptDynamicNativeFloatArray.SetAsFloat(index : Integer; const v : Double);
begin
   FData[index] := v;
end;

// GetAsInteger
//
function TScriptDynamicNativeFloatArray.GetAsInteger(index : Integer) : Int64;
begin
   Result := Round(FData[index]);
end;

// SetAsInteger
//
procedure TScriptDynamicNativeFloatArray.SetAsInteger(index : Integer; const v : Int64);
begin
   FData[index] := v;
end;

// GetAsBoolean
//
function TScriptDynamicNativeFloatArray.GetAsBoolean(index : Integer) : Boolean;
begin
   Result := FData[index] <> 0;
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeFloatArray.SetAsBoolean(index : Integer; const v : Boolean);
begin
   FData[index] := Ord(v);
end;

// SetAsVariant
//
procedure TScriptDynamicNativeFloatArray.SetAsVariant(index : Integer; const v : Variant);
begin
   FData[index] := VariantToFloat(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeFloatArray.EvalAsVariant(index : Integer; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeFloatArray.SetAsString(index : Integer; const v : String);
begin
   FData[index] := StrToFloat(v);
end;

// EvalAsString
//
procedure TScriptDynamicNativeFloatArray.EvalAsString(index : Integer; var result : String);
begin
   FastFloatToStr(FData[index], result, FormatSettings);
end;

// SetAsInterface
//
procedure TScriptDynamicNativeFloatArray.SetAsInterface(index : Integer; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeFloatArray.EvalAsInterface(index : Integer; var result : IUnknown);
begin
   Assert(False);
end;

// IsEmpty
//
function TScriptDynamicNativeFloatArray.IsEmpty(addr : Integer) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeFloatArray.VarType(addr : Integer) : TVarType;
begin
   Result := varDouble;
end;

// HashCode
//
function TScriptDynamicNativeFloatArray.HashCode(addr : Integer; size : Integer) : Cardinal;
var
   i : Integer;
begin
   Result := cFNV_basis;
   for i := 0 to FArrayLength-1 do
      Result := (Result xor SimpleInt64Hash(PInt64(@FData[i])^)) * cFNV_prime;
   if Result = 0 then
      Result := cFNV_basis;
end;

// WriteToJSON
//
procedure TScriptDynamicNativeFloatArray.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteNumber(FData[i]);
   writer.EndArray;
end;

// ------------------
// ------------------ TScriptDynamicNativeStringArray ------------------
// ------------------

// SetArrayLength
//
procedure TScriptDynamicNativeStringArray.SetArrayLength(n : Integer);
begin
   SetLength(FData, n);
   FArrayLength := n;
end;

// ToStringArray
//
function TScriptDynamicNativeStringArray.ToStringArray : TStringDynArray;
begin
   Result := Copy(FData);
end;

// ToInt64Array
//
function TScriptDynamicNativeStringArray.ToInt64Array : TInt64DynArray;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := StrToInt64(FData[i]);
end;

// ToData
//
function TScriptDynamicNativeStringArray.ToData : TData;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      VarCopySafe(Result[i], FData[i]);
end;

// Insert
//
procedure TScriptDynamicNativeStringArray.Insert(index : Integer);
begin
   System.Insert('', FData, index);
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeStringArray.Delete(index, count : Integer);
begin
   System.Delete(FData, index, count);
   Dec(FArrayLength);
end;

// MoveItem
//
procedure TScriptDynamicNativeStringArray.MoveItem(source, destination : Integer);
var
   buf : Pointer;
begin
   if source = destination then Exit;

   buf := PPointer(@FData[source])^;
   if source < destination then
      System.Move(FData[source+1], FData[source], SizeOf(Pointer)*(destination-source))
   else System.Move(FData[destination], FData[destination+1], SizeOf(Pointer)*(source-destination));
   PPointer(@FData[destination])^ := buf;
end;

// Swap
//
procedure TScriptDynamicNativeStringArray.Swap(index1, index2 : Integer);
var
   buf : Pointer;
begin
   buf := PPointer(@FData[index1])^;
   PPointer(@FData[index1])^ := PPointer(@FData[index2])^;
   PPointer(@FData[index2])^ := buf;
end;

// IndexOfValue
//
function TScriptDynamicNativeStringArray.IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := IndexOfString(VariantToString(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeStringArray.IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
begin
   Result := IndexOfString(IntToStr(item), fromIndex);
end;

// IndexOfFloat
//
function TScriptDynamicNativeStringArray.IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
begin
   Result := IndexOfString(FloatToStr(item), fromIndex);
end;

// IndexOfString
//
function TScriptDynamicNativeStringArray.IndexOfString(const item : String; fromIndex : Integer) : Integer;
var
   i : Integer;
begin
   if fromIndex < 0 then
      fromIndex := 0;
   for i := fromIndex to FArrayLength-1 do begin
      if FData[i] = item then
         Exit(i);
   end;
   Result := -1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeStringArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeStringArray.WriteData(const src : TData; srcAddr, size : Integer);
var
   i : Integer;
begin
   for i := 0 to size-1 do
      FData[i] := VariantToString(src[i + srcAddr]);
end;

// ReplaceData
//
procedure TScriptDynamicNativeStringArray.ReplaceData(const v : TData);
begin
   FArrayLength := Length(v);
   SetLength(FData, FArrayLength);
   WriteData(v, 0, FArrayLength);
end;

// Concat
//
procedure TScriptDynamicNativeStringArray.Concat(const src : IScriptDynArray; index, size : Integer);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeStringArray;
   n, i : Integer;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeStringArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeStringArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      FArrayLength := n + size;
      SetLength(FData, FArrayLength);
      for i := 0 to size-1 do
         FData[n + i] := srcDyn.FData[index + i];
   end;
end;

// Reverse
//
procedure TScriptDynamicNativeStringArray.Reverse;
var
   pLow, pHigh : PPointer;
   t : Pointer;
begin
   if FArrayLength <= 1 then Exit;

   pLow := @FData[0];
   pHigh := @FData[FArrayLength-1];
   while NativeUInt(pHigh) > NativeUInt(pLow) do begin
      t := pLow^;
      pLow^ := pHigh^;
      pHigh^ := t;
      Inc(pLow);
      Dec(pHigh);
   end;
end;

// Compare
//
function TScriptDynamicNativeStringArray.Compare(index1, index2 : Integer) : Integer;
begin
   Result := CompareStr(FData[index1], FData[index2]);
end;

// NaturalSort
//
procedure TScriptDynamicNativeStringArray.NaturalSort;
var
   qs : TQuickSort;
begin
   qs.CompareMethod := Self.Compare;
   qs.SwapMethod := Self.Swap;
   qs.Sort(0, FArrayLength-1);
end;

// AddStrings
//
procedure TScriptDynamicNativeStringArray.AddStrings(sl : TStrings);
var
   i, n : Integer;
begin
   n := FArrayLength;
   FArrayLength := n + sl.Count;
   SetLength(FData, FArrayLength);
   for i := 0 to sl.Count-1 do
      FData[i+n] := sl[i];
end;

// AsPDouble
//
function TScriptDynamicNativeStringArray.AsPDouble(var nbElements, stride : Integer) : PDouble;
begin
   Assert(False);
   Result := nil;
end;

// GetAsFloat
//
function TScriptDynamicNativeStringArray.GetAsFloat(index : Integer) : Double;
begin
   Result := StrToFloat(FData[index]);
end;

// SetAsFloat
//
procedure TScriptDynamicNativeStringArray.SetAsFloat(index : Integer; const v : Double);
begin
   FData[index] := FloatToStr(v);
end;

// GetAsInteger
//
function TScriptDynamicNativeStringArray.GetAsInteger(index : Integer) : Int64;
begin
   Result := StrToInt64(FData[index]);
end;

// SetAsInteger
//
procedure TScriptDynamicNativeStringArray.SetAsInteger(index : Integer; const v : Int64);
begin
   FData[index] := IntToStr(v);
end;

// GetAsBoolean
//
function TScriptDynamicNativeStringArray.GetAsBoolean(index : Integer) : Boolean;
begin
   Result := StringToBoolean(FData[index]);
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeStringArray.SetAsBoolean(index : Integer; const v : Boolean);
begin
   if v then
      FData[index] := '1'
   else FData[index] := '0';
end;

// SetAsVariant
//
procedure TScriptDynamicNativeStringArray.SetAsVariant(index : Integer; const v : Variant);
begin
   FData[index] := VariantToString(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeStringArray.EvalAsVariant(index : Integer; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeStringArray.SetAsString(index : Integer; const v : String);
begin
   FData[index] := v;
end;

// EvalAsString
//
procedure TScriptDynamicNativeStringArray.EvalAsString(index : Integer; var result : String);
begin
   result := FData[index];
end;

// SetAsInterface
//
procedure TScriptDynamicNativeStringArray.SetAsInterface(index : Integer; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeStringArray.EvalAsInterface(index : Integer; var result : IUnknown);
begin
   Assert(False);
end;

// IsEmpty
//
function TScriptDynamicNativeStringArray.IsEmpty(addr : Integer) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeStringArray.VarType(addr : Integer) : TVarType;
begin
   Result := varUString;
end;

// HashCode
//
function TScriptDynamicNativeStringArray.HashCode(addr : Integer; size : Integer) : Cardinal;
var
   i : Integer;
begin
   Result := cFNV_basis;
   for i := 0 to FArrayLength-1 do
      Result := (Result xor SimpleStringHash(FData[i])) * cFNV_prime;
   if Result = 0 then
      Result := cFNV_basis;
end;

// WriteToJSON
//
procedure TScriptDynamicNativeStringArray.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteString(FData[i]);
   writer.EndArray;
end;

// ------------------
// ------------------ TScriptDynamicNativeBooleanArray ------------------
// ------------------

// Create
//
constructor TScriptDynamicNativeBooleanArray.Create(elemTyp : TTypeSymbol);
begin
   inherited Create(elemTyp);
   FBits := TBits.Create;
end;

// Destroy
//
destructor TScriptDynamicNativeBooleanArray.Destroy;
begin
   inherited;
   FBits.Free;
end;

// SetArrayLength
//
procedure TScriptDynamicNativeBooleanArray.SetArrayLength(n : Integer);
begin
   FBits.Size := n;
   FArrayLength := n;
end;

// ToStringArray
//
function TScriptDynamicNativeBooleanArray.ToStringArray : TStringDynArray;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      if FBits[i] then
         Result[i] := 'True'
      else Result[i] := 'False';
end;

// ToInt64Array
//
function TScriptDynamicNativeBooleanArray.ToInt64Array : TInt64DynArray;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := Ord(FBits[i]);
end;

// ToData
//
function TScriptDynamicNativeBooleanArray.ToData : TData;
var
   i : Integer;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      VarCopySafe(Result[i], FBits[i]);
end;

// Insert
//
procedure TScriptDynamicNativeBooleanArray.Insert(index : Integer);
var
   i : Integer;
begin
   SetArrayLength(FArrayLength + 1);
   for i := FArrayLength-1 downto index+1 do
      FBits[i] := FBits[i-1];
   FBits[index] := False;
end;

// Delete
//
procedure TScriptDynamicNativeBooleanArray.Delete(index, count : Integer);
var
   i : Integer;
begin
   for i := index to FArrayLength-count-1 do
      FBits[i] := FBits[i+count];
   SetArrayLength(FArrayLength - count);
end;

// MoveItem
//
procedure TScriptDynamicNativeBooleanArray.MoveItem(source, destination : Integer);
var
   buf : Boolean;
   i : Integer;
begin
   if source = destination then Exit;

   buf := FBits[source];

   if source < destination then begin
      for i := source to destination-1 do
         FBits[i] := FBits[i+1];
   end else begin
      for i := source downto destination+1 do
         FBits[i] := FBits[i-1];
   end;
   FBits[destination] := buf;
end;

// Swap
//
procedure TScriptDynamicNativeBooleanArray.Swap(index1, index2 : Integer);
var
   buf : Boolean;
begin
   buf := FBits[index1];
   FBits[index1] := FBits[index2];
   FBits[index2] := buf;
end;

// IndexOfValue
//
function TScriptDynamicNativeBooleanArray.IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := IndexOfInteger(VariantToInt64(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeBooleanArray.IndexOfInteger(item : Int64; fromIndex : Integer) : Integer;
var
   i : Integer;
   v : Boolean;
begin
   v := (item <> 0);
   for i := fromIndex to FArrayLength-1 do
      if FBits[i] = v then
         Exit(i);
   Result := -1;
end;

// IndexOfFloat
//
function TScriptDynamicNativeBooleanArray.IndexOfFloat(item : Double; fromIndex : Integer) : Integer;
begin
   Result := IndexOfInteger(Ord(item <> 0), fromIndex);
end;

// IndexOfString
//
function TScriptDynamicNativeBooleanArray.IndexOfString(const item : String; fromIndex : Integer) : Integer;
begin
   Result := IndexOfInteger(Ord(StringToBoolean(item)), fromIndex);
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeBooleanArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeBooleanArray.WriteData(const src : TData; srcAddr, size : Integer);
var
   i : Integer;
begin
   for i := 0 to size-1 do
      FBits[i] := VariantToBool(src[i + srcAddr]);
end;

// ReplaceData
//
procedure TScriptDynamicNativeBooleanArray.ReplaceData(const v : TData);
begin
   SetArrayLength(Length(v));
   WriteData(v, 0, FArrayLength);
end;

// Concat
//
procedure TScriptDynamicNativeBooleanArray.Concat(const src : IScriptDynArray; index, size : Integer);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeBooleanArray;
   i, n : Integer;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeBooleanArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeBooleanArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      SetArrayLength(n + size);
      for i := 0 to size-1 do
         FBits[n + i] := srcDyn.FBits[index + i];
   end;
end;

// Reverse
//
procedure TScriptDynamicNativeBooleanArray.Reverse;
var
   i, j : Integer;
   buf : Boolean;
begin
   i := 0;
   j := FArrayLength-1;
   while i < j do begin
      buf := FBits[i];
      FBits[i] := FBits[j];
      FBits[j] := buf;
      Inc(i);
      Dec(j);
   end;
end;

// Compare
//
function TScriptDynamicNativeBooleanArray.Compare(index1, index2 : Integer) : Integer;
begin
   Result := Ord(FBits[index1]) - Ord(FBits[index2]);
end;

// NaturalSort
//
procedure TScriptDynamicNativeBooleanArray.NaturalSort;
var
   i, j : Integer;
begin
   j := FArrayLength;
   for i := 0 to FArrayLength-1 do begin
      if FBits[i] then begin
         Dec(j);
         if i < j then
            FBits[i] := False;
      end;
   end;
   for i := j to FArrayLength-1 do
      FBits[i] := True;
end;

// AddStrings
//
procedure TScriptDynamicNativeBooleanArray.AddStrings(sl : TStrings);
begin
   DynamicArrayAddStrings(Self, sl);
end;

// AsPDouble
//
function TScriptDynamicNativeBooleanArray.AsPDouble(var nbElements, stride : Integer) : PDouble;
begin
   Assert(False);
   Result := nil;
end;

// GetAsFloat
//
function TScriptDynamicNativeBooleanArray.GetAsFloat(index : Integer) : Double;
begin
   Result := Ord(FBits[index]);
end;

// SetAsFloat
//
procedure TScriptDynamicNativeBooleanArray.SetAsFloat(index : Integer; const v : Double);
begin
   FBits[index] := (v <> 0);
end;

// GetAsInteger
//
function TScriptDynamicNativeBooleanArray.GetAsInteger(index : Integer) : Int64;
begin
   Result := Ord(FBits[index]);
end;

// SetAsInteger
//
procedure TScriptDynamicNativeBooleanArray.SetAsInteger(index : Integer; const v : Int64);
begin
   FBits[index] := (v <> 0);
end;

// GetAsBoolean
//
function TScriptDynamicNativeBooleanArray.GetAsBoolean(index : Integer) : Boolean;
begin
   Result := FBits[index];
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeBooleanArray.SetAsBoolean(index : Integer; const v : Boolean);
begin
   FBits[index] := v;
end;

// SetAsVariant
//
procedure TScriptDynamicNativeBooleanArray.SetAsVariant(index : Integer; const v : Variant);
begin
   FBits[index] := VariantToBool(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeBooleanArray.EvalAsVariant(index : Integer; var result : Variant);
begin
   VarCopySafe(result, FBits[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeBooleanArray.SetAsString(index : Integer; const v : String);
begin
   FBits[index] := StringToBoolean(v);
end;

// EvalAsString
//
procedure TScriptDynamicNativeBooleanArray.EvalAsString(index : Integer; var result : String);
begin
   if FBits[index] then
      result := 'True'
   else result := 'False';
end;

// SetAsInterface
//
procedure TScriptDynamicNativeBooleanArray.SetAsInterface(index : Integer; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeBooleanArray.EvalAsInterface(index : Integer; var result : IUnknown);
begin
   Assert(False);
end;

// IsEmpty
//
function TScriptDynamicNativeBooleanArray.IsEmpty(addr : Integer) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeBooleanArray.VarType(addr : Integer) : TVarType;
begin
   Result := varBoolean;
end;

// HashCode
//
function TScriptDynamicNativeBooleanArray.HashCode(addr : Integer; size : Integer) : Cardinal;
var
   i : Integer;
begin
   Result := cFNV_basis;
   for i := 0 to FArrayLength-1 do
      Result := (Result xor SimpleIntegerHash(Ord(FBits[i]))) * cFNV_prime;
   if Result = 0 then
      Result := cFNV_basis;
end;

// WriteToJSON
//
procedure TScriptDynamicNativeBooleanArray.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteBoolean(FBits[i]);
   writer.EndArray;
end;

end.
