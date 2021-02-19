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

         procedure Copy(src : TScriptDynamicArray; index, count : Integer);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);
         procedure MoveItem(srcIndex, dstIndex : Integer);

         function ToString : String; override;
         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure ReplaceData(const newData : TData); override;

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

         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

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
         constructor Create(elemTyp : TTypeSymbol);

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

         procedure WriteData(const src : TData; srcAddr, size : Integer);
         procedure ReplaceData(const v : TData);
         procedure Concat(const src : IScriptDynArray; index, size : Integer);

         procedure Reverse;
         function  Compare(index1, index2 : Integer) : Integer;
         procedure NaturalSort;

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

   TScriptDynamicStringArray = class (TScriptDynamicValueArray)
      public
         procedure Add(const s : String);
         procedure AddStrings(sl : TStrings);
         function VarType(addr : Integer) : TVarType; override;
         procedure NaturalSort; override;
   end;

   TScriptDynamicIntegerArray = class (TScriptDynamicValueArray)
      public
         procedure NaturalSort; override;
   end;

   TScriptDynamicFloatArray = class (TScriptDynamicValueArray)
      public
         function AsPDouble(var nbElements, stride : Integer) : PDouble; override;
         function VarType(addr : Integer) : TVarType; override;
         procedure NaturalSort; override;
   end;

   TScriptDynamicBooleanArray = class (TScriptDynamicValueArray)
      public
   end;

function CreateNewDynamicArray(elemTyp : TTypeSymbol) : IScriptDynArray;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs;

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
         Result := TScriptDynamicStringArray.Create(elemTyp)
      else if elemTypClass = TBaseFloatSymbol then
         Result := TScriptDynamicFloatArray.Create(elemTyp)
      else if elemTypClass = TBaseIntegerSymbol then
         Result := TScriptDynamicNativeIntegerArray.Create(elemTyp)
//         Result := TScriptDynamicIntegerArray.Create(elemTyp)
      else if elemTypClass = TBaseBooleanSymbol then
         Result := TScriptDynamicBooleanArray.Create(elemTyp)
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

// IndexOfFuncPtr
//
function TScriptDynamicValueArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
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
// ------------------ TScriptDynamicStringArray ------------------
// ------------------

// Add
//
procedure TScriptDynamicStringArray.Add(const s : String);
begin
   ArrayLength:=ArrayLength+1;
   if s<>'' then
      AsString[ArrayLength-1]:=s;
end;

// AddStrings
//
procedure TScriptDynamicStringArray.AddStrings(sl : TStrings);
var
   i, n : Integer;
begin
   n := ArrayLength;
   ArrayLength := n+sl.Count;
   for i := 0 to sl.Count-1 do
      AsString[n+i] := sl[i];
end;

// VarType
//
function TScriptDynamicStringArray.VarType(addr : Integer) : TVarType;
begin
   Result := varUString;
end;

// NaturalSort
//
procedure TScriptDynamicStringArray.NaturalSort;
var
   qs : TQuickSort;
begin
   qs.CompareMethod := Self.CompareString;
   qs.SwapMethod := Self.Swap;
   qs.Sort(0, FArrayLength-1);
end;

// ------------------
// ------------------ TScriptDynamicIntegerArray ------------------
// ------------------

// NaturalSort
//
procedure TScriptDynamicIntegerArray.NaturalSort;
var
   qs : TQuickSort;
begin
   qs.CompareMethod := Self.CompareInteger;
   qs.SwapMethod := Self.Swap;
   qs.Sort(0, FArrayLength-1);
end;

// ------------------
// ------------------ TScriptDynamicFloatArray ------------------
// ------------------

// AsPDouble
//
function TScriptDynamicFloatArray.AsPDouble(var nbElements, stride : Integer) : PDouble;
begin
   nbElements := ArrayLength;
   if nbElements = 0 then Exit(nil);

   stride := SizeOf(Variant);
   Result := @TVarData(AsPData^[0]).VDouble;
end;

// VarType
//
function TScriptDynamicFloatArray.VarType(addr : Integer) : TVarType;
begin
   Result := varDouble;
end;

// NaturalSort
//
procedure TScriptDynamicFloatArray.NaturalSort;
var
   qs : TQuickSort;
begin
   qs.CompareMethod := Self.CompareFloat;
   qs.SwapMethod := Self.Swap;
   qs.Sort(0, FArrayLength-1);
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
      Inc(p);
   end;
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

   srcDyn := TScriptDynamicNativeIntegerArray(src.GetSelf);
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

end.
