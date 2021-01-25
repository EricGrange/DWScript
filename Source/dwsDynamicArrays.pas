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
   dwsSymbols, dwsUtils, dwsDataContext;

type

   TScriptDynamicArray = class abstract (TDataContext, IScriptDynArray)
      private
         FElementTyp : TTypeSymbol;
         FElementSize : Integer;
         FArrayLength : Integer;

      protected
         function GetElementSize : Integer;
         function GetElementType : TTypeSymbol;
         procedure SetArrayLength(n : Integer);
         function GetArrayLength : Integer;

      public
         class function CreateNew(elemTyp : TTypeSymbol) : TScriptDynamicArray; static;

         procedure Delete(index, count : Integer);
         procedure Insert(index : Integer);
         procedure Swap(i1, i2 : Integer); virtual; abstract;
         procedure Reverse;
         procedure Copy(src : TScriptDynamicArray; index, count : Integer);
         procedure Concat(src : TScriptDynamicArray);
         procedure MoveItem(srcIndex, dstIndex : Integer);

         function ToString : String; override;
         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;
         function ToData : TData;

         procedure ReplaceData(const newData : TData); override;

         function AsPDouble(var nbElements, stride : Integer) : PDouble; virtual;

         function HashCode(addr : Integer; size : Integer) : Cardinal; virtual;

         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ElementSize : Integer read FElementSize;
         property ArrayLength : Integer read FArrayLength write SetArrayLength;
   end;

   TScriptDynamicDataArray = class (TScriptDynamicArray)
      public
         procedure Swap(i1, i2 : Integer); override;
   end;

   TScriptDynamicValueArray = class (TScriptDynamicArray)
      public
         procedure Swap(i1, i2 : Integer); override;

         function CompareString(i1, i2 : Integer) : Integer;
         function CompareInteger(i1, i2 : Integer) : Integer;
         function CompareFloat(i1, i2 : Integer) : Integer;
   end;

   TScriptDynamicStringArray = class (TScriptDynamicValueArray)
      public
         procedure Add(const s : String);
         procedure AddStrings(sl : TStrings);
         function VarType(addr : Integer) : TVarType; override;
   end;

   TScriptDynamicFloatArray = class (TScriptDynamicValueArray)
      public
         function AsPDouble(var nbElements, stride : Integer) : PDouble; override;
         function VarType(addr : Integer) : TVarType; override;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs;

// ------------------
// ------------------ TScriptDynamicArray ------------------
// ------------------

// CreateNew
//
class function TScriptDynamicArray.CreateNew(elemTyp : TTypeSymbol) : TScriptDynamicArray;
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
         Result := TScriptDynamicStringArray.Create
      else if elemTypClass = TBaseFloatSymbol then
         Result := TScriptDynamicFloatArray.Create
      else Result := TScriptDynamicValueArray.Create
   end else Result := TScriptDynamicDataArray.Create;
   Result.FElementTyp := elemTyp;
   Result.FElementSize := size;
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
   Result := nil;
end;

// HashCode
//
function TScriptDynamicArray.HashCode(addr : Integer; size : Integer) : Cardinal;
begin
   Result := DWSHashCode(@DirectData[addr], size);
end;

// IndexOfFuncPtr
//
function TScriptDynamicArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
var
   i : Integer;
   itemFunc : IFuncPointer;
   p : PVarData;
begin
   itemFunc:=IFuncPointer(IUnknown(item));
   if itemFunc=nil then begin
      for i:=fromIndex to ArrayLength-1 do begin
         p:=PVarData(AsPVariant(i));
         if (p.VType=varUnknown) and (p.VUnknown=nil) then
            Exit(i);
      end;
   end else begin
      for i:=fromIndex to ArrayLength-1 do
         if itemFunc.SameFunc(AsPVariant(i)^) then
            Exit(i);
   end;
   Result:=-1;
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
      VarClearSafe(AsPVariant(i)^);
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

// Copy
//
procedure TScriptDynamicArray.Copy(src : TScriptDynamicArray; index, count : Integer);
begin
   ArrayLength := count;
   WriteData(src, index*ElementSize, count*ElementSize);
end;

// Concat
//
procedure TScriptDynamicArray.Concat(src : TScriptDynamicArray);
var
   n, nSrc : Integer;
begin
   if src.ArrayLength > 0 then begin
      n := ArrayLength;
      nSrc := src.ArrayLength;
      FArrayLength := n + nSrc;
      SetDataLength(FArrayLength*ElementSize);
      WriteData(n*ElementSize, src, nSrc*ElementSize);
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

end.
