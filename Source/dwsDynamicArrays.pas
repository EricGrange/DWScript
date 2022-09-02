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
   Classes, SysUtils, System.Variants,
   dwsSymbols, dwsUtils, dwsDataContext, dwsJSON;

type
   IPDoubleArray = interface
      ['{1283A4C6-EEF4-42B6-A3AB-8FB122DBA478}']
      function AsPDouble(var nbElements, stride : NativeInt) : PDouble;
   end;

   // Here be dragons! this is a hack !
   // This is used for JIT casting of interface pointer to underlying to field offset
   TDynamicArrayInterfaceToOffsets = record
      DataPtrOffset : Integer;
      ArrayLengthOffset : Integer;
   end;

   TScriptDynamicDataArray = class (TDataContext, IScriptDynArray)
      private
         FElementTyp : TTypeSymbol;
         FElementSize : Integer;
         FArrayLength : NativeInt;

      protected
         constructor Create(elemTyp : TTypeSymbol);

         function GetElementSize : Integer;
         function GetElementType : TTypeSymbol;

         procedure SetArrayLength(n : NativeInt);
         function GetArrayLength : NativeInt;

         procedure SetAsVariant(index : NativeInt; const v : Variant);
         procedure EvalAsVariant(index : NativeInt; var result : Variant);

         function GetAsInteger(index : NativeInt) : Int64;
         procedure SetAsInteger(index : NativeInt; const v : Int64);

         function GetAsFloat(index : NativeInt) : Double;
         procedure SetAsFloat(index : NativeInt; const v : Double);

         function GetAsBoolean(index : NativeInt) : Boolean;
         procedure SetAsBoolean(index : NativeInt; const v : Boolean);

         procedure SetAsString(index : NativeInt; const v : String);
         procedure EvalAsString(index : NativeInt; var result : String);

         procedure SetAsInterface(index : NativeInt; const v : IUnknown);
         procedure EvalAsInterface(index : NativeInt; var result : IUnknown);

         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;

      public
         procedure FreeInstance; override;

         function ScriptTypeName : String; override;

         function BoundsCheckPassed(index : NativeInt) : Boolean; inline;

         procedure Delete(index, count : NativeInt);
         procedure Insert(index : NativeInt);
         procedure Swap(i1, i2 : NativeInt); virtual;
         procedure Reverse;
         procedure NaturalSort; virtual;

         function IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
         function IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;

         procedure Copy(src : TScriptDynamicDataArray; index, count : NativeInt);
         procedure Concat(const src : IScriptDynArray; index, size : NativeInt);
         procedure MoveItem(srcIndex, dstIndex : NativeInt);

         function ToString : String; override;
         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;

         procedure AddStrings(sl : TStrings);

         function HashCode(addr : NativeInt; size : NativeInt) : Cardinal; virtual;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ElementSize : Integer read FElementSize;
         property ArrayLength : NativeInt read FArrayLength write SetArrayLength;

         function VarType(addr : NativeInt) : TVarType; reintroduce; virtual;
         function IsEmpty(addr : NativeInt) : Boolean; virtual;

         property AsInteger[index : NativeInt] : Int64 read GetAsInteger write SetAsInteger;
         property AsFloat[index : NativeInt] : Double read GetAsFloat write SetAsFloat;
         property AsString[index : NativeInt] : String write SetAsString;
   end;

   TScriptDynamicValueArray = class sealed (TScriptDynamicDataArray)
      public
         class function NewInstance: TObject; override;
         procedure FreeInstance; override;

         procedure Swap(i1, i2 : NativeInt); override;
   end;

   TScriptDynamicNativeArray = class abstract (TInterfacedObject, IGetSelf)
      private
         FElementTyp : TTypeSymbol;

      protected
         FArrayLength : NativeInt;

         function GetElementSize : Integer;
         function GetElementType : TTypeSymbol;

         function GetArrayLength : NativeInt;

      public
         constructor Create(elemTyp : TTypeSymbol); virtual;

         function GetSelf : TObject;
         function ToString : String; override;
         function ScriptTypeName : String;

         class function InterfaceOffsets : TDynamicArrayInterfaceToOffsets; virtual; abstract;

         function BoundsCheckPassed(index : NativeInt) : Boolean; inline;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ArrayLength : NativeInt read FArrayLength;
   end;

   TScriptDynamicNativeIntegerArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FData : PInt64Array;
         FCapacity : NativeInt;

         procedure Grow;
         procedure Shrink;

      public
         destructor Destroy; override;

         class function InterfaceOffsets : TDynamicArrayInterfaceToOffsets; override; final;

         procedure SetArrayLength(n : NativeInt);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;

         procedure Add(v : Int64);
         procedure Insert(index : NativeInt);
         procedure Delete(index, count : NativeInt);
         procedure MoveItem(source, destination : NativeInt);
         procedure Swap(index1, index2 : NativeInt);

         function IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
         function IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;

         procedure WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt); overload;
         procedure Concat(const src : IScriptDynArray; index, size : NativeInt);

         procedure Reverse;
         function  Compare(index1, index2 : NativeInt) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function GetAsFloat(index : NativeInt) : Double;
         procedure SetAsFloat(index : NativeInt; const v : Double);

         function GetAsInteger(index : NativeInt) : Int64; inline;
         procedure SetAsInteger(index : NativeInt; const v : Int64); inline;

         function GetAsBoolean(index : NativeInt) : Boolean;
         procedure SetAsBoolean(index : NativeInt; const v : Boolean);

         procedure SetAsVariant(index : NativeInt; const v : Variant);
         procedure EvalAsVariant(index : NativeInt; var result : Variant);

         procedure SetAsString(index : NativeInt; const v : String);
         procedure EvalAsString(index : NativeInt; var result : String);

         procedure SetAsInterface(index : NativeInt; const v : IUnknown);
         procedure EvalAsInterface(index : NativeInt; var result : IUnknown);

         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType;

         function HashCode(addr : NativeInt; size : NativeInt) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   TScriptDynamicNativeFloatArray = class (
                                           TScriptDynamicNativeArray,
                                           IScriptDynArray, IJSONWriteAble, IPDoubleArray
                                          )
      protected
         FData : PDoubleArray;
         FCapacity : NativeInt;

         procedure Grow;
         procedure Shrink;

      public
         destructor Destroy; override;

         class function InterfaceOffsets : TDynamicArrayInterfaceToOffsets; override; final;

         procedure SetArrayLength(n : NativeInt);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;

         procedure Add(v : Double);
         procedure Insert(index : NativeInt);
         procedure Delete(index, count : NativeInt);
         procedure MoveItem(source, destination : NativeInt);
         procedure Swap(index1, index2 : NativeInt);

         function IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
         function IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;

         procedure WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt); overload;
         procedure Concat(const src : IScriptDynArray; index, size : NativeInt);

         procedure Reverse;
         function  Compare(index1, index2 : NativeInt) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function AsPDouble(var nbElements, stride : NativeInt) : PDouble;

         function GetAsFloat(index : NativeInt) : Double; inline;
         procedure SetAsFloat(index : NativeInt; const v : Double); inline;

         function GetAsInteger(index : NativeInt) : Int64;
         procedure SetAsInteger(index : NativeInt; const v : Int64);

         function GetAsBoolean(index : NativeInt) : Boolean;
         procedure SetAsBoolean(index : NativeInt; const v : Boolean);

         procedure SetAsVariant(index : NativeInt; const v : Variant);
         procedure EvalAsVariant(index : NativeInt; var result : Variant);

         procedure SetAsString(index : NativeInt; const v : String);
         procedure EvalAsString(index : NativeInt; var result : String);

         procedure SetAsInterface(index : NativeInt; const v : IUnknown);
         procedure EvalAsInterface(index : NativeInt; var result : IUnknown);

         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType;

         function HashCode(addr : NativeInt; size : NativeInt) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   TScriptDynamicNativeStringArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FData : TStringDynArray;

      public
         class function InterfaceOffsets : TDynamicArrayInterfaceToOffsets; override; final;

         procedure SetArrayLength(n : NativeInt);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;

         procedure Add(const v : String);
         procedure Insert(index : NativeInt);
         procedure Delete(index, count : NativeInt);
         procedure MoveItem(source, destination : NativeInt);
         procedure Swap(index1, index2 : NativeInt);

         function IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
         function IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;

         procedure WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt); overload;
         procedure Concat(const src : IScriptDynArray; index, size : NativeInt);

         procedure Reverse;
         function  Compare(index1, index2 : NativeInt) : Integer;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function GetAsFloat(index : NativeInt) : Double;
         procedure SetAsFloat(index : NativeInt; const v : Double);

         function GetAsInteger(index : NativeInt) : Int64;
         procedure SetAsInteger(index : NativeInt; const v : Int64);

         function GetAsBoolean(index : NativeInt) : Boolean;
         procedure SetAsBoolean(index : NativeInt; const v : Boolean);

         procedure SetAsVariant(index : NativeInt; const v : Variant);
         procedure EvalAsVariant(index : NativeInt; var result : Variant);

         procedure SetAsString(index : NativeInt; const v : String); inline;
         procedure EvalAsString(index : NativeInt; var result : String); inline;

         procedure SetAsInterface(index : NativeInt; const v : IUnknown);
         procedure EvalAsInterface(index : NativeInt; var result : IUnknown);

         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType;

         function HashCode(addr : NativeInt; size : NativeInt) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

   TScriptDynamicNativeBaseInterfaceArray = class (TScriptDynamicNativeArray)
      protected
         FData : TInterfaceDynArray;

      public
         class function InterfaceOffsets : TDynamicArrayInterfaceToOffsets; override; final;

         procedure FreeInstance; override;

         procedure SetArrayLength(n : NativeInt);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;

         procedure Add(const v : IUnknown);
         procedure Insert(index : NativeInt);
         procedure Delete(index, count : NativeInt);
         procedure MoveItem(source, destination : NativeInt);
         procedure Swap(index1, index2 : NativeInt);

         function IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
         function IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;

         procedure WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt); overload;
         procedure Concat(const src : IScriptDynArray; index, size : NativeInt);

         procedure Reverse;
         function  Compare(index1, index2 : NativeInt) : NativeInt;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function GetAsFloat(index : NativeInt) : Double;
         procedure SetAsFloat(index : NativeInt; const v : Double);

         function GetAsInteger(index : NativeInt) : Int64;
         procedure SetAsInteger(index : NativeInt; const v : Int64);

         function GetAsBoolean(index : NativeInt) : Boolean;
         procedure SetAsBoolean(index : NativeInt; const v : Boolean);

         procedure SetAsVariant(index : NativeInt; const v : Variant);
         procedure EvalAsVariant(index : NativeInt; var result : Variant);

         procedure SetAsString(index : NativeInt; const v : String);
         procedure EvalAsString(index : NativeInt; var result : String);

         procedure SetAsInterface(index : NativeInt; const v : IUnknown); inline;
         procedure EvalAsInterface(index : NativeInt; var result : IUnknown); inline;

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType;

         function HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
   end;

   TScriptDynamicNativeInterfaceArray = class (TScriptDynamicNativeBaseInterfaceArray, IScriptDynArray)
      public
         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
   end;
   TScriptDynamicNativeObjectArray = class sealed (TScriptDynamicNativeBaseInterfaceArray, IScriptDynArray)
      public
         class function NewInstance: TObject; override;
         procedure FreeInstance; override;

         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
   end;
   TScriptDynamicNativeDynArrayArray = class (TScriptDynamicNativeBaseInterfaceArray, IScriptDynArray)
      public
         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
         procedure SetArrayLength(n : NativeInt);
         procedure Insert(index : NativeInt);
   end;

   TScriptDynamicNativeBooleanArray = class (TScriptDynamicNativeArray, IScriptDynArray, IJSONWriteAble)
      protected
         FData : TUInt64DynArray;
         const cBlockByteSize = SizeOf(UInt64);
         const cBlockBitSize = 64;
         const cShiftBlockBitSize = 6;
         const cShiftBlockByteSize = 3;
         const cMaskBlockBits = (1 shl cShiftBlockBitSize)-1;

      public
         class function InterfaceOffsets : TDynamicArrayInterfaceToOffsets; override; final;

         procedure SetArrayLength(n : NativeInt);

         function ToStringArray : TStringDynArray;
         function ToInt64Array : TInt64DynArray;

         procedure Add(v : Boolean);
         procedure Insert(index : NativeInt);
         procedure Delete(index, count : NativeInt);
         procedure MoveItem(source, destination : NativeInt);
         procedure Swap(index1, index2 : NativeInt);

         function IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
         function IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
         function IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
         function IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
         function IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
         function IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;

         procedure WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt); overload;
         procedure Concat(const src : IScriptDynArray; index, size : NativeInt);

         procedure Reverse;
         function  Compare(index1, index2 : NativeInt) : NativeInt;
         procedure NaturalSort;

         procedure AddStrings(sl : TStrings);

         function GetAsFloat(index : NativeInt) : Double;
         procedure SetAsFloat(index : NativeInt; const v : Double);

         function GetAsInteger(index : NativeInt) : Int64;
         procedure SetAsInteger(index : NativeInt; const v : Int64);

         function GetAsBoolean(index : NativeInt) : Boolean;
         procedure SetAsBoolean(index : NativeInt; const v : Boolean);

         procedure SetAsVariant(index : NativeInt; const v : Variant);
         procedure EvalAsVariant(index : NativeInt; var result : Variant);

         procedure SetAsString(index : NativeInt; const v : String);
         procedure EvalAsString(index : NativeInt; var result : String);

         procedure SetAsInterface(index : NativeInt; const v : IUnknown);
         procedure EvalAsInterface(index : NativeInt; var result : IUnknown);

         procedure AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
         function SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;

         function IsEmpty(addr : NativeInt) : Boolean;
         function VarType(addr : NativeInt) : TVarType;

         function HashCode(addr : NativeInt; size : NativeInt) : Cardinal;

         procedure WriteToJSON(writer : TdwsJSONWriter);
   end;

procedure CreateNewDynamicArray(elemTyp : TTypeSymbol; var result : IScriptDynArray);

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

// CreateNewDynamicArray (proc IScriptDynArray)
//
procedure CreateNewDynamicArray(elemTyp : TTypeSymbol; var result : IScriptDynArray);
var
   size : Integer;
   ct : TClass;
begin
   if elemTyp<>nil then
      size := elemTyp.Size
   else size := 0;
   if size = 1 then begin
      ct := elemTyp.UnAliasedType.ClassType;
      if ct = TBaseStringSymbol then
         Result := TScriptDynamicNativeStringArray.Create(elemTyp)
      else if ct = TBaseFloatSymbol then
         Result := TScriptDynamicNativeFloatArray.Create(elemTyp)
      else if ct = TBaseIntegerSymbol then
         Result := TScriptDynamicNativeIntegerArray.Create(elemTyp)
      else if ct = TBaseBooleanSymbol then
         Result := TScriptDynamicNativeBooleanArray.Create(elemTyp)
      else if ct = TClassSymbol then
         Result := TScriptDynamicNativeObjectArray.Create(elemTyp)
      else if ct = TDynamicArraySymbol then
         Result := TScriptDynamicNativeDynArrayArray.Create(elemTyp)
//      else if ct = TInterfaceSymbol then
//         Result := TScriptDynamicNativeInterfaceArray.Create(elemTyp)
      else Result := TScriptDynamicValueArray.Create(elemTyp)

   end else Result := TScriptDynamicDataArray.Create(elemTyp);
end;

// ------------------
// ------------------ TScriptDynamicDataArray ------------------
// ------------------

// Create
//
constructor TScriptDynamicDataArray.Create(elemTyp : TTypeSymbol);
begin
   inherited Create;
   FElementTyp := elemTyp;
   if elemTyp <> nil then
      FElementSize := elemTyp.Size;
end;

// ScriptTypeName
//
function TScriptDynamicDataArray.ScriptTypeName : String;
begin
   Result := 'array of ' + ElementTyp.Caption;
end;

// SetArrayLength
//
procedure TScriptDynamicDataArray.SetArrayLength(n : NativeInt);
var
   i : NativeInt;
begin
   SetDataLength(n*ElementSize);
   for i := FArrayLength to n-1 do
      FElementTyp.InitDataContext(Self, i*ElementSize);
   FArrayLength:=n;
end;

// GetArrayLength
//
function TScriptDynamicDataArray.GetArrayLength : NativeInt;
begin
   Result:=FArrayLength;
end;

// SetAsVariant
//
procedure TScriptDynamicDataArray.SetAsVariant(index : NativeInt; const v : Variant);
begin
   inherited AsVariant[index] := v;
end;

// EvalAsVariant
//
procedure TScriptDynamicDataArray.EvalAsVariant(index : NativeInt; var result : Variant);
begin
   inherited EvalAsVariant(index, result);
end;

// BoundsCheckPassed
//
function TScriptDynamicDataArray.BoundsCheckPassed(index : NativeInt) : Boolean;
begin
   Result := Cardinal(index) < Cardinal(FArrayLength);
end;

// GetAsInteger
//
function TScriptDynamicDataArray.GetAsInteger(index : NativeInt) : Int64;
begin
   Result := inherited AsInteger[index];
end;

// SetAsInteger
//
procedure TScriptDynamicDataArray.SetAsInteger(index : NativeInt; const v : Int64);
begin
   inherited AsInteger[index] := v;
end;

// GetAsFloat
//
function TScriptDynamicDataArray.GetAsFloat(index : NativeInt) : Double;
begin
   Result := inherited AsFloat[index];
end;

// SetAsFloat
//
procedure TScriptDynamicDataArray.SetAsFloat(index : NativeInt; const v : Double);
begin
   inherited AsFloat[index] := v;
end;

// GetAsBoolean
//
function TScriptDynamicDataArray.GetAsBoolean(index : NativeInt) : Boolean;
begin
   Result := inherited AsBoolean[index];
end;

// SetAsBoolean
//
procedure TScriptDynamicDataArray.SetAsBoolean(index : NativeInt; const v : Boolean);
begin
   inherited AsBoolean[index] := v;
end;

// SetAsString
//
procedure TScriptDynamicDataArray.SetAsString(index : NativeInt; const v : String);
begin
   inherited AsString[index] := v;
end;

// EvalAsString
//
procedure TScriptDynamicDataArray.EvalAsString(index : NativeInt; var result : String);
begin
   inherited EvalAsString(index, result);
end;

// SetAsInterface
//
procedure TScriptDynamicDataArray.SetAsInterface(index : NativeInt; const v : IUnknown);
begin
   inherited AsInterface[index] := v;
end;

// EvalAsInterface
//
procedure TScriptDynamicDataArray.EvalAsInterface(index : NativeInt; var result : IUnknown);
begin
   inherited EvalAsInterface(index, result);
end;

// AddFromExpr
//
procedure TScriptDynamicDataArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
var
   n : NativeInt;
   v : Variant;
begin
   Assert(FElementTyp.Size = 1);
   valueExpr.EvalAsVariant(exec, v);
   n := FArrayLength;
   SetArrayLength(n + 1);
   AsVariant[n] := v;
end;

// SetFromExpr
//
function TScriptDynamicDataArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      valueExpr.EvalAsVariant(exec, DirectData[Addr+index]);
      Result := True;
   end else Result := False;
end;

// FreeInstance
//
procedure TScriptDynamicDataArray.FreeInstance;
begin
   ClearData;
   FreeMemory(Self);
end;

// AddStrings
//
procedure TScriptDynamicDataArray.AddStrings(sl : TStrings);
begin
   DynamicArrayAddStrings(Self, sl);
end;

// HashCode
//
function TScriptDynamicDataArray.HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
begin
   Result := DWSHashCode(@DirectData[addr], size);
end;

// VarType
//
function TScriptDynamicDataArray.VarType(addr : NativeInt) : TVarType;
begin
   Result := inherited VarType(addr);
end;

// IsEmpty
//
function TScriptDynamicDataArray.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := inherited IsEmpty(addr);
end;

// Insert
//
procedure TScriptDynamicDataArray.Insert(index : NativeInt);
var
   n : NativeInt;
   p : PData;
begin
   Inc(FArrayLength);
   SetDataLength(FArrayLength*ElementSize);
   n:=(FArrayLength-index-1)*ElementSize*SizeOf(Variant);
   p := AsPData;
   if n>0 then
      Move(p^[index*ElementSize], p^[(index+1)*ElementSize], n);
   FillChar(p^[index*ElementSize], ElementSize*SizeOf(Variant), 0);

   FElementTyp.InitDataContext(Self, index*ElementSize);
end;

// Delete
//
procedure TScriptDynamicDataArray.Delete(index, count : NativeInt);
var
   i, d : NativeInt;
   p : PData;
begin
   if count <= 0 then Exit;

   Dec(FArrayLength, count);
   index:=index*ElementSize;
   count:=count*ElementSize;
   for i:=index to index+count-1 do
      VarClearSafe(DirectData[i]);
   d := (FArrayLength-1)*ElementSize+count-index;
   p := AsPData;
   if d > 0 then
      System.Move(p^[index+count], p^[index], d*SizeOf(Variant));
   System.FillChar(p^[FArrayLength*ElementSize], count*SizeOf(Variant), 0);
   SetDataLength(FArrayLength*ElementSize);
end;

// Swap
//
procedure TScriptDynamicDataArray.Swap(i1, i2 : NativeInt);
var
   i : NativeInt;
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

// Reverse
//
procedure TScriptDynamicDataArray.Reverse;
var
   t, b : NativeInt;
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
procedure TScriptDynamicDataArray.NaturalSort;
begin
   Assert(False);
end;

// IndexOfValue
//
function TScriptDynamicDataArray.IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1)
end;

// IndexOfInteger
//
function TScriptDynamicDataArray.IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfFloat
//
function TScriptDynamicDataArray.IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfString
//
function TScriptDynamicDataArray.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfInterface
//
function TScriptDynamicDataArray.IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
begin
   Result := inherited IndexOfValue(item, fromIndex, FArrayLength-1);
end;

// IndexOfFuncPtr
//
function TScriptDynamicDataArray.IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
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
procedure TScriptDynamicDataArray.Copy(src : TScriptDynamicDataArray; index, count : NativeInt);
begin
   ArrayLength := count;
   WriteData(0, src, index*ElementSize, count*ElementSize);
end;

// Concat
//
procedure TScriptDynamicDataArray.Concat(const src : IScriptDynArray; index, size : NativeInt);

   procedure Fallback;
   var
      i, n, size : NativeInt;
   begin
      size := src.ArrayLength;
      if size = 0 then Exit;
      n := ArrayLength;
      SetArrayLength(n + size);
      Assert(ElementSize = 1);
      for i := 0 to size-1 do
         src.EvalAsVariant(i, AsPData^[n + i]);
   end;

var
   n : NativeInt;
   srcDyn : TScriptDynamicDataArray;
begin
   Assert(index >= 0);
   if src.GetSelf.ClassType = Self.ClassType then begin
      srcDyn := TScriptDynamicDataArray(src.GetSelf);
      if size > srcDyn.ArrayLength - index then
         size := srcDyn.ArrayLength - index;
      if size > 0 then begin
         n := ArrayLength;
         FArrayLength := n + size;
         SetDataLength(FArrayLength*ElementSize);
         WriteData(n*ElementSize, srcDyn, index*ElementSize, size*ElementSize);
      end;
   end else Fallback;
end;

// MoveItem
//
procedure TScriptDynamicDataArray.MoveItem(srcIndex, dstIndex : NativeInt);
begin
   MoveData(srcIndex*ElementSize, dstIndex*ElementSize, ElementSize);
end;

// ToString
//
function TScriptDynamicDataArray.ToString : String;
begin
   Result := 'array of '+FElementTyp.Name;
end;

// ToStringArray
//
function TScriptDynamicDataArray.ToStringArray : TStringDynArray;
var
   i : NativeInt;
begin
   Assert(FElementTyp.BaseType.ClassType=TBaseStringSymbol);

   System.SetLength(Result, ArrayLength);
   for i:=0 to ArrayLength-1 do
      EvalAsString(i, Result[i]);
end;

// ToInt64Array
//
function TScriptDynamicDataArray.ToInt64Array : TInt64DynArray;
var
   i : NativeInt;
begin
   Assert(FElementTyp.BaseType.ClassType=TBaseIntegerSymbol);

   System.SetLength(Result, ArrayLength);
   for i:=0 to ArrayLength-1 do
      Result[i]:=AsInteger[i];
end;

// GetElementSize
//
function TScriptDynamicDataArray.GetElementSize : Integer;
begin
   Result:=FElementSize;
end;

// GetElementType
//
function TScriptDynamicDataArray.GetElementType : TTypeSymbol;
begin
   Result := FElementTyp;
end;

// ------------------
// ------------------ TScriptDynamicValueArray ------------------
// ------------------

// NewInstance
//
var
   vDynamicValueArray : TClassInstanceTemplate<TScriptDynamicValueArray>;
class function TScriptDynamicValueArray.NewInstance: TObject;
begin
   if not vDynamicValueArray.Initialized then
      Result := inherited NewInstance
   else Result := vDynamicValueArray.CreateInstance;
end;

// FreeInstance
//
procedure TScriptDynamicValueArray.FreeInstance;
begin
   ClearData;
   vDynamicValueArray.ReleaseInstance(Self);
end;

// Swap
//
procedure TScriptDynamicValueArray.Swap(i1, i2 : NativeInt);
var
   elem1, elem2 : PVarData;
   buf : TVarData;
begin
   elem1 := @DirectData[i1];
   elem2 := @DirectData[i2];

   buf    := elem1^;
   elem1^ := elem2^;
   elem2^ := buf;
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
   Assert((elemTyp = nil) or (elemTyp.Size = 1));
end;

// GetSelf
//
function TScriptDynamicNativeArray.GetSelf : TObject;
begin
   Result := Self;
end;

// ToString
//
function TScriptDynamicNativeArray.ToString : String;
begin
   Result := ScriptTypeName;
end;

// ScriptTypeName
//
function TScriptDynamicNativeArray.ScriptTypeName : String;
begin
   Result := 'array of ' + ElementTyp.Name;
end;

// BoundsCheckPassed
//
function TScriptDynamicNativeArray.BoundsCheckPassed(index : NativeInt) : Boolean;
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
function TScriptDynamicNativeArray.GetArrayLength : NativeInt;
begin
   Result := FArrayLength;
end;

// ------------------
// ------------------ TScriptDynamicNativeIntegerArray ------------------
// ------------------

// Destroy
//
destructor TScriptDynamicNativeIntegerArray.Destroy;
begin
   inherited;
   FreeMemory(FData);
end;

// Grow
//
procedure TScriptDynamicNativeIntegerArray.Grow;
begin
   FCapacity := FCapacity + (FCapacity shr 2) + 8;
   ReallocMem(FData, FCapacity * SizeOf(Int64));
end;

// Shrink
//
procedure TScriptDynamicNativeIntegerArray.Shrink;
begin
   FCapacity := FArrayLength;
   ReallocMem(FData, FCapacity * SizeOf(Int64));
end;

// SetArrayLength
//
procedure TScriptDynamicNativeIntegerArray.SetArrayLength(n : NativeInt);
begin
   if n = FArrayLength then Exit;
   FCapacity := n;
   ReallocMem(FData, n*SizeOf(Int64));
   if n > FArrayLength then begin
      System.FillChar(FData[FArrayLength], (n-FArrayLength)*SizeOf(Int64), 0);
      FArrayLength := n;
   end else begin
      FArrayLength := n;
      if n < FCapacity - (FCapacity shr 2) then
        Shrink;
   end;
end;

// ToStringArray
//
function TScriptDynamicNativeIntegerArray.ToStringArray : TStringDynArray;
var
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := IntToStr(FData[i]);
end;

// ToInt64Array
//
function TScriptDynamicNativeIntegerArray.ToInt64Array : TInt64DynArray;
begin
   SetLength(Result, FArrayLength);
   if FArrayLength > 0 then
      System.Move(FData[0], Result[0], FArrayLength*SizeOf(Int64));
end;

// Add
//
procedure TScriptDynamicNativeIntegerArray.Add(v : Int64);
begin
   if FArrayLength = FCapacity then Grow;
   FData[FArrayLength] := v;
   Inc(FArrayLength);
end;

// Insert
//
procedure TScriptDynamicNativeIntegerArray.Insert(index : NativeInt);
var
   n : NativeInt;
begin
   if FArrayLength = FCapacity then Grow;
   n := FArrayLength-index;
   if n > 0 then
      System.Move(FData[index], FData[index+1], n*SizeOf(Int64));
   FData[index] := 0;
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeIntegerArray.Delete(index, count : NativeInt);
var
   n : Integer;
begin
   n := FArrayLength-index-count;
   if n > 0 then
      System.Move(FData[index+count], FData[index], n*SizeOf(Int64));
   Dec(FArrayLength, count);
   if FArrayLength < FCapacity shr 1 then
      Shrink;
end;

// MoveItem
//
procedure TScriptDynamicNativeIntegerArray.MoveItem(source, destination : NativeInt);
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
procedure TScriptDynamicNativeIntegerArray.Swap(index1, index2 : NativeInt);
begin
   SwapInt64(FData[index1], FData[index2]);
end;

// IndexOfValue
//
function TScriptDynamicNativeIntegerArray.IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfInteger(VariantToInt64(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeIntegerArray.IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
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
function TScriptDynamicNativeIntegerArray.IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
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
function TScriptDynamicNativeIntegerArray.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfInteger(StrToInt64(item), fromIndex);
end;

// IndexOfInterface
//
function TScriptDynamicNativeIntegerArray.IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeIntegerArray.IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeIntegerArray.WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt);
var
   i : NativeInt;
begin
   for i := 0 to size-1 do
      FData[i + destAddr] := src.AsInteger[i + srcAddr];
end;

// Concat
//
procedure TScriptDynamicNativeIntegerArray.Concat(const src : IScriptDynArray; index, size : NativeInt);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeIntegerArray;
   n : NativeInt;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeIntegerArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeIntegerArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      SetArrayLength(n + size);
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
function TScriptDynamicNativeIntegerArray.Compare(index1, index2 : NativeInt) : Integer;
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

// GetAsFloat
//
function TScriptDynamicNativeIntegerArray.GetAsFloat(index : NativeInt) : Double;
begin
   Result := FData[index];
end;

// SetAsFloat
//
procedure TScriptDynamicNativeIntegerArray.SetAsFloat(index : NativeInt; const v : Double);
begin
   FData[index] := Round(v);
end;

// GetAsInteger
//
function TScriptDynamicNativeIntegerArray.GetAsInteger(index : NativeInt) : Int64;
begin
   Result := FData[index];
end;

// SetAsInteger
//
procedure TScriptDynamicNativeIntegerArray.SetAsInteger(index : NativeInt; const v : Int64);
begin
   FData[index] := v;
end;

// GetAsBoolean
//
function TScriptDynamicNativeIntegerArray.GetAsBoolean(index : NativeInt) : Boolean;
begin
   Result := FData[index] <> 0;
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeIntegerArray.SetAsBoolean(index : NativeInt; const v : Boolean);
begin
   FData[index] := Ord(v);
end;

// SetAsVariant
//
procedure TScriptDynamicNativeIntegerArray.SetAsVariant(index : NativeInt; const v : Variant);
begin
   FData[index] := VariantToInt64(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeIntegerArray.EvalAsVariant(index : NativeInt; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeIntegerArray.SetAsString(index : NativeInt; const v : String);
begin
   FData[index] := StrToInt64(v);
end;

// EvalAsString
//
procedure TScriptDynamicNativeIntegerArray.EvalAsString(index : NativeInt; var result : String);
begin
   result := IntToStr(FData[index]);
end;

// SetAsInterface
//
procedure TScriptDynamicNativeIntegerArray.SetAsInterface(index : NativeInt; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeIntegerArray.EvalAsInterface(index : NativeInt; var result : IUnknown);
begin
   Assert(False);
end;

// AddFromExpr
//
procedure TScriptDynamicNativeIntegerArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
begin
   Add(valueExpr.EvalAsInteger(exec));
end;

// SetFromExpr
//
function TScriptDynamicNativeIntegerArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      FData[index] := valueExpr.EvalAsInteger(exec);
      Result := True;
   end else Result := False;
end;

// IsEmpty
//
function TScriptDynamicNativeIntegerArray.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeIntegerArray.VarType(addr : NativeInt) : TVarType;
begin
   Result := vtInt64;
end;

// HashCode
//
function TScriptDynamicNativeIntegerArray.HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
var
   i : NativeInt;
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
   i : NativeInt;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteInteger(FData[i]);
   writer.EndArray;
end;

// InterfaceOffsets
//
class function TScriptDynamicNativeIntegerArray.InterfaceOffsets : TDynamicArrayInterfaceToOffsets;
var
   instance : TScriptDynamicNativeIntegerArray;
   intf : IScriptDynArray;
begin
   instance := TScriptDynamicNativeIntegerArray.Create(nil);
   intf := instance;
   Result.DataPtrOffset := NativeInt(@instance.FData) - NativeInt(intf);
   Result.ArrayLengthOffset := NativeInt(@instance.FArrayLength) - NativeInt(intf);
end;

// ------------------
// ------------------ TScriptDynamicNativeFloatArray ------------------
// ------------------

// Destroy
//
destructor TScriptDynamicNativeFloatArray.Destroy;
begin
   inherited;
   FreeMemory(FData);
end;

// Grow
//
procedure TScriptDynamicNativeFloatArray.Grow;
begin
   FCapacity := FCapacity + (FCapacity shr 2) + 8;
   ReallocMem(FData, FCapacity * SizeOf(Double));
end;

// Shrink
//
procedure TScriptDynamicNativeFloatArray.Shrink;
begin
   FCapacity := FArrayLength;
   ReallocMem(FData, FCapacity * SizeOf(Double));
end;

// SetArrayLength
//
procedure TScriptDynamicNativeFloatArray.SetArrayLength(n : NativeInt);
begin
   if n = FArrayLength then Exit;
   FCapacity := n;
   ReallocMem(FData, n*SizeOf(Double));
   if n > FArrayLength then begin
      System.FillChar(FData[FArrayLength], (n-FArrayLength)*SizeOf(Double), 0);
      FArrayLength := n;
   end else begin
      FArrayLength := n;
      if n < FCapacity - (FCapacity shr 2) then
        Shrink;
   end;
end;

// ToStringArray
//
function TScriptDynamicNativeFloatArray.ToStringArray : TStringDynArray;
var
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      FastFloatToStr(FData[i], Result[i], FormatSettings);
end;

// ToInt64Array
//
function TScriptDynamicNativeFloatArray.ToInt64Array : TInt64DynArray;
var
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := Round(FData[i]);
end;

// Add
//
procedure TScriptDynamicNativeFloatArray.Add(v : Double);
begin
   if FArrayLength = FCapacity then Grow;
   FData[FArrayLength] := v;
   Inc(FArrayLength);
end;

// Insert
//
procedure TScriptDynamicNativeFloatArray.Insert(index : NativeInt);
var
   n : NativeInt;
begin
   if FArrayLength = FCapacity then Grow;
   n := FArrayLength-index;
   if n > 0 then
      System.Move(FData[index], FData[index+1], n*SizeOf(Double));
   FData[index] := 0;
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeFloatArray.Delete(index, count : NativeInt);
var
   n : Integer;
begin
   n := FArrayLength-index-count;
   if n > 0 then
      System.Move(FData[index+count], FData[index], n*SizeOf(Double));
   Dec(FArrayLength, count);
   if FArrayLength < FCapacity shr 1 then
      Shrink;
end;

// MoveItem
//
procedure TScriptDynamicNativeFloatArray.MoveItem(source, destination : NativeInt);
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
procedure TScriptDynamicNativeFloatArray.Swap(index1, index2 : NativeInt);
begin
   SwapDoubles(FData[index1], FData[index2]);
end;

// IndexOfValue
//
function TScriptDynamicNativeFloatArray.IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfFloat(VariantToInt64(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeFloatArray.IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfFloat(item, fromIndex);
end;

// IndexOfFloat
//
function TScriptDynamicNativeFloatArray.IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
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
function TScriptDynamicNativeFloatArray.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfFloat(StrToFloat(item), fromIndex);
end;

// IndexOfInterface
//
function TScriptDynamicNativeFloatArray.IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeFloatArray.IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeFloatArray.WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt);
var
   i : NativeInt;
begin
   for i := 0 to size-1 do
      FData[i + destAddr] := src.AsFloat[i + srcAddr];
end;

// Concat
//
procedure TScriptDynamicNativeFloatArray.Concat(const src : IScriptDynArray; index, size : NativeInt);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeFloatArray;
   n : NativeInt;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeFloatArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeFloatArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      SetArrayLength(n + size);
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
function TScriptDynamicNativeFloatArray.Compare(index1, index2 : NativeInt) : Integer;
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
function TScriptDynamicNativeFloatArray.AsPDouble(var nbElements, stride : NativeInt) : PDouble;
begin
   Result := Pointer(FData);
   nbElements := FArrayLength;
   stride := SizeOf(Double);
end;

// GetAsFloat
//
function TScriptDynamicNativeFloatArray.GetAsFloat(index : NativeInt) : Double;
begin
   Result := FData[index];
end;

// SetAsFloat
//
procedure TScriptDynamicNativeFloatArray.SetAsFloat(index : NativeInt; const v : Double);
begin
   FData[index] := v;
end;

// GetAsInteger
//
function TScriptDynamicNativeFloatArray.GetAsInteger(index : NativeInt) : Int64;
begin
   Result := Round(FData[index]);
end;

// SetAsInteger
//
procedure TScriptDynamicNativeFloatArray.SetAsInteger(index : NativeInt; const v : Int64);
begin
   FData[index] := v;
end;

// GetAsBoolean
//
function TScriptDynamicNativeFloatArray.GetAsBoolean(index : NativeInt) : Boolean;
begin
   Result := FData[index] <> 0;
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeFloatArray.SetAsBoolean(index : NativeInt; const v : Boolean);
begin
   FData[index] := Ord(v);
end;

// SetAsVariant
//
procedure TScriptDynamicNativeFloatArray.SetAsVariant(index : NativeInt; const v : Variant);
begin
   FData[index] := VariantToFloat(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeFloatArray.EvalAsVariant(index : NativeInt; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeFloatArray.SetAsString(index : NativeInt; const v : String);
begin
   FData[index] := StrToFloat(v);
end;

// EvalAsString
//
procedure TScriptDynamicNativeFloatArray.EvalAsString(index : NativeInt; var result : String);
begin
   FastFloatToStr(FData[index], result, FormatSettings);
end;

// SetAsInterface
//
procedure TScriptDynamicNativeFloatArray.SetAsInterface(index : NativeInt; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeFloatArray.EvalAsInterface(index : NativeInt; var result : IUnknown);
begin
   Assert(False);
end;

// AddFromExpr
//
procedure TScriptDynamicNativeFloatArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
begin
   Add(valueExpr.EvalAsFloat(exec));
end;

// SetFromExpr
//
function TScriptDynamicNativeFloatArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      FData[index] := valueExpr.EvalAsFloat(exec);
      Result := True;
   end else Result := False;
end;

// IsEmpty
//
function TScriptDynamicNativeFloatArray.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeFloatArray.VarType(addr : NativeInt) : TVarType;
begin
   Result := varDouble;
end;

// HashCode
//
function TScriptDynamicNativeFloatArray.HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
var
   i : NativeInt;
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
   i : NativeInt;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteNumber(FData[i]);
   writer.EndArray;
end;

// InterfaceOffsets
//
class function TScriptDynamicNativeFloatArray.InterfaceOffsets : TDynamicArrayInterfaceToOffsets;
var
   instance : TScriptDynamicNativeFloatArray;
   intf : IScriptDynArray;
begin
   instance := TScriptDynamicNativeFloatArray.Create(nil);
   intf := instance;
   Result.DataPtrOffset := NativeInt(@instance.FData) - NativeInt(intf);
   Result.ArrayLengthOffset := NativeInt(@instance.FArrayLength) - NativeInt(intf);
end;

// ------------------
// ------------------ TScriptDynamicNativeStringArray ------------------
// ------------------

// SetArrayLength
//
procedure TScriptDynamicNativeStringArray.SetArrayLength(n : NativeInt);
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
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := StrToInt64(FData[i]);
end;

// Add
//
procedure TScriptDynamicNativeStringArray.Add(const v : String);
var
   n : NativeInt;
begin
   n := FArrayLength+1;
   SetLength(FData, n);
   FData[FArrayLength] := v;
   FArrayLength := n;
end;

// Insert
//
procedure TScriptDynamicNativeStringArray.Insert(index : NativeInt);
begin
   System.Insert('', FData, index);
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeStringArray.Delete(index, count : NativeInt);
begin
   System.Delete(FData, index, count);
   Dec(FArrayLength, count);
end;

// MoveItem
//
procedure TScriptDynamicNativeStringArray.MoveItem(source, destination : NativeInt);
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
procedure TScriptDynamicNativeStringArray.Swap(index1, index2 : NativeInt);
begin
   SwapPointers(PPointer(@FData[index1])^, PPointer(@FData[index2])^);
end;

// IndexOfValue
//
function TScriptDynamicNativeStringArray.IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfString(VariantToString(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeStringArray.IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfString(IntToStr(item), fromIndex);
end;

// IndexOfFloat
//
function TScriptDynamicNativeStringArray.IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfString(FloatToStr(item), fromIndex);
end;

// IndexOfString
//
function TScriptDynamicNativeStringArray.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
begin
   if fromIndex < 0 then
      fromIndex := 0;
   for i := fromIndex to FArrayLength-1 do begin
      if FData[i] = item then
         Exit(i);
   end;
   Result := -1;
end;

// IndexOfInterface
//
function TScriptDynamicNativeStringArray.IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeStringArray.IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeStringArray.WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt);
var
   i : NativeInt;
begin
   for i := 0 to size-1 do
      src.EvalAsString(i + srcAddr, FData[i + destAddr]);
end;

// Concat
//
procedure TScriptDynamicNativeStringArray.Concat(const src : IScriptDynArray; index, size : NativeInt);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeStringArray;
   n, i : NativeInt;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = TScriptDynamicNativeStringArray);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeStringArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      SetArrayLength(n + size);
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
function TScriptDynamicNativeStringArray.Compare(index1, index2 : NativeInt) : Integer;
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
   i, n : NativeInt;
begin
   n := FArrayLength;
   SetArrayLength(n + sl.Count);
   for i := 0 to sl.Count-1 do
      FData[i+n] := sl[i];
end;

// GetAsFloat
//
function TScriptDynamicNativeStringArray.GetAsFloat(index : NativeInt) : Double;
begin
   Result := StrToFloat(FData[index]);
end;

// SetAsFloat
//
procedure TScriptDynamicNativeStringArray.SetAsFloat(index : NativeInt; const v : Double);
begin
   FData[index] := FloatToStr(v);
end;

// GetAsInteger
//
function TScriptDynamicNativeStringArray.GetAsInteger(index : NativeInt) : Int64;
begin
   Result := StrToInt64(FData[index]);
end;

// SetAsInteger
//
procedure TScriptDynamicNativeStringArray.SetAsInteger(index : NativeInt; const v : Int64);
begin
   FData[index] := IntToStr(v);
end;

// GetAsBoolean
//
function TScriptDynamicNativeStringArray.GetAsBoolean(index : NativeInt) : Boolean;
begin
   Result := StringToBoolean(FData[index]);
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeStringArray.SetAsBoolean(index : NativeInt; const v : Boolean);
begin
   if v then
      FData[index] := '1'
   else FData[index] := '0';
end;

// SetAsVariant
//
procedure TScriptDynamicNativeStringArray.SetAsVariant(index : NativeInt; const v : Variant);
begin
   FData[index] := VariantToString(v);
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeStringArray.EvalAsVariant(index : NativeInt; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeStringArray.SetAsString(index : NativeInt; const v : String);
begin
   FData[index] := v;
end;

// EvalAsString
//
procedure TScriptDynamicNativeStringArray.EvalAsString(index : NativeInt; var result : String);
begin
   result := FData[index];
end;

// SetAsInterface
//
procedure TScriptDynamicNativeStringArray.SetAsInterface(index : NativeInt; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeStringArray.EvalAsInterface(index : NativeInt; var result : IUnknown);
begin
   Assert(False);
end;

// AddFromExpr
//
procedure TScriptDynamicNativeStringArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
var
   s : String;
begin
   valueExpr.EvalAsString(exec, s);
   Add(s);
end;

// SetFromExpr
//
function TScriptDynamicNativeStringArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
var
   buf : String;
begin
   if BoundsCheckPassed(index) then begin
      valueExpr.EvalAsString(exec, buf);
      FData[index] := buf;
      Result := True;
   end else Result := False;
end;

// IsEmpty
//
function TScriptDynamicNativeStringArray.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeStringArray.VarType(addr : NativeInt) : TVarType;
begin
   Result := varUString;
end;

// HashCode
//
function TScriptDynamicNativeStringArray.HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
var
   i : NativeInt;
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
   i : NativeInt;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteString(FData[i]);
   writer.EndArray;
end;

// InterfaceOffsets
//
class function TScriptDynamicNativeStringArray.InterfaceOffsets : TDynamicArrayInterfaceToOffsets;
var
   instance : TScriptDynamicNativeIntegerArray;
   intf : IScriptDynArray;
begin
   instance := TScriptDynamicNativeIntegerArray.Create(nil);
   intf := instance;
   Result.DataPtrOffset := NativeInt(@instance.FData) - NativeInt(intf);
   Result.ArrayLengthOffset := NativeInt(@instance.FArrayLength) - NativeInt(intf);
end;

// ------------------
// ------------------ TScriptDynamicNativeBaseInterfaceArray ------------------
// ------------------

// SetArrayLength
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetArrayLength(n : NativeInt);
begin
   SetLength(FData, n);
   FArrayLength := n;
end;

// ToStringArray
//
function TScriptDynamicNativeBaseInterfaceArray.ToStringArray : TStringDynArray;
var
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := (FData[i] as IGetSelf).ToString;
end;

// ToInt64Array
//
function TScriptDynamicNativeBaseInterfaceArray.ToInt64Array : TInt64DynArray;
begin
   Assert(False);
end;

// Add
//
procedure TScriptDynamicNativeBaseInterfaceArray.Add(const v : IUnknown);
var
   n : NativeInt;
begin
   n := FArrayLength+1;
   SetLength(FData, n);
   FData[FArrayLength] := v;
   FArrayLength := n;
end;

// Insert
//
procedure TScriptDynamicNativeBaseInterfaceArray.Insert(index : NativeInt);
begin
   System.Insert(nil, FData, index);
   Inc(FArrayLength);
end;

// Delete
//
procedure TScriptDynamicNativeBaseInterfaceArray.Delete(index, count : NativeInt);
begin
   System.Delete(FData, index, count);
   Dec(FArrayLength, count);
end;

// MoveItem
//
procedure TScriptDynamicNativeBaseInterfaceArray.MoveItem(source, destination : NativeInt);
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
procedure TScriptDynamicNativeBaseInterfaceArray.Swap(index1, index2 : NativeInt);
begin
   SwapPointers(PPointer(@FData[index1])^, PPointer(@FData[index2])^);
end;

// IndexOfValue
//
function TScriptDynamicNativeBaseInterfaceArray.IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   if TVarData(item).VType = varUnknown then
      Result := IndexOfInterface(IUnknown(TVarData(item).VUnknown), fromIndex)
   else Result := -1;
end;

// IndexOfInteger
//
function TScriptDynamicNativeBaseInterfaceArray.IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfFloat
//
function TScriptDynamicNativeBaseInterfaceArray.IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfString
//
function TScriptDynamicNativeBaseInterfaceArray.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfInterface
//
function TScriptDynamicNativeBaseInterfaceArray.IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
begin
   for i := fromIndex to FArrayLength-1 do
      if FData[i] = item then
         Exit(i);
   Result := -1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeBaseInterfaceArray.IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
   itemFunc : IFuncPointer;
begin
   itemFunc := IFuncPointer(IUnknown(item));
   if itemFunc = nil then begin
      for i := fromIndex to ArrayLength-1 do begin
         if FData[i] = nil then
            Exit(i);
      end;
   end else begin
      for i := fromIndex to ArrayLength-1 do
         if itemFunc.SameFunc(FData[i]) then
            Exit(i);
   end;
   Result:=-1;
end;

// WriteData
//
procedure TScriptDynamicNativeBaseInterfaceArray.WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt);
var
   i : NativeInt;
begin
   for i := 0 to size-1 do
       src.EvalAsInterface(i + srcAddr, FData[i + destAddr]);
end;

// Concat
//
procedure TScriptDynamicNativeBaseInterfaceArray.Concat(const src : IScriptDynArray; index, size : NativeInt);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeBaseInterfaceArray;
   n, i : NativeInt;
begin
   srcSelf := src.GetSelf;
   Assert(srcSelf.ClassType = Self.ClassType);
   Assert(index >= 0);

   srcDyn := TScriptDynamicNativeBaseInterfaceArray(srcSelf);
   if size > srcDyn.ArrayLength - index then
      size := srcDyn.ArrayLength - index;
   if size > 0 then begin
      n := FArrayLength;
      SetArrayLength(n + size);
      for i := 0 to size-1 do
         FData[n + i] := srcDyn.FData[index + i];
   end;
end;

// Reverse
//
procedure TScriptDynamicNativeBaseInterfaceArray.Reverse;
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
function TScriptDynamicNativeBaseInterfaceArray.Compare(index1, index2 : NativeInt) : NativeInt;
begin
   Result := 0;
end;

// NaturalSort
//
procedure TScriptDynamicNativeBaseInterfaceArray.NaturalSort;
begin
   Assert(False);
end;

// AddStrings
//
procedure TScriptDynamicNativeBaseInterfaceArray.AddStrings(sl : TStrings);
begin
   Assert(False);
end;

// GetAsFloat
//
function TScriptDynamicNativeBaseInterfaceArray.GetAsFloat(index : NativeInt) : Double;
begin
   Assert(False);
   Result := 0;
end;

// SetAsFloat
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetAsFloat(index : NativeInt; const v : Double);
begin
   Assert(False);
end;

// GetAsInteger
//
function TScriptDynamicNativeBaseInterfaceArray.GetAsInteger(index : NativeInt) : Int64;
begin
   Assert(False);
   Result := 0;
end;

// SetAsInteger
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetAsInteger(index : NativeInt; const v : Int64);
begin
   Assert(False);
end;

// GetAsBoolean
//
function TScriptDynamicNativeBaseInterfaceArray.GetAsBoolean(index : NativeInt) : Boolean;
begin
   Assert(False);
   Result := False;
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetAsBoolean(index : NativeInt; const v : Boolean);
begin
   Assert(False);
end;

// SetAsVariant
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetAsVariant(index : NativeInt; const v : Variant);
begin
   FData[index] := v;
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeBaseInterfaceArray.EvalAsVariant(index : NativeInt; var result : Variant);
begin
   VarCopySafe(result, FData[index]);
end;

// SetAsString
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetAsString(index : NativeInt; const v : String);
begin
   Assert(False);
end;

// EvalAsString
//
procedure TScriptDynamicNativeBaseInterfaceArray.EvalAsString(index : NativeInt; var result : String);
begin
   result := VariantToString(FData[index]);
end;

// SetAsInterface
//
procedure TScriptDynamicNativeBaseInterfaceArray.SetAsInterface(index : NativeInt; const v : IUnknown);
begin
   FData[index] := v;
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeBaseInterfaceArray.EvalAsInterface(index : NativeInt; var result : IUnknown);
begin
   result := FData[index];
end;

// IsEmpty
//
function TScriptDynamicNativeBaseInterfaceArray.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeBaseInterfaceArray.VarType(addr : NativeInt) : TVarType;
begin
   Result := varUnknown;
end;

// HashCode
//
function TScriptDynamicNativeBaseInterfaceArray.HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
var
   i : NativeInt;
begin
   Result := cFNV_basis;
   for i := 0 to FArrayLength-1 do
      Result := (Result xor varUnknown) * cFNV_prime;
   if Result = 0 then
      Result := cFNV_basis;
end;

// InterfaceOffsets
//
class function TScriptDynamicNativeBaseInterfaceArray.InterfaceOffsets : TDynamicArrayInterfaceToOffsets;
var
   instance : TScriptDynamicNativeInterfaceArray;
   intf : IScriptDynArray;
begin
   instance := TScriptDynamicNativeInterfaceArray.Create(nil);
   intf := instance;
   Result.DataPtrOffset := NativeInt(@instance.FData) - NativeInt(intf);
   Result.ArrayLengthOffset := NativeInt(@instance.FArrayLength) - NativeInt(intf);
end;

// FreeInstance
//
procedure TScriptDynamicNativeBaseInterfaceArray.FreeInstance;
begin
   FData := nil;
   FreeMemory(Self);
end;

// ------------------
// ------------------ TScriptDynamicNativeInterfaceArray ------------------
// ------------------

// SetFromExpr
//
function TScriptDynamicNativeInterfaceArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      valueExpr.EvalAsInterface(exec, FData[index]);
      Result := True;
   end else Result := False;
end;

// AddFromExpr
//
procedure TScriptDynamicNativeInterfaceArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
var
   i : IInterface;
begin
   valueExpr.EvalAsInterface(exec, i);
   Add(i);
end;

// ------------------
// ------------------ TScriptDynamicNativeObjectArray ------------------
// ------------------

// NewInstance
//
var
   vDynamicNativeObjectArrayTemplate : TClassInstanceTemplate<TScriptDynamicNativeObjectArray>;
class function TScriptDynamicNativeObjectArray.NewInstance: TObject;
begin
   if not vDynamicNativeObjectArrayTemplate.Initialized then
      Result := inherited NewInstance
   else Result := vDynamicNativeObjectArrayTemplate.CreateInstance;
end;

// FreeInstance
//
procedure TScriptDynamicNativeObjectArray.FreeInstance;
begin
   FData := nil;
   vDynamicNativeObjectArrayTemplate.ReleaseInstance(Self);
end;

// SetFromExpr
//
function TScriptDynamicNativeObjectArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      valueExpr.EvalAsInterface(exec, FData[index]);
      Result := True;
   end else Result := False;
end;

// AddFromExpr
//
procedure TScriptDynamicNativeObjectArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
var
   i : IInterface;
begin
   valueExpr.EvalAsInterface(exec, i);
   Add(i);
end;

// ------------------
// ------------------ TScriptDynamicNativeDynArrayArray ------------------
// ------------------

// SetArrayLength
//
procedure TScriptDynamicNativeDynArrayArray.SetArrayLength(n : NativeInt);

   procedure InitElements(n : NativeInt);
   var
      i : NativeInt;
      subElemTyp : TTypeSymbol;
   begin
      subElemTyp := ElementTyp.UnAliasedType.Typ;
      for i := FArrayLength to n-1 do
         CreateNewDynamicArray(subElemTyp, IScriptDynArray(FData[i]))
   end;

begin
   SetLength(FData, n);
   if FArrayLength < n then
      InitElements(n);
   FArrayLength := n;
end;

// Insert
//
procedure TScriptDynamicNativeDynArrayArray.Insert(index : NativeInt);
begin
   inherited Insert(index);
   CreateNewDynamicArray(ElementTyp.UnAliasedType.Typ, IScriptDynArray(FData[index]));
end;

// SetFromExpr
//
function TScriptDynamicNativeDynArrayArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      valueExpr.EvalAsInterface(exec, FData[index]);
      Result := True;
   end else Result := False;
end;

// AddFromExpr
//
procedure TScriptDynamicNativeDynArrayArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
var
   i : IInterface;
begin
   valueExpr.EvalAsInterface(exec, i);
   Add(i);
end;

// ------------------
// ------------------ TScriptDynamicNativeBooleanArray ------------------
// ------------------

// InterfaceOffsets
//
class function TScriptDynamicNativeBooleanArray.InterfaceOffsets : TDynamicArrayInterfaceToOffsets;
var
   instance : TScriptDynamicNativeBooleanArray;
   intf : IScriptDynArray;
begin
   instance := TScriptDynamicNativeBooleanArray.Create(nil);
   intf := instance;
   Result.DataPtrOffset := NativeInt(@instance.FData) - NativeInt(intf);
   Result.ArrayLengthOffset := NativeInt(@instance.FArrayLength) - NativeInt(intf);
end;

// SetArrayLength
//
procedure TScriptDynamicNativeBooleanArray.SetArrayLength(n : NativeInt);
var
   blockCount : NativeInt;
   tailBits : Integer;
begin
   if n = FArrayLength then Exit;
   blockCount := n + ((cBlockBitSize - 1) shr cShiftBlockBitSize);
   SetLength(FData, blockCount);
   if n < FArrayLength then begin
      // wipe tail bits if any
      if n > 0 then begin
         tailBits := cBlockBitSize - n mod cBlockBitSize;
         if tailBits > 0 then
            FData[blockCount-1] := FData[blockCount-1] and (UInt64(-1) shr tailBits);
      end;
   end;
   FArrayLength := n;
end;

// ToStringArray
//
function TScriptDynamicNativeBooleanArray.ToStringArray : TStringDynArray;
var
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      if GetAsBoolean(i) then
         Result[i] := 'True'
      else Result[i] := 'False';
end;

// ToInt64Array
//
function TScriptDynamicNativeBooleanArray.ToInt64Array : TInt64DynArray;
var
   i : NativeInt;
begin
   SetLength(Result, FArrayLength);
   for i := 0 to FArrayLength-1 do
      Result[i] := GetAsInteger(i);
end;

// Add
//
procedure TScriptDynamicNativeBooleanArray.Add(v : Boolean);
var
   n : NativeInt;
begin
   n := FArrayLength;
   SetArrayLength(n+1);
   SetAsBoolean(n, v);
end;

// Insert
//
procedure TScriptDynamicNativeBooleanArray.Insert(index : NativeInt);
var
   i : NativeInt;
begin
   SetArrayLength(FArrayLength + 1);
   for i := FArrayLength-1 downto index+1 do
      SetAsBoolean(i, GetAsBoolean(i-1));
   SetAsBoolean(index, False);
end;

// Delete
//
procedure TScriptDynamicNativeBooleanArray.Delete(index, count : NativeInt);
var
   i : NativeInt;
begin
   for i := index to FArrayLength-count-1 do
      SetAsBoolean(i, GetAsBoolean(i+count));
   SetArrayLength(FArrayLength - count);
end;

// MoveItem
//
procedure TScriptDynamicNativeBooleanArray.MoveItem(source, destination : NativeInt);
var
   buf : Boolean;
   i : NativeInt;
begin
   if source = destination then Exit;

   buf := GetAsBoolean(source);

   if source < destination then begin
      for i := source to destination-1 do
         SetAsBoolean(i, GetAsBoolean(i+1));
   end else begin
      for i := source downto destination+1 do
         SetAsBoolean(i, GetAsBoolean(i-1));
   end;
   SetAsBoolean(destination, buf);
end;

// Swap
//
procedure TScriptDynamicNativeBooleanArray.Swap(index1, index2 : NativeInt);
var
   buf : Boolean;
begin
   buf := GetAsBoolean(index1);
   SetAsBoolean(index1, GetAsBoolean(index2));
   SetAsBoolean(index2, buf);
end;

// IndexOfValue
//
function TScriptDynamicNativeBooleanArray.IndexOfValue(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfInteger(VariantToInt64(item), fromIndex);
end;

// IndexOfInteger
//
function TScriptDynamicNativeBooleanArray.IndexOfInteger(item : Int64; fromIndex : NativeInt) : NativeInt;
var
   i : NativeInt;
   v : Boolean;
begin
   v := (item <> 0);
   for i := fromIndex to FArrayLength-1 do
      if GetAsBoolean(i) = v then
         Exit(i);
   Result := -1;
end;

// IndexOfFloat
//
function TScriptDynamicNativeBooleanArray.IndexOfFloat(item : Double; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfInteger(Ord(item <> 0), fromIndex);
end;

// IndexOfString
//
function TScriptDynamicNativeBooleanArray.IndexOfString(const item : String; fromIndex : NativeInt) : NativeInt;
begin
   Result := IndexOfInteger(Ord(StringToBoolean(item)), fromIndex);
end;

// IndexOfInterface
//
function TScriptDynamicNativeBooleanArray.IndexOfInterface(const item : IUnknown; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicNativeBooleanArray.IndexOfFuncPtr(const item : Variant; fromIndex : NativeInt) : NativeInt;
begin
   Result := -1;
end;

// WriteData
//
procedure TScriptDynamicNativeBooleanArray.WriteData(destAddr : NativeInt; const src : IDataContext; srcAddr, size : NativeInt);
var
   i : NativeInt;
begin
   for i := 0 to size-1 do
      SetAsBoolean(i + destAddr, src.AsBoolean[i + srcAddr]);
end;

// Concat
//
procedure TScriptDynamicNativeBooleanArray.Concat(const src : IScriptDynArray; index, size : NativeInt);
var
   srcSelf : TObject;
   srcDyn : TScriptDynamicNativeBooleanArray;
   i, n : NativeInt;
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
         SetAsBoolean(n + i, srcDyn.GetAsBoolean(index + i));
   end;
end;

// Reverse
//
procedure TScriptDynamicNativeBooleanArray.Reverse;
var
   i, j : NativeInt;
   buf : Boolean;
begin
   i := 0;
   j := FArrayLength-1;
   while i < j do begin
      buf := GetAsBoolean(i);
      SetAsBoolean(i, GetAsBoolean(j));
      SetAsBoolean(j, buf);
      Inc(i);
      Dec(j);
   end;
end;

// Compare
//
function TScriptDynamicNativeBooleanArray.Compare(index1, index2 : NativeInt) : NativeInt;
begin
   Result := Ord(GetAsBoolean(index1)) - Ord(GetAsBoolean(index2));
end;

// NaturalSort
//
procedure TScriptDynamicNativeBooleanArray.NaturalSort;
var
   i, j : NativeInt;
begin
   j := FArrayLength;
   for i := 0 to FArrayLength-1 do begin
      if GetAsBoolean(i) then begin
         Dec(j);
         if i < j then
            SetAsBoolean(i, False);
      end;
   end;
   for i := j to FArrayLength-1 do
      SetAsBoolean(i, True);
end;

// AddStrings
//
procedure TScriptDynamicNativeBooleanArray.AddStrings(sl : TStrings);
begin
   DynamicArrayAddStrings(Self, sl);
end;

// GetAsFloat
//
function TScriptDynamicNativeBooleanArray.GetAsFloat(index : NativeInt) : Double;
begin
   Result := Ord(GetAsBoolean(index));
end;

// SetAsFloat
//
procedure TScriptDynamicNativeBooleanArray.SetAsFloat(index : NativeInt; const v : Double);
begin
   SetAsBoolean(index, v <> 0);
end;

// GetAsInteger
//
function TScriptDynamicNativeBooleanArray.GetAsInteger(index : NativeInt) : Int64;
begin
   Result := Ord(GetAsBoolean(index));
end;

// SetAsInteger
//
procedure TScriptDynamicNativeBooleanArray.SetAsInteger(index : NativeInt; const v : Int64);
begin
   SetAsBoolean(index, v <> 0);
end;

// GetAsBoolean
//
function TScriptDynamicNativeBooleanArray.GetAsBoolean(index : NativeInt) : Boolean;
var
   p : PUInt64;
   mask : UInt64;
   bitSubIndex : Integer;
begin
   p := PUInt64(IntPtr(FData) + ((index shr cShiftBlockBitSize) shl cShiftBlockByteSize));
   // the following intermediate line is required to work around a compiler bug :(
   bitSubIndex := Integer(index) and cMaskBlockBits;
   mask := UInt64(1) shl bitSubIndex;
   Result := (p^ and mask) <> 0;
end;

// SetAsBoolean
//
procedure TScriptDynamicNativeBooleanArray.SetAsBoolean(index : NativeInt; const v : Boolean);
var
   p : PUInt64;
   mask : UInt64;
   bitSubIndex : Integer;
begin
   p := PUInt64(IntPtr(FData) + ((index shr cShiftBlockBitSize) shl cShiftBlockByteSize));
   // the following intermediate line is required to work around a compiler bug :(
   bitSubIndex := Integer(index) and cMaskBlockBits;
   mask := UInt64(1) shl bitSubIndex;
   if v then
      p^ := p^ or mask
   else p^ := p^ and not mask;
end;

// SetAsVariant
//
procedure TScriptDynamicNativeBooleanArray.SetAsVariant(index : NativeInt; const v : Variant);
begin
   SetAsBoolean(index, VariantToBool(v));
end;

// EvalAsVariant
//
procedure TScriptDynamicNativeBooleanArray.EvalAsVariant(index : NativeInt; var result : Variant);
begin
   VarCopySafe(result, GetAsBoolean(index));
end;

// SetAsString
//
procedure TScriptDynamicNativeBooleanArray.SetAsString(index : NativeInt; const v : String);
begin
   SetAsBoolean(index, StringToBoolean(v));
end;

// EvalAsString
//
procedure TScriptDynamicNativeBooleanArray.EvalAsString(index : NativeInt; var result : String);
begin
   if GetAsBoolean(index) then
      result := 'True'
   else result := 'False';
end;

// SetAsInterface
//
procedure TScriptDynamicNativeBooleanArray.SetAsInterface(index : NativeInt; const v : IUnknown);
begin
   Assert(False);
end;

// EvalAsInterface
//
procedure TScriptDynamicNativeBooleanArray.EvalAsInterface(index : NativeInt; var result : IUnknown);
begin
   Assert(False);
end;

// AddFromExpr
//
procedure TScriptDynamicNativeBooleanArray.AddFromExpr(exec : TdwsExecution; valueExpr : TExprBase);
begin
   Add(valueExpr.EvalAsBoolean(exec));
end;

// SetFromExpr
//
function TScriptDynamicNativeBooleanArray.SetFromExpr(index : NativeInt; exec : TdwsExecution; valueExpr : TExprBase) : Boolean;
begin
   if BoundsCheckPassed(index) then begin
      SetAsBoolean(index, valueExpr.EvalAsBoolean(exec));
      Result := True;
   end else Result := False;
end;

// IsEmpty
//
function TScriptDynamicNativeBooleanArray.IsEmpty(addr : NativeInt) : Boolean;
begin
   Result := False;
end;

// VarType
//
function TScriptDynamicNativeBooleanArray.VarType(addr : NativeInt) : TVarType;
begin
   Result := varBoolean;
end;

// HashCode
//
function TScriptDynamicNativeBooleanArray.HashCode(addr : NativeInt; size : NativeInt) : Cardinal;
var
   i : NativeInt;
begin
   Result := cFNV_basis;
   for i := 0 to FArrayLength-1 do
      Result := (Result xor SimpleIntegerHash(Ord(GetAsBoolean(i)))) * cFNV_prime;
   if Result = 0 then
      Result := cFNV_basis;
end;

// WriteToJSON
//
procedure TScriptDynamicNativeBooleanArray.WriteToJSON(writer : TdwsJSONWriter);
var
   i : NativeInt;
begin
   writer.BeginArray;
   for i := 0 to FArrayLength-1 do
      writer.WriteBoolean(GetAsBoolean(i));
   writer.EndArray;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vDynamicValueArray.Initialize;
   vDynamicNativeObjectArrayTemplate.Initialize;

finalization

   vDynamicValueArray.Finalize;
   vDynamicNativeObjectArrayTemplate.Finalize;

end.
