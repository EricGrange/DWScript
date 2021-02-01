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
unit dwsArrayExprs;

{$I dws.inc}

interface

uses
   SysUtils,
   dwsUtils, dwsDataContext, dwsExprs, dwsCompilerContext, dwsSymbols, dwsScriptSource,
   dwsDynamicArrays;

type

   // length of dynamic arrays
   TArrayLengthExpr = class (TUnaryOpIntExpr)
      private
         FDelta : Integer;
         FCapture : Boolean;
      public
         constructor Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos; expr : TTypedExpr; captureExpr : Boolean); reintroduce; virtual;
         destructor Destroy; override;

         function SpecializeTypedExpr(const context : ISpecializationContext) : TTypedExpr; override;

         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         property Delta : Integer read FDelta write FDelta;
   end;
   TArrayLengthExprClass = class of TArrayLengthExpr;

   // length of an open array
   TOpenArrayLengthExpr = class (TArrayLengthExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // length of an associative array
   TAssociativeArrayLengthExpr = class(TUnaryOpIntExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // Array expressions x[index]
   TArrayExpr = class(TPosDataExpr)
      protected
         FBaseExpr : TDataExpr;
         FIndexExpr : TTypedExpr;
         FElementSize : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;
         function GetBaseType : TTypeSymbol; override;

      public
         constructor Create(const aScriptPos: TScriptPos;
                            BaseExpr: TDataExpr; IndexExpr: TTypedExpr;
                            arraySymbol : TArraySymbol);
         destructor Destroy; override;
         procedure Orphan(context : TdwsCompilerContext); override;

         function IsWritable : Boolean; override;

         function SameDataExpr(expr : TTypedExpr) : Boolean; override;

         property BaseExpr : TDataExpr read FBaseExpr;
         property IndexExpr : TTypedExpr read FIndexExpr;
   end;

   EScriptOutOfBounds = class (EScriptError);

   // Array expressions x[index] for static arrays
   TStaticArrayExpr = class (TArrayExpr)
      private
         FLowBound : Integer;
         FCount : Integer;

      protected
         function GetIndex(exec : TdwsExecution) : Integer; virtual;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(const aScriptPos: TScriptPos;
                            baseExpr : TDataExpr; indexExpr : TTypedExpr;
                            arraySymbol : TStaticArraySymbol);

         function Optimize(context : TdwsCompilerContext) : TProgramExpr; override;

         procedure AssignExpr(exec : TdwsExecution; expr : TTypedExpr); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value: String); override;

         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         procedure EvalAsString(exec : TdwsExecution; var result : String); override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property LowBound : Integer read FLowBound write FLowBound;
         property Count : Integer read FCount write FCount;
   end;

   // Array expressions x[bool] for static arrays
   TStaticArrayBoolExpr = class (TStaticArrayExpr)
      protected
         function GetIndex(exec : TdwsExecution) : Integer; override;
   end;

   // Array expressions x[index] for open arrays
   TOpenArrayExpr = class(TArrayExpr)
      public
         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
         function IsWritable : Boolean; override;
   end;

   // Array expressions: x[index0] for dynamic arrays
   TDynamicArrayExpr = class(TArrayExpr)
      protected
         function EvalItem(exec : TdwsExecution; var dyn : IScriptDynArray) : PVariant;

      public
         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         procedure EvalAsString(exec : TdwsExecution; var result : String); override;

         function  SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr; override;

         procedure CreateArrayElementDataContext(exec : TdwsExecution; var result : IDataContext);
   end;

   // Array expressions: x[index0] for dynamic arrays where BaseExpr is a TObjectVarExpr
   TDynamicArrayVarExpr = class sealed (TDynamicArrayExpr)
      protected
         function EvalItem(exec : TdwsExecution) : PVariant;

      public
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
         procedure EvalAsString(exec : TdwsExecution; var result : String); override;

         function  SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr; override;
   end;

   // array[index]:=val for dynamic arrays
   TDynamicArraySetExpr = class (TNoResultExpr)
      private
         FArrayExpr : TTypedExpr;
         FIndexExpr : TTypedExpr;
         FValueExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            arrayExpr, indexExpr, valueExpr : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function  SpecializeProgramExpr(const context : ISpecializationContext) : TProgramExpr; override;

         property ArrayExpr : TTypedExpr read FArrayExpr;
         property IndexExpr : TTypedExpr read FIndexExpr;
         property ValueExpr : TTypedExpr read FValueExpr;
   end;
   TDynamicArraySetExprClass = class of TDynamicArraySetExpr;

   // array[index]:=val for dynamic arrays when ArrayExpr is TObjectVarExpr and size=1
   TDynamicArraySetVarExpr = class(TDynamicArraySetExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // array[index]:=val for dynamic arrays with elements larger than size = 1
   TDynamicArraySetDataExpr = class(TDynamicArraySetExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // Associative array x[key] for expressions
   TAssociativeArrayGetExpr = class (TPosDataExpr)
      protected
         FBaseExpr : TDataExpr;
         FKeyExpr : TTypedExpr;
         FElementSize : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;
         function GetBaseType : TTypeSymbol; override;

      public
         constructor Create(const aScriptPos: TScriptPos;
                            baseExpr: TDataExpr; keyExpr: TTypedExpr;
                            arraySymbol : TAssociativeArraySymbol);
         destructor Destroy; override;

         function IsWritable : Boolean; override;

         function SameDataExpr(expr : TTypedExpr) : Boolean; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         procedure EvalAsString(exec : TdwsExecution; var result : String); override;

         property BaseExpr : TDataExpr read FBaseExpr;
         property KeyExpr : TTypedExpr read FKeyExpr;
   end;

   // Associative array x[key] when key is a value
   TAssociativeArrayValueKeyGetExpr = class (TAssociativeArrayGetExpr)
      public
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         procedure EvalAsString(exec : TdwsExecution; var result : String); override;
   end;

   TAssociativeArraySetExpr = class (TNoResultExpr)
      protected
         FBaseExpr : TDataExpr;
         FKeyExpr : TTypedExpr;
         FValueExpr : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const aScriptPos: TScriptPos; baseExpr: TDataExpr;
                            keyExpr, valueExpr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property BaseExpr : TDataExpr read FBaseExpr;
         property KeyExpr : TTypedExpr read FKeyExpr;
         property ValueExpr : TTypedExpr read FValueExpr;
   end;

   /// Associative array x[key] := v when key and v are values
   TAssociativeArrayValueSetExpr = class (TAssociativeArraySetExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TAssociativeArrayContainsKeyExpr = class (TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // returns a dynamic array
   TDynamicArrayDataExpr = class(TPosDataExpr)
      public
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
   end;

   // new array[length,...]
   TNewArrayExpr = class(TDynamicArrayDataExpr)
      private
         FLengthExprs : TTightList;
         FTyps : TTightList;

         function GetLengthExpr(idx : Integer) : TTypedExpr; inline;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            elementTyp : TTypeSymbol); overload;
         constructor Create(const scriptPos: TScriptPos; arrayTyp : TDynamicArraySymbol); overload;
         destructor Destroy; override;

         procedure EvalAsScriptDynArray(exec : TdwsExecution; var Result : IScriptDynArray); override;

         procedure AddLengthExpr(expr : TTypedExpr; indexTyp : TTypeSymbol);
         property LengthExpr[idx : Integer] : TTypedExpr read GetLengthExpr;
         property LengthExprCount : Integer read FLengthExprs.FCount;
   end;

   // Pseudo-method for dynamic and associative arrays
   TArrayPseudoMethodExpr = class(TNoResultExpr)
      private
         FBaseExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const scriptPos: TScriptPos; aBase : TTypedExpr);
         destructor Destroy; override;

         property BaseExpr : TTypedExpr read FBaseExpr;
   end;

   // SetLength of dynamic array
   TArraySetLengthExpr = class sealed (TArrayPseudoMethodExpr)
      private
         FLengthExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const scriptPos: TScriptPos; aBase, aLength : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function SpecializeProgramExpr(const context : ISpecializationContext) : TProgramExpr; override;

         property LengthExpr : TTypedExpr read FLengthExpr;
   end;

   // TypedExpr for dynamic array
   TArrayTypedExpr = class(TTypedExpr)
      private
         FBaseExpr : TTypedExpr;
         FScriptPos : TScriptPos;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBaseExpr : TTypedExpr);
         destructor Destroy; override;

         function ScriptPos : TScriptPos; override;

         property BaseExpr : TTypedExpr read FBaseExpr;
   end;

   // TypedExpr for dynamic array that returns the array (for fluent-style)
   TArrayTypedFluentExpr = class(TArrayTypedExpr)
      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBaseExpr : TTypedExpr);

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // Swap two elements of a dynamic array
   TArraySwapExpr = class(TArrayTypedFluentExpr)
      private
         FIndex1Expr : TTypedExpr;
         FIndex2Expr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase, aIndex1, aIndex2 : TTypedExpr);
         destructor Destroy; override;

         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         property Index1Expr : TTypedExpr read FIndex1Expr;
         property Index2Expr : TTypedExpr read FIndex2Expr;
   end;

   // Sort a dynamic array
   TArraySortExpr = class(TArrayTypedFluentExpr)
      private
         FCompareExpr : TFuncPtrExpr;
         FLeft, FRight : TDataSymbol;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase : TTypedExpr; aCompare : TFuncPtrExpr);
         destructor Destroy; override;

         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         property CompareExpr : TFuncPtrExpr read FCompareExpr write FCompareExpr;
   end;

   // Sort a dynamic array (natural order)
   TArraySortNaturalExpr = class(TArrayTypedFluentExpr)
      public
         procedure SetCompareMethod(var qs : TQuickSort; dyn : TScriptDynamicValueArray); virtual;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;
   end;

   TArraySortNaturalStringExpr = class(TArraySortNaturalExpr)
      public
         procedure SetCompareMethod(var qs : TQuickSort; {%H-}dyn : TScriptDynamicValueArray); override;
   end;

   TArraySortNaturalIntegerExpr = class(TArraySortNaturalExpr)
      public
         procedure SetCompareMethod(var qs : TQuickSort; {%H-}dyn : TScriptDynamicValueArray); override;
   end;

   TArraySortNaturalFloatExpr = class(TArraySortNaturalExpr)
      public
         procedure SetCompareMethod(var qs : TQuickSort; {%H-}dyn : TScriptDynamicValueArray); override;
   end;

   // Returns the storage to use for the next call, normal sequence is
   // - first call with number of items as parameter (<0 if unknown), will return storage for 1st item
   // - second call with n=0, after having stored the first item, returns storage for second item (can be same)
   // - etc
   TArrayDataEnumeratorCallback = reference to function(n : Integer) : PVariant;
   TArrayDataEnumeratorCallbackString = reference to function(n : Integer) : PString;

   // Map a dynamic array
   TArrayMapExpr = class sealed (TArrayTypedExpr)
      private
         FMapFuncExpr : TFuncPtrExpr;
         FItem : TDataSymbol;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         procedure BaseAsCallback(exec : TdwsExecution; const initial, callback : TArrayDataEnumeratorCallback);

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase : TTypedExpr; aMapFunc : TFuncPtrExpr);
         destructor Destroy; override;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         procedure EvalAsCallback(exec : TdwsExecution; const initial, callback : TArrayDataEnumeratorCallback);
         procedure EvalAsCallbackString(exec : TdwsExecution; const initial, callback : TArrayDataEnumeratorCallbackString);

         property MapFuncExpr : TFuncPtrExpr read FMapFuncExpr write FMapFuncExpr;
   end;

   // Filter a dynamic array
   TArrayFilterExpr = class(TArrayTypedExpr)
      private
         FFilterFuncExpr : TFuncPtrExpr;
         FItem : TDataSymbol;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase : TTypedExpr; aFilterFunc : TFuncPtrExpr);
         destructor Destroy; override;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         property FilterFuncExpr : TFuncPtrExpr read FFilterFuncExpr write FFilterFuncExpr;
   end;

   // Reverse a dynamic array
   TArrayReverseExpr = class(TArrayTypedFluentExpr)
      public
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;
   end;

   // Add an item to a dynamic array
   TArrayAddExpr = class sealed (TArrayPseudoMethodExpr)
      private
         FArgs : TTightList;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetItemExpr(idx : Integer) : TTypedExpr;

         procedure DoEval(exec : TdwsExecution; var base : IScriptDynArray);

      public
         constructor Create(const scriptPos: TScriptPos;
                            aBase :  TTypedExpr; argExprs : TTypedExprList);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         procedure AddArg(expr : TTypedExpr);
         procedure ExtractArgs(destination : TArrayAddExpr);

         function SpecializeProgramExpr(const context : ISpecializationContext) : TProgramExpr; override;

         property ArgExpr[idx : Integer] : TTypedExpr read GetItemExpr;
         property ArgCount : Integer read FArgs.FCount;
   end;

   // base class for dynamic array expr that return a value
   TArrayDataExpr = class(TPosDataExpr)
      private
         FBaseExpr : TTypedExpr;
         FResultAddr : Integer;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetBaseDynArray(exec : TdwsExecution) : TScriptDynamicArray;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase :  TTypedExpr);
         destructor Destroy; override;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;
   end;

   // Peek the last value of a dynamic array
   TArrayPeekExpr = class (TArrayDataExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function  SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr; override;
   end;

   // Pop the last value of a dynamic array
   TArrayPopExpr = class sealed (TArrayPeekExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function  SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr; override;
   end;

   // Delete one or N elements of a dynamic array
   TArrayDeleteExpr = class(TArrayPseudoMethodExpr)
      private
         FIndexExpr : TTypedExpr;
         FCountExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const scriptPos: TScriptPos;
                            aBase, aIndex, aCount : TTypedExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property IndexExpr : TTypedExpr read FIndexExpr;
         property CountExpr : TTypedExpr read FCountExpr;
   end;

   // Shallow-copy of a subset of an array
   TArrayCopyExpr = class(TArrayTypedExpr)
      private
         FIndexExpr : TTypedExpr;
         FCountExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase, aIndex, aCount : TTypedExpr);
         destructor Destroy; override;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         property IndexExpr : TTypedExpr read FIndexExpr;
         property CountExpr : TTypedExpr read FCountExpr;
   end;

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

   TDynamicArrayIndexOfMethod = function (exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer of object;

   TDynamicArrayIndexOfExpr = class(TArrayIndexOfExpr)
      private
         FMethod : TDynamicArrayIndexOfMethod;

      protected
         function DoEvalValue(exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer;
         function DoEvalString(exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer;
         function DoEvalFuncPtr(exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer;

      public
         constructor Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            aBase : TTypedExpr; aItem : TTypedExpr; aFromIndex : TTypedExpr); override;

         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TDynamicArrayIndexOfDataExpr = class(TArrayIndexOfExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TDynamicArrayIndexOfIntegerExpr = class(TArrayIndexOfExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TStaticArrayIndexOfExpr = class(TArrayIndexOfExpr)
      private
         FForceZeroBased : Boolean;

      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;

         property ForceZeroBased : Boolean read FForceZeroBased write FForceZeroBased;
   end;

   // Remove an element in a dynamic array (shallow comparison)
   TArrayRemoveExpr = class(TDynamicArrayIndexOfExpr)
      public
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // Insert an element at a given index of a dynamic array
   TArrayInsertExpr = class(TArrayPseudoMethodExpr)
      private
         FIndexExpr : TTypedExpr;
         FItemExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const scriptPos: TScriptPos;
                            aBase, aIndex : TTypedExpr; aItem : TTypedExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property IndexExpr : TTypedExpr read FIndexExpr;
         property ItemExpr : TTypedExpr read FItemExpr;
   end;

   // Move an element from one index to another, shifting other items in the procees
   TArrayMoveExpr = class(TArrayPseudoMethodExpr)
      private
         FOriginIndexExpr : TTypedExpr;
         FDestinationIndexExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(const scriptPos: TScriptPos;
                            aBase, anOriginIndex, aDestinationIndex : TTypedExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property OriginIndexExpr : TTypedExpr read FOriginIndexExpr;
         property DestinationIndexExpr : TTypedExpr read FDestinationIndexExpr;
   end;

   // Concatenates two or more arrays
   TArrayConcatExpr = class(TDynamicArrayDataExpr)
      private
         FAddExpr : TArrayAddExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetArgs(index : Integer) : TTypedExpr; inline;

      public
         constructor Create(const scriptPos : TScriptPos; aTyp: TDynamicArraySymbol);
         destructor Destroy; override;

         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         procedure AddArg(arg : TTypedExpr);

         property AddExpr : TArrayAddExpr read FAddExpr;
         property ArgExpr[index : Integer] : TTypedExpr read GetArgs;
         function ArgCount : Integer; inline;
   end;

   TAssociativeArrayClearExpr = class (TArrayPseudoMethodExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TAssociativeArrayDeleteExpr = class (TTypedExpr)
      private
         FBaseExpr : TTypedExpr;
         FKeyExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(context : TdwsCompilerContext; aBase, aKey : TTypedExpr);
         destructor Destroy; override;

         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override; final;
         procedure EvalNoResult(exec : TdwsExecution); override; final;

         property BaseExpr : TTypedExpr read FBaseExpr;
         property KeyExpr : TTypedExpr read FKeyExpr;
   end;

   TAssociativeArrayKeysExpr = class (TUnaryOpExpr)
      public
         constructor Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr); override;

         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override; final;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsStrings, dwsConstExprs, dwsArrayElementContext, dwsCoreExprs,
   dwsCompilerUtils, dwsSpecializationContext, dwsStack, dwsExprList;

type
   // this needs to be in a helper (or more precisely implemented at the top of this unit)
   // otherwise inlining won't work
   TBoundsHelper = class helper for TProgramExpr
      procedure BoundsCheck(exec : TdwsExecution; aLength, index : Integer); inline;
      procedure BoundsCheckFailed(exec : TdwsExecution; index : Integer);
   end;

// BoundsCheck
//
procedure TBoundsHelper.BoundsCheck(exec : TdwsExecution; aLength, index : Integer);
begin
   if Cardinal(index)>=Cardinal(aLength) then
      BoundsCheckFailed(exec, index);
end;

// BoundsCheckFailed
//
procedure TBoundsHelper.BoundsCheckFailed(exec : TdwsExecution; index : Integer);
begin
   if index<0 then
      RaiseLowerExceeded(exec, index)
   else RaiseUpperExceeded(exec, index);
end;

// ------------------
// ------------------ TArrayLengthExpr ------------------
// ------------------

// Create
//
constructor TArrayLengthExpr.Create(context : TdwsCompilerContext; const aScriptPos : TScriptPos; expr : TTypedExpr; captureExpr : Boolean);
begin
   inherited Create(context, aScriptPos, expr);
   FCapture:=captureExpr;
end;

// Destroy
//
destructor TArrayLengthExpr.Destroy;
begin
   if not FCapture then
      Expr:=nil;
   inherited;
end;

// SpecializeTypedExpr
//
function TArrayLengthExpr.SpecializeTypedExpr(const context : ISpecializationContext) : TTypedExpr;
begin
   Result := TArrayLengthExprClass(ClassType).Create(
      CompilerContextFromSpecialization(context), FScriptPos,
      Expr.SpecializeTypedExpr(context), True
   );
   TArrayLengthExpr(Result).Delta := Delta;
end;

// EvalAsInteger
//
function TArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   dyn : IScriptDynArray;
begin
   FExpr.EvalAsScriptDynArray(exec, dyn);
   Result:=dyn.ArrayLength+FDelta
end;

// ------------------
// ------------------ TOpenArrayLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TOpenArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=TDataExpr(FExpr).DataPtr[exec].DataLength+FDelta;
end;

// ------------------
// ------------------ TAssociativeArrayLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TAssociativeArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   aa : IScriptAssociativeArray;
begin
   Expr.EvalAsScriptAssociativeArray(exec, aa);
   Result:=aa.Count;
end;

// ------------------
// ------------------ TArrayTypedExpr ------------------
// ------------------

// Create
//
constructor TArrayTypedExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                   aBaseExpr : TTypedExpr);
begin
   inherited Create;
   FScriptPos:=scriptPos;
   FBaseExpr:=aBaseExpr;
end;

// Destroy
//
destructor TArrayTypedExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
end;

// ScriptPos
//
function TArrayTypedExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// ------------------
// ------------------ TArrayTypedFluentExpr ------------------
// ------------------

// Create
//
constructor TArrayTypedFluentExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                         aBaseExpr : TTypedExpr);
begin
   inherited Create(context, scriptPos, aBaseExpr);
   Typ := aBaseExpr.Typ;
end;

// EvalAsVariant
//
procedure TArrayTypedFluentExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
   Result := dyn;
end;

// EvalNoResult
//
procedure TArrayTypedFluentExpr.EvalNoResult(exec : TdwsExecution);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
end;

// ------------------
// ------------------ TDynamicArrayDataExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TDynamicArrayDataExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
   Result:=dyn;
end;

// GetDataPtr
//
procedure TDynamicArrayDataExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   data : TData;
begin
   SetLength(data, 1);
   EvalAsVariant(exec, data[0]);
   result:=exec.Stack.CreateDataContext(data, 0);
end;

// ------------------
// ------------------ TNewArrayExpr ------------------
// ------------------

// Create
//
constructor TNewArrayExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                 elementTyp : TTypeSymbol);
begin
   inherited Create(scriptPos, TDynamicArraySymbol.Create('', elementTyp, context.TypInteger));
   FTyps.Add(FTyp);
end;

// Create
//
constructor TNewArrayExpr.Create(const scriptPos: TScriptPos; arrayTyp : TDynamicArraySymbol);
begin
   inherited Create(scriptPos, arrayTyp);
end;

// Destroy
//
destructor TNewArrayExpr.Destroy;
begin
   inherited;
   FTyps.Clean;
   FLengthExprs.Clean;
end;

// EvalAsScriptDynArray
//
procedure TNewArrayExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);

   function CreateDimension(d : Integer) : IScriptDynArray;
   var
      i : Integer;
      n : Int64;
   begin
      n:=LengthExpr[d].EvalAsInteger(exec);
      if n<0 then
         RaiseScriptError(exec, EScriptOutOfBounds.CreatePosFmt(ScriptPos, RTE_ArrayLengthIncorrectForDimension, [n, d]));
      Result := TScriptDynamicArray.CreateNew(TDynamicArraySymbol(FTyps.List[FTyps.Count-1-d]).Typ);
      Result.ArrayLength:=n;
      Inc(d);
      if d<LengthExprCount then begin
         for i:=0 to n-1 do
            Result.SetAsInterface(i, CreateDimension(d));
      end;
   end;

begin
   if LengthExprCount>0 then
      result:=CreateDimension(0)
   else result:=TScriptDynamicArray.CreateNew(Typ.Typ);
end;

// AddLengthExpr
//
procedure TNewArrayExpr.AddLengthExpr(expr : TTypedExpr; indexTyp : TTypeSymbol);
begin
   if FLengthExprs.Count>0 then begin
      FTyp:=TDynamicArraySymbol.Create('', FTyp, indexTyp);
      FTyps.Add(FTyp);
   end;
   FLengthExprs.Add(expr);
end;

// GetLengthExpr
//
function TNewArrayExpr.GetLengthExpr(idx : Integer) : TTypedExpr;
begin
   Result:=TTypedExpr(FLengthExprs.List[idx]);
end;

// GetSubExpr
//
function TNewArrayExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=TExprBase(FLengthExprs.List[i]);
end;

// GetSubExprCount
//
function TNewArrayExpr.GetSubExprCount : Integer;
begin
   Result:=FLengthExprs.Count;
end;

// ------------------
// ------------------ TArrayExpr ------------------
// ------------------

// Create
//
constructor TArrayExpr.Create(const aScriptPos: TScriptPos;
                              BaseExpr: TDataExpr; IndexExpr: TTypedExpr;
                              arraySymbol : TArraySymbol);
begin
   inherited Create(aScriptPos, arraySymbol.Typ);
   FBaseExpr := BaseExpr;
   FIndexExpr := IndexExpr;
   FElementSize := FTyp.Size; // Necessary because of arrays of records!
end;

// Destroy
//
destructor TArrayExpr.Destroy;
begin
   FBaseExpr.Free;
   FIndexExpr.Free;
   inherited;
end;

// Orphan
//
procedure TArrayExpr.Orphan(context : TdwsCompilerContext);
begin
   if FBaseExpr <> nil then begin
      FBaseExpr.Orphan(context);
      FBaseExpr := nil;
   end;
   if FIndexExpr <> nil then begin
      FIndexExpr.Orphan(context);
      FIndexExpr := nil;
   end;
   DecRefCount;
end;

// IsWritable
//
function TArrayExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

// SameDataExpr
//
function TArrayExpr.SameDataExpr(expr : TTypedExpr) : Boolean;
begin
   Result:=    (ClassType=expr.ClassType)
           and BaseExpr.SameDataExpr(TArrayExpr(expr).BaseExpr)
           and IndexExpr.SameDataExpr(TArrayExpr(expr).IndexExpr);
end;

// GetSubExpr
//
function TArrayExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=FIndexExpr;
end;

// GetSubExprCount
//
function TArrayExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// GetBaseType
//
function TArrayExpr.GetBaseType : TTypeSymbol;
begin
   Result:=FTyp;
end;

// ------------------
// ------------------ TStaticArrayExpr ------------------
// ------------------

// Create
//
constructor TStaticArrayExpr.Create(const aScriptPos: TScriptPos;
                                    baseExpr : TDataExpr; indexExpr : TTypedExpr;
                                    arraySymbol : TStaticArraySymbol);
begin
   inherited Create(aScriptPos, BaseExpr, IndexExpr, arraySymbol);
   FLowBound:=arraySymbol.LowBound;
   FCount:=arraySymbol.HighBound-arraySymbol.LowBound+1;
end;

// GetIsConstant
//
function TStaticArrayExpr.GetIsConstant : Boolean;
begin
   Result:=BaseExpr.IsConstant and IndexExpr.IsConstant;
end;

// Optimize
//
function TStaticArrayExpr.Optimize(context : TdwsCompilerContext) : TProgramExpr;

   function DoOptimize(exec : TdwsExecution) : TProgramExpr;
   var
      v : Variant;
      dc : IDataContext;
   begin
      if Typ.Size=1 then begin
         EvalAsVariant(exec, v);
         Result := TConstExpr.Create(Typ, v);
      end else begin
         dc := DataPtr[exec];
         Result := TConstExpr.Create(Typ, dc.AsPData^, dc.Addr);
      end;
      Orphan(context);
   end;

begin
   if IsConstant then
      Result := DoOptimize(context.Execution)
   else Result := Self;
end;

// AssignExpr
//
procedure TStaticArrayExpr.AssignExpr(exec : TdwsExecution; expr : TTypedExpr);
var
   arrayData : IDataContext;
begin
   FBaseExpr.GetDataPtr(exec, arrayData);
   expr.EvalAsVariant(exec, arrayData.AsPVariant(GetIndex(exec))^);
end;

// AssignValueAsInteger
//
procedure TStaticArrayExpr.AssignValueAsInteger(exec : TdwsExecution; const value : Int64);
begin
   FBaseExpr.DataPtr[exec].AsInteger[GetIndex(exec)]:=value;
end;

// AssignValueAsBoolean
//
procedure TStaticArrayExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   FBaseExpr.DataPtr[exec].AsBoolean[GetIndex(exec)]:=value;
end;

// AssignValueAsFloat
//
procedure TStaticArrayExpr.AssignValueAsFloat(exec : TdwsExecution; const value : Double);
begin
   FBaseExpr.DataPtr[exec].AsFloat[GetIndex(exec)]:=value;
end;

// AssignValueAsString
//
procedure TStaticArrayExpr.AssignValueAsString(exec : TdwsExecution; const value: String);
begin
   FBaseExpr.DataPtr[exec].AsString[GetIndex(exec)]:=value;
end;

// EvalAsInteger
//
function TStaticArrayExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   dc : IDataContext;
begin
   FBaseExpr.GetDataPtr(exec, dc);
   Result := dc.AsInteger[GetIndex(exec)];
end;

// EvalAsFloat
//
function TStaticArrayExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   dc : IDataContext;
begin
   FBaseExpr.GetDataPtr(exec, dc);
   Result := dc.AsFloat[GetIndex(exec)];
end;

// EvalAsBoolean
//
function TStaticArrayExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FBaseExpr.DataPtr[exec].AsBoolean[GetIndex(exec)];
end;

// EvalAsVariant
//
procedure TStaticArrayExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   FBaseExpr.DataPtr[exec].EvalAsVariant(GetIndex(exec), result);
end;

// EvalAsString
//
procedure TStaticArrayExpr.EvalAsString(exec : TdwsExecution; var result : String);
begin
   FBaseExpr.DataPtr[exec].EvalAsString(GetIndex(exec), Result);
end;

// GetDataPtr
//
procedure TStaticArrayExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   FBaseExpr.GetDataPtr(exec, result);
   result.CreateOffset(GetIndex(exec), result);
end;

// GetIndex
//
function TStaticArrayExpr.GetIndex(exec : TdwsExecution) : Integer;
begin
   // Get index
   Result:=FIndexExpr.EvalAsInteger(exec)-FLowBound;

   if Cardinal(Result)>=Cardinal(FCount) then begin
      if Result>=FCount then
         RaiseUpperExceeded(exec, Result+FLowBound)
      else RaiseLowerExceeded(exec, Result+FLowBound);
   end;

   Result:=Result*FElementSize;
end;

// ------------------
// ------------------ TStaticArrayBoolExpr ------------------
// ------------------

// GetIndex
//
function TStaticArrayBoolExpr.GetIndex(exec : TdwsExecution) : Integer;
begin
   if FIndexExpr.EvalAsBoolean(exec) then
      Result:=FElementSize
   else Result:=0;
end;

// ------------------
// ------------------ TOpenArrayExpr ------------------
// ------------------

// GetDataPtr
//
procedure TOpenArrayExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   index : Integer;
begin
   index := FIndexExpr.EvalAsInteger(exec);

   Result := FBaseExpr.DataPtr[exec];

   BoundsCheck(exec, Result.DataLength, index);

   result.CreateOffset(index, result);
end;

// IsWritable
//
function TOpenArrayExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TDynamicArrayExpr ------------------
// ------------------

// GetDataPtr
//
procedure TDynamicArrayExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   CreateArrayElementDataContext(exec, Result);
end;

// EvalItem
//
function TDynamicArrayExpr.EvalItem(exec : TdwsExecution; var dyn : IScriptDynArray) : PVariant;
var
   dynArray : TScriptDynamicArray;
   index : Integer;
begin
   FBaseExpr.EvalAsScriptDynArray(exec, dyn);
   dynArray:=TScriptDynamicArray(dyn.GetSelf);

   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dynArray.ArrayLength, index);

   Result:=dynArray.AsPVariant(index*FElementSize);
end;

// EvalAsInteger
//
function TDynamicArrayExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   dyn : IScriptDynArray;
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec, dyn));
   if p.VType=varInt64 then
      Result:=p.VInt64
   else VariantToInt64(PVariant(p)^, Result);
end;

// EvalAsFloat
//
function TDynamicArrayExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   dyn : IScriptDynArray;
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec, dyn));
   if p.VType=varDouble then
      Result:=p.VDouble
   else Result:=VariantToFloat(PVariant(p)^);
end;

// EvalAsVariant
//
procedure TDynamicArrayExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   dyn : IScriptDynArray;
begin
   VarCopySafe(result, EvalItem(exec, dyn)^);
end;

// EvalAsString
//
procedure TDynamicArrayExpr.EvalAsString(exec : TdwsExecution; var result : String);
var
   dyn : IScriptDynArray;
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec, dyn));
   {$ifdef FPC}
   if p.VType=varString then
      Result:=String(p.VString)
   {$else}
   if p.VType=varUString then
      Result:=String(p.VUString)
   {$endif}
   else VariantToString(PVariant(p)^, Result);
end;

// SpecializeDataExpr
//
function TDynamicArrayExpr.SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr;
begin
   Result := TDynamicArrayExpr.Create(
      ScriptPos,
      BaseExpr.SpecializeDataExpr(context), IndexExpr.SpecializeTypedExpr(context),
      context.SpecializeType(BaseExpr.Typ) as TArraySymbol
      );
end;

// CreateArrayElementDataContext
//
procedure TDynamicArrayExpr.CreateArrayElementDataContext(
      exec : TdwsExecution; var result : IDataContext);
var
   dyn : IScriptDynArray;
   index : Integer;
begin
   FBaseExpr.EvalAsScriptDynArray(exec, dyn);

   index := IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dyn.ArrayLength, index);

   result := TArrayElementDataContext.Create(dyn, index);
end;

// ------------------
// ------------------ TDynamicArrayVarExpr ------------------
// ------------------

// EvalItem
//
function TDynamicArrayVarExpr.EvalItem(exec : TdwsExecution) : PVariant;
var
   pIDyn : PIUnknown;
   dynArray : TScriptDynamicArray;
   index : Integer;
begin
   pIDyn := exec.Stack.PointerToInterfaceValue_BaseRelative(TObjectVarExpr(FBaseExpr).StackAddr);
   dynArray := TScriptDynamicArray(IScriptDynArray(pIDyn^).GetSelf);

   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dynArray.ArrayLength, index);

   Result:=dynArray.AsPVariant(index*FElementSize);
end;

// EvalAsInteger
//
function TDynamicArrayVarExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec));
   if p.VType=varInt64 then
      Result:=p.VInt64
   else VariantToInt64(PVariant(p)^, Result);
end;

// EvalAsFloat
//
function TDynamicArrayVarExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec));
   if p.VType=varDouble then
      Result:=p.VDouble
   else Result:=VariantToFloat(PVariant(p)^);
end;

// EvalAsString
//
procedure TDynamicArrayVarExpr.EvalAsString(exec : TdwsExecution; var result : String);
var
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec));
   {$ifdef FPC}
   if p.VType=varString then
      Result:=String(p.VString)
   {$else}
   if p.VType=varUString then
      Result:=String(p.VUString)
   {$endif}
   else VariantToString(PVariant(p)^, Result);
end;

// SpecializeDataExpr
//
function TDynamicArrayVarExpr.SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr;
begin
   Result := TDynamicArrayVarExpr.Create(
      ScriptPos,
      BaseExpr.SpecializeDataExpr(context), IndexExpr.SpecializeTypedExpr(context),
      context.SpecializeType(BaseExpr.Typ) as TArraySymbol
      );
end;

// ------------------
// ------------------ TDynamicArraySetExpr ------------------
// ------------------

// Create
//
constructor TDynamicArraySetExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                            arrayExpr, indexExpr, valueExpr : TTypedExpr);
begin
   inherited Create(scriptPos);
   FArrayExpr:=arrayExpr;
   FIndexExpr:=indexExpr;
   FValueExpr:=valueExpr;
end;

// Destroy
//
destructor TDynamicArraySetExpr.Destroy;
begin
   inherited;
   FArrayExpr.Free;
   FIndexExpr.Free;
   FValueExpr.Free;
end;

// EvalNoResult
//
procedure TDynamicArraySetExpr.EvalNoResult(exec : TdwsExecution);
var
   dynArray : TScriptDynamicArray;
   index : Integer;
   base : IScriptDynArray;
begin
   FArrayExpr.EvalAsScriptDynArray(exec, base);
   dynArray:=TScriptDynamicArray(base.GetSelf);
   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dynArray.ArrayLength, index);
   ValueExpr.EvalAsVariant(exec, dynArray.AsPVariant(index)^);
end;

// SpecializeProgramExpr
//
function TDynamicArraySetExpr.SpecializeProgramExpr(const context : ISpecializationContext) : TProgramExpr;
begin
   Result := CreateDynamicArraySetExpr(
      CompilerContextFromSpecialization(context),
      FScriptPos,
      ArrayExpr.SpecializeTypedExpr(context),
      IndexExpr.SpecializeIntegerExpr(context),
      ValueExpr.SpecializeTypedExpr(context)
   );
end;

// GetSubExpr
//
function TDynamicArraySetExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FArrayExpr;
      1 : Result:=FIndexExpr
   else
      Result:=FValueExpr;
   end;
end;

// GetSubExprCount
//
function TDynamicArraySetExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TDynamicArraySetVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDynamicArraySetVarExpr.EvalNoResult(exec : TdwsExecution);
var
   dyn : IScriptDynArray;
   index : Integer;
   v : Variant;
begin
   ArrayExpr.EvalAsScriptDynArray(exec, dyn);
   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dyn.ArrayLength, index);
   ValueExpr.EvalAsVariant(exec, v);
   dyn.SetAsVariant(index, v);
end;

// ------------------
// ------------------ TDynamicArraySetDataExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDynamicArraySetDataExpr.EvalNoResult(exec : TdwsExecution);
var
   dynArray : TScriptDynamicArray;
   index : Integer;
   base : IScriptDynArray;
   dataExpr : TDataExpr;
begin
   FArrayExpr.EvalAsScriptDynArray(exec, base);
   dynArray:=TScriptDynamicArray(base.GetSelf);
   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dynArray.ArrayLength, index);

   dataExpr := (ValueExpr as TDataExpr);
   dynArray.WriteData(index*dynArray.ElementSize, dataExpr.DataPtr[exec], dynArray.ElementSize);
end;

// ------------------
// ------------------ TAssociativeArrayGetExpr ------------------
// ------------------

// Create
//
constructor TAssociativeArrayGetExpr.Create(const aScriptPos: TScriptPos;
                            baseExpr: TDataExpr; keyExpr: TTypedExpr;
                            arraySymbol : TAssociativeArraySymbol);
begin
   inherited Create(aScriptPos, arraySymbol.Typ);
   FBaseExpr := baseExpr;
   FKeyExpr := KeyExpr;
   FElementSize := FTyp.Size; // Necessary because of arrays of records!
end;

// Destroy
//
destructor TAssociativeArrayGetExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
   FKeyExpr.Free;
end;

// GetSubExpr
//
function TAssociativeArrayGetExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=FKeyExpr;
end;

// GetSubExprCount
//
function TAssociativeArrayGetExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// GetBaseType
//
function TAssociativeArrayGetExpr.GetBaseType : TTypeSymbol;
begin
   Result:=FTyp;
end;

// IsWritable
//
function TAssociativeArrayGetExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

// SameDataExpr
//
function TAssociativeArrayGetExpr.SameDataExpr(expr : TTypedExpr) : Boolean;
begin
   Result:=    (ClassType=expr.ClassType)
           and BaseExpr.SameDataExpr(TAssociativeArrayGetExpr(expr).BaseExpr)
           and KeyExpr.SameDataExpr(TAssociativeArrayGetExpr(expr).KeyExpr);
end;

// GetDataPtr
//
procedure TAssociativeArrayGetExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   base : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);

   TScriptAssociativeArray(base.GetSelf).GetDataPtr(exec, KeyExpr, result);
end;

// EvalAsInteger
//
function TAssociativeArrayGetExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   Result := TScriptAssociativeArray(base.GetSelf).GetDataAsInteger(exec, KeyExpr);
end;

// EvalAsBoolean
//
function TAssociativeArrayGetExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   base : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   Result := TScriptAssociativeArray(base.GetSelf).GetDataAsBoolean(exec, KeyExpr);
end;

// EvalAsString
//
procedure TAssociativeArrayGetExpr.EvalAsString(exec : TdwsExecution; var result : String);
var
   base : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   TScriptAssociativeArray(base.GetSelf).GetDataAsString(exec, KeyExpr, result);
end;

// ------------------
// ------------------ TAssociativeArrayValueKeyGetExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TAssociativeArrayValueKeyGetExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   base : IScriptAssociativeArray;
   key : Variant;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   KeyExpr.EvalAsVariant(exec, key);
   TScriptAssociativeArray(base.GetSelf).GetDataAsVariant(exec, key, result);
end;

// EvalAsInteger
//
function TAssociativeArrayValueKeyGetExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptAssociativeArray;
   key : Variant;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   KeyExpr.EvalAsVariant(exec, key);
   Result := TScriptAssociativeArray(base.GetSelf).GetDataAsInteger(exec, key);
end;

// EvalAsBoolean
//
function TAssociativeArrayValueKeyGetExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   base : IScriptAssociativeArray;
   key : Variant;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   KeyExpr.EvalAsVariant(exec, key);
   Result := TScriptAssociativeArray(base.GetSelf).GetDataAsBoolean(exec, key);
end;

// EvalAsString
//
procedure TAssociativeArrayValueKeyGetExpr.EvalAsString(exec : TdwsExecution; var result : String);
var
   base : IScriptAssociativeArray;
   key : Variant;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   KeyExpr.EvalAsVariant(exec, key);
   TScriptAssociativeArray(base.GetSelf).GetDataAsString(exec, key, result);
end;

// ------------------
// ------------------ TAssociativeArraySetExpr ------------------
// ------------------

// Create
//
constructor TAssociativeArraySetExpr.Create(const aScriptPos: TScriptPos;
                                            baseExpr: TDataExpr; keyExpr, valueExpr: TTypedExpr);
begin
   inherited Create(aScriptPos);
   FBaseExpr:=baseExpr;
   FKeyExpr:=keyExpr;
   FValueExpr:=valueExpr;
end;

// Destroy
//
destructor TAssociativeArraySetExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
   FKeyExpr.Free;
   FValueExpr.Free;
end;

// GetSubExpr
//
function TAssociativeArraySetExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FBaseExpr;
      1 : Result:=FKeyExpr;
   else
      Result:=FValueExpr;
   end;
end;

// GetSubExprCount
//
function TAssociativeArraySetExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// EvalNoResult
//
procedure TAssociativeArraySetExpr.EvalNoResult(exec : TdwsExecution);
var
   aa : TScriptAssociativeArray;
   base : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   aa:=TScriptAssociativeArray(base.GetSelf);
   aa.ReplaceValue(exec, KeyExpr, valueExpr);
end;

// ------------------
// ------------------ TAssociativeArrayValueSetExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssociativeArrayValueSetExpr.EvalNoResult(exec : TdwsExecution);
var
   aa : TScriptAssociativeArray;
   base : IScriptAssociativeArray;
   k, v : Variant;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   KeyExpr.EvalAsVariant(exec, k);
   ValueExpr.EvalAsVariant(exec, v);
   aa := TScriptAssociativeArray(base.GetSelf);
   aa.ReplaceValue(exec, k, v);
end;

// ------------------
// ------------------ TAssociativeArrayContainsKeyExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssociativeArrayContainsKeyExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   base : IScriptAssociativeArray;
begin
   FRight.EvalAsScriptAssociativeArray(exec, base);
   Result := TScriptAssociativeArray(base.GetSelf).ContainsKey(exec, Left);
end;

// ------------------
// ------------------ TArrayPseudoMethodExpr ------------------
// ------------------

// Create
//
constructor TArrayPseudoMethodExpr.Create(const scriptPos: TScriptPos; aBase : TTypedExpr);
begin
   inherited Create(scriptPos);
   FBaseExpr:=aBase;
end;

// Destroy
//
destructor TArrayPseudoMethodExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
end;

// GetSubExpr
//
function TArrayPseudoMethodExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FBaseExpr;
end;

// GetSubExprCount
//
function TArrayPseudoMethodExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TArraySetLengthExpr ------------------
// ------------------

// Create
//
constructor TArraySetLengthExpr.Create(const scriptPos: TScriptPos;
                                       aBase, aLength : TTypedExpr);
begin
   inherited Create(scriptPos, aBase);
   FLengthExpr:=aLength;
end;

// Destroy
//
destructor TArraySetLengthExpr.Destroy;
begin
   inherited;
   FLengthExpr.Free;
end;

// GetSubExpr
//
function TArraySetLengthExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=FLengthExpr;
end;

// GetSubExprCount
//
function TArraySetLengthExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// EvalNoResult
//
procedure TArraySetLengthExpr.EvalNoResult(exec : TdwsExecution);
var
   dyn : IScriptDynArray;
   n : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, dyn);
   n:=LengthExpr.EvalAsInteger(exec);
   if n<0 then
      RaiseScriptError(exec, EScriptOutOfBounds.CreatePosFmt(FScriptPos, RTE_ArrayLengthIncorrect, [n]));
   dyn.ArrayLength:=n;
end;

// SpecializeProgramExpr
//
function TArraySetLengthExpr.SpecializeProgramExpr(const context : ISpecializationContext) : TProgramExpr;
begin
   Result := TArraySetLengthExpr.Create(
      FScriptPos,
      BaseExpr.SpecializeTypedExpr(context),
      LengthExpr.SpecializeTypedExpr(context)
   );
end;

// ------------------
// ------------------ TArraySwapExpr ------------------
// ------------------

// Create
//
constructor TArraySwapExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                  aBase, aIndex1, aIndex2 : TTypedExpr);
begin
   inherited Create(context, scriptPos, aBase);
   FIndex1Expr:=aIndex1;
   FIndex2Expr:=aIndex2;
end;

// Destroy
//
destructor TArraySwapExpr.Destroy;
begin
   inherited;
   FIndex1Expr.Free;
   FIndex2Expr.Free;
end;

// EvalAsScriptDynArray
//
procedure TArraySwapExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   dyn : TScriptDynamicArray;
   i1, i2 : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, result);
   dyn:=TScriptDynamicArray(result.GetSelf);
   i1:=Index1Expr.EvalAsInteger(exec);
   i2:=Index2Expr.EvalAsInteger(exec);
   BoundsCheck(exec, dyn.ArrayLength, i1);
   BoundsCheck(exec, dyn.ArrayLength, i2);
   dyn.Swap(i1, i2);
end;

// GetSubExpr
//
function TArraySwapExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=BaseExpr;
      1 : Result:=Index1Expr;
   else
      Result:=Index2Expr;
   end;
end;

// GetSubExprCount
//
function TArraySwapExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TArraySortComparer ------------------
// ------------------

type
   TArraySortComparer = class
      FExec : TdwsExecution;
      FDyn : TScriptDynamicArray;
      FFunc : TFuncPtrExpr;
      FFuncPointer : IFuncPointer;
      FLeftAddr, FRightAddr : Integer;
      FData : TData;
      constructor Create(exec : TdwsExecution; dyn : TScriptDynamicArray; compareFunc : TFuncPtrExpr);
      function CompareData(index1, index2 : Integer) : Integer;
      function CompareValue(index1, index2 : Integer) : Integer;
   end;

// Create
//
constructor TArraySortComparer.Create(exec : TdwsExecution; dyn : TScriptDynamicArray;
                                      compareFunc : TFuncPtrExpr);
begin
   FExec:=exec;
   FDyn:=dyn;
   FData:=dyn.AsPData^;
   FLeftAddr:=exec.Stack.BasePointer+(compareFunc.Args[0] as TVarExpr).StackAddr;
   FRightAddr:=exec.Stack.BasePointer+(compareFunc.Args[1] as TVarExpr).StackAddr;
   FFunc:=compareFunc;
   compareFunc.EvalAsFuncPointer(exec, FFuncPointer);
end;

// CompareData
//
function TArraySortComparer.CompareData(index1, index2 : Integer) : Integer;
begin
   DWSCopyData(FData, index1*FDyn.ElementSize, FExec.Stack.Data, FLeftAddr, FDyn.ElementSize);
   DWSCopyData(FData, index2*FDyn.ElementSize, FExec.Stack.Data, FRightAddr, FDyn.ElementSize);
   Result:=FFuncPointer.EvalAsInteger(FExec, FFunc);
end;

// CompareValue
//
function TArraySortComparer.CompareValue(index1, index2 : Integer) : Integer;
begin
   VarCopySafe(FExec.Stack.Data[FLeftAddr], FData[index1]);
   VarCopySafe(FExec.Stack.Data[FRightAddr], FData[index2]);
   Result:=FFuncPointer.EvalAsInteger(FExec, FFunc);
end;

// ------------------
// ------------------ TArraySortExpr ------------------
// ------------------

// Create
//
constructor TArraySortExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                  aBase : TTypedExpr; aCompare : TFuncPtrExpr);
var
   elemTyp : TTypeSymbol;
begin
   inherited Create(context, scriptPos, aBase);
   FCompareExpr:=aCompare;
   if aCompare<>nil then begin
      elemTyp:=aCompare.FuncSym.Params[0].Typ;
      FLeft:=TScriptDataSymbol.Create('', elemTyp);
      context.Table.AddSymbol(FLeft);
      FRight:=TScriptDataSymbol.Create('', elemTyp);
      context.Table.AddSymbol(FRight);
      FCompareExpr.AddArg(TVarExpr.CreateTyped(context, FLeft));
      FCompareExpr.AddArg(TVarExpr.CreateTyped(context, FRight));
   end;
end;

// Destroy
//
destructor TArraySortExpr.Destroy;
begin
   FCompareExpr.Free;
   inherited;
end;

// EvalAsScriptDynArray
//
procedure TArraySortExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   dyn : TScriptDynamicValueArray;
   qs : TQuickSort;
   comparer : TArraySortComparer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, result);
   dyn:=TScriptDynamicValueArray(result.GetSelf);
   comparer:=TArraySortComparer.Create(exec, dyn, CompareExpr);
   try
      if dyn.ElementSize>1 then
         qs.CompareMethod:=comparer.CompareData
      else qs.CompareMethod:=comparer.CompareValue;
      qs.SwapMethod:=dyn.Swap;
      qs.Sort(0, dyn.ArrayLength-1);
   finally
      comparer.Free;
   end;
end;


// GetSubExpr
//
function TArraySortExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=BaseExpr
   else Result:=CompareExpr;
end;

// GetSubExprCount
//
function TArraySortExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TArraySortNaturalExpr ------------------
// ------------------

// EvalAsScriptDynArray
//
procedure TArraySortNaturalExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   dyn : TScriptDynamicValueArray;
   qs : TQuickSort;
begin
   BaseExpr.EvalAsScriptDynArray(exec, result);
   dyn:=TScriptDynamicValueArray(result.GetSelf);
   SetCompareMethod(qs, dyn);
   qs.SwapMethod:=dyn.Swap;
   qs.Sort(0, dyn.ArrayLength-1);
end;

// SetCompareMethod
//
procedure TArraySortNaturalExpr.SetCompareMethod(var qs : TQuickSort; dyn : TScriptDynamicValueArray);
begin
   raise Exception.CreateFmt('%s does not yet supports %s', [ClassName, dyn.ClassName]);
end;

// ------------------
// ------------------ TArraySortNaturalStringExpr ------------------
// ------------------

// SetCompareMethod
//
procedure TArraySortNaturalStringExpr.SetCompareMethod(var qs : TQuickSort; dyn : TScriptDynamicValueArray);
begin
   qs.CompareMethod:=dyn.CompareString;
end;

// ------------------
// ------------------ TArraySortNaturalIntegerExpr ------------------
// ------------------

// SetCompareMethod
//
procedure TArraySortNaturalIntegerExpr.SetCompareMethod(var qs : TQuickSort; dyn : TScriptDynamicValueArray);
begin
   qs.CompareMethod:=dyn.CompareInteger;
end;

// ------------------
// ------------------ TArraySortNaturalFloatExpr ------------------
// ------------------

// SetCompareMethod
//
procedure TArraySortNaturalFloatExpr.SetCompareMethod(var qs : TQuickSort; dyn : TScriptDynamicValueArray);
begin
   qs.CompareMethod:=dyn.CompareFloat;
end;

// ------------------
// ------------------ TArrayMapExpr ------------------
// ------------------

// Create
//
constructor TArrayMapExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                 aBase : TTypedExpr; aMapFunc : TFuncPtrExpr);
var
   elemTyp : TTypeSymbol;
   arrayTyp : TDynamicArraySymbol;
begin
   inherited Create(context, scriptPos, aBase);
   FMapFuncExpr:=aMapFunc;
   if aMapFunc<>nil then
      elemTyp:=aMapFunc.Typ
   else elemTyp:=nil;
   if elemTyp=nil then
      elemTyp:=context.TypVariant;
   arrayTyp:=TDynamicArraySymbol.Create('', elemTyp, context.TypInteger);
   context.Table.AddSymbol(arrayTyp);
   Typ:=arrayTyp;

   if aMapFunc<>nil then begin
      elemTyp:=aMapFunc.FuncSym.Params[0].Typ;
      FItem:=TScriptDataSymbol.Create('', elemTyp);
      context.Table.AddSymbol(FItem);
      FMapFuncExpr.AddArg(TVarExpr.CreateTyped(context, FItem));
      FMapFuncExpr.InitializeResultAddr(context.Prog as TdwsProgram);
   end;
end;

// Destroy
//
destructor TArrayMapExpr.Destroy;
begin
   inherited;
   FMapFuncExpr.Free;
end;

// EvalAsVariant
//
procedure TArrayMapExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
   VarCopySafe(Result, dyn);
end;

// EvalAsScriptDynArray
//
procedure TArrayMapExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicValueArray;
   i, itemAddr, elemSize : Integer;
   funcPointer : IFuncPointer;
   newPData, oldPData : PData;
   dc : IDataContext;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   MapFuncExpr.EvalAsFuncPointer(exec, funcPointer);

   dyn:=TScriptDynamicValueArray(base.GetSelf);
   oldPData:=dyn.AsPData;

   result := TScriptDynamicArray.CreateNew(Typ.Typ);
   result.ArrayLength := dyn.ArrayLength;
   newPData := result.AsPData;

   elemSize := result.ElementSize;
   itemAddr:=exec.Stack.BasePointer+FItem.StackAddr;
   for i:=0 to dyn.ArrayLength-1 do begin
      if dyn.ElementSize = 1 then
         exec.Stack.WriteValue(itemAddr, oldPData^[i])
      else dyn.CopyData(i*dyn.ElementSize, exec.Stack.Data, itemAddr, dyn.ElementSize);
      if elemSize = 1 then
         funcPointer.EvalAsVariant(exec, MapFuncExpr, newPData^[i])
      else begin
         dc:=funcPointer.EvalDataPtr(exec,  MapFuncExpr, MapFuncExpr.ResultAddr);
         dc.CopyData(newPData^, i*elemSize, elemSize);
      end;
   end;
end;

// BaseAsCallback
//
procedure TArrayMapExpr.BaseAsCallback(exec : TdwsExecution; const initial, callback : TArrayDataEnumeratorCallback);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicValueArray;
   destPVariant : PVariant;
   i : Integer;
begin
   if BaseExpr is TArrayMapExpr then begin

      TArrayMapExpr(BaseExpr).EvalAsCallback(exec, initial, callback);

   end else begin

      BaseExpr.EvalAsScriptDynArray(exec, base);
      dyn := TScriptDynamicValueArray(base.GetSelf);
      destPVariant := initial(dyn.ArrayLength);
      if dyn.ElementSize = 1 then begin
         i := 0;
         while i < dyn.ArrayLength do begin
            VarCopySafe(destPVariant^, dyn.AsPVariant(i)^);
            destPVariant := callback(i);
            Inc(i);
         end;
      end else begin
         i := 0;
         while i < dyn.ArrayLength do begin
            dyn.CopyData(i*dyn.ElementSize, destPVariant, dyn.ElementSize);
            destPVariant := callback(i);
            Inc(i);
         end;
      end;

   end;
end;

// EvalAsCallback
//
procedure TArrayMapExpr.EvalAsCallback(exec : TdwsExecution; const initial, callback : TArrayDataEnumeratorCallback);
var
   itemAddr, destSize : Integer;
   itemPtr : PVariant;
   funcPointer : IFuncPointer;
   destPVariant : PVariant;
   loopCallback : TArrayDataEnumeratorCallback;
begin
   MapFuncExpr.EvalAsFuncPointer(exec, funcPointer);

   itemAddr := exec.Stack.BasePointer + FItem.StackAddr;
   itemPtr := @exec.Stack.Data[itemAddr];

   destSize := Typ.Typ.Size;
   if destSize = 1 then begin
      loopCallback := function (n : Integer) : PVariant
                      begin
                         funcPointer.EvalAsVariant(exec, MapFuncExpr, destPVariant^);
                         destPVariant := callback(n);
                         Result := itemPtr;
                      end;
   end else begin
      loopCallback := function (n : Integer) : PVariant
                      var
                         dc : IDataContext;
                      begin
                         dc := funcPointer.EvalDataPtr(exec,  MapFuncExpr, MapFuncExpr.ResultAddr);
                         dc.CopyData(destPVariant^, 0, destSize);
                         destPVariant := callback(n);
                         Result := itemPtr;
                      end;
   end;
   BaseAsCallback(exec,
      function (n : Integer) : PVariant
      begin
         destPVariant := initial(n);
         Result := itemPtr;
      end,
      loopCallback
   );
end;

// EvalAsCallbackString
//
procedure TArrayMapExpr.EvalAsCallbackString(exec : TdwsExecution; const initial, callback : TArrayDataEnumeratorCallbackString);
var
   itemAddr : Integer;
   itemPtr : PVariant;
   funcPointer : IFuncPointer;
   destString : PString;
begin
   MapFuncExpr.EvalAsFuncPointer(exec, funcPointer);

   itemAddr := exec.Stack.BasePointer + FItem.StackAddr;
   itemPtr := @exec.Stack.Data[itemAddr];

   BaseAsCallback(exec,
      function (n : Integer) : PVariant
      begin
         Result := itemPtr;
         destString := initial(n);
      end,
      function (n : Integer) : PVariant
      begin
         funcPointer.EvalAsString(exec, MapFuncExpr, destString^);
         Result := itemPtr;
         destString := callback(n);
      end
   );
end;

// GetSubExpr
//
function TArrayMapExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=BaseExpr
   else Result:=MapFuncExpr;
end;

// GetSubExprCount
//
function TArrayMapExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TArrayFilterExpr ------------------
// ------------------

// Create
//
constructor TArrayFilterExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                   aBase : TTypedExpr; aFilterFunc : TFuncPtrExpr);
var
   elemTyp : TTypeSymbol;
begin
   inherited Create(context, scriptPos, aBase);
   FFilterFuncExpr := aFilterFunc;
   Typ := aBase.Typ;

   if aFilterFunc <> nil then begin
      elemTyp := aFilterFunc.FuncSym.Params[0].Typ;
      FItem := TScriptDataSymbol.Create('', elemTyp);
      context.Table.AddSymbol(FItem);
      FFilterFuncExpr.AddArg(TVarExpr.CreateTyped(context, FItem));
      FFilterFuncExpr.InitializeResultAddr(context.Prog as TdwsProgram);
   end;
end;

// Destroy
//
destructor TArrayFilterExpr.Destroy;
begin
   inherited;
   FFilterFuncExpr.Free;
end;

// EvalAsVariant
//
procedure TArrayFilterExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
   VarCopySafe(Result, dyn);
end;

// EvalAsScriptDynArray
//
procedure TArrayFilterExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicValueArray;
   i, k, elementSize, itemAddr : Integer;
   funcPointer : IFuncPointer;
   newPData, oldPData : PData;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   FilterFuncExpr.EvalAsFuncPointer(exec, funcPointer);

   dyn := TScriptDynamicValueArray(base.GetSelf);
   oldPData := dyn.AsPData;

   result := TScriptDynamicArray.CreateNew(dyn.ElementTyp);
   result.ArrayLength := dyn.ArrayLength;
   newPData := result.AsPData;
   elementSize := result.ElementSize;
   k := 0;

   itemAddr := exec.Stack.BasePointer + FItem.StackAddr;
   if elementSize = 1 then begin
      for i := 0 to dyn.ArrayLength-1 do begin
         exec.Stack.WriteValue(itemAddr, oldPData^[i]);
         if funcPointer.EvalAsBoolean(exec, FilterFuncExpr) then begin
            VarCopySafe(newPData^[k], oldPData^[i]);
            Inc(k);
         end;
      end;
   end else begin
      for i := 0 to dyn.ArrayLength-1 do begin
         dyn.CopyData(i*dyn.ElementSize, exec.Stack.Data, itemAddr, dyn.ElementSize);
         if funcPointer.EvalAsBoolean(exec, FilterFuncExpr) then begin
            dyn.CopyData(i*dyn.ElementSize, newPData^, k*elementSize, elementSize);
            Inc(k);
         end;
      end;
   end;

   if k <> result.ArrayLength then
      result.ArrayLength := k;
end;

// GetSubExpr
//
function TArrayFilterExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i = 0 then
      Result := BaseExpr
   else Result := FilterFuncExpr;
end;

// GetSubExprCount
//
function TArrayFilterExpr.GetSubExprCount : Integer;
begin
   Result := 2;
end;

// ------------------
// ------------------ TArrayReverseExpr ------------------
// ------------------

// EvalAsScriptDynArray
//
procedure TArrayReverseExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
begin
   BaseExpr.EvalAsScriptDynArray(exec, result);
   result.Reverse;
end;

// ------------------
// ------------------ TArrayAddExpr ------------------
// ------------------

// Create
//
constructor TArrayAddExpr.Create(const scriptPos: TScriptPos;
                                 aBase :  TTypedExpr; argExprs : TTypedExprList);
var
   i : Integer;
begin
   inherited Create(scriptPos, aBase);
   if argExprs<>nil then
      for i:=0 to argExprs.Count-1 do
         FArgs.Add(argExprs[i]);
end;

// Destroy
//
destructor TArrayAddExpr.Destroy;
begin
   inherited;
   FArgs.Clean;
end;

// DoEval
//
procedure TArrayAddExpr.DoEval(exec : TdwsExecution; var base : IScriptDynArray);

   procedure AddDataArg(dyn : TScriptDynamicArray; arg : TTypedExpr);
   var
      n : Integer;
      argdata : TDataExpr;
   begin
      n := dyn.ArrayLength;
      dyn.ArrayLength := n+1;
      if arg.Typ.Size > 1 then begin
         argData := (arg as TDataExpr);
         dyn.WriteData(n*dyn.ElementSize, argData.DataPtr[exec], dyn.ElementSize);
      end else arg.EvalAsVariant(exec, dyn.AsPVariant(n)^);
   end;

   procedure AddStaticArrayArg(dyn : TScriptDynamicArray; arg : TTypedExpr);
   var
      k, n : Integer;
   begin
      k := arg.Typ.Size div dyn.ElementSize;
      if k > 0 then begin
         n := dyn.ArrayLength;
         dyn.ArrayLength := n + k;
         if arg is TArrayConstantExpr then
            TArrayConstantExpr(arg).EvalToTData(exec, dyn.AsPData^, n*dyn.ElementSize)
         else dyn.WriteData(n*dyn.ElementSize, (arg as TDataExpr).DataPtr[exec], k*dyn.ElementSize);
      end;
   end;

var
   src : IScriptDynArray;
   dyn, dynSrc : TScriptDynamicArray;
   i, n : Integer;
   arg : TTypedExpr;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);

   for i:=0 to FArgs.Count-1 do begin
      arg:=TTypedExpr(FArgs.List[i]);

      if dyn.ElementTyp.IsCompatible(arg.Typ) then begin

         AddDataArg(dyn, arg);

      end else if arg.Typ.ClassType=TDynamicArraySymbol then begin

         arg.EvalAsScriptDynArray(exec, src);
         dynSrc:=(src.GetSelf as TScriptDynamicArray);

         dyn.Concat(dynSrc);

      end else if arg.Typ.UnAliasedTypeIs(TBaseIntegerSymbol) and dyn.ElementTyp.UnAliasedTypeIs(TBaseFloatSymbol)  then begin

         n := dyn.ArrayLength;
         dyn.ArrayLength := n+1;
         dyn.AsFloat[n] := arg.EvalAsFloat(exec);

      end else begin

         Assert(arg.Typ is TStaticArraySymbol);
         AddStaticArrayArg(dyn, arg);

      end;
   end;
end;

// EvalNoResult
//
procedure TArrayAddExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptDynArray;
begin
   DoEval(exec, base);
end;

// AddArg
//
procedure TArrayAddExpr.AddArg(expr : TTypedExpr);
begin
   FArgs.Add(expr);
end;

// ExtractArgs
//
procedure TArrayAddExpr.ExtractArgs(destination : TArrayAddExpr);
var
   i : Integer;
begin
   for i:=0 to FArgs.Count-1 do
      destination.FArgs.Add(FArgs.List[i]);
   FArgs.Clear;
end;

// SpecializeProgramExpr
//
function TArrayAddExpr.SpecializeProgramExpr(const context : ISpecializationContext) : TProgramExpr;
var
   i : Integer;
   specialized : TArrayAddExpr;
   arg : TTypedExpr;
   elemTyp : TTypeSymbol;
begin
   specialized := TArrayAddExpr.Create(ScriptPos, BaseExpr.SpecializeTypedExpr(context), nil);
   Result := specialized;
   if BaseExpr = nil then Exit;
   elemTyp := specialized.BaseExpr.Typ.Typ;
   for i := 0 to ArgCount-1 do begin
      arg := ArgExpr[i].SpecializeTypedExpr(context);
      if (arg <> nil) and (not arg.Typ.IsOfType(elemTyp)) then
         specialized.AddArg(
            CompilerUtils.WrapWithImplicitConversion(
               (context.BaseSymbols as TdwsCompilerContext),
               arg, elemTyp,
               arg.ScriptPos,
               CPE_IncompatibleParameterTypes
            )
         )
      else specialized.AddArg(arg);
   end;
end;

// GetSubExpr
//
function TArrayAddExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=TExprBase(FArgs.List[i-1]);
end;

// GetSubExprCount
//
function TArrayAddExpr.GetSubExprCount : Integer;
begin
   Result:=1+FArgs.Count;
end;

// GetItemExpr
//
function TArrayAddExpr.GetItemExpr(idx : Integer) : TTypedExpr;
begin
   Result:=TTypedExpr(FArgs.List[idx]);
end;

// ------------------
// ------------------ TArrayDataExpr ------------------
// ------------------

// Create
//
constructor TArrayDataExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                            aBase :  TTypedExpr);
begin
   inherited Create(scriptPos, (aBase.Typ as TDynamicArraySymbol).Typ);
   FBaseExpr:=aBase;
   FResultAddr:=context.GetTempAddr(Typ.Size);
end;

// Destroy
//
destructor TArrayDataExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
end;

// GetDataPtr
//
procedure TArrayDataExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   EvalNoResult(exec);
   exec.DataContext_CreateBase(FResultAddr, Result);
end;

// GetSubExpr
//
function TArrayDataExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FBaseExpr;
end;

// GetSubExprCount
//
function TArrayDataExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// GetBaseDynArray
//
function TArrayDataExpr.GetBaseDynArray(exec : TdwsExecution) : TScriptDynamicArray;
var
   base : IScriptDynArray;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   Result:=TScriptDynamicArray(base.GetSelf);
end;

// ------------------
// ------------------ TArrayPeekExpr ------------------
// ------------------

// EvalNoResult
//
procedure TArrayPeekExpr.EvalNoResult(exec : TdwsExecution);
var
   dyn : TScriptDynamicArray;
   idx : Integer;
begin
   dyn := GetBaseDynArray(exec);
   idx := dyn.ArrayLength - 1;
   if idx < 0 then
      RaiseUpperExceeded(exec, 0);

   if dyn.ElementSize = 1 then begin
      dyn.EvalAsVariant(idx, exec.Stack.Data[exec.Stack.BasePointer+FResultAddr]);
   end else begin
      dyn.CopyData(idx*dyn.ElementSize, exec.Stack.Data, exec.Stack.BasePointer+FResultAddr, dyn.ElementSize);
   end;
end;

// SpecializeDataExpr
//
function TArrayPeekExpr.SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr;
begin
   Result := TArrayPeekExpr.Create(
      CompilerContextFromSpecialization(context),
      ScriptPos,
      BaseExpr.SpecializeTypedExpr(context)
   );
end;

// ------------------
// ------------------ TArrayPopExpr ------------------
// ------------------

// EvalNoResult
//
procedure TArrayPopExpr.EvalNoResult(exec : TdwsExecution);
var
   dyn : TScriptDynamicArray;
begin
   inherited EvalNoResult(exec);

   dyn:=GetBaseDynArray(exec);
   dyn.Delete(dyn.ArrayLength-1, 1);
end;

// SpecializeDataExpr
//
function TArrayPopExpr.SpecializeDataExpr(const context : ISpecializationContext) : TDataExpr;
begin
   Result := TArrayPopExpr.Create(
      CompilerContextFromSpecialization(context),
      ScriptPos,
      BaseExpr.SpecializeTypedExpr(context)
   );
end;

// ------------------
// ------------------ TArrayDeleteExpr ------------------
// ------------------

// Create
//
constructor TArrayDeleteExpr.Create(const scriptPos: TScriptPos;
                                    aBase, aIndex, aCount : TTypedExpr);
begin
   inherited Create(scriptPos, aBase);
   FIndexExpr:=aIndex;
   FCountExpr:=aCount;
end;

// Destroy
//
destructor TArrayDeleteExpr.Destroy;
begin
   inherited;
   FIndexExpr.Free;
   FCountExpr.Free;
end;

// EvalNoResult
//
procedure TArrayDeleteExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
   index, count : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);
   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dyn.ArrayLength, index);
   if CountExpr<>nil then begin
      count:=CountExpr.EvalAsInteger(exec);
      if count<0 then
         RaiseScriptError(exec, EScriptError.CreateFmt(RTE_PositiveCountExpected, [count]));
      BoundsCheck(exec, dyn.ArrayLength, index+count-1);
   end else count:=1;
   dyn.Delete(index, count);
end;

// GetSubExpr
//
function TArrayDeleteExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FBaseExpr;
      1 : Result:=FIndexExpr;
   else
      Result:=FCountExpr;
   end;
end;

// GetSubExprCount
//
function TArrayDeleteExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TArrayCopyExpr ------------------
// ------------------

// Create
//
constructor TArrayCopyExpr.Create(context : TdwsCompilerContext; const scriptPos: TScriptPos;
                                  aBase, aIndex, aCount : TTypedExpr);
begin
   inherited Create(context, scriptPos, aBase);
   FTyp:=aBase.Typ;
   FIndexExpr:=aIndex;
   FCountExpr:=aCount;
end;

// Destroy
//
destructor TArrayCopyExpr.Destroy;
begin
   inherited;
   FIndexExpr.Free;
   FCountExpr.Free;
end;

// EvalAsVariant
//
procedure TArrayCopyExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
   VarCopySafe(Result, dyn);
end;

// EvalAsScriptDynArray
//
procedure TArrayCopyExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
   index, count : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);
   if IndexExpr<>nil then begin
      index:=IndexExpr.EvalAsInteger(exec);
      BoundsCheck(exec, dyn.ArrayLength, index);
   end else index:=0;
   if CountExpr<>nil then begin
      count:=CountExpr.EvalAsInteger(exec);
      if count<0 then
         RaiseScriptError(exec, EScriptError.CreateFmt(RTE_PositiveCountExpected, [count]));
      if index+count >= dyn.ArrayLength then
         count := dyn.ArrayLength-index;
   end else count:=dyn.ArrayLength-index;

   Result := TScriptDynamicArray.CreateNew(dyn.ElementTyp);
   if count>0 then
      (Result.GetSelf as TScriptDynamicArray).Copy(dyn, index, count);
end;

// GetSubExpr
//
function TArrayCopyExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FBaseExpr;
      1 : Result:=FIndexExpr;
   else
      Result:=FCountExpr;
   end;
end;

// GetSubExprCount
//
function TArrayCopyExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

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
begin
   if arraySym.ClassType = TDynamicArraySymbol then begin
      elemTyp := arraySym.Typ.UnAliasedType;
      if elemTyp.Size > 1 then
         Result := TDynamicArrayIndexOfDataExpr
      else if elemTyp.ClassType = TBaseIntegerSymbol then
         Result := TDynamicArrayIndexOfIntegerExpr
      else Result := TDynamicArrayIndexOfExpr;
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
      0 : Result:=FBaseExpr;
      1 : Result:=FItemExpr;
   else
      Result:=FFromIndexExpr;
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

// Create
//
constructor TDynamicArrayIndexOfExpr.Create(context : TdwsCompilerContext; const scriptPos : TScriptPos;
                                            aBase : TTypedExpr; aItem : TTypedExpr; aFromIndex : TTypedExpr);
var
   arrayItemTyp : TTypeSymbol;
begin
   inherited Create(context, scriptPos, aBase, aItem, aFromIndex);
   // resolve internal method depending on array and item type
   if (FItemExpr<>nil) and (FItemExpr.Typ.AsFuncSymbol<>nil) then
      FMethod:=DoEvalFuncPtr
   else if (FBaseExpr<>nil) and (FBaseExpr.Typ<>nil) then begin
      arrayItemTyp:=FBaseExpr.Typ.Typ;
      if arrayItemTyp<>nil then begin
         Assert(arrayItemTyp.Size = 1);
         if arrayItemTyp.IsOfType(context.TypString) then
            FMethod:=DoEvalString
         else FMethod:=DoEvalValue;
      end;
   end;
end;

// EvalAsInteger
//
function TDynamicArrayIndexOfExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);
   Result:=FMethod(exec, dyn);
end;

// DoEvalValue
//
function TDynamicArrayIndexOfExpr.DoEvalValue(exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer;
var
   fromIndex : Integer;
   v : Variant;
begin
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   FItemExpr.EvalAsVariant(exec, v);
   Result:=dyn.IndexOfValue(v, fromIndex, dyn.ArrayLength-1);
end;

// DoEvalString
//
function TDynamicArrayIndexOfExpr.DoEvalString(exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer;
var
   fromIndex : Integer;
   v : String;
begin
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   FItemExpr.EvalAsString(exec, v);
   Result:=dyn.IndexOfString(v, fromIndex);
end;

// DoEvalFuncPtr
//
function TDynamicArrayIndexOfExpr.DoEvalFuncPtr(exec : TdwsExecution; dyn : TScriptDynamicArray) : Integer;
var
   fromIndex : Integer;
   v : Variant;
begin
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   FItemExpr.EvalAsVariant(exec, v);
   Result:=dyn.IndexOfFuncPtr(v, fromIndex)
end;

// ------------------
// ------------------ TDynamicArrayIndexOfDataExpr ------------------
// ------------------

// EvalAsInteger
//
function TDynamicArrayIndexOfDataExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
   fromIndex : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn := (base.GetSelf as TScriptDynamicDataArray);
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   Result:=dyn.IndexOfData(TDataExpr(FItemExpr).DataPtr[exec], fromIndex, dyn.ArrayLength-1, FItemExpr.Typ.Size)
end;

// ------------------
// ------------------ TDynamicArrayIndexOfIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TDynamicArrayIndexOfIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
   fromIndex : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn := (base.GetSelf as TScriptDynamicArray);
   if FFromIndexExpr <> nil then
      fromIndex := FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex := 0;
   Result := dyn.IndexOfInteger(FItemExpr.EvalAsInteger(exec), fromIndex);
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

// EvalAsInteger
//
function TArrayRemoveExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   index : Integer;
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);
   index:=FMethod(exec, dyn);
   if index>=0 then
      dyn.Delete(index, 1);
   Result:=index;
end;

// ------------------
// ------------------ TArrayInsertExpr ------------------
// ------------------

// Create
//
constructor TArrayInsertExpr.Create(const scriptPos: TScriptPos;
                                    aBase, aIndex : TTypedExpr; aItem : TTypedExpr);
begin
   inherited Create(scriptPos, aBase);
   FIndexExpr:=aIndex;
   FItemExpr:=aItem;
end;

// Destroy
//
destructor TArrayInsertExpr.Destroy;
begin
   inherited;
   FIndexExpr.Free;
   FItemExpr.Free;
end;

// EvalNoResult
//
procedure TArrayInsertExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
   n, index : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);

   n:=dyn.ArrayLength;

   index:=IndexExpr.EvalAsInteger(exec);
   if index=n then
      dyn.ArrayLength:=n+1
   else begin
      BoundsCheck(exec, n, index);
      dyn.Insert(index);
   end;

   if ItemExpr.Typ.Size>1 then
      dyn.WriteData(index*dyn.ElementSize, (ItemExpr as TDataExpr).DataPtr[exec], dyn.ElementSize)
   else ItemExpr.EvalAsVariant(exec, dyn.AsPVariant(index)^);
end;

// GetSubExpr
//
function TArrayInsertExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FBaseExpr;
      1 : Result:=FIndexExpr;
   else
      Result:=FItemExpr;
   end;
end;

// GetSubExprCount
//
function TArrayInsertExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TArrayMoveExpr ------------------
// ------------------

// Create
//
constructor TArrayMoveExpr.Create(const scriptPos: TScriptPos;
                            aBase, anOriginIndex, aDestinationIndex : TTypedExpr);
begin
   inherited Create(scriptPos, aBase);
   FOriginIndexExpr := anOriginIndex;
   FDestinationIndexExpr := aDestinationIndex;
end;

// Destroy
//
destructor TArrayMoveExpr.Destroy;
begin
   inherited;
   FOriginIndexExpr.Free;
   FDestinationIndexExpr.Free;
end;

// EvalNoResult
//
procedure TArrayMoveExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptDynArray;
   dyn : TScriptDynamicArray;
   n, indexOrigin, indexDest : Integer;
begin
   BaseExpr.EvalAsScriptDynArray(exec, base);
   dyn:=TScriptDynamicArray(base.GetSelf);

   n:=dyn.ArrayLength;

   indexOrigin := OriginIndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, n, indexOrigin);
   indexDest := DestinationIndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, n, indexDest);

   if indexOrigin <> indexDest then
      dyn.MoveItem(indexOrigin, indexDest);
end;

// GetSubExpr
//
function TArrayMoveExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result := FBaseExpr;
      1 : Result := FOriginIndexExpr;
   else
      Result := FDestinationIndexExpr;
   end;
end;

// GetSubExprCount
//
function TArrayMoveExpr.GetSubExprCount : Integer;
begin
   Result := 3;
end;

// ------------------
// ------------------ TArrayConcatExpr ------------------
// ------------------

// Create
//
constructor TArrayConcatExpr.Create(const scriptPos : TScriptPos; aTyp: TDynamicArraySymbol);
var
   newArray : TNewArrayExpr;
begin
   inherited Create(scriptPos, aTyp);
   newArray:=TNewArrayExpr.Create(scriptPos, aTyp);
   FAddExpr:=TArrayAddExpr.Create(scriptPos, newArray, nil);
end;

// Destroy
//
destructor TArrayConcatExpr.Destroy;
begin
   inherited;
   FAddExpr.Free;
end;

// EvalAsScriptDynArray
//
procedure TArrayConcatExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
begin
   FAddExpr.DoEval(exec, result);
end;

// AddArg
//
procedure TArrayConcatExpr.AddArg(arg : TTypedExpr);
var
   concat : TArrayConcatExpr;
begin
   if arg is TArrayConcatExpr then begin

      // coalesce
      concat:=TArrayConcatExpr(arg);
      concat.FAddExpr.ExtractArgs(FAddExpr);
      concat.Free;

   end else FAddExpr.AddArg(arg);
end;

// ArgCount
//
function TArrayConcatExpr.ArgCount : Integer;
begin
   Result:=FAddExpr.SubExprCount-1
end;

// GetSubExpr
//
function TArrayConcatExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FAddExpr.SubExpr[i+1];
end;

// GetSubExprCount
//
function TArrayConcatExpr.GetSubExprCount : Integer;
begin
   Result:=FAddExpr.SubExprCount-1;
end;

// GetArgs
//
function TArrayConcatExpr.GetArgs(index : Integer) : TTypedExpr;
begin
   Result:=FAddExpr.ArgExpr[index];
end;

// ------------------
// ------------------ TAssociativeArrayClearExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssociativeArrayClearExpr.EvalNoResult(exec : TdwsExecution);
var
   aa : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, aa);
   aa.Clear;
end;

// ------------------
// ------------------ TAssociativeArrayDeleteExpr ------------------
// ------------------

// Create
//
constructor TAssociativeArrayDeleteExpr.Create(context : TdwsCompilerContext; aBase, aKey : TTypedExpr);
begin
   inherited Create;
   FBaseExpr := aBase;
   FKeyExpr := aKey;
   Typ := context.TypBoolean;
end;

// Destroy
//
destructor TAssociativeArrayDeleteExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
   FKeyExpr.Free;
end;

// EvalAsBoolean
//
function TAssociativeArrayDeleteExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   base : IScriptAssociativeArray;
begin
   FBaseExpr.EvalAsScriptAssociativeArray(exec, base);
   Result := (base.GetSelf as TScriptAssociativeArray).Delete(exec, KeyExpr);
end;

// EvalAsVariant
//
procedure TAssociativeArrayDeleteExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   result := EvalAsBoolean(exec);
end;

// EvalNoResult
//
procedure TAssociativeArrayDeleteExpr.EvalNoResult(exec : TdwsExecution);
begin
   EvalAsBoolean(exec);
end;

// GetSubExpr
//
function TAssociativeArrayDeleteExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i = 0 then
      Result := BaseExpr
   else Result := KeyExpr;
end;

// GetSubExprCount
//
function TAssociativeArrayDeleteExpr.GetSubExprCount : Integer;
begin
   Result := 2;
end;

// ------------------
// ------------------ TAssociativeArrayKeysExpr ------------------
// ------------------

// Create
//
constructor TAssociativeArrayKeysExpr.Create(context : TdwsBaseSymbolsContext; const aScriptPos : TScriptPos; expr : TTypedExpr);
var
   a : TAssociativeArraySymbol;
begin
   inherited;
   a := (expr.Typ.UnAliasedType as TAssociativeArraySymbol);
   Typ := a.KeysArrayType(context);
end;

// EvalAsVariant
//
procedure TAssociativeArrayKeysExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   dyn : IScriptDynArray;
begin
   EvalAsScriptDynArray(exec, dyn);
   Result := dyn;
end;

// EvalAsScriptDynArray
//
procedure TAssociativeArrayKeysExpr.EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray);
var
   a : IScriptAssociativeArray;
begin
   Expr.EvalAsScriptAssociativeArray(exec, a);
   Result := TScriptDynamicArray.CreateNew(Typ.Typ);
   if a <> nil then
      Result.ReplaceData((a.GetSelf as TScriptAssociativeArray).CopyKeys);
end;

end.
