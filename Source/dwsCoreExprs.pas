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
unit dwsCoreExprs;

{$I dws.inc}

interface

uses Classes, Variants, SysUtils, dwsSymbols, dwsErrors, dwsStrings,
   dwsStack, dwsExprs, dwsUtils, dwsTokenizer, dwsUnitSymbols
   {$ifdef FPC},LazUTF8{$endif};

type

   TCaseCondition = class;

   IVarParamData = interface
      function GetData : TData;
      function GetAddr : Integer;
      function Eval : PVariant;

      property Data : TData read GetData;
      property Addr : Integer read GetAddr;
   end;

   TVarExpr = class (TDataExpr)
      protected
         FStackAddr : Integer;
         FDataSym : TDataSymbol;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(prog : TdwsProgram; dataSym : TDataSymbol);
         class function CreateTyped(prog: TdwsProgram; dataSym : TDataSymbol) : TVarExpr;

         procedure AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer); override;
         procedure AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr); override;
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const value : Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value : String); override;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); override;

         function Eval(exec : TdwsExecution) : Variant; override;

         function SameVarAs(expr : TVarExpr) : Boolean;

         property StackAddr : Integer read FStackAddr;
         property DataSym : TDataSymbol read FDataSym write FDataSym;
   end;

   TIntVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const Value: Int64); override;
         procedure AssignValueAsPInteger(exec : TdwsExecution; const pValue: PInt64);
         procedure IncValue(exec : TdwsExecution; const value: Int64);
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
         function  EvalAsPInteger(exec : TdwsExecution) : PInt64; inline;
   end;

   TFloatVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const Value: Double); override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   TStrVarExpr = class sealed (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsString(exec : TdwsExecution; const Value: String); override;
         function  SetChar(exec : TdwsExecution; index : Integer; c : WideChar) : Boolean;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         procedure Append(exec : TdwsExecution; const value : String);
   end;

   TBoolVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TObjectVarExpr = class (TVarExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   TSelfVarExpr = class (TVarExpr)
      public
         function IsWritable : Boolean; override;
   end;

   TVarParentExpr = class(TVarExpr)
      protected
         FLevel: Integer;
         function GetAddr(exec : TdwsExecution) : Integer; override;
      public
         constructor Create(prog : TdwsProgram; dataSym : TDataSymbol);
         property Level : Integer read FLevel;
   end;

   // Encapsulates a lazy parameter
   TLazyParamExpr = class(TTypedExpr)
      private
         FDataSym : TLazyParamSymbol;
         FStackAddr : Integer;
         FLevel : Integer;

      public
         constructor Create(Prog: TdwsProgram; dataSym : TLazyParamSymbol);
         function Eval(exec : TdwsExecution) : Variant; override;

         property DataSym : TLazyParamSymbol read FDataSym write FDataSym;
         property StackAddr : Integer read FStackAddr write FStackAddr;
         property Level : Integer read FLevel write FLevel;
   end;

   // Encapsulates a var parameter
   TByRefParamExpr = class (TVarExpr)
      protected
         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor CreateFromVarExpr(expr : TVarExpr);

         function GetVarParamDataAsPointer(exec : TdwsExecution) : Pointer; inline;
         procedure GetVarParamData(exec : TdwsExecution; var result : IVarParamData);
         function GetVarParamEval(exec : TdwsExecution) : PVariant; inline;

         procedure AssignData(exec : TdwsExecution; const sourceData : TData; sourceAddr : Integer); override;
         procedure AssignDataExpr(exec : TdwsExecution; dataExpr: TDataExpr); override;
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value : String); override;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); override;

         function  Eval(exec : TdwsExecution) : Variant; override;
   end;

   TVarParamExpr = class (TByRefParamExpr)
   end;

   TConstParamExpr = class (TByRefParamExpr)
      public
         function IsWritable : Boolean; override;
   end;

   // Encapsulates a var parameter
   TByRefParentParamExpr = class(TByRefParamExpr)
      protected
         FLevel: Integer;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(prog : TdwsProgram; dataSym : TDataSymbol);
   end;

   TVarParamParentExpr = class(TByRefParentParamExpr)
   end;

   TConstParamParentExpr = class(TByRefParentParamExpr)
      public
         function IsWritable : Boolean; override;
   end;

   TConstIntExpr = class;

   // A constant value (like 0, 3.14159, 'Hello' or true)
   TConstExpr = class(TDataExpr)
      protected
         FData : TData;
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); overload; virtual;
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer); overload;

         function Eval(exec : TdwsExecution) : Variant; override;
         function IsConstant : Boolean; override;
         function IsWritable : Boolean; override;
         function SameValueAs(otherConst : TConstExpr) : Boolean;

         class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr; overload; static;
         class function CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;

         class function CreateTypedVariantValue(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TConstExpr; overload; static;

         class function CreateIntegerValue(prog : TdwsProgram; const value : Int64) : TConstExpr; overload; static;
         class function CreateIntegerValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Int64) : TConstExpr; overload; static;

         class function CreateBooleanValue(prog : TdwsProgram; const value : Boolean) : TConstExpr; overload; static;

         class function CreateDynamicArrayValue(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr; overload; static;
   end;

   TUnifiedConstExprClass = class of TUnifiedConstExpr;

   // TUnifiedConstExpr
   //
   {: Unified constants go into a program root unified const list. }
   TUnifiedConstExpr = class (TConstExpr)
      public
         class function CreateUnified(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant) : TUnifiedConstExpr;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class(TUnifiedConstExpr)
      protected
         FValue : Boolean;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;

         property Value : Boolean read FValue;
   end;

   // TConstIntExpr
   //
   TConstIntExpr = class (TUnifiedConstExpr)
      private
         FValue : Int64;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Int64 read FValue write FValue;
   end;

   // TConstFloatExpr
   //
   TConstFloatExpr = class(TUnifiedConstExpr)
      private
         FValue : Double;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         function EvalAsFloat(exec : TdwsExecution) : Double; override;
         property Value : Double read FValue;
   end;

   // TConstStringExpr
   //
   TConstStringExpr = class(TUnifiedConstExpr)
      private
         FValue : String;
      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant); override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         property Value : String read FValue write FValue;
   end;

   TStandardIntegersConstIntExprArray = array [-1..2] of TUnifiedConstExpr;

   // TUnifiedConstList
   //
   TUnifiedConstList = class (TSortedExprBaseList)
      private
         FEmptyString : TUnifiedConstExpr;
         FIntegers : TStandardIntegersConstIntExprArray;
         FZeroFloat : TUnifiedConstExpr;

      protected
         function Compare(const item1, item2 : TExprBase) : Integer; override;

      public
         destructor Destroy; override;

         procedure Precharge(prog : TdwsMainProgram; systemTable : TSystemSymbolTable);

         property EmptyString : TUnifiedConstExpr read FEmptyString;
         property Integers : TStandardIntegersConstIntExprArray read FIntegers;
         property ZeroFloat : TUnifiedConstExpr read FZeroFloat;
   end;

   // TResourceStringExpr
   //
   // Returns a localized version of a resourcestring
   TResourceStringExpr = class sealed (TTypedExpr)
      private
         FResSymbol : TResourceStringSymbol;
         FScriptPos : TScriptPos;

      public
         constructor Create(aProg : TdwsProgram; const aScriptPos : TScriptPos; aRes : TResourceStringSymbol);

         function ScriptPos : TScriptPos; override;

         function  Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;

         property ResSymbol : TResourceStringSymbol read FResSymbol;
   end;

   TArrayConstantExpr = class sealed (TPosDataExpr)
      protected
         FArrayAddr : Integer;
         FElementExprs : TTightList;
         FArrayEvaled : Boolean;

         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetElement(idx : Integer) : TTypedExpr; inline;
         function GetElementCount : Integer; inline;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);
         destructor Destroy; override;

         property Elements[idx : Integer] : TTypedExpr read GetElement;
         property ElementCount : Integer read GetElementCount;
         procedure AddElementExpr(Prog: TdwsProgram; ElementExpr: TTypedExpr);
         procedure Prepare(Prog: TdwsProgram; ElementTyp : TTypeSymbol);
         procedure TypeCheckElements(prog : TdwsProgram);

         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsTData(exec : TdwsExecution) : TData;
         function EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;

         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         function IsConstant : Boolean; override;
         function IsWritable : Boolean; override;
   end;

   // Array expressions x[index]
   TArrayExpr = class(TPosDataExpr)
      protected
         FBaseExpr : TDataExpr;
         FIndexExpr : TTypedExpr;
         FElementSize : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TTypedExpr);
         destructor Destroy; override;

         function IsWritable : Boolean; override;

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
         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos;
                            baseExpr : TDataExpr; indexExpr : TTypedExpr;
                            lowBound, highBound : Integer);

         function IsConstant : Boolean; override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         procedure AssignExpr(exec : TdwsExecution; expr : TTypedExpr); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Array expressions x[bool] for static arrays
   TStaticArrayBoolExpr = class (TStaticArrayExpr)
      protected
         function GetAddr(exec : TdwsExecution) : Integer; override;
   end;

   // Array expressions x[index] for open arrays
   TOpenArrayExpr = class(TArrayExpr)
      protected
         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;
      public
         function IsWritable : Boolean; override;
   end;

   // Array expressions: x[index0] for dynamic arrays
   TDynamicArrayExpr = class(TArrayExpr)
      protected
         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function DynGetAddr(exec : TdwsExecution; dynArray : TScriptDynamicArray) : Integer;

         function EvalItem(exec : TdwsExecution) : PVariant;

      public
         function  Eval(exec : TdwsExecution) : Variant; override;

         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;

         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); override;
   end;

   // array[index]:=val for dynamic arrays
   TDynamicArraySetExpr = class(TNoResultPosExpr)
      private
         FArrayExpr : TTypedExpr;
         FIndexExpr : TTypedExpr;
         FValueExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                            arrayExpr, indexExpr, valueExpr : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property ArrayExpr : TTypedExpr read FArrayExpr;
         property IndexExpr : TTypedExpr read FIndexExpr;
         property ValueExpr : TTypedExpr read FValueExpr;
   end;

   // Record expression: record.member
   TRecordExpr = class(TPosDataExpr)
      protected
         FBaseExpr : TDataExpr;
         FMemberOffset : Integer;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr;
                            fieldSymbol: TFieldSymbol);
         destructor Destroy; override;

         function IsConstant : Boolean; override;
         function Eval(exec : TdwsExecution) : Variant; override;

         property BaseExpr : TDataExpr read FBaseExpr;
         property MemberOffset : Integer read FMemberOffset;

         function IsWritable : Boolean; override;
   end;

   TInitDataExpr = class(TNoResultPosExpr)
      protected
         FExpr : TDataExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property Expr : TDataExpr read FExpr;
   end;

   // dynamic anonymous record
   TDynamicRecordExpr = class(TPosDataExpr)
      private
         FAddr : Integer;

      protected
         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(aProg : TdwsProgram; const aPos : TScriptPos;
                            recordTyp : TRecordSymbol);

         procedure EvalNoResult(exec : TdwsExecution); override;

         property Addr : Integer read FAddr;
   end;

   // Field expression: obj.Field
   TFieldExpr = class(TPosDataExpr)
      protected
         FObjectExpr : TTypedExpr;
         FFieldAddr : Integer;

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos;
                            fieldSym : TFieldSymbol; ObjExpr: TTypedExpr);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;

         property ObjectExpr : TTypedExpr read FObjectExpr;
         property FieldAddr : Integer read FFieldAddr;
   end;

   TReadOnlyFieldExpr = class(TFieldExpr)
      function IsWritable: Boolean; override;
   end;

   // length of dynamic arrays
   TArrayLengthExpr = class(TUnaryOpIntExpr)
      private
         FDelta : Integer;
         FCapture : Boolean;

      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr; captureExpr : Boolean); reintroduce;
         destructor Destroy; override;

         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
         property Delta : Integer read FDelta write FDelta;
   end;

   // length of an open array
   TOpenArrayLengthExpr = class(TArrayLengthExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;

   end;

   // left[right] String read access
   TStringArrayOpExpr = class(TStringBinOpExpr)
      private
         FPos : TScriptPos;

      public
         constructor CreatePos(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TTypedExpr);
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         function ScriptPos : TScriptPos; override;
   end;

   TStringLengthExpr = class(TUnaryOpIntExpr)
      public
         function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // TypedExpr for dynamic array
   TArrayTypedExpr = class(TTypedExpr)
      private
         FScriptPos : TScriptPos;

      protected
         procedure BoundsCheck(exec : TdwsExecution; dynArray : TScriptDynamicArray; i : Integer);

      public
         constructor Create(prog: TdwsProgram; const scriptPos: TScriptPos);

         function ScriptPos : TScriptPos; override;
   end;

   // new array[length,...]
   TNewArrayExpr = class(TArrayTypedExpr)
      private
         FLengthExprs : TTightList;
         FTyps : TTightList;

         function GetLengthExpr(idx : Integer) : TTypedExpr; inline;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog: TdwsProgram; const scriptPos: TScriptPos;
                            elementTyp : TTypeSymbol);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;

         procedure AddLengthExpr(expr : TTypedExpr; indexTyp : TTypeSymbol);
         property LengthExpr[idx : Integer] : TTypedExpr read GetLengthExpr;
         property LengthExprCount : Integer read FLengthExprs.FCount;
   end;

   // Pseudo-method for dynamic array
   TArrayPseudoMethodExpr = class(TNoResultPosExpr)
      private
         FBaseExpr : TTypedExpr;

      protected
         procedure BoundsCheck(exec : TdwsExecution; dynArray : TScriptDynamicArray; i : Integer);

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase : TTypedExpr);
         destructor Destroy; override;

         property BaseExpr : TTypedExpr read FBaseExpr;
   end;

   // SetLength of dynamic array
   TArraySetLengthExpr = class(TArrayPseudoMethodExpr)
      private
         FLengthExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aLength : TTypedExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property LengthExpr : TTypedExpr read FLengthExpr;
   end;

   // Swap two elements of a dynamic array
   TArraySwapExpr = class(TArrayPseudoMethodExpr)
      private
         FIndex1Expr : TTypedExpr;
         FIndex2Expr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aIndex1, aIndex2 : TTypedExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property Index1Expr : TTypedExpr read FIndex1Expr;
         property Index2Expr : TTypedExpr read FIndex2Expr;
   end;

   // Sort a dynamic array
   TArraySortExpr = class(TArrayPseudoMethodExpr)
      private
         FCompareExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aCompare : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property CompareExpr : TTypedExpr read FCompareExpr write FCompareExpr;
   end;

   // Reverse a dynamic array
   TArrayReverseExpr = class(TArrayPseudoMethodExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // Add an item to a dynamic array
   TArrayAddExpr = class(TArrayPseudoMethodExpr)
      private
         FArgs : TTightList;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetItemExpr(idx : Integer) : TDataExpr;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase :  TTypedExpr; argExprs : TTypedExprList);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property ArgExpr[idx : Integer] : TDataExpr read GetItemExpr;
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

         function GetAddr(exec : TdwsExecution) : Integer; override;
         function GetData(exec : TdwsExecution) : TData; override;

         function GetBaseDynArray(exec : TdwsExecution) : TScriptDynamicArray;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase :  TTypedExpr);
         destructor Destroy; override;

         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;
   end;

   // Peek the last value of a dynamic array
   TArrayPeekExpr = class(TArrayDataExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // Pop the last value of a dynamic array
   TArrayPopExpr = class(TArrayPeekExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
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
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aIndex, aCount : TTypedExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property IndexExpr : TTypedExpr read FIndexExpr;
         property CountExpr : TTypedExpr read FCountExpr;
   end;

   // Shallow-copy of a subset of an array
   TArrayCopyExpr = class(TArrayTypedExpr)
      private
         FBaseExpr : TTypedExpr;
         FIndexExpr : TTypedExpr;
         FCountExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aIndex, aCount : TTypedExpr);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;

         property BaseExpr : TTypedExpr read FBaseExpr;
         property IndexExpr : TTypedExpr read FIndexExpr;
         property CountExpr : TTypedExpr read FCountExpr;
   end;

   // Find element in a dynamic array (shallow comparison)
   TArrayIndexOfExpr = class(TArrayTypedExpr)
      private
         FBaseExpr : TTypedExpr;
         FItemExpr : TDataExpr;
         FFromIndexExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                            aBase : TTypedExpr; aItem : TDataExpr; aFromIndex : TTypedExpr);
         destructor Destroy; override;

         function  Eval(exec : TdwsExecution) : Variant; override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;

         property BaseExpr : TTypedExpr read FBaseExpr;
         property ItemExpr : TDataExpr read FItemExpr;
         property FromIndexExpr : TTypedExpr read FFromIndexExpr;
   end;

   // Insert an elemet at a given index of a dynamic array
   TArrayInsertExpr = class(TArrayPseudoMethodExpr)
      private
         FIndexExpr : TTypedExpr;
         FItemExpr : TDataExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aIndex : TTypedExpr; aItem : TDataExpr);
         destructor Destroy; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property IndexExpr : TTypedExpr read FIndexExpr;
         property ItemExpr : TDataExpr read FItemExpr;
   end;

   TAssignedExpr = class(TUnaryOpBoolExpr)
   end;

   TAssignedInstanceExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TAssignedMetaClassExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TAssignedFuncPtrExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TOrdExpr = class(TUnaryOpIntExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TOrdIntExpr = class(TOrdExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TOrdBoolExpr = class(TOrdExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   TOrdStrExpr = class(TOrdExpr)
   public
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // obj is TMyClass
   TIsOpExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // cast something as Typ
   TAsCastExpr = class(TUnaryOpExpr)
      private
         FPos : TScriptPos;

      public
         constructor Create(prog : TdwsProgram; const aPos : TScriptPos;
                            expr : TTypedExpr; toTyp : TTypeSymbol); reintroduce;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // class as TMyClass
   TClassAsClassExpr = class(TAsCastExpr)
      protected
         procedure RaiseMetaClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);

      public
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // obj as TMyClass
   TObjAsClassExpr = class(TAsCastExpr)
      protected
         procedure RaiseInstanceClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);

      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // obj.ClassType
   TObjToClassTypeExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;

         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // obj left = obj right
   TObjCmpEqualExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // obj left <> obj right
   TObjCmpNotEqualExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // interface left = interface right
   TIntfCmpExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // obj as Interface
   TObjAsIntfExpr = class(TAsCastExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // interface as Interface
   TIntfAsIntfExpr = class(TAsCastExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // interface as class
   TIntfAsClassExpr = class(TAsCastExpr)
      public
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;
   end;

   // obj implements Interface
   TImplementsIntfOpExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // class implements Interface
   TClassImplementsIntfOpExpr = class(TBooleanBinOpExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // -x
   TNegVariantExpr = class(TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TNegIntExpr = class (TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TNegFloatExpr = class (TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a + b
   TAddVariantExpr = class(TVariantBinOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TAddStrExpr = class(TStringBinOpExpr)
      procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
   end;
   TAddIntExpr = class sealed (TIntegerBinOpExpr)
      function EvalAsInteger(exec : TdwsExecution) : Int64; override;
      function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;
   TAddFloatExpr = class(TFloatBinOpExpr)
      function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a - b
   TSubVariantExpr = class(TVariantBinOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TSubIntExpr = class sealed (TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;
   TSubFloatExpr = class(TFloatBinOpExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a * b
   TMultVariantExpr = class(TVariantBinOpExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;
   TMultIntExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TMultFloatExpr = class(TFloatBinOpExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // Sqr ( a )
   TSqrIntExpr = class(TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TSqrFloatExpr = class(TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a / b
   TDivideExpr = class(TFloatBinOpExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // a div b
   TDivExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // a mod b
   TModExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // not bool a
   TNotBoolExpr = class(TUnaryOpBoolExpr)
      function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   // not int a
   TNotIntExpr = class(TUnaryOpIntExpr)
      function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // not variant a
   TNotVariantExpr = class(TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // a and b
   TIntAndExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TBoolAndExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TVariantAndExpr = class(TVariantBinOpExpr)
     function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // a or b
   TIntOrExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TBoolOrExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TVariantOrExpr = class(TVariantBinOpExpr)
     function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // a xor b
   TIntXorExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   TBoolXorExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;
   TVariantXorExpr = class(TVariantBinOpExpr)
     function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // a implies b
   TBoolImpliesExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // a shl b
   TShlExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // a shr b
   TShrExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // a sar b
   TSarExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // newType(x)
   TConvExpr = class(TUnaryOpExpr)
      public
         class function WrapWithConvCast(prog : TdwsProgram; const scriptPos : TScriptPos;
                                         toTyp : TTypeSymbol; expr : TTypedExpr;
                                         reportError : Boolean) : TTypedExpr; static;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Float(x)
   TConvFloatExpr = class (TUnaryOpFloatExpr)
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   // Integer(x)
   TConvIntegerExpr = class (TUnaryOpIntExpr)
     function EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;

   // String(x)
   TConvStringExpr = class (TUnaryOpStringExpr)
     procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
   end;

   // Boolean(x)
   TConvBoolExpr = class (TUnaryOpBoolExpr)
     function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   // Variant(simple)
   TConvVariantExpr = class (TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // Static Array to Dynamic Array
   TConvStaticArrayToDynamicExpr = class (TUnaryOpExpr)
      constructor Create(prog : TdwsProgram; expr : TArrayConstantExpr; toTyp : TDynamicArraySymbol); reintroduce;
      function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // ExternalClass(x)
   TConvExternalExpr = class (TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // Assert(condition, message);
   TAssertExpr = class(TNoResultPosExpr)
      protected
         FCond : TTypedExpr;
         FMessage : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; condExpr, msgExpr : TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         property Cond : TTypedExpr read FCond;
         property Message : TTypedExpr read FMessage;
   end;

   // left := right;
   TAssignExpr = class(TNoResultPosExpr)
      protected
         FLeft : TDataExpr;
         FRight : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); virtual;
         destructor Destroy; override;

         property Left : TDataExpr read FLeft;
         property Right : TTypedExpr read FRight;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure TypeCheckAssign(prog : TdwsProgram); virtual;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         function  OptimizeConstAssignment(prog : TdwsProgram; exec : TdwsExecution) : TNoResultExpr;
   end;

   TAssignExprClass = class of TAssignExpr;

   // left := right; (class of)
   TAssignClassOfExpr = class(TAssignExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := right;
   TAssignDataExpr = class(TAssignExpr)
   protected
     FSize: Integer;
   public
     constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := right; (var, func)
   TAssignFuncExpr = class(TAssignExpr)
      public
         procedure TypeCheckAssign(prog : TdwsProgram); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // left := [constant array];
   TAssignArrayConstantExpr = class(TAssignDataExpr)
   public
     constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
     procedure EvalNoResult(exec : TdwsExecution); override;
     procedure TypeCheckAssign(prog : TdwsProgram); override;
   end;

   // var left := const right;
   TAssignConstDataToVarExpr = class(TAssignDataExpr)
   public
     constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := const right;
   TAssignConstExpr = class (TAssignExpr)
      public
         procedure TypeCheckAssign(prog : TdwsProgram); override;
   end;

   // left := const integer;
   TAssignConstToIntegerVarExpr = class(TAssignConstExpr)
      protected
         FRight : Int64;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : Int64 read FRight write FRight;
   end;

   // left := const float;
   TAssignConstToFloatVarExpr = class(TAssignConstExpr)
      protected
         FRight : Double;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : Double read FRight write FRight;
   end;

   // left := const bool;
   TAssignConstToBoolVarExpr = class(TAssignConstExpr)
      protected
         FRight : Boolean;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Boolean);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : Boolean read FRight write FRight;
   end;

   // left := const String;
   TAssignConstToStringVarExpr = class(TAssignConstExpr)
      protected
         FRight : String;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
         procedure EvalNoResult(exec : TdwsExecution); override;
         property Right : String read FRight write FRight;
   end;

   // left := nil (instance)
   TAssignNilToVarExpr = class(TAssignConstExpr)
      public
         constructor CreateVal(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr);
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // left := nil (class)
   TAssignNilClassToVarExpr = class(TAssignNilToVarExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a := a op b
   TOpAssignExpr = class(TAssignExpr)
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // a += b
   TPlusAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a += b (int)
   TPlusAssignIntExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   // a += b (float)
   TPlusAssignFloatExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a += b (String)
   TPlusAssignStrExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // a -= b
   TMinusAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a -= b (int)
   TMinusAssignIntExpr = class(TMinusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
     function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   // a -= b (float)
   TMinusAssignFloatExpr = class(TMinusAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a *= b
   TMultAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a *= b (int)
   TMultAssignIntExpr = class(TMultAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a *= b (float)
   TMultAssignFloatExpr = class(TMultAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a /= b
   TDivideAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // a += b (int var)
   TIncIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   // a -= b (int var)
   TDecIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // Abs(v) (int)
   TAbsIntExpr = class(TUnaryOpIntExpr)
      function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
   end;
   // Abs(v) (float)
   TAbsFloatExpr = class(TUnaryOpFloatExpr)
      function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;
   // Abs(v) (variant)
   TAbsVariantExpr = class(TUnaryOpVariantExpr)
      procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
   end;

   // a += b (String var)
   TAppendStringVarExpr = class(TAssignExpr)
     procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // (String var) += (String const)
   TAppendConstStringVarExpr = class(TAssignExpr)
      private
         FAppendString : String;
      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr); override;
         procedure EvalNoResult(exec : TdwsExecution); override;
         property AppendString : String read FAppendString;
   end;

   // val in [case conditions list]
   TInOpExpr = class(TTypedExpr)
      private
         FLeft : TTypedExpr;
         FCaseConditions : TTightList;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;
         function GetCaseConditions(idx : Integer) : TCaseCondition;

         function ConstantConditions : Boolean;

      public
         constructor Create(Prog: TdwsProgram; Left : TTypedExpr);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         function IsConstant : Boolean; override;
         procedure AddCaseCondition(cond : TCaseCondition);

         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         property Left : TTypedExpr read FLeft;
         property CaseConditions[idx : Integer] : TCaseCondition read GetCaseConditions; default;
         property Count : Integer read FCaseConditions.FCount;
   end;

   // bitwise val in [case conditions list]
   TBitwiseInOpExpr = class(TUnaryOpBoolExpr)
      private
         FMask : Integer;

      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;

         property Mask : Integer read FMask write FMask;
   end;

   // statement; statement; statement;
   TBlockExpr = class(TBlockExprBase)
      private
         FTable : TSymbolTable;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         property  Table: TSymbolTable read FTable;
   end;

   // statement; statement; statement;
   TBlockExprNoTable = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBlockExprNoTable2 = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBlockExprNoTable3 = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TBlockExprNoTable4 = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // if FCond then FThen
   TIfThenExpr = class(TNoResultPosExpr)
      private
         FCond : TTypedExpr;
         FThen : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const Pos : TScriptPos;
                            condExpr : TTypedExpr; thenExpr : TNoResultExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         property CondExpr : TTypedExpr read FCond;
         property ThenExpr : TNoResultExpr read FThen;
   end;

   // if FCond then FThen else FElse
   TIfThenElseExpr = class(TIfThenExpr)
      private
         FElse : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const Pos : TScriptPos;
                            condExpr : TTypedExpr; thenExpr, elseExpr : TNoResultExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;

         property ElseExpr : TNoResultExpr read FElse;
   end;

   // Part of a case statement
   TCaseCondition = class (TRefCountedObject)
      private
         FOwnsTrueExpr : Boolean;
         FTrueExpr : TNoResultExpr;
         FPos : TScriptPos;

         function IsOfTypeNumber(prog : TdwsProgram; typ : TTypeSymbol) : Boolean;

      public
         constructor Create(const aPos : TScriptPos);
         destructor Destroy; override;

         function GetSubExpr(i : Integer) : TExprBase; virtual; abstract;
         function GetSubExprCount : Integer; virtual; abstract;

         function IsTrue(exec : TdwsExecution; const value: Variant) : Boolean; virtual; abstract;
         procedure TypeCheck(prog : TdwsProgram; typ : TTypeSymbol); virtual; abstract;
         function IsConstant : Boolean; virtual; abstract;

         property Pos : TScriptPos read FPos;
         property TrueExpr: TNoResultExpr read FTrueExpr write FTrueExpr;
         property OwnsTrueExpr: Boolean read FOwnsTrueExpr write FOwnsTrueExpr;
   end;

   TCaseConditions = TObjectList<TCaseCondition>;

   TCompareCaseCondition = class(TCaseCondition)
      private
         FCompareExpr : TTypedExpr;

      public
         constructor Create(const aPos : TScriptPos; compareExpr : TTypedExpr);
         destructor Destroy; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function IsTrue(exec : TdwsExecution; const value : Variant) : Boolean; override;
         procedure TypeCheck(prog : TdwsProgram; typ : TTypeSymbol); override;
         function IsConstant : Boolean; override;

         property CompareExpr : TTypedExpr read FCompareExpr;
   end;

   TRangeCaseCondition = class(TCaseCondition)
      private
         FFromExpr : TTypedExpr;
         FToExpr : TTypedExpr;

      public
         constructor Create(const aPos : TScriptPos; fromExpr, toExpr : TTypedExpr);
         destructor Destroy; override;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function IsTrue(exec : TdwsExecution; const Value: Variant): Boolean; override;
         procedure TypeCheck(prog : TdwsProgram; typ : TTypeSymbol); override;
         function IsConstant : Boolean; override;

         property FromExpr : TTypedExpr read FFromExpr;
         property ToExpr : TTypedExpr read FToExpr;
   end;

   // case FValueExpr of {CaseConditions} else FElseExpr end;
   TCaseExpr = class(TNoResultPosExpr)
      private
         FCaseConditions : TTightList;
         FElseExpr : TNoResultExpr;
         FValueExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         procedure AddCaseCondition(cond : TCaseCondition);

         property CaseConditions : TTightList read FCaseConditions;
         property ValueExpr: TTypedExpr read FValueExpr write FValueExpr;
         property ElseExpr: TNoResultExpr read FElseExpr write FElseExpr;
   end;

   // for FVarExpr := FFromExpr to FToExpr do FDoExpr;
   TForExpr = class(TNoResultPosExpr)
      private
         FDoExpr : TNoResultExpr;
         FFromExpr : TTypedExpr;
         FToExpr : TTypedExpr;
         FVarExpr : TIntVarExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         property DoExpr: TNoResultExpr read FDoExpr write FDoExpr;
         property FromExpr: TTypedExpr read FFromExpr write FFromExpr;
         property ToExpr: TTypedExpr read FToExpr write FToExpr;
         property VarExpr: TIntVarExpr read FVarExpr write FVarExpr;
   end;

   TForExprClass = class of TForExpr;

   TForUpwardExpr = class(TForExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TForDownwardExpr = class(TForExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // for FVarExpr := FFromExpr to FToExpr step FStepExpr do FDoExpr;
   TForStepExpr = class(TForExpr)
      private
         FStepExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         function EvalStep(exec : TdwsExecution) : Int64;

         property StepExpr : TTypedExpr read FStepExpr write FStepExpr;
   end;

   TForSteprExprClass = class of TForStepExpr;

   TForUpwardStepExpr = class(TForStepExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TForDownwardStepExpr = class(TForStepExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // for something in aString do ...;
   TForInStrExpr = class(TNoResultPosExpr)
      private
         FDoExpr : TNoResultExpr;
         FInExpr : TTypedExpr;
         FVarExpr : TVarExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(aProg: TdwsProgram; const aPos: TScriptPos;
                            aVarExpr : TVarExpr; aInExpr : TTypedExpr; aDoExpr : TNoResultExpr);
         destructor Destroy; override;

         property DoExpr : TNoResultExpr read FDoExpr write FDoExpr;
         property InExpr : TTypedExpr read FInExpr write FInExpr;
         property VarExpr : TVarExpr read FVarExpr write FVarExpr;
   end;

   // for charCode in aString do ...;
   TForCharCodeInStrExpr = class(TForInStrExpr)
      public
         constructor Create(aProg: TdwsProgram; const aPos: TScriptPos;
                            aVarExpr : TIntVarExpr; aInExpr : TTypedExpr; aDoExpr : TNoResultExpr);

         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // for char in aString do ...;
   TForCharInStrExpr = class(TForInStrExpr)
      public
         constructor Create(aProg: TdwsProgram; const aPos: TScriptPos;
                            aVarExpr : TStrVarExpr; aInExpr : TTypedExpr; aDoExpr : TNoResultExpr);

         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // base class for while, repeat and infinite loops
   TLoopExpr = class(TNoResultPosExpr)
      private
         FCondExpr : TTypedExpr;
         FLoopExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property CondExpr : TTypedExpr read FCondExpr write FCondExpr;
         property LoopExpr : TNoResultExpr read FLoopExpr write FLoopExpr;
   end;

   // while FCondExpr do FLoopExpr
   TWhileExpr = class(TLoopExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // repeat FLoopExpr while FCondExpr
   TRepeatExpr = class(TLoopExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   TFlowControlExpr = class(TNoResultPosExpr)
      public
         function InterruptsFlow : Boolean; override;
   end;

   TBreakExpr = class(TFlowControlExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TExitExpr = class(TFlowControlExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TExitValueExpr = class(TExitExpr)
      private
         FAssignExpr : TAssignExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; assignExpr : TAssignExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property AssignExpr : TAssignExpr read FAssignExpr;
   end;

   TContinueExpr = class(TFlowControlExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TRaiseBaseExpr = class(TNoResultPosExpr)
   end;

   // raise TExceptionClass.Create;
   TRaiseExpr = class (TRaiseBaseExpr)
      private
         FExceptionExpr: TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const scriptPos: TScriptPos; ExceptionExpr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         function InterruptsFlow : Boolean; override;
   end;

   TReraiseExpr = class(TRaiseBaseExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TExceptionExpr = class(TNoResultPosExpr)
      private
         FTryExpr : TNoResultExpr;
         FHandlerExpr : TNoResultExpr;

      protected
         function CreateEDelphiObj(exec : TdwsExecution; const ClassName, Message: String): IScriptObj;

         procedure EnterExceptionBlock(exec : TdwsExecution; var exceptObj : IScriptObj);
         procedure LeaveExceptionBlock(exec : TdwsExecution);

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         property TryExpr : TNoResultExpr read FTryExpr write FTryExpr;
         property HandlerExpr : TNoResultExpr read FHandlerExpr write FHandlerExpr;
   end;

   TExceptDoExpr = class;

   // try FTryExpr except {FDoExprs}; else FElseExpr end;
   TExceptExpr = class(TExceptionExpr)
      private
         FDoExprs : TTightList;
         FElseExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;
         function GetDoExpr(i : Integer) : TExceptDoExpr;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         procedure AddDoExpr(expr : TExceptDoExpr);
         property DoExpr[i : Integer] : TExceptDoExpr read GetDoExpr;
         function DoExprCount : Integer;

         property ElseExpr : TNoResultExpr read FElseExpr write FElseExpr;
   end;

   // try..except on FExceptionVar: FExceptionVar.Typ do FDoBlockExpr; ... end;
   TExceptDoExpr = class(TNoResultPosExpr)
      private
         FExceptionVar : TDataSymbol;
         FDoBlockExpr : TNoResultExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property DoBlockExpr: TNoResultExpr read FDoBlockExpr write FDoBlockExpr;
         property ExceptionVar: TDataSymbol read FExceptionVar write FExceptionVar;
   end;

   // try FTryExpr finally FHandlerExpr end;
   TFinallyExpr = class(TExceptionExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TStringArraySetExpr = class(TNoResultPosExpr)
      private
         FStringExpr: TDataExpr;
         FIndexExpr: TTypedExpr;
         FValueExpr: TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; StringExpr : TDataExpr; IndexExpr, ValueExpr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property StringExpr : TDataExpr read FStringExpr;
         property IndexExpr : TTypedExpr read FIndexExpr;
         property ValueExpr : TTypedExpr read FValueExpr;
   end;

   TVarStringArraySetExpr = class(TStringArraySetExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   TSpecialUnaryBoolExpr = class(TUnaryOpBoolExpr)
      public
         function IsConstant : Boolean; override;
   end;

   TConditionalDefinedExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TDefinedExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
   end;

   TDeclaredExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         class function FindSymbol(symbolTable : TSymbolTable; const name : String) : TSymbol; static;
   end;

   TSwapExpr = class(TNoResultPosExpr)
      private
         FArg0 : TDataExpr;
         FArg1 : TDataExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                            expr0, expr1 : TDataExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

         property Arg0 : TDataExpr read FArg0;
         property Arg1 : TDataExpr read FArg1;
   end;

   EClassCast = class (EScriptError) end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsStringFunctions, dwsXPlatform;

// ------------------
// ------------------ TVarExpr ------------------
// ------------------

// Create
//
constructor TVarExpr.Create(prog : TdwsProgram; dataSym : TDataSymbol);
begin
   inherited Create(prog, dataSym.Typ);
   FStackAddr:=dataSym.StackAddr;
   FDataSym:=dataSym;
end;

// CreateTyped
//
class function TVarExpr.CreateTyped(prog : TdwsProgram; dataSym : TDataSymbol) : TVarExpr;
var
   typ : TTypeSymbol;
begin
   typ:=dataSym.Typ;
   if typ.IsOfType(prog.TypInteger) then
      Result:=TIntVarExpr.Create(prog, dataSym)
   else if typ.IsOfType(prog.TypFloat) then
      Result:=TFloatVarExpr.Create(prog, dataSym)
   else if typ.IsOfType(prog.TypString) then
      Result:=TStrVarExpr.Create(prog, dataSym)
   else if typ.IsOfType(prog.TypBoolean) then
      Result:=TBoolVarExpr.Create(prog, dataSym)
   else if dataSym.ClassType=TSelfSymbol then
      Result:=TSelfVarExpr.Create(prog, dataSym)
   else if (typ is TClassSymbol) or (typ is TDynamicArraySymbol) then
      Result:=TObjectVarExpr.Create(prog, dataSym)
   else Result:=TVarExpr.Create(prog, dataSym);
end;

// Eval
//
function TVarExpr.Eval(exec : TdwsExecution) : Variant;
begin
   exec.Stack.ReadValue(Addr[exec], Result);
end;

// SameVarAs
//
function TVarExpr.SameVarAs(expr : TVarExpr) : Boolean;
begin
   Result:=    (FStackAddr=expr.FStackAddr)
           and (ClassType=expr.ClassType);
end;

// GetAddr
//
function TVarExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=exec.Stack.BasePointer+FStackAddr;
end;

// GetData
//
function TVarExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result:=exec.Stack.Data;
end;

// AssignData
//
procedure TVarExpr.AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer);
begin
   exec.Stack.WriteData(SourceAddr, Addr[exec], Typ.Size, SourceData);
end;

// AssignDataExpr
//
procedure TVarExpr.AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr);
begin
   exec.Stack.WriteData(DataExpr.Addr[exec], Addr[exec], Typ.Size, DataExpr.Data[exec]);
end;

// AssignExpr
//
procedure TVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
var
   buf : Variant;
begin
   Expr.EvalAsVariant(exec, buf);
   exec.Stack.WriteValue(Addr[exec], buf);
end;

// AssignValue
//
procedure TVarExpr.AssignValue(exec : TdwsExecution; const Value: Variant);
begin
   exec.Stack.WriteValue(Addr[exec], Value);
end;

// AssignValueAsInteger
//
procedure TVarExpr.AssignValueAsInteger(exec : TdwsExecution; const Value: Int64);
begin
   exec.Stack.WriteIntValue(Addr[exec], Value);
end;

// AssignValueAsBoolean
//
procedure TVarExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   exec.Stack.WriteBoolValue(Addr[exec], Value);
end;

// AssignValueAsFloat
//
procedure TVarExpr.AssignValueAsFloat(exec : TdwsExecution; const Value: Double);
begin
   exec.Stack.WriteFloatValue(Addr[exec], Value);
end;

// AssignValueAsString
//
procedure TVarExpr.AssignValueAsString(exec : TdwsExecution; const Value: String);
begin
   exec.Stack.WriteStrValue(Addr[exec], Value);
end;

// AssignValueAsScriptObj
//
procedure TVarExpr.AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj);
begin
   exec.Stack.WriteInterfaceValue(Addr[exec], value);
end;

// ------------------
// ------------------ TIntVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TIntVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   exec.Stack.WriteIntValue_BaseRelative(FStackAddr, Expr.EvalAsInteger(exec));
end;

// AssignValue
//
procedure TIntVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsInteger(exec, value);
end;

// AssignValueAsInteger
//
procedure TIntVarExpr.AssignValueAsInteger(exec : TdwsExecution; const value: Int64);
begin
   exec.Stack.WriteIntValue_BaseRelative(FStackAddr, value);
end;

// AssignValueAsPInteger
//
procedure TIntVarExpr.AssignValueAsPInteger(exec : TdwsExecution; const pValue: PInt64);
begin
   exec.Stack.WriteIntValue_BaseRelative(FStackAddr, pValue);
end;

// IncValue
//
procedure TIntVarExpr.IncValue(exec : TdwsExecution; const value: Int64);
begin
   exec.Stack.IncIntValue_BaseRelative(FStackAddr, value);
end;

// EvalAsInteger
//
function TIntVarExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=exec.Stack.ReadIntValue_BaseRelative(FStackAddr);
end;

// EvalAsFloat
//
function TIntVarExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=exec.Stack.ReadIntAsFloatValue_BaseRelative(FStackAddr);
end;

// EvalAsPInteger
//
function TIntVarExpr.EvalAsPInteger(exec : TdwsExecution) : PInt64;
begin
   Result:=exec.Stack.PointerToIntValue(exec.Stack.BasePointer+FStackAddr);
end;

// ------------------
// ------------------ TFloatVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TFloatVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   exec.Stack.WriteFloatValue_BaseRelative(FStackAddr, Expr.EvalAsFloat(exec));
end;

// AssignValue
//
procedure TFloatVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsFloat(exec, value);
end;

// AssignValueAsFloat
//
procedure TFloatVarExpr.AssignValueAsFloat(exec : TdwsExecution; const value: Double);
begin
   exec.Stack.WriteFloatValue_BaseRelative(FStackAddr, value);
end;

// EvalAsFloat
//
{$if Defined(WIN32_ASM)}
type
   TdwsExecutionCracker = class(TdwsExecution);
{$ifend}
function TFloatVarExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$if Defined(WIN32_ASM)}
asm
   lea   ecx, [edx].TdwsExecutionCracker.FStack
   mov   edx, [eax].FStackAddr
   mov   eax, ecx
   call  TStackMixIn.PointerToFloatValue_BaseRelative;
   fld   qword [eax]
{$else}
begin
   Result:=exec.Stack.PointerToFloatValue_BaseRelative(FStackAddr)^;
{$ifend}
end;

// ------------------
// ------------------ TStrVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TStrVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
var
   buf : String;
begin
   Expr.EvalAsString(exec, buf);
   exec.Stack.WriteStrValue(exec.Stack.BasePointer + FStackAddr, buf);
end;

// AssignValue
//
procedure TStrVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsString(exec, value);
end;

// AssignValueAsString
//
procedure TStrVarExpr.AssignValueAsString(exec : TdwsExecution; const value: String);
begin
   exec.Stack.WriteStrValue(exec.Stack.BasePointer + FStackAddr, value);
end;

// SetChar
//
function TStrVarExpr.SetChar(exec : TdwsExecution; index : Integer; c : WideChar) : Boolean;
begin
   Result:=exec.Stack.SetStrChar(exec.Stack.BasePointer + FStackAddr, index, c);
end;

// EvalAsString
//
procedure TStrVarExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   exec.Stack.ReadStrValue(exec.Stack.BasePointer + FStackAddr, Result);
end;

// Append
//
procedure TStrVarExpr.Append(exec : TdwsExecution; const value : String);
begin
   exec.Stack.AppendStringValue_BaseRelative(FStackAddr, value);
end;

// ------------------
// ------------------ TBoolVarExpr ------------------
// ------------------

// AssignExpr
//
procedure TBoolVarExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   exec.Stack.WriteBoolValue(exec.Stack.BasePointer + FStackAddr, Expr.EvalAsBoolean(exec));
end;

// AssignValue
//
procedure TBoolVarExpr.AssignValue(exec : TdwsExecution; const value: Variant);
begin
   AssignValueAsBoolean(exec, value);
end;

// AssignValueAsBoolean
//
procedure TBoolVarExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   exec.Stack.WriteBoolValue(exec.Stack.BasePointer + FStackAddr, value);
end;

function TBoolVarExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=exec.Stack.ReadBoolValue(exec.Stack.BasePointer + FStackAddr);
end;

// EvalAsInteger
//
function TBoolVarExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Int64(exec.Stack.ReadBoolValue(exec.Stack.BasePointer + FStackAddr));
end;

// ------------------
// ------------------ TObjectVarExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TObjectVarExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
type
   PUnknown = ^IUnknown;
begin
   exec.Stack.ReadInterfaceValue(exec.Stack.BasePointer + FStackAddr, PUnknown(@Result)^);
end;

// ------------------
// ------------------ TSelfVarExpr ------------------
// ------------------

// IsWritable
//
function TSelfVarExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TVarParentExpr ------------------
// ------------------

// Create
//
constructor TVarParentExpr.Create(prog : TdwsProgram; dataSym : TDataSymbol);
begin
   inherited;
   FLevel:=dataSym.Level;
end;

// GetAddr
//
function TVarParentExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=exec.Stack.GetSavedBp(FLevel)+FStackAddr;
end;

// ------------------
// ------------------ TByRefParamExpr ------------------
// ------------------

// CreateFromVarExpr
//
constructor TByRefParamExpr.CreateFromVarExpr(expr : TVarExpr);
begin
   FTyp:=expr.Typ;
   FStackAddr:=expr.FStackAddr;
   FDataSym:=expr.DataSym;
end;

// GetVarParamDataPointer
//
function TByRefParamExpr.GetVarParamDataAsPointer(exec : TdwsExecution) : Pointer;
var
   varData : PVarData;
begin
   varData:=@exec.Stack.Data[exec.Stack.BasePointer+FStackAddr];
   Assert(varData.VType=varUnknown);
   Result:=varData.VUnknown
end;

// GetVarParamData
//
procedure TByRefParamExpr.GetVarParamData(exec : TdwsExecution; var result : IVarParamData);
begin
   result:=IVarParamData(GetVarParamDataAsPointer(exec));
end;

// GetVarParamEval
//
function TByRefParamExpr.GetVarParamEval(exec : TdwsExecution) : PVariant;
begin
   Result:=IVarParamData(GetVarParamDataAsPointer(exec)).Eval;
end;

// GetAddr
//
function TByRefParamExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=IVarParamData(GetVarParamDataAsPointer(exec)).Addr;
end;

// GetData
//
function TByRefParamExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result:=IVarParamData(GetVarParamDataAsPointer(exec)).Data;
end;

// AssignData
//
procedure TByRefParamExpr.AssignData(exec : TdwsExecution; const sourceData : TData; sourceAddr : Integer);
begin
   DWSCopyData(sourceData, sourceAddr, Data[exec], Addr[exec], Typ.Size);
end;

// AssignValue
//
procedure TByRefParamExpr.AssignValue(exec : TdwsExecution; const value : Variant);
begin
   VarCopy(Data[exec][Addr[exec]], value);
end;

// AssignValueAsInteger
//
procedure TByRefParamExpr.AssignValueAsInteger(exec : TdwsExecution; const value : Int64);
begin
   Data[exec][Addr[exec]]:=value;
end;

// AssignValueAsBoolean
//
procedure TByRefParamExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   Data[exec][Addr[exec]]:=value;
end;

// AssignValueAsFloat
//
procedure TByRefParamExpr.AssignValueAsFloat(exec : TdwsExecution; const value : Double);
begin
   Data[exec][Addr[exec]]:=value;
end;

// AssignValueAsString
//
procedure TByRefParamExpr.AssignValueAsString(exec : TdwsExecution; const value : String);
begin
   Data[exec][Addr[exec]]:=value;
end;

// AssignValueAsScriptObj
//
procedure TByRefParamExpr.AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj);
begin
   Data[exec][Addr[exec]]:=value;
end;

// AssignExpr
//
procedure TByRefParamExpr.AssignExpr(exec : TdwsExecution; expr : TTypedExpr);
var
   v : PVariant;
begin
   v:=GetVarParamEval(exec);
   Expr.EvalAsVariant(exec, v^);
end;

// AssignDataExpr
//
procedure TByRefParamExpr.AssignDataExpr(exec : TdwsExecution; dataExpr: TDataExpr);
begin
   DWSCopyData(dataExpr.Data[exec], dataExpr.Addr[exec], Data[exec], Addr[exec], Typ.Size);
end;

// Eval
//
function TByRefParamExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=GetVarParamEval(exec)^;
end;

// ------------------
// ------------------ TConstParamExpr ------------------
// ------------------

// IsWritable
//
function TConstParamExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TByRefParentParamExpr ------------------
// ------------------

// Create
//
constructor TByRefParentParamExpr.Create(prog : TdwsProgram; dataSym : TDataSymbol);
begin
   inherited;
   FLevel := DataSym.Level;
end;

// GetAddr
//
function TByRefParentParamExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result := IVarParamData(IUnknown(exec.Stack.Data[exec.Stack.GetSavedBp(FLevel) + FStackAddr])).Addr;
end;

// GetData
//
function TByRefParentParamExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result := IVarParamData(IUnknown(exec.Stack.Data[exec.Stack.GetSavedBp(FLevel) + FStackAddr])).Data;
end;

// ------------------
// ------------------ TConstParamParentExpr ------------------
// ------------------

// IsWritable
//
function TConstParamParentExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TUnifiedConstList<TExprBase> ------------------
// ------------------

// Destroy
//
destructor TUnifiedConstList.Destroy;
var
   i : Integer;
begin
   Clean;
   FEmptyString.Free;
   for i:=Low(FIntegers) to High(FIntegers) do begin
      Assert(FIntegers[i].RefCount=0);
      FIntegers[i].Free;
   end;
   FZeroFloat.Free;
   inherited;
end;

// Precharge
//
procedure TUnifiedConstList.Precharge(prog : TdwsMainProgram; systemTable : TSystemSymbolTable);
const
   cEmptyString : String = '';
   cZeroFloat : Double = 0;
var
   i : Integer;
begin
   inherited Create;
   FEmptyString:=TConstStringExpr.CreateUnified(prog, systemTable.TypString, cEmptyString);
   for i:=Low(FIntegers) to High(FIntegers) do
      FIntegers[i]:=TConstIntExpr.CreateUnified(prog, systemTable.TypInteger, Int64(i));
   FZeroFloat:=TConstFloatExpr.CreateUnified(prog, systemTable.TypFloat, cZeroFloat);
end;

// Compare
//
function TUnifiedConstList.Compare(const item1, item2 : TExprBase) : Integer;
var
   unified1, unified2 : TUnifiedConstExpr;
   vd1, vd2 : PVarData;
   rawResult : Int64;
begin
   unified1:=TUnifiedConstExpr(item1);
   unified2:=TUnifiedConstExpr(item2);
   if unified1.ClassType=unified2.ClassType then begin
      if unified1.Typ=unified2.Typ then begin
         vd1:=@unified1.FData[0];
         vd2:=@unified2.FData[0];
         rawResult:=Integer(vd1.VType)-Integer(vd2.VType);
         if rawResult=0 then begin
            case vd1.VType of
               {$ifdef FPC}
               varString : rawResult:=CompareStr(String(vd1.VString), String(vd2.VString));
               {$else}
               varUString : rawResult:=CompareStr(String(vd1.VUString), String(vd2.VUString));
               {$endif}
               varInt64 : rawResult:=vd1.VInt64-vd2.VInt64;
               varBoolean : rawResult:=Integer(vd1.VBoolean)-Integer(vd2.VBoolean);
            else
               case VarCompareValue(unified1.FData[0], unified2.FData[0]) of
                  vrEqual : rawResult:=0;
                  vrLessThan : rawResult:=-1;
                  vrGreaterThan : rawResult:=1;
               else
                  rawResult:=0;
                  Assert(False);
               end;
            end;
         end;
      end else rawResult:=NativeInt(unified1.Typ)-NativeInt(unified2.Typ);
   end else rawResult:=NativeInt(unified1.ClassType)-NativeInt(unified2.ClassType);

   if rawResult=0 then
      Result:=0
   else if rawResult>0 then
      Result:=1
   else Result:=-1;
end;

// ------------------
// ------------------ TConstExpr ------------------
// ------------------

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   inherited Create(Prog, Typ);
   SetLength(FData, Typ.Size);
   case Typ.Size of
      0 : ;
      1 : FData[0] := Value;
   else
      Assert(False);
   end;
end;

// Create
//
constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer);
begin
   inherited Create(Prog, Typ);
   SetLength(FData, Typ.Size);
   DWSCopyData(Data, addr, FData, 0, Typ.Size);
end;

// Eval
//
function TConstExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result := FData[0];
end;

// IsConstant
//
function TConstExpr.IsConstant : Boolean;
begin
   Result:=True;
end;

// IsWritable
//
function TConstExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// SameValueAs
//
function TConstExpr.SameValueAs(otherConst : TConstExpr) : Boolean;
begin
   Result:=   (Length(FData)=Length(otherConst.FData))
           and DWSSameData(FData, otherConst.FData, 0, 0, Length(FData));
end;

// GetData
//
function TConstExpr.GetData(exec : TdwsExecution) : TData;
begin
  Result := FData;
end;

// CreateTypedVariantValue
//
class function TConstExpr.CreateTypedVariantValue(
   prog : TdwsProgram; typ : TTypeSymbol; const value : Variant) : TConstExpr;
begin
   if typ=prog.TypString then
      Result:=TConstStringExpr.CreateUnified(prog, typ, value)
   else if typ.ClassType=TDynamicArraySymbol then
      Result:=CreateDynamicArrayValue(prog, typ)
   else if (typ=prog.TypInteger) or (typ.typ=prog.TypInteger) then
      Result:=CreateIntegerValue(prog, typ, value)
   else if typ=prog.TypBoolean then
      Result:=CreateBooleanValue(prog, value)
   else if typ=prog.TypFloat then
      Result:=TConstFloatExpr.CreateUnified(prog, typ, value)
   else Result:=TConstExpr.Create(prog, typ, value);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; const Data: TData; addr : Integer = 0) : TConstExpr;
begin
   case Length(Data) of
      0 : Result:=TConstExpr.Create(Prog, Typ, Null);
      1 : Result:=TConstExpr.CreateTypedVariantValue(Prog, Typ, Data[addr]);
   else
      Result:=TConstExpr.Create(Prog, Typ, Data, addr);
   end;
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TTypeSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   Assert(constSymbol<>nil);
   Result:=CreateTyped(Prog, Typ, constSymbol.Data);
end;

// CreateIntegerValue
//
class function TConstExpr.CreateIntegerValue(prog : TdwsProgram; const value : Int64) : TConstExpr;
begin
   Result:=TConstIntExpr.CreateUnified(prog, Prog.TypInteger, value);
end;

// CreateIntegerValue
//
class function TConstExpr.CreateIntegerValue(prog : TdwsProgram; typ : TTypeSymbol; const value : Int64) : TConstExpr;
begin
   Result:=TConstIntExpr.CreateUnified(prog, typ, value);
end;

// CreateBooleanValue
//
class function TConstExpr.CreateBooleanValue(prog : TdwsProgram; const value : Boolean) : TConstExpr;
begin
   Result:=TConstBooleanExpr.CreateUnified(prog, prog.TypBoolean, value);
end;

// CreateDynamicArrayValue
//
class function TConstExpr.CreateDynamicArrayValue(prog : TdwsProgram; typ : TTypeSymbol) : TConstExpr;
begin
   Result:=TConstExpr.Create(prog, typ, TScriptDynamicArray.Create(typ.Typ) as IScriptObj);
end;

// ------------------
// ------------------ TUnifiedConstExpr ------------------
// ------------------

// CreateUnified
//
class function TUnifiedConstExpr.CreateUnified(Prog: TdwsProgram; Typ: TTypeSymbol;
                                               const Value: Variant) : TUnifiedConstExpr;
var
   i : Integer;
   added : Boolean;
begin
   Result:=Self.Create(Prog, Typ, Value);

   i:=Prog.Root.UnifiedConstList.AddOrFind(Result, added);
   if not added then begin
      Result.Free;
      Result:=TUnifiedConstExpr(Prog.Root.UnifiedConstList[i]);
   end;
   Result.IncRefCount;
end;

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypBoolean;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstBooleanExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Integer(FValue);
end;

// EvalAsBoolean
//
function TConstBooleanExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TConstIntExpr ------------------
// ------------------

// Create
//
constructor TConstIntExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypInteger;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
{$if Defined(WIN32_ASM)}
asm
   mov   edx, [eax + OFFSET FValue + 4]
   mov   eax, [eax + OFFSET FValue]
{$else}
begin
   Result:=FValue;
{$ifend}
end;

// EvalAsFloat
//
function TConstIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$if Defined(WIN32_ASM)}
asm
   fild  qword [eax + OFFSET FValue]
{$else}
begin
   Result:=FValue;
{$ifend}
end;

// ------------------
// ------------------ TConstFloatExpr ------------------
// ------------------

// Create
//
constructor TConstFloatExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypFloat;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsFloat
//
function TConstFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$if Defined(WIN32_ASM)}
asm
   fld qword [eax].FValue
{$else}
begin
   Result:=FValue;
{$ifend}
end;

// ------------------
// ------------------ TConstStringExpr ------------------
// ------------------

// Create
//
constructor TConstStringExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypString;
   UnifyAssignString(Value, FValue);
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TArrayTypedExpr ------------------
// ------------------

// Create
//
constructor TArrayTypedExpr.Create(prog: TdwsProgram; const scriptPos: TScriptPos);
begin
   FScriptPos:=scriptPos;
end;

// ScriptPos
//
function TArrayTypedExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// BoundsCheck
//
procedure TArrayTypedExpr.BoundsCheck(exec : TdwsExecution; dynArray : TScriptDynamicArray; i : Integer);
begin
   if Cardinal(i)>=Cardinal(dynArray.Length) then begin
      if i>=dynArray.Length then
         RaiseUpperExceeded(exec, i)
      else if i<0 then
         RaiseLowerExceeded(exec, i);
   end;
end;

// ------------------
// ------------------ TNewArrayExpr ------------------
// ------------------

// Create
//
constructor TNewArrayExpr.Create(prog: TdwsProgram; const scriptPos: TScriptPos;
                                 elementTyp : TTypeSymbol);
begin
   inherited Create(prog, scriptPos);
   FTyp:=TDynamicArraySymbol.Create('', elementTyp, prog.TypInteger);
   FTyps.Add(FTyp);
end;

// Destroy
//
destructor TNewArrayExpr.Destroy;
begin
   inherited;
   FTyps.Clean;
   FLengthExprs.Clean;
end;

// Eval
//
function TNewArrayExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   EvalAsScriptObj(exec, obj);
   Result:=obj;
end;

// EvalAsScriptObj
//
procedure TNewArrayExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   function CreateDimension(d : Integer) : TScriptDynamicArray;
   var
      i : Integer;
      n : Int64;
   begin
      n:=LengthExpr[d].EvalAsInteger(exec);
      if n<0 then
         RaiseScriptError(exec, EScriptOutOfBounds.CreatePosFmt(FScriptPos, RTE_ArrayLengthIncorrectForDimension, [n, d]));
      Result:=TScriptDynamicArray.Create(TDynamicArraySymbol(FTyps.List[FTyps.Count-1-d]).Typ);
      Result.Length:=n;
      Inc(d);
      if d<LengthExprCount then begin
         for i:=0 to n-1 do
            Result.Data[i]:=IScriptObj(CreateDimension(d));
      end;
   end;

begin
   Result:=CreateDimension(0);
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
constructor TArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TTypedExpr);
begin
   inherited Create(Prog, Pos, BaseExpr.BaseType.Typ);
   FBaseExpr := BaseExpr;
   FIndexExpr := IndexExpr;
   FElementSize := Typ.Size; // Necessary because of arrays of records!
   FTyp:=FBaseExpr.Typ.Typ;
end;

// Destroy
//
destructor TArrayExpr.Destroy;
begin
   FBaseExpr.Free;
   FIndexExpr.Free;
   inherited;
end;

// IsWritable
//
function TArrayExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
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

// ------------------
// ------------------ TStaticArrayExpr ------------------
// ------------------

// Create
//
constructor TStaticArrayExpr.Create(prog : TdwsProgram; const pos : TScriptPos;
                                    baseExpr : TDataExpr; indexExpr : TTypedExpr;
                                    lowBound, highBound : Integer);
begin
   inherited Create(Prog, Pos, BaseExpr, IndexExpr);
   FLowBound:=LowBound;
   FCount:=HighBound-LowBound+1;
end;

// IsConstant
//
function TStaticArrayExpr.IsConstant : Boolean;
begin
   Result:=BaseExpr.IsConstant and IndexExpr.IsConstant;
end;

// Optimize
//
function TStaticArrayExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   v : Variant;
begin
   Result:=Self;
   if IsConstant then begin
      if Typ.Size=1 then begin
         EvalAsVariant(exec, v);
         Result:=TConstExpr.CreateTypedVariantValue(prog, Typ, v);
         Free;
      end;
   end;
end;

// AssignExpr
//
procedure TStaticArrayExpr.AssignExpr(exec : TdwsExecution; expr : TTypedExpr);
begin
   expr.EvalAsVariant(exec, FBaseExpr.Data[exec][GetAddr(exec)]);
end;

// Eval
//
function TStaticArrayExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=FBaseExpr.Data[exec][GetAddr(exec)];
end;

// GetAddr
//
function TStaticArrayExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   index : Integer;
begin
   // Get index
   index:=FIndexExpr.EvalAsInteger(exec)-FLowBound;

   if Cardinal(index)>=Cardinal(FCount) then begin
      if index>=FCount then
         RaiseUpperExceeded(exec, index+FLowBound)
      else RaiseLowerExceeded(exec, index+FLowBound);
   end;
   // Calculate the address
   Result:=FBaseExpr.Addr[exec]+(index*FElementSize);
end;

// GetData
//
function TStaticArrayExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result:=FBaseExpr.Data[exec];
end;

// ------------------
// ------------------ TStaticArrayBoolExpr ------------------
// ------------------

// GetAddr
//
function TStaticArrayBoolExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   index : Integer;
begin
   if FIndexExpr.EvalAsBoolean(exec) then
      index:=FElementSize
   else index:=0;
   Result:=FBaseExpr.Addr[exec]+index;
end;

// ------------------
// ------------------ TOpenArrayExpr ------------------
// ------------------

// GetAddr
//
function TOpenArrayExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   index, len: Integer;
begin
   index := FIndexExpr.EvalAsInteger(exec);

   len := Length(FBaseExpr.Data[exec]);

   if Cardinal(index)>=Cardinal(len) then begin
      if index >= len then
         RaiseUpperExceeded(exec, index)
      else if index < 0 then
         RaiseLowerExceeded(exec, index);
   end;
   // Calculate the address
   Result := index;
end;

// GetData
//
function TOpenArrayExpr.GetData(exec : TdwsExecution) : TData;
begin
  Result := FBaseExpr.Data[exec];
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

// GetAddr
//
function TDynamicArrayExpr.GetAddr(exec : TdwsExecution) : Integer;
var
   base : IScriptObj;
begin
   FBaseExpr.EvalAsScriptObj(exec, base);
   Result:=DynGetAddr(exec, TScriptDynamicArray(base.InternalObject))
end;

// GetData
//
function TDynamicArrayExpr.GetData(exec : TdwsExecution) : TData;
var
   base : IScriptObj;
begin
   FBaseExpr.EvalAsScriptObj(exec, base);
   Result:=base.Data
end;

// DynGetAddr
//
function TDynamicArrayExpr.DynGetAddr(exec : TdwsExecution; dynArray : TScriptDynamicArray) : Integer;
var
   index : Integer;
begin
   index:=IndexExpr.EvalAsInteger(exec);

   if Cardinal(index)>=Cardinal(dynArray.Length) then begin
      if index<0 then
         RaiseLowerExceeded(exec, index)
      else RaiseUpperExceeded(exec, index);
   end;

   Result:=index*FElementSize;
end;

// EvalItem
//
function TDynamicArrayExpr.EvalItem(exec : TdwsExecution) : PVariant;
var
   dynArray : TScriptDynamicArray;
   addr : Integer;
   base : IScriptObj;
begin
   FBaseExpr.EvalAsScriptObj(exec, base);
   dynArray:=TScriptDynamicArray(base.InternalObject);
   addr:=DynGetAddr(exec, dynArray);
   Result:=@dynArray.Data[addr];
end;

// Eval
//
function TDynamicArrayExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalItem(exec)^;
end;

// EvalAsInteger
//
function TDynamicArrayExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   p : PVarData;
begin
   p:=PVarData(EvalItem(exec));
   if p.VType=vtInt64 then
      Result:=p.VInt64
   else Result:=PVariant(p)^;
end;

// AssignExpr
//
procedure TDynamicArrayExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   Assert(IsWritable);
   Expr.EvalAsVariant(exec, EvalItem(exec)^);
end;

// ------------------
// ------------------ TDynamicArraySetExpr ------------------
// ------------------

// Create
//
constructor TDynamicArraySetExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                            arrayExpr, indexExpr, valueExpr : TTypedExpr);
begin
   inherited Create(prog, scriptPos);
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
   base : IScriptObj;
   dataExpr : TDataExpr;
begin
   FArrayExpr.EvalAsScriptObj(exec, base);
   dynArray:=TScriptDynamicArray(base.InternalObject);
   index:=IndexExpr.EvalAsInteger(exec);
   if Cardinal(index)>=Cardinal(dynArray.Length) then begin
      if index<0 then
         RaiseLowerExceeded(exec, index)
      else RaiseUpperExceeded(exec, index);
   end;

   if dynArray.ElementSize=1 then begin

      ValueExpr.EvalAsVariant(exec, dynArray.Data[index]);

   end else begin

      dataExpr:=(ValueExpr as TDataExpr);
      DWSCopyData(dataExpr.Data[exec], dataExpr.Addr[exec],
                  dynArray.Data, index*dynArray.ElementSize,
                  dynArray.ElementSize);

   end;
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
// ------------------ TArrayConstantExpr ------------------
// ------------------

// Create
//
constructor TArrayConstantExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   inherited Create(prog, pos,
      TStaticArraySymbol.Create('', prog.TypNil, prog.TypInteger, 0, -1));
end;

// Destroy
//
destructor TArrayConstantExpr.Destroy;
begin
   FElementExprs.Clean;
   FTyp.Free;
   inherited;
end;

// AddElementExpr
//
procedure TArrayConstantExpr.AddElementExpr(Prog: TdwsProgram; ElementExpr: TTypedExpr);
var
   arraySymbol : TStaticArraySymbol;
begin
   arraySymbol:=(FTyp as TStaticArraySymbol);
   if arraySymbol.Typ<>Prog.TypVariant then begin
      if arraySymbol.Typ=Prog.TypNil then
         arraySymbol.Typ:=ElementExpr.Typ
      else
      if arraySymbol.Typ<>ElementExpr.Typ then begin
         if arraySymbol.Typ=Prog.TypNil then
            arraySymbol.Typ:=ElementExpr.Typ
         else if (arraySymbol.Typ=Prog.TypInteger) and (ElementExpr.Typ=Prog.TypFloat) then
            arraySymbol.Typ:=Prog.TypFloat
         else if not ((arraySymbol.Typ=Prog.TypFloat) and (ElementExpr.Typ=Prog.TypInteger)) then
            arraySymbol.Typ:=Prog.TypVariant;
      end;
   end;
   FElementExprs.Add(ElementExpr);
   arraySymbol.AddElement;
end;

// Prepare
//
procedure TArrayConstantExpr.Prepare(prog: TdwsProgram; elementTyp : TTypeSymbol);
var
   x, n : Integer;
   elemExpr : TTypedExpr;
begin
   if (elementTyp<>nil) and (FTyp.Typ<>elementTyp) then begin
      if     elementTyp.IsCompatible(FTyp.Typ)
          or (elementTyp.IsOfType(prog.TypFloat) and FTyp.Typ.IsOfType(prog.TypInteger)) then
         (FTyp as TStaticArraySymbol).Typ:=elementTyp;
   end;

   for x := 0 to FElementExprs.Count - 1 do begin
      elemExpr:=TTypedExpr(FElementExprs.List[x]);
      if elemExpr is TArrayConstantExpr then
         TArrayConstantExpr(elemExpr).Prepare(prog, FTyp.Typ);
   end;

   if FTyp.Typ<>nil then
      n:=FElementExprs.Count * FTyp.Typ.Size
   else n:=0;
   FArrayAddr := prog.GetGlobalAddr(n);
end;

// GetData
//
function TArrayConstantExpr.GetData(exec : TdwsExecution) : TData;
begin
  Eval(exec);
  Result := exec.Stack.Data;// FData;
end;

// GetAddr
//
function TArrayConstantExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=FArrayAddr;
end;

// GetSubExpr
//
function TArrayConstantExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=TExprBase(FElementExprs.List[i]);
end;

// GetSubExprCount
//
function TArrayConstantExpr.GetSubExprCount : Integer;
begin
   Result:=FElementExprs.Count;
end;

// GetElement
//
function TArrayConstantExpr.GetElement(idx : Integer) : TTypedExpr;
begin
   Result:=TTypedExpr(FElementExprs.List[idx]);
end;

// GetElementCount
//
function TArrayConstantExpr.GetElementCount : Integer;
begin
   Result:=FElementExprs.Count;
end;

function TArrayConstantExpr.Eval(exec : TdwsExecution) : Variant;

   procedure DoEval;
   var
      x, addr : Integer;
      elemSize : Integer;
      elemExpr : TTypedExpr;
      dataExpr : TDataExpr;
   begin
      exec.Stack.WriteValue(FArrayAddr, FElementExprs.Count);

      elemSize:=Typ.Typ.Size;
      addr:=FArrayAddr;
      for x:=0 to FElementExprs.Count-1 do begin
         elemExpr:=TTypedExpr(FElementExprs.List[x]);
         if elemSize=1 then begin
            exec.Stack.WriteValue(addr, elemExpr.Eval(exec));
         end else begin
            dataExpr:=elemExpr as TDataExpr;
            exec.Stack.WriteData(dataExpr.Addr[exec], addr, elemSize, dataExpr.Data[exec]);
         end;
         Inc(addr, elemSize);
      end;
   end;

begin
   if not FArrayEvaled then begin
      FArrayEvaled:=True;
      DoEval;
   end;

   Result:=FArrayAddr;
end;

// EvalAsTData
//
function TArrayConstantExpr.EvalAsTData(exec : TdwsExecution) : TData;
var
   i : Integer;
   expr : TTypedExpr;
begin
   SetLength(Result, FElementExprs.Count);
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      expr.EvalAsVariant(exec, Result[i]);
   end;
end;

// EvalAsVarRecArray
//
function TArrayConstantExpr.EvalAsVarRecArray(exec : TdwsExecution) : TVarRecArrayContainer;
var
   i : Integer;
   expr : TTypedExpr;
   buf : Variant;
begin
   Result:=TVarRecArrayContainer.Create;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      expr.EvalAsVariant(exec, buf);
      Result.Add(buf);
   end;
   Result.Initialize;
end;

// Optimize
//
function TArrayConstantExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   i : Integer;
   expr : TTypedExpr;
begin
   Result:=Self;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TTypedExpr(FElementExprs.List[i]);
      FElementExprs.List[i]:=expr.Optimize(prog, exec);
   end;
end;

// IsConstant
//
function TArrayConstantExpr.IsConstant : Boolean;
var
   i : Integer;
begin
   for i:=0 to FElementExprs.Count-1 do
      if not TTypedExpr(FElementExprs.List[i]).IsConstant then
         Exit(False);
   Result:=True;
end;

// IsWritable
//
function TArrayConstantExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// TypeCheckElements
//
procedure TArrayConstantExpr.TypeCheckElements(prog : TdwsProgram);
var
   x : Integer;
   expr : TTypedExpr;
   elemTyp : TTypeSymbol;
begin
   if Typ.Typ=nil then
      prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_InvalidConstType, [SYS_VOID])
   else if FElementExprs.Count>0 then begin
      elemTyp:=Elements[0].Typ;
      for x:=1 to FElementExprs.Count-1 do begin
         expr:=Elements[x];
         if not elemTyp.IsCompatible(expr.Typ) then begin
            if elemTyp.IsOfType(prog.TypInteger) and expr.Typ.IsOfType(prog.TypFloat) then
               elemTyp:=prog.TypFloat
            else if elemTyp.IsOfType(prog.TypFloat) and expr.Typ.IsOfType(prog.TypInteger) then
               // handled below
            else if expr.Typ.IsCompatible(elemTyp) then
               elemTyp:=expr.Typ
            else if (expr.Typ is TStructuredTypeSymbol) and (elemTyp is TStructuredTypeSymbol) then begin
               repeat
                  elemTyp:=TStructuredTypeSymbol(elemTyp).Parent;
                  if elemTyp=nil then begin
                     prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_AssignIncompatibleTypes,
                                                          [expr.Typ.Caption, Elements[0].Typ.Caption]);
                     Exit;
                  end;
               until elemTyp.IsCompatible(expr.Typ);
            end else if prog.TypVariant.IsCompatible(expr.Typ) and prog.TypVariant.IsCompatible(elemTyp) then
               elemTyp:=prog.TypVariant
            else begin
               prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_AssignIncompatibleTypes,
                                                    [expr.Typ.Caption, elemTyp.Caption]);
               Exit;
            end;
         end;
      end;

      // implicit cast integer to float
      if elemTyp.IsOfType(Prog.TypFloat) then begin
         for x:=1 to FElementExprs.Count-1 do begin
            expr:=Elements[x];
            if expr.Typ.IsOfType(Prog.TypInteger) then begin
               expr:=TConvFloatExpr.Create(Prog, expr);
               FElementExprs.List[x]:=expr;
            end;
         end;
      end;

      Typ.Typ:=elemTyp;
   end;
end;

// ------------------
// ------------------ TRecordExpr ------------------
// ------------------

// Create
//
constructor TRecordExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                               BaseExpr: TDataExpr; fieldSymbol: TFieldSymbol);
begin
   inherited Create(Prog, Pos, fieldSymbol.Typ);
   FBaseExpr := BaseExpr;
   FMemberOffset := fieldSymbol.Offset;
end;

// Destroy
//
destructor TRecordExpr.Destroy;
begin
   FBaseExpr.Free;
   inherited;
end;

// IsConstant
//
function TRecordExpr.IsConstant : Boolean;
begin
   Result:=BaseExpr.IsConstant;
end;

// Eval
//
function TRecordExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=FBaseExpr.Data[exec][FBaseExpr.Addr[exec]+FMemberOffset];
end;

// GetAddr
//
function TRecordExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result:=FBaseExpr.Addr[exec]+FMemberOffset;
end;

// GetData
//
function TRecordExpr.GetData(exec : TdwsExecution) : TData;
begin
   Result:=FBaseExpr.Data[exec];
end;

// GetSubExpr
//
function TRecordExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FBaseExpr;
end;

// GetSubExprCount
//
function TRecordExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// IsWritable
//
function TRecordExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

// ------------------
// ------------------ TInitDataExpr ------------------
// ------------------

// Create
//
constructor TInitDataExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr);
begin
   inherited Create(Prog, Pos);
   FExpr := Expr;
end;

// Destroy
//
destructor TInitDataExpr.Destroy;
begin
   FExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TInitDataExpr.EvalNoResult(exec : TdwsExecution);
begin
   FExpr.Typ.InitData(FExpr.Data[exec], FExpr.Addr[exec]);
end;

// GetSubExpr
//
function TInitDataExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExpr;
end;

// GetSubExprCount
//
function TInitDataExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TDynamicRecordExpr ------------------
// ------------------

// Create
//
constructor TDynamicRecordExpr.Create(aProg : TdwsProgram; const aPos : TScriptPos;
                                      recordTyp : TRecordSymbol);
begin
   inherited Create(aProg, aPos, recordTyp);
   FAddr:=aProg.GetTempAddr(recordTyp.Size);
end;

// EvalNoResult
//
procedure TDynamicRecordExpr.EvalNoResult(exec : TdwsExecution);
var
   recType : TRecordSymbol;
   sym : TSymbol;
   expr : TExprBase;
   dataExpr : TDataExpr;
   fieldSym : TFieldSymbol;
begin
   recType:=TRecordSymbol(Typ);
   for sym in recType.Members do begin
      if sym.ClassType=TFieldSymbol then begin
         fieldSym:=TFieldSymbol(sym);
         expr:=fieldSym.DefaultExpr;
         if expr=nil then
            fieldSym.InitData(exec.Stack.Data, exec.Stack.BasePointer+FAddr)
         else if expr is TDataExpr then begin
            dataExpr:=TDataExpr(expr);
            DWSCopyData(dataExpr.Data[exec], dataExpr.Addr[exec],
                        exec.Stack.Data, exec.Stack.BasePointer+FAddr+fieldSym.Offset,
                        fieldSym.Size);
         end else expr.EvalAsVariant(exec, exec.Stack.Data[exec.Stack.BasePointer+FAddr+fieldSym.Offset]);
      end;
   end;
end;

// GetAddr
//
function TDynamicRecordExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
  Result:=exec.Stack.BasePointer+FAddr;
end;

// GetData
//
function TDynamicRecordExpr.GetData(exec : TdwsExecution) : TData;
begin
   EvalNoResult(exec);
   Result:=exec.Stack.Data;
end;

// GetSubExpr
//
function TDynamicRecordExpr.GetSubExpr(i : Integer) : TExprBase;
var
   recType : TRecordSymbol;
   sym : TSymbol;
   k : Integer;
begin
   recType:=TRecordSymbol(Typ);
   for k:=0 to recType.Members.Count-1 do begin
      sym:=recType.Members[k];
      if sym.ClassType=TFieldSymbol then begin
         Result:=TFieldSymbol(sym).DefaultExpr;
         if i=0 then
            Exit
         else Dec(i);
      end;
   end;
   Result:=nil;
end;

// GetSubExprCount
//
function TDynamicRecordExpr.GetSubExprCount : Integer;
var
   recType : TRecordSymbol;
   sym : TSymbol;
   k : Integer;
begin
   Result:=0;
   recType:=TRecordSymbol(Typ);
   for k:=0 to recType.Members.Count-1 do begin
      sym:=recType.Members[k];
      if sym.ClassType=TFieldSymbol then begin
         if TFieldSymbol(sym).DefaultExpr<>nil then
            Inc(Result);
      end;
   end;
end;

// ------------------
// ------------------ TFieldExpr ------------------
// ------------------

// Create
//
constructor TFieldExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                              fieldSym: TFieldSymbol; ObjExpr: TTypedExpr);
begin
   inherited Create(Prog, Pos, fieldSym.Typ);
   FObjectExpr := ObjExpr;
   FFieldAddr := FieldSym.Offset;
end;

// Destroy
//
destructor TFieldExpr.Destroy;
begin
   FObjectExpr.Free;
   inherited;
end;

// GetAddr
//
function TFieldExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
   Result := FFieldAddr;
end;

// GetData
//
function TFieldExpr.GetData(exec : TdwsExecution) : TData;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.Data;
end;

// GetSubExpr
//
function TFieldExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FObjectExpr;
end;

// GetSubExprCount
//
function TFieldExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// Eval
//
function TFieldExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.DataOfAddr(FFieldAddr);
end;

// EvalAsString
//
procedure TFieldExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.DataOfAddrAsString(FFieldAddr);
end;

// EvalAsInteger
//
function TFieldExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   Result:=obj.DataOfAddrAsInteger(FFieldAddr);
end;

// EvalAsScriptObj
//
procedure TFieldExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(exec, obj);
   CheckScriptObject(exec, obj);
   obj.DataOfAddrAsScriptObj(FFieldAddr, Result);
end;

// ------------------
// ------------------ TReadOnlyFieldExpr ------------------
// ------------------

// IsWritable
//
function TReadOnlyFieldExpr.IsWritable: Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TLazyParamExpr ------------------
// ------------------

// Create
//
constructor TLazyParamExpr.Create(Prog: TdwsProgram; dataSym : TLazyParamSymbol);
begin
   FDataSym:=dataSym;
   FTyp:=dataSym.Typ;
   FLevel:=dataSym.Level;
   FStackAddr:=dataSym.StackAddr;
end;

// Eval
//
function TLazyParamExpr.Eval(exec : TdwsExecution) : Variant;
var
   lazyExpr : TExprBase;
   oldBasePointer: Integer;
   lazyContext : Int64;
begin
   lazyContext:=exec.Stack.ReadIntValue(exec.Stack.BasePointer + FStackAddr);
   lazyExpr:=TExprBase(lazyContext and $FFFFFFFF);

   oldBasePointer:=exec.Stack.BasePointer;
   exec.Stack.BasePointer:=(lazyContext shr 32);//  stack.GetSavedBp(Level);
   try
      lazyExpr.EvalAsVariant(exec, Result);
   finally
      exec.Stack.BasePointer:=oldBasePointer;
   end;
end;

// ------------------
// ------------------ TArrayLengthExpr ------------------
// ------------------

// Create
//
constructor TArrayLengthExpr.Create(prog : TdwsProgram; expr : TTypedExpr; captureExpr : Boolean);
begin
   inherited Create(prog, expr);
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

// EvalAsInteger
//
function TArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   obj : IScriptObj;
begin
   FExpr.EvalAsScriptObj(exec, obj);
   Result:=TScriptDynamicArray(obj.InternalObject).Length+FDelta
end;

// ------------------
// ------------------ TOpenArrayLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TOpenArrayLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Length(TDataExpr(FExpr).Data[exec])+FDelta;
end;

// ------------------
// ------------------ TStringArrayOpExpr ------------------
// ------------------

// CreatePos
//
constructor TStringArrayOpExpr.CreatePos(Prog: TdwsProgram; const Pos: TScriptPos;
                                         Left, Right: TTypedExpr);
begin
   inherited Create(Prog, Left, Right);
   FPos := Pos;
end;

// EvalAsString
//
procedure TStringArrayOpExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   i : Integer;
   {$ifdef FPC}
   n : Integer;
   {$endif}
   buf : UnicodeString;
begin
   {$ifdef FPC}
   FLeft.EvalAsUnicodeString(exec, buf);
   {$else}
   FLeft.EvalAsString(exec, buf);
   {$endif}
   i:=FRight.EvalAsInteger(exec);
   if i>Length(buf) then
      RaiseUpperExceeded(exec, i)
   else if i<1 then
      RaiseLowerExceeded(exec, i);
   {$ifdef FPC}
   n:=Ord(buf[i]);
   if (n>=$D800) and (n<=$E000) then
      n:=n+$10000;
   Result:=UnicodeToUTF8(n);
   {$else}
   Result:=buf[i];
   {$endif}
end;

// ScriptPos
//
function TStringArrayOpExpr.ScriptPos : TScriptPos;
begin
   Result:=FPos;
end;

// ------------------
// ------------------ TStringLengthExpr ------------------
// ------------------

// EvalAsInteger
//
function TStringLengthExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   buf : UnicodeString;
begin
   {$ifdef FPC}
   FExpr.EvalAsUnicodeString(exec, buf);
   {$else}
   FExpr.EvalAsString(exec, buf);
   {$endif}
   Result:=Length(buf);
end;

// ------------------
// ------------------ TIsOpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TIsOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   scriptObj : IScriptObj;
begin
   FLeft.EvalAsScriptObj(exec, scriptObj);
   Result:=Assigned(scriptObj) and FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym);
end;

// ------------------
// ------------------ TAsCastExpr ------------------
// ------------------

// Create
//
constructor TAsCastExpr.Create(prog : TdwsProgram; const aPos : TScriptPos;
                               expr : TTypedExpr; toTyp : TTypeSymbol);
begin
   inherited Create(prog, expr);
   FPos:=aPos;
   FTyp:=toTyp;
end;

// Eval
//
function TAsCastExpr.Eval(exec : TdwsExecution) : Variant;
var
   scriptObj : IScriptObj;
begin
   EvalAsScriptObj(exec, scriptObj);
   Result:=scriptObj;
end;

// ------------------
// ------------------ TClassAsClassExpr ------------------
// ------------------

// RaiseMetaClassCastFailed
//
procedure TClassAsClassExpr.RaiseMetaClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);
begin
   RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_MetaClassCastFailed,
                                                  [classSym.Caption, FTyp.Name]))
end;

// Eval
//
function TClassAsClassExpr.Eval(exec : TdwsExecution) : Variant;
var
   ref : TClassSymbol;
begin
   ref:=TClassSymbol(Expr.EvalAsInteger(exec));
   Result:=Int64(ref);

   if ref<>nil then begin
      if not FTyp.IsCompatible(ref.MetaSymbol) then
         RaiseMetaClassCastFailed(exec, ref);
   end;
end;

// ------------------
// ------------------ TObjAsClassExpr ------------------
// ------------------

// RaiseInstanceClassCastFailed
//
procedure TObjAsClassExpr.RaiseInstanceClassCastFailed(exec : TdwsExecution; classSym : TClassSymbol);
begin
   RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_ClassInstanceCastFailed,
                                                  [classSym.Caption, FTyp.Caption]))
end;

// EvalAsScriptObj
//
procedure TObjAsClassExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) and not (FTyp.IsCompatible(Result.ClassSym)) then
      RaiseInstanceClassCastFailed(exec, Result.ClassSym);
end;

// ------------------
// ------------------ TObjToClassTypeExpr ------------------
// ------------------

// Create
//
constructor TObjToClassTypeExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   inherited Create(prog, expr);
   Typ:=(expr.Typ as TStructuredTypeSymbol).MetaSymbol;
end;

// Eval
//
function TObjToClassTypeExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   Expr.EvalAsScriptObj(exec, obj);
   if obj=nil then
      Result:=Int64(0)
   else Result:=Int64(obj.ClassSym);
end;

// ------------------
// ------------------ TInOpExpr ------------------
// ------------------

// Create
//
constructor TInOpExpr.Create(Prog: TdwsProgram; Left: TTypedExpr);
begin
   FLeft:=Left;
   FTyp:=Prog.TypBoolean;
end;

// Destroy
//
destructor TInOpExpr.Destroy;
begin
   FLeft.Free;
   FCaseConditions.Clean;
   inherited;
end;

// Eval
//
function TInOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsBoolean(exec);
end;

// EvalAsBoolean
//
function TInOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   i : Integer;
   value : Variant;
   cc : TCaseCondition;
begin
   FLeft.EvalAsVariant(exec, value);
   for i:=0 to FCaseConditions.Count-1 do begin
      cc:=TCaseCondition(FCaseConditions.List[i]);
      if cc.IsTrue(exec, Value) then
         Exit(True);
   end;
   Result:=False;
end;

// ConstantConditions
//
function TInOpExpr.ConstantConditions : Boolean;
var
   i : Integer;
begin
   for i:=0 to FCaseConditions.Count-1 do
      if not TCaseCondition(FCaseConditions.List[i]).IsConstant then
         Exit(False);
   Result:=True;
end;

// IsConstant
//
function TInOpExpr.IsConstant : Boolean;
begin
   Result:=FLeft.IsConstant and ConstantConditions;
end;

// Optimize
//
function TInOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   enumSym : TEnumerationSymbol;
   value : Variant;
   i, k, mask : Integer;
   cc : TCaseCondition;
begin
   Result:=Self;
   // if left is an enumeration with 31 or less symbols (31 is limit for JS)
   // and conditions are constants, then it can be optimized to a bitwise test
   if (FLeft.Typ is TEnumerationSymbol) and ConstantConditions then begin
      enumSym:=TEnumerationSymbol(FLeft.Typ);
      if (enumSym.LowBound<0) or (enumSym.HighBound>31) then Exit;
      mask:=0;
      for k:=enumSym.LowBound to enumSym.HighBound do begin
         value:=Int64(k);
         for i:=0 to FCaseConditions.Count-1 do begin
            cc:=TCaseCondition(FCaseConditions.List[i]);
            if cc.IsTrue(exec, Value) then begin
               mask:=mask or (1 shl k);
               Break;
            end;
         end;
      end;
      Result:=TBitwiseInOpExpr.Create(prog, FLeft);
      TBitwiseInOpExpr(Result).Mask:=mask;
      FLeft:=nil;
      Free;
   end;
end;

// GetSubExpr
// AddCaseCondition
//
procedure TInOpExpr.AddCaseCondition(cond : TCaseCondition);
begin
   FCaseConditions.Add(cond);
end;

//
function TInOpExpr.GetSubExpr(i : Integer) : TExprBase;
var
   j : Integer;
   cond : TCaseCondition;
begin
   if i=0 then
      Result:=FLeft
   else begin
      Dec(i);
      for j:=0 to Count-1 do begin
         cond:=CaseConditions[j];
         if i<cond.GetSubExprCount then
            Exit(cond.GetSubExpr(i));
         Dec(i, cond.GetSubExprCount);
      end;
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TInOpExpr.GetSubExprCount : Integer;
var
   i : Integer;
begin
   Result:=1;
   for i:=0 to Count-1 do
      Inc(Result, CaseConditions[i].GetSubExprCount);
end;

// GetCaseConditions
//
function TInOpExpr.GetCaseConditions(idx : Integer) : TCaseCondition;
begin
   Result:=TCaseCondition(FCaseConditions.List[idx]);
end;

// ------------------
// ------------------ TBitwiseInOpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TBitwiseInOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   i : Int64;
begin
   i:=Expr.EvalAsInteger(exec);
   Result:=    (UInt64(i)<UInt64(32))
           and (((1 shl i) and Mask)<>0);
end;

// ------------------
// ------------------ TConvExpr ------------------
// ------------------

// WrapWithConvCast
//
class function TConvExpr.WrapWithConvCast(prog : TdwsProgram; const scriptPos : TScriptPos;
                                          toTyp : TTypeSymbol; expr : TTypedExpr;
                                          reportError : Boolean) : TTypedExpr;

   procedure ReportIncompatibleTypes;
   var
      cleft, cright: String;
   begin
      if not reportError then Exit;
      if toTyp = nil then
         cleft := SYS_VOID
      else cleft := toTyp.Caption;
      if expr.Typ = nil then
         cright := SYS_VOID
      else cright := expr.Typ.Caption;
      prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_AssignIncompatibleTypes, [cright, cleft]);
   end;

var
   arrayConst : TArrayConstantExpr;
begin
   Result:=expr;
   if (toTyp=nil) or (expr.Typ=nil) then begin
      ReportIncompatibleTypes;
      Exit;
   end;

   if expr.Typ=toTyp then Exit;

   if expr.ClassType=TArrayConstantExpr then begin
      arrayConst:=TArrayConstantExpr(expr);
      if toTyp is TDynamicArraySymbol then begin
         if    (toTyp.Typ.IsOfType(expr.Typ.Typ))
            or ((arrayConst.ElementCount=0) and (arrayConst.Typ.Typ.IsOfType(prog.TypVariant)))  then
            Result:=TConvStaticArrayToDynamicExpr.Create(prog, arrayConst,
                                                         TDynamicArraySymbol(toTyp))
      end;
   end else if expr.Typ.UnAliasedType is TBaseVariantSymbol then begin
      if toTyp.IsOfType(prog.TypInteger) then
         Result:=TConvIntegerExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypFloat) then
         Result:=TConvFloatExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypString) then
         Result:=TConvStringExpr.Create(prog, expr)
      else if toTyp.IsOfType(prog.TypBoolean) then
         Result:=TConvBoolExpr.Create(prog, expr);
   end else begin
      if     toTyp.IsOfType(prog.TypFloat)
         and expr.IsOfType(prog.TypInteger) then begin
         if expr is TConstIntExpr then begin
            Result:=TConstFloatExpr.CreateTypedVariantValue(prog, prog.TypFloat, TConstIntExpr(expr).Value);
            expr.Free;
         end else Result:=TConvFloatExpr.Create(prog, expr);
      end;
   end;
   // Look if Types are compatible
   if not toTyp.IsCompatible(Result.Typ) then
      ReportIncompatibleTypes;
end;

// Eval
//
function TConvExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Assert(False);
end;

// ------------------
// ------------------ TConvFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TConvFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FExpr.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TConvIntegerExpr ------------------
// ------------------

// EvalAsInteger
//
function TConvIntegerExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TConvStringExpr ------------------
// ------------------

// EvalAsString
//
procedure TConvStringExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   FExpr.EvalAsString(exec, Result);
end;

// ------------------
// ------------------ TConvBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConvBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FExpr.EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TConvVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FExpr.EvalAsVariant(exec, Result);
end;

// ------------------
// ------------------ TConvStaticArrayToDynamicExpr ------------------
// ------------------

// Create
//
constructor TConvStaticArrayToDynamicExpr.Create(prog : TdwsProgram; expr : TArrayConstantExpr;
                                                 toTyp : TDynamicArraySymbol);
begin
   inherited Create(prog, expr);
   Typ:=toTyp;
end;

// Eval
//
function TConvStaticArrayToDynamicExpr.Eval(exec : TdwsExecution) : Variant;
var
   arr : TArrayConstantExpr;
   dynArray : TScriptDynamicArray;
begin
   arr:=TArrayConstantExpr(Expr);

   dynArray:=TScriptDynamicArray.Create(TDynamicArraySymbol(Typ).Typ);
   dynArray.Data:=arr.EvalAsTData(exec);

   Result:=IUnknown(IScriptObj(dynArray));
end;

// ------------------
// ------------------ TConvExternalExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TConvExternalExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Expr.EvalAsVariant(exec, Result);
end;

// ------------------
// ------------------ TAssertExpr ------------------
// ------------------

// Create
//
constructor TAssertExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; condExpr, msgExpr : TTypedExpr);
begin
   inherited Create(Prog, Pos);
   FCond:=condExpr;
   FMessage:=msgExpr;
end;

// Destroy
//
destructor TAssertExpr.Destroy;
begin
   FCond.Free;
   FMessage.Free;
   inherited;
end;

// EvalNoResult
//
procedure TAssertExpr.EvalNoResult(exec : TdwsExecution);

   procedure Triggered;
   var
      msg : String;
   begin
      if FMessage<>nil then begin
         FMessage.EvalAsString(exec, msg);
         msg:=' : '+msg;
      end else msg:='';
      (exec as TdwsProgramExecution).RaiseAssertionFailed(msg, FScriptPos);
   end;

begin
   if not FCond.EvalAsBoolean(exec) then
      Triggered;
end;

// Optimize
//
function TAssertExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCond.IsConstant and FCond.EvalAsBoolean(exec) then begin
      Result:=TNullExpr.Create(Prog, FScriptPos);
      Free;
   end;
end;

// GetSubExpr
//
function TAssertExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FCond
   else Result:=FMessage;
end;

// GetSubExprCount
//
function TAssertExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TAssignedInstanceExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedInstanceExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   obj : IScriptObj;
begin
   FExpr.EvalAsScriptObj(exec, obj);
   Result:=(obj<>nil);
end;

// ------------------
// ------------------ TAssignedMetaClassExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedMetaClassExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(FExpr.EvalAsInteger(exec)<>0);
end;

// ------------------
// ------------------ TAssignedFuncPtrExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedFuncPtrExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   v : Variant;
begin
   FExpr.EvalAsVariant(exec, v);
   Result:=(IUnknown(v)<>nil);
end;

// ------------------
// ------------------ TOrdExpr ------------------
// ------------------

// EvalAsInteger
//
function TOrdExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   v : Variant;
   s : UnicodeString;
begin
   Result:=0;
   FExpr.EvalAsVariant(exec, v);
   case VarType(v) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64, varUInt64 :
         Result:=v;
      varBoolean :
         if v then
            Result:=1
         else Result:=0;
      varSingle, varDouble, varCurrency :
         Result:=Round(v);
      varString, varUString, varOleStr : begin
         {$ifdef FPC}
         s:=UTF8Decode(String(v));
         {$else}
         s:=v;
         {$endif}
         if s<>'' then
            Result:=Ord(s[1]);
      end;
   else
      RaiseScriptError(exec, EScriptError.Create(RTE_OrdinalExpected));
   end;
end;

{ TOrdIntExpr }

// EvalAsInteger
//
function TOrdIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FExpr.EvalAsInteger(exec);
end;

{ TOrdBoolExpr }

// EvalAsInteger
//
function TOrdBoolExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Ord(FExpr.EvalAsBoolean(exec));
end;

{ TOrdStrExpr }

// EvalAsInteger
//
function TOrdStrExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   s : UnicodeString;
begin
   {$ifdef FPC}
   FExpr.EvalAsUnicodeString(exec, s);
   {$else}
   FExpr.EvalAsString(exec, s);
   {$endif}
   if s<>'' then
      Result:=Ord(s[1])
   else Result:=0;
end;

// ------------------
// ------------------ TObjCmpEqualExpr ------------------
// ------------------

// EvalAsBoolean
//
function TObjCmpEqualExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   iLeft, iRight : IScriptObj;
begin
   FLeft.EvalAsScriptObj(exec, iLeft);
   FRight.EvalAsScriptObj(exec, iRight);
   Result:=(iLeft=iRight);
end;

// ------------------
// ------------------ TObjCmpNotEqualExpr ------------------
// ------------------

// EvalAsBoolean
//
function TObjCmpNotEqualExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   iLeft, iRight : IScriptObj;
begin
   FLeft.EvalAsScriptObj(exec, iLeft);
   FRight.EvalAsScriptObj(exec, iRight);
   Result:=(iLeft<>iRight);
end;

// ------------------
// ------------------ TIntfCmpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TIntfCmpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   iLeft, iRight : IScriptObj;
begin
   FLeft.EvalAsScriptObj(exec, iLeft);
   FRight.EvalAsScriptObj(exec, iRight);
   Result:=(iLeft=iRight);
end;

// ------------------
// ------------------ TNegVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TNegVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Expr.EvalAsVariant(exec, Result);
   Result:=-Result;
end;

// ------------------
// ------------------ TNegIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TNegIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=-FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TNegFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TNegFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=-FExpr.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TAddVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TAddVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=lv+rv;
end;

// ------------------
// ------------------ TAddStrExpr ------------------
// ------------------

// EvalAsString
//
procedure TAddStrExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   buf : String;
begin
   FLeft.EvalAsString(exec, Result);
   FRight.EvalAsString(exec, buf);
   Result:=Result+buf;
end;

// ------------------
// ------------------ TAddIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TAddIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec)+FRight.EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TAddIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsInteger(exec)+FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TAddFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TAddFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)+FRight.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TSubVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TSubVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=lv-rv;
end;

// ------------------
// ------------------ TSubIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TSubIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec)-FRight.EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TSubIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsInteger(exec)-FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TSubFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TSubFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)-FRight.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TMultVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TMultVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   Result:=lv*rv;
end;

// ------------------
// ------------------ TMultIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TMultIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec)*FRight.EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TMultIntExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsInteger(exec)*FRight.EvalAsInteger(exec);
end;

// Optimize
//
function TMultIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if (FLeft is TVarExpr) and (FRight is TVarExpr) then begin
      if TVarExpr(FLeft).SameVarAs(TVarExpr(FRight)) then begin
         Result:=TSqrIntExpr.Create(Prog, FLeft);
         FLeft:=nil;
         Free;
         Exit;
      end;
   end;
   Result:=inherited;
end;

// ------------------
// ------------------ TMultFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TMultFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)*FRight.EvalAsFloat(exec);
end;

// Optimize
//
function TMultFloatExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if (FLeft is TFloatVarExpr) and (FRight is TFloatVarExpr) then begin
      if TFloatVarExpr(FLeft).SameVarAs(TFloatVarExpr(FRight)) then begin
         Result:=TSqrFloatExpr.Create(Prog, FLeft);
         FLeft:=nil;
         Free;
         Exit;
      end;
   end;
   Result:=inherited;
end;

// ------------------
// ------------------ TSqrIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TSqrIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FExpr.EvalAsInteger(exec);
   Result:=Result*Result;
end;

// ------------------
// ------------------ TSqrFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TSqrFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
{$if Defined(WIN32_ASM)}
asm
   mov   eax, [eax].FExpr
   mov   ecx, [eax]
   call  [ecx+VMTOFFSET EvalAsFloat]
   fmul  st(0), st(0)
{$else}
begin
   Result:=Sqr(FExpr.EvalAsFloat(exec));
{$ifend}
end;

// ------------------
// ------------------ TDivideExpr ------------------
// ------------------

// EvalAsFloat
//
function TDivideExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=FLeft.EvalAsFloat(exec)/FRight.EvalAsFloat(exec);
end;

// ------------------
// ------------------ TDivExpr ------------------
// ------------------

// TDivExpr
//
function TDivExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec) div FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TModExpr ------------------
// ------------------

// EvalAsInteger
//
function TModExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=FLeft.EvalAsInteger(exec) mod FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TNotBoolExpr ------------------
// ------------------

// EvalAsBoolean
//
function TNotBoolExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=not FExpr.EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TNotIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TNotIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=not FExpr.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TNotVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TNotVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   FExpr.EvalAsVariant(exec, Result);
   Result:=not Result;
end;

{ TIntAndExpr }

function TIntAndExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) and FRight.EvalAsInteger(exec);
end;

{ TBoolAndExpr }

function TBoolAndExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBoolean(exec) and Right.EvalAsBoolean(exec);
end;

{ TVariantAndExpr }

function TVariantAndExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=Left.Eval(exec) and Right.Eval(exec);
end;

{ TIntOrExpr }

function TIntOrExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) or FRight.EvalAsInteger(exec);
end;

{ TBoolOrExpr }

function TBoolOrExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := Left.EvalAsBoolean(exec) or Right.EvalAsBoolean(exec);
end;

{ TVariantOrExpr }

function TVariantOrExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result := Left.Eval(exec) or Right.Eval(exec);
end;

{ TIntXorExpr }

function TIntXorExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) xor FRight.EvalAsInteger(exec);
end;

{ TBoolXorExpr }

function TBoolXorExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result := FLeft.EvalAsBoolean(exec) xor FRight.EvalAsBoolean(exec);
end;

{ TVariantXorExpr }

function TVariantXorExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result := FLeft.Eval(exec) xor FRight.Eval(exec);
end;

// ------------------
// ------------------ TBoolImpliesExpr ------------------
// ------------------

// EvalAsBoolean
//
function TBoolImpliesExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=(not FLeft.EvalAsBoolean(exec)) or FRight.EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TShlExpr ------------------
// ------------------

// EvalAsInteger
//
function TShlExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) shl FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TShrExpr ------------------
// ------------------

// EvalAsInteger
//
function TShrExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result := FLeft.EvalAsInteger(exec) shr FRight.EvalAsInteger(exec);
end;

// ------------------
// ------------------ TSarExpr ------------------
// ------------------

// EvalAsInteger
//
function TSarExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   left, right : Int64;
begin
   left:=FLeft.EvalAsInteger(exec);
   right:=FRight.EvalAsInteger(exec);
   if right=0 then
      Result:=left
   else if right>63 then
      if left<0 then
         Result:=-1
      else Result:=0
   else if left>=0 then
      Result:=left shr right
   else Result:=(left shr right) or (Int64(-1) shl (64-right));
end;

// ------------------
// ------------------ TAssignExpr ------------------
// ------------------

constructor TAssignExpr.Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr);
begin
  inherited Create(Prog, Pos);
  FLeft := Left;
  FRight := Right;
  TypeCheckAssign(Prog);
end;

// Destroy
//
destructor TAssignExpr.Destroy;
begin
   FLeft.Free;
   FRight.Free;
   inherited;
end;

// EvalNoResult
//
procedure TAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignExpr(exec, FRight);
end;

// TypeCheckAssign
//
procedure TAssignExpr.TypeCheckAssign(prog : TdwsProgram);
begin
   if FRight.ClassType=TArrayConstantExpr then
      TArrayConstantExpr(FRight).Prepare(Prog, FLeft.Typ.Typ);

   FRight:=TConvExpr.WrapWithConvCast(prog, ScriptPos, FLeft.Typ, FRight, True);
end;

// Optimize
//
function TAssignExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   leftVarExpr : TVarExpr;
   addIntExpr : TAddIntExpr;
   addStrExpr : TAddStrExpr;
   subIntExpr : TSubIntExpr;
begin
   if FRight.IsConstant then
      Exit(OptimizeConstAssignment(prog, exec));

   Result:=Self;
   if FLeft.InheritsFrom(TVarExpr) then begin
      leftVarExpr:=TVarExpr(FLeft);
      if leftVarExpr.ClassType=TIntVarExpr then begin
         if FRight.ClassType=TAddIntExpr then begin
            addIntExpr:=TAddIntExpr(FRight);
            if (addIntExpr.Left is TVarExpr) and (TVarExpr(addIntExpr.Left).SameVarAs(leftVarExpr)) then begin
               Result:=TIncIntVarExpr.Create(Prog, FScriptPos, FLeft, addIntExpr.Right);
               FLeft:=nil;
               addIntExpr.Right:=nil;
               Free;
            end;
         end else if FRight.ClassType=TSubIntExpr then begin
            subIntExpr:=TSubIntExpr(FRight);
            if (subIntExpr.Left is TVarExpr) and (TVarExpr(subIntExpr.Left).SameVarAs(leftVarExpr)) then begin
               Result:=TDecIntVarExpr.Create(Prog, FScriptPos, FLeft, subIntExpr.Right);
               FLeft:=nil;
               subIntExpr.Right:=nil;
               Free;
            end;
         end;
      end else if leftVarExpr.ClassType=TStrVarExpr then begin
         if FRight.InheritsFrom(TAddStrExpr) then begin
            addStrExpr:=TAddStrExpr(FRight);
            if (addStrExpr.Left is TVarExpr) and (TVarExpr(addStrExpr.Left).SameVarAs(leftVarExpr)) then begin
               if addStrExpr.Right.InheritsFrom(TConstStringExpr) then begin
                  Result:=TAppendConstStringVarExpr.Create(Prog, FScriptPos, FLeft, addStrExpr.Right);
               end else begin
                  Result:=TAppendStringVarExpr.Create(Prog, FScriptPos, FLeft, addStrExpr.Right);
               end;
               FLeft:=nil;
               addStrExpr.Right:=nil;
               Free;
            end;
         end;
      end;
   end;
end;

// OptimizeConstAssignment
//
function TAssignExpr.OptimizeConstAssignment(prog : TdwsProgram; exec : TdwsExecution) : TNoResultExpr;
var
   stringBuf : String;
begin
   Result:=Self;
   if FRight.IsOfType(prog.TypInteger) then begin

      Result:=TAssignConstToIntegerVarExpr.CreateVal(prog, FScriptPos, FLeft, FRight.EvalAsInteger(exec));

   end else if FRight.IsOfType(prog.TypFloat) then begin

      Result:=TAssignConstToFloatVarExpr.CreateVal(prog, FScriptPos, FLeft, FRight.EvalAsFloat(exec));

   end else if FRight.IsOfType(prog.TypBoolean) then begin

      Result:=TAssignConstToBoolVarExpr.CreateVal(prog, FScriptPos, FLeft, FRight.EvalAsBoolean(exec));

   end else if FRight.IsOfType(prog.TypString) then begin

      FRight.EvalAsString(exec, stringBuf);
      Result:=TAssignConstToStringVarExpr.CreateVal(prog, FScriptPos, FLeft, stringBuf);

   end else if FRight.IsOfType(prog.TypNil) then begin

      if FLeft.Typ.UnAliasedType.ClassType=TClassSymbol then
         Result:=TAssignNilToVarExpr.CreateVal(prog, FScriptPos, FLeft)
      else if FLeft.Typ.UnAliasedType.ClassType=TClassOfSymbol then
         Result:=TAssignNilClassToVarExpr.CreateVal(prog, FScriptPos, FLeft);

   end;
   if Result<>Self then begin
      FLeft:=nil;
      Free;
   end;
end;

// GetSubExpr
//
function TAssignExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FLeft
   else Result:=FRight;
end;

// GetSubExprCount
//
function TAssignExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TAssignClassOfExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssignClassOfExpr.EvalNoResult(exec : TdwsExecution);
var
   v : Variant;
   obj : IScriptObj;
begin
   FRight.EvalAsVariant(exec, v);
   if VarIsOrdinal(v) then
      FLeft.AssignValue(exec, v)
   else begin
      obj:=IScriptObj(IUnknown(v));
      if obj<>nil then
         FLeft.AssignValueAsInteger(exec, Int64(IScriptObj(IUnknown(v)).ClassSym))
      else FLeft.AssignValueAsInteger(exec, 0);
   end;
end;

{ TAssignDataExpr }

constructor TAssignDataExpr.Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr);
begin
  inherited Create(Prog, Pos, Left, Right);
  FSize := FLeft.Typ.Size;
end;

procedure TAssignDataExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignDataExpr(exec, TDataExpr(FRight));
end;

// ------------------
// ------------------ TAssignFuncExpr ------------------
// ------------------

// TypeCheckAssign
//
procedure TAssignFuncExpr.TypeCheckAssign(prog : TdwsProgram);
begin
   if not FLeft.Typ.IsCompatible((FRight as TFuncExprBase).FuncSym) then
      prog.CompileMsgs.AddCompilerError(ScriptPos, CPE_IncompatibleOperands);
end;

// EvalNoResult
//
procedure TAssignFuncExpr.EvalNoResult(exec : TdwsExecution);
var
   funcPtr : TFuncPointer;
   funcExpr : TFuncExprBase;
begin
   funcExpr:=(FRight as TFuncExprBase);
   funcPtr:=TFuncPointer.Create(exec, funcExpr);
   FLeft.AssignValue(exec, IFuncPointer(funcPtr));
end;

// Optimize
//
function TAssignFuncExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TAssignArrayConstantExpr ------------------
// ------------------

constructor TAssignArrayConstantExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                                            Left : TDataExpr; Right: TTypedExpr);
begin
  inherited Create(Prog, Pos, Left, Right as TArrayConstantExpr); // typecheck Right
end;

procedure TAssignArrayConstantExpr.EvalNoResult(exec : TdwsExecution);
var
   obj : IScriptObj;
   dyn : TScriptDynamicArray;
   srcData : TData;
begin
   srcData:=TArrayConstantExpr(FRight).EvalAsTData(exec);
   if FLeft.Typ is TDynamicArraySymbol then begin
      // to dynamic array
      FLeft.EvalAsScriptObj(exec, obj);
      if obj=nil then begin
         // first init
         dyn:=TScriptDynamicArray.Create(TDynamicArraySymbol(FLeft.Typ).Typ);
         FLeft.AssignValueAsScriptObj(exec, dyn);
      end else begin
         dyn:=TScriptDynamicArray(obj.InternalObject);
      end;
      dyn.RawCopy(srcData, 0, Length(srcData));
   end else begin
      // to static array
      FLeft.AssignData(exec, srcData, 0);
   end;
end;

// TypeCheckAssign
//
procedure TAssignArrayConstantExpr.TypeCheckAssign(prog : TdwsProgram);
var
   leftItemTyp : TTypeSymbol;
begin
   if FLeft.Typ is TDynamicArraySymbol then begin
      leftItemTyp:=TDynamicArraySymbol(FLeft.Typ).Typ;
      if not (   leftItemTyp.IsOfType(TArraySymbol(FRight.Typ).Typ)
              or leftItemTyp.IsOfType(prog.TypVariant)) then
         prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_AssignIncompatibleTypes,
                                              [Right.Typ.Caption, Left.Typ.Caption]);
   end else inherited;
end;

// ------------------
// ------------------ TAssignConstDataToVarExpr ------------------
// ------------------

constructor TAssignConstDataToVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                                             Left : TDataExpr; Right: TTypedExpr);
begin
   inherited Create(Prog, Pos, Left, Right);
   Assert(Left is TVarExpr);
   if Right=nil then
      Assert(ClassType<>TAssignConstDataToVarExpr)
   else Assert(Right is TConstExpr);
end;

procedure TAssignConstDataToVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignData(exec, TConstExpr(FRight).FData, TConstExpr(FRight).Addr[exec]);
end;

// ------------------
// ------------------ TAssignConstExpr ------------------
// ------------------

// TypeCheckAssign
//
procedure TAssignConstExpr.TypeCheckAssign(prog : TdwsProgram);
begin
   // nothing, checked during optimize
end;

// ------------------
// ------------------ TAssignConstToIntegerVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToIntegerVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToIntegerVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsInteger(exec, FRight);
end;

// ------------------
// ------------------ TAssignConstToFloatVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToFloatVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToFloatVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsFloat(exec, FRight);
end;

// ------------------
// ------------------ TAssignConstToBoolVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToBoolVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Boolean);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToBoolVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsBoolean(exec, FRight);
end;

// ------------------
// ------------------ TAssignConstToStringVarExpr ------------------
// ------------------

// Create
//
constructor TAssignConstToStringVarExpr.CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
begin
   inherited Create(Prog, Pos, Left, nil);
   FRight:=rightValue;
end;

// EvalNoResult
//
procedure TAssignConstToStringVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsString(exec, FRight);
end;

// ------------------
// ------------------ TAssignNilToVarExpr ------------------
// ------------------

// CreateVal
//
constructor TAssignNilToVarExpr.CreateVal(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr);
begin
   inherited Create(prog, pos, left, nil);
end;

// EvalNoResult
//
procedure TAssignNilToVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsScriptObj(exec, nil);
end;

// ------------------
// ------------------ TAssignNilClassToVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssignNilClassToVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TVarExpr(FLeft).AssignValueAsInteger(exec, 0);
end;

// ------------------
// ------------------ TOpAssignExpr ------------------
// ------------------

// Optimize
//
function TOpAssignExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TPlusAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   FLeft.AssignValue(exec, lv+rv);
end;

// ------------------
// ------------------ TPlusAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignIntExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsInteger(exec, FLeft.EvalAsInteger(exec) + FRight.EvalAsInteger(exec));
end;

// Optimize
//
function TPlusAssignIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      Result:=TIncIntVarExpr.Create(Prog, FScriptPos, FLeft, FRight);
      FLeft:=nil;
      FRight:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TPlusAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignFloatExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)+FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TPlusAssignStrExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignStrExpr.EvalNoResult(exec : TdwsExecution);
var
   v1, v2 : String;
begin
   FLeft.EvalAsString(exec, v1);
   FRight.EvalAsString(exec, v2);
   FLeft.AssignValueAsString(exec, v1+v2);
end;

// Optimize
//
function TPlusAssignStrExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FLeft is TStrVarExpr then begin
      Result:=TAppendStringVarExpr.Create(Prog, FScriptPos, FLeft, FRight);
      FLeft:=nil;
      FRight:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TMinusAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   FLeft.AssignValue(exec, lv-rv);
end;

// ------------------
// ------------------ TMinusAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignIntExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsInteger(exec, FLeft.EvalAsInteger(exec) - FRight.EvalAsInteger(exec));
end;

// Optimize
//
function TMinusAssignIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      Result:=TDecIntVarExpr.Create(Prog, FScriptPos, FLeft, FRight);
      FLeft:=nil;
      FRight:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TMinusAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignFloatExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)-FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TMultAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignExpr.EvalNoResult(exec : TdwsExecution);
var
   lv, rv : Variant;
begin
   FLeft.EvalAsVariant(exec, lv);
   FRight.EvalAsVariant(exec, rv);
   FLeft.AssignValue(exec, lv*rv);
end;

// ------------------
// ------------------ TMultAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignIntExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsInteger(exec, FLeft.EvalAsInteger(exec) * FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TMultAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignFloatExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)*FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TDivideAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDivideAssignExpr.EvalNoResult(exec : TdwsExecution);
begin
   FLeft.AssignValueAsFloat(exec, FLeft.EvalAsFloat(exec)/FRight.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TIncIntVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TIncIntVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TIntVarExpr(FLeft).IncValue(exec, FRight.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TDecIntVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDecIntVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TIntVarExpr(FLeft).IncValue(exec, -FRight.EvalAsInteger(exec));
end;


// ------------------
// ------------------ TAbsIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TAbsIntExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   Result:=Abs(Expr.EvalAsInteger(exec));
end;

// ------------------
// ------------------ TAbsFloatExpr ------------------
// ------------------

// EvalAsFloat
//
function TAbsFloatExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=Abs(Expr.EvalAsFloat(exec));
end;

// ------------------
// ------------------ TAbsVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TAbsVariantExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Expr.EvalAsVariant(exec, Result);
   Result:=Abs(Result);
end;

// ------------------
// ------------------ TAppendStringVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAppendStringVarExpr.EvalNoResult(exec : TdwsExecution);
var
   buf : String;
begin
   FRight.EvalAsString(exec, buf);
   TStrVarExpr(FLeft).Append(exec, buf);
end;

// ------------------
// ------------------ TAppendConstStringVarExpr ------------------
// ------------------

// Create
//
constructor TAppendConstStringVarExpr.Create(prog : TdwsProgram; const pos : TScriptPos; left : TDataExpr; right : TTypedExpr);
begin
   inherited Create(Prog, Pos, Left, Right);
   FAppendString:=(right as TConstStringExpr).FValue;
end;

// EvalNoResult
//
procedure TAppendConstStringVarExpr.EvalNoResult(exec : TdwsExecution);
begin
   TStrVarExpr(FLeft).Append(exec, FAppendString);
end;

// ------------------
// ------------------ TBlockExpr ------------------
// ------------------

// Create
//
constructor TBlockExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   inherited Create(Prog, Pos);
   FTable:=TSymbolTable.Create(Prog.Table, Prog.Table.AddrGenerator);
end;

// Destroy
//
destructor TBlockExpr.Destroy;
begin
   FTable.Free;
   inherited;
end;

// EvalNoResult
//
procedure TBlockExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   oldTable : TSymbolTable;
   expr : PNoResultExpr;
begin
   oldTable:=exec.ContextTable;
   exec.ContextTable:=FTable;
   expr:=@FStatements[0];
   try
      for i:=1 to FCount do begin
         exec.DoStep(expr^);
         expr.EvalNoResult(exec);
         if exec.Status<>esrNone then Break;
         Inc(expr);
      end;
      exec.ContextTable:=oldTable;
   except
      exec.ContextTable:=oldTable;
      exec.SetScriptError(expr^);
      raise;
   end;
end;

// Optimize
//
function TBlockExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   i : Integer;
begin
   for i:=FCount-1 downto 0 do begin
      if FStatements[i].ClassType=TNullExpr then begin
         FStatements[i].Free;
         if i+1<FCount then
            Move(FStatements[i+1], FStatements[i], SizeOf(TNoResultExpr)*(FCount-1-i));
         Dec(FCount);
         ReallocMem(FStatements, FCount*SizeOf(TNoResultExpr));
      end;
   end;

   if FTable.Count=0 then begin
      case FCount of
         0 : Result:=TNullExpr.Create(Prog, FScriptPos);
         1 : begin
            Result:=FStatements[0];
            FreeMem(FStatements);
         end;
      else
         case FCount of
            2 : Result:=TBlockExprNoTable2.Create(Prog, FScriptPos);
            3 : Result:=TBlockExprNoTable3.Create(Prog, FScriptPos);
            4 : Result:=TBlockExprNoTable4.Create(Prog, FScriptPos);
         else
            Result:=TBlockExprNoTable.Create(Prog, FScriptPos);
         end;
         TBlockExprNoTable(Result).FStatements:=FStatements;
         TBlockExprNoTable(Result).FCount:=FCount;
      end;
      FStatements:=nil;
      FCount:=0;
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TBlockExprNoTable ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   iterator : PNoResultExpr;
begin
   iterator:=PNoResultExpr(FStatements);
   for i:=1 to FCount do begin
      exec.DoStep(iterator^);
      iterator^.EvalNoResult(exec);
      if exec.Status<>esrNone then Break;
      Inc(iterator);
   end;
end;

// ------------------
// ------------------ TBlockExprNoTable2 ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable2.EvalNoResult(exec : TdwsExecution);
var
   statements : PNoResultExprList;
begin
   statements:=FStatements;
   exec.DoStep(statements[0]);
   statements[0].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[1]);
   statements[1].EvalNoResult(exec);
end;

// ------------------
// ------------------ TBlockExprNoTable3 ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable3.EvalNoResult(exec : TdwsExecution);
var
   statements : PNoResultExprList;
begin
   statements:=FStatements;
   exec.DoStep(statements[0]);
   statements[0].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[1]);
   statements[1].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[2]);
   statements[2].EvalNoResult(exec);
end;

// ------------------
// ------------------ TBlockExprNoTable4 ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoTable4.EvalNoResult(exec : TdwsExecution);
var
   statements : PNoResultExprList;
begin
   statements:=FStatements;
   exec.DoStep(statements[0]);
   statements[0].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[1]);
   statements[1].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[2]);
   statements[2].EvalNoResult(exec);
   if exec.Status<>esrNone then Exit;
   exec.DoStep(statements[3]);
   statements[3].EvalNoResult(exec);
end;

// ------------------
// ------------------ TIfThenExpr ------------------
// ------------------

// Create
//
constructor TIfThenExpr.Create(prog : TdwsProgram; const Pos : TScriptPos;
                               condExpr : TTypedExpr; thenExpr : TNoResultExpr);
begin
   inherited Create(prog, pos);
   FCond:=condExpr;
   FThen:=thenExpr;
end;

// Destroy
//
destructor TIfThenExpr.Destroy;
begin
   FCond.Free;
   FThen.Free;
   inherited;
end;

// EvalNoResult
//
procedure TIfThenExpr.EvalNoResult(exec : TdwsExecution);
begin
   if FCond.EvalAsBoolean(exec) then begin
      exec.DoStep(FThen);
      FThen.EvalNoResult(exec);
   end;
end;

// Optimize
//
function TIfThenExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCond.IsConstant then begin
      if FCond.EvalAsBoolean(exec) then begin
         Result:=FThen;
         FThen:=nil;
      end else Result:=TNullExpr.Create(Prog, FScriptPos);
      Free;
   end;
end;

// GetSubExpr
//
function TIfThenExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FCond
   else Result:=FThen;
end;

// GetSubExprCount
//
function TIfThenExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TIfThenElseExpr ------------------
// ------------------

// Create
//
constructor TIfThenElseExpr.Create(prog : TdwsProgram; const Pos : TScriptPos;
                 condExpr : TTypedExpr; thenExpr, elseExpr : TNoResultExpr);
begin
   inherited Create(prog, pos, condExpr, thenExpr);
   FElse:=elseExpr;
end;

// Destroy
//
destructor TIfThenElseExpr.Destroy;
begin
   FElse.Free;
   inherited;
end;

// EvalNoResult
//
procedure TIfThenElseExpr.EvalNoResult(exec : TdwsExecution);
begin
   if FCond.EvalAsBoolean(exec) then begin
      exec.DoStep(FThen);
      FThen.EvalNoResult(exec);
   end else begin
      exec.DoStep(FElse);
      FElse.EvalNoResult(exec);
   end;
end;

// Optimize
//
function TIfThenElseExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCond.IsConstant then begin
      if FCond.EvalAsBoolean(exec) then begin
         Result:=FThen;
         FThen:=nil;
      end else begin
         Result:=FElse;
         FElse:=nil;
      end;
      Free;
   end;
end;

// GetSubExpr
//
function TIfThenElseExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=2 then
      Result:=FElse
   else Result:=inherited GetSubExpr(i);
end;

// GetSubExprCount
//
function TIfThenElseExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

{ TCaseExpr }

destructor TCaseExpr.Destroy;
begin
  FCaseConditions.Clean;
  FValueExpr.Free;
  FElseExpr.Free;
  inherited;
end;

// EvalNoResult
//
procedure TCaseExpr.EvalNoResult(exec : TdwsExecution);
var
   x : Integer;
   value: Variant;
   cc : TCaseCondition;
begin
   FValueExpr.EvalAsVariant(exec, value);
   for x := 0 to FCaseConditions.Count - 1 do begin
      cc:=TCaseCondition(FCaseConditions.List[x]);
      if cc.IsTrue(exec, value) then begin
         exec.DoStep(cc.TrueExpr);
         cc.TrueExpr.EvalNoResult(exec);
         Exit;
      end;
   end;

   if Assigned(FElseExpr) then begin
      exec.DoStep(FElseExpr);
      FElseExpr.EvalNoResult(exec);
   end;
end;

// AddCaseCondition
//
procedure TCaseExpr.AddCaseCondition(cond : TCaseCondition);
begin
   FCaseConditions.Add(cond);
end;

// GetSubExpr
//
function TCaseExpr.GetSubExpr(i : Integer) : TExprBase;
var
   j : Integer;
   cond : TCaseCondition;
begin
   case i of
      0 : Result:=ValueExpr;
      1 : Result:=ElseExpr;
   else
      Dec(i, 2);
      for j:=0 to FCaseConditions.Count-1 do begin
         cond:=TCaseCondition(FCaseConditions.List[j]);
         if i<cond.GetSubExprCount then
            Exit(cond.GetSubExpr(i));
         Dec(i, cond.GetSubExprCount);
      end;
      Result:=nil;
   end;
end;

// GetSubExprCount
//
function TCaseExpr.GetSubExprCount : Integer;
var
   i : Integer;
begin
   Result:=2;
   for i:=0 to FCaseConditions.Count-1 do
      Inc(Result, TCaseCondition(FCaseConditions.List[i]).GetSubExprCount);
end;

// ------------------
// ------------------ TCaseCondition ------------------
// ------------------

// Create
//
constructor TCaseCondition.Create(const aPos : TScriptPos);
begin
   FPos:=aPos;
end;

// Destroy
//
destructor TCaseCondition.Destroy;
begin
   if FOwnsTrueExpr then
      FTrueExpr.Free;
   inherited;
end;

// IsOfTypeNumber
//
function TCaseCondition.IsOfTypeNumber(prog : TdwsProgram; typ : TTypeSymbol) : Boolean;
begin
   Result:=   typ.IsOfType(prog.TypInteger) or typ.IsOfType(prog.TypFloat)
           or typ.IsOfType(prog.TypVariant)
           or (typ is TEnumerationSymbol);
end;

// ------------------
// ------------------ TCompareCaseCondition ------------------
// ------------------

// Create
//
constructor TCompareCaseCondition.Create(const aPos : TScriptPos; compareExpr : TTypedExpr);
begin
   inherited Create(aPos);
   FCompareExpr:=compareExpr;
end;

// Destroy
//
destructor TCompareCaseCondition.Destroy;
begin
   FCompareExpr.Free;
   inherited;
end;

// GetSubExpr
//
function TCompareCaseCondition.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FTrueExpr
   else Result:=FCompareExpr;
end;

// GetSubExprCount
//
function TCompareCaseCondition.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// IsTrue
//
function TCompareCaseCondition.IsTrue(exec : TdwsExecution; const value : Variant) : Boolean;
var
   buf : Variant;
begin
   FCompareExpr.EvalAsVariant(exec, buf);
   Result:=(buf=Value);
end;

// TypeCheck
//
procedure TCompareCaseCondition.TypeCheck(prog : TdwsProgram; typ : TTypeSymbol);
begin
   if (FCompareExpr.Typ=nil) or not typ.IsCompatible(FCompareExpr.Typ) then
      if not (IsOfTypeNumber(prog, FCompareExpr.Typ) and IsOfTypeNumber(prog, typ)) then
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                              [typ.Caption, FCompareExpr.Typ.Caption]);
end;

// IsConstant
//
function TCompareCaseCondition.IsConstant : Boolean;
begin
   Result:=FCompareExpr.IsConstant;
end;

// ------------------
// ------------------ TRangeCaseCondition ------------------
// ------------------

// Create
//
constructor TRangeCaseCondition.Create(const aPos : TScriptPos; fromExpr, toExpr : TTypedExpr);
begin
   inherited Create(aPos);
   FFromExpr:=FromExpr;
   FToExpr:=ToExpr;
end;

// Destroy
//
destructor TRangeCaseCondition.Destroy;
begin
   FFromExpr.Free;
   FToExpr.Free;
   inherited;
end;

// GetSubExpr
//
function TRangeCaseCondition.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FTrueExpr;
      1 : Result:=FFromExpr;
   else
      Result:=FToExpr;
   end;
end;

// GetSubExprCount
//
function TRangeCaseCondition.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// IsTrue
//
function TRangeCaseCondition.IsTrue(exec : TdwsExecution; const value : Variant) : Boolean;
var
   v : Variant;
begin
   FFromExpr.EvalAsVariant(exec, v);
   if value>=v then begin
      FToExpr.EvalAsVariant(exec, v);
      Result:=(value<=v);
   end else Result:=False;
end;

// TypeCheck
//
procedure TRangeCaseCondition.TypeCheck(prog : TdwsProgram; typ : TTypeSymbol);
var
   fromIsNumber : Boolean;
begin
   fromIsNumber:=IsOfTypeNumber(prog, FFromExpr.Typ);

   if (FFromExpr.Typ=nil) or not FFromExpr.Typ.IsCompatible(FToExpr.Typ) then begin
      if not (fromIsNumber and IsOfTypeNumber(prog, FToExpr.Typ)) then begin
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_RangeIncompatibleTypes,
                                              [FFromExpr.Typ.Caption, FToExpr.Typ.Caption]);
         Exit;
      end;
   end;

   if not typ.IsCompatible(FFromExpr.Typ) then
      if not (fromIsNumber and IsOfTypeNumber(prog, typ)) then
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                              [typ.Caption, FFromExpr.Typ.Caption]);
end;

// IsConstant
//
function TRangeCaseCondition.IsConstant : Boolean;
begin
   Result:=FFromExpr.IsConstant and FToExpr.IsConstant;
end;

// ------------------
// ------------------ TForExpr ------------------
// ------------------

// Destroy
//
destructor TForExpr.Destroy;
begin
   FDoExpr.Free;
   FFromExpr.Free;
   FToExpr.Free;
   FVarExpr.Free;
   inherited;
end;

// GetSubExpr
//
function TForExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FVarExpr;
      1 : Result:=FFromExpr;
      2 : Result:=FToExpr;
   else
      Result:=FDoExpr;
   end;
end;

// GetSubExprCount
//
function TForExpr.GetSubExprCount : Integer;
begin
   Result:=4;
end;

// ------------------
// ------------------ TForStepExpr ------------------
// ------------------

// Destroy
//
destructor TForStepExpr.Destroy;
begin
   FStepExpr.Free;
   inherited;
end;

// EvalStep
//
function TForStepExpr.EvalStep(exec : TdwsExecution) : Int64;
begin
   Result:=FStepExpr.EvalAsInteger(exec);
   if Result<=0 then
      RaiseScriptError(exec, EScriptError.CreateFmt(RTE_ForLoopStepShouldBeStrictlyPositive, [Result]));
end;

// GetSubExpr
//
function TForStepExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=4 then
      Result:=FStepExpr
   else Result:=inherited GetSubExpr(i);
end;

// GetSubExprCount
//
function TForStepExpr.GetSubExprCount : Integer;
begin
   Result:=5;
end;

// ------------------
// ------------------ TForUpwardExpr ------------------
// ------------------

procedure TForUpwardExpr.EvalNoResult(exec : TdwsExecution);
var
   toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   while i^<=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      Inc(i^);
   end;
end;

// ------------------
// ------------------ TForDownwardExpr ------------------
// ------------------

procedure TForDownwardExpr.EvalNoResult(exec : TdwsExecution);
var
   toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   while i^>=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      Dec(i^);
   end;
end;

// ------------------
// ------------------ TForUpwardStepExpr ------------------
// ------------------

procedure TForUpwardStepExpr.EvalNoResult(exec : TdwsExecution);
var
   step, toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   step:=EvalStep(exec);
   while i^<=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      try
         {$OVERFLOWCHECKS ON}
         i^:=i^+step;
         {$OVERFLOWCHECKS OFF}
      except
         Break;
      end;
   end;
end;

// ------------------
// ------------------ TForDownwardStepExpr ------------------
// ------------------

procedure TForDownwardStepExpr.EvalNoResult(exec : TdwsExecution);
var
   step, toValue: Int64;
   i : PInt64;
begin
   i:=FVarExpr.EvalAsPInteger(exec);
   i^:=FFromExpr.EvalAsInteger(exec);
   toValue:=FToExpr.EvalAsInteger(exec);
   step:=EvalStep(exec);
   while i^>=toValue do begin
      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      try
         {$OVERFLOWCHECKS ON}
         i^:=i^-step;
         {$OVERFLOWCHECKS OFF}
      except
         Break;
      end;
   end;
end;

// ------------------
// ------------------ TLoopExpr ------------------
// ------------------

// Destroy
//
destructor TLoopExpr.Destroy;
begin
   FCondExpr.Free;
   FLoopExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TLoopExpr.EvalNoResult(exec : TdwsExecution);
begin
   repeat
      exec.DoStep(FLoopExpr);
      FLoopExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               Break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
   until False;
end;

// GetSubExpr
//
function TLoopExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FCondExpr
   else Result:=FLoopExpr;
end;

// GetSubExprCount
//
function TLoopExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

{ TWhileExpr }

procedure TWhileExpr.EvalNoResult(exec : TdwsExecution);
begin
   while FCondExpr.EvalAsBoolean(exec) do begin
      exec.DoStep(FLoopExpr);
      FLoopExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               Break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
   end;
end;

// Optimize
//
function TWhileExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCondExpr.IsConstant then begin
      if not FCondExpr.EvalAsBoolean(exec) then begin
         Result:=TNullExpr.Create(prog, FScriptPos);
      end else begin
         Result:=TLoopExpr.Create(prog, FScriptPos);
         TLoopExpr(Result).FLoopExpr:=FLoopExpr;
         FLoopExpr:=nil;
      end;
      Free;
   end;
end;

{ TRepeatExpr }

procedure TRepeatExpr.EvalNoResult(exec : TdwsExecution);
begin
   repeat
      exec.DoStep(FLoopExpr);
      FLoopExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               Break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
      exec.DoStep(Self);
   until FCondExpr.EvalAsBoolean(exec);
end;

// Optimize
//
function TRepeatExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if FCondExpr.IsConstant and not FCondExpr.EvalAsBoolean(exec) then begin
      Result:=TLoopExpr.Create(prog, FScriptPos);
      TLoopExpr(Result).FLoopExpr:=FLoopExpr;
      FLoopExpr:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TFlowControlExpr ------------------
// ------------------

// InterruptsFlow
//
function TFlowControlExpr.InterruptsFlow : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TBreakExpr ------------------
// ------------------

// EvalNoResult
//
procedure TBreakExpr.EvalNoResult(exec : TdwsExecution);
begin
   exec.Status:=esrBreak;
end;

// ------------------
// ------------------ TBreakExpr ------------------
// ------------------

// EvalNoResult
//
procedure TExitExpr.EvalNoResult(exec : TdwsExecution);
begin
   exec.Status:=esrExit;
end;

// ------------------
// ------------------ TExitValueExpr ------------------
// ------------------

// Create
//
constructor TExitValueExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; assignExpr : TAssignExpr);
begin
   inherited Create(Prog, Pos);
   FAssignExpr:=assignExpr;
end;

// Destroy
//
destructor TExitValueExpr.Destroy;
begin
   FAssignExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TExitValueExpr.EvalNoResult(exec : TdwsExecution);
begin
   FAssignExpr.EvalNoResult(exec);
   exec.Status:=esrExit;
end;

// GetSubExpr
//
function TExitValueExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FAssignExpr;
end;

// GetSubExprCount
//
function TExitValueExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

{ TContinueExpr }

procedure TContinueExpr.EvalNoResult(exec : TdwsExecution);
begin
   exec.Status:=esrContinue;
end;

{ TExceptionExpr }

destructor TExceptionExpr.Destroy;
begin
  FTryExpr.Free;
  FHandlerExpr.Free;
  inherited;
end;

// CreateEDelphiObj
//
function TExceptionExpr.CreateEDelphiObj(exec : TdwsExecution; const ClassName, Message: String): IScriptObj;
var
   info: TProgramInfo;
begin
   info := (exec as TdwsProgramExecution).ProgramInfo;
   Result := IScriptObj(IUnknown(
      info.Vars[SYS_EDELPHI].Method[SYS_TOBJECT_CREATE].Call([ClassName, Message]).Value));
   (Result.ExternalObject as TdwsExceptionContext).ReplaceTop(exec.LastScriptError); // temporary constructor expression
end;

// EnterExceptionBlock
//
procedure TExceptionExpr.EnterExceptionBlock(exec : TdwsExecution; var exceptObj : IScriptObj);
var
   mainException : Exception;
   err : EScriptError;
   msg : String;
begin
   if exec.ExceptionObjectStack.Count>exec.Stack.MaxExceptionDepth then
      raise EScriptExceptionOverflow.CreateFmt(RTE_MaximalExceptionDepthExceeded, [exec.ExceptionObjectStack.Count]);

   {$ifdef FPC}
   mainException:=SysUtils.ExceptObject as Exception;
   {$else}
   mainException:=System.ExceptObject as Exception;
   {$endif}

   if mainException is EScriptException then begin
      // a raise-statement created an Exception object
      exceptObj:=EScriptException(mainException).ExceptionObj
   end else if mainException is EScriptError then begin
      msg:=mainException.Message;
      err:=EScriptError(mainException);
      if Length(err.ScriptCallStack)>0 then
         msg:=msg+' in '+(err.ScriptCallStack[High(err.ScriptCallStack)].Expr as TFuncExpr).FuncSym.QualifiedName;
      if EScriptError(mainException).ScriptPos.Defined then
         msg:=msg+EScriptError(mainException).ScriptPos.AsInfo;
      exceptObj:=CreateEDelphiObj(exec, mainException.ClassName, msg);
   end else if mainException is EScriptStackOverflow then begin
      exceptObj:=nil
   end else begin
      // A Delphi exception. Transform it to a EDelphi-dws exception
      exceptObj:=CreateEDelphiObj(exec, mainException.ClassName, mainException.Message);
   end;

   exec.ExceptionObjectStack.Push(exceptObj);
end;

// LeaveExceptionBlock
//
procedure TExceptionExpr.LeaveExceptionBlock(exec : TdwsExecution);
begin
   exec.ExceptionObjectStack.Peek:=Unassigned;
   exec.ExceptionObjectStack.Pop;
end;

// GetSubExpr
//
function TExceptionExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FTryExpr
   else Result:=FHandlerExpr;
end;

// GetSubExprCount
//
function TExceptionExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

{ TExceptExpr }

destructor TExceptExpr.Destroy;
begin
  inherited;
  FDoExprs.Clean;
  FElseExpr.Free;
end;

// EvalNoResult
//
procedure TExceptExpr.EvalNoResult(exec : TdwsExecution);
var
   x : Integer;
   exceptObj : IScriptObj;
   objSym : TTypeSymbol;
   doExpr : TExceptDoExpr;
   isCaught : Boolean;
   isReraise : Boolean;
   systemExceptObject : TObject;
begin
   try
      exec.DoStep(FTryExpr);
      FTryExpr.EvalNoResult(exec);
   except
      {$ifdef FPC}
      systemExceptObject:=SysUtils.ExceptObject;
      {$else}
      systemExceptObject:=System.ExceptObject;
      {$endif}
      if    (systemExceptObject.ClassType=EScriptStopped)
         or not (systemExceptObject is Exception) then raise;

      EnterExceptionBlock(exec, exceptObj);
      try

         isReraise := False;

         // script exceptions
         if FDoExprs.Count > 0 then begin

            isCaught := False;
            objSym := exceptObj.ClassSym;

            for x := 0 to FDoExprs.Count - 1 do begin
               // Find a "on x: Class do ..." statement matching to this exception class
               doExpr := TExceptDoExpr(FDoExprs.List[x]);
               if doExpr.ExceptionVar.Typ.IsCompatible(objSym) then begin
                  exec.Stack.Data[exec.Stack.BasePointer +  doExpr.FExceptionVar.StackAddr] := exceptObj;
                  try
                     exec.DoStep(doExpr);
                     doExpr.EvalNoResult(exec);
                  except
                     on E : EReraise do isReraise := True;
                  end;
                  if isReraise then break;
                  VarClear(exec.Stack.Data[exec.Stack.BasePointer + doExpr.FExceptionVar.StackAddr]);
                  isCaught := True;
                  Break;
               end;
            end;

            if (not isReraise) and (not isCaught) then begin
               if Assigned(FElseExpr) then begin
                  try
                     exec.DoStep(FElseExpr);
                     FElseExpr.EvalNoResult(exec);
                  except
                     on E : EReraise do isReraise := True;
                  end;
               end else isReraise:=True;
            end;

         end else begin

            try
               exec.DoStep(FHandlerExpr);
               FHandlerExpr.EvalNoResult(exec);
            except
               on E : EReraise do isReraise := True;
            end;

         end;

      finally
         LeaveExceptionBlock(exec);
      end;

      if isReraise then raise;
   end;
   exec.ClearScriptError;
end;

// AddDoExpr
//
procedure TExceptExpr.AddDoExpr(expr : TExceptDoExpr);
begin
   FDoExprs.Add(expr);
end;

// DoExprCount
//
function TExceptExpr.DoExprCount : Integer;
begin
   Result:=FDoExprs.Count;
end;

// GetSubExpr
//
function TExceptExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i<2 then
      Result:=inherited GetSubExpr(i)
   else if i<2+FDoExprs.Count then
      Result:=TExprBase(FDoExprs.List[i-2])
   else Result:=FElseExpr;
end;

// GetSubExprCount
//
function TExceptExpr.GetSubExprCount : Integer;
begin
   Result:=3+FDoExprs.Count;
end;

// GetDoExpr
//
function TExceptExpr.GetDoExpr(i : Integer) : TExceptDoExpr;
begin
   Result:=TExceptDoExpr(FDoExprs.List[i]);
end;

// ------------------
// ------------------ TFinallyExpr ------------------
// ------------------

// EvalNoResult
//
procedure TFinallyExpr.EvalNoResult(exec : TdwsExecution);
var
   oldStatus : TExecutionStatusResult;
   systemExceptObj : TObject;
   exceptObj : IScriptObj;
begin
   try
      exec.DoStep(FTryExpr);
      FTryExpr.EvalNoResult(exec);
   finally
      oldStatus:=exec.Status;
      try
         exec.Status:=esrNone;
         {$ifdef FPC}
         systemExceptObj:=SysUtils.ExceptObject;
         {$else}
         systemExceptObj:=System.ExceptObject;
         {$endif}
         if (systemExceptObj=nil) or (systemExceptObj.ClassType<>EScriptStopped) then begin
            if systemExceptObj is Exception then begin
               EnterExceptionBlock(exec, exceptObj);
               try
                  exec.DoStep(FHandlerExpr);
                  FHandlerExpr.EvalNoResult(exec);
               finally
                  LeaveExceptionBlock(exec);
               end;
            end else begin
               exec.DoStep(FHandlerExpr);
               FHandlerExpr.EvalNoResult(exec);
            end;
         end;
      finally
         exec.Status:=oldStatus;
      end;
   end;
end;

// ------------------
// ------------------ TRaiseExpr ------------------
// ------------------

// Create
//
constructor TRaiseExpr.Create(Prog: TdwsProgram; const scriptPos : TScriptPos; ExceptionExpr: TTypedExpr);
begin
   inherited Create(Prog, scriptPos);
   FExceptionExpr:=ExceptionExpr;
end;

// Destroy
//
destructor TRaiseExpr.Destroy;
begin
   FExceptionExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TRaiseExpr.EvalNoResult(exec : TdwsExecution);
var
   exceptObj : IScriptObj;
   exceptMessage : String;
begin
   FExceptionExpr.EvalAsScriptObj(exec, exceptObj);
   CheckScriptObject(exec, exceptObj);
   exceptMessage:=VarToStr(exceptObj.GetData[0]);
   if exceptMessage<>'' then
      exceptMessage:=Format(RTE_UserDefinedException_Msg, [exceptMessage])
   else exceptMessage:=RTE_UserDefinedException;
   raise EScriptException.Create(exceptMessage, exceptObj, FScriptPos);
end;

// InterruptsFlow
//
function TRaiseExpr.InterruptsFlow : Boolean;
begin
   Result:=True;
end;

// GetSubExpr
//
function TRaiseExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExceptionExpr;
end;

// GetSubExprCount
//
function TRaiseExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TReraiseExpr ------------------
// ------------------

// EvalNoResult
//
procedure TReraiseExpr.EvalNoResult(exec : TdwsExecution);
begin
   raise EReraise.Create('');
end;

// ------------------
// ------------------ TExceptDoExpr ------------------
// ------------------

// Destroy
//
destructor TExceptDoExpr.Destroy;
begin
   FDoBlockExpr.Free;
   FExceptionVar.Free;
   inherited;
end;

// EvalNoResult
//
procedure TExceptDoExpr.EvalNoResult(exec : TdwsExecution);
begin
   DoBlockExpr.EvalNoResult(exec);
end;

// GetSubExpr
//
function TExceptDoExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FDoBlockExpr;
end;

// GetSubExprCount
//
function TExceptDoExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TStringArraySetExpr ------------------
// ------------------

// Create
//
constructor TStringArraySetExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                     StringExpr : TDataExpr; IndexExpr, ValueExpr: TTypedExpr);
begin
   inherited Create(Prog,Pos);
   FStringExpr:=StringExpr;
   FIndexExpr:=IndexExpr;
   FValueExpr:=ValueExpr;
end;

// Destroy
//
destructor TStringArraySetExpr.Destroy;
begin
   FStringExpr.Free;
   FIndexExpr.Free;
   FValueExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TStringArraySetExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   s, buf : UnicodeString;
begin
   {$ifdef FPC}
   FStringExpr.EvalAsUnicodeString(exec, s);
   {$else}
   FStringExpr.EvalAsString(exec, s);
   {$endif}
   i:=FIndexExpr.EvalAsInteger(exec);
   if i>Length(s) then
      RaiseUpperExceeded(exec, i)
   else if i<1 then
      RaiseLowerExceeded(exec, i);
   {$ifdef FPC}
   FValueExpr.EvalAsUnicodeString(exec, buf);
   {$else}
   FValueExpr.EvalAsString(exec, buf);
   {$endif}
   if Length(buf)<>1 then
      RaiseScriptError(exec, EScriptError.CreateFmt(RTE_InvalidInputDataSize, [Length(buf), 1]));
   s[i]:=buf[1];
   {$ifdef FPC}
   FStringExpr.AssignValue(exec, UTF8Encode(s));
   {$else}
   FStringExpr.AssignValue(exec, s);
   {$endif}
end;

// GetSubExpr
//
function TStringArraySetExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FStringExpr;
      1 : Result:=FIndexExpr;
   else
      Result:=FValueExpr;
   end;
end;

// GetSubExprCount
//
function TStringArraySetExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TVarStringArraySetExpr ------------------
// ------------------

// EvalNoResult
//
procedure TVarStringArraySetExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   c : WideChar;
   buf : String;
begin
   i:=FIndexExpr.EvalAsInteger(exec);
   if i<1 then
      RaiseLowerExceeded(exec, i)
   else begin
      FValueExpr.EvalAsString(exec, buf);
      if Length(buf)<>1 then
         RaiseScriptError(exec, EScriptError.CreateFmt(RTE_InvalidInputDataSize, [Length(buf), 1]));
      c:=buf[1];
      if not TStrVarExpr(FStringExpr).SetChar(exec, i, c) then
         RaiseUpperExceeded(exec, i);
   end;
end;

// ------------------
// ------------------ TSpecialUnaryBoolExpr ------------------
// ------------------

// IsConstant
//
function TSpecialUnaryBoolExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TConditionalDefinedExpr ------------------
// ------------------

// EvalAsBoolean
//
function TConditionalDefinedExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   name : String;
begin
   Expr.EvalAsString(exec, name);
   Result:=((exec as TdwsProgramExecution).Prog.ConditionalDefines.Value.IndexOf(name)>=0);
end;

// ------------------
// ------------------ TDefinedExpr ------------------
// ------------------

// EvalAsBoolean
//
function TDefinedExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   v : Variant;
begin
   Expr.EvalAsVariant(exec, v);
   Result:=not VarIsEmpty(v);
end;

// ------------------
// ------------------ TDeclaredExpr ------------------
// ------------------

// EvalAsBoolean
//
function TDeclaredExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   name : String;
begin
   Expr.EvalAsString(exec, name);
   Result:=(FindSymbol((exec as TdwsProgramExecution).Prog.Table, name)<>nil);
end;

// FindSymbol
//
class function TDeclaredExpr.FindSymbol(symbolTable : TSymbolTable; const name : String) : TSymbol;
var
   p, i : Integer;
   identifier : String;
   helpers : THelperSymbols;
   sym : TSymbol;
begin
   p:=Pos('.', name);
   if p<=0 then
      Result:=symbolTable.FindSymbol(name, cvMagic)
   else begin
      Result:=symbolTable.FindSymbol(Copy(name, 1, p-1), cvMagic);
      if Result=nil then Exit;
      identifier:=Copy(name, p+1, MaxInt);
      if Result.ClassType=TUnitSymbol then
         Result:=FindSymbol(TUnitSymbol(Result).Table, identifier)
      else begin
         sym:=Result;
         if Result.InheritsFrom(TCompositeTypeSymbol) then begin
            Result:=FindSymbol(TCompositeTypeSymbol(Result).Members, identifier);
            if Result<>nil then Exit;
         end;
         if sym is TTypeSymbol then begin
            helpers:=THelperSymbols.Create;
            try
               symbolTable.EnumerateHelpers(TTypeSymbol(sym), helpers.AddHelper);
               for i:=0 to helpers.Count-1 do begin
                  Result:=helpers[i].Members.FindSymbol(identifier, cvMagic);
                  if Result<>nil then Exit;
               end;
            finally
               helpers.Free;
            end;
         end;
         Result:=nil;
      end;
   end;
end;

// ------------------
// ------------------ TArrayPseudoMethodExpr ------------------
// ------------------

// Create
//
constructor TArrayPseudoMethodExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                          aBase : TTypedExpr);
begin
   inherited Create(prog, scriptPos);
   FBaseExpr:=aBase;
end;

// Destroy
//
destructor TArrayPseudoMethodExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
end;

// BoundsCheck
//
procedure TArrayPseudoMethodExpr.BoundsCheck(exec : TdwsExecution; dynArray : TScriptDynamicArray; i : Integer);
begin
   if Cardinal(i)>=Cardinal(dynArray.Length) then begin
      if i>=dynArray.Length then
         RaiseUpperExceeded(exec, i)
      else if i<0 then
         RaiseLowerExceeded(exec, i);
   end;
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
constructor TArraySetLengthExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                       aBase, aLength : TTypedExpr);
begin
   inherited Create(prog, scriptPos, aBase);
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
   obj : IScriptObj;
   n : Integer;
begin
   BaseExpr.EvalAsScriptObj(exec, obj);
   n:=LengthExpr.EvalAsInteger(exec);
   if n<0 then
      RaiseScriptError(exec, EScriptOutOfBounds.CreatePosFmt(FScriptPos, RTE_ArrayLengthIncorrect, [n]));
   (obj.InternalObject as TScriptDynamicArray).Length:=n;
end;

// ------------------
// ------------------ TArraySwapExpr ------------------
// ------------------

// Create
//
constructor TArraySwapExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                  aBase, aIndex1, aIndex2 : TTypedExpr);
begin
   inherited Create(prog, scriptPos, aBase);
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

// EvalNoResult
//
procedure TArraySwapExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptObj;
   dyn : TScriptDynamicArray;
   i1, i2 : Integer;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);
   i1:=Index1Expr.EvalAsInteger(exec);
   i2:=Index2Expr.EvalAsInteger(exec);
   BoundsCheck(exec, dyn, i1);
   BoundsCheck(exec, dyn, i2);
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
// ------------------ TArraySortExpr ------------------
// ------------------

// Create
//
constructor TArraySortExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase, aCompare : TTypedExpr);
begin
   inherited Create(prog, scriptPos, aBase);
   FCompareExpr:=aCompare;
end;

// Destroy
//
destructor TArraySortExpr.Destroy;
begin
   inherited;
   FCompareExpr.Free;
end;

// EvalNoResult
//
procedure TArraySortExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptObj;
   dyn : TScriptDynamicArray;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);
   dyn.Sort(exec, CompareExpr);
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
// ------------------ TArrayReverseExpr ------------------
// ------------------

// EvalNoResult
//
procedure TArrayReverseExpr.EvalNoResult(exec : TdwsExecution);
var
   base : IScriptObj;
   dyn : TScriptDynamicArray;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);
   dyn.Reverse;
end;

// ------------------
// ------------------ TArrayAddExpr ------------------
// ------------------

// Create
//
constructor TArrayAddExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                 aBase :  TTypedExpr; argExprs : TTypedExprList);
var
   i : Integer;
begin
   inherited Create(prog, scriptPos, aBase);
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

// EvalNoResult
//
procedure TArrayAddExpr.EvalNoResult(exec : TdwsExecution);
var
   base, src : IScriptObj;
   dyn, dynSrc : TScriptDynamicArray;
   i, n, k : Integer;
   arg : TTypedExpr;
   argData : TDataExpr;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);

   for i:=0 to FArgs.Count-1 do begin
      arg:=TTypedExpr(FArgs.List[i]);

      if dyn.ElementTyp.IsCompatible(arg.Typ) then begin

         n:=dyn.Length;
         dyn.Length:=n+1;
         if arg.Typ.Size>1 then begin
            argData:=(arg as TDataExpr);
            DWSCopyData(argData.Data[exec], argData.Addr[exec],
                        dyn.Data, n*dyn.ElementSize, dyn.ElementSize);
         end else arg.EvalAsVariant(exec, dyn.Data[n]);

      end else if arg.Typ.ClassType=TDynamicArraySymbol then begin

         arg.EvalAsScriptObj(exec, src);
         dynSrc:=(src.InternalObject as TScriptDynamicArray);

         dyn.Concat(dynSrc);

      end else begin

         Assert(arg.Typ is TStaticArraySymbol);
         argData:=(arg as TDataExpr);

         n:=dyn.Length;
         k:=argData.Typ.Size div dyn.ElementSize;
         dyn.Length:=n+k;
         DWSCopyData(argData.Data[exec], argData.Addr[exec],
                     dyn.Data, n*dyn.ElementSize, k*dyn.ElementSize);

      end;
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
function TArrayAddExpr.GetItemExpr(idx : Integer) : TDataExpr;
begin
   Result:=TDataExpr(FArgs.List[idx]);
end;

// ------------------
// ------------------ TArrayDataExpr ------------------
// ------------------

// Create
//
constructor TArrayDataExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                            aBase :  TTypedExpr);
begin
   inherited Create(prog, scriptPos, (aBase.Typ as TDynamicArraySymbol).Typ);
   FBaseExpr:=aBase;
   FResultAddr:=prog.GetTempAddr(Typ.Size);
end;

// Destroy
//
destructor TArrayDataExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
end;

// GetData
//
function TArrayDataExpr.GetData(exec : TdwsExecution) : TData;
begin
   EvalNoResult(exec);
   Result:=exec.Stack.Data;
end;

// GetAddr
//
function TArrayDataExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
  Result:=exec.Stack.BasePointer+FResultAddr;
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
   base : IScriptObj;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   Result:=TScriptDynamicArray(base.InternalObject);
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
   dyn:=GetBaseDynArray(exec);
   if dyn.Length=0 then
      RaiseUpperExceeded(exec, 0);

   idx:=(dyn.Length-1)*dyn.ElementSize;
   DWSCopyData(dyn.Data, idx,
               exec.Stack.Data, exec.Stack.BasePointer+FResultAddr,
               dyn.ElementSize);
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
   dyn.Delete(dyn.Length-1, 1);
end;

// ------------------
// ------------------ TArrayDeleteExpr ------------------
// ------------------

// Create
//
constructor TArrayDeleteExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                    aBase, aIndex, aCount : TTypedExpr);
begin
   inherited Create(prog, scriptPos, aBase);
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
   base : IScriptObj;
   dyn : TScriptDynamicArray;
   index, count : Integer;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);
   index:=IndexExpr.EvalAsInteger(exec);
   BoundsCheck(exec, dyn, index);
   if CountExpr<>nil then begin
      count:=CountExpr.EvalAsInteger(exec);
      if count<0 then
         RaiseScriptError(exec, EScriptError.CreateFmt(RTE_PositiveCountExpected, [count]));
   end else count:=1;
   if count<>0 then begin
      BoundsCheck(exec, dyn, index+count-1);
      dyn.Delete(index, count);
   end;
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
constructor TArrayCopyExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                  aBase, aIndex, aCount : TTypedExpr);
begin
   inherited Create(prog, scriptPos);
   FTyp:=aBase.Typ;
   FBaseExpr:=aBase;
   FIndexExpr:=aIndex;
   FCountExpr:=aCount;
end;

// Destroy
//
destructor TArrayCopyExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
   FIndexExpr.Free;
   FCountExpr.Free;
end;

// Eval
//
function TArrayCopyExpr.Eval(exec : TdwsExecution) : Variant;
var
   obj : IScriptObj;
begin
   EvalAsScriptObj(exec, obj);
   Result:=obj;
end;

// EvalAsScriptObj
//
procedure TArrayCopyExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
var
   base : IScriptObj;
   dyn, newDyn : TScriptDynamicArray;
   index, count : Integer;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);
   if IndexExpr<>nil then begin
      index:=IndexExpr.EvalAsInteger(exec);
      BoundsCheck(exec, dyn, index);
   end else index:=0;
   if CountExpr<>nil then begin
      count:=CountExpr.EvalAsInteger(exec);
      if count<0 then
         RaiseScriptError(exec, EScriptError.CreateFmt(RTE_PositiveCountExpected, [count]));
      BoundsCheck(exec, dyn, index+count-1);
   end else count:=dyn.Length-index;

   newDyn:=TScriptDynamicArray.Create(dyn.ElementTyp);
   if count>0 then
      newDyn.Copy(dyn, index, count);
   Result:=newDyn;
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
constructor TArrayIndexOfExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                                     aBase : TTypedExpr; aItem : TDataExpr; aFromIndex : TTypedExpr);
begin
   inherited Create(prog, scriptPos);
   FBaseExpr:=aBase;
   FItemExpr:=aItem;
   FFromIndexExpr:=aFromIndex;
   Typ:=prog.TypInteger;
end;

// Destroy
//
destructor TArrayIndexOfExpr.Destroy;
begin
   inherited;
   FBaseExpr.Free;
   FItemExpr.Free;
   FFromIndexExpr.Free;
end;

// Eval
//
function TArrayIndexOfExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsInteger(exec);
end;

// EvalAsInteger
//
function TArrayIndexOfExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   base : IScriptObj;
   dyn : TScriptDynamicArray;
   fromIndex : Integer;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);
   if FFromIndexExpr<>nil then
      fromIndex:=FFromIndexExpr.EvalAsInteger(exec)
   else fromIndex:=0;
   if dyn.ElementSize>1 then
      Result:=dyn.IndexOf(TDataExpr(FItemExpr).Data[exec],
                          TDataExpr(FItemExpr).Addr[exec],
                          fromIndex)
   else if FItemExpr.Typ is TFuncSymbol then
      Result:=dyn.IndexOfFuncPtr(FItemExpr.Eval(exec), fromIndex)
   else Result:=dyn.IndexOf(FItemExpr.Eval(exec), fromIndex);
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
// ------------------ TArrayInsertExpr ------------------
// ------------------

// Create
//
constructor TArrayInsertExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
                                    aBase, aIndex : TTypedExpr; aItem : TDataExpr);
begin
   inherited Create(prog, scriptPos, aBase);
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
   base : IScriptObj;
   dyn : TScriptDynamicArray;
   n, index : Integer;
begin
   BaseExpr.EvalAsScriptObj(exec, base);
   dyn:=TScriptDynamicArray(base.InternalObject);

   n:=dyn.Length;

   index:=IndexExpr.EvalAsInteger(exec);
   if index=n then
      dyn.Length:=n+1
   else begin
      BoundsCheck(exec, dyn, index);
      dyn.Insert(index);
   end;

   if ItemExpr.Typ.Size>1 then begin
      DWSCopyData(ItemExpr.Data[exec], ItemExpr.Addr[exec],
                  dyn.Data, index*dyn.ElementSize, dyn.ElementSize);
   end else ItemExpr.EvalAsVariant(exec, dyn.Data[index]);
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
// ------------------ TObjAsIntfExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TObjAsIntfExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseIntfCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_ObjCastToIntfFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

var
   intf : TScriptInterface;
   resolved : TResolvedInterface;
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) then begin
      if not Result.ClassSym.ResolveInterface(TInterfaceSymbol(Typ), resolved) then
         RaiseIntfCastFailed;
      intf:=TScriptInterface.Create(Result, resolved);
      Result:=intf;
   end;
end;

// ------------------
// ------------------ TIntfAsIntfExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TIntfAsIntfExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseIntfCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_IntfCastToIntfFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

var
   intf : TScriptInterface;
   instance : IScriptObj;
   resolved : TResolvedInterface;
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) then begin
      instance:=TScriptInterface(Result.InternalObject).Instance;
      if not instance.ClassSym.ResolveInterface(TInterfaceSymbol(Typ), resolved) then
         RaiseIntfCastFailed;
      intf:=TScriptInterface.Create(instance, resolved);
      Result:=intf;
   end;
end;

// ------------------
// ------------------ TIntfAsClassExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TIntfAsClassExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);

   procedure RaiseIntfCastFailed;
   begin
      RaiseScriptError(exec, EClassCast.CreatePosFmt(FPos, RTE_IntfCastToObjFailed,
                                                     [Result.ClassSym.Caption, FTyp.Caption]))
   end;

var
   intf : TScriptInterface;
begin
   Expr.EvalAsScriptObj(exec, Result);

   if Assigned(Result) then begin
      intf:=TScriptInterface(Result.InternalObject);
      Result:=intf.Instance;
      if not Result.ClassSym.IsCompatible(FTyp) then
         RaiseIntfCastFailed;
   end;
end;

// ------------------
// ------------------ TImplementsIntfOpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TImplementsIntfOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   scriptObj : IScriptObj;
   typIntf : TInterfaceSymbol;
begin
   Left.EvalAsScriptObj(exec, scriptObj);

   if Assigned(scriptObj) then begin
      typIntf:=TInterfaceSymbol(Right.EvalAsInteger(exec));
      Result:=scriptObj.ClassSym.ImplementsInterface(typIntf);
   end else Result:=False;
end;

// ------------------
// ------------------ TClassImplementsIntfOpExpr ------------------
// ------------------

// EvalAsBoolean
//
function TClassImplementsIntfOpExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   classSym : TClassSymbol;
   typIntf : TInterfaceSymbol;
begin
   classSym:=TClassSymbol(Left.EvalAsInteger(exec));

   if Assigned(classSym) then begin
      typIntf:=TInterfaceSymbol(Right.EvalAsInteger(exec));
      Result:=classSym.ImplementsInterface(typIntf);
   end else Result:=False;
end;

// ------------------
// ------------------ TResourceStringExpr ------------------
// ------------------

// Create
//
constructor TResourceStringExpr.Create(aProg : TdwsProgram; const aScriptPos : TScriptPos; aRes : TResourceStringSymbol);
begin
   inherited Create;
   FScriptPos:=aScriptPos;
   FResSymbol:=aRes;
   Typ:=aProg.TypString;
end;

// ScriptPos
//
function TResourceStringExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// Eval
//
function TResourceStringExpr.Eval(exec : TdwsExecution) : Variant;
var
   buf : String;
begin
   exec.LocalizeSymbol(FResSymbol, buf);
   Result:=buf;
end;

// EvalAsString
//
procedure TResourceStringExpr.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   exec.LocalizeSymbol(FResSymbol, Result);
end;

// ------------------
// ------------------ TSwapExpr ------------------
// ------------------

// Create
//
constructor TSwapExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos;
                             expr0, expr1 : TDataExpr);
begin
   inherited Create(prog, scriptPos);
   FArg0:=expr0;
   FArg1:=expr1;
end;

// Destroy
//
destructor TSwapExpr.Destroy;
begin
   FArg0.Free;
   FArg1.Free;
   inherited;
end;

// EvalNoResult
//
procedure TSwapExpr.EvalNoResult(exec : TdwsExecution);
var
   buf, buf0, buf1 : TData;
   size, addr0, addr1 : Integer;
   tmp : Variant;
begin
   size:=Arg0.Typ.Size;
   if size=1 then begin
      Arg0.EvalAsVariant(exec, tmp);
      Arg0.AssignValue(exec, Arg1.Eval(exec));
      Arg1.AssignValue(exec, tmp);
   end else begin
      SetLength(buf, size);
      addr0:=Arg0.Addr[exec];
      buf0:=Arg0.Data[exec];
      addr1:=Arg1.Addr[exec];
      buf1:=Arg1.Data[exec];
      DWSCopyData(buf0, addr0, buf, 0, size);
      Arg0.AssignData(exec, buf1, addr1);
      Arg1.AssignData(exec, buf, 0);
   end;
end;

// GetSubExpr
//
function TSwapExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=Arg0
   else Result:=Arg1;
end;

// GetSubExprCount
//
function TSwapExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TForInStrExpr ------------------
// ------------------

// Create
//
constructor TForInStrExpr.Create(aProg: TdwsProgram; const aPos: TScriptPos;
         aVarExpr : TVarExpr; aInExpr : TTypedExpr; aDoExpr : TNoResultExpr);
begin
   inherited Create(aProg, aPos);
   FVarExpr:=aVarExpr;
   FInExpr:=aInExpr;
   FDoExpr:=aDoExpr;
end;

// Destroy
//
destructor TForInStrExpr.Destroy;
begin
   FDoExpr.Free;
   FInExpr.Free;
   FVarExpr.Free;
   inherited;
end;

// GetSubExpr
//
function TForInStrExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FVarExpr;
      1 : Result:=FInExpr;
   else
      Result:=FDoExpr;
   end;
end;

// GetSubExprCount
//
function TForInStrExpr.GetSubExprCount : Integer;
begin
   Result:=3;
end;

// ------------------
// ------------------ TForCharCodeInStrExpr ------------------
// ------------------

// Create
//
constructor TForCharCodeInStrExpr.Create(aProg: TdwsProgram; const aPos: TScriptPos;
         aVarExpr : TIntVarExpr; aInExpr : TTypedExpr; aDoExpr : TNoResultExpr);
begin
   inherited Create(aProg, aPos, aVarExpr, aInExpr, aDoExpr);
end;

// EvalNoResult
//
procedure TForCharCodeInStrExpr.EvalNoResult(exec : TdwsExecution);
var
   code, i : Integer;
   v : PInt64;
   p : PWideChar;
   s : UnicodeString;
begin
   {$ifdef FPC}
   FInExpr.EvalAsUnicodeString(exec, s);
   {$else}
   FInExpr.EvalAsString(exec, s);
   {$endif}

   v:=TIntVarExpr(FVarExpr).EvalAsPInteger(exec);

   p:=PWideChar(s);
   for i:=1 to Length(s) do begin
      code:=Ord(p^);
      Inc(p);
      case code of
         $D800..$DBFF : // high surrogate
            v^:=(code-$D800)*$400+(Ord(p^)-$DC00)+$10000;
         $DC00..$DFFF : //low surrogate
            continue;
      else
         v^:=code;
      end;

      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
   end;
end;

// ------------------
// ------------------ TForCharCodeInStrExpr ------------------
// ------------------

// Create
//
constructor TForCharInStrExpr.Create(aProg: TdwsProgram; const aPos: TScriptPos;
         aVarExpr : TStrVarExpr; aInExpr : TTypedExpr; aDoExpr : TNoResultExpr);
begin
   inherited Create(aProg, aPos, aVarExpr, aInExpr, aDoExpr);
end;

// EvalNoResult
//
procedure TForCharInStrExpr.EvalNoResult(exec : TdwsExecution);
var
   code, i : Integer;
   p : PWideChar;
   s : UnicodeString;
begin
   {$ifdef FPC}
   FInExpr.EvalAsUnicodeString(exec, s);
   {$else}
   FInExpr.EvalAsString(exec, s);
   {$endif}

   p:=PWideChar(s);
   for i:=1 to Length(s) do begin
      code:=Ord(p^);
      Inc(p);
      case code of
         $D800..$DBFF : // high surrogate
            {$ifdef FPC}
            FVarExpr.AssignValueAsString(exec, UTF8Encode(WideChar(code)+p^));
            {$else}
            FVarExpr.AssignValueAsString(exec, WideChar(code)+p^);
            {$endif}
         $DC00..$DFFF : //low surrogate
            continue;
      else
         {$ifdef FPC}
         if code<128 then
            FVarExpr.AssignValueAsString(exec, Char(code))
         else FVarExpr.AssignValueAsString(exec, UTF8Encode(WideChar(code)));
         {$else}
         FVarExpr.AssignValueAsString(exec, WideChar(code));
         {$endif}
      end;

      exec.DoStep(FDoExpr);
      FDoExpr.EvalNoResult(exec);
      if exec.Status<>esrNone then begin
         case exec.Status of
            esrBreak : begin
               exec.Status:=esrNone;
               break;
            end;
            esrContinue :
               exec.Status:=esrNone;
            esrExit : Exit;
         end;
      end;
   end;
end;

end.
