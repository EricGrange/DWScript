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
{$I dws.inc}
unit dwsCoreExprs;

interface

uses Windows, Classes, Variants, SysUtils, dwsSymbols, dwsErrors, dwsStrings,
   dwsStack, dwsExprs, dwsUtils, dwsTokenizer, dwsRelExprs;

type

   TCaseCondition = class;

   IVarParamData = interface
      function GetData: TData;
      function GetAddr: Integer;
      property Data: TData read GetData;
      property Addr: Integer read GetAddr;
   end;

   TVarExpr = class (TDataExpr)
      protected
         FStackAddr : Integer; // = DataSym.StackAddr
         FStack : TStack;
         function GetAddr: Integer; override;
         function GetData: TData; override;

      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol; DataSym : TDataSymbol);
         class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; DataSym : TDataSymbol) : TVarExpr;

         procedure AssignData(const SourceData: TData; SourceAddr: Integer); override;
         procedure AssignDataExpr(DataExpr: TDataExpr); override;
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         procedure AssignValueAsInteger(const Value: Int64); override;
         procedure AssignValueAsBoolean(const value : Boolean); override;
         procedure AssignValueAsFloat(var Value: Double); override;
         procedure AssignValueAsString(const Value: String); override;

         function Eval: Variant; override;

         function SameVarAs(expr : TVarExpr) : Boolean;
   end;

   TIntVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         procedure AssignValueAsInteger(const Value: Int64); override;
         procedure AssignValueAsPInteger(const pValue: PInt64);
         procedure IncValue(const value: Int64);
         function  EvalAsInteger : Int64; override;
         procedure EvalAsFloat(var Result : Double); override;
         function  EvalAsPInteger : PInt64; inline;
   end;

   TFloatVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         procedure EvalAsFloat(var Result : Double); override;
         function  EvalAsPFloat : PDouble; inline;
   end;

   TStrVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         function  SetChar(index : Integer; c : Char) : Boolean;
         procedure EvalAsString(var Result : String); override;
         procedure Append(const value : String);
   end;

   TBoolVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         function  EvalAsBoolean : Boolean; override;
         function  EvalAsInteger : Int64; override;
   end;

   TObjectVarExpr = class (TVarExpr)
      protected
      public
         procedure EvalAsScriptObj(var Result : IScriptObj); override;
   end;

   TVarParentExpr = class(TVarExpr)
   protected
     FLevel: Integer;
     function GetAddr: Integer; override;
   public
     constructor Create(Prog: TdwsProgram; Typ: TSymbol; DataSym: TDataSymbol);
   end;

   // Encapsulates a lazy parameter
   TLazyParamExpr = class(TNoPosExpr)
      private
         FStackAddr : Integer;
         FLevel : Integer;
      public
         constructor Create(Prog: TdwsProgram; aTyp : TSymbol; level, stackAddr : Integer);
         function  Eval : Variant; override;
         property StackAddr : Integer read FStackAddr write FStackAddr;
         property Level : Integer read FLevel write FLevel;
   end;

   // Encapsulates a var parameter
   TVarParamExpr = class(TVarExpr)
   protected
     function GetAddr: Integer; override;
     function GetData: TData; override;
   public
     procedure AssignData(const SourceData: TData; SourceAddr: Integer); override;
     procedure AssignDataExpr(DataExpr: TDataExpr); override;
     procedure AssignExpr(Expr: TNoPosExpr); override;
     procedure AssignValue(const Value: Variant); override;
     function  Eval : Variant; override;
   end;

   TConstParamExpr = class(TVarParamExpr)
      public
         function IsWritable : Boolean; override;
   end;

   // Encapsulates a var parameter
   TVarParamParentExpr = class(TVarParamExpr)
   protected
     FLevel: Integer;
     function GetAddr: Integer; override;
     function GetData: TData; override;
   public
     constructor Create(Prog: TdwsProgram; Typ: TSymbol; DataSym: TDataSymbol);
   end;

   TConstParamParentExpr = class(TVarParamParentExpr)
   public
      function IsWritable : Boolean; override;
   end;

   // A constant value (like 0, 3.14159, 'Hello' or true)
   TConstExpr = class(TDataExpr)
   protected
     FData: TData;
     function GetData: TData; override;
   public
     constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant); overload;
     constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Data: TData); overload;
     function Eval: Variant; override;
     function IsConstant : Boolean; override;
     function IsWritable : Boolean; override;

     class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Value: String) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Data: TData) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; constSymbol : TConstSymbol) : TConstExpr; overload; static;
   end;

   // TUnifiedConstList
   //
   TUnifiedConstList = class (TSortedList<TExprBase>)
      protected
         function Compare(const item1, item2 : TExprBase) : Integer; override;
      public
         destructor Destroy; override;
   end;

   TUnifiedConstExprClass = class of TUnifiedConstExpr;

   // TUnifiedConstExpr
   //
   {: Unified constants go into a program root unified const list. }
   TUnifiedConstExpr = class (TConstExpr)
      protected
         procedure DoNothing;
      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant); virtual;
         class function CreateUnified(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant) : TUnifiedConstExpr;
         destructor DestroyTrue;
   end;

   // TConstBooleanExpr
   //
   TConstBooleanExpr = class(TUnifiedConstExpr)
      protected
         FValue : Boolean;
      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant); override;
         function EvalAsInteger : Int64; override;
         function EvalAsBoolean : Boolean; override;
   end;

   // TConstIntExpr
   //
   TConstIntExpr = class (TUnifiedConstExpr)
      private
         FValue : Int64;
      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant); override;
         function EvalAsInteger : Int64; override;
         procedure EvalAsFloat(var Result : Double); override;
   end;

   // TConstFloatExpr
   //
   TConstFloatExpr = class(TUnifiedConstExpr)
      private
         FValue : Double;
      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant); override;
         procedure EvalAsFloat(var Result : Double); override;
   end;

   // TConstStringExpr
   //
   TConstStringExpr = class(TUnifiedConstExpr)
      private
         FValue : String;
      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant); override;
         procedure EvalAsString(var Result : String); override;
         property Value : String read FValue write FValue;
   end;

   TArrayConstantExpr = class(TDataExpr)
   protected
     FArrayAddr: Integer;
     FElementExprs: TTightList;
     function GetData : TData; override;
     function GetAddr : Integer; override;
   public
     constructor Create(Prog: TdwsProgram);
     destructor Destroy; override;
     procedure AddElementExpr(ElementExpr: TNoPosExpr);
     procedure Prepare(ElementTyp : TSymbol);
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function Eval: Variant; override;
     function EvalAsTData : TData;
     function EvalAsVarRecArray : TVarRecArrayContainer;
     procedure Initialize; override;
     function Optimize : TNoPosExpr; override;
     function IsConstant : Boolean; override;
     function IsWritable : Boolean; override;
   end;

   // Array expressions x[index]
   TArrayExpr = class(TPosDataExpr)
   protected
     FBaseExpr: TDataExpr;
     FIndexExpr: TNoPosExpr;
     FElementSize: Integer;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function IsWritable : Boolean; override;
   end;

   EScriptOutOfBounds = class (EScriptError);

   // Array expressions x[index] for static arrays
   TStaticArrayExpr = class(TArrayExpr)
   private
     FLowBound: Integer;
     FCount: Integer;
   protected
     function GetAddr: Integer; override;
     function GetData: TData; override;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr; LowBound, HighBound: Integer);
   end;

   // Array expressions x[index] for open arrays
   TOpenArrayExpr = class(TArrayExpr)
   protected
     function GetAddr: Integer; override;
     function GetData: TData; override;
   end;

   // Array expressions: x[index0] for dynamic arrays
   TDynamicArrayExpr = class(TArrayExpr)
   protected
     function GetAddr: Integer; override;
     function GetData: TData; override;
   end;

   // Record expression: record.member
   TRecordExpr = class(TPosDataExpr)
   protected
     FBaseExpr: TDataExpr;
     FMemberOffset: Integer;
     function GetAddr: Integer; override;
     function GetData: TData; override;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; MemberSymbol: TMemberSymbol);
     destructor Destroy; override;
     procedure Initialize; override;
     function IsWritable : Boolean; override;
   end;

   TInitDataExpr = class(TNoResultExpr)
   protected
     FExpr: TDataExpr;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr);
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // Field expression: obj.Field
   TFieldExpr = class(TPosDataExpr)
      protected
         FObjectExpr: TDataExpr;
         FFieldAddr: Integer;
         function GetAddr: Integer; override;
         function GetData: TData; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Typ: TSymbol;
                           FieldSym: TFieldSymbol; ObjExpr: TDataExpr);
         destructor Destroy; override;
         procedure Initialize; override;
         function Eval: Variant; override;
         procedure EvalAsString(var Result : String); override;
         function EvalAsInteger : Int64; override;
         procedure EvalAsScriptObj(var Result : IScriptObj); override;
   end;

   TReadOnlyFieldExpr = class(TFieldExpr)
      function IsWritable: Boolean; override;
   end;

  // length of dynamic arrays
  TArrayLengthExpr = class(TUnaryOpIntExpr)
  private
    FDelta: Integer;
  public
    constructor Create(Prog: TdwsProgram; Expr: TDataExpr; Delta: Integer);
    function EvalAsInteger : Int64; override;
  end;

  // length of an open array
  TOpenArrayLengthExpr = class(TArrayLengthExpr)
  public
    function EvalAsInteger : Int64; override;
  end;

  TStringArrayOpExpr = class(TBinaryOpExpr)
  private
    FPos : TScriptPos;
  public
    constructor CreatePos(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TNoPosExpr);
    function Eval: Variant; override;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
  end;

   TStringLengthExpr = class(TUnaryOpIntExpr)
   public
     function EvalAsInteger : Int64; override;
   end;

   TAssignedExpr = class(TUnaryOpBoolExpr)
   end;

   TAssignedInstanceExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean : Boolean; override;
   end;

   TAssignedMetaClassExpr = class(TAssignedExpr)
   public
     function EvalAsBoolean : Boolean; override;
   end;

   TChrExpr = class(TUnaryOpStringExpr)
   public
     procedure EvalAsString(var Result : String); override;
   end;

   TOrdExpr = class(TUnaryOpIntExpr)
   public
     function EvalAsInteger : Int64; override;
   end;

   TOrdIntExpr = class(TOrdExpr)
   public
     function EvalAsInteger : Int64; override;
   end;

   TOrdStrExpr = class(TOrdExpr)
   public
     function EvalAsInteger : Int64; override;
   end;

   // obj is TMyClass
   TIsOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; Left, Right: TNoPosExpr); override;
     function Eval: Variant; override;
     function EvalAsBoolean : Boolean; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // obj as TMyClass
   TAsOpExpr = class(TBinaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   TObjCmpExpr = class(TBinaryOpExpr)
     FEqual: Boolean;
     constructor CreateCmp(Prog: TdwsProgram; Left, Right: TNoPosExpr; Equal: Boolean);
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // -x
   TNegExpr = class(TUnaryOpExpr)
     function  Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;
   TNegExprClass = class of TNegExpr;
   TNegVariantExpr = class (TNegExpr)
     procedure EvalAsVariant(var Result : Variant); override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;
   TNegIntExpr = class (TNegExpr)
     function EvalAsInteger : Int64; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TNegFloatExpr = class (TNegExpr)
     procedure EvalAsFloat(var Result : Double); override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;

   TVariantBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TIntegerBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); override;
     function Eval: Variant; override;
     procedure EvalAsFloat(var Result : Double); override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TStringBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); override;
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TFloatBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); override;
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TBooleanBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); override;
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;

   TNumberStringBinOpExpr = class(TBinaryOpExpr)
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     class function DoTypeCheckNoPos(const aPos : TScriptPos; prog : TdwsProgram;
                                     left, right : TNoPosExpr) : TSymbol; static;
   end;

   // a + b
   TAddExpr = class(TVariantBinOpExpr)
      function Eval: Variant; override;
   end;
   TAddIntExpr = class(TIntegerBinOpExpr)
      function EvalAsInteger : Int64; override;
      procedure EvalAsFloat(var Result : Double); override;
   end;
   TAddStrExpr = class(TStringBinOpExpr)
      procedure EvalAsString(var Result : String); override;
   end;
   TAddFloatExpr = class(TFloatBinOpExpr)
      procedure EvalAsFloat(var Result : Double); override;
   end;

   // a - b
   TSubExpr = class(TVariantBinOpExpr)
     function Eval: Variant; override;
   end;
   TSubIntExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
     procedure EvalAsFloat(var Result : Double); override;
   end;
   TSubFloatExpr = class(TFloatBinOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a * b
   TMultExpr = class(TVariantBinOpExpr)
     function Eval: Variant; override;
   end;
   TMultIntExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
     procedure EvalAsFloat(var Result : Double); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TMultFloatExpr = class(TFloatBinOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
     function  Optimize : TNoPosExpr; override;
   end;

   // Sqr ( a )
   TSqrIntExpr = class(TUnaryOpIntExpr)
     function EvalAsInteger : Int64; override;
   end;
   TSqrFloatExpr = class(TUnaryOpFloatExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;
   TSqrFloatVarExpr = class(TUnaryOpFloatExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a / b
   TDivideExpr = class(TFloatBinOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a div b
   TDivExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // a mod b
   TModExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // not a
   TNotExpr = class(TUnaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // a and b
   TIntAndExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TBoolAndExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean : Boolean; override;
   end;

   // a or b
   TIntOrExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TBoolOrExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean : Boolean; override;
   end;

   // a xor b
   TIntXorExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TBoolXorExpr = class(TBooleanBinOpExpr)
     function EvalAsBoolean : Boolean; override;
   end;

   // a shl b
   TShlExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // a shr b
   TShrExpr = class(TIntegerBinOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // newType(x)
   TConvExpr = class(TUnaryOpExpr)
   end;

   // Float(x)
   TConvFloatExpr = class (TConvExpr)
     constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
     function Eval: Variant; override;
     procedure EvalAsFloat(var Result : Double); override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // Integer(float)
   TConvIntegerExpr = class (TConvExpr)
     constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
     function Eval: Variant; override;
     function EvalAsInteger : Int64; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // Variant(simple)
   TConvVariantExpr = class (TConvExpr)
     constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // Class(x)
   TConvClassExpr = class (TConvExpr)
     constructor Create(Prog: TdwsProgram; toTyp : TClassSymbol; Expr: TNoPosExpr);
     function Eval: Variant; override;
   end;

   // left := right;
   TAssignExpr = class(TNoResultExpr)
   protected
     FLeft: TDataExpr;
     FRight: TNoPosExpr;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr); virtual;
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
     function  OptimizeConstAssignment : TNoPosExpr;
   end;

   TAssignExprClass = class of TAssignExpr;

   // left := right; (class of)
   TAssignClassOfExpr = class(TAssignExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // left := right;
   TAssignDataExpr = class(TAssignExpr)
   protected
     FSize: Integer;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr); override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // left := [constant array];
   TAssignArrayConstantExpr = class(TAssignDataExpr)
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr); override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // var left := const right;
   TAssignConstDataToVarExpr = class(TAssignDataExpr)
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr); override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // left := const right;
   TAssignConstExpr = class (TAssignExpr)
      public
         procedure Initialize; override;
         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // left := const integer;
   TAssignConstToIntegerVarExpr = class(TAssignConstExpr)
      protected
         FRight : Int64;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         property Right : Int64 read FRight write FRight;
   end;

   // left := const float;
   TAssignConstToFloatVarExpr = class(TAssignConstExpr)
      protected
         FRight : Double;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         property Right : Double read FRight write FRight;
   end;

   // left := const string;
   TAssignConstToStringVarExpr = class(TAssignConstExpr)
      protected
         FRight : String;
      public
         constructor CreateVal(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         property Right : String read FRight write FRight;
   end;

   // a := a op b
   TOpAssignExpr = class(TAssignExpr)
     function  Optimize : TNoPosExpr; override;
   end;

   // a += b
   TPlusAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a += b (int)
   TPlusAssignIntExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     function  Optimize : TNoPosExpr; override;
   end;
   // a += b (float)
   TPlusAssignFloatExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a += b (string)
   TPlusAssignStrExpr = class(TPlusAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     function  Optimize : TNoPosExpr; override;
   end;

   // a -= b
   TMinusAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a -= b (int)
   TMinusAssignIntExpr = class(TMinusAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     function  Optimize : TNoPosExpr; override;
   end;
   // a -= b (float)
   TMinusAssignFloatExpr = class(TMinusAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // a *= b
   TMultAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a *= b (int)
   TMultAssignIntExpr = class(TMultAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a *= b (float)
   TMultAssignFloatExpr = class(TMultAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // a /= b
   TDivideAssignExpr = class(TOpAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // a += b (int var)
   TIncIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a -= b (int var)
   TDecIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // a += b (string var)
   TAppendStringVarExpr = class(TAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // (string var) += (string const)
   TAppendConstStringVarExpr = class(TAssignExpr)
      private
         FAppendString : String;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr); override;
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // val in [case conditions list]
   TInOpExpr = class(TExpr)
      private
         FLeft : TNoPosExpr;
         FCaseConditions: TTightList;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TNoPosExpr);
         destructor Destroy; override;
         function Eval: Variant; override;
         function EvalAsBoolean: Boolean; override;
         procedure Initialize; override;
         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
         function IsConstant : Boolean; override;
         procedure AddCaseCondition(cond : TCaseCondition);
   end;

   // statement; statement; statement;
   TBlockExpr = class(TBlockExprBase)
      private
         FTable : TSymbolTable;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);
         destructor Destroy; override;

         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         function  Optimize : TNoPosExpr; override;

         property  Table: TSymbolTable read FTable;
   end;

   // statement; statement; statement;
   TBlockExprNoTable = class(TBlockExprBase)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // if FCond then FThen else FElse
   TIfExpr = class(TNoResultExpr)
     FCond: TNoPosExpr;
     FElse: TExpr;
     FThen: TExpr;
   public
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // Part of a case statement
   TCaseCondition = class
   private
     FOwnsTrueExpr: Boolean;
     FTrueExpr: TExpr;
     FValueExpr: TNoPosExpr;
     FPos : TScriptPos;
   public
     constructor Create(const aPos : TScriptPos; ValueExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure Initialize; virtual;
     function IsTrue(const Value: Variant): Boolean; virtual; abstract;
     procedure TypeCheck(Typ: TSymbol); virtual; abstract;
     function IsConstant : Boolean; virtual; abstract;
     property Pos : TScriptPos read FPos;
     property TrueExpr: TExpr read FTrueExpr write FTrueExpr;
     property OwnsTrueExpr: Boolean read FOwnsTrueExpr write FOwnsTrueExpr;
   end;

   TCompareCaseCondition = class(TCaseCondition)
   private
     FCompareExpr: TNoPosExpr;
   public
     constructor Create(const aPos : TScriptPos; ValueExpr, CompareExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure Initialize; override;
     function IsTrue(const Value: Variant): Boolean; override;
     procedure TypeCheck(Typ: TSymbol); override;
     function IsConstant : Boolean; override;
   end;

   TRangeCaseCondition = class(TCaseCondition)
   private
     FFromExpr: TNoPosExpr;
     FToExpr: TNoPosExpr;
   public
     constructor Create(const aPos : TScriptPos; ValueExpr, FromExpr, ToExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure Initialize; override;
     function IsTrue(const Value: Variant): Boolean; override;
     procedure TypeCheck(Typ: TSymbol); override;
     function IsConstant : Boolean; override;
   end;

   // case FValueExpr of {CaseConditions} else FElseExpr end;
   TCaseExpr = class(TNoResultExpr)
   private
     FCaseConditions: TTightList;
     FElseExpr: TExpr;
     FValueExpr: TNoPosExpr;
   public
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     procedure AddCaseCondition(cond : TCaseCondition);
     property ValueExpr: TNoPosExpr read FValueExpr write FValueExpr;
     property ElseExpr: TExpr read FElseExpr write FElseExpr;
   end;

   // for FVarExpr := FFromExpr to FToExpr do FDoExpr;
   TForExpr = class(TNoResultExpr)
      private
         FDoExpr: TNoPosExpr;
         FFromExpr: TNoPosExpr;
         FToExpr: TNoPosExpr;
         FVarExpr: TIntVarExpr;
      public
         destructor Destroy; override;
         procedure Initialize; override;

         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
         property DoExpr: TNoPosExpr read FDoExpr write FDoExpr;
         property FromExpr: TNoPosExpr read FFromExpr write FFromExpr;
         property ToExpr: TNoPosExpr read FToExpr write FToExpr;
         property VarExpr: TIntVarExpr read FVarExpr write FVarExpr;
   end;

   TForExprClass = class of TForExpr;

   TForUpwardExpr = class(TForExpr)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TForDownwardExpr = class(TForExpr)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // for FVarExpr := FFromExpr to FToExpr step FStepExpr do FDoExpr;
   TForStepExpr = class(TForExpr)
      private
         FStepExpr: TNoPosExpr;
      public
         destructor Destroy; override;
         procedure Initialize; override;
         function EvalStep : Int64;
         property StepExpr : TNoPosExpr read FStepExpr write FStepExpr;
   end;

   TFoSteprExprClass = class of TForStepExpr;

   TForUpwardStepExpr = class(TForStepExpr)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TForDownwardStepExpr = class(TForStepExpr)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TLoopExpr = class(TNoResultExpr)
   private
     FCondExpr: TNoPosExpr;
     FLoopExpr: TExpr;
   public
     destructor Destroy; override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     property CondExpr: TNoPosExpr read FCondExpr write FCondExpr;
     property LoopExpr: TExpr read FLoopExpr write FLoopExpr;
   end;

   // while FCondExpr do FLoopExpr
   TWhileExpr = class(TLoopExpr)
   public
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // repeat FLoopExpr while FCondExpr
   TRepeatExpr = class(TLoopExpr)
   public
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TFlowControlExpr = class(TNoResultExpr)
   public
   end;

   TBreakExpr = class(TFlowControlExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TExitExpr = class(TFlowControlExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TExitValueExpr = class(TExitExpr)
      private
         FAssignExpr : TNoResultExpr;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; assignExpr : TNoResultExpr);
         destructor Destroy; override;
         procedure Initialize; override;
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TContinueExpr = class(TFlowControlExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TRaiseBaseExpr = class(TNoResultExpr)
   end;

   // raise TExceptionClass.Create;
   TRaiseExpr = class(TRaiseBaseExpr)
   private
     FExceptionExpr: TNoPosExpr;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; ExceptionExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   TReraiseExpr = class(TRaiseBaseExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TExceptionExpr = class(TNoResultExpr)
   private
     FTryExpr: TExpr;
     FHandlerExpr: TExpr;
   public
     destructor Destroy; override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;

     property TryExpr: TExpr read FTryExpr write FTryExpr;
     property HandlerExpr: TExpr read FHandlerExpr write FHandlerExpr;
   end;

   TExceptDoExpr = class;

   // try FTryExpr except {FDoExprs}; else FElseExpr end;
   TExceptExpr = class(TExceptionExpr)
   private
     FDoExprs: TTightList;
     FElseExpr: TExpr;
   public
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     procedure AddDoExpr(expr : TExceptDoExpr);
     property ElseExpr: TExpr read FElseExpr write FElseExpr;
   end;

   // try..except on FExceptionVar: FExceptionVar.Typ do FDoBlockExpr; ... end;
   TExceptDoExpr = class(TNoResultExpr)
   private
     FExceptionVar: TDataSymbol;
     FDoBlockExpr: TExpr;
   public
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     property DoBlockExpr: TExpr read FDoBlockExpr write FDoBlockExpr;
     property ExceptionVar: TDataSymbol read FExceptionVar write FExceptionVar;
   end;

   // try FTryExpr finally FHandlerExpr end;
   TFinallyExpr = class(TExceptionExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TStringArraySetExpr = class(TNoResultExpr)
   private
     FStringExpr: TDataExpr;
     FIndexExpr: TNoPosExpr;
     FValueExpr: TNoPosExpr;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; StringExpr : TDataExpr; IndexExpr, ValueExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
   end;

   TVarStringArraySetExpr = class(TStringArraySetExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TVarStringArraySetChrExpr = class(TStringArraySetExpr)
   public
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   TSpecialUnaryBoolExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function IsConstant : Boolean; override;
         function Eval: Variant; override;
   end;

   TDefinedExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean: Boolean; override;
   end;

   TDeclaredExpr = class(TSpecialUnaryBoolExpr)
      public
         function EvalAsBoolean: Boolean; override;
         class function FindSymbol(symbolTable : TSymbolTable; const name : String) : TSymbol; static;
   end;

   TRegisteredBinaryOperator = record
      ExprClass : TNoPosExprClass;
      LeftType : TSymbol;
      RighType : TSymbol;
   end;

   // lists of binary operators and their expression classes
   // used for operator overloading
   TBinaryOperators = class
      private
         FItems : array [TTokenType] of array of TRegisteredBinaryOperator;

      public
         constructor Create(table : TSymbolTable);

         procedure RegisterOperator(aToken : TTokenType; aExprClass : TNoPosExprClass;
                                    aLeftType, aRightType : TSymbol);

         function ExprClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TNoPosExprClass;
         function BinaryOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TBinaryOpExprClass;
         function AssignmentOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TAssignExprClass;
         function RelOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TRelOpExprClass;
   end;

   EClassCast = class (Exception) end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsStringFunctions;

// ------------------
// ------------------ TVarExpr ------------------
// ------------------

constructor TVarExpr.Create(Prog: TdwsProgram; Typ: TSymbol; DataSym: TDataSymbol);
begin
  inherited Create(Prog, Typ);
  FStackAddr := DataSym.StackAddr;
  FStack:=Prog.Stack;
end;

// CreateTyped
//
class function TVarExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; DataSym : TDataSymbol) : TVarExpr;
begin
   case Typ.BaseTypeID of
      typIntegerID :
         Result:=TIntVarExpr.Create(Prog, Typ, dataSym);
      typFloatID :
         Result:=TFloatVarExpr.Create(Prog, Typ, dataSym);
      typStringID :
         Result:=TStrVarExpr.Create(Prog, Typ, dataSym);
      typBooleanID :
         Result:=TBoolVarExpr.Create(Prog, Typ, dataSym);
   else
      if Typ is TClassSymbol then
         Result:=TObjectVarExpr.Create(Prog, Typ, dataSym)
      else Result:=TVarExpr.Create(Prog, Typ, dataSym);
   end;
end;

function TVarExpr.Eval: Variant;
begin
  Result := FStack.ReadValue(Addr);
end;

// SameVarAs
//
function TVarExpr.SameVarAs(expr : TVarExpr) : Boolean;
begin
   Result:=    (FStack=expr.FStack)
           and (FStackAddr=expr.FStackAddr)
           and (ClassType=expr.ClassType);
end;

function TVarExpr.GetAddr: Integer;
begin
  Result := FStack.BasePointer + FStackAddr;
end;

function TVarExpr.GetData: TData;
begin
  Result := FStack.Data;
end;

procedure TVarExpr.AssignData(const SourceData: TData; SourceAddr: Integer);
begin
  FStack.WriteData(SourceAddr, Addr, Typ.Size, SourceData);
end;

procedure TVarExpr.AssignDataExpr(DataExpr: TDataExpr);
begin
  FStack.WriteData(DataExpr.Addr, Addr, Typ.Size, DataExpr.Data);
end;

procedure TVarExpr.AssignExpr(Expr: TNoPosExpr);
begin
  FStack.WriteValue(Addr, Expr.Eval);
end;

procedure TVarExpr.AssignValue(const Value: Variant);
begin
  FStack.WriteValue(Addr, Value);
end;

// AssignValueAsInteger
//
procedure TVarExpr.AssignValueAsInteger(const Value: Int64);
begin
   FStack.WriteIntValue(Addr, Value);
end;

// AssignValueAsBoolean
//
procedure TVarExpr.AssignValueAsBoolean(const value : Boolean);
begin
   FStack.WriteBoolValue(Addr, Value);
end;

// AssignValueAsFloat
//
procedure TVarExpr.AssignValueAsFloat(var Value: Double);
begin
   FStack.WriteFloatValue(Addr, Value);
end;

// AssignValueAsString
//
procedure TVarExpr.AssignValueAsString(const Value: String);
begin
   FStack.WriteStrValue(Addr, Value);
end;

// ------------------
// ------------------ TIntVarExpr ------------------
// ------------------

procedure TIntVarExpr.AssignExpr(Expr: TNoPosExpr);
begin
   FStack.WriteIntValue(FStack.BasePointer + FStackAddr, Expr.EvalAsInteger);
end;

procedure TIntVarExpr.AssignValue(const Value: Variant);
begin
   FStack.WriteIntValue(FStack.BasePointer + FStackAddr, Value);
end;

// AssignValueAsInteger
//
procedure TIntVarExpr.AssignValueAsInteger(const Value: Int64);
begin
   FStack.WriteIntValue(FStack.BasePointer + FStackAddr, Value);
end;

// AssignValueAsPInteger
//
procedure TIntVarExpr.AssignValueAsPInteger(const pValue: PInt64);
begin
   FStack.WriteIntValue(FStack.BasePointer + FStackAddr, pValue);
end;

// IncValue
//
procedure TIntVarExpr.IncValue(const value: Int64);
begin
   FStack.IncIntValue(FStack.BasePointer + FStackAddr, value);
end;

function TIntVarExpr.EvalAsInteger : Int64;
begin
   Result:=FStack.ReadIntValue(FStack.BasePointer + FStackAddr);
end;

// EvalAsFloat
//
procedure TIntVarExpr.EvalAsFloat(var Result : Double);
begin
   FStack.ReadIntAsFloatValue(FStack.BasePointer + FStackAddr, Result);
end;

// EvalAsPInteger
//
function TIntVarExpr.EvalAsPInteger : PInt64;
begin
   Result:=FStack.PointerToIntValue(FStack.BasePointer + FStackAddr);
end;

// ------------------
// ------------------ TFloatVarExpr ------------------
// ------------------

procedure TFloatVarExpr.AssignExpr(Expr: TNoPosExpr);
var
   buf : Double;
begin
   Expr.EvalAsFloat(buf);
   FStack.WriteFloatValue(FStack.BasePointer+FStackAddr, buf);
end;

procedure TFloatVarExpr.AssignValue(const Value: Variant);
var
   buf : Double;
begin
   buf:=Value;
   FStack.WriteFloatValue(FStack.BasePointer + FStackAddr, buf);
end;

procedure TFloatVarExpr.EvalAsFloat(var Result : Double);
begin
   FStack.ReadFloatValue(FStack.BasePointer + FStackAddr, Result);
end;

// EvalAsPFloat
//
function TFloatVarExpr.EvalAsPFloat : PDouble;
begin
   Result:=FStack.PointerToFloatValue(FStack.BasePointer + FStackAddr);
end;

// ------------------
// ------------------ TStrVarExpr ------------------
// ------------------

procedure TStrVarExpr.AssignExpr(Expr: TNoPosExpr);
var
   buf : String;
begin
   Expr.EvalAsString(buf);
   FStack.WriteStrValue(FStack.BasePointer + FStackAddr, buf);
end;

procedure TStrVarExpr.AssignValue(const Value: Variant);
begin
   FStack.WriteStrValue(FStack.BasePointer + FStackAddr, Value);
end;

function TStrVarExpr.SetChar(index : Integer; c : Char) : Boolean;
begin
  Result:=FStack.SetStrChar(FStack.BasePointer + FStackAddr, index, c);
end;

// EvalAsString
//
procedure TStrVarExpr.EvalAsString(var Result : String);
begin
   FStack.ReadStrValue(FStack.BasePointer + FStackAddr, Result);
end;

// Append
//
procedure TStrVarExpr.Append(const value : String);
begin
   FStack.AppendStringValue(FStack.BasePointer + FStackAddr, value);
end;

// ------------------
// ------------------ TBoolVarExpr ------------------
// ------------------

procedure TBoolVarExpr.AssignExpr(Expr: TNoPosExpr);
begin
   FStack.WriteBoolValue(FStack.BasePointer + FStackAddr, Expr.EvalAsBoolean);
end;

procedure TBoolVarExpr.AssignValue(const Value: Variant);
begin
   FStack.WriteBoolValue(FStack.BasePointer + FStackAddr, Value);
end;

function TBoolVarExpr.EvalAsBoolean : Boolean;
begin
   Result:=FStack.ReadBoolValue(FStack.BasePointer + FStackAddr);
end;

// EvalAsInteger
//
function TBoolVarExpr.EvalAsInteger : Int64;
begin
   Result:=Int64(FStack.ReadBoolValue(FStack.BasePointer + FStackAddr));
end;

// ------------------
// ------------------ TObjectVarExpr ------------------
// ------------------

// EvalAsScriptObj
//
procedure TObjectVarExpr.EvalAsScriptObj(var Result : IScriptObj);
type
   PUnknown = ^IUnknown;
begin
   FStack.ReadInterfaceValue(FStack.BasePointer + FStackAddr, PUnknown(@Result)^);
end;

{ TVarParentExpr }

constructor TVarParentExpr.Create(Prog: TdwsProgram; Typ: TSymbol; DataSym: TDataSymbol);
begin
  inherited;
  FLevel := DataSym.Level;
end;

function TVarParentExpr.GetAddr: Integer;
begin
  Result := FStack.GetSavedBp(FLevel) + FStackAddr;
end;

{ TVarParamExpr }

function TVarParamExpr.GetAddr: Integer;
begin
  Result := IVarParamData(IUnknown(FStack.Data[FStack.BasePointer + FStackAddr])).Addr;
end;

function TVarParamExpr.GetData: TData;
begin
  Result := IVarParamData(IUnknown(FStack.Data[FStack.BasePointer + FStackAddr])).Data;
end;

procedure TVarParamExpr.AssignData(const SourceData: TData; SourceAddr: Integer);
begin
  CopyData(SourceData, SourceAddr, Data, Addr, Typ.Size);
end;

procedure TVarParamExpr.AssignValue(const Value: Variant);
begin
  VarCopy(Data[Addr], Value);
end;

procedure TVarParamExpr.AssignExpr(Expr: TNoPosExpr);
begin
  VarCopy(Data[Addr], Expr.Eval);
end;

procedure TVarParamExpr.AssignDataExpr(DataExpr: TDataExpr);
begin
  CopyData(DataExpr.Data, DataExpr.Addr, Data, Addr, Typ.Size);
end;

function TVarParamExpr.Eval: Variant;
begin
  Result := Data[Addr];
end;

{ TConstParamExpr }

function TConstParamExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

{ TVarParamParentExpr }

constructor TVarParamParentExpr.Create(Prog: TdwsProgram; Typ: TSymbol; DataSym: TDataSymbol);
begin
  inherited;
  FLevel := DataSym.Level;
end;

function TVarParamParentExpr.GetAddr: Integer;
begin
  Result := IVarParamData(IUnknown(FStack.Data[FStack.GetSavedBp(FLevel) + FStackAddr])).Addr;
end;

function TVarParamParentExpr.GetData: TData;
begin
  Result := IVarParamData(IUnknown(FStack.Data[FStack.GetSavedBp(FLevel) + FStackAddr])).Data;
end;

{ TConstParamParentExpr }

function TConstParamParentExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TUnifiedConstList<TExprBase> ------------------
// ------------------

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
               varUString : rawResult:=CompareStr(String(vd1.VUString), String(vd2.VUString));
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

// Destroy
//
destructor TUnifiedConstList.Destroy;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      TUnifiedConstExpr(Items[i]).DestroyTrue;
   inherited;
end;

{ TConstExpr }

constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
  inherited Create(Prog, Typ);
  Assert(Typ.Size=1);
  SetLength(FData, 1);
  FData[0] := Value;
end;

constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Data: TData);
begin
  inherited Create(Prog, Typ);
  FData := Data;
end;

function TConstExpr.Eval: Variant;
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

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant) : TConstExpr;
begin
   if Typ=Prog.TypString then
      Result:=TConstStringExpr.CreateUnified(Prog, Typ, Value)
   else if (Typ=Prog.TypInteger) or (Typ.Typ=Prog.TypInteger) then
      Result:=TConstIntExpr.CreateUnified(Prog, Typ, Value)
   else if Typ=Prog.TypBoolean then
      Result:=TConstBooleanExpr.CreateUnified(Prog, Typ, Value)
   else if Typ=Prog.TypFloat then
      Result:=TConstFloatExpr.CreateUnified(Prog, Typ, Value)
   else Result:=TConstExpr.Create(Prog, Typ, Value);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Value: String) : TConstExpr;
begin
   Result:=CreateTyped(Prog, Typ, Variant(Value));
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Data: TData) : TConstExpr;
begin
   if Length(Data)=1 then
      Result:=TConstExpr.CreateTyped(Prog, Typ, Data[0])
   else Result:=TConstExpr.Create(Prog, Typ, Data);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; constSymbol : TConstSymbol) : TConstExpr;
begin
   if constSymbol<>nil then
      Result:=CreateTyped(Prog, Typ, constSymbol.Data)
   else Result:=CreateTyped(Prog, Typ, Unassigned);
end;

// GetData
//
function TConstExpr.GetData: TData;
begin
  Result := FData;
end;

// ------------------
// ------------------ TUnifiedConstExpr ------------------
// ------------------

// Create
//
constructor TUnifiedConstExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
   inherited Create(Prog, Typ, Value);
end;

// CreateUnified
//
class function TUnifiedConstExpr.CreateUnified(Prog: TdwsProgram; Typ: TSymbol;
                                               const Value: Variant) : TUnifiedConstExpr;
const
   vmtDestroy = -4;
var
   i : Integer;
   p : Pointer;
   n : Cardinal;
   added : Boolean;
begin
   Result:=Self.Create(Prog, Typ, Value);

   i:=Prog.Root.UnifiedConstList.AddOrFind(Result, added);
   if not added then begin
      Result.DestroyTrue;
      Result:=TUnifiedConstExpr(Prog.Root.UnifiedConstList[i]);
      Exit;
   end;

   p:=@TUnifiedConstExpr.DoNothing;
   if PPointer(NativeInt(Self)+vmtDestroy)^<>p then begin
      WriteProcessMemory(GetCurrentProcess,
                         Pointer(NativeInt(Self)+vmtDestroy),
                         @p, SizeOf(Pointer), n);
   end;
end;

// DoNothing
//
procedure TUnifiedConstExpr.DoNothing;
begin
   // nothing
end;

// DestroyTrue
//
destructor TUnifiedConstExpr.DestroyTrue;
begin
   inherited Destroy;
end;

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypBoolean;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstBooleanExpr.EvalAsInteger : Int64;
begin
   Result:=Integer(FValue);
end;

// EvalAsBoolean
//
function TConstBooleanExpr.EvalAsBoolean : Boolean;
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TConstIntExpr ------------------
// ------------------

// Create
//
constructor TConstIntExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypInteger;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsInteger
//
function TConstIntExpr.EvalAsInteger : Int64;
{$ifdef PUREPASCAL}
begin
   Result:=FValue;
{$else}
asm
   mov   edx, [eax + OFFSET FValue + 4]
   mov   eax, [eax + OFFSET FValue]
{$endif}
end;

// EvalAsFloat
//
procedure TConstIntExpr.EvalAsFloat(var Result : Double);
{$ifdef PUREPASCAL}
begin
   Result:=FValue;
{$else}
asm
   fild  qword ptr [eax + OFFSET FValue]
   fstp  qword ptr [edx]
{$endif}
end;

// ------------------
// ------------------ TConstFloatExpr ------------------
// ------------------

// Create
//
constructor TConstFloatExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypFloat;
   FValue:=Value;
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsFloat
//
procedure TConstFloatExpr.EvalAsFloat(var Result : Double);
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TConstStringExpr ------------------
// ------------------

// Create
//
constructor TConstStringExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
   if Typ=nil then
      Typ:=Prog.TypString;
   UnifyAssignString(Value, FValue);
   inherited Create(Prog, Typ, FValue);
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(var Result : String);
begin
   Result:=FValue;
end;

{ TArrayExpr }

constructor TArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr);
begin
  inherited Create(Prog, Pos, BaseExpr.BaseType.Typ);
  FBaseExpr := BaseExpr;
  FIndexExpr := IndexExpr;
  FElementSize := Typ.Size; // Necessary because of arrays of records!
end;

destructor TArrayExpr.Destroy;
begin
  FBaseExpr.Free;
  FIndexExpr.Free;
  inherited;
end;

procedure TArrayExpr.Initialize;
begin
  FBaseExpr.Initialize;
  FIndexExpr.Initialize;
  inherited;
end;

// TypeCheckNoPos
//
procedure TArrayExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   FBaseExpr.TypeCheckNoPos(aPos);
   FIndexExpr.TypeCheckNoPos(aPos);
   FTyp:=FBaseExpr.Typ.Typ;
end;

// IsWritable
//
function TArrayExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

{ TStaticArrayExpr }

constructor TStaticArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr; LowBound, HighBound: Integer);
begin
   inherited Create(Prog, Pos, BaseExpr, IndexExpr);
   FLowBound:=LowBound;
   FCount:=HighBound-LowBound+1;
end;

function TStaticArrayExpr.GetAddr: Integer;
var
  index: Integer;
begin
   // Get index
   index := FIndexExpr.EvalAsInteger - FLowBound;

   if Cardinal(index)>=Cardinal(FCount) then begin
      if index>=FCount then
         raise EScriptOutOfBounds.Create(RTE_UpperBoundExceeded)
      else raise EScriptOutOfBounds.Create(RTE_LowerBoundExceeded);
   end;
   // Calculate the address
   Result := FBaseExpr.Addr + (index * FElementSize);
end;

function TStaticArrayExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

{ TOpenArrayExpr }

function TOpenArrayExpr.GetAddr: Integer;
var
   index, len: Integer;
begin
   index := FIndexExpr.EvalAsInteger;

   len := Length(FBaseExpr.Data);

   if Cardinal(index)>=Cardinal(len) then begin
      if index >= len then
         AddExecutionStop(RTE_UpperBoundExceeded)
      else if index < 0 then
         AddExecutionStop(RTE_LowerBoundExceeded);
   end;
   // Calculate the address
   Result := index;
end;

function TOpenArrayExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

{ TDynamicArrayExpr }

function TDynamicArrayExpr.GetAddr: Integer;
var
   index, length: Integer;
   baseAddr: Integer;
begin
   baseAddr := FBaseExpr.EvalAsInteger;
   index := FIndexExpr.EvalAsInteger;

   length := Prog.Stack.Data[baseAddr - 1];

   if Cardinal(index)>=Cardinal(length) then begin
      if index >= length then
         AddExecutionStop(RTE_UpperBoundExceeded)
      else if index < 0 then
         AddExecutionStop(RTE_LowerBoundExceeded);
   end;
   // Calculate the address
   Result := baseAddr + (index * FElementSize);
end;

function TDynamicArrayExpr.GetData: TData;
begin
  Result := Prog.Stack.Data;
end;

{ TArrayConstantExpr }

constructor TArrayConstantExpr.Create(Prog: TdwsProgram);
begin
  inherited Create(Prog, TStaticArraySymbol.Create('', Prog.TypNil, 0, -1));
end;

destructor TArrayConstantExpr.Destroy;
begin
  FElementExprs.Clean;
  FTyp.Free;
  inherited;
end;

procedure TArrayConstantExpr.AddElementExpr(ElementExpr: TNoPosExpr);
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

procedure TArrayConstantExpr.Prepare(ElementTyp : TSymbol);
var
   x : Integer;
   elemExpr : TNoPosExpr;
begin
   if (ElementTyp<>nil) and (FTyp.Typ <> ElementTyp) then begin
      // need a compatibility check here maybe???
      (FTyp as TStaticArraySymbol).Typ:=ElementTyp;
   end;

   for x := 0 to FElementExprs.Count - 1 do begin
      elemExpr:=FElementExprs.List[x];
      if elemExpr is TArrayConstantExpr then
         TArrayConstantExpr(elemExpr).Prepare(FTyp.Typ.Typ);
   end;

   FArrayAddr := FProg.GetGlobalAddr(FElementExprs.Count * FTyp.Typ.Size + 1);
end;

function TArrayConstantExpr.GetData: TData;
begin
  Eval;
  Result := FProg.Stack.Data;// FData;
end;

// GetAddr
//
function TArrayConstantExpr.GetAddr : Integer;
begin
   Result:=FArrayAddr+1;
end;

function TArrayConstantExpr.Eval: Variant;
var
  x: Integer;
  elemSize: Integer;
begin
   Assert(False); // at the moment, Eval shouldn't ever be invoked

   if FElementExprs.Count>0 then
      Prog.Stack.WriteValue(FArrayAddr, FElementExprs.Count);

  elemSize := Typ.Typ.Size;
  if elemSize = 1 then
  begin
    for x := 0 to FElementExprs.Count - 1 do
    begin
      FProg.Stack.WriteValue(FArrayAddr + 1 + x, TNoPosExpr(FElementExprs.List[x]).Eval);
    end;
  end
  else begin
    for x := 0 to FElementExprs.Count - 1 do
    begin
      FProg.Stack.WriteData(
        TDataExpr(FElementExprs.List[x]).Addr,
        FArrayAddr + 1 + x * elemSize,
        elemSize,
        TDataExpr(FElementExprs.List[x]).Data);
    end;
  end;
  Result := FArrayAddr + 1;
end;

// EvalAsTData
//
function TArrayConstantExpr.EvalAsTData : TData;
var
   i : Integer;
   expr : TNoPosExpr;
begin
   SetLength(Result, FElementExprs.Count);
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TNoPosExpr(FElementExprs.List[i]);
      Result[i]:=expr.Eval;
   end;
end;

// EvalAsVarRecArray
//
function TArrayConstantExpr.EvalAsVarRecArray : TVarRecArrayContainer;
var
   i : Integer;
   expr : TNoPosExpr;
begin
   Result:=TVarRecArrayContainer.Create;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TNoPosExpr(FElementExprs.List[i]);
      Result.Add(expr.Eval);
   end;
   Result.Initialize;
end;

procedure TArrayConstantExpr.Initialize;
var
   i : Integer;
begin
  inherited;
  for i:=0 to FElementExprs.Count-1 do
     TNoPosExpr(FElementExprs.List[i]).Initialize;
end;

// Optimize
//
function TArrayConstantExpr.Optimize : TNoPosExpr;
var
   i : Integer;
   expr : TNoPosExpr;
begin
   Result:=Self;
   for i:=0 to FElementExprs.Count-1 do begin
      expr:=TNoPosExpr(FElementExprs.List[i]);
      FElementExprs.List[i]:=expr.Optimize;
   end;
end;

// IsConstant
//
function TArrayConstantExpr.IsConstant : Boolean;
var
   i : Integer;
begin
   for i:=0 to FElementExprs.Count-1 do
      if not TNoPosExpr(FElementExprs.List[i]).IsConstant then
         Exit(False);
   Result:=True;
end;

// IsWritable
//
function TArrayConstantExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// TypeCheckNoPos
//
procedure TArrayConstantExpr.TypeCheckNoPos(const aPos : TScriptPos);
var
   x: Integer;
   expr : TNoPosExpr;
begin
   if FElementExprs.Count=0 then begin
      (FTyp as TStaticArraySymbol).Typ:=Prog.TypVariant;
      Exit;
   end;
   for x:=0 to FElementExprs.Count - 1 do begin
      expr:=TNoPosExpr(FElementExprs.List[x]);
      expr.TypeCheckNoPos(aPos);
      if (Typ.Typ=Prog.TypFloat) and (expr.Typ=Prog.TypInteger) then begin
         expr:=TConvFloatExpr.Create(Prog, expr);
         FElementExprs.List[x]:=expr;
      end;
      if not expr.Typ.IsCompatible(Typ.Typ) then
         expr.Prog.Msgs.AddCompilerErrorFmt(aPos, CPE_AssignIncompatibleTypes,
                                            [expr.Typ.Caption, Typ.Typ.Caption]);
   end;
end;

{ TRecordExpr }

constructor TRecordExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  BaseExpr: TDataExpr; MemberSymbol: TMemberSymbol);
begin
  inherited Create(Prog, Pos, MemberSymbol.Typ);
  FBaseExpr := BaseExpr;
  FMemberOffset := MemberSymbol.Offset;
end;

destructor TRecordExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

function TRecordExpr.GetAddr: Integer;
begin
  Result := FBaseExpr.Addr + FMemberOffset;
end;

function TRecordExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

procedure TRecordExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
end;

// IsWritable
//
function TRecordExpr.IsWritable : Boolean;
begin
   Result:=FBaseExpr.IsWritable;
end;

{ TInitDataExpr }

constructor TInitDataExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr);
begin
  inherited Create(Prog, Pos);
  FExpr := Expr;
end;

destructor TInitDataExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

procedure TInitDataExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
  FExpr.Typ.InitData(FExpr.Data, FExpr.Addr);
end;

{ TFieldExpr }

constructor TFieldExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Typ: TSymbol;
  FieldSym: TFieldSymbol; ObjExpr: TDataExpr);
begin
  inherited Create(Prog, Pos, Typ);
  FObjectExpr := ObjExpr;
  FFieldAddr := FieldSym.Offset;
end;

destructor TFieldExpr.Destroy;
begin
  FObjectExpr.Free;
  inherited;
end;

function TFieldExpr.GetAddr: Integer;
begin
  Result := FFieldAddr;
end;

function TFieldExpr.GetData: TData;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(obj);
   if obj=nil then
      AddExecutionStop(RTE_ObjectNotInstantiated);
   Result:=obj.Data;
end;

procedure TFieldExpr.Initialize;
begin
   FObjectExpr.Initialize;
end;

// Eval
//
function TFieldExpr.Eval: Variant;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(obj);
   if obj=nil then
      AddExecutionStop(RTE_ObjectNotInstantiated);
   Result:=obj.DataOfAddr(FFieldAddr);
end;

// EvalAsString
//
procedure TFieldExpr.EvalAsString(var Result : String);
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(obj);
   if obj=nil then
      AddExecutionStop(RTE_ObjectNotInstantiated);
   Result:=obj.DataOfAddrAsString(FFieldAddr);
end;

// EvalAsInteger
//
function TFieldExpr.EvalAsInteger : Int64;
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(obj);
   if obj=nil then
      AddExecutionStop(RTE_ObjectNotInstantiated);
   Result:=obj.DataOfAddrAsInteger(FFieldAddr);
end;

// EvalAsScriptObj
//
procedure TFieldExpr.EvalAsScriptObj(var Result : IScriptObj);
var
   obj : IScriptObj;
begin
   FObjectExpr.EvalAsScriptObj(obj);
   if obj=nil then
      AddExecutionStop(RTE_ObjectNotInstantiated);
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
constructor TLazyParamExpr.Create(Prog: TdwsProgram; aTyp : TSymbol; level, stackAddr : Integer);
begin
   inherited Create(Prog);
   FTyp:=aTyp;
   FLevel:=level;
   FStackAddr:=stackAddr;
end;

// Eval
//
function TLazyParamExpr.Eval : Variant;
var
   stack : TStack;
   lazyExpr : TExprBase;
   oldBasePointer: Integer;
   lazyContext : Int64;
begin
   stack:=FProg.Stack;

   lazyContext:=stack.ReadIntValue(stack.BasePointer + FStackAddr);
   lazyExpr:=TExprBase(lazyContext and $FFFFFFFF);

   oldBasePointer:=stack.BasePointer;
   stack.BasePointer:=(lazyContext shr 32);//  stack.GetSavedBp(Level);
   try
      Result:=lazyExpr.Eval;
   finally
      stack.BasePointer:=oldBasePointer;
   end;
end;

{ TArrayLengthExpr }

constructor TArrayLengthExpr.Create(Prog: TdwsProgram; Expr: TDataExpr; Delta: Integer);
begin
  inherited Create(Prog, Expr);
  FDelta := Delta;
end;

function TArrayLengthExpr.EvalAsInteger : Int64;
var
  adr: Integer;
begin
  adr := TDataExpr(FExpr).Data[TDataExpr(FExpr).Addr];
  Result := TDataExpr(FExpr).Data[adr - 1] + FDelta;
end;

{ TOpenArrayLengthExpr }

function TOpenArrayLengthExpr.EvalAsInteger : Int64;
begin
  Result := Length(TDataExpr(FExpr).Data)+FDelta;
end;

{ TStringArrayOpExpr }

constructor TStringArrayOpExpr.CreatePos(Prog: TdwsProgram; const Pos: TScriptPos;
                                         Left, Right: TNoPosExpr);
begin
  inherited Create(Prog, Left, Right);
  FPos := Pos;
  FTyp := FProg.TypString;
end;

function TStringArrayOpExpr.Eval: Variant;
var
   i : Integer;
   buf : String;
begin
   FLeft.EvalAsString(buf);
   i:=FRight.EvalAsInteger;
   if i>Length(buf) then
      raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayUpperBoundExceeded, [i])
   else if i<1 then
      raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayLowerBoundExceeded, [i]);
   Result:=buf[i];
end;

// TypeCheckNoPos
//
procedure TStringArrayOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if not (FLeft.IsStringValue) and (FRight.IsIntegerValue) then
      Prog.Msgs.AddCompilerStop(FPos, CPE_StringExpected);
end;

{ TIsOpExpr }

constructor TIsOpExpr.Create(Prog: TdwsProgram; Left, Right: TNoPosExpr);
begin
   inherited;
   FTyp := Prog.TypBoolean;
end;

function TIsOpExpr.Eval: Variant;
begin
   Result:=EvalAsBoolean;
end;

// EvalAsBoolean
//
function TIsOpExpr.EvalAsBoolean : Boolean;
var
  scriptObj: IScriptObj;
begin
  scriptObj := IScriptObj(IUnknown(FLeft.Eval));
  if Assigned(scriptObj) then
    Result := FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym)
  else
    Result := False;
end;

// TypeCheckNoPos
//
procedure TIsOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FLeft.TypeCheckNoPos(aPos);
   FRight.TypeCheckNoPos(aPos);
   if not (FLeft.Typ is TClassSymbol) then
      FProg.Msgs.AddCompilerStop(aPos, CPE_ObjectExpected);
   if not (FRight.Typ is TClassOfSymbol) then
      FProg.Msgs.AddCompilerStop(aPos, CPE_ClassRefExpected);
end;

{ TAsOpExpr }

function TAsOpExpr.Eval: Variant;
var
  scriptObj: IScriptObj;
begin
  Result := FLeft.Eval;
  scriptObj := IScriptObj(IUnknown(Result));

  if Assigned(scriptObj) and not (FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym)) then
    raise EClassCast.CreateFmt(RTE_ClassCastFailed, [scriptObj.ClassSym.Caption, FRight.Typ.Typ.Caption]);
end;

// TypeCheckNoPos
//
procedure TAsOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FLeft.TypeCheckNoPos(aPos);
   FRight.TypeCheckNoPos(aPos);
   if not (FLeft.Typ is TClassSymbol) then
      FProg.Msgs.AddCompilerStop(aPos, CPE_ObjectExpected);
   if not (FRight.Typ is TClassOfSymbol) then
      FProg.Msgs.AddCompilerStop(aPos, CPE_ClassRefExpected);
   FTyp := FRight.Typ.Typ;
end;

{ TInOpExpr }

// Create
//
constructor TInOpExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left: TNoPosExpr);
begin
   inherited Create(Prog, Pos);
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
function TInOpExpr.Eval: Variant;
begin
   Result:=EvalAsBoolean;
end;

// EvalAsBoolean
//
function TInOpExpr.EvalAsBoolean: Boolean;
var
   i : Integer;
   value : Variant;
   cc : TCaseCondition;
begin
   value:=FLeft.Eval;
   for i:=0 to FCaseConditions.Count-1 do begin
      cc:=TCaseCondition(FCaseConditions.List[i]);
      if cc.IsTrue(Value) then
         Exit(True);
   end;
   Result:=False;
end;

// Initialize
//
procedure TInOpExpr.Initialize;
var
   i : Integer;
begin
   FLeft.Initialize;
   for i:=0 to FCaseConditions.Count-1 do
      TCaseCondition(FCaseConditions.List[i]).Initialize;
end;

// TypeCheckNoPos
//
procedure TInOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
var
   i : Integer;
begin
   for i:=0 to FCaseConditions.Count-1 do
      TCaseCondition(FCaseConditions.List[i]).TypeCheck(FLeft.Typ);
end;

// IsConstant
//
function TInOpExpr.IsConstant : Boolean;
var
   i : Integer;
begin
   Result:=FLeft.IsConstant;
   if Result then
      for i:=0 to FCaseConditions.Count-1 do
         if not TCaseCondition(FCaseConditions.List[i]).IsConstant then
            Exit(False);
end;

// AddCaseCondition
//
procedure TInOpExpr.AddCaseCondition(cond : TCaseCondition);
begin
   FCaseConditions.Add(cond);
end;

{ TConvFloatExpr }

constructor TConvFloatExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
  inherited;
  FTyp := Prog.TypFloat;
end;

function TConvFloatExpr.Eval: Variant;
begin
  VarCast(Result, FExpr.Eval, varDouble);
end;

// EvalAsFloat
//
procedure TConvFloatExpr.EvalAsFloat(var Result : Double);
begin
   FExpr.EvalAsFloat(Result);
end;

// TypeCheckNoPos
//
procedure TConvFloatExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if not (FExpr.IsIntegerValue) or (FExpr.IsVariantValue) then
      FProg.Msgs.AddCompilerError(aPos, CPE_IntegerExpected);
end;

{ TConvIntegerExpr }

constructor TConvIntegerExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
  inherited;
  FTyp := Prog.TypInteger;
end;

function TConvIntegerExpr.Eval: Variant;
begin
   Result:=FExpr.EvalAsInteger;
end;

function TConvIntegerExpr.EvalAsInteger : Int64;
begin
   Result:=FExpr.EvalAsInteger;
end;

// TypeCheckNoPos
//
procedure TConvIntegerExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if not (   FExpr.IsNumberValue or FExpr.IsBooleanValue
          or (FExpr.Typ is TEnumerationSymbol) or FExpr.IsVariantValue) then
    FProg.Msgs.AddCompilerError(aPos, CPE_IntegerCastInvalid);
end;

{ TConvVariantExpr }

constructor TConvVariantExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
  inherited;
  FTyp := FProg.TypInteger;
end;

function TConvVariantExpr.Eval: Variant;
begin
  Result := FExpr.Eval;
end;

// TypeCheckNoPos
//
procedure TConvVariantExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if not FExpr.IsVariantValue then
    FProg.Msgs.AddCompilerError(aPos, CPE_VariantExpected);
end;

// ------------------
// ------------------ TConvClassExpr ------------------
// ------------------

// Create
//
constructor TConvClassExpr.Create(Prog: TdwsProgram; toTyp : TClassSymbol; Expr: TNoPosExpr);
begin
   inherited Create(Prog, Expr);
   FTyp:=toTyp;
end;

// Eval
//
function TConvClassExpr.Eval: Variant;
var
   obj : IScriptObj;
begin
   FExpr.EvalAsScriptObj(obj);
   if (obj<>nil) and (not obj.ClassSym.IsOfType(Typ)) then
      raise EClassCast.CreateFmt(RTE_ClassCastFailed, [obj.ClassSym.Name, Typ.Name]);
   Result:=obj;
end;

{ TStringLengthExpr }

// EvalAsInteger
//
function TStringLengthExpr.EvalAsInteger : Int64;
var
   buf : String;
begin
   FExpr.EvalAsString(buf);
   Result:=Length(buf);
end;

// ------------------
// ------------------ TAssignedInstanceExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedInstanceExpr.EvalAsBoolean : Boolean;
var
   obj : IScriptObj;
begin
   FExpr.EvalAsScriptObj(obj);
   Result:=(obj<>nil);
end;

// ------------------
// ------------------ TAssignedMetaClassExpr ------------------
// ------------------

// EvalAsBoolean
//
function TAssignedMetaClassExpr.EvalAsBoolean : Boolean;
var
   s : String;
begin
   FExpr.EvalAsString(s);
   Result:=(s<>'');
end;

{ TChrExpr }

// EvalAsString
//
procedure TChrExpr.EvalAsString(var Result : String);
begin
   Result:=Chr(FExpr.EvalAsInteger);
end;

{ TOrdExpr }

// EvalAsInteger
//
function TOrdExpr.EvalAsInteger : Int64;
var
   v : Variant;
   s : String;
begin
   v:=FExpr.Eval;
   if VarIsOrdinal(v) then
      Result:=v
   else if VarIsStr(v) then begin
      s:=v;
      if s<>'' then
         Result:=Ord(s[1])
      else Result:=0;
   end else Result:=0;
end;

{ TOrdIntExpr }

// EvalAsInteger
//
function TOrdIntExpr.EvalAsInteger : Int64;
begin
   Result:=FExpr.EvalAsInteger;
end;

{ TOrdStrExpr }

// EvalAsInteger
//
function TOrdStrExpr.EvalAsInteger : Int64;
var
   s : String;
begin
   FExpr.EvalAsString(s);
   if s<>'' then
      Result:=Ord(s[1])
   else Result:=0;
end;

{ TObjCmpExpr }

constructor TObjCmpExpr.CreateCmp(Prog: TdwsProgram; Left, Right: TNoPosExpr; Equal: Boolean);
begin
  inherited Create(Prog, Left, Right);
  FEqual := Equal;
  FTyp := FProg.TypBoolean;
end;

function TObjCmpExpr.Eval: Variant;

  function SameInterface(const Left, Right: Variant) : Boolean;
  begin
    result := TVarData(Left).VUnknown = TVarData(Right).VUnknown;
    // if not result then compare external objects ?
  end;

begin
  if FEqual then
    Result := SameInterface(FLeft.Eval, FRight.Eval)
  else
    Result := not SameInterface(FLeft.Eval, FRight.Eval);
end;

// TypeCheckNoPos
//
procedure TObjCmpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FLeft.TypeCheckNoPos(aPos);
   FRight.TypeCheckNoPos(aPos);
   if not ((FLeft.Typ is TClassSymbol) or (FLeft.Typ = FProg.TypNil)) then
      Prog.Msgs.AddCompilerStop(aPos, CPE_ObjectExpected);
   if not ((FRight.Typ is TClassSymbol) or (FRight.Typ = FProg.TypNil)) then
      Prog.Msgs.AddCompilerStop(aPos, CPE_ObjectExpected);
end;

{ TNegExpr }

function TNegExpr.Eval: Variant;
begin
  Result := -FExpr.Eval;
end;

// TypeCheckNoPos
//
procedure TNegExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FExpr.TypeCheckNoPos(aPos);
   if FTyp=nil then begin
      Prog.Msgs.AddCompilerError(aPos, CPE_NumericalExpected);
      FTyp:=FProg.TypVariant;
   end;
end;

// ------------------
// ------------------ TNegVariantExpr ------------------
// ------------------

// EvalAsVariant
//
procedure TNegVariantExpr.EvalAsVariant(var Result : Variant);
begin
   Result:=-FExpr.Eval;
end;

// TypeCheckNoPos
//
procedure TNegVariantExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FTyp:=FProg.TypVariant;
   inherited;
end;

// ------------------
// ------------------ TNegIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TNegIntExpr.EvalAsInteger : Int64;
begin
   Result:=-FExpr.EvalAsInteger;
end;

// TypeCheckNoPos
//
procedure TNegIntExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FTyp:=FProg.TypInteger;
   inherited;
end;

// Optimize
//
function TNegIntExpr.Optimize : TNoPosExpr;
begin
   if FExpr.IsConstant then begin
      Result:=TConstIntExpr.CreateUnified(FProg, nil, -FExpr.EvalAsInteger);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TNegFloatExpr ------------------
// ------------------

// EvalAsFloat
//
procedure TNegFloatExpr.EvalAsFloat(var Result : Double);
begin
   FExpr.EvalAsFloat(Result);
   Result:=-Result;
end;

// TypeCheckNoPos
//
procedure TNegFloatExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FTyp:=FProg.TypFloat;
   inherited;
end;

// Optimize
//
function TNegFloatExpr.Optimize : TNoPosExpr;
var
   xf : Double;
begin
   if FExpr.IsConstant then begin
      FExpr.EvalAsFloat(xf);
      Result:=TConstFloatExpr.CreateUnified(FProg, nil, -xf);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TAddExpr ------------------
// ------------------

function TAddExpr.Eval : Variant;
begin
  Result := FLeft.Eval + FRight.Eval;
end;

// ------------------
// ------------------ TAddIntExpr ------------------
// ------------------

function TAddIntExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger + FRight.EvalAsInteger;
end;

// EvalAsFloat
//
procedure TAddIntExpr.EvalAsFloat(var Result : Double);
begin
   Result := FLeft.EvalAsInteger + FRight.EvalAsInteger;
end;

// ------------------
// ------------------ TAddStrExpr ------------------
// ------------------

// EvalAsString
//
procedure TAddStrExpr.EvalAsString(var Result : String);
var
   buf : String;
begin
   FLeft.EvalAsString(Result);
   FRight.EvalAsString(buf);
   Result:=Result+buf;
end;

// ------------------
// ------------------ TAddFloatExpr ------------------
// ------------------

procedure TAddFloatExpr.EvalAsFloat(var Result : Double);
var
   bufLeft, bufRight : Double;
begin
   FLeft.EvalAsFloat(bufLeft);
   FRight.EvalAsFloat(bufRight);
   Result:=bufLeft+bufRight;
end;

// ------------------
// ------------------ TSubExpr ------------------
// ------------------

function TSubExpr.Eval: Variant;
begin
  Result := FLeft.Eval - FRight.Eval;
end;

// ------------------
// ------------------ TSubIntExpr ------------------
// ------------------

function TSubIntExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger - FRight.EvalAsInteger;
end;

// EvalAsFloat
//
procedure TSubIntExpr.EvalAsFloat(var Result : Double);
begin
   Result := FLeft.EvalAsInteger - FRight.EvalAsInteger;
end;

// ------------------
// ------------------ TSubFloatExpr ------------------
// ------------------

procedure TSubFloatExpr.EvalAsFloat(var Result : Double);
var
   bufLeft, bufRight : Double;
begin
   FLeft.EvalAsFloat(bufLeft);
   FRight.EvalAsFloat(bufRight);
   Result:=bufLeft-bufRight;
end;

// ------------------
// ------------------ TMultExpr ------------------
// ------------------

function TMultExpr.Eval: Variant;
begin
  Result := FLeft.Eval * FRight.Eval;
end;

// ------------------
// ------------------ TMultIntExpr ------------------
// ------------------

function TMultIntExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger * FRight.EvalAsInteger;
end;

// EvalAsFloat
//
procedure TMultIntExpr.EvalAsFloat(var Result : Double);
begin
   Result := FLeft.EvalAsInteger * FRight.EvalAsInteger;
end;

// Optimize
//
function TMultIntExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
   if (FLeft is TVarExpr) and (FRight is TVarExpr) then begin
      if TVarExpr(FLeft).SameVarAs(TVarExpr(FRight)) then begin
         Result:=TSqrIntExpr.Create(Prog, FLeft);
         FLeft:=nil;
      end;
   end;
end;

// ------------------
// ------------------ TMultFloatExpr ------------------
// ------------------

procedure TMultFloatExpr.EvalAsFloat(var Result : Double);
var
   bufLeft, bufRight : Double;
begin
   FLeft.EvalAsFloat(bufLeft);
   FRight.EvalAsFloat(bufRight);
   Result:=bufLeft*bufRight;
end;

// Optimize
//
function TMultFloatExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
   if (FLeft is TFloatVarExpr) and (FRight is TFloatVarExpr) then begin
      if TFloatVarExpr(FLeft).SameVarAs(TFloatVarExpr(FRight)) then begin
         Result:=TSqrFloatVarExpr.Create(Prog, FLeft);
         FLeft:=nil;
         Free;
      end;
   end;
end;

// ------------------
// ------------------ TSqrIntExpr ------------------
// ------------------

// EvalAsInteger
//
function TSqrIntExpr.EvalAsInteger : Int64;
begin
   Result:=Sqr(FExpr.EvalAsInteger);
end;

// ------------------
// ------------------ TSqrFloatExpr ------------------
// ------------------

// EvalAsFloat
//
procedure TSqrFloatExpr.EvalAsFloat(var Result : Double);
begin
   FExpr.EvalAsFloat(Result);
   Result:=Sqr(Result);
end;

// ------------------
// ------------------ TSqrFloatVarExpr ------------------
// ------------------

// EvalAsFloat
//
procedure TSqrFloatVarExpr.EvalAsFloat(var Result : Double);
begin
   Result:=Sqr(TFloatVarExpr(FExpr).EvalAsPFloat^);
end;

{ TDivideExpr }

// EvalAsFloat
//
procedure TDivideExpr.EvalAsFloat(var Result : Double);
var
   bufLeft, bufRight : Double;
begin
   FLeft.EvalAsFloat(bufLeft);
   FRight.EvalAsFloat(bufRight);
   Result:=bufLeft/bufRight;
end;

{ TDivExpr }

function TDivExpr.EvalAsInteger : Int64;
begin
  Result := FLeft.EvalAsInteger div FRight.EvalAsInteger;
end;

{ TModExpr }

function TModExpr.EvalAsInteger : Int64;
begin
  Result := FLeft.EvalAsInteger mod FRight.EvalAsInteger;
end;

{ TNotExpr }

function TNotExpr.Eval: Variant;
begin
  Result := not FExpr.Eval;
end;

// TypeCheckNoPos
//
procedure TNotExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  FExpr.TypeCheckNoPos(aPos);
  if FExpr.IsVariantValue then
    FTyp := FProg.TypVariant
  else if FExpr.IsBooleanValue then
    FTyp := FProg.TypBoolean
  else if FExpr.IsIntegerValue then
    FTyp := FProg.TypInteger
  else Prog.Msgs.AddCompilerStop(aPos, CPE_BooleanOrIntegerExpected);
end;

{ TIntAndExpr }

function TIntAndExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger and FRight.EvalAsInteger
end;

{ TBoolAndExpr }

function TBoolAndExpr.EvalAsBoolean : Boolean;
begin
   Result := Left.EvalAsBoolean and Right.EvalAsBoolean;
end;

{ TIntOrExpr }

function TIntOrExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger or FRight.EvalAsInteger
end;

{ TBoolOrExpr }

function TBoolOrExpr.EvalAsBoolean : Boolean;
begin
   Result := Left.EvalAsBoolean or Right.EvalAsBoolean;
end;

{ TIntXorExpr }

function TIntXorExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger xor FRight.EvalAsInteger
end;

{ TBoolXorExpr }

function TBoolXorExpr.EvalAsBoolean : Boolean;
begin
   Result := FLeft.EvalAsBoolean xor FRight.EvalAsBoolean;
end;

{ TShlExpr }

function TShlExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger shl FRight.EvalAsInteger
end;

{ TShrExpr }

function TShrExpr.EvalAsInteger : Int64;
begin
   Result := FLeft.EvalAsInteger shr FRight.EvalAsInteger
end;

// ------------------
// ------------------ TVariantBinOpExpr ------------------
// ------------------

// Create
//
constructor TVariantBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited;
   FTyp:=FProg.TypVariant;
end;

// Optimize
//
function TVariantBinOpExpr.Optimize : TNoPosExpr;
begin
   if IsConstant then begin
      Result:=TUnifiedConstExpr.CreateUnified(FProg, Prog.TypVariant, Eval);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TIntegerBinOpExpr ------------------
// ------------------

// Create
//
constructor TIntegerBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited;
   FTyp:=FProg.TypInteger;
end;

// Eval
//
function TIntegerBinOpExpr.Eval: Variant;
begin
   Result:=EvalAsInteger;
end;

// EvalAsFloat
//
procedure TIntegerBinOpExpr.EvalAsFloat(var Result : Double);
begin
   Result:=EvalAsInteger;
end;

// TypeCheckNoPos
//
procedure TIntegerBinOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   if not (    (FLeft.IsVariantValue or FLeft.IsIntegerValue)
           and (FRight.IsVariantValue or FRight.IsIntegerValue)) then
      Prog.Msgs.AddCompilerError(aPos, CPE_InvalidOperands);
end;

// Optimize
//
function TIntegerBinOpExpr.Optimize : TNoPosExpr;
begin
   if IsConstant then begin
      Result:=TConstIntExpr.CreateUnified(FProg, nil, EvalAsInteger);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TStringBinOpExpr ------------------
// ------------------

// Create
//
constructor TStringBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited;
   FTyp:=FProg.TypString;
end;

// Eval
//
function TStringBinOpExpr.Eval: Variant;
var
   buf : String;
begin
   EvalAsString(buf);
   Result:=buf;
end;

// TypeCheckNoPos
//
procedure TStringBinOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   if not (    (FLeft.IsVariantValue or FLeft.IsStringValue)
           and (FRight.IsVariantValue or FRight.IsStringValue)) then
      Prog.Msgs.AddCompilerStop(aPos, CPE_InvalidOperands);
end;

// Optimize
//
function TStringBinOpExpr.Optimize : TNoPosExpr;
var
   buf : String;
begin
   if IsConstant then begin
      EvalAsString(buf);
      Result:=TConstStringExpr.CreateUnified(FProg, nil, buf);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TFloatBinOpExpr ------------------
// ------------------

// Create
//
constructor TFloatBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited;
   FTyp:=FProg.TypFloat;
end;

// Eval
//
function TFloatBinOpExpr.Eval: Variant;
var
   d : Double;
begin
   EvalAsFloat(d);
   Result:=d;
end;

// TypeCheckNoPos
//
procedure TFloatBinOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   if not (    (FLeft.IsVariantValue or FLeft.IsNumberValue)
           and (FRight.IsVariantValue or FRight.IsNumberValue)) then
      Prog.Msgs.AddCompilerStop(aPos, CPE_InvalidOperands);
end;

// Optimize
//
function TFloatBinOpExpr.Optimize : TNoPosExpr;
var
   xf : Double;
begin
   if IsConstant then begin
      EvalAsFloat(xf);
      Result:=TConstFloatExpr.CreateUnified(FProg, nil, xf);
      Free;
   end else begin
      FLeft:=FLeft.OptimizeIntegerConstantToFloatConstant;
      FRight:=FRight.OptimizeIntegerConstantToFloatConstant;
      Result:=Self;
   end;
end;

// ------------------
// ------------------ TBooleanBinOpExpr ------------------
// ------------------

// Create
//
constructor TBooleanBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited;
   FTyp:=FProg.TypBoolean;
end;

function TBooleanBinOpExpr.Eval: Variant;
begin
   Result:=EvalAsBoolean;
end;

// TypeCheckNoPos
//
procedure TBooleanBinOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   if not (    (FLeft.IsVariantValue or FLeft.IsBooleanValue)
           and (FRight.IsVariantValue or FRight.IsBooleanValue)) then
      Prog.Msgs.AddCompilerStop(aPos, CPE_InvalidOperands);
end;

// Optimize
//
function TBooleanBinOpExpr.Optimize : TNoPosExpr;
begin
   if IsConstant then begin
      Result:=TConstBooleanExpr.CreateUnified(FProg, nil, EvalAsBoolean);
      Free;
   end else Result:=Self;
end;

{ TNumberStringBinOpExpr }

// TypeCheckNoPos
//
procedure TNumberStringBinOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  FTyp:=DoTypeCheckNoPos(aPos, FProg, FLeft, FRight);
end;

// DoTypeCheckNoPos
//
class function TNumberStringBinOpExpr.DoTypeCheckNoPos(
         const aPos : TScriptPos; prog : TdwsProgram; left, right : TNoPosExpr) : TSymbol;
begin
   if left.IsVariantValue or right.IsVariantValue then
      Result := prog.TypVariant
   else if left.IsIntegerValue and right.IsIntegerValue then
      Result := prog.TypInteger
   else if left.IsNumberValue and right.IsNumberValue then
      Result := prog.TypFloat
   else if left.IsStringValue and right.IsStringValue then
      Result := prog.TypString
   else if left.IsBooleanValue and right.IsBooleanValue then
      Result := prog.TypBoolean
   else begin
      Result := prog.TypVariant;
      Prog.Msgs.AddCompilerStop(aPos, CPE_IncompatibleOperands);
   end;
end;

{ TAssignExpr }

constructor TAssignExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr);
begin
  inherited Create(Prog, Pos);
  FLeft := Left;
  FRight := Right;
end;

destructor TAssignExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

procedure TAssignExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignExpr(FRight);
end;

procedure TAssignExpr.Initialize;
begin
  FLeft.Initialize;
  FRight.Initialize;
end;

// TypeCheckNoPos
//
procedure TAssignExpr.TypeCheckNoPos(const aPos : TScriptPos);

   procedure ReportIncompatibleTypes;
   var
      cleft, cright: string;
   begin
      if FLeft.Typ = nil then
         cleft := SYS_VOID
      else cleft := FLeft.Typ.Caption;
      if FRight.Typ = nil then
         cright := SYS_VOID
      else cright := FRight.Typ.Caption;
      AddCompilerErrorFmt(CPE_AssignIncompatibleTypes, [cright, cleft]);
   end;

begin
   if (FRight.Typ = nil) or (FLeft.Typ = nil) then
      ReportIncompatibleTypes
   else begin
      if FRight.InheritsFrom(TArrayConstantExpr) then
         TArrayConstantExpr(FRight).Prepare(FLeft.Typ.Typ);

      FRight.TypeCheckNoPos(Pos);

      // Automatic conversion from int to float values
      if (FLeft.Typ = FProg.TypFloat) and (FRight.Typ = FProg.TypInteger) then
         FRight := TConvFloatExpr.Create(FProg, FRight);

      // Look if Types are compatible
      if not FLeft.Typ.IsCompatible(FRight.Typ) then
         ReportIncompatibleTypes;
   end;
end;

// Optimize
//
function TAssignExpr.Optimize : TNoPosExpr;
var
   leftVarExpr : TVarExpr;
   addIntExpr : TAddIntExpr;
   addStrExpr : TAddStrExpr;
   subIntExpr : TSubIntExpr;
begin
   if FRight.IsConstant then
      Exit(OptimizeConstAssignment);

   Result:=Self;
   if FLeft.InheritsFrom(TVarExpr) then begin
      leftVarExpr:=TVarExpr(FLeft);
      if leftVarExpr.ClassType=TIntVarExpr then begin
         if FRight.InheritsFrom(TAddIntExpr) then begin
            addIntExpr:=TAddIntExpr(FRight);
            if (addIntExpr.Left is TVarExpr) and (TVarExpr(addIntExpr.Left).SameVarAs(leftVarExpr)) then begin
               Result:=TIncIntVarExpr.Create(Prog, Pos, FLeft, addIntExpr.Right);
               FLeft:=nil;
               addIntExpr.Right:=nil;
               Free;
            end;
         end else if FRight.InheritsFrom(TSubIntExpr) then begin
            subIntExpr:=TSubIntExpr(FRight);
            if (subIntExpr.Left is TVarExpr) and (TVarExpr(subIntExpr.Left).SameVarAs(leftVarExpr)) then begin
               Result:=TDecIntVarExpr.Create(Prog, Pos, FLeft, subIntExpr.Right);
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
                  Result:=TAppendConstStringVarExpr.Create(Prog, Pos, FLeft, TConstStringExpr(addStrExpr.Right));
               end else begin
                  Result:=TAppendStringVarExpr.Create(Prog, Pos, FLeft, addStrExpr.Right);
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
function TAssignExpr.OptimizeConstAssignment : TNoPosExpr;
var
   floatBuf : Double;
   stringBuf : String;
begin
   Result:=Self;
   if FRight.IsIntegerValue then begin
      Result:=TAssignConstToIntegerVarExpr.CreateVal(Prog, Pos, FLeft, FRight.EvalAsInteger);
   end else if FRight.IsFloatValue then begin
      FRight.EvalAsFloat(floatBuf);
      Result:=TAssignConstToFloatVarExpr.CreateVal(Prog, Pos, FLeft, floatBuf);
   end else if FRight.IsStringValue then begin
      FRight.EvalAsString(stringBuf);
      Result:=TAssignConstToStringVarExpr.CreateVal(Prog, Pos, FLeft, stringBuf);
   end;
   if Result<>Self then begin
      FLeft:=nil;
      Free;
   end;
end;

// ------------------
// ------------------ TAssignClassOfExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAssignClassOfExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   v : Variant;
   obj : IScriptObj;
begin
   FRight.EvalAsVariant(v);
   if VarIsStr(v) then
      FLeft.AssignValue(v)
   else begin
      obj:=IScriptObj(IUnknown(v));
      if obj<>nil then
         FLeft.AssignValueAsString(IScriptObj(IUnknown(v)).ClassSym.Name)
      else FLeft.AssignValueAsString('');
   end;
end;

{ TAssignDataExpr }

constructor TAssignDataExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr);
begin
  inherited Create(Prog, Pos, Left, Right);
  FSize := FLeft.Typ.Size;
end;

procedure TAssignDataExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignDataExpr(TDataExpr(FRight));
end;

{ TAssignArrayConstantExpr }

constructor TAssignArrayConstantExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                                            Left : TDataExpr; Right: TNoPosExpr);
begin
  inherited Create(Prog, Pos, Left, Right as TArrayConstantExpr); // typecheck Right
end;

procedure TAssignArrayConstantExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignData(TArrayConstantExpr(FRight).EvalAsTData, 0);
end;

{ TAssignConstDataToVarExpr }

constructor TAssignConstDataToVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
                                             Left : TDataExpr; Right: TNoPosExpr);
begin
   inherited Create(Prog, Pos, Left, Right);
   Assert(Left is TVarExpr);
   if Right=nil then
      Assert(ClassType<>TAssignConstDataToVarExpr)
   else Assert(Right is TConstExpr);
end;

procedure TAssignConstDataToVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TVarExpr(FLeft).AssignData(TConstExpr(FRight).FData, TConstExpr(FRight).Addr);
end;

// ------------------
// ------------------ TAssignConstExpr ------------------
// ------------------

// TypeCheckNoPos
//
procedure TAssignConstExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   // nothing, checked during optimize
end;

// Initialize
//
procedure TAssignConstExpr.Initialize;
begin
   FLeft.Initialize;
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
procedure TAssignConstToIntegerVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TVarExpr(FLeft).AssignValueAsInteger(FRight);
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
procedure TAssignConstToFloatVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TVarExpr(FLeft).AssignValueAsFloat(FRight);
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
procedure TAssignConstToStringVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TVarExpr(FLeft).AssignValueAsString(FRight);
end;

// ------------------
// ------------------ TOpAssignExpr ------------------
// ------------------

// Optimize
//
function TOpAssignExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TPlusAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignValue(FLeft.Eval + FRight.Eval);
end;

// ------------------
// ------------------ TPlusAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignIntExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignValueAsInteger(FLeft.EvalAsInteger + FRight.EvalAsInteger);
end;

// Optimize
//
function TPlusAssignIntExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      Result:=TIncIntVarExpr.Create(FProg, Pos, FLeft, FRight);
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
procedure TPlusAssignFloatExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   v1, v2 : Double;
begin
   FLeft.EvalAsFloat(v1);
   FRight.EvalAsFloat(v2);
   v1:=v1+v2;
   FLeft.AssignValueAsFloat(v1);
end;

// ------------------
// ------------------ TPlusAssignStrExpr ------------------
// ------------------

// EvalNoResult
//
procedure TPlusAssignStrExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   v1, v2 : String;
begin
   FLeft.EvalAsString(v1);
   FRight.EvalAsString(v2);
   FLeft.AssignValueAsString(v1+v2);
end;

// Optimize
//
function TPlusAssignStrExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
   if FLeft is TStrVarExpr then begin
      Result:=TAppendStringVarExpr.Create(FProg, Pos, FLeft, FRight);
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
procedure TMinusAssignExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignValue(FLeft.Eval - FRight.Eval);
end;

// ------------------
// ------------------ TMinusAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMinusAssignIntExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignValueAsInteger(FLeft.EvalAsInteger - FRight.EvalAsInteger);
end;

// Optimize
//
function TMinusAssignIntExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      Result:=TDecIntVarExpr.Create(FProg, Pos, FLeft, FRight);
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
procedure TMinusAssignFloatExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   v1, v2 : Double;
begin
   FLeft.EvalAsFloat(v1);
   FRight.EvalAsFloat(v2);
   v1:=v1-v2;
   FLeft.AssignValueAsFloat(v1);
end;

// ------------------
// ------------------ TMultAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignValue(FLeft.Eval * FRight.Eval);
end;

// ------------------
// ------------------ TMultAssignIntExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignIntExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FLeft.AssignValueAsInteger(FLeft.EvalAsInteger * FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TMultAssignFloatExpr ------------------
// ------------------

// EvalNoResult
//
procedure TMultAssignFloatExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   v1, v2 : Double;
begin
   FLeft.EvalAsFloat(v1);
   FRight.EvalAsFloat(v2);
   v1:=v1*v2;
   FLeft.AssignValueAsFloat(v1);
end;

// ------------------
// ------------------ TDivideAssignExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDivideAssignExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   v1, v2 : Double;
begin
   FLeft.EvalAsFloat(v1);
   FRight.EvalAsFloat(v2);
   v1:=v1/v2;
   FLeft.AssignValueAsFloat(v1);
end;

// ------------------
// ------------------ TIncIntVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TIncIntVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TIntVarExpr(FLeft).IncValue(FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TDecIntVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TDecIntVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TIntVarExpr(FLeft).IncValue(-FRight.EvalAsInteger);
end;

// ------------------
// ------------------ TAppendStringVarExpr ------------------
// ------------------

// EvalNoResult
//
procedure TAppendStringVarExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   buf : String;
begin
   FRight.EvalAsString(buf);
   TStrVarExpr(FLeft).Append(buf);
end;

// ------------------
// ------------------ TAppendConstStringVarExpr ------------------
// ------------------

// Create
//
constructor TAppendConstStringVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr);
begin
   inherited Create(Prog, Pos, Left, Right);
   Right.EvalAsString(FAppendString);
end;

// EvalNoResult
//
procedure TAppendConstStringVarExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   TStrVarExpr(FLeft).Append(FAppendString);
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
procedure TBlockExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   oldTable : TSymbolTable;
   expr : TExpr;
begin
   oldTable:=FProg.Table;
   try
      FProg.Table:=FTable;
      for i:=0 to FCount-1 do begin
         expr:=FStatements[i];
         FProg.DoStep(expr);
         expr.EvalNoResult(status);
         if status<>esrNone then Break;
      end;
   finally
      FProg.Table:=oldTable;
   end;
end;

// Optimize
//
function TBlockExpr.Optimize : TNoPosExpr;
begin
   if FTable.Count=0 then begin
      case FCount of
         0 : Result:=TNullExpr.Create(Prog, Pos);
         1 : begin
            Result:=FStatements[0];
            FreeMem(FStatements);
         end;
      else
         Result:=TBlockExprNoTable.Create(Prog, Pos);
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
procedure TBlockExprNoTable.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   iterator : PExpr;
begin
   iterator:=PExpr(FStatements);
   for i:=1 to FCount do begin
      FProg.DoStep(iterator^);
      iterator^.EvalNoResult(status);
      if status<>esrNone then Break;
      Inc(iterator);
   end;
end;

// ------------------
// ------------------ TIfExpr ------------------
// ------------------

destructor TIfExpr.Destroy;
begin
  FCond.Free;
  FThen.Free;
  FElse.Free;
  inherited;
end;

procedure TIfExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FProg.DoStep(Self);
   if FCond.EvalAsBoolean then begin
      FProg.DoStep(FThen);
      FThen.EvalNoResult(status);
   end else if Assigned(FElse) then begin
      FProg.DoStep(FElse);
      FElse.EvalNoResult(status);
   end;
end;

procedure TIfExpr.Initialize;
begin
   FCond.Initialize;
   FThen.Initialize;
   if Assigned(FElse) then
      FElse.Initialize;
end;

// TypeCheckNoPos
//
procedure TIfExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FCond.TypeCheckNoPos(Pos);
   if not (FCond.IsBooleanValue or FCond.IsVariantValue) then
      AddCompilerStop(CPE_BooleanExpected);
end;

{ TCaseExpr }

destructor TCaseExpr.Destroy;
begin
  FCaseConditions.Clean;
  FValueExpr.Free;
  FElseExpr.Free;
  inherited;
end;

procedure TCaseExpr.EvalNoResult(var status : TExecutionStatusResult);
var
  x: Integer;
  Value: Variant;
  cc : TCaseCondition;
begin
   FProg.DoStep(Self);

   Value := FValueExpr.Eval;
   for x := 0 to FCaseConditions.Count - 1 do begin
      cc:=TCaseCondition(FCaseConditions.List[x]);
      if cc.IsTrue(Value) then begin
         FProg.DoStep(cc.TrueExpr);
         cc.TrueExpr.EvalNoResult(status);
         Exit;
      end;
   end;

   if Assigned(FElseExpr) then begin
      FProg.DoStep(FElseExpr);
      FElseExpr.EvalNoResult(status);
   end;
end;

procedure TCaseExpr.Initialize;
var
  x: Integer;
begin
  FValueExpr.Initialize;
  for x := 0 to FCaseConditions.Count - 1 do
    TCaseCondition(FCaseConditions.List[x]).Initialize;
  if Assigned(FElseExpr) then
    FElseExpr.Initialize;
end;

// TypeCheckNoPos
//
procedure TCaseExpr.TypeCheckNoPos(const aPos : TScriptPos);
var
  x: Integer;
begin
  for x := 0 to FCaseConditions.Count - 1 do
    TCaseCondition(FCaseConditions.List[x]).TypeCheck(FValueExpr.Typ);
end;

// AddCaseCondition
//
procedure TCaseExpr.AddCaseCondition(cond : TCaseCondition);
begin
   FCaseConditions.Add(cond);
end;

{ TCaseCondition }

constructor TCaseCondition.Create(const aPos : TScriptPos; ValueExpr: TNoPosExpr);
begin
   FPos:=aPos;
   FValueExpr := ValueExpr;
end;

destructor TCaseCondition.Destroy;
begin
  if FOwnsTrueExpr then
    FTrueExpr.Free;
  inherited;
end;

procedure TCaseCondition.Initialize;
begin
   if FTrueExpr<>nil then
      FTrueExpr.Initialize;
end;

{ TCompareCaseCondition }

constructor TCompareCaseCondition.Create(const aPos : TScriptPos;ValueExpr, CompareExpr: TNoPosExpr);
begin
  inherited Create(aPos, ValueExpr);
  FCompareExpr := CompareExpr;
end;

destructor TCompareCaseCondition.Destroy;
begin
  FCompareExpr.Free;
  inherited;
end;

procedure TCompareCaseCondition.Initialize;
begin
  inherited;
  FCompareExpr.Initialize;
end;

function TCompareCaseCondition.IsTrue(const Value: Variant): Boolean;
begin
  Result := FCompareExpr.Eval = Value;
end;

procedure TCompareCaseCondition.TypeCheck(Typ: TSymbol);
begin
  if FValueExpr.IsFloatValue then
    if FCompareExpr.IsIntegerValue then
      FCompareExpr := TConvFloatExpr.Create(FValueExpr.Prog, FCompareExpr);

  if not FCompareExpr.Typ.IsCompatible(FValueExpr.Typ) then
    FCompareExpr.Prog.Msgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                               [FValueExpr.Typ.Caption, FCompareExpr.Typ.Caption]);
end;

// IsConstant
//
function TCompareCaseCondition.IsConstant : Boolean;
begin
   Result:=FCompareExpr.IsConstant;
end;

{ TRangeCaseCondition }

constructor TRangeCaseCondition.Create(const aPos : TScriptPos; ValueExpr, FromExpr, ToExpr: TNoPosExpr);
begin
  inherited Create(aPos, ValueExpr);
  FFromExpr := FromExpr;
  FToExpr := ToExpr;
end;

destructor TRangeCaseCondition.Destroy;
begin
  FFromExpr.Free;
  FToExpr.Free;
  inherited;
end;

procedure TRangeCaseCondition.Initialize;
begin
  inherited;
  FFromExpr.Initialize;
  FToExpr.Initialize;
end;

function TRangeCaseCondition.IsTrue(const Value: Variant): Boolean;
begin
  Result := (Value >= FFromExpr.Eval) and (Value <= FToExpr.Eval);
end;

procedure TRangeCaseCondition.TypeCheck(Typ: TSymbol);
var
  Prog: TdwsProgram;
begin
  Prog := FValueExpr.Prog;

  if FValueExpr.IsFloatValue then
  begin
    // Convert integers to float if necessary
    if FFromExpr.IsIntegerValue then
      FFromExpr := TConvFloatExpr.Create(Prog, FFromExpr);

    if FToExpr.IsIntegerValue then
      FToExpr := TConvFloatExpr.Create(Prog, FToExpr);
  end;

  if not FFromExpr.Typ.IsCompatible(FToExpr.Typ) then
    FFromExpr.Prog.Msgs.AddCompilerErrorFmt(Pos, CPE_RangeIncompatibleTypes,
                                            [FFromExpr.Typ.Caption, FToExpr.Typ.Caption]);

  if not FValueExpr.Typ.IsCompatible(FFromExpr.Typ) then
    FFromExpr.Prog.Msgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                            [FValueExpr.Typ.Caption, FFromExpr.Typ.Caption]);
end;

// IsConstant
//
function TRangeCaseCondition.IsConstant : Boolean;
begin
   Result:=FFromExpr.IsConstant and FToExpr.IsConstant;
end;

{ TForExpr }

destructor TForExpr.Destroy;
begin
  FDoExpr.Free;
  FFromExpr.Free;
  FToExpr.Free;
  FVarExpr.Free;
  inherited;
end;

procedure TForExpr.Initialize;
begin
  inherited;
  FFromExpr.Initialize;
  FToExpr.Initialize;
  FDoExpr.Initialize;
end;

// TypeCheckNoPos
//
procedure TForExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   // checked in compiler
end;

{ TForStepExpr }

destructor TForStepExpr.Destroy;
begin
   FStepExpr.Free;
   inherited;
end;

procedure TForStepExpr.Initialize;
begin
   inherited;
   FStepExpr.Initialize;
end;

// EvalStep
//
function TForStepExpr.EvalStep : Int64;
begin
   Result:=FStepExpr.EvalAsInteger;
   if Result<=0 then
      raise EScriptError.CreatePosFmt(Pos, RTE_ForLoopStepShouldBeStrictlyPositive, [Result])
end;

{ TForUpwardExpr }

procedure TForUpwardExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   toValue: Int64;
   i : PInt64;
begin
   status:=esrNone;
   i:=FVarExpr.EvalAsPInteger;
   i^:=FFromExpr.EvalAsInteger;
   toValue:=FToExpr.EvalAsInteger;
   while i^<=toValue do begin
      FProg.DoStep(Self);
      FDoExpr.EvalNoResult(status);
      if status<>esrNone then begin
         case status of
            esrBreak : begin
               status:=esrNone;
               break;
            end;
            esrContinue : status:=esrNone;
            esrExit : Exit;
         end;
      end;
      Inc(i^);
   end;
end;

{ TForUpwardExpr }

procedure TForDownwardExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   toValue: Int64;
   i : PInt64;
begin
   status:=esrNone;
   i:=FVarExpr.EvalAsPInteger;
   i^:=FFromExpr.EvalAsInteger;
   toValue:=FToExpr.EvalAsInteger;
   while i^>=toValue do begin
      FProg.DoStep(Self);
      FDoExpr.EvalNoResult(status);
      if status<>esrNone then begin
         case status of
            esrBreak : begin
               status:=esrNone;
               break;
            end;
            esrContinue : status:=esrNone;
            esrExit : Exit;
         end;
      end;
      Dec(i^);
   end;
end;

{ TForUpwardStepExpr }

procedure TForUpwardStepExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   step, toValue: Int64;
   i : PInt64;
begin
   status:=esrNone;
   i:=FVarExpr.EvalAsPInteger;
   i^:=FFromExpr.EvalAsInteger;
   toValue:=FToExpr.EvalAsInteger;
   step:=EvalStep;
   while i^<=toValue do begin
      FProg.DoStep(Self);
      FDoExpr.EvalNoResult(status);
      if status<>esrNone then begin
         case status of
            esrBreak : begin
               status:=esrNone;
               break;
            end;
            esrContinue : status:=esrNone;
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

{ TForDownwardStepExpr }

procedure TForDownwardStepExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   step, toValue: Int64;
   i : PInt64;
begin
   status:=esrNone;
   i:=FVarExpr.EvalAsPInteger;
   i^:=FFromExpr.EvalAsInteger;
   toValue:=FToExpr.EvalAsInteger;
   step:=EvalStep;
   while i^>=toValue do begin
      FProg.DoStep(Self);
      FDoExpr.EvalNoResult(status);
      if status<>esrNone then begin
         case status of
            esrBreak : begin
               status:=esrNone;
               break;
            end;
            esrContinue : status:=esrNone;
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

{ TLoopExpr }

destructor TLoopExpr.Destroy;
begin
  FCondExpr.Free;
  FLoopExpr.Free;
  inherited;
end;

procedure TLoopExpr.Initialize;
begin
  FCondExpr.Initialize;
  FLoopExpr.Initialize;
end;

// TypeCheckNoPos
//
procedure TLoopExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FCondExpr.TypeCheckNoPos(Pos);
   if not (FCondExpr.IsBooleanValue or FCondExpr.IsVariantValue) then
      AddCompilerStop(CPE_BooleanExpected);
end;

{ TWhileExpr }

procedure TWhileExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   status:=esrNone;
   while FCondExpr.EvalAsBoolean do begin
      FProg.DoStep(Self);
      FLoopExpr.EvalNoResult(status);
      if status<>esrNone then begin
         case status of
            esrBreak : begin
               status:=esrNone;
               Break;
            end;
            esrContinue : status:=esrNone;
            esrExit : Exit;
         end;
      end;
   end;
end;

// TypeCheckNoPos
//
procedure TWhileExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if (FCondExpr.IsConstant) and (FLoopExpr is TNullExpr) and (FCondExpr.EvalAsBoolean) then
      AddCompilerWarning(CPW_InfiniteLoop);
end;

{ TRepeatExpr }

procedure TRepeatExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   status:=esrNone;
   repeat
      FLoopExpr.EvalNoResult(status);
      if status<>esrNone then begin
         case status of
            esrBreak : begin
               status:=esrNone;
               Break;
            end;
            esrContinue : status:=esrNone;
            esrExit : Exit;
         end;
      end;
      FProg.DoStep(Self);
   until FCondExpr.EvalAsBoolean;
end;

// TypeCheckNoPos
//
procedure TRepeatExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if (FCondExpr.IsConstant) and (FLoopExpr is TNullExpr) and (not FCondExpr.EvalAsBoolean) then
      AddCompilerWarning(CPW_InfiniteLoop);
end;

{ TBreakExpr }

procedure TBreakExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   status:=esrBreak;
end;

{ TExitExpr }

procedure TExitExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   status:=esrExit;
end;

// ------------------
// ------------------ TExitValueExpr ------------------
// ------------------

// Create
//
constructor TExitValueExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; assignExpr : TNoResultExpr);
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

// Initialize
//
procedure TExitValueExpr.Initialize;
begin
   inherited;
   FAssignExpr.Initialize;
end;

// EvalNoResult
//
procedure TExitValueExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   FAssignExpr.EvalNoResult(status);
   status:=esrExit;
end;

{ TContinueExpr }

procedure TContinueExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   status:=esrContinue;
end;

{ TExceptionExpr }

destructor TExceptionExpr.Destroy;
begin
  FTryExpr.Free;
  FHandlerExpr.Free;
  inherited;
end;

procedure TExceptionExpr.Initialize;
begin
  FTryExpr.Initialize;
  if Assigned(FHandlerExpr) then
    FHandlerExpr.Initialize;
end;

// TypeCheckNoPos
//
procedure TExceptionExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FTryExpr.TypeCheck;
   FHandlerExpr.TypeCheck;
end;

{ TExceptExpr }

destructor TExceptExpr.Destroy;
begin
  inherited;
  FDoExprs.Clean;
  FElseExpr.Free;
end;

procedure TExceptExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   x: Integer;
   obj: Variant;
   objSym: TSymbol;
   doExpr: TExceptDoExpr;
   isCatched: Boolean;
   isReraise: Boolean;
   stack : TStack;
begin
   try
      FProg.DoStep(FTryExpr);
      FTryExpr.EvalNoResult(status);
   except
      on e: Exception do begin
         if e is EScriptException then begin
            // a raise-statement created an Exception object
            obj := EScriptException(e).Value;
            objSym := EScriptException(e).Typ;
         end else begin
            // A Delphi exception. Transform it to a EDelphi-dws exception
            obj := CreateEDelphiObj(e.ClassName, e.Message);
            objSym := IScriptObj(IUnknown(Obj)).ClassSym;
         end;

         // script exceptions
         if FDoExprs.Count > 0 then begin

            isCatched := False;

            for x := 0 to FDoExprs.Count - 1 do begin
               // Find a "on x: Class do ..." statement matching to this exception class
               doExpr := TExceptDoExpr(FDoExprs.List[x]);
               if doExpr.ExceptionVar.Typ.IsCompatible(objSym) then begin
                  stack:=FProg.Stack;
                  stack.Data[stack.BasePointer +  doExpr.FExceptionVar.StackAddr] := obj;
                  isReraise := False;
                  try
                     FProg.DoStep(doExpr);
                     doExpr.EvalNoResult(status);
                  except
                     on E : EReraise do isReraise := True;
                  end;
                  if isReraise then raise;
                  stack:=FProg.Stack;
                  VarClear(stack.Data[stack.BasePointer + doExpr.FExceptionVar.StackAddr]);
                  isCatched := True;
                  Break;
               end;
            end;

            if not isCatched and Assigned(FElseExpr) then begin
               isReraise := False;
               try
                  FProg.DoStep(FElseExpr);
                  FElseExpr.EvalNoResult(status);
               except
                  on E : EReraise do isReraise := True;
               end;
               if isReraise then raise;
            end;
         end else begin
            isReraise := False;
            try
               FProg.DoStep(FHandlerExpr);
               FHandlerExpr.EvalNoResult(status);
            except
               on E : EReraise do isReraise := True;
            end;
            if isReraise then raise;
         end;
      end;
   end;
   FProg.Msgs.SetLastScriptError(cNullPos);
end;

procedure TExceptExpr.Initialize;
var
   i : Integer;
begin
   inherited;
   for i:=0 to FDoExprs.Count-1 do
      TExceptDoExpr(FDoExprs.List[i]).Initialize;
   if Assigned(FElseExpr) then
      FElseExpr.Initialize;
end;

// TypeCheckNoPos
//
procedure TExceptExpr.TypeCheckNoPos(const aPos : TScriptPos);
var
   i : Integer;
begin
   for i:=0 to FDoExprs.Count-1 do
      TExceptDoExpr(FDoExprs.List[i]).TypeCheck;
   if ElseExpr<>nil then
      ElseExpr.TypeCheck;
end;

// AddDoExpr
//
procedure TExceptExpr.AddDoExpr(expr : TExceptDoExpr);
begin
   FDoExprs.Add(expr);
end;

{ TFinallyExpr }

procedure TFinallyExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
  try
    FProg.DoStep(FTryExpr);
    FTryExpr.EvalNoResult(status);
  finally
    FProg.DoStep(FHandlerExpr);
    FHandlerExpr.EvalNoResult(status);
  end;
end;

{ TRaiseExpr }

constructor TRaiseExpr.Create;
begin
  inherited Create(Prog, Pos);
  FExceptionExpr := ExceptionExpr;
end;

destructor TRaiseExpr.Destroy;
begin
  FExceptionExpr.Free;
  inherited;
end;

procedure TRaiseExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   exceptVal : Variant;
   exceptMessage : String;
begin
  FProg.DoStep(Self);
  exceptVal:=FExceptionExpr.Eval;
  exceptMessage:=VarToStr(IScriptObj(IUnknown(exceptVal)).GetData[0]);
  if exceptMessage<>'' then
     raise EScriptException.Create(Format(RTE_UserDefinedException_Msg, [exceptMessage]),
                                   exceptVal, FExceptionExpr.Typ, FPos)
  else raise EScriptException.Create(RTE_UserDefinedException,
                                     exceptVal, FExceptionExpr.Typ, FPos);
end;

procedure TRaiseExpr.Initialize;
begin
  FExceptionExpr.Initialize;
end;

// TypeCheckNoPos
//
procedure TRaiseExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   FExceptionExpr.TypeCheckNoPos(Pos);
end;

{ TReraiseExpr }

procedure TReraiseExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
  FProg.DoStep(Self);
  raise EReraise.Create('');
end;

{ TExceptDoExpr }

destructor TExceptDoExpr.Destroy;
begin
  FDoBlockExpr.Free;
  FExceptionVar.Free;
  inherited;
end;

procedure TExceptDoExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   DoBlockExpr.EvalNoResult(status);
end;

procedure TExceptDoExpr.Initialize;
begin
  FDoBlockExpr.Initialize;
  FExceptionVar.Initialize(Prog.Msgs);
end;

// TypeCheckNoPos
//
procedure TExceptDoExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FDoBlockExpr.TypeCheck;
end;

{ TStringArraySetExpr }

constructor TStringArraySetExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  StringExpr : TDataExpr; IndexExpr, ValueExpr: TNoPosExpr);
begin
  inherited Create(Prog,Pos);
  FStringExpr := StringExpr;
  FIndexExpr := IndexExpr;
  FValueExpr := ValueExpr;
end;

destructor TStringArraySetExpr.Destroy;
begin
  FStringExpr.Free;
  FIndexExpr.Free;
  FValueExpr.Free;
  inherited;
end;

procedure TStringArraySetExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   s, buf : String;
begin
   FStringExpr.EvalAsString(s);
   i:=FIndexExpr.EvalAsInteger;
   if i>Length(s) then
      raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayUpperBoundExceeded, [i])
   else if i<1 then
      raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayLowerBoundExceeded, [i]);
   FValueExpr.EvalAsString(buf);
   s[i]:=buf[1];
   FStringExpr.AssignValue(s);
end;

procedure TStringArraySetExpr.Initialize;
begin
  FStringExpr.Initialize;
  FIndexExpr.Initialize;
  FValueExpr.Initialize;
end;

{ TVarStringArraySetExpr }

procedure TVarStringArraySetExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   c : Char;
   buf : String;
begin
   i:=FIndexExpr.EvalAsInteger;
   if i<1 then
      raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayLowerBoundExceeded, [i])
   else begin
      FValueExpr.EvalAsString(buf);
      c:=buf[1];
      if not TStrVarExpr(FStringExpr).SetChar(i, c) then
         raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayUpperBoundExceeded, [i]);
   end;
end;

{ TVarStringArraySetChrExpr }

// EvalNoResult
//
procedure TVarStringArraySetChrExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   c : Char;
begin
   i:=FIndexExpr.EvalAsInteger;
   if i<1 then
      raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayLowerBoundExceeded, [i])
   else begin
      c:=Chr(FValueExpr.EvalAsInteger);
      if not TStrVarExpr(FStringExpr).SetChar(i, c) then
         raise EScriptOutOfBounds.CreatePosFmt(FPos, RTE_ArrayUpperBoundExceeded, [i]);
   end;
end;

// ------------------
// ------------------ TSpecialUnaryBoolExpr ------------------
// ------------------

// Create
//
constructor TSpecialUnaryBoolExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
   inherited;
   FTyp:=Prog.TypBoolean;
end;

// IsConstant
//
function TSpecialUnaryBoolExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// Eval
//
function TSpecialUnaryBoolExpr.Eval: Variant;
begin
   Result:=EvalAsBoolean;
end;

// ------------------
// ------------------ TDefinedExpr ------------------
// ------------------

// EvalAsBoolean
//
function TDefinedExpr.EvalAsBoolean : Boolean;
var
   name : String;
begin
   Expr.EvalAsString(name);
   Result:=(FProg.ConditionalDefines.IndexOf(name)>=0);
end;

// ------------------
// ------------------ TDeclaredExpr ------------------
// ------------------

// EvalAsBoolean
//
function TDeclaredExpr.EvalAsBoolean: Boolean;
var
   name : String;
begin
   Expr.EvalAsString(name);
   Result:=(FindSymbol(FProg.Root.Table, name)<>nil);
end;

// FindSymbol
//
class function TDeclaredExpr.FindSymbol(symbolTable : TSymbolTable; const name : String) : TSymbol;
var
   p : Integer;
   identifier : String;
begin
   p:=Pos('.', name);
   if p<=0 then
      Result:=symbolTable.FindSymbol(name)
   else begin
      Result:=symbolTable.FindSymbol(Copy(name, 1, p-1));
      if Result=nil then Exit;
      identifier:=Copy(name, p+1, MaxInt);
      if Result.InheritsFrom(TUnitSymbol) then
         Result:=FindSymbol(TUnitSymbol(Result).Table, identifier)
      else if Result.InheritsFrom(TClassSymbol) then
         Result:=FindSymbol(TClassSymbol(Result).Members, identifier)
      else if Result.InheritsFrom(TRecordSymbol) then
         Result:=FindSymbol(TRecordSymbol(Result).Members, identifier)
      else Result:=nil;
   end;
end;

// ------------------
// ------------------ TBinaryOperators ------------------
// ------------------

// Create
//
constructor TBinaryOperators.Create(table : TSymbolTable);
var
   typInteger : TTypeSymbol;
   typFloat : TTypeSymbol;
   typBoolean : TTypeSymbol;
   typString : TTypeSymbol;
   typVariant : TTypeSymbol;
   typClassOf : TTypeSymbol;

   procedure RegisterRelOp(aToken : TTokenType; intExpr, floatExpr, strExpr, varExpr : TNoPosExprClass);
   begin
      RegisterOperator(aToken,   intExpr,    typInteger, typInteger);
      RegisterOperator(aToken,   floatExpr,  typFloat,   typFloat);
      RegisterOperator(aToken,   floatExpr,  typFloat,   typInteger);
      RegisterOperator(aToken,   floatExpr,  typInteger, typFloat);
      RegisterOperator(aToken,   strExpr,    typString,  typString);
      RegisterOperator(aToken,   strExpr,    typString,  typVariant);
      RegisterOperator(aToken,   strExpr,    typVariant, typString);
      RegisterOperator(aToken,   varExpr,    typVariant, typVariant);
      RegisterOperator(aToken,   varExpr,    typFloat,   typVariant);
      RegisterOperator(aToken,   varExpr,    typInteger, typVariant);
      RegisterOperator(aToken,   varExpr,    typVariant, typInteger);
      RegisterOperator(aToken,   varExpr,    typVariant, typFloat);
   end;

begin
   typInteger:=table.FindSymbol(SYS_INTEGER) as TTypeSymbol;
   typFloat:=table.FindSymbol(SYS_FLOAT) as TTypeSymbol;
   typBoolean:=table.FindSymbol(SYS_BOOLEAN) as TTypeSymbol;
   typString:=table.FindSymbol(SYS_STRING) as TTypeSymbol;
   typVariant:=table.FindSymbol(SYS_VARIANT) as TTypeSymbol;
   typClassOf:=table.FindSymbol(SYS_TCLASS) as TTypeSymbol;

   // computation operators

   RegisterOperator(ttPLUS,   TAddIntExpr,      typInteger,  typInteger);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typString,   typString);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typString,   typVariant);
   RegisterOperator(ttPLUS,   TAddStrExpr,      typVariant,  typString);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttPLUS,   TAddFloatExpr,    typVariant,  typFloat);
   RegisterOperator(ttPLUS,   TAddExpr,         typInteger,  typVariant);
   RegisterOperator(ttPLUS,   TAddExpr,         typVariant,  typInteger);
   RegisterOperator(ttPLUS,   TAddExpr,         typVariant,  typVariant);

   RegisterOperator(ttMINUS,  TSubIntExpr,      typInteger,  typInteger);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typInteger,  typFloat);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typInteger);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typFloat);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typFloat,    typVariant);
   RegisterOperator(ttMINUS,  TSubFloatExpr,    typVariant,  typFloat);
   RegisterOperator(ttMINUS,  TSubExpr,         typInteger,  typVariant);
   RegisterOperator(ttMINUS,  TSubExpr,         typVariant,  typInteger);
   RegisterOperator(ttMINUS,  TSubExpr,         typVariant,  typVariant);

   RegisterOperator(ttTIMES,  TMultIntExpr,     typInteger,  typInteger);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typInteger,  typFloat);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typInteger);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typFloat);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typFloat,    typVariant);
   RegisterOperator(ttTIMES,  TMultFloatExpr,   typVariant,  typFloat);
   RegisterOperator(ttTIMES,  TMultExpr,        typInteger,  typVariant);
   RegisterOperator(ttTIMES,  TMultExpr,        typVariant,  typInteger);
   RegisterOperator(ttTIMES,  TMultExpr,        typVariant,  typVariant);

   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typInteger,  typVariant);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typFloat,    typVariant);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typInteger);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typFloat);
   RegisterOperator(ttDIVIDE, TDivideExpr,      typVariant,  typVariant);

   RegisterOperator(ttDIV,    TDivExpr,         typInteger,  typInteger);
   RegisterOperator(ttDIV,    TDivExpr,         typInteger,  typVariant);
   RegisterOperator(ttDIV,    TDivExpr,         typVariant,  typInteger);
   RegisterOperator(ttDIV,    TDivExpr,         typVariant,  typVariant);

   RegisterOperator(ttMOD,    TModExpr,         typInteger,  typInteger);
   RegisterOperator(ttMOD,    TModExpr,         typInteger,  typVariant);
   RegisterOperator(ttMOD,    TModExpr,         typVariant,  typInteger);
   RegisterOperator(ttMOD,    TModExpr,         typVariant,  typVariant);

   RegisterOperator(ttOR,     TBoolOrExpr,      typBoolean,  typBoolean);
   RegisterOperator(ttOR,     TBoolOrExpr,      typBoolean,  typVariant);
   RegisterOperator(ttOR,     TBoolOrExpr,      typVariant,  typBoolean);
   RegisterOperator(ttOR,     TIntOrExpr,       typInteger,  typInteger);
   RegisterOperator(ttOR,     TIntOrExpr,       typInteger,  typVariant);
   RegisterOperator(ttOR,     TIntOrExpr,       typVariant,  typInteger);
   RegisterOperator(ttOR,     TIntOrExpr,       typVariant,  typVariant);

   RegisterOperator(ttAND,    TBoolAndExpr,     typBoolean,  typBoolean);
   RegisterOperator(ttAND,    TBoolAndExpr,     typBoolean,  typVariant);
   RegisterOperator(ttAND,    TBoolAndExpr,     typVariant,  typBoolean);
   RegisterOperator(ttAND,    TIntAndExpr,      typInteger,  typInteger);
   RegisterOperator(ttAND,    TIntAndExpr,      typInteger,  typVariant);
   RegisterOperator(ttAND,    TIntAndExpr,      typVariant,  typInteger);
   RegisterOperator(ttAND,    TIntAndExpr,      typVariant,  typVariant);

   RegisterOperator(ttXOR,    TBoolXorExpr,     typBoolean,  typBoolean);
   RegisterOperator(ttXOR,    TBoolXorExpr,     typBoolean,  typVariant);
   RegisterOperator(ttXOR,    TBoolXorExpr,     typVariant,  typBoolean);
   RegisterOperator(ttXOR,    TIntXorExpr,      typInteger,  typInteger);
   RegisterOperator(ttXOR,    TIntXorExpr,      typInteger,  typVariant);
   RegisterOperator(ttXOR,    TIntXorExpr,      typVariant,  typInteger);
   RegisterOperator(ttXOR,    TIntXorExpr,      typVariant,  typVariant);

   RegisterOperator(ttSHL,    TShlExpr,         typInteger,  typInteger);
   RegisterOperator(ttSHL,    TShlExpr,         typInteger,  typVariant);
   RegisterOperator(ttSHL,    TShlExpr,         typVariant,  typInteger);
   RegisterOperator(ttSHL,    TShlExpr,         typVariant,  typVariant);

   RegisterOperator(ttSHR,    TShrExpr,         typInteger,  typInteger);
   RegisterOperator(ttSHR,    TShrExpr,         typInteger,  typVariant);
   RegisterOperator(ttSHR,    TShrExpr,         typVariant,  typInteger);
   RegisterOperator(ttSHR,    TShrExpr,         typVariant,  typVariant);

   // comparison operators

   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typBoolean, typBoolean);
   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typVariant, typBoolean);
   RegisterOperator(ttEQ,     TRelEqualBoolExpr,      typBoolean, typVariant);

   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typBoolean, typBoolean);
   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typBoolean, typVariant);
   RegisterOperator(ttNOTEQ,  TRelNotEqualBoolExpr,   typVariant, typBoolean);

   RegisterRelOp(ttEQ,     TRelEqualIntExpr, TRelEqualFloatExpr, TRelEqualStringExpr, TRelEqualVariantExpr);
   RegisterRelOp(ttNOTEQ,  TRelNotEqualIntExpr, TRelNotEqualFloatExpr, TRelNotEqualStringExpr, TRelNotEqualVariantExpr);
   RegisterRelOp(ttLESS,   TRelLessIntExpr, TRelLessFloatExpr, TRelLessStringExpr, TRelLessVariantExpr);
   RegisterRelOp(ttLESSEQ, TRelLessEqualIntExpr, TRelLessEqualFloatExpr, TRelLessEqualStringExpr, TRelLessEqualVariantExpr);
   RegisterRelOp(ttGTR,    TRelGreaterIntExpr, TRelGreaterFloatExpr, TRelGreaterStringExpr, TRelGreaterVariantExpr);
   RegisterRelOp(ttGTREQ,  TRelGreaterEqualIntExpr, TRelGreaterEqualFloatExpr, TRelGreaterEqualStringExpr, TRelGreaterEqualVariantExpr);

   RegisterOperator(ttEQ,     TRelEqualStringExpr,    typClassOf, typClassOf);
   RegisterOperator(ttNOTEQ,  TRelNotEqualStringExpr, typClassOf, typClassOf);

   // combined assignment operator

   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignIntExpr,     typInteger,    typInteger);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typInteger);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typInteger,    typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typFloat,      typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignFloatExpr,   typVariant,    typFloat);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typString,     typString);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typString,     typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignStrExpr,     typVariant,    typString);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typVariant,    typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typInteger,    typVariant);
   RegisterOperator(ttPLUS_ASSIGN,  TPlusAssignExpr,        typVariant,    typInteger);

   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignIntExpr,    typInteger,    typInteger);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typInteger);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typInteger,    typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typFloat,      typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignFloatExpr,  typVariant,    typFloat);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typInteger,    typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typVariant,    typVariant);
   RegisterOperator(ttMINUS_ASSIGN, TMinusAssignExpr,       typVariant,    typInteger);

   RegisterOperator(ttTIMES_ASSIGN, TMultAssignIntExpr,     typInteger,     typInteger);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typInteger);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typInteger,     typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typFloat,       typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignFloatExpr,   typVariant,     typFloat);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typInteger,     typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typVariant,     typVariant);
   RegisterOperator(ttTIMES_ASSIGN, TMultAssignExpr,        typVariant,     typInteger);

   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typInteger);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typInteger,     typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typInteger);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typFloat,       typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typFloat);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typVariant);
   RegisterOperator(ttDIVIDE_ASSIGN, TDivideAssignExpr,     typVariant,     typInteger);
end;

// RegisterOperator
//
procedure TBinaryOperators.RegisterOperator(aToken : TTokenType; aExprClass : TNoPosExprClass;
                                            aLeftType, aRightType : TSymbol);
var
   n : Integer;
begin
   n:=Length(FItems[aToken]);
   SetLength(FItems[aToken], n+1);
   with FItems[aToken][n] do begin
      ExprClass:=aExprClass;
      LeftType:=aLeftType;
      RighType:=aRightType;
   end;
end;

// ExprClassFor
//
function TBinaryOperators.ExprClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TNoPosExprClass;
var
   i : Integer;
begin
   if (aLeftType<>nil) and (aRightType<>nil) then begin
      for i:=0 to High(FItems[aToken]) do with FItems[aToken][i] do begin
         if aLeftType.IsOfType(LeftType) and aRightType.IsOfType(RighType) then
            Exit(ExprClass);
      end;
   end;
   Result:=nil;
end;

// BinaryOperatorClassFor
//
function TBinaryOperators.BinaryOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TBinaryOpExprClass;
var
   expr : TNoPosExprClass;
begin
   expr:=ExprClassFor(aToken, aLeftType, aRightType);
   if (expr<>nil) and expr.InheritsFrom(TBinaryOpExpr) then
      Result:=TBinaryOpExprClass(expr)
   else Result:=nil;
end;

// AssignmentOperatorClassFor
//
function TBinaryOperators.AssignmentOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TAssignExprClass;
var
   expr : TNoPosExprClass;
begin
   expr:=ExprClassFor(aToken, aLeftType, aRightType);
   if (expr<>nil) and expr.InheritsFrom(TAssignExpr) then
      Result:=TAssignExprClass(expr)
   else Result:=nil;
end;

// RelOperatorClassFor
//
function TBinaryOperators.RelOperatorClassFor(aToken : TTokenType; aLeftType, aRightType : TSymbol) : TRelOpExprClass;
var
   expr : TNoPosExprClass;
begin
   if (aLeftType=aRightType) and (aLeftType is TEnumerationSymbol) then begin
      aLeftType:=aLeftType.Typ;
      aRightType:=aRightType.Typ;
   end;
   expr:=ExprClassFor(aToken, aLeftType, aRightType);
   if (expr<>nil) and expr.InheritsFrom(dwsRelExprs.TRelOpExpr) then
      Result:=TRelOpExprClass(expr)
   else Result:=nil;
end;

end.
