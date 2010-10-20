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

uses Classes, Variants, SysUtils, dwsSymbols, dwsErrors, dwsStrings,
   dwsStack, dwsExprs, dwsUtils;

type

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
   end;

   TFloatVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         procedure EvalAsFloat(var Result : Double); override;
   end;

   TStrVarExpr = class (TVarExpr)
      protected
      public
         procedure AssignExpr(Expr: TNoPosExpr); override;
         procedure AssignValue(const Value: Variant); override;
         function  SetChar(index : Integer; c : Char) : Boolean;
         procedure EvalAsString(var Result : String); override;
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
     function  Eval: Variant; override;
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

     class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant) : TConstExpr; overload; static;
     class function CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Data: TData) : TConstExpr; overload; static;
   end;

   TConstBooleanExpr = class(TConstExpr)
   protected
     FValue : Boolean;
   public
     constructor Create(Prog: TdwsProgram; Value: Boolean);
     function EvalAsInteger : Int64; override;
     function EvalAsBoolean : Boolean; override;
   end;

   TConstIntExpr = class(TConstExpr)
   protected
     FValue : Int64;
   public
     constructor Create(Prog: TdwsProgram; const Value: Int64; Typ: TSymbol = nil);
     function EvalAsInteger : Int64; override;
     procedure EvalAsFloat(var Result : Double); override;
   end;

   TConstStringExpr = class(TConstExpr)
   protected
   public
     constructor Create(Prog: TdwsProgram; const Value: String);
     procedure EvalAsString(var Result : String); override;
   end;

   TConstFloatExpr = class(TConstExpr)
   protected
   public
     constructor Create(Prog: TdwsProgram; const Value: Double);
     procedure EvalAsFloat(var Result : Double); override;
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
   end;

   // Array expressions x[index]
   TArrayExpr = class(TPosDataExpr)
   protected
     FBaseExpr: TDataExpr;
     FElementSize: Integer;
     FIndexExpr: TNoPosExpr;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr);
     destructor Destroy; override;
     procedure Initialize; override;
   end;

   EScriptOutOfBounds = class (EScriptError);

   // Array expressions x[index] for static arrays
   TStaticArrayExpr = class(TArrayExpr)
   private
     FLowBound: Integer;
     FLastIndex: Integer;
   protected
     function GetAddr: Integer; override;
     function GetData: TData; override;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr; LowBound, HighBound: Integer);
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

  // length of dynamic arrays
  TArrayLengthExpr = class(TUnaryOpExpr)
  private
    FDelta: Integer;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TDataExpr; Delta: Integer);
    function Eval: Variant; override;
    function EvalAsInteger : Int64; override;
  end;

  TStringArrayOpExpr = class(TBinaryOpExpr)
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TNoPosExpr);
    function Eval: Variant; override;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
  end;

   TStringLengthExpr = class(TUnaryOpExpr)
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
     function Eval: Variant; override;
     function EvalAsInteger : Int64; override;
   end;

   TChrExpr = class(TUnaryOpExpr)
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
     function Eval: Variant; override;
     procedure EvalAsString(var Result : String); override;
   end;

   // obj is TMyClass
   TIsOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TNoPosExpr);
     function Eval: Variant; override;
     function EvalAsBoolean : Boolean; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // obj as TMyClass
   TAsOpExpr = class(TBinaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // >, <, =, <=, >=, <>
   TRelOpExpr = class(TBinaryOpExpr)
     FRelOp: TRelOps;
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TNoPosExpr; RelOp: TRelOps);
     function Eval: Variant; override;
     function EvalAsBoolean: Boolean; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;
   TRelOpExprClass = class of TRelOpExpr;

   TRelOpIntExpr = class(TRelOpExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelOpFloatExpr = class(TRelOpExpr)
     function EvalAsBoolean: Boolean; override;
   end;
   TRelOpStrExpr = class(TRelOpExpr)
     function EvalAsBoolean: Boolean; override;
   end;

   TObjCmpExpr = class(TBinaryOpExpr)
     FEqual: Boolean;
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TNoPosExpr;
                        Equal: Boolean);
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

   TNumberOpExpr = class(TBinaryOpExpr)
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   TIntegerOpExpr = class(TBinaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TStringOpExpr = class(TBinaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TFloatOpExpr = class(TBinaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;
   TBooleanOpExpr = class(TBinaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
   end;

   TNumberStringOpExpr = class(TBinaryOpExpr)
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // a + b
   TAddExpr = class(TNumberStringOpExpr)
     function Eval: Variant; override;
   end;
   TAddIntExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TAddStrExpr = class(TStringOpExpr)
     procedure EvalAsString(var Result : String); override;
   end;
   TAddFloatExpr = class(TFloatOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a - b
   TSubExpr = class(TNumberOpExpr)
     function Eval: Variant; override;
   end;
   TSubIntExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TSubFloatExpr = class(TFloatOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a * b
   TMultExpr = class(TNumberOpExpr)
     function Eval: Variant; override;
     function Optimize : TNoPosExpr; override;
   end;
   TMultIntExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TMultFloatExpr = class(TFloatOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a / b
   TDivideExpr = class(TFloatOpExpr)
     procedure EvalAsFloat(var Result : Double); override;
   end;

   // a div b
   TDivExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // a mod b
   TModExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // not a
   TNotExpr = class(TUnaryOpExpr)
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // a and b
   TIntAndExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TBoolAndExpr = class(TBooleanOpExpr)
     function EvalAsBoolean : Boolean; override;
   end;

   // a or b
   TIntOrExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TBoolOrExpr = class(TBooleanOpExpr)
     function EvalAsBoolean : Boolean; override;
   end;

   // a xor b
   TIntXorExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;
   TBoolXorExpr = class(TBooleanOpExpr)
     function EvalAsBoolean : Boolean; override;
   end;

   // a shl b
   TShlExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // a shr b
   TShrExpr = class(TIntegerOpExpr)
     function EvalAsInteger : Int64; override;
   end;

   // Float(x)
   TConvFloatExpr = class(TUnaryOpExpr)
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
     function Eval: Variant; override;
     procedure EvalAsFloat(var Result : Double); override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // Integer(float)
   TConvIntegerExpr = class(TUnaryOpExpr)
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
     function Eval: Variant; override;
     function EvalAsInteger : Int64; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // Variant(simple)
   TConvVariantExpr = class(TUnaryOpExpr)
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
     function Eval: Variant; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
   end;

   // left := right;
   TAssignExpr = class(TNoResultExpr)
   protected
     FLeft: TDataExpr;
     FRight: TNoPosExpr;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr);
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     function  Optimize : TNoPosExpr; override;
     function  OptimizeConstAssignment : TNoPosExpr;
   end;

   // left := right;
   TAssignDataExpr = class(TAssignExpr)
   protected
     FSize: Integer;
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr);
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // left := const right;
   TAssignConstDataToVarExpr = class(TAssignDataExpr)
   public
     constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; Right: TNoPosExpr);
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
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         property Right : Int64 read FRight write FRight;
   end;

   // left := const float;
   TAssignConstToFloatVarExpr = class(TAssignConstExpr)
      protected
         FRight : Double;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         property Right : Double read FRight write FRight;
   end;

   // left := const string;
   TAssignConstToStringVarExpr = class(TAssignConstExpr)
      protected
         FRight : String;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         property Right : String read FRight write FRight;
   end;

   // a := a + b
   TIncIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;
   // a := a - b
   TDecIntVarExpr = class(TAssignExpr)
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // statement; statement; statement;
   TBlockExpr = class(TNoResultExpr)
      private
         FStatements: TTightList;
         FTable: TSymbolTable;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);
         destructor Destroy; override;

         procedure AddStatement(Expr: TExpr);
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
         procedure Initialize; override;
         function  Optimize : TNoPosExpr; override;
         property  Table: TSymbolTable read FTable;
   end;

   // statement; statement; statement;
   TBlockExprNoTable = class(TBlockExpr)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

   // statement; statement; statement;
   TBlockExprNoStep = class(TBlockExpr)
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
     FIsUpWard: Boolean;
   public
     destructor Destroy; override;
     procedure EvalNoResult(var status : TExecutionStatusResult); override;
     procedure Initialize; override;
     procedure TypeCheckNoPos(const aPos : TScriptPos); override;
     property DoExpr: TNoPosExpr read FDoExpr write FDoExpr;
     property FromExpr: TNoPosExpr read FFromExpr write FFromExpr;
     property ToExpr: TNoPosExpr read FToExpr write FToExpr;
     property IsUpward: Boolean read FIsUpWard write FIsUpWard;
     property VarExpr: TIntVarExpr read FVarExpr write FVarExpr;
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
   if Typ=Prog.TypInteger then
      Result:=TIntVarExpr.Create(Prog, Typ, dataSym)
   else if Typ=Prog.TypFloat then
      Result:=TFloatVarExpr.Create(Prog, Typ, dataSym)
   else if Typ=Prog.TypString then
      Result:=TStrVarExpr.Create(Prog, Typ, dataSym)
   else if Typ=Prog.TypBoolean then
      Result:=TBoolVarExpr.Create(Prog, Typ, dataSym)
   else if Typ is TClassSymbol then
      Result:=TObjectVarExpr.Create(Prog, Typ, dataSym)
   else Result:=TVarExpr.Create(Prog, Typ, dataSym);
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

// ------------------
// ------------------ TFloatVarExpr ------------------
// ------------------

procedure TFloatVarExpr.AssignExpr(Expr: TNoPosExpr);
var
   buf : Double;
begin
   Expr.EvalAsFloat(buf);
   FStack.WriteFloatValue(FStack.BasePointer + FStackAddr, buf);
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

{ TConstExpr }

constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant);
begin
  inherited Create(Prog, Typ);
  Assert(Typ.Size=1);
  SetLength(FData, 1);
  FData[0] := Value;
  FIsWritable := False;
end;

constructor TConstExpr.Create(Prog: TdwsProgram; Typ: TSymbol; const Data: TData);
begin
  inherited Create(Prog, Typ);
  FData := Data;
  FIsWritable := False;
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

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Value: Variant) : TConstExpr;
begin
   if Typ=Prog.TypString then
      Result:=TConstStringExpr.Create(Prog, Value)
   else if (Typ=Prog.TypInteger) or (Typ.Typ=Prog.TypInteger) then
      Result:=TConstIntExpr.Create(Prog, Value, Typ)
   else if Typ=Prog.TypBoolean then
      Result:=TConstBooleanExpr.Create(Prog, Value)
   else if Typ=Prog.TypFloat then
      Result:=TConstFloatExpr.Create(Prog, Value)
   else Result:=TConstExpr.Create(Prog, Typ, Value);
end;

// CreateTyped
//
class function TConstExpr.CreateTyped(Prog: TdwsProgram; Typ: TSymbol; const Data: TData) : TConstExpr;
begin
   if Length(Data)=1 then
      Result:=TConstExpr.CreateTyped(Prog, Typ, Data[0])
   else Result:=TConstExpr.Create(Prog, Typ, Data);
end;

// GetData
//
function TConstExpr.GetData: TData;
begin
  Result := FData;
end;

// ------------------
// ------------------ TConstBooleanExpr ------------------
// ------------------

// Create
//
constructor TConstBooleanExpr.Create(Prog: TdwsProgram; Value: Boolean);
begin
   inherited Create(Prog, Prog.TypBoolean, Value);
   FValue:=Value;
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
constructor TConstIntExpr.Create(Prog: TdwsProgram; const Value: Int64; Typ: TSymbol = nil);
begin
   if Typ=nil then
      Typ:=Prog.TypInteger;
   inherited Create(Prog, Typ, Value);
   FValue:=Value;
end;

// EvalAsInteger
//
function TConstIntExpr.EvalAsInteger : Int64;
begin
   Result:=FValue;
end;

// EvalAsFloat
//
procedure TConstIntExpr.EvalAsFloat(var Result : Double);
begin
   Result:=FValue;
end;

// ------------------
// ------------------ TConstStringExpr ------------------
// ------------------

constructor TConstStringExpr.Create(Prog: TdwsProgram; const Value: String);
var
   str : String;
begin
   UnifyCopyString(Value, str);
   inherited Create(Prog, Prog.TypString, str);
end;

// EvalAsString
//
procedure TConstStringExpr.EvalAsString(var Result : String);
begin
   Result:=String(PVarData(@FData[0]).VUString);
end;

// ------------------
// ------------------ TConstFloatExpr ------------------
// ------------------

constructor TConstFloatExpr.Create(Prog: TdwsProgram; const Value: Double);
begin
   inherited Create(Prog, Prog.TypFloat, Value);
end;

procedure TConstFloatExpr.EvalAsFloat(var Result : Double);
begin
   Result:=PVarData(@FData[0]).VDouble;
end;

{ TArrayExpr }

constructor TArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr);
begin
  inherited Create(Prog, Pos, BaseExpr.BaseType.Typ);
  FBaseExpr := BaseExpr;
  FIndexExpr := IndexExpr;
  FElementSize := Typ.Size; // Necessary because of arrays of records!
  FIsWritable := BaseExpr.IsWritable;
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

{ TStaticArrayExpr }

constructor TStaticArrayExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TNoPosExpr; LowBound, HighBound: Integer);
begin
  inherited Create(Prog, Pos, BaseExpr, IndexExpr);
  FLowBound := LowBound;
  FLastIndex := HighBound - LowBound;
end;

function TStaticArrayExpr.GetAddr: Integer;
var
  index: Integer;
begin
   // Get index
   index := FIndexExpr.Eval - FLowBound;

   if Cardinal(index)>Cardinal(FLastIndex) then begin
      if index > FLastIndex then
         raise EScriptOutOfBounds.Create(RTE_UpperBoundExceeded)
      else if index < 0 then
         raise EScriptOutOfBounds.Create(RTE_LowerBoundExceeded);
   end;
   // Calculate the address
   Result := FBaseExpr.Addr + (index * FElementSize);
end;

function TStaticArrayExpr.GetData: TData;
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
  FIsWritable := False;
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

// TypeCheckNoPos
//
procedure TArrayConstantExpr.TypeCheckNoPos(const aPos : TScriptPos);
var
  x: Integer;
  expr : TNoPosExpr;
begin
   for x:=0 to FElementExprs.Count - 1 do begin
      expr:=TNoPosExpr(FElementExprs.List[x]);
      expr.TypeCheckNoPos(aPos);
      if (Typ.Typ=Prog.TypFloat) and (expr.Typ=Prog.TypInteger) then begin
         expr:=TConvFloatExpr.Create(Prog, aPos, expr);
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
  FIsWritable := FBaseExpr.IsWritable;
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

{ TArrayLengthExpr }

constructor TArrayLengthExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  Expr: TDataExpr; Delta: Integer);
begin
  inherited Create(Prog, Pos, Expr);
  FDelta := Delta;
  FTyp := FProg.TypInteger;
end;

function TArrayLengthExpr.Eval: Variant;
begin
   Result:=EvalAsInteger;
end;

function TArrayLengthExpr.EvalAsInteger : Int64;
var
  adr: Integer;
begin
  adr := TDataExpr(FExpr).Data[TDataExpr(FExpr).Addr];
  Result := TDataExpr(FExpr).Data[adr - 1] + FDelta;
end;

{ TStringArrayOpExpr }

constructor TStringArrayOpExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  Left, Right: TNoPosExpr);
begin
  inherited;
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
      AddExecutionStopFmt(RTE_ArrayUpperBoundExceeded, [i])
   else if i<1 then
      AddExecutionStopFmt(RTE_ArrayLowerBoundExceeded, [i]);
   Result:=buf[i];
end;

// TypeCheckNoPos
//
procedure TStringArrayOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if not (FLeft.IsStringValue) and (FRight.IsIntegerValue) then
    AddCompilerStop(CPE_StringExpected);
end;

{ TIsOpExpr }

constructor TIsOpExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left,
  Right: TNoPosExpr);
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
   FLeft.TypeCheckNoPos(Pos);
   FRight.TypeCheckNoPos(Pos);
   if not (FLeft.Typ is TClassSymbol) then
      AddCompilerStop(CPE_ObjectExpected);
   if not (FRight.Typ is TClassOfSymbol) then
      AddCompilerStop(CPE_ClassRefExpected);
end;

{ TAsOpExpr }

function TAsOpExpr.Eval: Variant;
var
  scriptObj: IScriptObj;
begin
  Result := FLeft.Eval;
  scriptObj := IScriptObj(IUnknown(Result));

  if Assigned(scriptObj) and not (FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym)) then
    raise EScriptException.CreateFmt(RTE_ClassCastFailed, [scriptObj.ClassSym.Caption, FRight.Typ.Typ.Caption]);
end;

// TypeCheckNoPos
//
procedure TAsOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  FLeft.TypeCheckNoPos(Pos);
  FRight.TypeCheckNoPos(Pos);
  if not (FLeft.Typ is TClassSymbol) then
    AddCompilerStop(CPE_ObjectExpected);
  if not (FRight.Typ is TClassOfSymbol) then
    AddCompilerStop(CPE_ClassRefExpected);
  FTyp := FRight.Typ.Typ;
end;

{ TConvFloatExpr }

constructor TConvFloatExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
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
      FProg.Msgs.AddCompilerError(FPos, CPE_IntegerExpected);
end;

{ TConvIntegerExpr }

constructor TConvIntegerExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
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
    FProg.Msgs.AddCompilerError(FPos, CPE_IntegerCastInvalid);
end;

{ TConvVariantExpr }

constructor TConvVariantExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
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
    FProg.Msgs.AddCompilerError(FPos, CPE_VariantExpected);
end;

{ TStringLengthExpr }

constructor TStringLengthExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
begin
  inherited;
  FTyp := FProg.TypInteger;
end;

function TStringLengthExpr.Eval: Variant;
begin
   Result:=EvalAsInteger;
end;

// EvalAsInteger
//
function TStringLengthExpr.EvalAsInteger : Int64;
var
   buf : String;
begin
   FExpr.EvalAsString(buf);
   Result:=Length(buf);
end;

{ TChrExpr }

constructor TChrExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
begin
   inherited;
   FTyp := FProg.TypString;
end;

function TChrExpr.Eval: Variant;
var
   buf : String;
begin
   EvalAsString(buf);
   Result:=buf;
end;

// EvalAsString
//
procedure TChrExpr.EvalAsString(var Result : String);
begin
   Result:=Chr(FExpr.EvalAsInteger);
end;

{ TObjCmpExpr }

constructor TObjCmpExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left,
  Right: TNoPosExpr; Equal: Boolean);
begin
  inherited Create(Prog, Pos, Left, Right);
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
  FLeft.TypeCheckNoPos(Pos);
  FRight.TypeCheckNoPos(Pos);
  if not ((FLeft.Typ is TClassSymbol) or (FLeft.Typ = FProg.TypNil)) then
    AddCompilerStop(CPE_ObjectExpected);
  if not ((FRight.Typ is TClassSymbol) or (FRight.Typ = FProg.TypNil)) then
    AddCompilerStop(CPE_ObjectExpected);
end;

{ TRelOpExpr }

constructor TRelOpExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left, Right: TNoPosExpr;
  RelOp: TRelOps);
begin
  inherited Create(Prog, Pos, Left, Right);
  FRelOp := RelOp;
  FTyp := FProg.TypBoolean;
end;

function TRelOpExpr.Eval: Variant;
begin
  Result:=EvalAsBoolean;
end;

function TRelOpExpr.EvalAsBoolean: Boolean;
begin
   case FRelOp of
      roEqual:      Result := FLeft.Eval = FRight.Eval;
      roUnEqual:    Result := FLeft.Eval <> FRight.Eval;
      roLess:       Result := FLeft.Eval < FRight.Eval;
      roLessEqual:  Result := FLeft.Eval <= FRight.Eval;
      roMore:       Result := FLeft.Eval > FRight.Eval;
      roMoreEqual:  Result := FLeft.Eval >= FRight.Eval;
   else
      Result:=False;
      Assert(False);
   end;
end;

// TypeCheckNoPos
//
procedure TRelOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if not (FLeft.Typ.IsCompatible(FRight.Typ)) then
    AddCompilerStop(CPE_InvalidOperands);
end;

// ------------------
// ------------------ TRelOpIntExpr ------------------
// ------------------

function TRelOpIntExpr.EvalAsBoolean: Boolean;
begin
   case FRelOp of
      roEqual:      Result := FLeft.EvalAsInteger = FRight.EvalAsInteger;
      roUnEqual:    Result := FLeft.EvalAsInteger <> FRight.EvalAsInteger;
      roLess:       Result := FLeft.EvalAsInteger < FRight.EvalAsInteger;
      roLessEqual:  Result := FLeft.EvalAsInteger <= FRight.EvalAsInteger;
      roMore:       Result := FLeft.EvalAsInteger > FRight.EvalAsInteger;
      roMoreEqual:  Result := FLeft.EvalAsInteger >= FRight.EvalAsInteger;
   else
      Result:=False;
      Assert(False);
   end;
end;

// ------------------
// ------------------ TRelOpFloatExpr ------------------
// ------------------

function TRelOpFloatExpr.EvalAsBoolean: Boolean;
var
   left, right : Double;
begin
   FLeft.EvalAsFloat(left);
   FRight.EvalAsFloat(right);
   case FRelOp of
      roEqual:      Result := left = right;
      roUnEqual:    Result := left <> right;
      roLess:       Result := left < right;
      roLessEqual:  Result := left <= right;
      roMore:       Result := left > right;
      roMoreEqual:  Result := left >= right;
   else
      Result:=False;
      Assert(False);
   end;
end;

// ------------------
// ------------------ TRelOpStrExpr ------------------
// ------------------

function TRelOpStrExpr.EvalAsBoolean: Boolean;
var
   bufLeft, bufRight : String;
begin
   FLeft.EvalAsString(bufLeft);
   FRight.EvalAsString(bufRight);
   case FRelOp of
      roEqual:      Result := bufLeft = bufRight;
      roUnEqual:    Result := bufLeft <> bufRight;
      roLess:       Result := bufLeft < bufRight;
      roLessEqual:  Result := bufLeft <= bufRight;
      roMore:       Result := bufLeft > bufRight;
      roMoreEqual:  Result := bufLeft >= bufRight;
   else
      Result:=False;
      Assert(False);
   end;
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
   FExpr.TypeCheckNoPos(Pos);
   if FTyp=nil then begin
      AddCompilerError(CPE_NumericalExpected);
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
      Result:=TConstIntExpr.Create(FProg, -FExpr.EvalAsInteger);
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
      Result:=TConstFloatExpr.Create(FProg, -xf);
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
   bufRight : Double;
begin
   FLeft.EvalAsFloat(Result);
   FRight.EvalAsFloat(bufRight);
   Result:=Result+bufRight;
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

// ------------------
// ------------------ TSubFloatExpr ------------------
// ------------------

procedure TSubFloatExpr.EvalAsFloat(var Result : Double);
var
   bufRight : Double;
begin
   FLeft.EvalAsFloat(Result);
   FRight.EvalAsFloat(bufRight);
   Result:=Result-bufRight;
end;

{ TMultExpr }

function TMultExpr.Eval: Variant;
begin
  Result := FLeft.Eval * FRight.Eval;
end;

// Optimize
//
function TMultExpr.Optimize : TNoPosExpr;
begin
   if FLeft.IsIntegerValue and FRight.IsIntegerValue then
      Result:=TMultIntExpr.Create(FProg, Pos, FLeft, FRight)
   else if FLeft.IsNumberValue and FRight.IsNumberValue then
      Result:=TMultFloatExpr.Create(FProg, Pos, FLeft, FRight)
   else Result:=Self;
   if Result<>Self then begin
      FLeft:=nil;
      FRight:=nil;
      Free;
      Result:=Result.Optimize;
   end;
end;

{ TMultIntExpr }

function TMultIntExpr.EvalAsInteger : Int64;
begin
  Result := FLeft.EvalAsInteger * FRight.EvalAsInteger;
end;

{ TMultFloatExpr }

procedure TMultFloatExpr.EvalAsFloat(var Result : Double);
var
   bufRight : Double;
begin
   FLeft.EvalAsFloat(Result);
   FRight.EvalAsFloat(bufRight);
   Result:=Result*bufRight;
end;

{ TDivideExpr }

// EvalAsFloat
//
procedure TDivideExpr.EvalAsFloat(var Result : Double);
var
   b : Double;
begin
   FLeft.EvalAsFloat(Result);
   FRight.EvalAsFloat(b);
   Result:=Result/b;
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
  FExpr.TypeCheckNoPos(Pos);
  if FExpr.IsVariantValue then
    FTyp := FProg.TypVariant
  else if FExpr.IsBooleanValue then
    FTyp := FProg.TypBoolean
  else if FExpr.IsIntegerValue then
    FTyp := FProg.TypInteger
  else AddCompilerStop(CPE_BooleanOrIntegerExpected);
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

{ TNumberOpExpr }

// TypeCheckNoPos
//
procedure TNumberOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if FLeft.IsIntegerValue and FRight.IsIntegerValue then
      FTyp := FProg.TypInteger
   else if FLeft.IsNumberValue and FRight.IsNumberValue then
      FTyp := FProg.TypFloat
   else if     (FLeft.IsVariantValue or FLeft.IsNumberValue)
           and (FRight.IsVariantValue or FRight.IsNumberValue) then
      FTyp := FProg.TypVariant
   else AddCompilerError(CPE_InvalidOperands);
end;

// ------------------
// ------------------ TIntegerOpExpr ------------------
// ------------------

function TIntegerOpExpr.Eval: Variant;
begin
   Result:=EvalAsInteger;
end;

// TypeCheckNoPos
//
procedure TIntegerOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if     (FLeft.IsVariantValue or FLeft.IsIntegerValue)
     and (FRight.IsVariantValue or FRight.IsIntegerValue) then
     FTyp:=FProg.TypInteger
  else AddCompilerError(CPE_InvalidOperands);
end;

// Optimize
//
function TIntegerOpExpr.Optimize : TNoPosExpr;
begin
   if IsConstant then begin
      Result:=TConstIntExpr.Create(FProg, EvalAsInteger);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TStringOpExpr ------------------
// ------------------

// Eval
//
function TStringOpExpr.Eval: Variant;
var
   buf : String;
begin
   EvalAsString(buf);
   Result:=buf;
end;

// TypeCheckNoPos
//
procedure TStringOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if     (FLeft.IsVariantValue or FLeft.IsStringValue)
     and (FRight.IsVariantValue or FRight.IsStringValue) then
     FTyp:=FProg.TypString
  else AddCompilerStop(CPE_InvalidOperands);
end;

// Optimize
//
function TStringOpExpr.Optimize : TNoPosExpr;
var
   buf : String;
begin
   if IsConstant then begin
      EvalAsString(buf);
      Result:=TConstStringExpr.Create(FProg, buf);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TFloatOpExpr ------------------
// ------------------

// Eval
//
function TFloatOpExpr.Eval: Variant;
var
   d : Double;
begin
   EvalAsFloat(d);
   Result:=d;
end;

// TypeCheckNoPos
//
procedure TFloatOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if     (FLeft.IsVariantValue or FLeft.IsNumberValue)
     and (FRight.IsVariantValue or FRight.IsNumberValue) then
     FTyp:=FProg.TypFloat
  else AddCompilerStop(CPE_InvalidOperands);
end;

// Optimize
//
function TFloatOpExpr.Optimize : TNoPosExpr;
var
   xf : Double;
begin
   if IsConstant then begin
      EvalAsFloat(xf);
      Result:=TConstFloatExpr.Create(FProg, xf);
      Free;
   end else begin
      FLeft:=FLeft.OptimizeIntegerConstantToFloatConstant;
      FRight:=FRight.OptimizeIntegerConstantToFloatConstant;
      Result:=Self;
   end;
end;

// ------------------
// ------------------ TBooleanOpExpr ------------------
// ------------------

function TBooleanOpExpr.Eval: Variant;
begin
   Result:=EvalAsBoolean;
end;

// TypeCheckNoPos
//
procedure TBooleanOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if     (FLeft.IsVariantValue or FLeft.IsBooleanValue)
     and (FRight.IsVariantValue or FRight.IsBooleanValue) then
     FTyp:=FProg.TypBoolean
  else AddCompilerStop(CPE_InvalidOperands);
end;

// Optimize
//
function TBooleanOpExpr.Optimize : TNoPosExpr;
begin
   if IsConstant then begin
      Result:=TConstBooleanExpr.Create(FProg, EvalAsBoolean);
      Free;
   end else Result:=Self;
end;

{ TNumberStringOpExpr }

// TypeCheckNoPos
//
procedure TNumberStringOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  inherited;
  if FLeft.IsVariantValue or FRight.IsVariantValue then
    FTyp := FProg.TypVariant
  else if FLeft.IsIntegerValue and FRight.IsIntegerValue then
    FTyp := FProg.TypInteger
  else if FLeft.IsNumberValue and FRight.IsNumberValue then
    FTyp := FProg.TypFloat
  else if FLeft.IsStringValue and FRight.IsStringValue then
    FTyp := FProg.TypString
  else if FLeft.IsBooleanValue and FRight.IsBooleanValue then
    FTyp := FProg.TypBoolean
  else
    AddCompilerStop(CPE_IncompatibleOperands);
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
var
  cleft, cright: string;
begin
  if FLeft.Typ = nil then
    cleft := SYS_VOID
  else
    cleft := FLeft.Typ.Caption;

  if FRight.Typ = nil then
    cright := SYS_VOID
  else
    cright := FRight.Typ.Caption;

  if (FRight.Typ = nil) or (FLeft.Typ = nil) then
    AddCompilerErrorFmt(CPE_AssignIncompatibleTypes, [cright, cleft])
  else begin
    if FRight is TArrayConstantExpr then
      TArrayConstantExpr(FRight).Prepare(FLeft.Typ.Typ);

    FRight.TypeCheckNoPos(Pos);

    // Automatic conversion from int to float values
    if (FLeft.Typ = FProg.TypFloat) and (FRight.Typ = FProg.TypInteger) then
      FRight := TConvFloatExpr.Create(FProg, FPos, FRight);

    // Look if Types are compatible
    if not FLeft.Typ.IsCompatible(FRight.Typ) then
      AddCompilerErrorFmt(CPE_AssignIncompatibleTypes, [cright, cleft]);
  end;
end;

// Optimize
//
function TAssignExpr.Optimize : TNoPosExpr;
var
   leftVarExpr : TVarExpr;
   addExpr : TAddIntExpr;
   subExpr : TSubIntExpr;
begin
   Result:=Self;
   if FLeft is TIntVarExpr then begin
      leftVarExpr:=TVarExpr(FLeft);
      if FRight is TAddIntExpr then begin
         addExpr:=TAddIntExpr(FRight);
         if (addExpr.Left is TVarExpr) and (TVarExpr(addExpr.Left).SameVarAs(leftVarExpr)) then begin
            Result:=TIncIntVarExpr.Create(Prog, Pos, FLeft, addExpr.Right);
            FLeft:=nil;
            addExpr.Right:=nil;
            Free;
         end;
      end else if FRight is TSubIntExpr then begin
         subExpr:=TSubIntExpr(FRight);
         if (subExpr.Left is TVarExpr) and (TVarExpr(subExpr.Left).SameVarAs(leftVarExpr)) then begin
            Result:=TDecIntVarExpr.Create(Prog, Pos, FLeft, subExpr.Right);
            FLeft:=nil;
            subExpr.Right:=nil;
            Free;
         end;
      end else if FRight.IsConstant then begin
         Result:=OptimizeConstAssignment;
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
      Result:=TAssignConstToIntegerVarExpr.Create(Prog, Pos, FLeft, FRight.EvalAsInteger);
   end else if FRight.IsFloatValue then begin
      FRight.EvalAsFloat(floatBuf);
      Result:=TAssignConstToFloatVarExpr.Create(Prog, Pos, FLeft, floatBuf);
   end else if FRight.IsStringValue then begin
      FRight.EvalAsString(stringBuf);
      Result:=TAssignConstToStringVarExpr.Create(Prog, Pos, FLeft, stringBuf);
   end;
   if Result<>Self then begin
      FLeft:=nil;
      Free;
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
constructor TAssignConstToIntegerVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Int64);
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
constructor TAssignConstToFloatVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : Double);
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
constructor TAssignConstToStringVarExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Left : TDataExpr; const rightValue : String);
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
// ------------------ TBlockExpr ------------------
// ------------------

// Create
//
constructor TBlockExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   inherited Create(Prog, Pos);
   FTable := TSymbolTable.Create(Prog.Table, Prog.Table.AddrGenerator);
end;

// Destroy
//
destructor TBlockExpr.Destroy;
begin
   FStatements.Clean;
   FTable.Free;
   inherited;
end;

// AddStatement
//
procedure TBlockExpr.AddStatement(Expr: TExpr);
begin
   FStatements.Add(Expr);
end;

// EvalNoResult
//
procedure TBlockExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   oldTable : TSymbolTable;
   expr : TExpr;
   list : PPointerList;
begin
   oldTable:=FProg.Table;
   try
      FProg.Table:=FTable;
      list:=FStatements.List;
      for i:=0 to FStatements.Count-1 do begin
         expr:=TExpr(list[i]);
         FProg.DoStep(expr);
         expr.EvalNoResult(status);
         if status<>esrNone then Break;
      end;
   finally
      FProg.Table:=oldTable;
   end;
end;

procedure TBlockExpr.Initialize;
var
   i : Integer;
begin
   for i:=0 to FStatements.Count-1 do
      TNoPosExpr(FStatements.List[i]).Initialize;
end;

// Optimize
//
function TBlockExpr.Optimize : TNoPosExpr;
begin
   if FTable.Count=0 then begin
      case FStatements.Count of
         0 : Result:=TNullExpr.Create(Prog, Pos);
         1 : Result:=TNoPosExpr(FStatements.List[0]);
      else
         Result:=TBlockExprNoTable.Create(Prog, Pos);
         TBlockExprNoTable(Result).FStatements.Assign(FStatements);
      end;
      FStatements.Clear;
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
   expr : TExpr;
   list : PPointerList;
begin
   list:=FStatements.List;
   for i:=0 to FStatements.Count-1 do begin
      expr:=TExpr(list[i]);
      FProg.DoStep(expr);
      expr.EvalNoResult(status);
      if status<>esrNone then Break;
   end;
end;

// ------------------
// ------------------ TBlockExprNoStep ------------------
// ------------------

// EvalNoResult
//
procedure TBlockExprNoStep.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   oldTable : TSymbolTable;
   expr : TExpr;
begin
   oldTable:=FProg.Table;
   try
      FProg.Table:=FTable;
      for i:=0 to FStatements.Count-1 do begin
         expr:=TExpr(FStatements.List[i]);
         expr.EvalNoResult(status);
         if status<>esrNone then Break;
      end;
   finally
      FProg.Table:=oldTable;
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
   if not FCond.IsBooleanValue then
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
      FCompareExpr := TConvFloatExpr.Create(FValueExpr.Prog, Pos, FCompareExpr);

  if not FCompareExpr.Typ.IsCompatible(FValueExpr.Typ) then
    FCompareExpr.Prog.Msgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                               [FValueExpr.Typ.Caption, FCompareExpr.Typ.Caption]);
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
      FFromExpr := TConvFloatExpr.Create(Prog, Pos, FFromExpr);

    if FToExpr.IsIntegerValue then
      FToExpr := TConvFloatExpr.Create(Prog, Pos, FToExpr);
  end;

  if not FFromExpr.Typ.IsCompatible(FToExpr.Typ) then
    FFromExpr.Prog.Msgs.AddCompilerErrorFmt(Pos, CPE_RangeIncompatibleTypes,
                                            [FFromExpr.Typ.Caption, FToExpr.Typ.Caption]);

  if not FValueExpr.Typ.IsCompatible(FFromExpr.Typ) then
    FFromExpr.Prog.Msgs.AddCompilerErrorFmt(Pos, CPE_IncompatibleTypes,
                                            [FValueExpr.Typ.Caption, FFromExpr.Typ.Caption]);
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

procedure TForExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Int64;
   toValue: Int64;
begin
   status:=esrNone;
   i:=FFromExpr.EvalAsInteger;
   toValue:=FToExpr.EvalAsInteger;
   if FIsUpWard then begin
      FVarExpr.AssignValueAsPInteger(@i);
      while i<=toValue do begin
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
         Inc(i);
         FVarExpr.AssignValueAsPInteger(@i);
      end;
   end else begin
      FVarExpr.AssignValueAsPInteger(@i);
      while i>=toValue do begin
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
         Dec(i);
         FVarExpr.AssignValueAsPInteger(@i);
      end;
   end;
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
   FFromExpr.TypeCheckNoPos(Pos);
   if not FFromExpr.IsIntegerValue then
      AddCompilerStop(CPE_IntegerExpected);
   FToExpr.TypeCheckNoPos(Pos);
   if not FToExpr.IsIntegerValue then
      AddCompilerStop(CPE_IntegerExpected);
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
   if not FCondExpr.IsBooleanValue then
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
  FExceptionVar.Initialize;
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
      AddExecutionStopFmt(RTE_ArrayUpperBoundExceeded, [i])
   else if i<1 then
      AddExecutionStopFmt(RTE_ArrayLowerBoundExceeded, [i]);
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
      raise EScriptException.CreateFmt(RTE_ArrayLowerBoundExceeded, [i])
   else begin
      FValueExpr.EvalAsString(buf);
      c:=buf[1];
      if not TStrVarExpr(FStringExpr).SetChar(i, c) then
         raise EScriptException.CreateFmt(RTE_ArrayUpperBoundExceeded, [i]);
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
      raise EScriptException.CreateFmt(RTE_ArrayLowerBoundExceeded, [i])
   else begin
      c:=Chr(FValueExpr.EvalAsInteger);
      if not TStrVarExpr(FStringExpr).SetChar(i, c) then
         raise EScriptException.CreateFmt(RTE_ArrayUpperBoundExceeded, [i]);
   end;
end;

end.
