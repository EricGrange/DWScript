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
unit dwsSymbols;

interface

uses Windows, SysUtils, Variants, Classes, dwsStrings, dwsErrors,
   dwsUtils, dwsTokenizer, dwsStack;

type

   IScriptObj = interface;
   PIScriptObj = ^IScriptObj;
   TdwsExecution = class;
   TExprBase = class;
   TSymbol = class;
   TBaseSymbol = class;
   TDataSymbol = class;
   TFuncSymbol = class;
   TClassSymbol = class;
   TMethodSymbol = class;
   TFieldSymbol = class;
   TRecordSymbol = class;
   TParamSymbol = class;
   TVarParamSymbol = class;
   TSymbolTable = class;
   TMembersSymbolTable = class;
   TUnSortedSymbolTable = class;
   TStaticSymbolTable = class;
   TTypeSymbol = class;
   TParamsSymbolTable = class;
   TConditionsSymbolTable = class;
   TdwsRuntimeMessageList = class;

   TdwsExprLocation = record
      Expr : TExprBase;
      Prog : TObject;
      function Line : Integer; inline;
      function SourceName : String; inline;
      function Location : String;
   end;
   TdwsExprLocationArray = array of TdwsExprLocation;

   // Interface for external debuggers
   IDebugger = interface
      ['{8D534D14-4C6B-11D5-8DCB-0000216D9E86}']
      procedure StartDebug(exec: TdwsExecution);
      procedure DoDebug(exec: TdwsExecution; expr: TExprBase);
      procedure StopDebug(exec: TdwsExecution);
      procedure EnterFunc(exec: TdwsExecution; funcExpr: TExprBase);
      procedure LeaveFunc(exec: TdwsExecution; funcExpr: TExprBase);
   end;

   TProgramState = (psUndefined, psReadyToRun, psRunning, psRunningStopped, psTerminated);

   IdwsExecution = interface
      ['{8F2D1D7E-9954-4391-B919-86EF1EE21C8C}']
      function GetMsgs : TdwsRuntimeMessageList;
      function  GetDebugger : IDebugger;
      procedure SetDebugger(const aDebugger : IDebugger);
      function GetExecutionObject : TdwsExecution;
      function GetUserObject : TObject;
      procedure SetUserObject(const value : TObject);
      function GetStack : TStack;
      function GetProgramState : TProgramState;

      function GetCallStack : TdwsExprLocationArray;

      property ProgramState : TProgramState read GetProgramState;
      property Stack : TStack read GetStack;
      property Msgs : TdwsRuntimeMessageList read GetMsgs;
      property Debugger : IDebugger read GetDebugger write SetDebugger;
      property ExecutionObject : TdwsExecution read GetExecutionObject;
      property UserObject : TObject read GetUserObject write SetUserObject;
   end;

   TRuntimeErrorMessage = class(TScriptMessage)
      private
         FCallStack : TdwsExprLocationArray;

      public
         function AsInfo: String; override;

         property CallStack : TdwsExprLocationArray read FCallStack;
   end;

   // TdwsRuntimeMessageList
   //
   TdwsRuntimeMessageList = class (TdwsMessageList)
      public
         procedure AddRuntimeError(const Text: String); overload;
         procedure AddRuntimeError(const scriptPos : TScriptPos; const Text: String;
                                   const callStack : TdwsExprLocationArray); overload;
   end;

   TExecutionStatusResult = (esrNone, esrExit, esrBreak, esrContinue);

   // TdwsExecution
   //
   TdwsExecution = class abstract (TInterfacedObject, IdwsExecution)
      protected
         FStack : TStackMixIn;
         FStatus : TExecutionStatusResult;
         FCallStack : TTightStack; // expr + prog duples
         FSelfScriptObject : PIScriptObj;
         FSelfScriptClassSymbol : TClassSymbol;
         FLastScriptError : TExprBase;
         FLastScriptCallStack : TdwsExprLocationArray;
         FExceptionObjectStack : TSimpleStack<Variant>;

         FDebugger : IDebugger;
         FIsDebugging : Boolean;

         FContextTable : TSymbolTable;
         FExternalObject : TObject;
         FUserObject : TObject;

      protected
         FProgramState : TProgramState;

         function  GetDebugger : IDebugger;
         procedure SetDebugger(const aDebugger : IDebugger);
         procedure StartDebug;
         procedure StopDebug;

         function GetMsgs : TdwsRuntimeMessageList; virtual; abstract;

         function GetExecutionObject : TdwsExecution;

         function GetUserObject : TObject; virtual;
         procedure SetUserObject(const value : TObject); virtual;

         function GetStack : TStack;

         function GetProgramState : TProgramState;

      public
         constructor Create(const stackParams : TStackParameters);
         destructor Destroy; override;

         procedure DoStep(expr : TExprBase); inline;

         property Status : TExecutionStatusResult read FStatus write FStatus;
         property Stack : TStackMixIn read FStack;
         property SelfScriptObject : PIScriptObj read FSelfScriptObject write FSelfScriptObject;
         property SelfScriptClassSymbol : TClassSymbol read FSelfScriptClassSymbol write FSelfScriptClassSymbol;

         procedure SetScriptError(expr : TExprBase);
         procedure ClearScriptError;

         function GetCallStack : TdwsExprLocationArray; virtual; abstract;
         function CallStackDepth : Integer; virtual; abstract;

         property LastScriptError : TExprBase read FLastScriptError;
         property LastScriptCallStack : TdwsExprLocationArray read FLastScriptCallStack;
         property ExceptionObjectStack : TSimpleStack<Variant> read FExceptionObjectStack;

         property ProgramState : TProgramState read FProgramState;

         property ContextTable : TSymbolTable read FContextTable write FContextTable;
         property Debugger : IDebugger read FDebugger write SetDebugger;
         property IsDebugging : Boolean read FIsDebugging;

         property Msgs : TdwsRuntimeMessageList read GetMsgs;

         // specifies an external object for IInfo constructors, temporary
         property ExternalObject : TObject read FExternalObject write FExternalObject;

         // user object, to attach to an execution
         property UserObject : TObject read GetUserObject write SetUserObject;
   end;

   TExprBaseEnumeratorProc = reference to procedure (parent, expr : TExprBase; var abort : Boolean);

   // Base class for all Exprs
   TExprBase = class
      protected
         function GetSubExpr(i : Integer) : TExprBase; virtual;
         function GetSubExprCount : Integer; virtual;

      public
         function  Eval(exec : TdwsExecution) : Variant; virtual; abstract;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; virtual; abstract;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; virtual; abstract;
         function  EvalAsFloat(exec : TdwsExecution) : Double; virtual; abstract;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); overload; virtual; abstract;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); overload; virtual; abstract;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); virtual; abstract;

         procedure AssignValue(exec : TdwsExecution; const value : Variant); virtual; abstract;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); virtual; abstract;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); virtual; abstract;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); virtual; abstract;
         procedure AssignValueAsString(exec : TdwsExecution; const value : String); virtual; abstract;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); virtual; abstract;

         property SubExpr[i : Integer] : TExprBase read GetSubExpr;
         property SubExprCount : Integer read GetSubExprCount;

         function ScriptPos : TScriptPos; virtual; abstract;
         function ScriptLocation(prog : TObject) : String; virtual; abstract;

         class function CallStackToString(const callStack : TdwsExprLocationArray) : String; static;

         procedure RecursiveEnumerateSubExprs(const callback : TExprBaseEnumeratorProc);
         function IndexOfSubExpr(expr : TExprBase) : Integer;
   end;

   TExprBaseClass = class of TExprBase;

   // TExprBaseList
   //
   PExprBaseListRec = ^TExprBaseListRec;
   TExprBaseListRec = record
      private
         FList : TTightList;

         function GetExprBase(const x : Integer) : TExprBase; inline;
         procedure SetExprBase(const x : Integer; expr : TExprBase);

      public
         procedure Clean;

         function Add(expr : TExprBase) : Integer; inline;
         procedure Delete(index : Integer);

         property ExprBase[const x : Integer] : TExprBase read GetExprBase write SetExprBase; default;
         property Count : Integer read FList.FCount;
   end;

   // TExprBaseList
   //
   TExprBaseList = ^TExprBaseListExec;
   TExprBaseListExec = record
      private
         FList : PExprBaseListRec;
         FExec : TdwsExecution;

         function GetExprBase(const x : Integer): TExprBase; {$IFNDEF VER200}inline;{$ENDIF} // D2009 Compiler bug workaround
         procedure SetExprBase(const x : Integer; expr : TExprBase); inline;
         function GetCount : Integer; inline;

         function GetAsInteger(const x : Integer) : Int64;
         procedure SetAsInteger(const x : Integer; const value : Int64);
         function GetAsBoolean(const x : Integer) : Boolean;
         procedure SetAsBoolean(const x : Integer; const value : Boolean);
         function GetAsFloat(const x : Integer) : Double;
         procedure SetAsFloat(const x : Integer; const value : Double);
         function GetAsString(const x : Integer) : String;
         procedure SetAsString(const x : Integer; const value : String);
         function GetAsDataString(const x : Integer) : RawByteString;

      public
         property List : PExprBaseListRec read FList write FList;
         property Exec : TdwsExecution read FExec write FExec;

         property ExprBase[const x : Integer] : TExprBase read GetExprBase write SetExprBase; default;
         property Count : Integer read GetCount;

         property AsInteger[const x : Integer] : Int64 read GetAsInteger write SetAsInteger;
         property AsBoolean[const x : Integer] : Boolean read GetAsBoolean write SetAsBoolean;
         property AsFloat[const x : Integer] : Double read GetAsFloat write SetAsFloat;
         property AsString[const x : Integer] : String read GetAsString write SetAsString;
         property AsDataString[const x : Integer] : RawByteString read GetAsDataString;
   end;

   // All functions callable from the script implement this interface
   IExecutable = interface
      ['{8D534D18-4C6B-11D5-8DCB-0000216D9E86}']
      procedure InitSymbol(symbol: TSymbol);
      procedure InitExpression(Expr: TExprBase);
   end;

   IBooleanEvalable = interface (IExecutable)
      ['{6D0552ED-6FBD-4BC7-AADA-8D8F8DBDF29B}']
      function EvalAsBoolean(exec : TdwsExecution) : Boolean;
   end;

   IStringEvalable = interface (IExecutable)
      ['{6D0552ED-6FBD-4BC7-AADA-8D8F8DBDF29B}']
      procedure EvalAsString(exec : TdwsExecution; var Result : String);
   end;

   TAddrGeneratorSign = (agsPositive, agsNegative);

   // TAddrGenerator
   //
   TAddrGeneratorRec = record
      private
         FDataSize : Integer;
         FLevel : SmallInt;
         FSign : TAddrGeneratorSign;

      public
         constructor CreatePositive(aLevel : SmallInt; anInitialSize : Integer = 0);
         constructor CreateNegative(aLevel : SmallInt);

         function GetStackAddr(size : Integer) : Integer;

         property DataSize : Integer read FDataSize;
         property Level : SmallInt read FLevel;
   end;
   TAddrGenerator = ^TAddrGeneratorRec;

   TClassVisibility = (cvMagic, cvPrivate, cvProtected, cvPublic, cvPublished);

   // TSymbol
   //
   // Named item in the script
   TSymbol = class
      strict private
         FName : String;

      protected
         FTyp : TTypeSymbol;
         FSize : Integer;

         function GetCaption : String; virtual;
         function GetDescription : String; virtual;

      public
         constructor Create(const aName : String; aType : TTypeSymbol);

         procedure Initialize(const msgs : TdwsCompileMessageList); virtual;
         function  BaseType : TTypeSymbol; virtual;
         procedure SetName(const newName : String);

         class function IsBaseType : Boolean; virtual;

         function QualifiedName : String; virtual;

         function IsVisibleFor(const aVisibility : TClassVisibility) : Boolean; virtual;

         property Caption : String read GetCaption;
         property Description : String read GetDescription;
         property Name : String read FName;
         property Typ : TTypeSymbol read FTyp write FTyp;
         property Size : Integer read FSize;
   end;

   TSymbolClass = class of TSymbol;

   // All Symbols containing a value
   TValueSymbol = class (TSymbol)
      protected
         function GetCaption : String; override;
         function GetDescription : String; override;
   end;

   // named constant: const x = 123;
   TConstSymbol = class (TValueSymbol)
      protected
         FData : TData;

         function GetCaption : String; override;
         function GetDescription : String; override;

      public
         constructor Create(const name : string; typ : TTypeSymbol; const value : Variant); overload;
         constructor Create(const name : string; typ : TTypeSymbol; const data : TData; addr: Integer); overload;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         property Data : TData read FData;
   end;
   TConstSymbolClass = class of TConstSymbol;

   // variable: var x: Integer;
   TDataSymbol = class (TValueSymbol)
      protected
         FStackAddr : Integer;
         FLevel : SmallInt;

         function GetDescription : String; override;

      public
         property Level : SmallInt read FLevel write FLevel;
         property StackAddr: Integer read FStackAddr write FStackAddr;
   end;

   // parameter: procedure P(x: Integer);
   TParamSymbol = class (TDataSymbol)
      protected
         function GetDescription : String; override;
   end;

   TParamSymbolWithDefaultValue = class(TParamSymbol)
      private
         FDefaultValue : TData;

      protected
         function GetDescription : String; override;

      public
         procedure SetDefaultValue(const Data: TData; Addr: Integer); overload;
         procedure SetDefaultValue(const Value: Variant); overload;

         property DefaultValue : TData read FDefaultValue;
   end;

   // const/var parameter: procedure P(const/var x: Integer)
   TByRefParamSymbol = class(TParamSymbol)
      public
         constructor Create(const Name: string; Typ: TTypeSymbol);
   end;

   // lazy parameter: procedure P(lazy x: Integer)
   TLazyParamSymbol = class sealed (TParamSymbol)
      protected
         function GetDescription : String; override;
   end;

   // const parameter: procedure P(const x: Integer)
   TConstParamSymbol = class sealed (TByRefParamSymbol)
      protected
         function GetDescription : string; override;
   end;

   // var parameter: procedure P(var x: Integer)
   TVarParamSymbol = class sealed (TByRefParamSymbol)
      protected
         function GetDescription : String; override;
   end;

   // variable with functions for read/write: var x: integer; extern 'type' in 'selector';
   TExternalVarSymbol = class(TValueSymbol)
      private
         FReadFunc : TFuncSymbol;
         FWriteFunc : TFuncSymbol;

      protected
         function GetReadFunc : TFuncSymbol; virtual;
         function GetWriteFunc : TFuncSymbol; virtual;

      public
         destructor Destroy; override;

         property ReadFunc : TFuncSymbol read GetReadFunc write FReadFunc;
         property WriteFunc : TFuncSymbol read GetWriteFunc write FWriteFunc;
   end;

   TTypeSymbolClass = class of TTypeSymbol;

   // Base class for all types
   TTypeSymbol = class(TSymbol)
      public
         procedure InitData(const data : TData; offset : Integer); virtual; abstract;
         function BaseType : TTypeSymbol; override;
         function UnAliasedType : TTypeSymbol; virtual;
         function IsOfType(typSym : TTypeSymbol) : Boolean; virtual;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; virtual;
   end;

   TFuncKind = (fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod);

   // Record used for TFuncSymbol.Generate
   PParamRec = ^TParamRec;
   TParamRec = record
      ParamName : String;
      ParamType : String;
      IsVarParam : Boolean;
      IsConstParam : Boolean;
      HasDefaultValue : Boolean;
      DefaultValue : TData;
   end;
   TParamArray = array of TParamRec;

   // Condition, as part of contracts
   TConditionSymbol = class (TSymbol)
      private
         FScriptPos : TScriptPos;
         FCondition : IBooleanEvalable;
         FMessage : IStringEvalable;

      protected

      public
         constructor Create(const pos : TScriptPos; const cond : IBooleanEvalable; const msg : IStringEvalable);

         property ScriptPos : TScriptPos read FScriptPos write FScriptPos;
         property Condition : IBooleanEvalable read FCondition write FCondition;
         property Message : IStringEvalable read FMessage write FMessage;
   end;
   TConditionSymbolClass = class of TConditionSymbol;

   TPreConditionSymbol = class (TConditionSymbol)
      private

      protected

      public

   end;

   TPostConditionSymbol = class (TConditionSymbol)
      private

      protected

      public

   end;

   TClassInvariantSymbol = class (TConditionSymbol)
      private

      protected

      public

   end;

   TFuncSymbolFlag = (fsfStateless, fsfExternal);
   TFuncSymbolFlags = set of TFuncSymbolFlag;

   // A script function / procedure: procedure X(param: Integer);
   TFuncSymbol = class (TTypeSymbol)
      protected
         FAddrGenerator : TAddrGeneratorRec;
         FExecutable : IExecutable;
         FInternalParams : TSymbolTable;
         FDeprecatedMessage : String;
         FForwardPosition : PScriptPos;
         FParams : TParamsSymbolTable;
         FResult : TDataSymbol;
         FConditions : TConditionsSymbolTable;
         FFlags : TFuncSymbolFlags;
         FKind : TFuncKind;

         procedure SetType(const Value: TTypeSymbol);
         function GetCaption : String; override;
         function GetIsForwarded : Boolean;
         function GetDescription : String; override;
         function GetLevel: SmallInt; inline;
         function GetParamSize : Integer; inline;
         function GetIsDeprecated : Boolean; inline;
         procedure SetIsDeprecated(const val : Boolean);
         function GetIsStateless : Boolean; inline;
         procedure SetIsStateless(const val : Boolean);
         function GetIsExternal : Boolean; inline;
         procedure SetIsExternal(const val : Boolean);
         function GetSourcePosition : TScriptPos; virtual;
         procedure SetSourcePosition(const val : TScriptPos); virtual;

         function GetSourceSubExpr(i : Integer) : TExprBase;
         function GetSourceSubExprCount : Integer;
         property SubExpr[i : Integer] : TExprBase read GetSourceSubExpr;
         property SubExprCount : Integer read GetSourceSubExprCount;

      public
         constructor Create(const Name: string; FuncKind: TFuncKind; FuncLevel: SmallInt);
         destructor Destroy; override;

         constructor Generate(Table: TSymbolTable; const FuncName: string;
                              const FuncParams: TParamArray; const FuncType: string);
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure AddParam(param: TParamSymbol); virtual;
         procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         procedure InitData(const Data : TData; Offset : Integer); override;
         procedure AddCondition(cond : TConditionSymbol);

         function  ParamsDescription : String;

         procedure SetForwardedPos(const pos : TScriptPos);
         procedure ClearIsForwarded;

         property Executable : IExecutable read FExecutable write FExecutable;
         property DeprecatedMessage : String read FDeprecatedMessage write FDeprecatedMessage;
         property IsDeprecated : Boolean read GetIsDeprecated write SetIsDeprecated;
         property IsStateless : Boolean read GetIsStateless write SetIsStateless;
         property IsForwarded : Boolean read GetIsForwarded;
         property IsExternal : Boolean read GetIsExternal write SetIsExternal;
         property Kind : TFuncKind read FKind write FKind;
         property Level : SmallInt read GetLevel;
         property InternalParams : TSymbolTable read FInternalParams;
         property Params : TParamsSymbolTable read FParams;
         property ParamSize : Integer read GetParamSize;
         property Result : TDataSymbol read FResult;
         property Typ : TTypeSymbol read FTyp write SetType;
         property Conditions : TConditionsSymbolTable read FConditions;
         property SourcePosition : TScriptPos read GetSourcePosition write SetSourcePosition;
   end;

   TSourceFuncSymbol = class(TFuncSymbol)
      private
         FSourcePosition : TScriptPos;

      protected
         function GetSourcePosition : TScriptPos; override;
         procedure SetSourcePosition(const val : TScriptPos); override;

      public
         property SubExpr;
         property SubExprCount;
   end;

   TMagicFuncDoEvalEvent = function(args : TExprBaseList) : Variant of object;
   TMagicProcedureDoEvalEvent = procedure(args : TExprBaseList) of object;
   TMagicFuncDoEvalAsIntegerEvent = function(args : TExprBaseList) : Int64 of object;
   TMagicFuncDoEvalAsBooleanEvent = function(args : TExprBaseList) : Boolean of object;
   TMagicFuncDoEvalAsFloatEvent = procedure(args : TExprBaseList; var Result : Double) of object;
   TMagicFuncDoEvalAsStringEvent = procedure(args : TExprBaseList; var Result : String) of object;

   // TMagicFuncSymbol
   //
   TMagicFuncSymbol = class(TFuncSymbol)
      private
         FInternalFunction : TObject;
      public
         destructor Destroy; override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         property InternalFunction : TObject read FInternalFunction write FInternalFunction;
   end;

   TMethodKind = ( mkProcedure, mkFunction, mkConstructor, mkDestructor, mkMethod,
                   mkClassProcedure, mkClassFunction, mkClassMethod );
   TMethodAttribute = (maVirtual, maOverride, maReintroduce, maAbstract,
                       maOverlap, maClassMethod, maFinal);
   TMethodAttributes = set of TMethodAttribute;

   // A method of a script class: TMyClass = class procedure X(param: String); end;
   TMethodSymbol = class(TFuncSymbol)
      private
         FClassSymbol : TClassSymbol;
         FParentMeth : TMethodSymbol;
         FSelfSym : TDataSymbol;
         FVisibility : TClassVisibility;
         FAttributes : TMethodAttributes;
         FVMTIndex : Integer;

      protected
         function GetIsClassMethod : Boolean;

         function GetIsOverride : Boolean; inline;
         procedure SetIsOverride(const val : Boolean); inline;
         function GetIsOverlap : Boolean; inline;
         procedure SetIsOverlap(const val : Boolean); inline;
         function GetIsVirtual : Boolean; inline;
         procedure SetIsVirtual(const val : Boolean);
         function GetIsAbstract : Boolean; inline;
         procedure SetIsAbstract(const val : Boolean); inline;
         function GetIsFinal : Boolean; inline;

         function GetDescription : String; override;

      public
         constructor Create(const Name: string; FuncKind: TFuncKind; aClassSym : TClassSymbol;
                            aVisibility : TClassVisibility; isClassMethod : Boolean;
                            FuncLevel: SmallInt = 1); virtual;
         constructor Generate(Table: TSymbolTable; MethKind: TMethodKind;
                              const Attributes: TMethodAttributes; const MethName: string;
                              const MethParams: TParamArray; const MethType: string;
                              Cls: TClassSymbol; aVisibility : TClassVisibility);

         procedure SetOverride(meth: TMethodSymbol);
         procedure SetOverlap(meth: TMethodSymbol);
         procedure SetIsFinal;
         procedure InitData(const data : TData; offset : Integer); override;
         function QualifiedName : String; override;
         function HasConditions : Boolean;
         function IsVisibleFor(const aVisibility : TClassVisibility) : Boolean; override;

         property ClassSymbol : TClassSymbol read FClassSymbol;
         property VMTIndex : Integer read FVMTIndex;
         property IsAbstract : Boolean read GetIsAbstract write SetIsAbstract;
         property IsVirtual : Boolean read GetIsVirtual write SetIsVirtual;
         property IsOverride : Boolean read GetIsOverride;
         property IsFinal : Boolean read GetIsFinal;
         property IsOverlap : Boolean read GetIsOverlap;
         property IsClassMethod : Boolean read GetIsClassMethod;
         property ParentMeth : TMethodSymbol read FParentMeth;
         property SelfSym : TDataSymbol read FSelfSym;
         property Visibility : TClassVisibility read FVisibility;
      end;

   TSourceMethodSymbol = class(TMethodSymbol)
      private
         FDeclarationPos : TScriptPos;
         FSourcePosition : TScriptPos;

      protected
         function GetSourcePosition : TScriptPos; override;
         procedure SetSourcePosition(const val : TScriptPos); override;

      public
         property DeclarationPos : TScriptPos read FDeclarationPos write FDeclarationPos;

         property SubExpr;
         property SubExprCount;
   end;

   TNameSymbol = class(TTypeSymbol)
   end;

   // type x = TMyType;
   TAliasSymbol = class(TNameSymbol)
      public
         function BaseType : TTypeSymbol; override;
         function UnAliasedType : TTypeSymbol; override;
         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsOfType(typSym : TTypeSymbol) : Boolean; override;
   end;

   // integer/string/float/boolean/variant
   TBaseSymbol = class(TNameSymbol)
      public
         constructor Create(const name : String);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         class function IsBaseType : Boolean; override;
   end;

   TBaseIntegerSymbol = class (TBaseSymbol)
      public
         constructor Create;

         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TBaseFloatSymbol = class (TBaseSymbol)
      public
         constructor Create;

         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TBaseStringSymbol = class (TBaseSymbol)
      public
         constructor Create;

         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TBaseBooleanSymbol = class (TBaseSymbol)
      public
         constructor Create;

         procedure InitData(const data : TData; offset : Integer); override;
   end;

   TBaseVariantSymbol = class (TBaseSymbol)
      public
         constructor Create(const name : String = '');

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
   end;

   IConnectorType = interface;

   IConnector = interface
      ['{8D534D1A-4C6B-11D5-8DCB-0000216D9E86}']
      function ConnectorCaption: string;
      function ConnectorName: string;
      function GetUnit(const UnitName: string): IConnectorType;
   end;

   TConnectorArgs = array of TData;

   IConnectorCall = interface
      ['{8D534D1B-4C6B-11D5-8DCB-0000216D9E86}']
      function Call(const Base: Variant; Args: TConnectorArgs): TData;
   end;

   IConnectorMember = interface
      ['{8D534D1C-4C6B-11D5-8DCB-0000216D9E86}']
      function Read(const Base: Variant): TData;
      procedure Write(const Base: Variant; const Data: TData);
   end;

   TConnectorParam = record
      IsVarParam : Boolean;
      TypSym : TTypeSymbol;
   end;

   TConnectorParamArray = array of TConnectorParam;

   IConnectorType = interface
     ['{8D534D1D-4C6B-11D5-8DCB-0000216D9E86}']
     function ConnectorCaption: string;
     function HasMethod(const MethodName: string; const Params: TConnectorParamArray;
                        var TypSym: TTypeSymbol): IConnectorCall;
     function HasMember(const MemberName: string; var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
     function HasIndex(const PropName: string; const Params: TConnectorParamArray;
                       var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
   end;

   TConnectorSymbol = class(TBaseVariantSymbol)
      private
         FConnectorType : IConnectorType;

      public
         constructor Create(const name : String; const connectorType : IConnectorType);

         property ConnectorType : IConnectorType read FConnectorType write FConnectorType;
   end;

   TArraySymbol = class(TTypeSymbol)
   end;

   // array of FTyp
   TDynamicArraySymbol = class(TArraySymbol)
   protected
     function GetCaption : String; override;
   public
     constructor Create(const Name: string; Typ: TTypeSymbol);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   // array [FLowBound..FHighBound] of FTyp
   TStaticArraySymbol = class(TArraySymbol)
   private
     FHighBound: Integer;
     FLowBound: Integer;
     FElementCount: Integer;
   protected
     function GetCaption : String; override;
   public
     constructor Create(const Name: string; Typ: TTypeSymbol; LowBound, HighBound: Integer);
     procedure InitData(const Data: TData; Offset: Integer); override;
     function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
     function IsOfType(typSym : TTypeSymbol) : Boolean; override;
     procedure AddElement;
     property HighBound: Integer read FHighBound;
     property LowBound: Integer read FLowBound;
     property ElementCount: Integer read FElementCount;
   end;

   // static array whose bounds are contextual
   TOpenArraySymbol = class (TStaticArraySymbol)
     constructor Create(const Name: string; Typ: TTypeSymbol);
     function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   // Member of a record
   TMemberSymbol = class(TValueSymbol)
      protected
         FRecordSymbol: TRecordSymbol;
         FOffset: Integer;
      public
         function QualifiedName : String; override;

         property Offset: Integer read FOffset write FOffset;
         property RecordSymbol: TRecordSymbol read FRecordSymbol write FRecordSymbol;
   end;

   // record member1: Integer; member2: Integer end;
   TRecordSymbol = class(TTypeSymbol)
      private
         FMembers : TSymbolTable;

      protected
         function GetCaption : String; override;
         function GetDescription : String; override;

      public
         constructor Create(const Name: string);
         destructor Destroy; override;

         procedure AddMember(Member: TMemberSymbol);
         function MemberAtOffset(offset : Integer) : TMemberSymbol;
         procedure InitData(const Data: TData; Offset: Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         property Members: TSymbolTable read FMembers;
   end;

   // Field of a script object
   TFieldSymbol = class(TValueSymbol)
      protected
         FClassSymbol: TClassSymbol;
         FVisibility : TClassVisibility;
         FOffset: Integer;

      public
         constructor Create(const name : String; typ : TTypeSymbol; aVisibility : TClassVisibility);

         function QualifiedName : String; override;
         function IsVisibleFor(const aVisibility : TClassVisibility) : Boolean; override;

         property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
         property Offset: Integer read FOffset;
         property Visibility : TClassVisibility read FVisibility write FVisibility;
   end;

   // Const attached to a class
   TClassConstSymbol = class(TConstSymbol)
      protected
         FClassSymbol : TClassSymbol;
         FVisibility : TClassVisibility;

      public
         function QualifiedName : String; override;
         function IsVisibleFor(const aVisibility : TClassVisibility) : Boolean; override;

         property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
         property Visibility : TClassVisibility read FVisibility write FVisibility;
   end;

   // property X: Integer read FReadSym write FWriteSym;
   TPropertySymbol = class(TValueSymbol)
      private
         FClassSymbol : TClassSymbol;
         FReadSym : TSymbol;
         FWriteSym : TSymbol;
         FArrayIndices : TSymbolTable;
         FIndexSym : TTypeSymbol;
         FIndexValue: TData;
         FVisibility : TClassVisibility;

      protected
         function GetCaption : String; override;
         function GetDescription : String; override;
         function GetIsDefault: Boolean;
         function GetArrayIndices : TSymbolTable;
         procedure AddParam(Param : TParamSymbol);

      public
         constructor Create(const name : String; typ : TTypeSymbol; aVisibility : TClassVisibility);
         destructor Destroy; override;

         procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
         procedure SetIndex(const Data: TData; Addr: Integer; Sym: TTypeSymbol);
         function GetArrayIndicesDescription: string;
         function QualifiedName : String; override;
         function IsVisibleFor(const aVisibility : TClassVisibility) : Boolean; override;
         function HasArrayIndices : Boolean;

         property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
         property Visibility : TClassVisibility read FVisibility write FVisibility;
         property ArrayIndices : TSymbolTable read GetArrayIndices;
         property ReadSym : TSymbol read FReadSym write FReadSym;
         property WriteSym : TSymbol read FWriteSym write FWriteSym;
         property IsDefault : Boolean read GetIsDefault;
         property IndexValue : TData read FIndexValue;
         property IndexSym : TTypeSymbol read FIndexSym;
   end;

   // class operator X (params) uses method;
   TClassOperatorSymbol = class(TSymbol)
      private
         FClassSymbol : TClassSymbol;
         FTokenType : TTokenType;
         FUsesSym : TMethodSymbol;

      protected
         function GetCaption : String; override;
         function GetDescription : String; override;

      public
         constructor Create(tokenType : TTokenType);
         function QualifiedName : String; override;

         property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property UsesSym : TMethodSymbol read FUsesSym write FUsesSym;
   end;

   // type X = class of TMyClass;
   TClassOfSymbol = class sealed (TTypeSymbol)
      protected
         function GetCaption : String; override;
      public
         constructor Create(const Name: string; Typ: TClassSymbol);
         procedure InitData(const Data: TData; Offset: Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsOfType(typSym : TTypeSymbol) : Boolean; override;
         function TypClassSymbol : TClassSymbol; inline;
   end;

   TObjectDestroyEvent = procedure (ExternalObject: TObject) of object;

   TClassSymbolFlag = (csfAbstract, csfExplicitAbstract, csfSealed,
                       csfStatic, csfExternal);
   TClassSymbolFlags = set of TClassSymbolFlag;

   // type X = class ... end;
   TClassSymbol = class (TTypeSymbol)
      private
         FClassOfSymbol : TClassOfSymbol;
         FFlags : TClassSymbolFlags;
         FForwardPosition : PScriptPos;
         FMembers : TMembersSymbolTable;
         FOperators : TTightList;
         FScriptInstanceSize : Integer;
         FOnObjectDestroy : TObjectDestroyEvent;
         FParent : TClassSymbol;
         FDefaultProperty : TPropertySymbol;
         FVirtualMethodTable : array of TMethodSymbol;

      protected
         function CreateMembersTable : TMembersSymbolTable; virtual;
         function GetDescription : String; override;
         function GetIsForwarded : Boolean; inline;
         function GetIsExplicitAbstract : Boolean; inline;
         procedure SetIsExplicitAbstract(const val : Boolean); inline;
         function GetIsAbstract : Boolean; inline;
         function GetIsSealed : Boolean; inline;
         procedure SetIsSealed(const val : Boolean); inline;
         function GetIsStatic : Boolean; inline;
         procedure SetIsStatic(const val : Boolean); inline;
         function GetIsExternal : Boolean; inline;
         procedure SetIsExternal(const val : Boolean); inline;

         function AllocateVMTindex : Integer;

      public
         constructor Create(const name : String);
         destructor Destroy; override;

         procedure AddField(Sym: TFieldSymbol);
         procedure AddMethod(methSym : TMethodSymbol);
         procedure AddProperty(Sym: TPropertySymbol);
         procedure AddOperator(Sym: TClassOperatorSymbol);
         procedure AddConst(sym : TClassConstSymbol);

         function  FieldAtOffset(offset : Integer) : TFieldSymbol;
         procedure InheritFrom(ancestorClassSym : TClassSymbol);
         procedure InitData(const Data: TData; Offset: Integer); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function  IsOfType(typSym : TTypeSymbol) : Boolean; override;

         function  VMTMethod(index : Integer) : TMethodSymbol;
         function  VMTCount : Integer;

         procedure SetForwardedPos(const pos : TScriptPos);
         procedure ClearIsForwarded;

         function FindClassOperatorStrict(tokenType : TTokenType; paramType : TSymbol; recursive : Boolean) : TClassOperatorSymbol;
         function FindClassOperator(tokenType : TTokenType; paramType : TTypeSymbol) : TClassOperatorSymbol;

         class function VisibilityToString(visibility : TClassVisibility) : String; static;

         property ClassOf : TClassOfSymbol read FClassOfSymbol;
         property ScriptInstanceSize : Integer read FScriptInstanceSize;

         property IsForwarded : Boolean read GetIsForwarded;
         property IsExplicitAbstract : Boolean read GetIsExplicitAbstract write SetIsExplicitAbstract;
         property IsAbstract : Boolean read GetIsAbstract;
         property IsSealed : Boolean read GetIsSealed write SetIsSealed;
         property IsStatic : Boolean read GetIsStatic write SetIsStatic;
         property IsExternal : Boolean read GetIsExternal write SetIsExternal;

         property Members : TMembersSymbolTable read FMembers;
         property OnObjectDestroy : TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
         property Parent : TClassSymbol read FParent;
         property DefaultProperty : TPropertySymbol read FDefaultProperty write FDefaultProperty;
   end;

   // nil "class"
   TNilSymbol = class(TTypeSymbol)
      protected
         function GetCaption : String; override;

      public
         constructor Create;

         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   // Invisible symbol for units (e. g. for TdwsUnit)
   TUnitSymbol = class sealed (TNameSymbol)
      private
         FIsTableOwner: Boolean;
         FTable: TSymbolTable;
         FInitialized: Boolean;

      public
         constructor Create(const Name: string; Table: TSymbolTable; IsTableOwner: Boolean = False);
         destructor Destroy; override;

         procedure InitData(const data : TData; offset : Integer); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         property Table: TSymbolTable read FTable write FTable;
   end;

   // Element of an enumeration type. E. g. "type DummyEnum = (Elem1, Elem2, Elem3);"
   TElementSymbol = class(TConstSymbol)
      private
         FIsUserDef: Boolean;
         FUserDefValue: Integer;

      protected
         function GetDescription : String; override;

      public
         constructor Create(const Name: string; Typ: TTypeSymbol; Value: Integer; IsUserDef: Boolean);
         property IsUserDef: Boolean read FIsUserDef;
         property UserDefValue: Integer read FUserDefValue;
   end;

   // Enumeration type. E. g. "type myEnum = (One, Two, Three);"
   TEnumerationSymbol = class sealed (TNameSymbol)
      private
         FElements : TSymbolTable;
         FLowBound, FHighBound : Integer;

      protected
         function GetCaption : String; override;
         function GetDescription : String; override;

      public
         constructor Create(const name : String; baseType : TTypeSymbol);
         destructor Destroy; override;

         function DefaultValue : Integer;
         procedure InitData(const data : TData; offset : Integer); override;
         function BaseType : TTypeSymbol; override;
         function IsOfType(typSym : TTypeSymbol) : Boolean; override;

         procedure AddElement(element : TElementSymbol);

         property Elements : TSymbolTable read FElements;
         property LowBound : Integer read FLowBound write FLowBound;
         property HighBound : Integer read FHighBound write FHighBound;
         function ShortDescription : String;
   end;

   IObjectOwner = interface
      procedure ReleaseObject;
   end;

   // A table of symbols connected to other symboltables (property Parents)
   TSymbolTable = class
      private
         FAddrGenerator : TAddrGenerator;
         FSymbols : TTightList;
         FParents : TTightList;
         FSymbolsSorted : Boolean;

         function GetParentCount : Integer;
         function GetParents(Index: Integer) : TSymbolTable;

      protected
         function GetSymbol(Index: Integer): TSymbol; inline;
         function GetCount : Integer; inline;

         procedure SortSymbols(minIndex, maxIndex : Integer);
         function FindLocalSorted(const name: string) : TSymbol;
         function FindLocalUnSorted(const name: string) : TSymbol;

      public
         constructor Create(Parent: TSymbolTable = nil; AddrGenerator: TAddrGenerator = nil);
         destructor Destroy; override;

         procedure InsertParent(Index: Integer; Parent: TSymbolTable); virtual;
         function RemoveParent(Parent: TSymbolTable): Integer; virtual;
         function IndexOfParent(Parent: TSymbolTable): Integer;
         procedure MoveParent(CurIndex, NewIndex: Integer);
         procedure ClearParents;
         procedure AddParent(Parent: TSymbolTable);

         function AddSymbol(Sym: TSymbol): Integer;
         function AddSymbolDirect(sym : TSymbol) : Integer;
         function FindLocal(const aName : String; ofClass : TSymbolClass = nil) : TSymbol; virtual;
         function FindTypeLocal(const aName : String) : TTypeSymbol;
         function FindSymbolAtStackAddr(const stackAddr, level : Integer) : TDataSymbol;
         function Remove(Sym: TSymbol): Integer;
         procedure Clear;

         function FindSymbol(const aName : String; minVisibility : TClassVisibility;
                             ofClass : TSymbolClass = nil) : TSymbol; virtual;
         function FindTypeSymbol(const aName : string; minVisibility : TClassVisibility) : TTypeSymbol;

         function HasClass(const aClass : TSymbolClass) : Boolean;

         procedure Initialize(const msgs : TdwsCompileMessageList); virtual;

         property AddrGenerator : TAddrGenerator read FAddrGenerator;
         property Count : Integer read GetCount;
         property Symbols[x : Integer] : TSymbol read GetSymbol; default;
         property ParentCount : Integer read GetParentCount;
         property Parents[Index : Integer] : TSymbolTable read GetParents;

         type
            TSymbolTableEnumerator = record
               Index : Integer;
               Table : TSymbolTable;
               function MoveNext : Boolean;
               function GetCurrent : TSymbol;
               property Current : TSymbol read GetCurrent;
            end;
         function GetEnumerator : TSymbolTableEnumerator;
   end;

   TSymbolTableClass = class of TSymbolTable;

   // TMembersSymbolTable
   //
   TMembersSymbolTable = class (TSymbolTable)
      private
         FClassSym : TClassSymbol;

      public
         procedure AddParent(parent : TMembersSymbolTable);
         function FindSymbol(const aName : String; minVisibility : TClassVisibility; ofClass : TSymbolClass = nil) : TSymbol; override;
         function FindSymbolFromClass(const aName : String; fromClass : TClassSymbol) : TSymbol; reintroduce;

         property ClassSym : TClassSymbol read FClassSym write FClassSym;
   end;

   // TUnSortedSymbolTable
   //
   TUnSortedSymbolTable = class (TSymbolTable)
      public
         function FindLocal(const aName : String; ofClass : TSymbolClass = nil) : TSymbol; override;
   end;

   // TConditionsSymbolTable
   //
   TConditionsSymbolTable = class (TUnSortedSymbolTable)
   end;

   // TParamsSymbolTable
   //
   TParamsSymbolTable = class (TUnSortedSymbolTable)
   end;

   // TProgramSymbolTable
   //
   TProgramSymbolTable = class (TSymbolTable)
      private
         FSystemTable : TStaticSymbolTable;
         FDestructionList: TTightList;

      public
         constructor Create(Parent: TSymbolTable = nil; AddrGenerator: TAddrGenerator = nil);
         destructor Destroy; override;

         procedure AddToDestructionList(Sym: TSymbol);
   end;

   // TUnitSymbolTable
   //
   TUnitSymbolTable = class (TSymbolTable)
      private
         FObjects: TInterfaceList;

      public
         destructor Destroy; override;
         procedure BeforeDestruction; override;

         procedure AddObjectOwner(AOwner : IObjectOwner);
   end;

   TStaticSymbolTable = class (TUnitSymbolTable)
   private
     FRefCount: Integer;
     FInitialized: Boolean;
   public
     constructor Create(Parent: TStaticSymbolTable = nil; Reference: Boolean = True);
     destructor Destroy; override;
     procedure Initialize(const msgs : TdwsCompileMessageList); override;
     procedure InsertParent(Index: Integer; Parent: TSymbolTable); override;
     function RemoveParent(Parent: TSymbolTable): Integer; override;
     procedure _AddRef;
     procedure _Release;
   end;

   TLinkedSymbolTable = class (TUnitSymbolTable)
   private
     FParent: TStaticSymbolTable;
   public
     constructor Create(Parent: TStaticSymbolTable; AddrGenerator: TAddrGenerator = nil);
     destructor Destroy; override;
     function FindLocal(const Name: string; ofClass : TSymbolClass = nil): TSymbol; override;
     function FindSymbol(const Name: string; minVisibility : TClassVisibility; ofClass : TSymbolClass = nil): TSymbol; override;
     procedure Initialize(const msgs : TdwsCompileMessageList); override;
     property Parent: TStaticSymbolTable read FParent;
   end;

   IScriptObj = interface
      ['{8D534D1E-4C6B-11D5-8DCB-0000216D9E86}']
      function GetClassSym: TClassSymbol;
      function GetData: TData;
      function GetExternalObject: TObject;
      procedure SetExternalObject(value: TObject);
      function GetDestroyed : Boolean;
      procedure SetDestroyed(const val : Boolean);

      property ClassSym : TClassSymbol read GetClassSym;
      property Data : TData read GetData;
      property ExternalObject : TObject read GetExternalObject write SetExternalObject;
      property Destroyed : Boolean read GetDestroyed write SetDestroyed;

      function DataOfAddr(addr : Integer) : Variant;
      function DataOfAddrAsString(addr : Integer) : String;
      function DataOfAddrAsInteger(addr : Integer) : Int64;
      procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
   end;

   // The script has to be stopped because of an error
   EScriptError = class(Exception)
      private
         FScriptPos : TScriptPos;
         FScriptCallStack : TdwsExprLocationArray;
         FRawClassName : String;

      public
         constructor CreatePosFmt(const pos : TScriptPos; const Msg: string; const Args: array of const);

         property Pos : TScriptPos read FScriptPos write FScriptPos;
         property ScriptCallStack : TdwsExprLocationArray read FScriptCallStack write FScriptCallStack;
         property RawClassName : String read FRawClassName write FRawClassName;
   end;
   EScriptErrorClass = class of EScriptError;

   // Is thrown by "raise" statements in script code
   EScriptException = class(Exception)
      private
         FTyp: TSymbol;
         FValue: Variant;
         FPos: TScriptPos;
         FScriptCallStack : TdwsExprLocationArray;

      public
         constructor Create(const Message: string; const ExceptionObj: IScriptObj; const Pos: TScriptPos); overload;
         constructor Create(const Message: string; const Value: Variant; Typ: TTypeSymbol; const Pos: TScriptPos); overload;

         property ExceptionObj: Variant read FValue;
         property Value: Variant read FValue;
         property Typ: TSymbol read FTyp;
         property Pos: TScriptPos read FPos;
         property ScriptCallStack : TdwsExprLocationArray read FScriptCallStack;
   end;

   // Is thrown by failed Assert() statements in script code
   EScriptAssertionFailed = class(EScriptException)
   end;

const
   cFuncKindToString : array [Low(TFuncKind)..High(TFuncKind)] of String = (
      'function', 'procedure', 'constructor', 'destructor', 'method' );

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs;

// ------------------
// ------------------ TdwsExprLocation ------------------
// ------------------

// Line
//
function TdwsExprLocation.Line : Integer;
begin
   Result:=Expr.ScriptPos.Line;
end;

// SourceName
//
function TdwsExprLocation.SourceName : String;
begin
   Result:=Expr.ScriptPos.SourceFile.Name;
end;

// Location
//
function TdwsExprLocation.Location : String;
begin
   Result:=Expr.ScriptLocation(Prog);
end;

// ------------------
// ------------------ TExprBase ------------------
// ------------------

// CallStackToString
//
class function TExprBase.CallStackToString(const callStack : TdwsExprLocationArray) : String;
var
   i : Integer;
   buffer : TWriteOnlyBlockStream;
begin
   buffer:=TWriteOnlyBlockStream.Create;
   try
      for i:=0 to High(callStack) do begin
         if i>0 then
            buffer.WriteString(#13#10);
         buffer.WriteString(callStack[i].Expr.ScriptLocation(callStack[i].Prog));
      end;
      Result:=buffer.ToString;
   finally
      buffer.Free;
   end;
end;

// RecursiveEnumerateSubExprs
//
procedure TExprBase.RecursiveEnumerateSubExprs(const callback : TExprBaseEnumeratorProc);
var
   i : Integer;
   abort : Boolean;
   base, expr : TExprBase;
   stack : TSimpleStack<TExprBase>;
begin
   stack:=TSimpleStack<TExprBase>.Create;
   try
      abort:=False;
      stack.Push(Self);
      repeat
         base:=stack.Peek;
         stack.Pop;
         for i:=0 to base.SubExprCount-1 do begin
            expr:=base.SubExpr[i];
            if expr<>nil then begin
               stack.Push(expr);
               callback(base, expr, abort);
               if abort then Exit;
            end;
         end;
      until stack.Count=0;
   finally
      stack.Free;
   end;
end;

// IndexOfSubExpr
//
function TExprBase.IndexOfSubExpr(expr : TExprBase) : Integer;
var
   i : Integer;
begin
   for i:=0 to SubExprCount-1 do
      if SubExpr[i]=expr then
         Exit(i);
   Result:=-1;
end;

// GetSubExpr
//
function TExprBase.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=nil;
end;

// GetSubExprCount
//
function TExprBase.GetSubExprCount : Integer;
begin
   Result:=0;
end;

// ------------------
// ------------------ TExprBaseListRec ------------------
// ------------------

// Clean
//
procedure TExprBaseListRec.Clean;
begin
   FList.Clean;
end;

// Add
//
function TExprBaseListRec.Add(expr : TExprBase) : Integer;
begin
   Result:=FList.Add(expr);
end;

// Delete
//
procedure TExprBaseListRec.Delete(index : Integer);
begin
   FList.Delete(index);
end;

// GetExprBase
//
function TExprBaseListRec.GetExprBase(const x : Integer): TExprBase;
begin
   Result:=TExprBase(FList.List[x]);
end;

// SetExprBase
//
procedure TExprBaseListRec.SetExprBase(const x : Integer; expr : TExprBase);
begin
   FList.List[x]:=expr;
end;

// ------------------
// ------------------ TExprBaseListExec ------------------
// ------------------

// GetExprBase
//
function TExprBaseListExec.GetExprBase(const x: Integer): TExprBase;
begin
   Result:=FList.ExprBase[x];
end;

// SetExprBase
//
procedure TExprBaseListExec.SetExprBase(const x : Integer; expr : TExprBase);
begin
   FList.ExprBase[x]:=expr;
end;

// GetCount
//
function TExprBaseListExec.GetCount : Integer;
begin
   Result:=FList.Count;
end;

// GetAsInteger
//
function TExprBaseListExec.GetAsInteger(const x : Integer) : Int64;
begin
   Result:=ExprBase[x].EvalAsInteger(Exec);
end;

// SetAsInteger
//
procedure TExprBaseListExec.SetAsInteger(const x : Integer; const value : Int64);
begin
   ExprBase[x].AssignValueAsInteger(Exec, value);
end;

// GetAsBoolean
//
function TExprBaseListExec.GetAsBoolean(const x : Integer) : Boolean;
begin
   Result:=ExprBase[x].EvalAsBoolean(Exec);
end;

// SetAsBoolean
//
procedure TExprBaseListExec.SetAsBoolean(const x : Integer; const value : Boolean);
begin
   ExprBase[x].AssignValueAsBoolean(Exec, value);
end;

// GetAsFloat
//
function TExprBaseListExec.GetAsFloat(const x : Integer) : Double;
begin
   Result:=ExprBase[x].EvalAsFloat(Exec);
end;

// SetAsFloat
//
procedure TExprBaseListExec.SetAsFloat(const x : Integer; const value : Double);
begin
   ExprBase[x].AssignValueAsFloat(Exec, value);
end;

// GetAsString
//
function TExprBaseListExec.GetAsString(const x : Integer) : String;
begin
   ExprBase[x].EvalAsString(Exec, Result);
end;

// SetAsString
//
procedure TExprBaseListExec.SetAsString(const x : Integer; const value : String);
begin
   ExprBase[x].AssignValueAsString(Exec, value);
end;

// GetAsDataString
//
function TExprBaseListExec.GetAsDataString(const x : Integer) : RawByteString;
var
   ustr : String;
   i, n : Integer;
   pSrc : PChar;
   pDest : PByteArray;
begin
   ustr:=GetAsString(x);
   if ustr='' then Exit('');
   n:=Length(ustr);
   SetLength(Result, n);
   pSrc:=PChar(NativeUInt(ustr));
   pDest:=PByteArray(NativeUInt(Result));
   for i:=0 to n-1 do
      pDest[i]:=PByte(@pSrc[i])^;
end;

// ------------------
// ------------------ TSymbol ------------------
// ------------------

// Create
//
constructor TSymbol.Create(const aName : String; aType : TTypeSymbol);
begin
   UnifyAssignString(aName, FName);
   FTyp:=aType;
   if Assigned(aType) then
      FSize:=aType.FSize
   else FSize:=0;
end;

// GetCaption
//
function TSymbol.GetCaption : String;
begin
   Result:=FName;
end;

// GetDescription
//
function TSymbol.GetDescription : String;
begin
   Result:=Caption;
end;

// Initialize
//
procedure TSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
end;

// IsBaseType
//
class function TSymbol.IsBaseType : Boolean;
begin
   Result:=False;
end;

// QualifiedName
//
function TSymbol.QualifiedName : String;
begin
   Result:=Name;
end;

// IsVisibleFor
//
function TSymbol.IsVisibleFor(const aVisibility : TClassVisibility) : Boolean;
begin
   Result:=True;
end;

function TSymbol.BaseType: TTypeSymbol;
begin
  Result := nil;
end;

// SetName
//
procedure TSymbol.SetName(const newName : String);
begin
   Assert(FName='');
   FName:=newName;
end;

// ------------------
// ------------------ TRecordSymbol ------------------
// ------------------

// Create
//
constructor TRecordSymbol.Create;
begin
   inherited Create(Name, nil);
   FMembers:=TSymbolTable.Create(nil);
end;

// Destroy
//
destructor TRecordSymbol.Destroy;
begin
   FMembers.Free;
   inherited;
end;

// AddMember
//
procedure TRecordSymbol.AddMember(member : TMemberSymbol);
begin
   Member.RecordSymbol:=Self;
   Member.Offset:=FSize;
   FSize:=FSize+Member.Typ.Size;
   FMembers.AddSymbol(Member);
end;

// MemberAtOffset
//
function TRecordSymbol.MemberAtOffset(offset : Integer) : TMemberSymbol;
var
   i : Integer;
begin
   for i:=0 to Members.Count-1 do begin
      Result:=TMemberSymbol(Members[i]);
      if Result.Offset=offset then Exit;
   end;
   Result:=nil;
end;

// InitData
//
procedure TRecordSymbol.InitData(const data: TData; Offset: Integer);
var
   i : Integer;
   member : TMemberSymbol;
begin
   for i:=0 to FMembers.Count-1 do begin
      member:=TMemberSymbol(FMembers[i]);
      member.Typ.InitData(Data, offset+member.Offset);
   end;
end;

// IsCompatible
//
function TRecordSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
var
   i : Integer;
   otherRecordSym : TRecordSymbol;
begin
   typSym:=typSym.BaseType;
   if not (typSym is TRecordSymbol) then
      Exit(False);

   otherRecordSym:=TRecordSymbol(typSym);
   if FMembers.Count<>otherRecordSym.FMembers.Count then
      Exit(False);

   for i:=0 to FMembers.Count-1 do
      if not FMembers[i].Typ.IsCompatible(otherRecordSym.FMembers[i].Typ) then
         Exit(False);
   Result:=True;
end;

function TRecordSymbol.GetCaption: string;
var
  x: Integer;
begin
  Result := 'record';
  if FMembers.Count > 0 then
  begin
    Result := Result + ' ' + FMembers[0].Typ.Caption;
    for x := 1 to FMembers.Count - 1 do
      Result := Result + ', ' + FMembers[x].Typ.Caption;
  end;
  Result := Result + ' end';
end;

function TRecordSymbol.GetDescription: string;
var
   x: Integer;
begin
   Result:=Name+' = record'#13#10;
   for x:=0 to FMembers.Count-1 do
      Result:=Result+'   '+FMembers[x].Name+' : '+FMembers[x].Typ.Name+';'#13#10;
   Result:=Result+'end;';
end;

// ------------------
// ------------------ TFieldSymbol ------------------
// ------------------

// Create
//
constructor TFieldSymbol.Create(const Name: string; Typ: TTypeSymbol; aVisibility : TClassVisibility);
begin
   inherited Create(Name, Typ);
   FVisibility:=aVisibility;
end;

// QualifiedName
//
function TFieldSymbol.QualifiedName : String;
begin
   Result:=ClassSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TFieldSymbol.IsVisibleFor(const aVisibility : TClassVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// ------------------
// ------------------ TClassConstSymbol ------------------
// ------------------

// QualifiedName
//
function TClassConstSymbol.QualifiedName : String;
begin
   Result:=ClassSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TClassConstSymbol.IsVisibleFor(const aVisibility : TClassVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// ------------------
// ------------------ TFuncSymbol ------------------
// ------------------

// Create
//
constructor TFuncSymbol.Create(const Name: string; FuncKind: TFuncKind;
                               FuncLevel: SmallInt);
begin
   inherited Create(Name, nil);
   FKind := FuncKind;
   FAddrGenerator := TAddrGeneratorRec.CreateNegative(FuncLevel);
   FInternalParams := TUnSortedSymbolTable.Create(nil, @FAddrGenerator);
   FParams := TParamsSymbolTable.Create(FInternalParams, @FAddrGenerator);
   FSize := 1;
end;

destructor TFuncSymbol.Destroy;
begin
   if FForwardPosition<>nil then
      Dispose(FForwardPosition);
   FParams.Free;
   FInternalParams.Free;
   FConditions.Free;
   inherited;
end;

constructor TFuncSymbol.Generate(Table: TSymbolTable; const FuncName: string;
  const FuncParams: TParamArray; const FuncType: string);
var
  typSym: TTypeSymbol;
begin
  if FuncType <> '' then
  begin
    Self.Create(FuncName, fkFunction, 1);
    // Set function type
    typSym := Table.FindTypeSymbol(FuncType, cvMagic);
    if not (Assigned(typSym) and (typSym.BaseType <> nil)) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [FuncType]);
    Self.SetType(typSym);
  end
  else
    Self.Create(FuncName, fkProcedure, 1);

  GenerateParams(Table, FuncParams);
end;

procedure TFuncSymbol.AddParam(param: TParamSymbol);
begin
  Params.AddSymbol(param);
end;

procedure TFuncSymbol.SetType(const Value: TTypeSymbol);
begin
   FTyp:=Value;
   Assert(FResult=nil);
   if FTyp<>nil then begin
      FResult:=TDataSymbol.Create(SYS_RESULT, Value);
      FInternalParams.AddSymbol(FResult);
   end;
end;

type TAddParamProc = procedure (param: TParamSymbol) of object;

procedure GenerateParams(const Name: String; Table: TSymbolTable; const funcParams: TParamArray; AddProc: TAddParamProc);
var
   i : Integer;
   typSym : TTypeSymbol;
   paramSym : TParamSymbol;
   paramSymWithDefault : TParamSymbolWithDefaultValue;
   paramRec : PParamRec;
begin
   for i := 0 to Length(FuncParams) - 1 do begin

      paramRec:=@FuncParams[i];
      typSym := Table.FindTypeSymbol(paramRec.ParamType, cvMagic);
      if not Assigned(typSym) then
         raise Exception.CreateFmt(CPE_TypeForParamNotFound,
                                   [paramRec.ParamType, paramRec.ParamName, Name]);

      if paramRec.HasDefaultValue then begin

         if paramRec.IsVarParam then
            raise Exception.Create(CPE_VarParamCantHaveDefaultValue);
         if paramRec.IsConstParam then
            raise Exception.Create(CPE_ConstParamCantHaveDefaultValue);

         paramSymWithDefault := TParamSymbolWithDefaultValue.Create(paramRec.ParamName, typSym);
         paramSymWithDefault.SetDefaultValue(paramRec.DefaultValue, 0);
         paramSym:=paramSymWithDefault;

      end else begin

         if paramRec.IsVarParam then
            paramSym := TVarParamSymbol.Create(paramRec.ParamName, typSym)
         else if paramRec.IsConstParam then
            paramSym := TConstParamSymbol.Create(paramRec.ParamName, typSym)
         else paramSym := TParamSymbol.Create(paramRec.ParamName, typSym);

      end;

      AddProc(paramSym);

   end;
end;

// GenerateParams
//
procedure TFuncSymbol.GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
begin
   dwsSymbols.GenerateParams(Name,Table,FuncParams,AddParam);
end;

// GetCaption
//
function TFuncSymbol.GetCaption : string;
var
   i : Integer;
   nam : String;
begin
   if Name <> '' then
      nam := Name
   else nam := cFuncKindToString[Kind]+' ';

   if Params.Count > 0 then begin
      Result := Params[0].Typ.Caption;
      for i := 1 to Params.Count - 1 do
         Result := Result + ', ' + Params[i].Typ.Caption;
      Result := '(' + Result + ')';
   end else Result := '';

   if Typ <> nil then
      Result := nam + Result + ': ' + Typ.Name
   else Result := nam + Result;
end;

// GetIsForwarded
//
function TFuncSymbol.GetIsForwarded : Boolean;
begin
   Result:=Assigned(FForwardPosition);
end;

// GetDescription
//
function TFuncSymbol.GetDescription: string;
begin
   Result:=cFuncKindToString[Kind]+' '+Name+ParamsDescription;
   if Typ<>nil then
      Result:=Result+': '+Typ.Name;
end;

// Initialize
//
procedure TFuncSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   inherited;
   if IsExternal then Exit;
   FInternalParams.Initialize(msgs);
   if Assigned(FExecutable) then
      FExecutable.InitSymbol(Self)
   else if Level>=0 then
      msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_ForwardNotImplemented, [Name]);
end;

// GetLevel
//
function TFuncSymbol.GetLevel: SmallInt;
begin
   Result := FAddrGenerator.Level;
end;

// GetParamSize
//
function TFuncSymbol.GetParamSize: Integer;
begin
   Result := FAddrGenerator.DataSize;
end;

// GetIsDeprecated
//
function TFuncSymbol.GetIsDeprecated : Boolean;
begin
   Result:=(FDeprecatedMessage<>'');
end;

// SetIsDeprecated
//
procedure TFuncSymbol.SetIsDeprecated(const val : Boolean);
begin
   if val then
      FDeprecatedMessage:='!'
   else FDeprecatedMessage:='';
end;

// GetIsStateless
//
function TFuncSymbol.GetIsStateless : Boolean;
begin
   Result:=(fsfStateless in FFlags);
end;

// SetIsStateless
//
procedure TFuncSymbol.SetIsStateless(const val : Boolean);
begin
   if val then
      Include(FFlags, fsfStateless)
   else Exclude(FFlags, fsfStateless);
end;

// GetIsExternal
//
function TFuncSymbol.GetIsExternal : Boolean;
begin
   Result:=(fsfExternal in FFlags);
end;

// SetIsExternal
//
procedure TFuncSymbol.SetIsExternal(const val : Boolean);
begin
   if val then
      Include(FFlags, fsfExternal)
   else Exclude(FFlags, fsfExternal);
end;

// GetSourcePosition
//
function TFuncSymbol.GetSourcePosition : TScriptPos;
begin
   Result:=cNullPos;
end;

// SetSourcePosition
//
procedure TFuncSymbol.SetSourcePosition(const val : TScriptPos);
begin
   // ignore
end;

// GetSourceSubExpr
//
function TFuncSymbol.GetSourceSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=(FExecutable as TdwsProgram).InitExpr;
      1 : Result:=(FExecutable as TdwsProgram).Expr;
   else
      Result:=nil;
   end;
end;

// GetSourceSubExprCount
//
function TFuncSymbol.GetSourceSubExprCount : Integer;
begin
   Result:=2;
end;

// IsCompatible
//
function TFuncSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
var
   funcSym : TFuncSymbol;
   i : Integer;
   param, otherParam : TSymbol;
begin
   typSym := typSym.BaseType;
   if typSym is TNilSymbol then
      Result := True
   else begin
      Result := False;
      if not (typSym is TFuncSymbol) then
         Exit;
      funcSym := TFuncSymbol(typSym);
      if (Kind <> funcSym.Kind) or (Params.Count <> funcSym.Params.Count) then
         Exit;
      if Typ <> funcSym.Typ then Exit;
      for i:=0 to Params.Count-1 do begin
         param:=Params[i];
         otherParam:=funcSym.Params[i];
         if param.ClassType<>otherParam.ClassType then Exit;
         if param.Typ<>otherParam.Typ then Exit;
      end;
      Result := True;
   end;
end;

procedure TFuncSymbol.InitData(const Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  Data[Offset] := nilIntf;
end;

// AddCondition
//
procedure TFuncSymbol.AddCondition(cond : TConditionSymbol);
begin
   if FConditions=nil then
      FConditions:=TConditionsSymbolTable.Create(nil, @FAddrGenerator);
   FConditions.AddSymbol(cond);
end;

// ParamsDescription
//
function TFuncSymbol.ParamsDescription : String;
var
   i : Integer;
begin
   if Params.Count>0 then begin
      Result:=Params.Symbols[0].Description;
      for i:=1 to Params.Count-1 do
         Result:=Result+'; '+Params.Symbols[i].Description;
      Result:='('+Result+')';
  end else Result:='()';
end;

// SetForwardedPos
//
procedure TFuncSymbol.SetForwardedPos(const pos : TScriptPos);
begin
   if FForwardPosition=nil then
      New(FForwardPosition);
   FForwardPosition^:=pos;
end;

// ClearIsForwarded
//
procedure TFuncSymbol.ClearIsForwarded;
begin
   Dispose(FForwardPosition);
   FForwardPosition:=nil;
end;

// ------------------
// ------------------ TSourceFuncSymbol ------------------
// ------------------

// GetSourcePosition
//
function TSourceFuncSymbol.GetSourcePosition : TScriptPos;
begin
   Result:=FSourcePosition;
end;

// SetSourcePosition
//
procedure TSourceFuncSymbol.SetSourcePosition(const val : TScriptPos);
begin
   FSourcePosition:=val;
end;

// ------------------
// ------------------ TMagicFuncSymbol ------------------
// ------------------

procedure TMagicFuncSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   FInternalParams.Initialize(msgs);
end;

// Destroy
//
destructor TMagicFuncSymbol.Destroy;
begin
   FreeAndNil(FInternalFunction);
   inherited;
end;

// ------------------
// ------------------ TMethodSymbol ------------------
// ------------------

// Create
//
constructor TMethodSymbol.Create(const Name: string; FuncKind: TFuncKind;
  aClassSym : TClassSymbol; aVisibility : TClassVisibility; isClassMethod : Boolean;
  FuncLevel: SmallInt);
begin
   inherited Create(Name, FuncKind, FuncLevel);
   FClassSymbol := aClassSym;
   if isClassMethod then begin
      Include(FAttributes, maClassMethod);
      FSelfSym := TDataSymbol.Create(SYS_SELF, aClassSym.ClassOf);
   end else FSelfSym := TDataSymbol.Create(SYS_SELF, aClassSym);
   FInternalParams.AddSymbol(FSelfSym);
   FSize := 2; // code + data
   FParams.AddParent(FClassSymbol.Members);
   FVisibility:=aVisibility;
   FVMTIndex:=-1;
end;

constructor TMethodSymbol.Generate(Table: TSymbolTable; MethKind: TMethodKind;
  const Attributes: TMethodAttributes; const MethName: string; const MethParams: TParamArray;
  const MethType: string; Cls: TClassSymbol; aVisibility : TClassVisibility);
var
   typSym : TTypeSymbol;
   meth : TSymbol;
begin
   // Check if name is already used
   meth:=Cls.Members.FindSymbol(MethName, cvPrivate);
   if meth<>nil then begin
      if meth is TFieldSymbol then
         raise Exception.CreateFmt(CPE_FieldExists, [MethName])
      else if meth is TPropertySymbol then
         raise Exception.CreateFmt(CPE_PropertyExists, [MethName])
      else if meth is TMethodSymbol then begin
         if TMethodSymbol(meth).ClassSymbol = Cls then
            raise Exception.CreateFmt(CPE_MethodExists, [MethName]);
      end;
   end;

   // Initialize MethodSymbol
   case MethKind of
      mkConstructor:
         Create(MethName, fkConstructor, Cls, aVisibility, False);
      mkDestructor:
         Create(MethName, fkDestructor, Cls, aVisibility, False);
      mkProcedure:
         Create(MethName, fkProcedure, Cls, aVisibility, False);
      mkFunction:
         Create(MethName, fkFunction, Cls, aVisibility, False);
      mkMethod :
         Create(MethName, fkMethod, Cls, aVisibility, False);
      mkClassProcedure:
         Create(MethName, fkProcedure, Cls, aVisibility, True);
      mkClassFunction:
         Create(MethName, fkFunction, Cls, aVisibility, True);
      mkClassMethod:
         Create(MethName, fkMethod, Cls, aVisibility, True);
   else
      Assert(False);
   end;

   // Set Resulttype
   if MethType <> '' then begin
      if not (Kind in [fkFunction, fkMethod]) then
         raise Exception.Create(CPE_NoResultTypeRequired);

      typSym := Table.FindTypeSymbol(MethType, cvMagic);
      if not Assigned(typSym) then
         raise Exception.CreateFmt(CPE_TypeIsUnknown, [MethType]);
      SetType(typSym);
   end;

   if (Kind = fkFunction) and (MethType = '') then
      raise Exception.Create(CPE_ResultTypeExpected);

   GenerateParams(Table, MethParams);

   if Assigned(meth) then
      SetOverlap(TMethodSymbol(meth));

   if Attributes = [maVirtual] then
      IsVirtual := True
   else if Attributes = [maVirtual, maAbstract] then begin
      IsVirtual := True;
      IsAbstract := True;
   end else if Attributes = [maOverride] then begin
      if IsOverlap then
         SetOverride(TMethodSymbol(meth))
      else raise Exception.CreateFmt(CPE_CanNotOverride, [Name]);
   end else if Attributes = [maReintroduce] then
      //
   else if Attributes = [] then
      //
   else raise Exception.Create(CPE_InvalidArgCombination);
end;

// GetIsClassMethod
//
function TMethodSymbol.GetIsClassMethod: Boolean;
begin
   Result:=(maClassMethod in FAttributes);
end;

// GetIsOverride
//
function TMethodSymbol.GetIsOverride : Boolean;
begin
   Result:=maOverride in FAttributes;
end;

// SetIsOverride
//
procedure TMethodSymbol.SetIsOverride(const val : Boolean);
begin
   if val then
      Include(FAttributes, maOverride)
   else Exclude(FAttributes, maOverride);
end;

// GetIsOverlap
//
function TMethodSymbol.GetIsOverlap : Boolean;
begin
   Result:=maOverlap in FAttributes;
end;

// SetIsOverlap
//
procedure TMethodSymbol.SetIsOverlap(const val : Boolean);
begin
   if val then
      Include(FAttributes, maOverlap)
   else Exclude(FAttributes, maOverlap);
end;

// GetIsVirtual
//
function TMethodSymbol.GetIsVirtual : Boolean;
begin
   Result:=maVirtual in FAttributes;
end;

// SetIsVirtual
//
procedure TMethodSymbol.SetIsVirtual(const val : Boolean);
begin
   if val then begin
      Include(FAttributes, maVirtual);
      if FVMTIndex<0 then begin
         FVMTIndex:=ClassSymbol.AllocateVMTindex;
         ClassSymbol.FVirtualMethodTable[FVMTIndex]:=Self;
      end;
   end else Exclude(FAttributes, maVirtual);
end;

// GetIsAbstract
//
function TMethodSymbol.GetIsAbstract : Boolean;
begin
   Result:=maAbstract in FAttributes;
end;

// SetIsAbstract
//
procedure TMethodSymbol.SetIsAbstract(const val : Boolean);
begin
   if val then
      Include(FAttributes, maAbstract)
   else Exclude(FAttributes, maAbstract);
end;

// GetIsFinal
//
function TMethodSymbol.GetIsFinal : Boolean;
begin
   Result:=maFinal in FAttributes;
end;

// SetIsFinal
//
procedure TMethodSymbol.SetIsFinal;
begin
   Include(FAttributes, maFinal);
end;

// GetDescription
//
function TMethodSymbol.GetDescription: string;
begin
   Result:=inherited GetDescription;
   if IsClassMethod then
      Result:='class '+Result;
end;

procedure TMethodSymbol.InitData(const Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  inherited;
  if Size = 2 then
    Data[Offset + 1] := nilIntf;
end;

// QualifiedName
//
function TMethodSymbol.QualifiedName : String;
begin
   Result:=ClassSymbol.QualifiedName+'.'+Name;
end;

// HasConditions
//
function TMethodSymbol.HasConditions : Boolean;
begin
   Result:=(FConditions<>nil);
   if (not Result) and IsOverride and (ParentMeth<>nil) then
      Result:=ParentMeth.HasConditions;
end;

// IsVisibleFor
//
function TMethodSymbol.IsVisibleFor(const aVisibility : TClassVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// SetOverride
//
procedure TMethodSymbol.SetOverride(meth: TMethodSymbol);
begin
   FParentMeth:=meth;
   FVMTIndex:=meth.FVMTIndex;
   IsVirtual:=True;
   SetIsOverride(True);
   SetIsOverlap(False);

   // make array unique
   SetLength(ClassSymbol.FVirtualMethodTable, Length(ClassSymbol.FVirtualMethodTable));
   ClassSymbol.FVirtualMethodTable[FVMTIndex]:=Self;
end;

// SetOverlap
//
procedure TMethodSymbol.SetOverlap(meth: TMethodSymbol);
begin
   FParentMeth := meth;
   SetIsOverride(False);
   SetIsOverlap(True);
end;

// ------------------
// ------------------ TSourceMethodSymbol ------------------
// ------------------

// GetSourcePosition
//
function TSourceMethodSymbol.GetSourcePosition : TScriptPos;
begin
   Result:=FSourcePosition;
end;

// SetSourcePosition
//
procedure TSourceMethodSymbol.SetSourcePosition(const val : TScriptPos);
begin
   FSourcePosition:=val;
end;

// ------------------
// ------------------ TPropertySymbol ------------------
// ------------------

// Create
//
constructor TPropertySymbol.Create(const Name: string; Typ: TTypeSymbol; aVisibility : TClassVisibility);
begin
   inherited Create(Name, Typ);
   FIndexValue:=nil;
   FVisibility:=aVisibility;
end;

destructor TPropertySymbol.Destroy;
begin
  FArrayIndices.Free;
  inherited;
end;

// GetArrayIndices
//
function TPropertySymbol.GetArrayIndices : TSymbolTable;
begin
   if FArrayIndices=nil then
      FArrayIndices:=TSymbolTable.Create;
   Result:=FArrayIndices;
end;

procedure TPropertySymbol.AddParam(Param: TParamSymbol);
begin
   ArrayIndices.AddSymbol(Param);
end;

procedure TPropertySymbol.GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
begin
   dwsSymbols.GenerateParams(Name,Table,FuncParams,AddParam);
end;

function TPropertySymbol.GetCaption: string;
begin
   Result := GetDescription;
end;

function TPropertySymbol.GetArrayIndicesDescription: string;
var
   i, j : Integer;
   sym, nextSym : TSymbol;
begin
   if (FArrayIndices=nil) or (ArrayIndices.Count=0) then
      Result:=''
   else begin
      Result:='[';
      i:=0;
      while i<ArrayIndices.Count do begin
         sym:=ArrayIndices[i];
         if i>0 then
            Result:=Result+', ';
         Result:=Result+sym.Name;
         for j:=i+1 to ArrayIndices.Count-1 do begin
            nextSym:=ArrayIndices[j];
            if nextSym.Typ<>sym.Typ then Break;
            Result:=Result+', '+nextSym.Name;
            i:=j;
         end;
         Result:=Result+': '+sym.Typ.Name;
         Inc(i);
      end;
      Result:=Result+']';
    end;
end;

// QualifiedName
//
function TPropertySymbol.QualifiedName : String;
begin
   Result:=ClassSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TPropertySymbol.IsVisibleFor(const aVisibility : TClassVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// HasArrayIndices
//
function TPropertySymbol.HasArrayIndices : Boolean;
begin
   Result:=Assigned(FArrayIndices) and (FArrayIndices.Count>0);
end;

// GetDescription
//
function TPropertySymbol.GetDescription : String;
begin
   Result := Format('property %s%s: %s', [Name, GetArrayIndicesDescription, Typ.Name]);

   if Assigned(FIndexSym) then
      Result:=Result+' index '+VarToStr(FIndexValue[0]);

   if Assigned(FReadSym) then
      Result := Result + ' read ' + FReadSym.Name;

   if Assigned(FWriteSym) then
      Result := Result + ' write ' + FWriteSym.Name;

   if ClassSymbol.DefaultProperty = Self then
      Result := Result + '; default;';
end;

function TPropertySymbol.GetIsDefault: Boolean;
begin
  Result := ClassSymbol.DefaultProperty = Self;
end;

procedure TPropertySymbol.SetIndex(const Data: TData; Addr: Integer; Sym: TTypeSymbol);
begin
   FIndexSym := Sym;
   SetLength(FIndexValue,FIndexSym.Size);
   DWSCopyData(Data, Addr, FIndexValue, 0, FIndexSym.Size);
end;

// ------------------
// ------------------ TClassOperatorSymbol ------------------
// ------------------

// Create
//
constructor TClassOperatorSymbol.Create(tokenType : TTokenType);
begin
   inherited Create(cTokenStrings[tokenType], nil);
   FTokenType:=tokenType;
end;

// QualifiedName
//
function TClassOperatorSymbol.QualifiedName : String;
begin
   Result:=ClassSymbol.QualifiedName+'.'+Name;
end;

// GetCaption
//
function TClassOperatorSymbol.GetCaption: string;
begin
   Result:='class operator '+cTokenStrings[TokenType]+' ';
   if (UsesSym<>nil) and (UsesSym.Params.Count>0) then
      Result:=Result+UsesSym.Params[0].Typ.Name
   else Result:=Result+'???';
   Result:=Result+' uses '+FUsesSym.Name;
end;

// GetDescription
//
function TClassOperatorSymbol.GetDescription: string;
begin
   Result:=GetCaption;
end;

// ------------------
// ------------------ TClassSymbol ------------------
// ------------------

// Create
//
constructor TClassSymbol.Create(const name : String);
begin
   inherited Create(name, nil);
   FSize:=1;
   FMembers:=CreateMembersTable;
   FClassOfSymbol:=TClassOfSymbol.Create('class of '+Name, Self);
end;

// Destroy
//
destructor TClassSymbol.Destroy;
begin
   if FForwardPosition<>nil then
      Dispose(FForwardPosition);
   FOperators.Free;
   FMembers.Free;
   FClassOfSymbol.Free;
   inherited;
end;

// CreateMembersTable
//
function TClassSymbol.CreateMembersTable : TMembersSymbolTable;
begin
   Result:=TMembersSymbolTable.Create(nil);
   Result.ClassSym:=Self;
end;

procedure TClassSymbol.AddField(Sym: TFieldSymbol);
begin
  FMembers.AddSymbol(Sym);
  Sym.FClassSymbol := Self;

  Sym.FOffset := FScriptInstanceSize;
  FScriptInstanceSize := FScriptInstanceSize + Sym.Typ.Size;
end;

// AddMethod
//
procedure TClassSymbol.AddMethod(methSym : TMethodSymbol);
var
   x : Integer;
   memberSymbol : TSymbol;
begin
   FMembers.AddSymbol(methSym);
   methSym.FClassSymbol:=Self;

   // Check if class is abstract or not
   if methSym.IsAbstract then
      Include(FFlags, csfAbstract)
   else if methSym.IsOverride and methSym.FParentMeth.IsAbstract then begin
      Exclude(FFlags, csfAbstract);
      for x:=0 to FMembers.Count - 1 do begin
         memberSymbol:=FMembers[x];
         if (memberSymbol is TMethodSymbol) and (TMethodSymbol(memberSymbol).IsAbstract) then begin
            Include(FFlags, csfAbstract);
            Break;
         end;
      end;
   end;
end;

// AddProperty
//
procedure TClassSymbol.AddProperty(Sym: TPropertySymbol);
begin
   sym.ClassSymbol:=Self;
   FMembers.AddSymbol(Sym);
end;

// AddOperator
//
procedure TClassSymbol.AddOperator(sym: TClassOperatorSymbol);
begin
   sym.ClassSymbol:=Self;
   FMembers.AddSymbol(sym);
   FOperators.Add(sym);
end;

// AddConst
//
procedure TClassSymbol.AddConst(sym : TClassConstSymbol);
begin
   sym.ClassSymbol:=Self;
   FMembers.AddSymbol(sym);
end;

// FieldAtOffset
//
function TClassSymbol.FieldAtOffset(offset : Integer) : TFieldSymbol;
var
   i : Integer;
begin
   for i:=0 to Members.Count-1 do begin
      Result:=TFieldSymbol(Members[i]);
      if (Result.ClassType=TFieldSymbol) and (Result.Offset=offset) then Exit;
   end;
   if Parent<>nil then
      Result:=Parent.FieldAtOffset(offset)
   else Result:=nil;
end;

procedure TClassSymbol.InitData(const Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  Data[Offset] := IUnknown(nilIntf);
end;

// Initialize
//
procedure TClassSymbol.Initialize(const msgs : TdwsCompileMessageList);
var
   i : Integer;
   methSym : TMethodSymbol;
begin
   // Check validity of the class declaration
   if IsForwarded then begin
      msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_ClassNotCompletelyDefined, [Name]);
      Exit;
   end;

   for i := 0 to FMembers.Count-1 do begin
      if FMembers[i] is TMethodSymbol then begin
         methSym:=TMethodSymbol(FMembers[i]);
         if not methSym.IsAbstract then begin
            if Assigned(methSym.FExecutable) then
               methSym.FExecutable.InitSymbol(FMembers[i])
            else if not IsExternal then begin
               msgs.AddCompilerErrorFmt((methSym as TSourceMethodSymbol).DeclarationPos, CPE_MethodNotImplemented,
                                        [methSym.Name, methSym.ClassSymbol.Caption]);
            end;
         end;
      end;
   end;
end;

// InheritFrom
//
procedure TClassSymbol.InheritFrom(ancestorClassSym : TClassSymbol);
begin
   FMembers.AddParent(ancestorClassSym.Members);
   if csfAbstract in ancestorClassSym.FFlags then
      Include(FFlags, csfAbstract);
   FScriptInstanceSize:=ancestorClassSym.ScriptInstanceSize;
   FParent:=ancestorClassSym;

   FVirtualMethodTable:=ancestorClassSym.FVirtualMethodTable;

   IsStatic:=IsStatic or ancestorClassSym.IsStatic;
end;

function TClassSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
var
  csym: TClassSymbol;
begin
  Result := False;
  typSym := typSym.BaseType;
  if typSym is TNilSymbol then
    Result := True
  else if typSym is TClassSymbol then
  begin
    csym := TClassSymbol(typSym);
    while csym <> nil do
    begin
      if csym = Self then
      begin
        Result := True;
        exit;
      end;
      csym := csym.Parent;
    end;
  end;
end;

// IsOfType
//
function TClassSymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(Self=typSym);
   if Result or (Self=nil) then Exit;
   if Parent<>nil then
      Result:=Parent.IsOfType(typSym)
   else Result:=False;
end;

// VMTMethod
//
function TClassSymbol.VMTMethod(index : Integer) : TMethodSymbol;
begin
   Assert(Cardinal(index)<Cardinal(Length(FVirtualMethodTable)));
   Result:=FVirtualMethodTable[index];
end;

// VMTCount
//
function TClassSymbol.VMTCount : Integer;
begin
   Result:=Length(FVirtualMethodTable);
end;

function TClassSymbol.GetDescription: string;
var
  i: Integer;
begin
  if FParent <> nil then
    Result := Name + ' = class (' + FParent.Name + ')'#13#10
  else
    Result := Name + ' = class'#13#10;

  for i := 0 to Members.Count - 1 do
    Result := Result + '   ' + Members.Symbols[i].Description + ';'#13#10;

  Result := Result + 'end';
end;

// GetIsForwarded
//
function TClassSymbol.GetIsForwarded : Boolean;
begin
   Result:=Assigned(FForwardPosition);
end;

// GetIsExplicitAbstract
//
function TClassSymbol.GetIsExplicitAbstract : Boolean;
begin
   Result:=(csfExplicitAbstract in FFlags);
end;

// SetIsExplicitAbstract
//
procedure TClassSymbol.SetIsExplicitAbstract(const val : Boolean);
begin
   if val then
      Include(FFlags, csfExplicitAbstract)
   else Exclude(FFlags, csfExplicitAbstract);
end;

// GetIsAbstract
//
function TClassSymbol.GetIsAbstract : Boolean;
begin
   Result:=(([csfAbstract, csfExplicitAbstract]*FFlags)<>[]);
end;

// GetIsSealed
//
function TClassSymbol.GetIsSealed : Boolean;
begin
   Result:=(csfSealed in FFlags);
end;

// SetIsSealed
//
procedure TClassSymbol.SetIsSealed(const val : Boolean);
begin
   if val then
      Include(FFlags, csfSealed)
   else Exclude(FFlags, csfSealed);
end;

// GetIsStatic
//
function TClassSymbol.GetIsStatic : Boolean;
begin
   Result:=(csfStatic in FFlags);
end;

// SetIsStatic
//
procedure TClassSymbol.SetIsStatic(const val : Boolean);
begin
   if val then
      Include(FFlags, csfStatic)
   else Exclude(FFlags, csfStatic);
end;

// GetIsExternal
//
function TClassSymbol.GetIsExternal : Boolean;
begin
   Result:=(csfExternal in FFlags);
end;

// SetIsExternal
//
procedure TClassSymbol.SetIsExternal(const val : Boolean);
begin
   if val then
      Include(FFlags, csfExternal)
   else Exclude(FFlags, csfExternal);
end;

// AllocateVMTindex
//
function TClassSymbol.AllocateVMTindex : Integer;
begin
   Result:=Length(FVirtualMethodTable);
   SetLength(FVirtualMethodTable, Result+1);
end;

// SetForwardedPos
//
procedure TClassSymbol.SetForwardedPos(const pos : TScriptPos);
begin
   if FForwardPosition=nil then
      New(FForwardPosition);
   FForwardPosition^:=pos;
end;

// ClearIsForwarded
//
procedure TClassSymbol.ClearIsForwarded;
begin
   Dispose(FForwardPosition);
   FForwardPosition:=nil;
end;

// FindClassOperatorStrict
//
function TClassSymbol.FindClassOperatorStrict(tokenType : TTokenType; paramType : TSymbol; recursive : Boolean) : TClassOperatorSymbol;
var
   i : Integer;
begin
   for i:=0 to FOperators.Count-1 do begin
      Result:=TClassOperatorSymbol(FOperators.List[i]);
      if     (Result.TokenType=tokenType)
         and (Result.Typ=paramType) then Exit;
   end;
   if recursive and (Parent<>nil) then
      Result:=Parent.FindClassOperatorStrict(tokenType, paramType, True)
   else Result:=nil;
end;

// FindClassOperator
//
function TClassSymbol.FindClassOperator(tokenType : TTokenType; paramType : TTypeSymbol) : TClassOperatorSymbol;
var
   i : Integer;
begin
   Result:=FindClassOperatorStrict(tokenType, paramType, False);
   if Result<>nil then Exit;

   if FOperators.Count>0 then begin
      for i:=0 to FOperators.Count-1 do begin
         Result:=TClassOperatorSymbol(FOperators.List[i]);
         if     (Result.TokenType=tokenType)
            and paramType.IsOfType(Result.Typ) then Exit;
      end;
      for i:=0 to FOperators.Count-1 do begin
         Result:=TClassOperatorSymbol(FOperators.List[i]);
         if     (Result.TokenType=tokenType)
            and Result.Typ.IsCompatible(paramType) then Exit;
      end;
   end;
   if Parent<>nil then
      Result:=Parent.FindClassOperator(tokenType, paramType)
   else Result:=nil;
end;

// VisibilityToString
//
class function TClassSymbol.VisibilityToString(visibility : TClassVisibility) : String;
const
   cVisibilityNames : array [TClassVisibility] of String = (
      'magic', 'private', 'protected', 'public', 'published' );
begin
   Result:=cVisibilityNames[visibility];
end;

{ TNilSymbol }

constructor TNilSymbol.Create;
begin
  inherited Create('', nil);
  FSize := 1;
end;

// InitData
//
procedure TNilSymbol.InitData(const data : TData; offset : Integer);
begin
   Assert(False);
end;

function TNilSymbol.GetCaption: string;
begin
  Result := 'nil';
end;

function TNilSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  typSym := typSym.BaseType;
  Result := (TypSym is TClassSymbol) or (TypSym is TNilSymbol);
end;

{ TClassOfSymbol }

constructor TClassOfSymbol.Create(const Name: string; Typ: TClassSymbol);
begin
  inherited Create(Name, Typ);
end;

function TClassOfSymbol.GetCaption: string;
begin
  if Typ <> nil then
    Result := 'class of ' + Typ.Name
  else
    Result := 'class of ???';
end;

procedure TClassOfSymbol.InitData(const Data: TData; Offset: Integer);
begin
  Data[Offset] := Int64(0);
end;

function TClassOfSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  typSym := typSym.BaseType;
  Result :=    (typSym is TNilSymbol)
            or ((typSym is TClassOfSymbol) and Typ.IsCompatible(typSym.Typ));
end;

// IsOfType
//
function TClassOfSymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   if typSym is TClassOfSymbol then
      Result:=Typ.IsOfType(typSym.Typ)
   else Result:=False;
end;

// TypClassSymbol
//
function TClassOfSymbol.TypClassSymbol : TClassSymbol;
begin
   Result:=TClassSymbol(Typ);
end;

// ------------------
// ------------------ TBaseSymbol ------------------
// ------------------

// Create
//
constructor TBaseSymbol.Create(const name : String);
begin
   inherited Create(name, nil);
   FSize:=1;
end;

// IsCompatible
//
function TBaseSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=    (typSym<>nil)
           and (UnAliasedType=typSym.UnAliasedType);
end;

// IsBaseType
//
class function TBaseSymbol.IsBaseType : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TBaseIntegerSymbol ------------------
// ------------------

// Create
//
constructor TBaseIntegerSymbol.Create;
begin
   inherited Create(SYS_INTEGER);
end;

// InitData
//
procedure TBaseIntegerSymbol.InitData(const data : TData; offset : Integer);
const
   cZero64 : Int64 = 0;
begin
   data[offset]:=cZero64;
end;

// ------------------
// ------------------ TBaseFloatSymbol ------------------
// ------------------

// Create
//
constructor TBaseFloatSymbol.Create;
begin
   inherited Create(SYS_FLOAT);
end;

// InitData
//
procedure TBaseFloatSymbol.InitData(const data : TData; offset : Integer);
const
   cZero : Double = 0;
begin
   data[offset]:=cZero;
end;

// ------------------
// ------------------ TBaseStringSymbol ------------------
// ------------------

// Create
//
constructor TBaseStringSymbol.Create;
begin
   inherited Create(SYS_STRING);
end;

// InitData
//
procedure TBaseStringSymbol.InitData(const data : TData; offset : Integer);
begin
   data[offset]:='';
end;

// ------------------
// ------------------ TBaseBooleanSymbol ------------------
// ------------------

// Create
//
constructor TBaseBooleanSymbol.Create;
begin
   inherited Create(SYS_BOOLEAN);
end;

// InitData
//
procedure TBaseBooleanSymbol.InitData(const data : TData; offset : Integer);
begin
   data[offset]:=False;
end;

// ------------------
// ------------------ TBaseVariantSymbol ------------------
// ------------------

// Create
//
constructor TBaseVariantSymbol.Create(const name : String = '');
begin
   if name='' then
      inherited Create(SYS_VARIANT)
   else inherited Create(name);
end;

// IsCompatible
//
function TBaseVariantSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=    (typSym<>nil)
           and (   (typSym.UnAliasedType is TBaseSymbol)
                or (typSym.UnAliasedType is TEnumerationSymbol));
end;

// InitData
//
procedure TBaseVariantSymbol.InitData(const data : TData; offset : Integer);
begin
   VarClear(data[offset]);
end;

// ------------------
// ------------------ TConnectorSymbol ------------------
// ------------------

// Create
//
constructor TConnectorSymbol.Create(const name : String; const connectorType : IConnectorType);
begin
   inherited Create(name);
   FConnectorType:=ConnectorType;
end;

// ------------------
// ------------------ TValueSymbol ------------------
// ------------------

function TValueSymbol.GetCaption: string;
begin
  Result := Name + ': ' + Typ.Caption;
end;

function TValueSymbol.GetDescription: string;
begin
  Result := Name + ': ' + Typ.Description;
end;

{ TConstSymbol }

constructor TConstSymbol.Create(const Name: string; Typ: TTypeSymbol; const Value: Variant);
begin
  inherited Create(Name, Typ);
  SetLength(FData, 1);
  VarCopy(FData[0], Value);
end;

constructor TConstSymbol.Create(const Name: string; Typ: TTypeSymbol; const Data: TData;
  Addr: Integer);
begin
  inherited Create(Name, Typ);
  SetLength(FData, Typ.Size);
  DWSCopyData(Data, Addr, FData, 0, Typ.Size);
end;

function TConstSymbol.GetCaption: string;
begin
  Result := 'const ' + inherited GetCaption;
end;

function TConstSymbol.GetDescription: string;
begin
  if VarType(FData[0]) = varError then
    Result := 'const ' + inherited GetDescription + ' = [varError]'
  else
    Result := 'const ' + inherited GetDescription + ' = ' + VarToStr(FData[0]);
end;

procedure TConstSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
end;

{ TMemberSymbol }

// QualifiedName
//
function TMemberSymbol.QualifiedName : String;
begin
   Result:=RecordSymbol.QualifiedName+'.'+Name;
end;

{ TDataSymbol }

function TDataSymbol.GetDescription: string;
begin
  if Assigned(Typ) then
    Result := Name + ': ' + Typ.Name
  else
    Result := Name;
end;

{ TParamSymbol }

function TParamSymbol.GetDescription: string;
begin
  if Typ <> nil then
    Result := Name + ': ' + Typ.Name
  else
    Result := Name + ': ???';
end;

{ TParamSymbol }

function TParamSymbolWithDefaultValue.GetDescription: string;
begin
   Result := inherited GetDescription;

   // Has a default parameter. Format display of param to show it.
   if Length(FDefaultValue) > 0 then begin
      if (Typ is TBaseStringSymbol) then
         Result := Result + ' = ''' + VarToStr(FDefaultValue[0]) + ''''  // put quotes around value
       else Result := Result + ' = ' + VarToStr(FDefaultValue[0]);
   end;
end;

procedure TParamSymbolWithDefaultValue.SetDefaultValue(const Data: TData; Addr: Integer);
begin
  SetLength(FDefaultValue, Typ.Size);
  DWSCopyData(Data, Addr, FDefaultValue, 0, Typ.Size);
end;

procedure TParamSymbolWithDefaultValue.SetDefaultValue(const Value: Variant);
begin
  Assert(Typ.Size = 1);
  SetLength(FDefaultValue, 1);
  VarCopy(FDefaultValue[0], Value);
end;

{ TByRefParamSymbol }

constructor TByRefParamSymbol.Create(const Name: string; Typ: TTypeSymbol);
begin
  inherited Create(Name, Typ);
  FSize := 1;
end;

// ------------------
// ------------------ TLazyParamSymbol ------------------
// ------------------

// GetDescription
//
function TLazyParamSymbol.GetDescription: string;
begin
   Result:='lazy '+inherited GetDescription;
end;

{ TConstParamSymbol }

function TConstParamSymbol.GetDescription: string;
begin
  Result := 'const ' + inherited GetDescription;
end;

{ TVarParamSymbol }

function TVarParamSymbol.GetDescription: string;
begin
  Result := 'var ' + inherited GetDescription;
end;

{ TSymbolTable }

constructor TSymbolTable.Create(Parent: TSymbolTable; AddrGenerator: TAddrGenerator);
begin
   FAddrGenerator := AddrGenerator;
   if Assigned(Parent) then
      AddParent(Parent);
end;

destructor TSymbolTable.Destroy;
begin
   FSymbols.Clean;
   ClearParents;
   FParents.Clear;
   inherited;
end;

procedure TSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
var
   i : Integer;
   ptrList : PPointerList;
begin
   ptrList:=FSymbols.List;
   for i:=0 to FSymbols.Count-1 do
      TSymbol(ptrList[i]).Initialize(msgs);
end;

// FindLocal
//
function TSymbolTable.FindLocal(const aName : String; ofClass : TSymbolClass = nil) : TSymbol;
var
   n : Integer;
begin
   n:=FSymbols.Count;
   if n>6 then begin
      if not FSymbolsSorted then begin
         SortSymbols(0, n-1);
         FSymbolsSorted:=True;
      end;
      Result:=FindLocalSorted(aName);
   end else begin
      Result:=FindLocalUnSorted(aName);
   end;
   if (Result<>nil) and (ofClass<>nil) and (not (Result is ofClass)) then
      Result:=nil;
end;

// FindTypeLocal
//
function TSymbolTable.FindTypeLocal(const aName : String) : TTypeSymbol;
begin
   Result:=TTypeSymbol(FindLocal(aName, TTypeSymbol));
end;

// FindSymbolAtStackAddr
//
function TSymbolTable.FindSymbolAtStackAddr(const stackAddr, level : Integer) : TDataSymbol;
var
   i : Integer;
   sym : TSymbol;
begin
   for i:=0 to FSymbols.Count-1 do begin
      sym:=FSymbols.List[i];
      if sym.InheritsFrom(TDataSymbol) then begin
         Result:=TDataSymbol(sym);
         if (Result.StackAddr=stackAddr) and (Result.Level=level) then
            Exit;
      end;
   end;

   for i:=0 to ParentCount-1 do begin
      Result:=Parents[i].FindSymbolAtStackAddr(stackAddr, level);
      if Assigned(Result) then Exit;
   end;

   Result:=nil;
end;

// SortSymbols
//
procedure TSymbolTable.SortSymbols(minIndex, maxIndex : Integer);
var
  i, j, p : Integer;
  pSym : TSymbol;
  ptrList : PPointerList;
begin
   if (maxIndex<=minIndex) then
      Exit;
   ptrList:=FSymbols.List;
   repeat
      i:=minIndex;
      j:=maxIndex;
      p:=((i+j) shr 1);
      repeat
         pSym:=TSymbol(ptrList[p]);
         while UnicodeCompareText(TSymbol(ptrList[i]).Name, pSym.Name)<0 do Inc(i);
         while UnicodeCompareText(TSymbol(ptrList[j]).Name, pSym.Name)>0 do Dec(j);
         if i<=j then begin
            FSymbols.Exchange(i, j);
            if p=i then
               p:=j
            else if p=j then
               p:=i;
            Inc(i);
            Dec(j);
         end;
      until i>j;
      if minIndex<j then
         SortSymbols(minIndex, j);
      minIndex:=i;
   until i>=maxIndex;
end;

// FindLocalSorted
//
function TSymbolTable.FindLocalSorted(const name: string): TSymbol;
var
   lo, hi, mid, cmpResult: Integer;
   ptrList : PPointerList;
begin
   lo:=0;
   hi:=FSymbols.Count-1;
   ptrList:=FSymbols.List;
   while lo<=hi do begin
      mid:=(lo+hi) shr 1;
      cmpResult:=UnicodeCompareText(TSymbol(ptrList[mid]).Name, name);
      if cmpResult<0 then
         lo:=mid+1
      else begin
         if cmpResult=0 then
            Exit(TSymbol(ptrList[mid]))
         else hi:=mid-1;
      end;
   end;
   Result:=nil;
end;

// FindLocalUnSorted
//
function TSymbolTable.FindLocalUnSorted(const name: string) : TSymbol;
var
   i : Integer;
   ptrList : PPointerList;
begin
   ptrList:=FSymbols.List;
   for i:=FSymbols.Count-1 downto 0 do begin
      if UnicodeCompareText(TSymbol(ptrList[i]).Name, Name)=0 then
         Exit(TSymbol(ptrList[i]));
   end;
   Result:=nil;
end;

// FindSymbol
//
function TSymbolTable.FindSymbol(const aName : String; minVisibility : TClassVisibility;
                                 ofClass : TSymbolClass = nil) : TSymbol;
var
   i : Integer;
begin
   // Find Symbol in the local List
   Result:=FindLocal(aName, ofClass);
   if Assigned(Result) then begin
      if Result.IsVisibleFor(minVisibility) then
         Exit
      else Result:=nil;
   end;

   // Find Symbol in all parent lists
   for i:=0 to ParentCount-1 do begin
      Result:=Parents[i].FindSymbol(aName, minVisibility, ofClass);
      if Assigned(Result) then Break;
   end;
end;

// FindTypeSymbol
//
function TSymbolTable.FindTypeSymbol(const aName : string; minVisibility : TClassVisibility) : TTypeSymbol;
begin
   Result:=TTypeSymbol(FindSymbol(aName, minVisibility, TTypeSymbol));
end;

// HasClass
//
function TSymbolTable.HasClass(const aClass : TSymbolClass) : Boolean;
var
   i : Integer;
   ptrList : PPointerList;
begin
   ptrList:=FSymbols.List;
   for i:=FSymbols.Count-1 downto 0 do begin
      if TSymbol(ptrList[i]) is aClass then
         Exit(True);
   end;
   Result:=False;
end;

// GetCount
//
function TSymbolTable.GetCount: Integer;
begin
   Result := FSymbols.Count
end;

// GetSymbol
//
function TSymbolTable.GetSymbol(Index: Integer): TSymbol;
begin
   Result := TSymbol(FSymbols.List[Index])
end;

function TSymbolTable.AddSymbol(sym : TSymbol) : Integer;
begin
   Result:=AddSymbolDirect(sym);
   if (sym is TDataSymbol) and (FAddrGenerator <> nil) then begin
      TDataSymbol(sym).Level := FAddrGenerator.Level;
      TDataSymbol(sym).StackAddr := FAddrGenerator.GetStackAddr(sym.Size);
   end;
end;

// AddSymbolDirect
//
function TSymbolTable.AddSymbolDirect(sym : TSymbol) : Integer;
var
   n : Integer;
   ptrList : PPointerList;
begin
   if FSymbolsSorted then begin
      Result:=0;
      n:=FSymbols.Count;
      ptrList:=FSymbols.List;
      while Result<n do begin
         if UnicodeCompareText(TSymbol(ptrList[Result]).Name, Sym.Name)>=0 then
            Break;
         Inc(Result);
      end;
      FSymbols.Insert(Result, sym);
   end else Result:=FSymbols.Add(sym);
end;

// Remove
//
function TSymbolTable.Remove(Sym: TSymbol): Integer;
begin
   Result:=FSymbols.Remove(Sym);
end;

// Clear
//
procedure TSymbolTable.Clear;
begin
   FSymbols.Clear;
end;

// AddParent
//
procedure TSymbolTable.AddParent(Parent: TSymbolTable);
begin
   InsertParent(ParentCount,Parent);
end;

// InsertParent
//
procedure TSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
begin
   FParents.Insert(Index,Parent);
end;

// RemoveParent
//
function TSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
   Result := FParents.Remove(Parent);
end;

// ClearParents
//
procedure TSymbolTable.ClearParents;
begin
   while FParents.Count > 0 do
      RemoveParent(FParents.List[0]);
end;

// GetParentCount
//
function TSymbolTable.GetParentCount: Integer;
begin
   Result := FParents.Count
end;

// GetParents
//
function TSymbolTable.GetParents(Index: Integer): TSymbolTable;
begin
   Result := TSymbolTable(FParents.List[Index]);
end;

// IndexOfParent
//
function TSymbolTable.IndexOfParent(Parent: TSymbolTable): Integer;
begin
   Result := FParents.IndexOf(Parent)
end;

// MoveParent
//
procedure TSymbolTable.MoveParent(CurIndex, NewIndex: Integer);
begin
   FParents.Move(CurIndex,NewIndex);
end;

// MoveNext
//
function TSymbolTable.TSymbolTableEnumerator.MoveNext : Boolean;
begin
   Dec(Index);
   Result:=(Index>=0);
end;

// GetCurrent
//
function TSymbolTable.TSymbolTableEnumerator.GetCurrent : TSymbol;
begin
   Result:=Table[Index];
end;

// GetEnumerator
//
function TSymbolTable.GetEnumerator : TSymbolTableEnumerator;
begin
   if Self=nil then begin
      Result.Table:=nil;
      Result.Index:=0;
   end else begin
      Result.Table:=Self;
      Result.Index:=Count;
   end;
end;

// ------------------
// ------------------ TMembersSymbolTable ------------------
// ------------------

// AddParent
//
procedure TMembersSymbolTable.AddParent(parent : TMembersSymbolTable);
begin
   inherited AddParent(parent);
end;

// FindSymbol
//
function TMembersSymbolTable.FindSymbol(const aName : String; minVisibility : TClassVisibility;
                                        ofClass : TSymbolClass = nil) : TSymbol;
var
   i : Integer;
begin
   // Find Symbol in the local List
   Result:=FindLocal(aName, ofClass);
   if Assigned(Result) and Result.IsVisibleFor(minVisibility) then
      Exit;
   Result:=nil;

   // Find Symbol in all parent lists
   if minVisibility=cvPrivate then
      minVisibility:=cvProtected;
   i:=0;
   while not Assigned(Result) and (i<ParentCount) do begin
      Result:=(Parents[i] as TMembersSymbolTable).FindSymbol(aName, minVisibility, ofClass);
      Inc(i);
   end;
end;

// FindSymbolFromClass
//
function TMembersSymbolTable.FindSymbolFromClass(const aName : String; fromClass : TClassSymbol) : TSymbol;
begin
   if fromClass=nil then
      Result:=FindSymbol(aName, cvPublic)
   else if fromClass=ClassSym then
      Result:=FindSymbol(aName, cvPrivate)
   else if fromClass.IsOfType(ClassSym) then
      Result:=FindSymbol(aName, cvProtected)
   else Result:=FindSymbol(aName, cvPublic);
end;

// ------------------
// ------------------ TUnSortedSymbolTable ------------------
// ------------------

// FindLocal
//
function TUnSortedSymbolTable.FindLocal(const aName : String; ofClass : TSymbolClass = nil) : TSymbol;
begin
   Result:=FindLocalUnSorted(aName);
   if (Result<>nil) and (ofClass<>nil) and (not (Result is ofClass)) then
      Result:=nil;
end;

// ------------------
// ------------------ TProgramSymbolTable ------------------
// ------------------

// Create
//
constructor TProgramSymbolTable.Create(Parent: TSymbolTable = nil; AddrGenerator: TAddrGenerator = nil);
begin
   inherited;
   if Parent is TStaticSymbolTable then begin
      FSystemTable:=(Parent as TStaticSymbolTable);
      FSystemTable._AddRef;
   end;
end;

// Destroy
//
destructor TProgramSymbolTable.Destroy;
begin
   inherited;
   FDestructionList.Clean;
   if FSystemTable<>nil then
      FSystemTable._Release;
end;

// AddToDestructionList
//
procedure TProgramSymbolTable.AddToDestructionList(Sym: TSymbol);
begin
   FDestructionList.Add(sym);
end;

// ------------------
// ------------------ TUnitSymbolTable ------------------
// ------------------

// Destroy
//
destructor TUnitSymbolTable.Destroy;
begin
   inherited;
   FObjects.Free;
end;

// BeforeDestruction
//
procedure TUnitSymbolTable.BeforeDestruction;
var
   objOwner : IObjectOwner;
begin
   if Assigned(FObjects) then begin
      while FObjects.Count > 0 do begin
         objOwner := IObjectOwner(FObjects[0]);
         FObjects.Delete(0);
         objOwner.ReleaseObject;
      end;
   end;
   inherited;
end;

// AddObjectOwner
//
procedure TUnitSymbolTable.AddObjectOwner(AOwner : IObjectOwner);
begin
   if not Assigned(FObjects) then
      FObjects := TInterfaceList.Create;
   FObjects.Add(AOwner);
end;

{ TExternalVarSymbol }

destructor TExternalVarSymbol.Destroy;
begin
  FReadFunc.Free;
  FWriteFunc.Free;
  inherited;
end;

function TExternalVarSymbol.GetReadFunc: TFuncSymbol;
begin
  Result := FReadFunc;
end;

function TExternalVarSymbol.GetWriteFunc: TFuncSymbol;
begin
  Result := FWriteFunc;
end;

{ TUnitSymbol }

constructor TUnitSymbol.Create(const Name: string; Table: TSymbolTable;
  IsTableOwner: Boolean = False);
begin
  inherited Create(Name, nil);
  FIsTableOwner := IsTableOwner;
  FTable := Table;
  FInitialized := False;
end;

destructor TUnitSymbol.Destroy;
begin
  if FIsTableOwner then
    FTable.Free;
  inherited;
end;

// InitData
//
procedure TUnitSymbol.InitData(const data : TData; offset : Integer);
begin
   // nothing
end;

procedure TUnitSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
  if not FInitialized then
  begin
    FTable.Initialize(msgs);
    FInitialized := True;
  end;
end;

// ------------------
// ------------------ TAddrGeneratorRec ------------------
// ------------------

// CreatePositive
//
constructor TAddrGeneratorRec.CreatePositive(aLevel : SmallInt; anInitialSize: Integer = 0);
begin
   FDataSize:=anInitialSize;
   FLevel:=aLevel;
   FSign:=agsPositive;
end;

// CreateNegative
//
constructor TAddrGeneratorRec.CreateNegative(aLevel : SmallInt);
begin
   FDataSize:=0;
   FLevel:=aLevel;
   FSign:=agsNegative;
end;

// GetStackAddr
//
function TAddrGeneratorRec.GetStackAddr(size : Integer): Integer;
begin
   if FSign=agsPositive then begin
      Result:=FDataSize;
      Inc(FDataSize, Size);
   end else begin
      Inc(FDataSize, Size);
      Result:=-FDataSize;
   end;
end;

// ------------------
// ------------------ TDynamicArraySymbol ------------------
// ------------------

constructor TDynamicArraySymbol.Create(const Name: string; Typ: TTypeSymbol);
begin
  inherited Create(Name, Typ);
  FSize := 1;
end;

function TDynamicArraySymbol.GetCaption: string;
begin
  Result := 'array of ' + Typ.Caption
end;

procedure TDynamicArraySymbol.InitData(const Data: TData; Offset: Integer);
begin
  Data[Offset] := Null; // ADR
end;

function TDynamicArraySymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  Result :=     (TypSym is TDynamicArraySymbol)
            and (Typ.IsCompatible(TypSym.Typ) or (TypSym.Typ is TNilSymbol));
end;

{ TStaticArraySymbol }

constructor TStaticArraySymbol.Create(const Name: string; Typ: TTypeSymbol; LowBound, HighBound: Integer);
begin
  inherited Create(Name, Typ);
  FLowBound := LowBound;
  FHighBound := HighBound;
  FElementCount := HighBound - LowBound + 1;
  FSize := FElementCount * Typ.Size;
end;

procedure TStaticArraySymbol.InitData(const Data: TData; Offset: Integer);
var
  x: Integer;
begin
  for x := 1 to ElementCount do
  begin
    Typ.InitData(Data, Offset);
    Inc(Offset, Typ.BaseType.Size);
  end;
end;

function TStaticArraySymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  TypSym := TypSym.BaseType;
  Result :=     (TypSym is TStaticArraySymbol)
            and (ElementCount = TStaticArraySymbol(TypSym).ElementCount)
            and Typ.IsCompatible(TypSym.Typ);
end;

// IsOfType
//
function TStaticArraySymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=inherited IsOfType(typSym);
   if not Result then begin
      if (typSym is TOpenArraySymbol) then
         Result:=(ElementCount=0) or (Typ.IsCompatible(TypSym.Typ));
   end;
end;

// AddElement
//
procedure TStaticArraySymbol.AddElement;
begin
   Inc(FHighBound);
   Inc(FElementCount);
   FSize := FElementCount * Typ.Size;
end;

function TStaticArraySymbol.GetCaption;
begin
  Result := 'array [' + IntToStr(FLowBound) + '..' + IntToStr(FHighBound) + '] of ';
  if Assigned(Typ) then
    Result := Result + Typ.Caption
  else
    Result := Result + '<unknown>';
end;

// ------------------
// ------------------ TOpenArraySymbol ------------------
// ------------------

// Create
//
constructor TOpenArraySymbol.Create(const Name: string; Typ: TTypeSymbol);
begin
   inherited Create(Name, Typ, 0, -1);
   FSize:=1;
end;

// IsCompatible
//
function TOpenArraySymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  TypSym := TypSym.BaseType;
  Result :=     (TypSym is TStaticArraySymbol)
            and Typ.IsCompatible(TypSym.Typ);
end;

// ------------------
// ------------------ TElementSymbol ------------------
// ------------------

// Create
//
constructor TElementSymbol.Create(const Name: string; Typ: TTypeSymbol;
  Value: Integer; IsUserDef: Boolean);
begin
   inherited Create(Name, Typ, Value);
   FIsUserDef := IsUserDef;
   FUserDefValue := Value;
end;

// GetDescription
//
function TElementSymbol.GetDescription: string;
begin
   if FIsUserDef then
      Result:=Name+' = '+IntToStr(Data[0])
   else Result:=Name;
end;

// ------------------
// ------------------ TEnumerationSymbol ------------------
// ------------------

// Create
//
constructor TEnumerationSymbol.Create(const Name: string; BaseType: TTypeSymbol);
begin
   inherited Create(Name, BaseType);
   FElements:=TUnSortedSymbolTable.Create;
   FLowBound:=MaxInt;
   FHighBound:=-MaxInt;
end;

// Destroy
//
destructor TEnumerationSymbol.Destroy;
begin
   FElements.Clear;
   FElements.Free;
   inherited;
end;

// DefaultValue
//
function TEnumerationSymbol.DefaultValue : Integer;
begin
   if FElements.Count>0 then
      Result:=TElementSymbol(FElements[0]).FUserDefValue
   else Result:=0;
end;

// InitData
//
procedure TEnumerationSymbol.InitData(const Data: TData; Offset: Integer);
begin
   Data[Offset]:=DefaultValue;
end;

// BaseType
//
function TEnumerationSymbol.BaseType : TTypeSymbol;
begin
   Result:=Typ;
end;

// IsOfType
//
function TEnumerationSymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited IsOfType(typSym)
           or BaseType.IsOfType(typSym);
end;

// AddElement
//
procedure TEnumerationSymbol.AddElement(Element: TElementSymbol);
begin
   FElements.AddSymbol(Element);
   if Element.UserDefValue<FLowBound then
      FLowBound:=Element.UserDefValue;
   if Element.UserDefValue>FHighBound then
      FHighBound:=Element.UserDefValue;
end;

// GetCaption
//
function TEnumerationSymbol.GetCaption: string;
begin
   Result:=Name;
end;

// GetDescription
//
function TEnumerationSymbol.GetDescription: string;
var
   i : Integer;
begin
   Result:='(';
   for i:=0 to FElements.Count-1 do begin
      if i<>0 then
         Result:=Result+', ';
      Result:=Result+FElements[i].GetDescription;
   end;
   Result:=Result+')';
end;

// ShortDescription
//
function TEnumerationSymbol.ShortDescription : String;
begin
   case FElements.Count of
      0 : Result:=' ';
      1 : Result:=FElements[0].GetDescription;
   else
      Result:=FElements[0].Name+',...';
   end;
   Result:='('+Result+')';
end;

{ TStaticSymbolTable }

constructor TStaticSymbolTable.Create(Parent: TStaticSymbolTable; Reference: Boolean);
begin
  inherited Create(Parent);
  FInitialized := False;
  FRefCount := 0;
  if Reference then
    _AddRef;
end;

procedure TStaticSymbolTable._AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

procedure TStaticSymbolTable._Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
    Free;
end;

procedure TStaticSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
var
  staticSymbols: TStaticSymbolTable;
begin
  // accept only static parents
  if Parent is TLinkedSymbolTable then
    staticSymbols := TLinkedSymbolTable(Parent).Parent
  else if Parent is TStaticSymbolTable then
    staticSymbols := TStaticSymbolTable(Parent)
  else
    staticSymbols := nil;

  if Assigned(StaticSymbols) then
  begin
    staticSymbols._AddRef;
    inherited InsertParent(Index, staticSymbols);
  end
  else
    raise Exception.Create(CPE_NoStaticSymbols);
end;

function TStaticSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
  Result := inherited RemoveParent(Parent);
  (Parent as TStaticSymbolTable)._Release;
end;

destructor TStaticSymbolTable.Destroy;
begin
  Assert(FRefCount = 0);
  inherited;
end;

procedure TStaticSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
begin
  if not FInitialized then
  begin
    inherited;
    FInitialized := True;
  end;
end;

{ TLinkedSymbolTable }

constructor TLinkedSymbolTable.Create(Parent: TStaticSymbolTable;
  AddrGenerator: TAddrGenerator);
begin
  inherited Create(nil,AddrGenerator);
  FParent := Parent;
  FParent._AddRef;
end;

destructor TLinkedSymbolTable.Destroy;
begin
  FParent._Release;
  inherited;
end;

function TLinkedSymbolTable.FindLocal(const Name: string; ofClass : TSymbolClass = nil): TSymbol;
begin
  Result := FParent.FindLocal(Name, ofClass);
  if not Assigned(Result) then
    Result := inherited FindLocal(Name, ofClass);
end;

function TLinkedSymbolTable.FindSymbol(const Name: string; minVisibility : TClassVisibility;
                                       ofClass : TSymbolClass = nil): TSymbol;
begin
  Result := FParent.FindSymbol(Name, minVisibility, ofClass);
  if not Assigned(Result) then
    Result := inherited FindSymbol(Name, minVisibility, ofClass);
end;

procedure TLinkedSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
begin
  FParent.Initialize(msgs);
  inherited;
end;

// ------------------
// ------------------ TAliasSymbol ------------------
// ------------------

// BaseType
//
function TAliasSymbol.BaseType : TTypeSymbol;
begin
   Result:=Typ.BaseType;
end;

// UnAliasedType
//
function TAliasSymbol.UnAliasedType : TTypeSymbol;
begin
   Result:=Typ.UnAliasedType;
end;

// InitData
//
procedure TAliasSymbol.InitData(const data : TData; offset : Integer);
begin
   Typ.InitData(Data, Offset);
end;

// IsCompatible
//
function TAliasSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=Typ.IsCompatible(typSym);
end;

// IsOfType
//
function TAliasSymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=Typ.IsOfType(typSym);
end;

// ------------------
// ------------------ TTypeSymbol ------------------
// ------------------

// BaseType
//
function TTypeSymbol.BaseType: TTypeSymbol;
begin
   Result:=Self;
end;

// UnAliasedType
//
function TTypeSymbol.UnAliasedType : TTypeSymbol;
begin
   Result:=Self;
end;

// IsOfType
//
function TTypeSymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(Self=typSym);
end;

// IsCompatible
//
function TTypeSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  Result:=(BaseType.IsCompatible(typSym.BaseType));
end;

// ------------------
// ------------------ EScriptError ------------------
// ------------------

// CreatePosFmt
//
constructor EScriptError.CreatePosFmt(const pos : TScriptPos; const Msg: string; const Args: array of const);
begin
   inherited CreateFmt(msg, args);
   FScriptPos:=pos;
end;

// ------------------
// ------------------ EScriptException ------------------
// ------------------

constructor EScriptException.Create(const Message: string; const Value: Variant;
  Typ: TTypeSymbol; const Pos: TScriptPos);
begin
  inherited Create(Message);
  FValue := Value;
  FTyp := Typ;
  FPos := Pos;
end;

constructor EScriptException.Create(const Message: string;
  const ExceptionObj: IScriptObj; const Pos: TScriptPos);
begin
  Create(Message,ExceptionObj,ExceptionObj.ClassSym,Pos);
end;

// ------------------
// ------------------ TdwsExecution ------------------
// ------------------

// Create
//
constructor TdwsExecution.Create(const stackParams : TStackParameters);
begin
   inherited Create;
   FStack.Initialize(stackParams);
   FStack.Reset;
   FExceptionObjectStack:=TSimpleStack<Variant>.Create;
end;

// Destroy
//
destructor TdwsExecution.Destroy;
begin
   Assert(not Assigned(FSelfScriptObject));
   FExceptionObjectStack.Free;
   FStack.Finalize;
   FCallStack.Free;
   inherited;
end;

// DoStep
//
procedure TdwsExecution.DoStep(expr : TExprBase);
begin
   if ProgramState=psRunningStopped then
      EScriptStopped.DoRaise;
   if IsDebugging then
      Debugger.DoDebug(Self, expr);
end;

// SetScriptError
//
procedure TdwsExecution.SetScriptError(expr : TExprBase);
begin
   if FLastScriptError<>nil then Exit;
   FLastScriptError:=expr;
   FLastScriptCallStack:=GetCallStack;
end;

// ClearScriptError
//
procedure TdwsExecution.ClearScriptError;
begin
   FLastScriptError:=nil;
   SetLength(FLastScriptCallStack, 0);
end;

// GetDebugger
//
function TdwsExecution.GetDebugger : IDebugger;
begin
   Result:=FDebugger;
end;

// SetDebugger
//
procedure TdwsExecution.SetDebugger(const aDebugger : IDebugger);
begin
   FDebugger:=aDebugger;
   FIsDebugging:=(aDebugger<>nil);
end;

// StartDebug
//
procedure TdwsExecution.StartDebug;
begin
   FIsDebugging:=Assigned(FDebugger);
   if FIsDebugging then
      FDebugger.StartDebug(Self);
end;

// StopDebug
//
procedure TdwsExecution.StopDebug;
begin
   if Assigned(FDebugger) then
      FDebugger.StopDebug(Self);
   FIsDebugging:=False;
end;

// GetUserObject
//
function TdwsExecution.GetUserObject : TObject;
begin
   Result:=FUserObject;
end;

// SetUserObject
//
procedure TdwsExecution.SetUserObject(const value : TObject);
begin
   FUserObject:=value;
end;

// GetStack
//
function TdwsExecution.GetStack : TStack;
begin
   Result:=@FStack;
end;

// GetProgramState
//
function TdwsExecution.GetProgramState : TProgramState;
begin
   Result:=FProgramState;
end;

// GetExecutionObject
//
function TdwsExecution.GetExecutionObject : TdwsExecution;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TConditionSymbol ------------------
// ------------------

// Create
//
constructor TConditionSymbol.Create(const pos : TScriptPos; const cond : IBooleanEvalable; const msg : IStringEvalable);
begin
   inherited Create('', nil);
   FScriptPos:=pos;
   FCondition:=cond;
   FMessage:=msg;
end;

// ------------------
// ------------------ TRuntimeErrorMessage ------------------
// ------------------

// AsInfo
//
function TRuntimeErrorMessage.AsInfo: String;
begin
   Result:=Text;
   if Length(FCallStack)>0 then
      Result:=Result+' in '+(FCallStack[High(FCallStack)].Expr as TFuncExpr).FuncSym.QualifiedName;
   if Pos.Defined then
      Result:=Result+Pos.AsInfo;
   if Length(FCallStack)>0 then begin
      Result:=result+#13#10+TExprBase.CallStackToString(FCallStack);
   end;
   Result:=Format(MSG_RuntimeError, [Result]);
end;

// ------------------
// ------------------ TdwsRuntimeMessageList ------------------
// ------------------

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(const Text: String);
begin
   AddRuntimeError(cNullPos, Text, nil);
end;

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(const scriptPos : TScriptPos;
                     const Text: String; const callStack : TdwsExprLocationArray);
var
   msg : TRuntimeErrorMessage;
begin
   msg:=TRuntimeErrorMessage.Create(Self, Text, scriptPos);
   msg.FCallStack:=callStack;
   AddMsg(msg);
   HasErrors:=True;
end;

end.
