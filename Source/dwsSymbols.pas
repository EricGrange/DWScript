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
unit dwsSymbols;

{$I dws.inc}

interface

uses SysUtils, Variants, Classes,
   dwsStrings, dwsErrors, dwsUtils,
   dwsTokenizer, dwsStack, dwsXPlatform, dwsDataContext
   {$ifdef FPC},LazUTF8{$endif};

type

   IScriptObj = interface;
   IScriptObjInterface = interface;
   IScriptDynArray = interface;

   PIScriptObj = ^IScriptObj;

   TdwsExecution = class;
   TExprBase = class;
   TSymbol = class;
   TBaseSymbol = class;
   TDataSymbol = class;
   TClassSymbol = class;
   TCompositeTypeSymbol = class;
   TStructuredTypeSymbol = class;
   TMethodSymbol = class;
   TFieldSymbol = class;
   TTypeSymbol = class;
   TParamSymbol = class;
   THelperSymbol = class;
   TOperatorSymbol = class;
   TPropertySymbol = class;
   TSymbolTable = class;
   TdwsRuntimeMessageList = class;
   EScriptError = class;
   EScriptErrorClass = class of EScriptError;

   TdwsExprLocation = record
      Expr : TExprBase;
      Prog : TObject;
      function Line : Integer; inline;
      function SourceName : UnicodeString; inline;
      function Location : UnicodeString;
   end;
   TdwsExprLocationArray = array of TdwsExprLocation;

   // Interface for external debuggers
   IDebugger = interface
      ['{8D534D14-4C6B-11D5-8DCB-0000216D9E86}']
      procedure StartDebug(exec : TdwsExecution);
      procedure DoDebug(exec : TdwsExecution; expr : TExprBase);
      procedure StopDebug(exec : TdwsExecution);
      procedure EnterFunc(exec : TdwsExecution; funcExpr : TExprBase);
      procedure LeaveFunc(exec : TdwsExecution; funcExpr : TExprBase);
      function  LastDebugStepExpr : TExprBase;
      procedure DebugMessage(const msg : UnicodeString);
      procedure NotifyException(exec : TdwsExecution; const exceptObj : IScriptObj);
   end;

   TProgramState = (psUndefined, psReadyToRun, psRunning, psRunningStopped, psTerminated);

   IdwsExecution = interface (dwsUtils.IGetSelf)
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

   TRuntimeErrorMessage = class(TErrorMessage)
      private
         FCallStack : TdwsExprLocationArray;

      public
         function AsInfo: UnicodeString; override;

         property CallStack : TdwsExprLocationArray read FCallStack;
   end;

   // TdwsRuntimeMessageList
   //
   TdwsRuntimeMessageList = class (TdwsMessageList)
      public
         procedure AddRuntimeError(const Text: UnicodeString); overload;
         procedure AddRuntimeError(e : Exception); overload;
         procedure AddRuntimeError(const scriptPos : TScriptPos; const Text: UnicodeString;
                                   const callStack : TdwsExprLocationArray); overload;
   end;

   TExecutionStatusResult = (esrNone, esrExit, esrBreak, esrContinue);

   TExprBaseEnumeratorProc = procedure (parent, expr : TExprBase; var abort : Boolean) of object;

   // Is thrown by "raise" statements in script code
   EScriptException = class(Exception)
      private
         FExceptObj : IScriptObj;
         FScriptPos : TScriptPos;
         FScriptCallStack : TdwsExprLocationArray;

      public
         constructor Create(const msgString : UnicodeString; const anExceptionObj : IScriptObj;
                            const aScriptPos: TScriptPos); overload;

         property ExceptionObj : IScriptObj read FExceptObj;
         property ScriptPos : TScriptPos read FScriptPos write FScriptPos;
         property ScriptCallStack : TdwsExprLocationArray read FScriptCallStack write FScriptCallStack;
   end;

   // Is thrown by failed Assert() statements in script code
   EScriptAssertionFailed = class(EScriptException)
   end;
   // Base class for all Exprs

   { TExprBase }

   TExprBaseClass = class of TExprBase;

   TExprBase = class (TRefCountedObject)
      protected
         function GetSubExpr(i : Integer) : TExprBase; virtual;
         function GetSubExprCount : Integer; virtual;

         function  GetIsConstant : Boolean; virtual;

      public
         function  IsConstant : Boolean; inline;

         function  Eval(exec : TdwsExecution) : Variant; virtual; abstract;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; virtual; abstract;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; virtual; abstract;
         function  EvalAsFloat(exec : TdwsExecution) : Double; virtual; abstract;
         procedure EvalAsString(exec : TdwsExecution; var result : UnicodeString); overload; virtual; abstract;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); overload; virtual; abstract;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); virtual; abstract;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); virtual; abstract;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); virtual; abstract;
         procedure EvalNoResult(exec : TdwsExecution); virtual;

         procedure AssignValue(exec : TdwsExecution; const value : Variant); virtual; abstract;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); virtual; abstract;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); virtual; abstract;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); virtual; abstract;
         procedure AssignValueAsString(exec : TdwsExecution; const value : UnicodeString); virtual; abstract;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); virtual; abstract;
         procedure AssignValueAsScriptDynArray(exec : TdwsExecution; const value : IScriptDynArray); virtual; abstract;

         property SubExpr[i : Integer] : TExprBase read GetSubExpr;
         property SubExprCount : Integer read GetSubExprCount;

         function ScriptPos : TScriptPos; virtual; abstract;
         function ScriptLocation(prog : TObject) : UnicodeString; virtual; abstract;

         function FuncSymQualifiedName : UnicodeString; virtual;
         class function CallStackToString(const callStack : TdwsExprLocationArray) : UnicodeString; static;

         // returns True if aborted
         function RecursiveEnumerateSubExprs(const callback : TExprBaseEnumeratorProc) : Boolean;
         function ReferencesVariable(varSymbol : TDataSymbol) : Boolean; virtual;
         function IndexOfSubExpr(expr : TExprBase) : Integer;

         procedure RaiseScriptError(exec : TdwsExecution; e : EScriptError); overload;
         procedure RaiseScriptError(exec : TdwsExecution); overload;
         procedure RaiseScriptError(exec : TdwsExecution; const msg : UnicodeString); overload;
         procedure RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass; const msg : UnicodeString); overload;
         procedure RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass; const msg : UnicodeString;
                                    const args : array of const); overload;

         procedure CheckScriptObject(exec : TdwsExecution; const scriptObj : IScriptObj); inline;
         procedure RaiseObjectNotInstantiated(exec : TdwsExecution);
         procedure RaiseObjectAlreadyDestroyed(exec : TdwsExecution);
   end;

   // All functions callable from the script implement this interface
   IExecutable = interface (IGetSelf)
      ['{8D534D18-4C6B-11D5-8DCB-0000216D9E86}']
      procedure InitSymbol(symbol : TSymbol; const msgs : TdwsCompileMessageList);
      procedure InitExpression(expr : TExprBase);
      function SubExpr(i : Integer) : TExprBase;
      function SubExprCount : Integer;
   end;

   IBooleanEvalable = interface (IExecutable)
      ['{6D0552ED-6FBD-4BC7-AADA-8D8F8DBDF29B}']
      function EvalAsBoolean(exec : TdwsExecution) : Boolean;
   end;

   IStringEvalable = interface (IExecutable)
      ['{6D0552ED-6FBD-4BC7-AADA-8D8F8DBDF29B}']
      procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
   end;

   TAddrGeneratorSign = (agsPositive, agsNegative);

   // TAddrGenerator
   //
   TAddrGeneratorRec = record
      private
         FLevel : SmallInt;
         FSign : TAddrGeneratorSign;

      public
         DataSize : Integer;

         class function CreatePositive(aLevel : SmallInt; anInitialSize : Integer = 0) : TAddrGeneratorRec; static;
         class function CreateNegative(aLevel : SmallInt) : TAddrGeneratorRec; static;

         function GetStackAddr(size : Integer) : Integer;

         property Level : SmallInt read FLevel;
   end;
   TAddrGenerator = ^TAddrGeneratorRec;

   TdwsVisibility = (cvMagic, cvPrivate, cvProtected, cvPublic, cvPublished);
   TdwsVisibilities = set of TdwsVisibility;

   TFuncSymbol = class;

   // TSymbol
   //
   // Named item in the script
   TSymbol = class (TRefCountedObject)
      private
         FName : UnicodeString;

      protected
         FTyp : TTypeSymbol;
         FSize : Integer;

         function SafeGetCaption : UnicodeString;
         function GetCaption : UnicodeString; virtual;
         function GetDescription : UnicodeString; virtual;
         function GetAsFuncSymbol : TFuncSymbol; virtual;

      public
         constructor Create(const aName : UnicodeString; aType : TTypeSymbol);

         procedure Initialize(const msgs : TdwsCompileMessageList); virtual;
         function  BaseType : TTypeSymbol; virtual;
         procedure SetName(const newName : UnicodeString);

         class function IsBaseType : Boolean; virtual;
         function IsType : Boolean; virtual;
         function IsPointerType : Boolean; virtual;
         function AsFuncSymbol : TFuncSymbol; overload;
         function AsFuncSymbol(var funcSym : TFuncSymbol) : Boolean; overload;

         function QualifiedName : UnicodeString; virtual;

         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; virtual;

         property Caption : UnicodeString read SafeGetCaption;
         property Description : UnicodeString read GetDescription;
         property Name : UnicodeString read FName;
         property Typ : TTypeSymbol read FTyp write FTyp;
         property Size : Integer read FSize;
   end;

   TSymbolClass = class of TSymbol;

   // return True to abort
   TSymbolEnumerationCallback = function (symbol : TSymbol) : Boolean of object;

   THelperSymbolEnumerationCallback = function (helper : THelperSymbol) : Boolean of object;

   TOperatorSymbolEnumerationCallback = function (opSym : TOperatorSymbol) : Boolean of object;

   TSymbolTableFlag = (stfSorted,
                       stfHasChildTables, stfHasHelpers,
                       stfHasLocalOperators, stfHasParentOperators, stfHasOperators);
   TSymbolTableFlags = set of TSymbolTableFlag;

   TSimpleSymbolList = TSimpleList<TSymbol>;

   // A table of symbols connected to other symboltables (property Parents)
   TSymbolTable = class (TRefCountedObject)
      private
         FAddrGenerator : TAddrGenerator;
         FSymbols : TTightList;
         FParents : TTightList;
         FFlags : TSymbolTableFlags;

         function GetParentCount : Integer;
         function GetParents(Index: Integer) : TSymbolTable;

      protected
         function GetSymbol(Index: Integer): TSymbol; inline;
         function GetCount : Integer; inline;

         procedure SortSymbols(minIndex, maxIndex : Integer);
         function FindLocalSorted(const name : UnicodeString) : TSymbol;
         function FindLocalUnSorted(const name : UnicodeString) : TSymbol;

      public
         constructor Create(parent : TSymbolTable = nil; addrGenerator: TAddrGenerator = nil);
         destructor Destroy; override;

         procedure InsertParent(index : Integer; parent : TSymbolTable); virtual;
         function RemoveParent(parent : TSymbolTable) : Integer; virtual;
         function IndexOfParent(parent : TSymbolTable) : Integer;
         procedure MoveParent(curIndex, newIndex : Integer);
         procedure ClearParents; virtual;
         procedure AddParent(parent : TSymbolTable);

         function AddSymbol(sym : TSymbol): Integer;
         function AddSymbolDirect(sym : TSymbol) : Integer;
         function FindLocal(const aName : UnicodeString; ofClass : TSymbolClass = nil) : TSymbol; virtual;
         function FindTypeLocal(const aName : UnicodeString) : TTypeSymbol;
         function FindSymbolAtStackAddr(const stackAddr, level : Integer) : TDataSymbol;
         function Remove(sym : TSymbol): Integer;
         procedure Clear;

         procedure TransferSymbolsTo(destTable : TSymbolTable);

         function FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility;
                             ofClass : TSymbolClass = nil) : TSymbol; virtual;
         function FindTypeSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility) : TTypeSymbol;

         // returns True if aborted
         function EnumerateLocalSymbolsOfName(const aName : UnicodeString;
                              const callback : TSymbolEnumerationCallback) : Boolean; virtual;
         function EnumerateSymbolsOfNameInScope(const aName : UnicodeString;
                              const callback : TSymbolEnumerationCallback) : Boolean; virtual;

         function EnumerateLocalHelpers(helpedType : TTypeSymbol;
                              const callback : THelperSymbolEnumerationCallback) : Boolean; virtual;
         function EnumerateHelpers(helpedType : TTypeSymbol;
                              const callback : THelperSymbolEnumerationCallback) : Boolean; virtual;

         function EnumerateLocalOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean; virtual;
         function EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                        const callback : TOperatorSymbolEnumerationCallback) : Boolean; virtual;
         function HasSameLocalOperator(anOpSym : TOperatorSymbol) : Boolean; virtual;
         function HasOperators : Boolean; inline;

         procedure CollectPublishedSymbols(symbolList : TSimpleSymbolList); virtual;

         function HasChildTables : Boolean; inline;
         function HasClass(const aClass : TSymbolClass) : Boolean;
         function HasSymbol(sym : TSymbol) : Boolean;
         function HasMethods : Boolean;
         class function IsUnitTable : Boolean; virtual;

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

   // TUnSortedSymbolTable
   //
   TUnSortedSymbolTable = class (TSymbolTable)
      public
         function FindLocal(const aName : UnicodeString; ofClass : TSymbolClass = nil) : TSymbol; override;
   end;

   // TConditionsSymbolTable
   //
   TConditionsSymbolTable = class (TUnSortedSymbolTable)
   end;

   // TParamsSymbolTable
   //
   TParamsSymbolTable = class (TUnSortedSymbolTable)
      protected
         function GetSymbol(x : Integer) : TParamSymbol;
      public
         property Symbols[x : Integer] : TParamSymbol read GetSymbol; default;
   end;

   // TExpressionSymbolTable
   //
   TExpressionSymbolTable = class (TSymbolTable)
   end;

   // A resource string (hybrid between a constant and a function)
   TResourceStringSymbol = class sealed (TSymbol)
      private
         FValue : UnicodeString;
         FIndex : Integer;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

      public
         constructor Create(const aName, aValue : UnicodeString);

         property Value : UnicodeString read FValue;
         property Index : Integer read FIndex write FIndex;
   end;

   TResourceStringSymbolList = class(TSimpleList<TResourceStringSymbol>)
      public
         procedure ComputeIndexes;
   end;

   // All Symbols containing a value
   TValueSymbol = class (TSymbol)
      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

      public
         constructor Create(const aName : UnicodeString; aType : TTypeSymbol);
   end;

   // named constant: const x = 123;
   TConstSymbol = class (TValueSymbol)
      protected
         FData : TData;

         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

      public
         constructor CreateValue(const name : UnicodeString; typ : TTypeSymbol; const value : Variant); overload;
         constructor CreateData(const name : UnicodeString; typ : TTypeSymbol; const data : TData); overload;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         property Data : TData read FData;
   end;
   TConstSymbolClass = class of TConstSymbol;

   // variable: var x: Integer;
   TDataSymbol = class (TValueSymbol)
      protected
         FExternalName : UnicodeString;
         FStackAddr : Integer;
         FLevel : SmallInt;

         function GetDescription : UnicodeString; override;
         function GetExternalName : UnicodeString;

      public
         procedure AllocateStackAddr(generator : TAddrGenerator);

         function HasExternalName : Boolean;

         property ExternalName : UnicodeString read GetExternalName write FExternalName;
         property Level : SmallInt read FLevel write FLevel;
         property StackAddr: Integer read FStackAddr write FStackAddr;
   end;

   // used for script engine internal purposes
   TScriptDataSymbol = class (TDataSymbol)
   end;

   // parameter: procedure P(x: Integer);
   TParamSymbol = class (TDataSymbol)
      public
         function Clone : TParamSymbol; virtual;
         function SameParam(other : TParamSymbol) : Boolean; virtual;
   end;

   THasParamSymbolMethod = function (param : TParamSymbol) : Boolean of object;
   TAddParamSymbolMethod = procedure (param : TParamSymbol) of object;

   TParamSymbolWithDefaultValue = class sealed (TParamSymbol)
      private
         FDefaultValue : TData;

      protected
         function GetDescription : UnicodeString; override;

      public
         constructor Create(const aName : UnicodeString; aType : TTypeSymbol;
                            const data : TData);

         function Clone : TParamSymbol; override;
         function SameParam(other : TParamSymbol) : Boolean; override;

         property DefaultValue : TData read FDefaultValue;
   end;

   // const/var parameter: procedure P(const/var x: Integer)
   TByRefParamSymbol = class(TParamSymbol)
      public
         constructor Create(const Name: UnicodeString; Typ: TTypeSymbol);
         function Clone : TParamSymbol; override;
   end;

   // lazy parameter: procedure P(lazy x: Integer)
   TLazyParamSymbol = class sealed (TParamSymbol)
      protected
         function GetDescription : UnicodeString; override;
      public
         function Clone : TParamSymbol; override;
   end;

   // const parameter: procedure P(const x: Integer)
   TConstParamSymbol = class sealed (TByRefParamSymbol)
      protected
         function GetDescription : UnicodeString; override;
      public
         function Clone : TParamSymbol; override;
   end;

   // var parameter: procedure P(var x: Integer)
   TVarParamSymbol = class sealed (TByRefParamSymbol)
      protected
         function GetDescription : UnicodeString; override;
      public
         function Clone : TParamSymbol; override;
   end;

   TTypeSymbolClass = class of TTypeSymbol;
   TTypeSymbols = array of TTypeSymbol;

   // Base class for all types
   TTypeSymbol = class(TSymbol)
      private
         FDeprecatedMessage : UnicodeString;

      protected
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; virtual;

         function GetIsDeprecated : Boolean; inline;

      public
         procedure InitData(const data : TData; offset : Integer); virtual;

         function IsType : Boolean; override;
         function BaseType : TTypeSymbol; override;
         function UnAliasedType : TTypeSymbol; virtual;
         function UnAliasedTypeIs(const typeSymbolClass : TTypeSymbolClass) : Boolean; inline;
         function IsOfType(typSym : TTypeSymbol) : Boolean;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; virtual;
         function DistanceTo(typeSym : TTypeSymbol) : Integer; virtual;
         // doesn't treat aliases of a type as the the same type,
         // but identical declarations are
         function SameType(typSym : TTypeSymbol) : Boolean; virtual;
         function HasMetaSymbol : Boolean; virtual;

         property DeprecatedMessage : UnicodeString read FDeprecatedMessage write FDeprecatedMessage;
         property IsDeprecated : Boolean read GetIsDeprecated;
   end;

   TAnyTypeSymbol = class(TTypeSymbol)
      public
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   TFuncKind = (fkFunction, fkProcedure,
                fkConstructor, fkDestructor, fkMethod,
                fkLambda);

   // Record used for TFuncSymbol.Generate
   PParamRec = ^TParamRec;
   TParamRec = record
      ParamName : UnicodeString;
      ParamType : UnicodeString;
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
         constructor Create(const aScriptPos: TScriptPos; const cond : IBooleanEvalable; const msg : IStringEvalable);

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

   TResultSymbol = class(TDataSymbol)
   end;

   TFuncSymbolFlag = (fsfStateless, fsfExternal, fsfType, fsfOverloaded, fsfLambda,
                      fsfInline, fsfProperty, fsfExport);
   TFuncSymbolFlags = set of TFuncSymbolFlag;

   // A script function / procedure: procedure X(param: Integer);
   TFuncSymbol = class (TTypeSymbol)
      protected
         FAddrGenerator : TAddrGeneratorRec;
         FExecutable : IExecutable;
         FInternalParams : TSymbolTable;
         FForwardPosition : PScriptPos;
         FParams : TParamsSymbolTable;
         FResult : TDataSymbol;
         FConditions : TConditionsSymbolTable;
         FFlags : TFuncSymbolFlags;
         FKind : TFuncKind;
         FExternalName : UnicodeString;
         FExternalConvention: TTokenType;

         procedure SetType(const Value: TTypeSymbol);
         function GetCaption : UnicodeString; override;
         function GetIsForwarded : Boolean;
         function GetDescription : UnicodeString; override;
         function GetLevel : SmallInt; inline;
         function GetParamSize : Integer; inline;
         function GetIsStateless : Boolean; inline;
         procedure SetIsStateless(const val : Boolean);
         function GetIsExternal : Boolean; inline;
         procedure SetIsExternal(const val : Boolean);
         function GetIsExport : Boolean; inline;
         procedure SetIsExport(const val : Boolean);
         function GetIsProperty : Boolean; inline;
         procedure SetIsProperty(const val : Boolean);
         function GetIsOverloaded : Boolean; inline;
         procedure SetIsOverloaded(const val : Boolean);
         function GetIsLambda : Boolean; inline;
         procedure SetIsLambda(const val : Boolean);
         function GetSourcePosition : TScriptPos; virtual;
         procedure SetSourcePosition(const val : TScriptPos); virtual;
         function GetExternalName : UnicodeString;

         function GetSourceSubExpr(i : Integer) : TExprBase;
         function GetSourceSubExprCount : Integer;

         function  DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; funcKind : TFuncKind; funcLevel : SmallInt);
         destructor Destroy; override;

         constructor Generate(table : TSymbolTable; const funcName : UnicodeString;
                              const funcParams : TParamArray; const funcType : UnicodeString);
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function  IsType : Boolean; override;
         procedure SetIsType;
         function  GetAsFuncSymbol : TFuncSymbol; override;
         procedure SetInline;
         procedure AddParam(param : TParamSymbol);
         procedure AddParams(params : TParamsSymbolTable);
         function  HasParam(param : TParamSymbol) : Boolean;
         procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
         function  GetParamType(idx : Integer) : TTypeSymbol;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         procedure InitData(const data : TData; offset : Integer); override;
         procedure AddCondition(cond : TConditionSymbol);

         function  IsValidOverloadOf(other : TFuncSymbol) : Boolean;
         function  IsSameOverloadOf(other : TFuncSymbol) : Boolean; virtual;

         function  ParamsDescription : UnicodeString; virtual;

         procedure SetForwardedPos(const aScriptPos: TScriptPos);
         procedure ClearIsForwarded;

         property SubExpr[i : Integer] : TExprBase read GetSourceSubExpr;
         property SubExprCount : Integer read GetSourceSubExprCount;

         property Executable : IExecutable read FExecutable write FExecutable;
         property IsDeprecated : Boolean read GetIsDeprecated;
         property IsStateless : Boolean read GetIsStateless write SetIsStateless;
         property IsForwarded : Boolean read GetIsForwarded;
         property IsOverloaded : Boolean read GetIsOverloaded write SetIsOverloaded;
         property IsExternal : Boolean read GetIsExternal write SetIsExternal;
         property IsExport : Boolean read GetIsExport write SetIsExport;
         property IsProperty : Boolean read GetIsProperty write SetIsProperty;
         property Kind : TFuncKind read FKind write FKind;
         property ExternalName : UnicodeString read GetExternalName write FExternalName;
         function HasExternalName : Boolean;
         property ExternalConvention: TTokenType read FExternalConvention write FExternalConvention;
         property IsLambda : Boolean read GetIsLambda write SetIsLambda;
         property Level : SmallInt read GetLevel;
         property InternalParams : TSymbolTable read FInternalParams;
         property Params : TParamsSymbolTable read FParams;
         property ParamSize : Integer read FAddrGenerator.DataSize;
         property Result : TDataSymbol read FResult;
         property Typ : TTypeSymbol read FTyp write SetType;
         property Conditions : TConditionsSymbolTable read FConditions;
         property SourcePosition : TScriptPos read GetSourcePosition write SetSourcePosition;
   end;

   // referring list of function symbols
   TFuncSymbolList = class(TSimpleList<TFuncSymbol>)
      public
         function ContainsChildMethodOf(methSym : TMethodSymbol) : Boolean;
   end;

   TAnyFuncSymbol = class(TFuncSymbol)
      public
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
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

   // TSelfSymbol
   //
   TSelfSymbol = class sealed (TDataSymbol)
   end;

   TMethodKind = ( mkProcedure, mkFunction, mkConstructor, mkDestructor, mkMethod,
                   mkClassProcedure, mkClassFunction, mkClassMethod );
   TMethodAttribute = ( maVirtual, maOverride, maReintroduce, maAbstract,
                        maOverlap, maClassMethod, maFinal, maDefault, maInterfaced,
                        maStatic );
   TMethodAttributes = set of TMethodAttribute;

   // A method of a script class: TMyClass = class procedure X(param: UnicodeString); end;
   TMethodSymbol = class (TFuncSymbol)
      private
         FStructSymbol : TCompositeTypeSymbol;
         FParentMeth : TMethodSymbol;
         FSelfSym : TDataSymbol;
         FVMTIndex : Integer;
         FVisibility : TdwsVisibility;
         FAttributes : TMethodAttributes;

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
         function GetIsInterfaced : Boolean; inline;
         procedure SetIsInterfaced(const val : Boolean); inline;
         function GetIsDefault : Boolean; inline;
         procedure SetIsDefault(const val : Boolean); inline;
         function GetIsStatic : Boolean; inline;

         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

         function GetRootParentMeth : TMethodSymbol;

      public
         constructor Create(const Name: UnicodeString; FuncKind: TFuncKind; aStructSymbol : TCompositeTypeSymbol;
                            aVisibility : TdwsVisibility; isClassMethod : Boolean;
                            funcLevel : Integer = 1); virtual;
         constructor Generate(Table: TSymbolTable; MethKind: TMethodKind;
                              const Attributes: TMethodAttributes; const MethName: UnicodeString;
                              const MethParams: TParamArray; const MethType: UnicodeString;
                              Cls: TCompositeTypeSymbol; aVisibility : TdwsVisibility;
                              overloaded : Boolean);

         procedure SetOverride(meth: TMethodSymbol);
         procedure SetOverlap(meth: TMethodSymbol);
         procedure SetIsFinal;
         procedure SetIsStatic;
         procedure InitData(const data : TData; offset : Integer); override;
         function QualifiedName : UnicodeString; override;
         function HasConditions : Boolean;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;
         function IsSameOverloadOf(other : TFuncSymbol) : Boolean; override;

         property StructSymbol : TCompositeTypeSymbol read FStructSymbol;
         property VMTIndex : Integer read FVMTIndex;
         property IsDefault : Boolean read GetIsDefault write SetIsDefault;
         property IsAbstract : Boolean read GetIsAbstract write SetIsAbstract;
         property IsVirtual : Boolean read GetIsVirtual write SetIsVirtual;
         property IsOverride : Boolean read GetIsOverride;
         property IsInterfaced : Boolean read GetIsInterfaced write SetIsInterfaced;
         property IsFinal : Boolean read GetIsFinal;
         property IsOverlap : Boolean read GetIsOverlap;
         property IsClassMethod : Boolean read GetIsClassMethod;
         property IsStatic : Boolean read GetIsStatic;
         property ParentMeth : TMethodSymbol read FParentMeth;
         property RootParentMeth : TMethodSymbol read GetRootParentMeth;
         property SelfSym : TDataSymbol read FSelfSym;
         property Visibility : TdwsVisibility read FVisibility;
   end;

   TMethodSymbolArray = array of TMethodSymbol;

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

   TAliasMethodSymbol = class sealed (TSourceMethodSymbol)
      private
         FAlias : TFuncSymbol;

      protected
         function GetSourcePosition : TScriptPos; override;

      public
         function IsPointerType : Boolean; override;

         function ParamsDescription : UnicodeString; override;

         property Alias : TFuncSymbol read FAlias write FAlias;
   end;

   TOperatorSymbol = class sealed (TSymbol)
      private
         FToken : TTokenType;
         FParams : TTypeSymbols;
         FUsesSym : TFuncSymbol;
         FBinExprClass : TExprBaseClass;
         FAssignExprClass : TExprBaseClass;

      protected
         function GetCaption : UnicodeString; override;

      public
         constructor Create(const aTokenType : TTokenType);

         procedure AddParam(p : TTypeSymbol);

         property Token : TTokenType read FToken write FToken;
         property Params : TTypeSymbols read FParams;
         property UsesSym : TFuncSymbol read FUsesSym write FUsesSym;
         property BinExprClass : TExprBaseClass read FBinExprClass write FBinExprClass;
         property AssignExprClass : TExprBaseClass read FAssignExprClass write FAssignExprClass;
   end;

   // type x = TMyType;
   TAliasSymbol = class sealed (TTypeSymbol)
      protected
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;
         function GetAsFuncSymbol : TFuncSymbol; override;

      public
         function BaseType : TTypeSymbol; override;
         function UnAliasedType : TTypeSymbol; override;
         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsPointerType : Boolean; override;
   end;

   // integer/UnicodeString/float/boolean/variant
   TBaseSymbol = class(TTypeSymbol)
      public
         constructor Create(const name : UnicodeString);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         class function IsBaseType : Boolean; override;
   end;

   TBaseIntegerSymbol = class (TBaseSymbol)
      public
         constructor Create;

         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
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
         constructor Create(const name : UnicodeString = '');

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;
         function SupportsEmptyParam : Boolean; virtual;
   end;

   TSetOfSymbol = class sealed (TTypeSymbol)
      private
         FMinValue : Integer;
         FCountValue : Integer;

      protected
         function GetMaxValue : Integer; inline;

      public
         constructor Create(const name : UnicodeString; indexType : TTypeSymbol;
                            aMin, aMax : Integer);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         procedure InitData(const data : TData; offset : Integer); override;

         function ValueToOffsetMask(value : Integer; var mask : Int64) : Integer; inline;
         function ValueToByteOffsetMask(value : Integer; var mask : Byte) : Integer; inline;

         property MinValue : Integer read FMinValue write FMinValue;
         property MaxValue : Integer read GetMaxValue;
         property CountValue : Integer read FCountValue write FCountValue;
   end;

   TArraySymbol = class abstract (TTypeSymbol)
      private
         FIndexType : TTypeSymbol;
         FSortFunctionType : TFuncSymbol;
         FMapFunctionType : TFuncSymbol;

      protected
         function ElementSize : Integer;

      public
         constructor Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
         destructor Destroy; override;

         function SortFunctionType(integerType : TTypeSymbol) : TFuncSymbol;
         function MapFunctionType(anyType : TTypeSymbol) : TFuncSymbol;

         property IndexType : TTypeSymbol read FIndexType write FIndexType;
   end;

   TInitDynamicArrayProc = procedure (typ : TTypeSymbol; var result : Variant);

   // array of FTyp
   TDynamicArraySymbol = class sealed (TArraySymbol)
      protected
         function GetCaption : UnicodeString; override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
         procedure InitData(const Data: TData; Offset: Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsPointerType : Boolean; override;
         function SameType(typSym : TTypeSymbol) : Boolean; override;

         class procedure SetInitDynamicArrayProc(const aProc : TInitDynamicArrayProc);
   end;

   // array [FLowBound..FHighBound] of FTyp
   TStaticArraySymbol = class (TArraySymbol)
      private
         FHighBound : Integer;
         FLowBound : Integer;
         FElementCount : Integer;

      protected
         function GetCaption : UnicodeString; override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; elementType, indexType : TTypeSymbol;
                            lowBound, highBound : Integer);

         procedure InitData(const Data: TData; Offset: Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function SameType(typSym : TTypeSymbol) : Boolean; override;
         procedure AddElement;
         function IsEmptyArray : Boolean;

         property HighBound : Integer read FHighBound;
         property LowBound : Integer read FLowBound;
         property ElementCount : Integer read FElementCount;
   end;

   // static array whose bounds are contextual
   TOpenArraySymbol = class sealed (TStaticArraySymbol)
      protected
         function GetCaption : UnicodeString; override;

      public
         constructor Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   // TMembersSymbolTable
   //
   TMembersSymbolTable = class (TSymbolTable)
      private
         FOwner : TCompositeTypeSymbol;

      public
         procedure AddParent(parent : TMembersSymbolTable);
         function FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility; ofClass : TSymbolClass = nil) : TSymbol; override;
         function VisibilityFromScope(scopeSym : TCompositeTypeSymbol) : TdwsVisibility;
         function FindSymbolFromScope(const aName : UnicodeString; scopeSym : TCompositeTypeSymbol) : TSymbol; reintroduce;
         function Visibilities : TdwsVisibilities;

         property Owner : TCompositeTypeSymbol read FOwner write FOwner;
   end;

   TStructuredTypeMetaSymbol = class;

   // Const attached to a class
   TClassConstSymbol = class sealed (TConstSymbol)
      protected
         FOwnerSymbol : TCompositeTypeSymbol;
         FVisibility : TdwsVisibility;

      public
         function QualifiedName : UnicodeString; override;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;

         property OwnerSymbol : TCompositeTypeSymbol read FOwnerSymbol write FOwnerSymbol;
         property Visibility : TdwsVisibility read FVisibility write FVisibility;
   end;

   // Var attached to a class
   TClassVarSymbol = class sealed (TDataSymbol)
      protected
         FOwnerSymbol : TCompositeTypeSymbol;
         FVisibility : TdwsVisibility;

      public
         function QualifiedName : UnicodeString; override;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;

         property OwnerSymbol : TCompositeTypeSymbol read FOwnerSymbol write FOwnerSymbol;
         property Visibility : TdwsVisibility read FVisibility write FVisibility;
   end;

   // type symbol with members
   TCompositeTypeSymbol = class(TTypeSymbol)
      private
         FUnitSymbol : TSymbol;
         FParent : TCompositeTypeSymbol;
         FMembers : TMembersSymbolTable;
         FDefaultProperty : TPropertySymbol;
         FFirstField : TFieldSymbol;

      protected
         function CreateMembersTable : TMembersSymbolTable; virtual;

         function GetIsStatic : Boolean; virtual;
         function GetIsExternal : Boolean; virtual;
         function GetIsExternalRooted : Boolean; virtual;
         function GetExternalName : UnicodeString; virtual;
         function GetIsPartial : Boolean; virtual;
         function GetIsImmutable : Boolean; virtual;

         procedure CheckMethodsImplemented(const msgs : TdwsCompileMessageList);

         function PrepareFirstField : TFieldSymbol;

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);
         destructor Destroy; override;

         procedure AddConst(sym : TClassConstSymbol); overload;
         procedure AddConst(sym : TClassConstSymbol; visibility : TdwsVisibility); overload;
         procedure AddClassVar(sym : TClassVarSymbol);
         procedure AddProperty(propSym : TPropertySymbol);
         procedure AddMethod(methSym : TMethodSymbol); virtual;
         procedure AddField(fieldSym : TFieldSymbol); virtual;

         function FieldAtOffset(offset : Integer) : TFieldSymbol; virtual;

         function AllowVirtualMembers : Boolean; virtual;
         function AllowOverloads : Boolean; virtual;
         function AllowDefaultProperty : Boolean; virtual; abstract;
         function AllowFields : Boolean; virtual;
         function AllowAnonymousMethods : Boolean; virtual;

         function FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol; virtual;

         function MembersVisibilities : TdwsVisibilities;

         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; virtual; abstract;
         function CreateAnonymousMethod(aFuncKind : TFuncKind; aVisibility : TdwsVisibility;
                                        isClassMethod : Boolean) : TMethodSymbol; virtual; abstract;

         function FirstField : TFieldSymbol; inline;

         function ExternalRoot : TCompositeTypeSymbol;

         property UnitSymbol : TSymbol read FUnitSymbol;
         property Parent : TCompositeTypeSymbol read FParent;
         property Members : TMembersSymbolTable read FMembers;
         property DefaultProperty : TPropertySymbol read FDefaultProperty write FDefaultProperty;

         property IsStatic : Boolean read GetIsStatic;
         property IsPartial : Boolean read GetIsPartial;
         property IsExternal : Boolean read GetIsExternal;
         property IsExternalRooted : Boolean read GetIsExternalRooted;
         property ExternalName : UnicodeString read GetExternalName;
         property IsImmutable : Boolean read GetIsImmutable;
   end;

   // class, record, interface
   TStructuredTypeSymbol = class(TCompositeTypeSymbol)
      private
         FMetaSymbol : TStructuredTypeMetaSymbol;
         FForwardPosition : PScriptPos;
         FExternalName : UnicodeString;

      protected
         function GetIsForwarded : Boolean; inline;
         function GetIsExternal : Boolean; override;
         function GetExternalName : UnicodeString; override;

         procedure DoInheritFrom(ancestor : TStructuredTypeSymbol);

      public
         destructor Destroy; override;

         function DuckTypedMatchingMethod(methSym : TMethodSymbol; visibility : TdwsVisibility) : TMethodSymbol; virtual;

         function NthParentOf(structType : TCompositeTypeSymbol) : Integer;
         function DistanceTo(typeSym : TTypeSymbol) : Integer; override;
         function FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol; override;
         function AllowDefaultProperty : Boolean; override;

         procedure SetForwardedPos(const aScriptPos: TScriptPos);
         procedure ClearIsForwarded;

         property IsForwarded : Boolean read GetIsForwarded;
         property ExternalName : UnicodeString read GetExternalName write FExternalName;

         property MetaSymbol : TStructuredTypeMetaSymbol read FMetaSymbol;
   end;

   // class of, record of
   TStructuredTypeMetaSymbol = class (TTypeSymbol)
      public
         constructor Create(const name : UnicodeString; typ : TStructuredTypeSymbol);

         procedure InitData(const Data: TData; Offset: Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         function StructSymbol : TStructuredTypeSymbol; inline;

         function Parent : TStructuredTypeMetaSymbol;
   end;

   // field of a script object
   TFieldSymbol = class sealed (TValueSymbol)
      protected
         FStructSymbol : TCompositeTypeSymbol;
         FOffset : Integer;
         FVisibility : TdwsVisibility;
         FDefaultValue : TData;
         FDefaultExpr : TExprBase;
         FExternalName : UnicodeString;
         FNextField : TFieldSymbol;

         function GetExternalName : UnicodeString;

      public
         constructor Create(const name : UnicodeString; typ : TTypeSymbol;
                            aVisibility : TdwsVisibility);
         destructor Destroy; override;

         function QualifiedName : UnicodeString; override;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;
         function HasExternalName : Boolean; inline;

         procedure InitData(const data : TData; structOffset : Integer);

         property StructSymbol : TCompositeTypeSymbol read FStructSymbol;
         property Offset : Integer read FOffset;
         property Visibility : TdwsVisibility read FVisibility write FVisibility;
         property DefaultValue : TData read FDefaultValue write FDefaultValue;
         property DefaultExpr : TExprBase read FDefaultExpr write FDefaultExpr;
         property ExternalName : UnicodeString read GetExternalName write FExternalName;
         property NextField : TFieldSymbol read FNextField write FNextField;
   end;

   TRecordSymbolFlag = (rsfDynamic, rsfFullyDefined, rsfImmutable);
   TRecordSymbolFlags = set of TRecordSymbolFlag;

   // record member1: Integer; member2: Integer end;
   TRecordSymbol = class sealed (TStructuredTypeSymbol)
      private
         FFlags : TRecordSymbolFlags;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

         function GetIsDynamic : Boolean; inline;
         procedure SetIsDynamic(const val : Boolean);
         function GetIsImmutable : Boolean; override;
         procedure SetIsImmutable(const val : Boolean);
         function GetIsFullyDefined : Boolean; inline;
         procedure SetIsFullyDefined(const val : Boolean);

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);

         procedure AddField(fieldSym : TFieldSymbol); override;
         procedure AddMethod(methSym : TMethodSymbol); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         function AllowFields : Boolean; override;

         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;
         function CreateAnonymousMethod(aFuncKind : TFuncKind; aVisibility : TdwsVisibility;
                                        isClassMethod : Boolean) : TMethodSymbol; override;

         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         property IsDynamic : Boolean read GetIsDynamic write SetIsDynamic;
         property IsImmutable : Boolean read GetIsImmutable write SetIsImmutable;
         property IsFullyDefined : Boolean read GetIsFullyDefined write SetIsFullyDefined;
   end;

   // interface
   TInterfaceSymbol = class sealed (TStructuredTypeSymbol)
      private
         FMethodCount : Integer;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;
         function  DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);

         procedure InheritFrom(ancestor : TInterfaceSymbol);

         procedure AddMethod(methSym : TMethodSymbol); override;

         procedure InitData(const Data: TData; Offset: Integer); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsPointerType : Boolean; override;

         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;
         function CreateAnonymousMethod(aFuncKind : TFuncKind; aVisibility : TdwsVisibility;
                                        isClassMethod : Boolean) : TMethodSymbol; override;

         function Parent : TInterfaceSymbol; inline;
         property MethodCount : Integer read FMethodCount;
   end;

   // property X: Integer read FReadSym write FWriteSym;
   TPropertySymbol = class sealed (TValueSymbol)
      private
         FOwnerSymbol : TCompositeTypeSymbol;
         FReadSym : TSymbol;
         FWriteSym : TSymbol;
         FArrayIndices : TParamsSymbolTable;
         FIndexSym : TTypeSymbol;
         FIndexValue: TData;
         FDefaultSym : TConstSymbol;
         FVisibility : TdwsVisibility;
         FDeprecatedMessage : UnicodeString;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;
         function GetIsDefault: Boolean;
         function GetArrayIndices : TParamsSymbolTable;
         procedure AddParam(Param : TParamSymbol);
         function GetIsDeprecated : Boolean; inline;

      public
         constructor Create(const name : UnicodeString; typ : TTypeSymbol; aVisibility : TdwsVisibility;
                            aArrayIndices : TParamsSymbolTable);
         destructor Destroy; override;

         procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
         procedure SetIndex(const data : TData; Sym: TTypeSymbol);
         function GetArrayIndicesDescription: UnicodeString;
         function QualifiedName : UnicodeString; override;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;
         function HasArrayIndices : Boolean;

         property OwnerSymbol : TCompositeTypeSymbol read FOwnerSymbol;
         property Visibility : TdwsVisibility read FVisibility write FVisibility;
         property ArrayIndices : TParamsSymbolTable read GetArrayIndices;
         property ReadSym : TSymbol read FReadSym write FReadSym;
         property WriteSym : TSymbol read FWriteSym write FWriteSym;
         property IsDefault : Boolean read GetIsDefault;
         property IndexValue : TData read FIndexValue;
         property IndexSym : TTypeSymbol read FIndexSym;
         property DefaultSym : TConstSymbol read FDefaultSym write FDefaultSym;
         property DeprecatedMessage : UnicodeString read FDeprecatedMessage write FDeprecatedMessage;
         property IsDeprecated : Boolean read GetIsDeprecated;
   end;

   // class operator X (params) uses method;
   TClassOperatorSymbol = class sealed (TSymbol)
      private
         FCompositeSymbol : TCompositeTypeSymbol;
         FTokenType : TTokenType;
         FUsesSym : TMethodSymbol;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

      public
         constructor Create(tokenType : TTokenType);
         function QualifiedName : UnicodeString; override;

         property CompositeSymbol : TCompositeTypeSymbol read FCompositeSymbol write FCompositeSymbol;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property UsesSym : TMethodSymbol read FUsesSym write FUsesSym;
   end;

   // type X = class of TMyClass;
   TClassOfSymbol = class sealed (TStructuredTypeMetaSymbol)
      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; typ : TClassSymbol);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function SameType(typSym : TTypeSymbol) : Boolean; override;
         function TypClassSymbol : TClassSymbol; inline;
   end;

   // A resolved interface (attached to a class symbol)
   TResolvedInterface = record
      IntfSymbol : TInterfaceSymbol;
      VMT : TMethodSymbolArray;
   end;

   TResolvedInterfaces = class (TSimpleHash<TResolvedInterface>)
      protected
         function SameItem(const item1, item2 : TResolvedInterface) : Boolean; override;
         function GetItemHashCode(const item1 : TResolvedInterface) : Integer; override;
   end;

   TObjectDestroyEvent = procedure (ExternalObject: TObject) of object;

   TClassSymbolFlag = (csfAbstract, csfExplicitAbstract, csfSealed,
                       csfStatic, csfExternal, csfPartial,
                       csfNoVirtualMembers, csfNoOverloads,
                       csfExternalRooted,
                       csfInitialized,
                       csfAttribute);
   TClassSymbolFlags = set of TClassSymbolFlag;

   // type X = class ... end;
   TClassSymbol = class sealed (TStructuredTypeSymbol)
      private
         FFlags : TClassSymbolFlags;
         FOperators : TTightList;
         FScriptInstanceSize : Integer;
         FOnObjectDestroy : TObjectDestroyEvent;
         FVirtualMethodTable : TMethodSymbolArray;
         FInterfaces : TResolvedInterfaces;

      protected
         function GetDescription : UnicodeString; override;
         function GetIsExplicitAbstract : Boolean; inline;
         procedure SetIsExplicitAbstract(const val : Boolean); inline;
         function GetIsAbstract : Boolean; inline;
         function GetIsSealed : Boolean; inline;
         procedure SetIsSealed(const val : Boolean); inline;
         function GetIsStatic : Boolean; override;
         procedure SetIsStatic(const val : Boolean); inline;
         function GetIsExternal : Boolean; override;
         procedure SetIsExternal(const val : Boolean); inline;
         function GetIsExternalRooted : Boolean; override;
         function GetIsPartial : Boolean; override;
         function GetIsAttribute : Boolean; inline;
         procedure SetIsAttribute(const val : Boolean); inline;

         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

         procedure ProcessOverriddenInterfaceCallback(const item : TResolvedInterface);
         procedure ProcessOverriddenInterfaces;
         function  ProcessOverriddenInterface(const ancestorResolved : TResolvedInterface) : Boolean; // True if added

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);
         destructor Destroy; override;

         procedure AddField(fieldSym : TFieldSymbol); override;
         procedure AddMethod(methSym : TMethodSymbol); override;
         procedure AddOperator(Sym: TClassOperatorSymbol);

         function  AddInterface(intfSym : TInterfaceSymbol; visibility : TdwsVisibility;
                                var missingMethod : TMethodSymbol) : Boolean; // True if added
         function  ResolveInterface(intfSym : TInterfaceSymbol; var resolved : TResolvedInterface) : Boolean;
         function  ImplementsInterface(intfSym : TInterfaceSymbol) : Boolean;
         procedure SetIsPartial; inline;
         procedure SetNoVirtualMembers; inline;
         procedure SetNoOverloads; inline;

         function  FieldAtOffset(offset : Integer) : TFieldSymbol; override;
         procedure InheritFrom(ancestorClassSym : TClassSymbol);
         procedure InitData(const Data: TData; Offset: Integer); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function  IsPointerType : Boolean; override;
         function  HasMetaSymbol : Boolean; override;

         function VMTMethod(index : Integer) : TMethodSymbol;
         function VMTCount : Integer;

         function FindClassOperatorStrict(tokenType : TTokenType; paramType : TSymbol; recursive : Boolean) : TClassOperatorSymbol;
         function FindClassOperator(tokenType : TTokenType; paramType : TTypeSymbol) : TClassOperatorSymbol;

         function FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol; override;

         function AllowVirtualMembers : Boolean; override;
         function AllowOverloads : Boolean; override;
         function AllowFields : Boolean; override;
         function AllowAnonymousMethods : Boolean; override;

         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;
         function CreateAnonymousMethod(aFuncKind : TFuncKind; aVisibility : TdwsVisibility;
                                        isClassMethod : Boolean) : TMethodSymbol; override;

         class function VisibilityToString(visibility : TdwsVisibility) : UnicodeString; static;

         function Parent : TClassSymbol; inline;
         property ScriptInstanceSize : Integer read FScriptInstanceSize;
         property Interfaces : TResolvedInterfaces read FInterfaces;
         property Flags : TClassSymbolFlags read FFlags;

         property IsExplicitAbstract : Boolean read GetIsExplicitAbstract write SetIsExplicitAbstract;
         property IsAbstract : Boolean read GetIsAbstract;
         property IsSealed : Boolean read GetIsSealed write SetIsSealed;
         property IsStatic : Boolean read GetIsStatic write SetIsStatic;
         property IsExternal : Boolean read GetIsExternal write SetIsExternal;
         property IsPartial : Boolean read GetIsPartial;
         property IsAttribute : Boolean read GetIsAttribute write SetIsAttribute;

         property OnObjectDestroy : TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
   end;

   // class or type helper
   THelperSymbol = class sealed (TCompositeTypeSymbol)
      private
         FForType : TTypeSymbol;
         FUnAliasedForType : TTypeSymbol;
         FMetaForType : TTypeSymbol;
         FPriority : Integer;

      protected

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol;
                            aForType : TTypeSymbol; priority : Integer);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function IsType : Boolean; override;
         function AllowDefaultProperty : Boolean; override;
         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;
         function CreateAnonymousMethod(aFuncKind : TFuncKind; aVisibility : TdwsVisibility;
                                        isClassMethod : Boolean) : TMethodSymbol; override;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         function HelpsType(typ : TTypeSymbol) : Boolean;

         property ForType : TTypeSymbol read FForType;
         property Priority : Integer read FPriority;
   end;

   THelperSymbols = class(TSimpleList<THelperSymbol>)
      public
         function AddHelper(helper : THelperSymbol) : Boolean;
   end;

   // nil "class"
   TNilSymbol = class sealed (TTypeSymbol)
      protected
         function GetCaption : UnicodeString; override;

      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   TEnumerationSymbol = class;

   // Element of an enumeration type. E. g. "type DummyEnum = (Elem1, Elem2, Elem3);"
   TElementSymbol = class sealed (TConstSymbol)
      private
         FEnumeration : TEnumerationSymbol;
         FIsUserDef : Boolean;

      protected
         function GetDescription : UnicodeString; override;
         function GetValue : Int64; inline;

      public
         constructor Create(const Name: UnicodeString; Typ: TTypeSymbol;
                            const aValue : Int64; isUserDef: Boolean);

         function StandardName : UnicodeString; inline;
         function QualifiedName : UnicodeString; override;

         property Enumeration : TEnumerationSymbol read FEnumeration;
         property IsUserDef : Boolean read FIsUserDef;
         property Value : Int64 read GetValue;
   end;

   TEnumerationSymbolStyle = (enumClassic, enumScoped, enumFlags);

   // Enumeration type. E. g. "type myEnum = (One, Two, Three);"
   TEnumerationSymbol = class sealed (TTypeSymbol)
      private
         FElements : TSymbolTable;
         FLowBound, FHighBound : Int64;
         FStyle : TEnumerationSymbolStyle;
         FContinuous : Boolean;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; baseType : TTypeSymbol;
                            aStyle : TEnumerationSymbolStyle);
         destructor Destroy; override;

         function DefaultValue : Int64;
         procedure InitData(const data : TData; offset : Integer); override;
         function BaseType : TTypeSymbol; override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         procedure AddElement(element : TElementSymbol);
         function ElementByValue(const value : Int64) : TElementSymbol;

         property Elements : TSymbolTable read FElements;
         property Style : TEnumerationSymbolStyle read FStyle;
         property Continuous : Boolean read FContinuous write FContinuous;
         property LowBound : Int64 read FLowBound write FLowBound;
         property HighBound : Int64 read FHighBound write FHighBound;
         function ShortDescription : UnicodeString;
   end;

   // variable with functions for read/write: var x: integer; extern 'type' in 'selector';
   TExternalVarSymbol = class sealed (TValueSymbol)
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

   // TdwsExecution
   //
   TdwsExecution = class abstract (TInterfacedSelfObject, IdwsExecution)
      protected
         FStack : TStackMixIn;
         FStatus : TExecutionStatusResult;
         FCallStack : TTightStack; // expr + prog duples
         FSelfScriptObject : PIScriptObj;
         FSelfScriptClassSymbol : TClassSymbol;

         FDebugger : IDebugger;
         FIsDebugging : Boolean;

      protected
         FProgramState : TProgramState;  // here to reduce its offset

      private
         FExternalObject : TObject;
         FUserObject : TObject;

         FExceptionObjectStack : TSimpleStack<Variant>;
         FLastScriptError : TExprBase;
         FLastScriptCallStack : TdwsExprLocationArray;

         FRandSeed : UInt64;

      protected
         function  GetDebugger : IDebugger;
         procedure SetDebugger(const aDebugger : IDebugger);
         procedure StartDebug;
         procedure StopDebug;

         function GetMsgs : TdwsRuntimeMessageList; virtual; abstract;

         function GetExecutionObject : TdwsExecution;

         function GetUserObject : TObject; virtual;
         procedure SetUserObject(const value : TObject); virtual;
         procedure SetRandSeed(const val : UInt64);

         function GetStack : TStack;

         function GetProgramState : TProgramState;

      public
         constructor Create(const stackParams : TStackParameters);
         destructor Destroy; override;

         procedure DoStep(expr : TExprBase);

         property Status : TExecutionStatusResult read FStatus write FStatus;
         property Stack : TStackMixIn read FStack;
         property SelfScriptObject : PIScriptObj read FSelfScriptObject write FSelfScriptObject;
         property SelfScriptClassSymbol : TClassSymbol read FSelfScriptClassSymbol write FSelfScriptClassSymbol;

         class function Status_Offset : Integer;

         procedure SetScriptError(expr : TExprBase);
         procedure ClearScriptError;

         function GetCallStack : TdwsExprLocationArray; virtual; abstract;
         function CallStackLastExpr : TExprBase; virtual; abstract;
         function CallStackLastProg : TObject; virtual; abstract;
         function CallStackDepth : Integer; virtual; abstract;

         procedure DataContext_Create(const data : TData; addr : Integer; var Result : IDataContext); inline;
         procedure DataContext_CreateBase(addr : Integer; var Result : IDataContext); inline;
         procedure DataContext_CreateLevel(level, addr : Integer; var Result : IDataContext); inline;
         function DataContext_Nil : IDataContext; inline;

         procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString); virtual;
         procedure LocalizeString(const aString : UnicodeString; var Result : UnicodeString); virtual;

         function ValidateFileName(const path : String) : String; virtual;

         function Random : Double;

         property LastScriptError : TExprBase read FLastScriptError;
         property LastScriptCallStack : TdwsExprLocationArray read FLastScriptCallStack;
         property ExceptionObjectStack : TSimpleStack<Variant> read FExceptionObjectStack;

         procedure EnterExceptionBlock(var exceptObj : IScriptObj); virtual;
         procedure LeaveExceptionBlock;

         property ProgramState : TProgramState read FProgramState;

         property Debugger : IDebugger read FDebugger write SetDebugger;
         property IsDebugging : Boolean read FIsDebugging;

         procedure DebuggerNotifyException(const exceptObj : IScriptObj); virtual; abstract;

         property Msgs : TdwsRuntimeMessageList read GetMsgs;

         // per-execution randseed
         property RandSeed : UInt64 read FRandSeed write SetRandSeed;

         // specifies an external object for IInfo constructors, temporary
         property ExternalObject : TObject read FExternalObject write FExternalObject;

         // user object, to attach to an execution
         property UserObject : TObject read GetUserObject write SetUserObject;
   end;

   // IScriptObj
   //
   IScriptObj = interface (IDataContext)
      ['{8D534D1E-4C6B-11D5-8DCB-0000216D9E86}']
      function GetClassSym: TClassSymbol;
      function GetExternalObject: TObject;
      procedure SetExternalObject(value: TObject);
      function GetDestroyed : Boolean;
      procedure SetDestroyed(const val : Boolean);

      property ClassSym : TClassSymbol read GetClassSym;
      property ExternalObject : TObject read GetExternalObject write SetExternalObject;
      property Destroyed : Boolean read GetDestroyed write SetDestroyed;
   end;

   // IScriptObjInterface
   //
   IScriptObjInterface = interface (IDataContext)
      ['{86B77C28-C396-4D53-812B-8FF1867A6128}']
      function GetScriptObj : IScriptObj;
   end;

   // IScriptDynArray
   //
   IScriptDynArray = interface (IDataContext)
      ['{29767B6E-05C0-40E1-A41A-94DF54142312}']
      function GetElementSize : Integer;
      property ElementSize : Integer read GetElementSize;

      function GetArrayLength : Integer;
      procedure SetArrayLength(n : Integer);
      property ArrayLength : Integer read GetArrayLength write SetArrayLength;

      function ToStringArray : TStringDynArray;

      procedure ReplaceData(const v : TData);
   end;

   TPerfectMatchEnumerator = class
      FuncSym, Match : TFuncSymbol;
      function Callback(sym : TSymbol) : Boolean;
   end;

   // The script has to be stopped because of an error
   EScriptError = class(Exception)
      private
         FScriptPos : TScriptPos;
         FScriptCallStack : TdwsExprLocationArray;
         FRawClassName : UnicodeString;

         procedure SetScriptPos(const aPos : TScriptPos);

      public
         constructor CreatePosFmt(const aScriptPos: TScriptPos; const Msg: UnicodeString; const Args: array of const);

         property ScriptPos : TScriptPos read FScriptPos write SetScriptPos;//FScriptPos;
         property ScriptCallStack : TdwsExprLocationArray read FScriptCallStack write FScriptCallStack;
         property RawClassName : UnicodeString read FRawClassName write FRawClassName;
   end;

   EScriptStopped = class (EScriptError)
      public
         class procedure DoRaise(exec : TdwsExecution; stoppedOn : TExprBase); static;
   end;

const
   cFuncKindToString : array [Low(TFuncKind)..High(TFuncKind)] of UnicodeString = (
      'function', 'procedure', 'constructor', 'destructor', 'method', 'lambda' );
   cFirstFieldUnprepared : TFieldSymbol = Pointer(-1);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDefaultRandSeed : UInt64 = 88172645463325252;

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
function TdwsExprLocation.SourceName : UnicodeString;
begin
   Result:=Expr.ScriptPos.SourceName;
end;

// Location
//
function TdwsExprLocation.Location : UnicodeString;
begin
   Result:=Expr.ScriptLocation(Prog);
end;

// ------------------
// ------------------ TExprBase ------------------
// ------------------

// CallStackToString
//
class function TExprBase.CallStackToString(const callStack : TdwsExprLocationArray) : UnicodeString;
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
function TExprBase.RecursiveEnumerateSubExprs(const callback : TExprBaseEnumeratorProc) : Boolean;
var
   i : Integer;
   abort : Boolean;
   base, expr : TExprBase;
   stack : TSimpleStack<TExprBase>;
begin
   if Self=nil then Exit(False);
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
               if abort then Exit(True);
            end;
         end;
      until stack.Count=0;
   finally
      stack.Free;
   end;
   Result:=False;
end;

// ReferencesVariable
//
function TExprBase.ReferencesVariable(varSymbol : TDataSymbol) : Boolean;
var
   i : Integer;
   sub : TExprBase;
begin
   for i:=0 to SubExprCount-1 do begin
      sub:=SubExpr[i];
      if (sub<>nil) and sub.ReferencesVariable(varSymbol) then
         Exit(True)
   end;
   Result:=False;
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

procedure TExprBase.EvalNoResult(exec : TdwsExecution);
begin
   Eval(exec);
end;

// GetIsConstant
//
function TExprBase.GetIsConstant : Boolean;
begin
   Result:=False;
end;

// IsConstant
//
function TExprBase.IsConstant : Boolean;
begin
   Result:=(Self<>nil) and GetIsConstant;
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec : TdwsExecution; e : EScriptError);
begin
   e.ScriptPos:=ScriptPos;
   e.ScriptCallStack:=exec.GetCallStack;
   raise e;
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec : TdwsExecution);
var
   exc : Exception;
   e : EScriptError;
begin
   Assert(ExceptObject is Exception);
   exc:=Exception(ExceptObject);
   e:=EScriptError.Create(exc.Message);
   e.RawClassName:=exc.ClassName;
   RaiseScriptError(exec, e);
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec : TdwsExecution; const msg : UnicodeString);
begin
   RaiseScriptError(exec, EScriptError, msg);
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass; const msg : UnicodeString);
begin
   RaiseScriptError(exec, exceptClass.Create(msg));
end;

// RaiseScriptError
//
procedure TExprBase.RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass;
                                        const msg : UnicodeString; const args : array of const);
begin
   RaiseScriptError(exec, exceptClass.CreateFmt(msg, args));
end;

// CheckScriptObject
//
procedure TExprBase.CheckScriptObject(exec : TdwsExecution; const scriptObj : IScriptObj);
begin
   if scriptObj=nil then
      RaiseObjectNotInstantiated(exec)
   else if scriptObj.Destroyed then
      RaiseObjectAlreadyDestroyed(exec);
end;

// RaiseObjectNotInstantiated
//
procedure TExprBase.RaiseObjectNotInstantiated(exec : TdwsExecution);
begin
   RaiseScriptError(exec, EScriptError, RTE_ObjectNotInstantiated);
end;

// RaiseObjectAlreadyDestroyed
//
procedure TExprBase.RaiseObjectAlreadyDestroyed(exec : TdwsExecution);
begin
   RaiseScriptError(exec, EScriptError, RTE_ObjectAlreadyDestroyed);
end;

// FuncSymQualifiedName
//
function TExprBase.FuncSymQualifiedName : UnicodeString;
begin
   Result:='';
end;

// ------------------
// ------------------ TSymbol ------------------
// ------------------

// Create
//
constructor TSymbol.Create(const aName : UnicodeString; aType : TTypeSymbol);
begin
   FName:=aName;
   FTyp:=aType;
   if Assigned(aType) then
      FSize:=aType.FSize
   else FSize:=0;
end;

// GetCaption
//
function TSymbol.GetCaption : UnicodeString;
begin
   Result:=FName;
end;

// GetDescription
//
function TSymbol.GetDescription : UnicodeString;
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

// IsType
//
function TSymbol.IsType : Boolean;
begin
   Result:=False;
end;

// IsPointerType
//
function TSymbol.IsPointerType : Boolean;
begin
   Result:=False;
end;

// GetAsFuncSymbol
//
function TSymbol.GetAsFuncSymbol : TFuncSymbol;
begin
   Result:=nil;
end;

// AsFuncSymbol
//
function TSymbol.AsFuncSymbol : TFuncSymbol;
begin
   if Self<>nil then
      Result:=GetAsFuncSymbol
   else Result:=nil;
end;

// AsFuncSymbol
//
function TSymbol.AsFuncSymbol(var funcSym : TFuncSymbol) : Boolean;
begin
   if Self<>nil then
      funcSym:=GetAsFuncSymbol
   else funcSym:=nil;
   Result:=(funcSym<>nil);
end;

// QualifiedName
//
function TSymbol.QualifiedName : UnicodeString;
begin
   Result:=Name;
end;

// IsVisibleFor
//
function TSymbol.IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean;
begin
   Result:=True;
end;

function TSymbol.BaseType: TTypeSymbol;
begin
  Result := nil;
end;

// SetName
//
procedure TSymbol.SetName(const newName : UnicodeString);
begin
   Assert(FName='');
   FName:=newName;
end;

// SafeGetCaption
//
function TSymbol.SafeGetCaption : UnicodeString;
begin
   if Self=nil then
      Result:=SYS_VOID
   else Result:=GetCaption;
end;

// ------------------
// ------------------ TCompositeTypeSymbol ------------------
// ------------------

// Create
//
constructor TCompositeTypeSymbol.Create(const name : UnicodeString; aUnit : TSymbol);
begin
   inherited Create(name, nil);
   FUnitSymbol:=aUnit;
   FMembers:=CreateMembersTable;
   FFirstField:=cFirstFieldUnprepared;
end;

// Destroy
//
destructor TCompositeTypeSymbol.Destroy;
begin
   FMembers.Free;
   inherited;
end;

// AddConst
//
procedure TCompositeTypeSymbol.AddConst(sym : TClassConstSymbol);
begin
   sym.OwnerSymbol:=Self;
   FMembers.AddSymbol(sym);
end;

// AddConst
//
procedure TCompositeTypeSymbol.AddConst(sym : TClassConstSymbol; visibility : TdwsVisibility);
begin
   sym.Visibility:=visibility;
   AddConst(sym);
end;

// AddClassVar
//
procedure TCompositeTypeSymbol.AddClassVar(sym : TClassVarSymbol);
begin
   sym.OwnerSymbol:=Self;
   FMembers.AddSymbol(sym);
end;

// AddProperty
//
procedure TCompositeTypeSymbol.AddProperty(propSym : TPropertySymbol);
begin
   FMembers.AddSymbol(propSym);
   propSym.FOwnerSymbol:=Self;
end;

// AddMethod
//
procedure TCompositeTypeSymbol.AddMethod(methSym : TMethodSymbol);
begin
   FMembers.AddSymbol(methSym);
   methSym.FStructSymbol:=Self;
end;

// AddField
//
procedure TCompositeTypeSymbol.AddField(fieldSym : TFieldSymbol);
begin
   Assert(FFirstField=cFirstFieldUnprepared);
   FMembers.AddSymbol(fieldSym);
   fieldSym.FStructSymbol:=Self;
end;

// FieldAtOffset
//
function TCompositeTypeSymbol.FieldAtOffset(offset : Integer) : TFieldSymbol;
var
   sym : TSymbol;
begin
   for sym in Members do begin
      if sym.ClassType=TFieldSymbol then begin
         Result:=TFieldSymbol(sym);
         if Result.Offset=offset then Exit;
      end;
   end;
   Result:=nil;
end;

// AllowVirtualMembers
//
function TCompositeTypeSymbol.AllowVirtualMembers : Boolean;
begin
   Result:=False;
end;

// AllowOverloads
//
function TCompositeTypeSymbol.AllowOverloads : Boolean;
begin
   Result:=True;
end;

// CreateMembersTable
//
function TCompositeTypeSymbol.CreateMembersTable : TMembersSymbolTable;
begin
   Result:=TMembersSymbolTable.Create(nil);
   Result.Owner:=Self;
end;

// GetIsStatic
//
function TCompositeTypeSymbol.GetIsStatic : Boolean;
begin
   Result:=False;
end;

// GetIsExternal
//
function TCompositeTypeSymbol.GetIsExternal : Boolean;
begin
   Result:=False;
end;

// GetIsExternalRooted
//
function TCompositeTypeSymbol.GetIsExternalRooted : Boolean;
begin
   Result:=IsExternal;
end;

// GetExternalName
//
function TCompositeTypeSymbol.GetExternalName : UnicodeString;
begin
   Result:=Name;
end;

// GetIsPartial
//
function TCompositeTypeSymbol.GetIsPartial : Boolean;
begin
   Result:=False;
end;

// GetIsImmutable
//
function TCompositeTypeSymbol.GetIsImmutable : Boolean;
begin
   Result:=False;
end;

// FindDefaultConstructor
//
function TCompositeTypeSymbol.FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol;
begin
   Result:=nil;
end;

// MembersVisibilities
//
function TCompositeTypeSymbol.MembersVisibilities : TdwsVisibilities;
begin
   Result:=Members.Visibilities;
   if Parent<>nil then
      Result:=Result+Parent.MembersVisibilities;
end;

// CheckMethodsImplemented
//
procedure TCompositeTypeSymbol.CheckMethodsImplemented(const msgs : TdwsCompileMessageList);
var
   i, k : Integer;
   methSym : TMethodSymbol;
   msg : TScriptMessage;
   afa : TdwsAFAAddImplementation;
   buf : UnicodeString;
begin
   for i:=0 to FMembers.Count-1 do begin
      if FMembers[i] is TMethodSymbol then begin
         methSym:=TMethodSymbol(FMembers[i]);
         if methSym.ClassType=TAliasMethodSymbol then continue;
         if not methSym.IsAbstract then begin
            if Assigned(methSym.FExecutable) then
               methSym.FExecutable.InitSymbol(FMembers[i], msgs)
            else if not methSym.IsExternal then begin
               msg:=msgs.AddCompilerErrorFmt((methSym as TSourceMethodSymbol).DeclarationPos, CPE_MethodNotImplemented,
                                             [methSym.Name, methSym.StructSymbol.Caption]);
               afa:=TdwsAFAAddImplementation.Create(msg, AFA_AddImplementation);
               buf:=methSym.GetDescription;
               FastStringReplace(buf, '()', ' ');
               afa.Text:= #13#10
                         +TrimRight(buf)
                         +';'#13#10'begin'#13#10#9'|'#13#10'end;'#13#10;
               k:=Pos(methSym.Name, afa.Text);
               afa.Text:=Copy(afa.Text, 1, k-1)+methSym.StructSymbol.Name+'.'
                        +Copy(afa.Text, k, MaxInt);
            end;
         end;
      end;
   end;
end;

// ExternalRoot
//
function TCompositeTypeSymbol.ExternalRoot : TCompositeTypeSymbol;
begin
   Result:=Self;
   while (Result<>nil) and not Result.IsExternal do
      Result:=Result.Parent;
end;

// AllowFields
//
function TCompositeTypeSymbol.AllowFields : Boolean;
begin
   Result:=False;
end;

// AllowAnonymousMethods
//
function TCompositeTypeSymbol.AllowAnonymousMethods : Boolean;
begin
   Result:=True;
end;

// PrepareFirstField
//
function TCompositeTypeSymbol.PrepareFirstField : TFieldSymbol;
var
   member : TSymbol;
begin
   if Parent<>nil then
      Result:=Parent.FirstField
   else Result:=nil;
   for member in Members do begin
      if member is TFieldSymbol then begin
         TFieldSymbol(member).NextField:=Result;
         Result:=TFieldSymbol(member);
      end;
   end;
   FFirstField:=Result;
end;

// FirstField
//
function TCompositeTypeSymbol.FirstField : TFieldSymbol;
begin
   if FFirstField=cFirstFieldUnprepared then
      PrepareFirstField;
   Result:=FFirstField;
end;

// ------------------
// ------------------ TStructuredTypeSymbol ------------------
// ------------------

// Destroy
//
destructor TStructuredTypeSymbol.Destroy;
begin
   if FForwardPosition<>nil then
      Dispose(FForwardPosition);
   FMetaSymbol.Free;
   inherited;
end;

// DoInheritFrom
//
procedure TStructuredTypeSymbol.DoInheritFrom(ancestor : TStructuredTypeSymbol);
begin
   Assert(FParent=nil);
   Assert(FMembers.Count=0);

   FMembers.AddParent(ancestor.Members);
   FParent:=ancestor;
end;

// NthParentOf
//
function TStructuredTypeSymbol.NthParentOf(structType : TCompositeTypeSymbol) : Integer;
begin
   Result:=0;
   while structType<>nil do begin
      if structType=Self then
         Exit
      else begin
         structType:=structType.Parent;
         Inc(Result);
      end;
   end;
   Result:=-1;
end;

// DistanceTo
//
function TStructuredTypeSymbol.DistanceTo(typeSym : TTypeSymbol) : Integer;
begin
   if typeSym=Self then
      Result:=0
   else if typeSym is TStructuredTypeSymbol then
      Result:=TStructuredTypeSymbol(typeSym).NthParentOf(Self)
   else Result:=MaxInt;
end;

// GetIsForwarded
//
function TStructuredTypeSymbol.GetIsForwarded : Boolean;
begin
   Result:=Assigned(FForwardPosition);
end;

// DuckTypedMatchingMethod
//
function TStructuredTypeSymbol.DuckTypedMatchingMethod(methSym : TMethodSymbol; visibility : TdwsVisibility) : TMethodSymbol;
var
   sym : TSymbol;
   meth : TMethodSymbol;
begin
   for sym in Members do begin
      if sym is TMethodSymbol then begin
         meth:=TMethodSymbol(sym);
         if     (meth.Visibility>=visibility)
            and UnicodeSameText(meth.Name, methSym.Name)
            and meth.IsCompatible(methSym) then
               Exit(meth);
      end;
   end;
   if Parent<>nil then
      Result:=(Parent as TStructuredTypeSymbol).DuckTypedMatchingMethod(methSym, cvPublic)
   else Result:=nil;
end;

// FindDefaultConstructor
//
function TStructuredTypeSymbol.FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol;
begin
   Result:=nil;
end;

// AllowDefaultProperty
//
function TStructuredTypeSymbol.AllowDefaultProperty : Boolean;
begin
   Result:=True;
end;

// SetForwardedPos
//
procedure TStructuredTypeSymbol.SetForwardedPos(const aScriptPos: TScriptPos);
begin
   if FForwardPosition=nil then
      New(FForwardPosition);
   FForwardPosition^:=aScriptPos;
end;

// ClearIsForwarded
//
procedure TStructuredTypeSymbol.ClearIsForwarded;
begin
   Dispose(FForwardPosition);
   FForwardPosition:=nil;
end;

// GetIsExternal
//
function TStructuredTypeSymbol.GetIsExternal : Boolean;
begin
   Result:=False;
end;

// GetExternalName
//
function TStructuredTypeSymbol.GetExternalName : UnicodeString;
begin
   if FExternalName='' then
      Result:=Name
   else Result:=FExternalName;
end;

// ------------------
// ------------------ TStructuredTypeMetaSymbol ------------------
// ------------------

// Create
//
constructor TStructuredTypeMetaSymbol.Create(const name : UnicodeString; typ : TStructuredTypeSymbol);
begin
   inherited Create(name, typ);
   FSize:=1;
end;

// InitData
//
procedure TStructuredTypeMetaSymbol.InitData(const Data: TData; Offset: Integer);
begin
   Data[Offset] := Int64(0);
end;

// IsCompatible
//
function TStructuredTypeMetaSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym is TStructuredTypeMetaSymbol) and (Typ.BaseType=typSym.Typ.BaseType);
end;

// StructSymbol
//
function TStructuredTypeMetaSymbol.StructSymbol : TStructuredTypeSymbol;
begin
   Result:=TStructuredTypeSymbol(Typ);
end;

// Parent
//
function TStructuredTypeMetaSymbol.Parent : TStructuredTypeMetaSymbol;
var
   p : TCompositeTypeSymbol;
begin
   p:=StructSymbol.Parent;
   if (p=nil) or not (p is TStructuredTypeSymbol) then
      Result:=nil
   else Result:=TStructuredTypeSymbol(p).MetaSymbol;
end;

// ------------------
// ------------------ TRecordSymbol ------------------
// ------------------

// Create
//
constructor TRecordSymbol.Create(const name : UnicodeString; aUnit : TSymbol);
begin
   inherited Create(name, aUnit);
   FMetaSymbol:=TStructuredTypeMetaSymbol.Create('meta of '+name, Self);
end;

// AddField
//
procedure TRecordSymbol.AddField(fieldSym : TFieldSymbol);
begin
   inherited;
   fieldSym.FOffset:=FSize;
   FSize:=FSize+fieldSym.Typ.Size;
   if fieldSym.DefaultExpr<>nil then
      IsDynamic:=True;
end;

// AddMethod
//
procedure TRecordSymbol.AddMethod(methSym : TMethodSymbol);
begin
   inherited;
   if methSym.IsClassMethod then
      methSym.SetIsStatic;
end;

// Initialize
//
procedure TRecordSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   CheckMethodsImplemented(msgs);
end;

// AllowFields
//
function TRecordSymbol.AllowFields : Boolean;
begin
   Result:=True;
end;

// CreateSelfParameter
//
function TRecordSymbol.CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol;
begin
   if methSym.IsClassMethod then
      Result:=nil
   else begin
      Result:=TVarParamSymbol.Create(SYS_SELF, Self);
      methSym.Params.AddSymbol(Result);
   end;
end;

// CreateAnonymousMethod
//
function TRecordSymbol.CreateAnonymousMethod(
      aFuncKind : TFuncKind; aVisibility : TdwsVisibility; isClassMethod : Boolean) : TMethodSymbol;
begin
   Result:=TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility, isClassMethod);
   if isClassMethod then
      TSourceMethodSymbol(Result).SetIsStatic;
end;

// InitData
//
procedure TRecordSymbol.InitData(const data : TData; offset : Integer);
var
   field : TFieldSymbol;
begin
   field:=FirstField;
   while field<>nil do begin
      field.InitData(data, offset);
      field:=field.NextField;
   end;
end;

// IsCompatible
//
function TRecordSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   typSym:=typSym.UnAliasedType.BaseType;
   if not (typSym is TRecordSymbol) then
      Exit(False);
   if Self=typSym then
      Exit(True);

   Result:=False;
end;

// GetCaption
//
function TRecordSymbol.GetCaption : UnicodeString;
begin
   Result:='record '+Name;
end;

// GetDescription
//
function TRecordSymbol.GetDescription : UnicodeString;
var
   member : TSymbol;
begin
   if Name='' then
      Result := 'anonymous record'#13#10
   else Result:=Name+' = record'#13#10;
   for member in FMembers do begin
      if member is TFieldSymbol then
         Result:=Result+'   '+member.Name+' : '+member.Typ.Name+';'#13#10;
   end;
   Result:=Result+'end;';
end;

// GetIsDynamic
//
function TRecordSymbol.GetIsDynamic : Boolean;
begin
   Result:=(rsfDynamic in FFlags);
end;

// SetIsDynamic
//
procedure TRecordSymbol.SetIsDynamic(const val : Boolean);
begin
   if val then
      Include(FFlags, rsfDynamic)
   else Exclude(FFlags, rsfDynamic);
end;

// GetIsImmutable
//
function TRecordSymbol.GetIsImmutable : Boolean;
begin
   Result:=(rsfImmutable in FFlags);
end;

// SetIsImmutable
//
procedure TRecordSymbol.SetIsImmutable(const val : Boolean);
begin
   if val then
      Include(FFlags, rsfImmutable)
   else Exclude(FFlags, rsfImmutable);
end;

// GetIsFullyDefined
//
function TRecordSymbol.GetIsFullyDefined : Boolean;
begin
   Result:=(rsfFullyDefined in FFlags);
end;

// SetIsFullyDefined
//
procedure TRecordSymbol.SetIsFullyDefined(const val : Boolean);
begin
   if val then
      Include(FFlags, rsfFullyDefined)
   else Exclude(FFlags, rsfFullyDefined);
end;

// ------------------
// ------------------ TInterfaceSymbol ------------------
// ------------------

// Create
//
constructor TInterfaceSymbol.Create(const name : UnicodeString; aUnit : TSymbol);
begin
   inherited;
   FSize:=1;
end;

// GetCaption
//
function TInterfaceSymbol.GetCaption : UnicodeString;
begin
   Result:=Name;
end;

// GetDescription
//
function TInterfaceSymbol.GetDescription : UnicodeString;
begin
   Result:=Name+' = interface';
   if Parent<>nil then
      Result:=Result+'('+Parent.Name+')';
end;

// InheritFrom
//
procedure TInterfaceSymbol.InheritFrom(ancestor : TInterfaceSymbol);
begin
   DoInheritFrom(ancestor);
   FMethodCount:=ancestor.MethodCount;
end;

// AddMethod
//
procedure TInterfaceSymbol.AddMethod(methSym : TMethodSymbol);
begin
   inherited;
   if methSym.Name<>'' then begin
      methSym.FVMTIndex:=FMethodCount;
      Inc(FMethodCount);
   end;
end;

// InitData
//
procedure TInterfaceSymbol.InitData(const Data: TData; Offset: Integer);
const
   cNilIntf : IUnknown = nil;
begin
   Data[Offset]:=cNilIntf;
end;

// Initialize
//
procedure TInterfaceSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   // Check validity of the interface declaration
   if IsForwarded then
      msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_InterfaceNotCompletelyDefined, [Name]);
end;

// IsCompatible
//
function TInterfaceSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   typSym:=typSym.UnAliasedType;
   if typSym is TNilSymbol then
      Result:=True
   else if typSym is TInterfaceSymbol then
      Result:=(NthParentOf(TInterfaceSymbol(typSym))>=0)
   else Result:=False;
end;

// IsPointerType
//
function TInterfaceSymbol.IsPointerType : Boolean;
begin
   Result:=True;
end;

// CreateSelfParameter
//
function TInterfaceSymbol.CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol;
begin
   Assert(not methSym.IsClassMethod);
   Result:=TSelfSymbol.Create(SYS_SELF, Self);
   methSym.InternalParams.AddSymbol(Result);
end;

// CreateAnonymousMethod
//
function TInterfaceSymbol.CreateAnonymousMethod(
      aFuncKind : TFuncKind; aVisibility : TdwsVisibility; isClassMethod : Boolean) : TMethodSymbol;
begin
   Result:=TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility, isClassMethod);
end;

// DoIsOfType
//
function TInterfaceSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   typSym:=typSym.UnAliasedType;
   if typSym is TInterfaceSymbol then
      Result:=(NthParentOf(TInterfaceSymbol(typSym))>=0)
   else Result:=False;
end;

// Parent
//
function TInterfaceSymbol.Parent : TInterfaceSymbol;
begin
   Result:=TInterfaceSymbol(FParent);
end;

// ------------------
// ------------------ TFieldSymbol ------------------
// ------------------

// Create
//
constructor TFieldSymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol; aVisibility : TdwsVisibility);
begin
   inherited Create(Name, Typ);
   FVisibility:=aVisibility;
end;

// Destroy
//
destructor TFieldSymbol.Destroy;
begin
   FDefaultExpr.Free;
   inherited;
end;

// QualifiedName
//
function TFieldSymbol.QualifiedName : UnicodeString;
begin
   Result:=StructSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TFieldSymbol.IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// HasExternalName
//
function TFieldSymbol.HasExternalName : Boolean;
begin
   Result:=(FExternalName<>'');
end;

// InitData
//
procedure TFieldSymbol.InitData(const data : TData; structOffset : Integer);
begin
   if DefaultValue<>nil then
      DWSCopyData(DefaultValue, 0, data, structOffset+Offset, Typ.Size)
   else Typ.InitData(data, structOffset+Offset);
end;

// GetExternalName
//
function TFieldSymbol.GetExternalName : UnicodeString;
begin
   if FExternalName='' then
      Result:=Name
   else Result:=FExternalName;
end;

// ------------------
// ------------------ TClassConstSymbol ------------------
// ------------------

// QualifiedName
//
function TClassConstSymbol.QualifiedName : UnicodeString;
begin
   Result:=OwnerSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TClassConstSymbol.IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// ------------------
// ------------------ TClassVarSymbol ------------------
// ------------------

// QualifiedName
//
function TClassVarSymbol.QualifiedName : UnicodeString;
begin
   Result:=OwnerSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TClassVarSymbol.IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// ------------------
// ------------------ TFuncSymbol ------------------
// ------------------

// Create
//
constructor TFuncSymbol.Create(const name : UnicodeString; funcKind : TFuncKind;
                               funcLevel : SmallInt);
begin
   inherited Create(name, nil);
   FKind:=funcKind;
   FAddrGenerator:=TAddrGeneratorRec.CreateNegative(funcLevel);
   FInternalParams:=TUnSortedSymbolTable.Create(nil, @FAddrGenerator);
   FParams:= TParamsSymbolTable.Create(FInternalParams, @FAddrGenerator);
   FSize:=1;
end;

// Destroy
//
destructor TFuncSymbol.Destroy;
begin
   if FForwardPosition<>nil then
      Dispose(FForwardPosition);
   FParams.Free;
   FInternalParams.Free;
   FConditions.Free;
   inherited;
end;

// Generate
//
constructor TFuncSymbol.Generate(table : TSymbolTable; const funcName : UnicodeString;
                                 const funcParams : TParamArray; const funcType : UnicodeString);
var
   typSym : TTypeSymbol;
begin
   if funcType<>'' then begin
      Self.Create(funcName, fkFunction, 1);
      // Set function type
      typSym:=table.FindTypeSymbol(funcType, cvMagic);
      if (typSym=nil) or (typSym.BaseType=nil) then
         raise Exception.CreateFmt(CPE_TypeIsUnknown, [funcType]);
      Self.SetType(typSym);
   end else begin
      Self.Create(funcName, fkProcedure, 1);
   end;

   GenerateParams(table, funcParams);
end;

// AddParam
//
procedure TFuncSymbol.AddParam(param : TParamSymbol);
begin
   Params.AddSymbol(param);
end;

// AddParams
//
procedure TFuncSymbol.AddParams(params : TParamsSymbolTable);
var
   i : Integer;
begin
   for i:=0 to params.Count-1 do
      AddParam(params[i].Clone);
end;

// HasParam
//
function TFuncSymbol.HasParam(param : TParamSymbol) : Boolean;
begin
   Result:=(Params.FindLocal(param.Name)<>nil);
end;

// SetType
//
procedure TFuncSymbol.SetType(const value : TTypeSymbol);
begin
   FTyp:=Value;
   Assert(FResult=nil);
   if FTyp<>nil then begin
      FResult:=TResultSymbol.Create(SYS_RESULT, Value);
      FInternalParams.AddSymbol(FResult);
   end;
end;

// GenerateParams
//
procedure GenerateParams(table : TSymbolTable; const funcParams : TParamArray;
                         const addProc : TAddParamSymbolMethod);
var
   i : Integer;
   typSym : TTypeSymbol;
   paramSym : TParamSymbol;
   paramSymWithDefault : TParamSymbolWithDefaultValue;
   paramRec : PParamRec;
begin
   typSym:=nil;

   for i:=0 to High(FuncParams) do begin

      paramRec:=@FuncParams[i];
      if (typSym=nil) or not UnicodeSameText(typSym.Name, paramRec.ParamType) then
         typSym:=Table.FindTypeSymbol(paramRec.ParamType, cvMagic);
      if not Assigned(typSym) then
         raise Exception.CreateFmt(CPE_TypeForParamNotFound,
                                   [paramRec.ParamType, paramRec.ParamName]);

      if paramRec.HasDefaultValue then begin

         if paramRec.IsVarParam then
            raise Exception.Create(CPE_VarParamCantHaveDefaultValue);
         if paramRec.IsConstParam then
            raise Exception.Create(CPE_ConstParamCantHaveDefaultValue);

         paramSymWithDefault:=TParamSymbolWithDefaultValue.Create(paramRec.ParamName, typSym,
                                                                  paramRec.DefaultValue);
         paramSym:=paramSymWithDefault;

      end else begin

         if paramRec.IsVarParam then
            paramSym:=TVarParamSymbol.Create(paramRec.ParamName, typSym)
         else if paramRec.IsConstParam then
            paramSym:=TConstParamSymbol.Create(paramRec.ParamName, typSym)
         else paramSym:=TParamSymbol.Create(paramRec.ParamName, typSym);

      end;

      addProc(paramSym);

   end;
end;

// GenerateParams
//
procedure TFuncSymbol.GenerateParams(table : TSymbolTable; const funcParams : TParamArray);
begin
   dwsSymbols.GenerateParams(table, funcParams, addParam);
end;

// GetParamType
//
function TFuncSymbol.GetParamType(idx : Integer) : TTypeSymbol;
begin
   if Cardinal(idx)<Cardinal(Params.Count) then
      Result:=Params[idx].Typ
   else Result:=nil;
end;

// GetCaption
//
function TFuncSymbol.GetCaption : UnicodeString;
var
   i : Integer;
   nam : UnicodeString;
begin
   nam:=cFuncKindToString[Kind]+' '+Name;

   if Params.Count>0 then begin
      Result:=Params[0].Typ.Caption;
      for i:=1 to Params.Count - 1 do
         Result:=Result+', '+Params[i].Typ.Caption;
      Result:='('+Result+')';
   end else Result:='';

   if Typ<>nil then
      if Typ.Name<>'' then
         Result:=nam+Result+': '+Typ.Name
      else Result:=nam+Result+': '+Typ.Caption
   else Result:=nam+Result;
end;

// GetIsForwarded
//
function TFuncSymbol.GetIsForwarded : Boolean;
begin
   Result:=Assigned(FForwardPosition);
end;

// GetDescription
//
function TFuncSymbol.GetDescription: UnicodeString;
begin
   Result:=cFuncKindToString[Kind]+' '+Name+ParamsDescription;
   if Typ<>nil then
      Result:=Result+': '+Typ.Name;
end;

// Initialize
//
procedure TFuncSymbol.Initialize(const msgs : TdwsCompileMessageList);
var
   msg : TScriptMessage;
   afa : TdwsAFAAddImplementation;
begin
   inherited;
   if IsExternal then Exit;
   FInternalParams.Initialize(msgs);
   if Assigned(FExecutable) then
      FExecutable.InitSymbol(Self, msgs)
   else if Level>=0 then begin
      msg:=msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_ForwardNotImplemented, [Name]);
      afa:=TdwsAFAAddImplementation.Create(msg, AFA_AddImplementation);
      afa.Text:= #13#10
                +TrimRight(StringReplace(GetDescription, '()', ' ', [rfIgnoreCase]))
                +';'#13#10'begin'#13#10#9'|'#13#10'end;'#13#10;
   end;
end;

// GetLevel
//
function TFuncSymbol.GetLevel: SmallInt;
begin
   Result:=FAddrGenerator.Level;
end;

// GetParamSize
//
function TFuncSymbol.GetParamSize : Integer;
begin
   Result:=FAddrGenerator.DataSize;
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

// GetIsExport
//
function TFuncSymbol.GetIsExport : Boolean;
begin
   Result:=(fsfExport in FFlags);
end;

// SetIsExport
//
procedure TFuncSymbol.SetIsExport(const val : Boolean);
begin
   if val then
      Include(FFlags, fsfExport)
   else Exclude(FFlags, fsfExport);
end;

// GetIsProperty
//
function TFuncSymbol.GetIsProperty : Boolean;
begin
   Result:=(fsfProperty in FFlags);
end;

// SetIsProperty
//
procedure TFuncSymbol.SetIsProperty(const val : Boolean);
begin
   if val then
      Include(FFlags, fsfProperty)
   else Exclude(FFlags, fsfProperty);
end;

// GetIsOverloaded
//
function TFuncSymbol.GetIsOverloaded : Boolean;
begin
   Result:=(fsfOverloaded in FFlags);
end;

// SetIsOverloaded
//
procedure TFuncSymbol.SetIsOverloaded(const val : Boolean);
begin
   if val then
      Include(FFlags, fsfOverloaded)
   else Exclude(FFlags, fsfOverloaded);
end;

// GetIsLambda
//
function TFuncSymbol.GetIsLambda : Boolean;
begin
   Result:=(fsfLambda in FFlags);
end;

// SetIsLambda
//
procedure TFuncSymbol.SetIsLambda(const val : Boolean);
begin
   if val then
      Include(FFlags, fsfLambda)
   else Exclude(FFlags, fsfLambda);
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

// GetExternalName
//
function TFuncSymbol.GetExternalName : UnicodeString;
begin
   if FExternalName='' then
      Result:=Name
   else Result:=FExternalName;
end;

// GetSourceSubExpr
//
function TFuncSymbol.GetSourceSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExecutable.SubExpr(i);
end;

// GetSourceSubExprCount
//
function TFuncSymbol.GetSourceSubExprCount : Integer;
begin
   if FExecutable<>nil then
      Result:=FExecutable.SubExprCount
   else Result:=0;
end;

// IsCompatible
//
function TFuncSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
const
   cCompatibleKinds : array [TFuncKind, TFuncKind] of Boolean =
      //  fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod, fkLambda
      ( (     True,      False,        False,         False,      True,     True),      // fkFunction
        (     False,     True,         False,         False,      True,     True),      // fkProcedure
        (     False,     False,        True,          False,      False,    False),     // fkConstructor
        (     False,     False,        False,         True,       False,    False),     // fkDestructor
        (     True,      True,         False,         False,      True,     True),      // fkMethod
        (     True,      True,         False,         False,      True,     True) );    // fkLambda
var
   funcSym : TFuncSymbol;
   i : Integer;
   param, otherParam : TSymbol;
begin
   if typSym=nil then Exit(False);
   typSym:=typSym.BaseType;
   if (typSym.ClassType=TNilSymbol) or (typSym.ClassType=TAnyFuncSymbol) then
      Result:=True
//   else if typSym.IsType and not IsType then
//      Result:=False
   else begin
      Result:=False;
      funcSym:=typSym.AsFuncSymbol;
      if funcSym=nil then
         Exit;
      if Params.Count<>funcSym.Params.Count then Exit;
      if not cCompatibleKinds[Kind, funcSym.Kind] then Exit;
      if    (Typ=funcSym.Typ)
         or (Typ.IsOfType(funcSym.Typ))
         or (funcSym.Typ is TAnyTypeSymbol) then begin
         for i:=0 to Params.Count-1 do begin
            param:=Params[i];
            otherParam:=funcSym.Params[i];
            if param.ClassType<>otherParam.ClassType then Exit;
            if param.Typ<>otherParam.Typ then begin
               if not param.Typ.IsCompatible(otherParam.Typ) then Exit;
               if not otherParam.Typ.IsCompatible(param.Typ) then Exit;
            end;
         end;
         Result:=True;
      end;
   end;
end;

// DoIsOfType
//
function TFuncSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
var
   i : Integer;
   funcSym : TFuncSymbol;
begin
   funcSym:=typSym.AsFuncSymbol;
   if funcSym=nil then
      Exit(False);

   Result:=    (Kind=funcSym.Kind)
           and (Params.Count=funcSym.Params.Count);
   if not Result then Exit;

   if Typ=nil then begin
      if funcSym.Typ<>nil then
         Exit(False)
   end else if not Typ.IsCompatible(funcSym.Typ) then
      Exit(False);

   for i:=0 to Params.Count-1 do begin
      if not Params[i].Typ.DoIsOfType(funcSym.Params[i].Typ) then
         Exit(False);
   end;
   Result:=True;
end;

// IsType
//
function TFuncSymbol.IsType : Boolean;
begin
   Result:=(fsfType in FFlags);
end;

// SetIsType
//
procedure TFuncSymbol.SetIsType;
begin
   Include(FFlags, fsfType);
end;

// GetAsFuncSymbol
//
function TFuncSymbol.GetAsFuncSymbol : TFuncSymbol;
begin
   Result:=Self;
end;

// SetInline
//
procedure TFuncSymbol.SetInline;
begin
   Include(FFlags, fsfInline);
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

// IsValidOverloadOf
//
function TFuncSymbol.IsValidOverloadOf(other : TFuncSymbol) : Boolean;
var
   i : Integer;
   n : Integer;
   locParam, otherParam : TParamSymbol;
begin
   // overload is valid if parameter types differ,
   // and there is no ambiguity with default params

   n:=Min(Params.Count, other.Params.Count);

   // check special case of an overload of a parameter-less function
   if (Params.Count=0) and (other.Params.Count=0) then Exit(False);

   // check parameters positions defined in both
   for i:=0 to n-1 do begin
      locParam:=Params[i];
      otherParam:=other.Params[i];
      if     (locParam.ClassType=TParamSymbolWithDefaultValue)
         and (otherParam.ClassType=TParamSymbolWithDefaultValue) then Exit(False);
      if not locParam.Typ.IsOfType(otherParam.Typ) then Exit(True);
   end;

   // check that there is at least one remaining param that is not with a default
   if Params.Count>n then
      Result:=(Params[n].ClassType<>TParamSymbolWithDefaultValue)
   else if other.Params.Count>n then
      Result:=(other.Params[n].ClassType<>TParamSymbolWithDefaultValue)
   else Result:=False;
end;

// IsSameOverloadOf
//
function TFuncSymbol.IsSameOverloadOf(other : TFuncSymbol) : Boolean;
var
   i : Integer;
begin
   Result:=(Kind=other.Kind) and (Typ=other.Typ) and (Params.Count=other.Params.Count);
   if Result then begin
      for i:=0 to Params.Count-1 do begin
         if not Params[i].SameParam(other.Params[i]) then begin
            Result:=False;
            Break;
         end;
      end;
   end;
end;

// ParamsDescription
//
function TFuncSymbol.ParamsDescription : UnicodeString;
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
procedure TFuncSymbol.SetForwardedPos(const aScriptPos: TScriptPos);
begin
   if FForwardPosition=nil then
      New(FForwardPosition);
   FForwardPosition^:=aScriptPos;
end;

// ClearIsForwarded
//
procedure TFuncSymbol.ClearIsForwarded;
begin
   Dispose(FForwardPosition);
   FForwardPosition:=nil;
end;

// HasExternalName
//
function TFuncSymbol.HasExternalName : Boolean;
begin
   Result:=(FExternalName<>'');
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
// ------------------ TMethodSymbol ------------------
// ------------------

// Create
//
constructor TMethodSymbol.Create(const Name: UnicodeString; FuncKind: TFuncKind;
  aStructSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility; isClassMethod : Boolean;
  funcLevel : Integer);
begin
   inherited Create(Name, FuncKind, funcLevel);
   FStructSymbol := aStructSymbol;
   if isClassMethod then
      Include(FAttributes, maClassMethod);
   FSelfSym:=aStructSymbol.CreateSelfParameter(Self);
   FSize:=1; // wrapped in a interface
   FParams.AddParent(FStructSymbol.Members);
   FVisibility:=aVisibility;
   FVMTIndex:=-1;
   if aStructSymbol.IsExternal then
      IsExternal:=True;
end;

constructor TMethodSymbol.Generate(Table: TSymbolTable; MethKind: TMethodKind;
                              const Attributes: TMethodAttributes; const MethName: UnicodeString;
                              const MethParams: TParamArray; const MethType: UnicodeString;
                              Cls: TCompositeTypeSymbol; aVisibility : TdwsVisibility;
                              overloaded : Boolean);
var
   typSym : TTypeSymbol;
   meth : TSymbol;
   enumerator : TPerfectMatchEnumerator;
begin
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

   // Set Result type
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

   // Check if name is already used
   if overloaded then begin
      enumerator:=TPerfectMatchEnumerator.Create;
      try
         enumerator.FuncSym:=Self;
         Cls.Members.EnumerateSymbolsOfNameInScope(MethName, enumerator.Callback);
         meth:=enumerator.Match;
      finally
         enumerator.Free;
      end;
   end else begin
      meth:=Cls.Members.FindSymbol(MethName, cvPrivate);
   end;
   if meth<>nil then begin
      if meth is TFieldSymbol then
         raise Exception.CreateFmt(CPE_FieldExists, [MethName])
      else if meth is TPropertySymbol then
         raise Exception.CreateFmt(CPE_PropertyExists, [MethName])
      else if meth is TMethodSymbol then begin
         if TMethodSymbol(meth).StructSymbol=Cls then begin
            if not overloaded then
               raise Exception.CreateFmt(CPE_MethodExists, [MethName])
            else if not TMethodSymbol(meth).IsOverloaded then
               raise Exception.CreateFmt(UNT_PreviousNotOverloaded, [MethName])
         end;
      end;
   end;

   if overloaded then
      IsOverloaded:=True;
   if Assigned(meth) then
      SetOverlap(TMethodSymbol(meth));

   if Attributes = [maVirtual] then
      IsVirtual := True
   else if Attributes = [maVirtual, maAbstract] then begin
      IsVirtual := True;
      IsAbstract := True;
   end else if Attributes = [maVirtual, maOverride] then begin
      if (not IsOverlap) or (ParentMeth=nil) then
         raise Exception.CreateFmt(CPE_CanNotOverride, [Name])
      else if (not ParentMeth.IsVirtual)   then
         raise Exception.CreateFmt(CPE_CantOverrideNotVirtual, [Name])
      else if ParentMeth.IsFinal then
         raise Exception.CreateFmt(CPE_CantOverrideFinal, [Name])
      else SetOverride(TMethodSymbol(meth));
   end else if Attributes = [maReintroduce] then
      //
   else if IsClassMethod and ((Attributes = [maStatic]) or (Attributes = [maStatic, maClassMethod]))  then
      SetIsStatic
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
   if val then
      Include(FAttributes, maVirtual)
   else Exclude(FAttributes, maVirtual);
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

// GetIsInterfaced
//
function TMethodSymbol.GetIsInterfaced : Boolean;
begin
   Result:=maInterfaced in FAttributes;
end;

// SetIsInterfaced
//
procedure TMethodSymbol.SetIsInterfaced(const val : Boolean);
begin
   if val then
      Include(FAttributes, maInterfaced)
   else Exclude(FAttributes, maInterfaced);
end;

// GetIsDefault
//
function TMethodSymbol.GetIsDefault : Boolean;
begin
   Result:=maDefault in FAttributes;
end;

// SetIsDefault
//
procedure TMethodSymbol.SetIsDefault(const val : Boolean);
begin
   if val then
      Include(FAttributes, maDefault)
   else Exclude(FAttributes, maDefault);
end;

// GetIsStatic
//
function TMethodSymbol.GetIsStatic : Boolean;
begin
   Result:=maStatic in FAttributes;
end;

// SetIsStatic
//
procedure TMethodSymbol.SetIsStatic;
begin
   Include(FAttributes, maStatic);
   if FSelfSym<>nil then begin
      FInternalParams.Remove(FSelfSym);
      FParams.Remove(FSelfSym);
      FSelfSym.Free;
      FSelfSym:=nil;
   end;
end;

// SetIsFinal
//
procedure TMethodSymbol.SetIsFinal;
begin
   Include(FAttributes, maFinal);
end;

// GetCaption
//
function TMethodSymbol.GetCaption : UnicodeString;
begin
   Result:=inherited GetCaption;
   if IsClassMethod then
      Result:='class '+Result;
end;

// GetDescription
//
function TMethodSymbol.GetDescription: UnicodeString;
begin
   Result:=inherited GetDescription;
   if IsClassMethod then
      Result:='class '+Result;
end;

// GetRootParentMeth
//
function TMethodSymbol.GetRootParentMeth : TMethodSymbol;
begin
   Result:=Self;
   while Result.IsOverride do
      Result:=Result.ParentMeth;
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
function TMethodSymbol.QualifiedName : UnicodeString;
begin
   Result:=StructSymbol.QualifiedName+'.'+Name;
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
function TMethodSymbol.IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean;
begin
   Result:=(FVisibility>=aVisibility);
end;

// IsSameOverloadOf
//
function TMethodSymbol.IsSameOverloadOf(other : TFuncSymbol) : Boolean;
begin
   Result:=    inherited IsSameOverloadOf(other)
           and (other is TMethodSymbol)
           and (IsClassMethod=TMethodSymbol(other).IsClassMethod);
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
constructor TPropertySymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol; aVisibility : TdwsVisibility;
                                   aArrayIndices : TParamsSymbolTable);
begin
   inherited Create(Name, Typ);
   FIndexValue:=nil;
   FVisibility:=aVisibility;
   FArrayIndices:=aArrayIndices;
end;

destructor TPropertySymbol.Destroy;
begin
  FArrayIndices.Free;
  FDefaultSym.Free;
  inherited;
end;

// GetArrayIndices
//
function TPropertySymbol.GetArrayIndices : TParamsSymbolTable;
begin
   if FArrayIndices=nil then
      FArrayIndices:=TParamsSymbolTable.Create;
   Result:=FArrayIndices;
end;

// AddParam
//
procedure TPropertySymbol.AddParam(Param: TParamSymbol);
begin
   ArrayIndices.AddSymbol(Param);
end;

// GetIsDeprecated
//
function TPropertySymbol.GetIsDeprecated : Boolean;
begin
   Result:=(FDeprecatedMessage<>'');
end;

procedure TPropertySymbol.GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
begin
   dwsSymbols.GenerateParams(Table, FuncParams, AddParam);
end;

function TPropertySymbol.GetCaption: UnicodeString;
begin
   Result := GetDescription;
end;

function TPropertySymbol.GetArrayIndicesDescription: UnicodeString;
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
function TPropertySymbol.QualifiedName : UnicodeString;
begin
   Result:=OwnerSymbol.QualifiedName+'.'+Name;
end;

// IsVisibleFor
//
function TPropertySymbol.IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean;
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
function TPropertySymbol.GetDescription : UnicodeString;
begin
   Result := Format('property %s%s: %s', [Name, GetArrayIndicesDescription, Typ.Name]);

   if Assigned(FIndexSym) then
      Result:=Result+' index '+VarToStr(FIndexValue[0]);

   if Assigned(FReadSym) then
      Result := Result + ' read ' + FReadSym.Name;

   if Assigned(FWriteSym) then
      Result := Result + ' write ' + FWriteSym.Name;

   if IsDefault then
      Result := Result + '; default;';
end;

// GetIsDefault
//
function TPropertySymbol.GetIsDefault : Boolean;
begin
   Result:=(OwnerSymbol.DefaultProperty=Self);
end;

procedure TPropertySymbol.SetIndex(const data : TData; Sym: TTypeSymbol);
begin
   FIndexSym := Sym;
   SetLength(FIndexValue,FIndexSym.Size);
   DWSCopyData(data, 0, FIndexValue, 0, FIndexSym.Size);
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
function TClassOperatorSymbol.QualifiedName : UnicodeString;
begin
   Result:=CompositeSymbol.QualifiedName+'.'+Name;
end;

// GetCaption
//
function TClassOperatorSymbol.GetCaption: UnicodeString;
begin
   Result:='class operator '+cTokenStrings[TokenType]+' ';
   if (UsesSym<>nil) and (UsesSym.Params.Count>0) then
      Result:=Result+UsesSym.Params[0].Typ.Name
   else Result:=Result+'???';
   Result:=Result+' uses '+FUsesSym.Name;
end;

// GetDescription
//
function TClassOperatorSymbol.GetDescription: UnicodeString;
begin
   Result:=GetCaption;
end;

// ------------------
// ------------------ TClassSymbol ------------------
// ------------------

// Create
//
constructor TClassSymbol.Create(const name : UnicodeString; aUnit : TSymbol);
begin
   inherited;
   FSize:=1;
   FMetaSymbol:=TClassOfSymbol.Create('class of '+Name, Self);
end;

// Destroy
//
destructor TClassSymbol.Destroy;
begin
   FOperators.Free;
   FInterfaces.Free;
   inherited;
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

// GetIsExternalRooted
//
function TClassSymbol.GetIsExternalRooted : Boolean;
begin
   Result:=IsExternal or (csfExternalRooted in FFlags);
end;

// GetIsPartial
//
function TClassSymbol.GetIsPartial : Boolean;
begin
   Result:=(csfPartial in FFlags);
end;

// GetIsAttribute
//
function TClassSymbol.GetIsAttribute : Boolean;
begin
   Result:=(csfAttribute in FFlags);
end;

// SetIsAttribute
//
procedure TClassSymbol.SetIsAttribute(const val : Boolean);
begin
   if val then
      Include(FFlags, csfAttribute)
   else Exclude(FFlags, csfAttribute);
end;

// SetIsPartial
//
procedure TClassSymbol.SetIsPartial;
begin
   Include(FFlags, csfPartial);
end;

// SetNoVirtualMembers
//
procedure TClassSymbol.SetNoVirtualMembers;
begin
   Include(FFlags, csfNoVirtualMembers);
end;

// SetNoOverloads
//
procedure TClassSymbol.SetNoOverloads;
begin
   Include(FFlags, csfNoOverloads);
end;

// AddField
//
procedure TClassSymbol.AddField(fieldSym : TFieldSymbol);
begin
   inherited;
   fieldSym.FOffset := FScriptInstanceSize;
   FScriptInstanceSize := FScriptInstanceSize + fieldSym.Typ.Size;
end;

// AddMethod
//
procedure TClassSymbol.AddMethod(methSym : TMethodSymbol);
begin
   inherited;
   if methSym.IsAbstract then
      Include(FFlags, csfAbstract);
end;

// AddOperator
//
procedure TClassSymbol.AddOperator(sym: TClassOperatorSymbol);
begin
   sym.CompositeSymbol:=Self;
   FMembers.AddSymbol(sym);
   FOperators.Add(sym);
end;

// AddInterface
//
function TClassSymbol.AddInterface(intfSym : TInterfaceSymbol; visibility : TdwsVisibility;
                                   var missingMethod : TMethodSymbol) : Boolean;
var
   sym : TSymbol;
   iter : TInterfaceSymbol;
   resolved : TResolvedInterface;
   lookup, match : TMethodSymbol;
begin
   resolved.IntfSymbol:=intfSym;
   SetLength(resolved.VMT, intfSym.MethodCount);
   iter:=intfSym;
   while iter<>nil do begin
      for sym in iter.Members do begin
         if sym.Name='' then continue;
         if sym is TMethodSymbol then begin
            lookup:=TMethodSymbol(sym);
            match:=DuckTypedMatchingMethod(lookup, visibility);
            if match=nil then begin
               missingMethod:=lookup;
               Exit(False);
            end else begin
               resolved.VMT[lookup.VMTIndex]:=match;
               match.IsInterfaced:=True;
            end;
         end;
      end;
      iter:=iter.Parent;
   end;

   if FInterfaces=nil then
      FInterfaces:=TResolvedInterfaces.Create;
   FInterfaces.Add(resolved);
   missingMethod:=nil;
   Result:=True;
end;

// ProcessOverriddenInterface
//
function TClassSymbol.ProcessOverriddenInterface(const ancestorResolved : TResolvedInterface) : Boolean;
var
   i : Integer;
   newResolved : TResolvedInterface;
   meth : TMethodSymbol;
begin
   Result:=False;
   newResolved:=ancestorResolved;
   if (FInterfaces<>nil) and FInterfaces.Contains(newResolved) then Exit;
   SetLength(newResolved.VMT, Length(newResolved.VMT)); // make unique
   for i:=0 to High(newResolved.VMT) do begin
      meth:=newResolved.VMT[i];
      if meth.IsVirtual then begin
         if FVirtualMethodTable[meth.VMTIndex]<>meth then begin
            newResolved.VMT[i]:=FVirtualMethodTable[meth.VMTIndex];
            Result:=True;
         end;
      end;
   end;
   if Result then begin
      if FInterfaces=nil then
         FInterfaces:=TResolvedInterfaces.Create;
      FInterfaces.Add(newResolved);
   end;
end;

// ProcessOverriddenInterfaces
//
procedure TClassSymbol.ProcessOverriddenInterfaces;
var
   iter : TClassSymbol;
   loopProtection : TList;
   ri : TResolvedInterfaces;
begin
   iter:=Parent;
   loopProtection:=TList.Create;
   try
      while iter<>nil do begin
         if loopProtection.IndexOf(iter)>0 then Break;
         loopProtection.Add(iter);
         ri:=iter.Interfaces;
         if ri<>nil then begin
            ri.Enumerate(ProcessOverriddenInterfaceCallback);
         end;
         iter:=iter.Parent;
      end;
   finally
      loopProtection.Free;
   end;
end;

// ProcessOverriddenInterfaceCallback
//
procedure TClassSymbol.ProcessOverriddenInterfaceCallback(const item : TResolvedInterface);
begin
   ProcessOverriddenInterface(item);
end;

// ResolveInterface
//
function TClassSymbol.ResolveInterface(intfSym : TInterfaceSymbol; var resolved : TResolvedInterface) : Boolean;
begin
   if FInterfaces<>nil then begin;
      resolved.IntfSymbol:=intfSym;
      Result:=FInterfaces.Match(resolved);
      if Result then Exit;
   end;
   if Parent<>nil then
      Result:=Parent.ResolveInterface(intfSym, resolved)
   else Result:=False;
end;

// ImplementsInterface
//
function TClassSymbol.ImplementsInterface(intfSym : TInterfaceSymbol) : Boolean;
var
   resolved : TResolvedInterface;
begin
   Result:=ResolveInterface(intfSym, resolved);
end;

// FieldAtOffset
//
function TClassSymbol.FieldAtOffset(offset : Integer) : TFieldSymbol;
begin
   Result:=inherited FieldAtOffset(offset);
   if Result=nil then begin
      if Parent<>nil then
         Result:=Parent.FieldAtOffset(offset);
   end;
end;

procedure TClassSymbol.InitData(const Data: TData; Offset: Integer);
const
   cNilIntf : IUnknown = nil;
begin
   Data[Offset]:=cNilIntf;
end;

// Initialize
//
procedure TClassSymbol.Initialize(const msgs : TdwsCompileMessageList);
var
   i, a, v : Integer;
   differentVMT : Boolean;
   sym : TSymbol;
   field : TFieldSymbol;
   meth : TMethodSymbol;
begin
   if csfInitialized in Flags then Exit;
   Include(FFlags, csfInitialized);

   // Check validity of the class declaration
   if IsForwarded then begin
      msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_ClassNotCompletelyDefined, [Name]);
      Exit;
   end;

   if Parent<>nil then begin
      Parent.Initialize(msgs);
      a:=Parent.ScriptInstanceSize;
      FVirtualMethodTable:=Parent.FVirtualMethodTable;
   end else begin
      a:=0;
      FVirtualMethodTable:=nil;
   end;
   v:=Length(FVirtualMethodTable);
   differentVMT:=False;

   // remap field offset & vmt index (cares for partial classes)
   for i:=0 to FMembers.Count-1 do begin
      sym:=FMembers[i];
      if sym.ClassType=TFieldSymbol then begin
         field:=TFieldSymbol(sym);
         field.FOffset:=a;
         Inc(a, field.Typ.Size);
      end else if sym is TMethodSymbol then begin
         meth:=TMethodSymbol(sym);
         if meth.IsVirtual then begin
            differentVMT:=True;
            if meth.IsOverride then
               meth.FVMTIndex:=meth.ParentMeth.VMTIndex
            else begin
               meth.FVMTIndex:=v;
               Inc(v);
            end;
         end;
      end;
   end;
   FScriptInstanceSize:=a;
   // prepare VMT
   if differentVMT then begin
      SetLength(FVirtualMethodTable, v); // make unique (and resize if necessary)
      for sym in FMembers do begin
         if sym is TMethodSymbol then begin
            meth:=TMethodSymbol(sym);
            if meth.IsVirtual then
               FVirtualMethodTable[meth.FVMTIndex]:=meth;
         end;
      end;
   end;
   // update abstract flag
   if csfAbstract in FFlags then begin
      if differentVMT then begin
         Exclude(FFlags, csfAbstract);
         for i:=0 to High(FVirtualMethodTable) do begin
            if FVirtualMethodTable[i].IsAbstract then begin
               Include(FFlags, csfAbstract);
               Break;
            end;
         end;
      end else if not (csfAbstract in Parent.FFlags) then
         Exclude(FFlags, csfAbstract);
   end;
   // process overridden interfaces
   ProcessOverriddenInterfaces;

   CheckMethodsImplemented(msgs);
end;

// InheritFrom
//
procedure TClassSymbol.InheritFrom(ancestorClassSym : TClassSymbol);
begin
   DoInheritFrom(ancestorClassSym);

   if csfAbstract in ancestorClassSym.FFlags then
      Include(FFlags, csfAbstract);
   FScriptInstanceSize:=ancestorClassSym.ScriptInstanceSize;

   IsStatic:=IsStatic or ancestorClassSym.IsStatic;

   if ancestorClassSym.IsAttribute then
      Include(FFlags, csfAttribute);

   if [csfExternalRooted, csfExternal]*ancestorClassSym.Flags<>[] then
      Include(FFlags, csfExternalRooted);

   if csfNoVirtualMembers in ancestorClassSym.FFlags then
      SetNoVirtualMembers;
   if    (csfNoOverloads in ancestorClassSym.FFlags)
      or (        (csfExternalRooted in FFlags)
          and not (csfExternal in FFlags)) then
      SetNoOverloads;
end;

// IsCompatible
//
function TClassSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   if typSym=nil then
      Result:=False
   else begin
      typSym:=typSym.UnAliasedType;
      if typSym is TNilSymbol then
         Result:=True
      else if typSym is TClassSymbol then
         Result:=(NthParentOf(TClassSymbol(typSym))>=0)
      else Result:=False;
   end;
end;

// IsPointerType
//
function TClassSymbol.IsPointerType : Boolean;
begin
   Result:=True;
end;

// HasMetaSymbol
//
function TClassSymbol.HasMetaSymbol : Boolean;
begin
   Result:=True;
end;

// DoIsOfType
//
function TClassSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(Self=typSym.UnAliasedType);
   if Result or (Self=nil) then Exit;
   if Parent<>nil then
      Result:=Parent.DoIsOfType(typSym.UnAliasedType)
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

function TClassSymbol.GetDescription: UnicodeString;
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
            and paramType.DoIsOfType(Result.Typ) then Exit;
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

// FindDefaultConstructor
//
function TClassSymbol.FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol;
var
   i : Integer;
   member : TSymbol;
   createConstructor : TMethodSymbol;
begin
   createConstructor:=nil;
   for i:=0 to FMembers.Count-1 do begin
      member:=FMembers[i];
      if member is TMethodSymbol then begin
         Result:=TMethodSymbol(member);
         if (Result.Visibility>=minVisibility) and (Result.Kind=fkConstructor) then begin
            if Result.IsDefault then
               Exit;
            if UnicodeSameText(Result.Name, 'Create') then
               createConstructor:=Result;
         end;
      end;
   end;
   if createConstructor<>nil then
      Result:=createConstructor
   else if Parent<>nil then begin
      if minVisibility=cvPrivate then
         minVisibility:=cvProtected;
      Result:=Parent.FindDefaultConstructor(minVisibility);
   end else Result:=nil;
end;

// AllowVirtualMembers
//
function TClassSymbol.AllowVirtualMembers : Boolean;
begin
   Result:=not (csfNoVirtualMembers in FFlags);
end;

// AllowOverloads
//
function TClassSymbol.AllowOverloads : Boolean;
begin
   Result:=not (csfNoOverloads in FFlags);
end;

// AllowFields
//
function TClassSymbol.AllowFields : Boolean;
begin
   Result:=True;
end;

// AllowAnonymousMethods
//
function TClassSymbol.AllowAnonymousMethods : Boolean;
begin
   Result:=(not IsExternal);
end;

// CreateSelfParameter
//
function TClassSymbol.CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol;
begin
   if methSym.IsClassMethod then
      Result:=TSelfSymbol.Create(SYS_SELF, MetaSymbol)
   else Result:=TSelfSymbol.Create(SYS_SELF, Self);
   methSym.InternalParams.AddSymbol(Result);
end;

// CreateAnonymousMethod
//
function TClassSymbol.CreateAnonymousMethod(
      aFuncKind : TFuncKind; aVisibility : TdwsVisibility; isClassMethod : Boolean) : TMethodSymbol;
begin
   Result:=TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility, isClassMethod);
end;

// VisibilityToString
//
class function TClassSymbol.VisibilityToString(visibility : TdwsVisibility) : UnicodeString;
const
   cVisibilityNames : array [TdwsVisibility] of UnicodeString = (
      'magic', 'private', 'protected', 'public', 'published' );
begin
   Result:=cVisibilityNames[visibility];
end;

// Parent
//
function TClassSymbol.Parent : TClassSymbol;
begin
   Result:=TClassSymbol(FParent);
end;

// ------------------
// ------------------ TNilSymbol ------------------
// ------------------

constructor TNilSymbol.Create;
begin
  inherited Create('<nil>', nil);
  FSize := 1;
end;

function TNilSymbol.GetCaption: UnicodeString;
begin
  Result := 'nil';
end;

function TNilSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  typSym := typSym.BaseType;
  Result := (TypSym is TClassSymbol) or (TypSym is TNilSymbol);
end;

// ------------------
// ------------------ TClassOfSymbol ------------------
// ------------------

constructor TClassOfSymbol.Create(const Name: UnicodeString; Typ: TClassSymbol);
begin
  inherited Create(Name, Typ);
end;

function TClassOfSymbol.GetCaption: UnicodeString;
begin
   if Name <> '' then
      Result := Name
   else Result := GetDescription;
end;

// GetDescription
//
function TClassOfSymbol.GetDescription : UnicodeString;
begin
   if Typ <> nil then
      Result := 'class of ' + Typ.Name
   else Result := 'class of ???';
end;

function TClassOfSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  typSym := typSym.BaseType;
  Result :=    (typSym is TNilSymbol)
            or ((typSym is TClassOfSymbol) and Typ.IsCompatible(typSym.Typ));
end;

// SameType
//
function TClassOfSymbol.SameType(typSym : TTypeSymbol) : Boolean;
begin
   Result :=     (typSym<>nil)
             and (typSym.ClassType=TClassOfSymbol)
             and (Typ.SameType(typSym.Typ));
end;

// DoIsOfType
//
function TClassOfSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   if typSym is TClassOfSymbol then
      Result:=Typ.DoIsOfType(typSym.Typ.UnAliasedType)
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
constructor TBaseSymbol.Create(const name : UnicodeString);
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

// IsCompatible
//
function TBaseIntegerSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   if typSym<>nil then begin
      Result:=   (UnAliasedType=typSym.UnAliasedType)
              or (    (typSym.ClassType=TEnumerationSymbol)
                  and  IsCompatible(typSym.Typ));
   end else Result:=False;
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
constructor TBaseVariantSymbol.Create(const name : UnicodeString = '');
begin
   if name='' then
      inherited Create(SYS_VARIANT)
   else inherited Create(name);
end;

// IsCompatible
//
function TBaseVariantSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
var
   ct : TClass;
begin
   if typSym<>nil then begin
      typSym:=typSym.UnAliasedType;
      if typSym.InheritsFrom(TBaseSymbol) then
         Result:=True
      else begin
         ct:=typSym.ClassType;
         Result:=   (ct=TEnumerationSymbol)
                 or (ct=TClassSymbol)
                 or (ct=TNilSymbol)
                 or (ct=TInterfaceSymbol);
      end;
   end else Result:=False;
end;

// InitData
//
procedure TBaseVariantSymbol.InitData(const data : TData; offset : Integer);
begin
   VarClear(data[offset]);
end;

// SupportsEmptyParam
//
function TBaseVariantSymbol.SupportsEmptyParam : Boolean;
begin
   Result:=True;
end;

// ------------------
// ------------------ TParamsSymbolTable ------------------
// ------------------

// GetSymbol
//
function TParamsSymbolTable.GetSymbol(x : Integer) : TParamSymbol;
begin
   Result:=TParamSymbol(inherited Symbols[x]);
   Assert(Result is TParamSymbol);
end;

// ------------------
// ------------------ TValueSymbol ------------------
// ------------------

// Create
//
constructor TValueSymbol.Create(const aName : UnicodeString; aType : TTypeSymbol);
begin
   UnifyAssignString(aName, FName);
   FTyp:=aType;
   FSize:=aType.Size;
end;

function TValueSymbol.GetCaption: UnicodeString;
begin
  Result := Name + ': ' + Typ.Caption;
end;

function TValueSymbol.GetDescription: UnicodeString;
begin
  Result := Name + ': ' + Typ.Caption;
end;

// ------------------
// ------------------ TConstSymbol ------------------
// ------------------

// CreateValue
//
constructor TConstSymbol.CreateValue(const Name: UnicodeString; Typ: TTypeSymbol; const Value: Variant);
begin
   inherited Create(Name, Typ);
   Assert(Typ.Size=1);
   SetLength(FData, 1);
   VarCopy(FData[0], Value);
end;

// CreateData
//
constructor TConstSymbol.CreateData(const Name: UnicodeString; Typ: TTypeSymbol; const data : TData);
begin
   inherited Create(Name, Typ);
   SetLength(FData, Typ.Size);
   DWSCopyData(data, 0, FData, 0, Typ.Size);
end;

function TConstSymbol.GetCaption: UnicodeString;
begin
  Result := 'const ' + inherited GetCaption;
end;

function TConstSymbol.GetDescription: UnicodeString;
begin
  if VarType(FData[0]) = varError then
    Result := 'const ' + inherited GetDescription + ' = [varError]'
  else
    Result := 'const ' + inherited GetDescription + ' = ' + VarToStr(FData[0]);
end;

procedure TConstSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
end;

// ------------------
// ------------------ TDataSymbol ------------------
// ------------------

function TDataSymbol.GetDescription: UnicodeString;
begin
   if Assigned(Typ) then
      if Typ.Name<>'' then
         Result:=Name+': '+Typ.Name
      else Result:=Name+': '+Typ.Description
  else Result:=Name+': ???';
end;

// GetExternalName
//
function TDataSymbol.GetExternalName : UnicodeString;
begin
   if FExternalName='' then
      Result:=Name
   else Result:=FExternalName;
end;

// AllocateStackAddr
//
procedure TDataSymbol.AllocateStackAddr(generator : TAddrGenerator);
begin
   FLevel:=generator.Level;
   FStackAddr:=generator.GetStackAddr(Size);
end;

// HasExternalName
//
function TDataSymbol.HasExternalName : Boolean;
begin
   Result:=(FExternalName<>'');
end;

// ------------------
// ------------------ TParamSymbol ------------------
// ------------------

// SameParam
//
function TParamSymbol.SameParam(other : TParamSymbol) : Boolean;
begin
   Result:=    (   (ClassType=other.ClassType)
                or (    (ClassType=TParamSymbol)
                    and (other.ClassType=TParamSymbolWithDefaultValue)))
           and (Typ.SameType(other.Typ))
           and UnicodeSameText(Name, other.Name);
end;

// Clone
//
function TParamSymbol.Clone : TParamSymbol;
begin
   Result:=TParamSymbol.Create(Name, Typ);
end;

// ------------------
// ------------------ TParamSymbolWithDefaultValue ------------------
// ------------------

// Create
//
constructor TParamSymbolWithDefaultValue.Create(const aName : UnicodeString; aType : TTypeSymbol;
                                                const data : TData);
begin
   inherited Create(aName, aType);
   SetLength(FDefaultValue, Typ.Size);
   if Length(data)>0 then
      DWSCopyData(data, 0, FDefaultValue, 0, Typ.Size);
end;

// Clone
//
function TParamSymbolWithDefaultValue.Clone : TParamSymbol;
begin
   Result:=TParamSymbolWithDefaultValue.Create(Name, Typ, FDefaultValue);
end;

// SameParam
//
function TParamSymbolWithDefaultValue.SameParam(other : TParamSymbol) : Boolean;
begin
   Result:=    inherited SameParam(other)
           and DWSSameData(FDefaultValue, (other as TParamSymbolWithDefaultValue).FDefaultValue,
                           0, 0, Typ.Size);
end;

function TParamSymbolWithDefaultValue.GetDescription: UnicodeString;
begin
   Result := inherited GetDescription;

   // Has a default parameter. Format display of param to show it.
   if Length(FDefaultValue) > 0 then begin
      if (Typ is TBaseStringSymbol) then
         Result := Result + ' = ''' + VarToStr(FDefaultValue[0]) + ''''  // put quotes around value
      else if VarType(FDefaultValue[0])=varUnknown then
         Result := Result + ' = nil'
      else Result := Result + ' = ' + VarToStr(FDefaultValue[0]);
   end;
end;

// ------------------
// ------------------ TByRefParamSymbol ------------------
// ------------------

constructor TByRefParamSymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol);
begin
  inherited Create(Name, Typ);
  FSize := 1;
end;

// Clone
//
function TByRefParamSymbol.Clone : TParamSymbol;
begin
   Result:=TByRefParamSymbol.Create(Name, Typ);
end;

// ------------------
// ------------------ TLazyParamSymbol ------------------
// ------------------

// GetDescription
//
function TLazyParamSymbol.GetDescription: UnicodeString;
begin
   Result:='lazy '+inherited GetDescription;
end;

// Clone
//
function TLazyParamSymbol.Clone : TParamSymbol;
begin
   Result:=TLazyParamSymbol.Create(Name, Typ);
end;

{ TConstParamSymbol }

function TConstParamSymbol.GetDescription: UnicodeString;
begin
  Result := 'const ' + inherited GetDescription;
end;

// Clone
//
function TConstParamSymbol.Clone : TParamSymbol;
begin
   Result:=TConstParamSymbol.Create(Name, Typ);
end;

{ TVarParamSymbol }

function TVarParamSymbol.GetDescription: UnicodeString;
begin
  Result := 'var ' + inherited GetDescription;
end;

// Clone
//
function TVarParamSymbol.Clone : TParamSymbol;
begin
   Result:=TVarParamSymbol.Create(Name, Typ);
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
   FParents.Clear;
   inherited;
end;

procedure TSymbolTable.Initialize(const msgs : TdwsCompileMessageList);
var
   i : Integer;
   ptrList : PObjectTightList;
begin
   ptrList:=FSymbols.List;
   for i:=0 to FSymbols.Count-1 do
      TSymbol(ptrList[i]).Initialize(msgs);
end;

// FindLocal
//
function TSymbolTable.FindLocal(const aName : UnicodeString; ofClass : TSymbolClass = nil) : TSymbol;
var
   n : Integer;
begin
   n:=FSymbols.Count;
   if n>6 then begin
      if not (stfSorted in FFlags) then begin
         SortSymbols(0, n-1);
         Include(FFlags, stfSorted);
      end;
      Result:=FindLocalSorted(aName);
   end else begin
      Result:=FindLocalUnSorted(aName);
   end;
   if (Result<>nil) and (ofClass<>nil) and (not Result.InheritsFrom(ofClass)) then
      Result:=nil;
end;

// FindTypeLocal
//
function TSymbolTable.FindTypeLocal(const aName : UnicodeString) : TTypeSymbol;
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
      sym:=TSymbol(FSymbols.List[i]);
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
  ptrList : PObjectTightList;
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
function TSymbolTable.FindLocalSorted(const name : UnicodeString) : TSymbol;
var
   lo, hi, mid, cmpResult: Integer;
   ptrList : PObjectTightList;
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
function TSymbolTable.FindLocalUnSorted(const name: UnicodeString) : TSymbol;
var
   i : Integer;
   ptrList : PObjectTightList;
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
function TSymbolTable.FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility;
                                 ofClass : TSymbolClass = nil) : TSymbol;
var
   i : Integer;
   parentSymTable : TSymbolTable;
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
      parentSymTable:=Parents[i];
      Result:=parentSymTable.FindSymbol(aName, minVisibility, ofClass);
      if Assigned(Result) then Break;
   end;
end;

// FindTypeSymbol
//
function TSymbolTable.FindTypeSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility) : TTypeSymbol;
begin
   Result:=TTypeSymbol(FindSymbol(aName, minVisibility, TTypeSymbol));
end;

// EnumerateLocalSymbolsOfName
//
function TSymbolTable.EnumerateLocalSymbolsOfName(const aName : UnicodeString; const callback : TSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   sym : TSymbol;
begin
   // TODO: optimize to take advantage of sorting
   for i:=0 to Count-1 do begin
      sym:=Symbols[i];
      if UnicodeSameText(sym.Name, aName) then begin
         if callback(sym) then Exit(True);
      end;
   end;
   Result:=False;
end;

// EnumerateSymbolsOfNameInScope
//
function TSymbolTable.EnumerateSymbolsOfNameInScope(const aName : UnicodeString;
                        const callback : TSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   visitedTables : TSimpleObjectHash<TSymbolTable>;
   tableStack : TSimpleStack<TSymbolTable>;
   current : TSymbolTable;
begin
   visitedTables:=TSimpleObjectHash<TSymbolTable>.Create;
   tableStack:=TSimpleStack<TSymbolTable>.Create;
   try
      tableStack.Push(Self);
      while tableStack.Count>0 do begin
         current:=tableStack.Peek;
         tableStack.Pop;
         if visitedTables.Add(current) then begin
            if current.EnumerateLocalSymbolsOfName(aName, callback) then Exit(True);
            for i:=0 to current.ParentCount-1 do
               tableStack.Push(current.Parents[i]);
         end;
      end;
      Result:=False;
   finally
      tableStack.Free;
      visitedTables.Free;
   end;
end;

// EnumerateLocalHelpers
//
function TSymbolTable.EnumerateLocalHelpers(helpedType : TTypeSymbol; const callback : THelperSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   sym : TSymbol;
begin
   if stfHasHelpers in FFlags then begin
      for i:=0 to Count-1 do begin
         sym:=Symbols[i];
         if sym.ClassType=THelperSymbol then
            if THelperSymbol(sym).HelpsType(helpedType) then begin
               if callback(THelperSymbol(sym)) then Exit(True);
         end;
      end;
   end;
   Result:=False;
end;

// EnumerateHelpers
//
function TSymbolTable.EnumerateHelpers(helpedType : TTypeSymbol; const callback : THelperSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   p : TSymbolTable;
begin
   if EnumerateLocalHelpers(helpedType, callback) then Exit(True);
   for i:=0 to ParentCount-1 do begin
      p:=Parents[i];
      if p.IsUnitTable then begin
         if p.EnumerateLocalHelpers(helpedType, callback) then
            Exit(True)
      end;
      if p.EnumerateHelpers(helpedType, callback) then Exit(True);
   end;
   Result:=False;
end;

// EnumerateLocalOperatorsFor
//
function TSymbolTable.EnumerateLocalOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   sym : TSymbol;
   opSym : TOperatorSymbol;
   leftParam, rightParam : TTypeSymbol;
begin
   if stfHasLocalOperators in FFlags then begin
      for i:=0 to Count-1 do begin
         sym:=Symbols[i];
         if sym.ClassType=TOperatorSymbol then begin
            opSym:=TOperatorSymbol(sym);
            if opSym.Token<>aToken then continue;
            leftParam:=opSym.Params[0];
            if     (aLeftType<>leftParam)
               and not aLeftType.IsOfType(leftParam) then continue;
            rightParam:=opSym.Params[1];
            if     (aRightType<>rightParam)
               and not aRightType.IsOfType(rightParam) then continue;
            if callback(opSym) then Exit(True);
         end;
      end;
   end;
   Result:=False;
end;

// EnumerateOperatorsFor
//
function TSymbolTable.EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                            const callback : TOperatorSymbolEnumerationCallback) : Boolean;
var
   i : Integer;
   p : TSymbolTable;
begin
   if stfHasLocalOperators in FFlags then
      if EnumerateLocalOperatorsFor(aToken, aLeftType, aRightType, callback) then Exit(True);
   if stfHasParentOperators in FFlags then begin
      for i:=0 to ParentCount-1 do begin
         p:=Parents[i];
         if p.EnumerateOperatorsFor(aToken, aLeftType, aRightType, callback) then Exit(True);
      end;
   end;
   Result:=False;
end;

// HasSameLocalOperator
//
function TSymbolTable.HasSameLocalOperator(anOpSym : TOperatorSymbol) : Boolean;
var
   i : Integer;
   sym : TSymbol;
   opSym : TOperatorSymbol;
   leftType, rightType : TTypeSymbol;
begin
   Result:=False;
   if not (stfHasLocalOperators in FFlags) then Exit;
   if Length(anOpSym.Params)<>2 then Exit;
   leftType:=anOpSym.Params[0];
   rightType:=anOpSym.Params[1];
   if (leftType=nil) or (rightType=nil) then Exit;

   leftType:=leftType.UnAliasedType;
   rightType:=rightType.UnAliasedType;
   for i:=0 to Count-1 do begin
      sym:=Symbols[i];
      if sym=anOpSym then continue;
      if sym.ClassType=TOperatorSymbol then begin
         opSym:=TOperatorSymbol(sym);
         if     (opSym.Token=anOpSym.Token)
            and (leftType=opSym.Params[0].UnAliasedType)
            and (rightType=opSym.Params[1].UnAliasedType) then begin
            Exit(True);
         end;
      end;
   end;
end;

// HasOperators
//
function TSymbolTable.HasOperators : Boolean;
begin
   Result:=(stfHasOperators in FFlags);
end;

// CollectPublishedSymbols
//
procedure TSymbolTable.CollectPublishedSymbols(symbolList : TSimpleSymbolList);
var
   sym, member : TSymbol;
begin
   for sym in Self do begin
      if sym.ClassType=TClassSymbol then begin
         for member in TClassSymbol(sym).Members do begin
            if member.ClassType=TPropertySymbol then begin
               if TPropertySymbol(member).Visibility=cvPublished then
                  symbolList.Add(member);
            end else if member.ClassType=TFieldSymbol then begin
               if TFieldSymbol(member).Visibility=cvPublished then
                  symbolList.Add(member);
            end else if member.InheritsFrom(TMethodSymbol) then begin
               if TMethodSymbol(member).Visibility=cvPublished then
                  symbolList.Add(member);
            end;
         end;
      end;
   end;
end;

// HasChildTables
//
function TSymbolTable.HasChildTables : Boolean;
begin
   Result:=stfHasChildTables in FFlags;
end;

// HasClass
//
function TSymbolTable.HasClass(const aClass : TSymbolClass) : Boolean;
var
   i : Integer;
   ptrList : PObjectTightList;
begin
   ptrList:=FSymbols.List;
   for i:=FSymbols.Count-1 downto 0 do begin
      if TSymbol(ptrList[i]) is aClass then
         Exit(True);
   end;
   Result:=False;
end;

// HasSymbol
//
function TSymbolTable.HasSymbol(sym : TSymbol) : Boolean;
begin
   Result:=Assigned(Self) and (FSymbols.IndexOf(sym)>=0);
end;

// HasMethods
//
function TSymbolTable.HasMethods : Boolean;
var
   i : Integer;
   ptrList : PObjectTightList;
begin
   ptrList:=FSymbols.List;
   for i:=FSymbols.Count-1 downto 0 do begin
      if TSymbol(ptrList[i]).AsFuncSymbol<>nil then
         Exit(True);
   end;
   Result:=False;
end;

// IsUnitTable
//
class function TSymbolTable.IsUnitTable : Boolean;
begin
   Result:=False;
end;

// GetCount
//
function TSymbolTable.GetCount : Integer;
begin
   Result:=FSymbols.Count
end;

// GetSymbol
//
function TSymbolTable.GetSymbol(index : Integer) : TSymbol;
begin
   Result:=TSymbol(FSymbols.List[Index]);
end;

// AddSymbol
//
function TSymbolTable.AddSymbol(sym : TSymbol) : Integer;
var
   ct : TClass;
begin
   Result:=AddSymbolDirect(sym);
   ct:=sym.ClassType;
   if ct=THelperSymbol then
      Include(FFlags, stfHasHelpers)
   else if ct=TOperatorSymbol then
      FFlags:=FFlags+[stfHasOperators, stfHasLocalOperators]
   else if (FAddrGenerator<>nil) and sym.InheritsFrom(TDataSymbol) then
      TDataSymbol(sym).AllocateStackAddr(FAddrGenerator);
end;

// AddSymbolDirect
//
function TSymbolTable.AddSymbolDirect(sym : TSymbol) : Integer;
var
   n : Integer;
   ptrList : PObjectTightList;
begin
   if stfSorted in FFlags then begin
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

// TransferSymbolsTo
//
procedure TSymbolTable.TransferSymbolsTo(destTable : TSymbolTable);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      destTable.AddSymbol(Symbols[i]);
   FSymbols.Clear;
end;

// AddParent
//
procedure TSymbolTable.AddParent(Parent: TSymbolTable);
begin
   InsertParent(ParentCount, Parent);
end;

// InsertParent
//
procedure TSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
begin
   Include(Parent.FFlags, stfHasChildTables);
   FParents.Insert(Index, Parent);
   if stfHasOperators in Parent.FFlags then
      FFlags:=FFlags+[stfHasOperators, stfHasParentOperators];
end;

// RemoveParent
//
function TSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
   Result:=FParents.Remove(Parent);
end;

// ClearParents
//
procedure TSymbolTable.ClearParents;
begin
   FParents.Clear;
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
   FParents.MoveItem(CurIndex,NewIndex);
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
function TMembersSymbolTable.FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility;
                                        ofClass : TSymbolClass = nil) : TSymbol;
var
   i : Integer;
begin
   // Find Symbol in the local List
   Result:=FindLocal(aName, ofClass);
   if Assigned(Result) then begin
      if Result.IsVisibleFor(minVisibility) then Exit;
      // try harder in case of overload with different visibility
      for Result in Self do begin
         if     (UnicodeCompareText(Result.Name, aName)=0)
            and Result.IsVisibleFor(minVisibility) then Exit;
      end;
   end;
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

// VisibilityFromScope
//
function TMembersSymbolTable.VisibilityFromScope(scopeSym : TCompositeTypeSymbol) : TdwsVisibility;
begin
   if scopeSym=nil then
      Result:=cvPublic
   else if    (scopeSym=Owner)
           or (    (scopeSym.UnitSymbol<>nil)
               and (scopeSym.UnitSymbol=Owner.UnitSymbol)) then
      Result:=cvPrivate
   else if scopeSym.DoIsOfType(Owner) then
      Result:=cvProtected
   else Result:=cvPublic;
end;

// FindSymbolFromScope
//
function TMembersSymbolTable.FindSymbolFromScope(const aName : UnicodeString; scopeSym : TCompositeTypeSymbol) : TSymbol;
begin
   Result:=FindSymbol(aName, VisibilityFromScope(scopeSym));
end;

// Visibilities
//
function TMembersSymbolTable.Visibilities : TdwsVisibilities;
var
   sym : TSymbol;
   symClass : TClass;
begin
   Result:=[];
   for sym in Self do begin
      symClass:=sym.ClassType;
      if symClass=TFieldSymbol then
         Include(Result, TFieldSymbol(sym).Visibility)
      else if symClass.InheritsFrom(TPropertySymbol) then
         Include(Result, TPropertySymbol(sym).Visibility)
      else if symClass.InheritsFrom(TMethodSymbol) then
         Include(Result, TMethodSymbol(symClass).Visibility)
   end;
end;

// ------------------
// ------------------ TUnSortedSymbolTable ------------------
// ------------------

// FindLocal
//
function TUnSortedSymbolTable.FindLocal(const aName : UnicodeString; ofClass : TSymbolClass = nil) : TSymbol;
begin
   Result:=FindLocalUnSorted(aName);
   if (Result<>nil) and (ofClass<>nil) and (not (Result is ofClass)) then
      Result:=nil;
end;

// ------------------
// ------------------ TExternalVarSymbol ------------------
// ------------------

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

// ------------------
// ------------------ TAddrGeneratorRec ------------------
// ------------------

// CreatePositive
//
class function TAddrGeneratorRec.CreatePositive(aLevel : SmallInt; anInitialSize: Integer = 0) : TAddrGeneratorRec;
begin
   Result.DataSize:=anInitialSize;
   Result.FLevel:=aLevel;
   Result.FSign:=agsPositive;
end;

// CreateNegative
//
class function TAddrGeneratorRec.CreateNegative(aLevel : SmallInt) : TAddrGeneratorRec;
begin
   Result.DataSize:=0;
   Result.FLevel:=aLevel;
   Result.FSign:=agsNegative;
end;

// GetStackAddr
//
function TAddrGeneratorRec.GetStackAddr(size : Integer): Integer;
begin
   if FSign=agsPositive then begin
      Result:=DataSize;
      Inc(DataSize, Size);
   end else begin
      Inc(DataSize, Size);
      Result:=-DataSize;
   end;
end;

// ------------------
// ------------------ TSetOfSymbol ------------------
// ------------------

// Create
//
constructor TSetOfSymbol.Create(const name : UnicodeString; indexType : TTypeSymbol;
                              aMin, aMax : Integer);
begin
   inherited Create(name, indexType);
   FMinValue:=aMin;
   FCountValue:=aMax-aMin+1;
   FSize:=1+(FCountValue shr 6);
end;

// IsCompatible
//
function TSetOfSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   typSym:=typSym.UnAliasedType;
   if typSym is TSetOfSymbol then begin
      Result:=     TSetOfSymbol(typSym).Typ.IsOfType(Typ)
              and  (TSetOfSymbol(typSym).MinValue=MinValue)
              and  (TSetOfSymbol(typSym).CountValue=CountValue);
   end else Result:=False;
end;

// InitData
//
procedure TSetOfSymbol.InitData(const data : TData; offset : Integer);
const
   cZero64 : Int64 = 0;
var
   i : Integer;
begin
   for i:=offset to offset+Size-1 do
      data[i]:=cZero64;
end;

// ValueToOffsetMask
//
function TSetOfSymbol.ValueToOffsetMask(value : Integer; var mask : Int64) : Integer;
begin
   Result:=(value-MinValue) shr 6;
   mask:=Int64(1) shl (value and 63);
end;

// ValueToByteOffsetMask
//
function TSetOfSymbol.ValueToByteOffsetMask(value : Integer; var mask : Byte) : Integer;
begin
   Result:=(value-MinValue) shr 3;
   mask:=1 shl (value and 7);
end;

// GetMaxValue
//
function TSetOfSymbol.GetMaxValue : Integer;
begin
   Result:=MinValue+CountValue-1;
end;

// ------------------
// ------------------ TArraySymbol ------------------
// ------------------

// Create
//
constructor TArraySymbol.Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
begin
   inherited Create(name, elementType);
   FIndexType:=indexType;
end;

// Destroy
//
destructor TArraySymbol.Destroy;
begin
   FSortFunctionType.Free;
   FMapFunctionType.Free;
   inherited;
end;

// ElementSize
//
function TArraySymbol.ElementSize : Integer;
begin
   if Typ<>nil then
      Result:=Typ.Size
   else Result:=0;
end;

// SortFunctionType
//
function TArraySymbol.SortFunctionType(integerType : TTypeSymbol) : TFuncSymbol;
begin
   if FSortFunctionType=nil then begin
      FSortFunctionType:=TFuncSymbol.Create('', fkFunction, 0);
      FSortFunctionType.Typ:=integerType;
      FSortFunctionType.AddParam(TParamSymbol.Create('left', Typ));
      FSortFunctionType.AddParam(TParamSymbol.Create('right', Typ));
   end;
   Result:=FSortFunctionType;
end;

// MapFunctionType
//
function TArraySymbol.MapFunctionType(anyType : TTypeSymbol) : TFuncSymbol;
begin
   if FMapFunctionType=nil then begin
      FMapFunctionType:=TFuncSymbol.Create('', fkFunction, 0);
      FMapFunctionType.Typ:=anyType;
      FMapFunctionType.AddParam(TParamSymbol.Create('v', Typ));
   end;
   Result:=FMapFunctionType;
end;

// ------------------
// ------------------ TDynamicArraySymbol ------------------
// ------------------

var
   vInitDynamicArray : TInitDynamicArrayProc;

// Create
//
constructor TDynamicArraySymbol.Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
begin
  inherited;
  FSize:=1;
end;

// GetCaption
//
function TDynamicArraySymbol.GetCaption: UnicodeString;
begin
   Result := 'array of '+Typ.Caption
end;

// InitData
//
procedure TDynamicArraySymbol.InitData(const Data: TData; Offset: Integer);
begin
   vInitDynamicArray(Self.Typ, Data[Offset]);
end;

// DoIsOfType
//
function TDynamicArraySymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   (typSym=Self)
           or ((typSym is TDynamicArraySymbol) and typSym.Typ.DoIsOfType(Typ));
end;

// IsCompatible
//
function TDynamicArraySymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  Result :=    (    (typSym is TDynamicArraySymbol)
//                and (typSym.Typ.IsCompatible(Typ) or (typSym.Typ is TNilSymbol))
                and (Typ.IsCompatible(typSym.Typ) or (typSym.Typ is TNilSymbol))
            or (    (typSym is TStaticArraySymbol)
                and TStaticArraySymbol(typSym).IsEmptyArray));
end;

// IsPointerType
//
function TDynamicArraySymbol.IsPointerType : Boolean;
begin
   Result:=True;
end;

// SameType
//
function TDynamicArraySymbol.SameType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=    (typSym<>nil)
           and (typSym.ClassType=TDynamicArraySymbol)
           and Typ.SameType(typSym.Typ);
end;

// SetInitDynamicArrayProc
//
class procedure TDynamicArraySymbol.SetInitDynamicArrayProc(const aProc : TInitDynamicArrayProc);
begin
   vInitDynamicArray:=aProc;
end;

// ------------------
// ------------------ TStaticArraySymbol ------------------
// ------------------

// Create
//
constructor TStaticArraySymbol.Create(const name : UnicodeString; elementType, indexType : TTypeSymbol;
                                      lowBound, highBound : Integer);
begin
   inherited Create(name, elementType, indexType);
   FLowBound := lowBound;
   FHighBound := highBound;
   FElementCount := highBound - lowBound + 1;
   FSize := FElementCount * ElementSize;
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
  typSym := typSym.UnAliasedType;
  Result :=     (typSym is TStaticArraySymbol)
            and (ElementCount = TStaticArraySymbol(typSym).ElementCount)
            and Typ.IsCompatible(typSym.Typ);
end;

// SameType
//
function TStaticArraySymbol.SameType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=    (typSym<>nil)
           and (typSym.ClassType=ClassType)
           and (Typ.SameType(typSym.Typ))
           and (LowBound=TStaticArraySymbol(typSym).LowBound)
           and (HighBound=TStaticArraySymbol(typSym).HighBound);
end;

// DoIsOfType
//
function TStaticArraySymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=inherited DoIsOfType(typSym);
   if not Result then begin
      if typSym.ClassType=TStaticArraySymbol then
         Result:=    (LowBound=TStaticArraySymbol(typSym).LowBound)
                 and (HighBound=TStaticArraySymbol(typSym).HighBound)
                 and Typ.IsCompatible(TypSym.Typ)
      else if typSym is TOpenArraySymbol then
         Result:=(ElementCount=0) or (Typ.IsCompatible(TypSym.Typ))
   end;
end;

// AddElement
//
procedure TStaticArraySymbol.AddElement;
begin
   Inc(FHighBound);
   Inc(FElementCount);
   FSize:=FElementCount*ElementSize;
end;

// IsEmptyArray
//
function TStaticArraySymbol.IsEmptyArray : Boolean;
begin
   Result:=(HighBound<LowBound);
end;

// GetCaption
//
function TStaticArraySymbol.GetCaption;
begin
   Result:= 'array ['+IntToStr(FLowBound)+'..'+IntToStr(FHighBound)
           +'] of '+Typ.Caption;
end;

// ------------------
// ------------------ TOpenArraySymbol ------------------
// ------------------

// Create
//
constructor TOpenArraySymbol.Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
begin
   inherited Create(name, elementType, indexType, 0, -1);
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

// GetCaption
//
function TOpenArraySymbol.GetCaption : UnicodeString;
begin
   Result:='array of const';
end;

// ------------------
// ------------------ TElementSymbol ------------------
// ------------------

// Create
//
constructor TElementSymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol;
                                  const aValue : Int64; isUserDef: Boolean);
begin
   inherited CreateValue(Name, Typ, aValue);
   FIsUserDef := IsUserDef;
end;

// StandardName
//
function TElementSymbol.StandardName : UnicodeString;
begin
   if Enumeration.Style=enumClassic then
      Result:=Name
   else Result:=QualifiedName;
end;

// QualifiedName
//
function TElementSymbol.QualifiedName : UnicodeString;
begin
   Result:=Enumeration.Name+'.'+Name;
end;

// GetDescription
//
function TElementSymbol.GetDescription: UnicodeString;
begin
   if FIsUserDef then
      Result:=Name+' = '+IntToStr(Value)
   else Result:=Name;
end;

// GetValue
//
function TElementSymbol.GetValue : Int64;
begin
   Result:=PVarData(@Data[0]).VInt64;
end;

// ------------------
// ------------------ TEnumerationSymbol ------------------
// ------------------

// Create
//
constructor TEnumerationSymbol.Create(const Name: UnicodeString; BaseType: TTypeSymbol;
                                      aStyle : TEnumerationSymbolStyle);
begin
   inherited Create(Name, BaseType);
   FElements:=TUnSortedSymbolTable.Create;
   FLowBound:=MaxInt;
   FHighBound:=-MaxInt;
   FStyle:=aStyle;
   FContinuous:=True;
end;

// Destroy
//
destructor TEnumerationSymbol.Destroy;
begin
   FElements.Free;
   inherited;
end;

// DefaultValue
//
function TEnumerationSymbol.DefaultValue : Int64;
begin
   if FElements.Count>0 then
      Result:=TElementSymbol(FElements[0]).Value
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

// IsCompatible
//
function TEnumerationSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym.UnAliasedType=Self);
end;

// DoIsOfType
//
function TEnumerationSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited DoIsOfType(typSym)
           or BaseType.DoIsOfType(typSym);
end;

// AddElement
//
procedure TEnumerationSymbol.AddElement(element : TElementSymbol);
begin
   if FContinuous and (FElements.Count>0) then
      if element.Value<>FHighBound+1 then
         FContinuous:=False;
   if element.Value<FLowBound then
      FLowBound:=element.Value;
   if element.Value>FHighBound then
      FHighBound:=element.Value;
   FElements.AddSymbol(element);
   element.FEnumeration:=Self;
end;

// ElementByValue
//
function TEnumerationSymbol.ElementByValue(const value : Int64) : TElementSymbol;
var
   i : Integer;
begin
   if (value>=FLowBound) and (value<=FHighBound) then begin
      if Continuous then begin
         Result:=TElementSymbol(Elements[value-FLowBound]);
         Exit;
      end else begin
         for i:=0 to Elements.Count-1 do begin
            Result:=TElementSymbol(Elements[i]);
            if Result.Value=value then Exit;
         end;
      end;
   end;
   Result:=nil;
end;

// GetCaption
//
function TEnumerationSymbol.GetCaption: UnicodeString;
begin
   Result:=Name;
end;

// GetDescription
//
function TEnumerationSymbol.GetDescription: UnicodeString;
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
function TEnumerationSymbol.ShortDescription : UnicodeString;
begin
   case FElements.Count of
      0 : Result:=' ';
      1 : Result:=FElements[0].GetDescription;
   else
      Result:=FElements[0].Name+',...';
   end;
   Result:='('+Result+')';
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

// IsPointerType
//
function TAliasSymbol.IsPointerType : Boolean;
begin
   Result:=Typ.IsPointerType;
end;

// DoIsOfType
//
function TAliasSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=Typ.DoIsOfType(typSym);
end;

// GetAsFuncSymbol
//
function TAliasSymbol.GetAsFuncSymbol : TFuncSymbol;
begin
   Result:=Typ.GetAsFuncSymbol;
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

// UnAliasedTypeIs
//
function TTypeSymbol.UnAliasedTypeIs(const typeSymbolClass : TTypeSymbolClass) : Boolean;
begin
   Result:=(Self<>nil) and UnAliasedType.InheritsFrom(typeSymbolClass);
end;

// IsOfType
//
function TTypeSymbol.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   if Self=nil then
      Result:=(typSym=nil)
   else Result:=(typSym<>nil) and DoIsOfType(typSym);
end;

// DoIsOfType
//
function TTypeSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(Self=typSym.UnAliasedType);
end;

// GetIsDeprecated
//
function TTypeSymbol.GetIsDeprecated : Boolean;
begin
   Result:=(FDeprecatedMessage<>'');
end;

// IsCompatible
//
function TTypeSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=BaseType.IsCompatible(typSym.BaseType);
end;

// DistanceTo
//
function TTypeSymbol.DistanceTo(typeSym : TTypeSymbol) : Integer;
begin
   if Self=typeSym then
      Result:=0
   else if UnAliasedType=typeSym.UnAliasedType then
      Result:=1
   else if IsCompatible(typeSym) then
      Result:=2
   else Result:=3;
end;

// SameType
//
function TTypeSymbol.SameType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(Self=typSym);
end;

// HasMetaSymbol
//
function TTypeSymbol.HasMetaSymbol : Boolean;
begin
   Result:=False;
end;

// IsType
//
function TTypeSymbol.IsType : Boolean;
begin
   Result:=True;
end;

// InitData
//
procedure TTypeSymbol.InitData(const data : TData; offset : Integer);
begin
   Assert(False);
end;

// ------------------
// ------------------ TAnyTypeSymbol ------------------
// ------------------

// IsCompatible
//
function TAnyTypeSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym<>nil);
end;

// ------------------
// ------------------ EScriptError ------------------
// ------------------

// CreatePosFmt
//
constructor EScriptError.CreatePosFmt(const aScriptPos: TScriptPos; const Msg: UnicodeString; const Args: array of const);
begin
   inherited CreateFmt(msg, args);
   FScriptPos:=aScriptPos;
end;

// SetScriptPos
//
procedure EScriptError.SetScriptPos(const aPos : TScriptPos);
begin
   FScriptPos:=aPos;
end;

// ------------------
// ------------------ EScriptStopped ------------------
// ------------------

// DoRaise
//
class procedure EScriptStopped.DoRaise(exec : TdwsExecution; stoppedOn : TExprBase);
var
   e : EScriptStopped;
begin
   e:=EScriptStopped.CreatePosFmt(stoppedOn.ScriptPos, RTE_ScriptStopped, []);
   e.ScriptCallStack:=exec.GetCallStack;
   raise e;
end;

// ------------------
// ------------------ EScriptException ------------------
// ------------------

// Create
//
constructor EScriptException.Create(const msgString : UnicodeString;
      const anExceptionObj : IScriptObj; const aScriptPos: TScriptPos);
begin
   inherited Create(msgString);
   FExceptObj:=anExceptionObj;
   FScriptPos:=aScriptPos;
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
   FRandSeed:=cDefaultRandSeed xor (UInt64(System.Random($7FFFFFFF)) shl 15);
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

   procedure DoDebug(exec : TdwsExecution; expr : TExprBase);
   begin
      exec.Debugger.DoDebug(exec, expr);
      if exec.ProgramState=psRunningStopped then
         EScriptStopped.DoRaise(exec, expr);
   end;

begin
   if ProgramState=psRunningStopped then
      EScriptStopped.DoRaise(Self, expr)
   else if IsDebugging then
      DoDebug(Self, expr);
end;

// Status_Offset
//
class function TdwsExecution.Status_Offset : Integer;
{$ifdef WIN32_ASM}
asm
   mov eax, OFFSET FStatus
{$else}
begin
   Result:=0;
{$endif}
end;

// SetScriptError
//
procedure TdwsExecution.SetScriptError(expr : TExprBase);
begin
   if FLastScriptError=nil then begin
      FLastScriptError:=expr;
      FLastScriptCallStack:=GetCallStack;
   end;
end;

// ClearScriptError
//
procedure TdwsExecution.ClearScriptError;
begin
   if FLastScriptError<>nil then begin
      FLastScriptError:=nil;
      FLastScriptCallStack:=nil;
   end;
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

// Random
//
{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ELSE}
  {$UNDEF RANGEON}
{$ENDIF}
function TdwsExecution.Random : Double;
// Marsaglia, George (July 2003). "Xorshift RNGs". Journal of Statistical Software Vol. 8 (Issue  14).
const
   cScale : Double = (2.0 / $10000 / $10000 / $10000 / $10000);  // 2^-63
var
   buf : Uint64;
begin
   if FRandSeed=0 then
      buf:=cDefaultRandSeed
   else begin
      buf:=FRandSeed xor (FRandSeed shl 13);
      buf:=buf xor (buf shr 17);
      buf:=buf xor (buf shl 5);
   end;
   FRandSeed:=buf;
   Result:=(buf shr 1)*cScale;
end;
{$IFDEF RANGEON}
  {$R+}
  {$UNDEF RANGEON}
{$ENDIF}

// EnterExceptionBlock
//
procedure TdwsExecution.EnterExceptionBlock(var exceptObj : IScriptObj);
begin
   ExceptionObjectStack.Push(exceptObj);
end;

// LeaveExceptionBlock
//
procedure TdwsExecution.LeaveExceptionBlock;
begin
   ExceptionObjectStack.Peek:=Unassigned;
   ExceptionObjectStack.Pop;
end;

// SetRandSeed
//
procedure TdwsExecution.SetRandSeed(const val : UInt64);
begin
   if val=0 then
      FRandSeed:=cDefaultRandSeed
   else FRandSeed:=val;
end;

// LocalizeSymbol
//
procedure TdwsExecution.LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString);
begin
   LocalizeString(aResSymbol.Value, Result);
end;

// LocalizeString
//
procedure TdwsExecution.LocalizeString(const aString : UnicodeString; var Result : UnicodeString);
begin
   Result:=aString;
end;

// ValidateFileName
//
function TdwsExecution.ValidateFileName(const path : String) : String;
begin
   raise EScriptException.CreateFmt(RTE_UnauthorizedFilePath, [path]);
end;

// DataContext_Create
//
procedure TdwsExecution.DataContext_Create(const data : TData; addr : Integer; var result : IDataContext);
begin
   Result:=FStack.CreateDataContext(data, addr);
end;

// DataContext_CreateBase
//
procedure TdwsExecution.DataContext_CreateBase(addr : Integer; var result : IDataContext);
begin
   FStack.InitDataPtr(Result, addr);
end;

// DataContext_CreateLevel
//
procedure TdwsExecution.DataContext_CreateLevel(level, addr : Integer; var Result : IDataContext);
begin
   FStack.InitDataPtrLevel(Result, level, addr);
end;

// DataContext_Nil
//
function TdwsExecution.DataContext_Nil : IDataContext;
begin
   Result:=FStack.CreateDataContext(nil, 0);
end;

// ------------------
// ------------------ TConditionSymbol ------------------
// ------------------

// Create
//
constructor TConditionSymbol.Create(const aScriptPos: TScriptPos; const cond : IBooleanEvalable; const msg : IStringEvalable);
begin
   inherited Create('', nil);
   FScriptPos:=aScriptPos;
   FCondition:=cond;
   FMessage:=msg;
end;

// ------------------
// ------------------ TRuntimeErrorMessage ------------------
// ------------------

// AsInfo
//
function TRuntimeErrorMessage.AsInfo: UnicodeString;
begin
   Result:=Text;
   if ScriptPos.Defined then
      Result:=Result+ScriptPos.AsInfo
   else if Length(FCallStack)>0 then
      Result:=Result+' in '+FCallStack[High(FCallStack)].Expr.FuncSymQualifiedName;
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
procedure TdwsRuntimeMessageList.AddRuntimeError(const Text: UnicodeString);
begin
   AddRuntimeError(cNullPos, Text, nil);
end;

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(e : Exception);
begin
   AddRuntimeError(E.ClassName+': '+E.Message);
end;

// AddRuntimeError
//
procedure TdwsRuntimeMessageList.AddRuntimeError(const scriptPos : TScriptPos;
                     const Text: UnicodeString; const callStack : TdwsExprLocationArray);
var
   msg : TRuntimeErrorMessage;
begin
   msg:=TRuntimeErrorMessage.Create(Self, Text, scriptPos);
   msg.FCallStack:=callStack;
end;

// ------------------
// ------------------ TOperatorSymbol ------------------
// ------------------

// Create
//
constructor TOperatorSymbol.Create(const aTokenType : TTokenType);
begin
   inherited Create('operator '+cTokenStrings[aTokenType], nil);
   FToken:=aTokenType;
end;

// AddParam
//
procedure TOperatorSymbol.AddParam(p : TTypeSymbol);
var
   n : Integer;
begin
   n:=Length(FParams);
   SetLength(FParams, n+1);
   FParams[n]:=p;
end;

// GetCaption
//
function TOperatorSymbol.GetCaption : UnicodeString;
var
   i : Integer;
begin
   Result:='operator '+cTokenStrings[Token]+' (';
   for i:=0 to High(Params) do begin
      if i>0 then
         Result:=Result+', ';
      Result:=Result+Params[i].Typ.Caption;
   end;
   Result:=Result+') : '+Typ.Caption+' uses '+FUsesSym.Name;
end;

// ------------------
// ------------------ TResolvedInterfaces ------------------
// ------------------

// SameItem
//
function TResolvedInterfaces.SameItem(const item1, item2 : TResolvedInterface) : Boolean;
begin
   Result:=(item1.IntfSymbol=item2.IntfSymbol);
end;

// GetItemHashCode
//
function TResolvedInterfaces.GetItemHashCode(const item1 : TResolvedInterface) : Integer;
begin
   Result:=(NativeInt(item1.IntfSymbol) shr 4);
end;

// ------------------
// ------------------ TAnyFuncSymbol ------------------
// ------------------

// IsCompatible
//
function TAnyFuncSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym.AsFuncSymbol<>nil);
end;

// ------------------
// ------------------ TResourceStringSymbol ------------------
// ------------------

// Create
//
constructor TResourceStringSymbol.Create(const aName, aValue : UnicodeString);
begin
   inherited Create(aName, nil);
   FValue:=aValue;
   FIndex:=-1;
end;

// GetCaption
//
function TResourceStringSymbol.GetCaption : UnicodeString;
begin
   Result:='resourcestring '+Name;
end;

// GetDescription
//
function TResourceStringSymbol.GetDescription : UnicodeString;
begin
   Result:=Value;
   FastStringReplace(Result, '''', '''''');
   Result:='resourcestring '+Name+' = '''+Result+'''';
end;

// ------------------
// ------------------ TResourceStringSymbolList ------------------
// ------------------

// ComputeIndexes
//
procedure TResourceStringSymbolList.ComputeIndexes;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Index:=i;
end;

// ------------------
// ------------------ TFuncSymbolList ------------------
// ------------------

// ContainsChildMethodOf
//
function TFuncSymbolList.ContainsChildMethodOf(methSym : TMethodSymbol) : Boolean;
var
   i : Integer;
   funcSym : TFuncSymbol;
   meth : TMethodSymbol;
begin
   for i:=0 to Count-1 do begin
      funcSym:=Items[i];
      if funcSym is TMethodSymbol then begin
         meth:=TMethodSymbol(funcSym);
         repeat
            if meth=methSym then Exit(True);
            meth:=meth.ParentMeth;
         until meth=nil;
      end;
   end;
   Result:=False;
end;

// ------------------
// ------------------ THelperSymbol ------------------
// ------------------

// Create
//
constructor THelperSymbol.Create(const name : UnicodeString; aUnit : TSymbol;
                                 aForType : TTypeSymbol; priority : Integer);
begin
   inherited Create(name, aUnit);
   FForType:=aForType;
   FUnAliasedForType:=aForType.UnAliasedType;
   if FUnAliasedForType is TStructuredTypeSymbol then
      FMetaForType:=TStructuredTypeSymbol(FUnAliasedForType).MetaSymbol;
end;

// IsCompatible
//
function THelperSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   Result:=(typSym=Self);
end;

// IsType
//
function THelperSymbol.IsType : Boolean;
begin
   Result:=False;
end;

// AllowDefaultProperty
//
function THelperSymbol.AllowDefaultProperty : Boolean;
begin
   Result:=False;
end;

// CreateSelfParameter
//
function THelperSymbol.CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol;
var
   meta : TStructuredTypeMetaSymbol;
begin
   if methSym.IsClassMethod then begin
      if ForType is TStructuredTypeSymbol then begin
         meta:=TStructuredTypeSymbol(ForType).MetaSymbol;
         if meta<>nil then
            Result:=TParamSymbol.Create(SYS_SELF, meta)
         else Result:=nil;
      end else Result:=nil
   end else begin
      if    (ForType is TClassSymbol) or (ForType is TInterfaceSymbol)
         or (ForType is TDynamicArraySymbol) then
         Result:=TParamSymbol.Create(SYS_SELF, ForType)
      else Result:=TConstParamSymbol.Create(SYS_SELF, ForType);
   end;
   if Result<>nil then begin
      methSym.Params.AddSymbol(Result);
      if Result.Typ is TCompositeTypeSymbol then
         methSym.Params.AddParent(TCompositeTypeSymbol(Result.Typ).Members)
      else if Result.Typ is TStructuredTypeMetaSymbol then
         methSym.Params.AddParent(TStructuredTypeMetaSymbol(Result.Typ).StructSymbol.Members)
   end;
end;

// CreateAnonymousMethod
//
function THelperSymbol.CreateAnonymousMethod(aFuncKind : TFuncKind;
                                             aVisibility : TdwsVisibility;
                                             isClassMethod : Boolean) : TMethodSymbol;
begin
   Result:=TSourceMethodSymbol.Create('', aFuncKind, Self, aVisibility, isClassMethod);
   if isClassMethod and (not ForType.HasMetaSymbol) then
      TSourceMethodSymbol(Result).SetIsStatic;
end;

// Initialize
//
procedure THelperSymbol.Initialize(const msgs : TdwsCompileMessageList);
begin
   CheckMethodsImplemented(msgs);
end;

// HelpsType
//
function THelperSymbol.HelpsType(typ : TTypeSymbol) : Boolean;
begin
   if typ=nil then
      Result:=False
   else if typ=ForType then
      Result:=True
   else if typ.IsOfType(FUnAliasedForType) then
      Result:=True
   else if FMetaForType<>nil then
      Result:=typ.IsOfType(FMetaForType)
   else Result:=False;
end;

// ------------------
// ------------------ THelperSymbols ------------------
// ------------------

// AddHelper
//
function THelperSymbols.AddHelper(helper : THelperSymbol) : Boolean;
begin
   Add(helper);
   Result:=False;
end;

// ------------------
// ------------------ TAliasMethodSymbol ------------------
// ------------------

// GetSourcePosition
//
function TAliasMethodSymbol.GetSourcePosition : TScriptPos;
begin
   Result:=Alias.GetSourcePosition;
end;

// IsPointerType
//
function TAliasMethodSymbol.IsPointerType : Boolean;
begin
   Result:=Alias.IsPointerType;
end;

// ParamsDescription
//
function TAliasMethodSymbol.ParamsDescription : UnicodeString;
var
   i : Integer;
begin
   if Params.Count>1 then begin
      Result:=Params.Symbols[1].Description;
      for i:=2 to Params.Count-1 do
         Result:=Result+'; '+Params.Symbols[i].Description;
      Result:='('+Result+')';
  end else Result:='()';
end;

// ------------------
// ------------------ TPerfectMatchEnumerator ------------------
// ------------------

// Callback
//
function TPerfectMatchEnumerator.Callback(sym : TSymbol) : Boolean;
var
   locSym : TFuncSymbol;
begin
   locSym:=sym.AsFuncSymbol;
   if locSym<>nil then begin
      if locSym.Level=FuncSym.Level then begin
         if FuncSym.IsSameOverloadOf(locSym) then begin
            Match:=locSym;
            Exit(True);
         end;
      end;
   end;
   Result:=False;
end;


end.
