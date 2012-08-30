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

uses SysUtils, Variants, Classes, dwsStrings, dwsErrors, dwsUtils,
   dwsTokenizer, dwsStack;

type

   IScriptObj = interface;
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
   TdwsRuntimeMessageList = class;

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
   end;

   TProgramState = (psUndefined, psReadyToRun, psRunning, psRunningStopped, psTerminated);

   IdwsExecution = interface (IGetSelf)
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

   TExprBaseEnumeratorProc = reference to procedure (parent, expr : TExprBase; var abort : Boolean);

   // Base class for all Exprs
   TExprBase = class (TRefCountedObject)
      protected
         function GetSubExpr(i : Integer) : TExprBase; virtual;
         function GetSubExprCount : Integer; virtual;

      public
         function  Eval(exec : TdwsExecution) : Variant; virtual; abstract;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; virtual; abstract;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; virtual; abstract;
         function  EvalAsFloat(exec : TdwsExecution) : Double; virtual; abstract;
         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString); overload; virtual; abstract;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); overload; virtual; abstract;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); virtual; abstract;

         procedure AssignValue(exec : TdwsExecution; const value : Variant); virtual; abstract;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); virtual; abstract;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); virtual; abstract;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); virtual; abstract;
         procedure AssignValueAsString(exec : TdwsExecution; const value : UnicodeString); virtual; abstract;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); virtual; abstract;

         property SubExpr[i : Integer] : TExprBase read GetSubExpr;
         property SubExprCount : Integer read GetSubExprCount;

         function ScriptPos : TScriptPos; virtual; abstract;
         function ScriptLocation(prog : TObject) : UnicodeString; virtual; abstract;

         class function CallStackToString(const callStack : TdwsExprLocationArray) : UnicodeString; static;

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
         procedure Clear;

         function Add(expr : TExprBase) : Integer; inline;
         procedure Insert(idx : Integer;expr : TExprBase); inline;
         procedure Delete(index : Integer);
         procedure Assign(const src : TExprBaseListRec);

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
         function GetAsString(const x : Integer) : UnicodeString;
         procedure SetAsString(const x : Integer; const value : UnicodeString);
         function GetAsDataString(const x : Integer) : RawByteString;

      public
         property List : PExprBaseListRec read FList write FList;
         property Exec : TdwsExecution read FExec write FExec;

         property ExprBase[const x : Integer] : TExprBase read GetExprBase write SetExprBase; default;
         property Count : Integer read GetCount;

         property AsInteger[const x : Integer] : Int64 read GetAsInteger write SetAsInteger;
         property AsBoolean[const x : Integer] : Boolean read GetAsBoolean write SetAsBoolean;
         property AsFloat[const x : Integer] : Double read GetAsFloat write SetAsFloat;
         property AsString[const x : Integer] : UnicodeString read GetAsString write SetAsString;
         property AsDataString[const x : Integer] : RawByteString read GetAsDataString;
   end;

   // All functions callable from the script implement this interface
   IExecutable = interface (IGetSelf)
      ['{8D534D18-4C6B-11D5-8DCB-0000216D9E86}']
      procedure InitSymbol(symbol : TSymbol);
      procedure InitExpression(expr : TExprBase);
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

   TdwsVisibility = (cvMagic, cvPrivate, cvProtected, cvPublic, cvPublished);
   TdwsVisibilities = set of TdwsVisibility;

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

      public
         constructor Create(const aName : UnicodeString; aType : TTypeSymbol);

         procedure Initialize(const msgs : TdwsCompileMessageList); virtual;
         function  BaseType : TTypeSymbol; virtual;
         procedure SetName(const newName : UnicodeString);

         class function IsBaseType : Boolean; virtual;
         function IsType : Boolean; virtual;

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
   TSymbolEnumerationCallback = reference to function (symbol : TSymbol) : Boolean;

   THelperSymbolEnumerationCallback = function (helper : THelperSymbol) : Boolean of object;

   TOperatorSymbolEnumerationCallback = function (opSym : TOperatorSymbol) : Boolean of object;

   TSymbolTableFlag = (stfSorted, stfHasHelpers, stfHasOperators);
   TSymbolTableFlags = set of TSymbolTableFlag;

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

         function FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility;
                             ofClass : TSymbolClass = nil) : TSymbol; virtual;
         function FindTypeSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility) : TTypeSymbol;

         // returns True if aborted
         function EnumerateLocalSymbolsOfName(const aName : UnicodeString; const callback : TSymbolEnumerationCallback) : Boolean; virtual;
         function EnumerateSymbolsOfNameInScope(const aName : UnicodeString; const callback : TSymbolEnumerationCallback) : Boolean; virtual;

         function EnumerateLocalHelpers(helpedType : TTypeSymbol; const callback : THelperSymbolEnumerationCallback) : Boolean; virtual;
         function EnumerateHelpers(helpedType : TTypeSymbol; const callback : THelperSymbolEnumerationCallback) : Boolean; virtual;

         function EnumerateLocalOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                             const callback : TOperatorSymbolEnumerationCallback) : Boolean; virtual;
         function EnumerateOperatorsFor(aToken : TTokenType; aLeftType, aRightType : TTypeSymbol;
                                        const callback : TOperatorSymbolEnumerationCallback) : Boolean; virtual;
         function HasSameLocalOperator(anOpSym : TOperatorSymbol) : Boolean; virtual;

         procedure CollectPropertyAttributes(tableList : TSimpleObjectHash<TSymbolTable>;
                                             propertyList : TSimpleList<TPropertySymbol>);

         function HasClass(const aClass : TSymbolClass) : Boolean;
         function HasSymbol(sym : TSymbol) : Boolean;
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
         constructor Create(const name : UnicodeString; typ : TTypeSymbol; const value : Variant); overload;
         constructor Create(const name : UnicodeString; typ : TTypeSymbol; const data : TData; addr: Integer); overload;

         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         property Data : TData read FData;
   end;
   TConstSymbolClass = class of TConstSymbol;

   // variable: var x: Integer;
   TDataSymbol = class (TValueSymbol)
      protected
         FStackAddr : Integer;
         FLevel : SmallInt;

         function GetDescription : UnicodeString; override;

      public
         procedure AllocateStackAddr(generator : TAddrGenerator);

         property Level : SmallInt read FLevel write FLevel;
         property StackAddr: Integer read FStackAddr write FStackAddr;
   end;

   // parameter: procedure P(x: Integer);
   TParamSymbol = class (TDataSymbol)
      public
         function SameParam(other : TParamSymbol) : Boolean; virtual;
   end;

   THasParamSymbolMethod = function (param : TParamSymbol) : Boolean of object;
   TAddParamSymbolMethod = procedure (param : TParamSymbol) of object;

   TParamSymbolWithDefaultValue = class(TParamSymbol)
      private
         FDefaultValue : TData;

      protected
         function GetDescription : UnicodeString; override;

      public
         constructor Create(const aName : UnicodeString; aType : TTypeSymbol;
                            const data : TData; addr : Integer);

         function SameParam(other : TParamSymbol) : Boolean; override;

         property DefaultValue : TData read FDefaultValue;
   end;

   // const/var parameter: procedure P(const/var x: Integer)
   TByRefParamSymbol = class(TParamSymbol)
      public
         constructor Create(const Name: UnicodeString; Typ: TTypeSymbol);
   end;

   // lazy parameter: procedure P(lazy x: Integer)
   TLazyParamSymbol = class sealed (TParamSymbol)
      protected
         function GetDescription : UnicodeString; override;
   end;

   // const parameter: procedure P(const x: Integer)
   TConstParamSymbol = class sealed (TByRefParamSymbol)
      protected
         function GetDescription : UnicodeString; override;
   end;

   // var parameter: procedure P(var x: Integer)
   TVarParamSymbol = class sealed (TByRefParamSymbol)
      protected
         function GetDescription : UnicodeString; override;
   end;

   TTypeSymbolClass = class of TTypeSymbol;
   TTypeSymbols = array of TTypeSymbol;

   // Base class for all types
   TTypeSymbol = class(TSymbol)
      protected
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; virtual;

      public
         procedure InitData(const data : TData; offset : Integer); virtual;
         function IsType : Boolean; override;
         function BaseType : TTypeSymbol; override;
         function UnAliasedType : TTypeSymbol; virtual;
         function IsOfType(typSym : TTypeSymbol) : Boolean;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; virtual;
   end;

   TAnyTypeSymbol = class(TTypeSymbol)
      public
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   TFuncKind = (fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod);

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

   TResultSymbol = class(TDataSymbol)
   end;

   TFuncSymbolFlag = (fsfStateless, fsfExternal, fsfType, fsfOverloaded);
   TFuncSymbolFlags = set of TFuncSymbolFlag;

   // A script function / procedure: procedure X(param: Integer);
   TFuncSymbol = class (TTypeSymbol)
      protected
         FAddrGenerator : TAddrGeneratorRec;
         FExecutable : IExecutable;
         FInternalParams : TSymbolTable;
         FDeprecatedMessage : UnicodeString;
         FForwardPosition : PScriptPos;
         FParams : TParamsSymbolTable;
         FResult : TDataSymbol;
         FConditions : TConditionsSymbolTable;
         FFlags : TFuncSymbolFlags;
         FKind : TFuncKind;
         FExternalName : String;

         procedure SetType(const Value: TTypeSymbol);
         function GetCaption : UnicodeString; override;
         function GetIsForwarded : Boolean;
         function GetDescription : UnicodeString; override;
         function GetLevel : SmallInt; inline;
         function GetParamSize : Integer; inline;
         function GetIsDeprecated : Boolean; inline;
         function GetIsStateless : Boolean; inline;
         procedure SetIsStateless(const val : Boolean);
         function GetIsExternal : Boolean; inline;
         procedure SetIsExternal(const val : Boolean);
         function GetIsOverloaded : Boolean; inline;
         procedure SetIsOverloaded(const val : Boolean);
         function GetSourcePosition : TScriptPos; virtual;
         procedure SetSourcePosition(const val : TScriptPos); virtual;
         function GetExternalName : String;
         procedure SetExternalName(const val : String);

         function GetSourceSubExpr(i : Integer) : TExprBase;
         function GetSourceSubExprCount : Integer;
         property SubExpr[i : Integer] : TExprBase read GetSourceSubExpr;
         property SubExprCount : Integer read GetSourceSubExprCount;

         function  DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; funcKind : TFuncKind; funcLevel : SmallInt);
         destructor Destroy; override;

         constructor Generate(table : TSymbolTable; const funcName : UnicodeString;
                              const funcParams : TParamArray; const funcType : UnicodeString);
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;
         function  IsType : Boolean; override;
         procedure SetIsType;
         procedure AddParam(param : TParamSymbol);
         function  HasParam(param : TParamSymbol) : Boolean;
         procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
         function  GetParamType(idx : Integer) : TTypeSymbol;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         procedure InitData(const data : TData; offset : Integer); override;
         procedure AddCondition(cond : TConditionSymbol);

         function  IsValidOverloadOf(other : TFuncSymbol) : Boolean;
         function  IsSameOverloadOf(other : TFuncSymbol) : Boolean; virtual;

         function  ParamsDescription : UnicodeString;

         procedure SetForwardedPos(const pos : TScriptPos);
         procedure ClearIsForwarded;

         property Executable : IExecutable read FExecutable write FExecutable;
         property DeprecatedMessage : UnicodeString read FDeprecatedMessage write FDeprecatedMessage;
         property IsDeprecated : Boolean read GetIsDeprecated;
         property IsStateless : Boolean read GetIsStateless write SetIsStateless;
         property IsForwarded : Boolean read GetIsForwarded;
         property IsOverloaded : Boolean read GetIsOverloaded write SetIsOverloaded;
         property IsExternal : Boolean read GetIsExternal write SetIsExternal;
         property Kind : TFuncKind read FKind write FKind;
         property ExternalName : String read GetExternalName write SetExternalName;
         property Level : SmallInt read GetLevel;
         property InternalParams : TSymbolTable read FInternalParams;
         property Params : TParamsSymbolTable read FParams;
         property ParamSize : Integer read GetParamSize;
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
   TMethodSymbol = class(TFuncSymbol)
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
                              Cls: TCompositeTypeSymbol; aVisibility : TdwsVisibility);

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
         destructor Destroy; override;

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

      public
         function BaseType : TTypeSymbol; override;
         function UnAliasedType : TTypeSymbol; override;
         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
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
   end;

   IConnectorType = interface;

   IConnector = interface
      ['{8D534D1A-4C6B-11D5-8DCB-0000216D9E86}']
      function ConnectorCaption: UnicodeString;
      function ConnectorName: UnicodeString;
      function GetUnit(const UnitName: UnicodeString): IConnectorType;
   end;

   TConnectorArgs = array of TData;

   IConnectorCall = interface (IGetSelf)
      ['{8D534D1B-4C6B-11D5-8DCB-0000216D9E86}']
      function Call(const base : Variant; args : TConnectorArgs) : TData;
      function NeedDirectReference : Boolean;
   end;

   IConnectorMember = interface (IGetSelf)
      ['{8D534D1C-4C6B-11D5-8DCB-0000216D9E86}']
      function Read(const base : Variant) : TData;
      procedure Write(const base : Variant; const data : TData);
   end;

   TConnectorParam = record
      IsVarParam : Boolean;
      TypSym : TTypeSymbol;
   end;

   TConnectorParamArray = array of TConnectorParam;

   IConnectorType = interface
     ['{8D534D1D-4C6B-11D5-8DCB-0000216D9E86}']
     function ConnectorCaption: UnicodeString;
     function AcceptsParams(const params: TConnectorParamArray) : Boolean;
     function HasMethod(const MethodName: UnicodeString; const Params: TConnectorParamArray;
                        var TypSym: TTypeSymbol): IConnectorCall;
     function HasMember(const MemberName: UnicodeString; var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorMember;
     function HasIndex(const PropName: UnicodeString; const Params: TConnectorParamArray;
                       var TypSym: TTypeSymbol; IsWrite: Boolean): IConnectorCall;
   end;

   TConnectorSymbol = class(TBaseVariantSymbol)
      private
         FConnectorType : IConnectorType;

      protected
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; const connectorType : IConnectorType);

         function Specialize(table : TSymbolTable; const qualifier : UnicodeString) : TConnectorSymbol; virtual;

         property ConnectorType : IConnectorType read FConnectorType write FConnectorType;
   end;

   TArraySymbol = class abstract(TTypeSymbol)
      private
         FIndexType : TTypeSymbol;

      protected
         function ElementSize : Integer;

      public
         constructor Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);

         property IndexType : TTypeSymbol read FIndexType write FIndexType;
   end;

   // array of FTyp
   TDynamicArraySymbol = class sealed (TArraySymbol)
      protected
         function GetCaption : UnicodeString; override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
         procedure InitData(const Data: TData; Offset: Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
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
         procedure AddElement;

         property HighBound : Integer read FHighBound;
         property LowBound : Integer read FLowBound;
         property ElementCount : Integer read FElementCount;
   end;

   // static array whose bounds are contextual
   TOpenArraySymbol = class (TStaticArraySymbol)
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

      protected
         function CreateMembersTable : TMembersSymbolTable; virtual;

         function GetIsStatic : Boolean; virtual;
         function GetIsExternal : Boolean; virtual;
         function GetExternalName : String; virtual;

         procedure CheckMethodsImplemented(const msgs : TdwsCompileMessageList);

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);
         destructor Destroy; override;

         procedure AddConst(sym : TClassConstSymbol); overload;
         procedure AddConst(sym : TClassConstSymbol; visibility : TdwsVisibility); overload;
         procedure AddClassVar(sym : TClassVarSymbol);
         procedure AddProperty(propSym : TPropertySymbol);
         procedure AddMethod(methSym : TMethodSymbol); virtual;

         function AllowVirtualMembers : Boolean; virtual;
         function AllowDefaultProperty : Boolean; virtual; abstract;

         function FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol; virtual;

         function MembersVisibilities : TdwsVisibilities;

         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; virtual; abstract;

         property UnitSymbol : TSymbol read FUnitSymbol;
         property Parent : TCompositeTypeSymbol read FParent;
         property Members : TMembersSymbolTable read FMembers;
         property DefaultProperty : TPropertySymbol read FDefaultProperty write FDefaultProperty;

         property IsStatic : Boolean read GetIsStatic;
         property IsExternal : Boolean read GetIsExternal;
         property ExternalName : String read GetExternalName;
   end;

   // class, record, interface
   TStructuredTypeSymbol = class(TCompositeTypeSymbol)
      private
         FMetaSymbol : TStructuredTypeMetaSymbol;
         FForwardPosition : PScriptPos;
         FExternalName : String;

      protected
         function GetIsForwarded : Boolean; inline;
         function GetIsExternal : Boolean; override;
         function GetExternalName : String; override;

         procedure DoInheritFrom(ancestor : TStructuredTypeSymbol);

      public
         destructor Destroy; override;

         procedure AddField(fieldSym : TFieldSymbol); virtual;

         function FieldAtOffset(offset : Integer) : TFieldSymbol; virtual;
         function DuckTypedMatchingMethod(methSym : TMethodSymbol; visibility : TdwsVisibility) : TMethodSymbol; virtual;

         function NthParentOf(structType : TCompositeTypeSymbol) : Integer;
         function FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol; override;
         function AllowDefaultProperty : Boolean; override;

         procedure SetForwardedPos(const pos : TScriptPos);
         procedure ClearIsForwarded;

         property IsForwarded : Boolean read GetIsForwarded;
         property ExternalName : String read GetExternalName write FExternalName;

         property MetaSymbol : TStructuredTypeMetaSymbol read FMetaSymbol;
   end;

   // class of, record of
   TStructuredTypeMetaSymbol = class(TTypeSymbol)
      public
         constructor Create(const name : UnicodeString; typ : TStructuredTypeSymbol);

         procedure InitData(const Data: TData; Offset: Integer); override;

         function StructSymbol : TStructuredTypeSymbol; inline;
   end;

   // field of a script object
   TFieldSymbol = class sealed (TValueSymbol)
      protected
         FStructSymbol : TStructuredTypeSymbol;
         FOffset : Integer;
         FVisibility : TdwsVisibility;
         FDefaultValue : TData;
         FDefaultExpr : TExprBase;
         FExternalName : String;

         function GetExternalName : String;

      public
         constructor Create(const name : UnicodeString; typ : TTypeSymbol;
                            aVisibility : TdwsVisibility);
         destructor Destroy; override;

         function QualifiedName : UnicodeString; override;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;

         procedure InitData(const data : TData; structOffset : Integer);

         property StructSymbol : TStructuredTypeSymbol read FStructSymbol;
         property Offset : Integer read FOffset;
         property Visibility : TdwsVisibility read FVisibility write FVisibility;
         property DefaultValue : TData read FDefaultValue write FDefaultValue;
         property DefaultExpr : TExprBase read FDefaultExpr write FDefaultExpr;
         property ExternalName : String read GetExternalName write FExternalName;
   end;

   // record member1: Integer; member2: Integer end;
   TRecordSymbol = class sealed (TStructuredTypeSymbol)
      private
         FIsDynamic : Boolean;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);

         procedure AddField(fieldSym : TFieldSymbol); override;
         procedure AddMethod(methSym : TMethodSymbol); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;

         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;

         procedure InitData(const data : TData; offset : Integer); override;
         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         property IsDynamic : Boolean read FIsDynamic write FIsDynamic;
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
         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;

         function Parent : TInterfaceSymbol; inline;
         property MethodCount : Integer read FMethodCount;
   end;

   // property X: Integer read FReadSym write FWriteSym;
   TPropertySymbol = class sealed (TValueSymbol)
      private
         FOwnerSymbol : TCompositeTypeSymbol;
         FReadSym : TSymbol;
         FWriteSym : TSymbol;
         FArrayIndices : TSymbolTable;
         FIndexSym : TTypeSymbol;
         FIndexValue: TData;
         FVisibility : TdwsVisibility;
         FDeprecatedMessage : UnicodeString;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;
         function GetIsDefault: Boolean;
         function GetArrayIndices : TSymbolTable;
         procedure AddParam(Param : TParamSymbol);
         function GetIsDeprecated : Boolean; inline;

      public
         constructor Create(const name : UnicodeString; typ : TTypeSymbol; aVisibility : TdwsVisibility);
         destructor Destroy; override;

         procedure GenerateParams(Table: TSymbolTable; const FuncParams: TParamArray);
         procedure SetIndex(const Data: TData; Addr: Integer; Sym: TTypeSymbol);
         function GetArrayIndicesDescription: UnicodeString;
         function QualifiedName : UnicodeString; override;
         function IsVisibleFor(const aVisibility : TdwsVisibility) : Boolean; override;
         function HasArrayIndices : Boolean;

         property OwnerSymbol : TCompositeTypeSymbol read FOwnerSymbol;
         property Visibility : TdwsVisibility read FVisibility write FVisibility;
         property ArrayIndices : TSymbolTable read GetArrayIndices;
         property ReadSym : TSymbol read FReadSym write FReadSym;
         property WriteSym : TSymbol read FWriteSym write FWriteSym;
         property IsDefault : Boolean read GetIsDefault;
         property IndexValue : TData read FIndexValue;
         property IndexSym : TTypeSymbol read FIndexSym;
         property DeprecatedMessage : UnicodeString read FDeprecatedMessage write FDeprecatedMessage;
         property IsDeprecated : Boolean read GetIsDeprecated;
   end;

   // class operator X (params) uses method;
   TClassOperatorSymbol = class sealed (TSymbol)
      private
         FClassSymbol : TClassSymbol;
         FTokenType : TTokenType;
         FUsesSym : TMethodSymbol;

      protected
         function GetCaption : UnicodeString; override;
         function GetDescription : UnicodeString; override;

      public
         constructor Create(tokenType : TTokenType);
         function QualifiedName : UnicodeString; override;

         property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
         property TokenType : TTokenType read FTokenType write FTokenType;
         property UsesSym : TMethodSymbol read FUsesSym write FUsesSym;
   end;

   // type X = class of TMyClass;
   TClassOfSymbol = class sealed (TStructuredTypeMetaSymbol)
      protected
         function GetCaption : UnicodeString; override;
         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; typ : TClassSymbol);

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
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
                       csfStatic, csfExternal, csfPartial);
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
         function GetIsPartial : Boolean; inline;

         function AllocateVMTindex : Integer;

         function DoIsOfType(typSym : TTypeSymbol) : Boolean; override;

      public
         constructor Create(const name : UnicodeString; aUnit : TSymbol);
         destructor Destroy; override;

         procedure AddField(fieldSym : TFieldSymbol); override;
         procedure AddMethod(methSym : TMethodSymbol); override;
         procedure AddOperator(Sym: TClassOperatorSymbol);

         function  AddInterface(intfSym : TInterfaceSymbol; visibility : TdwsVisibility;
                                var missingMethod : TMethodSymbol) : Boolean; // True if added
         function  AddOverriddenInterface(const ancestorResolved : TResolvedInterface) : Boolean; // True if added
         procedure AddOverriddenInterfaces;
         function  ResolveInterface(intfSym : TInterfaceSymbol; var resolved : TResolvedInterface) : Boolean;
         function  ImplementsInterface(intfSym : TInterfaceSymbol) : Boolean;
         procedure SetIsPartial; inline;

         function  FieldAtOffset(offset : Integer) : TFieldSymbol; override;
         procedure InheritFrom(ancestorClassSym : TClassSymbol);
         procedure InitData(const Data: TData; Offset: Integer); override;
         procedure Initialize(const msgs : TdwsCompileMessageList); override;
         function  IsCompatible(typSym : TTypeSymbol) : Boolean; override;

         function VMTMethod(index : Integer) : TMethodSymbol;
         function VMTCount : Integer;

         function FindClassOperatorStrict(tokenType : TTokenType; paramType : TSymbol; recursive : Boolean) : TClassOperatorSymbol;
         function FindClassOperator(tokenType : TTokenType; paramType : TTypeSymbol) : TClassOperatorSymbol;

         function FindDefaultConstructor(minVisibility : TdwsVisibility) : TMethodSymbol; override;
         function AllowVirtualMembers : Boolean; override;
         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;

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

         function IsType : Boolean; override;
         function AllowDefaultProperty : Boolean; override;
         function CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol; override;

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
   TNilSymbol = class(TTypeSymbol)
      protected
         function GetCaption : UnicodeString; override;

      public
         constructor Create;

         function IsCompatible(typSym : TTypeSymbol) : Boolean; override;
   end;

   // Element of an enumeration type. E. g. "type DummyEnum = (Elem1, Elem2, Elem3);"
   TElementSymbol = class (TConstSymbol)
      private
         FIsUserDef : Boolean;

      protected
         function GetDescription : UnicodeString; override;
         function GetValue : Int64; inline;

      public
         constructor Create(const Name: UnicodeString; Typ: TTypeSymbol;
                            const aValue : Int64; isUserDef: Boolean);
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

         procedure AddElement(element : TElementSymbol);
         function ElementByValue(const value : Int64) : TElementSymbol;

         property Elements : TSymbolTable read FElements;
         property Style : TEnumerationSymbolStyle read FStyle;
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
         FContextTable : TSymbolTable;
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

         procedure SetScriptError(expr : TExprBase);
         procedure ClearScriptError;

         function GetCallStack : TdwsExprLocationArray; virtual; abstract;
         function CallStackDepth : Integer; virtual; abstract;

         procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString); virtual;
         procedure LocalizeString(const aString : UnicodeString; var Result : UnicodeString); virtual;

         function Random : Double;

         property LastScriptError : TExprBase read FLastScriptError;
         property LastScriptCallStack : TdwsExprLocationArray read FLastScriptCallStack;
         property ExceptionObjectStack : TSimpleStack<Variant> read FExceptionObjectStack;

         property ProgramState : TProgramState read FProgramState;

         property ContextTable : TSymbolTable read FContextTable write FContextTable;
         property Debugger : IDebugger read FDebugger write SetDebugger;
         property IsDebugging : Boolean read FIsDebugging;

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
   IScriptObj = interface
      ['{8D534D1E-4C6B-11D5-8DCB-0000216D9E86}']
      function GetClassSym: TClassSymbol;
      function GetData: TData;
      function GetInternalObject: TObject;
      function GetExternalObject: TObject;
      procedure SetExternalObject(value: TObject);
      function GetDestroyed : Boolean;
      procedure SetDestroyed(const val : Boolean);

      property ClassSym : TClassSymbol read GetClassSym;
      property Data : TData read GetData;
      property InternalObject : TObject read GetInternalObject;
      property ExternalObject : TObject read GetExternalObject write SetExternalObject;
      property Destroyed : Boolean read GetDestroyed write SetDestroyed;

      function DataOfAddr(addr : Integer) : Variant;
      function DataOfAddrAsString(addr : Integer) : UnicodeString;
      function DataOfAddrAsInteger(addr : Integer) : Int64;
      procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
   end;

   // The script has to be stopped because of an error
   EScriptError = class(Exception)
      private
         FScriptPos : TScriptPos;
         FScriptCallStack : TdwsExprLocationArray;
         FRawClassName : UnicodeString;

      public
         constructor CreatePosFmt(const pos : TScriptPos; const Msg: UnicodeString; const Args: array of const);

         property ScriptPos : TScriptPos read FScriptPos write FScriptPos;
         property ScriptCallStack : TdwsExprLocationArray read FScriptCallStack write FScriptCallStack;
         property RawClassName : UnicodeString read FRawClassName write FRawClassName;
   end;
   EScriptErrorClass = class of EScriptError;

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
         property ScriptPos : TScriptPos read FScriptPos;
         property ScriptCallStack : TdwsExprLocationArray read FScriptCallStack;
   end;

   // Is thrown by failed Assert() statements in script code
   EScriptAssertionFailed = class(EScriptException)
   end;

const
   cFuncKindToString : array [Low(TFuncKind)..High(TFuncKind)] of UnicodeString = (
      'function', 'procedure', 'constructor', 'destructor', 'method' );

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsExprs;

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
   Result:=Expr.ScriptPos.SourceFile.Name;
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
procedure TExprBase.RecursiveEnumerateSubExprs(const callback : TExprBaseEnumeratorProc);
var
   i : Integer;
   abort : Boolean;
   base, expr : TExprBase;
   stack : TSimpleStack<TExprBase>;
begin
   if Self=nil then Exit;
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

// Clear
//
procedure TExprBaseListRec.Clear;
begin
   FList.Clear;
end;

// Add
//
function TExprBaseListRec.Add(expr : TExprBase) : Integer;
begin
   Result:=FList.Add(expr);
end;

// Insert
//
procedure TExprBaseListRec.Insert(idx : Integer;expr : TExprBase);
begin
   FList.Insert(0, expr);
end;

// Delete
//
procedure TExprBaseListRec.Delete(index : Integer);
begin
   FList.Delete(index);
end;

// Assign
//
procedure TExprBaseListRec.Assign(const src : TExprBaseListRec);
begin
   FList.Assign(src.FList);
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
function TExprBaseListExec.GetAsString(const x : Integer) : UnicodeString;
begin
   ExprBase[x].EvalAsString(Exec, Result);
end;

// SetAsString
//
procedure TExprBaseListExec.SetAsString(const x : Integer; const value : UnicodeString);
begin
   ExprBase[x].AssignValueAsString(Exec, value);
end;

// GetAsDataString
//
function TExprBaseListExec.GetAsDataString(const x : Integer) : RawByteString;
var
   ustr : UnicodeString;
   i, n : Integer;
   pSrc : PWideChar;
   pDest : PByteArray;
begin
   ustr:=GetAsString(x);
   if ustr='' then Exit('');
   n:=Length(ustr);
   SetLength(Result, n);
   pSrc:=PWideChar(Pointer(ustr));
   pDest:=PByteArray(Pointer(Result));
   for i:=0 to n-1 do
      pDest[i]:=PByte(@pSrc[i])^;
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

// AllowVirtualMembers
//
function TCompositeTypeSymbol.AllowVirtualMembers : Boolean;
begin
   Result:=False;
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

// GetExternalName
//
function TCompositeTypeSymbol.GetExternalName : String;
begin
   Result:=Name;
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
begin
   for i:=0 to FMembers.Count-1 do begin
      if FMembers[i] is TMethodSymbol then begin
         methSym:=TMethodSymbol(FMembers[i]);
         if not methSym.IsAbstract then begin
            if Assigned(methSym.FExecutable) then
               methSym.FExecutable.InitSymbol(FMembers[i])
            else if not IsExternal then begin
               msg:=msgs.AddCompilerErrorFmt((methSym as TSourceMethodSymbol).DeclarationPos, CPE_MethodNotImplemented,
                                             [methSym.Name, methSym.StructSymbol.Caption]);
               afa:=TdwsAFAAddImplementation.Create(msg, AFA_AddImplementation);
               afa.Text:= #13#10
                         +StringReplace(methSym.GetDescription, '()', ' ', [rfIgnoreCase])
                         +';'#13#10'begin'#13#10#9'|'#13#10'end'#13#10;
               k:=Pos(methSym.Name, afa.Text);
               afa.Text:=Copy(afa.Text, 1, k-1)+methSym.StructSymbol.Name+'.'
                        +Copy(afa.Text, k, MaxInt);
            end;
         end;
      end;
   end;
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

// GetIsForwarded
//
function TStructuredTypeSymbol.GetIsForwarded : Boolean;
begin
   Result:=Assigned(FForwardPosition);
end;

// AddField
//
procedure TStructuredTypeSymbol.AddField(fieldSym : TFieldSymbol);
begin
   FMembers.AddSymbol(fieldSym);
   fieldSym.FStructSymbol:=Self;
end;

// FieldAtOffset
//
function TStructuredTypeSymbol.FieldAtOffset(offset : Integer) : TFieldSymbol;
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
            and SameText(meth.Name, methSym.Name)
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
procedure TStructuredTypeSymbol.SetForwardedPos(const pos : TScriptPos);
begin
   if FForwardPosition=nil then
      New(FForwardPosition);
   FForwardPosition^:=pos;
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
function TStructuredTypeSymbol.GetExternalName : String;
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
end;

// InitData
//
procedure TStructuredTypeMetaSymbol.InitData(const Data: TData; Offset: Integer);
begin
   Data[Offset] := Int64(0);
end;

// StructSymbol
//
function TStructuredTypeMetaSymbol.StructSymbol : TStructuredTypeSymbol;
begin
   Result:=TStructuredTypeSymbol(Typ);
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
      FIsDynamic:=True;
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

// InitData
//
procedure TRecordSymbol.InitData(const data : TData; offset : Integer);
var
   sym : TSymbol;
begin
   for sym in FMembers do
      if sym.ClassType=TFieldSymbol then
         TFieldSymbol(sym).InitData(Data, offset);
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
   Result:=Name+' = record'#13#10;
   for member in FMembers do begin
      if member is TFieldSymbol then
         Result:=Result+'   '+member.Name+' : '+member.Typ.Name+';'#13#10;
   end;
   Result:=Result+'end;';
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
   methSym.FVMTIndex:=FMethodCount;
   Inc(FMethodCount);
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

// CreateSelfParameter
//
function TInterfaceSymbol.CreateSelfParameter(methSym : TMethodSymbol) : TDataSymbol;
begin
   Assert(not methSym.IsClassMethod);
   Result:=TSelfSymbol.Create(SYS_SELF, Self);
   methSym.InternalParams.AddSymbol(Result);
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
function TFieldSymbol.GetExternalName : String;
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
procedure GenerateParams(const name : UnicodeString; table : TSymbolTable;
                         const funcParams : TParamArray; const addProc : TAddParamSymbolMethod);
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
                                   [paramRec.ParamType, paramRec.ParamName, Name]);

      if paramRec.HasDefaultValue then begin

         if paramRec.IsVarParam then
            raise Exception.Create(CPE_VarParamCantHaveDefaultValue);
         if paramRec.IsConstParam then
            raise Exception.Create(CPE_ConstParamCantHaveDefaultValue);

         paramSymWithDefault:=TParamSymbolWithDefaultValue.Create(paramRec.ParamName, typSym,
                                                                  paramRec.DefaultValue, 0);
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
   dwsSymbols.GenerateParams(name, table, funcParams, addParam);
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
      FExecutable.InitSymbol(Self)
   else if Level>=0 then begin
      msg:=msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_ForwardNotImplemented, [Name]);
      afa:=TdwsAFAAddImplementation.Create(msg, AFA_AddImplementation);

      afa.Text:= #13#10
                +StringReplace(GetDescription, '()', ' ', [rfIgnoreCase])
                +';'#13#10'begin'#13#10#9'|'#13#10'end'#13#10;
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

// GetIsDeprecated
//
function TFuncSymbol.GetIsDeprecated : Boolean;
begin
   Result:=(FDeprecatedMessage<>'');
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
function TFuncSymbol.GetExternalName : String;
begin
   if FExternalName='' then
      Result:=Name
   else Result:=FExternalName;
end;

// SetExternalName
//
procedure TFuncSymbol.SetExternalName(const val : String);
begin
   FExternalName:=val;
end;

// GetSourceSubExpr
//
function TFuncSymbol.GetSourceSubExpr(i : Integer) : TExprBase;
var
   prog : TdwsProgram;
begin
   prog:=(FExecutable.GetSelf as TdwsProgram);
   if i=0 then
      Result:=prog.InitExpr
   else Result:=prog.Expr;
end;

// GetSourceSubExprCount
//
function TFuncSymbol.GetSourceSubExprCount : Integer;
begin
   Result:=Ord(FExecutable<>nil)*2;
end;

// IsCompatible
//
function TFuncSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
const
   cCompatibleKinds : array [TFuncKind, TFuncKind] of Boolean =
      //  fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod
      ( (     True,      False,        False,         False,      True ),       // fkFunction
        (     False,     True,         False,         False,      True ),       // fkProcedure
        (     False,     False,        True,          False,      False),       // fkConstructor
        (     False,     False,        False,         True,       False),       // fkDestructor
        (     True,      True,         False,         False,      True )  );    // fkMethod
var
   funcSym : TFuncSymbol;
   i : Integer;
   param, otherParam : TSymbol;
begin
   if typSym=nil then Exit(False);
   typSym:=typSym.BaseType;
   if (typSym.ClassType=TNilSymbol) or (typSym.ClassType=TAnyFuncSymbol) then
      Result:=True
   else if typSym.IsType and not IsType then
      Result:=False
   else begin
      Result:=False;
      if not (typSym is TFuncSymbol) then
         Exit;
      funcSym:=TFuncSymbol(typSym);
      if Params.Count<>funcSym.Params.Count then Exit;
      if not cCompatibleKinds[Kind, funcSym.Kind] then Exit;
      if Typ<>funcSym.Typ then Exit;
      for i:=0 to Params.Count-1 do begin
         param:=Params[i];
         otherParam:=funcSym.Params[i];
         if param.ClassType<>otherParam.ClassType then Exit;
         if param.Typ<>otherParam.Typ then Exit;
      end;
      Result:=True;
   end;
end;

// DoIsOfType
//
function TFuncSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
var
   i : Integer;
   funcSym : TFuncSymbol;
begin
   Result:=    (typSym<>nil)
           and (typSym is TFuncSymbol);
   if not Result then Exit;

   funcSym:=TFuncSymbol(typSym);
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
  const Attributes: TMethodAttributes; const MethName: UnicodeString; const MethParams: TParamArray;
  const MethType: UnicodeString; Cls: TCompositeTypeSymbol; aVisibility : TdwsVisibility);
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
         if TMethodSymbol(meth).StructSymbol = Cls then
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
var
   classSymbol : TClassSymbol;
begin
   if val then begin
      classSymbol:=(StructSymbol as TClassSymbol);
      Include(FAttributes, maVirtual);
      if FVMTIndex<0 then begin
         FVMTIndex:=classSymbol.AllocateVMTindex;
         classSymbol.FVirtualMethodTable[FVMTIndex]:=Self;
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
var
   classSymbol : TClassSymbol;
begin
   FParentMeth:=meth;
   FVMTIndex:=meth.FVMTIndex;
   IsVirtual:=True;
   SetIsOverride(True);
   SetIsOverlap(False);

   classSymbol:=(StructSymbol as TClassSymbol);
   // make array unique
   SetLength(classSymbol.FVirtualMethodTable, Length(classSymbol.FVirtualMethodTable));
   classSymbol.FVirtualMethodTable[FVMTIndex]:=Self;
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
constructor TPropertySymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol; aVisibility : TdwsVisibility);
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
   dwsSymbols.GenerateParams(Name, Table, FuncParams, AddParam);
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
function TClassOperatorSymbol.QualifiedName : UnicodeString;
begin
   Result:=ClassSymbol.QualifiedName+'.'+Name;
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
var
   i : Integer;
   methodSymbol : TMethodSymbol;
begin
   inherited;
   // Check if class is abstract or not
   if methSym.IsAbstract then
      Include(FFlags, csfAbstract)
   else if methSym.IsOverride and methSym.FParentMeth.IsAbstract then begin
      Exclude(FFlags, csfAbstract);
      for i:=0 to High(FVirtualMethodTable) do begin
         methodSymbol:=FVirtualMethodTable[i];
         if methodSymbol.IsAbstract then begin
            Include(FFlags, csfAbstract);
            Break;
         end;
      end;
   end;
end;

// AddOperator
//
procedure TClassSymbol.AddOperator(sym: TClassOperatorSymbol);
begin
   sym.ClassSymbol:=Self;
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

// AddOverriddenInterface
//
function TClassSymbol.AddOverriddenInterface(const ancestorResolved : TResolvedInterface) : Boolean;
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

// AddOverriddenInterfaces
//
procedure TClassSymbol.AddOverriddenInterfaces;
var
   iter : TClassSymbol;
   loopProtection : TList;
begin
   iter:=Parent;
   loopProtection:=TList.Create;
   try
      while iter<>nil do begin
         if loopProtection.IndexOf(iter)>0 then Break;
         loopProtection.Add(iter);
         if iter.Interfaces<>nil then begin
            iter.Interfaces.Enumerate(
               procedure (const item : TResolvedInterface)
               begin
                  Self.AddOverriddenInterface(item);
               end);
         end;
         iter:=iter.Parent;
      end;
   finally
      loopProtection.Free;
   end;
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
begin
   // Check validity of the class declaration
   if IsForwarded then begin
      msgs.AddCompilerErrorFmt(FForwardPosition^, CPE_ClassNotCompletelyDefined, [Name]);
      Exit;
   end;

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

   FVirtualMethodTable:=ancestorClassSym.FVirtualMethodTable;

   IsStatic:=IsStatic or ancestorClassSym.IsStatic;
end;

// IsCompatible
//
function TClassSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
   typSym:=typSym.UnAliasedType;
   if typSym is TNilSymbol then
      Result:=True
   else if typSym is TClassSymbol then
      Result:=(NthParentOf(TClassSymbol(typSym))>=0)
   else Result:=False;
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

// GetIsPartial
//
function TClassSymbol.GetIsPartial : Boolean;
begin
   Result:=(csfPartial in FFlags);
end;

// SetIsPartial
//
procedure TClassSymbol.SetIsPartial;
begin
   Include(FFlags, csfPartial);
end;

// AllocateVMTindex
//
function TClassSymbol.AllocateVMTindex : Integer;
begin
   Result:=Length(FVirtualMethodTable);
   SetLength(FVirtualMethodTable, Result+1);
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
   Result:=True;
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
  if Typ <> nil then
    Result := 'class of ' + Typ.Name
  else
    Result := 'class of ???';
end;

function TClassOfSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  typSym := typSym.BaseType;
  Result :=    (typSym is TNilSymbol)
            or ((typSym is TClassOfSymbol) and Typ.IsCompatible(typSym.Typ));
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
begin
   Result:=    (typSym<>nil)
           and (   (typSym.UnAliasedType is TBaseSymbol)
                or (typSym.UnAliasedType is TEnumerationSymbol)
                or (typSym.UnAliasedType is TClassSymbol));
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
constructor TConnectorSymbol.Create(const name : UnicodeString; const connectorType : IConnectorType);
begin
   inherited Create(name);
   FConnectorType:=ConnectorType;
end;

// DoIsOfType
//
function TConnectorSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   (inherited DoIsOfType(typSym))
           or (typSym is TBaseVariantSymbol);
end;

// Specialize
//
function TConnectorSymbol.Specialize(table : TSymbolTable; const qualifier : UnicodeString) : TConnectorSymbol;
begin
   Result:=Self;
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

constructor TConstSymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol; const Value: Variant);
begin
  inherited Create(Name, Typ);
  SetLength(FData, 1);
  VarCopy(FData[0], Value);
end;

constructor TConstSymbol.Create(const Name: UnicodeString; Typ: TTypeSymbol; const Data: TData;
  Addr: Integer);
begin
  inherited Create(Name, Typ);
  SetLength(FData, Typ.Size);
  DWSCopyData(Data, Addr, FData, 0, Typ.Size);
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
      Result:=Name+': '+Typ.Name
  else Result:=Name+': ???';
end;

// AllocateStackAddr
//
procedure TDataSymbol.AllocateStackAddr(generator : TAddrGenerator);
begin
   FLevel:=generator.Level;
   FStackAddr:=generator.GetStackAddr(Size);
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
           and (Typ=other.Typ)
           and UnicodeSameText(Name, other.Name);
end;

// ------------------
// ------------------ TParamSymbolWithDefaultValue ------------------
// ------------------

// Create
//
constructor TParamSymbolWithDefaultValue.Create(const aName : UnicodeString; aType : TTypeSymbol;
                                                const data : TData; addr : Integer);
begin
   inherited Create(aName, aType);
   SetLength(FDefaultValue, Typ.Size);
   DWSCopyData(data, addr, FDefaultValue, 0, Typ.Size);
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

// ------------------
// ------------------ TLazyParamSymbol ------------------
// ------------------

// GetDescription
//
function TLazyParamSymbol.GetDescription: UnicodeString;
begin
   Result:='lazy '+inherited GetDescription;
end;

{ TConstParamSymbol }

function TConstParamSymbol.GetDescription: UnicodeString;
begin
  Result := 'const ' + inherited GetDescription;
end;

{ TVarParamSymbol }

function TVarParamSymbol.GetDescription: UnicodeString;
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
function TSymbolTable.EnumerateSymbolsOfNameInScope(const aName : UnicodeString; const callback : TSymbolEnumerationCallback) : Boolean;
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
         if p.EnumerateLocalHelpers(helpedType, callback) then Exit(True)
      end else begin
         if p.EnumerateHelpers(helpedType, callback) then Exit(True);
      end;
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
   if stfHasOperators in FFlags then begin
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
   if EnumerateLocalOperatorsFor(aToken, aLeftType, aRightType, callback) then Exit(True);
   for i:=0 to ParentCount-1 do begin
      p:=Parents[i];
      if p.EnumerateOperatorsFor(aToken, aLeftType, aRightType, callback) then Exit(True);
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
   if not (stfHasOperators in FFlags) then Exit;
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

// CollectPropertyAttributes
//
procedure TSymbolTable.CollectPropertyAttributes(tableList : TSimpleObjectHash<TSymbolTable>;
                                                 propertyList : TSimpleList<TPropertySymbol>);
var
   i : Integer;
   parent : TSymbolTable;
   sym, member : TSymbol;
begin
   tableList.Add(Self);
   for i:=0 to ParentCount-1 do begin
      parent:=Parents[i];
      if not tableList.Contains(parent) then
         parent.CollectPropertyAttributes(tableList, propertyList);
   end;
   for sym in Self do begin
      if sym.ClassType=TClassSymbol then begin
         for member in TClassSymbol(sym).Members do begin
            if member.ClassType=TPropertySymbol then begin
               if TPropertySymbol(member).Visibility=cvPublished then
                  propertyList.Add(TPropertySymbol(member));
            end;
         end;
      end;
   end;
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
      Include(FFlags, stfHasOperators)
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
function TMembersSymbolTable.FindSymbol(const aName : UnicodeString; minVisibility : TdwsVisibility;
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

// FindSymbolFromScope
//
function TMembersSymbolTable.FindSymbolFromScope(const aName : UnicodeString; scopeSym : TCompositeTypeSymbol) : TSymbol;
begin
   if scopeSym=nil then
      Result:=FindSymbol(aName, cvPublic)
   else if scopeSym=Owner then
      Result:=FindSymbol(aName, cvPrivate)
   else if scopeSym.DoIsOfType(Owner) then
      Result:=FindSymbol(aName, cvProtected)
   else Result:=FindSymbol(aName, cvPublic);
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
// ------------------ TArraySymbol ------------------
// ------------------

// Create
//
constructor TArraySymbol.Create(const name : UnicodeString; elementType, indexType : TTypeSymbol);
begin
   inherited Create(name, elementType);
   FIndexType:=indexType;
end;

// ElementSize
//
function TArraySymbol.ElementSize : Integer;
begin
   if Typ<>nil then
      Result:=Typ.Size
   else Result:=0;
end;

// ------------------
// ------------------ TDynamicArraySymbol ------------------
// ------------------

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
   Data[Offset]:=IScriptObj(TScriptDynamicArray.Create(Self.Typ));
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
  Result :=     (typSym is TDynamicArraySymbol)
            and (Typ.IsCompatible(typSym.Typ) or (typSym.Typ is TNilSymbol));
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
   inherited Create(Name, Typ, aValue);
   FIsUserDef := IsUserDef;
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

// DoIsOfType
//
function TEnumerationSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=   inherited DoIsOfType(typSym)
           or BaseType.DoIsOfType(typSym);
end;

// AddElement
//
procedure TEnumerationSymbol.AddElement(Element: TElementSymbol);
begin
   FElements.AddSymbol(Element);
   if Element.Value<FLowBound then
      FLowBound:=Element.Value;
   if Element.Value>FHighBound then
      FHighBound:=Element.Value;
end;

// ElementByValue
//
function TEnumerationSymbol.ElementByValue(const value : Int64) : TElementSymbol;
var
   i : Integer;
begin
   if (value>=FLowBound) and (value<FHighBound) then begin
      for i:=0 to Elements.Count-1 do begin
         Result:=TElementSymbol(Elements[i]);
         if Result.Value=value then Exit;
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

// DoIsOfType
//
function TAliasSymbol.DoIsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=Typ.DoIsOfType(typSym);
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

// IsCompatible
//
function TTypeSymbol.IsCompatible(typSym : TTypeSymbol) : Boolean;
begin
  Result:=(BaseType.IsCompatible(typSym.BaseType));
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
constructor EScriptError.CreatePosFmt(const pos : TScriptPos; const Msg: UnicodeString; const Args: array of const);
begin
   inherited CreateFmt(msg, args);
   FScriptPos:=pos;
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
function TRuntimeErrorMessage.AsInfo: UnicodeString;
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
   AddMsg(msg);
   HasErrors:=True;
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

// Destroy
//
destructor TOperatorSymbol.Destroy;
begin
   inherited;
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
   Result:=(typSym is TFuncSymbol);
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
   Result:='resourcestring '+Name+' = '''+StringReplace(Value, '''', '''''', [rfReplaceAll])+'''';
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

end.
