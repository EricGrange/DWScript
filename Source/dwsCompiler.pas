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
unit dwsCompiler;

interface

uses
  Variants, Classes, SysUtils, dwsExprs, dwsSymbols, dwsTokenizer, dwsErrors,
  dwsStrings, dwsFunctions, dwsStack, dwsCoreExprs, dwsFileSystem, dwsUtils,
  dwsMagicExprs, dwsRelExprs, dwsOperators;

type
   TCompilerOption = ( coOptimize, coSymbolDictionary, coContextMap, coAssertions,
                       coHintsDisabled, coWarningsDisabled, coExplicitUnitUses );
   TCompilerOptions = set of TCompilerOption;

const
   cDefaultCompilerOptions = [coOptimize, coAssertions];
   cDefaultMaxRecursionDepth = 1024;

type
  TIncludeEvent = procedure(const scriptName: string; var scriptSource: string) of object;
  TdwsOnNeedUnitEvent = function(const unitName : String; var unitSource : String) : IdwsUnit of object;

  TdwsCompiler = class;
  TCompilerReadInstrEvent = function (compiler : TdwsCompiler) : TNoResultExpr of object;
  TCompilerSectionChangedEvent = procedure (compiler : TdwsCompiler) of object;
  TCompilerReadScriptEvent = procedure (compiler : TdwsCompiler; sourceFile : TSourceFile; scriptType : TScriptSourceType) of object;

  TdwsFilter = class;

  TdwsConfiguration = class(TPersistent)
  private
    FCompilerOptions: TCompilerOptions;
    FConnectors: TStrings;
    FDefaultResultType: TdwsResultType;
    FFilter: TdwsFilter;
    FMaxDataSize: Integer;
    FMaxRecursionDepth : Integer;
    FOnInclude: TIncludeEvent;
    FOnNeedUnit : TdwsOnNeedUnitEvent;

    FOwner: TComponent;
    FResultType: TdwsResultType;
    FScriptPaths: TStrings;
    FConditionals: TStringList;
    FStackChunkSize: Integer;
    FSystemTable: TSymbolTable;
    FTimeoutMilliseconds: Integer;
    FUnits : TIdwsUnitList;
    FCompileFileSystem : TdwsCustomFileSystem;
    FRuntimeFileSystem : TdwsCustomFileSystem;

  protected
    procedure InitSystemTable;
    procedure SetResultType(const Value: TdwsResultType);
    procedure SetFilter(const Value: TdwsFilter);
    procedure SetTimeOut(const val : Integer);
    procedure SetCompileFileSystem(const val : TdwsCustomFileSystem);
    procedure SetRuntimeFileSystem(const val : TdwsCustomFileSystem);
    procedure SetScriptPaths(const values : TStrings);
    procedure SetConditionals(const val : TStringList);

  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);

    property Connectors: TStrings read FConnectors write FConnectors;
    property SystemTable: TSymbolTable read FSystemTable write FSystemTable;
    property Units : TIdwsUnitList read FUnits;

  published
    property Filter: TdwsFilter read FFilter write SetFilter;
    property ResultType: TdwsResultType read FResultType write SetResultType;
    property CompilerOptions: TCompilerOptions read FCompilerOptions write FCompilerOptions default cDefaultCompilerOptions;
    property MaxDataSize: Integer read FMaxDataSize write FMaxDataSize default 0;
    property MaxRecursionDepth : Integer read FMaxRecursionDepth write FMaxRecursionDepth default cDefaultMaxRecursionDepth;
    property Conditionals : TStringList read FConditionals write SetConditionals;
    property ScriptPaths: TStrings read FScriptPaths write SetScriptPaths;
    property CompileFileSystem : TdwsCustomFileSystem read FCompileFileSystem write SetCompileFileSystem;
    property RuntimeFileSystem : TdwsCustomFileSystem read FRuntimeFileSystem write SetRuntimeFileSystem;
    property TimeoutMilliseconds: Integer read FTimeoutMilliseconds write FTimeoutMilliseconds default 0;
    property TimeOut : Integer write SetTimeOut;
    property StackChunkSize: Integer read FStackChunkSize write FStackChunkSize default C_DefaultStackChunkSize;
    property OnInclude : TIncludeEvent read FOnInclude write FOnInclude;
    property OnNeedUnit : TdwsOnNeedUnitEvent read FOnNeedUnit write FOnNeedUnit;
  end;

  TdwsFilter = class(TComponent)
  private
    FSubFilter: TdwsFilter;
    FDependencies: TStrings;
    FPrivateDependencies: TStrings;
    function GetDependencies: TStrings;
  protected
    procedure SetSubFilter(const Filter: TdwsFilter); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property PrivateDependencies: TStrings read FPrivateDependencies;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Process(const Text: string; Msgs: TdwsMessageList): string; virtual;
    property SubFilter: TdwsFilter read FSubFilter write SetSubFilter;
    property Dependencies: TStrings read GetDependencies;
  end;

   TAddArgProcedure = procedure (argExpr : TTypedExpr) of object;
   TExpectedArgFunction = function : TParamSymbol of object;

   TSpecialKeywordKind = (skNone, skAssert, skAssigned, skHigh, skLength, skLow,
                          skOrd, skSizeOf, skDefined, skDeclared, skSqr,
                          skInc, skDec, skSucc, skPred);

   TSwitchInstruction = (siNone,
                         siIncludeLong, siIncludeShort, siIncludeOnce,
                         siFilterLong, siFilterShort,
                         siDefine, siUndef,
                         siIfDef, siIfNDef, siIf, siEndIf, siElse,
                         siHint, siHints, siWarning, siWarnings,
                         siError, siFatal );

   TLoopExitable = (leNotExitable, leBreak, leExit);

   TdwsOptimizationMessageList = class(TdwsRuntimeMessageList)
      private
         FCompileMsgs : TdwsCompileMessageList;

      public
         procedure AddMsg(aMessage : TdwsMessage); override;

   end;

   // holds execution context for optimizations during compilation
   TdwsCompilerExecution = class (TdwsExecution)
      private
         FCompiler : TdwsCompiler;
         FOptimMsgs : TdwsOptimizationMessageList;

      protected
         function GetMsgs : TdwsRuntimeMessageList; override;

      public
         constructor Create(const stackParams : TStackParameters; compiler : TdwsCompiler);
         destructor Destroy; override;

         function GetCallStack : TdwsExprLocationArray; override;
         function CallStackDepth : Integer; override;
   end;

   IdwsEvaluateExpr = interface
      ['{43410A86-3D04-4201-ABD5-02B935D6C6AF}']
      function GetExecution : IdwsProgramExecution;
      function GetRootProgram : IdwsProgram;
      function GetContextProcedure : TdwsProcedure;
      function GetExpression : TTypedExpr;
      function GetEvaluationError : Boolean;

      function ContextIsValid : Boolean;

      property Execution : IdwsProgramExecution read GetExecution;
      property RootProgram : IdwsProgram read GetRootProgram;
      property ContextProcedure : TdwsProcedure read GetContextProcedure;
      property Expression : TTypedExpr read GetExpression;
      property EvaluationError : Boolean read GetEvaluationError;
   end;

   TdwsEvaluateOption = (eoRootContext);
   TdwsEvaluateOptions = set of TdwsEvaluateOption;

   // holds and evaluated expression
   TdwsEvaluateExpr = class (TInterfacedObject, IdwsEvaluateExpr)
      private
         FExecution : IdwsProgramExecution;
         FContextProcedure : TdwsProcedure;
         FExpression : TTypedExpr;
         FEvaluationError : Boolean;

      protected
         function GetExecution : IdwsProgramExecution;
         function GetRootProgram : IdwsProgram;
         function GetContextProcedure : TdwsProcedure;
         function GetExpression : TTypedExpr;
         function GetEvaluationError : Boolean;

      public
         destructor Destroy; override;

         function ContextIsValid : Boolean;

         property Execution : IdwsProgramExecution read FExecution;
         property RootProgram : IdwsProgram read GetRootProgram;
         property ContextProcedure : TdwsProcedure read FContextProcedure;
         property Expression : TTypedExpr read FExpression;
         property EvaluationError : Boolean read FEvaluationError;
   end;

   TdwsReadTypeContext = (tcDeclaration, tcVariable, tcConstant, tcMember,
                          tcParameter, tcResult, tcOperand, tcExceptionClass,
                          tcProperty);

   TdwsUnitSection = (secMixed, secHeader, secInterface, secImplementation, secEnd);
   TdwsRootStatementAction = (rsaNone, rsaNoSemiColon, rsaInterface, rsaImplementation, rsaEnd);

   TdwsCompilerUnitContext = record
      Tokenizer : TTokenizer;
      UnitSymbol : TUnitSymbol;
      ConditionalDefines : TStringList;
   end;

   TdwsCompilerUnitContextStack = class(TSimpleStack<TdwsCompilerUnitContext>)
      public
         destructor Destroy; override;
         procedure Clean;

         procedure PushContext(compiler : TdwsCompiler);
         procedure PopContext(compiler : TdwsCompiler);
   end;

   // TdwsCompiler
   //
   TdwsCompiler = class
   private
      FOptions : TCompilerOptions;
      FTok : TTokenizer;
      FProg : TdwsProgram;
      FMainProg : TdwsMainProgram;
      FContextMap : TContextMap;
      FSymbolDictionary : TSymbolDictionary;
      FOperators : TOperators;
      FLoopExprs : TSimpleStack<TNoResultExpr>;
      FLoopExitable : TSimpleStack<TLoopExitable>;
      FFinallyExprs : TSimpleStack<Boolean>;
      FConditionalDepth : TSimpleStack<TSwitchInstruction>;
      FMsgs : TdwsCompileMessageList;

      FExec : TdwsCompilerExecution;
      FConnectors : TStrings;
      FCompileFileSystem : IdwsFileSystem;
      FOnInclude : TIncludeEvent;
      FOnNeedUnit : TdwsOnNeedUnitEvent;
      FUnits : TIdwsUnitList;
      FSystemTable : TSymbolTable;
      FScriptPaths : TStrings;
      FFilter : TdwsFilter;
      FIsExcept : Boolean;
      FIsSwitch : Boolean;
      FLineCount : Integer;
      FSourcePostConditionsIndex : Integer;
      FUnitSection : TdwsUnitSection;
      FUnitContextStack : TdwsCompilerUnitContextStack;
      FUnitsFromStack : TSimpleStack<String>;
      FUnitSymbol : TUnitSymbol;

      FOnReadInstr : TCompilerReadInstrEvent;
      FOnSectionChanged : TCompilerSectionChangedEvent;
      FOnReadScript : TCompilerReadScriptEvent;

      function Optimize : Boolean;

      function CheckFuncParams(paramsA, paramsB : TSymbolTable; indexSym : TSymbol = nil;
                               typSym : TTypeSymbol = nil) : Boolean;
      procedure CheckName(const Name: string);
      function IdentifySpecialName(const name : String) : TSpecialKeywordKind;
      procedure CheckSpecialName(const name : String);
      function CheckParams(A, B: TSymbolTable; CheckNames: Boolean): Boolean;
      procedure CompareFuncKinds(a, b : TFuncKind);
      procedure CompareFuncSymbolParams(a, b : TFuncSymbol);
      function CurrentClass : TClassSymbol;
      procedure HintUnusedSymbols;
      procedure HintUnusedResult(resultSymbol : TDataSymbol);

      function OpenStreamForFile(const scriptName : String) : TStream;
      function GetScriptSource(const scriptName : String) : String;
      function GetIncludeScriptSource(const scriptName : String) : String;

      function GetVarExpr(dataSym : TDataSymbol): TVarExpr;

      function GetLazyParamExpr(dataSym : TLazyParamSymbol) : TLazyParamExpr;
      function GetVarParamExpr(dataSym : TVarParamSymbol) : TVarParamExpr;
      function GetConstParamExpr(dataSym : TConstParamSymbol) : TVarParamExpr;

      function ReadAssign(token : TTokenType; left : TDataExpr) : TNoResultExpr;
      function ReadArrayType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
      function ReadArrayConstant(expecting : TTypeSymbol = nil) : TArrayConstantExpr;
      function ReadArrayMethod(const name : String; const namePos : TScriptPos;
                               baseExpr : TTypedExpr; isWrite : Boolean) : TProgramExpr;
      function ReadCase : TCaseExpr;
      function ReadCaseConditions(condList : TCaseConditions; valueExpr : TTypedExpr) : Integer;
      function ReadClassOf(const typeName : String) : TClassOfSymbol;
      function ReadClass(const typeName : String) : TClassSymbol;
      procedure ReadClassFields(const classSymbol : TClassSymbol; aVisibility : TdwsVisibility);
      function ReadInterface(const typeName : String) : TInterfaceSymbol;
      function ReadConnectorSym(const name : String; baseExpr : TTypedExpr;
                                const connectorType : IConnectorType; IsWrite: Boolean) : TProgramExpr;
      function ReadConnectorArray(const name : String; baseExpr : TTypedExpr;
                                  const connectorType : IConnectorType; IsWrite: Boolean) : TConnectorCallExpr;
      function ReadConstDecl(constSymbolClass : TConstSymbolClass) : TConstSymbol;
      function ReadConstValue : TConstExpr;
      function ReadConstRecord(symbol : TRecordSymbol) : TData;
      function ReadBlock : TNoResultExpr;
      function ReadBlocks(const endTokens : TTokenTypes; var finalToken : TTokenType) : TNoResultExpr;
      function ReadEnumeration(const typeName : String) : TEnumerationSymbol;
      function ReadExit : TNoResultExpr;
      function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
      function ReadExprAdd(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
      function ReadExprMult(expecting : TTypeSymbol = nil) : TTypedExpr;
      function ReadExprIn(var left : TTypedExpr) : TTypedExpr;
      function ReadExprInConditions(var left : TTypedExpr) : TInOpExpr;
      function ReadExternalVar(sym : TExternalVarSymbol; isWrite : Boolean) : TFuncExpr;
      function ReadField(expr : TDataExpr; sym : TFieldSymbol) : TFieldExpr;
      function ReadFor: TForExpr;
      function ReadStaticMethod(methodSym : TMethodSymbol; isWrite : Boolean;
                                expecting : TTypeSymbol = nil) : TProgramExpr;
      function ReadFunc(funcSym : TFuncSymbol; isWrite: Boolean;
                        codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TTypedExpr;
      function WrapUpFunctionRead(funcExpr : TFuncExprBase; expecting : TTypeSymbol = nil) : TTypedExpr;

      procedure ReadFuncArgs(funcExpr : TFuncExprBase); overload;
      procedure ReadArguments(const addArgProc : TAddArgProcedure;
                              leftDelim, rightDelim : TTokenType;
                              var argPosArray : TScriptPosArray;
                              const expectedProc : TExpectedArgFunction = nil); overload;
      function ReadFuncResultType(funcKind : TFuncKind) : TTypeSymbol;

      function ReadIf: TNoResultExpr;
      function ReadInherited(IsWrite: Boolean): TProgramExpr;
      function ReadInstr : TNoResultExpr;
      function ReadInstrSwitch(semiPending : Boolean): TNoResultExpr;
      function ReadExprSwitch : TTypedExpr;
      function ReadUntilEndOrElseSwitch(allowElse : Boolean) : Boolean;
      function ReadIntfMethodDecl(intfSym : TInterfaceSymbol; funcKind : TFuncKind) : TSourceMethodSymbol;
      function ReadMethodDecl(classSym : TClassSymbol; funcKind : TFuncKind;
                              aVisibility : TdwsVisibility; isClassMethod : Boolean) : TSourceMethodSymbol;
      function ReadMethodImpl(classSym : TClassSymbol; funcKind : TFuncKind;
                              isClassMethod : Boolean) : TMethodSymbol;
      procedure ReadDeprecated(funcSym : TFuncSymbol);
      procedure WarnDeprecated(funcSym : TFuncSymbol);
      function ReadName(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TProgramExpr;
      function ReadClassSymbolName(baseType : TClassSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
      function ReadInterfaceSymbolName(baseType : TInterfaceSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
      function ReadConstName(constSym : TConstSymbol; isWrite: Boolean) : TProgramExpr;
      function ReadNameOld(isWrite: Boolean): TTypedExpr;
      function ReadNameInherited(isWrite: Boolean): TProgramExpr;
      // Created overloaded ReadNameList to deal with script positions
      procedure ReadNameList(names : TStrings); overload;
      procedure ReadNameList(names : TStrings; var posArray : TScriptPosArray); overload;
      function  ReadNew(isWrite : Boolean) : TProgramExpr;
      function  ReadNewArray(elementTyp : TTypeSymbol; isWrite : Boolean) : TProgramExpr;
      procedure ReadArrayParams(ArrayIndices: TSymbolTable);
      // Don't want to add param symbols to dictionary when a method implementation (they get thrown away)
      procedure ReadParams(const addParamMeth : TParamSymbolMethod; paramsToDictionary : Boolean = True);
      function ReadProcDecl(funcKind : TFuncKind; isClassMethod : Boolean = False;
                            isType : Boolean = False) : TFuncSymbol;
      procedure ReadProcBody(funcSymbol : TFuncSymbol);
      procedure ReadConditions(funcSymbol : TFuncSymbol; conditions : TSourceConditions;
                               condsSymClass : TConditionSymbolClass);
      procedure ReadPostConditions(funcSymbol : TFuncSymbol; conditions : TSourcePostConditions;
                                   condsSymClass : TConditionSymbolClass);
      function ReadOperatorDecl : TOperatorSymbol;
      function ReadClassOperatorDecl(ClassSym: TClassSymbol) : TClassOperatorSymbol;
      function ReadPropertyDecl(structSym : TStructuredTypeSymbol; aVisibility : TdwsVisibility) : TPropertySymbol;
      function ReadPropertyExpr(var expr : TDataExpr; propertySym : TPropertySymbol; isWrite: Boolean) : TProgramExpr;
      function ReadPropertyReadExpr(var expr : TDataExpr; propertySym : TPropertySymbol) : TProgramExpr;
      function ReadPropertyWriteExpr(var expr : TDataExpr; propertySym : TPropertySymbol) : TProgramExpr;
      function ReadPropertyArrayAccessor(var expr : TDataExpr; propertySym : TPropertySymbol;
                                         typedExprList : TTypedExprList;
                                         const scriptPos : TScriptPos; isWrite : Boolean) : TFuncExpr;
      function ReadRecord(const typeName : String) : TRecordSymbol;
      function ReadRaise : TRaiseBaseExpr;
      function ReadRepeat : TNoResultExpr;
      function ReadRootStatement(var action : TdwsRootStatementAction) : TNoResultExpr;
      function ReadRootBlock(const endTokens: TTokenTypes; var finalToken: TTokenType) : TBlockExpr;
      procedure ReadSemiColon;
      function ReadScript(sourceFile : TSourceFile; scriptType : TScriptSourceType) : TNoResultExpr;
      function ReadSpecialFunction(const namePos : TScriptPos; specialKind : TSpecialKeywordKind) : TProgramExpr;
      function ReadStatement : TNoResultExpr;
      function ReadStringArray(Expr: TDataExpr; IsWrite: Boolean): TProgramExpr;
      function ReadSwitch(const SwitchName: string): Boolean;
      function ReadSymbol(expr : TProgramExpr; isWrite : Boolean = False;
                          expecting : TTypeSymbol = nil) : TProgramExpr;
      function ReadTerm(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TTypedExpr;
      function ReadNegation : TTypedExpr;

      function ReadTry : TExceptionExpr;
      function ReadFinally(tryExpr : TNoResultExpr) : TFinallyExpr;
      function ReadExcept(tryExpr : TNoResultExpr) : TExceptExpr;

      function ReadType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
      function ReadTypeCast(const namePos : TScriptPos; typeSym : TTypeSymbol) : TTypedExpr;
      procedure ReadTypeDecl;
      procedure ReadUses;
      procedure ReadUnitHeader;
      function ReadVarDecl : TNoResultExpr;
      function ReadWhile : TNoResultExpr;
      function ResolveUnitReferences : TIdwsUnitList;

   protected
      procedure EnterLoop(loopExpr : TNoResultExpr);
      procedure MarkLoopExitable(level : TLoopExitable);
      procedure LeaveLoop;

      function GetFuncExpr(funcSym : TFuncSymbol; isWrite : Boolean;
                           codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TFuncExprBase;
      function GetMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
                             const Pos: TScriptPos; ForceStatic : Boolean): TFuncExpr;

      procedure MemberSymbolWithNameAlreadyExists(sym : TSymbol);
      procedure IncompatibleTypes(const scriptPos : TScriptPos; const fmt : String; typ1, typ2 : TTypeSymbol);

      function CreateProgram(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                             const stackParams : TStackParameters) : TdwsMainProgram;
      function CreateProcedure(Parent : TdwsProgram) : TdwsProcedure;
      function CreateAssign(const pos : TScriptPos; token : TTokenType; left : TDataExpr; right : TTypedExpr) : TNoResultExpr;

      function CreateArrayLow(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
      function CreateArrayHigh(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
      function CreateArrayLength(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;

      function CreateOperatorFunction(funcSym : TFuncSymbol; left, right : TTypedExpr) : TTypedExpr;

      procedure DoSectionChanged;

      function SwitchTokenizerToInclude(const sourceName, sourceCode : String) : TNoResultExpr;
      procedure SwitchTokenizerToUnit(srcUnit : TSourceUnit; const sourceCode : String);

      procedure SetupCompileOptions(conf : TdwsConfiguration);
      procedure CleanupAfterCompile;

      procedure CheckFilterDependencies(confUnits : TIdwsUnitList);
      procedure HandleUnitDependencies;
      function  HandleExplicitDependency(const unitName : String) : TUnitSymbol;

   public
      constructor Create;
      destructor Destroy; override;

      function Compile(const aCodeText : String; aConf : TdwsConfiguration) : IdwsProgram;
      procedure RecompileInContext(const context : IdwsProgram; const aCodeText : String; aConf : TdwsConfiguration);

      class function Evaluate(exec : IdwsProgramExecution; const anExpression : String;
                              options : TdwsEvaluateOptions = []) : IdwsEvaluateExpr;

      procedure WarnForVarUsage(varExpr : TVarExpr; const pos : TScriptPos);

      property CurrentProg : TdwsProgram read FProg write FProg;
      property Options : TCompilerOptions read FOptions write FOptions;
      property UnitSection : TdwsUnitSection read FUnitSection write FUnitSection;
      property Tokenizer : TTokenizer read FTok write FTok;

      property OnReadInstr : TCompilerReadInstrEvent read FOnReadInstr write FOnReadInstr;
      property OnSectionChanged : TCompilerSectionChangedEvent read FOnSectionChanged write FOnSectionChanged;
      property OnReadScript : TCompilerReadScriptEvent read FOnReadScript write FOnReadScript;
   end;

  TdwsDefaultResult = class(TdwsResult)
  private
    FTextBuilder: TWriteOnlyBlockStream;
    function GetText: String; inline;
  public
    constructor Create(resultType : TdwsResultType); override;
    destructor Destroy; override;
    procedure AddString(const str : String); override;
    function ToString : String; override;
    property Text: String read GetText;
  end;

  TdwsDefaultResultType = class(TdwsResultType)
  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
    function CreateProgResult: TdwsResult; override;
  end;

  TPrintFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

  TPrintLnFunction = class(TInternalFunction)
  public
    procedure Execute(info : TProgramInfo); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cSwitchInstructions : array [TSwitchInstruction] of String = (
      '',
      SWI_INCLUDE_LONG, SWI_INCLUDE_SHORT, SWI_INCLUDE_ONCE,
      SWI_FILTER_LONG, SWI_FILTER_SHORT,
      SWI_DEFINE, SWI_UNDEF,
      SWI_IFDEF, SWI_IFNDEF, SWI_IF, SWI_ENDIF, SWI_ELSE,
      SWI_HINT, SWI_HINTS, SWI_WARNING, SWI_WARNINGS, SWI_ERROR, SWI_FATAL
      );
   cAssignmentTokens : TTokenTypes = [ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
                                      ttTIMES_ASSIGN, ttDIVIDE_ASSIGN];

type
   TReachStatus = (rsReachable, rsUnReachable, rsUnReachableWarned);

   TObjectClassNameMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TObjectClassTypeMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TObjectDestroyMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TObjectFreeMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TExceptionCreateMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TExceptionDestroyMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TExceptionStackTraceMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TDelphiExceptionCreateMethod = class(TExceptionCreateMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TExceptObjFunc = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
   end;

   TParamFunc = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
   end;

   TParamStrFunc = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
   end;

   TParamCountFunc = class(TInternalFunction)
      procedure Execute(info : TProgramInfo); override;
   end;

// StringToSwitchInstruction
//
function StringToSwitchInstruction(const str : String) : TSwitchInstruction;
begin
   // This procedure is called by the tokenizer if it finds {$xx in the string
   for Result:=Low(TSwitchInstruction) to High(TSwitchInstruction) do begin
      if str=cSwitchInstructions[Result] then
         Exit;
   end;
   Result:=siNone;
end;

// ------------------
// ------------------ TdwsCompiler ------------------
// ------------------

// Create
//
constructor TdwsCompiler.Create;
var
   stackParams : TStackParameters;
begin
   inherited;
   FLoopExprs:=TSimpleStack<TNoResultExpr>.Create;
   FLoopExitable:=TSimpleStack<TLoopExitable>.Create;
   FConditionalDepth:=TSimpleStack<TSwitchInstruction>.Create;
   FFinallyExprs:=TSimpleStack<Boolean>.Create;
   FUnitsFromStack:=TSimpleStack<String>.Create;
   FUnitContextStack:=TdwsCompilerUnitContextStack.Create;

   stackParams.MaxLevel:=1;
   stackParams.ChunkSize:=512;
   stackParams.MaxByteSize:=MaxInt;
   stackParams.MaxRecursionDepth:=MaxInt;

   FExec:=TdwsCompilerExecution.Create(stackParams, Self);
end;

// Destroy
//
destructor TdwsCompiler.Destroy;
begin
   FUnitsFromStack.Free;
   FUnitContextStack.Free;
   FExec.Free;
   FFinallyExprs.Free;
   FConditionalDepth.Free;
   FLoopExitable.Free;
   FLoopExprs.Free;
   inherited;
end;

function TdwsCompiler.ResolveUnitReferences : TIdwsUnitList;
var
   x, y, z : Integer;
   expectedUnitCount : Integer;
   deps: TStrings;
   refCount : array of Integer;
   changed : Boolean;
   unitName: string;
   curUnit : IdwsUnit;
begin
   // initialize reference count vector
   expectedUnitCount:=FUnits.Count;
   SetLength(refCount, expectedUnitCount);

   // Calculate number of outgoing references
   for x := 0 to FUnits.Count-1 do begin
      curUnit:=FUnits[x];
      if (ufImplicitUse in curUnit.GetUnitFlags) or not (coExplicitUnitUses in FOptions) then begin
         deps := curUnit.GetDependencies;
         for y := 0 to deps.Count - 1 do begin
            if FUnits.IndexOf(deps[y]) < 0 then
               FMsgs.AddCompilerStopFmt(cNullPos, CPE_UnitNotFound, [deps[y], curUnit.GetUnitName]);
         end;
         refCount[x] := deps.Count;
      end else begin
         refCount[x] := -1;
         Dec(expectedUnitCount);
      end;
   end;

  Result := TIdwsUnitList.Create;
  try

    // Resolve references
    changed := True;
    while changed do
    begin
      changed := False;
      for x := 0 to FUnits.Count - 1 do begin
        curUnit:=FUnits[x];
        // Find unit that is not referencing other units
        if refCount[x] = 0 then
        begin
          Result.Add(curUnit);

          // Remove the references to this unit from all other units
          unitName := curUnit.GetUnitName;
          for y := 0 to FUnits.Count - 1 do
          begin
            deps := FUnits[y].GetDependencies;
            for z := 0 to deps.Count - 1 do
              if UnicodeSameText(deps[z], unitName) then
                Dec(refCount[y]);
          end;

          refCount[x] := -1;
          changed := True;
        end;
      end;
    end;

    if Result.Count<>expectedUnitCount then
      FMsgs.AddCompilerStop(cNullPos, CPE_UnitCircularReference);
  except
    Result.Free;
    raise;
  end;
end;

// EnterLoop
//
procedure TdwsCompiler.EnterLoop(loopExpr : TNoResultExpr);
begin
   FLoopExprs.Push(loopExpr);
   FLoopExitable.Push(leNotExitable);
   if FFinallyExprs.Count>0 then
      FFinallyExprs.Push(False);
end;

// MarkLoopExitable
//
procedure TdwsCompiler.MarkLoopExitable(level : TLoopExitable);
var
   i : Integer;
begin
   if FLoopExprs.Count=0 then Exit;
   case level of
      leBreak : begin
         if FLoopExitable.Peek=leNotExitable then
            FLoopExitable.Peek:=level;
      end;
      leExit : begin
         for i:=0 to FLoopExitable.Count-1 do begin
            if FLoopExitable.Items[i]=level then Break;
            FLoopExitable.Items[i]:=level;
         end;
      end;
   end;
end;

// LeaveLoop
//
procedure TdwsCompiler.LeaveLoop;
begin
   if FLoopExitable.Peek=leNotExitable then
      FProg.CompileMsgs.AddCompilerWarning(FLoopExprs.Peek.ScriptPos, CPW_InfiniteLoop);

   if (FFinallyExprs.Count>0) and (not FFinallyExprs.Peek) then
      FFinallyExprs.Pop;

   FLoopExprs.Pop;
   FLoopExitable.Pop;
end;

// GetFuncExpr
//
function TdwsCompiler.GetFuncExpr(funcSym : TFuncSymbol; isWrite : Boolean;
                               codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TFuncExprBase;
var
   magicFuncSym : TMagicFuncSymbol;
begin
   if funcSym.InheritsFrom(TMethodSymbol) and not (TMethodSymbol(funcSym).IsClassMethod) then begin

      if codeExpr=nil then begin

         Result:=TMethodStaticExpr.Create(FProg, FTok.HotPos, TMethodSymbol(funcSym),
                                          TMethodObjExpr.Create(FProg, FTok.HotPos, codeExpr));

      end else begin

         Result:=nil;
         Assert(False);
//         Result:=TMethodStaticExpr.Create(FProg, FTok.HotPos, TMethodSymbol(funcSym),
//                                          TMethodObjExpr.Create(FProg, FTok.HotPos, codeExpr),
//                                          True, codeExpr, isWrite);

      end;

   end else if codeExpr=nil then begin

      if funcSym.InheritsFrom(TMagicFuncSymbol) then begin

         magicFuncSym:=TMagicFuncSymbol(funcSym);
         Result:=TMagicFuncExpr.CreateMagicFuncExpr(FProg, FTok.HotPos, magicFuncSym);

      end else Result:=TFuncExpr.Create(FProg, FTok.HotPos, funcSym);

   end else begin

      Result:=TFuncPtrExpr.Create(FProg, FTok.HotPos, codeExpr);

   end;

   if (funcSym.Typ<>nil) and (funcSym.Typ.Size>1) and Result.InheritsFrom(TFuncExpr) then
      TFuncExpr(Result).SetResultAddr(FProg, nil);

end;

// GetMethodExpr
//
function TdwsCompiler.GetMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
             const Pos: TScriptPos; ForceStatic : Boolean): TFuncExpr;
begin
   WarnDeprecated(meth);
   Result:=CreateMethodExpr(FProg, meth, Expr, RefKind, Pos, ForceStatic);
end;

// MemberSymbolWithNameAlreadyExists
//
procedure TdwsCompiler.MemberSymbolWithNameAlreadyExists(sym : TSymbol);
var
   msgFmt : String;
begin
   if sym is TFieldSymbol then
      msgFmt:=CPE_FieldRedefined
   else if sym is TPropertySymbol then
      msgFmt:=CPE_PropertyRedefined
   else if sym is TMethodSymbol then
      msgFmt:=CPE_MethodRedefined
   else msgFmt:=CPE_NameAlreadyExists;
   FMsgs.AddCompilerErrorFmt(FTok.HotPos, msgFmt, [sym.Name])
end;

// IncompatibleTypes
//
procedure TdwsCompiler.IncompatibleTypes(const scriptPos : TScriptPos;
                                         const fmt : String; typ1, typ2 : TTypeSymbol);
var
   caption1, caption2 : String;
begin
   if typ1=nil then
      caption1:='void'
   else caption1:=typ1.Caption;
   if typ2=nil then
      caption2:='void'
   else caption2:=typ2.Caption;
   FMsgs.AddCompilerErrorFmt(scriptPos, fmt, [caption1, caption2]);
end;

// SetupCompileOptions
//
procedure TdwsCompiler.SetupCompileOptions(conf : TdwsConfiguration);
begin
   FFilter := conf.Filter;
   FConnectors := conf.Connectors;
   FOptions := conf.CompilerOptions;
   FUnits := TIdwsUnitList.Create;
   FUnits.AddUnits(conf.Units);
   FOnInclude := conf.OnInclude;
   FOnNeedUnit := conf.OnNeedUnit;
   FScriptPaths := conf.ScriptPaths;
   FSystemTable := conf.SystemTable;

   if Conf.CompileFileSystem<>nil then
      FCompileFileSystem := Conf.CompileFileSystem.AllocateFileSystem
   else FCompileFileSystem := TdwsOSFileSystem.Create;
end;

// CleanupAfterCompile
//
procedure TdwsCompiler.CleanupAfterCompile;
begin
   FIsExcept:=False;

   FOperators:=nil;

   FMsgs:=nil;

   FCompileFileSystem:=nil;
   FOnInclude:=nil;
   FOnNeedUnit:=nil;
   FUnits.Free;
   FSystemTable:=nil;
   FUnitContextStack.Clean;
   FUnitsFromStack.Clear;

   FProg:=nil;
   FMainProg:=nil;
   FContextMap:=nil;
   FSymbolDictionary:=nil;

   FLoopExprs.Clear;
   FLoopExitable.Clear;
   FConditionalDepth.Clear;
   FFinallyExprs.Clear;
end;

// Compile
//
function TdwsCompiler.Compile(const aCodeText : String; aConf : TdwsConfiguration) : IdwsProgram;
var
   stackParams : TStackParameters;
   codeText : String;
   sourceFile : TSourceFile;
begin
   SetupCompileOptions(aConf);

   stackParams.MaxByteSize:=aConf.MaxDataSize;
   if stackParams.MaxByteSize=0 then
      stackParams.MaxByteSize:=MaxInt;

   stackParams.ChunkSize:=aConf.StackChunkSize;
   if stackParams.ChunkSize<=0 then
      stackParams.ChunkSize:=1;

   stackParams.MaxRecursionDepth:=aConf.MaxRecursionDepth;

   FLineCount:=0;

   // Create the TdwsProgram
   FMainProg:=CreateProgram(aConf.SystemTable, aConf.ResultType, stackParams);
   FMsgs:=FMainProg.CompileMsgs;

   FMsgs.HintsDisabled:=(coHintsDisabled in aConf.CompilerOptions);
   FMsgs.WarningsDisabled:=(coWarningsDisabled in aConf.CompilerOptions);

   FMainProg.Compiler:=Self;
   FMainProg.TimeoutMilliseconds:=aConf.TimeoutMilliseconds;
   FMainProg.RuntimeFileSystem:=aConf.RuntimeFileSystem;
   FMainProg.ConditionalDefines:=aConf.Conditionals;
   FContextMap:=FMainProg.ContextMap;
   FSymbolDictionary:=FMainProg.SymbolDictionary;
   FUnitSection:=secMixed;

   FProg:=FMainProg;

   FOperators:=TOperators.Create(FProg.Table);
   FMainProg.Operators:=FOperators;
   FOperators.FunctionOperatorConstructor:=CreateOperatorFunction;

   try
      CheckFilterDependencies(aConf.Units);

      // Filter stuff
      if Assigned(FFilter) then
         codeText := FFilter.Process(aCodeText, FMsgs)
      else codeText := aCodeText;

      sourceFile:=FMainProg.RegisterSourceFile(MSG_MainModule, codeText);

      // Start compilation
      FProg.Expr := ReadScript(sourceFile, stMain);

      // Initialize symbol table
      FProg.Table.Initialize(FMsgs);
   except
      on e: ECompileError do
         ;
      on e: Exception do
         FMsgs.AddCompilerError(cNullPos, e.Message);
   end;

   FMainProg.LineCount:=FLineCount;
   FMainProg.Compiler:=nil;
   Result:=FMainProg;

   CleanupAfterCompile;
end;

// CheckFilterDependencies
//
procedure TdwsCompiler.CheckFilterDependencies(confUnits : TIdwsUnitList);
var
   f : TdwsFilter;
   dep : String;
begin
   // Check for missing units
   f:=FFilter;
   while Assigned(f) do begin
      for dep in f.Dependencies do begin
         if confUnits.IndexOf(dep)<0 then
            FMsgs.AddCompilerErrorFmt(cNullPos, CPE_FilterDependsOnUnit,
                                      [f.ClassName, dep]);
      end;
      f:=f.SubFilter;
   end;
end;

// HandleUnitDependencies
//
procedure TdwsCompiler.HandleUnitDependencies;
var
   i : Integer;
   unitsResolved: TIdwsUnitList;
   unitsTable: TSymbolTable;
   unitTables: TList;
   unitTable : TUnitSymbolTable;
   unitSymbol : TUnitSymbol;
begin
   // Handle unit dependencies
   unitsResolved := ResolveUnitReferences;//(conf);

   unitTables := TList.Create;
   unitsTable := TSymbolTable.Create;
   try
      try
         // Get the symboltables of the units
         for i := 0 to unitsResolved.Count - 1 do begin
            unitTable := unitsResolved[i].GetUnitTable(FSystemTable, unitsTable, FOperators);
            unitTables.Add(unitTable);
            unitsTable.AddSymbol(TUnitSymbol.Create(unitsResolved[i].GetUnitName, unitTable));
         end;
      except
         on e: Exception do begin
            for i:=0 to unitTables.Count-1 do
               TObject(unitTables[i]).Free;
            raise;
         end;
      end;

      // Add the units to the program-symboltable
      for i := 0 to unitsTable.Count - 1 do begin
         unitSymbol:=TUnitSymbol(unitsTable[i]);

         FProg.Table.AddSymbol(TUnitSymbol.Create( unitSymbol.Name, unitSymbol.Table, True));
         FProg.Table.AddParent(unitSymbol.Table);
      end;

      unitSymbol:=FProg.Table.FindSymbol(SYS_INTERNAL, cvMagic) as TUnitSymbol;
      FProg.Table.AddSymbol(TUnitSymbol.Create( SYS_SYSTEM, unitSymbol.Table, False ));

   finally
      unitsTable.Free;
      unitTables.Free;
      unitsResolved.Free;
   end;
end;

// HandleExplicitDependency
//
function TdwsCompiler.HandleExplicitDependency(const unitName : String) : TUnitSymbol;
var
   i : Integer;
   unitResolved : IdwsUnit;
   unitTable : TUnitSymbolTable;
   dependencies : TStrings;
   unitSource : String;
   srcUnit : TSourceUnit;
begin
   for i:=0 to FUnitsFromStack.Count-1 do
      if SameText(FUnitsFromStack.Items[i], unitName) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UnitCircularReference);

   Result:=TUnitSymbol(FProg.Table.FindLocal(unitName, TUnitSymbol));
   if Result<>nil then begin
      // ignore multiple requests (for now)
      Exit;
   end;

   i:=FUnits.IndexOf(unitName);
   if i<0 then begin
      if Assigned(FOnNeedUnit) then begin
         unitResolved:=FOnNeedUnit(unitName, unitSource);
         if unitResolved<>nil then
            FUnits.Add(unitResolved)
         else begin
            if unitSource='' then
               unitSource:=GetScriptSource(unitName);
            if unitSource<>'' then begin
               srcUnit:=TSourceUnit.Create(unitName, FProg.Root.RootTable);
               unitResolved:=srcUnit;
               FUnits.Add(unitResolved);
               try
                  SwitchTokenizerToUnit(srcUnit, unitSource);
               except
                  srcUnit.Symbol.IsTableOwner:=True;
                  raise;
               end;
            end;
         end;
      end;
      if unitResolved=nil then begin
         if FUnitsFromStack.Count=0 then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnknownUnit, [unitName])
         else FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnitNotFound,
                                        [unitName, FUnitsFromStack.Peek]);
         Exit;
      end;
   end else unitResolved:=FUnits[i];

   dependencies:=unitResolved.GetDependencies;
   for i:=0 to dependencies.Count-1 do begin
      FUnitsFromStack.Push(unitName);
      try
         HandleExplicitDependency(dependencies[i]);
      finally
         FUnitsFromStack.Pop;
      end;
   end;

   unitTable:=nil;
   try
      unitTable:=unitResolved.GetUnitTable(FSystemTable, nil, FOperators);
   except
      unitTable.Free;
      raise;
   end;

   Result:=TUnitSymbol.Create(unitResolved.GetUnitName, unitTable,
                              not (ufOwnsSymbolTable in unitResolved.GetUnitFlags));
   FProg.Table.AddSymbol(Result);
   FProg.Table.AddParent(Result.Table);
end;

// RecompileInContext
//
procedure TdwsCompiler.RecompileInContext(const context : IdwsProgram;
               const aCodeText : String; aConf : TdwsConfiguration);
var
   codeText : String;
   sourceFile : TSourceFile;
begin
   SetupCompileOptions(aConf);

   FMainProg:=context as TdwsMainProgram;
   FMainProg.Compiler:=Self;
   FContextMap:=FMainProg.ContextMap;
   FSymbolDictionary:=FMainProg.SymbolDictionary;

   FMsgs:=FMainProg.CompileMsgs;
   FMsgs.HintsDisabled:=(coHintsDisabled in aConf.CompilerOptions);
   FMsgs.WarningsDisabled:=(coWarningsDisabled in aConf.CompilerOptions);

   FProg:=FMainProg;

   FOperators:=FMainProg.Operators as TOperators;

   FLineCount:=0;
   try
      // Filter stuff
      if Assigned(FFilter) then
         codeText := FFilter.Process(aCodeText, FMsgs)
      else codeText := aCodeText;

      sourceFile:=FMainProg.SourceList.FindScriptSourceItem(MSG_MainModule).SourceFile;
      sourceFile.Code:=codeText;

      FMainProg.ResetExprs;
      FUnitSymbol:=nil;
      FUnitSection:=secMixed;

      // Start compilation
      FProg.Expr := ReadScript(sourceFile, stInclude);

      // Initialize symbol table
      FProg.Table.Initialize(FMsgs);
   except
      on e: ECompileError do
         ;
      on e: Exception do
         FMsgs.AddCompilerError(cNullPos, e.Message);
   end;

   FMainProg.LineCount:=FLineCount;
   FMainProg.Compiler:=nil;

   CleanupAfterCompile;
end;

// Optimize
//
function TdwsCompiler.Optimize : Boolean;
begin
   Result:=(coOptimize in FOptions) and (not FMsgs.HasErrors);
end;

// ReadRootBlock
//
function TdwsCompiler.ReadRootBlock(const endTokens : TTokenTypes; var finalToken : TTokenType) : TBlockExpr;
var
   reach : TReachStatus;
   stmt : TNoResultExpr;
   action : TdwsRootStatementAction;
begin
   reach:=rsReachable;
   Result:=TBlockExpr.Create(FProg, FTok.HotPos);
   try
      while FTok.HasTokens do begin
         finalToken:=FTok.TestDeleteAny(endTokens);
         if finalToken<>ttNone then Break;

         if reach=rsUnReachable then begin
            reach:=rsUnReachableWarned;
            FMsgs.AddCompilerWarning(FTok.HotPos, CPW_UnReachableCode);
         end;

         stmt:=ReadRootStatement(action);
         if Assigned(stmt) then begin
            Result.AddStatement(Stmt);
            if     (reach=rsReachable)
               and (   (stmt is TFlowControlExpr)
                    or (stmt is TRaiseExpr)) then
               reach:=rsUnReachable;
         end;

         case action of
            rsaNone : begin
               if  not FTok.TestDelete(ttSEMI) then begin
                  if endTokens<>[] then begin
                     finalToken:=FTok.TestDeleteAny(endTokens);
                     if finalToken=ttNone then
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
                     Break;
                  end else begin
                     while FTok.HasTokens and FTok.Test(ttSWITCH) do begin
                        ReadInstrSwitch(True);
                        FTok.KillToken;
                     end;
                     if FTok.HasTokens then
                        FMsgs.AddCompilerStop(FTok.CurrentPos, CPE_SemiExpected);
                  end;
               end;
            end;
            rsaImplementation : begin
               finalToken:=ttIMPLEMENTATION;
               Exit;
            end;
            rsaEnd : begin
               finalToken:=ttEND;
               Exit;
            end;
         end;
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadSemiColon
//
procedure TdwsCompiler.ReadSemiColon;
begin
   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
end;

// ReadScript
//
function TdwsCompiler.ReadScript(sourceFile : TSourceFile; scriptType : TScriptSourceType) : TNoResultExpr;
var
   finalToken : TTokenType;
   oldTok : TTokenizer;
   oldSection : TdwsUnitSection;
   unitBlock : TBlockExpr;
   implemTable : TUnitImplementationTable;
   oldTable : TSymbolTable;
   oldUnit : TUnitSymbol;
begin
   oldTok:=FTok;
   oldSection:=FUnitSection;
   FTok:=TTokenizer.Create(sourceFile, FProg.CompileMsgs);
   try
      FTok.SwitchHandler:=ReadSwitch;

      if Assigned(FOnReadScript) then
         FOnReadScript(Self, sourceFile, scriptType);

      FMainProg.SourceList.Add(sourceFile.Name, sourceFile, scriptType);

      case scriptType of
         stMain :
            HandleUnitDependencies;
         stUnit : begin
            FUnitSection:=secHeader;
            ReadUnitHeader;
         end;
      end;

      Result:=ReadRootBlock([], finalToken);
      if scriptType=stUnit then
         FreeAndNil(Result);

      if scriptType<>stInclude then
         if FConditionalDepth.Count>0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_UnbalancedConditionalDirective);
      if FUnitSymbol<>nil then
         FUnitSymbol.UnParentInterfaceTable;

      if finalToken=ttIMPLEMENTATION then begin
         FUnitContextStack.PushContext(Self);
         FTok:=nil;
      end else begin
         Inc(FLineCount, FTok.CurrentPos.Line-2);
         FreeAndNil(FTok);
      end;

      if scriptType=stMain then begin
         while FUnitContextStack.Count>0 do begin
            oldUnit:=FUnitSymbol;
            FUnitContextStack.PopContext(Self);
            FUnitSection:=secImplementation;
            try
               implemTable:=TUnitImplementationTable.Create(FUnitSymbol);
               oldTable:=FProg.Table;
               FProg.Table:=implemTable;
               try
                  unitBlock:=ReadRootBlock([], finalToken);
                  FreeAndNil(unitBlock);
               finally
                  FProg.Table:=oldTable;
               end;
            finally
               Inc(FLineCount, FTok.CurrentPos.Line-2);
               FreeAndNil(FTok);
               FUnitSymbol:=oldUnit;
            end;
         end;
      end;

      if (Result<>nil) and Optimize then
         Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
   finally
      FTok.Free;
      FTok:=oldTok;
      FUnitSection:=oldSection;
   end;
end;

// ReadRootStatement
//
function TdwsCompiler.ReadRootStatement(var action : TdwsRootStatementAction) : TNoResultExpr;
var
   token : TTokenType;
begin
   action:=rsaNone;
   Result:=nil;
   token:=FTok.TestDeleteAny([ttTYPE, ttPROCEDURE, ttFUNCTION,
                              ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttCLASS,
                              ttIMPLEMENTATION, ttEND]);
   case token of
      ttTYPE :
         ReadTypeDecl;
      ttPROCEDURE :
         ReadProcBody(ReadProcDecl(fkProcedure));
      ttFUNCTION :
         ReadProcBody(ReadProcDecl(fkFunction));
      ttCONSTRUCTOR :
         ReadProcBody(ReadProcDecl(fkConstructor));
      ttDESTRUCTOR :
         ReadProcBody(ReadProcDecl(fkDestructor));
      ttMETHOD :
         ReadProcBody(ReadProcDecl(fkMethod));
      ttCLASS : begin
         token:=FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION, ttMETHOD]);
         case token of
            ttPROCEDURE :
               ReadProcBody(ReadProcDecl(fkProcedure, True));
            ttFUNCTION :
               ReadProcBody(ReadProcDecl(fkFunction, True));
            ttMETHOD :
               ReadProcBody(ReadProcDecl(fkMethod, True));
         else
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
         end;
      end;
      ttIMPLEMENTATION : begin
         if (FProg.Table<>FProg.Root.Table) or (UnitSection<>secInterface) then begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnexpectedSection,
                                      [cTokenStrings[token]]);
            action:=rsaNoSemiColon;
         end else begin
            FUnitSection:=secImplementation;
            DoSectionChanged;
            action:=rsaImplementation;
         end;
      end;
      ttEND : begin
         if (FProg.Table<>FProg.Root.Table) or (UnitSection<>secImplementation) then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnexpectedEnd,
                                      [cTokenStrings[token]])
         else begin
            if FTok.TestDelete(ttDOT) then begin
               FUnitSection:=secEnd;
               action:=rsaEnd;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_DotExpected);
         end;
      end;
   else
      Result:=ReadStatement;
   end;
end;

// ReadStatement
//
function TdwsCompiler.ReadStatement : TNoResultExpr;
var
   token : TTokenType;
   constSym : TConstSymbol;
begin
   Result:=nil;
   token:=Ftok.TestDeleteAny([ttVAR, ttCONST, ttUSES, ttOPERATOR]);
   case token of
      ttVAR :
         Result:=ReadVarDecl;
      ttCONST : begin
         constSym:=ReadConstDecl(TConstSymbol);
         FProg.Table.AddSymbol(constSym);
      end;
      ttUSES :
         ReadUses;
      ttOPERATOR :
         ReadOperatorDecl;
   else
      if (UnitSection<>secMixed) and (FProg.Level=0) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_UnexpectedStatement);
      Result:=ReadBlock
   end;
end;

// Evaluate
//
class function TdwsCompiler.Evaluate(exec : IdwsProgramExecution;
                                     const anExpression : String;
                                     options : TdwsEvaluateOptions = []) : IdwsEvaluateExpr;
var
   oldProgMsgs : TdwsCompileMessageList;
   sourceFile : TSourceFile;
   compiler : TdwsCompiler;
   expr : TTypedExpr;
   resultObj : TdwsEvaluateExpr;
   contextProgram : TdwsProgram;
   config : TdwsConfiguration;
   gotError : Boolean;
begin
   { This will evaluate an expression by tokenizing it evaluating it in the
     Context provided. }

   gotError:=False;
   expr:=nil;
   compiler:=Self.Create;
   try
      if exec=nil then begin
         config:=TdwsConfiguration.Create(nil);
         try
            exec:=compiler.Compile('', config).CreateNewExecution;
         finally
            config.Free;
         end;
      end;
      if (eoRootContext in options) then
         contextProgram:=exec.Prog.ProgramObject
      else begin
         contextProgram:=TdwsProgram((exec.ExecutionObject as TdwsProgramExecution).CurrentProg);
         if contextProgram=nil then
            contextProgram:=exec.Prog.ProgramObject;
      end;
      compiler.FProg:=contextProgram;
      compiler.FMainProg:=contextProgram.Root;
      compiler.FContextMap:=compiler.FMainProg.ContextMap;
      compiler.FSymbolDictionary:=compiler.FMainProg.SymbolDictionary;
      try
         oldProgMsgs:=compiler.FProg.CompileMsgs;
         compiler.FMsgs:=TdwsCompileMessageList.Create;
         compiler.FProg.CompileMsgs:=compiler.FMsgs;
         compiler.FOperators:=(compiler.FMainProg.Operators as TOperators);

         sourceFile:=TSourceFile.Create;
         try
            sourceFile.Code:=anExpression;
            sourceFile.Name:=MSG_MainModule;
            compiler.FTok:=TTokenizer.Create(sourceFile, compiler.FMsgs);
            try
               try
                  expr:=compiler.ReadExpr;
               except
                  on E : Exception do begin
                     gotError:=True;
                     if compiler.FMsgs.Count>0 then
                        expr:=TConstExpr.Create(contextProgram,
                                                contextProgram.TypString,
                                                compiler.FMsgs.AsInfo);
                  end;
               end;
            finally
               FreeAndNil(compiler.FTok);
            end;
         finally
            FreeAndNil(sourceFile);
            compiler.FProg.CompileMsgs:=oldProgMsgs;
            FreeAndNil(compiler.FMsgs);
         end;
      finally
         compiler.FSymbolDictionary:=nil;
         compiler.FContextMap:=nil;
         compiler.FMainProg:=nil;
         compiler.FProg:=nil;
      end;
   finally
      compiler.Free;
   end;

   resultObj:=TdwsEvaluateExpr.Create;
   resultObj.FExecution:=exec;
   if contextProgram is TdwsProcedure then
      resultObj.FContextProcedure:=TdwsProcedure(contextProgram);
   resultObj.FExpression:=expr;
   resultObj.FEvaluationError:=gotError;
   Result:=resultObj;
end;

// ReadVarDecl
//
function TdwsCompiler.ReadVarDecl : TNoResultExpr;
var
   x : Integer;
   names : TStringList;
   sym : TDataSymbol;
   typ : TTypeSymbol;
   pos : TScriptPos;
   posArray : TScriptPosArray;
   initData : TData;
   initExpr : TTypedExpr;
   assignExpr : TAssignExpr;
   constExpr : TConstExpr;
   varExpr : TVarExpr;
begin
   Result := nil;

   names := TStringList.Create;
   initExpr := nil;
   try
      // Conditionally pass in dynamic array
      if coSymbolDictionary in FOptions then
         ReadNameList(names, posArray)     // use overloaded version
      else ReadNameList(names);

      pos := FTok.HotPos;

      if FTok.TestDelete(ttCOLON) then begin

         // explicit typing
         //    var myVar : type
         //    var myVar : type = expr
         //    var myVar : type := expr
         typ := ReadType('', tcVariable);
         if names.Count = 1 then begin
            if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then
               initExpr := ReadExpr(typ)
         end;

      end else if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then begin

         // inferred typing
         //    var myVar = expr
         //    var myVar := expr
         if names.Count<>1 then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
         initExpr:=ReadExpr;
         typ:=initExpr.Typ;

         if typ=nil then begin
            FMsgs.AddCompilerError(pos, CPE_RightSideNeedsReturnType);
            typ:=FProg.TypVariant; // keep going
            FreeAndNil(initExpr);
         end;

      end else begin

         typ := nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      end;

      if (typ is TClassSymbol) and TClassSymbol(typ).IsStatic then
         FMsgs.AddCompilerErrorFmt(pos, CPE_ClassIsStatic, [TClassSymbol(typ).Name]);

      for x := 0 to names.Count - 1 do begin
         CheckName(names[x]);
         sym:=TDataSymbol.Create(names[x], typ);
         FProg.Table.AddSymbol(sym);

         varExpr:=GetVarExpr(sym);
         if Assigned(initExpr) then begin

            // Initialize with an expression

            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddValueSymbol(sym, posArray[x],
                                               [suDeclaration, suReference, suWrite]);

            Result:=CreateAssign(pos, ttASSIGN, varExpr, initExpr);
            initExpr:=nil;

         end else begin

            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddValueSymbol(sym, posArray[x], [suDeclaration]);

            if sym.Typ is TArraySymbol then begin

               // TODO: if Sym.DynamicInit?
               FProg.InitExpr.AddStatement(
                  TInitDataExpr.Create(FProg, Pos, varExpr));

            end else begin

               // Initialize with default value
               if (varExpr.Typ=FProg.TypInteger) or (varExpr.Typ is TEnumerationSymbol) then
                  assignExpr:=TAssignConstToIntegerVarExpr.CreateVal(FProg, pos, varExpr, 0)
               else if varExpr.Typ=FProg.TypFloat then
                  assignExpr:=TAssignConstToFloatVarExpr.CreateVal(FProg, pos, varExpr, 0)
               else if varExpr.Typ=FProg.TypBoolean then
                  assignExpr:=TAssignConstToBoolVarExpr.CreateVal(FProg, pos, varExpr, False)
               else if varExpr.Typ=FProg.TypString then
                  assignExpr:=TAssignConstToStringVarExpr.CreateVal(FProg, pos, varExpr, '')
               else if varExpr.Typ.ClassType=TClassSymbol then
                  assignExpr:=TAssignNilToVarExpr.CreateVal(FProg, pos, varExpr)
               else if varExpr.Typ.ClassType=TClassOfSymbol then
                  assignExpr:=TAssignNilClassToVarExpr.CreateVal(FProg, pos, varExpr)
               else if varExpr.Typ is TFuncSymbol then
                  assignExpr:=TAssignNilToVarExpr.CreateVal(FProg, pos, varExpr)
               else begin
                  initData := nil;
                  SetLength(initData, sym.Typ.Size);
                  TDataSymbol(sym).Typ.InitData(initData, 0);

                  constExpr:=TConstExpr.CreateTyped(FProg, sym.Typ, initData);
                  assignExpr:=TAssignConstDataToVarExpr.Create(FProg, pos, varExpr, constExpr);
               end;
               FProg.InitExpr.AddStatement(assignExpr);

            end;

         end;
      end;
  finally
      initExpr.Free;
      names.Free;
  end;
end;

// ReadConstDecl
//
function TdwsCompiler.ReadConstDecl(constSymbolClass : TConstSymbolClass) : TConstSymbol;
var
   name : String;
   expr : TTypedExpr;
   typ : TTypeSymbol;
   sas : TStaticArraySymbol;
   detachTyp : Boolean;
   constPos : TScriptPos;
   val : Variant;
   recordData : TData;
begin
   if not FTok.TestDeleteNamePos(name, constPos) then begin

      Result:=nil; // warning workaround
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   end else begin

      CheckName(name);

      if FTok.TestDelete(ttCOLON) then
         typ:=ReadType('', tcConstant)
      else typ:=nil;

      if typ is TFuncSymbol then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InvalidConstType, [typ.Caption]);

      if not FTok.TestDelete(ttEQ) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

      if typ is TRecordSymbol then begin
         recordData:=ReadConstRecord(TRecordSymbol(typ));
         Result:=constSymbolClass.Create(name, typ, recordData, 0);
      end else begin
         detachTyp:=False;
         expr:=ReadExpr;
         try
            if Assigned(typ) then begin
               if not typ.IsCompatible(expr.typ) then
                  IncompatibleTypes(FTok.HotPos, CPE_AssignIncompatibleTypes, expr.typ, typ);
            end else begin
               typ:=expr.typ;
               detachTyp:=(typ.Name='');
            end;

            if (expr=nil) or (not expr.IsConstant) then begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
               // keep compiling
               Result:=constSymbolClass.Create(name, typ, Null);
               Exit;
            end;

            if typ is TArraySymbol then begin
               sas:=TStaticArraySymbol.Create('', typ, FProg.TypInteger, 0, TArraySymbol(typ).typ.Size-1);
               FProg.Table.AddSymbol(sas);
               Result:=constSymbolClass.Create(name, sas, (expr as TArrayConstantExpr).EvalAsTData(FExec), 0);
            end else if typ.Size>1 then
               Result:=constSymbolClass.Create(name, typ, TConstExpr(expr).Data[FExec], TConstExpr(expr).Addr[FExec])
            else begin
               expr.EvalAsVariant(FExec, val);
               Result:=constSymbolClass.Create(name, typ, val);
            end;

            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddConstSymbol(Result, constPos, [suDeclaration]);
         finally
            if detachTyp then begin
               FProg.Table.AddSymbol(typ);
               expr.Typ:=nil;
            end;
            expr.Free;
         end;
      end;
   end;
end;

// ReadTypeDecl
//
procedure TdwsCompiler.ReadTypeDecl;
var
   name : String;
   typNew, typOld : TTypeSymbol;
   typePos : TScriptPos;
   oldSymPos : TSymbolPosition; // Mark *where* the old declaration was
begin
   if not FTok.TestDeleteNamePos(name, typePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

   typOld := FProg.Table.FindTypeSymbol(name, cvMagic);
   oldSymPos := nil;
   if coSymbolDictionary in FOptions then begin
      if Assigned(typOld) then
         oldSymPos := FSymbolDictionary.FindSymbolUsage(typOld, suDeclaration);  // may be nil
   end;

   typNew := ReadType(name, tcDeclaration);

   // Wrap whole type declarations in a context.
   if coContextMap in FOptions then
      FContextMap.OpenContext(typePos, typNew);

   try
      if typNew.Name<>'' then begin
         try
            // typOld = typNew if a forwarded class declaration was overwritten
            if typOld <> typNew then begin
               CheckName(name);
               if typNew.Name<>'' then
                  FProg.Table.AddSymbol(typNew);
            end  else begin
               // Handle overwriting forwards in Dictionary
               // Original symbol was a forward. Update symbol entry
               // If the type is in the SymbolDictionary (disabled dictionary would leave pointer nil),
               if Assigned(oldSymPos) then              // update original position information
                  oldSymPos.SymbolUsages := [suForward]; // update old postion to reflect that the type was forwarded
            end;

            // Add symbol position as being the type being declared (works for forwards too)
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddTypeSymbol(typNew, typePos, [suDeclaration]);
         except
            typNew.Free;
            raise;
         end;
      end;
   finally
      if coContextMap in FOptions then
         FContextMap.CloseContext(FTok.CurrentPos);
   end;
end;

// ReadProcDecl
//
function TdwsCompiler.ReadProcDecl(funcKind : TFuncKind;
              isClassMethod : Boolean = False; isType : Boolean = False) : TFuncSymbol;
var
   name : String;
   sym : TSymbol;
   funcPos : TScriptPos;
   forwardedSym : TFuncSymbol;
   forwardedSymPos : TSymbolPosition;
begin
   if not isType then begin
      // Find Symbol for Functionname
      if not FTok.TestDeleteNamePos(name, funcPos) then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
         name:='';
      end;
      CheckSpecialName(name);

      sym := FProg.Table.FindSymbol(name, cvMagic);

      // Open context for procedure declaration. Closed in ReadProcBody.
      if coContextMap in FOptions then
         FContextMap.OpenContext(funcPos, sym);
   end else begin
      sym := nil;
      name := '';
   end;

   // name is the name of class -> Method
   if sym is TClassSymbol then begin

      // Store reference to class in dictionary
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.AddTypeSymbol(TClassSymbol(sym), funcPos, [suReference]);
      Result := ReadMethodImpl(TClassSymbol(sym), funcKind, isClassMethod);

   end else begin

      // Read normal procedure/function declaration
      if isClassMethod or (funcKind in [fkConstructor, fkDestructor, fkMethod]) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ImplClassNameExpected);

      if     (sym is TFuncSymbol)
         and TFuncSymbol(sym).IsForwarded
         and (FUnitSymbol.HasSymbol(sym) or FProg.Table.HasSymbol(sym)) then
         // There was already a (forward) declaration
         forwardedSym := TFuncSymbol(sym)
      else forwardedSym := nil;

      if not Assigned(forwardedSym) then
         CheckName(name);

      if isType then
         Result := TSourceFuncSymbol.Create('', funcKind, -1)
      else Result := TSourceFuncSymbol.Create(name, funcKind, FMainProg.NextStackLevel(FProg.Level));
      try
         ReadParams(Result.AddParam, forwardedSym=nil);  // Don't add params to dictionary when function is forwarded. It is already declared.

         Result.Typ:=ReadFuncResultType(funcKind);

         if not isType then begin
            if Assigned(forwardedSym) then begin
               CompareFuncKinds(forwardedSym.Kind, Result.Kind);
               CompareFuncSymbolParams(forwardedSym, Result);
            end;

            // forward & external declarations
            if not Assigned(forwardedSym) then begin
               if FTok.Test(ttSEMI) then begin
                  if UnitSection=secInterface then begin
                     // default to forward in interface section
                     Result.SetForwardedPos(funcPos);
                     if FTok.NextTest(ttFORWARD) then begin
                        FMsgs.AddCompilerHint(FTok.HotPos, CPW_ForwardIsImplicit);
                        FTok.KillToken; // SEMI
                        FTok.TestDelete(ttFORWARD);
                     end;
                     if FTok.Test(ttSEMI) then
                        if FTok.NextTest(ttDEPRECATED) then
                           FTok.KillToken
                  end else begin
                     FTok.KillToken; // SEMI
                     if FTok.TestDelete(ttFORWARD) then begin
                        Result.SetForwardedPos(funcPos);
                     end else if FTok.TestDelete(ttEXTERNAL) then begin
                        Result.IsExternal:=True;
                     end;
                  end;
               end;
            end else ReadSemiColon;

            ReadDeprecated(Result);

            if Assigned(forwardedSym) then begin
               // Get forwarded position in script. If compiled without symbols it will just return from empty list (could optimize here to prevent the push/pop of call stack
               forwardedSymPos := FSymbolDictionary.FindSymbolUsage(forwardedSym, suDeclaration);  // may be nil
               // Adapt dictionary entry to reflect that it was a forward
               // If the record is in the SymbolDictionary (disabled dictionary would leave pointer nil)
               if Assigned(forwardedSymPos) then
                  forwardedSymPos.SymbolUsages := [suForward];  // update old postion to reflect that the type was forwarded

               Result.Free;
               Result := forwardedSym;
               Result.ClearIsForwarded;
            end else FProg.Table.AddSymbol(Result);
         end;

         // Procedure is both Declared and Implemented here
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddTypeSymbol(Result, funcPos, [suDeclaration, suImplementation]);
      except
         // Remove reference to symbol (gets freed)
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.Remove(Result);
         Result.Free;
         raise;
      end;
   end;
end;

// ReadIntfMethodDecl
//
function TdwsCompiler.ReadIntfMethodDecl(intfSym : TInterfaceSymbol; funcKind : TFuncKind) : TSourceMethodSymbol;
var
   name : String;
   sym : TSymbol;
   methPos : TScriptPos;
begin
   // Find Symbol for Functionname
   if not FTok.TestDeleteNamePos(name, methPos) then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
      name:='';
   end;

   // Check if name is already used
   sym:=intfSym.Members.FindSymbolFromScope(name, intfSym);
   if sym<>nil then
      MemberSymbolWithNameAlreadyExists(sym);

   // Read declaration of method implementation
   Result:=TSourceMethodSymbol.Create(name, funcKind, intfSym, cvPublished, False);
   Result.DeclarationPos:=methPos;

   try
      ReadParams(Result.AddParam);

      Result.Typ:=ReadFuncResultType(funcKind);
      ReadSemiColon;

      // Added as last step. OnExcept, won't need to be freed.
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.AddTypeSymbol(Result, methPos, [suDeclaration]);
   except
      Result.Free;
      raise;
   end;
end;

// ReadMethodDecl
//
function TdwsCompiler.ReadMethodDecl(classSym: TClassSymbol; funcKind: TFuncKind;
   aVisibility : TdwsVisibility; isClassMethod: Boolean): TSourceMethodSymbol;

   function OverrideParamsCheck(newMeth, oldMeth : TMethodSymbol) : Boolean;
   var
      i : Integer;
      oldParam, newParam : TSymbol;
   begin
      if newMeth.Params.Count<>oldMeth.Params.Count then
         Exit(False);
      for i:=0 to newMeth.Params.Count-1 do begin
         newParam:=newMeth.Params[i];
         oldParam:=oldMeth.Params[i];
         if    (newParam.Typ<>oldParam.Typ)
            or (not UnicodeSameText(newParam.Name, oldParam.Name)) then
            Exit(False);
      end;
      Result:=True;
   end;

var
   name : String;
   sym : TSymbol;
   meth, defaultConstructor : TMethodSymbol;
   isReintroduced : Boolean;
   methPos: TScriptPos;
   qualifier : TTokenType;
begin
   // Find Symbol for Functionname
   if not FTok.TestDeleteNamePos(name, methPos) then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
      name:='';
   end;

   // Check if name is already used
   sym := classSym.Members.FindSymbolFromScope(name, classSym);
   if sym<>nil then begin
      if sym is TMethodSymbol then begin
         meth:=TMethodSymbol(sym);
         if meth.StructSymbol = classSym then
         MemberSymbolWithNameAlreadyExists(meth);
      end else begin
         MemberSymbolWithNameAlreadyExists(sym);
         meth:=nil;
      end;
   end else meth:=nil;

   if classSym.IsStatic and (not IsClassMethod) then
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ClassIsStatic, [classSym.Name]);

   // Read declaration of method implementation
   Result := TSourceMethodSymbol.Create(name, funcKind, classSym, aVisibility, isClassMethod);
   Result.DeclarationPos:=methPos;

   try
      if meth<>nil then begin
         Result.SetOverlap(meth);
         isReintroduced := meth.IsVirtual;
      end else isReintroduced := False;

      ReadParams(Result.AddParam);

      Result.Typ:=ReadFuncResultType(funcKind);
      ReadSemiColon;

      qualifier:=FTok.TestDeleteAny([ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttABSTRACT]);
      if qualifier<>ttNone then begin
         case qualifier of
            ttVIRTUAL : begin
               Result.IsVirtual := True;
               if FTok.Test(ttSEMI) and FTok.NextTest(ttABSTRACT) then begin
                  FTok.KillToken;
                  FTok.TestDelete(ttABSTRACT);
                  Result.IsAbstract := True;
               end;
            end;
            ttOVERRIDE : begin
               if meth=nil then
                  FMsgs.AddCompilerErrorFmt(methPos, CPE_CantOverrideNotInherited, [name])
               else if not meth.IsVirtual then
                  FMsgs.AddCompilerErrorFmt(methPos, CPE_CantOverrideNotVirtual, [name])
               else begin
                  if Result.Kind<>meth.Kind then
                     FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_CantOverrideWrongFuncKind,
                                               [cFuncKindToString[meth.Kind],
                                                cFuncKindToString[Result.Kind]])
                  else if Result.IsClassMethod<>meth.IsClassMethod then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_CantOverrideWrongMethodType)
                  else if    ((Result.Typ=nil) and (meth.Typ<>nil))
                          or ((Result.Typ<>nil) and not Result.Typ.IsOfType(meth.Typ)) then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_CantOverrideWrongResultType)
                  else if not OverrideParamsCheck(Result, meth) then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_CantOverrideWrongParameterList)
                  else if meth.IsFinal then
                     FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_CantOverrideFinal, [name]);
                  Result.SetOverride(meth);
                  isReintroduced := False;
               end;
            end;
            ttREINTRODUCE : begin
               if not isReintroduced then
                  FMsgs.AddCompilerErrorFmt(methPos, CPE_CantReintroduce, [name]);
               isReintroduced := False;
            end;
            ttABSTRACT : begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_NonVirtualAbstract);
            end;
         end;

         ReadSemiColon;
      end;

      if FTok.TestDelete(ttDEFAULT) then begin
         if funcKind<>fkConstructor then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NonConstructorDefault)
         else begin
            defaultConstructor:=classSym.FindDefaultConstructor(cvMagic);
            if (defaultConstructor<>nil) and (defaultConstructor.StructSymbol=classSym) then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_DefaultConstructorAlreadyDefined,
                                         [classSym.Name, defaultConstructor.Name])
            else Result.IsDefault:=True;
         end;
         ReadSemiColon;
      end;

      if FTok.TestDelete(ttFINAL) then begin
         if not Result.IsOverride then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_CantFinalWithoutOverride)
         else Result.SetIsFinal;
         ReadSemiColon;
      end;

      ReadDeprecated(Result);

      if isReintroduced then
         FMsgs.AddCompilerWarningFmt(methPos, CPE_ReintroduceWarning, [name]);

      // Added as last step. OnExcept, won't need to be freed.
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.AddTypeSymbol(Result, methPos, [suDeclaration]);
   except
      Result.Free;
      raise;
   end;

   if FTok.Test(ttBEGIN) then begin
      // inline declaration
      try
         if coContextMap in FOptions then
            FContextMap.OpenContext(FTok.HotPos, Result);
         ReadProcBody(Result);
         ReadSemiColon;
      except
         Result.Free;
         raise;
      end;
   end;
end;

// ReadMethodImpl
//
function TdwsCompiler.ReadMethodImpl(classSym : TClassSymbol;
               funcKind : TFuncKind; isClassMethod : Boolean) : TMethodSymbol;
var
   methName : String;
   sym : TSymbol;
   tmpMeth : TMethodSymbol;
   methPos : TScriptPos;
begin
   if not (FTok.TestDelete(ttDOT) and FTok.TestName) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   methName := FTok.GetToken.FString;
   methPos := FTok.HotPos;
   FTok.KillToken;
   FTok.Test(ttBLEFT);

   sym := classSym.Members.FindSymbol(methName, cvPrivate);

   if not (sym is TMethodSymbol) then
      FMsgs.AddCompilerStop(methPos, CPE_ImplNotAMethod);
   Result:=TMethodSymbol(sym);

   if Result.StructSymbol<>classSym then begin
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplInvalidClass, [methName, classSym.Name]);
      classSym:=Result.StructSymbol as TClassSymbol;
   end;

   if Result.IsAbstract then
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplAbstract, [classSym.Name, methName]);

   if Result.IsClassMethod<>isClassMethod then begin
      if Result.IsClassMethod then
         FMsgs.AddCompilerError(methPos, CPE_ImplClassExpected)
      else FMsgs.AddCompilerError(methPos, CPE_ImplNotClassExpected);
      isClassMethod:=Result.IsClassMethod;
   end;

   CompareFuncKinds(Result.Kind, funcKind);

   if not FTok.TestDelete(ttSEMI) then begin
      tmpMeth:=TSourceMethodSymbol.Create(methName, funcKind, classSym,
                                          TMethodSymbol(Result).Visibility, isClassMethod);
      try
         ReadParams(tmpMeth.AddParam, False);  // Don't store these params to Dictionary. They will become invalid when the method is freed.
         tmpMeth.Typ:=ReadFuncResultType(funcKind);
         ReadSemiColon;
         CompareFuncSymbolParams(Result, tmpMeth);
      finally
         tmpMeth.Free;
      end;
   end;

   if coSymbolDictionary in FOptions then
      FSymbolDictionary.AddTypeSymbol(Result, methPos, [suImplementation]);
end;

// ReadDeprecated
//
procedure TdwsCompiler.ReadDeprecated(funcSym : TFuncSymbol);
begin
   if FTok.TestDelete(ttDEPRECATED) then begin
      if FTok.Test(ttStrVal) then begin
         funcSym.DeprecatedMessage:=FTok.GetToken.FString;
         FTok.KillToken;
      end else funcSym.IsDeprecated:=True;
      if UnitSection<>secInterface then
         ReadSemiColon;
   end;
end;

// WarnDeprecated
//
procedure TdwsCompiler.WarnDeprecated(funcSym : TFuncSymbol);
begin
   if FuncSym.IsDeprecated then begin
      if FuncSym.DeprecatedMessage<>'!' then
         FMsgs.AddCompilerWarningFmt(FTok.HotPos, CPW_DeprecatedWithMessage,
                                     [FuncSym.Name, FuncSym.DeprecatedMessage])
      else FMsgs.AddCompilerWarningFmt(FTok.HotPos, CPW_Deprecated, [FuncSym.Name]);
   end;
end;

// ReadProcBody
//
procedure TdwsCompiler.ReadProcBody(funcSymbol : TFuncSymbol);
var
   oldprog : TdwsProgram;
   proc : TdwsProcedure;
   assignExpr : TNoResultExpr;
   sectionType, finalToken : TTokenType;
   constSym : TConstSymbol;
begin
   // Stop if declaration was forwarded or external
   if (funcSymbol.IsForwarded or funcSymbol.IsExternal) then begin
      // Closed context of procedure (was only a forward)
      if coContextMap in FOptions then
         FContextMap.CloseContext(FTok.HotPos);
      Exit;
   end;

   if funcSymbol.Executable<>nil then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MethodRedefined, [funcSymbol.Name]);

   if UnitSection=secInterface then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_UnexpectedImplementationInInterface);

   // Open context of full procedure body (may include a 'var' section)
   if coContextMap in FOptions then
      FContextMap.OpenContext(FTok.CurrentPos, funcSymbol);   // attach to symbol that it belongs to (perhaps a class)

   funcSymbol.SourcePosition:=FTok.HotPos;

   try
      // Function Body
      oldprog:=FProg;
      proc:=CreateProcedure(FProg);
      FProg:=proc;
      try
         FMainProg.Compiler := Self;
         TdwsProcedure(FProg).AssignTo(funcSymbol);
         // Set the current context's LocalTable to be the table of the new procedure
         if coContextMap in FOptions then
            FContextMap.Current.LocalTable := FProg.Table;

         if FTok.TestDelete(ttREQUIRE) then begin
            if funcSymbol is TMethodSymbol then begin
               proc.PreConditions:=TSourceMethodPreConditions.Create(proc);
               if TMethodSymbol(funcSymbol).IsOverride then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_PreconditionsMustBeInRootMethod);
            end else proc.PreConditions:=TSourcePreConditions.Create(proc);
            ReadConditions(funcSymbol, proc.PreConditions, TPreConditionSymbol);
         end else if funcSymbol is TMethodSymbol then begin
            if TMethodSymbol(funcSymbol).HasConditions then
               proc.PreConditions:=TSourceMethodPreConditions.Create(proc);
         end;

         // Read local variable declarations
         if FTok.Test(ttVAR) or FTok.Test(ttCONST) then begin
            // Read names of local variable and constants
            sectionType:=ttNone;
            repeat

               if FTok.TestDelete(ttVAR) then
                  sectionType:=ttVAR
               else if FTok.TestDelete(ttCONST) then
                  sectionType:=ttCONST;

               if sectionType=ttVAR then begin
                  assignExpr:=ReadVarDecl;
                  if assignExpr<>nil then
                     FProg.InitExpr.AddStatement(assignExpr);
               end else if sectionType=ttCONST then begin
                  constSym:=ReadConstDecl(TConstSymbol);
                  FProg.Table.AddSymbol(constSym);
               end;

               ReadSemiColon;

            until FTok.Test(ttBEGIN);
         end;

         if coContextMap in FOptions then
            FContextMap.OpenContext(FTok.CurrentPos, nil);
         try
            // Read procedure body
            if not FTok.TestDelete(ttBEGIN) then begin
               if FTok.Test(ttFORWARD) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_FuncForwardAlreadyExists)
               else FMsgs.AddCompilerStop(FTok.HotPos, CPE_BeginExpected);
            end;

            FProg.Expr:=ReadRootBlock([ttEND, ttENSURE], finalToken);
            if Optimize then
               FProg.Expr:=FProg.Expr.OptimizeToNoResultExpr(FProg, FExec);

            if finalToken=ttENSURE then begin
               if funcSymbol is TMethodSymbol then
                  proc.PostConditions:=TSourceMethodPostConditions.Create(proc)
               else proc.PostConditions:=TSourcePostConditions.Create(proc);
               ReadPostConditions(funcSymbol, proc.PostConditions, TPostConditionSymbol);
               if FTok.TestDelete(ttEND) then
                  finalToken:=ttEND;
            end else if funcSymbol is TMethodSymbol then begin
               if TMethodSymbol(funcSymbol).HasConditions then
                  proc.PostConditions:=TSourceMethodPostConditions.Create(proc);
            end;

            if finalToken<>ttEND then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndOfBlockExpected);

            HintUnusedSymbols;
            HintUnusedResult(proc.Func.Result);
         finally
            if coContextMap in FOptions then
               FContextMap.CloseContext(FTok.CurrentPos);  // close with inside procedure end
         end;
      finally
         FMainProg.Compiler := nil;
         FProg := oldprog;
      end;
   finally
      // Closed procedure body and procedure implementation (from declaration to body)
      if coContextMap in FOptions then begin
         FContextMap.CloseContext(FTok.CurrentPos);  // closed begin..end body (may include 'var' section)
         FContextMap.CloseContext(FTok.CurrentPos);  // closed from declaration through implementation
      end;
   end;
end;

// ReadConditions
//
procedure TdwsCompiler.ReadConditions(funcSymbol : TFuncSymbol; conditions : TSourceConditions;
                                      condsSymClass : TConditionSymbolClass);
var
   hotPos : TScriptPos;
   testExpr, msgExpr : TTypedExpr;
   testStart : PChar;
   testLength : Integer;
   msg : String;
   srcCond : TSourceCondition;
begin
   repeat

      testStart:=FTok.PosPtr;
      FTok.Test(ttNone);
      hotPos:=FTok.HotPos;

      msgExpr:=nil;
      testExpr:=ReadExpr(FProg.TypBoolean);
      try
         if not testExpr.IsOfType(FProg.TypBoolean) then
            FMsgs.AddCompilerError(hotPos, CPE_BooleanExpected);
         if Optimize then
            testExpr:=testExpr.OptimizeToTypedExpr(FProg, FExec);
         if testExpr.IsConstant then
            FMsgs.AddCompilerWarning(hotPos, CPW_ConstantCondition);

         testLength:=(NativeUInt(FTok.PosPtr)-NativeUInt(testStart)) div 2;
         if FTok.TestDelete(ttCOLON) then begin
            msgExpr:=ReadExpr;
            if not msgExpr.IsOfType(FProg.TypString) then
               FMsgs.AddCompilerError(hotPos, CPE_StringExpected);
            if Optimize then
               msgExpr:=msgExpr.OptimizeToTypedExpr(FProg, FExec);
         end else begin
            SetString(msg, testStart, testLength);
            msg:=Trim(msg);
            if (msg<>'') and (msg[Length(msg)]=';') then
               SetLength(msg, Length(msg)-1);
            msgExpr:=TConstStringExpr.CreateUnified(FProg, FProg.TypString, msg);
         end;

         ReadSemiColon;

         srcCond:=TSourceCondition.Create(hotPos, testExpr, msgExpr);
         conditions.AddCondition(srcCond);
         funcSymbol.AddCondition(condsSymClass.Create(hotPos, srcCond, srcCond));
      except
         testExpr.Free;
         msgExpr.Free;
         raise;
      end;

   until FTok.TestAny([ttVAR, ttCONST, ttBEGIN, ttEND, ttENSURE, ttREQUIRE,
                       ttFUNCTION, ttPROCEDURE, ttTYPE])<>ttNone;
end;

// ReadPostConditions
//
procedure TdwsCompiler.ReadPostConditions(funcSymbol : TFuncSymbol; conditions : TSourcePostConditions;
                                          condsSymClass : TConditionSymbolClass);
begin
   if conditions is TSourcePostConditions then
      FSourcePostConditionsIndex:=1;
   try
      ReadConditions(funcSymbol, conditions, condsSymClass);
   finally
      FSourcePostConditionsIndex:=0;
   end;
end;

// ReadOperatorDecl
//
function TdwsCompiler.ReadOperatorDecl : TOperatorSymbol;
var
   tt : TTokenType;
   usesName : String;
   opPos, usesPos : TScriptPos;
   sym : TTypeSymbol;
   usesSym : TFuncSymbol;
   typ : TTypeSymbol;
begin
   opPos:=FTok.HotPos;
   tt:=FTok.TestDeleteAny([ttPLUS, ttMINUS, ttTIMES, ttDIVIDE, ttMOD, ttDIV,
                           ttOR, ttAND, ttXOR, ttIMPLIES, ttSHL, ttSHR,
                           ttEQ, ttNOTEQ, ttGTR, ttGTREQ, ttLESS, ttLESSEQ,
                           ttLESSLESS, ttGTRGTR, ttCARET]);
   if tt=ttNone then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_OverloadableOperatorExpected);

   Result:=TOperatorSymbol.Create(tt);
   try
      if not FTok.TestDelete(ttBLEFT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

      repeat
         typ:=ReadType('', tcOperand);
         Result.AddParam(typ);
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

      if Length(Result.Params)<>2 then begin
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadNumberOfParameters, [2, Length(Result.Params)]);
         Result.Token:=ttNone;  // nerf the operator
      end;

      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      Result.Typ:=ReadType('', tcOperand);

      if not FTok.TestDelete(ttUSES) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UsesExpected);

      usesSym:=nil;
      if not FTok.TestDeleteNamePos(usesName, usesPos) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected)
      else begin
         sym:=FProg.Table.FindTypeSymbol(usesName, cvPublic);
         if (sym=nil) or sym.IsType or not (sym is TFuncSymbol) then
            FMsgs.AddCompilerError(usesPos, CPE_FunctionMethodExpected)
         else usesSym:=TFuncSymbol(sym);
      end;

      // TODO: typecheck used function
      if usesSym<>nil then begin
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddTypeSymbol(usesSym, usesPos);

         if usesSym.Typ<>Result.Typ then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadResultType, [Result.Typ.Caption])
         else if usesSym.Params.Count<>2 then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadNumberOfParameters, [2, usesSym.Params.Count])
         else if not usesSym.Params[0].Typ.IsOfType(Result.Params[0]) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType, [0, Result.Params[0].Caption, usesSym.Params[0].Typ.Caption])
         else if usesSym.Params[0] is TVarParamSymbol then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [0])
         else if not usesSym.Params[1].Typ.IsOfType(Result.Params[1]) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType, [1, Result.Params[1].Caption, usesSym.Params[1].Typ.Caption])
         else if usesSym.Params[1] is TVarParamSymbol then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [1])
         else Result.UsesSym:=usesSym;
      end;
   except
      Result.Free;
      raise;
   end;

   if (Result.Token<>ttNone) and (Result.UsesSym<>nil) then begin
      FProg.Table.AddSymbol(Result);
      if FProg.Table<>FProg.Root.RootTable then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_OverloadOnlyInGlobalScope)
      else if not FOperators.RegisterOperator(Result) then
         FMsgs.AddCompilerError(opPos, CPE_OverloadAlreadyExists);
   end else FreeAndNil(Result);
end;

// ReadBlocks
//
function TdwsCompiler.ReadBlocks(const endTokens: TTokenTypes; var finalToken: TTokenType): TNoResultExpr;
var
   stmt : TNoResultExpr;
   oldTable : TSymbolTable;
   token : TToken;
   closePos : TScriptPos; // Position at which the ending token was found (for context)
   blockExpr : TBlockExpr;
   sym : TSymbol;
   reach : TReachStatus;
begin
   // Read a block of instructions enclosed in "begin" and "end"
   reach:=rsReachable;
   blockExpr:=TBlockExpr.Create(FProg, FTok.HotPos);
   try
      if coContextMap in FOptions then begin
         FContextMap.OpenContext(FTok.CurrentPos, nil);
         closePos:=FTok.CurrentPos;     // default to close context where it opened (used on errors)
      end;

      oldTable:=FProg.Table;
      FProg.Table:=blockExpr.Table;
      try
         // Add local table to context for the new block
         if coContextMap in FOptions then
            FContextMap.Current.LocalTable:=FProg.Table;

         repeat

            if not FTok.HasTokens then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndOfBlockExpected);

            if FTok.GetToken.FTyp in EndTokens then begin
               finalToken:=FTok.GetToken.FTyp;
               closePos:=FTok.GetToken.FPos;    // get start position of ending token
               FTok.KillToken;
               Break;
            end;

            if reach=rsUnReachable then begin
               reach:=rsUnReachableWarned;
               FMsgs.AddCompilerWarning(FTok.CurrentPos, CPW_UnReachableCode);
            end;

            stmt:=ReadStatement;

            if Assigned(stmt) then begin
               blockExpr.AddStatement(stmt);

               if     (reach=rsReachable)
                  and (   (stmt is TFlowControlExpr)
                       or (stmt is TRaiseExpr)) then
                  reach:=rsUnReachable;
            end;

            if not FTok.TestDelete(ttSEMI) then begin
               token:=FTok.GetToken;
               if (token=nil) or (not (token.FTyp in EndTokens)) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
            end;

         until False;

         HintUnusedSymbols;
      finally
         FProg.Table:=oldTable;
         if coContextMap in FOptions then
            FContextMap.CloseContext(closePos);   // get to end of block
      end;

      if Optimize then
         Result:=blockExpr.OptimizeToNoResultExpr(FProg, FExec)
      else Result:=blockExpr;

   except
      // Remove any symbols in the expression's table. Table will be freed.
      if coSymbolDictionary in FOptions then
         for sym in blockExpr.Table do
            FSymbolDictionary.Remove(sym);
      blockExpr.Free;
      raise;
   end;
end;

// ReadBlock
//
function TdwsCompiler.ReadBlock: TNoResultExpr;
var
   tt: TTokenType;
begin
   Result := nil;
   if FTok.TestDelete(ttBEGIN) then begin
      Result:=ReadBlocks([ttEND], tt);
   end else if FTok.HasTokens then begin
      // Read a single instruction
      Result:=ReadInstr;
   end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndOfBlockExpected);
end;

// ReadInstr
//
function TdwsCompiler.ReadInstr : TNoResultExpr;
var
   token : TTokenType;
   locExpr : TProgramExpr;
   hotPos : TScriptPos;
begin
   if Assigned(FOnReadInstr) then begin
      Result:=FOnReadInstr(Self);
      if Result<>nil then Exit;
   end;

   // Decide which instruction to read
   case FTok.TestDeleteAny([ttIF, ttCASE, ttFOR, ttWHILE, ttREPEAT, ttBREAK,
                            ttEXIT, ttTRY, ttRAISE, ttCONTINUE]) of
      ttIF :
         Result := ReadIf;
      ttCASE :
         Result := ReadCase;
      ttFOR :
         Result := ReadFor;
      ttWHILE :
         Result := ReadWhile;
      ttREPEAT :
         Result := ReadRepeat;
      ttBREAK : begin
         if FLoopExprs.Count=0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_BreakOutsideOfLoop)
         else if (FFinallyExprs.Count>0) and FFinallyExprs.Peek then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_BreakContinueInFinally);
         Result := TBreakExpr.Create(FProg, FTok.HotPos);
         MarkLoopExitable(leBreak);
      end;
      ttEXIT : begin
         if FFinallyExprs.Count>0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExitInFinally);
         Result := ReadExit;
         MarkLoopExitable(leExit);
      end;
      ttTRY :
         Result := ReadTry;
      ttCONTINUE : begin
         if FLoopExprs.Count=0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ContinueOutsideOfLoop)
         else if (FFinallyExprs.Count>0) and FFinallyExprs.Peek then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_BreakContinueInFinally);
         Result := TContinueExpr.Create(FProg, FTok.HotPos);
      end;
      ttRAISE :
         Result := ReadRaise;
   else
      // Try to read a function call, method call or an assignment
      if FTok.Test(ttSWITCH) then
         Result := ReadInstrSwitch(False)
      else if (FTok.TestAny([ttBLEFT, ttINHERITED, ttNEW])<>ttNone) or FTok.TestName then begin // !! TestName must be the last !!
         hotPos:=FTok.HotPos;
         if FTok.Test(ttBLEFT) then // (X as TY)
            locExpr := ReadSymbol(ReadTerm(True))
         else locExpr := ReadName(True);
         if (FTok.TestAny([ttLESSLESS, ttGTRGTR])<>ttNone) and (locExpr is TTypedExpr) then
            locExpr:=ReadExprAdd(nil, TTypedExpr(locExpr));
         try
            token:=FTok.TestDeleteAny(cAssignmentTokens);
            if token<>ttNone then begin
               if not (locExpr is TDataExpr) then begin
                  FMsgs.AddCompilerError(hotPos, CPE_CantWriteToLeftSide);
                  FreeAndNil(locExpr);
                  ReadExpr.Free; // keep compiling
                  Result:=nil;
               end else begin
                  if not TDataExpr(locExpr).IsWritable then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_CantWriteToLeftSide);
                  if locExpr is TVarExpr then
                     WarnForVarUsage(TVarExpr(locExpr), hotPos);
                  Result := ReadAssign(token, TDataExpr(locExpr));
               end;
            end else if locExpr is TAssignExpr then
               Result:=TAssignExpr(locExpr)
            else if    (locExpr is TFuncExprBase)
                    or (locExpr is TConnectorCallExpr) then begin
               Result:=TNoResultWrapperExpr.Create(FProg, (locExpr as  TPosDataExpr).Pos, locExpr);
               if locExpr.IsConstant then begin
                  if not FMsgs.LastMessagePos.SamePosAs(hotPos) then   // avoid hint on calls with issues
                     FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
               end;
            end else if locExpr is TConnectorWriteExpr then
               Result:=TConnectorWriteExpr(locExpr)
            else if locExpr is TDynamicArraySetExpr then
               Result:=TDynamicArraySetExpr(locExpr)
            else if locExpr is TStringArraySetExpr then
               Result:=TStringArraySetExpr(locExpr)
            else if locExpr is TArrayPseudoMethodExpr then
               Result:=TArrayPseudoMethodExpr(locExpr)
            else if locExpr is TConstExpr then begin
               FreeAndNil(locExpr);
               Result:=TNullExpr.Create(FProg, hotPos);
               FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
            end else if locExpr is TNullExpr then begin
               Result:=TNullExpr(locExpr);
               locExpr:=nil;
            end else if locExpr is TAssertExpr then begin
               Result:=TAssertExpr(locExpr);
               locExpr:=nil;
            end else begin
               Result:=nil;
               FMsgs.AddCompilerStop(hotPos, CPE_InvalidInstruction)
            end;
         except
            locExpr.Free;
            raise;
         end;
      end else begin
         Result := TNullExpr.Create(FProg, FTok.HotPos);
      end;
   end;
end;

// ReadInherited
//
function TdwsCompiler.ReadInherited(isWrite : Boolean) : TProgramExpr;
var
   name : string;
   sym : TSymbol;
   methSym : TMethodSymbol;
   classSym, parentSym : TClassSymbol;
   varExpr : TDataExpr;
begin
   Result := nil;
   if not ((FProg is TdwsProcedure) and (TdwsProcedure(FProg).Func is TMethodSymbol)) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedOnlyAllowedInMethods);

   methSym := TMethodSymbol(TdwsProcedure(FProg).Func);
   classSym := methSym.StructSymbol as TClassSymbol;
   parentSym := ClassSym.Parent;
   sym := nil;

   if FTok.TestName then begin
      name := FTok.GetToken.FString;
      FTok.KillToken;
      sym := ParentSym.Members.FindSymbol(name, cvPrivate);
   end else if not methSym.IsOverride then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName)
   else sym := methSym.ParentMeth;

   if Assigned(sym) then begin
      if sym is TMethodSymbol then begin
         if TMethodSymbol(sym).IsAbstract then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_AbstractMethodUsage);
         varExpr := TVarExpr.CreateTyped(FProg, methSym.SelfSym.Typ, methSym.SelfSym);
         try
            Result:=GetMethodExpr(TMethodSymbol(sym), varExpr, rkObjRef, FTok.HotPos, True);
         except
            varExpr.Free;
            raise;
         end;
         try
            ReadFuncArgs(TFuncExpr(Result));
            if TMethodSymbol(sym).Kind = fkConstructor then
               (Result as TMethodExpr).Typ := (methSym.StructSymbol as TClassSymbol).Parent;
            TFuncExpr(Result).TypeCheckArgs(FProg);
         except
            Result.Free;
            raise;
         end;
      end else if sym is TPropertySymbol then begin
         varExpr := TVarExpr.CreateTyped(FProg, parentSym, methSym.SelfSym);
         try
            Result := ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
         except
            varExpr.Free;
            raise;
         end;
      end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);
   end else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InheritedMethodNotFound, [Name]);
end;

// ReadName
//
function TdwsCompiler.ReadName(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TProgramExpr;
var
   sym : TSymbol;
   nameToken : TToken;
   namePos : TScriptPos;
   varExpr : TDataExpr;
   fieldExpr : TFieldExpr;
   progMeth : TMethodSymbol;
   baseType : TTypeSymbol;
   sk : TSpecialKeywordKind;
   symClassType : TClass;
begin
   if (FSourcePostConditionsIndex<>0) and FTok.TestDelete(ttOLD) then
      Exit(ReadNameOld(isWrite));

   if FTok.TestDelete(ttNEW) then
      Exit(ReadNew(isWrite));

   if FTok.TestDelete(ttINHERITED) then
      Exit(ReadNameInherited(isWrite));

   // Get name
   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   nameToken := FTok.GetToken;
   namePos := FTok.HotPos;

   // Test for special functions
   sk:=IdentifySpecialName(nameToken.FString);
   if sk<>skNone then begin
      FTok.KillToken;
      Exit(ReadSpecialFunction(namePos, sk));
   end;

   // Find name in symboltable
   sym := FProg.Table.FindSymbol(nameToken.FString, cvPrivate);

   if not Assigned(sym) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownName, [nameToken.FString]);

   FTok.KillToken;

   // Add the symbol usage to Dictionary
   if coSymbolDictionary in FOptions then
      FSymbolDictionary.AddSymbolReference(sym, namePos, isWrite);

   Result := nil;
   try
      baseType := sym.BaseType;

      if baseType<>nil then begin

         // Unit prefix found
         if baseType.ClassType=TUnitSymbol then begin

            if not FTok.TestDelete(ttDOT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
            if not FTok.TestName then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
            namePos := FTok.HotPos;   // reuse token pos variable
            sym := TUnitSymbol(baseType).Table.FindLocal(FTok.GetToken.FString);

            if not Assigned(sym) then
               FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownNameDotName,
                                        [baseType.Name, FTok.GetToken.FString]);

            FTok.KillToken;

            // Already added symbol usage of the unit. Now add for the unit's specified symbol.
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddSymbolReference(sym, namePos, isWrite);
         end;

         if baseType.ClassType = TEnumerationSymbol then
            baseType := TEnumerationSymbol(baseType).Typ.BaseType;

      end;

      // "Variables"

      symClassType:=sym.ClassType;

      if symClassType=TLazyParamSymbol then begin
         Result:=ReadSymbol(GetLazyParamExpr(TLazyParamSymbol(sym)), IsWrite, expecting);
         Exit;
      end else if symClassType=TVarParamSymbol then begin
         Result:=ReadSymbol(GetVarParamExpr(TVarParamSymbol(sym)), IsWrite, expecting);
         Exit;
      end else if symClassType=TConstParamSymbol then begin
         Result:=ReadSymbol(GetConstParamExpr(TConstParamSymbol(sym)), IsWrite, expecting);
         Exit;
      end;

      if sym.InheritsFrom(TConstSymbol) then begin
         Result:=ReadConstName(TConstSymbol(sym), IsWrite);
         Exit;
      end;

      if sym.InheritsFrom(TDataSymbol) then begin

         if sym.Typ is TFuncSymbol then
            if FTok.Test(ttASSIGN) then
               Result:=GetVarExpr(TDataSymbol(sym))
            else Result:=ReadFunc(TFuncSymbol(sym.Typ), IsWrite, GetVarExpr(TDataSymbol(sym)), expecting)
         else Result:=ReadSymbol(GetVarExpr(TDataSymbol(sym)), IsWrite, expecting);

      end else if sym.InheritsFrom(TExternalVarSymbol) then

         Result := ReadSymbol(ReadExternalVar(TExternalVarSymbol(sym), IsWrite), IsWrite, expecting)

      // OOP related stuff

      else if baseType is TClassSymbol then begin

         Result:=ReadClassSymbolName(TClassSymbol(baseType), isWrite, expecting);

      end else if baseType is TInterfaceSymbol then begin

         Result:=ReadInterfaceSymbolName(TInterfaceSymbol(baseType), isWrite, expecting);

      end else if sym.InheritsFrom(TFieldSymbol) then begin

         progMeth := TMethodSymbol(TdwsProcedure(FProg).Func);
         if progMeth.IsClassMethod then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
         varExpr := TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym);
         try
            fieldExpr:=ReadField(varExpr, TFieldSymbol(sym));
         except
            varExpr.Free;
            raise;
         end;
         Result := ReadSymbol(fieldExpr, IsWrite, expecting);

      end else if sym.InheritsFrom(TPropertySymbol) then begin

         progMeth := TMethodSymbol(TdwsProcedure(FProg).Func);
         if progMeth.IsClassMethod then
            varExpr := TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym)
         else varExpr := TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym);
         try
            Result := ReadSymbol(ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite), IsWrite, expecting);
         except
            varExpr.Free;
            raise;
         end;

      // Methods
      end else if sym.InheritsFrom(TMethodSymbol) then

         Result:=ReadStaticMethod(TMethodSymbol(sym), IsWrite, expecting)

      // Functions/Procedures
      else if sym.InheritsFrom(TFuncSymbol) then

         Result := ReadSymbol(ReadFunc(TFuncSymbol(sym), IsWrite, nil, expecting), IsWrite, expecting)

      // Type casts
      else if sym.InheritsFrom(TTypeSymbol) then

         Result := ReadTypeCast(namePos, TTypeSymbol(sym))

      else begin

         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownType, [sym.Name]);

      end;

   except
      Result.Free;
      raise;
   end;
end;

// ReadClassSymbolName
//
function TdwsCompiler.ReadClassSymbolName(baseType : TClassSymbol; isWrite : Boolean;
                                          expecting : TTypeSymbol) : TProgramExpr;
var
   namePos : TScriptPos;
   constExpr : TTypedExpr;
   convExpr : TConvClassExpr;
   castedExprTyp : TTypeSymbol;
begin
   if FTok.TestDelete(ttBLEFT) then begin
      // Cast
      FTok.TestName;
      namePos:=FTok.HotPos;
      Result:=ReadExpr;
      if not (Result is TTypedExpr) then
         FMsgs.AddCompilerStopFmt(namePos, CPE_IncompatibleTypes,
                                  ['void', baseType.Name]);
      castedExprTyp:=TTypedExpr(Result).Typ;
      if    (not (castedExprTyp is TClassSymbol))
         or (
                   (not TClassSymbol(castedExprTyp).IsOfType(baseType))
               and (not baseType.IsOfType(castedExprTyp))
            ) then
         FMsgs.AddCompilerErrorFmt(namePos, CPE_IncompatibleTypes,
                                   [castedExprTyp.Name, baseType.Name]);
      if not (FTok.TestDelete(ttBRIGHT)) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      convExpr:=TConvClassExpr.Create(FProg, TTypedExpr(Result));
      convExpr.Typ:=baseType;
      Result:=ReadSymbol(convExpr, IsWrite, expecting);

   end else begin

      constExpr:=TConstExpr.CreateTyped(FProg, baseType.ClassOf, Int64(baseType));
      Result:=ReadSymbol(constExpr, IsWrite, expecting);

   end;
end;

// ReadInterfaceSymbolName
//
function TdwsCompiler.ReadInterfaceSymbolName(baseType : TInterfaceSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   constExpr : TTypedExpr;
begin
   constExpr:=TConstExpr.CreateTyped(FProg, baseType, Int64(baseType));
   Result:=ReadSymbol(constExpr, IsWrite, expecting);
end;

// ReadConstName
//
function TdwsCompiler.ReadConstName(constSym : TConstSymbol; IsWrite: Boolean) : TProgramExpr;
var
   typ : TTypeSymbol;
begin
   typ:=constSym.Typ;
   if typ.Typ is TArraySymbol then
      typ:=typ.Typ;
   Result := ReadSymbol(TConstExpr.CreateTyped(FProg, typ, constSym), IsWrite)
end;

// ReadNameOld
//
function TdwsCompiler.ReadNameOld(IsWrite: Boolean): TTypedExpr;
var
   sym : TDataSymbol;
   oldExpr : TProgramExpr;
   expr : TTypedExpr;
   initExpr : TNoResultExpr;
   varExpr : TVarExpr;
begin
   oldExpr:=ReadName(IsWrite);
   if (not (oldExpr is TTypedExpr)) or (TTypedExpr(oldExpr).Typ=nil) then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionOrValueExpected);
      // keep going
      oldExpr.Free;
      expr:=TUnifiedConstExpr.CreateUnified(FProg, FProg.TypVariant, Unassigned);
   end else expr:=TTypedExpr(oldExpr);

   sym:=TDataSymbol.Create('old$'+IntToStr(FSourcePostConditionsIndex), expr.Typ);
   Inc(FSourcePostConditionsIndex);
   FProg.Table.AddSymbol(sym);
   varExpr:=GetVarExpr(sym);
   initExpr:=CreateAssign(FTok.HotPos, ttASSIGN, varExpr, expr);
   FProg.InitExpr.AddStatement(initExpr);

   Result:=GetVarExpr(sym);
end;

// ReadNameInherited
//
function TdwsCompiler.ReadNameInherited(IsWrite: Boolean): TProgramExpr;
begin
   // Name with inherited
   Result := ReadInherited(IsWrite);
   try
      Result := ReadSymbol(Result, IsWrite);
   except
      Result.Free;
      raise;
   end;
end;

// ReadField
//
function TdwsCompiler.ReadField(expr : TDataExpr; sym : TFieldSymbol) : TFieldExpr;
begin
   Result:=TFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, sym, expr);
end;

// Parses statements like "property[i, j, k] := expr" and "expr := property[i, j, k]"
function TdwsCompiler.ReadPropertyExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol; IsWrite: Boolean): TProgramExpr;
begin
   if IsWrite then
      Result:=ReadPropertyWriteExpr(expr, propertySym)
   else Result:=ReadPropertyReadExpr(expr, propertySym);
end;

// ReadPropertyReadExpr
//
function TdwsCompiler.ReadPropertyReadExpr(var expr : TDataExpr; propertySym : TPropertySymbol) : TProgramExpr;
var
   sym : TSymbol;
   aPos : TScriptPos;
   typedExprList : TTypedExprList;
   argPosArray : TScriptPosArray;
begin
   Result := nil;
   aPos:=FTok.HotPos;

   sym := propertySym.ReadSym;

   // No ReadSym
   if sym = nil then

      FMsgs.AddCompilerStop(FTok.HotPos, CPE_WriteOnlyProperty)

   else if sym is TFieldSymbol then begin

      // ReadSym is a field
      if expr.Typ is TClassSymbol then
         Result := TReadOnlyFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, TFieldSymbol(sym),
                                             expr)
      else FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);

   end else if sym is TMethodSymbol then begin

      typedExprList:=TTypedExprList.Create;
      try
         if propertySym.HasArrayIndices then begin
            typedExprList.Table:=propertySym.ArrayIndices;
            ReadArguments(typedExprList.AddExpr, ttALEFT, ttARIGHT, argPosArray, typedExprList.ExpectedArg);
         end;

         Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, False);

      finally
         typedExprList.Free;
      end;

   end else Assert(False);
end;

// ReadPropertyWriteExpr
//
function TdwsCompiler.ReadPropertyWriteExpr(var expr : TDataExpr; propertySym : TPropertySymbol) : TProgramExpr;
var
   sym : TSymbol;
   aPos : TScriptPos;
   fieldExpr : TFieldExpr;
   tokenType : TTokenType;
   typedExprList : TTypedExprList;
   argPosArray : TScriptPosArray;
begin
   Result := nil;
   aPos:=FTok.HotPos;

   typedExprList:=TTypedExprList.Create;
   try
      if propertySym.HasArrayIndices then begin
         typedExprList.Table:=propertySym.ArrayIndices;
         ReadArguments(typedExprList.AddExpr, ttALEFT, ttARIGHT,
                       argPosArray, typedExprList.ExpectedArg);
      end;

      tokenType:=FTok.TestDeleteAny(cAssignmentTokens);

      if tokenType<>ttNone then begin

         if tokenType<>ttASSIGN then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_CantUseCombinedAssignmentOnProperty);

         sym := propertySym.WriteSym;

         // No WriteSym
         if sym = nil then

            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ReadOnlyProperty)

         else if sym is TFieldSymbol then begin

            // WriteSym is a Field
            if Expr.Typ is TClassOfSymbol then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
            fieldExpr := TFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, TFieldSymbol(sym), expr);
            Result := ReadAssign(ttASSIGN, fieldExpr);

         end else if sym is TMethodSymbol then begin

            // WriteSym is a Method
            // Convert an assignment to a function call f := x  -->  f(x)

            Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, True);

         end else Assert(False);

      end else begin

         if    FTok.Test(ttDOT)
            or (FTok.Test(ttBLEFT) and (propertySym.BaseType is TFuncSymbol))  then begin

            sym:=propertySym.ReadSym;

            if sym is TMethodSymbol then begin

               Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, False);

            end else if sym is TFieldSymbol then begin

               if expr.Typ is TClassSymbol then
                  Result:=TReadOnlyFieldExpr.Create(FProg, FTok.HotPos, sym.Typ,
                                                    TFieldSymbol(sym), expr)
               else FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);

            end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_WriteOnlyProperty)

         end else begin

            FMsgs.AddCompilerError(aPos, CPE_InvalidInstruction);
            // fake to keep going
            FreeAndNil(Expr);
            Result:=TConstExpr.Create(FProg, propertySym.Typ, Null);

         end;

      end;

   finally
      typedExprList.Free;
   end;
end;

// ReadPropertyArrayAccessor
//
function TdwsCompiler.ReadPropertyArrayAccessor(var expr : TDataExpr; propertySym : TPropertySymbol;
      typedExprList : TTypedExprList; const scriptPos : TScriptPos; isWrite : Boolean) : TFuncExpr;
var
   i : Integer;
   sym : TSymbol;
begin
   if isWrite then
      sym:=propertySym.WriteSym
   else sym:=propertySym.ReadSym;

   if expr.Typ is TClassOfSymbol then begin
      // Class properties
      if not TMethodSymbol(sym).IsClassMethod then
         FMsgs.AddCompilerStop(scriptPos, CPE_StaticPropertyWriteExpected);
      Result:=GetMethodExpr(TMethodSymbol(sym), expr, rkClassOfRef, scriptPos, False);
   end else Result:=GetMethodExpr(TMethodSymbol(sym), expr, rkObjRef, scriptPos, False);

   try
      expr := nil; // is part of Result

      // Add array indices (if any)
      for i:=0 to typedExprList.Count-1 do
         Result.AddArg(typedExprList.Expr[i]);
      typedExprList.Clear;

      if Assigned(propertySym.IndexSym) then
         Result.AddArg(TConstExpr.CreateTyped(FProg, propertySym.IndexSym,
                                              propertySym.IndexValue));

      // Add right side of assignment
      if isWrite then
         Result.AddArg(ReadExpr(propertySym.Typ));

      Result.TypeCheckArgs(FProg);
   except
      Result.Free;
      raise;
   end;
end;

// ReadSymbol
//
function TdwsCompiler.ReadSymbol(expr : TProgramExpr; isWrite : Boolean = False;
                                 expecting : TTypeSymbol = nil) : TProgramExpr;

   function GetDefaultProperty(struct : TStructuredTypeSymbol) : TPropertySymbol;
   begin
      while Assigned(struct) and not Assigned(struct.DefaultProperty) do
         struct:=struct.Parent;

      if Assigned(struct) then
         Result:=struct.DefaultProperty
      else Result:=nil;
   end;

   function ReadArrayExpr(var baseExpr : TDataExpr) : TProgramExpr;
   var
      idx : Int64;
      indexExpr, valueExpr : TTypedExpr;
      newBaseExpr : TDataExpr;
      baseType : TArraySymbol;
      arraySymbol : TStaticArraySymbol;
      errCount : Integer;
      hotPos : TScriptPos;
   begin
      FTok.KillToken;

      newBaseExpr:=nil;

      if FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

      errCount:=FMsgs.Count;

      // There is at one index expression
      repeat
         hotPos:=FTok.HotPos;
         indexExpr := ReadExpr;
         if not (baseExpr.BaseType is TArraySymbol) then begin
            FMsgs.AddCompilerError(hotPos, RTE_TooManyIndices);
            indexExpr.Free;
            Continue;
         end;

         baseType := TArraySymbol(baseExpr.BaseType);

         try
            if not (   (indexExpr.Typ.UnAliasedType=baseType.IndexType.UnAliasedType)
                    or indexExpr.Typ.IsOfType(FProg.TypVariant)) then
               FMsgs.AddCompilerErrorFmt(hotPos, CPE_ArrayIndexMismatch,
                                         [baseType.IndexType.Name, indexExpr.Typ.Name]);

            if baseType is TStaticArraySymbol then begin

               arraySymbol:=TStaticArraySymbol(baseType);
               if arraySymbol is TOpenArraySymbol then begin

                  newBaseExpr := TOpenArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr)

               end else begin

                  newBaseExpr := TStaticArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr,
                                                    arraySymbol.LowBound, arraySymbol.HighBound);
                  if indexExpr.IsConstant and (FMsgs.Count=errCount) then begin
                     idx:=indexExpr.EvalAsInteger(FExec);
                     if idx<arraySymbol.LowBound then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, RTE_ArrayLowerBoundExceeded, [idx])
                     else if idx>arraySymbol.HighBound then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, RTE_ArrayUpperBoundExceeded, [idx]);
                  end;
               end;

            end else if baseType is TDynamicArraySymbol then begin

               if FTok.Test(ttCOMMA) then
                  newBaseExpr:=TDynamicArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr)
               else if FTok.TestDelete(ttARIGHT) then begin
                  if FTok.TestDelete(ttASSIGN) then begin
                     hotPos:=FTok.HotPos;
                     valueExpr:=ReadExpr(baseType.Typ);
                     try
                        if not baseType.Typ.IsCompatible(valueExpr.Typ) then
                           FMsgs.AddCompilerErrorFmt(hotPos, CPE_AssignIncompatibleTypes,
                                                     [valueExpr.Typ.Name, baseType.Typ.Name]);
                        Result:=TDynamicArraySetExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr, valueExpr);
                     except
                        valueExpr.Free;
                        raise;
                     end;
                  end else Result:=TDynamicArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr);
                  Exit;
               end;

            end else Assert(False);

         except
            indexExpr.Free;
            raise;
         end;

         baseExpr := newBaseExpr;
      until not FTok.TestDelete(ttCOMMA);

      Result:=baseExpr;

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
   end;

var
   name : string;
   member : TSymbol;
   defaultProperty : TPropertySymbol;
   symPos : TScriptPos;
   baseType : TTypeSymbol;
   dataExpr : TDataExpr;
   funcExpr : TFuncExpr;
begin
   Result := Expr;
   try
      repeat
         Expr := Result;
         baseType := Result.BaseType;

         // Member
         if FTok.TestDelete(ttDOT) then begin
            if FTok.TestName then begin
               Name := FTok.GetToken.FString;
               symPos := FTok.HotPos;
               FTok.KillToken;

               // Record
               if baseType is TRecordSymbol then begin

                  member := TRecordSymbol(baseType).Members.FindLocal(Name);
                  if coSymbolDictionary in FOptions then
                     FSymbolDictionary.AddSymbolReference(member, symPos, isWrite);

                  if Result is TFuncExpr then
                     TFuncExpr(Result).SetResultAddr(FProg, nil);
                  if member is TFieldSymbol then
                     Result := TRecordExpr.Create(FProg, FTok.HotPos, TDataExpr(Result), TFieldSymbol(member))
                  else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownMember, [Name]);
                  Expr := nil;

               // Class
               end else if baseType is TStructuredTypeSymbol then begin

                  member:=TStructuredTypeSymbol(baseType).Members.FindSymbolFromScope(Name, CurrentClass);

                  if coSymbolDictionary in FOptions then
                     FSymbolDictionary.AddSymbolReference(member, symPos, isWrite);

                  if member is TMethodSymbol then begin

                     if TMethodSymbol(member).IsClassMethod then
                        funcExpr := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result), rkClassOfRef, symPos, False)
                     else funcExpr := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result), rkObjRef, symPos, False);
                     Result:=nil;
                     Result:=WrapUpFunctionRead(funcExpr, expecting);

                  end else if member is TFieldSymbol then

                     Result := TFieldExpr.Create(FProg, FTok.HotPos, member.Typ,
                                                 TFieldSymbol(member), TDataExpr(Result))

                  else if member is TPropertySymbol then

                     Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite)

                  else if member is TConstSymbol then begin

                     FreeAndNil(Result);
                     Result := ReadConstName(TConstSymbol(member), IsWrite);

                  end else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownMember, [Name]);

               // Class Of
               end else if baseType is TClassOfSymbol then begin

                  member := TClassSymbol(baseType.Typ).Members.FindSymbolFromScope(Name, CurrentClass);
                  if coSymbolDictionary in FOptions then
                     FSymbolDictionary.AddSymbolReference(member, FTok.HotPos, isWrite);

                  // Class method
                  if member is TMethodSymbol then begin

                     case TMethodSymbol(member).Kind of
                        fkFunction, fkProcedure, fkMethod:
                           if not TMethodSymbol(member).IsClassMethod then
                              FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
                        fkDestructor:
                           FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
                     end;
                     funcExpr:=GetMethodExpr(TMethodSymbol(member), TDataExpr(Result),
                                             rkClassOfRef, symPos, False);
                     Result:=nil;
                     Result:=WrapUpFunctionRead(funcExpr, expecting);

                  // Static property
                  end else if member is TPropertySymbol then

                     Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite)

                  else if member is TConstSymbol then begin

                     FreeAndNil(Result);
                     Result:=ReadConstName(TConstSymbol(member), IsWrite);

                  end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);

               // Array symbol
               end else if baseType is TArraySymbol then begin

                  Result := ReadArrayMethod(name, symPos, Result as TTypedExpr, isWrite);

               // Connector symbol
               end else if baseType is TConnectorSymbol then begin

                  Result := ReadConnectorSym(Name, Result as TTypedExpr,
                                             TConnectorSymbol(baseType).ConnectorType, IsWrite)

               end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMemberExpected);
            end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
         end
         // Arrays
         else if FTok.Test(ttALEFT) then begin
            if Assigned(Result) then begin
               if baseType is TStructuredTypeSymbol then begin
                  // array property
                  defaultProperty := GetDefaultProperty(TStructuredTypeSymbol(baseType));
                  if Assigned(defaultProperty) then
                     Result := ReadPropertyExpr(TDataExpr(Result), defaultProperty, IsWrite)
                  else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NoDefaultProperty,
                                                [TDataExpr(Result).Typ.Name]);
               end else begin
                  // Type "array"
                  dataExpr:=(Result as TDataExpr);
                  if baseType is TArraySymbol then
                     Result := ReadArrayExpr(dataExpr)
                  else if baseType is TConnectorSymbol then
                     Result := ReadConnectorArray('', Result as TTypedExpr,
                                                  TConnectorSymbol(baseType).ConnectorType, IsWrite)
                  else if dataExpr.IsOfType(FProg.TypString) then begin
                     FTok.KillToken;
                     Result := ReadStringArray(dataExpr, IsWrite)
                  end else FMsgs.AddCompilerError(FTok.HotPos, CPE_ArrayExpected);
                  if Optimize then
                     Result:=Result.Optimize(FProg, FExec);
               end;
            end;
         end else if FTok.Test(ttBLEFT) then begin
            if baseType is TFuncSymbol then
               Result := ReadFunc(TFuncSymbol(baseType), IsWrite, Result as TDataExpr)
            else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMethodExpected);
         end;

      until (Expr = Result);
   except
      Result.Free;
      raise;
   end;
end;

// ReadExternalVar
//
function TdwsCompiler.ReadExternalVar(sym : TExternalVarSymbol; isWrite : Boolean) : TFuncExpr;
begin
   Result := nil;
   try
      if IsWrite then begin
         if FTok.TestDelete(ttASSIGN) then begin
            if not Assigned(Sym.WriteFunc) then
               FMsgs.AddCompilerStop(FTok.HotPos,CPE_CantWriteToLeftSide);
            // Transform a := b into a(b)
            Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.WriteFunc);
            Result.AddArg(ReadExpr);
         end else if (Sym.Typ is TClassSymbol) or (Sym.Typ is TClassOfSymbol) then begin
            if not Assigned(Sym.ReadFunc) then
               FMsgs.AddCompilerStop(FTok.HotPos,CPE_RightSideNeedsReturnType);
            Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc)
         end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
      end else if Assigned(Sym.ReadFunc) then
         Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc)
      else FMsgs.AddCompilerStop(FTok.HotPos,CPE_WriteOnlyProperty); // ??
      Result.TypeCheckArgs(FProg);
   except
      Result.Free;
      raise;
   end;
end;

// ReadFor
//
function TdwsCompiler.ReadFor: TForExpr;
var
   expr : TProgramExpr;
   loopVarExpr : TIntVarExpr;
   fromExpr, toExpr, stepExpr : TTypedExpr;
   sym : TSymbol;
   forPos, enumPos, stepPos : TScriptPos;
   enumSymbol : TEnumerationSymbol;
   forExprClass : TForExprClass;
begin
   Result:=nil;
   loopVarExpr:=nil;
   fromExpr:=nil;
   toExpr:=nil;
   stepExpr:=nil;
   try
      forPos:=FTok.HotPos;

      expr:=ReadName(True);
      try
         if not (expr is TVarExpr) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_VariableExpected);
         if not TVarExpr(expr).IsOfType(FProg.TypInteger) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_IntegerExpected);
         if not (expr is TIntVarExpr) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_FORLoopMustBeLocalVariable);
      except
         expr.Free;
         raise;
      end;

      loopVarExpr:=TIntVarExpr(expr);
      WarnForVarUsage(loopVarExpr, FTok.HotPos);

      if FTok.TestDelete(ttIN) then begin

         forExprClass:=TForUpwardExpr;

         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         enumPos:=FTok.HotPos;
         sym:=FProg.Table.FindSymbol(FTok.GetToken.FString, cvPrivate);
         if not Assigned(sym) then
            FMsgs.AddCompilerStopFmt(enumPos, CPE_UnknownName, [FTok.GetToken.FString]);
         FTok.KillToken;

         if sym.InheritsFrom(TEnumerationSymbol) then begin

            enumSymbol:=TEnumerationSymbol(sym);

            if loopVarExpr.Typ<>enumSymbol then
               FMsgs.AddCompilerStop(enumPos, CPE_IncompatibleOperands);

            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddTypeSymbol(enumSymbol, enumPos);

            fromExpr:=TConstExpr.CreateTyped(FProg, loopVarExpr.Typ, enumSymbol.LowBound);
            toExpr:=TConstExpr.CreateTyped(FProg, loopVarExpr.Typ, enumSymbol.HighBound);

         end else FMsgs.AddCompilerStop(enumPos, CPE_EnumerationExpected);

      end else begin

         if not FTok.TestDelete(ttASSIGN) then
           FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

         fromExpr:=ReadExpr;
         if not fromExpr.IsOfType(FProg.TypInteger) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_IntegerExpected);

         if FTok.TestDelete(ttTO) then
            forExprClass:=TForUpwardExpr
         else if FTok.TestDelete(ttDOWNTO) then
            forExprClass:=TForDownwardExpr
         else begin
            forExprClass:=nil;
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ToOrDowntoExpected);
         end;

         toExpr:=ReadExpr;
         if not toExpr.IsOfType(FProg.TypInteger) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);

      end;

      if FTok.Test(ttNAME) and SameText(FTok.GetToken.FString, 'step') then begin
         FTok.KillToken;
         FTok.Test(ttNone);
         stepPos:=FTok.HotPos;
         stepExpr:=ReadExpr;
         if not stepExpr.IsOfType(FProg.TypInteger) then
            FMsgs.AddCompilerError(stepPos, CPE_IntegerExpected);
         if stepExpr.InheritsFrom(TConstIntExpr) and (TConstIntExpr(stepExpr).Value<=0) then
            FMsgs.AddCompilerErrorFmt(stepPos, RTE_ForLoopStepShouldBeStrictlyPositive,
                                      [TConstIntExpr(stepExpr).Value]);
         if forExprClass=TForUpwardExpr then
            forExprClass:=TForUpwardStepExpr
         else forExprClass:=TForDownwardStepExpr;
      end;

      Result:=forExprClass.Create(FProg, forPos);
      EnterLoop(Result);
      try
         MarkLoopExitable(leBreak);
         Result.VarExpr:=loopVarExpr;
         loopVarExpr:=nil;

         Result.FromExpr:=fromExpr;
         fromExpr:=nil;

         Result.ToExpr:=toExpr;
         toExpr:=nil;

         if stepExpr<>nil then begin
            TForStepExpr(Result).StepExpr:=stepExpr;
            stepExpr:=nil;
         end;

         if not FTok.TestDelete(ttDO) then
           FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

         Result.DoExpr:=ReadBlock;
      except
         Result.Free;
         raise;
      end;
      LeaveLoop;
   finally
      loopVarExpr.Free;
      fromExpr.Free;
      toExpr.Free;
      stepExpr.Free;
   end;
end;

// WarnForVarUsage
//
procedure TdwsCompiler.WarnForVarUsage(varExpr : TVarExpr; const pos : TScriptPos);
var
   i : Integer;
   loopExpr : TNoResultExpr;
   currVarExpr : TVarExpr;
begin
   for i:=0 to FLoopExprs.Count-1 do begin
      loopExpr:=FLoopExprs.Items[i];
      if loopExpr.InheritsFrom(TForExpr) then begin
         currVarExpr:=TForExpr(loopExpr).VarExpr;
         if currVarExpr.SameVarAs(varExpr) then begin
            FMsgs.AddCompilerWarning(pos, CPE_AssignementToFORLoopVariable);
            Break;
         end;
      end;
   end;
end;

// ReadIf
//
function TdwsCompiler.ReadIf : TNoResultExpr;
var
   hotPos : TScriptPos;
   condExpr : TTypedExpr;
   thenExpr : TNoResultExpr;
   elseExpr : TNoResultExpr;
begin
   hotPos:=FTok.HotPos;

   condExpr:=nil;
   thenExpr:=nil;
   elseExpr:=nil;
   try
      condExpr:=ReadExpr;
      if not (condExpr.IsOfType(FProg.TypBoolean) or condExpr.IsOfType(FProg.TypVariant)) then
         FMsgs.AddCompilerError(hotPos, CPE_BooleanExpected);

      if not FTok.TestDelete(ttTHEN) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ThenExpected);

      if FTok.TestDelete(ttELSE) then begin // if () then else;

         condExpr:=TNotBoolExpr.Create(FProg, condExpr);
         thenExpr:=ReadBlock;

      end else begin

         thenExpr:=ReadBlock;
         if FTok.TestDelete(ttELSE) then
            elseExpr:=ReadBlock;

      end;

      if elseExpr=nil then
         Result:=TIfThenExpr.Create(FProg, hotPos, condExpr, thenExpr)
      else Result:=TIfThenElseExpr.Create(FProg, hotPos, condExpr, thenExpr, elseExpr);
   except
      condExpr.Free;
      thenExpr.Free;
      elseExpr.Free;
      raise;
   end;

   if Optimize then begin
      try
         Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
      except
         Result.Free;
         raise;
      end;
   end;
end;

// ReadCase
//
function TdwsCompiler.ReadCase;
var
   expr : TNoResultExpr;
   condList : TCaseConditions;
   condition : TCaseCondition;
   tt : TTokenType;
   x : Integer;
begin
   condList := TCaseConditions.Create;
   try
      Result := TCaseExpr.Create(FProg, FTok.HotPos);
      try
         Result.ValueExpr := ReadExpr;

         if not FTok.TestDelete(ttOF) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_OfExpected);

         while not FTok.TestDelete(ttEND) do begin
            if FTok.TestDelete(ttELSE) then begin
               Result.ElseExpr := ReadBlocks([ttEND], tt);
               break;
            end else begin
               ReadCaseConditions(condList, Result.ValueExpr);

               if not FTok.TestDelete(ttCOLON) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

               Expr := ReadBlock;

               // Add case conditions to TCaseExpr
               for x:=0 to condList.Count-1 do begin
                  condition:=condList[x];
                  condition.TrueExpr:=Expr;
                  if x=0 then
                     condition.OwnsTrueExpr:=True;
                  Result.AddCaseCondition(condition);
               end;
               condList.ExtractAll;

               if not (FTok.Test(ttELSE) or FTok.Test(ttEND) or FTok.TestDelete(ttSEMI)) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
            end;
         end;
      except
         Result.Free;
         raise;
      end;
   finally
      condList.Free;
   end;
end;

// ReadCaseConditions
//
function TdwsCompiler.ReadCaseConditions(condList : TCaseConditions; valueExpr : TTypedExpr) : Integer;
var
   hotPos : TScriptPos;
   exprFrom, exprTo : TTypedExpr;
   condition : TCaseCondition;
begin
   // Find a comma sparated list of case conditions  0, 1, 2..4: ;
   repeat

      hotPos:=FTok.HotPos;
      exprFrom := ReadExpr;

      try
         if not Assigned(exprFrom) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

         if FTok.TestDelete(ttDOTDOT) then begin
            // range condition e. g. 0..12
            exprTo := ReadExpr;
            if not Assigned(exprTo) then begin
               exprTo.Free;
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);
            end;
            condition:=TRangeCaseCondition.Create(hotPos, exprFrom, exprTo);
         end else begin
            // compare condition e. g. 123:
            condition:=TCompareCaseCondition.Create(hotPos, exprFrom);
         end;
         condList.Add(condition);
         condition.TypeCheck(FProg, valueExpr.Typ);
      except
         exprFrom.Free;
         raise;
      end;

   until not FTok.TestDelete(ttCOMMA);

   Result:=condList.Count;
end;

// ReadWhile
//
function TdwsCompiler.ReadWhile : TNoResultExpr;
var
   condExpr : TTypedExpr;
begin
   Result:=TWhileExpr.Create(FProg, FTok.HotPos);
   EnterLoop(Result);
   try
      condExpr:=ReadExpr;
      TWhileExpr(Result).CondExpr:=condExpr;
      if not (condExpr.IsOfType(FProg.TypBoolean) or condExpr.IsOfType(FProg.TypVariant)) then
         FMsgs.AddCompilerError(Result.ScriptPos, CPE_BooleanExpected);

      if not FTok.TestDelete(ttDO) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

      if    (not condExpr.IsConstant)
         or (not condExpr.IsOfType(FProg.TypBoolean))
         or (not condExpr.EvalAsBoolean(FExec)) then
         MarkLoopExitable(leBreak);

      TWhileExpr(Result).LoopExpr := ReadBlock;
   except
      Result.Free;
      raise;
   end;
   LeaveLoop;

   if Optimize then begin
      try
         Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
      except
         Result.Free;
         raise;
      end;
   end;
end;

// ReadRepeat
//
function TdwsCompiler.ReadRepeat : TNoResultExpr;
var
   tt : TTokenType;
   condExpr : TTypedExpr;
begin
   Result := TRepeatExpr.Create(FProg, FTok.HotPos);
   EnterLoop(Result);
   try
      TRepeatExpr(Result).LoopExpr := ReadBlocks([ttUNTIL], tt);
      condExpr:=ReadExpr;
      TRepeatExpr(Result).CondExpr := condExpr;
      if not (condExpr.IsOfType(FProg.TypBoolean) or condExpr.IsOfType(FProg.TypVariant)) then
         FMsgs.AddCompilerError(Result.ScriptPos, CPE_BooleanExpected)
      else if (not condExpr.IsConstant) or condExpr.EvalAsBoolean(FExec) then
         MarkLoopExitable(leBreak);
   except
      Result.Free;
      raise;
   end;
   LeaveLoop;

   if Optimize then begin
      try
         Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
      except
         Result.Free;
         raise;
      end;
   end;
end;

// ReadAssign
//
function TdwsCompiler.ReadAssign(token : TTokenType; left : TDataExpr) : TNoResultExpr;
var
   pos : TScriptPos;
   right : TTypedExpr;
begin
   pos:=FTok.HotPos;
   right:=ReadExpr(left.Typ);
   try
      Result:=CreateAssign(pos, token, left, right);
   except
      right.Free;
      raise;
   end;
end;

// ReadStaticMethod
//
function TdwsCompiler.ReadStaticMethod(methodSym : TMethodSymbol;
               isWrite : Boolean; expecting : TTypeSymbol = nil) : TProgramExpr;
var
   progMeth: TMethodSymbol;
begin
   progMeth := TMethodSymbol(TdwsProcedure(FProg).Func);

   Result := GetMethodExpr(methodSym,
                           TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym),
                           rkObjRef, FTok.HotPos, False);

   Result:=WrapUpFunctionRead(TFuncExpr(Result), expecting);

   Result:=ReadSymbol(Result, IsWrite, expecting);
end;

// ReadFunc
//
function TdwsCompiler.ReadFunc(funcSym : TFuncSymbol; isWrite : Boolean;
                               codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TTypedExpr;
var
   funcExpr : TFuncExprBase;
begin
   WarnDeprecated(funcSym);

   funcExpr:=GetFuncExpr(funcSym, isWrite, codeExpr, expecting);
   Result:=WrapUpFunctionRead(funcExpr, expecting);

   if funcSym.IsExternal then
      funcSym.Executable:=TExternalFuncHandler.Create;

   if Optimize then
      Result:=Result.OptimizeToTypedExpr(FProg, FExec);
end;

// WrapUpFunctionRead
//
function TdwsCompiler.WrapUpFunctionRead(funcExpr : TFuncExprBase; expecting : TTypeSymbol = nil) : TTypedExpr;
begin
   Result:=funcExpr;
   try
      if FTok.Test(ttBLEFT) then begin
         ReadFuncArgs(funcExpr);
         funcExpr.TypeCheckArgs(FProg);
      end else begin
         if    (    (expecting is TNilSymbol)
                and (funcExpr is TFuncPtrExpr)
                and not FTok.Test(ttDOT))
            or (    (expecting is TFuncSymbol)
                and funcExpr.funcSym.IsCompatible(expecting)) then begin
            if funcExpr.FuncSym.Level>1 then
               FMsgs.AddCompilerError(funcExpr.Pos, CPE_LocalFunctionAsDelegate);
            Result:=TFuncRefExpr.Create(FProg, funcExpr);
         end else begin
            funcExpr.TypeCheckArgs(FProg);
         end;
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadFuncResultType
//
function TdwsCompiler.ReadFuncResultType(funcKind : TFuncKind) : TTypeSymbol;
begin
   Result:=nil;
   if FTok.TestDelete(ttCOLON) then begin
      if not (funcKind in [fkFunction, fkMethod]) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NoResultTypeExpected);
      Result:=ReadType('', tcResult);
   end else if funcKind=fkFunction then begin
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionTypeExpected);
   end;
end;

// ReadFuncArgs
//
procedure TdwsCompiler.ReadFuncArgs(funcExpr : TFuncExprBase);
var
   argPosArray : TScriptPosArray;
begin
   ReadArguments(funcExpr.AddArg, ttBLEFT, ttBRIGHT, argPosArray, funcExpr.ExpectedArg);
end;

// ReadArguments
//
procedure TdwsCompiler.ReadArguments(const addArgProc : TAddArgProcedure;
                                     leftDelim, rightDelim : TTokenType;
                                     var argPosArray : TScriptPosArray;
                                     const expectedProc : TExpectedArgFunction = nil);
var
   arg : TTypedExpr;
   argSym : TParamSymbol;
   argPos : TScriptPos;
   expectedType : TTypeSymbol;
   n : Integer;
begin
   if FTok.TestDelete(leftDelim) then begin
      if not FTok.TestDelete(rightDelim) then begin
         // At least one argument was found
         repeat
            argPos:=FTok.HotPos;

            if Assigned(expectedProc) then
               argSym:=expectedProc()
            else argSym:=nil;
            if argSym<>nil then
               expectedType:=argSym.Typ
            else expectedType:=nil;

            if argSym is TVarParamSymbol then
               arg:=ReadTerm(True, expectedType)
            else arg:=ReadExpr(expectedType);
            AddArgProc(arg);
            n:=Length(argPosArray);
            SetLength(argPosArray, n+1);
            argPosArray[n]:=argPos;

            if (argSym is TVarParamSymbol) and (arg is TVarExpr) then
               WarnForVarUsage(TVarExpr(arg), argPos);
         until not FTok.TestDelete(ttCOMMA);
         if not FTok.TestDelete(rightDelim) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;
   end;
end;

// ReadArrayType
//
function TdwsCompiler.ReadArrayType(const TypeName: String; typeContext : TdwsReadTypeContext): TTypeSymbol;
var
   hotPos : TScriptPos;

   procedure CheckBound(bound : TTypedExpr);
   begin
      if not bound.IsConstant then
         FMsgs.AddCompilerStop(hotPos, CPE_ArrayBoundNotAConstant);
      if not (   bound.Typ.IsOfType(FProg.TypInteger)
              or (bound.Typ is TEnumerationSymbol)
              or bound.Typ.IsOfType(FProg.TypBoolean)) then
         FMsgs.AddCompilerStop(hotPos, CPE_ArrayBoundNotOrdinal);
   end;

var
   x: Integer;
   min, max: TTypedExprList;
   typ: TTypeSymbol;
begin
   min := TTypedExprList.Create;
   max := TTypedExprList.Create;
   try

      if FTok.TestDelete(ttALEFT) then begin

         repeat
            // Lower bound
            hotPos:=FTok.HotPos;
            min.Insert0(ReadExpr);

            CheckBound(min[0]);

            if not FTok.TestDelete(ttDOTDOT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotDotExpected);

            // Upper bound
            hotPos:=FTok.HotPos;
            max.Insert0(ReadExpr);

            CheckBound(max[0]);

            if max[0].Typ<>min[0].Typ then
               FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundsOfDifferentTypes);

            if max[0].EvalAsInteger(FExec)<min[0].EvalAsInteger(FExec) then begin
               FMsgs.AddCompilerError(hotPos, CPE_LowerBoundGreaterThanUpperBound);
               // keep compiling
               max[0].Free;
               max[0]:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, min[0].EvalAsInteger(FExec));
            end;

            if FTok.Test(ttARIGHT) then
               Break;
         until not FTok.TestDelete(ttCOMMA);

         if not FTok.TestDelete(ttARIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
      end;

      if not FTok.TestDelete(ttOF) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_OfExpected);

      if FTok.TestDelete(ttCONST) then begin

         if not (typeContext in [tcDeclaration, tcParameter, tcOperand]) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_TypeExpected);
         Result := TOpenArraySymbol.Create(TypeName, FProg.TypVariant, FProg.TypInteger);

      end else begin

         typ := ReadType('', typeContext);

         if min.Count > 0 then begin
            // initialize innermost array
            Result := TStaticArraySymbol.Create('', typ, min[0].Typ,
                                 min[0].EvalAsInteger(FExec),
                                 max[0].EvalAsInteger(FExec));
            try
               // add outer arrays
               for x := 1 to min.Count - 1 do begin
                  FProg.RootTable.AddToDestructionList(Result);
                  Result := TStaticArraySymbol.Create('', Result, min[0].Typ,
                                 min[x].EvalAsInteger(FExec),
                                 max[x].EvalAsInteger(FExec));
               end;

               // only outermost array is named
               Result.SetName(TypeName);
            except
               Result.Free;
               raise;
            end;

         end else begin

            Result := TDynamicArraySymbol.Create(TypeName, typ, FProg.TypInteger);

         end;

      end;

   finally
      min.Free;
      max.Free;
   end;
end;

// ReadArrayConstant
//
function TdwsCompiler.ReadArrayConstant(expecting : TTypeSymbol = nil) : TArrayConstantExpr;
begin
   Result:=TArrayConstantExpr.Create(FProg, FTok.HotPos);
   try
      if not FTok.TestDelete(ttARIGHT) then begin
         // At least one argument was found
         repeat
            TArrayConstantExpr(Result).AddElementExpr(FProg, ReadExpr);
         until not FTok.TestDelete(ttCOMMA);

         if not FTok.TestDelete(ttARIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end else begin
         // empty array
         (Result.Typ as TStaticArraySymbol).Typ:=FProg.TypVariant;
      end;

      if not (expecting is TOpenArraySymbol) then
         Result.TypeCheckElements(FProg);
      if Optimize then
         Result:=Result.Optimize(FProg, FExec) as TArrayConstantExpr;
   except
      Result.Free;
      raise;
   end;
end;

// ReadArrayMethod
//
function TdwsCompiler.ReadArrayMethod(const name : String; const namePos : TScriptPos;
                                      baseExpr : TTypedExpr; isWrite : Boolean) : TProgramExpr;
var
   arraySym : TArraySymbol;
   argList : TTypedExprList;
   argPosArray : TScriptPosArray;

   procedure CheckRestricted;
   begin
      if arraySym.ClassType<>TDynamicArraySymbol then
         FMsgs.AddCompilerErrorFmt(namePos, CPE_ArrayMethodRestrictedToDynamicArrays, [name]);
   end;

   function CheckArguments(expectedMin, expectedMax : Integer) : Boolean;
   begin
      Result:=argList.Count in [expectedMin..expectedMax];
      if not Result then begin
         if expectedMax=0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NoArgumentsExpected)
         else if argList.Count>expectedMax then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_TooManyArguments)
         else FMsgs.AddCompilerError(FTok.HotPos, CPE_TooFewArguments);
      end;
   end;

begin
   Result:=nil;
   argList:=TTypedExprList.Create;
   try
      ReadArguments(argList.AddExpr, ttBLEFT, ttBRIGHT, argPosArray);
      try
         arraySym:=baseExpr.Typ as TArraySymbol;
         if SameText(name, 'low') then begin
            CheckArguments(0, 0);
            Result:=CreateArrayLow(baseExpr, arraySym);
         end else if SameText(name, 'high') then begin
            CheckArguments(0, 0);
            Result:=CreateArrayHigh(baseExpr, arraySym);
         end else if SameText(name, 'length') then begin
            CheckArguments(0, 0);
            Result:=CreateArrayLength(baseExpr, arraySym);
         end else if SameText(name, 'add') or SameText(name, 'push') then begin
            CheckRestricted;
            if CheckArguments(1, 1) then begin
               if not arraySym.Typ.IsCompatible(argList[0].Typ) then
                  FMsgs.AddCompilerErrorFmt(argPosArray[0], CPE_BadParameterType,
                                            [0, arraySym.Typ.Caption, argList[0].Typ.Caption] );
               Result:=TArrayAddExpr.Create(FProg, namePos, baseExpr, argList[0] as TDataExpr);
               argList.Clear;
            end else Result:=TArrayAddExpr.Create(FProg, namePos, baseExpr, nil);
         end else if SameText(name, 'delete') then begin
            CheckRestricted;
            if CheckArguments(1, 2) then begin
               if not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               if argList.Count>1 then begin
                  if not argList[1].Typ.IsOfType(FProg.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                  Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr,
                                                  argList[0], argList[1]);
               end else Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr,
                                                        argList[0], nil);
               argList.Clear;
            end else Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr, nil, nil);
         end else if SameText(name, 'setlength') then begin
            CheckRestricted;
            if CheckArguments(1, 1) then begin
               if not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               Result:=TArraySetLengthExpr.Create(FProg, namePos, baseExpr, argList[0]);
               argList.Clear;
            end else Result:=TArraySetLengthExpr.Create(FProg, namePos, baseExpr, nil);
         end else if SameText(name, 'swap') then begin
            CheckRestricted;
            if CheckArguments(2, 2) then begin
               if not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               if not argList[1].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
               Result:=TArraySwapExpr.Create(FProg, namePos, baseExpr,
                                             argList[0], argList[1]);
               argList.Clear;
            end else Result:=TArraySwapExpr.Create(FProg, namePos, baseExpr, nil, nil);
         end else if SameText(name, 'copy') then begin
            CheckRestricted;
            if CheckArguments(0, 2) then begin
               if argList.Count>0 then begin
                  if not argList[0].Typ.IsOfType(FProg.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  if argList.Count>1 then begin
                     if not argList[1].Typ.IsOfType(FProg.TypInteger) then
                        FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                     Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr,
                                                   argList[0], argList[1]);
                  end else Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr,
                                                         argList[0], nil);
               end else Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr, nil, nil);
               argList.Clear;
            end else Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr, nil, nil);
         end else if SameText(name, 'reverse') then begin
            CheckRestricted;
            CheckArguments(0, 0);
            Result:=TArrayReverseExpr.Create(FProg, namePos, baseExpr);
         end else FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownName, [name]);
      except
         Result.Free;
         raise;
      end;
   finally
      argList.Free;
   end;
end;

procedure TdwsCompiler.ReadNameList(names : TStrings);
begin
  names.Clear;
  repeat
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    names.Add(FTok.GetToken.FString);
    CheckSpecialName(FTok.GetToken.FString);
    FTok.KillToken;
  until not FTok.TestDelete(ttCOMMA);
end;

// ReadNew
//
function TdwsCompiler.ReadNew(isWrite : Boolean) : TProgramExpr;
var
   sym : TSymbol;
   typSym : TTypeSymbol;
   classSym : TClassSymbol;
   methSym : TMethodSymbol;
   nameToken : TToken;
   hotPos : TScriptPos;
   typedExpr : TTypedExpr;
   baseExpr : TDataExpr;
begin
   baseExpr:=nil;
   classSym:=nil;

   if FTok.TestDelete(ttBLEFT) then begin

      hotPos:=FTok.HotPos;
      typedExpr:=ReadExpr;
      try
         if (typedExpr.Typ is TClassOfSymbol) and (typedExpr is TDataExpr) then begin
            baseExpr:=TDataExpr(typedExpr);
            classSym:=TClassOfSymbol(typedExpr.Typ).TypClassSymbol;
         end else FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);
         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(hotPos, CPE_BrackRightExpected);
      except
         typedExpr.Free;
         raise;
      end;

   end else begin

      // Get name
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ClassRefExpected);

      nameToken := FTok.GetToken;
      hotPos := FTok.HotPos;

      sym:=FProg.Table.FindSymbol(nameToken.FString, cvPrivate);
      FTok.KillToken;

      if FTok.TestDelete(ttALEFT) then begin
         if sym.IsType then begin
            typSym:=TTypeSymbol(sym);
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddTypeSymbol(typSym, hotPos);
            Result:=ReadNewArray(typSym, isWrite);
            Exit;
         end else FMsgs.AddCompilerError(hotPos, CPE_TypeExpected);
      end else if sym is TClassSymbol then begin
         classSym:=TClassSymbol(sym);
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddTypeSymbol(classSym, hotPos);
      end else if (sym is TDataSymbol) and (sym.Typ is TClassOfSymbol) then begin
         classSym:=TClassOfSymbol(sym.Typ).TypClassSymbol;
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddSymbolReference(TDataSymbol(sym), hotPos, False);
      end else FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);

      if sym is TClassSymbol then
         baseExpr:=TConstExpr.CreateTyped(FProg, classSym.ClassOf, Int64(classSym))
      else baseExpr:=TVarExpr.CreateTyped(FProg, classSym, TDataSymbol(sym));

   end;

   methSym:=classSym.FindDefaultConstructor(cvPrivate);

   try
      Result:=GetMethodExpr(methSym, baseExpr, rkClassOfRef, FTok.HotPos, False);
   except
      baseExpr.Free;
      raise;
   end;
   try
      ReadFuncArgs(TFuncExpr(Result));
      (Result as TMethodExpr).Typ:=classSym;
      TFuncExpr(Result).TypeCheckArgs(FProg);
   except
      Result.Free;
      raise;
   end;

   Result:=ReadSymbol(Result, isWrite);
end;

// ReadNewArray
//
function TdwsCompiler.ReadNewArray(elementTyp : TTypeSymbol; isWrite : Boolean) : TProgramExpr;
var
   lengthExpr : TTypedExpr;
   hotPos : TScriptPos;
   newExpr : TNewArrayExpr;
begin
   newExpr:=TNewArrayExpr.Create(FProg, hotPos, elementTyp);
   try
      repeat
         FTok.HasTokens;
         hotPos:=FTok.HotPos;
         lengthExpr:=ReadExpr;
         newExpr.AddLengthExpr(lengthExpr, FProg.TypInteger);
         if not (lengthExpr.IsOfType(FProg.TypInteger)) then
            FMsgs.AddCompilerError(hotPos, CPE_IntegerExpressionExpected);
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
   except
      newExpr.Free;
      raise;
   end;
   Result:=newExpr;
end;

procedure TdwsCompiler.ReadNameList(names : TStrings; var posArray : TScriptPosArray);
begin
  Names.Clear;
  repeat
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    // Added HotPos positions to PosArray. Used for dictionary
    if coSymbolDictionary in FOptions then
    begin
      SetLength(PosArray, Length(PosArray)+1);  // grow the array as needed
      PosArray[High(PosArray)] := FTok.HotPos;
    end;
    Names.Add(FTok.GetToken.FString);
    FTok.KillToken;
  until not FTok.TestDelete(ttCOMMA);
end;

// ReadClassOf
//
function TdwsCompiler.ReadClassOf(const typeName : String) : TClassOfSymbol;
var
   name : String;
   typ : TTypeSymbol;
   classTyp : TClassSymbol;
begin
   // Declaration of a class reference
   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   name:=FTok.GetToken.FString;
   FTok.KillToken;

   typ:=FProg.Table.FindTypeSymbol(name, cvMagic);
   if not (typ is TClassSymbol) then begin
      if not Assigned(typ) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnknownClass, [name])
      else FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NotAClass, [name]);
      classTyp:=FProg.TypObject; // keep compiling
   end else begin
      classTyp:=TClassSymbol(typ);
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.AddTypeSymbol(classTyp, FTok.HotPos);
   end;

   if typeName<>'' then
      Result:=TClassOfSymbol.Create(typeName, classTyp)
   else Result:=classTyp.ClassOf;
end;

// ReadClass
//
function TdwsCompiler.ReadClass(const typeName : String) : TClassSymbol;
var
   name : String;
   namePos : TScriptPos;
   sym, typ : TSymbol;
   propSym : TPropertySymbol;
   constSym : TClassConstSymbol;
   ancestorTyp : TClassSymbol;
   intfTyp : TInterfaceSymbol;
   interfaces : TList;
   missingMethod : TMethodSymbol;
   isInSymbolTable: Boolean;
   visibility : TdwsVisibility;
   tt : TTokenType;
   i : Integer;
begin
   // Check for a forward declaration of this class
   sym:=FProg.Table.FindSymbol(typeName, cvMagic);
   Result:=nil;

   if Assigned(sym) then begin
      if sym is TClassSymbol then begin
         if TClassSymbol(sym).IsForwarded then
            Result:=TClassSymbol(sym)
         else FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassAlreadyDefined, [sym.Name]);
      end else begin
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NameAlreadyExists, [sym.Name]);
      end;
      if Result=nil then // make anonymous to keep compiling
         Result:=TClassSymbol.Create('', FUnitSymbol);
   end;

   isInSymbolTable := Assigned(Result);

   if not Assigned(Result) then
      Result:=TClassSymbol.Create(typeName, FUnitSymbol);

   // forwarded declaration
   if FTok.Test(ttSEMI) then begin
      if Result.IsForwarded then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassForwardAlreadyExists, [sym.Name]);
      Result.SetForwardedPos(FTok.HotPos);
      Exit;
   end else Result.ClearIsForwarded;

   if not isInSymbolTable then
      FProg.Table.AddSymbol(Result);   // auto-forward
   interfaces:=TList.Create;
   try
      try
         if FTok.TestDelete(ttSTATIC) then
            Result.IsStatic:=True;
         tt:=FTok.TestDeleteAny([ttABSTRACT, ttSEALED]);
         case tt of
            ttABSTRACT :
               Result.IsExplicitAbstract:=True;
            ttSEALED :
               Result.IsSealed:=True;
         end;
         if FTok.TestDelete(ttEXTERNAL) then
            Result.IsExternal:=True;

         // inheritance
         if FTok.TestDelete(ttBLEFT) then begin

            if not FTok.TestDeleteNamePos(name, namePos) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

            Typ := FProg.Table.FindSymbol(name, cvMagic);
            if not (Typ is TClassSymbol) then begin
               FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAClass, [name]);
               Typ:=FProg.TypObject;
            end;

            ancestorTyp:=TClassSymbol(typ);

            if ancestorTyp.IsForwarded then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassNotImplementedYet, [Name]);

            if ancestorTyp.IsSealed then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassIsSealed, [Typ.Name]);

            while FTok.TestDelete(ttCOMMA) do begin

               if not FTok.TestDeleteNamePos(name, namePos) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

               intfTyp:=TInterfaceSymbol(FProg.Table.FindSymbol(name, cvMagic, TInterfaceSymbol));
               if intfTyp=nil then
                  FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAnInterface, [name])
               else begin
                  if interfaces.IndexOf(intfTyp)>=0 then
                     FMsgs.AddCompilerErrorFmt(namePos, CPE_InterfaceAlreadyImplemented, [name])
                  else interfaces.Add(intfTyp);
               end;
            end;

            if not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

         end else ancestorTyp:=FProg.TypObject;

         if     Result.IsStatic
            and (ancestorTyp<>FProg.TypObject)
            and (not ancestorTyp.IsStatic) then
           FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassAncestorNotStatic, [ancestorTyp.Name]);

         Result.InheritFrom(ancestorTyp);

         visibility:=cvPublished;

         // standard class definition
         while not FTok.Test(ttEND) do begin

            // Read methods and properties
            tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
                                    ttCONSTRUCTOR, ttDESTRUCTOR,
                                    ttCLASS, ttPROPERTY, ttCONST,
                                    ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED]);
            case tt of

               ttFUNCTION :
                  Result.AddMethod(ReadMethodDecl(Result, fkFunction, visibility, False));
               ttPROCEDURE :
                  Result.AddMethod(ReadMethodDecl(Result, fkProcedure, visibility, False));
               ttMETHOD :
                  Result.AddMethod(ReadMethodDecl(Result, fkMethod, visibility, False));
               ttCONSTRUCTOR :
                  Result.AddMethod(ReadMethodDecl(Result, fkConstructor, visibility, False));
               ttDESTRUCTOR :
                  Result.AddMethod(ReadMethodDecl(Result, fkDestructor, visibility, False));
               ttCLASS : begin

                  tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttOPERATOR]);
                  case tt of
                     ttPROCEDURE :
                        Result.AddMethod(ReadMethodDecl(Result, fkProcedure, visibility, True));
                     ttFUNCTION :
                        Result.AddMethod(ReadMethodDecl(Result, fkFunction, visibility, True));
                     ttMETHOD :
                        Result.AddMethod(ReadMethodDecl(Result, fkMethod, visibility, True));
                     ttOPERATOR :
                        Result.AddOperator(ReadClassOperatorDecl(Result));
                  else
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
                  end;

               end;
               ttPROPERTY : begin

                  propSym := ReadPropertyDecl(Result, visibility);
                  Result.AddProperty(propSym);

               end;
               ttCONST : begin

                  constSym:=ReadConstDecl(TClassConstSymbol) as TClassConstSymbol;
                  constSym.Visibility:=visibility;
                  Result.AddConst(constSym);
                  ReadSemiColon;

               end;
               ttPRIVATE : visibility:=cvPrivate;
               ttPROTECTED : visibility:=cvProtected;
               ttPUBLIC : visibility:=cvPublic;
               ttPUBLISHED : visibility:=cvPublished;

            else

               if FTok.TestName then begin
                  ReadClassFields(Result, visibility);
                  if not (FTok.TestDelete(ttSEMI) or FTok.Test(ttEND)) then
                     Break;
               end else Break;

            end;

         end; // while

         if not FTok.TestDelete(ttEND) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);

         if not FMsgs.HasErrors then begin
            // resolve interface tables
            for i:=0 to interfaces.Count-1 do begin
               intfTyp:=interfaces[i];
               if not Result.AddInterface(intfTyp, cvPrivate, missingMethod) then
                  FMsgs.AddCompilerErrorFmt(namePos, CPE_MissingMethodForInterface, [missingMethod.Name, name]);
            end;
            Result.AddOverriddenInterfaces;
         end;

      except
         // Set Result to nil to prevent auto-forward removal then re-reraise
         Result:=nil;
         raise;
      end;
   finally
      interfaces.Free;
      if not isInSymbolTable then
         FProg.Table.Remove(Result);  // auto-forward
   end;
end;

// ReadClassFields
//
procedure TdwsCompiler.ReadClassFields(const classSymbol : TClassSymbol; aVisibility : TdwsVisibility);
var
   i : Integer;
   sym : TSymbol;
   typ : TTypeSymbol;
   fieldSym : TFieldSymbol;
   names : TStringList;
   posArray : TScriptPosArray;    // positions of items pulled from ReadNameList call
begin
   names:=TStringList.Create;
   try
      // Conditionally pass in dynamic array
      if coSymbolDictionary in FOptions then
         ReadNameList(Names, PosArray)     // use overloaded version
      else ReadNameList(Names);

      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      typ := ReadType('', tcMember);
      for i := 0 to Names.Count - 1 do begin
         // Check if name isn't already used
         sym := classSymbol.Members.FindLocal(Names[i]);
         if Assigned(sym) then
            MemberSymbolWithNameAlreadyExists(sym);

         // Create Internal Field
         fieldSym := TFieldSymbol.Create(Names[i], typ, aVisibility);
         classSymbol.AddField(fieldSym);

         // Enter Field symbol in dictionary
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddValueSymbol(fieldSym, PosArray[i], [suDeclaration]);
      end;
   finally
      names.Free;
   end;
end;

// ReadInterface
//
function TdwsCompiler.ReadInterface(const typeName : String) : TInterfaceSymbol;
var
   sym : TSymbol;
   ancestor : TInterfaceSymbol;
   name : String;
   namePos, hotPos : TScriptPos;
   tt : TTokenType;
   propSym : TPropertySymbol;
begin
   hotPos:=FTok.HotPos;
   sym:=FProg.Table.FindSymbol(typeName, cvMagic);

   if Assigned(sym) then begin
      if sym is TInterfaceSymbol then
         FMsgs.AddCompilerErrorFmt(hotPos, CPE_InterfaceAlreadyDefined, [sym.Name])
      else FMsgs.AddCompilerErrorFmt(hotPos, CPE_NameAlreadyExists, [sym.Name]);
      // keep compiling, make it anonymous
      Result:=TInterfaceSymbol.Create('', FUnitSymbol);
   end else Result:=TInterfaceSymbol.Create(typeName, FUnitSymbol);

   try

      if FTok.TestDelete(ttBLEFT) then begin
         if not FTok.TestDeleteNamePos(name, namePos) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
         ancestor:=TInterfaceSymbol(FProg.Table.FindSymbol(name, cvMagic, TInterfaceSymbol));
         if ancestor=nil then
            FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAnInterface, [Name])
         else Result.InheritFrom(ancestor);
         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(namePos, CPE_BrackRightExpected);
      end;
      if Result.Parent=nil then
         Result.InheritFrom(FProg.TypInterface);

      while not FTok.Test(ttEND) do begin

         // Read methods and properties
         tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttPROPERTY]);
         case tt of
            ttFUNCTION :
               Result.AddMethod(ReadIntfMethodDecl(Result, fkFunction));
            ttPROCEDURE :
               Result.AddMethod(ReadIntfMethodDecl(Result, fkProcedure));
            ttMETHOD :
               Result.AddMethod(ReadIntfMethodDecl(Result, fkMethod));

            ttPROPERTY : begin

               propSym := ReadPropertyDecl(Result, cvPublished);
               Result.AddProperty(propSym);

            end;
         else
            Break;
         end;

      end;

      if not FTok.TestDelete(ttEND) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);

   except
      Result.Free;
      raise;
   end;

   if coSymbolDictionary in FOptions then
      FSymbolDictionary.AddTypeSymbol(Result, hotPos, [suDeclaration]);
end;

// CheckFuncParams
//
function TdwsCompiler.CheckFuncParams(paramsA, paramsB : TSymbolTable;
                                      indexSym : TSymbol = nil;
                                      typSym : TTypeSymbol = nil) : Boolean;
begin
   Result:=False;

   if Assigned(indexSym) then begin
      if Assigned(typSym) then begin
         if paramsB.Count<>paramsA.Count+2 then Exit;
         if paramsB[paramsA.Count+1].Typ<>typSym then Exit;
         if paramsB[paramsA.Count].Typ<>indexSym then Exit;
      end else begin
         if paramsB.Count<>paramsA.Count+1 then Exit
         else if paramsB[paramsA.Count].Typ<>indexSym then Exit;
      end;
   end else begin
      if Assigned(typSym) then begin
         if paramsB.Count<>paramsA.Count+1 then Exit;
         if paramsB[paramsA.Count].Typ<>typSym then Exit;
      end else begin
         if paramsA.Count<>paramsB.Count then Exit;
      end;
   end;

   Result:=CheckParams(paramsA, paramsB, False);
end;

// ReadClassOperatorDecl
//
function TdwsCompiler.ReadClassOperatorDecl(ClassSym: TClassSymbol) : TClassOperatorSymbol;
var
   tt : TTokenType;
   usesName : String;
   usesPos : TScriptPos;
   sym : TTypeSymbol;
begin
   tt:=FTok.TestDeleteAny([ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN, ttIN,
                           ttCARET_ASSIGN]);
   if tt=ttNone then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_OverloadableOperatorExpected);

   Result:=TClassOperatorSymbol.Create(tt);
   try
      Result.Typ:=ReadType('', tcOperand);

      if ClassSym.FindClassOperatorStrict(tt, Result.Typ, False)<>nil then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassOperatorRedefined, [Result.Typ.Name]);

      if not FTok.TestDelete(ttUSES) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UsesExpected);

      if not FTok.TestDeleteNamePos(usesName, usesPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      sym:=ClassSym.Members.FindTypeSymbol(usesName, cvPrivate);

      if    (not Assigned(sym))
         or (not (sym is TMethodSymbol)) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureMethodExpected);

      Result.UsesSym:=TMethodSymbol(sym);
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.AddTypeSymbol(sym, usesPos);

      if Result.UsesSym.Params.Count<>1 then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_SingleParameterExpected);
      if tt=ttIN then begin
         if Result.UsesSym.Params[0].Typ<>Result.Typ then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidParameterType, [Result.UsesSym.Name]);
         if not Result.UsesSym.Result.Typ.IsOfType(FProg.TypBoolean) then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidResultType, [Result.UsesSym.Result.Typ.Name]);
      end else begin
         if Result.UsesSym.Params[0].Typ<>Result.Typ then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidParameterType, [Result.UsesSym.Name]);
      end;

      ReadSemiColon;
   except
      // Remove reference to symbol (gets freed)
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.Remove(Result);
      Result.Free;
      raise;
   end;
end;

// ReadPropertyDecl
//
function TdwsCompiler.ReadPropertyDecl(structSym : TStructuredTypeSymbol; aVisibility : TdwsVisibility) : TPropertySymbol;
var
   x : Integer;
   name : String;
   sym : TSymbol;
   typ : TTypeSymbol;
   arrayIndices : TSymbolTable;
   propPos : TScriptPos;
   accessPos : TScriptPos;  // Position where either a Read or Write symbol is found
   indexExpr : TTypedExpr;
   indexTyp : TTypeSymbol;
begin
   // Read property name
   if not FTok.TestDeleteNamePos(name, propPos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   // Check if property name is free
   sym := structSym.Members.FindSymbolFromScope(name, CurrentClass);
   if Assigned(sym) then begin
      if sym is TPropertySymbol then begin
         if TPropertySymbol(sym).StructSymbol = structSym then
            MemberSymbolWithNameAlreadyExists(sym);
      end else MemberSymbolWithNameAlreadyExists(sym);
   end;

   arrayIndices := TSymbolTable.Create;
   try
      // Check if it is an array property
      ReadArrayParams(arrayIndices);

      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      typ := ReadType('', tcProperty);
      Result := TPropertySymbol.Create(name, typ, aVisibility);
      try
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddValueSymbol(Result, propPos, [suDeclaration]);

         if FTok.TestDelete(ttINDEX) then begin
            indexExpr := ReadExpr;
            if not (indexExpr is TConstExpr) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);
            indexTyp := indexExpr.Typ;
            Result.SetIndex(TConstExpr(indexExpr).Data[FExec],
                            TConstExpr(indexExpr).Addr[FExec], indexTyp);
         end else indexTyp := nil;

         // Generates a suggestion of how to fix it for class completion
         if FTok.TestDelete(ttREAD) then begin
            if not FTok.TestDeleteNamePos(name, accessPos) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

            sym := structSym.Members.FindSymbol(name, cvPrivate);

            if not Assigned(sym) or (sym is TPropertySymbol) then begin
               { Register the error and break the compilation process }
               FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);
            end;

            if Result.Typ <> sym.Typ then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleType, [name])
            else if sym is TMethodSymbol then begin
               if not (TFuncSymbol(sym).Kind in [fkFunction, fkMethod]) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionMethodExpected)
               else if not CheckFuncParams(arrayIndices, TMethodSymbol(sym).Params, indexTyp) then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleParameters, [name]);
            end else if arrayIndices.Count > 0 then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionMethodExpected);

            Result.ReadSym := sym;
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddSymbol(sym, accessPos, [suReference, suRead])
         end;

         // Generates a suggestion of how to fix it for class completion
         if FTok.TestDelete(ttWRITE) then begin
            // Read name
            if not FTok.TestDeleteNamePos(name, accessPos) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

            // Check if symbol exists
            sym := structSym.Members.FindSymbol(Name, cvPrivate);

            if not Assigned(sym) or (sym is TPropertySymbol) then begin
               { Register the error and break the compilation process }
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);
               raise EClassPropertyIncompleteError.Create('');
            end;

            if sym is TFuncSymbol then begin
               if    (not (TFuncSymbol(sym).Kind in [fkProcedure, fkMethod]))
                  or (TFuncSymbol(sym).Typ<>nil) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ProcedureMethodExpected)
               else if not CheckFuncParams(arrayIndices, TFuncSymbol(sym).Params, indexTyp, Result.Typ) then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleParameters, [name]);
            end else if Result.Typ <> sym.Typ then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleWriteSymbol, [Name]);

            Result.WriteSym := sym;
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddSymbol(sym, accessPos, [suReference, suWrite]);
         end;

         if (Result.ReadSym = nil) and (Result.WriteSym = nil) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ReadOrWriteExpected, [name]);

         ReadSemiColon;

         // Add array indices to property symbol (if any)
         for x := 0 to arrayIndices.Count - 1 do
            Result.ArrayIndices.AddSymbol(arrayIndices[x]);
         arrayIndices.Clear;

         // Array-Prop can be default
         if Result.HasArrayIndices then begin
            if FTok.TestDelete(ttDEFAULT) then begin
               ReadSemiColon;
               if structSym.DefaultProperty<>nil then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MultipleDefaultProperties,
                                            [structSym.Name, structSym.DefaultProperty.Name])
               else structSym.DefaultProperty:=Result;
            end;
         end;

      except
         // Remove reference to symbol (gets freed)
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.Remove(Result);
         Result.Free;
         raise;
      end;
   finally
      arrayIndices.Free;
  end;

end;

// ReadRecord
//
function TdwsCompiler.ReadRecord(const typeName : String) : TRecordSymbol;
var
   x : Integer;
   names : TStringList;
   member : TFieldSymbol;
   typ : TTypeSymbol;
   posArray : TScriptPosArray;
begin
   Result := TRecordSymbol.Create(typeName, FUnitSymbol);
   try
      names := TStringList.Create;
      try
         repeat

            if FTok.Test(ttEND) then
               break;

            if coSymbolDictionary in FOptions then
               ReadNameList(names, posArray)     // use overloaded version
            else ReadNameList(names);

            if not FTok.TestDelete(ttCOLON) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

            typ := ReadType('', tcMember);
            for x := 0 to names.Count - 1 do begin
               if Result.Members.FindLocal(names[x]) <> nil then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NameAlreadyExists, [names[x]]);

               member := TFieldSymbol.Create(names[x], typ, cvPublic);
               Result.AddField(member);

               // Add member symbols and positions
               if coSymbolDictionary in FOptions then
                  FSymbolDictionary.AddValueSymbol(member, posArray[x], [suDeclaration]);
            end;

         until not FTok.TestDelete(ttSEMI) or FTok.Test(ttEND);
      finally
         names.Free;
      end;

      if not FTok.TestDelete(ttEND) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
   except
      // Removed added record symbols. Destroying object
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.Remove(Result);
      Result.Free;
      raise;
   end;
end;

// ReadTry
//
function TdwsCompiler.ReadTry: TExceptionExpr;
var
   tryBlock : TNoResultExpr;
   tt : TTokenType;
   wasExcept : Boolean;
begin
   wasExcept:=FIsExcept;
   FIsExcept:=False;
   try
      tryBlock:=ReadBlocks([ttFINALLY, ttEXCEPT], tt);
      if tt=ttEXCEPT then begin
         FIsExcept:=True;
         Result:=ReadExcept(tryBlock);
      end else begin
         Result:=ReadFinally(tryBlock);
      end;
   finally
      FIsExcept:=wasExcept;
   end;
end;

// ReadFinally
//
function TdwsCompiler.ReadFinally(tryExpr : TNoResultExpr) : TFinallyExpr;
var
   tt : TTokenType;
begin
   Result:=TFinallyExpr.Create(FProg, tryExpr.ScriptPos);
   Result.TryExpr:=tryExpr;
   try
      FFinallyExprs.Push(True);
      try
         Result.HandlerExpr:=ReadBlocks([ttEND], tt);
      finally
         FFinallyExprs.Pop;
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadRaise
//
function TdwsCompiler.ReadRaise : TRaiseBaseExpr;
var
   exceptExpr : TTypedExpr;
   exceptObjTyp : TSymbol;
begin
   if FIsExcept and (FTok.Test(ttSEMI) or FTok.Test(ttEND)) then
      Result:=TReraiseExpr.Create(FProg, FTok.HotPos)
   else begin
      exceptExpr:=ReadExpr;
      try
         exceptObjTyp:=exceptExpr.Typ;
         if not (    (exceptObjTyp is TClassSymbol)
                 and TClassSymbol(exceptObjTyp).IsOfType(FProg.TypException)) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExceptionObjectExpected);
         Result:=TRaiseExpr.Create(FProg, FTok.HotPos, exceptExpr);
      except
         exceptExpr.Free;
         raise;
      end;
   end;
end;

// ReadExcept
//
function TdwsCompiler.ReadExcept(tryExpr : TNoResultExpr) : TExceptExpr;
var
   tt : TTokenType;
   doExpr : TExceptDoExpr;
   varName : String;
   classSym : TTypeSymbol;
begin
   Result:=TExceptExpr.Create(FProg, TryExpr.ScriptPos);
   try
      Result.TryExpr:=tryExpr;
      if FTok.Test(ttON) then begin
         while FTok.TestDelete(ttON) do begin
            if not FTok.TestName then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
            varName:=FTok.GetToken.FString;
            FTok.KillToken;

            if not FTok.TestDelete(ttCOLON) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

            classSym:=ReadType('', tcExceptionClass);
            if not (classSym.BaseType is TClassSymbol) then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassRefExpected);

            if not FTok.TestDelete(ttDO) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

            doExpr:=TExceptDoExpr.Create(FProg, FTok.HotPos);
            try
               doExpr.ExceptionVar:=TDataSymbol.Create(varName, ClassSym);

               FProg.Table.AddSymbol(doExpr.ExceptionVar);
               try
                  doExpr.DoBlockExpr:=ReadBlock;
               finally
                  FProg.Table.Remove(doExpr.ExceptionVar);
               end;
            except
               doExpr.Free;
               raise;
            end;

            Result.AddDoExpr(DoExpr);

            if not FTok.Test(ttEND) then
               ReadSemiColon;
         end;

         if FTok.TestDelete(ttELSE) then
            Result.ElseExpr:=ReadBlocks([ttEND], tt)
         else if not FTok.TestDelete(ttEND) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      end else begin
         Result.HandlerExpr:=ReadBlocks([ttEND], tt);
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExit
//
function TdwsCompiler.ReadExit : TNoResultExpr;
var
   gotParenthesis : Boolean;
   leftExpr : TDataExpr;
   assignExpr : TNoResultExpr;
   proc : TdwsProcedure;
   exitPos : TScriptPos;
begin
   exitPos:=FTok.HotPos;
   if FTok.TestAny([ttEND, ttSEMI, ttELSE, ttUNTIL])<>ttNone then
      Result:=TExitExpr.Create(FProg, FTok.HotPos)
   else begin
      if not (FProg is TdwsProcedure) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoResultRequired);
      gotParenthesis:=FTok.TestDelete(ttBLEFT);
      proc:=TdwsProcedure(FProg);
      if proc.Func.Result=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoResultRequired);
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.AddValueSymbol(proc.Func.Result, exitPos, [suReference, suWrite]);
      leftExpr:=TVarExpr.CreateTyped(FProg, proc.Func.Result.Typ, proc.Func.Result);
      try
         assignExpr:=ReadAssign(ttASSIGN, leftExpr);
         try
            leftExpr:=nil;
            if gotParenthesis and not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
            Result:=TExitValueExpr.Create(FProg, exitPos, assignExpr);
         except
            assignExpr.Free;
            raise;
         end;
      except
         leftExpr.Free;
         raise;
      end;
   end;
end;

// ReadType
//
function TdwsCompiler.ReadType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
var
   tt : TTokenType;
   name : String;
   namePos : TScriptPos;
   sym : TSymbol;
begin
   tt:=FTok.TestDeleteAny([ttRECORD, ttARRAY, ttCLASS, ttINTERFACE, ttBLEFT,
                           ttPROCEDURE, ttFUNCTION, ttMETHOD]);
   case tt of
      ttRECORD :
         Result:=ReadRecord(typeName);

      ttARRAY :
         Result:=ReadArrayType(typeName, typeContext);

      ttCLASS :
         if FTok.TestDelete(ttOF) then
            Result:=ReadClassOf(typeName)
         else begin
            if typeContext=tcDeclaration then
               Result:=ReadClass(typeName)
            else begin
               Result:=nil;
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);
            end;
         end;

      ttINTERFACE :
         if typeContext=tcDeclaration then
            Result:=ReadInterface(typeName)
         else begin
            Result:=nil;
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);
         end;

      ttBLEFT :
         Result:=ReadEnumeration(typeName);

      ttPROCEDURE : begin
         Result:=ReadProcDecl(fkProcedure, False, True);
         Result.SetName(typeName);
      end;

      ttFUNCTION : begin
         Result:=ReadProcDecl(fkFunction, False, True);
         Result.SetName(typeName);
      end;

      ttMETHOD : begin
         Result:=ReadProcDecl(fkMethod, False, True);
         Result.SetName(typeName);
      end;

   else

      if FTok.TestName then begin

         name:=FTok.GetToken.FString;
         namePos:=FTok.HotPos;        // get the position before token is deleted
         FTok.KillToken;
         sym:=FProg.Table.FindSymbol(name, cvMagic);
         Result:=nil;

         if (sym<>nil) and (sym.ClassType=TUnitSymbol) then begin
            if not FTok.TestDelete(ttDOT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
            if not FTok.TestName then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
            name:=FTok.GetToken.FString;
            FTok.KillToken;
            sym:=TUnitSymbol(sym).Table.FindLocal(name);
         end;

         if not Assigned(sym) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_TypeUnknown, [name])
         else if not sym.IsType then begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidType, [name]);
            Result:=FProg.TypVariant; // keep compiling
         end else Result:=TTypeSymbol(sym);

         // Create name symbol, e. g.: type a = integer;
         if typeName <> '' then
            Result:=TAliasSymbol.Create(typeName, Result);

         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddTypeSymbol(Result, namePos);

      end else begin

         Result:=nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);

      end;

   end;

   // Ensure that unnamed symbols will be freed
   if Result.Name='' then
      FProg.RootTable.AddToDestructionList(Result);
end;

// ReadExpr
//
function TdwsCompiler.ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
var
   r : TTypedExpr;
   tt : TTokenType;
   hotPos : TScriptPos;
   opExpr : TTypedExpr;
begin
   // Read left argument
   hotPos:=FTok.HotPos;
   Result:=ReadExprAdd(expecting);
   try
      // Read operator
      repeat
         tt:=FTok.TestDeleteAny([ttEQ, ttNOTEQ, ttLESS, ttLESSEQ, ttGTR, ttGTREQ,
                                 ttIS, ttAS, ttIMPLEMENTS]);
         if tt=ttNone then Break;

         hotPos := FTok.HotPos;

         // Read right argument
         r:=ReadExprAdd;
         try
            case tt of
               ttIS : begin
                  if not (Result.Typ is TClassSymbol) then
                     FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                  else if not (r.Typ is TClassOfSymbol) then
                     FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                  Result:=TIsOpExpr.Create(FProg, Result, r)
               end;
               ttAS : begin
                  if Result.Typ is TInterfaceSymbol then begin
                     if r.Typ is TInterfaceSymbol then begin
                        Result:=TIntfAsIntfExpr.Create(FProg, hotPos, Result, TInterfaceSymbol(r.Typ));
                     end else begin
                        if not (r.Typ is TClassOfSymbol) then
                           FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                        Result:=TIntfAsClassExpr.Create(FProg, hotPos, Result, TClassOfSymbol(r.Typ).Typ);
                     end;
                  end else begin
                     if not (Result.Typ is TClassSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
                     if r.Typ is TInterfaceSymbol then
                        Result:=TObjAsIntfExpr.Create(FProg, hotPos, Result, TInterfaceSymbol(r.Typ))
                     else begin
                        if not (r.Typ is TClassOfSymbol) then
                           FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                        Result:=TObjAsClassExpr.Create(FProg, hotPos, Result, TClassOfSymbol(r.Typ).Typ);
                     end;
                  end;
                  r.Free;
               end;
               ttIMPLEMENTS : begin
                  if not (r.Typ is TInterfaceSymbol) then
                     FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                  if Result.Typ is TClassOfSymbol then
                     Result:=TClassImplementsIntfOpExpr.Create(FProg, Result, r)
                  else begin
                     if not (Result.Typ is TClassSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
                     Result:=TImplementsIntfOpExpr.Create(FProg, Result, r);
                  end;
               end;
            else
               if    (Result.Typ is TClassSymbol)
                  or (Result.Typ is TInterfaceSymbol)
                  or (Result.Typ=FProg.TypNil) then begin
                  case tt of
                     ttEQ, ttNOTEQ: begin
                        if not ((r.Typ.ClassType=Result.Typ.ClassType) or (r.Typ=FProg.TypNil)) then
                           if Result.Typ is TClassSymbol then
                              FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                           else FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                        if Result.Typ is TClassSymbol then
                           Result:=TObjCmpExpr.Create(FProg, Result, r)
                        else Result:=TIntfCmpExpr.Create(FProg, Result, r);
                        if tt=ttNOTEQ then
                           Result:=TNotBoolExpr.Create(FProg, Result);
                     end;
                  else
                     FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     Result:=TRelOpExpr.Create(FProg, Result, r); // keep going
                  end;
               end else begin
                  opExpr:=FOperators.GenerateTyped(FProg, hotPos, tt, Result, r);
                  if opExpr=nil then begin
                     FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     // keep going
                     Result:=TRelOpExpr.Create(FProg, Result, r);
                  end else Result:=opExpr;
               end;
            end;
         except
            r.Free;
            raise;
         end;
      until False;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExprAdd
//
function TdwsCompiler.ReadExprAdd(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
var
   right: TTypedExpr;
   tt: TTokenType;
   hotPos: TScriptPos;
   opExpr : TTypedExpr;
begin
   // Read left argument
   if leftExpr=nil then
      Result:=ReadExprMult(expecting)
   else Result:=leftExpr;
   try

      repeat
         tt:=FTok.TestDeleteAny([ttPLUS, ttMINUS, ttOR, ttAND, ttXOR, ttIMPLIES,
                                 ttSHL, ttSHR, ttIN, ttNOT,
                                 ttLESSLESS, ttGTRGTR]);
         if tt=ttNone then Break;

         hotPos := FTok.HotPos;

         if tt = ttNOT then begin

            if not FTok.TestDelete(ttIN) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_InExpected);
            Result:=ReadExprIn(Result);
            Result:=TNotBoolExpr.Create(FProg, Result);

         end else if tt = ttIN then

            Result := ReadExprIn(Result)

         else begin
            // Read right argument
            right := ReadExprMult;
            try
               // Generate function and add left and right argument
               if (Result.Typ=nil) or (right.Typ=nil) then
                  FMsgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands)
               else begin
                  opExpr:=FOperators.GenerateTyped(FProg, hotPos, tt, Result, right);
                  if opExpr=nil then begin
                     FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     // fake result to keep compiler going and report further issues
                     Result:=TBinaryOpExpr.Create(FProg, Result, right);
                     Result.Typ:=FProg.TypVariant;
                  end else Result:=opExpr;
               end;
            except
               right.Free;
               raise;
            end;
         end;

         if Optimize then
            Result:=Result.OptimizeToTypedExpr(FProg, FExec);
      until False;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExprMult
//
function TdwsCompiler.ReadExprMult(expecting : TTypeSymbol = nil) : TTypedExpr;
var
   right : TTypedExpr;
   tt : TTokenType;
   hotPos : TScriptPos;
   opExpr : TTypedExpr;
begin
   // Read left argument
   Result := ReadTerm(False, expecting);
   try
      repeat
         tt:=FTok.TestDeleteAny([ttTIMES, ttDIVIDE, ttMOD, ttDIV, ttCARET]);
         if tt=ttNone then Break;

         // Save position of the operator
         hotPos := FTok.HotPos;

         // Read right argument
         right := ReadTerm;
         try

            // Generate function and add left and right argument
            if (Result.Typ=nil) or (right.Typ=nil) then
               FMsgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands)
            else begin
               opExpr:=FOperators.GenerateTyped(FProg, hotPos, tt, Result, right);
               if opExpr=nil then begin
                  FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                  // fake result to keep compiler going and report further issues
                  Result:=TBinaryOpExpr.Create(FProg, Result, right);
                  Result.Typ:=right.Typ;
               end else Result:=opExpr;
            end;

         except
            right.Free;
            raise;
         end;

         if Optimize then
            Result:=Result.OptimizeToTypedExpr(FProg, FExec);
      until False;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExprIn
//
function TdwsCompiler.ReadExprIn(var left : TTypedExpr) : TTypedExpr;
var
   hotPos : TScriptPos;
   setExpr : TTypedExpr;
   classOpSymbol : TClassOperatorSymbol;
   classOpExpr : TFuncExpr;
begin
   hotPos:=FTok.HotPos;

   if FTok.TestDelete(ttALEFT) then begin

      Result:=ReadExprInConditions(left);

   end else begin

      setExpr:=ReadExpr;
      try

         if (setExpr.Typ=nil) or not (setExpr.Typ is TClassSymbol) then
            FMsgs.AddCompilerStop(hotPos, CPE_ArrayBracketOrClassExpected);
         if not (setExpr is TDataExpr) then
            FMsgs.AddCompilerStop(hotPos, CPE_ObjectExpected);

         classOpSymbol:=(setExpr.Typ as TClassSymbol).FindClassOperator(ttIN, left.Typ);
         if classOpSymbol=nil then
            FMsgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands);
         classOpExpr:=GetMethodExpr(classOpSymbol.UsesSym, (setExpr as TDataExpr),
                                    rkObjRef, hotPos, False);
         try
            setExpr:=nil;
            classOpExpr.AddArg(left);
            left:=nil;
            classOpExpr.TypeCheckArgs(FProg);
         except
            classOpExpr.Free;
            raise;
         end;
         Result:=classOpExpr;

      except
         setExpr.Free;
         raise;
      end;

   end;
end;

// ReadExprInConditions
//
function TdwsCompiler.ReadExprInConditions(var left : TTypedExpr) : TInOpExpr;
var
   i : Integer;
   condList : TCaseConditions;
   hotPos : TScriptPos;
begin
   hotPos:=FTok.HotPos;

   condList:=TCaseConditions.Create;
   try
      if not FTok.TestDelete(ttARIGHT) then begin
         ReadCaseConditions(condList, left);
         if not FTok.TestDelete(ttARIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
      end;

      Result:=TInOpExpr.Create(FProg, left);
      left:=nil;

      // Add case conditions to TCaseExpr
      for i:=0 to condList.Count-1 do
         Result.AddCaseCondition(condList[i]);
      condList.ExtractAll;
   finally
      condList.Free;
   end;
end;

// ReadTerm
//
function TdwsCompiler.ReadTerm(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TTypedExpr;

   function ReadNilTerm : TTypedExpr;
   const
      cNilIntf : IUnknown = nil;
   begin
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypNil, cNilIntf);
   end;

   function ReadNotTerm : TUnaryOpExpr;
   var
      operand : TTypedExpr;
      hotPos : TScriptPos;
   begin
      hotPos:=FTok.HotPos;
      operand:=ReadTerm;
      if operand.IsOfType(FProg.TypBoolean) then
         Result:=TNotBoolExpr.Create(FProg, operand)
      else if operand.IsOfType(FProg.TypInteger) then
         Result:=TNotIntExpr.Create(FProg, operand)
      else begin
         if not operand.IsOfType(FProg.TypVariant) then
            FMsgs.AddCompilerError(hotPos, CPE_BooleanOrIntegerExpected);
         Result:=TNotVariantExpr.Create(FProg, operand);
      end;
   end;

   function ReadTrue : TConstExpr;
   begin
      Result:=TConstBooleanExpr.CreateUnified(FProg, nil, True);
   end;

   function ReadFalse : TConstExpr;
   begin
      Result:=TConstBooleanExpr.CreateUnified(FProg, nil, False);
   end;

   function ReadNull(expecting : TTypeSymbol) : TConstExpr;
   begin
      Result:=TConstExpr.Create(FProg, expecting, Null);
   end;

var
   tt : TTokenType;
   nameExpr : TProgramExpr;
begin
   tt:=FTok.TestAny([ttPLUS, ttMINUS, ttALEFT, ttNOT, ttBLEFT,
                     ttTRUE, ttFALSE, ttNIL, ttSWITCH]);
   if not (tt in [ttNone, ttSWITCH]) then
      FTok.KillToken;
   case tt of
      ttPLUS :
         Result:=ReadTerm; // (redundant) plus sign
      ttMINUS :
         Result:=ReadNegation;
      ttALEFT :
         Result:=ReadArrayConstant(expecting);
      ttNOT :
         Result:=ReadNotTerm;
      ttBLEFT : begin
         // Read expression in brackets
         Result := ReadExpr;
         if not FTok.TestDelete(ttBRIGHT) then begin
            Result.Free;
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
         end;
         if Result.Typ is TClassSymbol then
            Result:=ReadSymbol(Result, isWrite) as TTypedExpr;
      end;
      ttTRUE :
         Result:=ReadTrue;
      ttFALSE :
         Result:=ReadFalse;
      ttNIL :
         Result:=ReadNilTerm;
      ttSWITCH :
         Result:=ReadExprSwitch;
   else
      if FTok.Test(ttINHERITED) or FTok.TestName  then begin
         // Variable or Function
         nameExpr:=ReadName(isWrite, expecting);
         if (nameExpr<>nil) and not (nameExpr is TTypedExpr) then begin
            nameExpr.Free;
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExpressionExpected);
            // keep compiling
            if expecting=nil then
               expecting:=FProg.TypVariant;
            Result:=ReadNull(expecting);
         end else Result:=TTypedExpr(nameExpr);
      end else // Constant values in the code
         Result := ReadConstValue;
   end;

   // No expression found
   if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);
end;

// ReadNegation
//
function TdwsCompiler.ReadNegation : TTypedExpr;
var
   negExprClass : TUnaryOpExprClass;
   negTerm : TTypedExpr;
begin
   negTerm:=ReadTerm;
   if negTerm.IsOfType(FProg.TypInteger) then
      negExprClass:=TNegIntExpr
   else if negTerm.IsOfType(FProg.TypFloat) then
      negExprClass:=TNegFloatExpr
   else begin
      if not negTerm.IsOfType(FProg.TypVariant) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NumericalExpected);
      negExprClass:=TNegVariantExpr;
   end;
   Result:=negExprClass.Create(FProg, negTerm);
   if Optimize or (negTerm is TConstExpr) then
      Result:=Result.OptimizeToTypedExpr(FProg, FExec);
end;

// ReadConstValue
//
function TdwsCompiler.ReadConstValue: TConstExpr;
var
   tt : TTokenType;
begin
   Result:=nil;
   tt:=FTok.TestAny([ttStrVal, ttIntVal, ttFloatVal]);
   if tt<>ttNone then begin
      case tt of
         ttIntVal :
            Result:=TConstIntExpr.CreateUnified(FProg, nil, FTok.GetToken.FInteger);
         ttFloatVal:
            Result:=TConstFloatExpr.CreateUnified(FProg, nil, FTok.GetToken.FFloat);
         ttStrVal:
            Result:=TConstStringExpr.CreateUnified(FProg, nil, FTok.GetToken.FString);
      end;
      FTok.KillToken;
   end;
end;

// ReadConstRecord
//
function TdwsCompiler.ReadConstRecord(symbol : TRecordSymbol) : TData;
var
   sym : TSymbol;
   memberSym : TFieldSymbol;
   memberSet : array of Boolean;
   expr : TTypedExpr;
   constExpr : TConstExpr;
begin
   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

   SetLength(memberSet, symbol.Size);

   SetLength(Result, symbol.Size);
   symbol.InitData(Result, 0);

   while not FTok.Test(ttBRIGHT) do begin
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      sym:=symbol.Members.FindLocal(FTok.GetToken.FString);
      if not (sym is TFieldSymbol) then begin
         FMsgs.AddCompilerErrorFmt(FTok.GetToken.FPos, CPE_UnknownMember, [FTok.GetToken.FString]);
         sym:=nil;
      end;
      memberSym:=TFieldSymbol(sym);
      if memberSym<>nil then begin
         if memberSet[memberSym.Offset] then
            FMsgs.AddCompilerError(FTok.GetToken.FPos, CPE_FieldAlreadySet);
         memberSet[memberSym.Offset]:=True;
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddSymbolReference(memberSym, FTok.GetToken.FPos, True);
      end;
      FTok.KillToken;
      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
      expr:=ReadExpr;
      try
         if not (expr is TConstExpr) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected)
         else if memberSym<>nil then begin
            constExpr:=TConstExpr(expr);
            if constExpr.Typ.IsOfType(FProg.TypInteger) and memberSym.Typ.IsOfType(FProg.TypFloat) then
               Result[memberSym.Offset]:=constExpr.EvalAsInteger(FExec)
            else if not constExpr.Typ.IsCompatible(memberSym.Typ) then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidConstType, [constExpr.Typ.Caption])
            else DWSCopyData(constExpr.Data[FExec], constExpr.Addr[FExec],
                             result, memberSym.Offset, memberSym.Typ.Size);
         end;
      finally
         expr.Free;
      end;
      if not FTok.TestDelete(ttSEMI) then
         Break;
   end;

   if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
end;

procedure TdwsCompiler.ReadArrayParams(ArrayIndices: TSymbolTable);
var
  x: Integer;
  names: TStringList;
  typSym: TTypeSymbol;
  isVarParam, isConstParam: Boolean;
begin
  if FTok.TestDelete(ttALEFT) then
  begin
    if FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ParamsExpected);

    // At least one argument was found
    names := TStringList.Create;
    try
      repeat
        isVarParam := FTok.TestDelete(ttVAR);

        if not isVarParam then
        begin
          isConstParam := FTok.TestDelete(ttCONST);
        end
        else
          isConstParam := False;

        ReadNameList(names);

        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)
        else
        begin
          typSym := ReadType('', tcParameter);
          for x := 0 to names.Count - 1 do
          begin
            if isVarParam then
              ArrayIndices.AddSymbol(TVarParamSymbol.Create(names[x], typSym))
            else if isConstParam then
              ArrayIndices.AddSymbol(TConstParamSymbol.Create(names[x], typSym))
            else
              ArrayIndices.AddSymbol(TParamSymbol.Create(names[x], typSym));
          end;
        end;
      until not FTok.TestDelete(ttSEMI);

    finally
      names.Free;
    end;

    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
  end;
end;

// ReadParams
//
procedure TdwsCompiler.ReadParams(const addParamMeth : TParamSymbolMethod; paramsToDictionary : Boolean = True);
var
   i : Integer;
   names : TStringList;
   typ : TTypeSymbol;
   lazyParam, varParam, constParam : Boolean;
   posArray : TScriptPosArray;
   sym : TParamSymbol;
   defaultExpr : TTypedExpr;
begin
   if FTok.TestDelete(ttBLEFT) then begin
      if not FTok.TestDelete(ttBRIGHT) then begin
         // At least one argument was found
         names := TStringList.Create;
         try
            repeat
               lazyParam := FTok.TestDelete(ttLAZY);
               varParam := FTok.TestDelete(ttVAR);
               if not varParam then
                  constParam := FTok.TestDelete(ttCONST)
               else constParam := False;

               if lazyParam and (varParam or constParam) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantBeVarOrConst);

               // Conditionally pass in dynamic array
               if ParamsToDictionary and (coSymbolDictionary in FOptions) then
                  ReadNameList(names, posArray)     // use overloaded version
               else ReadNameList(names);

               if not FTok.TestDelete(ttCOLON) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)
               else begin
                  defaultExpr := nil;
                  typ := ReadType('', tcParameter);
                  try
                     if (not constParam) and (typ is TOpenArraySymbol) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_OpenArrayParamMustBeConst);

                     if FTok.TestDelete(ttEQ) then begin
                        if lazyParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantHaveDefaultValue);
                        if varParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_VarParamCantHaveDefaultValue);
                        if constParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstParamCantHaveDefaultValue);

                        defaultExpr:=ReadExpr;

                        if not (defaultExpr is TConstExpr) then begin
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
                           FreeAndNil(defaultExpr);
                        end;

                        if defaultExpr=nil then
                           Typ.IsCompatible(defaultExpr.Typ);
                        if not Typ.IsCompatible(defaultExpr.Typ) then begin
                           FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleTypes,
                                                     [Typ.Caption, defaultExpr.Typ.Caption]);
                           FreeAndNil(defaultExpr);
                        end;
                     end;

                     for i:=0 to names.Count-1 do begin
                        if lazyParam then begin
                           sym := TLazyParamSymbol.Create(names[i], Typ)
                        end else if varParam then begin
                           sym := TVarParamSymbol.Create(names[i], Typ)
                        end else if constParam then begin
                           sym := TConstParamSymbol.Create(names[i], Typ)
                        end else begin
                           if Assigned(defaultExpr) then begin
                              sym := TParamSymbolWithDefaultValue.Create(names[i], Typ);
                              TParamSymbolWithDefaultValue(sym).SetDefaultValue(TConstExpr(defaultExpr).Data[FExec],
                                                                                TConstExpr(defaultExpr).Addr[FExec]);
                           end else begin
                              sym := TParamSymbol.Create(names[i], Typ);
                           end;
                        end;

                        addParamMeth(sym);

                        // Enter Field symbol in dictionary
                        if ParamsToDictionary and (coSymbolDictionary in FOptions) then begin
                           FSymbolDictionary.AddValueSymbol(sym, posArray[i], [suDeclaration]);  // add variable symbol
                           FSymbolDictionary.AddTypeSymbol(typ, FTok.HotPos);  // add type symbol
                        end;
                     end;
                  finally
                     FreeAndNil(defaultExpr);
                  end;
               end;
            until not FTok.TestDelete(ttSEMI);

         finally
            names.Free;
         end;

         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;
   end;
end;

// ReadSwitch
//
function TdwsCompiler.ReadSwitch(const SwitchName: string) : Boolean;
var
   sw : TSwitchInstruction;
begin
   sw:=StringToSwitchInstruction(SwitchName);
   if sw<>siNone then
      Exit(True);

   Result := False;

   FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_CompilerSwitchUnknown, [SwitchName]);

   while not FTok.TestDelete(ttCRIGHT) do
      FTok.KillToken;
end;

// ReadInstrSwitch
//
function TdwsCompiler.ReadInstrSwitch(semiPending : Boolean): TNoResultExpr;
var
   switch : TSwitchInstruction;
   name, scriptSource : String;
   i : Integer;
   conditionalTrue : Boolean;
   switchPos, condPos : TScriptPos;
   condExpr : TTypedExpr;
   sourceFile : TSourceFile;
begin
   Result := nil;

   switchPos:=FTok.HotPos;

   switch:=StringToSwitchInstruction(FTok.GetToken.FString);
   FTok.KillToken;

   case switch of
      siIncludeLong, siIncludeShort, siIncludeOnce, siFilterLong, siFilterShort : begin

         if semiPending then
            FMsgs.AddCompilerStop(switchPos, CPE_SemiExpected);

         if not FTok.Test(ttStrVal) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_IncludeFileExpected);
         name := FTok.GetToken.FString;
         FTok.KillToken;

         if (switch=siIncludeOnce) then begin
            sourceFile:=FMainProg.GetSourceFile(name);
            if (sourceFile<>nil) and (sourceFile.Name<>name) then
               FMsgs.AddCompilerWarningFmt(switchPos, CPW_IncludeOnceWithDifferentCase,
                                           [name, sourceFile.Name]);
         end else sourceFile:=nil;
         if sourceFile=nil then begin
            try
               scriptSource := GetIncludeScriptSource(name);

               if switch in [siFilterLong, siFilterShort] then begin
                  if Assigned(FFilter) then begin
                     // Include file is processed by the filter
                     scriptSource:=FFilter.Process(scriptSource, FMsgs);
                  end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoFilterAvailable);
               end else begin
                  // Include file is included as-is
               end;

               Result:=SwitchTokenizerToInclude(name, scriptSource);
            except
               on e: ECompileError do
                  raise;
               on e: Exception do
                  FMsgs.AddCompilerStop(FTok.HotPos, e.Message);
            end;
         end;

      end;
      siDefine : begin

         if not FTok.Test(ttNAME) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         FMainProg.ConditionalDefines.Add(FTok.GetToken.FString);
         FTok.KillToken;

      end;
      siUndef : begin

         if not FTok.Test(ttNAME) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         i:=FMainProg.ConditionalDefines.IndexOf(FTok.GetToken.FString);
         if i>=0 then
            FMainProg.ConditionalDefines.Delete(i);
         FTok.KillToken;

      end;
      siIfDef, siIfNDef, siIf : begin

         case switch of
            siIfDef, siIfNDef : begin
               if not FTok.Test(ttNAME) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
               conditionalTrue:=    (FMainProg.ConditionalDefines.IndexOf(FTok.GetToken.FString)>=0)
                                xor (switch = siIfNDef);
               FTok.KillToken;
            end;
            siIf : begin
               condPos:=Ftok.HotPos;
               FIsSwitch:=True;
               try
                  condExpr:=ReadExpr;
                  try
                     if not condExpr.IsConstant then
                        FMsgs.AddCompilerStop(condPos, CPE_ConstantExpressionExpected);
                     if not condExpr.IsOfType(FProg.TypBoolean) then
                        FMsgs.AddCompilerStop(condPos, CPE_BooleanExpected);

                     conditionalTrue:=condExpr.EvalAsBoolean(FExec);
                  finally
                     condExpr.Free;
                  end;
               finally
                  FIsSwitch:=False;
               end;
            end
         else
            conditionalTrue:=False;
            Assert(False);
         end;

         if conditionalTrue then
            FConditionalDepth.Push(switch)
         else begin
            if ReadUntilEndOrElseSwitch(True) then
               FConditionalDepth.Push(siElse);
            if not FTok.HasTokens then
               FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);
         end;

      end;
      siElse : begin

         if FConditionalDepth.Count=0 then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);
         if FConditionalDepth.Peek=siElse then
            FMsgs.AddCompilerStop(switchPos, CPE_UnfinishedConditionalDirective);

         FConditionalDepth.Pop;
         ReadUntilEndOrElseSwitch(False);
         if not FTok.HasTokens then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);

      end;
      siEndIf : begin

         if FConditionalDepth.Count=0 then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective)
         else FConditionalDepth.Pop;

      end;
      siHint, siWarning, siError, siFatal : begin

         if not FTok.Test(ttStrVal) then
            if switch<>siFatal then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected)
            else FMsgs.AddCompilerStop(FTok.HotPos, CPE_StringExpected)
         else begin
            case switch of
               siHint    : FMsgs.AddCompilerHint(switchPos, FTok.GetToken.FString);
               siWarning : FMsgs.AddCompilerWarning(switchPos, FTok.GetToken.FString);
               siError   : FMsgs.AddCompilerError(switchPos, FTok.GetToken.FString, TCompilerErrorMessage);
               siFatal   : FMsgs.AddCompilerStop(switchPos, FTok.GetToken.FString, TCompilerErrorMessage);
            end;
            FTok.KillToken;
         end;

      end;
      siHints, siWarnings : begin
         if not FTok.TestDeleteNamePos(name, condPos) then
            name:='';
         conditionalTrue:=SameText(name, 'ON');
         if conditionalTrue or SameText(name, 'OFF') then begin
            if switch=siHints then
               FMsgs.HintsDisabled:=not conditionalTrue
            else FMsgs.WarningsDisabled:=not conditionalTrue;
         end else FMsgs.AddCompilerError(FTok.HotPos, CPE_OnOffExpected);
      end;
   else
      FMsgs.AddCompilerStopFmt(switchPos, CPE_CompilerSwitchUnknown, [Name]);
   end;

   if not FTok.Test(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

   // Simulate a semicolon
   FTok.GetToken.FTyp := ttSEMI
end;

// ReadExprSwitch
//
function TdwsCompiler.ReadExprSwitch : TTypedExpr;
var
   switch : TSwitchInstruction;
   name, value : String;
   hotPos : TScriptPos;
   funcSym : TFuncSymbol;
begin
   Result:=nil;

   hotPos:=FTok.HotPos;

   switch:=StringToSwitchInstruction(FTok.GetToken.FString);
   FTok.KillToken;

   case switch of
      siIncludeLong, siIncludeShort : begin

         name:='';
         hotPos:=FTok.HotPos;
         if FTok.TestDelete(ttPERCENT) then begin
            if FTok.TestAny([ttNAME, ttFUNCTION])<>ttNone then begin
               name:=FTok.GetToken.FString;
               FTok.KillToken;
               if not FTok.TestDelete(ttPERCENT) then
                  name:='';
            end;
         end;
         value:='';
         if name='' then
            FMsgs.AddCompilerError(hotPos, CPE_IncludeItemExpected)
         else if SameText(name, 'FILE') then
            value:=hotPos.SourceFile.Name
         else if SameText(name, 'LINE') then
            value:=IntToStr(hotPos.Line)
         else if SameText(name, 'DATE') then
            value:=FormatDateTime('yyyy-mm-dd', Date)
         else if SameText(name, 'TIME') then
            value:=FormatDateTime('hh:nn:ss', Time)
         else if SameText(name, 'FUNCTION') then begin
            if FProg is TdwsProcedure then begin
               funcSym:=TdwsProcedure(FProg).Func;
               if funcSym is TMethodSymbol then
                  value:=TMethodSymbol(funcSym).StructSymbol.Name+'.'+funcSym.Name
               else value:=funcSym.Name;
            end else value:=MSG_MainFunction;
         end else FMsgs.AddCompilerErrorFmt(hotPos, CPE_IncludeItemUnknown, [name]);

         Result:=TConstStringExpr.CreateUnified(FProg, nil, value);
      end;
   else
      FMsgs.AddCompilerStopFmt(hotPos, CPE_CompilerSwitchUnknown, [Name]);
   end;

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);
end;

// ReadUntilEndOrElseSwitch
//
function TdwsCompiler.ReadUntilEndOrElseSwitch(allowElse : Boolean) : Boolean;
var
   startPos : TScriptPos;
   switch : TSwitchInstruction;
   innerDepth : Integer;
begin
   startPos:=FTok.HotPos;

   // flush the switch that triggered the block
   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

   innerDepth:=0;
   Result:=False;

   while FTok.HasTokens do begin

      // kill everything up to next switch
      while FTok.HasTokens and (not FTok.Test(ttSWITCH)) do
         FTok.KillToken;

      if not FTok.HasTokens then begin
         FMsgs.AddCompilerStop(startPos, CPE_UnbalancedConditionalDirective);
         Break;
      end;

      startPos:=FTok.HotPos;
      switch:=StringToSwitchInstruction(FTok.GetToken.FString);
      FTok.KillToken;

      case switch of

         siEndIf : begin

            Dec(innerDepth);
            if innerDepth<0 then Break;

         end;
         siElse : begin

            if innerDepth=0 then begin
               if not allowElse then
                  FMsgs.AddCompilerStop(startPos, CPE_UnfinishedConditionalDirective);
               Result:=True;
               Break;
            end;

         end;
         siIfDef, siIfNDef : begin

            while FTok.HasTokens and not FTok.Test(ttCRIGHT) do
               FTok.KillToken;
            Inc(innerDepth);

         end;

      else
         while FTok.HasTokens and not FTok.Test(ttCRIGHT) do
            FTok.KillToken;
      end;

      if not FTok.TestDelete(ttCRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

   end;
end;

// Checks if a name already exists in the Symboltable

procedure TdwsCompiler.CheckName(const Name: string);
var
   sym: TSymbol;
begin
   sym := FProg.Table.FindLocal(Name);

   if not Assigned(sym) and (FProg is TdwsProcedure) then
      sym := TdwsProcedure(FProg).Func.Params.FindLocal(Name);

   if Assigned(sym) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameAlreadyExists, [Name])
   else CheckSpecialName(Name);
end;

// IdentifySpecialName
//
function TdwsCompiler.IdentifySpecialName(const name : String) : TSpecialKeywordKind;
var
   n : Integer;
begin
   n:=Length(name);
   case n of
      3 : case name[1] of
         'd', 'D' : if SameText(name, 'dec') then Exit(skDec);
         'i', 'I' : if SameText(name, 'inc') then Exit(skInc);
         'l', 'L' : if SameText(name, 'low') then Exit(skLow);
         'o', 'O' : if SameText(name, 'ord') then Exit(skOrd);
         's', 'S' : if SameText(name, 'sqr') then Exit(skSqr);
      end;
      4 : case name[1] of
         'h', 'H' : if SameText(name, 'high') then Exit(skHigh);
         'p', 'P' : if SameText(name, 'pred') then Exit(skPred);
         's', 'S' : if SameText(name, 'succ') then Exit(skSucc);
      end;
      6 : case name[1] of
         'a', 'A' : if SameText(name, 'assert') then Exit(skAssert);
         'l', 'L' : if SameText(name, 'length') then Exit(skLength);
         's', 'S' : if SameText(name, 'sizeof') then Exit(skSizeOf);
      end;
      7 : case name[1] of
         'd', 'D' : if SameText(name, 'defined') then Exit(skDefined);
      end;
      8 : case name[1] of
         'a', 'A' : if SameText(name, 'assigned') then Exit(skAssigned);
         'd', 'D' : if SameText(name, 'declared') then Exit(skDeclared);
      end;
   end;
   Result:=skNone;
end;

// CheckSpecialName
//
procedure TdwsCompiler.CheckSpecialName(const name : String);
begin
   if IdentifySpecialName(name)<>skNone then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameIsReserved, [Name]);
end;

// OpenStreamForFile
//
function TdwsCompiler.OpenStreamForFile(const scriptName : String) : TStream;
var
   i : Integer;
   fname : String;
begin
   for i:=0 to FScriptPaths.Count-1 do begin
      if FScriptPaths[i]<>'' then
         fname:=IncludeTrailingPathDelimiter(FScriptPaths[i])+scriptName
      else fname:=scriptName;
      if FCompileFileSystem.FileExists(fname) then
         Exit(FCompileFileSystem.OpenFileStream(fname, fomReadOnly);
   end;
   Result:=nil;
end;

// GetScriptSource
//
function TdwsCompiler.GetScriptSource(const scriptName : String) : String;
var
   stream : TStream;
   sl : TStringList;
begin
   stream:=OpenStreamForFile(scriptName);
   sl:=TStringList.Create;
   try
      if stream=nil then
         Result:=''
      else begin
         sl.LoadFromStream(stream);
         if sl.Count>0 then
            Result:=sl.Text
         else Result:=' ';
      end;
   finally
      sl.Free;
      stream.Free;
   end;
end;

// GetIncludeScriptSource
//
function TdwsCompiler.GetIncludeScriptSource(const scriptName : String) : String;
begin
   Result:='';

   if Assigned(FOnInclude) then begin
      FOnInclude(ScriptName, Result);
      if Result<>'' then Exit;
   end;

   Result:=GetScriptSource(scriptName);
   if Result='' then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_IncludeFileNotFound,
                               [scriptName], TCompilerErrorMessage)
end;

function TdwsCompiler.GetVarExpr(dataSym: TDataSymbol): TVarExpr;
begin
   if FProg.Level = dataSym.Level then begin
      Result:=TVarExpr.CreateTyped(FProg, dataSym.Typ, dataSym);
   end else begin
      Result:=TVarParentExpr.Create(FProg, dataSym.Typ, dataSym)
   end;
end;

// GetLazyParamExpr
//
function TdwsCompiler.GetLazyParamExpr(dataSym: TLazyParamSymbol): TLazyParamExpr;
begin
   Result:=TLazyParamExpr.Create(FProg, dataSym.Typ, dataSym.Level, dataSym.StackAddr);
end;

// GetVarParamExpr
//
function TdwsCompiler.GetVarParamExpr(dataSym: TVarParamSymbol): TVarParamExpr;
begin
  if FProg.Level=dataSym.Level then
      Result:=TVarParamExpr.Create(FProg, dataSym.Typ, dataSym)
  else Result:=TVarParamParentExpr.Create(FProg, dataSym.Typ, dataSym)
end;

// GetConstParamExpr
//
function TdwsCompiler.GetConstParamExpr(dataSym: TConstParamSymbol): TVarParamExpr;
begin
   if FProg.Level = dataSym.Level then
      Result := TConstParamExpr.Create(FProg, dataSym.Typ, dataSym)
   else Result := TConstParamParentExpr.Create(FProg, dataSym.Typ, dataSym);
end;

function TdwsCompiler.CheckParams(A, B: TSymbolTable; CheckNames: Boolean): Boolean;
var
  x: Integer;
  r: Boolean;
begin
  Result := True;
  for x := 0 to A.Count - 1 do begin
    r := False;
    if CheckNames and not UnicodeSameText(A[x].Name, B[x].Name) then
        FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterName, [x, A[x].Name])
    else if not A[x].Typ.IsCompatible(B[x].Typ) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterType,
                                [x, A[x].Typ.Caption, B[x].Typ.Caption])
    else if (A[x] is TVarParamSymbol) and not (B[x] is TVarParamSymbol) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_VarParameterExpected, [x, A[x].Name])
    else if not (A[x] is TVarParamSymbol) and (B[x] is TVarParamSymbol) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ValueParameterExpected, [x, A[x].Name])
    else if (A[x] is TConstParamSymbol) and not (B[x] is TConstParamSymbol) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConstParameterExpected, [x, A[x].Name])
    else if not (A[x] is TConstParamSymbol) and (B[x] is TConstParamSymbol) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ValueParameterExpected, [x, A[x].Name])
    else r := True;
    Result := Result and r;
  end;
end;

// CompareFuncKinds
//
procedure TdwsCompiler.CompareFuncKinds(a, b : TFuncKind);
begin
   if a<>b then begin
      case a of
         fkFunction : FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionExpected);
         fkProcedure : FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureExpected);
         fkConstructor : FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstructorExpected);
         fkDestructor : FMsgs.AddCompilerStop(FTok.HotPos, CPE_DestructorExpected);
         fkMethod : FMsgs.AddCompilerStop(FTok.HotPos, CPE_MethodExpected);
      else
         Assert(False);
      end;
   end;
end;

// CompareFuncSymbolParams
//
procedure TdwsCompiler.CompareFuncSymbolParams(a, b : TFuncSymbol);
begin
   if Assigned(a.Typ) and not a.Typ.IsCompatible(b.Typ) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadResultType, [a.Typ.Caption]);

   if a.Params.Count<>b.Params.Count then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadNumberOfParameters,
                                [a.Params.Count, b.Params.Count])
   else CheckParams(a.Params, b.Params, True);
end;

// CurrentClass
//
function TdwsCompiler.CurrentClass : TClassSymbol;
begin
   if (FProg is TdwsProcedure) and (TdwsProcedure(FProg).Func is TMethodSymbol) then
      Result:=TMethodSymbol(TdwsProcedure(FProg).Func).StructSymbol as TClassSymbol
   else Result:=nil;
end;

// HintUnusedSymbols
//
procedure TdwsCompiler.HintUnusedSymbols;
var
   i : Integer;
   sym : TSymbol;
   symDecl : TSymbolPosition;
   symDic : TSymbolDictionary;
   symPosList : TSymbolPositionList;
begin
   if not (coSymbolDictionary in FOptions) then Exit;

   symDic:=FMainProg.SymbolDictionary;
   for i:=0 to FProg.Table.Count-1 do begin
      sym:=FProg.Table[i];
      if sym.ClassType=TDataSymbol then begin
         symPosList:=symDic.FindSymbolPosList(sym);
         symDecl:=symPosList.FindUsage(suDeclaration);
         if symDecl<>nil then begin
            if symPosList.FindUsage(suReference)=nil then
               FMsgs.AddCompilerHintFmt(symDecl.ScriptPos, CPH_VariableDeclaredButNotUsed, [sym.Name])
            else if symPosList.FindUsage(suWrite)=nil then
               FMsgs.AddCompilerHintFmt(symDecl.ScriptPos, CPH_VariableDeclaredButNotWrittenTo, [sym.Name]);
         end;
      end;
   end;
end;

// HintUnusedResult
//
procedure TdwsCompiler.HintUnusedResult(resultSymbol : TDataSymbol);
begin
   if resultSymbol=nil then Exit;
   if not (coSymbolDictionary in FOptions) then Exit;

   if FMainProg.SymbolDictionary.FindSymbolUsage(resultSymbol, suReference)=nil then
      FMsgs.AddCompilerHint(FTok.HotPos, CPH_ResultNotUsed);
end;

function TdwsCompiler.ReadConnectorSym(const Name: string;
  BaseExpr: TTypedExpr; const ConnectorType: IConnectorType; IsWrite: Boolean): TProgramExpr;

   function TryConnectorCall : TConnectorCallExpr;
   var
      argPosArray : TScriptPosArray;
   begin
      // Try to read the call of a connector function
      Result:=TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr, IsWrite);

      ReadArguments(Result.AddArg, ttBLEFT, ttBRIGHT, argPosArray);

      if not Result.AssignConnectorSym(FProg, connectorType) then begin
         Result.BaseExpr:=nil; // freed by caller
         FreeAndNil(Result);
      end;
  end;

begin
  if FTok.Test(ttALEFT) then begin
    Result := ReadConnectorArray(Name,BaseExpr,ConnectorType,IsWrite);
  end
  else if FTok.Test(ttBLEFT) then
  begin
    // Brackets -> always a function
    Result := TryConnectorCall;
    if Result=nil then
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypVariant, Null); // keep compiling
  end
  else if not IsWrite then
  begin
    // The assignment ":=" was already read.
    Result := TConnectorReadExpr.Create(FProg, FTok.HotPos, Name, BaseExpr);

    if not TConnectorReadExpr(Result).AssignConnectorSym(ConnectorType) then
    begin
      Result.Free;
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                               [Name, ConnectorType.ConnectorCaption]);
    end;
  end
  else if FTok.TestDelete(ttASSIGN) then
  begin
    // A assignment of the form "connector.member := expr" was found
    // and is transformed into "connector.member(expr)"
    Result := TConnectorWriteExpr.Create(FProg, FTok.HotPos,  Name, BaseExpr, ReadExpr);

    if not TConnectorWriteExpr(Result).AssignConnectorSym(FProg, ConnectorType) then
    begin
      Result.Free;
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                               [Name, ConnectorType.ConnectorCaption]);
    end;
  end
  else
  begin
    // It's possible that we should read a connector member or
    // call a connector function without arguments.
    Result := TConnectorReadExpr.Create(FProg, FTok.HotPos, Name, BaseExpr);

    if not TConnectorReadExpr(Result).AssignConnectorSym(ConnectorType) then
    begin
      // Don't destroy BaseExpr!
      TConnectorReadExpr(Result).BaseExpr := nil;
      Result.Free;

      // Try to read a connector call
      Result := TryConnectorCall;
    end;

    if not Assigned(Result) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                               [Name, ConnectorType.ConnectorCaption]);
  end;
end;

// ReadConnectorArray
//
function TdwsCompiler.ReadConnectorArray(const Name: String; BaseExpr: TTypedExpr;
            const ConnectorType: IConnectorType; IsWrite: Boolean): TConnectorCallExpr;
var
   argPosArray : TScriptPosArray;
begin
   Result:=TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr, IsWrite, True);
   try
      ReadArguments(Result.AddArg, ttALEFT, ttARIGHT, argPosArray);

      if IsWrite and FTok.TestDelete(ttASSIGN) then
         Result.AddArg(ReadExpr);

      if not Result.AssignConnectorSym(FProg, connectorType) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorIndex, [ConnectorType.ConnectorCaption]);
   except
      Result.BaseExpr:=nil;
      Result.Free;
      raise;
  end;
end;

function TdwsCompiler.ReadStringArray(expr : TDataExpr; IsWrite: Boolean): TProgramExpr;
var
   indexExpr, valueExpr: TTypedExpr;
   pos: TScriptPos;
   n : Integer;
begin
   pos := FTok.HotPos;
   indexExpr := ReadExpr;
   try
      if not (indexExpr.IsOfType(FProg.TypInteger) or indexExpr.IsOfType(FProg.TypVariant)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);

      if FTok.TestDelete(ttASSIGN) and IsWrite then begin
         valueExpr:=ReadExpr;
         if valueExpr is TConstStringExpr then begin
            n:=Length(TConstStringExpr(valueExpr).Value);
            if n<>1 then
               FMsgs.AddCompilerErrorFmt(pos, RTE_InvalidInputDataSize, [n, 1]);
         end;
         if Expr is TStrVarExpr then
            Result:=TVarStringArraySetExpr.Create(FProg, pos, expr, indexExpr, valueExpr)
         else Result := TStringArraySetExpr.Create(FProg, pos, expr, indexExpr, valueExpr);
      end else Result := TStringArrayOpExpr.CreatePos(FProg, pos, expr, indexExpr);
   except
      indexExpr.Free;
      raise;
   end;
end;

// CreateProgram
//
function TdwsCompiler.CreateProgram(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                                    const stackParams : TStackParameters) : TdwsMainProgram;
begin
   Result:=TdwsMainProgram.Create(SystemTable, ResultType, stackParams);
end;

// ReadEnumeration
//
function TdwsCompiler.ReadEnumeration(const typeName : String) : TEnumerationSymbol;
var
   name : String;
   elemSym : TElementSymbol;
   constExpr : TTypedExpr;
   enumInt : Integer;
   namePos : TScriptPos;
   isUserDef : Boolean;
begin
   Result := TEnumerationSymbol.Create(TypeName, FProg.TypInteger);
   try
      enumInt := 0;

      repeat
         // Read a member of the enumeration
         if not FTok.TestDeleteNamePos(name, namePos) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         // Member has a user defined value
         if FTok.TestDelete(ttEQ) then begin
            constExpr := ReadExpr;

            if not (constExpr is TConstExpr) then begin
               FreeAndNil(constExpr);
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
            end else if not(constExpr.Typ = FProg.TypInteger) then begin
               FreeAndNil(constExpr);
               FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);
            end;

            if Assigned(constExpr) then begin
               enumInt := constExpr.EvalAsInteger(FExec);
               constExpr.Free;
            end;

            isUserDef := True;
         end else isUserDef := False;

         // Create member symbol
         elemSym := TElementSymbol.Create(name, Result, enumInt, isUserDef);

         Inc(enumInt);

         // Add member symbol to table and enumeration type
         FProg.Table.AddSymbol(elemSym);
         Result.AddElement(elemSym);

         // Add member symbol to Symbol Dictionary
         if coSymbolDictionary in FOptions then
            FSymbolDictionary.AddConstSymbol(elemSym, namePos, [suDeclaration]);

      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

   except
      Result.Free;
      raise;
   end;
end;

// ReadUses
//
procedure TdwsCompiler.ReadUses;
var
   names : TStringList;
   x, y, z, u : Integer;
   rt : TSymbolTable;
   rSym : TSymbol;
   unitSymbol : TUnitSymbol;
begin
   names:=TStringList.Create;
   try
      ReadNameList(names);
      u:=0;
      if FUnitSymbol<>nil then
         if UnitSection=secImplementation then begin
            rt:=FUnitSymbol.ImplementationTable;
            u:=1;
         end else rt:=FUnitSymbol.InterfaceTable
      else rt:=FProg.Root.RootTable;
      for x:=0 to names.Count-1 do begin
         y:=0;
         z:=-1;
         while (y<rt.Count) do begin
            rSym:=rt[y];
            if (rSym.ClassType=TUnitSymbol) and UnicodeSameText(rSym.Name,names[x]) then begin
               z:=rt.IndexOfParent(TUnitSymbol(rSym).Table);
               if z>=u then begin // uses A,B,A,C => uses A,B,C
                  rt.MoveParent(z,u);
//                  Inc(u);
               end;
               Break;
            end;
            Inc(y);
         end;
         if z<0 then begin
            unitSymbol:=HandleExplicitDependency(names[x]);
            if unitSymbol<>nil then begin
               z:=rt.IndexOfParent(unitSymbol.Table);
               if z>u then
                  rt.MoveParent(z, u);
            end;
         end;
      end;
   finally
      names.Free;
   end;
end;

// ReadUnitHeader
//
procedure TdwsCompiler.ReadUnitHeader;
var
   name : String;
   namePos : TScriptPos;
begin
   if not FTok.TestDelete(ttUNIT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_UnitExpected);
   if not FTok.TestDeleteNamePos(name, namePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
   if not SameText(name, namePos.SourceFile.Name) then
      FMsgs.AddCompilerError(namePos, CPE_UnitNameDoesntMatch);
   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
   if FTok.TestDelete(ttINTERFACE) then
      FUnitSection:=secInterface
   else FUnitSection:=secMixed;
   DoSectionChanged;
end;

// CreateProcedure
//
function TdwsCompiler.CreateProcedure(Parent : TdwsProgram): TdwsProcedure;
begin
   Result := TdwsProcedure.Create(Parent);
end;

// CreateAssign
//
function TdwsCompiler.CreateAssign(const pos : TScriptPos; token : TTokenType;
                                   left : TDataExpr; right : TTypedExpr) : TNoResultExpr;
var
   classOpSymbol : TClassOperatorSymbol;
   classOpExpr : TFuncExpr;
   assignOpExpr : TAssignExpr;
   classSymbol : TClassSymbol;
   intfSymbol : TInterfaceSymbol;
begin
   if Assigned(right.Typ) then begin

      case token of
         ttASSIGN : begin
            if left.Typ=nil then begin
               // error assumed to have already been reported
               left.Free;
               right.Free;
               Result:=TNullExpr.Create(FProg, Pos);
            end else if left.Typ.ClassType=TClassOfSymbol then begin
               Result:=TAssignClassOfExpr.Create(FProg, pos, left, right);
            end else if left.Typ.ClassType=TInterfaceSymbol then begin
               if right.Typ is TClassSymbol then begin
                  classSymbol:=TClassSymbol(right.Typ);
                  intfSymbol:=TInterfaceSymbol(left.Typ);
                  if not classSymbol.ImplementsInterface(intfSymbol) then
                     FMsgs.AddCompilerErrorFmt(pos, RTE_ObjCastToIntfFailed,
                                               [classSymbol.Name, intfSymbol.Name]);
                  Result:=TAssignExpr.Create(FProg, pos, left, TObjAsIntfExpr.Create(FProg, pos, right, intfSymbol));
               end else Result:=TAssignExpr.Create(FProg, pos, left, right);
            end else if right.InheritsFrom(TDataExpr) and ((right.Typ.Size<>1) or (right.Typ is TArraySymbol)) then begin
               if right.InheritsFrom(TFuncExpr) then
                  TFuncExpr(right).SetResultAddr(FProg, nil);
               if right.InheritsFrom(TArrayConstantExpr) then
                  Result:=TAssignArrayConstantExpr.Create(FProg, pos, left, TArrayConstantExpr(right))
               else Result:=TAssignDataExpr.Create(FProg, pos, left, right)
            end else if left.Typ is TFuncSymbol then begin
               if (right.Typ is TFuncSymbol) or (right.Typ is TNilSymbol) then begin
                  if right is TFuncRefExpr then begin
                     right:=TFuncRefExpr(right).Extract;
                     if right is TFuncPtrExpr then begin
                        right:=TFuncPtrExpr(right).Extract;
                        Result:=TAssignExpr.Create(FProg, pos, left, right);
                     end else begin
                        Assert(right is TFuncExprBase);
                        Result:=TAssignFuncExpr.Create(FProg, pos, left, right);
                     end;
                  end else begin
                     Result:=TAssignExpr.Create(FProg, pos, left, right);
                  end;
               end else begin
                  FMsgs.AddCompilerError(pos, CPE_IncompatibleOperands);
                  Result:=TAssignExpr.Create(FProg, pos, left, right); // keep going
               end;
            end else begin
               Result:=TAssignExpr.Create(FProg, pos, left, right);
            end;
         end;
         ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN, ttCARET_ASSIGN : begin
            if left.Typ is TClassSymbol then begin

               classOpSymbol:=(left.Typ as TClassSymbol).FindClassOperator(token, right.Typ);
               if classOpSymbol=nil then
                  FMsgs.AddCompilerStop(pos, CPE_IncompatibleOperands);
               classOpExpr:=GetMethodExpr(classOpSymbol.UsesSym, left, rkObjRef, pos, False);
               try
                  classOpExpr.AddArg(right);
                  classOpExpr.TypeCheckArgs(FProg);
               except
                  classOpExpr.Free;
                  raise;
               end;
               Result:=TNoResultWrapperExpr.Create(FProg, pos, classOpExpr);

            end else begin

               assignOpExpr:=FOperators.GenerateAssign(Fprog, pos, token, left, right);
               if assignOpExpr=nil then begin
                  FMsgs.AddCompilerError(pos, CPE_IncompatibleOperands);
                  Result:=TAssignExpr.Create(FProg, pos, left, right);
               end else Result:=assignOpExpr;

            end;
         end;
      else
         Result:=nil;
         Assert(False);
      end;

      if Optimize then
         Result:=Result.OptimizeToNoResultExpr(FProg, FExec);

   end else begin

      left.Free;
      right.Free;
      FMsgs.AddCompilerError(Pos, CPE_RightSideNeedsReturnType);
      Result:=TNullExpr.Create(FProg, Pos);

   end;
end;

// CreateArrayLow
//
function TdwsCompiler.CreateArrayLow(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
begin
   if typ is TStaticArraySymbol then
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TStaticArraySymbol(typ).LowBound)
   else if typ is TDynamicArraySymbol then
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, 0)
   else Result:=nil;
   baseExpr.Free;
end;

// CreateArrayHigh
//
function TdwsCompiler.CreateArrayHigh(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
begin
   if typ is TOpenArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TOpenArrayLengthExpr.Create(FProg, TDataExpr(baseExpr));
      TOpenArrayLengthExpr(Result).Delta:=-1;
   end else if typ is TDynamicArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TArrayLengthExpr.Create(FProg, baseExpr);
      TArrayLengthExpr(Result).Delta:=-1;
   end else if typ is TStaticArraySymbol then begin
      baseExpr.Free;
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TStaticArraySymbol(typ).HighBound);
   end else Result:=nil;
end;

// CreateArrayLength
//
function TdwsCompiler.CreateArrayLength(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
begin
   if typ is TOpenArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TOpenArrayLengthExpr.Create(FProg, TDataExpr(baseExpr));
   end else if typ is TDynamicArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TArrayLengthExpr.Create(FProg, baseExpr);
   end else if typ is TStaticArraySymbol then begin
      baseExpr.Free;
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TStaticArraySymbol(typ).ElementCount);
   end else Result:=nil;
end;

// CreateOperatorFunction
//
function TdwsCompiler.CreateOperatorFunction(funcSym : TFuncSymbol; left, right : TTypedExpr) : TTypedExpr;
var
   funcExpr : TFuncExprBase;
begin
   funcExpr:=GetFuncExpr(funcSym, False);
   funcExpr.AddArg(left);
   funcExpr.AddArg(right);
   funcExpr.TypeCheckArgs(FProg);
   Result:=funcExpr;

//   if Result.InheritsFrom(TFuncExpr) then
//      TFuncExpr(right).SetResultAddr(FProg, nil);

   if Optimize then
      Result:=Result.OptimizeToTypedExpr(FProg, FExec);
end;

// DoSectionChanged
//
procedure TdwsCompiler.DoSectionChanged;
begin
   if Assigned(FOnSectionChanged) then
      FOnSectionChanged(Self);
end;

// SwitchTokenizerToInclude
//
function TdwsCompiler.SwitchTokenizerToInclude(const sourceName, sourceCode : String) : TNoResultExpr;
var
   sourceFile : TSourceFile;
begin
   sourceFile:=FMainProg.RegisterSourceFile(sourceName, sourceCode);
   Result:=ReadScript(sourceFile, stInclude);
end;

// SwitchTokenizerToUnit
//
procedure TdwsCompiler.SwitchTokenizerToUnit(srcUnit : TSourceUnit; const sourceCode : String);
var
   sourceFile : TSourceFile;
   oldUnit : TUnitSymbol;
   oldTable : TSymbolTable;
begin
   sourceFile:=FMainProg.RegisterSourceFile(srcUnit.GetUnitName, sourceCode);

   oldUnit:=FUnitSymbol;
   FUnitSymbol:=srcUnit.Symbol;
   oldTable:=FProg.Table;
   FProg.Table:=FUnitSymbol.Table;
   FUnitsFromStack.Push(sourceFile.Name);
   try
      ReadScript(sourceFile, stUnit);
   finally
      FUnitsFromStack.Pop;
      FUnitSymbol:=oldUnit;
      FProg.Table:=oldTable;
   end;
end;

// ReadSpecialFunction
//
function TdwsCompiler.ReadSpecialFunction(const namePos : TScriptPos; specialKind : TSpecialKeywordKind) : TProgramExpr;

   function EvaluateDefined(argExpr : TTypedExpr) : Boolean;
   var
      name : String;
   begin
      argExpr.EvalAsString(FExec, name);
      Result:=(FMainProg.ConditionalDefines.IndexOf(name)>=0);
   end;

   function EvaluateDeclared(argExpr : TTypedExpr) : Boolean;
   var
      name : String;
   begin
      argExpr.EvalAsString(FExec, name);
      Result:=(TDeclaredExpr.FindSymbol(FProg.Root.Table, name)<>nil);
   end;

var
   argExpr, msgExpr, operandExpr : TTypedExpr;
   argTyp : TTypeSymbol;
   argPos : TScriptPos;
begin
   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

   // Test for statements like "Low(Integer)"
   if FTok.Test(ttName) and FTok.NextTest(ttBRIGHT) then
      argTyp:=FProg.Table.FindTypeSymbol(FTok.GetToken.FString, cvMagic)
   else argTyp:=nil;

   if     Assigned(argTyp)
      and argTyp.InheritsFrom(TTypeSymbol)
      and not argTyp.InheritsFrom(TFuncSymbol) then begin
      argExpr:=nil;
      argPos:=cNullPos;
      FTok.KillToken;
      FTok.KillToken;
   end else begin
      argPos:=FTok.HotPos;
      case specialKind of
         skAssigned :
            argExpr:=ReadExpr(FProg.TypNil);
         skInc, skDec :
            argExpr:=ReadTerm(True);
      else
         argExpr:=ReadExpr;
      end;
      argTyp:=argExpr.Typ;
      while argTyp is TAliasSymbol do
         argTyp:=TAliasSymbol(argTyp).BaseType;
   end;

   msgExpr:=nil;
   try
      if not Assigned(argTyp) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);

      Result := nil;

      case specialKind of
         skAssert : begin
            if not argTyp.IsOfType(FProg.TypBoolean) then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_BooleanExpected);
            if FTok.TestDelete(ttCOMMA) then begin
               msgExpr:=ReadExpr;
               if (msgExpr=nil) or (not msgExpr.IsOfType(FProg.TypString)) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
            end;
            if coAssertions in FOptions then
               Result:=TAssertExpr.Create(FProg, namePos, argExpr, msgExpr)
            else begin
               Result:=TNullExpr.Create(FProg, namePos);
               argExpr.Free;
               msgExpr.Free;
            end;
            argExpr:=nil;
            msgExpr:=nil;
         end;
         skAssigned : begin
            if argTyp is TClassSymbol then
               Result:=TAssignedInstanceExpr.Create(FProg, argExpr)
            else if argTyp is TClassOfSymbol then
               Result:=TAssignedMetaClassExpr.Create(FProg, argExpr)
            else if argTyp is TFuncSymbol then
               Result:=TAssignedFuncPtrExpr.Create(FProg, argExpr)
            else FMsgs.AddCompilerError(FTok.HotPos, CPE_InvalidOperands);
            argExpr:=nil;
         end;
         skHigh : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayHigh(argExpr, TArraySymbol(argTyp));
               argExpr:=nil;
            end else if argTyp is TEnumerationSymbol then begin
               FreeAndNil(argExpr);
               Result:=TConstExpr.CreateTyped(FProg, argTyp, TEnumerationSymbol(argTyp).HighBound)
            end else if argTyp is TDynamicArraySymbol and Assigned(argExpr) then begin
               Result:=TArrayLengthExpr.Create(FProg, TDataExpr(argExpr));
               TArrayLengthExpr(Result).Delta:=-1;
               argExpr:=nil;
            end else if argTyp.IsOfType(FProg.TypString) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp=FProg.TypInteger then begin
               FreeAndNil(argExpr);
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, High(Int64));
            end else begin
               FreeAndNil(argExpr);
               FMsgs.AddCompilerError(FTok.HotPos, CPE_InvalidOperands);
            end;
         end;
         skInc, skDec, skSucc, skPred : begin
            if     (specialKind in [skInc, skDec])
               and not (   (argExpr is TDataExpr)
                        and TDataExpr(argExpr).IsWritable) then
               FMsgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [0, 'a'])
            else if    argTyp.IsOfType(FProg.TypInteger)
                    or (argTyp is TEnumerationSymbol) then begin
               if specialKind in [skInc, skDec] then
                  WarnForVarUsage(TVarExpr(argExpr), argPos);
               if FTok.TestDelete(ttCOMMA) then begin
                  operandExpr:=ReadExpr;
                  if (operandExpr=nil) or (not operandExpr.IsOfType(FProg.TypInteger)) then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);
               end else operandExpr:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, 1);
               case specialKind of
                  skInc : Result:=TIncVarFuncExpr.Create(FProg, FTok.HotPos, argExpr, operandExpr);
                  skDec : Result:=TDecVarFuncExpr.Create(FProg, FTok.HotPos, argExpr, operandExpr);
                  skSucc : Result:=TSuccFuncExpr.Create(FProg, FTok.HotPos, argExpr, operandExpr);
                  skPred : Result:=TPredFuncExpr.Create(FProg, FTok.HotPos, argExpr, operandExpr);
               else
                  Assert(False);
               end;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);
         end;
         skLength : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayLength(argExpr, TArraySymbol(argTyp));
               argExpr:=nil;
            end else if ((argTyp=FProg.TypString) or (argTyp=FProg.TypVariant)) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_InvalidOperands);
         end;
         skLow : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayLow(argExpr, TArraySymbol(argTyp));
               argExpr:=nil;
            end else begin
               FreeAndNil(argExpr);
               if argTyp is TEnumerationSymbol then begin
                  Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TEnumerationSymbol(argTyp).LowBound);
               end else if argTyp=FProg.TypString then begin
                  Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, 1);
               end else if (argTyp=FProg.TypInteger) then begin
                  Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, Low(Int64));
               end else begin
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_InvalidOperands);
               end;
            end;
         end;
         skSqr : begin
            if argTyp=FProg.TypInteger then begin
               Result:=TSqrIntExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp=FProg.TypFloat then begin
               Result:=TSqrFloatExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_NumericalExpected);
         end;
         skOrd : begin
            if argTyp.IsOfType(FProg.TypInteger) or argTyp.InheritsFrom(TEnumerationSymbol) then begin
               Result:=TOrdIntExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp=FProg.TypBoolean then begin
               Result:=TOrdBoolExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp=FProg.TypString then begin
               Result:=TOrdStrExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp=FProg.TypVariant then begin
               Result:=TOrdExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_InvalidOperands);
         end;
         skSizeOf : begin
             Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, argTyp.Size);
             FreeAndNil(argExpr);
         end;
         skDefined, skDeclared : begin
            if not argExpr.IsOfType(FProg.TypString) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_StringExpected);
            if FIsSwitch then begin
               if not argExpr.IsConstant then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);
               try
                  case SpecialKind of
                     skDefined :
                        Result:=TConstBooleanExpr.CreateUnified(FProg, FProg.TypBoolean, EvaluateDefined(argExpr));
                     skDeclared :
                        Result:=TConstBooleanExpr.CreateUnified(FProg, FProg.TypBoolean, EvaluateDeclared(argExpr));
                  end;
               finally
                  FreeAndNil(argExpr);
               end;
            end else begin
               case SpecialKind of
                  skDefined :
                     Result:=TDefinedExpr.Create(FProg, argExpr);
                  skDeclared :
                     Result:=TDeclaredExpr.Create(FProg, argExpr);
               end;
               argExpr:=nil;
            end;
         end;
      end;

      if Result=nil then begin
         // fake expression to keep compiling
         FreeAndNil(argExpr);
         case SpecialKind of
            skDefined, skDeclared, skAssigned :
               Result:=TConstBooleanExpr.CreateUnified(FProg, nil, False);
         else
            Result:=TConstIntExpr.CreateUnified(FProg, nil, 0);
         end;
      end else if Optimize then
         Result:=Result.Optimize(FProg, FExec);

      try
         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      except
         Result.Free;
         raise;
      end;

   except
      argExpr.Free;
      msgExpr.Free;
      raise;
   end;
end;

// ReadTypeCast
//
function TdwsCompiler.ReadTypeCast(const namePos : TScriptPos; typeSym : TTypeSymbol) : TTypedExpr;
var
   argExpr : TTypedExpr;
   hotPos : TScriptPos;
begin
   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

   hotPos:=FTok.CurrentPos;
   argExpr:=ReadExpr;

   Result:=nil;
   try
      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

      if typeSym = FProg.TypInteger then begin

         // Cast Integer(...)
         Result := TConvIntegerExpr.Create(FProg, argExpr);
         if not (   argExpr.IsOfType(FProg.TypInteger) or argExpr.IsOfType(FProg.TypFloat)
                 or argExpr.IsOfType(FProg.TypBoolean)
                 or (argExpr.Typ is TEnumerationSymbol) or argExpr.IsOfType(FProg.TypVariant)) then
            FMsgs.AddCompilerError(hotPos, CPE_IntegerCastInvalid);

      end else if typeSym = FProg.TypFloat then begin

         // Cast Float(...)
         Result := TConvFloatExpr.Create(FProg, argExpr);
         if not (   argExpr.IsOfType(FProg.TypInteger) or argExpr.IsOfType(FProg.TypFloat)
                 or argExpr.IsOfType(FProg.TypVariant)) then
            FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);

      end else if typeSym = FProg.TypString then begin

         // Cast String(...)
         Result := TConvStringExpr.Create(FProg, argExpr);
         if not (argExpr.IsOfType(FProg.TypString) or argExpr.IsOfType(FProg.TypVariant)) then
            FMsgs.AddCompilerError(hotPos, CPE_StringExpected);

      end else if typeSym = FProg.TypBoolean then begin

         // Cast Boolean(...)
         Result := TConvBoolExpr.Create(FProg, argExpr);
         if not (   argExpr.IsOfType(FProg.TypInteger) or argExpr.IsOfType(FProg.TypFloat)
                 or argExpr.IsOfType(FProg.TypBoolean)
                 or argExpr.IsOfType(FProg.TypVariant)) then
            FMsgs.AddCompilerError(hotPos, CPE_BooleanOrIntegerExpected);

      end else if typeSym = FProg.TypVariant then

         // Cast Variant(...)
         Result := TConvVariantExpr.Create(FProg, argExpr)

      else FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

  except
    argExpr.Free;
    raise;
  end;
end;

// ------------------
// ------------------ TdwsConfiguration ------------------
// ------------------

// Create
//
constructor TdwsConfiguration.Create(owner : TComponent);
begin
   inherited Create;
   FOwner := Owner;
   FSystemTable := TStaticSymbolTable.Create;
   FConnectors := TStringList.Create;
   FScriptPaths := TStringList.Create;
   FConditionals := TStringList.Create;
   FUnits := TIdwsUnitList.Create;
   InitSystemTable;
   FUnits.Add(dwsInternalUnit);
   FStackChunkSize := C_DefaultStackChunkSize;
   FDefaultResultType := TdwsDefaultResultType.Create(nil);
   FResultType := FDefaultResultType;
   FCompilerOptions := cDefaultCompilerOptions;
   FMaxRecursionDepth := cDefaultMaxRecursionDepth;
end;

destructor TdwsConfiguration.Destroy;
begin
   inherited;
   (FSystemTable as TStaticSymbolTable)._Release;
   FConnectors.Free;
   FScriptPaths.Free;
   FConditionals.Free;
   FUnits.Free;
   FDefaultResultType.Free;
end;

procedure TdwsConfiguration.Assign(Source: TPersistent);
begin
  if Source is TdwsConfiguration then
  begin
    FCompilerOptions := TdwsConfiguration(Source).CompilerOptions;
    FMaxDataSize := TdwsConfiguration(Source).MaxDataSize;
    FScriptPaths.Assign(TdwsConfiguration(Source).ScriptPaths);
    FTimeoutMilliseconds := TdwsConfiguration(Source).TimeoutMilliseconds;
    FCompileFileSystem := TdwsConfiguration(Source).CompileFileSystem;
    FRuntimeFileSystem := TdwsConfiguration(Source).RuntimeFileSystem;
  end
  else
    inherited;
end;

procedure TdwsConfiguration.InitSystemTable;
var
   clsObject, clsException, clsDelphiException, clsAssertionFailed : TClassSymbol;
   clsMeta : TClassOfSymbol;
   meth : TMethodSymbol;
   varSym : TBaseSymbol;
   fldSym : TFieldSymbol;
   propSym : TPropertySymbol;
   typInteger, typString : TBaseSymbol;
begin
   // Create base data types
   SystemTable.AddSymbol(TBaseBooleanSymbol.Create);
   SystemTable.AddSymbol(TBaseFloatSymbol.Create);
   typInteger:=TBaseIntegerSymbol.Create;
   SystemTable.AddSymbol(typInteger);
   typString:=TBaseStringSymbol.Create;
   SystemTable.AddSymbol(typString);

   varSym:=TBaseVariantSymbol.Create;
   SystemTable.AddSymbol(varSym);
   SystemTable.AddSymbol(TConstSymbol.Create('Null', varSym, Null));
   SystemTable.AddSymbol(TConstSymbol.Create('Unassigned', varSym, Unassigned));

   SystemTable.AddSymbol(TOpenArraySymbol.Create('array of const', varSym, typInteger));

   SystemTable.AddSymbol(TInterfaceSymbol.Create(SYS_IINTERFACE, nil));

   // Create "root" class TObject
   clsObject:=TClassSymbol.Create(SYS_TOBJECT, nil);
   // Add constructor Create
   meth:=TMethodSymbol.Create(SYS_TOBJECT_CREATE, fkConstructor, clsObject, cvPublic, False);
   meth.Executable:=ICallable(TEmptyFunc.Create);
   meth.IsDefault:=True;
   clsObject.AddMethod(meth);
   // Add destructor Destroy
   TObjectDestroyMethod.Create(mkDestructor, [maVirtual], SYS_TOBJECT_DESTROY,
                               [], '', clsObject, cvPublic, SystemTable);
   // Add procedure Free
   TObjectFreeMethod.Create(mkProcedure, [], SYS_TOBJECT_FREE,
                            [], '', clsObject, cvPublic, SystemTable);
   // Add ClassName method
   TObjectClassNameMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSNAME,
                                 [], SYS_STRING, clsObject, cvPublic, SystemTable);
   SystemTable.AddSymbol(clsObject);

   // Create "root" metaclass TClass
   clsMeta:=TClassOfSymbol.Create(SYS_TCLASS, clsObject);
   SystemTable.AddSymbol(clsMeta);

   // Add ClassType method
   TObjectClassTypeMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSTYPE,
                                 [], SYS_TCLASS, clsObject, cvPublic, SystemTable);

   // Create class Exception
   clsException := TClassSymbol.Create(SYS_EXCEPTION, nil);
   clsException.InheritFrom(clsObject);
   fldSym:=TFieldSymbol.Create(SYS_EXCEPTION_MESSAGE_FIELD, typString, cvProtected);
   clsException.AddField(fldSym);
   propSym:=TPropertySymbol.Create(SYS_EXCEPTION_MESSAGE, typString, cvPublic);
   propSym.ReadSym:=fldSym;
   propSym.WriteSym:=fldSym;
   clsException.AddProperty(propSym);
   TExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
                                 ['Msg', SYS_STRING], '', clsException, cvPublic, SystemTable);
   TExceptionDestroyMethod.Create(mkDestructor, [maOverride], SYS_TOBJECT_DESTROY,
                                 [], '', clsException, cvPublic, SystemTable);
   TExceptionStackTraceMethod.Create(mkFunction, [], SYS_EXCEPTION_STACKTRACE,
                                 [], SYS_STRING, clsException, cvPublic, SystemTable);
   SystemTable.AddSymbol(clsException);

   // Create class EAssertionFailed
   clsAssertionFailed := TClassSymbol.Create(SYS_EASSERTIONFAILED, nil);
   clsAssertionFailed.InheritFrom(clsException);
   SystemTable.AddSymbol(clsAssertionFailed);

   // Create class EDelphi
   clsDelphiException := TClassSymbol.Create(SYS_EDELPHI, nil);
   clsDelphiException.InheritFrom(clsException);
   fldSym:=TFieldSymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS_FIELD, typString, cvProtected);
   clsDelphiException.AddField(fldSym);
   propSym:=TPropertySymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS, typString, cvPublic);
   propSym.ReadSym:=fldSym;
   propSym.WriteSym:=fldSym;
   clsDelphiException.AddProperty(propSym);
   TDelphiExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
                                       ['Cls', SYS_STRING, 'Msg', SYS_STRING], '',
                                       clsDelphiException, cvPublic, SystemTable);
   SystemTable.AddSymbol(clsDelphiException);

   // ExceptObj function
   TExceptObjFunc.Create(SystemTable, 'ExceptObject', [], SYS_EXCEPTION, False);

   // Runtime parameters
   TParamFunc.Create(SystemTable, 'Param', ['Index', SYS_INTEGER], SYS_VARIANT, False);
   TParamStrFunc.Create(SystemTable, 'ParamStr', ['Index', SYS_INTEGER], SYS_STRING, False);
   TParamCountFunc.Create(SystemTable, 'ParamCount', [], SYS_INTEGER, False);
end;

// SetFilter
//
procedure TdwsConfiguration.SetFilter(const Value: TdwsFilter);
begin
   if Assigned(FFilter) then
      FFilter.RemoveFreeNotification(FOwner);

   FFilter := Value;

   if Assigned(FFilter) then
      FFilter.FreeNotification(FOwner);
end;

// SetResultType
//
procedure TdwsConfiguration.SetResultType(const Value: TdwsResultType);
begin
   if Assigned(FResultType) and (FResultType <> FDefaultResultType) then
      FResultType.RemoveFreeNotification(FOwner);

   FResultType := Value;

   if Assigned(FResultType) then
      FResultType.FreeNotification(FOwner)
   else FResultType := FDefaultResultType;
end;

// SetTimeOut
//
procedure TdwsConfiguration.SetTimeOut(const val : Integer);
begin
   TimeoutMilliseconds:=val*1000;
end;

// SetCompileFileSystem
//
procedure TdwsConfiguration.SetCompileFileSystem(const val : TdwsCustomFileSystem);
begin
   if Assigned(FCompileFileSystem) then
      FOwner.RemoveFreeNotification(FCompileFileSystem);

   FCompileFileSystem:=val;

   if Assigned(FCompileFileSystem) then
      FOwner.FreeNotification(FCompileFileSystem);
end;

// SetRuntimeFileSystem
//
procedure TdwsConfiguration.SetRuntimeFileSystem(const val : TdwsCustomFileSystem);
begin
   if Assigned(FRuntimeFileSystem) then
      FOwner.RemoveFreeNotification(FRuntimeFileSystem);

   FRuntimeFileSystem:=val;

   if Assigned(FRuntimeFileSystem) then
      FOwner.FreeNotification(FRuntimeFileSystem);
end;

// Notification
//
procedure TdwsConfiguration.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) then begin
      if AComponent=Filter then
         Filter:=nil
      else if AComponent=ResultType then
         ResultType:=nil
      else if AComponent=CompileFileSystem then
         CompileFileSystem:=nil
      else if AComponent=RuntimeFileSystem then
         RuntimeFileSystem:=nil;
   end;
end;

// SetScriptPaths
//
procedure TdwsConfiguration.SetScriptPaths(const values : TStrings);
begin
   FScriptPaths.Assign(values);
end;

// SetConditionals
//
procedure TdwsConfiguration.SetConditionals(const val : TStringList);
begin
   FConditionals.Assign(val);
end;

// ------------------
// ------------------ TObjectClassNameMethod ------------------
// ------------------

// Execute
//
procedure TObjectClassNameMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   Info.ResultAsString:=info.ValueAsClassSymbol[SYS_SELF].Name; //.ClassSym.Name;
end;

// ------------------
// ------------------ TObjectClassTypeMethod ------------------
// ------------------

// Execute
//
procedure TObjectClassTypeMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   Info.ResultAsInteger:=Int64(info.ValueAsClassSymbol[SYS_SELF]);
end;

// ------------------
// ------------------ TObjectDestroyMethod ------------------
// ------------------

// Execute
//
procedure TObjectDestroyMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   scriptObj : PIScriptObj;
begin
   scriptObj:=info.Execution.SelfScriptObject;
   scriptObj.Destroyed:=True;
end;

// ------------------
// ------------------ TObjectFreeMethod ------------------
// ------------------

// Execute
//
procedure TObjectFreeMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   scriptObj : PIScriptObj;
begin
   scriptObj:=info.Execution.SelfScriptObject;
   if (scriptObj^<>nil) then
      info.Method['Destroy'].Call;
end;

// ------------------
// ------------------ TExceptionCreateMethod ------------------
// ------------------

// Execute
//
procedure TExceptionCreateMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   context : TdwsExceptionContext;
begin
   Info.ValueAsString[SYS_EXCEPTION_MESSAGE_FIELD]:=Info.ValueAsString['Msg'];

   context:=TdwsExceptionContext.Create(info.Execution.GetCallStack);
   ExternalObject:=context;
end;

// ------------------
// ------------------ TExceptionDestroyMethod ------------------
// ------------------

// Execute
//
procedure TExceptionDestroyMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   FreeAndNil(ExternalObject);
end;

// ------------------
// ------------------ TExceptionStackTraceMethod ------------------
// ------------------

// Execute
//
procedure TExceptionStackTraceMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   context : TdwsExceptionContext;
begin
   context:=ExternalObject as TdwsExceptionContext;
   Info.ResultAsString:=info.Execution.CallStackToString(context.CallStack);
end;

// ------------------
// ------------------ TDelphiExceptionCreateMethod ------------------
// ------------------

// Execute
//
procedure TDelphiExceptionCreateMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   inherited;
//   Info.ValueAsString[SYS_EXCEPTION_MESSAGE_FIELD]:=Info.ValueAsString['Msg'];
   Info.ValueAsVariant[SYS_EDELPHI_EXCEPTIONCLASS_FIELD]:=Info.ValueAsVariant['Cls']
end;

// ------------------
// ------------------ TExceptObjFunc ------------------
// ------------------

// Execute
//
procedure TExceptObjFunc.Execute(info : TProgramInfo);
begin
   if info.Execution.ExceptionObjectStack.Count>0 then
      info.ResultAsVariant:=info.Execution.ExceptionObjectStack.Peek
   else info.ResultAsVariant:=IScriptObj(nil);
end;

{ TParamFunc }

procedure TParamFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsVariant := Info.Execution.Parameters[Info.ValueAsInteger['Index']];
end;

{ TParamStrFunc }

procedure TParamStrFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsString := Info.Execution.Parameters[Info.ValueAsInteger['Index']];
end;

{ TParamCount }

procedure TParamCountFunc.Execute(info : TProgramInfo);
begin
  Info.ResultAsInteger := Length(Info.Execution.Parameters);
end;

{ TdwsFilter }

constructor TdwsFilter.Create(AOwner: TComponent);
begin
  inherited;
  FDependencies := TStringList.Create;
  FPrivateDependencies := TStringList.Create;
end;

destructor TdwsFilter.Destroy;
begin
  inherited;
  FDependencies.Free;
  FPrivateDependencies.Free;
end;

function TdwsFilter.GetDependencies: TStrings;
begin
  FDependencies.Clear;
  FDependencies.AddStrings(FPrivateDependencies);

  // Merge dependencies with subfilter dependencies
  if Assigned(FSubFilter) then
    FDependencies.AddStrings(FSubFilter.Dependencies);

  Result := FDependencies;
end;

procedure TdwsFilter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSubFilter) then
    SetSubFilter(nil);
end;

function TdwsFilter.Process(const Text: string; Msgs: TdwsMessageList): string;
begin
  if Assigned(FSubFilter) then
    Result := FSubFilter.Process(Text, Msgs)
  else
    Result := Text;
end;

procedure TdwsFilter.SetSubFilter(const Filter: TdwsFilter);
begin
  if Assigned(FSubFilter) then
    FSubFilter.RemoveFreeNotification(Self);

  FSubFilter := Filter;

  if Assigned(FSubFilter) then
    FSubFilter.FreeNotification(Self);
end;


{ TdwsDefaultResult }

// Create
//
constructor TdwsDefaultResult.Create(resultType: TdwsResultType);
begin
   inherited;
   FTextBuilder:=TWriteOnlyBlockStream.Create;
end;

// Destroy
//
destructor TdwsDefaultResult.Destroy;
begin
   inherited;
   FTextBuilder.Free;
end;

// AddString
//
procedure TdwsDefaultResult.AddString(const str : String);
begin
   FTextBuilder.WriteString(str);
end;

// ToString
//
function TdwsDefaultResult.ToString : String;
begin
   Result:=GetText;
end;

// GetText
//
function TdwsDefaultResult.GetText : String;
begin
   Result:=FTextBuilder.ToString;
end;

{ TdwsDefaultResultType }

function TdwsDefaultResultType.CreateProgResult: TdwsResult;
begin
  Result := TdwsDefaultResult.Create(Self);
end;

procedure TdwsDefaultResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  inherited;
  TPrintFunction.Create(SymbolTable, 'Print', ['v', 'Variant'], '', False);
  TPrintLnFunction.Create(SymbolTable, 'PrintLn', ['v', 'Variant'], '', False);
end;

{ TPrintFunction }

procedure TPrintFunction.Execute(info : TProgramInfo);
begin
   info.Execution.Result.AddString(info.ValueAsString['v']);
end;

{ TPrintLnFunction }

procedure TPrintLnFunction.Execute(info : TProgramInfo);
var
   result : TdwsResult;
begin
   result:=info.Execution.Result;
   result.AddString(Info.ValueAsString['v']);
   result.AddString(#13#10);
end;

// ------------------
// ------------------ TdwsOptimizationMessageList ------------------
// ------------------

// AddMsg
//
procedure TdwsOptimizationMessageList.AddMsg(aMessage : TdwsMessage);
begin
   inherited;
   FCompileMsgs.AddMsg(aMessage);
   FCompileMsgs.HasErrors:=FCompileMsgs.HasErrors or HasErrors;
end;

// ------------------
// ------------------ TdwsCompilerExecution ------------------
// ------------------

// Create
//
constructor TdwsCompilerExecution.Create(const stackParams : TStackParameters; compiler : TdwsCompiler);
begin
   inherited Create(stackParams);
   FCompiler:=compiler;
   FOptimMsgs:=TdwsOptimizationMessageList.Create;
   FOptimMsgs.FCompileMsgs:=compiler.FMsgs;
end;

// Destroy
//
destructor TdwsCompilerExecution.Destroy;
begin
   inherited;
   FOptimMsgs.Free;
end;

// GetCallStack
//
function TdwsCompilerExecution.GetCallStack : TdwsExprLocationArray;
begin
   Result:=nil;
end;

// CallStackDepth
//
function TdwsCompilerExecution.CallStackDepth : Integer;
begin
   Result:=0;
end;

// GetMsgs
//
function TdwsCompilerExecution.GetMsgs : TdwsRuntimeMessageList;
begin
   Result:=FOptimMsgs;
end;

// ------------------
// ------------------ TdwsEvaluateExpr ------------------
// ------------------

// Destroy
//
destructor TdwsEvaluateExpr.Destroy;
begin
   FExpression.Free;
   inherited;
end;

// ContextIsValid
//
function TdwsEvaluateExpr.ContextIsValid : Boolean;
begin
   Result:=(FContextProcedure=(FExecution.ExecutionObject as TdwsProgramExecution).CurrentProg);
end;

// GetExecution
//
function TdwsEvaluateExpr.GetExecution : IdwsProgramExecution;
begin
   Result:=FExecution;
end;

// GetRootProgram
//
function TdwsEvaluateExpr.GetRootProgram : IdwsProgram;
begin
   Result:=FExecution.Prog;
end;

// GetContextProcedure
//
function TdwsEvaluateExpr.GetContextProcedure : TdwsProcedure;
begin
   Result:=FContextProcedure;
end;

// GetExpression
//
function TdwsEvaluateExpr.GetExpression : TTypedExpr;
begin
   Result:=FExpression;
end;

// GetEvaluationError
//
function TdwsEvaluateExpr.GetEvaluationError : Boolean;
begin
   Result:=FEvaluationError;
end;

// ------------------
// ------------------ TdwsCompilerUnitContextStack ------------------
// ------------------

// Destroy
//
destructor TdwsCompilerUnitContextStack.Destroy;
begin
   Clean;
   inherited;
end;

// Clean
//
procedure TdwsCompilerUnitContextStack.Clean;
begin
   while Count>0 do begin
      Peek.Tokenizer.Free;
      Peek.ConditionalDefines.Free;
      Pop;
   end;
end;

// PushContext
//
procedure TdwsCompilerUnitContextStack.PushContext(compiler : TdwsCompiler);
var
   context : TdwsCompilerUnitContext;
begin
   context.Tokenizer:=compiler.FTok;
   context.UnitSymbol:=compiler.FUnitSymbol;
   context.ConditionalDefines:=nil;
   Push(context);
end;

// PopContext
//
procedure TdwsCompilerUnitContextStack.PopContext(compiler : TdwsCompiler);
begin
   compiler.FTok:=Peek.Tokenizer;
   compiler.FUnitSymbol:=Peek.UnitSymbol;
   Pop;
end;

end.

