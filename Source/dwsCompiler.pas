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
unit dwsCompiler;

{$I dws.inc}

interface

uses
  Variants, Classes, SysUtils, dwsExprs, dwsSymbols, dwsTokenizer, dwsErrors,
  dwsStrings, dwsFunctions, dwsStack, dwsCoreExprs, dwsFileSystem, dwsUtils,
  dwsMagicExprs, dwsRelExprs, dwsOperators, dwsPascalTokenizer, dwsSystemOperators,
  dwsUnitSymbols, dwsXPlatform;

type
   TCompilerOption = (
      coOptimize,          // enable compiler optimizations
      coSymbolDictionary,  // fillup symbol dictionary
      coContextMap,        // fillup context map
      coAssertions,        // compile asserions (if absent, ignores assertions)
      coHintsDisabled,     // don't generate hints messages
      coWarningsDisabled,  // don't generate warnings messages
      coExplicitUnitUses,  // unit dependencies must be explicit via a "uses" clause
      coVariablesAsVarOnly,// only variable can be passed as "var" parameters
                           // (for CodeGen that does not support passing record fields or array elements)
      coAllowClosures      // allow closures, ie. capture of local procedures as function pointers
                           // (not suppported yet by script engine, may be supported by CodeGen)
      );
   TCompilerOptions = set of TCompilerOption;

const
   cDefaultCompilerOptions = [coOptimize, coAssertions];
   cDefaultMaxRecursionDepth = 1024;
   cDefaultMaxExceptionDepth = 10;
   cDefaultStackChunkSize = 4096;  // 64 kB in 32bit Delphi, each stack entry is a Variant

type
   TdwsCompiler = class;
   TdwsFilter = class;

   TIncludeEvent = procedure(const scriptName: String; var scriptSource: String) of object;
   TdwsOnNeedUnitEvent = function(const unitName : String; var unitSource : String) : IdwsUnit of object;
   TdwsResourceEvent = procedure(compiler : TdwsCompiler; const resourceName : String) of object;

   TCompilerCreateBaseVariantSymbol = function (table : TSystemSymbolTable) : TBaseVariantSymbol of object;
   TCompilerReadInstrEvent = function (compiler : TdwsCompiler) : TNoResultExpr of object;
   TCompilerReadInstrSwitchEvent = function (compiler : TdwsCompiler) : Boolean of object;
   TCompilerFindUnknownNameEvent = function (compiler : TdwsCompiler; const name : String) : TSymbol of object;
   TCompilerSectionChangedEvent = procedure (compiler : TdwsCompiler) of object;
   TCompilerReadScriptEvent = procedure (compiler : TdwsCompiler; sourceFile : TSourceFile; scriptType : TScriptSourceType) of object;
   TCompilerGetDefaultEnvironmentEvent = function : IdwsEnvironment of object;
   TCompilerGetDefaultLocalizerEvent = function : IdwsLocalizer of object;

   TdwsNameListOption = (nloAllowDots, nloNoCheckSpecials, nloAllowStrings);
   TdwsNameListOptions = set of TdwsNameListOption;

   // TdwsLocalizerComponent
   //
   TdwsLocalizerComponent = class (TComponent)
      public
         function GetLocalizer : IdwsLocalizer; virtual; abstract;
   end;

   TdwsConfiguration = class(TPersistent)
      private
         FCompilerOptions : TCompilerOptions;
         FHintsLevel : TdwsHintsLevel;
         FConnectors : TStrings;
         FDefaultResultType : TdwsResultType;
         FFilter : TdwsFilter;
         FMaxDataSize : Integer;
         FMaxRecursionDepth : Integer;
         FMaxExceptionDepth : Integer;
         FOnInclude : TIncludeEvent;
         FOnNeedUnit : TdwsOnNeedUnitEvent;
         FOnResource : TdwsResourceEvent;
         FOnCreateBaseVariantSymbol : TCompilerCreateBaseVariantSymbol;
         FOwner : TComponent;
         FResultType : TdwsResultType;
         FScriptPaths : TStrings;
         FConditionals : TStringList;
         FStackChunkSize : Integer;
         FSystemTable : ISystemSymbolTable;
         FTimeoutMilliseconds: Integer;
         FUnits : TIdwsUnitList;
         FCompileFileSystem : TdwsCustomFileSystem;
         FRuntimeFileSystem : TdwsCustomFileSystem;
         FLocalizer : TdwsLocalizerComponent;

      protected
         procedure InitSystemTable;
         procedure SetResultType(const Value : TdwsResultType);
         procedure SetFilter(const Value : TdwsFilter);
         procedure SetTimeOut(const val : Integer);
         procedure SetCompileFileSystem(const val : TdwsCustomFileSystem);
         procedure SetRuntimeFileSystem(const val : TdwsCustomFileSystem);
         procedure SetScriptPaths(const values : TStrings);
         procedure SetConditionals(const val : TStringList);
         function  GetSystemTable : ISystemSymbolTable;
         procedure SetLocalizer(const val : TdwsLocalizerComponent);

         function DoGetLocalizer : IdwsLocalizer;

      public
         constructor Create(Owner: TComponent);
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure Notification(AComponent: TComponent; Operation: TOperation);

         procedure DetachSystemTable;

         property Connectors : TStrings read FConnectors write FConnectors;
         property SystemTable : ISystemSymbolTable read GetSystemTable;
         property Units : TIdwsUnitList read FUnits;

      published
         property Filter : TdwsFilter read FFilter write SetFilter;
         property ResultType : TdwsResultType read FResultType write SetResultType;
         property CompilerOptions : TCompilerOptions read FCompilerOptions write FCompilerOptions default cDefaultCompilerOptions;
         property HintsLevel : TdwsHintsLevel read FHintsLevel write FHintsLevel default hlStrict;
         property MaxDataSize : Integer read FMaxDataSize write FMaxDataSize default 0;
         property MaxRecursionDepth : Integer read FMaxRecursionDepth write FMaxRecursionDepth default cDefaultMaxRecursionDepth;
         property MaxExceptionDepth : Integer read FMaxExceptionDepth write FMaxExceptionDepth default cDefaultMaxExceptionDepth;
         property Conditionals : TStringList read FConditionals write SetConditionals;
         property ScriptPaths : TStrings read FScriptPaths write SetScriptPaths;
         property CompileFileSystem : TdwsCustomFileSystem read FCompileFileSystem write SetCompileFileSystem;
         property RuntimeFileSystem : TdwsCustomFileSystem read FRuntimeFileSystem write SetRuntimeFileSystem;
         property Localizer : TdwsLocalizerComponent read FLocalizer write SetLocalizer;
         property TimeoutMilliseconds : Integer read FTimeoutMilliseconds write FTimeoutMilliseconds default 0;
         property TimeOut : Integer write SetTimeOut;
         property StackChunkSize : Integer read FStackChunkSize write FStackChunkSize default cDefaultStackChunkSize;
         property OnInclude : TIncludeEvent read FOnInclude write FOnInclude;
         property OnNeedUnit : TdwsOnNeedUnitEvent read FOnNeedUnit write FOnNeedUnit;
         property OnResource : TdwsResourceEvent read FOnResource write FOnResource;
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
    function Process(const Text: String; Msgs: TdwsMessageList): String; virtual;
    property Dependencies: TStrings read GetDependencies;
  published
    property SubFilter: TdwsFilter read FSubFilter write SetSubFilter;
  end;

   TAddArgProcedure = procedure (argExpr : TTypedExpr) of object;
   TExpectedArgFunction = function : TParamSymbol of object;

   TSpecialKeywordKind = (skNone, skAbs, skAssert, skAssigned,
                          skHigh, skLength, skLow,
                          skOrd, skSizeOf, skDefined, skDeclared, skSqr,
                          skInc, skDec, skSucc, skPred,
                          skSwap,
                          skConditionalDefined);

   TSwitchInstruction = (siNone,
                         siIncludeLong, siIncludeShort, siIncludeOnce,
                         siFilterLong, siFilterShort,
                         siResourceLong, siResourceShort,
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
                          tcProperty, tcHelper);

   TdwsReadProcDeclOption = (pdoClassMethod, pdoType, pdoAnonymous, pdoLambda);
   TdwsReadProcDeclOptions = set of TdwsReadProcDeclOption;

   TdwsUnitSection = (secMixed, secHeader, secInterface, secImplementation,
                      secInitialization, secFinalization, secEnd);
   TdwsStatementAction = (saNone, saNoSemiColon, saInterface, saImplementation, saEnd);

   TdwsCompilerUnitContext = record
      Tokenizer : TTokenizer;
      UnitSymbol : TUnitMainSymbol;
      Context : TdwsSourceContext;
   end;

   IdwsDataSymbolFactory = interface
      procedure CheckName(const name : String; const namePos : TScriptPos);
      function CreateDataSymbol(const name : String; const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol;
      function CreateConstSymbol(const name : String; const namePos : TScriptPos; typ : TTypeSymbol;
                                 const data : TData; addr : Integer) : TConstSymbol;
      function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
      function ReadArrayConstantExpr(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
   end;

   ISymbolAttributesBag = interface
      function Attributes : TdwsSymbolAttributes;
   end;

   TdwsCompilerUnitContextStack = class(TSimpleStack<TdwsCompilerUnitContext>)
      public
         destructor Destroy; override;
         procedure Clean;

         procedure PushContext(compiler : TdwsCompiler);
         procedure PopContext(compiler : TdwsCompiler; var oldUnitSymbol : TUnitMainSymbol);
   end;

   // TdwsCompiler
   //
   TdwsCompiler = class
      private
         FOptions : TCompilerOptions;
         FTokRules : TTokenizerRules;
         FTok : TTokenizer;
         FProg : TdwsProgram;
         FMainProg : TdwsMainProgram;
         FSourceContextMap : TdwsSourceContextMap;
         FSymbolDictionary : TdwsSymbolDictionary;
         FOperators : TOperators;
         FLoopExprs : TSimpleStack<TNoResultExpr>;
         FLoopExitable : TSimpleStack<TLoopExitable>;
         FFinallyExprs : TSimpleStack<Boolean>;
         FMsgs : TdwsCompileMessageList;
         FDefaultHintsLevel : TdwsHintsLevel;

         FExec : TdwsCompilerExecution;
         FConnectors : TStrings;
         FCompileFileSystem : IdwsFileSystem;
         FOnInclude : TIncludeEvent;
         FOnNeedUnit : TdwsOnNeedUnitEvent;
         FOnResource : TdwsResourceEvent;
         FUnits : TIdwsUnitList;
         FSystemTable : TSystemSymbolTable;
         FScriptPaths : TStrings;
         FFilter : TdwsFilter;
         FIsExcept : Boolean;
         FIsSwitch : Boolean;
         FLineCount : Integer;
         FSourcePostConditionsIndex : Integer;
         FUnitSection : TdwsUnitSection;
         FUnitContextStack : TdwsCompilerUnitContextStack;
         FUnitsFromStack : TSimpleStack<String>;
         FCurrentUnitSymbol : TUnitMainSymbol;
         FAnyFuncSymbol : TAnyFuncSymbol;
         FAnyTypeSymbol : TAnyTypeSymbol;
         FStandardDataSymbolFactory : IdwsDataSymbolFactory;
         FPendingAttributes : TdwsSymbolAttributes;

         FDataSymbolExprReuse : TSimpleObjectObjectHash<TDataSymbol,TVarExpr>;

         FOnCreateBaseVariantSymbol : TCompilerCreateBaseVariantSymbol;
         FOnReadInstr : TCompilerReadInstrEvent;
         FOnReadInstrSwitch : TCompilerReadInstrSwitchEvent;
         FOnFindUnknownName : TCompilerFindUnknownNameEvent;
         FOnSectionChanged : TCompilerSectionChangedEvent;
         FOnReadScript : TCompilerReadScriptEvent;
         FOnGetDefaultEnvironment : TCompilerGetDefaultEnvironmentEvent;
         FOnGetDefaultLocalizer : TCompilerGetDefaultLocalizerEvent;

         function Optimize : Boolean;

         function CheckPropertyFuncParams(paramsA : TSymbolTable; methSym : TMethodSymbol;
                                          indexSym : TSymbol = nil; typSym : TTypeSymbol = nil) : Boolean;
         procedure CheckName(const name : String; const namePos : TScriptPos);
         function IdentifySpecialName(const name : String) : TSpecialKeywordKind;
         procedure CheckSpecialName(const name : String);
         function CheckParams(A, B: TSymbolTable; CheckNames: Boolean; skipB : Integer = 0): Boolean;
         procedure CompareFuncKinds(a, b : TFuncKind);
         procedure CompareFuncSymbolParams(a, b : TFuncSymbol);
         function  CurrentStruct : TCompositeTypeSymbol;
         function  FindStructMember(typ : TStructuredTypeSymbol; const name : String) : TSymbol;

         procedure HintUnusedSymbols;
         procedure HintUnusedPrivateSymbols;
         procedure HintUnusedResult(resultSymbol : TDataSymbol);
         procedure HintReferenceConstVarParams(funcSym : TFuncSymbol);

         function GetVarExpr(dataSym : TDataSymbol): TVarExpr;

         function GetLazyParamExpr(dataSym : TLazyParamSymbol) : TLazyParamExpr;
         function GetVarParamExpr(dataSym : TVarParamSymbol) : TByRefParamExpr;
         function GetConstParamExpr(dataSym : TConstParamSymbol) : TByRefParamExpr;

         function GetSelfParamExpr(selfSym : TDataSymbol) : TVarExpr;
         function ReadAssign(token : TTokenType; var left : TDataExpr) : TNoResultExpr;
         function ReadArrayType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
         function ReadArrayConstant(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
         function ReadArrayMethod(const name : String; const namePos : TScriptPos;
                                  baseExpr : TTypedExpr) : TProgramExpr;
         function ReadCase : TCaseExpr;
         function ReadCaseConditions(condList : TCaseConditions; valueExpr : TTypedExpr) : Integer;
         function ReadNameSymbol(var namePos : TScriptPos) : TSymbol;
         function ReadClassName : TClassSymbol;
         function ReadClassOf(const typeName : String) : TClassOfSymbol;
         function ReadClass(const typeName : String; const flags : TClassSymbolFlags) : TClassSymbol;
         procedure ReadClassVars(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
         procedure ReadClassConst(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
         function ReadInterface(const typeName : String) : TInterfaceSymbol;
         function ReadConnectorSym(const name : String; baseExpr : TTypedExpr;
                                   const connectorType : IConnectorType; isWrite: Boolean) : TProgramExpr;
         function ReadConnectorArray(const name : String; baseExpr : TTypedExpr;
                                     const connectorType : IConnectorType; isWrite: Boolean) : TConnectorCallExpr;
         procedure ReadConstDecl(const factory : IdwsDataSymbolFactory);
         procedure ReadConstDeclBlock(var action : TdwsStatementAction);
         function ReadConstValue : TConstExpr;
         function ReadConstRecord(symbol : TRecordSymbol) : TData;
         function ReadBlock : TNoResultExpr;
         function ReadBlocks(const endTokens : TTokenTypes; var finalToken : TTokenType) : TNoResultExpr;
         function ReadEnumeration(const typeName : String; aStyle : TEnumerationSymbolStyle) : TEnumerationSymbol;
         function ReadExit : TNoResultExpr;
         function ReadClassExpr(ownerSymbol : TCompositeTypeSymbol; expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadExprAdd(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
         function ReadExprMult(expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadExprIn(var left : TTypedExpr) : TTypedExpr;
         function ReadExprInConditions(var left : TTypedExpr) : TInOpExpr;
         function ReadExternalVar(sym : TExternalVarSymbol; isWrite : Boolean) : TFuncExpr;

         function ReadField(const scriptPos : TScriptPos; selfSym : TDataSymbol;
                            fieldSym : TFieldSymbol) : TDataExpr; overload;
         function ReadField(const scriptPos : TScriptPos; selfSym : TDataSymbol;
                         fieldSym : TFieldSymbol; var varExpr : TTypedExpr) : TDataExpr; overload;

         function ReadFor : TNoResultExpr;
         function ReadForTo(const forPos : TScriptPos; loopVarExpr : TVarExpr) : TForExpr;
         function ReadForIn(const forPos : TScriptPos; loopVarExpr : TVarExpr) : TNoResultExpr;
         function ReadForStep(const forPos : TScriptPos; forExprClass : TForExprClass;
                              iterVarExpr : TIntVarExpr; fromExpr, toExpr : TTypedExpr;
                              loopFirstStatement : TNoResultExpr) : TForExpr;

         function ReadFuncOverloaded(funcSym : TFuncSymbol; fromTable : TSymbolTable;
                                     codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TTypedExpr;
         procedure CollectMethodOverloads(methSym : TMethodSymbol; overloads : TFuncSymbolList);
         function ReadSelfMethOverloaded(methSym : TMethodSymbol; isWrite : Boolean;
                                         expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadMethOverloaded(methSym : TMethodSymbol; instanceExpr : TTypedExpr;
                                     const scriptPos : TScriptPos;
                                     expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadStaticMethOverloaded(methSym : TMethodSymbol; metaExpr : TTypedExpr;
                                           const scriptPos : TScriptPos;
                                           expecting : TTypeSymbol = nil) : TTypedExpr;

         function ResolveOverload(var funcExpr : TFuncExprBase; overloads : TFuncSymbolList;
                                  const argPosArray : TScriptPosArray) : Boolean;

         function FuncHasConflictingOverload(funcSym, forwardedSym : TFuncSymbol) : Boolean;
         function MethHasConflictingOverload(methSym : TMethodSymbol) : Boolean;

         function FuncPerfectMatchOverload(funcSym : TFuncSymbol) : TFuncSymbol;
         function MethPerfectMatchOverload(methSym : TMethodSymbol; recurse : Boolean) : TMethodSymbol;

         function ReadFunc(funcSym : TFuncSymbol; codeExpr : TDataExpr = nil;
                           expecting : TTypeSymbol = nil;
                           overloads : TFuncSymbolList = nil) : TTypedExpr;
         function ReadSelfMethod(methodSym : TMethodSymbol; isWrite : Boolean;
                                 expecting : TTypeSymbol = nil;
                                 overloads : TFuncSymbolList = nil) : TTypedExpr;
         function ReadMethod(methodSym : TMethodSymbol; instanceExpr : TTypedExpr;
                             const scriptPos : TScriptPos;
                             expecting : TTypeSymbol = nil;
                             overloads : TFuncSymbolList = nil) : TTypedExpr;
         function ReadStaticMethod(methodSym : TMethodSymbol; metaExpr : TTypedExpr;
                                   const scriptPos : TScriptPos;
                                   expecting : TTypeSymbol = nil;
                                   overloads : TFuncSymbolList = nil) : TTypedExpr;

         function WrapUpFunctionRead(funcExpr : TFuncExprBase; expecting : TTypeSymbol = nil;
                                     overloads : TFuncSymbolList = nil) : TTypedExpr;

         procedure ReadFuncArgs(funcExpr : TFuncExprBase; var argPosArray : TScriptPosArray;
                                overloads : TFuncSymbolList);
         procedure TypeCheckArgs(funcExpr : TFuncExprBase; const argPosArray : TScriptPosArray);
         procedure ReadArguments(const addArgProc : TAddArgProcedure;
                                 leftDelim, rightDelim : TTokenType;
                                 var argPosArray : TScriptPosArray;
                                 const expectedProc : TExpectedArgFunction = nil);
         function ReadFuncResultType(funcKind : TFuncKind) : TTypeSymbol;

         function ReadIf: TNoResultExpr;
         function ReadInherited(isWrite : Boolean) : TProgramExpr;
         function ReadInstr : TNoResultExpr;
         function ReadUntilEndOrElseSwitch(allowElse : Boolean) : Boolean;
         function ReadIntfMethodDecl(intfSym : TInterfaceSymbol; funcKind : TFuncKind) : TSourceMethodSymbol;
         function ReadMethodDecl(const hotPos : TScriptPos;
                                 ownerSym : TCompositeTypeSymbol; funcKind : TFuncKind;
                                 aVisibility : TdwsVisibility; isClassMethod : Boolean) : TMethodSymbol;
         function ReadMethodImpl(ownerSym : TCompositeTypeSymbol; funcKind : TFuncKind;
                                 isClassMethod : Boolean) : TMethodSymbol;
         procedure ReadDeprecatedFunc(funcSym : TFuncSymbol);
         procedure WarnDeprecatedFunc(funcExpr : TFuncExprBase);
         procedure WarnDeprecatedProp(propSym : TPropertySymbol);
         function ResolveUnitNameSpace(unitPrefix : TUnitSymbol) : TUnitSymbol;
         function ReadName(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TProgramExpr;
         function ReadEnumerationSymbolName(const enumPos : TScriptPos; enumSym : TEnumerationSymbol; acceptTypeRef : Boolean) : TProgramExpr;
         function ReadClassSymbolName(baseType : TClassSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadInterfaceSymbolName(baseType : TInterfaceSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadRecordSymbolName(baseType : TRecordSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadConstName(constSym : TConstSymbol; isWrite: Boolean) : TProgramExpr;
         function ReadDataSymbolName(dataSym : TDataSymbol; fromTable : TSymbolTable; isWrite: Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadResourceStringName(resSym : TResourceStringSymbol; const namePos : TScriptPos) : TResourceStringExpr;
         function ReadNameOld(isWrite : Boolean) : TTypedExpr;
         function ReadNameInherited(isWrite : Boolean) : TProgramExpr;
         procedure ReadNameList(names : TStrings; var posArray : TScriptPosArray;
                                const options : TdwsNameListOptions = []);
         procedure ReadExternalName(funcSym : TFuncSymbol);
         function  ReadNew(restrictTo : TClassSymbol) : TProgramExpr;
         function  ReadNewArray(elementTyp : TTypeSymbol) : TNewArrayExpr;
         procedure ReadArrayParams(ArrayIndices: TSymbolTable);
         // Don't want to add param symbols to dictionary when a method implementation (they get thrown away)
         procedure ReadParams(const hasParamMeth : THasParamSymbolMethod;
                              const addParamMeth : TAddParamSymbolMethod;
                              forwardedParams : TParamsSymbolTable;
                              expectedLambdaParams : TParamsSymbolTable);
         function ReadProcDecl(funcToken : TTokenType; const hotPos : TScriptPos;
                               declOptions : TdwsReadProcDeclOptions = [];
                               expectedLambdaParams : TParamsSymbolTable = nil) : TFuncSymbol;
         procedure ReadProcBody(funcSymbol : TFuncSymbol);
         procedure ReadProcEmpty(funcSymbol : TFuncSymbol);
         procedure ReadConditions(funcSymbol : TFuncSymbol; conditions : TSourceConditions;
                               condsSymClass : TConditionSymbolClass);
         procedure ReadPostConditions(funcSymbol : TFuncSymbol; conditions : TSourcePostConditions;
                                   condsSymClass : TConditionSymbolClass);
         function ReadOperatorDecl : TOperatorSymbol;
         function ReadClassOperatorDecl(ClassSym: TClassSymbol) : TClassOperatorSymbol;
         function ReadPropertyDecl(ownerSym : TCompositeTypeSymbol; aVisibility : TdwsVisibility) : TPropertySymbol;
         function ReadPropertyExpr(var expr : TDataExpr; propertySym : TPropertySymbol; isWrite: Boolean) : TProgramExpr; overload;
         function ReadPropertyExpr(var expr : TTypedExpr; propertySym : TPropertySymbol; isWrite: Boolean) : TProgramExpr; overload;
         function ReadPropertyReadExpr(var expr : TTypedExpr; propertySym : TPropertySymbol) : TTypedExpr;
         function ReadPropertyWriteExpr(var expr : TTypedExpr; propertySym : TPropertySymbol) : TProgramExpr;
         function ReadPropertyArrayAccessor(var expr : TTypedExpr; propertySym : TPropertySymbol;
                                            typedExprList : TTypedExprList;
                                            const scriptPos : TScriptPos; isWrite : Boolean) : TFuncExpr;

         function ReadRecordDecl(const typeName : String; allowNonConstExpressions : Boolean) : TRecordSymbol;
         procedure ReadFieldsDecl(struct : TStructuredTypeSymbol; visibility : TdwsVisibility;
                                  allowNonConstExpressions : Boolean);

         function ReadHelperDecl(const typeName : String; qualifierToken : TTokenType) : THelperSymbol;

         function ReadRaise : TRaiseBaseExpr;
         function ReadRepeat : TNoResultExpr;
         function ReadRootStatement(var action : TdwsStatementAction) : TNoResultExpr;
         function ReadRootBlock(const endTokens: TTokenTypes; var finalToken: TTokenType) : TBlockExpr;
         procedure ReadImplementationBlock;
         procedure ReadSemiColon;
         function ReadScript(sourceFile : TSourceFile; scriptType : TScriptSourceType) : TNoResultExpr;
         procedure ReadScriptImplementations;
         function ReadSpecialFunction(const namePos : TScriptPos; specialKind : TSpecialKeywordKind) : TProgramExpr;
         function ReadStatement(var action : TdwsStatementAction) : TNoResultExpr;
         function ReadResourceStringDecl : TResourceStringSymbol;
         procedure ReadResourceStringDeclBlock(var action : TdwsStatementAction);
         function ReadStringArray(expr : TDataExpr; isWrite : Boolean) : TProgramExpr;

         function ReadSwitch(const switchName : String) : Boolean;
         function ReadInstrSwitch(const switchName : String) : Boolean;
         function ReadExprSwitch(const switchPos : TScriptPos) : Boolean;
         procedure SkipUntilCurlyRight;

         function ReadSymbol(expr : TProgramExpr; isWrite : Boolean = False;
                             expecting : TTypeSymbol = nil) : TProgramExpr;
         function ReadSymbolArrayExpr(var baseExpr : TDataExpr) : TProgramExpr;
         function ReadSymbolMemberExpr(var expr : TProgramExpr;
                                       isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;

         function ReadTerm(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadNegation : TTypedExpr;

         function ReadTry : TExceptionExpr;
         function ReadFinally(tryExpr : TNoResultExpr) : TFinallyExpr;
         function ReadExcept(tryExpr : TNoResultExpr; var finalToken : TTokenType) : TExceptExpr;

         function ReadType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
         function ReadTypeCast(const namePos : TScriptPos; typeSym : TTypeSymbol) : TTypedExpr;
         function ReadTypeExpr(const namePos : TScriptPos; typeSym : TTypeSymbol;
                               isWrite : Boolean; expecting : TTypeSymbol = nil) : TProgramExpr;

         procedure ReadAttributes;
         function  BagPendingAttributes : ISymbolAttributesBag;
         procedure AttachBaggedAttributes(symbol : TSymbol; const bag : ISymbolAttributesBag);
         procedure CheckNoPendingAttributes;

         function EnumerateHelpers(typeSym : TTypeSymbol) : THelperSymbols;
         function ReadTypeHelper(expr : TTypedExpr;
                                 const name : String; const namePos : TScriptPos;
                                 expecting : TTypeSymbol) : TProgramExpr;

         procedure ReadTypeDeclBlock;
         function  ReadTypeDecl(firstInBlock : Boolean) : Boolean;
         procedure ReadUses;
         procedure ReadUnitHeader;
         function ReadVarDecl(const dataSymbolFactory : IdwsDataSymbolFactory) : TNoResultExpr;
         function ReadWhile : TNoResultExpr;
         function ResolveUnitReferences : TIdwsUnitList;

      protected
         procedure EnterLoop(loopExpr : TNoResultExpr);
         procedure MarkLoopExitable(level : TLoopExitable);
         procedure LeaveLoop;

         function GetFuncExpr(funcSym : TFuncSymbol; codeExpr : TDataExpr = nil) : TFuncExprBase;
         function GetMethodExpr(meth: TMethodSymbol; Expr: TTypedExpr; RefKind: TRefKind;
                                const scriptPos: TScriptPos; ForceStatic : Boolean): TFuncExpr;

         procedure MemberSymbolWithNameAlreadyExists(sym : TSymbol; const hotPos : TScriptPos);
         procedure IncompatibleTypes(const scriptPos : TScriptPos; const fmt : String; typ1, typ2 : TTypeSymbol);

         function CreateProgram(const systemTable : ISystemSymbolTable;
                                resultType : TdwsResultType;
                                const stackParams : TStackParameters) : TdwsMainProgram;
         function CreateAssign(const scriptPos : TScriptPos; token : TTokenType;
                               left : TDataExpr; right : TTypedExpr) : TNoResultExpr;

         function CreateArrayLow(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
         function CreateArrayHigh(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
         function CreateArrayLength(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
         function CreateArrayExpr(const scriptPos : TScriptPos; baseExpr : TDataExpr; indexExpr : TTypedExpr) : TArrayExpr;

         function ResolveOperatorFor(token : TTokenType; aLeftType, aRightType : TTypeSymbol) : TOperatorSymbol;
         function CreateTypedOperatorExpr(token : TTokenType; aLeft, aRight : TTypedExpr) : TTypedExpr;
         function CreateAssignOperatorExpr(token : TTokenType; const scriptPos : TScriptPos;
                                           aLeft, aRight : TTypedExpr) : TAssignExpr;

         procedure DoSectionChanged;
         procedure DoTokenizerEndSourceFile(sourceFile : TSourceFile);

         property  CurrentUnitSymbol : TUnitMainSymbol read FCurrentUnitSymbol;
         procedure EnterUnit(unitSymbol : TUnitMainSymbol; var oldUnitSymbol : TUnitMainSymbol);
         procedure LeaveUnit(oldUnitSymbol : TUnitMainSymbol);
         procedure SwitchTokenizerToUnit(srcUnit : TSourceUnit; const sourceCode : String);

         procedure SetupCompileOptions(conf : TdwsConfiguration);
         procedure SetupMsgsOptions(conf : TdwsConfiguration);
         procedure CleanupAfterCompile;

         procedure CheckFilterDependencies(confUnits : TIdwsUnitList);
         procedure HandleUnitDependencies;
         function  HandleExplicitDependency(const unitName : String) : TUnitSymbol;

         procedure SetupInitializationFinalization;

      public
         constructor Create;
         destructor Destroy; override;

         function Compile(const aCodeText : String; aConf : TdwsConfiguration) : IdwsProgram;
         procedure RecompileInContext(const context : IdwsProgram; const aCodeText : String; aConf : TdwsConfiguration);

         class function Evaluate(exec : IdwsProgramExecution; const anExpression : String;
                              options : TdwsEvaluateOptions = []) : IdwsEvaluateExpr;

         procedure WarnForVarUsage(varExpr : TVarExpr; const scriptPos : TScriptPos);

         procedure RecordSymbolUse(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
         procedure RecordSymbolUseReference(sym : TSymbol; const scriptPos : TScriptPos; isWrite : Boolean);
         procedure RecordSymbolUseImplicitReference(sym : TSymbol; const scriptPos : TScriptPos; isWrite : Boolean);
         procedure ReplaceSymbolUse(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);

         function OpenStreamForFile(const fileName : String) : TStream;
         function GetScriptSource(const scriptName : String) : String;
         function GetIncludeScriptSource(const scriptName : String) : String;

         property CurrentProg : TdwsProgram read FProg write FProg;
         property Msgs : TdwsCompileMessageList read FMsgs;
         property Options : TCompilerOptions read FOptions write FOptions;
         property ScriptPaths : TStrings read FScriptPaths;
         property Filter : TdwsFilter read FFilter;
         property SymbolDictionary : TdwsSymbolDictionary read FSymbolDictionary;
         property UnitSection : TdwsUnitSection read FUnitSection write FUnitSection;
         property TokenizerRules : TTokenizerRules read FTokRules;
         property Tokenizer : TTokenizer read FTok write FTok;

         property OnCreateBaseVariantSymbol : TCompilerCreateBaseVariantSymbol read FOnCreateBaseVariantSymbol write FOnCreateBaseVariantSymbol;
         property OnReadInstr : TCompilerReadInstrEvent read FOnReadInstr write FOnReadInstr;
         property OnReadInstrSwitch : TCompilerReadInstrSwitchEvent read FOnReadInstrSwitch write FOnReadInstrSwitch;
         property OnFindUnknownName : TCompilerFindUnknownNameEvent read FOnFindUnknownName write FOnFindUnknownName;
         property OnSectionChanged : TCompilerSectionChangedEvent read FOnSectionChanged write FOnSectionChanged;
         property OnReadScript : TCompilerReadScriptEvent read FOnReadScript write FOnReadScript;
         property OnGetDefaultEnvironment : TCompilerGetDefaultEnvironmentEvent read FOnGetDefaultEnvironment write FOnGetDefaultEnvironment;
         property OnGetDefaultLocalizer : TCompilerGetDefaultLocalizerEvent read FOnGetDefaultLocalizer write FOnGetDefaultLocalizer;
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
      SWI_RESOURCE_LONG, SWI_RESOURCE_SHORT,
      SWI_DEFINE, SWI_UNDEF,
      SWI_IFDEF, SWI_IFNDEF, SWI_IF, SWI_ENDIF, SWI_ELSE,
      SWI_HINT, SWI_HINTS, SWI_WARNING, SWI_WARNINGS, SWI_ERROR, SWI_FATAL
      );

   cAssignmentTokens : TTokenTypes = [ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
                                      ttTIMES_ASSIGN, ttDIVIDE_ASSIGN];

   cTokenToVisibility : array [ttPRIVATE..ttPUBLISHED] of TdwsVisibility = (
      cvPrivate, cvProtected, cvPublic, cvPublished );
   cTokenToFuncKind : array [ttFUNCTION..ttLAMBDA] of TFuncKind = (
      fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod, fkLambda );

type
   TReachStatus = (rsReachable, rsUnReachable, rsUnReachableWarned);

   TObjectClassNameMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TObjectClassTypeMethod = class(TInternalMethod)
      procedure Execute(info : TProgramInfo; var ExternalObject: TObject); override;
   end;

   TObjectClassParentMethod = class(TInternalMethod)
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

   TStandardSymbolFactory = class (TInterfacedObject, IdwsDataSymbolFactory)
      FCompiler : TdwsCompiler;
      constructor Create(aCompiler : TdwsCompiler);
      procedure CheckName(const name : String; const namePos : TScriptPos); virtual;
      function CreateDataSymbol(const name : String; const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol; virtual;
      function CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                 typ : TTypeSymbol; const data : TData; addr : Integer) : TConstSymbol; virtual;
      function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr; virtual;
      function ReadArrayConstantExpr(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
   end;

   TCompositeTypeSymbolFactory = class (TStandardSymbolFactory)
      FOwnerType : TCompositeTypeSymbol;
      FVisibility : TdwsVisibility;
      constructor Create(aCompiler : TdwsCompiler; ownerType : TCompositeTypeSymbol;
                         aVisibility : TdwsVisibility);
      procedure CheckName(const name : String; const namePos : TScriptPos); override;
      function CreateDataSymbol(const name : String; const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol; override;
      function CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                 typ : TTypeSymbol; const data : TData; addr : Integer) : TConstSymbol; override;
      function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr; override;
   end;

   // const expr created to "keep compiling"
   TBogusConstExpr = class sealed (TConstExpr);

   TSymbolAttributesBag = class (TInterfacedSelfObject, ISymbolAttributesBag)
      FAttributes : TdwsSymbolAttributes;
      destructor Destroy; override;
      function Attributes : TdwsSymbolAttributes;
   end;

// StringToSwitchInstruction
//
function StringToSwitchInstruction(const str : String) : TSwitchInstruction;
begin
   // This procedure is called by the tokenizer if it finds {$xx in the String
   for Result:=Low(TSwitchInstruction) to High(TSwitchInstruction) do begin
      if str=cSwitchInstructions[Result] then
         Exit;
   end;
   Result:=siNone;
end;

// ------------------
// ------------------ TSymbolAttributesBag ------------------
// ------------------

// Destroy
//
destructor TSymbolAttributesBag.Destroy;
begin
   FAttributes.Free;
   inherited;
end;

// Attributes
//
function TSymbolAttributesBag.Attributes : TdwsSymbolAttributes;
begin
   Result:=FAttributes;
end;

// ------------------
// ------------------ TStandardSymbolFactory ------------------
// ------------------

// Create
//
constructor TStandardSymbolFactory.Create(aCompiler : TdwsCompiler);
begin
   inherited Create;
   FCompiler:=aCompiler;
end;

// CheckName
//
procedure TStandardSymbolFactory.CheckName(const name : String; const namePos : TScriptPos);
begin
   FCompiler.CheckName(name, namePos);
end;

// CreateDataSymbol
//
function TStandardSymbolFactory.CreateDataSymbol(const name : String; const namePos : TScriptPos;
                                                 typ : TTypeSymbol) : TDataSymbol;
begin
   CheckName(name, namePos);
   Result:=TDataSymbol.Create(name, typ);
   FCompiler.FProg.Table.AddSymbol(Result);
end;

// CreateConstSymbol
//
function TStandardSymbolFactory.CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                                  typ : TTypeSymbol; const data : TData; addr : Integer) : TConstSymbol;
begin
   if data<>nil then
      Result:=TConstSymbol.Create(name, typ, data, addr)
   else Result:=TConstSymbol.Create(name, typ);
   FCompiler.FProg.Table.AddSymbol(Result);
end;

// ReadExpr
//
function TStandardSymbolFactory.ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
begin
   Result:=FCompiler.ReadExpr(expecting);
end;

// ReadArrayConstantExpr
//
function TStandardSymbolFactory.ReadArrayConstantExpr(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
begin
   Result:=FCompiler.ReadArrayConstant(closingToken, expecting);
end;

// ------------------
// ------------------ TCompositeTypeSymbolFactory ------------------
// ------------------

// Create
//
constructor TCompositeTypeSymbolFactory.Create(aCompiler : TdwsCompiler;
      ownerType : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
begin
   inherited Create(aCompiler);
   FOwnerType:=ownerType;
   FVisibility:=aVisibility;
end;

// CheckName
//
procedure TCompositeTypeSymbolFactory.CheckName(const name : String; const namePos : TScriptPos);
var
   sym : TSymbol;
begin
   if name='' then Exit;

   sym:=FOwnerType.Members.FindLocal(name);
   if Assigned(sym) then
      FCompiler.FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);
end;

// CreateDataSymbol
//
function TCompositeTypeSymbolFactory.CreateDataSymbol(const name : String; const namePos : TScriptPos;
                                                       typ : TTypeSymbol) : TDataSymbol;
var
   cvs : TClassVarSymbol;
begin
   CheckName(name, namePos);

   cvs:=TClassVarSymbol.Create(name, typ);
   cvs.Visibility:=FVisibility;
   cvs.AllocateStackAddr(FCompiler.FProg.Table.AddrGenerator);
   FOwnerType.AddClassVar(cvs);
   Result:=cvs;
end;

// CreateConstSymbol
//
function TCompositeTypeSymbolFactory.CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                                        typ : TTypeSymbol; const data : TData; addr : Integer) : TConstSymbol;
var
   classConstSym : TClassConstSymbol;
begin
   if data<>nil then
      classConstSym:=TClassConstSymbol.Create(name, typ, data, addr)
   else classConstSym:=TClassConstSymbol.Create(name, typ);
   classConstSym.Visibility:=FVisibility;
   FOwnerType.AddConst(classConstSym);
   Result:=classConstSym;
end;

// ReadExpr
//
function TCompositeTypeSymbolFactory.ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
begin
   Result:=FCompiler.ReadClassExpr(FOwnerType, expecting);
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

   FStandardDataSymbolFactory:=TStandardSymbolFactory.Create(Self);

   FTokRules:=TPascalTokenizerStateRules.Create;

   FLoopExprs:=TSimpleStack<TNoResultExpr>.Create;
   FLoopExitable:=TSimpleStack<TLoopExitable>.Create;
   FFinallyExprs:=TSimpleStack<Boolean>.Create;
   FUnitsFromStack:=TSimpleStack<String>.Create;
   FUnitContextStack:=TdwsCompilerUnitContextStack.Create;

   FAnyFuncSymbol:=TAnyFuncSymbol.Create('', fkFunction, 0);
   FAnyTypeSymbol:=TAnyTypeSymbol.Create('', nil);

   FPendingAttributes:=TdwsSymbolAttributes.Create;

   stackParams.MaxLevel:=1;
   stackParams.ChunkSize:=512;
   stackParams.MaxByteSize:=MaxInt;
   stackParams.MaxRecursionDepth:=cDefaultMaxRecursionDepth;
   stackParams.MaxExceptionDepth:=cDefaultMaxExceptionDepth;

   FExec:=TdwsCompilerExecution.Create(stackParams, Self);
end;

// Destroy
//
destructor TdwsCompiler.Destroy;
begin
   FPendingAttributes.Free;

   FAnyFuncSymbol.Free;
   FAnyTypeSymbol.Free;

   FUnitsFromStack.Free;
   FUnitContextStack.Free;
   FExec.Free;
   FFinallyExprs.Free;
   FLoopExitable.Free;
   FLoopExprs.Free;
   FTokRules.Free;
   inherited;
end;

function TdwsCompiler.ResolveUnitReferences : TIdwsUnitList;
var
   x, y, z : Integer;
   expectedUnitCount : Integer;
   deps: TStrings;
   refCount : array of Integer;
   changed : Boolean;
   unitName: String;
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
            if FUnits.IndexOfName(deps[y]) < 0 then
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
function TdwsCompiler.GetFuncExpr(funcSym : TFuncSymbol; codeExpr : TDataExpr = nil) : TFuncExprBase;
var
   magicFuncSym : TMagicFuncSymbol;
begin
   if (codeExpr=nil) and funcSym.IsType then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionMethodExpected);

   if codeExpr=nil then begin

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
function TdwsCompiler.GetMethodExpr(meth: TMethodSymbol; Expr: TTypedExpr; RefKind: TRefKind;
                                    const scriptPos : TScriptPos; ForceStatic : Boolean): TFuncExpr;
begin
   Result:=CreateMethodExpr(FProg, meth, Expr, RefKind, scriptPos, ForceStatic);

   if (meth.Typ<>nil) and (meth.Typ.Size>1) then
      Result.SetResultAddr(FProg, nil);
end;

// MemberSymbolWithNameAlreadyExists
//
procedure TdwsCompiler.MemberSymbolWithNameAlreadyExists(sym : TSymbol; const hotPos : TScriptPos);
var
   msgFmt : String;
begin
   if sym is TFieldSymbol then
      msgFmt:=CPE_FieldRedefined
   else if sym is TPropertySymbol then
      msgFmt:=CPE_PropertyRedefined
   else if sym is TClassVarSymbol then
      msgFmt:=CPE_ClassVarRedefined
   else if sym is TConstSymbol then
      msgFmt:=CPE_ClassConstRedefined
   else begin
      Assert(sym is TMethodSymbol);
      msgFmt:=CPE_MethodRedefined
   end;
   FMsgs.AddCompilerErrorFmt(hotPos, msgFmt, [sym.Name])
end;

// IncompatibleTypes
//
procedure TdwsCompiler.IncompatibleTypes(const scriptPos : TScriptPos;
                                         const fmt : String; typ1, typ2 : TTypeSymbol);
begin
   FMsgs.AddCompilerErrorFmt(scriptPos, fmt, [typ1.Caption, typ2.Caption]);
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
   FOnResource := conf.OnResource;
   FScriptPaths := conf.ScriptPaths;

   conf.FOnCreateBaseVariantSymbol:=FOnCreateBaseVariantSymbol;
   if Assigned(FOnCreateBaseVariantSymbol) then
      conf.DetachSystemTable;

   if conf.CompileFileSystem<>nil then
      FCompileFileSystem := conf.CompileFileSystem.AllocateFileSystem
   else FCompileFileSystem := TdwsOSFileSystem.Create;

   FOnGetDefaultLocalizer := conf.DoGetLocalizer;

   FDataSymbolExprReuse:=TSimpleObjectObjectHash<TDataSymbol,TVarExpr>.Create;
end;

// SetupMsgsOptions
//
procedure TdwsCompiler.SetupMsgsOptions(conf : TdwsConfiguration);
begin
   FDefaultHintsLevel:=conf.HintsLevel;
   if coHintsDisabled in conf.CompilerOptions then
      FMsgs.HintsLevel:=hlDisabled
   else FMsgs.HintsLevel:=conf.HintsLevel;
   FMsgs.WarningsDisabled:=(coWarningsDisabled in conf.CompilerOptions);
end;

// CleanupAfterCompile
//
procedure TdwsCompiler.CleanupAfterCompile;
begin
   FPendingAttributes.Clear;

   FDataSymbolExprReuse.CleanValues;
   FDataSymbolExprReuse.Free;
   FDataSymbolExprReuse:=nil;

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
   FCurrentUnitSymbol:=nil;

   FProg:=nil;
   FMainProg:=nil;
   FSourceContextMap:=nil;
   FSymbolDictionary:=nil;

   FLoopExprs.Clear;
   FLoopExitable.Clear;
   FFinallyExprs.Clear;
end;

// Compile
//
function TdwsCompiler.Compile(const aCodeText : String; aConf : TdwsConfiguration) : IdwsProgram;
var
   stackParams : TStackParameters;
   codeText : String;
   sourceFile : TSourceFile;
   compileStartTime : TDateTime;
begin
   compileStartTime:=Now;

   SetupCompileOptions(aConf);

   stackParams.MaxByteSize:=aConf.MaxDataSize;
   if stackParams.MaxByteSize<=0 then
      stackParams.MaxByteSize:=MaxInt;

   stackParams.ChunkSize:=aConf.StackChunkSize;
   Assert(stackParams.ChunkSize>0);

   stackParams.MaxRecursionDepth:=aConf.MaxRecursionDepth;
   stackParams.MaxExceptionDepth:=aConf.MaxExceptionDepth;

   FLineCount:=0;

   // Create the TdwsProgram
   FMainProg:=CreateProgram(aConf.SystemTable, aConf.ResultType, stackParams);
   FSystemTable:=FMainProg.SystemTable.SymbolTable;

   FMsgs:=FMainProg.CompileMsgs;
   SetupMsgsOptions(aConf);

   FMainProg.Compiler:=Self;
   FMainProg.TimeoutMilliseconds:=aConf.TimeoutMilliseconds;
   FMainProg.RuntimeFileSystem:=aConf.RuntimeFileSystem;
   FMainProg.ConditionalDefines.Value.Assign(aConf.Conditionals);
   FSourceContextMap:=FMainProg.SourceContextMap;
   FSymbolDictionary:=FMainProg.SymbolDictionary;
   FUnitSection:=secMixed;

   FProg:=FMainProg;

   FOperators:=TSystemOperators.Create(FSystemTable, FProg.Table);
   FMainProg.Operators:=FOperators;

   try
      CheckFilterDependencies(aConf.Units);

      // Filter stuff
      if Assigned(FFilter) then
         codeText := FFilter.Process(aCodeText, FMsgs)
      else codeText := aCodeText;

      sourceFile:=FMainProg.SourceList.Add(MSG_MainModule, codeText, stMain);

      // Start compilation
      FProg.Expr:=ReadScript(sourceFile, stMain);

      if FProg.Expr=nil then
         FProg.Expr:=TNullExpr.Create(FProg, cNullPos);

      HintUnusedPrivateSymbols;

      // Initialize symbol table
      FProg.Table.Initialize(FMsgs);
      FProg.UnitMains.Initialize(FMsgs);

      SetupInitializationFinalization;

      // setup environment
      if Assigned(FOnGetDefaultEnvironment) then
         FMainProg.DefaultEnvironment:=FOnGetDefaultEnvironment();

      // setup localizer
      if Assigned(FOnGetDefaultLocalizer) then
         FMainProg.DefaultLocalizer:=FOnGetDefaultLocalizer();

   except
      on e: ECompileError do
         ;
      on e: ECompileException do
         FMsgs.AddCompilerError(e.ScriptPos, e.Message);
      on e: Exception do
         FMsgs.AddCompilerError(cNullPos, e.Message);
   end;

   FMainProg.TimeStamp:=compileStartTime;
   FMainProg.CompileDuration:=Now-compileStartTime;
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
         if confUnits.IndexOfName(dep)<0 then
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
   unitsResolved : TIdwsUnitList;
   unitTable : TUnitSymbolTable;
   unitSymbol : TUnitMainSymbol;
begin
   unitsResolved:=ResolveUnitReferences;
   try
      // Get the symboltables of the units
      for i:=0 to unitsResolved.Count-1 do begin
         unitTable:=unitsResolved[i].GetUnitTable(FSystemTable, FMainProg.UnitMains, FOperators);
         unitSymbol:=TUnitMainSymbol.Create(unitsResolved[i].GetUnitName, unitTable, FMainProg.UnitMains);
         unitSymbol.ReferenceInSymbolTable(FProg.Table, True);
      end;
   finally
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
   unitMain : TUnitMainSymbol;
   dependencies : TStrings;
   unitSource : String;
   srcUnit : TSourceUnit;
   oldContext : TdwsSourceContext;
begin
   for i:=0 to FUnitsFromStack.Count-1 do
      if SameText(FUnitsFromStack.Items[i], unitName) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UnitCircularReference);

   Result:=TUnitSymbol(FProg.Table.FindLocal(unitName, TUnitSymbol));
   if (Result<>nil) and (Result.Main<>nil) then begin
      // ignore multiple requests (for now)
      Exit;
   end;

   i:=FUnits.IndexOfName(unitName);
   if i<0 then begin
      if Assigned(FOnNeedUnit) then
         unitResolved:=FOnNeedUnit(unitName, unitSource);
      if unitResolved<>nil then
         FUnits.Add(unitResolved)
      else begin
         if unitSource='' then
            unitSource:=GetScriptSource(unitName+'.pas');
         if unitSource<>'' then begin
            srcUnit:=TSourceUnit.Create(unitName, FProg.Root.RootTable, FProg.UnitMains);
            unitResolved:=srcUnit;
            FUnits.Add(unitResolved);
            oldContext:=FSourceContextMap.SuspendContext;
            SwitchTokenizerToUnit(srcUnit, unitSource);
            FSourceContextMap.ResumeContext(oldContext);
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

   unitMain:=FProg.UnitMains.Find(unitName);
   if unitMain=nil then begin
      unitTable:=nil;
      try
         unitTable:=unitResolved.GetUnitTable(FSystemTable, FProg.UnitMains, FOperators);
         unitMain:=TUnitMainSymbol.Create(unitName, unitTable, FProg.UnitMains);
      except
         unitTable.Free;
         raise;
      end;
   end;

   Result:=unitMain.ReferenceInSymbolTable(FProg.Table, False);
end;

// SetupInitializationFinalization
//
procedure TdwsCompiler.SetupInitializationFinalization;
var
   i : Integer;
   rankedUnits : array of TUnitMainSymbol;
   ums : TUnitMainSymbol;
begin
   // collect and rank all units with an initialization or finalization sections
   // NOTE: UnitMains order may change arbitrarily in the future, hence the need to reorder
   SetLength(rankedUnits, FProg.UnitMains.Count);
   for i:=0 to FProg.UnitMains.Count-1 do begin
      ums:=FProg.UnitMains[i];
      if (ums.InitializationExpr<>nil) or (ums.FinalizationExpr<>nil) then begin
         rankedUnits[i]:=ums;
      end;
   end;
   // append initializations to InitExpr of the main prog
   for i:=0 to High(rankedUnits) do begin
      ums:=rankedUnits[i];
      if (ums<>nil) and (ums.InitializationExpr<>nil) then begin
         FMainProg.InitExpr.AddStatement(ums.InitializationExpr as TBlockExprBase);
         ums.InitializationExpr.IncRefCount;
      end;
   end;
   // append initializations to FinalExpr of the main prog in reverse order
   for i:=High(rankedUnits) downto 0 do begin
      ums:=rankedUnits[i];
      if (ums<>nil) and (ums.FinalizationExpr<>nil) then begin
         if FMainProg.FinalExpr=nil then
            FMainProg.FinalExpr:=TBlockFinalExpr.Create(FMainProg, cNullPos);
         FMainProg.FinalExpr.AddStatement(ums.FinalizationExpr as TBlockExprBase);
         ums.FinalizationExpr.IncRefCount;
      end;
   end;
end;

// RecordSymbolUse
//
procedure TdwsCompiler.RecordSymbolUse(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
begin
   if coSymbolDictionary in Options then
      FSymbolDictionary.AddSymbol(sym, scriptPos, useTypes);
end;

// RecordSymbolUseReference
//
procedure TdwsCompiler.RecordSymbolUseReference(sym : TSymbol; const scriptPos : TScriptPos; isWrite : Boolean);
begin
   if isWrite then
      RecordSymbolUse(sym, scriptPos, [suReference, suWrite])
   else RecordSymbolUse(sym, scriptPos, [suReference, suRead]);
end;

// RecordSymbolUseImplicitReference
//
procedure TdwsCompiler.RecordSymbolUseImplicitReference(sym : TSymbol; const scriptPos : TScriptPos; isWrite : Boolean);
begin
   if isWrite then
      RecordSymbolUse(sym, scriptPos, [suReference, suWrite, suImplicit])
   else RecordSymbolUse(sym, scriptPos, [suReference, suRead, suImplicit]);
end;

// ReplaceSymbolUse
//
procedure TdwsCompiler.ReplaceSymbolUse(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
begin
   if coSymbolDictionary in Options then
      FSymbolDictionary.ReplaceSymbolAt(oldSym, newSym, scriptPos);
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

   FMainProg:=context.ProgramObject as TdwsMainProgram;
   FMainProg.Compiler:=Self;
   FSystemTable:=FMainProg.SystemTable.SymbolTable;
   FSourceContextMap:=FMainProg.SourceContextMap;
   FSymbolDictionary:=FMainProg.SymbolDictionary;

   FMsgs:=FMainProg.CompileMsgs;
   SetupMsgsOptions(aConf);

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
      FCurrentUnitSymbol:=nil;
      FUnitSection:=secMixed;

      // Start compilation
      FProg.Expr := ReadScript(sourceFile, stRecompile);

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
   action : TdwsStatementAction;
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
            saNone : begin
               if  not FTok.TestDelete(ttSEMI) then begin
                  if endTokens<>[] then begin
                     finalToken:=FTok.TestDeleteAny(endTokens);
                     if finalToken=ttNone then
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
                     Break;
                  end else begin
                     if FTok.HasTokens then
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
                  end;
               end;
            end;
            saImplementation : begin
               finalToken:=ttIMPLEMENTATION;
               Exit;
            end;
            saEnd : begin
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
   oldTok : TTokenizer;
   oldSection : TdwsUnitSection;
   finalToken : TTokenType;
   unitBlock : TBlockExpr;
   readingMain : Boolean;
   contextFix : TdwsSourceContext;
begin
   oldTok:=FTok;
   oldSection:=FUnitSection;
   FTok:=FTokRules.CreateTokenizer(FProg.CompileMsgs);
   try
      FTok.BeginSourceFile(sourceFile);
      if coContextMap in Options then begin
         case scriptType of
            stMain :
               FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttPROGRAM);
            stUnit :
               FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttUNIT);
         else
            FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttNone);
         end;
      end;
      FTok.SwitchHandler:=ReadSwitch;
      FTok.SwitchProcessor:=ReadInstrSwitch;
      if scriptType=stMain then
         FTok.ConditionalDefines:=FMainProg.ConditionalDefines
      else FTok.ConditionalDefines:=TAutoStrings.Create(TStringList.Create);

//!!!!      FMainProg.SourceList.Add(sourceFile.Name, sourceFile, scriptType);

      readingMain:=(scriptType=stMain);
      if readingMain and FTok.Test(ttUNIT) then begin
         if coContextMap in Options then begin
            // need to fix the context map
            // the convoluted code below is required in case the first code content encountered
            // was an include switch, worst case is a 'UNIT' keyword in a bunch of nested includes
            contextFix:=FSourceContextMap.Current;
            while     (contextFix<>nil)
                  and (contextFix.Token<>ttPROGRAM)
                  and (contextFix.StartPos.SourceFile<>sourceFile) do
               contextFix:=contextFix.Parent;
            if contextFix<>nil then
               contextFix.Token:=ttUNIT;
         end;
         HandleUnitDependencies;
         scriptType:=stUnit;
      end;

      if Assigned(FOnReadScript) then
         FOnReadScript(Self, sourceFile, scriptType);

      case scriptType of
         stMain : begin
            HandleUnitDependencies;
         end;
         stUnit : begin
            FUnitSection:=secHeader;
            ReadUnitHeader;
         end;
      end;

      Result:=ReadRootBlock([], finalToken);

      case scriptType of
         stUnit : begin
            if (finalToken=ttNone) or (finalToken=ttEND) then begin
               if coContextMap in Options then
                  FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
            end;
            FProg.InitExpr.AddStatement(Result);
            Result:=nil;
         end;
         stMain : begin
            if coContextMap in Options then begin
               if scriptType=stMain then
                  FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
            end;
         end;
      end;

      if FTok.ConditionalDepth.Count>0 then
         FMsgs.AddCompilerError(FTok.ConditionalDepth.Peek.ScriptPos, CPE_UnbalancedConditionalDirective);

      if finalToken=ttIMPLEMENTATION then begin
         if coSymbolDictionary in Options then
            RecordSymbolUse(CurrentUnitSymbol, FTok.HotPos, [suImplementation, suImplicit]);
         if readingMain then begin
            ReadImplementationBlock;
            unitBlock:=ReadRootBlock([], finalToken);
            FProg.InitExpr.AddStatement(unitBlock);
            FTok.Free;
         end else FUnitContextStack.PushContext(Self);
         FTok:=nil;
      end else begin
         FTok.Free;
         FTok:=nil;
      end;

      if scriptType=stMain then
         ReadScriptImplementations;

      if (Result<>nil) and Optimize then
         Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
   finally
      FTok.Free;
      FTok:=oldTok;
      FUnitSection:=oldSection;
   end;
end;

// ReadImplementationBlock
//
procedure TdwsCompiler.ReadImplementationBlock;
var
   finalToken : TTokenType;
   unitBlock : TBlockExpr;
   initializationBlock, finalizationBlock : TBlockExpr;
begin
   initializationBlock:=nil;
   finalizationBlock:=nil;
   unitBlock:=ReadRootBlock([ttINITIALIZATION, ttFINALIZATION], finalToken);
   try
      if finalToken=ttINITIALIZATION then begin
         FUnitSection:=secInitialization;
         if coContextMap in Options then
            FSourceContextMap.OpenContext(FTok.HotPos, FCurrentUnitSymbol, ttINITIALIZATION);
         initializationBlock:=ReadRootBlock([ttFINALIZATION, ttEND], finalToken);
         if coContextMap in Options then
            FSourceContextMap.CloseContext(FTok.HotPos);
      end;
      if finalToken=ttFINALIZATION then begin
         FUnitSection:=secFinalization;
         if coContextMap in Options then
            FSourceContextMap.OpenContext(FTok.HotPos, FCurrentUnitSymbol, ttFINALIZATION);
         finalizationBlock:=ReadRootBlock([ttEND], finalToken);
         if coContextMap in Options then
            FSourceContextMap.CloseContext(FTok.HotPos);
      end;

      if coContextMap in Options then
         FSourceContextMap.CloseAllContexts(FTok.CurrentPos);

      if unitBlock.SubExprCount>0 then begin
         FProg.InitExpr.AddStatement(unitBlock);
         unitBlock:=nil;
      end;

      if FCurrentUnitSymbol<>nil then begin
         if (initializationBlock<>nil) and (initializationBlock.SubExprCount>0) then begin
            FCurrentUnitSymbol.InitializationExpr:=initializationBlock;
            initializationBlock:=nil;
         end;
         if (finalizationBlock<>nil) and (finalizationBlock.SubExprCount>0) then begin
            FCurrentUnitSymbol.FinalizationExpr:=finalizationBlock;
            finalizationBlock:=nil;
         end;
      end;
   finally
      unitBlock.Free;
      initializationBlock.Free; // TODO!!!
      finalizationBlock.Free; // TODO!!!
   end;
end;

// ReadScriptImplementations
//
procedure TdwsCompiler.ReadScriptImplementations;
var
   implemTable : TUnitImplementationTable;
   oldUnit : TUnitMainSymbol;
begin
   while FUnitContextStack.Count>0 do begin
      FUnitContextStack.PopContext(Self, oldUnit);
      FUnitSection:=secImplementation;
      try
         implemTable:=TUnitImplementationTable.Create(CurrentUnitSymbol);
         FProg.EnterSubTable(implemTable);
         try
            ReadImplementationBlock;
         finally
            FProg.LeaveSubTable;
         end;
      finally
         FTok.Free;
         FTok:=nil;
         LeaveUnit(oldUnit);
      end;
   end;
end;

// ReadRootStatement
//
function TdwsCompiler.ReadRootStatement(var action : TdwsStatementAction) : TNoResultExpr;
var
   hotPos : TScriptPos;
   token : TTokenType;
begin
   action:=saNone;
   Result:=nil;

   FTok.TestName;
   hotPos:=FTok.HotPos;

   token:=FTok.TestDeleteAny([ttTYPE, ttPROCEDURE, ttFUNCTION,
                              ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttCLASS,
                              ttUSES, ttIMPLEMENTATION, ttEND]);
   case token of
      ttTYPE :
         if UnitSection in [secInterface, secImplementation] then begin
            ReadTypeDeclBlock;
            action:=saNoSemiColon
         end else ReadTypeDecl(True);
      ttPROCEDURE, ttFUNCTION, ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD :
         ReadProcBody(ReadProcDecl(token, hotPos));
      ttCLASS : begin
         token:=FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION, ttMETHOD]);
         case token of
            ttPROCEDURE, ttFUNCTION, ttMETHOD :
               ReadProcBody(ReadProcDecl(token, hotPos, [pdoClassMethod]));
         else
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
         end;
      end;
      ttUSES :
         ReadUses;
      ttIMPLEMENTATION : begin
         if (FProg.Table<>FProg.Root.Table) or (UnitSection<>secInterface) then begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnexpectedSection,
                                      [cTokenStrings[token]]);
            action:=saNoSemiColon;
         end else begin
            if coContextMap in FOptions then begin
               if coContextMap in Options then
                  FSourceContextMap.CloseContext(FTok.HotPos, ttINTERFACE);
               FSourceContextMap.OpenContext(FTok.HotPos, CurrentUnitSymbol, ttIMPLEMENTATION);
            end;
            FUnitSection:=secImplementation;
            DoSectionChanged;
            action:=saImplementation;
         end;
      end;
      ttEND : begin
         if (FProg.Table<>FProg.Root.Table) or (UnitSection<>secImplementation) then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnexpectedEnd,
                                      [cTokenStrings[token]])
         else begin
            if FTok.TestDelete(ttDOT) then begin
               FUnitSection:=secEnd;
               action:=saEnd;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_DotExpected);
         end;
      end;
   else
      Result:=ReadStatement(action);
   end;
end;

// ReadStatement
//
function TdwsCompiler.ReadStatement(var action : TdwsStatementAction) : TNoResultExpr;
var
   token : TTokenType;
begin
   Result:=nil;
   token:=FTok.TestDeleteAny([ttVAR, ttCONST, ttOPERATOR, ttRESOURCESTRING]);
   case token of
      ttVAR :
         Result:=ReadVarDecl(FStandardDataSymbolFactory);
      ttCONST :
         ReadConstDeclBlock(action);
      ttRESOURCESTRING :
         ReadResourceStringDeclBlock(action);
      ttOPERATOR :
         ReadOperatorDecl;
   else
      if (FProg.Level=0) and not (UnitSection in [secMixed, secInitialization, secFinalization]) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_UnexpectedStatement);
      Result:=ReadBlock
   end;
end;

// ReadResourceStringDecl
//
function TdwsCompiler.ReadResourceStringDecl : TResourceStringSymbol;
var
   name, buf : String;
   namePos : TScriptPos;
   expr : TTypedExpr;
begin
   if not FTok.TestDeleteNamePos(name, namePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   CheckName(name, namePos);
   CheckSpecialName(name);

   if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

   expr:=ReadExpr;
   try
      if (expr=nil) or (not expr.IsConstant) then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
         expr.Free;
         expr:=nil;
      end else if (expr.Typ=nil) or not expr.Typ.IsOfType(FProg.TypString) then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
         expr.Free;
         expr:=nil;
      end;
      // keep compiling
      if expr=nil then
         Result:=TResourceStringSymbol.Create(name, '')
      else begin
         expr.EvalAsString(FExec, buf);
         Result:=TResourceStringSymbol.Create(name, buf);
      end;
      FMainProg.ResourceStringList.Add(Result);
      RecordSymbolUse(Result, namePos, [suDeclaration]);
   finally
      expr.Free;
   end;
end;

// ReadResourceStringDeclBlock
//
procedure TdwsCompiler.ReadResourceStringDeclBlock(var action : TdwsStatementAction);
var
   resStringSym : TResourceStringSymbol;
begin
   action:=saNoSemiColon;
   repeat
      resStringSym:=ReadResourceStringDecl;
      FProg.Table.AddSymbol(resStringSym);
      ReadSemiColon;
   until not (    (UnitSection in [secInterface, secImplementation])
              and (FProg.Level=0)
              and FTok.TestName);
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
      compiler.FSourceContextMap:=compiler.FMainProg.SourceContextMap;
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
            compiler.FTok:=compiler.FTokRules.CreateTokenizer(compiler.FMsgs);
            try
               compiler.FTok.BeginSourceFile(sourceFile);
               try
                  expr:=compiler.ReadExpr;
               except
                  on E : Exception do begin
                     gotError:=True;
                  end;
               end;
               if compiler.FMsgs.HasErrors then begin
                  gotError:=True;
                  expr:=TConstExpr.Create(contextProgram,
                                          contextProgram.TypString,
                                          compiler.FMsgs.AsInfo);
               end;
            finally
               compiler.FTok.EndSourceFile;
               compiler.FTok.Free;
               compiler.FTok:=nil;
            end;
         finally
            sourceFile.Free;
            compiler.FProg.CompileMsgs:=oldProgMsgs;
            compiler.FMsgs.Free;
            compiler.FMsgs:=nil;
         end;
      finally
         compiler.FSymbolDictionary:=nil;
         compiler.FSourceContextMap:=nil;
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
function TdwsCompiler.ReadVarDecl(const dataSymbolFactory : IdwsDataSymbolFactory) : TNoResultExpr;
var
   x : Integer;
   names : TStringList;
   sym : TDataSymbol;
   typ : TTypeSymbol;
   hotPos : TScriptPos;
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
      ReadNameList(names, posArray);

      hotPos:=FTok.HotPos;

      if FTok.TestDelete(ttCOLON) then begin

         // explicit typing
         //    var myVar : type
         //    var myVar : type = expr
         //    var myVar : type := expr
         typ:=ReadType('', tcVariable);
         if names.Count=1 then begin
            if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then begin
               if (typ is TRecordSymbol) and FTok.Test(ttBLEFT) then begin
                  initExpr:=TConstExpr.Create(FProg, typ, ReadConstRecord(TRecordSymbol(typ)), 0);
               end else begin
                  initExpr:=dataSymbolFactory.ReadExpr(typ)
               end;
            end;
         end;

      end else if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then begin

         // inferred typing
         //    var myVar = expr
         //    var myVar := expr
         if names.Count<>1 then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
         initExpr:=dataSymbolFactory.ReadExpr(nil);
         typ:=initExpr.Typ;

         if typ=nil then begin
            FMsgs.AddCompilerError(hotPos, CPE_RightSideNeedsReturnType);
            initExpr.Free;
            initExpr:=nil;
         end else if typ=FProg.TypNil then
            if not (initExpr is TBogusConstExpr) then
               FMsgs.AddCompilerError(hotPos, CPE_TypeCouldNotBeInferenced);

      end else begin

         // keep going
         typ := FProg.TypVariant;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      end;

      // keep going in case of error
      if typ=nil then
         typ:=FProg.TypVariant
      else if (typ is TClassSymbol) and TClassSymbol(typ).IsStatic then
         FMsgs.AddCompilerErrorFmt(hotPos, CPE_ClassIsStatic, [typ.Name]);

      for x:=0 to names.Count-1 do begin
         sym:=dataSymbolFactory.CreateDataSymbol(names[x], posArray[x], typ);

         varExpr:=GetVarExpr(sym);
         if Assigned(initExpr) then begin

            // Initialize with an expression

            RecordSymbolUse(sym, posArray[x], [suDeclaration, suReference, suWrite]);

            Result:=CreateAssign(hotPos, ttASSIGN, varExpr, initExpr);
            initExpr:=nil;

         end else begin

            RecordSymbolUse(sym, posArray[x], [suDeclaration]);

            if sym.Typ is TArraySymbol then begin

               // TODO: if Sym.DynamicInit?
               FProg.InitExpr.AddStatement(
                  TInitDataExpr.Create(FProg, hotPos, varExpr));

            end else begin

               // Initialize with default value
               if (varExpr.Typ=FProg.TypInteger) or (varExpr.Typ is TEnumerationSymbol) then
                  assignExpr:=TAssignConstToIntegerVarExpr.CreateVal(FProg, hotPos, varExpr, 0)
               else if varExpr.Typ=FProg.TypFloat then
                  assignExpr:=TAssignConstToFloatVarExpr.CreateVal(FProg, hotPos, varExpr, 0)
               else if varExpr.Typ=FProg.TypBoolean then
                  assignExpr:=TAssignConstToBoolVarExpr.CreateVal(FProg, hotPos, varExpr, False)
               else if varExpr.Typ=FProg.TypString then
                  assignExpr:=TAssignConstToStringVarExpr.CreateVal(FProg, hotPos, varExpr, '')
               else if varExpr.Typ.ClassType=TClassSymbol then
                  assignExpr:=TAssignNilToVarExpr.CreateVal(FProg, hotPos, varExpr)
               else if varExpr.Typ.ClassType=TClassOfSymbol then
                  assignExpr:=TAssignNilClassToVarExpr.CreateVal(FProg, hotPos, varExpr)
               else if varExpr.Typ is TFuncSymbol then
                  assignExpr:=TAssignNilToVarExpr.CreateVal(FProg, hotPos, varExpr)
               else begin
                  initData := nil;
                  SetLength(initData, sym.Typ.Size);
                  TDataSymbol(sym).Typ.InitData(initData, 0);

                  constExpr:=TConstExpr.CreateTyped(FProg, sym.Typ, initData);
                  assignExpr:=TAssignConstDataToVarExpr.Create(FProg, hotPos, varExpr, constExpr);
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
procedure TdwsCompiler.ReadConstDecl(const factory : IdwsDataSymbolFactory);
var
   name : String;
   expr : TTypedExpr;
   dataExpr : TDataExpr;
   typ : TTypeSymbol;
   sas : TStaticArraySymbol;
   detachTyp : Boolean;
   constPos : TScriptPos;
   recordData : TData;
   constSym : TConstSymbol;
begin
   if not FTok.TestDeleteNamePos(name, constPos) then begin

      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   end else begin

      factory.CheckName(name, constPos);
      CheckSpecialName(name);

      if FTok.TestDelete(ttCOLON) then
         typ:=ReadType('', tcConstant)
      else typ:=nil;

      if typ is TFuncSymbol then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InvalidConstType, [typ.Caption]);

      if not FTok.TestDelete(ttEQ) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

      if typ is TRecordSymbol then begin

         recordData:=ReadConstRecord(TRecordSymbol(typ));
         constSym:=factory.CreateConstSymbol(name, constPos, typ, recordData, 0);

      end else begin

         detachTyp:=False;
         if typ is TArraySymbol then begin
            case FTok.TestDeleteAny([ttALEFT, ttBLEFT]) of
               ttALEFT : expr:=factory.ReadArrayConstantExpr(ttARIGHT, TArraySymbol(typ).Typ);
               ttBLEFT : expr:=factory.ReadArrayConstantExpr(ttBRIGHT, TArraySymbol(typ).Typ);
            else
               expr:=factory.ReadExpr(nil);
            end;
         end else expr:=factory.ReadExpr(nil);
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
               constSym:=factory.CreateConstSymbol(name, constPos, typ, nil, 0);

            end else begin

               if typ is TArraySymbol then begin
                  if typ is TStaticArraySymbol then
                     sas:=TStaticArraySymbol(typ)
                  else begin
                     sas:=TStaticArraySymbol.Create('', typ.Typ, FProg.TypInteger, 0, TArraySymbol(typ).typ.Size-1);
                     FProg.Table.AddSymbol(sas);
                  end;
                  if expr is TConstExpr then begin
                     constSym:=factory.CreateConstSymbol(name, constPos, sas, TConstExpr(expr).Data[FExec], TConstExpr(expr).Addr[FExec]);
                     detachTyp:=False;
                  end else constSym:=factory.CreateConstSymbol(name, constPos, sas, (expr as TArrayConstantExpr).EvalAsTData(FExec), 0);
               end else begin
                  if typ.Size=1 then begin
                     SetLength(recordData, 1);
                     expr.EvalAsVariant(FExec, recordData[0]);
                     constSym:=factory.CreateConstSymbol(name, constPos, typ, recordData, 0);
                  end else begin
                     dataExpr:=(expr as TDataExpr);
                     FExec.Stack.Push(FProg.DataSize);
                     try
                        constSym:=factory.CreateConstSymbol(name, constPos, typ,
                                                            dataExpr.Data[FExec],
                                                            dataExpr.Addr[FExec]);
                     finally
                        FExec.Stack.Pop(FProg.DataSize);
                     end;
                  end;
               end;

            end;

         finally
            if detachTyp then begin
               FProg.Table.AddSymbol(typ);
               expr.Typ:=nil;
            end;
            expr.Free;
         end;
      end;

      RecordSymbolUse(constSym, constPos, [suDeclaration]);
   end;
end;

// ReadConstDeclBlock
//
procedure TdwsCompiler.ReadConstDeclBlock(var action : TdwsStatementAction);
begin
   action:=saNoSemiColon;
   repeat
      ReadConstDecl(FStandardDataSymbolFactory);
      ReadSemiColon;
   until not (    (UnitSection in [secInterface, secImplementation])
              and (FProg.Level=0)
              and FTok.TestName);
end;

// ReadTypeDeclBlock
//
procedure TdwsCompiler.ReadTypeDeclBlock;
var
   token : TTokenType;
begin
   token:=ttTYPE;
   repeat
      if not ReadTypeDecl(token=ttTYPE) then Break;
      if not FTok.TestDelete(ttSEMI) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
      token:=FTok.TestAny([ttINTERFACE, ttIMPLEMENTATION,
                           ttTYPE, ttVAR, ttCONST, ttEND,
                           ttCLASS, ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR,
                           ttOPERATOR]);
   until (not FTok.HasTokens) or (token<>ttNone);
end;

// ReadTypeDecl
//
function TdwsCompiler.ReadTypeDecl(firstInBlock : Boolean) : Boolean;
var
   name : String;
   typNew, typOld : TTypeSymbol;
   typePos : TScriptPos;
   oldSymPos : TSymbolPosition; // Mark *where* the old declaration was
   typContext : TdwsSourceContext;
   attributesBag : ISymbolAttributesBag;
begin
   Result:=True;

   ReadAttributes;

   attributesBag:=BagPendingAttributes;

   if not FTok.TestDeleteNamePos(name, typePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   if not FTok.TestDelete(ttEQ) then begin
      if firstInBlock then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected)
      else begin
         FTok.SimulateNameToken(typePos, name);
         Exit(False);
      end;
   end;

   if FSystemTable.FindLocal(name)<>nil then
      FMsgs.AddCompilerErrorFmt(typePos, CPE_NameIsReserved, [name]);

   typOld:=FProg.Table.FindTypeSymbol(name, cvMagic);
   oldSymPos:=nil;
   if coSymbolDictionary in FOptions then begin
      if Assigned(typOld) then
         oldSymPos := FSymbolDictionary.FindSymbolUsage(typOld, suDeclaration);  // may be nil
   end;

   // Wrap whole type declarations in a context.
   if coContextMap in FOptions then begin
      FSourceContextMap.OpenContext(typePos, nil, ttNAME);
      typContext:=FSourceContextMap.Current;
   end else typContext:=nil;
   try

      typNew := ReadType(name, tcDeclaration);

      if typContext<>nil then
         typContext.ParentSym:=typNew;

      AttachBaggedAttributes(typNew, attributesBag);

      if typNew.Name<>'' then begin
         // typOld = typNew if a forwarded class declaration was overwritten
         if typOld <> typNew then begin
            CheckName(name, typePos);
            CheckSpecialName(name);
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
         RecordSymbolUse(typNew, typePos, [suDeclaration]);
      end;

   finally
      if coContextMap in FOptions then
         FSourceContextMap.CloseContext(FTok.CurrentPos);
   end;
end;

// ReadProcDecl
//
function TdwsCompiler.ReadProcDecl(funcToken : TTokenType; const hotPos : TScriptPos;
                                   declOptions : TdwsReadProcDeclOptions = [];
                                   expectedLambdaParams : TParamsSymbolTable = nil) : TFuncSymbol;
var
   funcKind : TFuncKind;
   name : String;
   sym : TSymbol;
   funcPos : TScriptPos;
   overloadFuncSym, existingFuncSym, forwardedSym : TFuncSymbol;
   forwardedSymPos : TSymbolPosition;
   sourceContext : TdwsSourceContext;
begin
   sym:=nil;

   funcKind:=cTokenToFuncKind[funcToken];
   funcPos:=hotPos;
   if funcToken=ttLAMBDA then
      Include(declOptions, pdoLambda);

   if not (pdoType in declOptions) then begin
      // Find existing symbol for function name (if any)
      if pdoAnonymous in declOptions then begin
         name:='';
      end else begin
         if not FTok.TestDeleteNamePos(name, funcPos) then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
            name:='';
         end;
         CheckSpecialName(name);
         sym:=FProg.Table.FindSymbol(name, cvMagic);
      end;
   end else begin
      name:='';
   end;

   // Open context. Closed in ReadProcBody.
   if coContextMap in Options then begin
      FSourceContextMap.OpenContext(hotPos, nil, funcToken);
      sourceContext:=FSourceContextMap.Current;
   end else sourceContext:=nil;

   // name is the name of composite type -> this is a method implementation
   if sym is TCompositeTypeSymbol then begin

      // Store reference to class in dictionary
      RecordSymbolUse(sym, funcPos, [suReference]);
      Result:=ReadMethodImpl(TCompositeTypeSymbol(sym), funcKind, pdoClassMethod in declOptions);

   end else begin

      // Read normal procedure/function declaration
      if (pdoClassMethod in declOptions) or (funcKind in [fkConstructor, fkDestructor, fkMethod]) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ImplClassNameExpected);

      forwardedSym:=nil;
      overloadFuncSym:=nil;
      if sym is TFuncSymbol then begin
         existingFuncSym:=TFuncSymbol(sym);
         if existingFuncSym.IsOverloaded then
            overloadFuncSym:=existingFuncSym;
         if     existingFuncSym.IsForwarded
            and (CurrentUnitSymbol.HasSymbol(sym) or FProg.Table.HasSymbol(sym)) then begin
            // There was already a (forward) declaration
            forwardedSym:=existingFuncSym;
         end;
      end;

      if (forwardedSym=nil) and (overloadFuncSym=nil) then
         CheckName(name, funcPos);

      if pdoType in declOptions then
         Result := TSourceFuncSymbol.Create('', funcKind, -1)
      else Result := TSourceFuncSymbol.Create(name, funcKind, FMainProg.NextStackLevel(FProg.Level));
      try
         // Don't add params to dictionary when function is forwarded. It is already declared.
         if forwardedSym<>nil then
            ReadParams(Result.HasParam, Result.AddParam, forwardedSym.Params, nil)
         else ReadParams(Result.HasParam, Result.AddParam, nil, expectedLambdaParams);

         if (funcToken<>ttLAMBDA) or FTok.Test(ttCOLON) then
            Result.Typ:=ReadFuncResultType(funcKind);

         if not (pdoAnonymous in declOptions) then begin
                
            if pdoType in declOptions then begin

               if FTok.TestDelete(ttOF) then begin
                  if FTok.TestDelete(ttOBJECT) then
                     FMsgs.AddCompilerHint(FTok.HotPos, CPH_OfObjectIsLegacy, hlPedantic)
                  else FMsgs.AddCompilerError(FTok.HotPos, CPE_OfObjectExpected);
               end;

            end else begin

               ReadSemiColon;

               if overloadFuncSym<>nil then
                  forwardedSym:=FuncPerfectMatchOverload(Result);

               // handle function overloading
               if FTok.TestDelete(ttOVERLOAD) then begin

                  if FuncHasConflictingOverload(Result, forwardedSym) then
                     FMsgs.AddCompilerErrorFmt(hotPos, CPE_MatchingOverload, [name]);

                  Result.IsOverloaded:=True;
                  ReadSemiColon;

               end else if overloadFuncSym<>nil then begin

                  forwardedSym:=FuncPerfectMatchOverload(Result);
                  if forwardedSym=nil then begin
                     // no match, possible name conflict or fogotten overload keyword
                     FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustExplicitOverloads, [name]);
                     // keep compiling, mark overloaded
                     Result.IsOverloaded:=True;
                  end;

               end;

               if Assigned(forwardedSym) then begin

                  // check forward symbol match

                  CompareFuncKinds(forwardedSym.Kind, Result.Kind);
                  CompareFuncSymbolParams(forwardedSym, Result);

               end else begin

                  // forward & external declarations

                  if FTok.TestDelete(ttEXTERNAL) then begin
                     Result.IsExternal:=True;
                     ReadExternalName(Result);
                  end;

                  if UnitSection=secInterface then begin
                     // default to forward in interface section, except for external funcs
                     if not Result.IsExternal then
                        Result.SetForwardedPos(funcPos);
                     if FTok.TestDelete(ttFORWARD) then begin
                        FMsgs.AddCompilerHint(FTok.HotPos, CPW_ForwardIsImplicit);
                        ReadSemiColon;
                     end;
                  end else begin
                     if FTok.TestDelete(ttFORWARD) then begin
                        if Result.IsExternal then
                           FMsgs.AddCompilerHint(FTok.HotPos, CPW_ForwardIsMeaningless);
                        Result.SetForwardedPos(funcPos);
                        ReadSemiColon;
                     end;
                  end;

               end;

               ReadDeprecatedFunc(Result);

               if Assigned(forwardedSym) then begin
                  // Get forwarded position in script. If compiled without symbols it will just return a nil
                  forwardedSymPos := FSymbolDictionary.FindSymbolUsage(forwardedSym, suDeclaration);  // may be nil

                  // Adapt dictionary entry to reflect that it was a forward
                  // If the record is in the SymbolDictionary (disabled dictionary would leave pointer nil)
                  if Assigned(forwardedSymPos) then
                     forwardedSymPos.SymbolUsages := [suForward];  // update old postion to reflect that the type was forwarded

                  Result.Free;
                  Result := forwardedSym;
                  Result.ClearIsForwarded;
               end else FProg.Table.AddSymbol(Result);

               if Result.IsForwarded or Result.IsExternal then
                  FTok.SimulateToken(ttSEMI, FTok.HotPos);
            end;
            
         end;

         // Procedure is both Declared and Implemented here
         RecordSymbolUse(Result, funcPos, [suDeclaration, suImplementation]);
      except
         Result.Free;
         raise;
      end;
   end;

   if sourceContext<>nil then
      sourceContext.ParentSym:=Result;
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
      MemberSymbolWithNameAlreadyExists(sym, methPos);

   // Read declaration of method
   Result:=TSourceMethodSymbol.Create(name, funcKind, intfSym, cvPublished, False);
   Result.DeclarationPos:=methPos;

   try
      ReadParams(Result.HasParam, Result.AddParam, nil, nil);

      Result.Typ:=ReadFuncResultType(funcKind);
      ReadSemiColon;

      // Added as last step. OnExcept, won't need to be freed.
      RecordSymbolUse(Result, methPos, [suDeclaration]);
   except
      Result.Free;
      raise;
   end;
end;

// ReadMethodDecl
//
function TdwsCompiler.ReadMethodDecl(const hotPos : TScriptPos; ownerSym : TCompositeTypeSymbol; funcKind: TFuncKind;
                                     aVisibility : TdwsVisibility; isClassMethod: Boolean) : TMethodSymbol;

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
         if    (not newParam.Typ.IsOfType(oldParam.Typ))
            or (not UnicodeSameText(newParam.Name, oldParam.Name)) then
            Exit(False);
      end;
      Result:=True;
   end;

var
   name : String;
   sym : TSymbol;
   meth, defaultConstructor, match : TMethodSymbol;
   isReintroduced : Boolean;
   methPos: TScriptPos;
   qualifier : TTokenType;
   funcResult : TSourceMethodSymbol;
   bodyToken : TTokenType;
begin
   // Find Symbol for Functionname
   if not FTok.TestDeleteNamePos(name, methPos) then begin
      methPos:=FTok.HotPos;
      FMsgs.AddCompilerError(methPos, CPE_NameExpected);
      name:='';
   end;

   // Check if name is already used
   sym:=ownerSym.Members.FindSymbolFromScope(name, ownerSym);
   if (sym<>nil) then begin
      if sym is TMethodSymbol then begin
         meth:=TMethodSymbol(sym);
      end else begin
         meth:=nil;
         if ownerSym.Members.HasSymbol(sym) then
            MemberSymbolWithNameAlreadyExists(sym, methPos);
      end;
   end else meth:=nil;

   if ownerSym.IsStatic and (not IsClassMethod) then
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ClassIsStatic, [ownerSym.Name]);

   // Read declaration of method implementation
   funcResult:=TSourceMethodSymbol.Create(name, funcKind, ownerSym, aVisibility, isClassMethod);
   funcResult.DeclarationPos:=methPos;
   try
      ReadParams(funcResult.HasParam, funcResult.AddParam, nil, nil);

      funcResult.Typ:=ReadFuncResultType(funcKind);
      ReadSemiColon;

      if meth<>nil then
         isReintroduced:=meth.IsVirtual
      else isReintroduced:=False;

      if FTok.TestDelete(ttREINTRODUCE) then begin
         if not isReintroduced then
            FMsgs.AddCompilerErrorFmt(methPos, CPE_CantReintroduce, [name]);
         isReintroduced:=False;
         ReadSemiColon;
      end;

      // handle method overloading
      if FTok.TestDelete(ttOVERLOAD) then begin

         if MethHasConflictingOverload(funcResult) then
            FMsgs.AddCompilerErrorFmt(hotPos, CPE_MatchingOverload, [name]);

         funcResult.IsOverloaded:=True;
         ReadSemiColon;

      end else begin

         funcResult.SetOverlap(meth);
         if meth<>nil then begin
            if MethPerfectMatchOverload(funcResult, False)<>nil then
               MemberSymbolWithNameAlreadyExists(sym, Ftok.HotPos)
            else if meth.StructSymbol=ownerSym then begin
               if FTok.Test(ttOVERRIDE) then begin
                  // this could actually be an override of an inherited method
                  // and not just an overload of a local method
                  // in that case 'overload' is optional
                  match:=MethPerfectMatchOverload(funcResult, True);
                  if match<>nil then
                     funcResult.IsOverloaded:=True;
               end;
               if not funcResult.IsOverloaded then begin
                  // name conflict or fogotten overload keyword
                  FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustExplicitOverloads, [name]);
                  // keep compiling, mark overloaded
                  funcResult.IsOverloaded:=True;
               end;
            end;
         end;

      end;

      if funcResult.IsOverloaded then
         isReintroduced:=False;

      if ownerSym.AllowVirtualMembers then begin
         qualifier:=FTok.TestDeleteAny([ttVIRTUAL, ttOVERRIDE, ttABSTRACT]);
         if qualifier<>ttNone then begin
            case qualifier of
               ttVIRTUAL : begin
                  if aVisibility=cvPrivate then
                     FMsgs.AddCompilerHint(FTok.HotPos, CPH_PrivateVirtualMethodCantBeOverridden);
                  funcResult.IsVirtual:=True;
                  ReadSemiColon;
                  if FTok.TestDelete(ttABSTRACT) then begin
                     funcResult.IsAbstract:=True;
                     ReadSemiColon;
                  end;
               end;
               ttOVERRIDE : begin
                  if funcResult.IsOverloaded then
                     meth:=MethPerfectMatchOverload(funcResult, True);
                  if meth=nil then
                     FMsgs.AddCompilerErrorFmt(methPos, CPE_CantOverrideNotInherited, [name])
                  else if not meth.IsVirtual then
                     FMsgs.AddCompilerErrorFmt(methPos, CPE_CantOverrideNotVirtual, [name])
                  else begin
                     if funcResult.Kind<>meth.Kind then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_CantOverrideWrongFuncKind,
                                                  [cFuncKindToString[meth.Kind],
                                                   cFuncKindToString[funcResult.Kind]])
                     else if funcResult.IsClassMethod<>meth.IsClassMethod then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_CantOverrideWrongMethodType)
                     else if not funcResult.Typ.IsOfType(meth.Typ) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_CantOverrideWrongResultType)
                     else if not OverrideParamsCheck(funcResult, meth) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_CantOverrideWrongParameterList)
                     else if meth.IsFinal then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_CantOverrideFinal, [name]);
                     funcResult.SetOverride(meth);
                     isReintroduced := False;
                  end;
                  ReadSemiColon;
               end;
               ttABSTRACT : begin
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_NonVirtualAbstract);
                  ReadSemiColon;
               end;
            end;
         end;
      end;

      if FTok.TestDelete(ttDEFAULT) then begin
         if funcKind<>fkConstructor then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NonConstructorDefault)
         else begin
            defaultConstructor:=ownerSym.FindDefaultConstructor(cvMagic);
            if (defaultConstructor<>nil) and (defaultConstructor.StructSymbol=ownerSym) then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_DefaultConstructorAlreadyDefined,
                                         [ownerSym.Name, defaultConstructor.Name])
            else funcResult.IsDefault:=True;
         end;
         ReadSemiColon;
      end;

      if ownerSym.AllowVirtualMembers then begin
         if FTok.TestDelete(ttFINAL) then begin
            if not funcResult.IsOverride then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_CantFinalWithoutOverride)
            else funcResult.SetIsFinal;
            ReadSemiColon;
         end;
      end;

      if FTok.TestDelete(ttSTATIC) then begin
         if    funcResult.IsVirtual
            or (not funcResult.IsClassMethod)
            or (not (funcKind in [fkFunction, fkProcedure, fkMethod])) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_OnlyNonVirtualClassMethodsAsStatic)
         else if not funcResult.IsClassMethod then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_OnlyNonVirtualClassMethodsAsStatic)
         else funcResult.SetIsStatic;
         ReadSemiColon;
      end;

      if FTok.TestDelete(ttEXTERNAL) then begin
         if not ownerSym.IsExternal then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_StructureIsNotExternal, [funcResult.QualifiedName]);
         ReadExternalName(funcResult);
      end;

      ReadDeprecatedFunc(funcResult);

      if isReintroduced then
         FMsgs.AddCompilerWarningFmt(methPos, CPE_ReintroduceWarning, [name]);

      RecordSymbolUse(funcResult, methPos, [suDeclaration]);

   finally
      ownerSym.AddMethod(funcResult);
   end;

   bodyToken:=FTok.TestAny([ttBEGIN, ttREQUIRE, ttEMPTY]);
   case bodyToken of
      ttBEGIN, ttREQUIRE : begin
         // inline declaration
         if coContextMap in FOptions then
            FSourceContextMap.OpenContext(FTok.HotPos, funcResult, bodyToken);
         ReadProcBody(funcResult);
         ReadSemiColon;
      end;
      ttEMPTY : begin
         // empty body
         ReadProcEmpty(funcResult);
      end;
   end;

   Result:=funcResult;
end;

// ReadMethodImpl
//
function TdwsCompiler.ReadMethodImpl(ownerSym : TCompositeTypeSymbol;
               funcKind : TFuncKind; isClassMethod : Boolean) : TMethodSymbol;
var
   methName : String;
   sym : TSymbol;
   tmpMeth, overloadedMeth : TMethodSymbol;
   methPos : TScriptPos;
   declaredMethod, explicitParams : Boolean;
begin
   if not FTok.TestDelete(ttDOT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
   if not FTok.TestDeleteNamePos(methName, methPos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   FTok.Test(ttBLEFT);

   sym:=ownerSym.Members.FindSymbol(methName, cvPrivate);

   declaredMethod:=(sym is TMethodSymbol) and (TMethodSymbol(sym).StructSymbol=ownerSym);
   if declaredMethod then
      Result:=TMethodSymbol(sym)
   else begin
      // keep compiling
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplInvalidClass, [methName, ownerSym.Name]);
      Result:=TSourceMethodSymbol.Create(methName, funcKind, ownerSym, cvPublic, isClassMethod);
      ownerSym.AddMethod(Result);
   end;

   explicitParams:=not FTok.Test(ttSEMI);
   if declaredMethod then begin
      tmpMeth:=TSourceMethodSymbol.Create(methName, funcKind, ownerSym,
                                          TMethodSymbol(Result).Visibility, isClassMethod);
      try
         // Don't store these params to Dictionary. They will become invalid when the method is freed.
         if not FTok.TestDelete(ttSEMI) then begin
            ReadParams(tmpMeth.HasParam, tmpMeth.AddParam, Result.Params, nil);
            tmpMeth.Typ:=ReadFuncResultType(funcKind);
            ReadSemiColon;
         end;
         if Result.IsOverloaded then begin
            overloadedMeth:=MethPerfectMatchOverload(tmpMeth, False);
            if overloadedMeth=nil then
               FMsgs.AddCompilerErrorFmt(methPos, CPE_NoMatchingOverloadDeclaration, [tmpMeth.Name])
            else Result:=overloadedMeth;
         end else if explicitParams then
            CompareFuncSymbolParams(Result, tmpMeth);
      finally
         tmpMeth.Free;
      end;
   end else begin
      // keep compiling a method that wasn't declared in class
      if not FTok.TestDelete(ttSEMI) then begin
         ReadParams(Result.HasParam, Result.AddParam, nil, nil);
         Result.Typ:=ReadFuncResultType(funcKind);
         ReadSemiColon;
      end;
   end;

   if Result.IsClassMethod<>isClassMethod then begin
      if Result.IsClassMethod then
         FMsgs.AddCompilerError(methPos, CPE_ImplClassExpected)
      else FMsgs.AddCompilerError(methPos, CPE_ImplNotClassExpected);
   end;

   CompareFuncKinds(Result.Kind, funcKind);

   if Result.IsAbstract then
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplAbstract, [ownerSym.Name, methName]);

   RecordSymbolUse(Result, methPos, [suImplementation]);
end;

// ReadDeprecatedFunc
//
procedure TdwsCompiler.ReadDeprecatedFunc(funcSym : TFuncSymbol);
begin
   if FTok.TestDelete(ttDEPRECATED) then begin
      if FTok.Test(ttStrVal) then begin
         funcSym.DeprecatedMessage:=FTok.GetToken.FString;
         FTok.KillToken;
      end;
      if funcSym.DeprecatedMessage='' then
         funcSym.DeprecatedMessage:=MSG_DeprecatedEmptyMsg;
      ReadSemiColon;
   end;
end;

// WarnDeprecatedFunc
//
procedure TdwsCompiler.WarnDeprecatedFunc(funcExpr : TFuncExprBase);
var
   funcSym : TFuncSymbol;
begin
   funcSym:=funcExpr.FuncSym;
   if funcSym.IsDeprecated then begin
      if funcSym.DeprecatedMessage<>MSG_DeprecatedEmptyMsg then
         FMsgs.AddCompilerWarningFmt(funcExpr.ScriptPos, CPW_DeprecatedWithMessage,
                                     [funcSym.Name, funcSym.DeprecatedMessage])
      else FMsgs.AddCompilerWarningFmt(funcExpr.ScriptPos, CPW_Deprecated, [funcSym.Name]);
   end;
end;

// WarnDeprecatedProp
//
procedure TdwsCompiler.WarnDeprecatedProp(propSym : TPropertySymbol);
begin
   if propSym.DeprecatedMessage<>MSG_DeprecatedEmptyMsg then
      FMsgs.AddCompilerWarningFmt(FTok.HotPos, CPW_DeprecatedWithMessage,
                                  [propSym.Name, propSym.DeprecatedMessage])
   else FMsgs.AddCompilerWarningFmt(FTok.HotPos, CPW_Deprecated, [propSym.Name]);
end;

// ReadProcBody
//
procedure TdwsCompiler.ReadProcBody(funcSymbol : TFuncSymbol);
var
   oldprog : TdwsProgram;

   proc : TdwsProcedure;
   assignExpr : TNoResultExpr;
   tt, sectionType, finalToken : TTokenType;
   hotPos : TScriptPos;
   progExpr : TBlockExpr;
begin
   // Stop if declaration was forwarded or external
   if (funcSymbol.IsForwarded or funcSymbol.IsExternal) then begin
      // Closed context of procedure (was only a forward)
      if coContextMap in FOptions then
         FSourceContextMap.CloseContext(FTok.HotPos);
      Exit;
   end;

   if funcSymbol.Executable<>nil then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MethodRedefined, [funcSymbol.Name]);

   if UnitSection=secInterface then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_UnexpectedImplementationInInterface);

   // Open context of full procedure body (may include a 'var' section)
   if coContextMap in FOptions then
      FSourceContextMap.OpenContext(FTok.CurrentPos, funcSymbol, ttBEGIN);   // attach to symbol that it belongs to (perhaps a class)

   funcSymbol.SourcePosition:=FTok.HotPos;

   try
      // Function Body
      oldprog:=FProg;
      proc:=TdwsProcedure.Create(FProg);
      FProg:=proc;
      try
         FMainProg.Compiler := Self;
         proc.AssignTo(funcSymbol);
         // Set the current context's LocalTable to be the table of the new procedure
         if coContextMap in FOptions then
            FSourceContextMap.Current.LocalTable:=FProg.Table;

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
         tt:=FTok.TestAny([ttVAR, ttCONST, ttPROCEDURE, ttFUNCTION]);
         if    (tt in [ttVAR, ttCONST])
            or ((UnitSection=secImplementation) and (tt in [ttPROCEDURE, ttFUNCTION])) then begin
            // Read names of local variable and constants
            sectionType:=ttNone;
            repeat

               tt:=FTok.TestAny([ttVAR, ttCONST, ttPROCEDURE, ttFUNCTION]);
               case tt of
                  ttVAR, ttCONST : begin
                     sectionType:=tt;
                     FTok.KillToken;
                  end;
                  ttPROCEDURE, ttFUNCTION : begin
                     if UnitSection=secImplementation then begin
                        hotPos:=FTok.HotPos;
                        FTok.KillToken;
                        ReadProcBody(ReadProcDecl(tt, hotPos));
                        sectionType:=ttNone;
                        ReadSemiColon;
                        continue;
                     end else Break;
                  end;
                  ttBEGIN :
                     Break;
               end;

               case sectionType of
                  ttVAR : begin
                     assignExpr:=ReadVarDecl(FStandardDataSymbolFactory);
                     if assignExpr<>nil then
                        FProg.InitExpr.AddStatement(assignExpr);
                  end;
                  ttCONST : begin
                     ReadConstDecl(FStandardDataSymbolFactory);
                  end;
               else
                  Break;
               end;

               ReadSemiColon;

            until FTok.Test(ttBEGIN);
         end;

         if coContextMap in FOptions then
            FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttBEGIN);
         try
            // Read procedure body
            if funcSymbol.Kind<>fkLambda then begin
               if not FTok.TestDelete(ttBEGIN) then begin
                  if FTok.Test(ttFORWARD) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_FuncForwardAlreadyExists)
                  else FMsgs.AddCompilerStop(FTok.HotPos, CPE_BeginExpected);
               end;
            end;

            proc.SetBeginPos(FTok.HotPos);

            progExpr:=ReadRootBlock([ttEND, ttENSURE], finalToken);
            if Optimize then begin
               proc.OptimizeConstAssignments(progExpr);
               FProg.Expr:=progExpr.OptimizeToNoResultExpr(FProg, FExec);
            end else FProg.Expr:=progExpr;

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
            HintReferenceConstVarParams(proc.Func);

         finally
            if coContextMap in FOptions then
               FSourceContextMap.CloseContext(FTok.CurrentPos);  // close with inside procedure end
         end;
      finally
         FMainProg.Compiler := nil;
         FProg := oldprog;
      end;
   finally
      // Closed procedure body and procedure implementation (from declaration to body)
      if coContextMap in FOptions then begin
         FSourceContextMap.CloseContext(FTok.CurrentPos);  // closed begin..end body (may include 'var' section)
         FSourceContextMap.CloseContext(FTok.CurrentPos);  // closed from declaration through implementation
      end;
   end;
end;

// ReadProcEmpty
//
procedure TdwsCompiler.ReadProcEmpty(funcSymbol : TFuncSymbol);
var
   proc : TdwsProcedure;
begin
   FTok.KillToken;

   funcSymbol.SourcePosition:=FTok.HotPos;

   proc:=TdwsProcedure.Create(FProg);
   proc.SetBeginPos(funcSymbol.SourcePosition);
   proc.AssignTo(funcSymbol);
   proc.Expr:=TNullExpr.Create(proc, funcSymbol.SourcePosition);

   ReadSemiColon;
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
   endToken : TTokenType;
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
            testExpr:=testExpr.OptimizeToTypedExpr(FProg, FExec, hotPos);
         if testExpr.IsConstant then
            FMsgs.AddCompilerWarning(hotPos, CPW_ConstantCondition);

         testLength:=(NativeUInt(FTok.PosPtr)-NativeUInt(testStart)) div SizeOf(Char);
         if FTok.TestDelete(ttCOLON) then begin
            msgExpr:=ReadExpr;
            if not msgExpr.IsOfType(FProg.TypString) then
               FMsgs.AddCompilerError(hotPos, CPE_StringExpected);
            if Optimize then
               msgExpr:=msgExpr.OptimizeToTypedExpr(FProg, FExec, hotPos);
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

      endToken:=FTok.TestAny([ttVAR, ttCONST, ttBEGIN, ttEND, ttENSURE, ttREQUIRE,
                              ttFUNCTION, ttPROCEDURE, ttTYPE])
   until endToken<>ttNone;
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

type
   TFindOverloadedFunc = class
      OpSymbol : TOperatorSymbol;
      CapturableUsesSym : TFuncSymbol;
      function Callback(symbol : TSymbol) : Boolean;
   end;

function TFindOverloadedFunc.Callback(symbol : TSymbol) : Boolean;
var
   funcSym : TFuncSymbol;
begin
   Result:=False;
   if (symbol is TFuncSymbol) and (not symbol.IsType) then begin
      funcSym:=TFuncSymbol(symbol);
      if     (funcSym.Params.Count=2) and (funcSym.Typ<>nil)
         and funcSym.Typ.IsOfType(opSymbol.Typ)
         and funcSym.Params[0].Typ.IsOfType(opSymbol.Params[0])
         and funcSym.Params[1].Typ.IsOfType(opSymbol.Params[1]) then begin
         CapturableUsesSym:=funcSym;
         Result:=True;
      end;
   end;
end;

// ReadOperatorDecl
//
function TdwsCompiler.ReadOperatorDecl : TOperatorSymbol;

   procedure FindOverloadedFunc(var usesSym : TFuncSymbol; const usesName : String;
                                fromTable : TSymbolTable; opSymbol : TOperatorSymbol);
   var
      finder : TFindOverloadedFunc;
   begin
      finder:=TFindOverloadedFunc.Create;
      try
         finder.CapturableUsesSym:=usesSym;
         finder.OpSymbol:=opSymbol;
         fromTable.EnumerateSymbolsOfNameInScope(usesName, finder.Callback);
         usesSym:=finder.CapturableUsesSym;
      finally
         finder.Free;
      end;
   end;

var
   tt : TTokenType;
   usesName : String;
   opPos, usesPos : TScriptPos;
   sym : TTypeSymbol;
   usesSym : TFuncSymbol;
   fromTable : TSymbolTable;
   typ : TTypeSymbol;
begin
   opPos:=FTok.HotPos;
   tt:=FTok.TestDeleteAny([ttPLUS, ttMINUS, ttTIMES, ttDIVIDE, ttMOD, ttDIV,
                           ttOR, ttAND, ttXOR, ttIMPLIES, ttSHL, ttSHR, ttSAR,
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
      fromTable:=FProg.Table;
      if not FTok.TestDeleteNamePos(usesName, usesPos) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected)
      else begin
         sym:=fromTable.FindTypeSymbol(usesName, cvPublic);
         if sym is THelperSymbol then begin
            RecordSymbolUse(sym, usesPos, [suReference]);
            if not FTok.TestDelete(ttDOT) then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_DotExpected)
            else if not FTok.TestDeleteNamePos(usesName, usesPos) then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected)
            else begin
               fromTable:=THelperSymbol(sym).Members;
               sym:=fromTable.FindTypeSymbol(usesName, cvPublic);
            end;
         end;
         if (sym=nil) or sym.IsType or not (sym is TFuncSymbol) then
            FMsgs.AddCompilerError(usesPos, CPE_FunctionMethodExpected)
         else usesSym:=TFuncSymbol(sym);
      end;

      if usesSym<>nil then begin

         if usesSym.IsOverloaded then
            FindOverloadedFunc(usesSym, usesName, fromTable, Result);

         RecordSymbolUse(usesSym, usesPos, [suReference]);

         if (usesSym.typ=nil) or not usesSym.Typ.IsOfType(Result.Typ) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadResultType, [Result.Typ.Caption])
         else if usesSym.Params.Count<>2 then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadNumberOfParameters, [2, usesSym.Params.Count])
         else if not usesSym.Params[0].Typ.IsOfType(Result.Params[0]) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType, [0, Result.Params[0].Caption, usesSym.Params[0].Typ.Caption])
         else if usesSym.Params[0].ClassType=TVarParamSymbol then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [0])
         else if not usesSym.Params[1].Typ.IsOfType(Result.Params[1]) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType, [1, Result.Params[1].Caption, usesSym.Params[1].Typ.Caption])
         else if usesSym.Params[1].ClassType=TVarParamSymbol then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [1])
         else Result.UsesSym:=usesSym;
      end;
   except
      Result.Free;
      raise;
   end;

   FProg.Table.AddSymbol(Result);
   if (Result.Token<>ttNone) and (Result.UsesSym<>nil) then begin
      if FProg.Table<>FProg.Root.RootTable then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_OverloadOnlyInGlobalScope)
      else begin
         if FProg.Table.HasSameLocalOperator(Result) then
            FMsgs.AddCompilerError(opPos, CPE_OverloadAlreadyExists);
      end;
   end;
end;

// ReadBlocks
//
function TdwsCompiler.ReadBlocks(const endTokens : TTokenTypes; var finalToken : TTokenType) : TNoResultExpr;
var
   stmt : TNoResultExpr;
   token : TToken;
   closePos : TScriptPos; // Position at which the ending token was found (for context)
   blockExpr : TBlockExpr;
   sym : TSymbol;
   reach : TReachStatus;
   action : TdwsStatementAction;
begin
   // Read a block of instructions enclosed in "begin" and "end"
   reach:=rsReachable;
   blockExpr:=TBlockExpr.Create(FProg, FTok.HotPos);
   try
      if coContextMap in FOptions then begin
         FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttBEGIN);
         closePos:=FTok.CurrentPos;     // default to close context where it opened (used on errors)
      end;

      FProg.EnterSubTable(blockExpr.Table);
      try

         repeat

            if not FTok.HasTokens then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndOfBlockExpected);

            if FTok.GetToken.FTyp in EndTokens then begin
               finalToken:=FTok.GetToken.FTyp;
               closePos:=FTok.GetToken.FScriptPos;    // get start position of ending token
               FTok.KillToken;
               Break;
            end;

            if reach=rsUnReachable then begin
               reach:=rsUnReachableWarned;
               FMsgs.AddCompilerWarning(FTok.CurrentPos, CPW_UnReachableCode);
            end;

            action:=saNone;
            stmt:=ReadStatement(action);

            if Assigned(stmt) then begin
               blockExpr.AddStatement(stmt);

               if stmt.InterruptsFlow then
                  reach:=rsUnReachable;
            end;

            case action of
               saNoSemiColon : ;
               saNone : begin
                  if not FTok.TestDelete(ttSEMI) then begin
                     token:=FTok.GetToken;
                     if (token=nil) or (not (token.FTyp in EndTokens)) then
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
                  end;
               end;
            else
               Assert(False);
            end;

         until False;

         HintUnusedSymbols;
      finally
         FProg.LeaveSubTable;
      end;

      if Optimize then
         Result:=blockExpr.OptimizeToNoResultExpr(FProg, FExec)
      else Result:=blockExpr;

      if coContextMap in FOptions then begin
         if Result is TBlockExpr then
            FSourceContextMap.Current.LocalTable:=TBlockExpr(Result).Table;
         FSourceContextMap.CloseContext(closePos);
      end;

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
   msgsCount : Integer;
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
      if (FTok.TestAny([ttBLEFT, ttINHERITED, ttNEW])<>ttNone) or FTok.TestName then begin // !! TestName must be the last !!
         hotPos:=FTok.HotPos;
         msgsCount:=FMsgs.Count;
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
                  locExpr.Free;
                  locExpr:=nil;
                  ReadExpr.Free; // keep compiling
                  Result:=nil;
               end else begin
                  if not TDataExpr(locExpr).IsWritable then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_CantWriteToLeftSide);
                  if locExpr is TVarExpr then
                     WarnForVarUsage(TVarExpr(locExpr), hotPos);
                  Result := ReadAssign(token, TDataExpr(locExpr));
               end;
            end else begin
               if (locExpr is TDataExpr) and (locExpr.Typ is TFuncSymbol) then
                  locExpr:=ReadFunc(TFuncSymbol(locExpr.Typ), locExpr as TDataExpr);

               if locExpr is TAssignExpr then
                  Result:=TAssignExpr(locExpr)
               else if    (locExpr is TFuncExprBase)
                       or (locExpr is TConnectorCallExpr) then begin
                  Result:=TNoResultWrapperExpr.Create(FProg, (locExpr as  TPosDataExpr).Pos, TPosDataExpr(locExpr));
                  if locExpr.IsConstant then begin
                     if FMsgs.Count=msgsCount then   // avoid hint on calls with issues
                        FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
                  end;
               end else if locExpr is TConnectorWriteExpr then
                  Result:=TNoResultWrapperExpr.Create(FProg, locExpr.ScriptPos, TConnectorWriteExpr(locExpr))
               else if locExpr is TDynamicArraySetExpr then
                  Result:=TDynamicArraySetExpr(locExpr)
               else if locExpr is TStringArraySetExpr then
                  Result:=TStringArraySetExpr(locExpr)
               else if locExpr is TArrayPseudoMethodExpr then
                  Result:=TArrayPseudoMethodExpr(locExpr)
               else if locExpr is TConstExpr then begin
                  locExpr.Free;
                  locExpr:=nil;
                  Result:=TNullExpr.Create(FProg, hotPos);
                  if FMsgs.Count=msgsCount then   // avoid hint on expression with issues
                     FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
               end else if locExpr is TNullExpr then begin
                  Result:=TNullExpr(locExpr);
                  locExpr:=nil;
               end else if locExpr is TNoResultPosExpr then begin
                  Result:=TNoResultPosExpr(locExpr);
                  locExpr:=nil;
               end else begin
                  Result:=nil;
                  FMsgs.AddCompilerStop(hotPos, CPE_InvalidInstruction)
               end;
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
   name : String;
   namePos : TScriptPos;
   sym : TSymbol;
   methSym : TMethodSymbol;
   compositeSym, parentSym : TCompositeTypeSymbol;
   varExpr : TDataExpr;
   argPosArray : TScriptPosArray;
begin
   Result := nil;
   if not ((FProg is TdwsProcedure) and (TdwsProcedure(FProg).Func is TMethodSymbol)) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedOnlyAllowedInMethods);

   methSym := TMethodSymbol(TdwsProcedure(FProg).Func);

   if not FTok.TestDeleteNamePos(name, namePos) then begin

      sym := methSym.ParentMeth;
      if not methSym.IsOverride then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);

   end else begin

      compositeSym := methSym.StructSymbol;
      if compositeSym.ClassType=THelperSymbol then begin
         sym:=THelperSymbol(compositeSym).ForType.UnAliasedType;
         if sym is TArraySymbol then begin
            if sym is TDynamicArraySymbol then
               varExpr:=GetVarExpr(methSym.SelfSym)
            else varExpr:=GetConstParamExpr(methSym.SelfSym as TConstParamSymbol);
            Result:=ReadArrayMethod(name, namePos, varExpr);
            Exit;
         end;
         if sym is TCompositeTypeSymbol then
            parentSym:=TCompositeTypeSymbol(sym)
         else parentSym:=nil;
      end else parentSym:=compositeSym.Parent;

      if parentSym<>nil then
         sym:=parentSym.Members.FindSymbol(name, cvPrivate)
      else sym:=nil;

   end;

   if Assigned(sym) then begin

      RecordSymbolUseReference(sym, FTok.HotPos, isWrite);

      if sym is TMethodSymbol then begin
         if TMethodSymbol(sym).IsAbstract then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_AbstractMethodUsage);
         varExpr := TVarExpr.CreateTyped(FProg, methSym.SelfSym);
         try
            Result:=GetMethodExpr(TMethodSymbol(sym), varExpr, rkObjRef, FTok.HotPos, True);
         except
            varExpr.Free;
            raise;
         end;
         try
            ReadFuncArgs(TFuncExpr(Result), argPosArray, nil);
            if TMethodSymbol(sym).Kind = fkConstructor then
               (Result as TMethodExpr).Typ := (methSym.StructSymbol as TClassSymbol).Parent;
            TypeCheckArgs(TFuncExpr(Result), argPosArray);
         except
            Result.Free;
            raise;
         end;
      end else if sym is TPropertySymbol then begin
         varExpr := TVarExpr.CreateTyped(FProg, methSym.SelfSym);
         try
            Result := ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
         except
            varExpr.Free;
            raise;
         end;
      end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);
   end else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InheritedMethodNotFound, [Name]);
end;

// ResolveUnitNameSpace
//
function TdwsCompiler.ResolveUnitNameSpace(unitPrefix : TUnitSymbol) : TUnitSymbol;
var
   dottedName, nextDottedName : String;
begin
   if not FTok.Test(ttDOT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);

   dottedName:=unitPrefix.Name;
   while FTok.TestDelete(ttDOT) do begin
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      nextDottedName:=dottedName+'.'+FTok.GetToken.FString;
      if not unitPrefix.PossibleNameSpace(nextDottedName) then Break;
      dottedName:=nextDottedName;
      FTok.KillToken;
   end;

   Result:=unitPrefix.FindNameSpaceUnit(dottedName);
   if Result=nil then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownName, [dottedName]);
end;


// ReadName
//
function TdwsCompiler.ReadName(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TProgramExpr;
var
   sym : TSymbol;
   nameToken : TToken;
   namePos : TScriptPos;
   varExpr : TDataExpr;
   fieldExpr : TTypedExpr;
   propExpr, castExpr : TProgramExpr;
   funcExpr : TTypedExpr;
   progMeth : TMethodSymbol;
   selfSym : TDataSymbol;
   baseType : TTypeSymbol;
   sk : TSpecialKeywordKind;
   symClassType : TClass;
begin
   if (FSourcePostConditionsIndex<>0) and FTok.TestDelete(ttOLD) then
      Exit(ReadNameOld(isWrite));

   if FTok.TestDelete(ttNEW) then begin
      Result:=ReadNew(nil);
      Result:=ReadSymbol(Result, isWrite);
      Exit;
   end;

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
      Exit(ReadSymbol(ReadSpecialFunction(namePos, sk), isWrite, expecting));
   end;

   // Find name in symboltable
   sym:=FProg.Table.FindSymbol(nameToken.FString, cvPrivate);
   if not Assigned(sym) then begin
      if Assigned(FOnFindUnknownName) then
         sym:=FOnFindUnknownName(Self, nameToken.FString);
      if sym=nil then begin
         sym:=FProg.Table.FindSymbol(nameToken.FString, cvMagic);
         if sym=nil then
            FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownName, [nameToken.FString])
         else FMsgs.AddCompilerErrorFmt(namePos, CPE_MemberSymbolNotVisible, [nameToken.FString]);
      end;
   end;

   FTok.KillToken;

   // Add the symbol usage to Dictionary
   RecordSymbolUseReference(sym, namePos, isWrite);

   Result := nil;
   try
      baseType := sym.BaseType;

      if baseType<>nil then begin

         // Namespace prefix found
         if baseType.ClassType=TUnitSymbol then begin

            baseType:=ResolveUnitNameSpace(TUnitSymbol(baseType));

            namePos := FTok.HotPos;   // reuse token pos variable
            sym := TUnitSymbol(baseType).Table.FindLocal(FTok.GetToken.FString);

            if not Assigned(sym) then
               FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownNameDotName,
                                        [baseType.Name, FTok.GetToken.FString]);

            baseType:=sym.BaseType;

            FTok.KillToken;

            // Already added symbol usage of the unit. Now add for the unit's specified symbol.
            RecordSymbolUseReference(sym, namePos, isWrite);

         end;

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
      end else if symClassType=TResourceStringSymbol then begin
         Result:=ReadResourceStringName(TResourceStringSymbol(sym), namePos);
         Exit;
      end;

      if sym.InheritsFrom(TConstSymbol) then begin
         Result:=ReadConstName(TConstSymbol(sym), IsWrite);
         Exit;
      end;

      if sym.InheritsFrom(TDataSymbol) then begin

         Result:=ReadDataSymbolName(TDataSymbol(sym), FProg.Table, isWrite, expecting);

      end else if sym.ClassType=TExternalVarSymbol then begin

         Result := ReadSymbol(ReadExternalVar(TExternalVarSymbol(sym), IsWrite), IsWrite, expecting);
         Result := ReadSymbol(Result, IsWrite, expecting);

      // OOP related stuff

      end else if baseType is TStructuredTypeSymbol then begin

         if baseType.ClassType=TClassSymbol then begin

            Result:=ReadClassSymbolName(TClassSymbol(baseType), isWrite, expecting);

         end else if baseType.ClassType=TInterfaceSymbol then begin

            Result:=ReadInterfaceSymbolName(TInterfaceSymbol(baseType), isWrite, expecting);

         end else begin

            Assert(baseType.ClassType=TRecordSymbol);
            Result:=ReadRecordSymbolName(TRecordSymbol(baseType), isWrite, expecting);

         end;

      end else if sym.InheritsFrom(TFieldSymbol) then begin

         if TdwsProcedure(FProg).Func is TMethodSymbol then begin
            progMeth:=FProg.ContextMethodSymbol;
            selfSym:=progMeth.SelfSym;
         end else begin
            selfSym:=TDataSymbol(FProg.Table.FindSymbol(SYS_SELF, cvMagic, TDataSymbol));
         end;

         if (selfSym=nil) or (selfSym.Typ is TStructuredTypeMetaSymbol) then begin

            FMsgs.AddCompilerError(FTok.HotPos, CPE_ObjectReferenceExpected);
            fieldExpr:=TFieldExpr.Create(FProg, namePos, TFieldSymbol(sym), nil);

         end else begin

            fieldExpr:=ReadField(namePos, selfSym, TFieldSymbol(sym));

         end;
         Result:=ReadSymbol(fieldExpr, IsWrite, expecting);

      end else if sym.InheritsFrom(TPropertySymbol) then begin

         progMeth:=FProg.ContextMethodSymbol;
         selfSym:=progMeth.SelfSym;

         if selfSym=nil then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);

         if selfSym.ClassType=TVarParamSymbol then
            varExpr:=GetVarParamExpr(progMeth.SelfSym as TVarParamSymbol)
         else varExpr:=GetVarExpr(progMeth.SelfSym);
         try
            propExpr:=ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
         except
            varExpr.Free;
            raise;
         end;

         Result:=ReadSymbol(propExpr, IsWrite, expecting);

      // Methods
      end else if sym.InheritsFrom(TMethodSymbol) then begin

         if TMethodSymbol(sym).IsOverloaded then
            funcExpr:=ReadSelfMethOverloaded(TMethodSymbol(sym), isWrite, expecting)
         else funcExpr:=ReadSelfMethod(TMethodSymbol(sym), isWrite, expecting);
         Result:=ReadSymbol(funcExpr, IsWrite, expecting);

      // Functions/Procedures
      end else if sym.InheritsFrom(TFuncSymbol) then begin

         if TFuncSymbol(sym).IsOverloaded then
            funcExpr:=ReadFuncOverloaded(TFuncSymbol(sym), FProg.Table, nil, expecting)
         else funcExpr:=ReadFunc(TFuncSymbol(sym), nil, expecting);
         Result:=ReadSymbol(funcExpr, IsWrite, expecting);

      // Enumeration type cast or type symbol
      end else if sym.InheritsFrom(TEnumerationSymbol) then begin

         Result:=ReadEnumerationSymbolName(namePos, TEnumerationSymbol(sym), expecting=FAnyTypeSymbol)

      // helpers and generic type casts
      end else if sym.InheritsFrom(TTypeSymbol) then begin

         if FTok.TestDelete(ttBLEFT) then
            castExpr:=ReadTypeCast(namePos, TTypeSymbol(sym))
         else castExpr:=ReadTypeExpr(namePos, TTypeSymbol(sym), isWrite, expecting);
         Result:=ReadSymbol(castExpr, IsWrite, expecting);

      end else begin

         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownType, [sym.Name]);

      end;

   except
      Result.Free;
      raise;
   end;
end;

// ReadEnumerationSymbolName
//
function TdwsCompiler.ReadEnumerationSymbolName(const enumPos : TScriptPos; enumSym : TEnumerationSymbol;
                                                acceptTypeRef : Boolean) : TProgramExpr;
var
   name : String;
   elemPos : TScriptPos;
   elem : TSymbol;
   elemValue : Int64;
begin
   if FTok.TestDelete(ttBLEFT) then begin

      Result:=ReadTypeCast(elemPos, enumSym);

   end else if FTok.TestDelete(ttDOT) then begin

      if not FTok.TestDeleteNamePos(name, elemPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      elem:=enumSym.Elements.FindLocal(name);
      if elem=nil then begin
         FMsgs.AddCompilerErrorFmt(elemPos, CPE_UnknownNameDotName, [enumSym.Name, name]);
         elemValue:=0;
      end else elemValue:=TElementSymbol(elem).Value;

      Result:=TConstExpr.CreateIntegerValue(FProg, enumSym, elemValue);

   end else begin

      Result:=TTypeReferenceExpr.Create(enumSym, enumPos);
      if not acceptTypeRef then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackLeftExpected);

   end;
end;

// ReadClassSymbolName
//
function TdwsCompiler.ReadClassSymbolName(baseType : TClassSymbol; isWrite : Boolean;
                                          expecting : TTypeSymbol) : TProgramExpr;
var
   namePos : TScriptPos;
   constExpr : TTypedExpr;
   operandExpr, convExpr : TTypedExpr;
   castedExprTyp : TTypeSymbol;
begin
   if baseType.IsForwarded then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassNotCompletelyDefined, [baseType.Name]);

   if FTok.TestDelete(ttBLEFT) then begin
      // Cast
      FTok.TestName;
      namePos:=FTok.HotPos;
      operandExpr:=ReadExpr(expecting);
      try
         if baseType.IsExternal then begin
            convExpr:=TConvExternalExpr.Create(FProg, operandExpr);
            convExpr.Typ:=baseType;
         end else begin
            castedExprTyp:=operandExpr.Typ;
            if castedExprTyp<>FProg.TypNil then begin
               if    (not (castedExprTyp is TClassSymbol))
                  or (
                            (not TClassSymbol(castedExprTyp).IsOfType(baseType))
                        and (not baseType.IsOfType(castedExprTyp))
                     ) then begin
                  IncompatibleTypes(namePos, CPE_IncompatibleTypes, castedExprTyp, baseType);
               end;
            end;
            convExpr:=TObjAsClassExpr.Create(FProg, namePos, operandExpr, baseType);
         end;
      except
         operandExpr.Free;
         raise;
      end;
      if not (FTok.TestDelete(ttBRIGHT)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackRightExpected);
      Result:=ReadSymbol(convExpr, IsWrite, expecting);

   end else begin

      constExpr:=TConstExpr.CreateTypedVariantValue(FProg, baseType.MetaSymbol, Int64(baseType));
      Result:=ReadSymbol(constExpr, IsWrite, expecting);

   end;
end;

// ReadInterfaceSymbolName
//
function TdwsCompiler.ReadInterfaceSymbolName(baseType : TInterfaceSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   constExpr : TTypedExpr;
begin
   constExpr:=TConstExpr.CreateTypedVariantValue(FProg, baseType, Int64(baseType));
   Result:=ReadSymbol(constExpr, IsWrite, expecting);
end;

// ReadRecordSymbolName
//
function TdwsCompiler.ReadRecordSymbolName(baseType : TRecordSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   constExpr : TTypedExpr;
begin
   constExpr:=TConstExpr.CreateTypedVariantValue(FProg, baseType.MetaSymbol, Int64(baseType));
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

// ReadDataSymbolName
//
function TdwsCompiler.ReadDataSymbolName(dataSym : TDataSymbol; fromTable : TSymbolTable;
                                         isWrite: Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   funcSym : TFuncSymbol;
   varExpr : TVarExpr;
begin
   varExpr:=GetVarExpr(dataSym);
   if dataSym.Typ is TFuncSymbol then begin
      if     FTok.Test(ttASSIGN)
         or  (    (expecting<>nil)
              and dataSym.Typ.IsOfType(expecting)
              and not FTok.Test(ttBLEFT)) then
         Result:=varExpr
      else begin
         funcSym:=TFuncSymbol(dataSym.Typ);
         if funcSym.IsOverloaded then
            Result:=ReadFuncOverloaded(funcSym, fromTable, varExpr, expecting)
         else Result:=ReadFunc(funcSym, varExpr, expecting);
      end;
   end else Result:=varExpr;
   Result:=ReadSymbol(Result, isWrite, expecting);
end;

// ReadResourceStringName
//
function TdwsCompiler.ReadResourceStringName(resSym : TResourceStringSymbol; const namePos : TScriptPos) : TResourceStringExpr;
begin
   RecordSymbolUse(resSym, namePos, [suReference, suRead]);
   Result:=TResourceStringExpr.Create(FProg, namePos, resSym);
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
function TdwsCompiler.ReadField(const scriptPos : TScriptPos; selfSym : TDataSymbol;
                                fieldSym : TFieldSymbol) : TDataExpr;
var
   varExpr : TTypedExpr;
begin
   varExpr:=nil;
   Result:=ReadField(scriptPos, selfSym, fieldSym, varExpr);
end;

// ReadField
//
function TdwsCompiler.ReadField(const scriptPos : TScriptPos; selfSym : TDataSymbol;
                                fieldSym : TFieldSymbol; var varExpr : TTypedExpr) : TDataExpr;
begin
   if varExpr=nil then
      varExpr:=GetSelfParamExpr(selfSym);
   if fieldSym.StructSymbol.ClassType=TRecordSymbol then begin
      Result:=TRecordExpr.Create(FProg, scriptPos, (varExpr as TDataExpr), fieldSym)
   end else begin
      Result:=TFieldExpr.Create(FProg, FTok.HotPos, fieldSym, varExpr);
   end;
   varExpr:=nil;
end;

// ReadPropertyExpr
//
function TdwsCompiler.ReadPropertyExpr(var expr : TDataExpr; propertySym : TPropertySymbol;
                                       isWrite : Boolean) : TProgramExpr;
begin
   Result:=ReadPropertyExpr(TTypedExpr(expr), propertySym, isWrite);
end;

// ReadPropertyExpr
//
function TdwsCompiler.ReadPropertyExpr(var expr : TTypedExpr; propertySym : TPropertySymbol;
                                       isWrite : Boolean) : TProgramExpr;
begin
   if propertySym.IsDeprecated then
      WarnDeprecatedProp(propertySym);
   if isWrite then
      Result:=ReadPropertyWriteExpr(expr, propertySym)
   else Result:=ReadPropertyReadExpr(expr, propertySym);
end;

// ReadPropertyReadExpr
//
function TdwsCompiler.ReadPropertyReadExpr(var expr : TTypedExpr; propertySym : TPropertySymbol) : TTypedExpr;
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

      FMsgs.AddCompilerStop(aPos, CPE_WriteOnlyProperty)

   else if sym is TFieldSymbol then begin

      // ReadSym is a field
      if Expr.Typ is TStructuredTypeMetaSymbol then
         FMsgs.AddCompilerError(aPos, CPE_ObjectReferenceExpected);
      Result:=ReadField(aPos, nil, TFieldSymbol(sym), expr);

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

   end else if sym is TClassVarSymbol then begin

      expr.Free;
      expr:=nil;
      Result:=GetVarExpr(TClassVarSymbol(sym));

   end else if sym is TConstSymbol then begin

      expr.Free;
      expr:=nil;
      Result:=TConstExpr.CreateTyped(FProg, sym.Typ, TConstSymbol(sym));

   end else FMsgs.AddCompilerStop(aPos, CPE_WriteOnlyProperty);
end;

// ReadPropertyWriteExpr
//
function TdwsCompiler.ReadPropertyWriteExpr(var expr : TTypedExpr; propertySym : TPropertySymbol) : TProgramExpr;
var
   sym : TSymbol;
   aPos : TScriptPos;
   fieldExpr : TDataExpr;
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
            fieldExpr:=ReadField(aPos, nil, TFieldSymbol(sym), expr);
            Result:=ReadAssign(ttASSIGN, fieldExpr);

         end else if sym is TClassVarSymbol then begin

            // WriteSym is a class var
            expr.Free;
            expr:=nil;
            fieldExpr:=GetVarExpr(TClassVarSymbol(sym));
            Result:=ReadAssign(ttASSIGN, fieldExpr);

         end else if sym is TMethodSymbol then begin

            // WriteSym is a Method
            // Convert an assignment to a function call f := x  -->  f(x)
            Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, True);

         end else begin

            expr.Free;
            expr:=nil;
            Result:=TNullExpr.Create(FProg, aPos);
            FMsgs.AddCompilerError(aPos, CPE_ReadOnlyProperty)

         end;

      end else begin

         if    FTok.Test(ttDOT)
            or (FTok.Test(ttBLEFT) and (propertySym.BaseType is TFuncSymbol))  then begin

            sym:=propertySym.ReadSym;

            if sym is TMethodSymbol then begin

               Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, False);

            end else if sym is TFieldSymbol then begin

               if Expr.Typ is TClassOfSymbol then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
               Result:=TReadOnlyFieldExpr.Create(FProg, FTok.HotPos, TFieldSymbol(sym), expr);
               expr:=nil;

            end else if sym is TClassVarSymbol then begin

               expr.Free;
               expr:=nil;
               Result:=GetVarExpr(TClassVarSymbol(sym));

            end else begin

               expr.Free;
               expr:=nil;
               Result:=TNullExpr.Create(FProg, aPos);
               FMsgs.AddCompilerError(aPos, CPE_WriteOnlyProperty)

            end;

         end else begin

            FMsgs.AddCompilerError(aPos, CPE_InvalidInstruction);
            // fake to keep going
            expr.Free;
            expr:=nil;
            Result:=TConstExpr.Create(FProg, propertySym.Typ, Null);

         end;

      end;

   finally
      typedExprList.Free;
   end;
end;

// ReadPropertyArrayAccessor
//
function TdwsCompiler.ReadPropertyArrayAccessor(var expr : TTypedExpr; propertySym : TPropertySymbol;
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
      if not TMethodSymbol(sym).IsClassMethod then begin
         if isWrite then
            FMsgs.AddCompilerStop(scriptPos, CPE_StaticPropertyWriteExpected)
         else FMsgs.AddCompilerStop(scriptPos, CPE_StaticPropertyReadExpected);
      end;
      Result:=GetMethodExpr(TMethodSymbol(sym), expr, rkClassOfRef, scriptPos, False);
   end else Result:=GetMethodExpr(TMethodSymbol(sym), expr, rkObjRef, scriptPos, False);

   expr:=nil;
   try
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

      TypeCheckArgs(Result, nil);
   except
      Result.Free;
      raise;
   end;
end;

// ReadSymbol
//
function TdwsCompiler.ReadSymbol(expr : TProgramExpr; isWrite : Boolean = False;
                                 expecting : TTypeSymbol = nil) : TProgramExpr;

   function GetDefaultProperty(struct : TCompositeTypeSymbol) : TPropertySymbol;
   begin
      while Assigned(struct) and not Assigned(struct.DefaultProperty) do
         struct:=struct.Parent;

      if Assigned(struct) then
         Result:=struct.DefaultProperty
      else Result:=nil;
   end;

var
   defaultProperty : TPropertySymbol;
   baseType : TTypeSymbol;
   dataExpr : TDataExpr;
begin
   Result := Expr;
   try
      repeat
         Expr := Result;

         // Member
         case FTok.TestAny([ttDOT, ttALEFT, ttBLEFT]) of

            ttDOT : begin

               FTok.KillToken;
               Result:=nil;
               Result:=ReadSymbolMemberExpr(expr, isWrite, expecting);

            end;
            ttALEFT : begin
               // Arrays

               if Assigned(Result) then begin

                  baseType := Result.BaseType;
                  if (baseType is TStructuredTypeSymbol) or (baseType is TStructuredTypeMetaSymbol) then begin

                     // class array property
                     if baseType is TStructuredTypeSymbol then
                        defaultProperty:=GetDefaultProperty(TStructuredTypeSymbol(baseType))
                     else defaultProperty:=GetDefaultProperty(TStructuredTypeMetaSymbol(baseType).StructSymbol);

                     if Assigned(defaultProperty) then begin
                        RecordSymbolUseImplicitReference(defaultProperty, FTok.HotPos, isWrite);
                        Result:=ReadPropertyExpr(TDataExpr(Result), defaultProperty, isWrite)
                     end else begin
                        FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NoDefaultProperty,
                                                 [TDataExpr(Result).Typ.Name]);
                     end;

                  end else begin

                     // Type "array"
                     dataExpr:=(Result as TDataExpr);
                     if baseType is TArraySymbol then
                        Result := ReadSymbolArrayExpr(dataExpr)
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
            end;

            ttBLEFT : begin

               baseType := Result.BaseType;
               if baseType is TFuncSymbol then begin
                  dataExpr:=Result as TDataExpr;
                  Result:=nil;
                  Result:=ReadFunc(TFuncSymbol(baseType), dataExpr);
               end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMethodExpected);

            end;

         end;

      until (Expr = Result);
   except
      Result.Free;
      raise;
   end;
end;

// ReadSymbolArrayExpr
//
function TdwsCompiler.ReadSymbolArrayExpr(var baseExpr : TDataExpr) : TProgramExpr;
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
         if    (indexExpr.Typ=nil)
            or not (   (indexExpr.Typ.UnAliasedType=baseType.IndexType.UnAliasedType)
                    or indexExpr.Typ.IsOfType(FProg.TypVariant)) then
            IncompatibleTypes(hotPos, CPE_ArrayIndexMismatch,
                              baseType.IndexType, indexExpr.Typ);

         if baseType is TStaticArraySymbol then begin

            arraySymbol:=TStaticArraySymbol(baseType);
            if arraySymbol is TOpenArraySymbol then begin

               newBaseExpr := TOpenArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr)

            end else begin

               if arraySymbol.IndexType.IsOfType(FProg.TypBoolean) then begin

                  newBaseExpr:=TStaticArrayBoolExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr,
                                                           arraySymbol.LowBound, arraySymbol.HighBound);

               end else begin

                  newBaseExpr:=TStaticArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr,
                                                       arraySymbol.LowBound, arraySymbol.HighBound);
                  if indexExpr.IsConstant and (FMsgs.Count=errCount) then begin
                     idx:=indexExpr.EvalAsInteger(FExec);
                     if idx<arraySymbol.LowBound then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, RTE_ArrayLowerBoundExceeded, [idx])
                     else if idx>arraySymbol.HighBound then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, RTE_ArrayUpperBoundExceeded, [idx]);
                  end;

               end;
            end;

         end else begin

            Assert(baseType is TDynamicArraySymbol);

            if FTok.Test(ttCOMMA) then
               newBaseExpr:=TDynamicArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr)
            else if FTok.TestDelete(ttARIGHT) then begin
               if FTok.TestDelete(ttASSIGN) then begin
                  hotPos:=FTok.HotPos;
                  valueExpr:=ReadExpr(baseType.Typ);
                  if not baseType.Typ.IsCompatible(valueExpr.Typ) then
                     IncompatibleTypes(hotPos, CPE_AssignIncompatibleTypes,
                                       valueExpr.Typ, baseType.Typ);
                  Result:=TDynamicArraySetExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr, valueExpr);
               end else begin
                  Result:=TDynamicArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr);
               end;
               Exit;
            end else begin
               indexExpr.Free;
               indexExpr:=nil;
            end;

         end;

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

// ReadSymbolMemberExpr
//
function TdwsCompiler.ReadSymbolMemberExpr(var expr : TProgramExpr;
                                           isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   name : String;
   namePos : TScriptPos;
   helperExpr : TProgramExpr;
   member : TSymbol;
   memberClassType : TClass;
   baseExpr : TTypedExpr;
   meth : TMethodSymbol;
   baseType : TTypeSymbol;
begin
   Result:=nil;
   try

      if FTok.TestDeleteAnyNamePos(name, namePos) then begin

         baseType:=expr.BaseType;

         if baseType<>nil then begin
            helperExpr:=ReadTypeHelper(expr as TTypedExpr,
                                       name, namePos, expecting);
            if helperExpr<>nil then begin

               expr:=nil;
               Result:=helperExpr;
               Exit;

            end;
         end;
         Result:=expr;

         // Class, record, intf
         if baseType is TStructuredTypeSymbol then begin

            member:=FindStructMember(TStructuredTypeSymbol(baseType), name);
            if member<>nil then
               memberClassType:=member.ClassType
            else memberClassType:=nil;

            RecordSymbolUseReference(member, namePos, isWrite);

            if (baseType is TRecordSymbol) and (Result is TFuncExpr) then
               TFuncExpr(Result).SetResultAddr(FProg, nil);

            if member is TMethodSymbol then begin

               baseExpr:=(Result as TTypedExpr);
               Result:=nil;
               meth:=TMethodSymbol(member);
               if meth.IsOverloaded then
                  Result:=ReadMethOverloaded(meth, baseExpr, namePos, expecting)
               else Result:=ReadMethod(meth, baseExpr, namePos, expecting);

            end else if member is TFieldSymbol then begin

               Result:=ReadField(FTok.HotPos, nil, TFieldSymbol(member), TTypedExpr(Result));

            end else if member is TPropertySymbol then begin

               Assert(Result is TTypedExpr);
               Result := ReadPropertyExpr(TTypedExpr(Result), TPropertySymbol(member), IsWrite)

            end else if memberClassType=TClassVarSymbol then begin

               Result.Free;
               Result:=ReadDataSymbolName(TDataSymbol(member), TStructuredTypeSymbol(member).Members, IsWrite, expecting);

            end else if memberClassType=TClassConstSymbol then begin

               Result.Free;
               Result:=ReadConstName(TConstSymbol(member), IsWrite);

            end else begin

               FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);

            end;

         // Meta (Class Of, Record Of)
         end else if baseType is TStructuredTypeMetaSymbol then begin

            member:=TStructuredTypeSymbol(baseType.Typ).Members.FindSymbolFromScope(Name, CurrentStruct);
            if member<>nil then
               memberClassType:=member.ClassType
            else memberClassType:=nil;

            RecordSymbolUseReference(member, namePos, isWrite);

            // Class method
            if member is TMethodSymbol then begin

               baseExpr:=(Result as TTypedExpr);
               Result:=nil;
               meth:=TMethodSymbol(member);
               if meth.IsOverloaded then
                  Result:=ReadStaticMethOverloaded(meth, baseExpr, namePos, expecting)
               else Result:=ReadStaticMethod(meth, baseExpr, namePos, expecting);

            // Static property
            end else if member is TPropertySymbol then begin

               Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite);

            end else if memberClassType=TClassVarSymbol then begin

               Result.Free;
               Result:=ReadDataSymbolName(TDataSymbol(member), TStructuredTypeSymbol(baseType.Typ).Members,
                                          IsWrite, expecting);

            end else if memberClassType=TClassConstSymbol then begin

               Result.Free;
               Result:=ReadConstName(TConstSymbol(member), IsWrite);

            end else if member<>nil then begin

               FMsgs.AddCompilerStop(namePos, CPE_StaticMethodExpected);

            end else begin

               FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);

            end;

         // Array symbol
         end else if baseType is TArraySymbol then begin

            Result:=ReadArrayMethod(name, namePos, Result as TTypedExpr);

         // Connector symbol
         end else if baseType is TConnectorSymbol then begin

            try
               Result:=ReadConnectorSym(Name, Result as TTypedExpr,
                                          TConnectorSymbol(baseType).ConnectorType, IsWrite)
            except
               Result:=nil;
               raise;
            end;

         end else FMsgs.AddCompilerStop(namePos, CPE_NoMemberExpected);

      end else begin

         expr.Free;
         expr:=nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      end;
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
      if IsWrite and not FTok.Test(ttDOT) then begin
         if FTok.TestDelete(ttASSIGN) then begin
            if not Assigned(Sym.WriteFunc) then
               FMsgs.AddCompilerStop(FTok.HotPos,CPE_CantWriteToLeftSide);
            // Transform a := b into a(b)
            Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.WriteFunc);
            Result.AddArg(ReadExpr);
         end else if (Sym.Typ is TClassSymbol) or (Sym.Typ is TClassOfSymbol) or (Sym.Typ is TConnectorSymbol) then begin
            if not Assigned(Sym.ReadFunc) then
               FMsgs.AddCompilerStop(FTok.HotPos,CPE_RightSideNeedsReturnType);
            Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc)
         end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
      end else if Assigned(Sym.ReadFunc) then
         Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc)
      else FMsgs.AddCompilerStop(FTok.HotPos,CPE_WriteOnlyProperty); // ??
      TypeCheckArgs(Result, nil);
   except
      Result.Free;
      raise;
   end;
end;

// ReadFor
//
function TdwsCompiler.ReadFor : TNoResultExpr;
var
   forPos : TScriptPos;
   expr : TProgramExpr;
   loopVarExpr : TVarExpr;
begin
   forPos:=FTok.HotPos;

   expr:=ReadName(True);

   if expr is TFuncPtrExpr then
      expr:=TFuncPtrExpr(expr).Extract;
   if not (expr is TVarExpr) then begin
      expr.Free;
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_VariableExpected);
   end;

   loopVarExpr:=TVarExpr(expr);

   WarnForVarUsage(loopVarExpr, FTok.HotPos);

   case FTok.TestDeleteAny([ttASSIGN, ttIN]) of
      ttASSIGN :
         Result:=ReadForTo(forPos, loopVarExpr);
      ttIN :
         Result:=ReadForIn(forPos, loopVarExpr);
   else
      expr.Free;
      Result:=nil;
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
   end;
end;

// ReadForTo
//
function TdwsCompiler.ReadForTo(const forPos : TScriptPos; loopVarExpr : TVarExpr) : TForExpr;
var
   iterVarExpr : TIntVarExpr;
   fromExpr, toExpr : TTypedExpr;
   forExprClass : TForExprClass;
begin
   fromExpr:=nil;
   toExpr:=nil;
   try
      if not loopVarExpr.IsOfType(FProg.TypInteger) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_IntegerExpected);
      if not (loopVarExpr is TIntVarExpr) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_FORLoopMustBeLocalVariable);

      iterVarExpr:=TIntVarExpr(loopVarExpr);

      fromExpr:=ReadExpr;
      if not fromExpr.IsOfType(FProg.TypInteger) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_IntegerExpected);

      if FTok.TestDelete(ttTO) then
         forExprClass:=TForUpwardExpr
      else if FTok.TestDelete(ttDOWNTO) then
         forExprClass:=TForDownwardExpr
      else begin
         forExprClass:=TForUpwardExpr;
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ToOrDowntoExpected);
      end;

      toExpr:=ReadExpr;
      if not toExpr.IsOfType(FProg.TypInteger) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);
   except
      fromExpr.Free;
      toExpr.Free;
      loopVarExpr.Free;
      raise;
   end;

   Result:=ReadForStep(forPos, forExprClass, iterVarExpr,
                       fromExpr, toExpr, nil);
end;

// ReadForIn
//
function TdwsCompiler.ReadForIn(const forPos : TScriptPos; loopVarExpr : TVarExpr) : TNoResultExpr;
var
   iterVarExpr : TIntVarExpr;
   iterVarSym : TDataSymbol;
   initIterVarExpr : TAssignConstToIntegerVarExpr;
   inExpr : TProgramExpr;
   inExprVarSym : TDataSymbol;
   fromExpr, toExpr : TTypedExpr;
   forExprClass : TForExprClass;
   arraySymbol : TArraySymbol;
   enumSymbol : TTypeSymbol;
   inPos : TScriptPos;
   inExprAssignExpr, readArrayItemExpr : TAssignExpr;
   inExprVarExpr : TVarExpr;
   blockExpr : TBlockExprNoTable2;
begin
   forExprClass:=TForUpwardExpr;

   inPos:=FTok.HotPos;

   inExpr:=ReadName(False, FAnyTypeSymbol);

   readArrayItemExpr:=nil;
   inExprAssignExpr:=nil;

   if (inExpr is TTypedExpr) and (inExpr.ClassType<>TTypeReferenceExpr) then begin

      if      inExpr.Typ.IsOfType(FProg.TypString)
         and (   loopVarExpr.Typ.IsOfType(FProg.TypInteger)
              or loopVarExpr.Typ.IsOfType(FProg.TypString)) then begin

         if not FTok.TestDelete(ttDO) then begin
            inExpr.Free;
            loopVarExpr.Free;
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);
         end;
         if loopVarExpr.Typ.IsOfType(FProg.TypInteger) then begin
            Result:=TForCharCodeInStrExpr.Create(FProg, forPos, loopVarExpr as TIntVarExpr,
                                                 TTypedExpr(inExpr), ReadBlock)
         end else begin
            Result:=TForCharInStrExpr.Create(FProg, forPos, loopVarExpr as TStrVarExpr,
                                             TTypedExpr(inExpr), ReadBlock);
         end;
         Exit;

      end else if inExpr.Typ is TArraySymbol then begin

         arraySymbol:=TArraySymbol(inExpr.Typ);

         // if inExpr is an expression, create a temporary variable
         // so it is evaluated only once
         if not ((inExpr is TVarExpr) or (inExpr is TConstExpr)) then begin
            inExprVarSym:=TDataSymbol.Create('', arraySymbol);
            FProg.Table.AddSymbol(inExprVarSym);
            inExprVarExpr:=GetVarExpr(inExprVarSym);
            inExprAssignExpr:=TAssignExpr.Create(FProg, FTok.HotPos, inExprVarExpr, TTypedExpr(inExpr));
            inExpr:=inExprVarExpr;
            inExpr.IncRefCount;
         end;

         // create anonymous iter variables & it's initialization expression
         iterVarSym:=TDataSymbol.Create('', arraySymbol.IndexType);
         FProg.Table.AddSymbol(iterVarSym);
         iterVarExpr:=GetVarExpr(iterVarSym) as TIntVarExpr;
         initIterVarExpr:=TAssignConstToIntegerVarExpr.CreateVal(FProg, inPos, iterVarExpr, 0);
         FProg.InitExpr.AddStatement(initIterVarExpr);

         fromExpr:=CreateArrayLow(inExpr, arraySymbol, False);
         toExpr:=CreateArrayHigh(inExpr, arraySymbol, False);

         iterVarExpr:=GetVarExpr(iterVarSym) as TIntVarExpr;
         readArrayItemExpr:=TAssignExpr.Create(FProg, FTok.HotPos, loopVarExpr,
                                               CreateArrayExpr(FTok.HotPos, (inExpr as TDataExpr), iterVarExpr));

         iterVarExpr:=GetVarExpr(iterVarSym) as TIntVarExpr;

      end else begin

         loopVarExpr.Free;
         iterVarExpr:=nil;
         fromExpr:=nil;
         toExpr:=nil;
         inExpr.Free;
         FMsgs.AddCompilerStop(inPos, CPE_ArrayExpected);

      end;

   end else begin

      enumSymbol:=nil;
      if inExpr is TTypeReferenceExpr then begin
         if inExpr.Typ.InheritsFrom(TEnumerationSymbol) then
            enumSymbol:=TEnumerationSymbol(inExpr.Typ);
      end;
      if enumSymbol=nil then begin
         FMsgs.AddCompilerError(inPos, CPE_EnumerationExpected);
         enumSymbol:=FProg.TypBoolean;
      end;

      inExpr.Free;

      if not loopVarExpr.Typ.IsOfType(enumSymbol) then begin
         FMsgs.AddCompilerError(inPos, CPE_IncompatibleOperands);
         enumSymbol:=nil;
      end;

      RecordSymbolUse(enumSymbol, inPos, [suReference]);

      if enumSymbol is TEnumerationSymbol then begin
         fromExpr:=TConstExpr.CreateIntegerValue(FProg, loopVarExpr.Typ, TEnumerationSymbol(enumSymbol).LowBound);
         toExpr:=TConstExpr.CreateIntegerValue(FProg, loopVarExpr.Typ, TEnumerationSymbol(enumSymbol).HighBound);
      end else begin
         fromExpr:=TConstExpr.CreateIntegerValue(FProg, loopVarExpr.Typ, 0);
         toExpr:=TConstExpr.CreateIntegerValue(FProg, loopVarExpr.Typ, 1);
      end;

      iterVarExpr:=(loopVarExpr as TIntVarExpr);

   end;

   Result:=ReadForStep(forPos, forExprClass, iterVarExpr,
                       fromExpr, toExpr, readArrayItemExpr);

   if inExprAssignExpr<>nil then begin
      blockExpr:=TBlockExprNoTable2.Create(FProg, forPos);
      blockExpr.AddStatement(inExprAssignExpr);
      blockExpr.AddStatement(Result);
      Result:=blockExpr;
   end;
end;

// ReadForStep
//
function TdwsCompiler.ReadForStep(const forPos : TScriptPos; forExprClass : TForExprClass;
                           iterVarExpr : TIntVarExpr; fromExpr, toExpr : TTypedExpr;
                           loopFirstStatement : TNoResultExpr) : TForExpr;
var
   stepExpr : TTypedExpr;
   stepPos : TScriptPos;
   iterBlockExpr : TBlockExpr;
begin
   try
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
      end else stepExpr:=nil;

      iterBlockExpr:=nil;
      Result:=forExprClass.Create(FProg, forPos);
      EnterLoop(Result);
      try
         MarkLoopExitable(leBreak);
         Result.VarExpr:=iterVarExpr;
         iterVarExpr:=nil;

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

         if loopFirstStatement<>nil then begin
            iterBlockExpr:=TBlockExpr.Create(FProg, FTok.HotPos);
            iterBlockExpr.AddStatement(loopFirstStatement);
            loopFirstStatement:=nil;
            iterBlockExpr.AddStatement(ReadBlock);
            Result.DoExpr:=iterBlockExpr;
            iterBlockExpr:=nil;
         end else begin
            Result.DoExpr:=ReadBlock;
         end;

      except
         iterBlockExpr.Free;
         stepExpr.Free;
         Result.Free;
         raise;
      end;
      LeaveLoop;
   except
      iterVarExpr.Free;
      fromExpr.Free;
      toExpr.Free;
      loopFirstStatement.Free;
      raise;
   end;
end;

// WarnForVarUsage
//
procedure TdwsCompiler.WarnForVarUsage(varExpr : TVarExpr; const scriptPos : TScriptPos);
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
            FMsgs.AddCompilerWarning(scriptPos, CPE_AssignementToFORLoopVariable);
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

   if Optimize then
      Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
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
         Result.ValueExpr:=ReadExpr;
         if Result.ValueExpr.Typ=nil then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExpressionExpected);

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
      exprFrom:=ReadExpr;

      try
         if FTok.TestDelete(ttDOTDOT) then begin
            // range condition e. g. 0..12
            exprTo:=ReadExpr;
            condition:=TRangeCaseCondition.Create(hotPos, exprFrom, exprTo);
         end else begin
            // compare condition e. g. 123:
            condition:=TCompareCaseCondition.Create(hotPos, exprFrom);
         end;
         exprFrom:=nil;
         condList.Add(condition);
         if valueExpr.Typ<>nil then
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

   if Optimize then
      Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
end;

// ReadRepeat
//
function TdwsCompiler.ReadRepeat : TNoResultExpr;
var
   tt : TTokenType;
   condExpr : TTypedExpr;
begin
   Result:=TRepeatExpr.Create(FProg, FTok.HotPos);
   EnterLoop(Result);
   try
      TRepeatExpr(Result).LoopExpr:=ReadBlocks([ttUNTIL], tt);
      TRepeatExpr(Result).SetScriptPos(FTok.HotPos);
      condExpr:=ReadExpr;
      TRepeatExpr(Result).CondExpr:=condExpr;
      if not (condExpr.IsOfType(FProg.TypBoolean) or condExpr.IsOfType(FProg.TypVariant)) then
         FMsgs.AddCompilerError(Result.ScriptPos, CPE_BooleanExpected)
      else if (not condExpr.IsConstant) or condExpr.EvalAsBoolean(FExec) then
         MarkLoopExitable(leBreak);
   except
      Result.Free;
      raise;
   end;
   LeaveLoop;

   if Optimize then
      Result:=Result.OptimizeToNoResultExpr(FProg, FExec);
end;

// ReadAssign
//
function TdwsCompiler.ReadAssign(token : TTokenType; var left : TDataExpr) : TNoResultExpr;
var
   pos : TScriptPos;
   right : TTypedExpr;
begin
   pos:=FTok.HotPos;
   right:=nil;
   try
      right:=ReadExpr(left.Typ);
      Result:=CreateAssign(pos, token, left, right);
      left:=nil;
   except
      left.Free;
      left:=nil;
      right.Free;
      raise;
   end;
end;

// ReadSelfMethod
//
function TdwsCompiler.ReadSelfMethod(methodSym : TMethodSymbol;
               isWrite : Boolean; expecting : TTypeSymbol = nil;
               overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   progMeth : TMethodSymbol;
   structSym : TCompositeTypeSymbol;
begin
   progMeth:=FProg.ContextMethodSymbol;

   if progMeth<>nil then begin
      if methodSym.IsStatic then
         Result:=GetMethodExpr(methodSym, nil, rkObjRef, FTok.HotPos, False)
      else if progMeth.IsStatic then begin
         structSym:=progMeth.StructSymbol;
         Result:=GetMethodExpr(methodSym,
                               TConstExpr.Create(FProg, (structSym as TStructuredTypeSymbol).MetaSymbol, Int64(structSym)),
                               rkClassOfRef, FTok.HotPos, False);
      end else if progMeth.SelfSym is TConstParamSymbol then begin
         Result:=GetMethodExpr(methodSym,
                               GetConstParamExpr(TConstParamSymbol(progMeth.SelfSym)),
                               rkObjRef, FTok.HotPos, False);
      end else if progMeth.SelfSym=nil then begin
         Result:=GetMethodExpr(methodSym, nil, rkClassOfRef, FTok.HotPos, False);
      end else begin
         Result:=GetMethodExpr(methodSym,
                               GetVarExpr(progMeth.SelfSym),
                               rkObjRef, FTok.HotPos, False);
      end;
   end else begin
      structSym:=methodSym.StructSymbol;
      Result:=GetMethodExpr(methodSym,
                            TConstExpr.Create(FProg, (structSym as TStructuredTypeSymbol).MetaSymbol, Int64(structSym)),
                            rkClassOfRef, FTok.HotPos, True);
   end;

   Result:=WrapUpFunctionRead(TFuncExpr(Result), expecting, overloads);
end;

// ReadMethod
//
function TdwsCompiler.ReadMethod(methodSym : TMethodSymbol; instanceExpr : TTypedExpr;
                                 const scriptPos : TScriptPos;
                                 expecting : TTypeSymbol = nil;
                                 overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   funcExpr : TFuncExpr;
begin
   if methodSym.IsClassMethod then
      funcExpr:=GetMethodExpr(methodSym, instanceExpr, rkClassOfRef, scriptPos, False)
   else begin
      funcExpr:=GetMethodExpr(methodSym, instanceExpr, rkObjRef, scriptPos, False);
   end;
   Result:=WrapUpFunctionRead(funcExpr, expecting, overloads);
end;

// ReadStaticMethod
//
function TdwsCompiler.ReadStaticMethod(methodSym : TMethodSymbol; metaExpr : TTypedExpr;
                                       const scriptPos : TScriptPos;
                                       expecting : TTypeSymbol = nil;
                                       overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   funcExpr : TFuncExpr;
   compoSym : TCompositeTypeSymbol;
begin
   if methodSym.Kind=fkConstructor then begin
      compoSym:=(metaExpr.Typ as TStructuredTypeMetaSymbol).StructSymbol;
      if compoSym.IsStatic then
         FMsgs.AddCompilerErrorFmt(scriptPos, CPE_ClassIsStatic, [compoSym.Name]);
   end;
   funcExpr:=GetMethodExpr(methodSym, metaExpr, rkClassOfRef, scriptPos, False);
   Result:=WrapUpFunctionRead(funcExpr, expecting, overloads);
end;

type
   TFuncAtLevelSymbolList = class(TFuncSymbolList)
      Level : Integer;
      function Callback(sym : TSymbol) : Boolean;
   end;

function TFuncAtLevelSymbolList.Callback(sym : TSymbol) : Boolean;
var
   locFuncSym : TFuncSymbol;
begin
   if sym is TFuncSymbol then begin
      locFuncSym:=TFuncSymbol(sym);
      if locFuncSym.Level=Level then
         Add(locFuncSym);
   end;
   Result:=False;
end;

// ReadFuncOverloaded
//
function TdwsCompiler.ReadFuncOverloaded(funcSym : TFuncSymbol; fromTable : TSymbolTable;
      codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TTypedExpr;
var
   overloads : TFuncAtLevelSymbolList;
begin
   overloads:=TFuncAtLevelSymbolList.Create;
   try
      overloads.Level:=funcSym.Level;
      fromTable.EnumerateSymbolsOfNameInScope(funcSym.Name, overloads.Callback);
      Result:=ReadFunc(funcSym, codeExpr, expecting, overloads);
   finally
      overloads.Free;
   end;
end;

// CollectMethodOverloads
//
procedure TdwsCompiler.CollectMethodOverloads(methSym : TMethodSymbol; overloads : TFuncSymbolList);
var
   member : TSymbol;
   struct : TCompositeTypeSymbol;
   lastOverloaded : TMethodSymbol;
begin
   lastOverloaded:=methSym;
   struct:=methSym.StructSymbol;
   repeat
      for member in struct.Members do begin
         if not UnicodeSameText(member.Name, methSym.Name) then continue;
         if not (member is TMethodSymbol) then continue;
         lastOverloaded:=TMethodSymbol(member);
         if not overloads.ContainsChildMethodOf(lastOverloaded) then
            overloads.Add(lastOverloaded);
      end;
      struct:=struct.Parent;
   until (struct=nil) or not lastOverloaded.IsOverloaded;
end;

// ReadSelfMethOverloaded
//
function TdwsCompiler.ReadSelfMethOverloaded(methSym : TMethodSymbol; isWrite : Boolean;
                                               expecting : TTypeSymbol = nil) : TTypedExpr;
var
   overloads : TFuncSymbolList;
begin
   overloads:=TFuncSymbolList.Create;
   try
      CollectMethodOverloads(methSym, overloads);
      Result:=ReadSelfMethod(methSym, isWrite, expecting, overloads);
   finally
      overloads.Free;
   end;
end;

// ReadMethOverloaded
//
function TdwsCompiler.ReadMethOverloaded(methSym : TMethodSymbol; instanceExpr : TTypedExpr;
                                         const scriptPos : TScriptPos;
                                         expecting : TTypeSymbol = nil) : TTypedExpr;
var
   overloads : TFuncSymbolList;
begin
   overloads:=TFuncSymbolList.Create;
   try
      CollectMethodOverloads(methSym, overloads);
      Result:=ReadMethod(methSym, instanceExpr, scriptPos, expecting, overloads);
   finally
      overloads.Free;
   end;
end;

// ReadStaticMethOverloaded
//
function TdwsCompiler.ReadStaticMethOverloaded(methSym : TMethodSymbol; metaExpr : TTypedExpr;
                                               const scriptPos : TScriptPos;
                                               expecting : TTypeSymbol = nil) : TTypedExpr;
var
   i : Integer;
   overloads : TFuncSymbolList;
   meth : TMethodSymbol;
begin
   overloads:=TFuncSymbolList.Create;
   try
      CollectMethodOverloads(methSym, overloads);
      for i:=overloads.Count-1 downto 0 do begin
         meth:=(overloads[i] as TMethodSymbol);
         case meth.Kind of
            fkFunction, fkProcedure, fkMethod:
               if not meth.IsClassMethod then
                  overloads.Extract(i);
            fkDestructor :
               overloads.Extract(i);
         end;
      end;
      if overloads.Count=0 then
         FMsgs.AddCompilerStop(scriptPos, CPE_StaticMethodExpected);
      meth:=methSym;
      methSym:=TMethodSymbol(overloads[0]);
      if (meth<>methSym) and (coSymbolDictionary in Options) then
         ReplaceSymbolUse(meth, methSym, scriptPos);
      Result:=ReadStaticMethod(methSym, metaExpr, scriptPos, expecting, overloads);
   finally
      overloads.Free;
   end;
end;

// ResolveOverload
//
function TdwsCompiler.ResolveOverload(var funcExpr : TFuncExprBase; overloads : TFuncSymbolList;
                                      const argPosArray : TScriptPosArray) : Boolean;
var
   i : Integer;
   j : Integer;
   match, bestMatch : TFuncSymbol;
   struct : TCompositeTypeSymbol;
   matchDistance, bestMatchDistance, bestCount : Integer;
   matchParamType, funcExprParamType : TTypeSymbol;
   wasVarParam, nowVarParam : Boolean;
begin
   bestMatch:=nil;
   bestCount:=0;
   bestMatchDistance:=MaxInt;
   for i:=0 to overloads.Count-1 do begin
      match:=overloads[i];
      if funcExpr.Args.Count>match.Params.Count then continue;
      matchDistance:=0;
      for j:=0 to funcExpr.Args.Count-1 do begin
         matchParamType:=match.GetParamType(j);
         funcExprParamType:=funcExpr.GetArgType(j);
         if not matchParamType.IsOfType(funcExprParamType) then begin

            if funcExprParamType.IsOfType(FProg.TypVariant) then begin

               if not funcExprParamType.IsCompatible(matchParamType) then begin
                  match:=nil;
                  break;
               end else begin
                  // disfavor variant promotion to integer
                  if matchParamType.IsOfType(FProg.TypInteger) then
                     Inc(matchDistance, 256)
                  else Inc(matchDistance, 1);
               end;

            end else if     (funcExprParamType is TStaticArraySymbol)
                        and (matchParamType is TDynamicArraySymbol) then begin

               if funcExprParamType.Typ.IsOfType(matchParamType.Typ) then begin

                  if (funcExprParamType.Typ is TClassSymbol) or (funcExprParamType.Typ is TInterfaceSymbol) then begin

                     Inc(matchDistance, (matchParamType.Typ as TStructuredTypeSymbol).NthParentOf(TStructuredTypeSymbol(funcExprParamType.Typ)));

                  end else begin

                     Inc(matchDistance, 1);

                  end;

               end else if    (TStaticArraySymbol(funcExprParamType).ElementCount=0)
                           or (    (funcExprParamType.Typ=FProg.TypNil)
                               and (   (matchParamType.Typ is TClassSymbol)
                                    or (matchParamType.Typ is TInterfaceSymbol))) then begin

                  Inc(matchDistance, 1);

               end else begin

                  match:=nil;
                  break;

               end;

            end else if not (   (matchParamType.IsOfType(FProg.TypFloat) and funcExprParamType.IsOfType(FProg.TypInteger))
                             or matchParamType.IsCompatible(funcExprParamType)) then begin

               match:=nil;
               break;

            end else begin

               if (funcExprParamType is TClassSymbol) or (funcExprParamType is TInterfaceSymbol) then begin

                  Inc(matchDistance, (matchParamType as TStructuredTypeSymbol).NthParentOf(TStructuredTypeSymbol(funcExprParamType)));

               end else begin

                  Inc(matchDistance, 1);

               end;

            end;
         end;
      end;
      if match=nil then continue;
      for j:=funcExpr.Args.Count to match.Params.Count-1 do begin
         if match.Params[j].ClassType<>TParamSymbolWithDefaultValue then begin
            match:=nil;
            Break;
         end;
      end;
      if match=nil then continue;

      if match is TMethodSymbol then begin
         // for method symbols give precedence to the deepest subclass
         // this will only differentiate matches that rated the same on parameters
         matchDistance:=(matchDistance+1) shl 16;
         struct:=TMethodSymbol(match).StructSymbol;
         while struct<>nil do begin
            Dec(matchDistance);
            struct:=struct.Parent;
         end;
      end;

      if matchDistance<=bestMatchDistance then begin
         if matchDistance<bestMatchDistance then begin
            bestMatch:=match;
            bestMatchDistance:=matchDistance;
            bestCount:=1;
         end else Inc(bestCount);
      end;
   end;
   if (bestMatch<>nil) and (bestCount=1) then begin
      if bestMatch<>funcExpr.FuncSym then begin
         if coSymbolDictionary in Options then begin
            ReplaceSymbolUse(funcExpr.FuncSym, bestMatch, funcExpr.ScriptPos);
            for i:=0 to Min(bestMatch.Params.Count, funcExpr.FuncSym.Params.Count)-1 do begin
               nowVarParam:=(bestMatch.Params[i] is TVarParamSymbol);
               wasVarParam:=(funcExpr.FuncSym.Params[i] is TVarParamSymbol);
               if wasVarParam<>nowVarParam then begin
                  if wasVarParam then
                     FSymbolDictionary.ChangeUsageAt(argPosArray[i], [], [suWrite])
                  else FSymbolDictionary.ChangeUsageAt(argPosArray[i], [suWrite], []);
               end;
            end;
         end;
         funcExpr:=funcExpr.ChangeFuncSymbol(FProg, bestMatch);
      end;
      Result:=True;
   end else begin
      FMsgs.AddCompilerErrorFmt(funcExpr.ScriptPos, CPE_NoMatchingOverloadForCall,
                                [funcExpr.FuncSym.Name]);
      Result:=False;
   end;
end;

type
   TFuncConflictEnumerator = class
      ForwardedSym, FuncSym : TFuncSymbol;
      function Callback(sym : TSymbol) : Boolean;
   end;

function TFuncConflictEnumerator.Callback(sym : TSymbol) : Boolean;
var
   locSym : TFuncSymbol;
begin
   if (sym<>ForwardedSym) and (sym is TFuncSymbol) then begin
      locSym:=TFuncSymbol(sym);
      if locSym.Level=FuncSym.Level then
         if not FuncSym.IsValidOverloadOf(locSym) then
            Exit(True);
   end;
   Result:=False;
end;

// FuncHasConflictingOverload
//
function TdwsCompiler.FuncHasConflictingOverload(funcSym, forwardedSym : TFuncSymbol) : Boolean;
var
   enumerator : TFuncConflictEnumerator;
begin
   enumerator:=TFuncConflictEnumerator.Create;
   try
      enumerator.ForwardedSym:=forwardedSym;
      enumerator.FuncSym:=funcSym;
      Result:=FProg.Table.EnumerateSymbolsOfNameInScope(funcSym.Name, enumerator.Callback);
   finally
      enumerator.Free;
   end;
end;

// MethHasConflictingOverload
//
function TdwsCompiler.MethHasConflictingOverload(methSym : TMethodSymbol) : Boolean;
var
   struct : TCompositeTypeSymbol;
   member : TSymbol;
begin
   struct:=methSym.StructSymbol;
   for member in struct.Members do begin
      if not UnicodeSameText(member.Name, methSym.Name) then continue;
      if not (member is TMethodSymbol) then continue;
      if not methSym.IsValidOverloadOf(TMethodSymbol(member)) then
         Exit(True);
   end;
   Result:=False;
end;

type
   TPerfectMatchEnumerator = class
      FuncSym, Match : TFuncSymbol;
      function Callback(sym : TSymbol) : Boolean;
   end;

function TPerfectMatchEnumerator.Callback(sym : TSymbol) : Boolean;
var
   locSym : TFuncSymbol;
begin
   if sym is TFuncSymbol then begin
      locSym:=TFuncSymbol(sym);
      if locSym.Level=FuncSym.Level then
         if FuncSym.IsSameOverloadOf(locSym) then begin
            Match:=locSym;
            Exit(True);
         end;
   end;
   Result:=False;
end;

// FuncPerfectMatchOverload
//
function TdwsCompiler.FuncPerfectMatchOverload(funcSym : TFuncSymbol) : TFuncSymbol;
var
   enumerator : TPerfectMatchEnumerator;
begin
   enumerator:=TPerfectMatchEnumerator.Create;
   try
      enumerator.FuncSym:=funcSym;
      FProg.Table.EnumerateSymbolsOfNameInScope(funcSym.Name, enumerator.Callback);
      Result:=enumerator.Match;
   finally
      enumerator.Free;
   end;
end;

// MethPerfectMatchOverload
//
function TdwsCompiler.MethPerfectMatchOverload(methSym : TMethodSymbol; recurse : Boolean) : TMethodSymbol;
var
   struct : TCompositeTypeSymbol;
   member : TSymbol;
   locSym : TMethodSymbol;
begin
   locSym:=methSym;
   struct:=methSym.StructSymbol;
   repeat
      for member in struct.Members do begin
         if not UnicodeSameText(member.Name, methSym.Name) then continue;
         if not (member is TMethodSymbol) then continue;
         locSym:=TMethodSymbol(member);
         if methSym.IsSameOverloadOf(locSym) then
            Exit(locSym);
      end;
      struct:=struct.Parent;
   until (not recurse) or (struct=nil) or (not locSym.IsOverloaded);
   Result:=nil;
end;

// ReadFunc
//
function TdwsCompiler.ReadFunc(funcSym : TFuncSymbol; codeExpr : TDataExpr = nil;
                               expecting : TTypeSymbol = nil;
                               overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   funcExpr : TFuncExprBase;
begin
   funcExpr:=GetFuncExpr(funcSym, codeExpr);
   Result:=WrapUpFunctionRead(funcExpr, expecting, overloads);

   if funcSym.IsExternal then
      funcSym.Executable:=TExternalFuncHandler.Create;

   if Optimize then
      Result:=Result.OptimizeToTypedExpr(FProg, FExec, Result.ScriptPos);
end;

// WrapUpFunctionRead
//
function TdwsCompiler.WrapUpFunctionRead(funcExpr : TFuncExprBase; expecting : TTypeSymbol = nil;
                                         overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   argPosArray : TScriptPosArray;
begin
   Result:=funcExpr;
   try
      if FTok.Test(ttBLEFT) then begin
         ReadFuncArgs(funcExpr, argPosArray, overloads);
         if overloads<>nil then begin
            if not ResolveOverload(funcExpr, overloads, argPosArray) then Exit;
            Result:=funcExpr;
         end;
         TypeCheckArgs(funcExpr, argPosArray);
      end else begin
         if    (    (expecting is TNilSymbol)
                and (funcExpr is TFuncPtrExpr)
                and not FTok.Test(ttDOT))
            or (    (expecting is TFuncSymbol)
                and expecting.IsCompatible(funcExpr.funcSym)) then begin
            if (funcExpr.FuncSym.Level>1) and not (coAllowClosures in Options) then
               FMsgs.AddCompilerError(funcExpr.Pos, CPE_LocalFunctionAsDelegate);
            Result:=TFuncRefExpr.Create(FProg, funcExpr);
         end else begin
            if overloads<>nil then begin
               if not ResolveOverload(funcExpr, overloads, argPosArray) then Exit;
               Result:=funcExpr;
            end;
            TypeCheckArgs(funcExpr, nil);
         end;
      end;
      WarnDeprecatedFunc(funcExpr);
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
      if not (funcKind in [fkFunction, fkMethod, fkLambda]) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NoResultTypeExpected);
      Result:=ReadType('', tcResult);
   end else if funcKind=fkFunction then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionTypeExpected);
      Result:=FProg.TypVariant;
   end;
end;

// ReadFuncArgs
//
procedure TdwsCompiler.ReadFuncArgs(funcExpr : TFuncExprBase; var argPosArray : TScriptPosArray;
                                    overloads : TFuncSymbolList);

   procedure ReadOverloaded;
   var
      helper : TFuncExprOverloadsHelper;
   begin
      helper:=TFuncExprOverloadsHelper.Create(funcExpr, overloads);
      try
         ReadArguments(funcExpr.AddArg, ttBLEFT, ttBRIGHT, argPosArray, helper.ExpectedArg)
      finally
         helper.Free;
      end;
   end;

begin
   if (overloads<>nil) and (overloads.Count>1) then
      ReadOverloaded
   else ReadArguments(funcExpr.AddArg, ttBLEFT, ttBRIGHT, argPosArray, funcExpr.ExpectedArg);
end;

// TypeCheckArgs
//
procedure TdwsCompiler.TypeCheckArgs(funcExpr : TFuncExprBase; const argPosArray : TScriptPosArray);

   procedure WrongArgumentError(const argPos : TScriptPos; n : Integer; typ : TTypeSymbol);
   begin
      FMsgs.AddCompilerErrorFmt(argPos, CPE_WrongArgumentType, [n, typ.Caption]);
   end;

   procedure WrongArgumentLongError(const argPos : TScriptPos; n : Integer; typ1, typ2 : TTypeSymbol);
   begin
      FMsgs.AddCompilerErrorFmt(argPos, CPE_WrongArgumentType_Long,
                                [n, typ1.Caption, typ2.Caption]);
   end;

var
   arg : TTypedExpr;
   x, paramCount, nbParamsToCheck : Integer;
   funcSym : TFuncSymbol;
   paramSymbol : TParamSymbol;
   argTyp : TTypeSymbol;
   initialErrorCount : Integer;
   tooManyArguments, tooFewArguments : Boolean;
   argPos : TScriptPos;
begin
   funcSym:=funcExpr.FuncSym;

   paramCount:=funcSym.Params.Count;

   initialErrorCount:=FMsgs.Count;

   // Check number of arguments = number of parameters
   if funcExpr.Args.Count>paramCount then begin
      tooManyArguments:=True;
      while funcExpr.Args.Count>paramCount do begin
         funcExpr.Args.ExprBase[funcExpr.Args.Count-1].Free;
         funcExpr.Args.Delete(funcExpr.Args.Count-1);
      end;
   end else tooManyArguments:=False;

   tooFewArguments:=False;
   while funcExpr.Args.Count<paramCount do begin
      // Complete missing args by default values
      paramSymbol:=TParamSymbol(funcSym.Params[funcExpr.Args.Count]);
      if paramSymbol is TParamSymbolWithDefaultValue then
         funcExpr.Args.Add(TConstExpr.CreateTyped(FProg, paramSymbol.Typ,
                                          TParamSymbolWithDefaultValue(paramSymbol).DefaultValue))
      else begin
         tooFewArguments:=True;
         Break;
      end;
   end;

   if paramCount<funcExpr.Args.Count then
      nbParamsToCheck:=paramCount
   else nbParamsToCheck:=funcExpr.Args.Count;

   for x:=0 to nbParamsToCheck-1 do begin
      arg:=TTypedExpr(funcExpr.Args.ExprBase[x]);
      paramSymbol:=TParamSymbol(funcSym.Params[x]);
      if x<Length(argPosArray) then
         argPos:=argPosArray[x]
      else argPos:=funcExpr.Pos;

      if arg.ClassType=TArrayConstantExpr then
         TArrayConstantExpr(arg).Prepare(FProg, paramSymbol.Typ.Typ);

      argTyp:=arg.Typ;
      // Wrap-convert arguments if necessary and possible
      if paramSymbol.ClassType<>TVarParamSymbol then begin
         arg:=TConvExpr.WrapWithConvCast(FProg, argPos, paramSymbol.Typ, arg, False);
      end;
      funcExpr.Args.ExprBase[x]:=arg;

      if argTyp=nil then
         WrongArgumentError(argPos, x, paramSymbol.Typ)
      else if not paramSymbol.Typ.IsCompatible(arg.Typ) then
         WrongArgumentLongError(argPos, x, paramSymbol.Typ, arg.Typ)
      else if paramSymbol.ClassType=TVarParamSymbol then begin
         if not paramSymbol.Typ.IsOfType(arg.Typ) then
            WrongArgumentLongError(argPos, x, paramSymbol.Typ, argTyp);
         if arg is TDataExpr then begin
            if     (coVariablesAsVarOnly in Options)
               and (not (arg is TVarExpr))
               and (not (argTyp.UnAliasedType.ClassType=TRecordSymbol))
               and (   (x>0)
                    or (not (funcSym is TMethodSymbol))
                    or (not (TMethodSymbol(funcSym).StructSymbol is TRecordSymbol))
                    or TMethodSymbol(funcSym).IsClassMethod
                    ) then
               FMsgs.AddCompilerError(argPos, CPE_OnlyVariablesAsVarParam)
            // Record methods ignore the IsWritable constraints, as in Delphi
            else if     (not TDataExpr(arg).IsWritable)
                    and (   (x>0)
                         or (not (funcSym is TMethodSymbol))
                         or (not (TMethodSymbol(funcSym).StructSymbol is TRecordSymbol))) then
               FMsgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [x, paramSymbol.Name]);
         end else FMsgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [x, paramSymbol.Name]);
      end;

   end;

   if initialErrorCount=FMsgs.Count then begin
      if tooManyArguments then
         FMsgs.AddCompilerError(funcExpr.Pos, CPE_TooManyArguments);
      if tooFewArguments then
         FMsgs.AddCompilerError(funcExpr.Pos, CPE_TooFewArguments);
   end;

   if not FMsgs.HasErrors then
      funcExpr.Initialize(FProg);
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

            if (argSym<>nil) and (argSym.ClassType=TVarParamSymbol) then
               arg:=ReadTerm(True, expectedType)
            else arg:=ReadExpr(expectedType);

            if Optimize then
               arg:=arg.OptimizeToTypedExpr(FProg, FExec, argPos);

            if     (expectedType<>nil)
               and (arg.Typ is TFuncSymbol)
               and not (expectedType is TFuncSymbol) then begin
               arg:=ReadFunc(TFuncSymbol(arg.Typ), arg as TDataExpr, nil);
            end;

            AddArgProc(arg);
            n:=Length(argPosArray);
            SetLength(argPosArray, n+1);
            argPosArray[n]:=argPos;

            if (argSym<>nil) and (argSym.ClassType=TVarParamSymbol) and (arg is TVarExpr) then
               WarnForVarUsage(TVarExpr(arg), argPos);
         until not (FTok.TestDelete(ttCOMMA) and FTok.HasTokens);
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

   function CheckBound(bound : TTypedExpr) : Boolean;
   begin
      Result:=False;
      if    (bound.typ=nil)
         or not (   bound.Typ.IsOfType(FProg.TypInteger)
                 or (bound.Typ is TEnumerationSymbol)
                 or bound.Typ.IsOfType(FProg.TypBoolean)) then
         FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundNotOrdinal)
      else if not bound.IsConstant then
         FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundNotAConstant)
      else Result:=True;
   end;

var
   x : Integer;
   min, max : TTypedExprList;
   typ : TTypeSymbol;
   boundsOk : Boolean;
   lowBound : TTypedExpr;
   enumSymbol : TEnumerationSymbol;
begin
   boundsOk:=True;
   min:=TTypedExprList.Create;
   max:=TTypedExprList.Create;
   try

      if FTok.TestDelete(ttALEFT) then begin

         repeat
            // Lower bound
            hotPos:=FTok.HotPos;
            lowBound:=ReadExpr(FAnyTypeSymbol);

            if lowBound is TTypeReferenceExpr then begin

               // handle "array [TEnum] of" special case

               if TTypeReferenceExpr(lowBound).Typ is TBaseBooleanSymbol then begin

                  min.Insert0(TConstExpr.CreateBooleanValue(FProg, False));
                  max.Insert0(TConstExpr.CreateBooleanValue(FProg, True));

               end else begin

                  if not (TTypeReferenceExpr(lowBound).Typ is TEnumerationSymbol) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBoundNotOrdinal);
                  enumSymbol:=TEnumerationSymbol(TTypeReferenceExpr(lowBound).Typ);

                  min.Insert0(TConstExpr.CreateIntegerValue(FProg, enumSymbol, enumSymbol.LowBound));
                  max.Insert0(TConstExpr.CreateIntegerValue(FProg, enumSymbol, enumSymbol.HighBound));

               end;
               lowBound.Free;

            end else begin

               // handle "array [low..high] of" normal case

               min.Insert0(lowBound);

               boundsOk:=boundsOK and CheckBound(min[0]);

               if not FTok.TestDelete(ttDOTDOT) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotDotExpected);

               // Upper bound
               hotPos:=FTok.HotPos;
               max.Insert0(ReadExpr);

               boundsOk:=boundsOK and CheckBound(max[0]);

               if max[0].Typ<>min[0].Typ then
                  FMsgs.AddCompilerError(hotPos, CPE_ArrayBoundsOfDifferentTypes);

               if boundsOk and (max[0].EvalAsInteger(FExec)<min[0].EvalAsInteger(FExec)) then begin
                  FMsgs.AddCompilerError(hotPos, CPE_LowerBoundGreaterThanUpperBound);
                  // keep compiling
                  max[0].Free;
                  max[0]:=TConstExpr.CreateIntegerValue(FProg, min[0].EvalAsInteger(FExec));
               end;

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
         if min.Count>0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NoIndicesExpectedForOpenArray);

         Result := TOpenArraySymbol.Create(TypeName, FProg.TypVariant, FProg.TypInteger);

      end else begin

         typ:=ReadType('', typeContext);

         if boundsOk and (min.Count>0) then begin
            // initialize innermost array
            Result:=TStaticArraySymbol.Create('', typ, min[0].Typ,
                                                min[0].EvalAsInteger(FExec),
                                                max[0].EvalAsInteger(FExec));
            try
               // add outer arrays
               for x:=1 to min.Count - 1 do begin
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
function TdwsCompiler.ReadArrayConstant(closingToken : TTokenType;
                                        expecting : TTypeSymbol) : TArrayConstantExpr;
begin
   Result:=TArrayConstantExpr.Create(FProg, FTok.HotPos);
   try
      if not FTok.TestDelete(closingToken) then begin
         // At least one argument was found
         repeat
            TArrayConstantExpr(Result).AddElementExpr(FProg, ReadExpr);
         until not FTok.TestDelete(ttCOMMA);

         if not FTok.TestDelete(closingToken) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end else begin
         // empty array
         (Result.Typ as TStaticArraySymbol).Typ:=FProg.TypVariant;
      end;

      if expecting is TOpenArraySymbol then
         (Result.Typ as TStaticArraySymbol).Typ:=FProg.TypVariant
      else Result.TypeCheckElements(FProg);
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
                                      baseExpr : TTypedExpr) : TProgramExpr;
var
   arraySym : TArraySymbol;
   argList : TTypedExprList;
   argPosArray : TScriptPosArray;
   argSymTable : TUnSortedSymbolTable;
//   delegateSym : TFuncSymbol;
   i : Integer;

   procedure CheckNotTypeReference;
   begin
      if baseExpr.ClassType=TTypeReferenceExpr then
         FMsgs.AddCompilerError(namePos, RTE_ArrayInstanceExpected);
   end;

   procedure CheckRestricted;
   begin
      if arraySym.ClassType<>TDynamicArraySymbol then
         FMsgs.AddCompilerErrorFmt(namePos, CPE_ArrayMethodRestrictedToDynamicArrays, [name])
      else CheckNotTypeReference;
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
   argSymTable:=nil;
   argList:=TTypedExprList.Create;
   try
      arraySym:=baseExpr.Typ as TArraySymbol;

      if UnicodeSameText(name, 'add') or UnicodeSameText(name, 'push') then begin

         argList.DefaultExpected:=TParamSymbol.Create('', arraySym.Typ)

//      else if UnicodeSameText(name, 'sort') then begin
//
//         delegateSym:=TFuncSymbol.Create('', fkFunction, 0);
//         delegateSym.Typ:=FProg.TypInteger;
//         delegateSym.AddParam(TConstParamSymbol.Create('item1', arraySym.Typ));
//         delegateSym.AddParam(TConstParamSymbol.Create('item2', arraySym.Typ));
//         argList.DefaultExpected:=TParamSymbol.Create('', delegateSym);

      end else if UnicodeSameText(name, 'indexof') then begin

         argSymTable:=TUnSortedSymbolTable.Create;
         argSymTable.AddSymbol(TParamSymbol.Create('', arraySym.Typ));
         argList.Table:=argSymTable;

      end;

      ReadArguments(argList.AddExpr, ttBLEFT, ttBRIGHT, argPosArray, argList.ExpectedArg);

      try
         if UnicodeSameText(name, 'low') then begin

            CheckArguments(0, 0);
            Result:=CreateArrayLow(baseExpr, arraySym, True);

         end else if UnicodeSameText(name, 'high') then begin

            CheckArguments(0, 0);
            if not (arraySym is TStaticArraySymbol) then
               CheckNotTypeReference;
            Result:=CreateArrayHigh(baseExpr, arraySym, True);

         end else if UnicodeSameText(name, 'length') or UnicodeSameText(name, 'count') then begin

            CheckArguments(0, 0);
            CheckNotTypeReference;
            Result:=CreateArrayLength(baseExpr, arraySym);

         end else if UnicodeSameText(name, 'add') or UnicodeSameText(name, 'push') then begin

            CheckRestricted;
            if CheckArguments(1, 99) then begin
               for i:=0 to argList.Count-1 do begin
                  if    (argList[i].Typ=nil)
                     or not (   arraySym.Typ.IsCompatible(argList[i].Typ)
                             or arraySym.IsCompatible(argList[i].Typ)
                             or (    (argList[i].Typ is TStaticArraySymbol)
                                 and (   arraySym.Typ.IsCompatible(argList[i].Typ.Typ)
                                      or (argList[i].Typ.Size=0)))) then begin
                     IncompatibleTypes(argPosArray[i], CPE_IncompatibleParameterTypes,
                                       arraySym.Typ, argList[i].Typ);
                     Break;
                  end else if argList[i].ClassType=TArrayConstantExpr then begin
                     TArrayConstantExpr(argList[i]).Prepare(FProg, arraySym.Typ);
                  end;
               end;
               Result:=TArrayAddExpr.Create(FProg, namePos, baseExpr, argList);
               argList.Clear;
            end else Result:=TArrayAddExpr.Create(FProg, namePos, baseExpr, argList);

         end else if UnicodeSameText(name, 'pop') then begin

            CheckRestricted;
            CheckArguments(0, 0);
            Result:=TArrayPopExpr.Create(FProg, namePos, baseExpr);

         end else if UnicodeSameText(name, 'peek') then begin

            CheckRestricted;
            CheckArguments(0, 0);
            Result:=TArrayPeekExpr.Create(FProg, namePos, baseExpr);

         end else if UnicodeSameText(name, 'delete') then begin

            CheckRestricted;
            if CheckArguments(1, 2) then begin
               if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               if argList.Count>1 then begin
                  if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FProg.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                  Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr,
                                                  argList[0], argList[1]);
               end else Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr,
                                                        argList[0], nil);
               argList.Clear;
            end else Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr, nil, nil);

         end else if UnicodeSameText(name, 'indexof') then begin

            CheckRestricted;
            if CheckArguments(1, 2) then begin
               if (argList[0].Typ=nil) or not arraySym.Typ.IsCompatible(argList[0].Typ) then
                  IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                                    arraySym.Typ, argList[0].Typ);
               if argList.Count>1 then begin
                  if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FProg.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  Result:=TArrayIndexOfExpr.Create(FProg, namePos, baseExpr,
                                                   argList[0] as TDataExpr, argList[1]);
               end else Result:=TArrayIndexOfExpr.Create(FProg, namePos, baseExpr,
                                                         argList[0] as TDataExpr, nil);
               argList.Clear;
            end else Result:=TArrayIndexOfExpr.Create(FProg, namePos, baseExpr, nil, nil);

         end else if UnicodeSameText(name, 'insert') then begin

            CheckRestricted;
            if CheckArguments(2, 2) then begin
               if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               if (argList[1].Typ=nil) or not arraySym.Typ.IsCompatible(argList[1].Typ) then
                  IncompatibleTypes(argPosArray[1], CPE_IncompatibleParameterTypes,
                                    arraySym.Typ, argList[1].Typ);
               Result:=TArrayInsertExpr.Create(FProg, namePos, baseExpr,
                                               argList[0], argList[1] as TDataExpr);
               argList.Clear;
            end else Result:=TArrayDeleteExpr.Create(FProg, namePos, baseExpr, nil, nil);

         end else if UnicodeSameText(name, 'setlength') then begin

            CheckRestricted;
            if CheckArguments(1, 1) then begin
               if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               Result:=TArraySetLengthExpr.Create(FProg, namePos, baseExpr, argList[0]);
               argList.Clear;
            end else Result:=TArraySetLengthExpr.Create(FProg, namePos, baseExpr, nil);

         end else if UnicodeSameText(name, 'clear') then begin

            CheckRestricted;
            CheckArguments(0, 0);
            Result:=TArraySetLengthExpr.Create(FProg, namePos, baseExpr, TConstIntExpr.CreateIntegerValue(FProg, 0));

         end else if UnicodeSameText(name, 'swap') then begin

            CheckRestricted;
            if CheckArguments(2, 2) then begin
               if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
               if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FProg.TypInteger) then
                  FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
               Result:=TArraySwapExpr.Create(FProg, namePos, baseExpr,
                                             argList[0], argList[1]);
               argList.Clear;
            end else Result:=TArraySwapExpr.Create(FProg, namePos, baseExpr, nil, nil);

         end else if UnicodeSameText(name, 'copy') then begin

            CheckRestricted;
            if CheckArguments(0, 2) then begin
               if argList.Count>0 then begin
                  if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FProg.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  if argList.Count>1 then begin
                     if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FProg.TypInteger) then
                        FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                     Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr,
                                                   argList[0], argList[1]);
                  end else Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr,
                                                         argList[0], nil);
               end else Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr, nil, nil);
               argList.Clear;
            end else Result:=TArrayCopyExpr.Create(FProg, namePos, baseExpr, nil, nil);

//         end else if UnicodeSameText(name, 'sort') then begin
//
//            CheckRestricted;
//            if CheckArguments(1, 1) then begin
//               if not argList[0].Typ.IsOfType(argList.DefaultExpected.Typ) then
//                  IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
//                                    argList.DefaultExpected.Typ, argList[0].Typ);
//               Result:=TArraySortExpr.Create(FProg, namePos, baseExpr, argList[0]);
//               argList.Clear;
//            end else Result:=TArraySortExpr.Create(FProg, namePos, baseExpr, nil);

         end else if UnicodeSameText(name, 'reverse') then begin

            CheckRestricted;
            CheckArguments(0, 0);
            Result:=TArrayReverseExpr.Create(FProg, namePos, baseExpr);

         end else FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);
      except
         Result.Free;
         raise;
      end;
   finally
      argSymTable.Free;
      argList.Free;
   end;
end;

// ReadNameList
//
procedure TdwsCompiler.ReadNameList(names : TStrings; var posArray : TScriptPosArray;
                                    const options : TdwsNameListOptions = []);
var
   n : Integer;
begin
   n:=0;
   names.Clear;
   repeat
      if not FTok.TestName then begin
         if not ((nloAllowStrings in Options) and FTok.Test(ttStrVal)) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      end;

      if n=Length(posArray) then
         SetLength(posArray, n+1);
      posArray[n]:=FTok.HotPos;
      Inc(n);

      names.Add(FTok.GetToken.FString);
      if not (nloNoCheckSpecials in options) then
         CheckSpecialName(FTok.GetToken.FString);
      FTok.KillToken;

      while (nloAllowDots in options) and FTok.TestDelete(ttDOT) do begin
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
         names[names.Count-1]:=names[names.Count-1]+'.'+FTok.GetToken.FString;
         FTok.KillToken;
      end;
   until not FTok.TestDelete(ttCOMMA);
end;

// ReadExternalName
//
procedure TdwsCompiler.ReadExternalName(funcSym : TFuncSymbol);
begin
   FTok.KillToken;
   if not FTok.Test(ttSEMI) then begin
      if not FTok.Test(ttStrVal) then begin
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_StringExpected);
      end else begin
         funcSym.ExternalName:=FTok.GetToken.FString;
         FTok.KillToken;
      end;
   end;
   ReadSemiColon;
end;

// ReadNew
//
function TdwsCompiler.ReadNew(restrictTo : TClassSymbol) : TProgramExpr;
var
   sym : TSymbol;
   typSym : TTypeSymbol;
   classSym : TClassSymbol;
   methSym : TMethodSymbol;
   nameToken : TToken;
   hotPos : TScriptPos;
   typedExpr : TTypedExpr;
   baseExpr : TDataExpr;
   argPosArray : TScriptPosArray;
   overloads : TFuncSymbolList;
   i : Integer;
   funcExpr : TFuncExprBase;
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

         if Assigned(sym) and sym.IsType then begin

            typSym:=TTypeSymbol(sym);
            RecordSymbolUse(typSym, hotPos, [suReference]);
            Result:=ReadNewArray(typSym);

         end else begin

            FMsgs.AddCompilerError(hotPos, CPE_TypeExpected);
            Result:=ReadNewArray(FProg.TypVariant);

         end;
         Exit;

      end else if sym is TClassSymbol then begin

         classSym:=TClassSymbol(sym);
         RecordSymbolUse(classSym, hotPos, [suReference]);

      end else if (sym is TDataSymbol) and (sym.Typ is TClassOfSymbol) then begin

         classSym:=TClassOfSymbol(sym.Typ).TypClassSymbol;
         RecordSymbolUseReference(sym, hotPos, False);

      end else FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);

      if sym is TClassSymbol then
         baseExpr:=TConstExpr.CreateTypedVariantValue(FProg, classSym.MetaSymbol, Int64(classSym))
      else baseExpr:=TVarExpr.CreateTyped(FProg, TDataSymbol(sym));

   end;

   if classSym.IsStatic then
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_ClassIsStatic, [classSym.Name]);
   if (restrictTo<>nil) and not classSym.IsOfType(restrictTo) then
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustBeSubClassOf, [restrictTo.Name]);

   methSym:=classSym.FindDefaultConstructor(cvPrivate);
   if methSym.IsOverloaded then
      overloads:=TFuncSymbolList.Create
   else overloads:=nil;
   try
      if overloads<>nil then begin
         CollectMethodOverloads(methSym, overloads);
         for i:=overloads.Count-1 downto 0 do
            if overloads[i].Kind<>fkConstructor then
               overloads.Extract(i);
      end;

      RecordSymbolUseImplicitReference(methSym, hotPos, False);

      Result:=nil;
      try
         Result:=GetMethodExpr(methSym, baseExpr, rkClassOfRef, hotPos, False);
      except
         baseExpr.Free;
         raise;
      end;
      try
         ReadFuncArgs(TFuncExpr(Result), argPosArray, overloads);
         (Result as TMethodExpr).Typ:=classSym;
         if overloads<>nil then begin
            funcExpr:=(Result as TFuncExpr);
            if not ResolveOverload(funcExpr, overloads, argPosArray) then Exit;
            Result:=funcExpr;
         end;
         TypeCheckArgs(TFuncExpr(Result), argPosArray);
      except
         Result.Free;
         raise;
      end;
   finally
      overloads.Free;
   end;
end;

// ReadNewArray
//
function TdwsCompiler.ReadNewArray(elementTyp : TTypeSymbol) : TNewArrayExpr;
var
   lengthExpr : TTypedExpr;
   hotPos : TScriptPos;
   newExpr : TNewArrayExpr;
begin
   newExpr:=TNewArrayExpr.Create(FProg, FTok.HotPos, elementTyp);
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

// ReadNameSymbol
//
function TdwsCompiler.ReadNameSymbol(var namePos : TScriptPos) : TSymbol;
var
   name : String;
   unitSym : TUnitSymbol;
begin
   // Declaration of a class reference
   if not FTok.TestDeleteNamePos(name, namePos) then begin
      namePos:=FTok.HotPos;
      FMsgs.AddCompilerError(namePos, CPE_NameExpected);
      Result:=nil;
      Exit;
   end;

   Result:=FProg.Table.FindTypeSymbol(name, cvMagic);

   if Result=nil then begin

      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnknownName, [name]);

   end else if (Result.BaseType<>nil) and (Result.BaseType.ClassType=TUnitSymbol) then begin

      RecordSymbolUse(Result, namePos, [suReference]);

      unitSym:=TUnitSymbol(Result.BaseType);
      unitSym:=ResolveUnitNameSpace(unitSym);

      namePos:=FTok.HotPos;   // reuse token pos variable
      Result:=unitSym.Table.FindLocal(FTok.GetToken.FString);

      if not Assigned(Result) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnknownNameDotName,
                                   [unitSym.Name, FTok.GetToken.FString]);

      FTok.KillToken;
   end;
end;

// ReadClassName
//
function TdwsCompiler.ReadClassName : TClassSymbol;
var
   namePos : TScriptPos;
   sym : TSymbol;
begin
   sym:=ReadNameSymbol(namePos);

   if not (sym is TClassSymbol) then begin
      if Assigned(sym) then
         FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAClass, [sym.Name]);
      Result:=FProg.TypObject; // keep compiling
   end else begin
      Result:=TClassSymbol(sym);
      RecordSymbolUse(Result, namePos, [suReference]);
   end;
end;

// ReadClassOf
//
function TdwsCompiler.ReadClassOf(const typeName : String) : TClassOfSymbol;
var
   classTyp : TClassSymbol;
begin
   // Declaration of a class reference
   classTyp:=ReadClassName;

   if typeName<>'' then
      Result:=TClassOfSymbol.Create(typeName, classTyp)
   else Result:=(classTyp.MetaSymbol as TClassOfSymbol);
end;

// ReadClass
//
function TdwsCompiler.ReadClass(const typeName : String; const flags : TClassSymbolFlags) : TClassSymbol;
var
   namePos, hotPos : TScriptPos;
   sym, typ : TSymbol;
   propSym : TPropertySymbol;
   ancestorTyp : TClassSymbol;
   intfTyp : TInterfaceSymbol;
   interfaces : TList;
   missingMethod : TMethodSymbol;
   isInSymbolTable : Boolean;
   previousClassFlags  : TClassSymbolFlags;
   visibility : TdwsVisibility;
   tt : TTokenType;
   i : Integer;
begin
   // Check for a forward declaration of this class
   sym:=FProg.Table.FindTypeLocal(typeName);
   Result:=nil;

   if Assigned(sym) then begin
      if sym is TClassSymbol then begin
         Result:=TClassSymbol(sym);
         if Result.IsForwarded or Result.IsPartial then
            Result:=TClassSymbol(sym)
         else begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassAlreadyDefined, [sym.Name]);
            Result:=nil;
         end;
      end else begin
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NameAlreadyExists, [sym.Name]);
      end;
      if Result=nil then // make anonymous to keep compiling
         Result:=TClassSymbol.Create('', CurrentUnitSymbol);
   end;

   isInSymbolTable:=Assigned(Result);

   if not Assigned(Result) then begin
      Result:=TClassSymbol.Create(typeName, CurrentUnitSymbol);
      previousClassFlags:=[];
   end else previousClassFlags:=Result.Flags;

   // forwarded declaration
   if FTok.Test(ttSEMI) then begin
      if Result.IsForwarded then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassForwardAlreadyExists, [sym.Name])
      else if csfPartial in flags then
         Result.SetIsPartial;
      Result.SetForwardedPos(FTok.HotPos);
      Exit;
   end else Result.ClearIsForwarded;

   if not isInSymbolTable then
      FProg.Table.AddSymbol(Result);   // auto-forward
   interfaces:=TList.Create;
   try
      try
         if FTok.TestDelete(ttSTATIC) or (csfStatic in flags) then begin
            Result.IsStatic:=True;
         end;
         tt:=FTok.TestDeleteAny([ttABSTRACT, ttSEALED]);
         case tt of
            ttABSTRACT :
               Result.IsExplicitAbstract:=True;
            ttSEALED :
               Result.IsSealed:=True;
         end;
         if FTok.TestDelete(ttEXTERNAL) then begin
            Result.IsExternal:=True;
            if FTok.Test(ttStrVal) then begin
               Result.ExternalName:=FTok.GetToken.FString;
               FTok.KillToken;
            end;
         end;
         if FTok.TestDelete(ttPARTIAL) or (csfPartial in flags) then begin
            Result.SetIsPartial;
            if isInSymbolTable then begin
               if not (csfPartial in previousClassFlags) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassWasNotPartial)
               else if previousClassFlags<>Result.Flags then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassPartialModifiersNotMatched);
            end;
         end;

         // inheritance
         if FTok.TestDelete(ttBLEFT) then begin

            typ:=ReadNameSymbol(namePos);

            if not (typ is TClassSymbol) then begin
               if typ is TInterfaceSymbol then
                  interfaces.Add(typ)
               else if typ<>nil then
                  FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAClass, [typ.name]);
               typ:=FProg.TypObject;
            end;
            RecordSymbolUse(typ, namePos, [suReference]);

            ancestorTyp:=TClassSymbol(typ);

            if ancestorTyp.IsForwarded or (ancestorTyp=Result) then begin
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassNotCompletelyDefined, [ancestorTyp.Name]);
               ancestorTyp:=FProg.TypObject;
            end;

            if ancestorTyp.IsSealed then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassIsSealed, [ancestorTyp.Name]);

            while FTok.TestDelete(ttCOMMA) do begin

               typ:=ReadNameSymbol(namePos);
               if not (typ is TInterfaceSymbol) then begin

                  if typ<>nil then
                     FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAnInterface, [typ.Name]);

               end else begin

                  intfTyp:=TInterfaceSymbol(typ);
                  if intfTyp.IsForwarded then
                     FMsgs.AddCompilerErrorFmt(namePos, CPE_InterfaceNotCompletelyDefined, [typ.Name]);
                  if interfaces.IndexOf(intfTyp)>=0 then
                     FMsgs.AddCompilerErrorFmt(namePos, CPE_InterfaceAlreadyImplemented, [typ.Name])
                  else interfaces.Add(intfTyp);

               end;
            end;

            if not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

         end else begin

            if csfPartial in previousClassFlags then
               ancestorTyp:=Result.Parent
            else ancestorTyp:=FProg.TypObject;

         end;

         if     Result.IsStatic
            and (ancestorTyp<>FProg.TypObject)
            and (not ancestorTyp.IsStatic) then begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassAncestorNotStatic, [ancestorTyp.Name]);
         end;

         if Result.Parent<>nil then begin
            if ancestorTyp<>Result.Parent then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassAncestorDoesNotMatch);
         end else if Result.Parent<>ancestorTyp then begin
            if (not Result.IsExternal) or (ancestorTyp.IsExternal) then
               Result.InheritFrom(ancestorTyp);
         end;

         visibility:=cvPublished;

         // standard class definition
         if not FTok.Test(ttSEMI) then begin
            while not FTok.Test(ttEND) do begin

               // Read methods and properties
               hotPos:=FTok.HotPos;
               tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
                                       ttCONSTRUCTOR, ttDESTRUCTOR,
                                       ttCLASS, ttPROPERTY, ttCONST,
                                       ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED]);
               case tt of

                  ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR :
                     ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, False);

                  ttCLASS : begin

                     tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttOPERATOR, ttVAR, ttCONST]);
                     case tt of
                        ttPROCEDURE, ttFUNCTION, ttMETHOD :
                           ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, True);
                        ttOPERATOR :
                           Result.AddOperator(ReadClassOperatorDecl(Result));
                        ttVAR :
                           ReadClassVars(Result, visibility);
                        ttCONST :
                           ReadClassConst(Result, visibility);
                     else
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
                     end;

                  end;
                  ttPROPERTY : begin

                     propSym:=ReadPropertyDecl(Result, visibility);
                     Result.AddProperty(propSym);

                  end;
                  ttCONST : begin

                     ReadClassConst(Result, visibility);

                  end;
                  ttPRIVATE..ttPUBLISHED : begin

                     if visibility=cTokenToVisibility[tt] then
                        FMsgs.AddCompilerHintFmt(FTok.HotPos, CPH_RedundantVisibilitySpecifier, [cTokenStrings[tt]], hlStrict)
                     else visibility:=cTokenToVisibility[tt];

                  end;

               else

                  if FTok.TestName then begin
                     ReadFieldsDecl(Result, visibility, False);
                     if not (FTok.TestDelete(ttSEMI) or FTok.Test(ttEND)) then
                        Break;
                  end else Break;

               end;

            end; // while

            if not FTok.TestDelete(ttEND) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);

            CheckNoPendingAttributes;
         end;

         // resolve interface tables
         for i:=0 to interfaces.Count-1 do begin
            intfTyp:=interfaces[i];
            if not Result.AddInterface(intfTyp, cvPrivate, missingMethod) then
               FMsgs.AddCompilerErrorFmt(namePos, CPE_MissingMethodForInterface, [missingMethod.Name, intfTyp.Name]);
         end;
         Result.AddOverriddenInterfaces;

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

// ReadClassVars
//
procedure TdwsCompiler.ReadClassVars(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
var
   assignExpr : TNoResultExpr;
   factory : IdwsDataSymbolFactory;
begin
   factory:=TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
   assignExpr:=ReadVarDecl(factory);
   if assignExpr<>nil then
      FProg.InitExpr.AddStatement(assignExpr);
   ReadSemiColon;
end;

// ReadClassConst
//
procedure TdwsCompiler.ReadClassConst(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
var
   factory : IdwsDataSymbolFactory;
begin
   factory:=TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
   ReadConstDecl(factory);
   ReadSemiColon;
end;

// ReadInterface
//
function TdwsCompiler.ReadInterface(const typeName : String) : TInterfaceSymbol;
var
   sym : TSymbol;
   ancestor : TInterfaceSymbol;
   namePos, hotPos : TScriptPos;
   tt : TTokenType;
   propSym : TPropertySymbol;
   wasForwarded : Boolean;
begin
   hotPos:=FTok.HotPos;
   sym:=FProg.Table.FindSymbol(typeName, cvMagic);

   wasForwarded:=False;
   if Assigned(sym) then begin
      if sym is TInterfaceSymbol then begin
         Result:=TInterfaceSymbol(sym);
         if not Result.IsForwarded then begin
            Result:=nil;
            FMsgs.AddCompilerErrorFmt(hotPos, CPE_InterfaceAlreadyDefined, [sym.Name]);
         end;
      end else begin
         Result:=nil;
         FMsgs.AddCompilerErrorFmt(hotPos, CPE_NameAlreadyExists, [sym.Name]);
      end;
      if Result=nil then begin
         // keep compiling, make it anonymous
         Result:=TInterfaceSymbol.Create('', CurrentUnitSymbol);
      end;
   end else Result:=TInterfaceSymbol.Create(typeName, CurrentUnitSymbol);

   if FTok.Test(ttSEMI) then begin
      // forward declaration
      if Result.IsForwarded then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InterfaceForwardAlreadyExists, [sym.Name])
      else Result.SetForwardedPos(hotPos);
      Exit;
   end else Result.ClearIsForwarded;

   try
      // auto-forward
      if not wasForwarded then
         FProg.Table.AddSymbol(Result);
      try

         if FTok.TestDelete(ttBLEFT) then begin

            sym:=ReadNameSymbol(namePos);

            if not (sym is TInterfaceSymbol) then begin

               if sym<>nil then
                  FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAnInterface, [sym.Name])

            end else begin

               ancestor:=TInterfaceSymbol(sym);
               if ancestor.IsForwarded then
                  msgs.AddCompilerErrorFmt(namePos, CPE_InterfaceNotCompletelyDefined, [ancestor.Name]);
               Result.InheritFrom(ancestor);

            end;
            if not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(namePos, CPE_BrackRightExpected);
         end;
         if Result.Parent=nil then
            Result.InheritFrom(FProg.TypInterface);

         while not FTok.Test(ttEND) do begin

            // Read methods and properties
            tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttPROPERTY]);
            case tt of
               ttFUNCTION, ttPROCEDURE, ttMETHOD :
                  Result.AddMethod(ReadIntfMethodDecl(Result, cTokenToFuncKind[tt]));

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
         CheckNoPendingAttributes;

      finally
         // remove auto-forward
         if not wasForwarded then
            FProg.Table.Remove(Result);
      end;
   except
      Result.Free;
      raise;
   end;
end;

// CheckPropertyFuncParams
//
function TdwsCompiler.CheckPropertyFuncParams(paramsA : TSymbolTable; methSym : TMethodSymbol;
                                      indexSym : TSymbol = nil;
                                      typSym : TTypeSymbol = nil) : Boolean;
var
   paramsB : TSymbolTable;
   skipB : Integer;
begin
   Result:=False;
   paramsB:=methSym.Params;

   if (paramsB.Count>0) and (paramsB[0]=methSym.SelfSym) then
      skipB:=1
   else skipB:=0;

   if Assigned(indexSym) then begin
      if Assigned(typSym) then begin
         if paramsB.Count<>paramsA.Count+skipB+2 then Exit;
         if paramsB[paramsA.Count+skipB+1].Typ<>typSym then Exit;
         if paramsB[paramsA.Count+skipB].Typ<>indexSym then Exit;
      end else begin
         if paramsB.Count<>paramsA.Count+skipB+1 then Exit
         else if paramsB[paramsA.Count+skipB].Typ<>indexSym then Exit;
      end;
   end else begin
      if Assigned(typSym) then begin
         if paramsB.Count<>paramsA.Count+skipB+1 then Exit;
         if paramsB[paramsA.Count+skipB].Typ<>typSym then Exit;
      end else begin
         if paramsA.Count+skipB<>paramsB.Count then Exit;
      end;
   end;

   Result:=CheckParams(paramsA, paramsB, False, skipB);
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
      RecordSymbolUse(sym, usesPos, [suReference]);

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
function TdwsCompiler.ReadPropertyDecl(ownerSym : TCompositeTypeSymbol; aVisibility : TdwsVisibility) : TPropertySymbol;
var
   x : Integer;
   name : String;
   sym : TSymbol;
   typ : TTypeSymbol;
   arrayIndices : TSymbolTable;
   propStartPos, propNamePos : TScriptPos;
   accessPos : TScriptPos;  // Position where either a Read or Write symbol is found
   indexExpr : TTypedExpr;
   indexTyp : TTypeSymbol;
   baseArrayIndices : Integer;
begin
   propStartPos:=FTok.HotPos;

   // Read property name
   if not FTok.TestDeleteNamePos(name, propNamePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   // Check if property name is free
   sym := ownerSym.Members.FindSymbolFromScope(name, CurrentStruct);
   if Assigned(sym) then begin
      if sym is TPropertySymbol then begin
         if TPropertySymbol(sym).OwnerSymbol = ownerSym then
            MemberSymbolWithNameAlreadyExists(sym, propNamePos);
      end else MemberSymbolWithNameAlreadyExists(sym, propNamePos);
   end;

   arrayIndices := TUnSortedSymbolTable.Create;
   try
      baseArrayIndices:=arrayIndices.Count;

      if FTok.TestDelete(ttALEFT) then
         ReadArrayParams(arrayIndices);

      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      typ := ReadType('', tcProperty);
      Result := TPropertySymbol.Create(name, typ, aVisibility);
      try
         RecordSymbolUse(Result, propNamePos, [suDeclaration]);

         if FTok.TestDelete(ttINDEX) then begin
            indexExpr:=ReadExpr;
            indexTyp:=indexExpr.Typ;
            if not (indexExpr is TConstExpr) then begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
            end else begin
               Result.SetIndex(TConstExpr(indexExpr).Data[FExec],
                               TConstExpr(indexExpr).Addr[FExec], indexTyp);
            end;
            indexExpr.Free;
         end else indexTyp:=nil;

         if FTok.TestDelete(ttREAD) then begin
            if not FTok.TestDeleteNamePos(name, accessPos) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

            sym := ownerSym.Members.FindSymbol(name, cvPrivate);

            if not Assigned(sym) or (sym is TPropertySymbol) then begin

               FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);

            end else if Result.Typ<>sym.Typ then

               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleType, [name])

            else if sym is TMethodSymbol then begin

               if not CheckPropertyFuncParams(arrayIndices, TMethodSymbol(sym), indexTyp) then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleParameters, [name]);

            end else if arrayIndices.Count>baseArrayIndices then begin

               FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionMethodExpected);

            end;

            Result.ReadSym := sym;
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddSymbol(sym, accessPos, [suReference, suRead])
         end;

         if FTok.TestDelete(ttWRITE) then begin
            // Read name
            if not FTok.TestDeleteNamePos(name, accessPos) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

            // Check if symbol exists
            sym := ownerSym.Members.FindSymbol(Name, cvPrivate);

            if not Assigned(sym) or (sym is TPropertySymbol) then begin

               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);

            end else if sym is TMethodSymbol then begin

               if    (not (TMethodSymbol(sym).Kind in [fkProcedure, fkMethod]))
                  or (TMethodSymbol(sym).Typ<>nil) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ProcedureMethodExpected)
               else if not CheckPropertyFuncParams(arrayIndices, TMethodSymbol(sym), indexTyp, Result.Typ) then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleParameters, [name]);

            end else if Result.Typ <> sym.Typ then begin

               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleWriteSymbol, [name]);

            end else if sym is TConstSymbol then  begin

               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConstantCannotBeWrittenTo, [name]);

            end;

            Result.WriteSym := sym;
            if coSymbolDictionary in FOptions then
               FSymbolDictionary.AddSymbol(sym, accessPos, [suReference, suWrite]);
         end;

         if (Result.ReadSym = nil) and (Result.WriteSym = nil) then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ReadOrWriteExpected, [name]);

         ReadSemiColon;

         // Add array indices to property symbol (if any)
         for x := 0 to arrayIndices.Count - 1 do
            Result.ArrayIndices.AddSymbol(arrayIndices[x]);
         arrayIndices.Clear;

         // Array-Prop can be default
         if Result.HasArrayIndices then begin
            if FTok.TestDelete(ttDEFAULT) then begin
               ReadSemiColon;
               if not ownerSym.AllowDefaultProperty then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_NoDefaultPropertyAllowed)
               else if ownerSym.DefaultProperty<>nil then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MultipleDefaultProperties,
                                            [ownerSym.Name, ownerSym.DefaultProperty.Name])
               else ownerSym.DefaultProperty:=Result;
            end;
         end;

         if FTok.TestDelete(ttDEPRECATED) then begin
            if FTok.Test(ttStrVal) then begin
               Result.DeprecatedMessage:=FTok.GetToken.FString;
               FTok.KillToken;
            end;
            if Result.DeprecatedMessage='' then
               Result.DeprecatedMessage:=MSG_DeprecatedEmptyMsg;
            ReadSemiColon;
         end;

         // register context only if we're sure the property symbol will survive
         if coContextMap in FOptions then begin
            FSourceContextMap.OpenContext(propStartPos, result, ttPROPERTY);
            FSourceContextMap.CloseContext(FTok.CurrentPos);
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

// ReadRecordDecl
//
function TdwsCompiler.ReadRecordDecl(const typeName : String;
                                     allowNonConstExpressions : Boolean) : TRecordSymbol;
var
   names : TStringList;
   propSym : TPropertySymbol;
   meth : TMethodSymbol;
   hotPos : TScriptPos;
   visibility : TdwsVisibility;
   tt : TTokenType;
begin
   Result:=TRecordSymbol.Create(typeName, CurrentUnitSymbol);
   try
      FProg.Table.AddSymbol(Result); // auto-forward
      names:=TStringList.Create;
      try
         if typeName='' then
            visibility:=cvPublished
         else visibility:=cvPublic;

         repeat

            hotPos:=FTok.HotPos;
            tt:=FTok.TestDeleteAny([ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED, ttCLASS,
                                    ttPROPERTY, ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONST]);
            case tt of
               ttPRIVATE, ttPUBLIC, ttPUBLISHED :
                  if visibility=cTokenToVisibility[tt] then
                     FMsgs.AddCompilerHintFmt(FTok.HotPos, CPH_RedundantVisibilitySpecifier, [cTokenStrings[tt]])
                  else visibility:=cTokenToVisibility[tt];
               ttPROTECTED :
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_NoProtectedVisibilityForRecords);
               ttPROPERTY : begin
                  propSym := ReadPropertyDecl(Result, visibility);
                  Result.AddProperty(propSym);
               end;
               ttFUNCTION, ttPROCEDURE, ttMETHOD : begin
                  meth:=ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, False);
                  if meth.IsForwarded then
                     FMsgs.AddCompilerError(hotPos, CPE_AnonymousRecordMethodsMustBeInline);
               end;
               ttCLASS : begin
                  tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttVAR, ttCONST]);
                  case tt of
                     ttPROCEDURE, ttFUNCTION, ttMETHOD : begin
                        meth:=ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, True);
                        if meth.IsForwarded then
                           FMsgs.AddCompilerError(hotPos, CPE_AnonymousRecordMethodsMustBeInline);
                     end;
                     ttVAR :
                        ReadClassVars(Result, visibility);
                     ttCONST :
                        ReadClassConst(Result, visibility);
                  else
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
                  end;
               end;
               ttCONST :
                  ReadClassConst(Result, visibility);
            else
               if FTok.Test(ttEND) then
                  Break;

               ReadFieldsDecl(Result, visibility, allowNonConstExpressions);

               if not FTok.TestDelete(ttSEMI) then
                  Break;
            end;
         until not FTok.HasTokens;
      finally
         names.Free;
         FProg.Table.Remove(Result);
      end;

      if not FTok.TestDelete(ttEND) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      if Result.Size=0 then
         FMsgs.AddCompilerError(FTok.HotPos, RTE_NoRecordFields);
      CheckNoPendingAttributes;

   except
      // Removed added record symbols. Destroying object
      if coSymbolDictionary in FOptions then
         FSymbolDictionary.Remove(Result);
      Result.Free;
      raise;
   end;
end;

// ReadFieldsDecl
//
procedure TdwsCompiler.ReadFieldsDecl(struct : TStructuredTypeSymbol; visibility : TdwsVisibility;
                                      allowNonConstExpressions : Boolean);
var
   x : Integer;
   names : TStringList;
   sym : TSymbol;
   member : TFieldSymbol;
   typ : TTypeSymbol;
   posArray : TScriptPosArray;
   expr : TTypedExpr;
   exprData : TData;
   exprDyn : TTypedExpr;
   detachTyp : Boolean;
   options : TdwsNameListOptions;
begin
   names:=TStringList.Create;
   try
      options:=[];
      if struct.Name='' then
         Include(options, nloAllowStrings);
      if struct.IsExternal then
         Include(options, nloNoCheckSpecials);
      ReadNameList(names, posArray, options);

      if FTok.TestDelete(ttCOLON) then
         typ:=ReadType('', tcConstant)
      else typ:=nil;

      exprDyn:=nil;
      if FTok.TestDeleteAny([ttEQ, ttASSIGN])<>ttNone then begin
         detachTyp:=False;
         expr:=ReadExpr(nil);
         try
            if Assigned(typ) then begin
               if not typ.IsCompatible(expr.typ) then
                  IncompatibleTypes(FTok.HotPos, CPE_AssignIncompatibleTypes, expr.typ, typ);
            end else begin
               typ:=expr.typ;
               detachTyp:=(typ.Name='');
            end;
            if typ=FProg.TypNil then
               if not (expr is TBogusConstExpr) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_TypeCouldNotBeInferenced);

            if (typ=nil) or (expr=nil) then begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
               if typ=nil then
                  typ:=FProg.TypVariant;
            end else if expr.IsConstant then begin
               if not FMsgs.HasErrors then begin
                  SetLength(exprData, typ.Size);
                  if typ.Size=1 then begin
                     expr.EvalAsVariant(FExec, exprData[0]);
                  end else begin
                     FExec.Stack.Push(typ.Size);
                     try
                        DWSCopyData((expr as TDataExpr).Data[FExec],
                                    (expr as TDataExpr).Addr[FExec],
                                    exprData, 0, typ.Size);
                     finally
                        FExec.Stack.Pop(typ.Size);
                     end;
                  end;
               end;
            end else if not allowNonConstExpressions then begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
            end else begin
               exprDyn:=expr;
               expr:=nil;
               detachTyp:=False;
            end;
         finally
            if detachTyp then begin
               if not FProg.Table.HasSymbol(typ) then
                  FProg.Table.AddSymbol(typ);
               expr.Typ:=nil;
            end;
            expr.Free;
         end;
      end else begin
         exprData:=nil;
         if typ=nil then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ColonExpected);
            typ:=FProg.TypVariant;
         end;
      end;

      member:=nil;
      for x:=0 to names.Count - 1 do begin
         sym:=struct.Members.FindLocal(names[x]);
         if Assigned(sym) then
            MemberSymbolWithNameAlreadyExists(sym, posArray[x]);

         member:=TFieldSymbol.Create(names[x], typ, visibility);
         if exprDyn<>nil then begin
            case x of
               0 : member.DefaultExpr:=exprDyn;
               1 : FMsgs.AddCompilerError(FTok.HotPos, CPE_OnlyOneFieldExpectedForExternal);
            end;
         end;
         struct.AddField(member);
         if exprData<>nil then
            member.DefaultValue:=exprData;

         // Add member symbols and positions
         RecordSymbolUse(member, posArray[x], [suDeclaration]);
      end;

      if FTok.TestDelete(ttSEMI) then begin

         if FTok.TestDelete(ttEXTERNAL) then begin

            if names.Count<>1 then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_OnlyOneFieldExpectedForExternal);

            if FTok.Test(ttStrVal) then begin
               if member<>nil then
                  member.ExternalName:=FTok.GetToken.FString;
               FTok.KillToken;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);

         end else FTok.SimulateToken(ttSEMI, FTok.HotPos);

      end;

   finally
      names.Free;
   end;
end;

// ReadHelperDecl
//
function TdwsCompiler.ReadHelperDecl(const typeName : String; qualifierToken : TTokenType) : THelperSymbol;
var
   propSym : TPropertySymbol;
   hotPos : TScriptPos;
   visibility : TdwsVisibility;
   tt : TTokenType;
   forType : TTypeSymbol;
begin
   if not FTok.TestDelete(ttFOR) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ForExpected);
   hotPos:=FTok.HotPos;
   forType:=ReadType('', tcHelper);
   if forType is TFuncSymbol then
      FMsgs.AddCompilerError(hotPos, CPE_HelpersNotAllowedForDelegates);
   case qualifierToken of
      ttNone : ;
      ttCLASS :
         if not ((forType is TClassSymbol) or (forType is TClassOfSymbol)) then
            FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
      ttRECORD :
         if not ((forType is TRecordSymbol) or (forType is TBaseSymbol)) then
            FMsgs.AddCompilerError(hotPos, CPE_RecordTypeExpected);
      ttINTERFACE :
         if not (forType is TInterfaceSymbol) then
            FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
   else
      Assert(False);
   end;

   Result:=THelperSymbol.Create(typeName, CurrentUnitSymbol, forType, FProg.Table.Count);
   try
      visibility:=cvPublic;

      repeat

         hotPos:=FTok.HotPos;
         tt:=FTok.TestDeleteAny([ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED, ttCLASS,
                                 ttPROPERTY, ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONST]);
         case tt of
            ttPRIVATE, ttPUBLIC, ttPUBLISHED :
               if visibility=cTokenToVisibility[tt] then
                  FMsgs.AddCompilerHintFmt(FTok.HotPos, CPH_RedundantVisibilitySpecifier, [cTokenStrings[tt]])
               else visibility:=cTokenToVisibility[tt];
            ttPROTECTED :
               FMsgs.AddCompilerError(FTok.HotPos, CPE_NoProtectedVisibilityForHelpers);
            ttPROPERTY : begin
               propSym:=ReadPropertyDecl(Result, visibility);
               Result.AddProperty(propSym);
            end;
            ttFUNCTION, ttPROCEDURE, ttMETHOD :
               ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, False);
            ttCLASS : begin
               tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttVAR, ttCONST]);
               case tt of
                  ttPROCEDURE, ttFUNCTION, ttMETHOD :
                     ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, True);
                  ttVAR :
                     ReadClassVars(Result, visibility);
                  ttCONST :
                     ReadClassConst(Result, visibility);
               else
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
               end;
            end;
            ttCONST :
               ReadClassConst(Result, visibility);
         else
            Break;
         end;
      until not FTok.HasTokens;

      if not FTok.TestDelete(ttEND) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      CheckNoPendingAttributes;

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
         Result:=ReadExcept(tryBlock, tt);
         if tt=ttFINALLY then
            Result:=ReadFinally(Result);
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

// ReadExcept
//
function TdwsCompiler.ReadExcept(tryExpr : TNoResultExpr; var finalToken : TTokenType) : TExceptExpr;
var
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

            if FTok.TestAny([ttEND, ttELSE])=ttNone then
               ReadSemiColon;
         end;

         if FTok.TestDelete(ttELSE) then
            Result.ElseExpr:=ReadBlocks([ttEND, ttFINALLY], finalToken)
         else begin
            finalToken:=FTok.TestDeleteAny([ttEND, ttFINALLY]);
            if finalToken=ttNone then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
         end;
      end else begin
         Result.HandlerExpr:=ReadBlocks([ttEND, ttFINALLY], finalToken);
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
      exceptObjTyp:=exceptExpr.Typ;
      if not (    (exceptObjTyp is TClassSymbol)
              and TClassSymbol(exceptObjTyp).IsOfType(FProg.TypException)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ExceptionObjectExpected);
      Result:=TRaiseExpr.Create(FProg, FTok.HotPos, exceptExpr);
   end;
end;

// ReadExit
//
function TdwsCompiler.ReadExit : TNoResultExpr;
var
   gotParenthesis : Boolean;
   leftExpr : TDataExpr;
   assignExpr : TAssignExpr;
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
      RecordSymbolUse(proc.Func.Result, exitPos, [suReference, suWrite]);
      leftExpr:=TVarExpr.CreateTyped(FProg, proc.Func.Result);
      try
         assignExpr:=ReadAssign(ttASSIGN, leftExpr) as TAssignExpr;
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

// ReadClassExpr
//
function TdwsCompiler.ReadClassExpr(ownerSymbol : TCompositeTypeSymbol; expecting : TTypeSymbol = nil) : TTypedExpr;
var
   membersTable : TSymbolTable;
begin
   membersTable:=TSymbolTable.Create(ownerSymbol.Members);
   try
      membersTable.AddParent(FProg.Table);
      FProg.EnterSubTable(membersTable);
      try
         Result:=ReadExpr(expecting);
      finally
         FProg.LeaveSubTable;
      end;
   finally
      membersTable.Free;
   end;
end;

// ReadType
//
function TdwsCompiler.ReadType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;

   function ReadClassFlags(token : TTokenType) : TTypeSymbol;
   var
      flags : TClassSymbolFlags;
   begin
      case token of
         ttPARTIAL : flags:=[csfPartial];
         ttSTATIC : flags:=[csfStatic];
      else
         flags:=[];
      end;
      if FTok.TestDelete(ttCLASS) and (typeContext=tcDeclaration) then
         Result:=ReadClass(typeName, flags)
      else begin
         Result:=nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ClassExpected);
      end;
   end;

   function ReadProcType(token : TTokenType; const hotPos : TScriptPos) : TTypeSymbol;
   begin
      Result:=ReadProcDecl(token, hotPos, [pdoType]);
      Result.SetName(typeName);
      (Result as TFuncSymbol).SetIsType;
      // close declaration context
      if coContextMap in Options then
         FSourceContextMap.CloseContext(FTok.HotPos);

   end;

var
   tt : TTokenType;
   name, connectorQualifier : String;
   hotPos, namePos : TScriptPos;
   sym : TSymbol;
begin
   hotPos:=FTok.HotPos;
   tt:=FTok.TestDeleteAny([ttRECORD, ttARRAY, ttCLASS, ttINTERFACE, ttHELPER,
                           ttBLEFT, ttENUM, ttFLAGS, ttPARTIAL, ttSTATIC,
                           ttPROCEDURE, ttFUNCTION, ttREFERENCE]);
   case tt of
      ttRECORD :
         if FTok.TestDelete(ttHELPER) then
            Result:=ReadHelperDecl(typeName, ttRECORD)
         else Result:=ReadRecordDecl(typeName, False);

      ttARRAY :
         Result:=ReadArrayType(typeName, typeContext);

      ttCLASS : begin
         tt:=FTok.TestDeleteAny([ttOF, ttHELPER]);
         case tt of
            ttOF :
               Result:=ReadClassOf(typeName);
            ttHELPER :
               Result:=ReadHelperDecl(typeName, ttCLASS);
         else
            if typeContext=tcDeclaration then
               Result:=ReadClass(typeName, [])
            else begin
               Result:=nil;
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);
            end;
         end;
      end;

      ttPARTIAL, ttSTATIC :
         Result:=ReadClassFlags(tt);

      ttINTERFACE : begin
         hotPos:=FTok.HotPos;
         if FTok.TestDelete(ttHELPER) then
            Result:=ReadHelperDecl(typeName, ttINTERFACE)
         else begin
            if typeContext=tcDeclaration then
               Result:=ReadInterface(typeName)
            else begin
               Result:=nil;
               FMsgs.AddCompilerStop(hotPos, CPE_TypeExpected);
            end;
         end;
      end;

      ttHELPER :
         Result:=ReadHelperDecl(typeName, ttNone);

      ttENUM, ttFLAGS : begin
         // explicitly scoped enum
         if not FTok.TestDelete(ttBLEFT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);
         if tt=ttENUM then
            Result:=ReadEnumeration(typeName, enumScoped)
         else Result:=ReadEnumeration(typeName, enumFlags);
      end;

      ttBLEFT : begin
         // class, globally scoped enum
         Result:=ReadEnumeration(typeName, enumClassic);
      end;

      ttREFERENCE : begin
         if FTok.TestDelete(ttTO) then
            FMsgs.AddCompilerHint(FTok.HotPos, CPH_ReferenceToIsLegacy, hlPedantic)
         else FMsgs.AddCompilerError(FTok.HotPos, CPE_ToExpected);
         tt:=FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION]);
         if tt=ttNone then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ProcOrFuncExpected);
            tt:=ttFUNCTION; // keep compiling
         end;
         Result:=ReadProcType(tt, hotPos)
      end;

      ttPROCEDURE, ttFUNCTION :
         Result:=ReadProcType(tt, hotPos);

   else

      if FTok.TestName then begin

         sym:=ReadNameSymbol(namePos);

         if not Assigned(sym) then begin
            // keep compiling
            Result:=FProg.TypVariant;
         end else if not sym.IsType then begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidType, [sym.Name]);
            Result:=FProg.TypVariant; // keep compiling
         end else if sym is TConnectorSymbol then begin
            connectorQualifier:='';
            if FTok.TestDelete(ttLESS) then begin
               repeat
                  if not FTok.TestDeleteNamePos(name, namePos) then
                     FMsgs.AddCompilerStop(namePos, CPE_NameExpected);
                  connectorQualifier:=connectorQualifier+name;
                  if FTok.TestDelete(ttGTR) then
                     Break;
                  if not FTok.TestDelete(ttDOT) then
                     FMsgs.AddCompilerStop(namePos, CPE_DotExpected);
                  connectorQualifier:=connectorQualifier+'.';
               until False;
            end;
            if connectorQualifier='' then
               Result:=TTypeSymbol(sym)
            else begin
               Result:=TConnectorSymbol(sym).Specialize(FProg.Table, connectorQualifier);
               if Result=sym then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConnectorCantBeSpecialized, [sym.Name])
               else if Result=nil then begin
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConnectorInvalidSpecifier, [sym.Name, connectorQualifier]);
                  Result:=TTypeSymbol(sym);
               end;
            end;
         end else Result:=TTypeSymbol(sym);

         // Create name symbol, e. g.: type a = integer;
         if typeName <> '' then
            Result:=TAliasSymbol.Create(typeName, Result);

         RecordSymbolUse(Result, namePos, [suReference]);

      end else begin

         Result:=nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);

      end;

   end;

   // Ensure that unnamed symbols will be freed
   if (Result<>nil) and (Result.Name='') then
      FProg.RootTable.AddToDestructionList(Result);
end;

// ReadExpr
//
function TdwsCompiler.ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
var
   right : TTypedExpr;
   rightTyp : TTypeSymbol;
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
         right:=ReadExprAdd;
         rightTyp:=right.Typ;
         try
            case tt of
               ttIS : begin
                  if not (Result.Typ is TClassSymbol) then
                     FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                  else if not (rightTyp is TClassOfSymbol) then
                     FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                  Result:=TIsOpExpr.Create(FProg, Result, right)
               end;
               ttAS : begin
                  if Result.Typ is TInterfaceSymbol then begin
                     if rightTyp is TInterfaceSymbol then begin
                        Result:=TIntfAsIntfExpr.Create(FProg, hotPos, Result, TInterfaceSymbol(rightTyp));
                     end else begin
                        if not (rightTyp is TClassOfSymbol) then begin
                           FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                           rightTyp:=FProg.TypObject.MetaSymbol;
                        end;
                        Result:=TIntfAsClassExpr.Create(FProg, hotPos, Result, TClassOfSymbol(rightTyp).Typ);
                     end;
                  end else if Result.Typ is TClassSymbol then begin
                     if rightTyp is TInterfaceSymbol then
                        Result:=TObjAsIntfExpr.Create(FProg, hotPos, Result, TInterfaceSymbol(rightTyp))
                     else begin
                        if not (rightTyp is TClassOfSymbol) then
                           FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                        Result:=TObjAsClassExpr.Create(FProg, hotPos, Result, TClassOfSymbol(rightTyp).Typ);
                     end;
                  end else begin
                     if not (Result.Typ is TClassOfSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                     else if not (rightTyp is TClassOfSymbol) then
                        FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);
                     Result:=TClassAsClassExpr.Create(FProg, hotPos, Result, TClassOfSymbol(rightTyp));
                  end;
                  right.Free;
               end;
               ttIMPLEMENTS : begin
                  if not (rightTyp is TInterfaceSymbol) then
                     FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                  if Result.Typ is TClassOfSymbol then
                     Result:=TClassImplementsIntfOpExpr.Create(FProg, Result, right)
                  else begin
                     if not (Result.Typ is TClassSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
                     Result:=TImplementsIntfOpExpr.Create(FProg, Result, right);
                  end;
               end;
            else
               opExpr:=CreateTypedOperatorExpr(tt, Result, right);
               if opExpr=nil then begin
                  if     ((tt=ttEQ) or (tt=ttNOTEQ))
                     and (
                             (Result.Typ is TClassSymbol)
                          or (Result.Typ is TInterfaceSymbol)
                          or (Result.Typ=FProg.TypNil)
                          ) then begin
                     if not ((rightTyp.ClassType=Result.Typ.ClassType) or (rightTyp=FProg.TypNil)) then
                        if Result.Typ is TClassSymbol then
                           FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                        else FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                     if Result.Typ is TClassSymbol then
                        if tt=ttNOTEQ then
                           Result:=TObjCmpNotEqualExpr.Create(FProg, Result, right)
                        else Result:=TObjCmpEqualExpr.Create(FProg, Result, right)
                     else begin
                        Result:=TIntfCmpExpr.Create(FProg, Result, right);
                        if tt=ttNOTEQ then
                           Result:=TNotBoolExpr.Create(FProg, Result);
                     end;
                  end else begin
                     FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     Result:=TRelOpExpr.Create(FProg, Result, right); // keep going
                  end;
               end else Result:=opExpr;
            end;
         except
            right.Free;
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
                                 ttSHL, ttSHR, ttSAR, ttIN, ttNOT,
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
                  opExpr:=CreateTypedOperatorExpr(tt, Result, right);
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
            Result:=Result.OptimizeToTypedExpr(FProg, FExec, hotPos);
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
               opExpr:=CreateTypedOperatorExpr(tt, Result, right);
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
            Result:=Result.OptimizeToTypedExpr(FProg, FExec, hotPos);
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
   elementType : TTypeSymbol;
   classOpSymbol : TClassOperatorSymbol;
   classOpExpr : TFuncExpr;
   argPosArray : TScriptPosArray;
begin
   hotPos:=FTok.HotPos;

   if FTok.TestDelete(ttALEFT) then begin

      Result:=ReadExprInConditions(left);

   end else begin

      setExpr:=ReadExpr;
      try

         if not (setExpr is TDataExpr) then begin

            FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
            // keep compiling
            left.Free;
            setExpr.Free;
            Result:=TConstExpr.CreateBooleanValue(FProg, False);

         end else if setExpr.Typ is TDynamicArraySymbol then begin

            elementType:=TDynamicArraySymbol(setExpr.Typ).Typ;
            if (left.Typ=nil) or not left.Typ.IsOfType(elementType) then begin
               // attempt cast & typecheck harder
               if (left is TFuncExpr) and (TFuncExpr(left).Args.Count=0) then begin
                  if left is TFuncPtrExpr then
                     left:=TFuncPtrExpr(left).Extract
                  else left:=TFuncRefExpr.Create(FProg, TFuncExpr(left));
               end;
               if (left.Typ=nil) or not elementType.IsCompatible(left.Typ) then
                  IncompatibleTypes(hotPos, CPE_IncompatibleTypes,
                                    left.Typ, elementType);
            end;

            Result:=TArrayIndexOfExpr.Create(FProg, hotPos, setExpr, left as TDataExpr, nil);
            Result:=TRelGreaterEqualIntExpr.Create(FProg, Result,
                                                   TConstExpr.CreateIntegerValue(FProg, 0));

         end else begin

            if (setExpr.Typ=nil) or not (setExpr.Typ is TClassSymbol) then
               FMsgs.AddCompilerStop(hotPos, CPE_ArrayBracketOrClassExpected);

            classOpSymbol:=(setExpr.Typ as TClassSymbol).FindClassOperator(ttIN, left.Typ);
            if classOpSymbol=nil then
               FMsgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands);
            classOpExpr:=GetMethodExpr(classOpSymbol.UsesSym, (setExpr as TDataExpr),
                                       rkObjRef, hotPos, False);
            try
               setExpr:=nil;
               classOpExpr.AddArg(left);
               left:=nil;
               TypeCheckArgs(classOpExpr, argPosArray);
            except
               classOpExpr.Free;
               raise;
            end;
            Result:=classOpExpr;

         end;

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
const
   cNilIntf : IUnknown = nil;

   function ReadNilTerm : TConstExpr;
   begin
      Result:=TConstExpr.CreateTypedVariantValue(FProg, FProg.TypNil, cNilIntf);
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

   function ReadAnonymousMethod(funcType : TTokenType; const hotPos : TScriptPos) : TAnonymousFuncRefExpr;
   var
      funcSym : TFuncSymbol;
   begin
      funcSym:=ReadProcDecl(funcType, hotPos, [pdoAnonymous]);
      FProg.Table.AddSymbol(funcSym);
      ReadProcBody(funcSym);
      Result:=TAnonymousFuncRefExpr.Create(FProg, GetFuncExpr(funcSym, nil));
   end;

   function ReadLambda(funcType : TTokenType; const hotPos : TScriptPos) : TAnonymousFuncRefExpr;
   var
      funcSym : TFuncSymbol;
      expectingParams : TParamsSymbolTable;
      resultExpr : TTypedExpr;
      resultVar : TVarExpr;
      proc : TdwsProcedure;
      procPos : TScriptPos;
      oldProg : TdwsProgram;
   begin
      if expecting is TFuncSymbol then
         expectingParams:=TFuncSymbol(expecting).Params
      else expectingParams:=nil;

      funcSym:=ReadProcDecl(funcType, hotPos, [pdoAnonymous], expectingParams);
      FProg.Table.AddSymbol(funcSym);

      if (funcSym.Typ=nil) and (expecting is TFuncSymbol) then
         funcSym.Typ:=TFuncSymbol(expecting).Typ;

      if FTok.TestDelete(ttEQGTR) then begin

         FTok.TestName;
         procPos:=FTok.HotPos;

         proc:=TdwsProcedure.Create(FProg);
         proc.SetBeginPos(procPos);
         proc.AssignTo(funcSym);

         oldProg:=FProg;
         FProg:=proc;
         try
            resultExpr:=ReadExpr(funcSym.Typ);
         finally
            FProg:=oldProg;
         end;
         if funcSym.Typ=nil then
            funcSym.Typ:=resultExpr.Typ;

         resultVar:=TVarExpr.CreateTyped(FProg, proc.Func.Result);
         proc.Expr:=CreateAssign(procPos, ttASSIGN, resultVar, resultExpr);

      end else begin

         ReadProcBody(funcSym);

      end;
      Result:=TAnonymousFuncRefExpr.Create(FProg, GetFuncExpr(funcSym, nil));
   end;

   function ReadAnonymousRecord : TTypedExpr;
   var
      scriptPos : TScriptPos;
      recordType : TRecordSymbol;
      data : TData;
   begin
      scriptPos:=FTok.HotPos;
      recordType:=ReadRecordDecl('', True);
      FProg.Table.AddSymbol(recordType);

      RecordSymbolUseImplicitReference(recordType, scriptPos, False);

      if recordType.IsDynamic then begin

         Result:=TDynamicRecordExpr.Create(FProg, scriptPos, recordType);

      end else begin

         SetLength(data, recordType.Size);
         recordType.InitData(data, 0);

         Result:=TConstExpr.CreateTyped(FProg, recordType, data);

      end;
   end;

   procedure ReportIncompatibleAt(const scriptPos : TScriptPos; expr : TTypedExpr);
   var
      exprTyp : String;
   begin
      if (expr is TFuncExprBase) and (TFuncExprBase(expr).FuncSym<>nil) then
         exprTyp:=TFuncExprBase(expr).FuncSym.Caption
      else if expr.Typ<>nil then
         exprTyp:=expr.Typ.Caption
      else exprTyp:=SYS_VOID;
      FMsgs.AddCompilerErrorFmt(scriptPos, CPE_IncompatibleTypes,
                                [expecting.Caption, exprTyp]);
   end;

   function ReadAt(expecting : TTypeSymbol = nil) : TTypedExpr;
   var
      hotPos : TScriptPos;
   begin
      hotPos:=FTok.HotPos;
      if expecting=nil then
         expecting:=FAnyFuncSymbol
      else if not (expecting is TFuncSymbol) then
         FMsgs.AddCompilerError(hotPos, CPE_UnexpectedAt)
      else if expecting=FAnyFuncSymbol then
         FMsgs.AddCompilerStop(hotPos, CPE_UnexpectedAt);
      Result:=ReadTerm(isWrite, expecting);
      if (Result.Typ=nil) or not (Result.Typ is TFuncSymbol) then begin
         if (expecting=FAnyFuncSymbol) or (Result is TConstExpr) then
            FMsgs.AddCompilerError(hotPos, CPE_UnexpectedAt)
         else ReportIncompatibleAt(hotPos, Result);
         // keep compiling
         Result.Free;
         Result:=TBogusConstExpr.Create(FProg, FProg.TypNil, cNilIntf);
      end;
   end;

var
   tt : TTokenType;
   nameExpr : TProgramExpr;
   hotPos : TScriptPos;
begin
   tt:=FTok.TestAny([ttPLUS, ttMINUS, ttALEFT, ttNOT, ttBLEFT, ttAT,
                     ttTRUE, ttFALSE, ttNIL, ttFUNCTION, ttPROCEDURE, ttLAMBDA,
                     ttRECORD]);
   if tt<>ttNone then
      FTok.KillToken;
   case tt of
      ttPLUS : begin
         FTok.TestName;
         hotPos:=FTok.HotPos;
         Result:=ReadTerm; // (redundant) plus sign
         if not (   Result.IsOfType(FProg.TypFloat)
                 or Result.IsOfType(FProg.TypInteger)
                 or Result.IsOfType(FProg.TypVariant)) then
            FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
      end;
      ttMINUS :
         Result:=ReadNegation;
      ttALEFT :
         Result:=ReadArrayConstant(ttARIGHT, expecting);
      ttNOT :
         Result:=ReadNotTerm;
      ttBLEFT : begin
         // Read expression in brackets
         Result := ReadExpr;
         if not FTok.TestDelete(ttBRIGHT) then begin
            Result.Free;
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
         end;
         if FTok.Test(ttDOT) then
            Result:=(ReadSymbol(Result, isWrite) as TTypedExpr);
      end;
      ttAT :
         Result:=ReadAt(expecting);
      ttTRUE :
         Result:=ReadTrue;
      ttFALSE :
         Result:=ReadFalse;
      ttNIL :
         Result:=ReadNilTerm;
      ttPROCEDURE, ttFUNCTION : begin
         if not (coAllowClosures in Options) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_LocalFunctionAsDelegate);
         Result:=ReadAnonymousMethod(tt, FTok.HotPos);
      end;
      ttLAMBDA : begin
         if not (coAllowClosures in Options) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_LocalFunctionAsDelegate);
         Result:=ReadLambda(tt, FTok.HotPos);
      end;
      ttRECORD :
         Result:=ReadAnonymousRecord;
   else
      if (FTok.TestAny([ttINHERITED, ttNEW])<>ttNone) or FTok.TestName then begin
         // Variable or Function
         nameExpr:=ReadName(isWrite, expecting);
         if (nameExpr<>nil) and not nameExpr.InheritsFrom(TTypedExpr) then begin
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
   hotPos : TScriptPos;
begin
   hotPos:=FTok.HotPos;
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
      Result:=Result.OptimizeToTypedExpr(FProg, FExec, hotPos);
end;

// ReadConstValue
//
function TdwsCompiler.ReadConstValue: TConstExpr;
var
   tt : TTokenType;
   unifiedList : TUnifiedConstList;
   token : TToken;
begin
   Result:=nil;
   unifiedList:=TUnifiedConstList(FMainProg.UnifiedConstList);
   tt:=FTok.TestAny([ttStrVal, ttIntVal, ttFloatVal]);
   if tt<>ttNone then begin
      token:=FTok.GetToken;
      case tt of
         ttIntVal :
            case token.FInteger of
               -1..2 : begin
                  Result:=unifiedList.Integers[token.FInteger];
                  Result.IncRefCount;
               end;
            else
               Result:=TConstIntExpr.CreateUnified(FProg, nil, token.FInteger);
            end;
         ttFloatVal :
            if token.FFloat=0 then begin
               Result:=unifiedList.ZeroFloat;
               Result.IncRefCount;
            end else Result:=TConstFloatExpr.CreateUnified(FProg, nil, token.FFloat);
         ttStrVal :
            if token.FString='' then begin
               Result:=unifiedList.EmptyString;
               Result.IncRefCount;
            end else Result:=TConstStringExpr.CreateUnified(FProg, nil, token.FString);
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
   exprPos : TScriptPos;
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
         FMsgs.AddCompilerErrorFmt(FTok.GetToken.FScriptPos, CPE_UnknownMember, [FTok.GetToken.FString]);
         sym:=nil;
      end;
      memberSym:=TFieldSymbol(sym);
      if memberSym<>nil then begin
         if memberSym.Visibility<cvPublic then
            FMsgs.AddCompilerErrorFmt(FTok.GetToken.FScriptPos, CPE_MemberSymbolNotVisible, [FTok.GetToken.FString]);
         if memberSet[memberSym.Offset] then
            FMsgs.AddCompilerError(FTok.GetToken.FScriptPos, CPE_FieldAlreadySet);
         memberSet[memberSym.Offset]:=True;
         RecordSymbolUseReference(memberSym, FTok.GetToken.FScriptPos, True);
      end;
      FTok.KillToken;
      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
      exprPos:=FTok.HotPos;
      expr:=ReadExpr;
      try
         if not (expr is TConstExpr) then begin
            if expr.IsConstant then
               expr:=expr.OptimizeToTypedExpr(FProg, FExec, exprPos)
            else begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
               FreeAndNil(expr);
            end;
         end;
         if (expr<>nil) and (memberSym<>nil) then begin
            constExpr:=TConstExpr(expr);
            if constExpr.Typ.IsOfType(FProg.TypInteger) and memberSym.Typ.IsOfType(FProg.TypFloat) then
               Result[memberSym.Offset]:=constExpr.EvalAsFloat(FExec)
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

// ReadArrayParams
//
procedure TdwsCompiler.ReadArrayParams(ArrayIndices: TSymbolTable);
var
   i : Integer;
   names : TStringList;
   typSym : TTypeSymbol;
   isVarParam, isConstParam : Boolean;
   posArray : TScriptPosArray;
begin
   if FTok.TestDelete(ttARIGHT) then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_ParamsExpected);
      Exit;
   end;

   // At least one argument was found
   names:=TStringList.Create;
   try
      repeat
         isVarParam:=FTok.TestDelete(ttVAR);
         if not isVarParam then
            isConstParam:=FTok.TestDelete(ttCONST)
         else isConstParam:= False;

         ReadNameList(names, posArray);

         if not FTok.TestDelete(ttCOLON) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)
         else begin
            typSym:=ReadType('', tcParameter);
            for i:=0 to names.Count-1 do begin
               if isVarParam then
                  ArrayIndices.AddSymbol(TVarParamSymbol.Create(names[i], typSym))
               else if isConstParam then
                  ArrayIndices.AddSymbol(TConstParamSymbol.Create(names[i], typSym))
               else ArrayIndices.AddSymbol(TParamSymbol.Create(names[i], typSym));
            end;
         end;
      until not FTok.TestDelete(ttSEMI);

   finally
      names.Free;
   end;

   if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
end;

// ReadParams
//
procedure TdwsCompiler.ReadParams(const hasParamMeth : THasParamSymbolMethod;
                                  const addParamMeth : TAddParamSymbolMethod;
                                  forwardedParams : TParamsSymbolTable;
                                  expectedLambdaParams : TParamsSymbolTable);
var
   lazyParam, varParam, constParam : Boolean;

   procedure GenerateParam(const curName : String; const scriptPos : TScriptPos;
                           paramType : TTypeSymbol; const typScriptPos : TScriptPos;
                           var defaultExpr : TTypedExpr);
   var
      paramSym : TParamSymbol;
   begin
      if lazyParam then begin
         paramSym := TLazyParamSymbol.Create(curName, paramType)
      end else if varParam then begin
         paramSym := TVarParamSymbol.Create(curName, paramType)
      end else if constParam then begin
         paramSym := TConstParamSymbol.Create(curName, paramType)
      end else begin
         if Assigned(defaultExpr) then begin
            paramSym := TParamSymbolWithDefaultValue.Create(curName, paramType,
                     (defaultExpr as TConstExpr).Data[FExec],
                     (defaultExpr as TConstExpr).Addr[FExec]);
         end else begin
            paramSym := TParamSymbol.Create(curName, paramType);
         end;
      end;

      if hasParamMeth(paramSym) then
         FMsgs.AddCompilerErrorFmt(scriptPos, CPE_NameAlreadyExists, [curName]);
      addParamMeth(paramSym);

      // Enter Field symbol in dictionary
      if coSymbolDictionary in FOptions then begin
         // add parameter symbol
         if forwardedParams=nil then begin
            // no forward, our param symbol is the actual one
            RecordSymbolUse(paramSym, scriptPos, [suDeclaration])
         end else begin
            // find the original param symbol and register it
            // in case of mismatch, RecordSymbolUse will discard
            // a nil automatically, so we don't have to check here
            RecordSymbolUse(forwardedParams.FindLocal(curName),
                            scriptPos, [suReference]);
         end;
         // record field's type symbol
         if typScriptPos.Defined then
            RecordSymbolUse(paramType, typScriptPos, [suReference])
         else RecordSymbolUse(paramType, scriptPos, [suReference, suImplicit])
      end;
   end;

var
   i, paramIdx : Integer;
   names : TStringList;
   typ : TTypeSymbol;
   onlyDefaultParamsNow : Boolean;
   posArray : TScriptPosArray;
   typScriptPos, exprPos : TScriptPos;
   defaultExpr : TTypedExpr;
   expectedParam : TParamSymbol;
begin
   if FTok.TestDelete(ttBLEFT) then begin
      if not FTok.TestDelete(ttBRIGHT) then begin
         // At least one argument was found
         names:=TStringList.Create;
         try
            paramIdx:=0;
            onlyDefaultParamsNow:=False;
            repeat
               lazyParam:=FTok.TestDelete(ttLAZY);
               varParam:=FTok.TestDelete(ttVAR);
               if not varParam then
                  constParam:=FTok.TestDelete(ttCONST)
               else constParam:=False;

               if lazyParam and (varParam or constParam) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantBeVarOrConst);

               ReadNameList(names, posArray);

               if not FTok.TestDelete(ttCOLON) then begin

                  if (expectedLambdaParams<>nil) and (paramIdx+names.Count-1<expectedLambdaParams.Count) then begin

                     defaultExpr:=nil;
                     for i:=0 to names.Count-1 do begin
                        expectedParam:=expectedLambdaParams[paramIdx+i];
                        if not (lazyParam or varParam or constParam) then begin
                           lazyParam:=(expectedParam.ClassType=TLazyParamSymbol);
                           varParam:=(expectedParam.ClassType=TVarParamSymbol);
                           constParam:=(expectedParam.ClassType=TConstParamSymbol);
                        end;
                        GenerateParam(names[i], posArray[i], expectedParam.Typ, cNullPos, defaultExpr);
                     end;

                  end else begin

                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)

                  end;

               end else begin

                  typ:=ReadType('', tcParameter);

                  defaultExpr:=nil;
                  try
                     typScriptPos:=FTok.HotPos;

                     if (not constParam) and (typ is TOpenArraySymbol) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_OpenArrayParamMustBeConst);
                     if lazyParam and (typ is TFuncSymbol) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantBeFunctionPointer);

                     if FTok.TestDelete(ttEQ) then begin
                        onlyDefaultParamsNow:=True;
                        if lazyParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantHaveDefaultValue);
                        if varParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_VarParamCantHaveDefaultValue);
                        if constParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstParamCantHaveDefaultValue);

                        exprPos:=FTok.HotPos;
                        defaultExpr:=ReadExpr;

                        if (not defaultExpr.IsConstant) or (defaultExpr.Typ=nil) then begin
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
                           defaultExpr.Free;
                           defaultExpr:=nil;
                        end else if not Typ.IsCompatible(defaultExpr.Typ) then begin
                           if Typ.IsOfType(FProg.TypFloat) and defaultExpr.Typ.IsOfType(FProg.TypInteger) then
                              defaultExpr:=TConvFloatExpr.Create(FProg, defaultExpr)
                           else begin
                              IncompatibleTypes(FTok.HotPos, CPE_IncompatibleTypes, Typ, defaultExpr.Typ);
                              defaultExpr.Free;
                              defaultExpr:=nil;
                           end;
                        end;
                     end else if onlyDefaultParamsNow then begin
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_DefaultValueRequired);
                     end;

                     if (defaultExpr<>nil) and not (defaultExpr is TConstExpr) then
                        defaultExpr:=defaultExpr.OptimizeToTypedExpr(Fprog, FExec, exprPos);

                     for i:=0 to names.Count-1 do
                        GenerateParam(names[i], posArray[i], typ, typScriptPos, defaultExpr);

                  finally
                     defaultExpr.Free;
                  end;

               end;

               Inc(paramIdx, names.Count);

            until not FTok.TestDelete(ttSEMI);

         finally
            names.Free;
         end;

         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;

   end else if (expectedLambdaParams<>nil) and (expectedLambdaParams.Count>0) then begin

      // implicit anonmous lambda params
      defaultExpr:=nil;
      for i:=0 to expectedLambdaParams.Count-1 do begin
         expectedParam:=expectedLambdaParams[i];
         lazyParam:=(expectedParam.ClassType=TLazyParamSymbol);
         varParam:=(expectedParam.ClassType=TVarParamSymbol);
         constParam:=(expectedParam.ClassType=TConstParamSymbol);
         GenerateParam('_implicit_'+expectedParam.Name, cNullPos,
                       expectedParam.Typ, cNullPos, defaultExpr);
      end;

   end;
end;

// ReadSwitch
//
function TdwsCompiler.ReadSwitch(const SwitchName: String) : Boolean;
var
   sw : TSwitchInstruction;
begin
   sw:=StringToSwitchInstruction(SwitchName);
   if sw<>siNone then
      Exit(True);

   Result := False;

   FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_CompilerSwitchUnknown, [SwitchName]);

   while FTok.HasTokens and not FTok.TestDelete(ttCRIGHT) do
      FTok.KillToken;
end;

// ReadInstrSwitch
//
function TdwsCompiler.ReadInstrSwitch(const switchName : String) : Boolean;
var
   switch : TSwitchInstruction;
   name, scriptSource : String;
   i : Integer;
   conditionalTrue : Boolean;
   switchPos, condPos, fileNamePos : TScriptPos;
   condExpr : TTypedExpr;
   sourceFile : TSourceFile;
   includeSymbol : TIncludeSymbol;
   condInfo : TTokenizerConditionalInfo;
begin
   Result:=True;
   if Assigned(FOnReadInstrSwitch) then begin
      if FOnReadInstrSwitch(Self) then
         Exit;
   end;

   switchPos:=FTok.HotPos;

   switch:=StringToSwitchInstruction(switchName);
   FTok.KillToken;

   case switch of
      siIncludeLong, siIncludeShort, siIncludeOnce, siFilterLong, siFilterShort : begin

         if FTok.Test(ttPERCENT) and (switch in [siIncludeShort, siIncludeLong]) then begin

            Result:=ReadExprSwitch(switchPos);
            Exit;

         end else if not FTok.Test(ttStrVal) then begin

            FMsgs.AddCompilerError(FTok.HotPos, CPE_IncludeFileExpected);
            // skip in attempt to recover from error
            Result:=False;
            SkipUntilCurlyRight;

         end else begin

            name:=FTok.GetToken.FString;
            FTok.KillToken;

            if coSymbolDictionary in Options then begin
               includeSymbol:=TIncludeSymbol.Create(name);
               FProg.Table.AddSymbol(includeSymbol);
               fileNamePos:=FTok.HotPos;
               fileNamePos.IncCol; // skip quote
               FSymbolDictionary.AddSymbol(includeSymbol, fileNamePos, [suReference]);
            end;

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

                  if not FTok.TestDelete(ttCRIGHT) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

                  sourceFile:=FMainProg.SourceList.Add(name, scriptSource, stInclude);
                  FTok.BeginSourceFile(sourceFile);
                  if coContextMap in Options then begin
                     FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttSWITCH);
                     FTok.OnEndSourceFile:=DoTokenizerEndSourceFile;
                  end;

                  FTok.SimulateToken(ttSEMI, FTok.HotPos);

                  Exit;
               except
                  on e: ECompileError do
                     raise;
                  on e: Exception do
                     FMsgs.AddCompilerStop(FTok.HotPos, e.Message);
               end;
            end;

         end;

      end;
      siDefine : begin

         if not FTok.Test(ttNAME) then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
            // skip in attempt to recover from error
            SkipUntilCurlyRight;
         end else begin
            FTok.ConditionalDefines.Value.Add(FTok.GetToken.FString);
            FTok.KillToken;
         end;

      end;
      siUndef : begin

         if not FTok.Test(ttNAME) then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
            // skip in attempt to recover from error
            SkipUntilCurlyRight;
         end else begin
            i:=FTok.ConditionalDefines.Value.IndexOf(FTok.GetToken.FString);
            if i>=0 then
               FTok.ConditionalDefines.Value.Delete(i);
            FTok.KillToken;
         end;

      end;
      siIfDef, siIfNDef, siIf : begin

         conditionalTrue:=True;
         case switch of
            siIfDef, siIfNDef : begin
               if not FTok.Test(ttNAME) then begin
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
                  // skip in attempt to recover from error
                  SkipUntilCurlyRight;
               end else begin
                  conditionalTrue:=    (FTok.ConditionalDefines.Value.IndexOf(FTok.GetToken.FString)>=0)
                                   xor (switch = siIfNDef);
                  FTok.KillToken;
               end;
            end;
            siIf : begin
               condPos:=Ftok.HotPos;
               FIsSwitch:=True;
               try
                  condExpr:=ReadExpr;
                  try
                     if not condExpr.IsConstant then
                        FMsgs.AddCompilerError(condPos, CPE_ConstantExpressionExpected)
                     else if not condExpr.IsOfType(FProg.TypBoolean) then
                        FMsgs.AddCompilerError(condPos, CPE_BooleanExpected)
                     else conditionalTrue:=condExpr.EvalAsBoolean(FExec);
                  finally
                     condExpr.Free;
                  end;
               finally
                  FIsSwitch:=False;
               end;
            end
         end;

         condInfo.ScriptPos:=switchPos;
         if conditionalTrue then begin
            condInfo.Conditional:=tcIf;
            FTok.ConditionalDepth.Push(condInfo);
         end else begin
            if ReadUntilEndOrElseSwitch(True) then begin
               condInfo.Conditional:=tcElse;
               FTok.ConditionalDepth.Push(condInfo);
            end;
            if not FTok.HasTokens then
               FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);
         end;

      end;
      siElse : begin

         if FTok.ConditionalDepth.Count=0 then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);
         if FTok.ConditionalDepth.Peek.Conditional=tcElse then
            FMsgs.AddCompilerStop(switchPos, CPE_UnfinishedConditionalDirective);

         FTok.ConditionalDepth.Pop;
         ReadUntilEndOrElseSwitch(False);
         if not FTok.HasTokens then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);

      end;
      siEndIf : begin

         if FTok.ConditionalDepth.Count=0 then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective)
         else FTok.ConditionalDepth.Pop;
         // tolerate junk after endif before the curly right
         SkipUntilCurlyRight;

      end;
      siResourceLong, siResourceShort : begin

         if not FTok.Test(ttStrVal) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
         if Assigned(FOnResource) then
            FOnResource(Self, FTok.GetToken.FString);
         FTok.KillToken;

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
      siWarnings : begin
         if not FTok.TestDeleteNamePos(name, condPos) then
            name:='';
         conditionalTrue:=SameText(name, 'ON');
         if conditionalTrue or SameText(name, 'OFF') then
            FMsgs.WarningsDisabled:=not conditionalTrue
         else FMsgs.AddCompilerError(FTok.HotPos, CPE_OnOffExpected);
      end;
      siHints : begin
         if not FTok.TestDeleteNamePos(name, condPos) then
            name:='';
         if SameText(name, 'OFF') then
            FMsgs.HintsLevel:=hlDisabled
         else if SameText(name, 'ON') then
            FMsgs.HintsLevel:=FDefaultHintsLevel
         else if SameText(name, 'NORMAL') then
            FMsgs.HintsLevel:=hlNormal
         else if SameText(name, 'STRICT') then
            FMsgs.HintsLevel:=hlStrict
         else FMsgs.AddCompilerError(FTok.HotPos, CPE_OnOffExpected);
      end;
   else
      FMsgs.AddCompilerErrorFmt(switchPos, CPE_CompilerSwitchUnknown, [Name]);
   end;

   if not FTok.Test(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

   // Simulate a semicolon
   FTok.GetToken.FTyp := ttSEMI
end;

// ReadExprSwitch
//
function TdwsCompiler.ReadExprSwitch(const switchPos : TScriptPos) : Boolean;
var
   name, value : String;
   hotPos : TScriptPos;
   funcSym : TFuncSymbol;
begin
   Result:=False;
   hotPos:=FTok.HotPos;

   hotPos:=FTok.HotPos;
   if FTok.TestDelete(ttPERCENT) then begin
      if FTok.TestAny([ttNAME, ttFUNCTION])<>ttNone then begin
         name:=FTok.GetToken.FString;
         FTok.KillToken;
         if not FTok.TestDelete(ttPERCENT) then
            name:='';
      end;
   end;
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

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

   FTok.SimulateStringToken(switchPos, value);
end;

// SkipUntilCurlyRight
//
procedure TdwsCompiler.SkipUntilCurlyRight;
begin
   while not FTok.Test(ttCRIGHT) do begin
      if not FTok.HasTokens then
         Exit;
      FTok.KillToken;
   end;
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

   FTok.SwitchProcessor:=nil;

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

            // tolerate junk after endif
            SkipUntilCurlyRight;
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

   FTok.SwitchProcessor:=ReadInstrSwitch;
end;

// Checks if a name already exists in the Symboltable

procedure TdwsCompiler.CheckName(const name : String; const namePos : TScriptPos);
var
   sym : TSymbol;
   i : Integer;
   subTable : TSymbolTable;
begin
   if name='' then Exit;

   sym:=FProg.Table.FindLocal(name);

   if not Assigned(sym) and (FProg is TdwsProcedure) then
      sym:=TdwsProcedure(FProg).Func.Params.FindLocal(name);

   if Assigned(sym) then
      FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);

   if FProg.Table.ClassType=TUnitImplementationTable then Exit;

   for i:=0 to FProg.SubTableDepth-1 do begin
      subTable:=FProg.SubTable(i);
      sym:=subTable.FindLocal(name);
      if sym<>nil then begin
         FMsgs.AddCompilerHintFmt(namePos, CPH_NameAmbiguousInScopeContext, [name]);
         Break;
      end;
   end;
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
         'a', 'A' : if SameText(name, 'abs') then Exit(skAbs);
         'd', 'D' : if SameText(name, 'dec') then Exit(skDec);
         'i', 'I' : if SameText(name, 'inc') then Exit(skInc);
         'l', 'L' : if SameText(name, 'low') then Exit(skLow);
         'o', 'O' : if SameText(name, 'ord') then Exit(skOrd);
         's', 'S' : if SameText(name, 'sqr') then Exit(skSqr);
      end;
      4 : case name[1] of
         'h', 'H' : if SameText(name, 'high') then Exit(skHigh);
         'p', 'P' : if SameText(name, 'pred') then Exit(skPred);
         's', 'S' : case name[2] of
            'u', 'U' : if SameText(name, 'succ') then Exit(skSucc);
            'w', 'W' : if SameText(name, 'swap') then Exit(skSwap);
         end;
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
      18 : if SameText(name, 'conditionaldefined') then Exit(skConditionalDefined);
   end;
   Result:=skNone;
end;

// CheckSpecialName
//
procedure TdwsCompiler.CheckSpecialName(const name : String);
begin
   if IdentifySpecialName(name)<>skNone then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NameIsReserved, [Name]);
end;

// OpenStreamForFile
//
function TdwsCompiler.OpenStreamForFile(const fileName : String) : TStream;
var
   i : Integer;
   fname : String;
begin
   for i:=0 to FScriptPaths.Count-1 do begin
      if FScriptPaths[i]<>'' then
         fname:=IncludeTrailingPathDelimiter(FScriptPaths[i])+fileName
      else fname:=fileName;
      if FCompileFileSystem.FileExists(fname) then
         Exit(FCompileFileSystem.OpenFileStream(fname, fomReadOnly));
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

// GetVarExpr
//
function TdwsCompiler.GetVarExpr(dataSym: TDataSymbol): TVarExpr;
begin
   if FProg.Level=dataSym.Level then begin
      if FDataSymbolExprReuse<>nil then begin
         Result:=FDataSymbolExprReuse.GetValue(dataSym);
         if Result=nil then begin
            Result:=TVarExpr.CreateTyped(FProg, dataSym);
            FDataSymbolExprReuse.SetValue(dataSym, Result);
         end;
         Result.IncRefCount;
      end else Result:=TVarExpr.CreateTyped(FProg, dataSym);
   end else Result:=TVarParentExpr.Create(FProg, dataSym);
end;

// GetLazyParamExpr
//
function TdwsCompiler.GetLazyParamExpr(dataSym: TLazyParamSymbol): TLazyParamExpr;
begin
   Result:=TLazyParamExpr.Create(FProg, dataSym);
end;

// GetVarParamExpr
//
function TdwsCompiler.GetVarParamExpr(dataSym: TVarParamSymbol): TByRefParamExpr;
begin
  if FProg.Level=dataSym.Level then
      Result:=TVarParamExpr.Create(FProg, dataSym)
  else Result:=TVarParamParentExpr.Create(FProg, dataSym)
end;

// GetConstParamExpr
//
function TdwsCompiler.GetConstParamExpr(dataSym: TConstParamSymbol): TByRefParamExpr;
begin
   if FProg.Level = dataSym.Level then
      Result := TConstParamExpr.Create(FProg, dataSym)
   else Result := TConstParamParentExpr.Create(FProg, dataSym);
end;

// GetSelfParamExpr
//
function TdwsCompiler.GetSelfParamExpr(selfSym : TDataSymbol) : TVarExpr;
var
   ct : TClass;
begin
   ct:=selfSym.ClassType;
   if ct=TConstParamSymbol then
      Result:=GetConstParamExpr(TConstParamSymbol(selfSym))
   else if ct=TVarParamSymbol then
      Result:=GetVarParamExpr(TVarParamSymbol(selfSym))
   else begin
      Assert((ct=TSelfSymbol) or (ct=TParamSymbol));
      Result:=GetVarExpr(selfSym);
   end;
end;

function TdwsCompiler.CheckParams(A, B: TSymbolTable; CheckNames: Boolean; skipB : Integer = 0): Boolean;
var
   x : Integer;
   r : Boolean;
   bParam : TSymbol;
begin
   Result := True;
   for x := 0 to A.Count - 1 do begin
      r := False;
      bParam:=B[x+skipB];
      if CheckNames and not UnicodeSameText(A[x].Name, bParam.Name) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterName, [x, A[x].Name])
      else if not A[x].Typ.IsCompatible(bParam.Typ) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterType,
                                   [x, A[x].Typ.Caption, bParam.Typ.Caption])
      else if (A[x].ClassType=TVarParamSymbol) and not (bParam.ClassType=TVarParamSymbol) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_VarParameterExpected, [x, A[x].Name])
      else if not (A[x].ClassType=TVarParamSymbol) and (bParam.ClassType=TVarParamSymbol) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ValueParameterExpected, [x, A[x].Name])
      else if (A[x] is TConstParamSymbol) and not (bParam is TConstParamSymbol) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConstParameterExpected, [x, A[x].Name])
      else if not (A[x] is TConstParamSymbol) and (bParam is TConstParamSymbol) then
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
         fkFunction : FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionExpected);
         fkProcedure : FMsgs.AddCompilerError(FTok.HotPos, CPE_ProcedureExpected);
         fkConstructor : FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstructorExpected);
         fkDestructor : FMsgs.AddCompilerError(FTok.HotPos, CPE_DestructorExpected);
         fkMethod, fkLambda : FMsgs.AddCompilerError(FTok.HotPos, CPE_MethodExpected);
      else
         Assert(False);
      end;
   end;
end;

// CompareFuncSymbolParams
//
procedure TdwsCompiler.CompareFuncSymbolParams(a, b : TFuncSymbol);
begin
   if (a.Typ<>nil) and ((b.Typ=nil) or not a.Typ.IsCompatible(b.Typ)) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadResultType, [a.Typ.Caption]);

   if a.Params.Count<>b.Params.Count then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadNumberOfParameters,
                                [a.Params.Count, b.Params.Count])
   else CheckParams(a.Params, b.Params, True);
end;

// CurrentStruct
//
function TdwsCompiler.CurrentStruct : TCompositeTypeSymbol;
var
   prog : TdwsProgram;
   func : TFuncSymbol;
begin
   prog:=FProg;
   while prog is TdwsProcedure do begin
      func:=TdwsProcedure(prog).Func;
      if func is TMethodSymbol then
         Exit(TMethodSymbol(func).StructSymbol)
      else prog:=prog.Parent;
   end;
   Result:=nil;
end;

// FindStructMember
//
function TdwsCompiler.FindStructMember(typ : TStructuredTypeSymbol; const name : String) : TSymbol;
begin
   Result:=typ.Members.FindSymbolFromScope(name, CurrentStruct);
   if Result=nil then begin
      Result:=typ.Members.FindSymbol(name, cvMagic);
      if Result<>nil then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MemberSymbolNotVisible, [name]);
   end;
end;

// HintUnusedSymbols
//
procedure TdwsCompiler.HintUnusedSymbols;
var
   sym : TSymbol;
   symDecl : TSymbolPosition;
   symDic : TdwsSymbolDictionary;
   symPosList : TSymbolPositionList;
begin
   if not (coSymbolDictionary in Options) then Exit;
   if coHintsDisabled in Options then Exit;

   symDic:=FMainProg.SymbolDictionary;
   for sym in FProg.Table do begin
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

// HintUnusedPrivateSymbols
//
procedure TdwsCompiler.HintUnusedPrivateSymbols;

   procedure HintIfUnused(sym : TSymbol; const msg : String);
   var
      symPos : TSymbolPosition;
   begin
      if FSymbolDictionary.FindSymbolUsage(sym, suReference)=nil then begin
         symPos:=FSymbolDictionary.FindSymbolUsage(sym, suDeclaration);
         if symPos<>nil then
            FMsgs.AddCompilerHintFmt(symPos.ScriptPos, msg, [sym.Name]);
      end;
   end;

   procedure DoHintUnusedPrivateSymbols(table : TSymbolTable);
   var
      sym : TSymbol;
      fieldSym : TFieldSymbol;
      methSym : TMethodSymbol;
   begin
      for sym in table do begin
         if sym is TStructuredTypeSymbol then
            DoHintUnusedPrivateSymbols(TStructuredTypeSymbol(sym).Members)
         else if sym is TFieldSymbol then begin
            fieldSym:=TFieldSymbol(sym);
            if fieldSym.Visibility=cvPrivate then
               HintIfUnused(fieldSym, CPH_PrivateFieldDeclaredButNotUsed);
         end else if sym is TMethodSymbol then begin
            methSym:=TMethodSymbol(sym);
            if (methSym.Visibility=cvPrivate) and (not methSym.IsInterfaced) then
               HintIfUnused(methSym, CPH_PrivateMethodDeclaredButNotUsed);
         end;
      end;
   end;

var
   i : Integer;
   ums : TUnitMainSymbol;
begin
   if coHintsDisabled in Options then Exit;
   if not (coSymbolDictionary in Options) then Exit;

   DoHintUnusedPrivateSymbols(FProg.Table);
   for i:=0 to FProg.UnitMains.Count-1 do begin
      ums:=FProg.UnitMains[i];
      if ums.Name=SYS_SYSTEM then Continue;
      if ums.Name=SYS_INTERNAL then Continue;
      DoHintUnusedPrivateSymbols(ums.Table);
      DoHintUnusedPrivateSymbols(ums.ImplementationTable);
   end;
end;

// HintUnusedResult
//
procedure TdwsCompiler.HintUnusedResult(resultSymbol : TDataSymbol);
begin
   if resultSymbol=nil then Exit;
   if not (coSymbolDictionary in FOptions) then Exit;
   if coHintsDisabled in Options then Exit;

   if FSymbolDictionary.FindSymbolUsage(resultSymbol, suReference)=nil then
      FMsgs.AddCompilerHint(FTok.HotPos, CPH_ResultNotUsed);
end;

// HintReferenceConstVarParams
//
procedure TdwsCompiler.HintReferenceConstVarParams(funcSym : TFuncSymbol);
var
   param : TSymbol;
   isVirtual : Boolean;
   paramPos : TSymbolPosition;
begin
   if not (coSymbolDictionary in FOptions) then Exit;
   if coHintsDisabled in Options then Exit;

   isVirtual:=(funcSym is TMethodSymbol) and TMethodSymbol(funcSym).IsVirtual;

   for param in funcSym.Params do begin
      if not (param is TByRefParamSymbol) then continue;
      if not (param.Typ is TClassSymbol) then continue;

      paramPos:=FSymbolDictionary.FindSymbolUsage(param, suDeclaration);
      if paramPos=nil then continue;

      if param is TConstParamSymbol then begin

         FMsgs.AddCompilerHintFmt(paramPos.ScriptPos, CPH_ReferenceTypeParamAsConst,
                                  [param.Name], hlPedantic);

      end else if not isVirtual then begin

         if param is TVarParamSymbol then begin

            if FSymbolDictionary.FindSymbolUsage(param, suWrite)=nil then
               FMsgs.AddCompilerHintFmt(paramPos.ScriptPos, CPH_ReferenceTypeParamAsVarButNeverWrittenTo,
                                        [param.Name], hlPedantic);

         end;

      end;

   end;
end;

// ReadConnectorSym
//
function TdwsCompiler.ReadConnectorSym(const name : String; baseExpr : TTypedExpr;
            const connectorType : IConnectorType; isWrite : Boolean): TProgramExpr;

   function TryConnectorCall : TConnectorCallExpr;
   var
      argPosArray : TScriptPosArray;
   begin
      // Try to read the call of a connector function
      Result:=TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr, IsWrite);

      ReadArguments(Result.AddArg, ttBLEFT, ttBRIGHT, argPosArray);

      if not Result.AssignConnectorSym(FProg, connectorType) then begin
         Result.BaseExpr:=nil; // freed by caller
         Result.Free;
         Result:=nil;
      end;
  end;

var
   connWrite : TConnectorWriteExpr;
   connRead : TConnectorReadExpr;
begin
   if FTok.Test(ttALEFT) then begin

      Result:=ReadConnectorArray(name, baseExpr, connectorType, isWrite);

   end else if FTok.Test(ttBLEFT) then begin

      Result:=TryConnectorCall;
      if Result=nil then
         Result:=TConstExpr.CreateTypedVariantValue(FProg, FProg.TypVariant, Null); // keep compiling

   end else if not isWrite then begin

      Result:=TConnectorReadExpr.Create(FProg, FTok.HotPos, name, baseExpr);

      if not TConnectorReadExpr(Result).AssignConnectorSym(connectorType) then begin
         Result.Free;
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                                  [name, connectorType.ConnectorCaption]);
      end;

   end else if FTok.TestDelete(ttASSIGN) then begin

      // An assignment of the form "connector.member := expr" was found
      // and is transformed into "connector.member(expr)"
      connWrite:=TConnectorWriteExpr.Create(FProg, FTok.HotPos, name, baseExpr, ReadExpr);

      if not connWrite.AssignConnectorSym(FProg, connectorType) then begin
         connWrite.Free;
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                                  [name, connectorType.ConnectorCaption]);
      end;
      Result:=connWrite;

   end else begin

      // It's possible that we should read a connector member or
      // call a connector function without arguments.
      connRead:=TConnectorReadExpr.Create(FProg, FTok.HotPos, name, baseExpr);

      if not connRead.AssignConnectorSym(connectorType) then begin
         // Don't destroy BaseExpr!
         connRead.baseExpr:=nil;
         connRead.Free;

         // Try to read a connector call
         Result:=TryConnectorCall;
         if not Assigned(Result) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                                     [name, connectorType.ConnectorCaption]);

      end else Result:=connRead;

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

      if IsWrite then begin
         if FTok.TestDelete(ttASSIGN) then
            Result.AddArg(ReadExpr)
         else Result.IsWrite:=False;
      end;

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
function TdwsCompiler.CreateProgram(const systemTable : ISystemSymbolTable;
                                    resultType : TdwsResultType;
                                    const stackParams : TStackParameters) : TdwsMainProgram;
begin
   Result:=TdwsMainProgram.Create(systemTable, resultType, stackParams);
end;

// ReadEnumeration
//
function TdwsCompiler.ReadEnumeration(const typeName : String;
                                      aStyle : TEnumerationSymbolStyle) : TEnumerationSymbol;
var
   name : String;
   elemSym : TElementSymbol;
   constExpr : TTypedExpr;
   enumInt, enumIntPrev : Int64;
   namePos : TScriptPos;
   isUserDef, overflowed : Boolean;
begin
   Result:=TEnumerationSymbol.Create(TypeName, FProg.TypInteger, aStyle);
   try
      if aStyle=enumFlags then
         enumInt:=1
      else enumInt:=0;
      overflowed:=False;

      repeat
         // Read a member of the enumeration
         if not FTok.TestDeleteNamePos(name, namePos) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         // Member has a user defined value
         if FTok.TestDelete(ttEQ) then begin
            if aStyle=enumFlags then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_FlagEnumerationCantHaveUserValues);
            constExpr:=ReadExpr;

            if not constExpr.IsConstant then begin
               constExpr.Free;
               constExpr:=nil;
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
            end else if not FProg.TypInteger.IsCompatible(constExpr.Typ) then begin
               constExpr.Free;
               constExpr:=nil;
               FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);
            end;

            if Assigned(constExpr) then begin
               if aStyle<>enumFlags then
                  enumInt:=constExpr.EvalAsInteger(FExec);
               constExpr.Free;
            end;

            isUserDef:=True;
         end else isUserDef:=False;

         // error for duplicate names
         if Result.Elements.FindLocal(name)<>nil then
            FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);

         // error for overflow
         if overflowed and (not isUserDef) then
            FMsgs.AddCompilerError(namePos, CPE_EnumerationElementOverflow);

         // Create member symbol
         elemSym:=TElementSymbol.Create(name, Result, enumInt, isUserDef);

         enumIntPrev:=enumInt;
         if aStyle=enumFlags then
            enumInt:=enumInt*2
         else Inc(enumInt);
         overflowed:=(enumInt<enumIntPrev);

         // Add member symbol to table and enumeration type
         if aStyle=enumClassic then begin
            FProg.Table.AddSymbol(elemSym);
            elemSym.IncRefCount;
         end;
         Result.AddElement(elemSym);

         // Add member symbol to Symbol Dictionary
         RecordSymbolUse(elemSym, namePos, [suDeclaration]);

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
   rt, rtInterface : TSymbolTable;
   rSym : TSymbol;
   unitSymbol : TUnitSymbol;
   posArray : TScriptPosArray;
begin
   names:=TStringList.Create;
   try
      if coContextMap in FOptions then
         FSourceContextMap.OpenContext(FTok.HotPos, nil, ttUSES);

      ReadNameList(names, posArray, [nloAllowDots]);

      if coContextMap in FOptions then
         FSourceContextMap.CloseContext(FTok.HotPos);

      u:=0;
      rtInterface:=nil;
      if CurrentUnitSymbol<>nil then begin
         if UnitSection=secImplementation then begin
            rt:=CurrentUnitSymbol.ImplementationTable;
            if rt=nil then
               rt:=FProg.Table;
            rtInterface:=CurrentUnitSymbol.InterfaceTable;
            u:=1;
         end else rt:=CurrentUnitSymbol.InterfaceTable;
      end else rt:=FProg.Root.RootTable;
      for x:=0 to names.Count-1 do begin
         if rtInterface<>nil then begin
            unitSymbol:=TUnitSymbol(rtInterface.FindLocal(names[x], TUnitSymbol));
            if (unitSymbol<>nil) and not unitSymbol.Implicit then
               FMsgs.AddCompilerHintFmt(posArray[x], CPH_UnitAlreadyReferredInInterface, [names[x]]);
         end;
         if names.IndexOf(names[x])<x then
            FMsgs.AddCompilerHintFmt(posArray[x], CPH_UnitAlreadyReferred, [names[x]]);

         y:=0;
         z:=-1;
         while (y<rt.Count) do begin
            rSym:=rt[y];
            if rSym.ClassType=TUnitSymbol then begin
               unitSymbol:=TUnitSymbol(rSym);
               if (unitSymbol.Main<>nil) and UnicodeSameText(rSym.Name, names[x]) then begin
                  if unitSymbol.Implicit then
                     unitSymbol.Implicit:=False;
                  z:=rt.IndexOfParent(TUnitSymbol(rSym).Table);
                  if z>=u then begin // uses A,B,A,C => uses A,B,C
                     rt.MoveParent(z,u);
//                     Inc(u);
                  end;
                  Break;
               end;
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
         if coSymbolDictionary in Options then begin
            rSym:=FProg.UnitMains.Find(names[x]);
            FSymbolDictionary.AddSymbol(rSym, posArray[x], [suReference]);
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
   name, part : String;
   namePos, partPos : TScriptPos;
   contextFix : TdwsSourceContext;
   srcUnit : TSourceUnit;
begin
   if not FTok.TestDelete(ttUNIT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_UnitExpected);

   if not FTok.TestDeleteNamePos(name, namePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   while FTok.TestDelete(ttDOT) do begin
      if not FTok.TestDeleteNamePos(part, partPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      name:=name+'.'+part;
   end;

   if CurrentUnitSymbol=nil then begin
      // special case of a unit compiled directly (not through main program)
      srcUnit:=TSourceUnit.Create(name, FProg.Root.RootTable, FProg.UnitMains);
      srcUnit.Symbol.InitializationRank:=FUnits.Count;
      FUnits.Add(srcUnit);
      FCurrentUnitSymbol:=srcUnit.Symbol;
   end;

   if coContextMap in Options then begin
      contextFix:=FSourceContextMap.Current;
      while (contextFix<>nil) and (contextFix.Token<>ttUNIT) do
         contextFix:=contextFix.Parent;
      if contextFix<>nil then begin
         Assert(contextFix.ParentSym=nil);
         contextFix.ParentSym:=CurrentUnitSymbol;
      end;
   end;

   RecordSymbolUse(CurrentUnitSymbol, namePos, [suDeclaration]);
   if not SameText(name, namePos.SourceFile.Name) then
      FMsgs.AddCompilerWarning(namePos, CPE_UnitNameDoesntMatch);

   if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

   if FTok.TestDelete(ttINTERFACE) then begin
      FUnitSection:=secInterface;
      if coContextMap in FOptions then
         FSourceContextMap.OpenContext(FTok.HotPos, CurrentUnitSymbol, ttINTERFACE);
   end else FUnitSection:=secMixed;
   DoSectionChanged;
end;

// CreateAssign
//
function TdwsCompiler.CreateAssign(const scriptPos : TScriptPos; token : TTokenType;
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
               Result:=TNullExpr.Create(FProg, scriptPos);
            end else if left.Typ.ClassType=TClassOfSymbol then begin
               Result:=TAssignClassOfExpr.Create(FProg, scriptPos, left, right);
            end else if left.Typ.ClassType=TInterfaceSymbol then begin
               if right.Typ is TClassSymbol then begin
                  classSymbol:=TClassSymbol(right.Typ);
                  intfSymbol:=TInterfaceSymbol(left.Typ);
                  if not classSymbol.ImplementsInterface(intfSymbol) then
                     FMsgs.AddCompilerErrorFmt(scriptPos, RTE_ObjCastToIntfFailed,
                                               [classSymbol.Name, intfSymbol.Name]);
                  Result:=TAssignExpr.Create(FProg, scriptPos, left, TObjAsIntfExpr.Create(FProg, scriptPos, right, intfSymbol));
               end else Result:=TAssignExpr.Create(FProg, scriptPos, left, right);
            end else if left.Typ.ClassType=TDynamicArraySymbol then begin
               Result:=TAssignExpr.Create(FProg, scriptPos, left, right);
            end else if right.InheritsFrom(TDataExpr) and ((right.Typ.Size<>1) or (right.Typ is TArraySymbol)) then begin
               if right.InheritsFrom(TFuncExpr) then
                  TFuncExpr(right).SetResultAddr(FProg, nil);
               if right.InheritsFrom(TArrayConstantExpr) then
                  Result:=TAssignArrayConstantExpr.Create(FProg, scriptPos, left, TArrayConstantExpr(right))
               else Result:=TAssignDataExpr.Create(FProg, scriptPos, left, right)
            end else if left.Typ is TFuncSymbol then begin
               if (right.Typ is TFuncSymbol) or (right.Typ is TNilSymbol) then begin
                  if right is TFuncRefExpr then begin
                     right:=TFuncRefExpr(right).Extract;
                     if right is TFuncPtrExpr then begin
                        right:=TFuncPtrExpr(right).Extract;
                        Result:=TAssignExpr.Create(FProg, scriptPos, left, right);
                     end else begin
                        Assert(right is TFuncExprBase);
                        Result:=TAssignFuncExpr.Create(FProg, scriptPos, left, right);
                     end;
                  end else begin
                     Result:=TAssignExpr.Create(FProg, scriptPos, left, right);
                  end;
               end else begin
                  FMsgs.AddCompilerError(scriptPos, CPE_IncompatibleOperands);
                  Result:=TAssignExpr.Create(FProg, scriptPos, left, right); // keep going
               end;
            end else begin
               Result:=TAssignExpr.Create(FProg, scriptPos, left, right);
            end;
         end;
         ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN, ttCARET_ASSIGN : begin
            if left.Typ is TClassSymbol then begin

               classOpSymbol:=(left.Typ as TClassSymbol).FindClassOperator(token, right.Typ);
               if classOpSymbol=nil then
                  FMsgs.AddCompilerStop(scriptPos, CPE_IncompatibleOperands);
               classOpExpr:=GetMethodExpr(classOpSymbol.UsesSym, left, rkObjRef, scriptPos, False);
               try
                  classOpExpr.AddArg(right);
                  TypeCheckArgs(classOpExpr, nil);
               except
                  classOpExpr.Free;
                  raise;
               end;
               Result:=TNoResultWrapperExpr.Create(FProg, scriptPos, classOpExpr);

            end else begin

               assignOpExpr:=CreateAssignOperatorExpr(token, scriptPos, left, right);
               if assignOpExpr=nil then begin
                  FMsgs.AddCompilerError(scriptPos, CPE_IncompatibleOperands);
                  Result:=TAssignExpr.Create(FProg, scriptPos, left, right);
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
      FMsgs.AddCompilerError(scriptPos, CPE_RightSideNeedsReturnType);
      Result:=TNullExpr.Create(FProg, scriptPos);

   end;
end;

// CreateArrayLow
//
function TdwsCompiler.CreateArrayLow(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
begin
   if typ is TStaticArraySymbol then
      Result:=TConstExpr.CreateIntegerValue(FProg, TStaticArraySymbol(typ).LowBound)
   else if typ is TDynamicArraySymbol then
      Result:=TConstExpr.CreateIntegerValue(FProg, 0)
   else Result:=nil;
   if captureBase then
      baseExpr.Free;
end;

// CreateArrayHigh
//
function TdwsCompiler.CreateArrayHigh(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
begin
   if typ is TOpenArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TOpenArrayLengthExpr.Create(FProg, TDataExpr(baseExpr), captureBase);
      TOpenArrayLengthExpr(Result).Delta:=-1;
   end else if typ is TDynamicArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TArrayLengthExpr.Create(FProg, baseExpr as TTypedExpr, captureBase);
      TArrayLengthExpr(Result).Delta:=-1;
   end else begin
      if captureBase then
         baseExpr.Free;
      if typ is TStaticArraySymbol then begin
         Result:=TConstExpr.CreateIntegerValue(FProg, TStaticArraySymbol(typ).HighBound);
      end else Result:=nil;
   end;
end;

// CreateArrayLength
//
function TdwsCompiler.CreateArrayLength(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
begin
   if typ is TOpenArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TOpenArrayLengthExpr.Create(FProg, TDataExpr(baseExpr), True);
   end else if typ is TDynamicArraySymbol then begin
      if baseExpr=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
      Result:=TArrayLengthExpr.Create(FProg, baseExpr, True);
   end else if typ is TStaticArraySymbol then begin
      baseExpr.Free;
      Result:=TConstExpr.CreateIntegerValue(FProg, TStaticArraySymbol(typ).ElementCount);
   end else Result:=nil;
end;

// CreateArrayExpr
//
function TdwsCompiler.CreateArrayExpr(const scriptPos : TScriptPos; baseExpr : TDataExpr; indexExpr : TTypedExpr) : TArrayExpr;
var
   baseType : TArraySymbol;
begin
   baseType:=baseExpr.Typ as TArraySymbol;

   if baseType is TStaticArraySymbol then begin

      if baseType is TOpenArraySymbol then
         Result:=TOpenArrayExpr.Create(FProg, scriptPos, baseExpr, indexExpr)
      else begin
         Result:=TStaticArrayExpr.Create(FProg, scriptPos, baseExpr, indexExpr,
                                         TStaticArraySymbol(baseType).LowBound,
                                         TStaticArraySymbol(baseType).HighBound);
      end;

   end else if baseType is TDynamicArraySymbol then begin

      Result:=TDynamicArrayExpr.Create(FProg, scriptPos, baseExpr, indexExpr);

   end else Result:=nil;
end;

type
   // manual "anonymous method" because compiler goes into internal error hell otherwise
   TOperatorResolver = class
      Resolved : TOperatorSymbol;
      LeftType, RightType : TTypeSymbol;
      function Callback(opSym : TOperatorSymbol) : Boolean;
   end;

// Callback
//
function TOperatorResolver.Callback(opSym : TOperatorSymbol) : Boolean;
begin
   Result:=(opSym.Params[0]=LeftType) and (opSym.Params[1]=RightType);
   if Result or (Resolved=nil) then
      Resolved:=opSym;
end;

// ResolveOperatorFor
//
function TdwsCompiler.ResolveOperatorFor(token : TTokenType; aLeftType, aRightType : TTypeSymbol) : TOperatorSymbol;
var
   resolver : TOperatorResolver;
begin
   resolver:=TOperatorResolver.Create;
   try
      resolver.LeftType:=aLeftType;
      resolver.RightType:=aRightType;
      if FProg.Table.EnumerateOperatorsFor(token, aLeftType, aRightType, resolver.Callback) then
         Exit(resolver.Resolved);
      FOperators.EnumerateOperatorsFor(token, aLeftType, aRightType, resolver.Callback);
      Result:=resolver.Resolved;
   finally
      resolver.Free;
   end;
end;

// CreateTypedOperatorExpr
//
function TdwsCompiler.CreateTypedOperatorExpr(token : TTokenType; aLeft, aRight : TTypedExpr) : TTypedExpr;
var
   opSym : TOperatorSymbol;
   funcExpr : TFuncExprBase;
begin
   Result:=nil;
   if (aLeft=nil) or (aRight=nil) then Exit;
   opSym:=ResolveOperatorFor(token, aLeft.Typ, aRight.Typ);
   if opSym<>nil then begin
      if opSym.BinExprClass<>nil then
         Result:=TBinaryOpExprClass(opSym.BinExprClass).Create(FProg, aLeft, aRight)
      else if opSym.UsesSym<>nil then begin
         if opSym.UsesSym is TMethodSymbol then begin
            funcExpr:=CreateMethodExpr(FProg, TMethodSymbol(opSym.UsesSym), aLeft, rkObjRef, FTok.HotPos)

         end else begin
            funcExpr:=GetFuncExpr(opSym.UsesSym);
            funcExpr.AddArg(aLeft);
         end;
         funcExpr.AddArg(aRight);
         TypeCheckArgs(funcExpr, nil);
         if Optimize then
            Result:=funcExpr.OptimizeToTypedExpr(FProg, FExec, funcExpr.ScriptPos)
         else Result:=funcExpr;
      end;
   end;
end;

// CreateAssignOperatorExpr
//
function TdwsCompiler.CreateAssignOperatorExpr(token : TTokenType; const scriptPos : TScriptPos;
                                               aLeft, aRight : TTypedExpr) : TAssignExpr;
var
   opSym : TOperatorSymbol;
begin
   Result:=nil;
   if (aLeft=nil) or (aRight=nil) then Exit;
   opSym:=ResolveOperatorFor(token, aLeft.Typ, aRight.Typ);
   if opSym<>nil then begin
      if opSym.AssignExprClass<>nil then
         Result:=TAssignExprClass(opSym.AssignExprClass).Create(FProg, scriptPos, aLeft as TDataExpr, aRight);
   end;
end;

// DoSectionChanged
//
procedure TdwsCompiler.DoSectionChanged;
begin
   if Assigned(FOnSectionChanged) then
      FOnSectionChanged(Self);
end;

// DoTokenizerEndSourceFile
//
procedure TdwsCompiler.DoTokenizerEndSourceFile(sourceFile : TSourceFile);
begin
   Inc(FLineCount, FTok.CurrentPos.Line-2);
   if     (coContextMap in Options)
      and (FSourceContextMap.Current<>nil)
      and (FSourceContextMap.Current.StartPos.SourceFile=sourceFile) then
      FSourceContextMap.CloseContext(FTok.CurrentPos);
end;

// EnterUnit
//
procedure TdwsCompiler.EnterUnit(unitSymbol : TUnitMainSymbol; var oldUnitSymbol : TUnitMainSymbol);
begin
   FCurrentUnitSymbol.StoreParents;
   oldUnitSymbol:=FCurrentUnitSymbol;
   unitSymbol.RestoreParents;
   FCurrentUnitSymbol:=unitSymbol;
end;

// LeaveUnit
//
procedure TdwsCompiler.LeaveUnit(oldUnitSymbol : TUnitMainSymbol);
begin
   FCurrentUnitSymbol.StoreParents;
   FCurrentUnitSymbol:=oldUnitSymbol;
   FCurrentUnitSymbol.RestoreParents;
end;

// SwitchTokenizerToUnit
//
procedure TdwsCompiler.SwitchTokenizerToUnit(srcUnit : TSourceUnit; const sourceCode : String);
var
   sourceFile : TSourceFile;
   oldUnit : TUnitMainSymbol;
begin
   sourceFile:=FMainProg.SourceList.Add(srcUnit.GetUnitName, sourceCode, stUnit);

   EnterUnit(srcUnit.Symbol, oldUnit);
   FProg.EnterSubTable(CurrentUnitSymbol.Table);
   FUnitsFromStack.Push(sourceFile.Name);
   try
      ReadScript(sourceFile, stUnit);
   finally
      FUnitsFromStack.Pop;
      LeaveUnit(oldUnit);
      FProg.LeaveSubTable;
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
      Result:=(FTok.ConditionalDefines.Value.IndexOf(name)>=0);
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

   FTok.HasTokens;  // get token in buffer for correct argPos below
   argPos:=FTok.HotPos;
   case specialKind of
      skAssigned :
         argExpr:=ReadExpr(FProg.TypNil);
      skInc, skDec :
         argExpr:=ReadTerm(True);
      skLow, skHigh :
         argExpr:=ReadExpr(FAnyTypeSymbol);
   else
      argExpr:=ReadExpr;
   end;
   argTyp:=argExpr.Typ;
   if argTyp<>nil then
      argTyp:=argTyp.UnAliasedType;

   msgExpr:=nil;
   try
      if not Assigned(argTyp) then
         FMsgs.AddCompilerStop(argPos, CPE_InvalidOperands);

      Result := nil;

      case specialKind of
         skAbs : begin
            if argTyp.IsOfType(FProg.TypInteger) then
               Result:=TAbsIntExpr.Create(FProg, argExpr)
            else if argTyp.IsOfType(FProg.TypFloat) then
               Result:=TAbsFloatExpr.Create(FProg, argExpr)
            else if argTyp.IsOfType(FProg.TypVariant) then
               Result:=TAbsVariantExpr.Create(FProg, argExpr)
            else begin
               Result:=dwsInternalUnit.HandleAbs(FProg, argExpr);
               if Result=nil then
                  FMsgs.AddCompilerError(argPos, CPE_NumericalExpected);
            end;
            argExpr:=nil;
         end;
         skAssert : begin
            if not argTyp.IsOfType(FProg.TypBoolean) then
               FMsgs.AddCompilerError(argPos, CPE_BooleanExpected);
            if FTok.TestDelete(ttCOMMA) then begin
               FTok.HasTokens;
               argPos:=FTok.HotPos;
               msgExpr:=ReadExpr;
               if (msgExpr=nil) or (not msgExpr.IsOfType(FProg.TypString)) then
                  FMsgs.AddCompilerError(argPos, CPE_StringExpected);
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
            if (argTyp is TClassSymbol) or (argTyp is TInterfaceSymbol) then
               Result:=TAssignedInstanceExpr.Create(FProg, argExpr)
            else if argTyp is TClassOfSymbol then
               Result:=TAssignedMetaClassExpr.Create(FProg, argExpr)
            else if argTyp is TFuncSymbol then
               Result:=TAssignedFuncPtrExpr.Create(FProg, argExpr)
            else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
               argExpr.Free;
            end;
            argExpr:=nil;
         end;
         skHigh : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayHigh(argExpr, TArraySymbol(argTyp), True);
            end else if argTyp is TEnumerationSymbol then begin
               argExpr.Free;
               Result:=TConstExpr.CreateIntegerValue(FProg, argTyp, TEnumerationSymbol(argTyp).HighBound)
            end else if argTyp is TDynamicArraySymbol and Assigned(argExpr) then begin
               Result:=TArrayLengthExpr.Create(FProg, TDataExpr(argExpr), True);
               TArrayLengthExpr(Result).Delta:=-1;
            end else if argTyp.IsOfType(FProg.TypString) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FProg, argExpr);
            end else if argTyp=FProg.TypInteger then begin
               argExpr.Free;
               Result:=TConstExpr.CreateIntegerValue(FProg, High(Int64));
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
               argExpr.Free;
            end;
            argExpr:=nil;
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
                  if operandExpr=nil then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);
                  if operandExpr.IsOfType(FProg.TypVariant) then
                     operandExpr:=TConvIntegerExpr.Create(FProg, operandExpr);
                  if not operandExpr.IsOfType(FProg.TypInteger) then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);
               end else operandExpr:=TConstExpr.CreateIntegerValue(FProg, 1);
               case specialKind of
                  skInc : Result:=TIncVarFuncExpr.Create(FProg, argPos, argExpr, operandExpr);
                  skDec : Result:=TDecVarFuncExpr.Create(FProg, argPos, argExpr, operandExpr);
                  skSucc : Result:=TSuccFuncExpr.Create(FProg, argPos, argExpr, operandExpr);
                  skPred : Result:=TPredFuncExpr.Create(FProg, argPos, argExpr, operandExpr);
               else
                  Assert(False);
               end;
               argExpr:=nil;
            end else FMsgs.AddCompilerError(argPos, CPE_IntegerExpected);
         end;
         skLength : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayLength(argExpr, TArraySymbol(argTyp));
            end else if ((argTyp=FProg.TypString) or (argTyp=FProg.TypVariant)) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FProg, argExpr);
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
               argExpr.Free;
            end;
            argExpr:=nil;
         end;
         skLow : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayLow(argExpr, TArraySymbol(argTyp), True);
            end else begin
               argExpr.Free;
               if argTyp is TEnumerationSymbol then begin
                  Result:=TConstExpr.CreateIntegerValue(FProg, argTyp, TEnumerationSymbol(argTyp).LowBound);
               end else if argTyp=FProg.TypString then begin
                  Result:=TConstExpr.CreateIntegerValue(FProg, 1);
               end else if (argTyp=FProg.TypInteger) then begin
                  Result:=TConstExpr.CreateIntegerValue(FProg, Low(Int64));
               end else begin
                  FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
               end;
            end;
            argExpr:=nil;
         end;
         skSqr : begin
            if argTyp=FProg.TypInteger then begin
               Result:=TSqrIntExpr.Create(FProg, argExpr);
            end else if argTyp=FProg.TypFloat then begin
               Result:=TSqrFloatExpr.Create(FProg, argExpr);
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_NumericalExpected);
               argExpr.Free;
            end;
            argExpr:=nil;
         end;
         skOrd : begin
            if argTyp.IsOfType(FProg.TypInteger) or argTyp.InheritsFrom(TEnumerationSymbol) then begin
               Result:=TOrdIntExpr.Create(FProg, argExpr);
            end else if argTyp=FProg.TypBoolean then begin
               Result:=TOrdBoolExpr.Create(FProg, argExpr);
            end else if argTyp=FProg.TypString then begin
               Result:=TOrdStrExpr.Create(FProg, argExpr);
            end else if argTyp=FProg.TypVariant then begin
               Result:=TOrdExpr.Create(FProg, argExpr);
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
               argExpr.Free;
            end;
            argExpr:=nil;
         end;
         skSizeOf : begin
            Result:=TConstExpr.CreateIntegerValue(FProg, argTyp.Size);
            argExpr.Free;
            argExpr:=nil;
         end;
         skDefined, skDeclared : begin
            if FIsSwitch then begin
               if not argExpr.IsOfType(FProg.TypString) then
                  FMsgs.AddCompilerStop(argPos, CPE_StringExpected);
               if not argExpr.IsConstant then
                  FMsgs.AddCompilerStop(argPos, CPE_ConstantExpressionExpected);
               try
                  case SpecialKind of
                     skDefined :
                        Result:=TConstBooleanExpr.CreateUnified(FProg, FProg.TypBoolean, EvaluateDefined(argExpr));
                     skDeclared :
                        Result:=TConstBooleanExpr.CreateUnified(FProg, FProg.TypBoolean, EvaluateDeclared(argExpr));
                  end;
               finally
                  argExpr.Free;
                  argExpr:=nil;
               end;
            end else begin
               case SpecialKind of
                  skDefined : begin
                     if argExpr.Typ=nil then
                        FMsgs.AddCompilerError(argPos, CPE_ExpressionExpected);
                     Result:=TDefinedExpr.Create(FProg, argExpr);
                  end;
                  skDeclared : begin
                     if not argExpr.IsOfType(FProg.TypString) then
                        FMsgs.AddCompilerError(argPos, CPE_StringExpected);
                     Result:=TDeclaredExpr.Create(FProg, argExpr);
                  end;
               end;
               argExpr:=nil;
            end;
         end;
         skConditionalDefined : begin
            if not argExpr.IsOfType(FProg.TypString) then
               FMsgs.AddCompilerError(argPos, CPE_StringExpected);
            Result:=TConditionalDefinedExpr.Create(FProg, argExpr);
            argExpr:=nil;
         end;
         skSwap : begin
            if not ((argExpr is TDataExpr) and TDataExpr(argExpr).IsWritable) then begin
               FMsgs.AddCompilerError(argPos, CPE_VariableExpected);
               argExpr.Free;
               argExpr:=nil;
            end;
            if not FTok.TestDelete(ttCOMMA) then begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_CommaExpected);
               msgExpr:=nil;
            end else begin
               FTok.TestName;
               argPos:=FTok.HotPos;
               msgExpr:=ReadExpr(argTyp);
               if not ((msgExpr is TDataExpr) and TDataExpr(msgExpr).IsWritable) then begin
                  FMsgs.AddCompilerError(argPos, CPE_VariableExpected);
                  msgExpr.Free;
                  msgExpr:=nil;
               end else if (argExpr<>nil) then begin
                  if (msgExpr=nil) or not (msgExpr.IsOfType(argTyp) and argTyp.IsOfType(msgExpr.Typ)) then
                     IncompatibleTypes(namePos, CPE_IncompatibleTypes, argTyp, msgExpr.Typ);
               end;
            end;
            Result:=TSwapExpr.Create(FProg, namePos, TDataExpr(argExpr), TDataExpr(msgExpr));
            argExpr:=nil;
            msgExpr:=nil;
         end;
      end;

      if argExpr<>nil then begin
         argExpr.Free;
         argExpr:=nil;
      end;
      if Result=nil then begin
         // fake expression to keep compiling
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
   hotPos:=FTok.CurrentPos;
   argExpr:=ReadExpr;

   Result:=nil;
   try
      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

      if typeSym.IsOfType(FProg.TypInteger) then begin

         // Cast Integer(...)
         Result := TConvIntegerExpr.Create(FProg, argExpr);
         Result.Typ:=typeSym;
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

      else if typeSym is TClassOfSymbol then begin

         // Cast Class(...)
         if argExpr.Typ is TClassSymbol then
            Result:=TObjAsClassExpr.Create(FProg, hotPos, argExpr, typeSym)
         else if argExpr.Typ is TClassOfSymbol then
            Result:=TClassAsClassExpr.Create(FProg, hotPos, argExpr, typeSym)
         else FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

      end else FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

      if Optimize then
         Result:=Result.OptimizeToTypedExpr(FProg, FExec, hotPos);

  except
    argExpr.Free;
    raise;
  end;
end;

// ReadTypeExpr
//
function TdwsCompiler.ReadTypeExpr(const namePos : TScriptPos; typeSym : TTypeSymbol;
                                   isWrite : Boolean; expecting : TTypeSymbol = nil) : TProgramExpr;

   function CreateClassSymbolExpr(typeSym : TTypeSymbol) : TConstExpr;
   begin
      Result:=TConstExpr.Create(FProg, typeSym, Int64(TClassOfSymbol(typeSym).TypClassSymbol));
   end;

var
   typeExpr : TTypedExpr;
begin
   if typeSym.ClassType=TClassOfSymbol then
      typeExpr:=CreateClassSymbolExpr(typeSym)
   else typeExpr:=TTypeReferenceExpr.Create(typeSym, namePos);

   if FTok.Test(ttDOT) then
      Result:=ReadSymbol(typeExpr, isWrite, expecting)
   else begin
      if     (expecting<>FAnyTypeSymbol)
         and not (   (typeSym.ClassType=TClassSymbol)
                  or (typeSym.ClassType=TClassOfSymbol)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackLeftExpected);
      Result:=typeExpr;
   end;
end;

// ReadAttributes
//
procedure TdwsCompiler.ReadAttributes;
var
   expr : TProgramExpr;
   customAttribute : TClassSymbol;
   hotPos : TScriptPos;
begin
   if not FTok.Test(ttALEFT) then Exit;

   customAttribute:=FProg.Root.SystemTable.SymbolTable.TypCustomAttribute;

   while FTok.TestDelete(ttALEFT) do begin
      hotPos:=FTok.HotPos;
      expr:=ReadNew(customAttribute);
      if not ((expr is TMethodExpr) and (TMethodExpr(expr).MethSym.Kind=fkConstructor)) then
         FMsgs.AddCompilerError(hotPos, CPE_AttributeConstructorExpected);
      FPendingAttributes.Add(TdwsSymbolAttribute.Create(hotPos, TMethodExpr(expr)));
      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(hotPos, CPE_ArrayBracketRightExpected);
   end;
end;

// BagPendingAttributes
//
function TdwsCompiler.BagPendingAttributes : ISymbolAttributesBag;
var
   bag : TSymbolAttributesBag;
begin
   if FPendingAttributes.Count>0 then begin
      bag:=TSymbolAttributesBag.Create;
      bag.FAttributes:=FPendingAttributes;
      FPendingAttributes:=TdwsSymbolAttributes.Create;
      Result:=bag;
   end else Result:=nil;
end;

// AttachBaggedAttributes
//
procedure TdwsCompiler.AttachBaggedAttributes(symbol : TSymbol; const bag : ISymbolAttributesBag);
var
   i : Integer;
   attrs : TdwsSymbolAttributes;
   attr : TdwsSymbolAttribute;
begin
   if bag=nil then Exit;

   attrs:=bag.Attributes;
   for i:=0 to attrs.Count-1 do begin
      attr:=attrs[i];
      attr.Symbol:=symbol;
      FMainProg.Attributes.Add(attr);
   end;
   attrs.ExtractAll;
end;

// CheckNoPendingAttributes
//
procedure TdwsCompiler.CheckNoPendingAttributes;

   procedure ErrorDanglingAttributes;
   var
      i : Integer;
      attr : TdwsSymbolAttribute;
   begin
      for i:=0 to FPendingAttributes.Count-1 do begin
         attr:=FPendingAttributes[i];
         FMsgs.AddCompilerError(attr.ScriptPos, CPE_DanglingAttribute);
      end;
      FPendingAttributes.Clear;
   end;

begin
   if FPendingAttributes.Count>0 then
      ErrorDanglingAttributes;
end;

// EnumerateHelpers
//
function TdwsCompiler.EnumerateHelpers(typeSym : TTypeSymbol) : THelperSymbols;
begin
   Result:=THelperSymbols.Create;
   if typeSym.ClassType=THelperSymbol then
      Result.Add(THelperSymbol(typeSym))
   else FProg.Table.EnumerateHelpers(typeSym, Result.AddHelper);
end;

// ReadTypeHelper
//
function TdwsCompiler.ReadTypeHelper(expr : TTypedExpr;
                                     const name : String; const namePos : TScriptPos;
                                     expecting : TTypeSymbol) : TProgramExpr;
var
   i : Integer;
   helper : THelperSymbol;
   helpers : THelperSymbols;
   sym : TSymbol;
   meth : TMethodSymbol;
   meta : TStructuredTypeMetaSymbol;
   typeSym : TTypeSymbol;
begin
   Result:=nil;
   typeSym:=expr.Typ;

   helpers:=EnumerateHelpers(typeSym);
   try
      if helpers.Count=0 then Exit;

      for i:=0 to helpers.Count-1 do begin
         helper:=helpers[i];
         sym:=helper.Members.FindSymbol(name, cvPublic);
         if sym=nil then continue;

         RecordSymbolUseReference(sym, namePos, False);

         if sym.ClassType=TPropertySymbol then begin

            Result:=ReadPropertyExpr(expr, TPropertySymbol(sym), FTok.Test(ttASSIGN));

         end else if sym.ClassType=TClassVarSymbol then begin

            expr.Free;
            expr:=nil;
            Result:=GetVarExpr(TClassVarSymbol(sym));

         end else if sym is TConstSymbol then begin

            expr.Free;
            expr:=nil;
            Result:=TConstExpr.CreateTyped(FProg, sym.Typ, TConstSymbol(sym));

         end else if sym is TMethodSymbol then begin

            meth:=TMethodSymbol(sym);
            if meth.IsClassMethod then begin

               if (helper.ForType is TStructuredTypeSymbol) then begin
                  meta:=TStructuredTypeSymbol(helper.ForType).MetaSymbol;
                  if meta<>nil then begin
                     if expr<>nil then begin
                        if expr.Typ is TStructuredTypeSymbol then
                           expr:=TObjToClassTypeExpr.Create(FProg, expr)
                     end else expr:=TConstExpr.Create(FProg, meta, Int64(helper.ForType));
                  end else begin
                     expr.Free;
                     expr:=nil;
                  end;
               end;

            end else begin

               if typeSym.ClassType<>THelperSymbol then begin
                  if    (expr is TTypeReferenceExpr)
                     or (expr.Typ is TStructuredTypeMetaSymbol) then begin
                     FMsgs.AddCompilerError(namePos, CPE_ClassMethodExpected);
                     // keep compiling
                     expr:=TConvExpr.Create(Fprog, expr);
                     expr.Typ:=meth.Params[0].Typ;
                  end;
               end;

            end;

            if (expr<>nil) and (expr.Typ is THelperSymbol) then begin
               expr.Free;
               expr:=nil;
            end;

            if meth.IsOverloaded then
               Result:=ReadMethOverloaded(meth, expr, namePos, expecting)
            else Result:=ReadMethod(meth, expr, namePos, expecting);

         end;
         Break;
      end;
   finally
      helpers.Free;
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
   FConnectors := TStringList.Create;
   FScriptPaths := TStringList.Create;
   FConditionals := TStringList.Create;
   FUnits := TIdwsUnitList.Create;
   FUnits.Add(dwsInternalUnit);
   FStackChunkSize := cDefaultStackChunkSize;
   FDefaultResultType := TdwsDefaultResultType.Create(nil);
   FResultType := FDefaultResultType;
   FCompilerOptions := cDefaultCompilerOptions;
   FHintsLevel := hlStrict;
   FMaxRecursionDepth := cDefaultMaxRecursionDepth;
   FMaxExceptionDepth := cDefaultMaxExceptionDepth;
end;

destructor TdwsConfiguration.Destroy;
begin
   inherited;
   FSystemTable:=nil;
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

// InitSystemTable
//
procedure TdwsConfiguration.InitSystemTable;
var
   clsDelphiException, clsAssertionFailed : TClassSymbol;
   meth : TMethodSymbol;
   fldSym : TFieldSymbol;
   propSym : TPropertySymbol;
   sysTable : TSystemSymbolTable;
begin
   sysTable:=TSystemSymbolTable.Create(nil);
   FSystemTable:=sysTable;

   // Create base data types
   sysTable.TypBoolean:=TBaseBooleanSymbol.Create;
   sysTable.AddSymbol(sysTable.TypBoolean);

   sysTable.TypFloat:=TBaseFloatSymbol.Create;
   sysTable.AddSymbol(sysTable.TypFloat);

   sysTable.TypInteger:=TBaseIntegerSymbol.Create;
   sysTable.AddSymbol(sysTable.TypInteger);

   sysTable.TypString:=TBaseStringSymbol.Create;
   sysTable.AddSymbol(sysTable.TypString);
   sysTable.AddSymbol(TDynamicArraySymbol.Create('array of string', sysTable.TypString, sysTable.TypInteger));

   if Assigned(FOnCreateBaseVariantSymbol) then
      FOnCreateBaseVariantSymbol(sysTable)
   else begin
      sysTable.TypVariant:=TBaseVariantSymbol.Create;
      sysTable.AddSymbol(sysTable.TypVariant);
   end;
   if sysTable.TypVariant<>nil then begin
      sysTable.AddSymbol(TConstSymbol.Create('Null', sysTable.TypVariant, Null));
      sysTable.AddSymbol(TConstSymbol.Create('Unassigned', sysTable.TypVariant, Unassigned));
      sysTable.AddSymbol(TOpenArraySymbol.Create('array of const', sysTable.TypVariant, sysTable.TypInteger));
   end;

   sysTable.TypInterface:=TInterfaceSymbol.Create(SYS_IINTERFACE, nil);
   sysTable.AddSymbol(sysTable.TypInterface);

   // Create "root" class TObject
   sysTable.TypObject:=TClassSymbol.Create(SYS_TOBJECT, nil);
   sysTable.AddSymbol(sysTable.TypObject);
   // Add constructor Create
   meth:=TMethodSymbol.Create(SYS_TOBJECT_CREATE, fkConstructor, sysTable.TypObject, cvPublic, False);
   meth.Executable:=ICallable(TEmptyFunc.Create);
   meth.IsDefault:=True;
   sysTable.TypObject.AddMethod(meth);
   // Add destructor Destroy
   TObjectDestroyMethod.Create(mkDestructor, [maVirtual], SYS_TOBJECT_DESTROY,
                               [], '', sysTable.TypObject, cvPublic, sysTable);
   // Add procedure Free
   TObjectFreeMethod.Create(mkProcedure, [], SYS_TOBJECT_FREE,
                            [], '', sysTable.TypObject, cvPublic, sysTable);
   // Add ClassName method
   TObjectClassNameMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSNAME,
                                 [], SYS_STRING, sysTable.TypObject, cvPublic, sysTable);

   // Create "root" metaclass TClass
   sysTable.TypClass:=TClassOfSymbol.Create(SYS_TCLASS, sysTable.TypObject);
   sysTable.AddSymbol(sysTable.TypClass);

   // Add ClassType method
   TObjectClassTypeMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSTYPE,
                                 [], SYS_TCLASS, sysTable.TypObject, cvPublic, sysTable);

   // Add ClassParent method
   TObjectClassParentMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSPARENT,
                                   [], SYS_TCLASS, sysTable.TypObject, cvPublic, sysTable);

   // Create class Exception
   sysTable.TypException := TClassSymbol.Create(SYS_EXCEPTION, nil);
   sysTable.TypException.InheritFrom(sysTable.TypObject);
   fldSym:=TFieldSymbol.Create(SYS_EXCEPTION_MESSAGE_FIELD, sysTable.TypString, cvProtected);
   sysTable.TypException.AddField(fldSym);
   propSym:=TPropertySymbol.Create(SYS_EXCEPTION_MESSAGE, sysTable.TypString, cvPublic);
   propSym.ReadSym:=fldSym;
   propSym.WriteSym:=fldSym;
   sysTable.TypException.AddProperty(propSym);
   TExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
                                 ['Msg', SYS_STRING], '', sysTable.TypException, cvPublic, sysTable);
   TExceptionDestroyMethod.Create(mkDestructor, [maOverride], SYS_TOBJECT_DESTROY,
                                 [], '', sysTable.TypException, cvPublic, sysTable);
   TExceptionStackTraceMethod.Create(mkFunction, [], SYS_EXCEPTION_STACKTRACE,
                                 [], SYS_STRING, sysTable.TypException, cvPublic, sysTable);
   sysTable.AddSymbol(sysTable.TypException);

   // Create class EAssertionFailed
   clsAssertionFailed := TClassSymbol.Create(SYS_EASSERTIONFAILED, nil);
   clsAssertionFailed.InheritFrom(sysTable.TypException);
   sysTable.AddSymbol(clsAssertionFailed);

   // Create class EDelphi
   clsDelphiException := TClassSymbol.Create(SYS_EDELPHI, nil);
   clsDelphiException.InheritFrom(sysTable.TypException);
   fldSym:=TFieldSymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS_FIELD, sysTable.TypString, cvProtected);
   clsDelphiException.AddField(fldSym);
   propSym:=TPropertySymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS, sysTable.TypString, cvPublic);
   propSym.ReadSym:=fldSym;
   propSym.WriteSym:=fldSym;
   clsDelphiException.AddProperty(propSym);
   TDelphiExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
                                       ['Cls', SYS_STRING, 'Msg', SYS_STRING], '',
                                       clsDelphiException, cvPublic, sysTable);
   sysTable.AddSymbol(clsDelphiException);

   // Create TCustomAttribute
   sysTable.TypCustomAttribute := TClassSymbol.Create(SYS_TCUSTOMATTRIBUTE, nil);
   sysTable.TypCustomAttribute.InheritFrom(sysTable.TypObject);
   sysTable.AddSymbol(sysTable.TypCustomAttribute);

   // ExceptObj function
   TExceptObjFunc.Create(sysTable, 'ExceptObject', [], SYS_EXCEPTION, []);

   // Runtime parameters
   if sysTable.TypVariant<>nil then
      TParamFunc.Create(sysTable, 'Param', ['Index', SYS_INTEGER], SYS_VARIANT, []);
   TParamStrFunc.Create(sysTable, 'ParamStr', ['Index', SYS_INTEGER], SYS_STRING, []);
   TParamCountFunc.Create(sysTable, 'ParamCount', [], SYS_INTEGER, []);
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
      else if AComponent=FLocalizer then
         SetLocalizer(nil)
      else begin
         // the samùe file system can be referred in two roles
         if AComponent=CompileFileSystem then
            CompileFileSystem:=nil;
         if AComponent=RuntimeFileSystem then
            RuntimeFileSystem:=nil;
      end;
   end;
end;

// DetachSystemTable
//
procedure TdwsConfiguration.DetachSystemTable;
begin
   FSystemTable:=nil;
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

// GetSystemTable
//
function TdwsConfiguration.GetSystemTable : ISystemSymbolTable;
begin
   if FSystemTable=nil then
      InitSystemTable;
   Result:=FSystemTable;
end;

// SetLocalizer
//
procedure TdwsConfiguration.SetLocalizer(const val : TdwsLocalizerComponent);
begin
   if FLocalizer<>nil then
      FLocalizer.RemoveFreeNotification(FOwner);
   FLocalizer:=val;
   if FLocalizer<>nil then
      FLocalizer.FreeNotification(FOwner);
end;

// DoGetLocalizer
//
function TdwsConfiguration.DoGetLocalizer : IdwsLocalizer;
begin
   if FLocalizer<>nil then
      Result:=FLocalizer.GetLocalizer
   else Result:=nil;
end;

// ------------------
// ------------------ TObjectClassNameMethod ------------------
// ------------------

// Execute
//
procedure TObjectClassNameMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
var
   classSym : TClassSymbol;
begin
   classSym:=info.ValueAsClassSymbol[SYS_SELF];
   if classSym=nil then
      raise EScriptError.CreatePosFmt(info.Execution.CallStackLastExpr.ScriptPos, RTE_ClassTypeIsNil, []);
   Info.ResultAsString:=classSym.Name;
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
// ------------------ TObjectClassParentMethod ------------------
// ------------------

// Execute
//
procedure TObjectClassParentMethod.Execute(info : TProgramInfo; var ExternalObject: TObject);
begin
   Info.ResultAsInteger:=Int64(info.ValueAsClassSymbol[SYS_SELF].Parent);
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
   ExternalObject.Free;
   ExternalObject:=nil;
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

function TdwsFilter.Process(const Text: String; Msgs: TdwsMessageList): String;
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
   context.UnitSymbol:=compiler.CurrentUnitSymbol;
   context.Context:=compiler.FSourceContextMap.SuspendContext;
   Push(context);
end;

// PopContext
//
procedure TdwsCompilerUnitContextStack.PopContext(compiler : TdwsCompiler; var oldUnitSymbol : TUnitMainSymbol);
begin
   compiler.FTok:=Peek.Tokenizer;
   compiler.EnterUnit(Peek.UnitSymbol, oldUnitSymbol);
   compiler.FSourceContextMap.ResumeContext(Peek.Context);
   Pop;
end;

end.

