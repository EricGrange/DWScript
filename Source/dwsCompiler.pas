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
  Classes, SysUtils, TypInfo, Variants,
  dwsFileSystem, dwsUtils, dwsXPlatform, dwsUnicode,
  dwsExprs, dwsSymbols, dwsTokenizer, dwsErrors, dwsDataContext, dwsExprList,
  dwsStrings, dwsFunctions, dwsStack, dwsConnectorSymbols, dwsFilter,
  dwsCoreExprs, dwsMagicExprs, dwsRelExprs, dwsMethodExprs, dwsConstExprs,
  dwsConnectorExprs, dwsConvExprs, dwsSetOfExprs, dwsUnifiedConstants,
  dwsOperators, dwsPascalTokenizer, dwsSystemOperators, dwsContextMap,
  dwsUnitSymbols, dwsCompilerUtils, dwsScriptSource, dwsSymbolDictionary,
  dwsCompilerContext, dwsGenericSymbols, dwsSpecializationContext,
  dwsGenericExprs;

const
   cDefaultCompilerOptions = [coOptimize, coAssertions];
   cDefaultMaxRecursionDepth = 1024;
   cDefaultMaxExceptionDepth = 10;
   cDefaultStackChunkSize = 4096;  // 64 kB in 32bit Delphi, each stack entry is a Variant

   // compiler version is date in YYYYMMDD format, dot subversion number
   cCompilerVersion = 20170117.0;

type
   TdwsCompiler = class;

   TIncludeEvent = procedure (const scriptName: String; var scriptSource: String) of object;
   TdwsOnNeedUnitEvent = function (const unitName : String; var unitSource : String) : IdwsUnit of object;
   TdwsResourceEvent = procedure (compiler : TdwsCompiler; const resourceName : String) of object;
   TdwsCodeGenEvent = procedure (compiler : TdwsCompiler; const switchPos : TScriptPos; const code : String) of object;
   TdwsFilterEvent = procedure (compiler : TdwsCompiler; const sourceName : String; var sourceCode : String; var filter : TdwsFilter) of object;

   TCompilerCreateBaseVariantSymbolEvent = function (table : TSystemSymbolTable) : TBaseVariantSymbol of object;
   TCompilerCreateSystemSymbolsEvent = procedure (table : TSystemSymbolTable) of object;
   TCompilerReadInstrEvent = function (compiler : TdwsCompiler) : TNoResultExpr of object;
   TCompilerReadInstrSwitchEvent = function (compiler : TdwsCompiler) : Boolean of object;
   TCompilerFindUnknownNameEvent = function (compiler : TdwsCompiler; const name : String) : TSymbol of object;
   TCompilerReadUnknownNameEvent = function (compiler : TdwsCompiler) : TTypedExpr of object;
   TCompilerSectionChangedEvent = procedure (compiler : TdwsCompiler) of object;
   TCompilerReadScriptEvent = procedure (compiler : TdwsCompiler; sourceFile : TSourceFile;
                                         scriptType : TScriptSourceType) of object;
   TCompilerGetDefaultEnvironmentEvent = function : IdwsEnvironment of object;
   TCompilerGetDefaultLocalizerEvent = function : IdwsLocalizer of object;
   TCompilerOnRootExternalClassEvent = function (compiler : TdwsCompiler;
                                                 const externalName : String) : TClassSymbol of object;
   TCompilerApplyConditionalDefines = procedure (defines : TStrings) of object;

   TdwsNameListOption = (nloAllowDots, nloNoCheckSpecials, nloAllowStrings);
   TdwsNameListOptions = set of TdwsNameListOption;

   // TdwsLocalizerComponent
   //
   TdwsLocalizerComponent = class (TComponent)
      public
         function GetLocalizer : IdwsLocalizer; virtual; abstract;
   end;

   ISystemSymbols = interface(ISystemSymbolTable)
      function Operators : TSystemOperators;
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
         FOnCodeGen : TdwsCodeGenEvent;
         FOnFilter : TdwsFilterEvent;
         FOnCreateBaseVariantSymbol : TCompilerCreateBaseVariantSymbolEvent;
         FOnCreateSystemSymbols : TCompilerCreateSystemSymbolsEvent;
         FOnExecutionStarted : TdwsExecutionEvent;
         FOnExecutionEnded : TdwsExecutionEvent;
         FOwner : TComponent;
         FResultType : TdwsResultType;
         FScriptPaths : TStrings;
         FConditionals : TStringList;
         FStackChunkSize : Integer;
         FSystemSymbols : ISystemSymbols;
         FTimeoutMilliseconds: Integer;
         FUnits : TIdwsUnitList;
         FCompileFileSystem : TdwsCustomFileSystem;
         FRuntimeFileSystem : TdwsCustomFileSystem;
         FLocalizer : TdwsLocalizerComponent;

      protected
         procedure InitSystemTable;
         procedure SetResultType(const value : TdwsResultType);
         procedure SetFilter(const value : TdwsFilter);
         procedure SetTimeOut(const val : Integer);
         procedure SetCompileFileSystem(const val : TdwsCustomFileSystem);
         procedure SetRuntimeFileSystem(const val : TdwsCustomFileSystem);
         procedure SetScriptPaths(const values : TStrings);
         procedure SetConditionals(const val : TStringList);
         function  GetSystemSymbols : ISystemSymbols;
         procedure SetLocalizer(const val : TdwsLocalizerComponent);

         function DoGetLocalizer : IdwsLocalizer;

      public
         constructor Create(Owner: TComponent);
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure Notification(AComponent: TComponent; Operation: TOperation);

         procedure DetachSystemTable;

         property Connectors : TStrings read FConnectors write FConnectors;
         property SystemSymbols : ISystemSymbols read GetSystemSymbols;
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
         property OnCodeGen : TdwsCodeGenEvent read FOnCodeGen write FOnCodeGen;
         property OnFilter : TdwsFilterEvent read FOnFilter write FOnFilter;
         property OnExecutionStarted : TdwsExecutionEvent read FOnExecutionStarted write FOnExecutionStarted;
         property OnExecutionEnded : TdwsExecutionEvent read FOnExecutionEnded write FOnExecutionEnded;
   end;

   TAddArgProcedure = procedure (argExpr : TTypedExpr) of object;
   TExpectedArgFunction = function : TParamSymbol of object;

   TSpecialKeywordKind = (skNone, skAbs, skAssert, skAssigned, skDefault,
                          skHigh, skLength, skLow,
                          skOrd, skSizeOf, skDefined, skDeclared,
                          skInc, skDec, skSucc, skPred,
                          skInclude, skExclude,
                          skSwap,
                          skConditionalDefined,
                          skDebugBreak);

   TSwitchInstruction = (siNone,
                         siIncludeLong, siIncludeShort, siIncludeOnce,
                         siFilterLong, siFilterShort,
                         siResourceLong, siResourceShort,
                         siDefine, siUndef,
                         siIfDef, siIfNDef, siIf, siEndIf, siElse,
                         siHint, siHints, siWarning, siWarnings,
                         siError, siFatal,
                         siRegion, siEndRegion,
                         siCodeGen );

   TLoopExitable = (leNotExitable, leBreak, leExit);

   TdwsOptimizationMessageList = class(TdwsRuntimeMessageList)
      private
         FCompileMsgs : TdwsCompileMessageList;

      public
         procedure AddMessage(aMessage : TdwsMessage); override;

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
         function CallStackLastExpr : TExprBase; override;
         function CallStackLastProg : TObject; override;
         function CallStackDepth : Integer; override;
         procedure DebuggerNotifyException(const exceptObj : IScriptObj); override;
   end;

   TdwsReadTypeContext = (tcDeclaration, tcVariable, tcConstant, tcMember,
                          tcParameter, tcResult, tcOperand, tcExceptionClass,
                          tcProperty, tcHelper, tcGeneric);

   TdwsReadProcDeclOption = (pdoClassMethod, pdoType, pdoAnonymous);
   TdwsReadProcDeclOptions = set of TdwsReadProcDeclOption;

   TdwsUnitSection = (secMixed, secHeader, secProgram, secInterface, secImplementation,
                      secInitialization, secFinalization, secEnd);
   TdwsStatementAction = (saNone, saNoSemiColon, saInterface, saImplementation, saEnd);

   TdwsCompilerUnitContext = record
      SourceUnit : TSourceUnit;
      Tokenizer : TTokenizer;
      UnitSymbol : TUnitMainSymbol;
      Context : TdwsSourceContext;
   end;

   IdwsDataSymbolFactory = interface
      procedure CheckName(const name : String; const namePos : TScriptPos);
      function CreateDataSymbol(const name, externalName : String;
                                const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol;
      function CreateConstSymbol(const name : String; const namePos : TScriptPos; typ : TTypeSymbol;
                                 const data : TData) : TConstSymbol;
      function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
      function ReadArrayConstantExpr(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
      function ReadInitExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
   end;

   ISymbolAttributesBag = interface
      function Attributes : TdwsSymbolAttributes;
   end;

   TdwsCompilerUnitContextStack = class(TSimpleStack<TdwsCompilerUnitContext>)
      public
         destructor Destroy; override;
         procedure Clean;

         procedure PushContext(compiler : TdwsCompiler);
         procedure PopContext(compiler : TdwsCompiler; var oldSourceUnit : TSourceUnit);
   end;

   IdwsCompiler = interface;

   TTypeConvertEvent = procedure (const source: IDataContext; var output);

   TTypeLookupData = record
      event: TTypeConvertEvent;
      info: PTypeInfo;
      constructor Create(event: TTypeConvertEvent; info: PTypeInfo);
   end;

   IdwsExternalFunctionsManager = interface
      ['{6365B0E4-4BEA-4AD4-8DDE-C37758A63FEF}']
      procedure BeginCompilation(const compiler : IdwsCompiler);
      procedure EndCompilation(const compiler : IdwsCompiler);

      function ConvertToMagicSymbol(value: TFuncSymbol) : TFuncSymbol;
      function CreateExternalFunction(funcSymbol : TFuncSymbol) : IExternalRoutine;

      procedure RegisterExternalFunction(const name: String; address: pointer);
      procedure RegisterTypeMapping(const name: String; const typ: TTypeLookupData);
   end;

   IdwsCompiler = interface
      // direct access to the underlying instance, use with caution!!!
      function Compiler : TdwsCompiler;

      function Compile(const aCodeText : String; aConf : TdwsConfiguration;
                       const mainFileName : String = '') : IdwsProgram;
      procedure RecompileInContext(const context : IdwsProgram; const aCodeText : String;
                                   aConf : TdwsConfiguration);

      procedure AbortCompilation;

      function GetCurrentProg : TdwsProgram;
      function GetMsgs : TdwsCompileMessageList;
      function GetTokenizer : TTokenizer;
      function CompileTimeExecution : TdwsExecution;
      function GetCompilerContext : TdwsCompilerContext;

      procedure SetExternalFunctionsManager(const value : IdwsExternalFunctionsManager);
      function  GetExternalFunctionsManager : IdwsExternalFunctionsManager;

      property CurrentProg : TdwsProgram read GetCurrentProg;
      property CompilerContext : TdwsCompilerContext read GetCompilerContext;
      property Msgs : TdwsCompileMessageList read GetMsgs;
      property Tokenizer : TTokenizer read GetTokenizer;
      property ExternalFunctionsManager : IdwsExternalFunctionsManager read GetExternalFunctionsManager write SetExternalFunctionsManager;

      function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
   end;

   // TdwsCompiler
   //
   TSimpleObjectObjectHash_TDataSymbol_TVarExpr = TSimpleObjectObjectHash<TDataSymbol,TVarExpr>;
   TdwsCompiler = class (TInterfacedObject, IdwsCompiler)
      private
         FOptions : TCompilerOptions;
         FTokRules : TTokenizerRules;
         FTok : TTokenizer;
         FMainProg : TdwsMainProgram;
         FCurrentProg : TdwsProgram;
         FCompilerContext : TdwsCompilerContext;
         FUnifiedConstants : TUnifiedConstants;
         FSourceContextMap : TdwsSourceContextMap;
         FSymbolDictionary : TdwsSymbolDictionary;
         FOperators : TOperators;
         FLoopExprs : TSimpleStack<TProgramExpr>;
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
         FOnCodeGen : TdwsCodeGenEvent;
         FOnFilter : TdwsFilterEvent;
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
         FCurrentSourceUnit : TSourceUnit;
         FCurrentUnitSymbol : TUnitMainSymbol;
         FCurrentStructure : TCompositeTypeSymbol;
         FAnyFuncSymbol : TAnyFuncSymbol;
         FStandardDataSymbolFactory : IdwsDataSymbolFactory;
         FPendingAttributes : TdwsSymbolAttributes;
         FStringListPool : TSimpleStringListPool;
         FDefaultConditionals : IAutoStrings;
         FHelperMemberNames : TSimpleStringHash;
         FGenericSymbol : TSimpleStack<TGenericSymbol>;

         // if set we're in a setter write expression or statement
         FPendingSetterValueExpr : TVarExpr;

         FDataSymbolExprReuse : TSimpleObjectObjectHash_TDataSymbol_TVarExpr;

         FExternalRoutinesManager : IdwsExternalFunctionsManager;

         FStaticExtensionSymbols : Boolean;
         FOnCreateBaseVariantSymbol : TCompilerCreateBaseVariantSymbolEvent;
         FOnCreateSystemSymbols : TCompilerCreateSystemSymbolsEvent;
         FOnReadInstr : TCompilerReadInstrEvent;
         FOnReadInstrSwitch : TCompilerReadInstrSwitchEvent;
         FOnFindUnknownName : TCompilerFindUnknownNameEvent;
         FOnReadUnknownName : TCompilerReadUnknownNameEvent;
         FOnSectionChanged : TCompilerSectionChangedEvent;
         FOnReadScript : TCompilerReadScriptEvent;
         FOnGetDefaultEnvironment : TCompilerGetDefaultEnvironmentEvent;
         FOnGetDefaultLocalizer : TCompilerGetDefaultLocalizerEvent;
         FOnRootExternalClass : TCompilerOnRootExternalClassEvent;
         FOnApplyConditionalDefines : TCompilerApplyConditionalDefines;
         FOnExecutionStarted : TdwsExecutionEvent;
         FOnExecutionEnded : TdwsExecutionEvent;

         F8087CW : Cardinal;
         FCompilerAbort : Boolean;

      protected
         function Optimize : Boolean;

         function CheckPropertyFuncParams(paramsA : TParamsSymbolTable; methSym : TMethodSymbol;
                                          indexSym : TSymbol = nil; typSym : TTypeSymbol = nil) : Boolean;
         procedure CheckName(const name : String; const namePos : TScriptPos);
         function  IdentifySpecialName(const name : String) : TSpecialKeywordKind;
         procedure CheckSpecialName(const name : String);
         procedure CheckSpecialNameCase(const name : String; sk : TSpecialKeywordKind;
                                        const namePos : TScriptPos);
         function  CheckParams(tableA, tableB : TParamsSymbolTable; checkNames : Boolean; skipB : Integer = 0) : Boolean;
         procedure CompareFuncKinds(a, b : TFuncKind);
         procedure CompareFuncSymbolParams(a, b : TFuncSymbol);
         function  CurrentStruct : TCompositeTypeSymbol;
         function  FindStructMember(typ : TStructuredTypeSymbol; const name : String) : TSymbol;

         procedure HintUnusedSymbols;
         procedure HintUnusedPrivateSymbols;
         procedure HintUnusedResult(resultSymbol : TDataSymbol);
         procedure HintReferenceConstVarParams(funcSym : TFuncSymbol);

         function GetVarExpr(const aScriptPos: TScriptPos; dataSym : TDataSymbol): TVarExpr;

         function GetLazyParamExpr(dataSym : TLazyParamSymbol) : TLazyParamExpr;
         function GetVarParamExpr(dataSym : TVarParamSymbol) : TByRefParamExpr;
         function GetConstByRefParamExpr(dataSym : TConstByRefParamSymbol) : TByRefParamExpr;
         function GetConstParamExpr(const aScriptPos: TScriptPos; dataSym : TParamSymbol) : TVarExpr;
         function GetSelfParamExpr(const aScriptPos: TScriptPos; selfSym : TDataSymbol) : TVarExpr;

         function ReadAssign(token : TTokenType; var left : TDataExpr) : TProgramExpr;

         function ReadSetOfType(const typeName : String; typeContext : TdwsReadTypeContext) : TSetOfSymbol;

         function ReadArrayType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
         function ReadAssociativeArrayType(const typeName : String; keyType : TTypeSymbol;
                                           typeContext : TdwsReadTypeContext) : TAssociativeArraySymbol;
         function ReadArrayConstant(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
         function ReadArrayMethod(const name : String; const namePos : TScriptPos;
                                  baseExpr : TTypedExpr) : TProgramExpr;
         function ReadAssociativeArrayMethod(const name : String; const namePos : TScriptPos;
                                             baseExpr : TTypedExpr) : TProgramExpr;
         function ReadStringMethod(const name : String; const namePos : TScriptPos;
                                   baseExpr : TTypedExpr) : TProgramExpr;
         function ReadSetOfMethod(const name : String; const namePos : TScriptPos;
                                  baseExpr : TTypedExpr) : TProgramExpr;
         function ReadElementMethod(const name : String; const namePos : TScriptPos;
                                    baseExpr : TTypedExpr) : TProgramExpr;

         function ReadCase : TCaseExpr;
         function ReadCaseConditions(var condList : TTightList; valueExpr : TTypedExpr) : Integer;
         function ReadAliasedNameSymbol(var namePos : TScriptPos) : TSymbol;
         function ReadNameSymbol(var namePos : TScriptPos) : TSymbol;
         function ReadClassName : TClassSymbol;
         function ReadClassOf(const typeName : String) : TClassOfSymbol;
         function ReadClassDecl(const typeName : String; const flags : TClassSymbolFlags;
                                allowNonConstExpressions : Boolean) : TClassSymbol;
         procedure ReadClassVars(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
         procedure ReadClassConst(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
         function ReadClassConstSymbol(const constPos : TScriptPos;
                                        typ : TTypeSymbol;
                                        const ownerSymbol : TCompositeTypeSymbol;
                                        aVisibility : TdwsVisibility) : TConstSymbol;
         function ReadInterface(const typeName : String) : TInterfaceSymbol;
         function ReadConnectorSym(const name : String; baseExpr : TTypedExpr;
                                   const connectorType : IConnectorType; isWrite: Boolean) : TProgramExpr;
         function ReadConnectorArray(const name : String; var baseExpr : TTypedExpr;
                                     const connectorType : IConnectorType; isWrite: Boolean) : TConnectorCallExpr;

         function ReadConstSymbol(const name : String; const constPos : TScriptPos;
                                  typ : TTypeSymbol; const factory : IdwsDataSymbolFactory) : TConstSymbol;
         procedure ReadConstDecl(const factory : IdwsDataSymbolFactory);
         procedure ReadConstDeclBlock(var action : TdwsStatementAction);
         function ReadConstImmediateValue : TConstExpr;
         function ReadConstRecord(symbol : TRecordSymbol) : TData;

         function ReadBlock : TProgramExpr;
         function ReadBlocks(const endTokens : TTokenTypes; var finalToken : TTokenType) : TProgramExpr;
         function ReadRootStatement(var action : TdwsStatementAction; initVarBlockExpr : TBlockExpr) : TProgramExpr;
         function ReadRootBlock(const endTokens: TTokenTypes; var finalToken: TTokenType) : TBlockExpr;
         procedure UnexpectedBlockTokenError(const endTokens : TTokenTypes);

         function ReadEnumeration(const typeName : String; aStyle : TEnumerationSymbolStyle) : TEnumerationSymbol;
         function ReadExit : TNoResultExpr;
         function ReadClassExpr(ownerSymbol : TCompositeTypeSymbol; expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadExprAdd(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
         function ReadExprMult(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
         function ReadBooleanExpr : TTypedExpr;
         function ReadExprIn(var left : TTypedExpr) : TTypedExpr;
         function ReadExprInConditions(var left : TTypedExpr) : TInOpExpr;
         function ReadExternalVar(sym : TExternalVarSymbol; isWrite : Boolean) : TFuncExpr;

         function ReadField(const scriptPos : TScriptPos; selfSym : TDataSymbol;
                            fieldSym : TFieldSymbol) : TDataExpr; overload;
         function ReadField(const scriptPos : TScriptPos; selfSym : TDataSymbol;
                            fieldSym : TFieldSymbol; var varExpr : TTypedExpr) : TDataExpr; overload;

         function ReadFor : TProgramExpr;
         function ReadForTo(const forPos : TScriptPos; loopVarExpr : TVarExpr;
                            const loopVarName : String; const loopVarNamePos : TScriptPos) : TNoResultExpr;
         function ReadForStep(const forPos : TScriptPos; forExprClass : TForExprClass;
                              iterVarExpr : TIntVarExpr; var fromExpr, toExpr : TTypedExpr;
                              loopFirstStatement : TProgramExpr) : TForExpr;
         function ReadForIn(const forPos : TScriptPos; loopVarExpr : TVarExpr;
                            const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
         function ReadForInString(const forPos : TScriptPos; inExpr : TProgramExpr; loopVarExpr : TVarExpr;
                                  const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
         function ReadForInSetOf(const forPos : TScriptPos; inExpr : TDataExpr; loopVarExpr : TVarExpr;
                                 const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
         function ReadForInConnector(const forPos : TScriptPos;
                            inExpr : TTypedExpr; const inPos : TScriptPos; loopVarExpr : TVarExpr;
                            const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;

         function ReadFuncOverloaded(funcSym : TFuncSymbol; fromTable : TSymbolTable;
                                     codeExpr : TDataExpr = nil; expecting : TTypeSymbol = nil) : TTypedExpr;
         procedure CollectMethodOverloads(methSym : TMethodSymbol; overloads : TFuncSymbolList);
         function ReadSelfMethOverloaded(methSym : TMethodSymbol; isWrite : Boolean;
                                         expecting : TTypeSymbol; options : TCreateFunctionOptions) : TTypedExpr;
         function ReadMethOverloaded(methSym : TMethodSymbol; instanceExpr : TTypedExpr;
                                     const scriptPos : TScriptPos;
                                     expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadStaticMethOverloaded(methSym : TMethodSymbol; metaExpr : TTypedExpr;
                                           const scriptPos : TScriptPos;
                                           expecting : TTypeSymbol = nil) : TTypedExpr;

         function ResolveOverload(var funcExpr : TFuncExprBase; overloads : TFuncSymbolList;
                                  const argPosArray : TScriptPosArray;
                                  expecting : TFuncSymbol;
                                  cfOptions : TCreateFunctionOptions) : Boolean;

         function FuncHasConflictingOverload(funcSym, forwardedSym : TFuncSymbol) : Boolean;
         function MethHasConflictingOverload(methSym : TMethodSymbol) : Boolean;

         function FuncPerfectMatchOverload(funcSym : TFuncSymbol) : TFuncSymbol;
         function MethPerfectMatchOverload(methSym : TMethodSymbol; recurse : Boolean) : TMethodSymbol;

         function ReadFunc(funcSym : TFuncSymbol; codeExpr : TTypedExpr = nil;
                           expecting : TTypeSymbol = nil;
                           overloads : TFuncSymbolList = nil) : TTypedExpr;
         function ReadSelfMethod(methodSym : TMethodSymbol; isWrite : Boolean;
                                 expecting : TTypeSymbol;
                                 overloads : TFuncSymbolList;
                                 options : TCreateFunctionOptions) : TTypedExpr;
         function ReadMethod(methodSym : TMethodSymbol; instanceExpr : TTypedExpr;
                             const scriptPos : TScriptPos;
                             expecting : TTypeSymbol = nil;
                             overloads : TFuncSymbolList = nil) : TTypedExpr;
         function ReadStaticMethod(methodSym : TMethodSymbol; metaExpr : TTypedExpr;
                                   const scriptPos : TScriptPos;
                                   expecting : TTypeSymbol = nil;
                                   overloads : TFuncSymbolList = nil) : TTypedExpr;

         function WrapUpFunctionRead(funcExpr : TFuncExprBase; expecting : TTypeSymbol;
                                     overloads : TFuncSymbolList;
                                     cfOptions : TCreateFunctionOptions) : TTypedExpr;

         procedure ReadFuncArgs(funcExpr : TFuncExprBase; var argPosArray : TScriptPosArray;
                                overloads : TFuncSymbolList);
         procedure ReadArguments(const addArgProc : TAddArgProcedure;
                                 leftDelim, rightDelim : TTokenType;
                                 var argPosArray : TScriptPosArray;
                                 const expectedProc : TExpectedArgFunction = nil);
         function ReadFuncResultType(funcKind : TFuncKind) : TTypeSymbol;

         function ReadIf : TProgramExpr;
         function ReadInherited(isWrite : Boolean) : TProgramExpr;
         function ReadInstr : TProgramExpr;
         function ReadUntilEndOrElseSwitch(allowElse : Boolean) : Boolean;
         function ReadIntfMethodDecl(intfSym : TInterfaceSymbol; funcKind : TFuncKind) : TSourceMethodSymbol;
         function ReadMethodDecl(const hotPos : TScriptPos;
                                 ownerSym : TCompositeTypeSymbol; funcKind : TFuncKind;
                                 aVisibility : TdwsVisibility; isClassMethod : Boolean) : TMethodSymbol;
         function ReadMethodImpl(ownerSym : TCompositeTypeSymbol; funcKind : TFuncKind;
                                 isClassMethod : Boolean) : TMethodSymbol;

         function  ReadDeprecatedMessage(withSemiColon : Boolean = True) : String;
         procedure WarnDeprecatedFunc(funcExpr : TFuncExprBase);
         procedure WarnDeprecatedType(const scriptPos : TScriptPos; typeSymbol : TTypeSymbol);
         procedure WarnDeprecatedSymbol(const scriptPos : TScriptPos; sym : TSymbol; const deprecatedMessage : String);

         function ResolveUnitNameSpace(const prefixPos : TScriptPos; unitPrefix : TUnitSymbol) : TUnitSymbol;
         function ReadName(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TProgramExpr;
         function ReadEnumerationSymbolName(const enumPos : TScriptPos; enumSym : TEnumerationSymbol; acceptTypeRef : Boolean) : TProgramExpr;
         function ReadClassSymbolName(baseType : TClassSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadInterfaceSymbolName(baseType : TInterfaceSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadRecordSymbolName(baseType : TRecordSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadConstName(const constPos : TScriptPos; constSym : TConstSymbol; isWrite: Boolean) : TProgramExpr;
         function ReadDataSymbolName(const aScriptPos: TScriptPos;dataSym : TDataSymbol;
                                     fromTable : TSymbolTable; isWrite: Boolean; expecting : TTypeSymbol) : TProgramExpr;
         function ReadImplicitCall(codeExpr : TTypedExpr; isWrite: Boolean;
                                   expecting : TTypeSymbol) : TProgramExpr;
         function ReadResourceStringName(resSym : TResourceStringSymbol; const namePos : TScriptPos) : TResourceStringExpr;
         function ReadNameOld(isWrite : Boolean) : TTypedExpr;
         function ReadNameInherited(isWrite : Boolean) : TProgramExpr;
         procedure ReadNameList(names : TSimpleStringList; var posArray : TScriptPosArray;
                                const options : TdwsNameListOptions = [];
                                externalNames : TSimpleStringList = nil);
         procedure ReadExternalName(funcSym : TFuncSymbol);
         function  ReadNew(restrictTo : TClassSymbol; asAttribute : Boolean) : TProgramExpr;
         function  ReadNewArray(elementTyp : TTypeSymbol) : TNewArrayExpr;
         procedure ReadArrayParams(ArrayIndices: TSymbolTable);
         // Don't want to add param symbols to dictionary when a method implementation (they get thrown away)
         procedure ReadParams(const hasParamMeth : THasParamSymbolMethod;
                              const addParamMeth : TAddParamSymbolMethod;
                              forwardedParams : TParamsSymbolTable;
                              expectedLambdaParams : TParamsSymbolTable;
                              var posArray : TScriptPosArray);
         procedure ReadProcCallQualifiers(funcSymbol : TFuncSymbol);
         procedure AdaptParametersSymPos(guess, actual : TFuncSymbol; const useTypes : TSymbolUsages;
                                         var posArray : TScriptPosArray);
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
         procedure ReadPropertyDecl(ownerSym : TCompositeTypeSymbol; aVisibility : TdwsVisibility;
                                    classProperty : Boolean);
         procedure ReadPropertyDeclAutoField(propSym : TPropertySymbol; classProperty : Boolean);
         function ReadPropertyDeclGetter(propSym : TPropertySymbol; var scriptPos : TScriptPos;
                                         classProperty : Boolean) : TSymbol;
         function ReadPropertyDeclSetter(propSym : TPropertySymbol; var scriptPos : TScriptPos;
                                         classProperty : Boolean) : TSymbol;
         function ReadPropertyProgExpr(var expr : TProgramExpr; propertySym : TPropertySymbol; isWrite: Boolean) : TProgramExpr;
         function ReadPropertyExpr(var expr : TTypedExpr; propertySym : TPropertySymbol; isWrite: Boolean) : TProgramExpr;
         function ReadPropertyReadExpr(var expr : TTypedExpr; propertySym : TPropertySymbol) : TTypedExpr;
         function ReadPropertyWriteExpr(var expr : TTypedExpr; propertySym : TPropertySymbol) : TProgramExpr;
         function ReadPropertyArrayAccessor(var expr : TTypedExpr; propertySym : TPropertySymbol;
                                            typedExprList : TTypedExprList;
                                            const scriptPos : TScriptPos; isWrite : Boolean) : TFuncExprBase;

         function ReadRecordDecl(const typeName : String; allowNonConstExpressions : Boolean) : TRecordSymbol;
         procedure ReadFieldsDecl(struct : TStructuredTypeSymbol; visibility : TdwsVisibility;
                                  allowNonConstExpressions : Boolean; forceExternal : Boolean);

         function ReadHelperDecl(const typeName : String;
                                 qualifierToken : TTokenType;
                                 isStrict : Boolean) : THelperSymbol;

         function ReadRaise : TRaiseBaseExpr;
         function ReadRepeat : TProgramExpr;
         function ReadImplementationBlock : TTokenType;
         procedure ReadSemiColon(fatal : Boolean = False);
         function ReadScript(sourceFile : TSourceFile; scriptType : TScriptSourceType) : TProgramExpr;
         procedure ReadScriptImplementations;
         function ReadSpecialFunction(const namePos : TScriptPos; specialKind : TSpecialKeywordKind) : TProgramExpr;
         function ReadIncludeExclude(const namePos : TScriptPos; specialKind : TSpecialKeywordKind;
                                     var argExpr : TTypedExpr; const argPos : TScriptPos) : TProgramExpr;

         function ReadStatement(var action : TdwsStatementAction; initVarBlockExpr : TBlockExpr) : TProgramExpr;
         function ReadResourceStringDecl : TResourceStringSymbol;
         procedure ReadResourceStringDeclBlock(var action : TdwsStatementAction);
         function ReadStringArray(expr : TDataExpr; isWrite : Boolean) : TProgramExpr;

         function ReadSwitch(const switchName : String) : Boolean;
         function ReadInstrSwitch(const switchName : String) : Boolean;
         function ReadExprSwitch(const switchPos : TScriptPos) : Boolean;
         procedure SkipUntilToken(tt : TTokenType);

         function ReadSymbol(expr : TProgramExpr; isWrite : Boolean = False;
                             expecting : TTypeSymbol = nil) : TProgramExpr;
         function ReadSymbolArrayExpr(var baseExpr : TDataExpr) : TProgramExpr;
         function ReadSymbolAssociativeArrayExpr(var baseExpr : TDataExpr) : TProgramExpr;
         function ReadSymbolMemberExpr(var expr : TProgramExpr;
                                       isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;

         function ReadTerm(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadBracket(expecting : TTypeSymbol = nil) : TTypedExpr;
         function ReadNegation : TTypedExpr;
         function ReadIfExpr(expecting : TTypeSymbol = nil) : TTypedExpr;

         function  ReadTry : TExceptionExpr;
         procedure ReadFinally(finallyExpr : TFinallyExpr);
         procedure ReadExcept(exceptExpr : TExceptExpr; var finalToken : TTokenType);

         function ReadGenericParametersDecl : IGenericParameters;
         function ReadSpecializedType(genericType : TGenericSymbol) : TTypeSymbol;
         procedure CheckGenericParameters(genericType : TGenericSymbol);

         function ReadType(const typeName : String; typeContext : TdwsReadTypeContext) : TTypeSymbol;
         function ReadTypeGenericDecl(const typeName : String; typeContext : TdwsReadTypeContext;
                                      const genericParameters : IGenericParameters = nil) : TTypeSymbol;
         function ReadTypeCast(const namePos : TScriptPos; typeSym : TTypeSymbol) : TTypedExpr;
         function ReadTypeExpr(const namePos : TScriptPos; typeSym : TTypeSymbol;
                               isWrite : Boolean; expecting : TTypeSymbol = nil) : TProgramExpr;

         procedure EnterGeneric(generic : TGenericSymbol);
         procedure LeaveGeneric;

         procedure ReadAttributes(tokenALEFTAlreadyDeleted : Boolean);
         function  BagPendingAttributes : ISymbolAttributesBag;
         procedure AttachBaggedAttributes(symbol : TSymbol; const bag : ISymbolAttributesBag);
         procedure CheckNoPendingAttributes;

         procedure AddProcHelper(func : TFuncSymbol);
         function EnumerateHelpers(typeSym : TTypeSymbol) : THelperSymbols;
         function ReadTypeHelper(expr : TTypedExpr;
                                 const name : String; const namePos : TScriptPos;
                                 expecting : TTypeSymbol; isWrite : Boolean;
                                 killNameToken : Boolean) : TProgramExpr;
         function ReadSelfTypeHelper(const name : TToken; const namePos : TScriptPos;
                                     expecting : TTypeSymbol) : TProgramExpr;

         procedure ReadTypeDeclBlock;
         function  ReadTypeDecl(firstInBlock : Boolean) : Boolean;

         procedure ReadUses;
         function  ReadUnitHeader : TScriptSourceType;

         procedure ReadVarDeclBlock(var action : TdwsStatementAction; initVarBlockExpr : TBlockExprBase);
         procedure ReadVarDecl(const dataSymbolFactory : IdwsDataSymbolFactory; initVarBlockExpr : TBlockExprBase);
         procedure ReadNamedVarsDecl(names, externalNames : TSimpleStringList;
                                     const posArray : TScriptPosArray;
                                     const dataSymbolFactory : IdwsDataSymbolFactory;
                                     initVarBlockExpr : TBlockExprBase);
         function CreateNamedVarDeclExpr(const dataSymbolFactory : IdwsDataSymbolFactory;
                                         const name, externalName : String;
                                         const scriptPos : TScriptPos;
                                         typ : TTypeSymbol; var initExpr : TTypedExpr;
                                         var sym : TDataSymbol) : TProgramExpr;
         function ReadWhile : TProgramExpr;
         function ReadWith : TProgramExpr;
         function ResolveUnitReferences(scriptType : TScriptSourceType) : TIdwsUnitList;

         function CreateTypedDefault(typ : TTypeSymbol) : TConstExpr;

         procedure SetCurrentProg(aProg : TdwsProgram);

         function GetCompilerContext : TdwsCompilerContext;

      protected
         procedure EnterLoop(loopExpr : TProgramExpr);
         procedure MarkLoopExitable(level : TLoopExitable);
         procedure LeaveLoop;

         function GetFuncExpr(funcSym : TFuncSymbol; codeExpr : TTypedExpr = nil) : TFuncExprBase;
         function GetMethodExpr(meth: TMethodSymbol; Expr: TTypedExpr; RefKind: TRefKind;
                                const scriptPos: TScriptPos; options : TCreateFunctionOptions) : TFuncExprBase;

         procedure MemberSymbolWithNameAlreadyExists(sym : TSymbol; const hotPos : TScriptPos);
         procedure IncompatibleTypes(const scriptPos : TScriptPos; const fmt : String; typ1, typ2 : TTypeSymbol);
         procedure IncompatibleTypesWarn(const scriptPos : TScriptPos; const fmt : String; typ1, typ2 : TTypeSymbol);

         function CreateProgram(const systemTable : ISystemSymbolTable;
                                resultType : TdwsResultType;
                                const stackParams : TStackParameters;
                                const mainFileName : String) : TdwsMainProgram;
         function CreateAssign(const scriptPos : TScriptPos; token : TTokenType;
                               left : TDataExpr; right : TTypedExpr) : TProgramExpr;

         function CreateArrayLow(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
         function CreateArrayHigh(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
         function CreateArrayLength(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
         function CreateArrayExpr(const scriptPos : TScriptPos; baseExpr : TDataExpr; indexExpr : TTypedExpr) : TArrayExpr;

         function EnsureLoopVarExpr(const loopPos : TScriptPos;
                                    const loopVarName : String; const loopVarNamePos : TScriptPos;
                                    var loopVarExpr : TVarExpr; loopVarTyp : TTypeSymbol) : TBlockExpr;

         function CreateTypedOperatorExpr(token : TTokenType; const scriptPos : TScriptPos;
                                          aLeft, aRight : TTypedExpr) : TTypedExpr;
         function CreateAssignOperatorExpr(token : TTokenType; const scriptPos : TScriptPos;
                                           exec : TdwsExecution;
                                           aLeft : TDataExpr; aRight : TTypedExpr) : TProgramExpr;

         procedure DoSectionChanged;
         procedure DoTokenizerEndSourceFile(sourceFile : TSourceFile);

         property  CurrentSourceUnit : TSourceUnit read FCurrentSourceUnit;
         property  CurrentUnitSymbol : TUnitMainSymbol read FCurrentUnitSymbol;
         procedure EnterUnit(srcUnit : TSourceUnit; var oldSrcUnit : TSourceUnit);
         procedure LeaveUnit(oldSrcUnit : TSourceUnit);
         procedure SwitchTokenizerToUnit(srcUnit : TSourceUnit; const sourceCode : String);

         procedure SetupCompileOptions(conf : TdwsConfiguration);
         procedure SetupMsgsOptions(conf : TdwsConfiguration);
         procedure CleanupAfterCompile;

         procedure CheckFilterDependencies(confUnits : TIdwsUnitList);
         procedure HandleUnitDependencies(scriptType : TScriptSourceType);
         function  HandleExplicitDependency(const unitName : String) : TUnitSymbol;

         procedure SetupInitializationFinalization;

         procedure OrphanObject(obj : TRefCountedObject); overload;

         procedure OrphanAndNil(var expr : TTypedExpr); overload;
         procedure OrphanAndNil(var expr : TProgramExpr); overload;
         procedure OrphanAndNil(var expr : TDataExpr); overload;
         procedure OrphanAndNil(var expr : TBlockExpr); overload;
         procedure OrphanAndNil(var expr : TVarExpr); overload;

         function Compiler : TdwsCompiler;
         function GetCurrentProg : TdwsProgram;
         function GetMsgs : TdwsCompileMessageList;
         function GetTokenizer : TTokenizer;
         function  GetExternalFunctionsManager : IdwsExternalFunctionsManager;
         procedure SetExternalFunctionsManager(const value : IdwsExternalFunctionsManager);
         function CompileTimeExecution : TdwsExecution;

         procedure AttachContextProgram(contextProgram : TdwsProgram);
         procedure DetachContextProgram;
         procedure AttachTokenizer(tok : TTokenizer);
         function  DetachTokenizer : TTokenizer;
         property  SourceContextMap : TdwsSourceContextMap read FSourceContextMap;


      public
         constructor Create;
         destructor Destroy; override;

         function Compile(const aCodeText : String; aConf : TdwsConfiguration;
                          const mainFileName : String = '') : IdwsProgram;
         procedure RecompileInContext(const context : IdwsProgram; const aCodeText : String; aConf : TdwsConfiguration);

         class procedure Evaluate; static; deprecated 'Moved to TdwsEvaluateExpr.Evaluate';

         procedure AbortCompilation;

         procedure WarnForVarUsage(varExpr : TVarExpr; const scriptPos : TScriptPos);

         procedure CheckMatchingDeclarationCase(const nameString : String; sym : TSymbol; const scriptPos : TScriptPos);

         procedure RecordSymbolUse(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
         procedure RecordSymbolUseReference(sym : TSymbol; const scriptPos : TScriptPos; isWrite : Boolean);
         procedure RecordSymbolUseImplicitReference(sym : TSymbol; const scriptPos : TScriptPos; isWrite : Boolean);
         procedure ReplaceSymbolUse(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);

         function OpenStreamForFile(const fileName : String) : TStream;
         function GetScriptSource(const scriptName : String) : String;
         function GetIncludeScriptSource(const scriptName : String) : String;

         property CurrentProg : TdwsProgram read FCurrentProg write SetCurrentProg;
         property CompilerContext : TdwsCompilerContext read FCompilerContext;

         property Msgs : TdwsCompileMessageList read FMsgs;
         property Options : TCompilerOptions read FOptions write FOptions;
         property ScriptPaths : TStrings read FScriptPaths;
         property Filter : TdwsFilter read FFilter;
         property SymbolDictionary : TdwsSymbolDictionary read FSymbolDictionary;
         property UnitSection : TdwsUnitSection read FUnitSection write FUnitSection;
         property TokenizerRules : TTokenizerRules read FTokRules;
         property Tokenizer : TTokenizer read FTok write FTok;

         property StaticExtensionSymbols : Boolean read FStaticExtensionSymbols write FStaticExtensionSymbols;
         property OnCreateBaseVariantSymbol : TCompilerCreateBaseVariantSymbolEvent read FOnCreateBaseVariantSymbol write FOnCreateBaseVariantSymbol;
         property OnCreateSystemSymbols : TCompilerCreateSystemSymbolsEvent read FOnCreateSystemSymbols write FOnCreateSystemSymbols;
         property OnReadInstr : TCompilerReadInstrEvent read FOnReadInstr write FOnReadInstr;
         property OnReadInstrSwitch : TCompilerReadInstrSwitchEvent read FOnReadInstrSwitch write FOnReadInstrSwitch;
         property OnFindUnknownName : TCompilerFindUnknownNameEvent read FOnFindUnknownName write FOnFindUnknownName;
         property OnReadUnknownName : TCompilerReadUnknownNameEvent read FOnReadUnknownName write FOnReadUnknownName;
         property OnSectionChanged : TCompilerSectionChangedEvent read FOnSectionChanged write FOnSectionChanged;
         property OnReadScript : TCompilerReadScriptEvent read FOnReadScript write FOnReadScript;
         property OnGetDefaultEnvironment : TCompilerGetDefaultEnvironmentEvent read FOnGetDefaultEnvironment write FOnGetDefaultEnvironment;
         property OnGetDefaultLocalizer : TCompilerGetDefaultLocalizerEvent read FOnGetDefaultLocalizer write FOnGetDefaultLocalizer;
         property OnRootExternalClass : TCompilerOnRootExternalClassEvent read FOnRootExternalClass write FOnRootExternalClass;
         property OnApplyConditionalDefines : TCompilerApplyConditionalDefines read FOnApplyConditionalDefines write FOnApplyConditionalDefines;
   end;

const
   cSpecialKeywords : array [TSpecialKeywordKind] of String = (
      '', 'Abs', 'Assert', 'Assigned', 'Default', 'High', 'Length', 'Low',
      'Ord', 'SizeOf', 'Defined', 'Declared', 'Inc', 'Dec', 'Succ', 'Pred',
      'Include', 'Exclude', 'Swap', 'ConditionalDefined', 'DebugBreak'
   );

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
      SWI_HINT, SWI_HINTS, SWI_WARNING, SWI_WARNINGS, SWI_ERROR, SWI_FATAL,
      SWI_REGION, SWI_ENDREGION,
      SWI_CODEGEN
      );

   cAssignmentTokens : TTokenTypes = [ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
                                      ttTIMES_ASSIGN, ttDIVIDE_ASSIGN];

   cTokenToVisibility : array [ttPRIVATE..ttPUBLISHED] of TdwsVisibility = (
      cvPrivate, cvProtected, cvPublic, cvPublished );
   cTokenToFuncKind : array [ttFUNCTION..ttLAMBDA] of TFuncKind = (
      fkFunction, fkProcedure, fkConstructor, fkDestructor, fkMethod, fkLambda );

   cMaxArrayItemRange = 1024;

type
   TReachStatus = (rsReachable, rsUnReachable, rsUnReachableWarned);

   TExceptObjFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TParamFunc = class(TInternalMagicVariantFunction)
      procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TParamStrFunc = class(TInternalMagicStringFunction)
      procedure DoEvalAsString(const args : TExprBaseListExec; var Result : String); override;
   end;

   TParamCountFunc = class(TInternalMagicIntFunction)
      function DoEvalAsInteger(const args : TExprBaseListExec) : Int64; override;
   end;

   TStandardSymbolFactory = class (TInterfacedObject, IdwsDataSymbolFactory)
      private
         FCompiler : TdwsCompiler;

      public
         constructor Create(aCompiler : TdwsCompiler);
         procedure CheckName(const name : String; const namePos : TScriptPos); virtual;
         function CreateDataSymbol(const name, externalName : String;
                                   const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol; virtual;
         function CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                    typ : TTypeSymbol; const data : TData) : TConstSymbol; virtual;
         function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr; virtual;
         function ReadArrayConstantExpr(closingToken : TTokenType; expecting : TTypeSymbol) : TArrayConstantExpr;
         function ReadInitExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
   end;

   TCompositeTypeSymbolFactory = class (TStandardSymbolFactory)
      private
         FOwnerType : TCompositeTypeSymbol;
         FVisibility : TdwsVisibility;

      public
         constructor Create(aCompiler : TdwsCompiler; ownerType : TCompositeTypeSymbol;
                            aVisibility : TdwsVisibility);
         procedure CheckName(const name : String; const namePos : TScriptPos); override;
         function CreateDataSymbol(const name, externalName : String;
                                   const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol; override;
         function CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                    typ : TTypeSymbol; const data : TData) : TConstSymbol; override;
         function ReadExpr(expecting : TTypeSymbol = nil) : TTypedExpr; override;
   end;

   // const expr created to "keep compiling"
   TBogusConstExpr = class sealed (TConstExpr);

   TSymbolAttributesBag = class (TInterfacedSelfObject, ISymbolAttributesBag)
      FAttributes : TdwsSymbolAttributes;
      destructor Destroy; override;
      function Attributes : TdwsSymbolAttributes;
   end;

   TSystemSymbols = class (TSystemSymbolTable, ISystemSymbols)
      FOperators : TSystemOperators;
      destructor Destroy; override;
      function Operators : TSystemOperators;
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
// ------------------ TSystemSymbols ------------------
// ------------------

// Destroy
//
destructor TSystemSymbols.Destroy;
begin
   FOperators.Free;
   inherited;
end;

// Operators
//
function TSystemSymbols.Operators : TSystemOperators;
begin
   Result:=FOperators;
end;

// ------------------
// ------------------ TStandardSymbolFactory ------------------
// ------------------

// Create
//
constructor TStandardSymbolFactory.Create(aCompiler : TdwsCompiler);
begin
   inherited Create;
   FCompiler := aCompiler;
end;

// CheckName
//
procedure TStandardSymbolFactory.CheckName(const name : String; const namePos : TScriptPos);
begin
   FCompiler.CheckName(name, namePos);
end;

// CreateDataSymbol
//
function TStandardSymbolFactory.CreateDataSymbol(const name, externalName : String;
      const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol;
begin
   CheckName(name, namePos);
   Result:=TDataSymbol.Create(name, typ);
   if externalName<>'' then
      Result.ExternalName:=externalName;
   FCompiler.CurrentProg.Table.AddSymbol(Result);
end;

// CreateConstSymbol
//
function TStandardSymbolFactory.CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                                  typ : TTypeSymbol; const data : TData) : TConstSymbol;
begin
   if data<>nil then
      Result:=TConstSymbol.CreateData(name, typ, data)
   else Result:=TConstSymbol.Create(name, typ);
   FCompiler.CurrentProg.Table.AddSymbol(Result);
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

// ReadInitExpr
//
function TStandardSymbolFactory.ReadInitExpr(expecting : TTypeSymbol = nil) : TTypedExpr;

   function ReadConstRecordInitExpr(recSym : TRecordSymbol) : TTypedExpr;
   begin
      // isolate because of the temporary dynamic array
      Result := TConstExpr.Create(expecting, FCompiler.ReadConstRecord(recSym), 0);
   end;

begin
   if expecting<>nil then begin
      case FCompiler.Tokenizer.TestAny([ttBLEFT, ttALEFT]) of
         ttBLEFT :
            if expecting.ClassType=TRecordSymbol then begin
               Result := ReadConstRecordInitExpr(TRecordSymbol(expecting));
               Exit;
            end else if expecting is TArraySymbol then begin
               FCompiler.Tokenizer.KillToken;
               Result := ReadArrayConstantExpr(ttBRIGHT, expecting);
               Exit;
            end;
         ttALEFT :
            if expecting is TArraySymbol then begin
               FCompiler.Tokenizer.KillToken;
               Result := ReadArrayConstantExpr(ttARIGHT, expecting);
               Exit;
            end else if expecting is TSetOfSymbol then begin
               FCompiler.Tokenizer.KillToken;
               Result := ReadArrayConstantExpr(ttARIGHT, expecting);
               Result := TConvExpr.WrapWithConvCast(FCompiler.FCompilerContext, FCompiler.Tokenizer.HotPos,
                                                    expecting, Result, CPE_IncompatibleTypes);
               Exit;
            end;
      end;
   end;
   Result:=ReadExpr(expecting)
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
function TCompositeTypeSymbolFactory.CreateDataSymbol(const name, externalName : String;
      const namePos : TScriptPos; typ : TTypeSymbol) : TDataSymbol;
var
   cvs : TClassVarSymbol;
begin
   CheckName(name, namePos);

   cvs:=TClassVarSymbol.Create(name, typ, FVisibility);
   cvs.AllocateStackAddr(FCompiler.CurrentProg.Table.AddrGenerator);
   if externalName<>'' then
      cvs.ExternalName:=externalName;
   FOwnerType.AddClassVar(cvs);
   Result:=cvs;
end;

// CreateConstSymbol
//
function TCompositeTypeSymbolFactory.CreateConstSymbol(const name : String; const namePos : TScriptPos;
                                                       typ : TTypeSymbol; const data : TData) : TConstSymbol;
var
   classConstSym : TClassConstSymbol;
begin
   if data<>nil then
      classConstSym:=TClassConstSymbol.CreateData(name, typ, data)
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

   FStringListPool.Initialize;

   FStandardDataSymbolFactory:=TStandardSymbolFactory.Create(Self);

   FTokRules:=TPascalTokenizerStateRules.Create;

   FLoopExprs:=TSimpleStack<TProgramExpr>.Create;
   FLoopExitable:=TSimpleStack<TLoopExitable>.Create;
   FFinallyExprs:=TSimpleStack<Boolean>.Create;
   FUnitsFromStack:=TSimpleStack<String>.Create;
   FUnitContextStack:=TdwsCompilerUnitContextStack.Create;
   FAnyFuncSymbol:=TAnyFuncSymbol.Create('', fkFunction, 0);

   FPendingAttributes:=TdwsSymbolAttributes.Create;

   stackParams.MaxLevel:=1;
   stackParams.ChunkSize:=512;
   stackParams.MaxByteSize:=MaxInt;
   stackParams.MaxRecursionDepth:=cDefaultMaxRecursionDepth;
   stackParams.MaxExceptionDepth:=cDefaultMaxExceptionDepth;

   FExec:=TdwsCompilerExecution.Create(stackParams, Self);

   FHelperMemberNames:=TSimpleStringHash.Create;

   FGenericSymbol := TSimpleStack<TGenericSymbol>.Create;
end;

// Destroy
//
destructor TdwsCompiler.Destroy;
begin
   FGenericSymbol.Free;

   FHelperMemberNames.Free;

   FPendingAttributes.Free;

   FAnyFuncSymbol.Free;

   FUnitsFromStack.Free;
   FUnitContextStack.Free;
   FExec.Free;
   FFinallyExprs.Free;
   FLoopExitable.Free;
   FLoopExprs.Free;
   FTokRules.Free;

   FStringListPool.Finalize;

   inherited;
end;

function TdwsCompiler.ResolveUnitReferences(scriptType : TScriptSourceType) : TIdwsUnitList;
var
   i, j, k : Integer;
   expectedUnitCount : Integer;
   deps : TStringList;
   refCount : array of Integer;
   changed : Boolean;
   unitName : String;
   curUnit : IdwsUnit;
begin
   // Check for duplicate unit names
   unitName:=FUnits.FindDuplicateUnitName;
   if unitName<>'' then
      FMsgs.AddCompilerStopFmt(cNullPos, CPH_UnitAlreadyReferred, [unitName]);

   // initialize reference count vector
   expectedUnitCount:=FUnits.Count;
   SetLength(refCount, expectedUnitCount);

   // Calculate number of outgoing references
   for i:=0 to FUnits.Count-1 do begin
      curUnit:=FUnits[i];
      if    (ufImplicitUse in curUnit.GetUnitFlags)
         or (  (scriptType<>stUnit)
             and not (coExplicitUnitUses in FOptions)) then begin
         deps := curUnit.GetDependencies;
         for j:=0 to deps.Count-1 do begin
            if FUnits.IndexOfName(deps[j])<0 then
               FMsgs.AddCompilerStopFmt(cNullPos, CPE_UnitNotFound,
                                        [deps[j], curUnit.GetUnitName]);
         end;
         refCount[i]:=deps.Count;
      end else begin
         refCount[i]:=-1;
         Dec(expectedUnitCount);
      end;
   end;

   Result:=TIdwsUnitList.Create;

   // Resolve references
   repeat
      changed:=False;
      for i:=0 to FUnits.Count-1 do begin
         // Find unit that is not referencing other units
         if refCount[i]=0 then begin
            curUnit:=FUnits[i];
            Result.Add(curUnit);

            // Remove the references to this unit from all other units
            unitName:=curUnit.GetUnitName;
            for j:=0 to FUnits.Count-1 do begin
               deps:=FUnits[j].GetDependencies;
               for k:=0 to deps.Count-1 do begin
                  if UnicodeSameText(deps[k], unitName) then
                     Dec(refCount[j]);
               end;
            end;

            refCount[i]:=-1;
            changed:=True;
         end;
      end;
   until not changed;

   if Result.Count<>expectedUnitCount then begin
      Result.Free;
      Result:=nil;
      FMsgs.AddCompilerStop(cNullPos, CPE_UnitCircularReference);
   end;
end;

// CreateTypedDefault
//
function TdwsCompiler.CreateTypedDefault(typ : TTypeSymbol) : TConstExpr;

   function CreateGenericTypedDefault : TConstExpr;
   var
      data : TData;
   begin
      SetLength(data, typ.Size);
      typ.InitData(data, 0);
      Result := TConstExpr.Create(typ, data, 0);
   end;

begin
   if typ = FCompilerContext.TypInteger then
      Result := FUnifiedConstants.CreateInteger(0)
   else if typ = FCompilerContext.TypString then
      Result := FUnifiedConstants.CreateEmptyString
   else Result := CreateGenericTypedDefault;
end;

// SetCurrentProg
//
procedure TdwsCompiler.SetCurrentProg(aProg : TdwsProgram);
begin
   FCurrentProg := aProg;
   if Assigned(aProg) then begin
      FCompilerContext := aProg.Root.CompilerContext;
      FCompilerContext.Prog := aProg;
      FCompilerContext.Execution := FExec;
      FCompilerContext.Options := FOptions;
   end else begin
      FCompilerContext := nil;
   end;
end;

// GetCompilerContext
//
function TdwsCompiler.GetCompilerContext : TdwsCompilerContext;
begin
   Result := FCompilerContext;
end;

// EnterLoop
//
procedure TdwsCompiler.EnterLoop(loopExpr : TProgramExpr);
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
      FCompilerContext.Msgs.AddCompilerWarning(FLoopExprs.Peek.ScriptPos, CPW_InfiniteLoop);

   if (FFinallyExprs.Count>0) and (not FFinallyExprs.Peek) then
      FFinallyExprs.Pop;

   FLoopExprs.Pop;
   FLoopExitable.Pop;
end;

// GetFuncExpr
//
function TdwsCompiler.GetFuncExpr(funcSym : TFuncSymbol; codeExpr : TTypedExpr = nil) : TFuncExprBase;
begin
   if codeExpr=nil then begin

      Result := CreateSimpleFuncExpr(FCompilerContext, FTok.HotPos, funcSym);

   end else begin

      Result:=TFuncPtrExpr.Create(FCompilerContext, FTok.HotPos, codeExpr);

   end;

   if (funcSym.Typ<>nil) and (funcSym.Typ.Size>1) and Result.InheritsFrom(TFuncExpr) then
      TFuncExpr(Result).SetResultAddr(CurrentProg, nil);
end;

// GetMethodExpr
//
function TdwsCompiler.GetMethodExpr(meth: TMethodSymbol; Expr: TTypedExpr; RefKind: TRefKind;
                                    const scriptPos : TScriptPos; options : TCreateFunctionOptions) : TFuncExprBase;
begin
   Result:=CreateMethodExpr(FCompilerContext, meth, Expr, RefKind, scriptPos, options);
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

// IncompatibleTypesWarn
//
procedure TdwsCompiler.IncompatibleTypesWarn(const scriptPos : TScriptPos; const fmt : String; typ1, typ2 : TTypeSymbol);
begin
   FMsgs.AddCompilerWarningFmt(scriptPos, fmt, [typ1.Caption, typ2.Caption]);
end;

// SetupCompileOptions
//
procedure TdwsCompiler.SetupCompileOptions(conf : TdwsConfiguration);
begin
   FCompilerAbort := False;

   FFilter := conf.Filter;
   FConnectors := conf.Connectors;
   FOptions := conf.CompilerOptions;
   FUnits := TIdwsUnitList.Create;
   FUnits.AddUnits(conf.Units);
   FOnInclude := conf.OnInclude;
   FOnNeedUnit := conf.OnNeedUnit;
   FOnResource := conf.OnResource;
   FOnCodeGen := conf.OnCodeGen;
   FOnFilter := conf.OnFilter;
   FScriptPaths := conf.ScriptPaths;

   FOnExecutionStarted := conf.OnExecutionStarted;
   FOnExecutionEnded := conf.OnExecutionEnded;

   conf.FOnCreateBaseVariantSymbol:=FOnCreateBaseVariantSymbol;
   conf.FOnCreateSystemSymbols:=FOnCreateSystemSymbols;
   if not StaticExtensionSymbols then
      conf.DetachSystemTable;

   if conf.CompileFileSystem<>nil then
      FCompileFileSystem := conf.CompileFileSystem.AllocateFileSystem
   else FCompileFileSystem := TdwsOSFileSystem.Create;

   FOnGetDefaultLocalizer := conf.DoGetLocalizer;

   FDataSymbolExprReuse := TSimpleObjectObjectHash_TDataSymbol_TVarExpr.Create;

   FDefaultConditionals := TAutoStrings.CreateClone(conf.Conditionals);
   FDefaultConditionals.Value.Duplicates := dupIgnore;
   FDefaultConditionals.Value.Sorted := True;
   FDefaultConditionals.Value.Add('DWSCRIPT');
   if Assigned(FOnApplyConditionalDefines) then
      FOnApplyConditionalDefines(FDefaultConditionals.Value);

   F8087CW:=DirectSet8087CW($133F);
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
var
   vCompileTidy : Integer = 8;
procedure TdwsCompiler.CleanupAfterCompile;
begin
   DirectSet8087CW(F8087CW);

   if InterlockedDecrement(vCompileTidy)=0 then begin
      TidyStringsUnifier;
      vCompileTidy:=8;
   end;

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
   FreeAndNil(FUnits);
   FSystemTable:=nil;
   FUnitContextStack.Clean;
   FUnitsFromStack.Clear;
   FCurrentUnitSymbol:=nil;
   FCurrentSourceUnit:=nil;

   FCurrentProg:=nil;
   if FCompilerContext <> nil then
      FCompilerContext.StringsUnifier.Clear;
   FCompilerContext:=nil;
   FMainProg:=nil;
   FUnifiedConstants:=nil;
   FSourceContextMap:=nil;
   FSymbolDictionary:=nil;

   FOnExecutionStarted:=nil;
   FOnExecutionEnded:=nil;

   FLoopExprs.Clear;
   FLoopExitable.Clear;
   FFinallyExprs.Clear;

   FHelperMemberNames.Clear;
end;

// Compile
//
function TdwsCompiler.Compile(const aCodeText : String; aConf : TdwsConfiguration;
                              const mainFileName : String = '') : IdwsProgram;
var
   stackParams : TStackParameters;
   codeText : String;
   sourceFile : TSourceFile;
   compileStartTicks : Int64;
begin
   compileStartTicks := GetSystemMilliseconds;

   SetupCompileOptions(aConf);

   // prepare Helper Member names lookup
   FHelperMemberNames.Clear;
   dwsInternalUnit.EnumerateHelperMemberNames(FHelperMemberNames);

   FGenericSymbol.Clear;

   stackParams.MaxByteSize:=aConf.MaxDataSize;
   if stackParams.MaxByteSize<=0 then
      stackParams.MaxByteSize:=MaxInt;

   stackParams.ChunkSize:=aConf.StackChunkSize;
   Assert(stackParams.ChunkSize>0);

   stackParams.MaxRecursionDepth:=aConf.MaxRecursionDepth;
   stackParams.MaxExceptionDepth:=aConf.MaxExceptionDepth;

   FLineCount:=0;

   // Create the TdwsProgram
   FMainProg:=CreateProgram(aConf.SystemSymbols, aConf.ResultType, stackParams, mainFileName);
   FSystemTable:=FMainProg.SystemTable.SymbolTable;
   FUnifiedConstants:=TUnifiedConstants(FMainProg.UnifiedConstants);

   FMsgs:=FMainProg.CompileMsgs;
   SetupMsgsOptions(aConf);

   FMainProg.Compiler:=Self;
   FMainProg.TimeoutMilliseconds:=aConf.TimeoutMilliseconds;
   FMainProg.RuntimeFileSystem:=aConf.RuntimeFileSystem;
   FMainProg.ConditionalDefines.Value.Assign(FDefaultConditionals.Value);
   FMainProg.OnExecutionStarted:=FOnExecutionStarted;
   FMainProg.OnExecutionEnded:=FOnExecutionEnded;
   FSourceContextMap:=FMainProg.SourceContextMap;
   FSymbolDictionary:=FMainProg.SymbolDictionary;
   FUnitSection:=secMixed;

   CurrentProg := FMainProg;


   FOperators:=aConf.SystemSymbols.Operators;
   FMainProg.Operators:=FOperators;

   if Assigned(FExternalRoutinesManager) then
      FExternalRoutinesManager.BeginCompilation(Self);

   try
      CheckFilterDependencies(aConf.Units);

      // Filter stuff
      if Assigned(FFilter) then
         codeText := FFilter.Process(aCodeText, FMsgs)
      else codeText := aCodeText;

      sourceFile:=FMainProg.SourceList.Add(MSG_MainModule, codeText, stMain);

      // Start compilation
      CurrentProg.Expr := ReadScript(sourceFile, stMain);
      ReadScriptImplementations;

      if CurrentProg.Expr=nil then
         CurrentProg.Expr:=TNullExpr.Create(cNullPos);

      HintUnusedPrivateSymbols;

      // Initialize symbol table
      CurrentProg.Table.Initialize(FMsgs);
      CurrentProg.UnitMains.Initialize(FMsgs);

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

   if Assigned(FExternalRoutinesManager) then
      FExternalRoutinesManager.EndCompilation(Self);

   if FMsgs.State=mlsInProgress then
      FMsgs.State:=mlsCompleted;
   FMsgs.RemoveInvalidDeferred;

   FMainProg.TimeStamp := Now;
   FMainProg.CompileDurationMSec := GetSystemMilliseconds-compileStartTicks;
   FMainProg.LineCount := FLineCount;
   FMainProg.Compiler := nil;

   Result := FMainProg;

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
procedure TdwsCompiler.HandleUnitDependencies(scriptType : TScriptSourceType);
var
   i : Integer;
   unitsResolved : TIdwsUnitList;
   unitTable : TUnitSymbolTable;
   unitSymbol : TUnitMainSymbol;
begin
   unitsResolved:=ResolveUnitReferences(scriptType);
   try
      // Get the symboltables of the units
      for i:=0 to unitsResolved.Count-1 do begin
         unitTable:=unitsResolved[i].GetUnitTable(FSystemTable, FMainProg.UnitMains, FOperators, FMainProg.RootTable);
         unitSymbol:=TUnitMainSymbol.Create(unitsResolved[i].GetUnitName, unitTable, FMainProg.UnitMains);
         unitSymbol.DeprecatedMessage:=unitsResolved[i].GetDeprecatedMessage;
         unitSymbol.ReferenceInSymbolTable(CurrentProg.Table, True);
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
   dependencies : TStringList;
   unitSource : String;
   srcUnit : TSourceUnit;
   oldContext : TdwsSourceContext;
begin
   for i:=0 to FUnitsFromStack.Count-1 do
      if UnicodeSameText(FUnitsFromStack.Items[i], unitName) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UnitCircularReference);

   Result:=TUnitSymbol(CurrentProg.Table.FindLocal(unitName, TUnitSymbol));
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
            srcUnit:=TSourceUnit.Create(unitName, CurrentProg.Root.RootTable, CurrentProg.UnitMains);
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

   dependencies := unitResolved.GetDependencies;
   for i:=0 to dependencies.Count-1 do begin
      FUnitsFromStack.Push(unitName);
      try
         HandleExplicitDependency(dependencies[i]);
      finally
         FUnitsFromStack.Pop;
      end;
   end;

   unitMain:=CurrentProg.UnitMains.Find(unitName);
   if unitMain=nil then begin
      unitTable:=nil;
      try
         unitTable:=unitResolved.GetUnitTable(FSystemTable, CurrentProg.UnitMains, FOperators, CurrentProg.RootTable);
         unitMain:=TUnitMainSymbol.Create(unitName, unitTable, CurrentProg.UnitMains);
         unitMain.DeprecatedMessage:=unitResolved.GetDeprecatedMessage;
      except
         unitTable.Free;
         raise;
      end;
   end;

   Result:=unitMain.ReferenceInSymbolTable(CurrentProg.Table, False);
end;

type
   TUnitMainSymbolArray = array of TUnitMainSymbol;

   TRankedUnits = class
      Ranked : TUnitMainSymbolArray;
      function Compare(index1, index2 : Integer) : Integer;
      procedure Swap(index1, index2 : Integer);
   end;

function TRankedUnits.Compare(index1, index2 : Integer) : Integer;
begin
   Result:=Ranked[index1].InitializationRank-Ranked[index2].InitializationRank;
end;

// Swap
//
procedure TRankedUnits.Swap(index1, index2 : Integer);
var
   t : TUnitMainSymbol;
begin
   t:=Ranked[index1];
   Ranked[index1]:=Ranked[index2];
   Ranked[index2]:=t;
end;

// SetupInitializationFinalization
//
procedure TdwsCompiler.SetupInitializationFinalization;
var
   rankedUnits : TUnitMainSymbolArray;

   procedure RankUnits(rankLow : Integer);
   var
      r : Integer;
      ums, dep : TUnitMainSymbol;
      change : Boolean;
   begin
      change:=False;
      for ums in rankedUnits do begin
         r:=ums.InitializationRank+1;
         if r>=rankLow then begin
            for dep in ums.Dependencies do begin
               if dep.InitializationRank<r then begin
                  dep.InitializationRank:=r;
                  change:=True;
               end;
            end;
         end;
      end;
      if change and (rankLow<High(rankedUnits)) then
         RankUnits(rankLow+1);
   end;

   procedure SortRankedUnits;
   var
      ranked : TRankedUnits;
      sorter : TQuickSort;
   begin
      ranked:=TRankedUnits.Create;
      try
         ranked.Ranked:=rankedUnits;
         sorter.CompareMethod:=ranked.Compare;
         sorter.SwapMethod:=ranked.Swap;
         sorter.Sort(0, High(rankedUnits));
         rankedUnits:=ranked.Ranked;
      finally
         ranked.Free;
      end;
   end;

var
   i, k : Integer;
   ums : TUnitMainSymbol;
   unitInitExpr : TBlockExprBase;
begin
   // collect and rank all units with an initialization or finalization sections
   // NOTE: UnitMains order may change arbitrarily in the future, hence the need to reorder
   SetLength(rankedUnits, CurrentProg.UnitMains.Count);
   k:=0;
   for i:=0 to CurrentProg.UnitMains.Count-1 do begin
      ums:=CurrentProg.UnitMains[i];
      if (ums.InitializationExpr<>nil) or (ums.FinalizationExpr<>nil) then begin
         rankedUnits[k]:=ums;
         Inc(k);
         ums.InitializationRank:=0;
      end;
   end;
   if k=0 then Exit;

   // compute dependency graph rank & sort accordingly
   SetLength(rankedUnits, k);
   RankUnits(0);
   SortRankedUnits;

   // append initializations to InitExpr of the main prog
   for i:=High(rankedUnits) downto 0 do begin
      ums:=rankedUnits[i];
      if (ums<>nil) and (ums.InitializationExpr<>nil) then begin
         unitInitExpr:=ums.InitializationExpr as TBlockExprBase;
         if coOptimize in Options then begin
            if unitInitExpr.StatementCount=0 then continue;
         end;
         FMainProg.InitExpr.AddStatement(unitInitExpr);
         ums.InitializationExpr.IncRefCount;
      end;
   end;
   // append initializations to FinalExpr of the main prog in reverse order
   for i:=0 to High(rankedUnits) do begin
      ums:=rankedUnits[i];
      if (ums<>nil) and (ums.FinalizationExpr<>nil) then begin
         FMainProg.AddFinalExpr(ums.FinalizationExpr as TBlockExprBase);
         ums.FinalizationExpr.IncRefCount;
      end;
   end;
end;

// OrphanObject
//
procedure TdwsCompiler.OrphanObject(obj : TRefCountedObject);
begin
   if obj <> nil then
      FCompilerContext.OrphanObject(obj);
end;

// OrphanAndNil (TTypedExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr : TTypedExpr);
begin
   if expr <> nil then begin
      expr.Orphan(FCompilerContext);
      expr := nil;
   end;
end;

// OrphanAndNil (TProgramExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr : TProgramExpr);
begin
   if expr <> nil then begin
      expr.Orphan(FCompilerContext);
      expr := nil;
   end;
end;

// OrphanAndNil (TDataExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr : TDataExpr);
begin
   if expr <> nil then begin
      expr.Orphan(FCompilerContext);
      expr := nil;
   end;
end;

// OrphanAndNil (TBlockExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr : TBlockExpr);
begin
   if expr <> nil then begin
      expr.Orphan(FCompilerContext);
      expr := nil;
   end;
end;

// OrphanAndNil (TVarExpr)
//
procedure TdwsCompiler.OrphanAndNil(var expr : TVarExpr);
begin
   if expr <> nil then begin
      expr.Orphan(FCompilerContext);
      expr := nil;
   end;
end;

// Compiler
//
function TdwsCompiler.Compiler : TdwsCompiler;
begin
   Result:=Self;
end;

// GetCurrentProg
//
function TdwsCompiler.GetCurrentProg : TdwsProgram;
begin
   Result:=CurrentProg;
end;

// GetMsgs
//
function TdwsCompiler.GetMsgs : TdwsCompileMessageList;
begin
   Result:=Msgs;
end;

// GetTokenizer
//
function TdwsCompiler.GetTokenizer : TTokenizer;
begin
   Result:=Tokenizer;
end;

// SetExternalFunctionsManager
//
function TdwsCompiler.GetExternalFunctionsManager: IdwsExternalFunctionsManager;
begin
   result := FExternalRoutinesManager;
end;

// SetExternalFunctionsManager
//
procedure TdwsCompiler.SetExternalFunctionsManager(const value : IdwsExternalFunctionsManager);
begin
   FExternalRoutinesManager:=value;
end;

// CompileTimeExecution
//
function TdwsCompiler.CompileTimeExecution : TdwsExecution;
begin
   Result:=FExec;
end;

// AttachContextProgram
//
procedure TdwsCompiler.AttachContextProgram(contextProgram : TdwsProgram);
begin
   CurrentProg := contextProgram;
   FMainProg := contextProgram.Root;
   FUnifiedConstants := TUnifiedConstants(FMainProg.UnifiedConstants);
   FSourceContextMap := FMainProg.SourceContextMap;
   FSymbolDictionary := FMainProg.SymbolDictionary;
   FOperators := (FMainProg.Operators as TOperators);
   FMsgs := FMainProg.CompileMsgs;
end;

// DetachContextProgram
//
procedure TdwsCompiler.DetachContextProgram;
begin
   FMsgs := nil;
   FOperators := nil;
   CurrentProg := nil;
   FMainProg := nil;
   FUnifiedConstants := nil;
   FSourceContextMap := nil;
   FSymbolDictionary := nil;
end;

// AttachTokenizer
//
procedure TdwsCompiler.AttachTokenizer(tok : TTokenizer);
begin
   FTok := tok;
end;

// DetachTokenizer
//
function TdwsCompiler.DetachTokenizer : TTokenizer;
begin
   Result := FTok;
   FTok := nil;
end;

// CheckMatchingDeclarationCase
//
procedure TdwsCompiler.CheckMatchingDeclarationCase(const nameString : String; sym : TSymbol;
                                                    const scriptPos : TScriptPos);
begin
   if (nameString<>sym.Name) and UnicodeSameText(nameString, sym.Name) then
      FMsgs.AddCompilerHintFmt(scriptPos, CPH_CaseDoesNotMatchDeclaration,
                               [nameString, sym.Name], hlPedantic);
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
   FUnifiedConstants:=TUnifiedConstants(FMainProg.UnifiedConstants);
   FSourceContextMap:=FMainProg.SourceContextMap;
   FSymbolDictionary:=FMainProg.SymbolDictionary;

   FMsgs:=FMainProg.CompileMsgs;
   SetupMsgsOptions(aConf);

   CurrentProg := FMainProg;

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
      FCurrentSourceUnit:=nil;
      FUnitSection:=secMixed;

      // Start compilation
      CurrentProg.Expr:=ReadScript(sourceFile, stRecompile);
      ReadScriptImplementations;

      // Initialize symbol table
      CurrentProg.Table.Initialize(FMsgs);
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

// Evaluate
//
class procedure TdwsCompiler.Evaluate;
begin
   // dummy
end;

// AbortCompilation
//
procedure TdwsCompiler.AbortCompilation;
begin
   FCompilerAbort:=True;
end;

// Optimize
//
function TdwsCompiler.Optimize : Boolean;
begin
   Result := FCompilerContext.Optimize;
end;

// ReadRootBlock
//
function TdwsCompiler.ReadRootBlock(const endTokens : TTokenTypes; var finalToken : TTokenType) : TBlockExpr;
var
   reach : TReachStatus;
   stmt : TProgramExpr;
   action : TdwsStatementAction;
begin
   reach:=rsReachable;
   Result:=TBlockExpr.Create(FCompilerContext, FTok.HotPos);
   try
      while FTok.HasTokens do begin
         finalToken:=FTok.TestDeleteAny(endTokens);
         if finalToken<>ttNone then Break;

         if reach=rsUnReachable then begin
            reach:=rsUnReachableWarned;
            FMsgs.AddCompilerWarning(FTok.HotPos, CPW_UnReachableCode);
         end;
         if FCompilerAbort then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_CompilationAborted);

         stmt:=ReadRootStatement(action, Result);
         if Assigned(stmt) then begin
            Result.AddStatement(Stmt);
            if (reach=rsReachable) and (stmt.InterruptsFlow) then
               reach:=rsUnReachable;
         end;

         case action of
            saNone : begin
               if not FTok.TestDelete(ttSEMI) then begin
                  if endTokens<>[] then begin
                     finalToken:=FTok.TestDeleteAny(endTokens);
                     if finalToken=ttNone then
                        UnexpectedBlockTokenError(endTokens);
                     Break;
                  end else begin
                     if FTok.HasTokens then
                        UnexpectedBlockTokenError(endTokens);
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
      OrphanAndNil(Result);
      raise;
   end;
end;

// ReadSemiColon
//
procedure TdwsCompiler.ReadSemiColon(fatal : Boolean = False);
begin
   if not FTok.TestDelete(ttSEMI) then begin
      if fatal then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected)
      else FMsgs.AddCompilerError(FTok.HotPos, CPE_SemiExpected);
   end;
end;

// ReadScript
//
function TdwsCompiler.ReadScript(sourceFile : TSourceFile; scriptType : TScriptSourceType) : TProgramExpr;
var
   oldTok : TTokenizer;
   oldSection : TdwsUnitSection;
   initialToken, finalToken : TTokenType;
   unitBlock : TBlockExpr;
   readingMain : Boolean;
   contextFix : TdwsSourceContext;
   i : Integer;
   unitSymbol : TUnitSymbol;
begin
   oldTok:=FTok;
   oldSection:=FUnitSection;
   FTok:=FTokRules.CreateTokenizer(FCompilerContext.Msgs, FCompilerContext.StringsUnifier);
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
      if scriptType in [stMain, stRecompile] then begin
         FTok.ConditionalDefines:=FMainProg.ConditionalDefines;
         readingMain:=True;
      end else begin
         FTok.ConditionalDefines:=FDefaultConditionals.Clone;
         readingMain:=False;
      end;

      if readingMain then begin
         if FTok.Test(ttUNIT) then begin
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
            scriptType:=stUnit;
            HandleUnitDependencies(scriptType);
         end else begin
            initialToken:=FTok.TestDeleteAny([ttPROGRAM, ttLIBRARY]);
            if initialToken<>ttNone then begin
               if initialToken=ttPROGRAM then
                  FMainProg.ProgramType:=ptProgram
               else FMainProg.ProgramType:=ptLibrary;
               UnitSection:=secProgram;
               if not FTok.TestName then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected)
               else FTok.KillToken;
               ReadSemiColon;
            end else begin
               FMainProg.ProgramType:=ptScript;
            end;
         end;
      end;

      if Assigned(FOnReadScript) then
         FOnReadScript(Self, sourceFile, scriptType);

      case scriptType of
         stMain : begin
            HandleUnitDependencies(scriptType);
         end;
         stUnit : begin
            FUnitSection:=secHeader;
            scriptType:=ReadUnitHeader;
         end;
      end;

      Result:=ReadRootBlock([], finalToken);

      case scriptType of
         stUnit : begin
            if finalToken in [ttNone, ttEND, ttINITIALIZATION, ttFINALIZATION] then begin
               if coContextMap in Options then
                  FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
            end;
            CurrentProg.InitExpr.AddStatement(Result);
            Result:=nil;
         end;
         stMain, stRecompile : begin
            if coContextMap in Options then begin
               FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
            end;
         end;
         stUnitNamespace : begin
            if (finalToken=ttNone) or (finalToken=ttEND) then begin
               if coContextMap in Options then
                  FSourceContextMap.CloseAllContexts(FTok.CurrentPos);
            end;
            for i:=0 to CurrentUnitSymbol.Table.Count-1 do begin
               if CurrentUnitSymbol.Table[i] is TUnitSymbol then begin
                  unitSymbol:=TUnitSymbol(CurrentUnitSymbol.Table[i]);
                  if unitSymbol.Main<>nil then
                     CurrentSourceUnit.GetDependencies.Add(unitSymbol.Name);
               end;
            end;
            OrphanAndNil(Result);
         end;
      end;

      if FTok.ConditionalDepth.Count>0 then
         FMsgs.AddCompilerError(FTok.ConditionalDepth.Peek.ScriptPos, CPE_UnbalancedConditionalDirective);

      if finalToken=ttIMPLEMENTATION then begin
         if coSymbolDictionary in Options then
            RecordSymbolUse(CurrentUnitSymbol, FTok.HotPos, [suImplementation, suImplicit]);
         if readingMain then begin
            if ReadImplementationBlock<>ttEND then begin
               unitBlock:=ReadRootBlock([], finalToken);
               CurrentProg.InitExpr.AddStatement(unitBlock);
            end;
            FLineCount:=FLineCount+FTok.CurrentPos.Line-2;
            FTok.Free;
         end else begin
            FUnitContextStack.PushContext(Self);
         end;
      end else begin
         FLineCount:=FLineCount+FTok.CurrentPos.Line-2;
         FTok.Free;
      end;
      FTok:=nil;

      if (Result<>nil) and Optimize then
         Result:=Result.Optimize(FCompilerContext);
   finally
      FTok.Free;
      FTok:=oldTok;
      FUnitSection:=oldSection;
   end;
end;

// ReadImplementationBlock
//
function TdwsCompiler.ReadImplementationBlock : TTokenType;
var
   unitBlock : TBlockExpr;
   initializationBlock, finalizationBlock : TBlockExpr;
begin
   initializationBlock:=nil;
   finalizationBlock:=nil;
   unitBlock:=ReadRootBlock([ttINITIALIZATION, ttFINALIZATION], Result);
   try
      if Result=ttINITIALIZATION then begin
         FUnitSection:=secInitialization;
         if coContextMap in Options then
            FSourceContextMap.OpenContext(FTok.HotPos, CurrentUnitSymbol, ttINITIALIZATION);
         initializationBlock:=ReadRootBlock([ttFINALIZATION, ttEND], Result);
         if coContextMap in Options then
            FSourceContextMap.CloseContext(FTok.HotPos);
      end;
      if Result=ttFINALIZATION then begin
         FUnitSection:=secFinalization;
         if coContextMap in Options then
            FSourceContextMap.OpenContext(FTok.HotPos, CurrentUnitSymbol, ttFINALIZATION);
         finalizationBlock:=ReadRootBlock([ttEND], Result);
         if coContextMap in Options then
            FSourceContextMap.CloseContext(FTok.HotPos);
      end;

      if coContextMap in Options then
         FSourceContextMap.CloseAllContexts(FTok.CurrentPos);

      if unitBlock.StatementCount>0 then begin
         CurrentProg.InitExpr.AddStatement(unitBlock);
         unitBlock:=nil;
      end;

      if coOptimize in Options then begin
         if (initializationBlock<>nil) and (initializationBlock.StatementCount=0) then
            OrphanAndNil(initializationBlock);
         if (finalizationBlock<>nil) and (finalizationBlock.StatementCount=0) then
            OrphanAndNil(finalizationBlock);
      end;

      if CurrentUnitSymbol<>nil then begin
         // this is a normal unit
         if initializationBlock<>nil then begin
            CurrentUnitSymbol.InitializationExpr:=initializationBlock;
            initializationBlock:=nil;
         end;
         if finalizationBlock<>nil then begin
            CurrentUnitSymbol.FinalizationExpr:=finalizationBlock;
            finalizationBlock:=nil;
         end;
      end else begin
         // special case of main program
         if initializationBlock<>nil then begin
            CurrentProg.InitExpr.AddStatement(initializationBlock);
            initializationBlock:=nil;
         end;
         if finalizationBlock<>nil then begin
            FMainProg.AddFinalExpr(finalizationBlock);
            finalizationBlock:=nil;
         end;
      end;
   finally
      OrphanAndNil(unitBlock);
      OrphanAndNil(initializationBlock);
      OrphanAndNil(finalizationBlock);
   end;
end;

// ReadScriptImplementations
//
procedure TdwsCompiler.ReadScriptImplementations;
var
   implemTable : TUnitImplementationTable;
   oldUnit : TSourceUnit;
begin
   while FUnitContextStack.Count>0 do begin
      FUnitContextStack.PopContext(Self, oldUnit);
      FUnitSection:=secImplementation;
      try
         implemTable:=TUnitImplementationTable.Create(CurrentUnitSymbol);
         CurrentProg.EnterSubTable(implemTable);
         try
            ReadImplementationBlock;
         finally
            CurrentProg.LeaveSubTable;
         end;
         FLineCount:=FLineCount+FTok.CurrentPos.Line-2;
      finally
         FTok.Free;
         FTok:=nil;
         LeaveUnit(oldUnit);
      end;
   end;
end;

// ReadRootStatement
//
function TdwsCompiler.ReadRootStatement(var action : TdwsStatementAction; initVarBlockExpr : TBlockExpr) : TProgramExpr;
var
   hotPos : TScriptPos;
   token : TTokenType;
   rootBlock : Boolean;
begin
   action:=saNone;
   Result:=nil;

   FTok.TestName;
   hotPos:=FTok.HotPos;

   token:=FTok.TestDeleteAny([ttTYPE, ttPROCEDURE, ttFUNCTION,
                              ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttCLASS,
                              ttUSES, ttIMPLEMENTATION, ttEND]);
   case token of
      ttTYPE : begin
         if UnitSection in [secInterface, secImplementation] then
            ReadTypeDeclBlock
         else ReadTypeDecl(True);
         action:=saNoSemiColon
      end;
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
         if (CurrentProg.Table<>CurrentProg.Root.Table) or (UnitSection<>secInterface) then begin
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
         if (CurrentProg.Table<>CurrentProg.Root.Table) or (UnitSection<>secImplementation) then
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
      rootBlock:=FTok.Test(ttBEGIN);
      Result:=ReadStatement(action, initVarBlockExpr);
      if rootBlock and FTok.TestDelete(ttDOT) then
         action:=saEnd;
   end;
end;

// ReadStatement
//
function TdwsCompiler.ReadStatement(var action : TdwsStatementAction; initVarBlockExpr : TBlockExpr) : TProgramExpr;
var
   token : TTokenType;
begin
   Result:=nil;
   token:=FTok.TestDeleteAny([ttVAR, ttCONST, ttOPERATOR, ttRESOURCESTRING]);
   case token of
      ttVAR :
         ReadVarDeclBlock(action, initVarBlockExpr);
      ttCONST :
         ReadConstDeclBlock(action);
      ttRESOURCESTRING :
         ReadResourceStringDeclBlock(action);
      ttOPERATOR :
         ReadOperatorDecl;
   else
      if CurrentProg.Level=0 then begin
         case UnitSection of
            secMixed, secInitialization, secFinalization : ;
            secProgram : UnitSection:=secMixed;
         else
            FMsgs.AddCompilerError(FTok.HotPos, CPE_UnexpectedStatement);
         end;
      end;
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
      if not expr.IsConstant then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
         OrphanAndNil(expr);
      end else if (expr.Typ=nil) or not expr.Typ.IsOfType(FCompilerContext.TypString) then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
         OrphanAndNil(expr);
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
      OrphanAndNil(expr);
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
      CurrentProg.Table.AddSymbol(resStringSym);
      ReadSemiColon;
   until not (    (UnitSection in [secInterface, secImplementation])
              and (CurrentProg.Level=0)
              and FTok.TestName);
end;

// ReadVarDeclBlock
//
procedure TdwsCompiler.ReadVarDeclBlock(var action : TdwsStatementAction; initVarBlockExpr : TBlockExprBase);
begin
   action:=saNoSemiColon;
   repeat
      ReadVarDecl(FStandardDataSymbolFactory, initVarBlockExpr);
      ReadSemiColon;
   until not (    (UnitSection in [secProgram, secInterface, secImplementation])
              and (CurrentProg.Level=0)
              and FTok.TestName);
end;

// ReadVarDecl
//
procedure TdwsCompiler.ReadVarDecl(const dataSymbolFactory : IdwsDataSymbolFactory; initVarBlockExpr : TBlockExprBase);
var
   names, externalNames : TSimpleStringList;
   posArray : TScriptPosArray;
begin
   names := FStringListPool.Acquire;
   externalNames := FStringListPool.Acquire;
   try
      ReadNameList(names, posArray, [], externalNames);
      ReadNamedVarsDecl(names, externalNames, posArray, dataSymbolFactory, initVarBlockExpr);
   finally
      FStringListPool.Release(externalNames);
      FStringListPool.Release(names);
   end;
end;

// ReadNamedVarsDecl
//
procedure TdwsCompiler.ReadNamedVarsDecl(names, externalNames : TSimpleStringList;
                                         const posArray : TScriptPosArray;
                                         const dataSymbolFactory : IdwsDataSymbolFactory;
                                         initVarBlockExpr : TBlockExprBase);
var
   x : Integer;
   sym : TDataSymbol;
   typ : TTypeSymbol;
   hotPos : TScriptPos;
   initExpr : TTypedExpr;
   assignExpr : TProgramExpr;
   externalName : String;
begin
   initExpr := nil;
   try
      hotPos:=FTok.HotPos;

      if FTok.TestDelete(ttCOLON) then begin

         // explicit typing
         //    var myVar : type
         //    var myVar : type = expr
         //    var myVar : type := expr
         typ:=ReadType('', tcVariable);
         if names.Count=1 then begin
            if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then
               initExpr:=dataSymbolFactory.ReadInitExpr(typ);
         end;

      end else if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then begin

         // inferred typing
         //    var myVar = expr
         //    var myVar := expr
         if names.Count<>1 then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
         initExpr:=dataSymbolFactory.ReadExpr(nil);
         if initExpr<>nil then begin
            typ:=initExpr.Typ;
            RecordSymbolUseImplicitReference(typ, hotPos, False);
         end else typ:=nil;

         if typ=nil then begin
            FMsgs.AddCompilerError(hotPos, CPE_RightSideNeedsReturnType);
            OrphanAndNil(initExpr);
         end else if typ=FCompilerContext.TypNil then
            if not (initExpr is TBogusConstExpr) then
               FMsgs.AddCompilerError(hotPos, CPE_TypeCouldNotBeInferenced);

      end else begin

         // keep going
         typ := FCompilerContext.TypVariant;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      end;

      // keep going in case of error
      if typ=nil then
         typ:=FCompilerContext.TypVariant
      else if (typ is TClassSymbol) and TClassSymbol(typ).IsStatic then
         FMsgs.AddCompilerErrorFmt(hotPos, CPE_ClassIsStaticNoInstances, [typ.Name]);

      for x:=0 to names.Count-1 do begin
         if externalNames<>nil then
            externalName:=externalNames[x];
         assignExpr:=CreateNamedVarDeclExpr(dataSymbolFactory, names[x], externalName,
                                            posArray[x], typ, initExpr, sym);
         if assignExpr<>nil then
            initVarBlockExpr.AddStatement(assignExpr);
      end;
   finally
      OrphanAndNil(initExpr);
   end;
end;

// CreateNamedVarDeclExpr
//
function TdwsCompiler.CreateNamedVarDeclExpr(const dataSymbolFactory : IdwsDataSymbolFactory;
                                             const name, externalName : String;
                                             const scriptPos : TScriptPos;
                                             typ : TTypeSymbol; var initExpr : TTypedExpr;
                                             var sym : TDataSymbol) : TProgramExpr;
var
   initData : TData;
   assignExpr : TAssignExpr;
   constExpr : TConstExpr;
   varExpr : TVarExpr;
begin
   Result:=nil;

   sym:=dataSymbolFactory.CreateDataSymbol(name, externalName, scriptPos, typ);

   varExpr:=GetVarExpr(scriptPos, sym);
   if Assigned(initExpr) then begin

      // Initialize with an expression
      RecordSymbolUse(sym, scriptPos, [suDeclaration, suReference, suWrite]);

      if (externalName<>'') and (sym is TClassVarSymbol) then
         FMsgs.AddCompilerError(scriptPos, CPE_ExternalClassVariablesInitializationIsNotSupported);

      {$ifndef COALESCE_VAR_INITIALIZATION}
      CurrentProg.InitExpr.AddStatement(
         TInitDataExpr.Create(FCompilerContext, scriptPos, varExpr));
      varExpr.IncRefCount;
      {$endif}

      Result:=CreateAssign(scriptPos, ttASSIGN, varExpr, initExpr);
      initExpr:=nil;

   end else begin

      RecordSymbolUse(sym, scriptPos, [suDeclaration]);

      if sym.Typ.DynamicInitialization then begin

         CurrentProg.InitExpr.AddStatement(
            TInitDataExpr.Create(FCompilerContext, scriptPos, varExpr));

      end else begin

         // Initialize with default value
         if (varExpr.Typ=FCompilerContext.TypInteger) or (varExpr.Typ is TEnumerationSymbol) then
            assignExpr:=TAssignConstToIntegerVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr, 0)
         else if varExpr.Typ=FCompilerContext.TypFloat then
            assignExpr:=TAssignConstToFloatVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr, 0)
         else if varExpr.Typ=FCompilerContext.TypBoolean then
            assignExpr:=TAssignConstToBoolVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr, False)
         else if varExpr.Typ=FCompilerContext.TypString then
            assignExpr:=TAssignConstToStringVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr, '')
         else if varExpr.Typ=FCompilerContext.TypVariant then
            assignExpr:=TAssignConstToVariantVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr, Unassigned)
         else if varExpr.Typ.ClassType=TClassSymbol then
            assignExpr:=TAssignNilToVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr)
         else if varExpr.Typ.ClassType=TClassOfSymbol then
            assignExpr:=TAssignNilClassToVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr)
         else if varExpr.Typ.AsFuncSymbol<>nil then
            assignExpr:=TAssignNilToVarExpr.CreateVal(FCompilerContext, scriptPos, varExpr)
         else begin
            initData := nil;
            SetLength(initData, sym.Typ.Size);
            TDataSymbol(sym).Typ.InitData(initData, 0);

            constExpr:=TConstExpr.Create(sym.Typ, initData, 0);
            assignExpr:=TAssignConstDataToVarExpr.Create(FCompilerContext, scriptPos, varExpr, constExpr);
         end;
         CurrentProg.InitExpr.AddStatement(assignExpr);

      end;

   end;
end;

// ReadConstSymbol
//
function TdwsCompiler.ReadConstSymbol(const name : String; const constPos : TScriptPos;
                                     typ : TTypeSymbol;
                                     const factory : IdwsDataSymbolFactory) : TConstSymbol;
var
   expr : TTypedExpr;
   dataExpr : TDataExpr;
   sas : TStaticArraySymbol;
   recordData : TData;
   exprPos : TScriptPos;
begin
   if typ is TRecordSymbol then begin

      recordData:=ReadConstRecord(TRecordSymbol(typ));
      Result:=factory.CreateConstSymbol(name, constPos, typ, recordData);

   end else begin

      exprPos := FTok.HotPos;
      if typ is TArraySymbol then begin
         case FTok.TestDeleteAny([ttALEFT, ttBLEFT]) of
            ttALEFT : expr:=factory.ReadArrayConstantExpr(ttARIGHT, typ);
            ttBLEFT : expr:=factory.ReadArrayConstantExpr(ttBRIGHT, typ);
         else
            expr:=factory.ReadExpr(nil);
         end;
      end else expr:=factory.ReadExpr(nil);
      if (expr <> nil) and expr.ScriptPos.Defined then
         exprPos := expr.ScriptPos;
      try
         if Assigned(typ) then begin
            if expr=nil then begin
               // keep compiling
               expr := TConvInvalidExpr.Create(FCompilerContext, nil, typ);
            end else if not typ.IsCompatible(expr.Typ) then
               expr:=CompilerUtils.WrapWithImplicitConversion(FCompilerContext, expr, typ, exprPos);
         end else if expr<>nil then begin
            typ:=expr.typ;
         end;

         if not expr.IsConstant then begin

            if not (expr is TConvInvalidExpr) then
               FMsgs.AddCompilerError(exprPos, CPE_ConstantExpressionExpected);
            // keep compiling
            if typ=nil then
               typ:=FCompilerContext.TypVariant;
            Result:=factory.CreateConstSymbol(name, constPos, typ, nil);

         end else begin

            if typ is TArraySymbol then begin

               if typ is TStaticArraySymbol then
                  sas:=TStaticArraySymbol(typ)
               else begin
                  sas:=TStaticArraySymbol.Create('', typ.Typ, FCompilerContext.TypInteger, 0, TArraySymbol(typ).typ.Size-1);
                  CurrentProg.Table.AddSymbol(sas);
               end;
               if expr is TConstExpr then begin
                  Result:=factory.CreateConstSymbol(name, constPos, sas, TConstExpr(expr).Data);
               end else begin
                  Result:=factory.CreateConstSymbol(name, constPos, sas,
                                                    (expr as TArrayConstantExpr).EvalAsTData(FExec));
               end;

            end else begin

               if typ.Size=1 then begin
                  SetLength(recordData, 1);
                  expr.EvalAsVariant(FExec, recordData[0]);
                  Result:=factory.CreateConstSymbol(name, constPos, typ, recordData);
               end else begin
                  dataExpr:=(expr as TDataExpr);
                  FExec.Stack.Push(CurrentProg.DataSize);
                  try
                     SetLength(recordData, typ.Size);
                     dataExpr.DataPtr[FExec].CopyData(recordData, 0, typ.Size);
                     Result:=factory.CreateConstSymbol(name, constPos, typ, recordData);
                  finally
                     FExec.Stack.Pop(CurrentProg.DataSize);
                  end;
               end;

            end;

         end;

      finally
         OrphanAndNil(expr);
      end;
   end;
end;

// ReadConstDecl
//
procedure TdwsCompiler.ReadConstDecl(const factory : IdwsDataSymbolFactory);
var
   name : String;
   typ : TTypeSymbol;
   constPos : TScriptPos;
   constSym : TConstSymbol;
begin
   if not FTok.TestDeleteNamePos(name, constPos) then begin

      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   end else begin

      factory.CheckName(name, constPos);
      CheckSpecialName(name);

      if FTok.TestDelete(ttCOLON) then begin

         typ:=ReadType('', tcConstant);
         if typ.AsFuncSymbol<>nil then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InvalidConstType, [typ.Caption]);

      end else typ:=nil;

      if not FTok.TestDelete(ttEQ) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

      constSym:=ReadConstSymbol(name, constPos, typ, factory);

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
   until not (    (UnitSection in [secProgram, secInterface, secImplementation])
              and (CurrentProg.Level=0)
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
      token:=FTok.TestAny([ttINTERFACE, ttIMPLEMENTATION, ttINITIALIZATION, ttFINALIZATION,
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
   typePos, endPos : TScriptPos;
   oldSymPos : TSymbolPosition; // Mark *where* the old declaration was
   typContext : TdwsSourceContext;
   attributesBag : ISymbolAttributesBag;
   genericParameters : IGenericParameters;
begin
   Result:=True;

   ReadAttributes(False);

   attributesBag:=BagPendingAttributes;

   if not FTok.TestDeleteNamePos(name, typePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   if FTok.TestDelete(ttLESS) then begin
      genericParameters := ReadGenericParametersDecl;
      if not FTok.TestDelete(ttGTR) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_GreaterExpected)
   end;

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

   typOld:=CurrentProg.Table.FindTypeSymbol(name, cvMagic);
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

      if genericParameters <> nil then
         typNew := ReadTypeGenericDecl(name, tcDeclaration, genericParameters)
      else typNew := ReadType(name, tcDeclaration);

      if typContext<>nil then
         typContext.ParentSym:=typNew;

      AttachBaggedAttributes(typNew, attributesBag);

      if typNew.Name<>'' then begin
         // typOld = typNew if a forwarded class declaration was overwritten
         if typOld <> typNew then begin
            CheckName(name, typePos);
            CheckSpecialName(name);
            if typNew.Name<>'' then
               CurrentProg.Table.AddSymbol(typNew);
         end  else begin
            // Handle overwriting forwards in Dictionary
            // Original symbol was a forward. Update symbol entry
            // If the type is in the SymbolDictionary (disabled dictionary would leave pointer nil),
            if Assigned(oldSymPos) and not (typOld.InheritsFrom(TClassSymbol) and TClassSymbol(typOld).IsPartial) then begin
               if FSymbolDictionary.FindSymbolUsage(typOld, suForward)=nil then
                  oldSymPos.SymbolUsages := [suForward]; // update old position to reflect that the type was forwarded
            end;
         end;

         // Add symbol position as being the type being declared (works for forwards too)
         if typNew.IsForwarded then
            RecordSymbolUse(typNew, typePos, [suForward])
         else RecordSymbolUse(typNew, typePos, [suDeclaration]);
      end;

      ReadSemiColon;
      endPos:=FTok.HotPos;

      typNew.DeprecatedMessage:=ReadDeprecatedMessage;
      if typNew.DeprecatedMessage<>'' then
         endPos:=FTok.HotPos;

   finally
      if coContextMap in FOptions then
         FSourceContextMap.CloseContext(endPos, ttName);
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
   compositeSym : TCompositeTypeSymbol;
   overloadFuncSym, existingFuncSym, forwardedSym : TFuncSymbol;
   forwardedSymForParams : TFuncSymbol;
   forwardedSymPos : TSymbolPosition;
   genericSymbol : TGenericSymbol;
   sourceContext : TdwsSourceContext;
   posArray : TScriptPosArray;
   isForward : Boolean;
begin
   Result:=nil;
   sym:=nil;
   isForward := False;

   funcKind:=cTokenToFuncKind[funcToken];
   funcPos:=hotPos;

   if not (pdoType in declOptions) then begin
      // Find existing symbol for function name (if any)
      if pdoAnonymous in declOptions then begin
         name:='';
      end else begin
         if not FTok.TestDeleteNamePos(name, funcPos) then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
            name:='';
         end else begin
            CheckSpecialName(name);
            sym:=CurrentProg.Table.FindSymbol(name, cvMagic);
         end;
      end;
   end else begin
      name:='';
   end;

   // Open context. Closed in ReadProcBody.
   if coContextMap in Options then begin
      FSourceContextMap.OpenContext(hotPos, nil, funcToken);
      sourceContext:=FSourceContextMap.Current;
   end else sourceContext:=nil;

   if sym is TGenericSymbol then begin
      genericSymbol := TGenericSymbol(sym);
      CheckGenericParameters(genericSymbol);
      sym := genericSymbol.GenericType;
   end else genericSymbol := nil;

   // name is the name of composite type -> this is a method implementation
   if sym is TCompositeTypeSymbol then begin

      compositeSym:=TCompositeTypeSymbol(sym);
      if compositeSym.IsPartial or (compositeSym.UnitSymbol=CurrentUnitSymbol) then begin

         if CurrentProg.Level<>0 then
            FMsgs.AddCompilerStop(hotPos, CPE_UnexpectedMethodImplementation);

         // Store reference to class in dictionary
         RecordSymbolUse(sym, funcPos, [suReference]);

         if genericSymbol <> nil then
            EnterGeneric(genericSymbol);
         try
            Result:=ReadMethodImpl(compositeSym, funcKind, pdoClassMethod in declOptions);
         finally
            if genericSymbol <> nil then
               LeaveGeneric;
         end;

      end;

   end;

   if Result=nil then begin

      // Read normal procedure/function declaration
      if (pdoClassMethod in declOptions) or (funcKind in [fkConstructor, fkDestructor, fkMethod]) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ImplClassNameExpected);

      forwardedSym:=nil;
      overloadFuncSym:=nil;
      existingFuncSym:=sym.AsFuncSymbol;
      if existingFuncSym<>nil then begin
         if existingFuncSym.IsOverloaded then
            overloadFuncSym:=existingFuncSym;
         if     existingFuncSym.IsForwarded
            and (CurrentUnitSymbol.HasSymbol(sym) or CurrentProg.Table.HasSymbol(sym)) then begin
            // There was already a (forward) declaration
            forwardedSym:=existingFuncSym;
         end;
      end;

      if (forwardedSym=nil) and (overloadFuncSym=nil) then
         CheckName(name, funcPos);

      if pdoType in declOptions then
         Result := TSourceFuncSymbol.Create('', funcKind, -1)
      else Result := TSourceFuncSymbol.Create(name, funcKind, FMainProg.NextStackLevel(CurrentProg.Level));
      try
         if funcToken=ttLAMBDA then
            Result.IsLambda:=True;

         // Don't add params to dictionary when function is forwarded. It is already declared.
         forwardedSymForParams:=forwardedSym;
         if forwardedSym<>nil then
            ReadParams(Result.HasParam, Result.AddParam, forwardedSym.Params, nil, posArray)
         else ReadParams(Result.HasParam, Result.AddParam, nil, expectedLambdaParams, posArray);

         if (funcToken<>ttLAMBDA) or FTok.Test(ttCOLON) then
            Result.Typ:=ReadFuncResultType(funcKind);

         if not (pdoAnonymous in declOptions) then begin

            if pdoType in declOptions then begin

               if FTok.TestDelete(ttOF) then begin
                  if FTok.TestDelete(ttOBJECT) then
                     FMsgs.AddCompilerHint(FTok.HotPos, CPH_OfObjectIsLegacy, hlPedantic)
                  else FMsgs.AddCompilerError(FTok.HotPos, CPE_OfObjectExpected);
               end;
               ReadProcCallQualifiers(result);

            end else begin

               if not FTok.TestDelete(ttSEMI) then
                  FMsgs.AddCompilerWarning(FTok.HotPos, CPE_SemiExpected);

               if overloadFuncSym<>nil then
                  forwardedSym:=FuncPerfectMatchOverload(Result);

               // handle function overloading
               if FTok.TestDelete(ttOVERLOAD) then begin

                  if FuncHasConflictingOverload(Result, forwardedSym) then
                     FMsgs.AddCompilerErrorFmt(hotPos, CPE_MatchingOverload, [name]);

                  Result.IsOverloaded:=True;
                  ReadSemiColon;

               end else if overloadFuncSym<>nil then begin

                  // nested funcs are allowed to overwrite overloads without errors
                  if Result.Level<=existingFuncSym.Level then begin
                     forwardedSym:=FuncPerfectMatchOverload(Result);
                     if forwardedSym=nil then begin
                        // no match, possible name conflict or fogotten overload keyword
                        FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustExplicitOverloads, [name]);
                        // keep compiling, mark overloaded
                        Result.IsOverloaded:=True;
                     end;
                  end;

               end;

               if Assigned(forwardedSym) then begin

                  // check forward symbol match

                  CompareFuncKinds(forwardedSym.Kind, Result.Kind);
                  CompareFuncSymbolParams(forwardedSym, Result);

               end else begin

                  // forward, external, export & helper declarations

                  if FTok.TestDelete(ttEXTERNAL) then begin
                     ReadExternalName(Result);
                     if Assigned(FExternalRoutinesManager) then
                        Result:=FExternalRoutinesManager.ConvertToMagicSymbol(Result);
                     Result.IsExternal:=True;
                     if FTok.TestDelete(ttPROPERTY) then begin
                        Result.IsProperty:=True;
                        if Result.Params.Count>0 then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ExternalPropertyNoArguments);
                     end;
                     ReadSemiColon;
                  end;

                  if UnitSection=secInterface then begin
                     // default to forward in interface section, except for external funcs
                     isForward := True;
                     if not Result.IsExternal then
                        Result.SetForwardedPos(funcPos);
                     if FTok.TestDelete(ttFORWARD) then begin
                        FMsgs.AddCompilerHint(FTok.HotPos, CPW_ForwardIsImplicit);
                        ReadSemiColon;
                     end;
                  end else begin
                     if FTok.TestDelete(ttFORWARD) then begin
                        isForward := True;
                        if Result.IsExternal then
                           FMsgs.AddCompilerHint(FTok.HotPos, CPW_ForwardIsMeaningless);
                        Result.SetForwardedPos(funcPos);
                        ReadSemiColon;
                     end;
                  end;

                  if FTok.TestDelete(ttEXPORT) then begin
                     Result.IsExport:=True;
                     if FTok.Test(ttStrVal) then begin
                        Result.ExternalName:=FTok.GetToken.AsString;
                        FTok.KillToken;
                     end;
                     ReadSemiColon;
                  end;

                  // helper anonymous declaration

                  if FTok.TestDelete(ttHELPER) then
                     AddProcHelper(Result);

               end;

               ReadProcCallQualifiers(result);

               if FTok.TestDelete(ttINLINE) then begin
                  Result.SetInline;
                  ReadSemiColon;
               end;

               if FTok.Test(ttDEPRECATED) then
                  Result.DeprecatedMessage:=ReadDeprecatedMessage;

               if Assigned(forwardedSym) then begin
                  // Get forwarded position in script. If compiled without symbols it will just return a nil
                  forwardedSymPos:=FSymbolDictionary.FindSymbolUsage(forwardedSym, suDeclaration);  // may be nil

                  // Adapt dictionary entry to reflect that it was a forward
                  // If the record is in the SymbolDictionary (disabled dictionary would leave pointer nil)
                  if Assigned(forwardedSymPos) then
                     forwardedSymPos.SymbolUsages:=[suDeclaration, suForward];  // update old position to reflect that the type was forwarded

                  if forwardedSymForParams<>nil then
                     AdaptParametersSymPos(forwardedSymForParams, forwardedSym, [suReference], posArray)
                  else AdaptParametersSymPos(Result, forwardedSym, [suReference], posArray);

                  SymbolDictionary.Remove(Result);

                  OrphanObject(Result);
                  Result := forwardedSym;
                  Result.ClearIsForwarded;

               end else begin

                  if forwardedSymForParams<>nil then
                     AdaptParametersSymPos(forwardedSymForParams, Result,
                                           [suDeclaration], posArray);

                  CurrentProg.Table.AddSymbol(Result);

               end;

               if Result.IsForwarded or Result.IsExternal then
                  FTok.SimulateToken(ttSEMI, FTok.HotPos);
            end;

         end;

         if Result.IsLambda then
            RecordSymbolUse(Result, funcPos, [suDeclaration, suImplementation, suReference])
         else if isForward then
            RecordSymbolUse(Result, funcPos, [suDeclaration, suForward])
         else if forwardedSym=nil then
            RecordSymbolUse(Result, funcPos, [suDeclaration, suImplementation])
         else RecordSymbolUse(Result, funcPos, [suImplementation]);
      except
         OrphanObject(Result);
         raise;
      end;
   end;

   if sourceContext<>nil then
      sourceContext.ParentSym:=Result;
end;

// AdaptParametersSymPos
//
procedure TdwsCompiler.AdaptParametersSymPos(guess, actual : TFuncSymbol; const useTypes : TSymbolUsages;
                                             var posArray : TScriptPosArray);
var
   i, d : Integer;
   guessSymPosList : TSymbolPositionList;
   guessParam : TSymbol;
   symPos : TSymbolPosition;
begin
   if not (coSymbolDictionary in Options) then Exit;

   d:=actual.Params.Count-Length(posArray);
   // params can have been mislocated in guess and must be reassigned to actual
   for i:=0 to actual.Params.Count-1 do begin
      // note: d can be negative in case of syntax errors
      // f.i. when more parameters were specified than were declared
      // so we can't use d as lower bound for the loop
      if i<d then continue;
      RecordSymbolUse(actual.Params[i], posArray[i-d], useTypes);
      guessParam:=guess.Params.FindLocal(actual.Params[i].Name);
      if guessParam<>nil then begin
         guessSymPosList:=SymbolDictionary.FindSymbolPosList(guessParam);
         if guessSymPosList<>nil then begin
            symPos:=guessSymPosList.Items[guessSymPosList.Count-1];
            if symPos.ScriptPos.SamePosAs(posArray[i-d]) then
               guessSymPosList.Delete(guessSymPosList.Count-1);
         end;
      end;
   end;
   d:=guess.Params.Count;
   if d>Length(posArray) then
      d:=Length(posArray);
   for i:=actual.Params.Count to d-1 do begin
      guessParam:=guess.Params.FindLocal(guess.Params[i].Name);
      if guessParam<>nil then begin
         guessSymPosList:=SymbolDictionary.FindSymbolPosList(guessParam);
         if guessSymPosList<>nil then begin
            symPos:=guessSymPosList.Items[guessSymPosList.Count-1];
            if symPos.ScriptPos.SamePosAs(posArray[i]) then
               guessSymPosList.Delete(guessSymPosList.Count-1);
         end;
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
   posArray : TScriptPosArray;
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
      ReadParams(Result.HasParam, Result.AddParam, nil, nil, posArray);

      Result.Typ:=ReadFuncResultType(funcKind);
      ReadSemiColon;

      // Added as last step. OnExcept, won't need to be freed.
      RecordSymbolUse(Result, methPos, [suDeclaration]);
   except
      OrphanObject(Result);
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
   meth, defaultConstructor : TMethodSymbol;
   isReintroduced : Boolean;
   methPos: TScriptPos;
   qualifier : TTokenType;
   funcResult : TSourceMethodSymbol;
   bodyToken : TTokenType;
   posArray : TScriptPosArray;
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
      FMsgs.AddCompilerErrorFmt(methPos, CPE_ClassIsStaticNoInstances, [ownerSym.Name]);

   // Read declaration of method implementation
   funcResult:=TSourceMethodSymbol.Create(name, funcKind, ownerSym, aVisibility, isClassMethod);
   funcResult.DeclarationPos:=methPos;
   try
      if FGenericSymbol.Count > 0 then
         funcResult.InternalParams.InsertParent(0, FGenericSymbol.Peek.Parameters.List);

      ReadParams(funcResult.HasParam, funcResult.AddParam, nil, nil, posArray);

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

         if not ownerSym.AllowOverloads then
            FMsgs.AddCompilerError(hotPos, CPE_OverloadNotAllowed);
         if MethHasConflictingOverload(funcResult) then
            FMsgs.AddCompilerErrorFmt(hotPos, CPE_MatchingOverload, [name]);

         funcResult.IsOverloaded:=True;
         ReadSemiColon;

      end else begin

         funcResult.SetOverlap(meth);
         if meth<>nil then begin
            if FTok.Test(ttOVERRIDE) then begin
               // this could actually be an override of an inherited method
               // and not just an overload of a local method
               // in that case 'overload' is optional (but recommand it)
               if meth.IsOverloaded then begin
                  FMsgs.AddCompilerHintFmt(hotPos, CPH_ShouldExplicitOverload, [name], hlStrict);
                  funcResult.IsOverloaded:=True;
               end else if meth.StructSymbol=ownerSym then begin
                  // name conflict or fogotten overload keyword
                  FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustExplicitOverloads, [name]);
                  // keep compiling, mark overloaded
                  funcResult.IsOverloaded:=True;
               end;
            end else if meth.StructSymbol=ownerSym then begin
               // name conflict or fogotten overload keyword
               FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustExplicitOverloads, [name]);
               // keep compiling, mark overloaded
               funcResult.IsOverloaded:=True;
            end else if MethPerfectMatchOverload(funcResult, False)<>nil then
               MemberSymbolWithNameAlreadyExists(sym, Ftok.HotPos)
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
                  else begin
                     RecordSymbolUse(meth, methPos, [suImplicit]);
                     if not meth.IsVirtual then
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
         else funcResult.SetIsStatic;
         ReadSemiColon;
      end;

      if FTok.TestDelete(ttEXTERNAL) then begin
         if not ownerSym.IsExternal then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_StructureIsNotExternal, [funcResult.QualifiedName]);
         ReadExternalName(funcResult);
         ReadSemiColon;
      end;

      ReadProcCallQualifiers(funcResult);

      if FTok.Test(ttDEPRECATED) then
         funcResult.DeprecatedMessage:=ReadDeprecatedMessage;

      if isReintroduced then
         FMsgs.AddCompilerWarningFmt(methPos, CPE_ReintroduceWarning, [name]);

      RecordSymbolUse(funcResult, methPos, [suDeclaration]);

   finally
      ownerSym.AddMethod(funcResult);
   end;

   bodyToken:=FTok.TestAny([ttBEGIN, ttREQUIRE, ttEMPTY]);
   if bodyToken<>ttNone then begin
      if funcResult.IsAbstract then
         FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplAbstract, [ownerSym.Name, funcResult.Name]);
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
   posArray : TScriptPosArray;
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
            ReadParams(tmpMeth.HasParam, tmpMeth.AddParam, Result.Params, nil, posArray);
            tmpMeth.Typ:=ReadFuncResultType(funcKind);
            if not FTok.TestDelete(ttSEMI) then
               FMsgs.AddCompilerWarning(FTok.HotPos, CPE_SemiExpected);
         end;
         if Result.IsOverloaded then begin
            overloadedMeth:=MethPerfectMatchOverload(tmpMeth, False);
            if overloadedMeth=nil then
               FMsgs.AddCompilerErrorFmt(methPos, CPE_NoMatchingOverloadDeclaration, [tmpMeth.Name])
            else begin
               AdaptParametersSymPos(Result, overloadedMeth, [suReference], posArray);
               Result:=overloadedMeth;
            end;
         end else if explicitParams then
            CompareFuncSymbolParams(Result, tmpMeth);
      finally
         OrphanObject(tmpMeth);
      end;
   end else begin
      // keep compiling a method that wasn't declared in class
      if not FTok.TestDelete(ttSEMI) then begin
         ReadParams(Result.HasParam, Result.AddParam, nil, nil, posArray);
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

// ReadDeprecatedMessage
//
function TdwsCompiler.ReadDeprecatedMessage(withSemiColon : Boolean = True) : String;
begin
   if FTok.TestDelete(ttDEPRECATED) then begin
      if FTok.Test(ttStrVal) then begin
         Result:=FTok.GetToken.AsString;
         FTok.KillToken;
      end;
      if Result='' then
         Result:=MSG_DeprecatedEmptyMsg;
      if withSemiColon then
         ReadSemiColon;
   end else Result:='';
end;

// WarnDeprecatedFunc
//
procedure TdwsCompiler.WarnDeprecatedFunc(funcExpr : TFuncExprBase);
var
   funcSym : TFuncSymbol;
begin
   funcSym:=funcExpr.FuncSym;
   if funcSym.IsDeprecated then
      WarnDeprecatedSymbol(funcExpr.ScriptPos, funcSym, funcSym.DeprecatedMessage);
end;

// WarnDeprecatedType
//
procedure TdwsCompiler.WarnDeprecatedType(const scriptPos : TScriptPos; typeSymbol : TTypeSymbol);
begin
   if (typeSymbol<>nil) and (typeSymbol.DeprecatedMessage<>'') then
      WarnDeprecatedSymbol(scriptPos, typeSymbol, typeSymbol.DeprecatedMessage);
end;

// WarnDeprecatedSymbol
//
procedure TdwsCompiler.WarnDeprecatedSymbol(const scriptPos : TScriptPos; sym : TSymbol;
                                            const deprecatedMessage : String);
begin
   if deprecatedMessage<>MSG_DeprecatedEmptyMsg then
      FMsgs.AddCompilerWarningFmt(scriptPos, CPW_DeprecatedWithMessage,
                                  [sym.Name, deprecatedMessage])
   else FMsgs.AddCompilerWarningFmt(scriptPos, CPW_Deprecated, [sym.Name]);
end;

// ReadProcBody
//
procedure TdwsCompiler.ReadProcBody(funcSymbol : TFuncSymbol);
var
   oldprog : TdwsProgram;
   proc : TdwsProcedure;
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
      oldprog:=CurrentProg;
      proc:=TdwsProcedure.Create(CurrentProg);
      CurrentProg:=proc;
      try
         FMainProg.Compiler := Self;
         proc.AssignTo(funcSymbol);

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

         if not funcSymbol.IsLambda then begin
            // Read local variable, constant & proc declarations
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
                     ttVAR :
                        ReadVarDecl(FStandardDataSymbolFactory, CurrentProg.InitExpr);
                     ttCONST : begin
                        ReadConstDecl(FStandardDataSymbolFactory);
                     end;
                  else
                     Break;
                  end;

                  ReadSemiColon;

               until FTok.Test(ttBEGIN);
            end;
         end;

         if coContextMap in FOptions then
            FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttBEGIN);
         try
            // Set the current context's LocalTable to be the table of the new procedure
            if coContextMap in FOptions then
               FSourceContextMap.Current.LocalTable:=CurrentProg.Table;

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
               CurrentProg.Expr:=progExpr.Optimize(FCompilerContext);
            end else CurrentProg.Expr:=progExpr;

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
         CurrentProg := oldprog;
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

   proc:=TdwsProcedure.Create(CurrentProg);
   proc.SetBeginPos(funcSymbol.SourcePosition);
   proc.AssignTo(funcSymbol);
   proc.Expr:=TNullExpr.Create(funcSymbol.SourcePosition);

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
      testExpr:=ReadExpr(FCompilerContext.TypBoolean);
      try
         if not testExpr.IsOfType(FCompilerContext.TypBoolean) then
            FMsgs.AddCompilerError(hotPos, CPE_BooleanExpected);
         if Optimize then
            testExpr:=testExpr.OptimizeToTypedExpr(FCompilerContext, hotPos);
         if testExpr.IsConstant then
            FMsgs.AddCompilerWarning(hotPos, CPW_ConstantCondition);

         testLength:=(NativeUInt(FTok.PosPtr)-NativeUInt(testStart)) div SizeOf(Char);
         if FTok.TestDelete(ttCOLON) then begin
            msgExpr:=ReadExpr;
            if not msgExpr.IsOfType(FCompilerContext.TypString) then
               FMsgs.AddCompilerError(hotPos, CPE_StringExpected);
            if Optimize then
               msgExpr:=msgExpr.OptimizeToTypedExpr(FCompilerContext, hotPos);
         end else begin
            SetString(msg, testStart, testLength);
            msg:=Trim(msg);
            if (msg<>'') and (msg[Length(msg)]=';') then
               SetLength(msg, Length(msg)-1);
            msgExpr := FUnifiedConstants.CreateString(msg);
         end;

         ReadSemiColon(True);

         srcCond:=TSourceCondition.Create(hotPos, testExpr, msgExpr);
         conditions.AddCondition(srcCond);
         funcSymbol.AddCondition(condsSymClass.Create(hotPos, srcCond, srcCond));
      except
         OrphanAndNil(testExpr);
         OrphanAndNil(msgExpr);
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
   funcSym:=symbol.AsFuncSymbol;
   if (funcSym<>nil) and (not symbol.IsType) then begin
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
   expectedNbParams : Integer;
begin
   opPos:=FTok.HotPos;
   tt:=FTok.TestDeleteAny([ttPLUS, ttMINUS, ttTIMES, ttDIVIDE, ttMOD, ttDIV,
                           ttOR, ttAND, ttXOR,
                           ttIN, ttIMPLIES, ttIMPLICIT,
                           ttSHL, ttSHR, ttSAR,
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

      case tt of
         ttIMPLICIT : expectedNbParams := 1;
      else
         expectedNbParams := 2;
      end;
      if Length(Result.Params) <> expectedNbParams then begin
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadNumberOfParameters,
                                   [expectedNbParams, Length(Result.Params)]);
         Result.Token := ttNone;  // nerf the operator
      end;

      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      Result.Typ:=ReadType('', tcOperand);

      if not FTok.TestDelete(ttUSES) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UsesExpected);

      usesSym:=nil;
      fromTable:=CurrentProg.Table;
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
         usesSym:=sym.AsFuncSymbol;
         if (usesSym=nil) or sym.IsType then
            FMsgs.AddCompilerError(usesPos, CPE_FunctionMethodExpected);
      end;

      if usesSym<>nil then begin

         if usesSym.IsOverloaded then
            FindOverloadedFunc(usesSym, usesName, fromTable, Result);

         RecordSymbolUse(usesSym, usesPos, [suReference]);

         if (usesSym.typ=nil) or not usesSym.Typ.IsOfType(Result.Typ) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadResultType, [Result.Typ.Caption])
         else if usesSym.Params.Count <> expectedNbParams then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadNumberOfParameters, [expectedNbParams, usesSym.Params.Count])
         else if not usesSym.Params[0].Typ.IsOfType(Result.Params[0]) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType, [0, Result.Params[0].Caption, usesSym.Params[0].Typ.Caption])
         else if usesSym.Params[0].ClassType=TVarParamSymbol then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [0])
         else if (expectedNbParams > 1) and not usesSym.Params[1].Typ.IsOfType(Result.Params[1]) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_BadParameterType, [1, Result.Params[1].Caption, usesSym.Params[1].Typ.Caption])
         else if (expectedNbParams > 1) and (usesSym.Params[1].ClassType=TVarParamSymbol) then
            FMsgs.AddCompilerErrorFmt(usesPos, CPE_VarParameterForbidden, [1])
         else Result.UsesSym:=usesSym;
      end;
   except
      OrphanObject(Result);
      raise;
   end;

   CurrentProg.Table.AddSymbol(Result);
   if (Result.Token<>ttNone) and (Result.UsesSym<>nil) then begin
      if CurrentProg.Table.HasSameLocalOperator(Result) then
         FMsgs.AddCompilerError(opPos, CPE_OverloadAlreadyExists);
   end;
end;

// UnexpectedBlockTokenError
//
procedure TdwsCompiler.UnexpectedBlockTokenError(const endTokens : TTokenTypes);
var
   msg, found : String;
   foundTyp : TTokenType;
begin
   msg:=TokenTypesToString(endTokens);
   if FTok.HasTokens then begin
      foundTyp:=FTok.GetToken.FTyp;
      found:=cTokenStrings[foundTyp];
   end else foundTyp:=ttNone;
   if msg='' then
      if found='' then
         msg:=CPE_SemiExpected
      else msg:=Format(CPE_Unexpected_X, [found])
   else case foundTyp of
      ttNAME :
         msg:=Format(CPE_X_ExpectedBut_Y_Found, [msg, 'identifier']);
      ttNone :
         msg:=msg+CPE_XxxExpected;
   else
      msg:=Format(CPE_X_ExpectedBut_Y_Found, [msg, '"'+found+'"']);
   end;
   FMsgs.AddCompilerStop(FTok.HotPos, msg);
end;

// ReadBlocks
//
function TdwsCompiler.ReadBlocks(const endTokens : TTokenTypes; var finalToken : TTokenType) : TProgramExpr;
var
   stmt : TProgramExpr;
   token : TToken;
   closePos : TScriptPos; // Position at which the ending token was found (for context)
   blockExpr : TBlockExpr;
   reach : TReachStatus;
   action : TdwsStatementAction;
begin
   // Read a block of instructions enclosed in "begin" and "end"
   Result:=nil;
   reach:=rsReachable;
   blockExpr:=TBlockExpr.Create(FCompilerContext, FTok.HotPos);
   try
      if coContextMap in FOptions then begin
         FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttBEGIN);
         closePos.Clear;
      end;

      CurrentProg.EnterSubTable(blockExpr.Table);
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
            stmt:=ReadStatement(action, blockExpr);

            if Assigned(stmt) then begin
               blockExpr.AddStatement(stmt);
               if (reach=rsReachable) and stmt.InterruptsFlow then
                  reach:=rsUnReachable;
            end;

            case action of
               saNoSemiColon : ;
               saNone : begin
                  if not FTok.TestDelete(ttSEMI) then begin
                     token:=FTok.GetToken;
                     if (token=nil) or (not (token.FTyp in EndTokens)) then
                        UnexpectedBlockTokenError(endTokens);
                  end;
               end;
            else
               Assert(False);
            end;

         until False;

         HintUnusedSymbols;

         if Optimize then begin
            Result:=blockExpr.Optimize(FCompilerContext);
            blockExpr:=nil;
         end else Result:=blockExpr;
      finally
         CurrentProg.LeaveSubTable;

         if coContextMap in FOptions then begin
            if blockExpr<>nil then
               FSourceContextMap.Current.LocalTable:=blockExpr.Table
            else if Result is TBlockExpr then
               FSourceContextMap.Current.LocalTable:=TBlockExpr(Result).Table;
            if not closePos.Defined then
               closePos:=FTok.CurrentPos; // means an error occured
            FSourceContextMap.CloseContext(closePos);
         end;
      end;
   except
      OrphanAndNil(blockExpr);
      raise;
   end;
end;

// ReadBlock
//
function TdwsCompiler.ReadBlock : TProgramExpr;
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
function TdwsCompiler.ReadInstr : TProgramExpr;
var
   token : TTokenType;
   locExpr : TProgramExpr;
   hotPos : TScriptPos;
   msgsCount : Integer;
   funcSym : TFuncSymbol;
begin
   if Assigned(FOnReadInstr) then begin
      Result:=FOnReadInstr(Self);
      if Result<>nil then Exit;
   end;

   // Decide which instruction to read
   case FTok.TestDeleteAny([ttIF, ttCASE, ttFOR, ttWHILE, ttREPEAT, ttBREAK,
                            ttEXIT, ttTRY, ttRAISE, ttCONTINUE, ttWITH]) of
      ttIF :
         Result := ReadIf;
      ttCASE : begin
         Result := ReadCase;
         if Optimize then
            Result := Result.Optimize(FCompilerContext);
      end;
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
         Result := TBreakExpr.Create(FTok.HotPos);
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
         Result := TContinueExpr.Create(FTok.HotPos);
      end;
      ttRAISE :
         Result := ReadRaise;
      ttWITH :
         Result := ReadWith;
   else
      // Try to read a function call, method call or an assignment
      if (FTok.TestAny([ttBLEFT, ttINHERITED, ttNEW])<>ttNone) or FTok.TestName then begin // !! TestName must be the last !!
         hotPos:=FTok.HotPos;
         msgsCount:=FMsgs.Count;
         if FTok.TestDelete(ttBLEFT) then // (X as TY)
            locExpr:=ReadSymbol(ReadBracket, True)
         else locExpr:=ReadName(True);
         if locExpr is TTypedExpr then begin
            if (FTok.TestAny([ttLESSLESS, ttGTRGTR])<>ttNone) then
               locExpr:=ReadExprMult(nil, TTypedExpr(locExpr));
         end;
         try
            token:=FTok.TestDeleteAny(cAssignmentTokens);
            if token<>ttNone then begin
               if not (locExpr is TDataExpr) then begin
                  FMsgs.AddCompilerError(hotPos, CPE_CantWriteToLeftSide);
                  OrphanAndNil(locExpr);
                  Result:=ReadExpr; // keep compiling
                  OrphanAndNil(Result);
               end else begin
                  if not TDataExpr(locExpr).IsWritable then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_CantWriteToLeftSide);
                  if locExpr is TVarExpr then
                     WarnForVarUsage(TVarExpr(locExpr), hotPos);
                  Result := ReadAssign(token, TDataExpr(locExpr));
               end;
            end else begin
               if locExpr is TDataExpr then begin
                  funcSym:=locExpr.Typ.AsFuncSymbol;
                  if funcSym<>nil then
                     locExpr:=ReadFunc(funcSym, locExpr as TDataExpr);
               end;

               if locExpr is TAssignExpr then
                  Result:=TAssignExpr(locExpr)
               else if    (locExpr is TFuncExprBase)
                       or (locExpr is TConnectorCallExpr) then begin
                  Result:=locExpr;
                  if locExpr.IsConstant then begin
                     if FMsgs.Count=msgsCount then   // avoid hint on calls with issues
                        FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
                  end;
               end else if locExpr is TConnectorWriteExpr then
                  Result:=locExpr
               else if locExpr is TDynamicArraySetExpr then
                  Result:=TDynamicArraySetExpr(locExpr)
               else if locExpr is TStringArraySetExpr then
                  Result:=TStringArraySetExpr(locExpr)
               else if locExpr is TArrayPseudoMethodExpr then
                  Result:=TArrayPseudoMethodExpr(locExpr)
               else if locExpr is TConstExpr then begin
                  OrphanAndNil(locExpr);
                  Result:=TNullExpr.Create(hotPos);
                  if FMsgs.Count=msgsCount then   // avoid hint on expression with issues
                     FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
               end else if locExpr is TNullExpr then begin
                  Result:=TNullExpr(locExpr);
                  locExpr:=nil;
               end else if (FPendingSetterValueExpr<>nil) and (locExpr is TTypedExpr) then begin
                  Result:=TNoResultWrapperExpr.Create(hotPos, TTypedExpr(locExpr));
                  locExpr:=nil;
               end else if locExpr is TProgramExpr then begin
                  Result:=locExpr;
                  locExpr:=nil;
               end else begin
                  Result:=nil;
                  FMsgs.AddCompilerStop(hotPos, CPE_InvalidInstruction)
               end;
            end;
         except
            OrphanAndNil(locExpr);
            raise;
         end;
      end else begin
         Result := TNullExpr.Create(FTok.HotPos);
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
   varExpr : TTypedExpr;
begin
   Result:=nil;
   if not ((CurrentProg is TdwsProcedure) and (TdwsProcedure(CurrentProg).Func is TMethodSymbol)) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedOnlyAllowedInMethods);

   methSym:=TMethodSymbol(TdwsProcedure(CurrentProg).Func);

   if not FTok.TestDeleteNamePos(name, namePos) then begin

      sym:=methSym.ParentMeth;
      if not methSym.IsOverride then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);

   end else begin

      compositeSym:=methSym.StructSymbol;
      if compositeSym.ClassType=THelperSymbol then begin
         sym:=THelperSymbol(compositeSym).ForType.UnAliasedType;
         if sym is TArraySymbol then begin
            if sym is TDynamicArraySymbol then
               varExpr:=GetVarExpr(namePos, methSym.SelfSym)
            else varExpr:=GetConstByRefParamExpr(methSym.SelfSym as TConstByRefParamSymbol);
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

      if sym is TMethodSymbol then begin

         methSym:=TMethodSymbol(sym);

         if name='' then
            RecordSymbolUse(methSym, FTok.HotPos, [suReference, suImplicit])
         else RecordSymbolUse(methSym, FTok.HotPos, [suReference]);

         if methSym.IsAbstract then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_AbstractMethodUsage);

         if methSym.IsOverloaded then
            Result:=ReadSelfMethOverloaded(methSym, isWrite, nil, [cfoForceStatic, cfoInheritedCall])
         else Result:=ReadSelfMethod(methSym, isWrite, nil, nil, [cfoForceStatic, cfoInheritedCall]);

      end else if sym is TPropertySymbol then begin

         RecordSymbolUseReference(sym, FTok.HotPos, isWrite);

         varExpr:=TVarExpr.CreateTyped(FCompilerContext, methSym.SelfSym);
         try
            Result:=ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
         except
            OrphanAndNil(varExpr);
            raise;
         end;

      end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);
   end else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InheritedMethodNotFound, [Name]);
end;

// ResolveUnitNameSpace
//
function TdwsCompiler.ResolveUnitNameSpace(const prefixPos : TScriptPos; unitPrefix : TUnitSymbol) : TUnitSymbol;
var
   dottedName, nextDottedName : String;
begin
   if not FTok.Test(ttDOT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);

   dottedName:=unitPrefix.Name;
   while FTok.TestDelete(ttDOT) do begin
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      nextDottedName:=dottedName+'.'+FTok.GetToken.AsString;
      if not unitPrefix.PossibleNameSpace(nextDottedName) then Break;
      dottedName:=nextDottedName;
      FTok.KillToken;
   end;

   Result:=unitPrefix.FindNameSpaceUnit(dottedName);
   if Result=nil then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownName, [dottedName])
   else begin
      if coSymbolDictionary in Options then
         SymbolDictionary.ReplaceSymbolAt(unitPrefix, Result, prefixPos);
   end;
end;


// ReadName
//
function TdwsCompiler.ReadName(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TProgramExpr;

   function ReadSpecialName(const name : String; const namePos : TScriptPos;
                            sk : TSpecialKeywordKind; var exprResult : TProgramExpr) : Boolean;
   begin
      FTok.KillToken;
      if (sk = skDefault) and not FTok.Test(ttBLEFT) then begin
         FTok.SimulateNameToken(namePos, name);
         FTok.TestName;
         Result := False;
      end else begin
         if not (coHintsDisabled in FOptions) then
            CheckSpecialNameCase(name, sk, namePos);
         exprResult := ReadSymbol(ReadSpecialFunction(namePos, sk), isWrite, expecting);
         Result := True;
      end;
   end;

var
   sym : TSymbol;
   nameToken : TToken;
   namePos : TScriptPos;
   varExpr : TTypedExpr;
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
      Result:=ReadNew(nil, False);
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
   sk:=IdentifySpecialName(nameToken.AsString);
   if sk<>skNone then begin
      if ReadSpecialName(nameToken.AsString, namePos, sk, Result) then Exit;
      nameToken := FTok.GetToken;
   end;

   // Find name in symboltable
   sym:=CurrentProg.Table.FindSymbol(nameToken.AsString, cvPrivate);
   if not Assigned(sym) then begin
      Result:=ReadSelfTypeHelper(nameToken, FTok.HotPos, expecting);
      if Result<>nil then
         Exit;
      if Assigned(FOnFindUnknownName) then
         sym:=FOnFindUnknownName(Self, nameToken.AsString);
      if Assigned(FOnReadUnknownName) then begin
         Result:=FOnReadUnknownName(Self);
         if Result<>nil then Exit;
      end;
      if sym=nil then begin
         sym:=CurrentProg.Table.FindSymbol(nameToken.AsString, cvMagic);
         if sym=nil then
            FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownName, [nameToken.AsString])
         else FMsgs.AddCompilerErrorFmt(namePos, CPE_MemberSymbolNotVisible, [nameToken.AsString]);
      end;
   end;

   if (sym<>nil) and not (coHintsDisabled in FOptions) then
      CheckMatchingDeclarationCase(nameToken.AsString, sym, namePos);

   FTok.KillToken;

   // Add the symbol usage to Dictionary
   RecordSymbolUseReference(sym, namePos, isWrite);

   if sym.ClassType = TGenericSymbol then
      sym := ReadSpecializedType(TGenericSymbol(sym));

   Result := nil;
   try
      baseType := sym.BaseType;

      if baseType<>nil then begin

         // Namespace prefix found
         if baseType.ClassType=TUnitSymbol then begin

            baseType:=ResolveUnitNameSpace(namePos, TUnitSymbol(baseType));

            namePos := FTok.HotPos;
            sym := TUnitSymbol(baseType).Table.FindLocal(FTok.GetToken.AsString);

            if not Assigned(sym) then
               FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownNameDotName,
                                        [baseType.Name, FTok.GetToken.AsString]);

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

      end else if symClassType=TVarParamSymbol then begin

         Result:=ReadSymbol(GetVarParamExpr(TVarParamSymbol(sym)), IsWrite, expecting);

      end else if symClassType=TConstByRefParamSymbol then begin

         Result:=ReadSymbol(GetConstByRefParamExpr(TConstByRefParamSymbol(sym)), IsWrite, expecting);

      end else if symClassType=TResourceStringSymbol then begin

         Result:=ReadResourceStringName(TResourceStringSymbol(sym), namePos);

      end else if sym.InheritsFrom(TConstSymbol) then begin

         Result:=ReadConstName(namePos, TConstSymbol(sym), IsWrite);

      end else if sym.InheritsFrom(TDataSymbol) then begin

         Result:=ReadDataSymbolName(namePos, TDataSymbol(sym), CurrentProg.Table, isWrite, expecting);

      end else if sym.ClassType=TExternalVarSymbol then begin

         Result := ReadSymbol(ReadExternalVar(TExternalVarSymbol(sym), IsWrite), IsWrite, expecting);
         Result := ReadSymbol(Result, IsWrite, expecting);

      // OOP related stuff

      end else if baseType is TStructuredTypeSymbol then begin

         WarnDeprecatedType(namePos, baseType);

         if baseType.ClassType=TClassSymbol then begin

            Result:=ReadClassSymbolName(TClassSymbol(baseType), isWrite, expecting);

         end else if baseType.ClassType=TInterfaceSymbol then begin

            Result:=ReadInterfaceSymbolName(TInterfaceSymbol(baseType), isWrite, expecting);

         end else begin

            Assert(baseType.ClassType=TRecordSymbol);
            if FTok.TestDelete(ttBLEFT) then
               Result:=ReadTypeCast(namePos, TTypeSymbol(sym))
            else Result:=ReadRecordSymbolName(TRecordSymbol(baseType), isWrite, expecting);

         end;

      end else if sym.InheritsFrom(TFieldSymbol) then begin

         if TdwsProcedure(CurrentProg).Func is TMethodSymbol then begin
            progMeth:=CurrentProg.ContextMethodSymbol;
            selfSym:=progMeth.SelfSym;
         end else begin
            selfSym:=TDataSymbol(CurrentProg.Table.FindSymbol(SYS_SELF, cvMagic, TDataSymbol));
         end;

         if (selfSym=nil) or (selfSym.Typ is TStructuredTypeMetaSymbol) then begin

            FMsgs.AddCompilerError(FTok.HotPos, CPE_ObjectReferenceExpected);
            fieldExpr:=TFieldExpr.Create(namePos, TFieldSymbol(sym), nil);

         end else begin

            fieldExpr:=ReadField(namePos, selfSym, TFieldSymbol(sym));

         end;
         Result:=ReadImplicitCall(fieldExpr, IsWrite, expecting);

      end else if sym.InheritsFrom(TPropertySymbol) then begin

         progMeth:=CurrentProg.ContextMethodSymbol;
         selfSym:=progMeth.SelfSym;

         if selfSym=nil then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);

         if selfSym.ClassType=TVarParamSymbol then
            varExpr:=GetVarParamExpr(progMeth.SelfSym as TVarParamSymbol)
         else varExpr:=GetVarExpr(namePos, progMeth.SelfSym);
         try
            propExpr:=ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
         except
            OrphanAndNil(varExpr);
            raise;
         end;

         Result:=ReadSymbol(propExpr, IsWrite, expecting);

      // Methods
      end else if sym.InheritsFrom(TMethodSymbol) then begin

         if TMethodSymbol(sym).IsOverloaded then
            funcExpr:=ReadSelfMethOverloaded(TMethodSymbol(sym), isWrite, expecting, [])
         else funcExpr:=ReadSelfMethod(TMethodSymbol(sym), isWrite, expecting, nil, []);
         Result:=ReadSymbol(funcExpr, IsWrite, expecting);

      // Functions/Procedures
      end else if sym.InheritsFrom(TFuncSymbol) then begin

         if TFuncSymbol(sym).IsOverloaded then
            funcExpr:=ReadFuncOverloaded(TFuncSymbol(sym), CurrentProg.Table, nil, expecting)
         else funcExpr:=ReadFunc(TFuncSymbol(sym), nil, expecting);
         Result:=ReadSymbol(funcExpr, IsWrite, expecting);

      // Enumeration type cast or type symbol
      end else if sym.InheritsFrom(TEnumerationSymbol) then begin

         Result:=ReadEnumerationSymbolName(namePos, TEnumerationSymbol(sym), expecting=FCompilerContext.TypAnyType)

      // helpers and generic type casts
      end else if sym.InheritsFrom(TTypeSymbol) then begin

         WarnDeprecatedType(namePos, TTypeSymbol(baseType));

         if FTok.TestDelete(ttBLEFT) then
            castExpr:=ReadTypeCast(namePos, TTypeSymbol(sym))
         else castExpr:=ReadTypeExpr(namePos, TTypeSymbol(sym), isWrite, expecting);
         Result:=ReadSymbol(castExpr, IsWrite, expecting);

      end else begin

         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownType, [sym.Name]);

      end;

   except
      OrphanAndNil(Result);
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
begin
   if FTok.TestDelete(ttBLEFT) then begin

      Result:=ReadTypeCast(elemPos, enumSym);
      Result:=ReadSymbol(Result, False, nil);

   end else if FTok.TestDelete(ttDOT) then begin

      if not FTok.TestDeleteNamePos(name, elemPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      if FTok.TestDelete(ttBLEFT) then begin
         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
         elem:=nil;
      end else begin
         elem:=enumSym.Elements.FindLocal(name);
      end;
      if elem=nil then begin
         if UnicodeSameText(name, 'low') then
            Result := TConstIntExpr.Create(enumSym, enumSym.LowBound)
         else if UnicodeSameText(name, 'high') then
            Result := TConstIntExpr.Create(enumSym, enumSym.HighBound)
         else begin
            FMsgs.AddCompilerErrorFmt(elemPos, CPE_UnknownNameDotName, [enumSym.Name, name]);
            Result := TConstIntExpr.Create(enumSym, 0);
         end;
      end else begin
         RecordSymbolUseReference(elem, elemPos, False);
         Result:=ReadConstName(elemPos, elem as TElementSymbol, False);
      end;

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
      if baseType.IsExternalRooted then begin
         convExpr:=TConvExternalExpr.Create(FCompilerContext, operandExpr);
         convExpr.Typ:=baseType;
      end else begin
         convExpr:=TObjAsClassExpr.Create(FCompilerContext, namePos, operandExpr, baseType);
         if operandExpr<>nil then begin
            castedExprTyp:=operandExpr.Typ;
            if castedExprTyp<>FCompilerContext.TypNil then begin
               if    (not (castedExprTyp is TClassSymbol))
                  or (
                            (not TClassSymbol(castedExprTyp).IsOfType(baseType))
                        and (not baseType.IsOfType(castedExprTyp))
                     ) then begin
                  IncompatibleTypes(namePos, CPE_IncompatibleTypes, castedExprTyp, baseType);
               end;
            end;
         end;
      end;
      if not (FTok.TestDelete(ttBRIGHT)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackRightExpected);
      Result:=ReadSymbol(convExpr, IsWrite, expecting);

   end else begin

      constExpr:=TConstExpr.Create(baseType.MetaSymbol, Int64(baseType));
      Result:=ReadSymbol(constExpr, IsWrite, expecting);

   end;
end;

// ReadInterfaceSymbolName
//
function TdwsCompiler.ReadInterfaceSymbolName(baseType : TInterfaceSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   constExpr : TTypedExpr;
begin
   constExpr:=TConstExpr.Create(baseType, Int64(baseType));
   Result:=ReadSymbol(constExpr, IsWrite, expecting);
end;

// ReadRecordSymbolName
//
function TdwsCompiler.ReadRecordSymbolName(baseType : TRecordSymbol; isWrite : Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   constExpr : TTypedExpr;
begin
   constExpr:=TConstExpr.Create(baseType.MetaSymbol, Int64(baseType));
   Result:=ReadSymbol(constExpr, IsWrite, expecting);
end;

// ReadConstName
//
function TdwsCompiler.ReadConstName(const constPos : TScriptPos; constSym : TConstSymbol; IsWrite: Boolean) : TProgramExpr;
var
   typ : TTypeSymbol;
begin
   if constSym.IsDeprecated then
      WarnDeprecatedSymbol(constPos, constSym, constSym.DeprecatedMessage);
   typ:=constSym.Typ;
   Result:=ReadSymbol(TConstExpr.CreateTyped(FCompilerContext, typ, constSym), IsWrite)
end;

// ReadDataSymbolName
//
function TdwsCompiler.ReadDataSymbolName(const aScriptPos: TScriptPos; dataSym : TDataSymbol; fromTable : TSymbolTable;
                                         isWrite: Boolean; expecting : TTypeSymbol) : TProgramExpr;
var
   varExpr : TVarExpr;
begin
   varExpr:=GetVarExpr(aScriptPos, dataSym);
   Result:=ReadImplicitCall(varExpr, isWrite, expecting);
end;

// ReadImplicitCall
//
function TdwsCompiler.ReadImplicitCall(codeExpr : TTypedExpr; isWrite: Boolean;
                                       expecting : TTypeSymbol) : TProgramExpr;
var
   codeExprTyp : TTypeSymbol;
   funcSym : TFuncSymbol;
begin
   if codeExpr=nil then Exit(nil);
   codeExprTyp:=codeExpr.Typ;
   if codeExprTyp=nil then Exit(nil);

   funcSym:=codeExprTyp.AsFuncSymbol;
   if funcSym<>nil then begin
      if     FTok.Test(ttASSIGN)
         or  (    (expecting<>nil)
              and codeExprTyp.IsOfType(expecting)
              and not FTok.Test(ttBLEFT)) then
         Result:=codeExpr
      else begin
         Assert(not funcSym.IsOverloaded);
         // Result:=ReadFuncOverloaded(funcSym, fromTable, varExpr, expecting)
         Result:=ReadFunc(funcSym, codeExpr, expecting);
      end;
   end else Result:=codeExpr;

   Result:=ReadSymbol(Result, isWrite, expecting);
end;

// ReadResourceStringName
//
function TdwsCompiler.ReadResourceStringName(resSym : TResourceStringSymbol; const namePos : TScriptPos) : TResourceStringExpr;
begin
   RecordSymbolUse(resSym, namePos, [suReference, suRead]);
   Result:=TResourceStringExpr.Create(FCompilerContext, namePos, resSym);
end;

// ReadNameOld
//
function TdwsCompiler.ReadNameOld(IsWrite: Boolean): TTypedExpr;
var
   sym : TDataSymbol;
   oldExpr : TProgramExpr;
   expr : TTypedExpr;
   initExpr : TProgramExpr;
   varExpr : TVarExpr;
begin
   oldExpr:=ReadName(IsWrite);
   if (not (oldExpr is TTypedExpr)) or (TTypedExpr(oldExpr).Typ=nil) then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionOrValueExpected);
      // keep going
      OrphanAndNil(oldExpr);
      expr := TConstExpr.CreateNull(FCompilerContext.TypVariant);
   end else expr := TTypedExpr(oldExpr);

   sym:=TDataSymbol.Create('old$'+IntToStr(FSourcePostConditionsIndex), expr.Typ);
   Inc(FSourcePostConditionsIndex);
   CurrentProg.Table.AddSymbol(sym);
   varExpr:=GetVarExpr(FTok.HotPos, sym);
   initExpr:=CreateAssign(FTok.HotPos, ttASSIGN, varExpr, expr);
   CurrentProg.InitExpr.AddStatement(initExpr);

   Result:=GetVarExpr(FTok.HotPos, sym);
end;

// ReadNameInherited
//
function TdwsCompiler.ReadNameInherited(IsWrite: Boolean): TProgramExpr;
begin
   // Name with inherited
   Result := ReadInherited(IsWrite);
   Result := ReadSymbol(Result, IsWrite);
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
      varExpr:=GetSelfParamExpr(scriptPos, selfSym);
   if fieldSym.StructSymbol.ClassType=TRecordSymbol then begin
      if varExpr.ClassType=TVarExpr then
         Result:=TRecordVarExpr.Create(scriptPos, TVarExpr(varExpr), fieldSym)
      else Result:=TRecordExpr.Create(scriptPos, (varExpr as TDataExpr), fieldSym)
   end else if varExpr is TObjectVarExpr then
      Result:=TFieldVarExpr.Create(FTok.HotPos, fieldSym, varExpr)
   else Result:=TFieldExpr.Create(FTok.HotPos, fieldSym, varExpr);
   varExpr:=nil;
end;

// ReadPropertyProgExpr
//
function TdwsCompiler.ReadPropertyProgExpr(var expr : TProgramExpr; propertySym : TPropertySymbol;
                                           isWrite : Boolean) : TProgramExpr;
var
   typedExpr : TTypedExpr;
begin
   try
      typedExpr:=(expr as TTypedExpr);
      Result:=ReadPropertyExpr(typedExpr, propertySym, isWrite);
   finally
      expr:=typedExpr;
   end;
end;

// ReadPropertyExpr
//
function TdwsCompiler.ReadPropertyExpr(var expr : TTypedExpr; propertySym : TPropertySymbol;
                                       isWrite : Boolean) : TProgramExpr;
begin
   if propertySym.IsDeprecated then
      WarnDeprecatedSymbol(FTok.HotPos, propertySym, propertySym.DeprecatedMessage);
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
   aPos:=FTok.HotPos;

   sym := propertySym.ReadSym;

   // No ReadSym
   if sym = nil then begin

      if propertySym.WriteSym=nil then
         FMsgs.AddCompilerError(aPos, CPE_CantReadProperty)
      else FMsgs.AddCompilerError(aPos, CPE_WriteOnlyProperty);
      OrphanAndNil(expr);
      Exit(CreateTypedDefault(propertySym.Typ));

   end;

   RecordSymbolUseImplicitReference(sym, aPos, False);

   if sym is TFieldSymbol then begin

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
         typedExprList.Orphan(FCompilerContext);
      end;

   end else if sym is TClassVarSymbol then begin

      OrphanAndNil(expr);
      Result:=GetVarExpr(aPos, TClassVarSymbol(sym));

   end else begin

      Assert(sym is TConstSymbol);

      OrphanAndNil(expr);
      Result:=TConstExpr.CreateTyped(FCompilerContext, sym.Typ, TConstSymbol(sym));

   end;
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
   aPos:=FTok.HotPos;

   typedExprList:=TTypedExprList.Create;
   try
      if propertySym.HasArrayIndices then begin
         typedExprList.Table:=propertySym.ArrayIndices;
         ReadArguments(typedExprList.AddExpr, ttALEFT, ttARIGHT,
                       argPosArray, typedExprList.ExpectedArg);
      end;

      tokenType:=FTok.TestDeleteAny(cAssignmentTokens);

      // implicit assign for setter write expressions
      if     (tokenType=ttNone) and (FPendingSetterValueExpr<>nil)
         and FTok.Test(ttBRIGHT) then
         tokenType:=ttASSIGN;

      if tokenType<>ttNone then begin

         if tokenType<>ttASSIGN then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_CantUseCombinedAssignmentOnProperty);

         sym:=propertySym.WriteSym;

         if sym is TFieldSymbol then begin

            // WriteSym is a Field
            RecordSymbolUseImplicitReference(sym, aPos, True);
            if Expr.Typ is TClassOfSymbol then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ObjectReferenceExpected);
            fieldExpr:=ReadField(aPos, nil, TFieldSymbol(sym), expr);
            Result:=ReadAssign(ttASSIGN, fieldExpr);

         end else if sym is TClassVarSymbol then begin

            // WriteSym is a class var
            RecordSymbolUseImplicitReference(sym, aPos, True);
            OrphanAndNil(expr);
            fieldExpr:=GetVarExpr(aPos, TClassVarSymbol(sym));
            Result:=ReadAssign(ttASSIGN, fieldExpr);

         end else if sym is TMethodSymbol then begin

            // WriteSym is a Method
            // Convert an assignment to a function call f := x  -->  f(x)
            RecordSymbolUseImplicitReference(sym, aPos, False);
            Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, True);

         end else begin

            OrphanAndNil(expr);
            Result:=TErrorExpr.Create(aPos);
            FMsgs.AddCompilerError(aPos, CPE_ReadOnlyProperty)

         end;

      end else begin

         if (FTok.TestAny([ttDOT, ttALEFT])<>ttNone) or (propertySym.Typ.AsFuncSymbol<>nil) then begin

            sym:=propertySym.ReadSym;

            if sym is TMethodSymbol then begin

               Result:=ReadPropertyArrayAccessor(expr, propertySym, typedExprList, aPos, False);

            end else if sym is TFieldSymbol then begin

               if Expr.Typ is TClassOfSymbol then begin
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ObjectReferenceExpected);
                  OrphanAndNil(expr);
               end;
               Result:=TReadOnlyFieldExpr.Create(FTok.HotPos, TFieldSymbol(sym), expr, propertySym.Typ);
               expr:=nil;

            end else if sym is TClassVarSymbol then begin

               OrphanAndNil(expr);
               Result:=GetVarExpr(aPos, TClassVarSymbol(sym));

            end else begin

               if not (sym is TConstSymbol) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);

               OrphanAndNil(expr);
               Result:=TConstExpr.CreateTyped(FCompilerContext, sym.Typ, TConstSymbol(sym));

            end;

         end else begin

            FMsgs.AddCompilerError(aPos, CPE_InvalidInstruction);
            // fake to keep going
            OrphanAndNil(expr);
            Result:=TConstExpr.Create(propertySym.Typ);

         end;

      end;

   finally
      typedExprList.Orphan(FCompilerContext);
   end;
end;

// ReadPropertyArrayAccessor
//
function TdwsCompiler.ReadPropertyArrayAccessor(var expr : TTypedExpr; propertySym : TPropertySymbol;
      typedExprList : TTypedExprList; const scriptPos : TScriptPos; isWrite : Boolean) : TFuncExprBase;
var
   i : Integer;
   sym : TMethodSymbol;
begin
   if isWrite then
      sym:=propertySym.WriteSym as TMethodSymbol
   else sym:=propertySym.ReadSym as TMethodSymbol;

   if expr.Typ is TStructuredTypeMetaSymbol then begin
      // Class properties
      if not sym.IsClassMethod then begin
         if isWrite then
            FMsgs.AddCompilerError(scriptPos, CPE_StaticPropertyWriteExpected)
         else FMsgs.AddCompilerError(scriptPos, CPE_StaticPropertyReadExpected);
      end;
      Result:=GetMethodExpr(sym, expr, rkClassOfRef, scriptPos, []);
   end else Result:=GetMethodExpr(sym, expr, rkObjRef, scriptPos, []);

   expr:=nil;
   try
      // Add array indices (if any)
      for i:=0 to typedExprList.Count-1 do
         Result.AddArg(typedExprList.Expr[i]);
      typedExprList.Clear;

      if Assigned(propertySym.IndexSym) then
         Result.AddArg(TConstExpr.CreateTyped(FCompilerContext, propertySym.IndexSym,
                                              propertySym.IndexValue));

      // Add right side of assignment
      if isWrite then
         Result.AddArg(ReadExpr(propertySym.Typ));

      TypeCheckArguments(FCompilerContext, Result, nil);
   except
      if Result <> nil then
         Result.Orphan(FCompilerContext);
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
   codeExpr : TTypedExpr;
   funcSym : TFuncSymbol;
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
                        Result:=ReadPropertyProgExpr(Result, defaultProperty, isWrite)
                     end else begin
                        FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NoDefaultProperty,
                                                 [TDataExpr(Result).Typ.Name]);
                     end;

                  end else begin

                     // Type "array"
                     if not (Result is TTypedExpr) then begin
                        OrphanAndNil(Result);
                     end;
                     if (baseType is TArraySymbol) and (Result is TDataExpr) then begin
                        Result := ReadSymbolArrayExpr(TDataExpr(Result))
                     end else if TTypedExpr(Result).IsOfType(FCompilerContext.TypString) and (Result is TDataExpr) then begin
                        FTok.KillToken;
                        Result := ReadStringArray(TDataExpr(Result), IsWrite);
                     end else if baseType is TAssociativeArraySymbol then begin
                        Result := ReadSymbolAssociativeArrayExpr(TDataExpr(Result))
                     end else if baseType is TConnectorSymbol then begin
                        Result := ReadConnectorArray('', TTypedExpr(Result),
                                                     TConnectorSymbol(baseType).ConnectorType, IsWrite)
                     end else begin
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_ArrayExpected);
                        SkipUntilToken(ttARIGHT);
                        FTok.TestDelete(ttARIGHT);
                     end;
                     if Optimize then
                        Result:=Result.Optimize(FCompilerContext);

                  end;

               end;
            end;

            ttBLEFT : begin

               baseType:=Result.BaseType;
               funcSym:=baseType.AsFuncSymbol;
               if funcSym<>nil then begin
                  codeExpr:=Result as TTypedExpr;
                  Result:=nil;
                  Result:=ReadFunc(funcSym, codeExpr);
               end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMethodExpected);

            end;

         end;

      until (Expr = Result);
   except
      OrphanAndNil(Result);
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
   indexExpr:=nil;

   if FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

   errCount:=FMsgs.Count;

   // There is at least one index expression
   try
      repeat
         hotPos:=FTok.HotPos;
         indexExpr := ReadExpr;
         if not (baseExpr.BaseType is TArraySymbol) then begin
            FMsgs.AddCompilerError(hotPos, RTE_TooManyIndices);
            OrphanAndNil(indexExpr);
            Continue;
         end else if indexExpr=nil then begin
            continue;
         end;

         baseType := TArraySymbol(baseExpr.BaseType);

         if    (indexExpr.Typ=nil)
            or not (   (indexExpr.Typ.UnAliasedType=baseType.IndexType.UnAliasedType)
                    or indexExpr.Typ.IsOfType(FCompilerContext.TypVariant)) then
            IncompatibleTypes(hotPos, CPE_ArrayIndexMismatch,
                              baseType.IndexType, indexExpr.Typ);

         if baseType is TStaticArraySymbol then begin

            arraySymbol:=TStaticArraySymbol(baseType);
            if arraySymbol is TOpenArraySymbol then begin

               newBaseExpr := TOpenArrayExpr.Create(FTok.HotPos, baseExpr, indexExpr, arraySymbol);
               indexExpr := nil;

            end else begin

               if arraySymbol.IndexType.IsOfType(FCompilerContext.TypBoolean) then begin

                  newBaseExpr:=TStaticArrayBoolExpr.Create(FTok.HotPos, baseExpr, indexExpr,
                                                           arraySymbol);
                  indexExpr := nil;

               end else begin

                  newBaseExpr:=TStaticArrayExpr.Create(FTok.HotPos, baseExpr, indexExpr,
                                                       arraySymbol);
                  if indexExpr.IsConstant and (FMsgs.Count=errCount) then begin
                     idx:=indexExpr.EvalAsInteger(FExec);
                     if idx<arraySymbol.LowBound then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, RTE_ArrayLowerBoundExceeded, [idx])
                     else if idx>arraySymbol.HighBound then
                        FMsgs.AddCompilerErrorFmt(FTok.HotPos, RTE_ArrayUpperBoundExceeded, [idx]);
                  end;
                  indexExpr := nil;

               end;
            end;

         end else begin

            Assert(baseType is TDynamicArraySymbol);

            if FTok.Test(ttCOMMA) then begin

               newBaseExpr:=TDynamicArrayExpr.Create(FTok.HotPos, baseExpr, indexExpr,
                                                     TDynamicArraySymbol(baseType));
               indexExpr:=nil;

            end else if FTok.TestDelete(ttARIGHT) then begin

               if FTok.TestDelete(ttASSIGN) then begin
                  hotPos:=FTok.HotPos;
                  valueExpr:=ReadExpr(baseType.Typ);
                  if valueExpr.Typ <> baseType.Typ then
                     FCompilerContext.WrapWithImplicitCast(baseType.Typ, hotPos, valueExpr);

                  if not baseType.Typ.IsCompatible(valueExpr.Typ) then begin
                     IncompatibleTypes(hotPos, CPE_AssignIncompatibleTypes,
                                       valueExpr.Typ, baseType.Typ);
                  end;

                  if baseType.Typ.Size=1 then
                     if baseExpr is TObjectVarExpr then
                        Result:=TDynamicArraySetVarExpr.Create(FCompilerContext, FTok.HotPos, baseExpr, indexExpr, valueExpr)
                     else Result:=TDynamicArraySetExpr.Create(FCompilerContext, FTok.HotPos, baseExpr, indexExpr, valueExpr)
                  else Result:=TDynamicArraySetDataExpr.Create(FCompilerContext, FTok.HotPos, baseExpr, indexExpr, valueExpr);
                  indexExpr:=nil;
               end else begin
                  if baseExpr is TObjectVarExpr then begin
                     Result:=TDynamicArrayVarExpr.Create(FTok.HotPos, baseExpr, indexExpr,
                                                         TDynamicArraySymbol(baseType));
                  end else begin
                     Result:=TDynamicArrayExpr.Create(FTok.HotPos, baseExpr, indexExpr,
                                                      TDynamicArraySymbol(baseType));
                  end;
                  indexExpr:=nil;
               end;
               Exit;

            end else begin

               OrphanAndNil(indexExpr);
               break;

            end;

         end;

         baseExpr := newBaseExpr;
         newBaseExpr := nil;
      until not FTok.TestDelete(ttCOMMA);
   except
      OrphanAndNil(indexExpr);
      OrphanAndNil(newBaseExpr);
      raise;
   end;

   Result:=baseExpr;

   if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_ArrayBracketRightExpected);
end;

// ReadSymbolAssociativeArrayExpr
//
function TdwsCompiler.ReadSymbolAssociativeArrayExpr(var baseExpr : TDataExpr) : TProgramExpr;
var
   baseType : TAssociativeArraySymbol;
   keyExpr, valueExpr : TTypedExpr;
   hotPos : TScriptPos;
begin
   FTok.KillToken;

   if FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

   baseType := (baseExpr.BaseType as TAssociativeArraySymbol);

   keyExpr:=nil;
   try
      hotPos := FTok.HotPos;
      keyExpr := ReadExpr;

      if keyExpr.Typ <> baseType.KeyType then
         FCompilerContext.WrapWithImplicitCast(baseType.KeyType, hotPos, keyExpr);

      if    (keyExpr.Typ=nil)
         or not (   (keyExpr.Typ.IsCompatible(baseType.KeyType))
                 or keyExpr.Typ.IsOfType(FCompilerContext.TypVariant)) then
         IncompatibleTypes(hotPos, CPE_ArrayIndexMismatch,
                           baseType.KeyType, keyExpr.Typ);

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);

      if FTok.TestDelete(ttASSIGN) then begin

         hotPos := FTok.HotPos;
         valueExpr := ReadExpr(baseType.Typ);
         if valueExpr.Typ <> baseType.Typ then
            FCompilerContext.WrapWithImplicitCast(baseType.Typ, hotPos, valueExpr);

         if not baseType.Typ.IsCompatible(valueExpr.Typ) then begin
            IncompatibleTypes(hotPos, CPE_AssignIncompatibleTypes,
                              valueExpr.Typ, baseType.Typ);
         end;

         Result := TAssociativeArraySetExpr.Create(FTok.HotPos, baseExpr, keyExpr, valueExpr);
         keyExpr := nil;
         Exit;

      end else begin

         baseExpr := TAssociativeArrayGetExpr.Create(FTok.HotPos, baseExpr, keyExpr, baseType);
         keyExpr := nil;

      end;

   except
      OrphanAndNil(keyExpr);
      raise;
   end;

   Result := baseExpr;
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

         if (baseType<>nil) and FHelperMemberNames.Contains(name) then begin
            helperExpr:=ReadTypeHelper(expr as TTypedExpr,
                                       name, namePos, expecting, isWrite, False);
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
            if member<>nil then begin
               memberClassType:=member.ClassType;
               if not (coHintsDisabled in FOptions) then
                  CheckMatchingDeclarationCase(name, member, namePos);
            end else memberClassType:=nil;

            RecordSymbolUseReference(member, namePos, isWrite);

            if (baseType is TRecordSymbol) and (Result is TFuncExpr) then
               TFuncExpr(Result).SetResultAddr(CurrentProg, nil);

            if member is TMethodSymbol then begin

               baseExpr:=(Result as TTypedExpr);
               Result:=nil;
               meth:=TMethodSymbol(member);
               if meth.IsOverloaded then
                  Result:=ReadMethOverloaded(meth, baseExpr, namePos, expecting)
               else Result:=ReadMethod(meth, baseExpr, namePos, expecting);

            end else if member is TFieldSymbol then begin

               Assert(Result is TTypedExpr);
               Result:=ReadField(FTok.HotPos, nil, TFieldSymbol(member), TTypedExpr(Result));

            end else if member is TPropertySymbol then begin

               Assert(Result is TTypedExpr);
               Result := ReadPropertyExpr(TTypedExpr(Result), TPropertySymbol(member), IsWrite)

            end else if memberClassType=TClassVarSymbol then begin

               OrphanAndNil(Result);
               Result:=ReadDataSymbolName(namePos, TDataSymbol(member), TStructuredTypeSymbol(member).Members, IsWrite, expecting);

            end else if memberClassType=TClassConstSymbol then begin

               OrphanAndNil(Result);
               Result:=ReadConstName(namePos, TConstSymbol(member), IsWrite);

            end else begin

               FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);

            end;

         // Meta (Class Of, Record Of)
         end else if baseType is TStructuredTypeMetaSymbol then begin

            member:=TStructuredTypeSymbol(baseType.Typ).Members.FindSymbolFromScope(Name, CurrentStruct);
            if member<>nil then begin
               memberClassType:=member.ClassType;
               if not (coHintsDisabled in FOptions) then
                  CheckMatchingDeclarationCase(name, member, namePos);
            end else memberClassType:=nil;

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

               expr := Result;
               Result := nil;
               Result := ReadPropertyProgExpr(expr, TPropertySymbol(member), IsWrite);

            end else if memberClassType=TClassVarSymbol then begin

               OrphanAndNil(Result);
               Result:=ReadDataSymbolName(namePos, TDataSymbol(member), TStructuredTypeSymbol(baseType.Typ).Members,
                                          IsWrite, expecting);

            end else if memberClassType=TClassConstSymbol then begin

               OrphanAndNil(Result);
               Result := ReadConstName(namePos, TConstSymbol(member), IsWrite);

            end else if member<>nil then begin

               FMsgs.AddCompilerStop(namePos, CPE_StaticMethodExpected);

            end else begin

               FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);

            end;

         // Array symbol
         end else if baseType is TArraySymbol then begin

            Result:=ReadArrayMethod(name, namePos, Result as TTypedExpr);

         // String symbol
         end else if baseType is TBaseStringSymbol then begin

            Result:=nil;
            Result:=ReadStringMethod(name, namePos, expr as TTypedExpr);

         // Associative Array symbol
         end else if baseType is TAssociativeArraySymbol then begin

            Result:=ReadAssociativeArrayMethod(name, namePos, Result as TTypedExpr);

         // "set of" symbol
         end else if baseType is TSetOfSymbol then begin

            Result:=nil;
            Result:=ReadSetOfMethod(name, namePos, expr as TTypedExpr);

         // enumeration element symbol
         end else if expr.Typ.UnAliasedTypeIs(TEnumerationSymbol) then begin

            Result:=nil;
            Result:=ReadElementMethod(name, namePos, expr as TTypedExpr);

         // Connector symbol
         end else if (baseType is TConnectorSymbol) and not (Result is TTypeReferenceExpr) then begin

            try
               Result:=ReadConnectorSym(Name, Result as TTypedExpr,
                                          TConnectorSymbol(baseType).ConnectorType, IsWrite)
            except
               Result:=nil;
               raise;
            end;

         end else FMsgs.AddCompilerStop(namePos, CPE_NoMemberExpected);

      end else begin

         OrphanAndNil(expr);
         expr:=nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      end;
   except
      OrphanAndNil(Result);
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
            Result := TFuncSimpleExpr.Create(FCompilerContext, FTok.HotPos, Sym.WriteFunc);
            Result.AddArg(ReadExpr);
         end else if (Sym.Typ is TClassSymbol) or (Sym.Typ is TClassOfSymbol) or (Sym.Typ is TConnectorSymbol) then begin
            if not Assigned(Sym.ReadFunc) then
               FMsgs.AddCompilerStop(FTok.HotPos,CPE_RightSideNeedsReturnType);
            Result := TFuncSimpleExpr.Create(FCompilerContext, FTok.HotPos, Sym.ReadFunc)
         end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
      end else if Assigned(Sym.ReadFunc) then
         Result := TFuncSimpleExpr.Create(FCompilerContext, FTok.HotPos, Sym.ReadFunc)
      else FMsgs.AddCompilerStop(FTok.HotPos,CPE_WriteOnlyProperty); // ??
      TypeCheckArguments(FCompilerContext, Result, nil);
   except
      if Result <> nil then
         Result.Orphan(FCompilerContext);
      raise;
   end;
end;

// ReadFor
//
function TdwsCompiler.ReadFor : TProgramExpr;
var
   forPos : TScriptPos;
   expr : TProgramExpr;
   loopVarExpr : TVarExpr;
   loopVarName : String;
   loopVarNamePos : TScriptPos;
begin
   forPos:=FTok.HotPos;

   if FTok.TestDelete(ttVAR) then begin

      expr:=nil;
      loopVarExpr:=nil;
      if not FTok.TestDeleteNamePos(loopVarName, loopVarNamePos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      CheckName(loopVarName, loopVarNamePos);

   end else begin

      expr:=ReadName(True);

      if expr is TFuncPtrExpr then
         expr:=TFuncPtrExpr(expr).Extract;
      if not (expr is TVarExpr) then begin
         OrphanAndNil(expr);
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_VariableExpected);
      end;

      loopVarExpr:=TVarExpr(expr);
      loopVarNamePos:=FTok.HotPos;

      WarnForVarUsage(loopVarExpr, loopVarNamePos);

   end;

   case FTok.TestDeleteAny([ttASSIGN, ttIN]) of
      ttASSIGN :
         Result:=ReadForTo(forPos, loopVarExpr, loopVarName, loopVarNamePos);
      ttIN :
         Result:=ReadForIn(forPos, loopVarExpr, loopVarName, loopVarNamePos);
   else
      OrphanAndNil(expr);
      Result:=nil;
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
   end;
end;

// ReadForTo
//
function TdwsCompiler.ReadForTo(const forPos : TScriptPos; loopVarExpr : TVarExpr;
                                const loopVarName : String; const loopVarNamePos : TScriptPos) : TNoResultExpr;
var
   iterVarExpr : TIntVarExpr;
   fromExpr, toExpr : TTypedExpr;
   forExprClass : TForExprClass;
   loopVarSymbol : TDataSymbol;
   loopBlockExpr : TBlockExpr;
begin
   fromExpr:=nil;
   toExpr:=nil;
   loopBlockExpr:=nil;
   try
      if loopVarExpr<>nil then begin
         if not loopVarExpr.IsOfType(FCompilerContext.TypInteger) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_IntegerExpected);
         if not (loopVarExpr is TIntVarExpr) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_FORLoopMustBeLocalVariable);
      end;

      fromExpr:=ReadExpr;
      if not (fromExpr.IsOfType(FCompilerContext.TypInteger) or fromExpr.IsOfType(FCompilerContext.TypVariant)) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_IntegerExpected);

      if loopVarExpr=nil then begin
         loopBlockExpr:=TBlockExpr.Create(FCompilerContext, forPos);
         loopVarSymbol:=TDataSymbol.Create(loopVarName, fromExpr.Typ);
         loopBlockExpr.Table.AddSymbol(loopVarSymbol);
         RecordSymbolUse(loopVarSymbol, loopVarNamePos, [suDeclaration, suReference, suWrite]);
         loopVarExpr:=GetVarExpr(loopVarNamePos, loopVarSymbol);
         CurrentProg.InitExpr.AddStatement(TAssignConstToIntegerVarExpr.CreateVal(FCompilerContext, loopVarNamePos, loopVarExpr, 0));
         loopVarExpr.IncRefCount;
      end;

      iterVarExpr:=TIntVarExpr(loopVarExpr);

      if FTok.TestDelete(ttTO) then
         forExprClass:=TForUpwardExpr
      else if FTok.TestDelete(ttDOWNTO) then
         forExprClass:=TForDownwardExpr
      else begin
         forExprClass:=TForUpwardExpr;
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ToOrDowntoExpected);
      end;

      if loopBlockExpr<>nil then begin
         if coContextMap in FOptions then
            FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttFOR);
         CurrentProg.EnterSubTable(loopBlockExpr.Table);
      end;
      try
         toExpr:=ReadExpr;
         if not (toExpr.IsOfType(FCompilerContext.TypInteger) or toExpr.IsOfType(FCompilerContext.TypVariant)) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);

         loopVarExpr:=nil;
         Result:=ReadForStep(forPos, forExprClass, iterVarExpr,
                             fromExpr, toExpr, nil);
      finally
         if loopBlockExpr<>nil then begin
            CurrentProg.LeaveSubTable;
            if coContextMap in Options then begin
               FSourceContextMap.Current.LocalTable:=loopBlockExpr.Table;
               FSourceContextMap.CloseContext(FTok.CurrentPos);
            end;
         end;
      end;

   except
      OrphanAndNil(loopBlockExpr);
      OrphanAndNil(fromExpr);
      OrphanAndNil(toExpr);
      OrphanAndNil(loopVarExpr);
      raise;
   end;

   if loopBlockExpr<>nil then begin
      loopBlockExpr.AddStatement(Result);
      Result:=loopBlockExpr;
   end;
end;

// ReadForStep
//
function TdwsCompiler.ReadForStep(const forPos : TScriptPos; forExprClass : TForExprClass;
                                  iterVarExpr : TIntVarExpr; var fromExpr, toExpr : TTypedExpr;
                                  loopFirstStatement : TProgramExpr) : TForExpr;
var
   stepExpr : TTypedExpr;
   stepPos : TScriptPos;
   iterBlockExpr : TBlockExpr;
begin
   try
      if FTok.Test(ttNAME) and ASCIISameText(FTok.GetToken.AsString, 'step') then begin
         FTok.KillToken;
         FTok.Test(ttNone);
         stepPos:=FTok.HotPos;
         stepExpr:=ReadExpr;
         if not stepExpr.IsOfType(FCompilerContext.TypInteger) then
            FMsgs.AddCompilerError(stepPos, CPE_IntegerExpected);
         if stepExpr.InheritsFrom(TConstIntExpr) and (TConstIntExpr(stepExpr).Value<=0) then
            FMsgs.AddCompilerErrorFmt(stepPos, RTE_ForLoopStepShouldBeStrictlyPositive,
                                      [TConstIntExpr(stepExpr).Value]);
         if forExprClass=TForUpwardExpr then
            forExprClass:=TForUpwardStepExpr
         else forExprClass:=TForDownwardStepExpr;
      end else begin
         stepExpr:=nil;
      end;

      iterBlockExpr:=nil;
      Result:=forExprClass.Create(forPos);
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
            iterBlockExpr:=TBlockExpr.Create(FCompilerContext, FTok.HotPos);
            iterBlockExpr.AddStatement(loopFirstStatement);
            loopFirstStatement:=nil;
            iterBlockExpr.AddStatement(ReadBlock);
            Result.DoExpr:=iterBlockExpr;
            iterBlockExpr:=nil;
         end else begin
            Result.DoExpr:=ReadBlock;
         end;

      except
         OrphanAndNil(iterBlockExpr);
         OrphanAndNil(stepExpr);
         if Result <> nil then
            Result.Orphan(FCompilerContext);
         raise;
      end;
      LeaveLoop;
   except
      if iterVarExpr <> nil then
         iterVarExpr.Orphan(FCompilerContext);
      OrphanAndNil(fromExpr);
      OrphanAndNil(toExpr);
      OrphanAndNil(loopFirstStatement);
      raise;
   end;
end;

// ReadForIn
//
function TdwsCompiler.ReadForIn(const forPos : TScriptPos; loopVarExpr : TVarExpr;
                                const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
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
   inExprAssignExpr : TAssignExpr;
   readArrayItemExpr : TProgramExpr;
   inExprVarExpr : TVarExpr;
   blockExpr : TBlockExpr;
begin
   forExprClass:=TForUpwardExpr;

   inPos:=FTok.HotPos;

   inExpr:=ReadName(False, FCompilerContext.TypAnyType);

   readArrayItemExpr:=nil;
   inExprAssignExpr:=nil;
   blockExpr:=nil;
   iterVarExpr:=nil;

   if (inExpr is TTypedExpr) and (inExpr.ClassType<>TTypeReferenceExpr) then begin

      if      inExpr.Typ.IsOfType(FCompilerContext.TypString)
         and (   (loopVarExpr=nil)
              or loopVarExpr.Typ.IsOfType(FCompilerContext.TypInteger)
              or loopVarExpr.Typ.IsOfType(FCompilerContext.TypString)) then begin

         Result:=ReadForInString(forPos, inExpr, loopVarExpr, loopVarName, loopVarNamePos);
         Exit;

      end else if inExpr.Typ is TConnectorSymbol then begin

         Result:=ReadForInConnector(forPos, inExpr as TTypedExpr, inPos, loopVarExpr, loopVarName, loopVarNamePos);
         Exit;

      end else begin

         // if inExpr is an expression, create a temporary variable
         // so it is evaluated only once
         if (inExpr.Typ<>nil) and not ((inExpr is TVarExpr) or (inExpr is TConstExpr)) then begin
            inExprVarSym:=TDataSymbol.Create('', inExpr.Typ);
            CurrentProg.Table.AddSymbol(inExprVarSym);
            inExprVarExpr:=GetVarExpr(inPos, inExprVarSym);
            inExprAssignExpr:=TAssignExpr.Create(FCompilerContext, FTok.HotPos, inExprVarExpr, TTypedExpr(inExpr));
            inExpr:=inExprVarExpr;
            inExpr.IncRefCount;
         end;

         if inExpr.Typ is TArraySymbol then begin


            arraySymbol:=TArraySymbol(inExpr.Typ);

            // create anonymous iter variables & its initialization expression
            iterVarSym:=TDataSymbol.Create('', arraySymbol.IndexType);
            CurrentProg.Table.AddSymbol(iterVarSym);
            iterVarExpr:=GetVarExpr(inPos, iterVarSym) as TIntVarExpr;
            initIterVarExpr:=TAssignConstToIntegerVarExpr.CreateVal(FCompilerContext, inPos, iterVarExpr, 0);
            CurrentProg.InitExpr.AddStatement(initIterVarExpr);

            fromExpr:=CreateArrayLow(inExpr, arraySymbol, False);
            toExpr:=CreateArrayHigh(inExpr, arraySymbol, False);

            blockExpr:=EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos, loopVarExpr, arraySymbol.Typ);

            iterVarExpr:=GetVarExpr(inPos, iterVarSym) as TIntVarExpr;
            readArrayItemExpr:=CreateAssign(FTok.HotPos, ttASSIGN, loopVarExpr,
                                            CreateArrayExpr(FTok.HotPos, (inExpr as TDataExpr), iterVarExpr));

            iterVarExpr:=GetVarExpr(inPos, iterVarSym) as TIntVarExpr;

         end else if inExpr.Typ is TSetOfSymbol then begin

            if inExprAssignExpr <> nil then
               inExprAssignExpr.Orphan(FCompilerContext);
            Result:=ReadForInSetOf(forPos, inExpr as TDataExpr, loopVarExpr, loopVarName, loopVarNamePos);
            Exit;

         end else begin

            if inExprAssignExpr <> nil then begin
               inExprAssignExpr.Orphan(FCompilerContext);
               inExprAssignExpr := nil;
            end;
            OrphanAndNil(loopVarExpr);
            if iterVarExpr <> nil then begin
               iterVarExpr.Orphan(FCompilerContext);
               iterVarExpr:=nil;
            end;
            fromExpr:=nil;
            toExpr:=nil;
            OrphanAndNil(inExpr);
            FMsgs.AddCompilerStop(inPos, CPE_ArrayExpected);

         end;

      end;

   end else begin

      enumSymbol:=nil;
      if inExpr is TTypeReferenceExpr then begin
         if inExpr.Typ.InheritsFrom(TEnumerationSymbol) then
            enumSymbol:=TEnumerationSymbol(inExpr.Typ);
      end;
      OrphanAndNil(inExpr);

      if enumSymbol=nil then begin
         FMsgs.AddCompilerError(inPos, CPE_EnumerationExpected);
         enumSymbol:=FCompilerContext.TypInteger;
      end;
      blockExpr:=EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos, loopVarExpr, enumSymbol);

      RecordSymbolUse(enumSymbol, inPos, [suReference]);

      if enumSymbol is TEnumerationSymbol then begin
         fromExpr := TConstIntExpr.Create(loopVarExpr.Typ, TEnumerationSymbol(enumSymbol).LowBound);
         toExpr := TConstIntExpr.Create(loopVarExpr.Typ, TEnumerationSymbol(enumSymbol).HighBound);
      end else begin
         fromExpr := TConstIntExpr.Create(loopVarExpr.Typ, 0);
         toExpr := TConstIntExpr.Create(loopVarExpr.Typ, 1);
      end;

      iterVarExpr:=(loopVarExpr as TIntVarExpr);

   end;

   if inExprAssignExpr<>nil then begin
      if blockExpr=nil then
         blockExpr:=TBlockExpr.Create(FCompilerContext, forPos);
      blockExpr.AddStatement(inExprAssignExpr);
   end;
   if blockExpr<>nil then begin
      if coContextMap in FOptions then
         FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttFOR);
      CurrentProg.EnterSubTable(blockExpr.Table);
   end;
   Result:=blockExpr;
   try
      try
         Result:=ReadForStep(forPos, forExprClass, iterVarExpr,
                             fromExpr, toExpr, readArrayItemExpr);
      except
         OrphanAndNil(blockExpr);
         raise;
      end;
      if Optimize then
         Result:=Result.Optimize(FCompilerContext);
   finally
      if blockExpr<>nil then begin
         CurrentProg.LeaveSubTable;
         blockExpr.AddStatement(Result);
         if Optimize then
            Result:=blockExpr.Optimize(FCompilerContext)
         else Result:=blockExpr;
         if coContextMap in FOptions then begin
            if blockExpr is TBlockExpr then
               FSourceContextMap.Current.LocalTable:=TBlockExpr(blockExpr).Table;
            FSourceContextMap.CloseContext(FTok.CurrentPos);
         end;
      end;
   end;
end;

// ReadForInString
//
function TdwsCompiler.ReadForInString(const forPos : TScriptPos; inExpr : TProgramExpr;
      loopVarExpr : TVarExpr; const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
var
   blockExpr : TBlockExpr;
   forInExpr : TForInStrExpr;
begin
   if not FTok.TestDelete(ttDO) then begin
      OrphanAndNil(inExpr);
      OrphanAndNil(loopVarExpr);
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);
   end;

   if loopVarExpr=nil then begin

      blockExpr:=EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos, loopVarExpr, FCompilerContext.TypString);

      forInExpr:=TForCharInStrExpr.Create(FCompilerContext, forPos, loopVarExpr as TStrVarExpr,
                                          TTypedExpr(inExpr));
      CurrentProg.EnterSubTable(blockExpr.Table);
      EnterLoop(forInExpr);
      try
         MarkLoopExitable(leBreak);
         forInExpr.DoExpr:=ReadBlock;
      finally
         LeaveLoop;
         CurrentProg.LeaveSubTable;
         blockExpr.AddStatement(forInExpr);
         Result:=blockExpr;
      end;

   end else begin

      if loopVarExpr.Typ.IsOfType(FCompilerContext.TypInteger) then
         forInExpr:=TForCharCodeInStrExpr.Create(FCompilerContext, forPos, loopVarExpr as TIntVarExpr,
                                                 TTypedExpr(inExpr))
      else forInExpr:=TForCharInStrExpr.Create(FCompilerContext, forPos, loopVarExpr as TStrVarExpr,
                                               TTypedExpr(inExpr));
      EnterLoop(forInExpr);
      try
         MarkLoopExitable(leBreak);
         forInExpr.DoExpr:=ReadBlock;
      finally
         LeaveLoop;
      end;
      Result:=forInExpr;

   end;
end;

// ReadForInSetOf
//
function TdwsCompiler.ReadForInSetOf(const forPos : TScriptPos; inExpr : TDataExpr; loopVarExpr : TVarExpr;
                                     const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
var
   setOfSymbol : TSetOfSymbol;
   elementTyp : TTypeSymbol;
   blockExpr : TBlockExpr;
   doBlock : TProgramExpr;
   loopVarInSetExpr : TSetOfInExpr;
   ifThenExpr : TIfThenExpr;
   forExpr : TForUpwardExpr;
begin
   setOfSymbol:=(inExpr.Typ as TSetOfSymbol);
   elementTyp:=setOfSymbol.Typ;

   blockExpr:=EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos, loopVarExpr, elementTyp);

   if not FTok.TestDelete(ttDO) then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_DoExpected);

   if blockExpr<>nil then
      CurrentProg.EnterSubTable(blockExpr.Table);

   try
      doBlock:=ReadBlock;
   except
      OrphanAndNil(blockExpr);
      OrphanAndNil(loopVarExpr);
      OrphanAndNil(inExpr);
      raise;
   end;

   loopVarInSetExpr:=TSetOfInExpr.CreateOptimal(FCompilerContext, forPos, loopVarExpr, inExpr);
   loopVarExpr.IncRefCount;

   ifThenExpr:=TIfThenExpr.Create(FCompilerContext, forPos, loopVarInSetExpr, doBlock);

   forExpr:=TForUpwardExpr.Create(forPos);
   forExpr.DoExpr:=ifThenExpr;
   forExpr.FromExpr:=TConstIntExpr.Create(elementTyp, setOfSymbol.MinValue);
   forExpr.ToExpr:=TConstIntExpr.Create(elementTyp, setOfSymbol.MaxValue);
   forExpr.VarExpr:=(loopVarExpr as TIntVarExpr);

   if blockExpr<>nil then begin
      CurrentProg.LeaveSubTable;
      blockExpr.AddStatement(forExpr);
      if Optimize then
         Result:=blockExpr.Optimize(FCompilerContext)
      else Result:=blockExpr;
   end else Result:=forExpr;
end;

// ReadForInConnector
//
function TdwsCompiler.ReadForInConnector(const forPos : TScriptPos;
    inExpr : TTypedExpr; const inPos : TScriptPos; loopVarExpr : TVarExpr;
    const loopVarName : String; const loopVarNamePos : TScriptPos) : TProgramExpr;
var
   connectorSymbol : TConnectorSymbol;
   enumerator : IConnectorEnumerator;
   itemType : TTypeSymbol;
   blockExpr : TBlockExpr;
   forInExpr : TConnectorForInExpr;
begin
   connectorSymbol:=(inExpr.Typ as TConnectorSymbol);

   enumerator:=connectorSymbol.ConnectorType.HasEnumerator(itemType);
   if enumerator=nil then begin
      FMsgs.AddCompilerError(inPos, CPE_ArrayExpected);
      itemType:=FCompilerContext.TypVariant;
   end;

   blockExpr:=EnsureLoopVarExpr(forPos, loopVarName, loopVarNamePos, loopVarExpr, itemType);

   if not FTok.TestDelete(ttDO) then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_DoExpected);

   forInExpr:=TConnectorForInExpr.Create(forPos, enumerator, loopVarExpr, inExpr);
   Result:=forInExpr;

   if blockExpr<>nil then
      CurrentProg.EnterSubTable(blockExpr.Table);
   EnterLoop(forInExpr);
   try
      MarkLoopExitable(leBreak);
      try
         forInExpr.DoExpr:=ReadBlock;
      except
         forInExpr.Orphan(FCompilerContext);
         raise;
      end;
   finally
      LeaveLoop;

      if blockExpr<>nil then begin
         CurrentProg.LeaveSubTable;
         blockExpr.AddStatement(Result);
         if Optimize then
            Result:=blockExpr.Optimize(FCompilerContext)
         else Result:=blockExpr;
      end;
   end;
end;

// WarnForVarUsage
//
procedure TdwsCompiler.WarnForVarUsage(varExpr : TVarExpr; const scriptPos : TScriptPos);
var
   i : Integer;
   loopExpr : TProgramExpr;
   currVarExpr : TVarExpr;
   varSymbol : TDataSymbol;
begin
   varSymbol:=varExpr.DataSym;
   for i:=0 to FLoopExprs.Count-1 do begin
      loopExpr:=FLoopExprs.Items[i];
      if loopExpr.InheritsFrom(TForExpr) then begin
         currVarExpr:=TForExpr(loopExpr).VarExpr;
         if currVarExpr.ReferencesVariable(varSymbol) then begin
            FMsgs.AddCompilerWarning(scriptPos, CPE_AssignementToFORLoopVariable);
            Break;
         end;
      end;
   end;
end;

// ReadIf
//
function TdwsCompiler.ReadIf : TProgramExpr;
var
   hotPos : TScriptPos;
   condExpr : TTypedExpr;
   thenExpr : TProgramExpr;
   elseExpr : TProgramExpr;
begin
   hotPos:=FTok.HotPos;

   condExpr:=nil;
   thenExpr:=nil;
   elseExpr:=nil;
   try
      condExpr:=ReadBooleanExpr;

      if not FTok.TestDelete(ttTHEN) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ThenExpected);

      if FTok.Test(ttSEMI) then
         FMsgs.AddCompilerHint(FTok.HotPos, CPH_EmptyThenBlock);

      if FTok.TestDelete(ttELSE) then begin // if () then else;

         FMsgs.AddCompilerHint(FTok.HotPos, CPH_EmptyThenBlock);
         condExpr:=TNotBoolExpr.Create(FCompilerContext, condExpr);
         thenExpr:=ReadBlock;

      end else begin

         thenExpr:=ReadBlock;
         if FTok.TestDelete(ttELSE) then begin
               if FTok.Test(ttSEMI) then
               FMsgs.AddCompilerHint(FTok.HotPos, CPH_EmptyElseBlock);
            elseExpr:=ReadBlock;
         end;

      end;

      if elseExpr=nil then
         Result:=TIfThenExpr.Create(FCompilerContext, hotPos, condExpr, thenExpr)
      else Result:=TIfThenElseExpr.Create(FCompilerContext, hotPos, condExpr, thenExpr, elseExpr);
   except
      OrphanAndNil(condExpr);
      OrphanAndNil(thenExpr);
      OrphanAndNil(elseExpr);
      raise;
   end;

   if Optimize then
      Result:=Result.Optimize(FCompilerContext);
end;

// ReadCase
//
function TdwsCompiler.ReadCase : TCaseExpr;
var
   expr : TProgramExpr;
   condList : TTightList;
   condition : TCaseCondition;
   tt : TTokenType;
   x : Integer;
begin
   condList.Initialize;
   try
      Result := TCaseExpr.Create(FTok.HotPos);
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

               expr := ReadBlock;

               // Add case conditions to TCaseExpr
               for x:=0 to condList.Count-1 do begin
                  condition:=(condList.List[x] as TCaseCondition);
                  condition.TrueExpr:=Expr;
                  if x=0 then
                     condition.OwnsTrueExpr:=True;
                  Result.AddCaseCondition(condition);
               end;
               condList.Clear;

               if not (FTok.Test(ttELSE) or FTok.Test(ttEND) or FTok.TestDelete(ttSEMI)) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
            end;
         end;
      except
         Result.Orphan(FCompilerContext);
         raise;
      end;
   finally
      condList.Clean;
   end;
end;

// ReadCaseConditions
//
function TdwsCompiler.ReadCaseConditions(var condList : TTightList; valueExpr : TTypedExpr) : Integer;
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
            if exprFrom is TConstStringExpr then begin
               condition := TCompareConstStringCaseCondition.Create(hotPos, TConstStringExpr(exprFrom).Value);
               OrphanAndNil(exprFrom);
            end else begin
               condition:=TCompareCaseCondition.Create(hotPos, exprFrom);
            end;
         end;
         exprFrom:=nil;
         condList.Add(condition);
         if valueExpr.Typ<>nil then
            condition.TypeCheck(FCompilerContext, valueExpr.Typ);
      except
         OrphanAndNil(exprFrom);
         raise;
      end;

   until not FTok.TestDelete(ttCOMMA);

   Result:=condList.Count;
end;

// ReadWhile
//
function TdwsCompiler.ReadWhile : TProgramExpr;
var
   condExpr : TTypedExpr;
begin
   Result:=TWhileExpr.Create(FTok.HotPos);
   EnterLoop(Result);
   try
      condExpr:=ReadBooleanExpr;
      TWhileExpr(Result).CondExpr:=condExpr;

      if not FTok.TestDelete(ttDO) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

      if    (not condExpr.IsConstant)
         or (not condExpr.IsOfType(FCompilerContext.TypBoolean))
         or (not condExpr.EvalAsBoolean(FExec)) then
         MarkLoopExitable(leBreak);

      TWhileExpr(Result).LoopExpr := ReadBlock;
   except
      OrphanAndNil(Result);
      raise;
   end;
   LeaveLoop;

   if Optimize then
      Result:=Result.Optimize(FCompilerContext);
end;

// ReadWith
//
function TdwsCompiler.ReadWith : TProgramExpr;
var
   doExpr : TProgramExpr;
   closePos : TScriptPos; // Position at which the ending token was found (for context)
   blockExpr : TBlockExpr;
begin
   // Read a block of instructions enclosed in "begin" and "end"
   blockExpr:=TBlockExpr.Create(FCompilerContext, FTok.HotPos);
   try
      if coContextMap in FOptions then begin
         FSourceContextMap.OpenContext(FTok.CurrentPos, nil, ttWRITE);
         closePos:=FTok.CurrentPos;     // default to close context where it opened (used on errors)
      end;

      CurrentProg.EnterSubTable(blockExpr.Table);
      try

         repeat
            ReadVarDecl(FStandardDataSymbolFactory, blockExpr);
            if not FTok.TestDelete(ttCOMMA) then break;
         until False;

         if not FTok.TestDelete(ttDO) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_DoExpected);

         doExpr:=ReadBlock;
         blockExpr.AddStatement(doExpr);

         HintUnusedSymbols;
      finally
         CurrentProg.LeaveSubTable;
      end;

      if Optimize then
         Result:=blockExpr.Optimize(FCompilerContext)
      else Result:=blockExpr;

      if coContextMap in FOptions then begin
         if Result is TBlockExpr then
            FSourceContextMap.Current.LocalTable:=TBlockExpr(Result).Table;
         FSourceContextMap.CloseContext(closePos);
      end;

   except
      OrphanAndNil(blockExpr);
      raise;
   end;
end;

// ReadRepeat
//
function TdwsCompiler.ReadRepeat : TProgramExpr;
var
   tt : TTokenType;
   condExpr : TTypedExpr;
begin
   Result:=TRepeatExpr.Create(FTok.HotPos);
   EnterLoop(Result);
   try
      TRepeatExpr(Result).LoopExpr:=ReadBlocks([ttUNTIL], tt);
      TRepeatExpr(Result).SetScriptPos(FTok.HotPos);
      condExpr:=ReadBooleanExpr;
      TRepeatExpr(Result).CondExpr:=condExpr;
      if (not condExpr.IsConstant) or condExpr.EvalAsBoolean(FExec) then
         MarkLoopExitable(leBreak);
   except
      OrphanAndNil(Result);
      raise;
   end;
   LeaveLoop;

   if Optimize then
      Result:=Result.Optimize(FCompilerContext);
end;

// ReadAssign
//
function TdwsCompiler.ReadAssign(token : TTokenType; var left : TDataExpr) : TProgramExpr;
var
   hotPos : TScriptPos;
   right : TTypedExpr;
begin
   hotPos:=FTok.HotPos;
   right:=nil;
   try
      right:=ReadExpr(left.Typ);
      Result:=CreateAssign(hotPos, token, left, right);
      left:=nil;
   except
      OrphanAndNil(left);
      OrphanAndNil(right);
      raise;
   end;
end;

// ReadSelfMethod
//
function TdwsCompiler.ReadSelfMethod(methodSym : TMethodSymbol;
               isWrite : Boolean; expecting : TTypeSymbol;
               overloads : TFuncSymbolList;
               options : TCreateFunctionOptions) : TTypedExpr;
var
   progMeth : TMethodSymbol;
   structSym : TCompositeTypeSymbol;
begin
   progMeth:=CurrentProg.ContextMethodSymbol;

   if progMeth<>nil then begin
      if methodSym.IsStatic then
         Result:=GetMethodExpr(methodSym, nil, rkObjRef, FTok.HotPos, options)
      else if progMeth.IsStatic then begin
         structSym:=progMeth.StructSymbol;
         Result:=GetMethodExpr(methodSym,
                               TConstExpr.Create(structSym.MetaSymbol, Int64(structSym)),
                               rkClassOfRef, FTok.HotPos, options);
      end else if progMeth.SelfSym is TConstByRefParamSymbol then begin
         Result:=GetMethodExpr(methodSym,
                               GetConstByRefParamExpr(TConstByRefParamSymbol(progMeth.SelfSym)),
                               rkObjRef, FTok.HotPos, options);
      end else if progMeth.SelfSym=nil then begin
         Result:=GetMethodExpr(methodSym, nil, rkClassOfRef, FTok.HotPos, options);
      end else begin
         Result:=GetMethodExpr(methodSym,
                               GetVarExpr(FTok.HotPos, progMeth.SelfSym),
                               rkObjRef, FTok.HotPos, options);
      end;
   end else begin
      structSym:=methodSym.StructSymbol;
      Result:=GetMethodExpr(methodSym,
                            TConstExpr.Create(structSym.MetaSymbol, Int64(structSym)),
                            rkClassOfRef, FTok.HotPos, [cfoForceStatic]);
   end;

   Result:=WrapUpFunctionRead(TFuncExpr(Result), expecting, overloads, options);
end;

// ReadMethod
//
function TdwsCompiler.ReadMethod(methodSym : TMethodSymbol; instanceExpr : TTypedExpr;
                                 const scriptPos : TScriptPos;
                                 expecting : TTypeSymbol = nil;
                                 overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   funcExpr : TFuncExprBase;
begin
   if methodSym.IsClassMethod then
      funcExpr:=GetMethodExpr(methodSym, instanceExpr, rkClassOfRef, scriptPos, [])
   else begin
      funcExpr:=GetMethodExpr(methodSym, instanceExpr, rkObjRef, scriptPos, []);
   end;
   Result:=WrapUpFunctionRead(funcExpr, expecting, overloads, []);
end;

// ReadStaticMethod
//
function TdwsCompiler.ReadStaticMethod(methodSym : TMethodSymbol; metaExpr : TTypedExpr;
                                       const scriptPos : TScriptPos;
                                       expecting : TTypeSymbol = nil;
                                       overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   funcExpr : TFuncExprBase;
   compoSym : TCompositeTypeSymbol;
begin
   if methodSym.Kind=fkConstructor then begin
      compoSym:=(metaExpr.Typ as TStructuredTypeMetaSymbol).StructSymbol;
      if compoSym.IsStatic then
         FMsgs.AddCompilerErrorFmt(scriptPos, CPE_ClassIsStaticNoInstantiation, [compoSym.Name]);
   end;
   funcExpr:=GetMethodExpr(methodSym, metaExpr, rkClassOfRef, scriptPos, []);
   Result:=WrapUpFunctionRead(funcExpr, expecting, overloads, []);
end;

type
   TFuncAtLevelSymbolList = class(TFuncSymbolList)
      public
         Level : Integer;
         function Callback(sym : TSymbol) : Boolean;
   end;

function TFuncAtLevelSymbolList.Callback(sym : TSymbol) : Boolean;
var
   locFuncSym : TFuncSymbol;
begin
   locFuncSym:=sym.AsFuncSymbol;
   if locFuncSym<>nil then begin
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
   visibility : TdwsVisibility;
begin
   lastOverloaded:=methSym;
   struct:=methSym.StructSymbol;
   visibility:=struct.Members.VisibilityFromScope(CurrentStruct);
   repeat
      for member in struct.Members do begin
         if not UnicodeSameText(member.Name, methSym.Name) then continue;
         if not (member is TMethodSymbol) then continue;
         if not member.IsVisibleFor(visibility) then continue;
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
                                             expecting : TTypeSymbol;
                                             options : TCreateFunctionOptions) : TTypedExpr;
var
   overloads : TFuncSymbolList;
begin
   overloads:=TFuncSymbolList.Create;
   try
      CollectMethodOverloads(methSym, overloads);
      Result:=ReadSelfMethod(methSym, isWrite, expecting, overloads, options);
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
                                      const argPosArray : TScriptPosArray;
                                      expecting : TFuncSymbol;
                                      cfOptions : TCreateFunctionOptions) : Boolean;
var
   i, delta : Integer;
   j : Integer;
   funcExprArgCount : Integer;
   match, bestMatch : TFuncSymbol;
   struct : TCompositeTypeSymbol;
   matchDistance, bestMatchDistance, bestCount : Integer;
   matchParamType, funcExprParamType : TTypeSymbol;
   wasVarParam, nowVarParam : Boolean;
   funcExprArg : TExprBase;
begin
   bestMatch:=nil;
   bestCount:=0;
   bestMatchDistance:=MaxInt;
   if expecting<>nil then
      funcExprArgCount:=expecting.Params.Count
   else funcExprArgCount:=funcExpr.Args.Count;
   for i:=0 to overloads.Count-1 do begin
      match:=overloads[i];
      if funcExprArgCount>match.Params.Count then continue;
      matchDistance:=0;
      for j:=0 to funcExprArgCount-1 do begin
         matchParamType:=match.GetParamType(j);
         if expecting<>nil then
            funcExprParamType:=expecting.Params[j].Typ
         else funcExprParamType:=funcExpr.GetArgType(j);
         if not matchParamType.IsOfType(funcExprParamType) then begin

            if funcExprParamType.IsOfType(FCompilerContext.TypVariant) then begin

               if not funcExprParamType.IsCompatible(matchParamType) then begin
                  match:=nil;
                  break;
               end else begin
                  // disfavor variant promotion to integer
                  if matchParamType.IsOfType(FCompilerContext.TypInteger) then
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
                           or (    (funcExprParamType.Typ=FCompilerContext.TypNil)
                               and (   (matchParamType.Typ is TClassSymbol)
                                    or (matchParamType.Typ is TInterfaceSymbol))) then begin

                  Inc(matchDistance, 1);

               end else begin

                  match:=nil;
                  break;

               end;

            end else if not (   (matchParamType.IsOfType(FCompilerContext.TypFloat) and funcExprParamType.IsOfType(FCompilerContext.TypInteger))
                             or matchParamType.IsCompatible(funcExprParamType)) then begin

               match:=nil;
               break;

            end else begin

               if (funcExprParamType is TClassSymbol) or (funcExprParamType is TInterfaceSymbol) then begin

                  if matchParamType is TStructuredTypeSymbol then
                     Inc(matchDistance, (matchParamType as TStructuredTypeSymbol).NthParentOf(TStructuredTypeSymbol(funcExprParamType)))
                  else Inc(matchDistance, 256);

               end else begin

                  Inc(matchDistance, 1);

               end;

            end;
         end;
      end;
      if match=nil then continue;
      for j:=funcExprArgCount to match.Params.Count-1 do begin
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
      if bestMatch.ClassType=TAliasMethodSymbol then
         bestMatch:=TAliasMethodSymbol(bestMatch).Alias;
      if bestMatch<>funcExpr.FuncSym then begin
         if coSymbolDictionary in Options then begin
            ReplaceSymbolUse(funcExpr.FuncSym, bestMatch, funcExpr.ScriptPos);
            delta:=funcExpr.Args.Count-funcExpr.FuncSym.Params.Count;
            if delta>=0 then begin
               for i:=0 to Min(bestMatch.Params.Count, funcExpr.Args.Count-delta)-1 do begin
                  nowVarParam:=(bestMatch.Params[i].ClassType=TVarParamSymbol);
                  funcExprArg:=funcExpr.Args[i+delta];
                  wasVarParam:=(funcExprArg is TByRefParamExpr) and TByRefParamExpr(funcExprArg).IsWritable;
                  if wasVarParam<>nowVarParam then begin
                     if wasVarParam then
                        FSymbolDictionary.ChangeUsageAt(argPosArray[i], [], [suWrite])
                     else FSymbolDictionary.ChangeUsageAt(argPosArray[i], [suWrite], []);
                  end;
               end;
            end;
         end;
         funcExpr:=funcExpr.ChangeFuncSymbol(FCompilerContext, bestMatch, cfOptions);
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
   if sym<>ForwardedSym then begin
      locSym:=sym.AsFuncSymbol;
      if locSym<>nil then
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
      Result:=CurrentProg.Table.EnumerateSymbolsOfNameInScope(funcSym.Name, enumerator.Callback);
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

// FuncPerfectMatchOverload
//
function TdwsCompiler.FuncPerfectMatchOverload(funcSym : TFuncSymbol) : TFuncSymbol;
var
   enumerator : TPerfectMatchEnumerator;
begin
   enumerator:=TPerfectMatchEnumerator.Create;
   try
      enumerator.FuncSym:=funcSym;
      CurrentProg.Table.EnumerateSymbolsOfNameInScope(funcSym.Name, enumerator.Callback);
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
function TdwsCompiler.ReadFunc(funcSym : TFuncSymbol; codeExpr : TTypedExpr = nil;
                               expecting : TTypeSymbol = nil;
                               overloads : TFuncSymbolList = nil) : TTypedExpr;
var
   funcExpr : TFuncExprBase;
begin
   if funcSym.IsExternal then begin
      if funcSym is TMagicFuncSymbol then begin
         // jitted external call
         Assert(Assigned(FExternalRoutinesManager));
         funcSym.Executable := FExternalRoutinesManager.CreateExternalFunction(funcSym);
         TMagicFuncSymbol(funcSym).InternalFunction := funcSym.Executable.GetSelf as TInternalMagicFunction;
         TMagicFuncSymbol(funcSym).InternalFunction.IncRefCount;
      end else begin
         // abstract external call
         funcSym.Executable:=TExternalFuncHandler.Create;
      end;
   end;

   funcExpr:=GetFuncExpr(funcSym, codeExpr);
   Result:=WrapUpFunctionRead(funcExpr, expecting, overloads, []);

   if Optimize then
      Result:=Result.OptimizeToTypedExpr(FCompilerContext, Result.ScriptPos);
end;

// WrapUpFunctionRead
//
function TdwsCompiler.WrapUpFunctionRead(funcExpr : TFuncExprBase; expecting : TTypeSymbol;
                                         overloads : TFuncSymbolList;
                                         cfOptions : TCreateFunctionOptions) : TTypedExpr;
var
   argPosArray : TScriptPosArray;
begin
   Result:=funcExpr;
   try
      if FTok.Test(ttBLEFT) then begin
         ReadFuncArgs(funcExpr, argPosArray, overloads);
         if overloads<>nil then begin
            if not ResolveOverload(funcExpr, overloads, argPosArray, nil, cfOptions) then begin
               Result := TErrorValueExpr.Create(FSystemTable.TypAnyType);
               funcExpr.Orphan(FCompilerContext);
               Exit;
            end;
            Result:=funcExpr;
         end;
         TypeCheckArguments(FCompilerContext, funcExpr, argPosArray);
      end else begin
         if    (    (expecting is TNilSymbol)
                and (funcExpr is TFuncPtrExpr)
                and not FTok.Test(ttDOT))
            or (    (expecting.AsFuncSymbol<>nil)
                and funcExpr.funcSym.IsCompatible(expecting)) then begin
            if (funcExpr.FuncSym.Level>1) and not (coAllowClosures in Options) then
               FMsgs.AddCompilerError(funcExpr.ScriptPos, CPE_LocalFunctionAsDelegate);
            Result:=TFuncRefExpr.Create(FCompilerContext, funcExpr);
         end else begin
            if overloads<>nil then begin
               if (funcExpr.Args.Count=0) and (expecting.AsFuncSymbol<>nil) then begin
                  if not ResolveOverload(funcExpr, overloads, argPosArray, expecting.AsFuncSymbol, cfOptions) then Exit;
                  if (funcExpr.FuncSym.Level>1) and not (coAllowClosures in Options) then
                     FMsgs.AddCompilerError(funcExpr.ScriptPos, CPE_LocalFunctionAsDelegate);
                  Result:=TFuncRefExpr.Create(FCompilerContext, funcExpr);
                  Exit;
               end else begin
                  if not ResolveOverload(funcExpr, overloads, argPosArray, nil, cfOptions) then Exit;
               end;
               Result:=funcExpr;
            end;
            TypeCheckArguments(FCompilerContext, funcExpr, nil);
         end;
      end;
      WarnDeprecatedFunc(funcExpr);
   except
      OrphanAndNil(Result);
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
      Result:=FCompilerContext.TypVariant;
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
   funcSym : TFuncSymbol;
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

            if arg<>nil then begin
               if Optimize then
                  arg:=arg.OptimizeToTypedExpr(FCompilerContext, argPos);

               if expectedType<>nil then begin
                  funcSym:=arg.Typ.AsFuncSymbol;
                  if    (funcSym<>nil)
                     and (expectedType.AsFuncSymbol=nil) then begin
                     arg:=ReadFunc(funcSym, arg as TDataExpr, nil);
                  end;
               end;

               AddArgProc(arg);
               n:=Length(argPosArray);
               SetLength(argPosArray, n+1);
               argPosArray[n]:=argPos;

               if (argSym<>nil) and (argSym.ClassType=TVarParamSymbol) and (arg is TVarExpr) then
                  WarnForVarUsage(TVarExpr(arg), argPos);
            end;
         until not (FTok.TestDelete(ttCOMMA) and FTok.HasTokens);
         if not FTok.TestDelete(rightDelim) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;
   end;
end;

// ReadSetOfType
//
function TdwsCompiler.ReadSetOfType(const typeName : String; typeContext : TdwsReadTypeContext) : TSetOfSymbol;
var
   elementType : TTypeSymbol;
   aMin, aMax : Integer;
   typePos : TScriptPos;
begin
   if not FTok.TestDelete(ttOF) then
      FMsgs.AddCompilerError(FTok.HotPos, CPE_OfExpected);

   typePos:=FTok.HotPos;
   elementType:=ReadType('', typeContext);
   aMin:=0;
   aMax:=0;

   if elementType.UnAliasedTypeIs(TEnumerationSymbol) then begin

      aMax:=TEnumerationSymbol(elementType.UnAliasedType).HighBound;

   end else FMsgs.AddCompilerError(typePos, CPE_EnumerationExpected);

   Result:=TSetOfSymbol.Create(typeName, elementType, aMin, aMax);
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
         or not (   bound.Typ.IsOfType(FCompilerContext.TypInteger)
                 or (bound.Typ is TEnumerationSymbol)
                 or bound.Typ.IsOfType(FCompilerContext.TypBoolean)) then
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
            lowBound:=ReadExpr(FCompilerContext.TypAnyType);

            if lowBound is TTypeReferenceExpr then begin

               // handle "array [someType] of" special cases

               if TTypeReferenceExpr(lowBound).Typ is TBaseBooleanSymbol then begin

                  min.Insert0(FUnifiedConstants.CreateBoolean(False));
                  max.Insert0(FUnifiedConstants.CreateBoolean(True));

               end else if TTypeReferenceExpr(lowBound).Typ is TEnumerationSymbol then begin

                  enumSymbol:=TEnumerationSymbol(TTypeReferenceExpr(lowBound).Typ);

                  min.Insert0(TConstIntExpr.Create(enumSymbol, enumSymbol.LowBound));
                  max.Insert0(TConstIntExpr.Create(enumSymbol, enumSymbol.HighBound));

               end else if (min.Count=0) and FTok.TestDelete(ttARIGHT) then begin

                  try
                     Result:=ReadAssociativeArrayType(typeName, TTypeReferenceExpr(lowBound).Typ, typeContext);
                  finally
                     OrphanAndNil(lowBound);
                  end;
                  Exit;

               end else begin

                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ArrayBoundNotOrdinal)

               end;
               OrphanAndNil(lowBound);

            end else if (min.Count=0) and (lowBound.Typ is TStructuredTypeMetaSymbol)
                                      and FTok.TestDelete(ttARIGHT) then begin
               try
                  Result:=ReadAssociativeArrayType(typeName, lowBound.Typ.Typ, typeContext);
               finally
                  OrphanAndNil(lowBound);
               end;
               Exit;

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
                  max[0].Orphan(FCompilerContext);
                  max[0] := FUnifiedConstants.CreateInteger(min[0].EvalAsInteger(FExec));
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

         Result := TOpenArraySymbol.Create(TypeName, FCompilerContext.TypVariant, FCompilerContext.TypInteger);

      end else begin

         typ:=ReadType('', typeContext);

         if boundsOk and (min.Count>0) then begin

            if typ.ClassType=TRecordSymbol then
               if not TRecordSymbol(typ).IsFullyDefined then
                  FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_RecordTypeNotFullyDefined, [typ.Name]);

            // initialize innermost array
            Result:=TStaticArraySymbol.Create('', typ, min[0].Typ,
                                              min[0].EvalAsInteger(FExec),
                                              max[0].EvalAsInteger(FExec));
            // add outer arrays
            for x:=1 to min.Count - 1 do begin
               CurrentProg.RootTable.AddToDestructionList(Result);
               Result := TStaticArraySymbol.Create('', Result, min[0].Typ,
                              min[x].EvalAsInteger(FExec),
                              max[x].EvalAsInteger(FExec));
            end;

            // only outermost array is named
            Result.SetName(TypeName);

         end else begin

            Result := TDynamicArraySymbol.Create(TypeName, typ, FCompilerContext.TypInteger);

         end;

      end;

   finally
      min.Orphan(FCompilerContext);
      max.Orphan(FCompilerContext);
   end;
end;

// ReadAssociativeArrayType
//
function TdwsCompiler.ReadAssociativeArrayType(const typeName : String; keyType : TTypeSymbol;
                                               typeContext : TdwsReadTypeContext) : TAssociativeArraySymbol;
var
   elementType : TTypeSymbol;
begin
   if not FTok.TestDelete(ttOF) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_OfExpected);

   elementType:=ReadType('', typeContext);
   Result:=TAssociativeArraySymbol.Create(typeName, elementType, keyType);
end;

// ReadArrayConstant
//
function TdwsCompiler.ReadArrayConstant(closingToken : TTokenType;
                                        expecting : TTypeSymbol) : TArrayConstantExpr;
var
   factory : IdwsDataSymbolFactory;
   itemExpecting : TTypeSymbol;

   procedure ReadArrayConstantRange(result : TArrayConstantExpr; expr1 : TTypedExpr);
   var
      expr2 : TTypedExpr;
      range1, range2 : Int64;
      boundsOk : Integer;
   begin
      boundsOk:=0;
      expr2:=nil;
      try
         if not expr1.IsOfType(FCompilerContext.TypInteger) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_OrdinalExpressionExpected)
         else if not expr1.IsConstant then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected)
         else boundsOk:=1;

         expr2:=factory.ReadInitExpr(itemExpecting);

         if (expr2=nil) or not expr2.IsOfType(FCompilerContext.TypInteger) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_OrdinalExpressionExpected)
         else if not expr2.IsConstant then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected)
         else Inc(boundsOk);

         if boundsOk=2 then begin
            if expr1.Typ.SameType(expr2.Typ) then begin
               range1:=expr1.EvalAsInteger(FExec);
               range2:=expr2.EvalAsInteger(FExec);
               if Abs(range2-range1)>cMaxArrayItemRange then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_RangeTooLarge)
               else result.AddElementRange(FCompilerContext, range1, range2, expr1.Typ);
            end else begin
               IncompatibleTypes(FTok.HotPos, CPE_RangeIncompatibleTypes, expr1.Typ, expr2.Typ);
            end;
         end;
      finally
         OrphanAndNil(expr1);
         OrphanAndNil(expr2);
      end;
   end;

var
   expr : TTypedExpr;
   hotPos : TScriptPos;
begin
   factory:=TStandardSymbolFactory.Create(Self);
   Result:=TArrayConstantExpr.Create(FCompilerContext, FTok.HotPos);
   try
      if expecting<>nil then
         itemExpecting:=expecting.Typ
      else itemExpecting:=nil;
      if not FTok.TestDelete(closingToken) then begin
         // At least one argument was found
         repeat
            hotPos:=FTok.CurrentPos;
            expr:=factory.ReadInitExpr(itemExpecting);
            if expr<>nil then begin
               if FTok.TestDelete(ttDOTDOT) then
                  ReadArrayConstantRange(TArrayConstantExpr(Result), expr)
               else begin
                  TArrayConstantExpr(Result).AddElementExpr(hotPos, FCompilerContext, expr);
               end;
            end;
         until not FTok.TestDelete(ttCOMMA);

         if not FTok.TestDelete(closingToken) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
      end else begin
         // empty array
         (Result.Typ as TStaticArraySymbol).Typ:=FCompilerContext.TypVariant;
      end;

      if expecting is TOpenArraySymbol then
         (Result.Typ as TStaticArraySymbol).Typ:=FCompilerContext.TypVariant
      else Result.TypeCheckElements(FCompilerContext);
      if Optimize then
         Result:=Result.Optimize(FCompilerContext) as TArrayConstantExpr;
   except
      Result.Orphan(FCompilerContext);
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
   i : Integer;
   mapFunctionType : TFuncSymbol;
   methodKind : TArrayMethodKind;

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
      arraySym := baseExpr.Typ.UnAliasedType as TArraySymbol;

      methodKind:=NameToArrayMethod(name, FMsgs, namePos);

      case methodKind of

         amkAdd, amkPush :
            argList.DefaultExpected:=TParamSymbol.Create('', arraySym.Typ);

         amkIndexOf, amkRemove : begin
            argSymTable:=TUnSortedSymbolTable.Create;
            argSymTable.AddSymbol(TParamSymbol.Create('', arraySym.Typ));
            argList.Table:=argSymTable;
         end;

         amkSort :
            argList.DefaultExpected:=TParamSymbol.Create('', arraySym.SortFunctionType(FCompilerContext.TypInteger));

         amkMap :
            argList.DefaultExpected:=TParamSymbol.Create('', arraySym.MapFunctionType(FCompilerContext.TypAnyType));

      end;

      ReadArguments(argList.AddExpr, ttBLEFT, ttBRIGHT, argPosArray, argList.ExpectedArg);

      try
         case methodKind of

            amkLow : begin
               CheckArguments(0, 0);
               Result:=CreateArrayLow(baseExpr, arraySym, True);
            end;

            amkHigh : begin
               CheckArguments(0, 0);
               if not (arraySym is TStaticArraySymbol) then
                  CheckNotTypeReference;
               Result:=CreateArrayHigh(baseExpr, arraySym, True);
            end;

            amkLength, amkCount : begin
               CheckArguments(0, 0);
               CheckNotTypeReference;
               Result:=CreateArrayLength(baseExpr, arraySym);
            end;

            amkAdd, amkPush : begin
               CheckRestricted;
               if CheckArguments(1, 99) then begin
                  for i:=0 to argList.Count-1 do begin
                     if    (argList[i].Typ=nil)
                        or not (   arraySym.Typ.IsCompatible(argList[i].Typ)
                                or arraySym.IsCompatible(argList[i].Typ)
                                or (    (argList[i].Typ is TStaticArraySymbol)
                                    and (   arraySym.Typ.IsCompatible(argList[i].Typ.Typ)
                                         or (argList[i].Typ.Size=0)))) then begin
                        argList[i]:=CompilerUtils.WrapWithImplicitConversion(
                                             FCompilerContext, argList[i], arraySym.Typ, argPosArray[i],
                                             CPE_IncompatibleParameterTypes);
                        Break;
                     end else if argList[i].ClassType=TArrayConstantExpr then begin
                        TArrayConstantExpr(argList[i]).Prepare(FCompilerContext, arraySym.Typ);
                     end;
                  end;
                  Result:=TArrayAddExpr.Create(namePos, baseExpr, argList);
                  argList.Clear;
               end else Result:=TArrayAddExpr.Create(namePos, baseExpr, argList);
            end;

            amkPop : begin
               CheckRestricted;
               CheckArguments(0, 0);
               Result:=TArrayPopExpr.Create(FCompilerContext, namePos, baseExpr);
            end;

            amkPeek : begin
               CheckRestricted;
               CheckArguments(0, 0);
               Result:=TArrayPeekExpr.Create(FCompilerContext, namePos, baseExpr);
            end;

            amkDelete : begin
               CheckRestricted;
               if CheckArguments(1, 2) then begin
                  if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FCompilerContext.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  if argList.Count>1 then begin
                     if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FCompilerContext.TypInteger) then
                        FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                     Result:=TArrayDeleteExpr.Create(namePos, baseExpr,
                                                     argList[0], argList[1]);
                  end else Result:=TArrayDeleteExpr.Create(namePos, baseExpr,
                                                           argList[0], nil);
                  argList.Clear;
               end else Result:=TArrayDeleteExpr.Create(namePos, baseExpr, nil, nil);
            end;

            amkIndexOf : begin
               CheckRestricted;
               if CheckArguments(1, 2) then begin
                  if (argList[0].Typ=nil) or not arraySym.Typ.IsCompatible(argList[0].Typ) then
                     IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                                       arraySym.Typ, argList[0].Typ);
                  if argList.Count>1 then begin
                     if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FCompilerContext.TypInteger) then
                        FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                     Result:=TArrayIndexOfExpr.Create(FCompilerContext, namePos, baseExpr,
                                                      argList[0], argList[1]);
                  end else Result:=TArrayIndexOfExpr.Create(FCompilerContext, namePos, baseExpr,
                                                            argList[0], nil);
                  argList.Clear;
               end else Result:=TArrayIndexOfExpr.Create(FCompilerContext, namePos, baseExpr, nil, nil);
            end;

            amkRemove : begin
               CheckRestricted;
               if CheckArguments(1, 2) then begin
                  if (argList[0].Typ=nil) or not arraySym.Typ.IsCompatible(argList[0].Typ) then
                     IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                                       arraySym.Typ, argList[0].Typ);
                  if argList.Count>1 then begin
                     if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FCompilerContext.TypInteger) then
                        FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                     Result:=TArrayRemoveExpr.Create(FCompilerContext, namePos, baseExpr,
                                                     argList[0], argList[1]);
                  end else Result:=TArrayRemoveExpr.Create(FCompilerContext, namePos, baseExpr,
                                                           argList[0], nil);
                  argList.Clear;
               end else Result:=TArrayRemoveExpr.Create(FCompilerContext, namePos, baseExpr, nil, nil);
            end;

            amkInsert : begin
               CheckRestricted;
               if CheckArguments(2, 2) then begin
                  if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FCompilerContext.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  if (argList[1].Typ=nil) or not arraySym.Typ.IsCompatible(argList[1].Typ) then
                     IncompatibleTypes(argPosArray[1], CPE_IncompatibleParameterTypes,
                                       arraySym.Typ, argList[1].Typ);
                  Result:=TArrayInsertExpr.Create(namePos, baseExpr,
                                                  argList[0], argList[1]);
                  argList.Clear;
               end else Result:=TArrayInsertExpr.Create(namePos, baseExpr, nil, nil);
            end;

            amkSetLength : begin
               CheckRestricted;
               if CheckArguments(1, 1) then begin
                  if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FCompilerContext.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  Result:=TArraySetLengthExpr.Create(namePos, baseExpr, argList[0]);
                  argList.Clear;
               end else Result:=TArraySetLengthExpr.Create(namePos, baseExpr, nil);
            end;

            amkClear : begin
               CheckRestricted;
               CheckArguments(0, 0);
               Result := TArraySetLengthExpr.Create(namePos, baseExpr, FUnifiedConstants.CreateInteger(0));
            end;

            amkSwap : begin
               CheckRestricted;
               if CheckArguments(2, 2) then begin
                  if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FCompilerContext.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                  if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FCompilerContext.TypInteger) then
                     FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                  Result:=TArraySwapExpr.Create(namePos, baseExpr,
                                                argList[0], argList[1]);
                  argList.Clear;
               end else Result:=TArraySwapExpr.Create(namePos, baseExpr, nil, nil);
            end;

            amkCopy : begin
               CheckRestricted;
               if CheckArguments(0, 2) then begin
                  if argList.Count>0 then begin
                     if (argList[0].Typ=nil) or not argList[0].Typ.IsOfType(FCompilerContext.TypInteger) then
                        FMsgs.AddCompilerError(argPosArray[0], CPE_IntegerExpressionExpected);
                     if argList.Count>1 then begin
                        if (argList[1].Typ=nil) or not argList[1].Typ.IsOfType(FCompilerContext.TypInteger) then
                           FMsgs.AddCompilerError(argPosArray[1], CPE_IntegerExpressionExpected);
                        Result:=TArrayCopyExpr.Create(FCompilerContext, namePos, baseExpr,
                                                      argList[0], argList[1]);
                     end else Result:=TArrayCopyExpr.Create(FCompilerContext, namePos, baseExpr,
                                                            argList[0], nil);
                  end else Result:=TArrayCopyExpr.Create(FCompilerContext, namePos, baseExpr, nil, nil);
                  argList.Clear;
               end else Result:=TArrayCopyExpr.Create(FCompilerContext, namePos, baseExpr, nil, nil);
            end;

            amkSort : begin
               CheckRestricted;
               if CheckArguments(0, 1) then begin
                  if argList.Count=0 then begin
                     if arraySym.Typ.IsOfType(FCompilerContext.TypString) then
                        Result:=TArraySortNaturalStringExpr.Create(FCompilerContext, namePos, baseExpr)
                     else if arraySym.Typ.IsOfType(FCompilerContext.TypInteger) then
                        Result:=TArraySortNaturalIntegerExpr.Create(FCompilerContext, namePos, baseExpr)
                     else if arraySym.Typ.IsOfType(FCompilerContext.TypFloat) then
                        Result:=TArraySortNaturalFloatExpr.Create(FCompilerContext, namePos, baseExpr)
                     else begin
                        FMsgs.AddCompilerError(namePos, CPE_ArrayDoesNotHaveNaturalSortOrder);
                        Result:=TArraySortNaturalExpr.Create(FCompilerContext, namePos, baseExpr);
                     end;
                  end else begin
                     if not argList[0].Typ.IsCompatible(arraySym.SortFunctionType(FCompilerContext.TypInteger)) then begin
                        IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                                          arraySym.SortFunctionType(FCompilerContext.TypBoolean), argList[0].Typ);
                        argList.OrphanItems(FCompilerContext);
                        argList.Clear;
                     end;
                     if argList.Count>0 then begin
                        Result:=TArraySortExpr.Create(FCompilerContext, namePos, baseExpr,
                                                      TFuncPtrExpr.Create(FCompilerContext, argPosArray[0], argList[0]));
                        argList.Clear;
                     end else Result:=TArraySortExpr.Create(FCompilerContext, namePos, baseExpr, nil);
                  end;
               end else Result:=TArraySortExpr.Create(FCompilerContext, namePos, baseExpr, nil);
            end;

            amkMap : begin
               CheckRestricted;
               if CheckArguments(1, 1) then begin
                  mapFunctionType:=arraySym.MapFunctionType(FCompilerContext.TypAnyType);
                  if      argList[0].Typ.IsCompatible(mapFunctionType)
                     and (argList[0].Typ.Typ<>nil) then begin
                     Result:=TArrayMapExpr.Create(FCompilerContext, namePos, baseExpr,
                                                  TFuncPtrExpr.Create(FCompilerContext, argPosArray[0], argList[0]));
                     argList.Clear;
                  end else begin
                     IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                                       mapFunctionType, argList[0].Typ);
                     argList.OrphanItems(FCompilerContext)
                  end;
               end;
               if Result=nil then
                  Result:=TArrayMapExpr.Create(FCompilerContext, namePos, baseExpr, nil);
            end;

            amkReverse : begin
               CheckRestricted;
               CheckArguments(0, 0);
               Result:=TArrayReverseExpr.Create(namePos, baseExpr);
            end;

         else
            FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);
         end;
      except
         OrphanAndNil(Result);
         raise;
      end;
   finally
      OrphanObject(argSymTable);
      argList.Orphan(FCompilerContext);
   end;
end;

// ReadAssociativeArrayMethod
//
function TdwsCompiler.ReadAssociativeArrayMethod(const name : String; const namePos : TScriptPos;
                                                 baseExpr : TTypedExpr) : TProgramExpr;
var
   argList : TTypedExprList;
   argPosArray : TScriptPosArray;
   argSymTable : TUnSortedSymbolTable;
   arraySym : TAssociativeArraySymbol;
   methodKind : TArrayMethodKind;

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
      arraySym := baseExpr.Typ.UnAliasedType as TAssociativeArraySymbol;

      methodKind := NameToArrayMethod(name, FMsgs, namePos);

      ReadArguments(argList.AddExpr, ttBLEFT, ttBRIGHT, argPosArray, argList.ExpectedArg);

      try
         case methodKind of

            amkLength, amkCount : begin
               CheckArguments(0, 0);
               Result:=TAssociativeArrayLengthExpr.Create(FCompilerContext, baseExpr);
            end;

            amkClear : begin
               CheckArguments(0, 0);
               Result:=TAssociativeArrayClearExpr.Create(namePos, baseExpr);
            end;

            amkDelete : begin
               if CheckArguments(1, 1) then begin
                  if (argList[0].Typ=nil) or not arraySym.KeyType.IsCompatible(argList[0].Typ) then
                     IncompatibleTypes(argPosArray[0], CPE_IncompatibleParameterTypes,
                                       arraySym.Typ, argList[0].Typ);
                  Result := TAssociativeArrayDeleteExpr.Create(FCompilerContext, baseExpr, argList[0]);
                  argList.Clear;
               end else Result := TAssociativeArrayDeleteExpr.Create(FCompilerContext, baseExpr, nil);
            end;

            amkKeys : begin
               CheckArguments(0, 0);
               Result := TAssociativeArrayKeysExpr.Create(FCompilerContext, baseExpr);
            end;

         else
            FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);
         end;
      except
         OrphanAndNil(Result);
         raise;
      end;
   finally
      OrphanObject(argSymTable);
      argList.Orphan(FCompilerContext);
   end;
end;

// ReadStringMethod
//
function TdwsCompiler.ReadStringMethod(const name : String; const namePos : TScriptPos;
                                       baseExpr : TTypedExpr) : TProgramExpr;
var
   sk : TSpecialKeywordKind;
begin
   try
      if FTok.TestDelete(ttBLEFT) and not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoParamsExpected);

      sk:=IdentifySpecialName(name);

      if sk in [skLength, skHigh] then

         Result:=TStringLengthExpr.Create(FCompilerContext, baseExpr)

      else begin

         if sk=skLow then begin
            OrphanAndNil(baseExpr);
            Result := FUnifiedConstants.CreateInteger(1);
         end else begin
            Result:=nil;
            FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);
         end;

      end;

      if not (coHintsDisabled in FOptions) then
         CheckSpecialNameCase(name, sk, namePos);
   except
      OrphanAndNil(baseExpr);
      raise;
   end;
end;

// ReadSetOfMethod
//
function TdwsCompiler.ReadSetOfMethod(const name : String; const namePos : TScriptPos;
                                      baseExpr : TTypedExpr) : TProgramExpr;
var
   sk : TSpecialKeywordKind;
begin
   try
      if not FTok.TestDelete(ttBLEFT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

      sk:=IdentifySpecialName(name);

      case sk of
         skInclude, skExclude :
            Result:=ReadIncludeExclude(namePos, sk, baseExpr, namePos);
      else
         Result:=nil;
         FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [Name]);
      end;

      if not (coHintsDisabled in FOptions) then
         CheckSpecialNameCase(name, sk, namePos);

      if not FTok.TestDelete(ttBRIGHT) then begin
         OrphanAndNil(Result);
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;
   except
      OrphanAndNil(baseExpr);
      raise;
   end;
end;

// ReadElementMethod
//
function TdwsCompiler.ReadElementMethod(const name : String; const namePos : TScriptPos;
                                        baseExpr : TTypedExpr) : TProgramExpr;
type
   TElementMethod = (emInvalid, emName, emQualifiedName, emValue);
var
   enumeration : TEnumerationSymbol;
   element : TElementSymbol;
   meth : TElementMethod;
begin
   enumeration:=(baseExpr.Typ.UnAliasedType as TEnumerationSymbol);

   if SameText(name, 'name') then
      meth:=emName
   else if SameText(name, 'qualifiedname') then
      meth:=emQualifiedName
   else if SameText(name, 'value') then
      meth:=emValue
   else meth:=emInvalid;

   case meth of
      emName, emQualifiedName : begin

         if baseExpr.ClassType=TConstIntExpr then begin

            element:=enumeration.ElementByValue(TConstIntExpr(baseExpr).Value);
            if element=nil then begin
               FMsgs.AddCompilerHint(namePos, CPH_UnnamedEnumerationElement);
               Result := FUnifiedConstants.CreateEmptyString;
            end else if meth=emName then
               Result := TConstStringExpr.Create(FCompilerContext.TypString, element.Name)
            else Result := TConstStringExpr.Create(FCompilerContext.TypString, element.QualifiedName);
            OrphanAndNil(baseExpr);

         end else begin

            if meth=emName then
               Result:=TEnumerationElementNameExpr.Create(FCompilerContext, baseExpr)
            else Result:=TEnumerationElementQualifiedNameExpr.Create(FCompilerContext, baseExpr);
            RecordSymbolUse(baseExpr.Typ, namePos, [suRTTI, suImplicit]);

         end;

      end;
      emValue : begin

         if baseExpr.ClassType=TConstIntExpr then begin

            Result := FUnifiedConstants.CreateInteger(TConstIntExpr(baseExpr).Value);
            OrphanAndNil(baseExpr);

         end else begin

            Result:=TOrdIntExpr.Create(FCompilerContext, baseExpr);

         end;

      end;
   else

      Result:=nil;
      OrphanAndNil(baseExpr);
      FMsgs.AddCompilerStopFmt(namePos, CPE_UnknownMember, [name]);

   end;
end;

// ReadNameList
//
procedure TdwsCompiler.ReadNameList(names : TSimpleStringList; var posArray : TScriptPosArray;
                                    const options : TdwsNameListOptions = [];
                                    externalNames : TSimpleStringList = nil);
var
   n : Integer;
begin
   n:=0;
   names.Clear;
   if externalNames<>nil then
      externalNames.Clear;
   repeat
      if not FTok.TestName then begin
         if not ((nloAllowStrings in Options) and FTok.Test(ttStrVal)) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      end;

      if n=Length(posArray) then
         SetLength(posArray, n+1);
      posArray[n]:=FTok.HotPos;
      Inc(n);

      names.Add(FTok.GetToken.AsString);
      if not (nloNoCheckSpecials in options) then
         CheckSpecialName(FTok.GetToken.AsString);
      FTok.KillToken;

      while (nloAllowDots in options) and FTok.TestDelete(ttDOT) do begin
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
         names[names.Count-1]:=names[names.Count-1]+'.'+FTok.GetToken.AsString;
         FTok.KillToken;
      end;

      if externalNames<>nil  then begin
         if FTok.TestDelete(ttEXTERNAL) then begin
            if CurrentProg.Level<>0 then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ExternalVariablesMustBeGlobal);
            if FTok.Test(ttStrVal) then begin
               externalNames.Add(FTok.GetToken.AsString);
               FTok.KillToken;
            end else begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
               externalNames.Add('');
            end;
         end else externalNames.Add('');
      end;

   until not FTok.TestDelete(ttCOMMA);
end;

// ReadExternalName
//
procedure TdwsCompiler.ReadExternalName(funcSym : TFuncSymbol);
begin
   FTok.KillToken;
   if FTok.TestAny([ttSEMI, ttPROPERTY])=ttNone then begin
      if FTok.TestDelete(ttARRAY) then begin
         if     (funcSym is TMethodSymbol)
            and (not TMethodSymbol(funcSym).IsVirtual)
            and (not TMethodSymbol(funcSym).IsClassMethod)  then
            else FMsgs.AddCompilerError(FTok.HotPos, CPE_ExternalArrayForStaticMethodsOnly);
         funcSym.ExternalName:=SYS_EXTERNAL_ARRAY
      end else begin
         if not FTok.Test(ttStrVal) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_StringExpected)
         else begin
            funcSym.ExternalName:=FTok.GetToken.AsString;
            FTok.KillToken;
         end;
      end;
   end;
end;

// ReadNew
//
function TdwsCompiler.ReadNew(restrictTo : TClassSymbol; asAttribute : Boolean) : TProgramExpr;

   function FindAsAttribute(table : TSymbolTable; const name : String) : TSymbol;
   begin
      Result:=table.FindSymbol(name+'Attribute', cvPrivate);
      if Result<>nil then begin
         if (Result is TClassSymbol) and TClassSymbol(Result).IsAttribute then
            exit
         else Result:=nil;
      end;
   end;

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
      if typedExpr<>nil then begin
         try
            if typedExpr.Typ.UnAliasedTypeIs(TClassOfSymbol) and (typedExpr is TDataExpr) then begin
               baseExpr:=TDataExpr(typedExpr);
               classSym:=TClassOfSymbol(typedExpr.Typ.UnAliasedType).TypClassSymbol;
            end else FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);
         except
            OrphanAndNil(typedExpr);
            raise;
         end;
      end else begin
         // error was already registered, attempt to keep compiling
         classSym := FCompilerContext.TypTObject;
         baseExpr := TBogusConstExpr.CreateNull(classSym);
      end;
      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerError(hotPos, CPE_BrackRightExpected);

   end else begin

      // Get name
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ClassRefExpected);

      nameToken:=FTok.GetToken;
      hotPos:=FTok.HotPos;
      if asAttribute then
         sym:=FindAsAttribute(CurrentProg.Table, nameToken.AsString)
      else sym:=nil;
      if sym=nil then
         sym:=CurrentProg.Table.FindSymbol(nameToken.AsString, cvPrivate);
      FTok.KillToken;

      if (sym <> nil) and (sym.ClassType = TGenericSymbol) then
         sym := ReadSpecializedType(TGenericSymbol(sym));

      if FTok.TestDelete(ttALEFT) then begin

         if Assigned(sym) and sym.IsType then begin

            typSym:=TTypeSymbol(sym);
            RecordSymbolUse(typSym, hotPos, [suReference]);
            Result:=ReadNewArray(typSym);

         end else begin

            FMsgs.AddCompilerError(hotPos, CPE_TypeExpected);
            Result:=ReadNewArray(FCompilerContext.TypVariant);

         end;
         Exit;

      end else begin

         if sym is TAliasSymbol then
            sym:=TAliasSymbol(sym).UnAliasedType;

         if sym is TClassSymbol then begin

            classSym:=TClassSymbol(sym);
            RecordSymbolUse(classSym, hotPos, [suReference]);

         end else if (sym is TDataSymbol) and (sym.Typ is TClassOfSymbol) then begin

            classSym:=TClassOfSymbol(sym.Typ).TypClassSymbol;
            RecordSymbolUseReference(sym, hotPos, False);

         end else FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);

      end;

      if sym is TClassSymbol then
         baseExpr:=TConstExpr.Create(classSym.MetaSymbol, Int64(classSym))
      else baseExpr:=TVarExpr.CreateTyped(FCompilerContext, TDataSymbol(sym));

   end;

   WarnDeprecatedType(hotPos, classSym);

   if classSym.IsStatic then begin
      OrphanAndNil(baseExpr);
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_ClassIsStaticNoInstantiation, [classSym.Name]);
      Result:=TConstExpr.Create(classSym, 0);
      Exit;
   end;
   if (restrictTo<>nil) and not classSym.IsOfType(restrictTo) then
      FMsgs.AddCompilerErrorFmt(hotPos, CPE_MustBeSubClassOf, [restrictTo.Name]);

   overloads:=nil;
   methSym:=classSym.FindDefaultConstructor(cvPrivate);
   if methSym = nil then begin
      OrphanAndNil(baseExpr);
      FMsgs.AddCompilerStopFmt(hotPos, CPE_NoDefaultConstructor, [classSym.Name])
   end else if methSym.IsOverloaded then
      overloads:=TFuncSymbolList.Create;
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
         Result:=GetMethodExpr(methSym, baseExpr, rkClassOfRef, hotPos, []);
      except
         OrphanAndNil(baseExpr);
         raise;
      end;
      try
         ReadFuncArgs(TFuncExpr(Result), argPosArray, overloads);
         (Result as TMethodExpr).Typ:=classSym;
         if overloads<>nil then begin
            funcExpr:=(Result as TFuncExpr);
            if not ResolveOverload(funcExpr, overloads, argPosArray, nil, []) then Exit;
            Result:=funcExpr;
         end;
         TypeCheckArguments(FCompilerContext, TFuncExpr(Result), argPosArray);
      except
         OrphanAndNil(Result);
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
   newExpr:=TNewArrayExpr.Create(FCompilerContext, FTok.HotPos, elementTyp);
   try
      repeat
         FTok.HasTokens;
         hotPos:=FTok.HotPos;
         lengthExpr:=ReadExpr;
         newExpr.AddLengthExpr(lengthExpr, FCompilerContext.TypInteger);
         if not (lengthExpr.IsOfType(FCompilerContext.TypInteger)) then
            FMsgs.AddCompilerError(hotPos, CPE_IntegerExpressionExpected);
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
   except
      newExpr.Orphan(FCompilerContext);
      raise;
   end;
   Result:=newExpr;
end;

// ReadAliasedNameSymbol
//
function TdwsCompiler.ReadAliasedNameSymbol(var namePos : TScriptPos) : TSymbol;
var
   name : String;
   unitSym : TUnitSymbol;
begin
   // Declaration of a class reference
   if not FTok.TestDeleteNamePos(name, namePos) then begin
      namePos:=FTok.HotPos;
      FMsgs.AddCompilerError(namePos, CPE_NameExpected);
      FTok.KillToken;
      Result:=nil;
      Exit;
   end;

   Result:=CurrentProg.Table.FindTypeSymbol(name, cvMagic);

   if Result=nil then begin

      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnknownName, [name]);

   end else if (Result.BaseType<>nil) and (Result.BaseType.ClassType=TUnitSymbol) then begin

      RecordSymbolUse(Result, namePos, [suReference]);

      unitSym:=TUnitSymbol(Result.BaseType);
      unitSym:=ResolveUnitNameSpace(namePos, unitSym);

      namePos:=FTok.HotPos;
      Result:=unitSym.Table.FindLocal(FTok.GetToken.AsString);

      if not Assigned(Result) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_UnknownNameDotName,
                                   [unitSym.Name, FTok.GetToken.AsString]);

      FTok.KillToken;
   end;
end;

// ReadNameSymbol
//
function TdwsCompiler.ReadNameSymbol(var namePos : TScriptPos) : TSymbol;
begin
   Result:=ReadAliasedNameSymbol(namePos);
   if (Result<>nil) and (Result.ClassType=TAliasSymbol) then
      Result:=TAliasSymbol(Result).UnAliasedType;
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
      Result:=FCompilerContext.TypTObject; // keep compiling
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

// ReadClassDeck
//
function TdwsCompiler.ReadClassDecl(const typeName : String; const flags : TClassSymbolFlags;
                                    allowNonConstExpressions : Boolean) : TClassSymbol;

   procedure CheckAndSetForwardDecl;
   begin
      if typeName='' then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_AnonymousClassesMustBeFullyDefined);
      if Result.IsForwarded then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassForwardAlreadyExists, [Result.Name])
      else if csfPartial in flags then
         Result.SetIsPartial;
      Result.SetForwardedPos(FTok.HotPos);
   end;


var
   namePos, hotPos : TScriptPos;
   sym, typ : TSymbol;
   ancestorTyp : TClassSymbol;
   intfTyp : TInterfaceSymbol;
   interfaces : TList;
   missingMethod : TMethodSymbol;
   isInSymbolTable, firstVisibilityToken : Boolean;
   previousClassFlags  : TClassSymbolFlags;
   visibility : TdwsVisibility;
   tt : TTokenType;
   i : Integer;
begin
   Result:=nil;

   // Check for a forward declaration of this class
   if typeName<>'' then begin
      if csfPartial in flags then
         sym:=CurrentProg.Table.FindTypeSymbol(typeName, cvMagic)
      else sym:=CurrentProg.Table.FindTypeLocal(typeName);

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
   end;

   isInSymbolTable:=Assigned(Result);

   if isInSymbolTable then begin
      previousClassFlags:=Result.Flags;
      if (csfPartial in flags) and not CurrentProg.Table.HasSymbol(Result) then begin
         CurrentProg.Table.AddSymbolDirect(Result);
         Result.IncRefCount;
      end;
   end else begin
      Result:=TClassSymbol.Create(typeName, CurrentUnitSymbol);
      previousClassFlags:=[];
   end;

   // forwarded declaration
   if FTok.Test(ttSEMI) then begin
      CheckAndSetForwardDecl;
      Exit;
   end else Result.ClearIsForwarded;

   if not isInSymbolTable then
      CurrentProg.Table.AddSymbol(Result);   // auto-forward
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
               Result.ExternalName:=FTok.GetToken.AsString;
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
         if FTok.Test(ttSEMI) then begin
            CheckAndSetForwardDecl;
            Exit;
         end;

         // inheritance
         if FTok.TestDelete(ttBLEFT) then begin

            typ := ReadNameSymbol(namePos);
            if typ is  TGenericSymbol then
               typ := ReadSpecializedType(TGenericSymbol(typ));

            if not (typ is TClassSymbol) then begin
               if typ is TInterfaceSymbol then
                  interfaces.Add(typ)
               else if typ<>nil then
                  FMsgs.AddCompilerErrorFmt(namePos, CPE_NotAClass, [typ.name]);
               typ:=FCompilerContext.TypTObject;
            end;
            RecordSymbolUse(typ, namePos, [suReference]);

            ancestorTyp:=TClassSymbol(typ);
            WarnDeprecatedType(namePos, ancestorTyp);

            if ancestorTyp.IsForwarded or (ancestorTyp=Result) then begin
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassNotCompletelyDefined, [ancestorTyp.Name]);
               ancestorTyp:=FCompilerContext.TypTObject;
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

            if (csfPartial in previousClassFlags) and (Result.Parent<>nil) then
               ancestorTyp:=Result.Parent
            else if Result.IsExternal or (typeName='') then begin
               if Assigned(FOnRootExternalClass) then
                  ancestorTyp:=FOnRootExternalClass(Self, Result.ExternalName)
               else ancestorTyp:=FCompilerContext.TypObject;
            end else ancestorTyp:=FCompilerContext.TypTObject;

         end;

         if     Result.IsExternal
            and (ancestorTyp<>FCompilerContext.TypObject)
            and (ancestorTyp.ExternalRoot=nil) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassExternalAncestorMustBeExternalOrObject);

         if     Result.IsStatic
            and (ancestorTyp<>FCompilerContext.TypTObject)
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
         firstVisibilityToken:=True;

         // standard class definition
         if not FTok.Test(ttSEMI) then begin

            while not FTok.Test(ttEND) do begin

               // Read methods and properties
               hotPos:=FTok.HotPos;
               tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
                                       ttCONSTRUCTOR, ttDESTRUCTOR, ttOPERATOR,
                                       ttCLASS, ttPROPERTY, ttCONST,
                                       ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
                                       ttALEFT]);
               case tt of

                  ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR :
                     ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, False);

                  ttCLASS : begin

                     tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD,
                                             ttOPERATOR, ttVAR, ttCONST, ttPROPERTY]);
                     case tt of
                        ttPROCEDURE, ttFUNCTION, ttMETHOD :
                           ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, True);
                        ttOPERATOR :
                           Result.AddOperator(ReadClassOperatorDecl(Result));
                        ttVAR :
                           ReadClassVars(Result, visibility);
                        ttCONST :
                           ReadClassConst(Result, visibility);
                        ttPROPERTY :
                           ReadPropertyDecl(Result, visibility, True);
                     else
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
                     end;

                  end;
                  ttPROPERTY : begin

                     ReadPropertyDecl(Result, visibility, False);

                  end;
                  ttOPERATOR : begin

                     Result.AddOperator(ReadClassOperatorDecl(Result));

                  end;
                  ttCONST : begin

                     ReadClassConst(Result, visibility);

                  end;
                  ttPRIVATE..ttPUBLISHED : begin

                     if visibility=cTokenToVisibility[tt] then begin
                        if not firstVisibilityToken then
                           FMsgs.AddCompilerHintFmt(FTok.HotPos, CPH_RedundantVisibilitySpecifier, [cTokenStrings[tt]], hlStrict)
                     end else visibility:=cTokenToVisibility[tt];
                     firstVisibilityToken:=False;

                  end;
                  ttALEFT : begin
                     ReadAttributes(True);
                  end;

               else

                  ReadFieldsDecl(Result, visibility, allowNonConstExpressions, False);
                  if not (FTok.TestDelete(ttSEMI) or FTok.Test(ttEND)) then
                     Break;

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

      except
         // Set Result to nil to prevent auto-forward removal then re-reraise
         if not isInSymbolTable then
            Result:=nil;
         raise;
      end;
   finally
      interfaces.Free;
      if not isInSymbolTable then
         CurrentProg.Table.Remove(Result);  // auto-forward
   end;
end;

// ReadClassVars
//
procedure TdwsCompiler.ReadClassVars(const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility);
var
   factory : IdwsDataSymbolFactory;
begin
   factory:=TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
   ReadVarDecl(factory, CurrentProg.InitExpr);
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

// ReadClassConstSymbol
//
function TdwsCompiler.ReadClassConstSymbol(
      const constPos : TScriptPos; typ : TTypeSymbol;
      const ownerSymbol : TCompositeTypeSymbol; aVisibility : TdwsVisibility) : TConstSymbol;
var
   factory : IdwsDataSymbolFactory;
begin
   factory:=TCompositeTypeSymbolFactory.Create(Self, ownerSymbol, aVisibility);
   Result:=ReadConstSymbol('', constPos, typ, factory);
end;

// ReadInterface
//
function TdwsCompiler.ReadInterface(const typeName : String) : TInterfaceSymbol;
var
   sym : TSymbol;
   ancestor : TInterfaceSymbol;
   namePos, hotPos : TScriptPos;
   tt : TTokenType;
   wasForwarded : Boolean;
begin
   hotPos:=FTok.HotPos;
   sym:=CurrentProg.Table.FindSymbol(typeName, cvMagic);

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
         CurrentProg.Table.AddSymbol(Result);
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
            Result.InheritFrom(FCompilerContext.TypInterface);

         if FTok.TestDelete(ttALEFT) then begin
            // accept but ignore GUID
            if not FTok.TestDelete(ttStrVal) then begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
               FTok.SkipTo(ttARIGHT);
            end;
            if not FTok.TestDelete(ttARIGHT) then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ArrayBracketRightExpected);
         end;

         while not FTok.Test(ttEND) do begin

            // Read methods and properties
            tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttPROPERTY]);
            case tt of
               ttFUNCTION, ttPROCEDURE, ttMETHOD :
                  Result.AddMethod(ReadIntfMethodDecl(Result, cTokenToFuncKind[tt]));
               ttPROPERTY :
                  ReadPropertyDecl(Result, cvPublished, False);
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
            CurrentProg.Table.Remove(Result);
      end;
   except
      OrphanObject(Result);
      raise;
   end;
end;

// CheckPropertyFuncParams
//
function TdwsCompiler.CheckPropertyFuncParams(paramsA : TParamsSymbolTable; methSym : TMethodSymbol;
                                      indexSym : TSymbol = nil;
                                      typSym : TTypeSymbol = nil) : Boolean;
var
   paramsB : TParamsSymbolTable;
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
         if not Result.UsesSym.Params[0].Typ.IsOfType(Result.Typ) then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidParameterType, [Result.UsesSym.Name]);
         if not Result.UsesSym.Result.Typ.IsOfType(FCompilerContext.TypBoolean) then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidResultType, [Result.UsesSym.Result.Typ.Name]);
      end else begin
         if Result.UsesSym.Params[0].Typ<>Result.Typ then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidParameterType, [Result.UsesSym.Name]);
      end;

      ReadSemiColon;
   except
      OrphanObject(Result);
      raise;
   end;
end;

// ReadPropertyDecl
//
procedure TdwsCompiler.ReadPropertyDecl(ownerSym : TCompositeTypeSymbol; aVisibility : TdwsVisibility;
                                        classProperty : Boolean);
var
   gotReadOrWrite : Boolean;
   name : String;
   propSym, promotedPropSym  : TPropertySymbol;
   sym : TSymbol;
   typ : TTypeSymbol;
   tempArrayIndices : TParamsSymbolTable;
   propStartPos, propNamePos : TScriptPos;
   accessPos : TScriptPos;  // Position where either a Read or Write symbol is found
   indexExpr : TTypedExpr;
   indexTyp : TTypeSymbol;
begin
   propStartPos:=FTok.HotPos;

   // Read property name
   if not FTok.TestDeleteNamePos(name, propNamePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   // Check if property name is available
   sym := ownerSym.Members.FindSymbolFromScope(name, ownerSym);
   if Assigned(sym) then begin
      if sym is TPropertySymbol then begin
         if TPropertySymbol(sym).OwnerSymbol = ownerSym then begin
            MemberSymbolWithNameAlreadyExists(sym, propNamePos);
            name:=''; // make anonymous to keep compiling
         end;
      end else if ownerSym.Members.HasSymbol(sym) then begin
         MemberSymbolWithNameAlreadyExists(sym, propNamePos);
         name:=''; // make anonymous to keep compiling
      end;
   end;

   if (not classProperty) and FTok.TestDelete(ttSEMI) then begin
      // property visibility promotion
      if sym=nil then
         FMsgs.AddCompilerErrorFmt(propNamePos, CPE_UnknownMember, [name])
      else if not (sym is TPropertySymbol) then
         FMsgs.AddCompilerErrorFmt(propNamePos, CPE_NotAProperty, [sym.Name])
      else begin
         propSym:=TPropertySymbol(sym);
         if propSym.Visibility>aVisibility then
            FMsgs.AddCompilerError(propNamePos, CPE_CannotDemotePropertyVisibility)
         else begin
            promotedPropSym:=TPropertySymbol.Create(propSym.Name, propSym.Typ, aVisibility,
                                                    propSym.ArrayIndices);
            propSym.ArrayIndices.IncRefCount;
            promotedPropSym.ReadSym:=propSym.ReadSym;
            promotedPropSym.WriteSym:=propSym.WriteSym;
            ownerSym.AddProperty(promotedPropSym);
         end;
      end;
      Exit;
   end;

   if FTok.TestDelete(ttALEFT) then begin
      tempArrayIndices:=TParamsSymbolTable.Create;
      try
         ReadArrayParams(tempArrayIndices);
      except
         OrphanObject(tempArrayIndices);
         raise;
      end;
   end else tempArrayIndices:=nil;

   if not FTok.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

   typ:=ReadType('', tcProperty);

   propSym:=TPropertySymbol.Create(name, typ, aVisibility, tempArrayIndices);
   ownerSym.AddProperty(propSym);

   RecordSymbolUse(propSym, propNamePos, [suDeclaration]);

   if FTok.TestDelete(ttEXTERNAL) then begin
      if not FTok.Test(ttStrVal) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected)
      else begin
         propSym.ExternalName := FTok.GetToken.AsString;
         FTok.KillToken;
      end;
   end;

   if FTok.TestDelete(ttINDEX) then begin
      indexExpr:=ReadExpr;
      indexTyp:=indexExpr.Typ;
      if not (indexExpr is TConstExpr) then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
      end else begin
         propSym.SetIndex(TConstExpr(indexExpr).Data, indexTyp);
      end;
      OrphanAndNil(indexExpr);
   end else indexTyp:=nil;

   gotReadOrWrite:=False;

   if     FTok.Test(ttSEMI)
      and ownerSym.AllowFields then begin

      gotReadOrWrite:=True;

      // shorthand with anonymous private field
      ReadPropertyDeclAutoField(propSym, classProperty);

   end else begin

      if FTok.TestDelete(ttREAD) then begin

         gotReadOrWrite:=True;

         sym:=ReadPropertyDeclGetter(propSym, accessPos, classProperty);

         if sym=nil then

            // error already handled

         else if (not sym.Typ.IsOfType(propSym.Typ)) and not (sym.IsGeneric or propSym.IsGeneric) then begin

            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleType, [sym.Name])

         end else if sym is TMethodSymbol then begin

            if classProperty and not TMethodSymbol(sym).IsClassMethod then
               FMsgs.AddCompilerError(accessPos, CPE_ClassMethodExpected);
            if not CheckPropertyFuncParams(propSym.ArrayIndices, TMethodSymbol(sym), indexTyp) then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleParameters, [sym.Name]);

         end else if propSym.HasArrayIndices then begin

            FMsgs.AddCompilerError(FTok.HotPos, CPE_FunctionMethodExpected);

         end else if classProperty and (sym.ClassType=TFieldSymbol) then begin

            FMsgs.AddCompilerError(accessPos, CPE_ClassMemberExpected);

         end;

         propSym.ReadSym := sym;
         RecordSymbolUse(sym, accessPos, [suReference, suRead]);

      end;

      if FTok.TestDelete(ttWRITE) then begin

         gotReadOrWrite:=True;
         sym:=ReadPropertyDeclSetter(propSym, accessPos, classProperty);

         if sym=nil then

            // error already handled

         else if sym is TMethodSymbol then begin

            if classProperty and not TMethodSymbol(sym).IsClassMethod then
               FMsgs.AddCompilerError(accessPos, CPE_ClassMethodExpected);
            if    (not (TMethodSymbol(sym).Kind in [fkProcedure, fkMethod]))
               or (TMethodSymbol(sym).Typ<>nil) then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ProcedureMethodExpected)
            else if not CheckPropertyFuncParams(propSym.ArrayIndices, TMethodSymbol(sym), indexTyp, propSym.Typ) then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleParameters, [sym.Name]);

         end else if propSym.Typ <> sym.Typ then begin

            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleWriteSymbol, [sym.Name]);

         end else if sym is TConstSymbol then  begin

            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConstantCannotBeWrittenTo, [sym.Name]);

         end else if classProperty and (sym.ClassType=TFieldSymbol) then begin

            FMsgs.AddCompilerError(accessPos, CPE_ClassMemberExpected);

         end;

         propSym.WriteSym := sym;
         RecordSymbolUse(sym, accessPos, [suReference, suWrite]);
      end;

   end;

   if not gotReadOrWrite then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ReadOrWriteExpected, [name]);

   if FTok.TestDelete(ttDEFAULT) then begin

      propSym.DefaultSym:=ReadClassConstSymbol(FTok.HotPos, propSym.Typ, ownerSym, aVisibility);
      if propSym.DefaultSym<>nil then
         propSym.DefaultSym.IncRefCount;

   end;

   ReadSemiColon;

   // Array-Prop can be default
   if propSym.HasArrayIndices then begin
      if FTok.TestDelete(ttDEFAULT) then begin
         ReadSemiColon;
         if not ownerSym.AllowDefaultProperty then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NoDefaultPropertyAllowed)
         else if ownerSym.DefaultProperty<>nil then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MultipleDefaultProperties,
                                      [ownerSym.Name, ownerSym.DefaultProperty.Name])
         else ownerSym.DefaultProperty:=propSym;
      end;
   end;

   if FTok.Test(ttDEPRECATED) then
      propSym.DeprecatedMessage:=ReadDeprecatedMessage;

   // register context only if we're sure the property symbol will survive
   if coContextMap in FOptions then begin
      FSourceContextMap.OpenContext(propStartPos, propSym, ttPROPERTY);
      FSourceContextMap.CloseContext(FTok.CurrentPos);
   end;
end;

// ReadPropertyDeclAutoField
//
procedure TdwsCompiler.ReadPropertyDeclAutoField(propSym : TPropertySymbol; classProperty : Boolean);
var
   field : TFieldSymbol;
   classVar : TDataSymbol;
   sym : TSymbol;
   factory : IdwsDataSymbolFactory;
   assignExpr : TProgramExpr;
   initExpr : TTypedExpr;
begin
   if classProperty then begin
      factory:=TCompositeTypeSymbolFactory.Create(Self, propSym.OwnerSymbol, propSym.Visibility);
      if FTok.TestDeleteAny([ttEQ, ttASSIGN])<>ttNone then
         initExpr:=factory.ReadInitExpr(propSym.Typ)
      else initExpr:=nil;
      assignExpr:=CreateNamedVarDeclExpr(factory, '', '', FTok.HotPos, propSym.Typ, initExpr, classVar);
      if assignExpr<>nil then
         CurrentProg.InitExpr.AddStatement(assignExpr);
      sym:=classVar;
   end else begin
      field:=TFieldSymbol.Create('', propSym.Typ, cvPrivate);
      field.ExternalName:=propSym.Name;
      propSym.OwnerSymbol.AddField(field);
      sym:=field;
   end;
   propSym.ReadSym:=sym;
   propSym.WriteSym:=sym;
end;

// ReadPropertyDeclGetter
//
function TdwsCompiler.ReadPropertyDeclGetter(
      propSym : TPropertySymbol; var scriptPos : TScriptPos; classProperty : Boolean) : TSymbol;
var
   name : String;
   expr : TTypedExpr;
   resultExpr : TVarExpr;
   meth : TMethodSymbol;
   oldProg : TdwsProgram;
   proc : TdwsProcedure;
begin
   if not FTok.TestDelete(ttBLEFT) then begin

      if not FTok.TestDeleteNamePos(name, scriptPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      Result:=propSym.OwnerSymbol.Members.FindSymbol(name, cvPrivate);

      if (Result<>nil) and (Result.ClassType=TPropertySymbol) then begin
         Result:=TPropertySymbol(Result).ReadSym;
         if Result=nil then begin
            FMsgs.AddCompilerError(scriptPos, CPE_WriteOnlyProperty);
            Exit;
         end;
      end;

   end else begin

      scriptPos:=FTok.HotPos;

      if not propSym.OwnerSymbol.AllowAnonymousMethods then
         FMsgs.AddCompilerError(scriptPos, CPE_AnonymousMethodsNotAllowedHere);
      meth:=propSym.OwnerSymbol.CreateAnonymousMethod(fkFunction, cvPrivate, classProperty);
      meth.Typ:=propSym.Typ;
      propSym.OwnerSymbol.AddMethod(meth);
      meth.AddParams(propSym.ArrayIndices);

      RecordSymbolUse(meth, scriptPos, [suDeclaration, suImplicit]);

      proc:=TdwsProcedure.Create(CurrentProg);
      proc.AssignTo(meth);

      oldProg:=CurrentProg;
      CurrentProg:=proc;
      try
         expr:=ReadExpr(propSym.Typ);
         if expr<>nil then begin
            resultExpr:=TVarExpr.CreateTyped(FCompilerContext, proc.Func.Result);
            proc.Expr:=CreateAssign(scriptPos, ttASSIGN, resultExpr, expr);
         end;
      finally
         CurrentProg:=oldProg;
      end;

      Result:=meth;

      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackRightExpected);
   end;

   if Result=nil then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);
end;

// ReadPropertyDeclSetter
//
function TdwsCompiler.ReadPropertyDeclSetter(
      propSym : TPropertySymbol; var scriptPos : TScriptPos; classProperty : Boolean) : TSymbol;
var
   name : String;
   instr : TProgramExpr;
   expr : TTypedExpr;
   leftExpr : TDataExpr;
   paramExpr : TVarExpr;
   meth : TMethodSymbol;
   paramSymbol : TParamSymbol;
   oldProg : TdwsProgram;
   proc : TdwsProcedure;
begin
   if not FTok.TestDelete(ttBLEFT) then begin

      if not FTok.TestDeleteNamePos(name, scriptPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      Result:=propSym.OwnerSymbol.Members.FindSymbol(name, cvPrivate);

      if (Result<>nil) and (Result.ClassType=TPropertySymbol) then begin
         Result:=TPropertySymbol(Result).WriteSym;
         if Result=nil then begin
            FMsgs.AddCompilerError(scriptPos, CPE_WriteOnlyProperty);
            Exit;
         end;
      end;

   end else begin

      scriptPos:=FTok.HotPos;

      if not propSym.OwnerSymbol.AllowAnonymousMethods then
         FMsgs.AddCompilerError(scriptPos, CPE_AnonymousMethodsNotAllowedHere);
      meth:=propSym.OwnerSymbol.CreateAnonymousMethod(fkProcedure, cvPrivate, classProperty);

      meth.AddParams(propSym.ArrayIndices);
      paramSymbol := CreateConstParamSymbol('Value', propSym.Typ);
      meth.Params.AddSymbol(paramSymbol);

      propSym.OwnerSymbol.AddMethod(meth);

      RecordSymbolUse(meth, scriptPos, [suDeclaration, suImplicit]);

      proc:=TdwsProcedure.Create(CurrentProg);
      proc.AssignTo(meth);

      oldProg:=CurrentProg;
      CurrentProg:=proc;
      FPendingSetterValueExpr:=nil;
      try
         FPendingSetterValueExpr := GetConstParamExpr(scriptPos, paramSymbol);
         instr:=ReadInstr;
         if instr is TNullExpr then
            FMsgs.AddCompilerWarning(scriptPos, CPW_PropertyWriterDoesNothing);
         if (instr is TVarExpr) or (instr is TFieldExpr) or (instr is TNoResultWrapperExpr) then begin
            if instr is TNoResultWrapperExpr then
               expr:=TNoResultWrapperExpr(instr).Expr
            else expr:=TTypedExpr(instr);
            if expr.Typ.IsOfType(propSym.Typ) then begin
               if expr<>nil then begin
                  TNoResultWrapperExpr(instr).Expr:=nil;
                  OrphanAndNil(instr);
               end;
               instr:=nil;
               if (expr is TDataExpr) and TDataExpr(expr).IsWritable then begin
                  leftExpr:=TDataExpr(expr);
                  paramExpr:=GetConstParamExpr(scriptPos, paramSymbol);
                  proc.Expr:=CreateAssign(scriptPos, ttASSIGN, leftExpr, paramExpr);
               end else begin
                  FMsgs.AddCompilerError(scriptPos, CPE_CantWriteToLeftSide);
                  OrphanAndNil(expr);
               end;
            end;
         end;
         if instr<>nil then
            proc.Expr:=instr;
      finally
         OrphanAndNil(FPendingSetterValueExpr);
         CurrentProg:=oldProg;
      end;

      Result:=meth;

      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackRightExpected);
   end;

   if Result=nil then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);
end;

// ReadRecordDecl
//
function TdwsCompiler.ReadRecordDecl(const typeName : String;
                                     allowNonConstExpressions : Boolean) : TRecordSymbol;
var
   meth : TMethodSymbol;
   hotPos : TScriptPos;
   visibility : TdwsVisibility;
   tt : TTokenType;
begin
   Result:=TRecordSymbol.Create(typeName, CurrentUnitSymbol);
   try
      CurrentProg.Table.AddSymbol(Result); // auto-forward
      try
         if typeName = '' then
            visibility := cvPublished
         else visibility := cvPublic;

         if FTok.TestDelete(ttEXTERNAL)then
            Result.SetIsExternal;

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
               ttPROPERTY :
                  ReadPropertyDecl(Result, visibility, False);
               ttFUNCTION, ttPROCEDURE, ttMETHOD : begin
                  meth:=ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, False);
                  if meth.IsForwarded then
                     FMsgs.AddCompilerError(hotPos, CPE_AnonymousRecordMethodsMustBeInline);
               end;
               ttCLASS : begin
                  tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttVAR, ttCONST, ttPROPERTY]);
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
                     ttPROPERTY :
                        ReadPropertyDecl(Result, visibility, True);
                  else
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
                  end;
               end;
               ttCONST :
                  ReadClassConst(Result, visibility);
            else
               if FTok.Test(ttEND) then
                  Break;

               if Result.Members.HasMethods then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_RecordFieldsMustBeBeforeMethods);

               ReadFieldsDecl(Result, visibility, allowNonConstExpressions, Result.IsExternal);

               if not FTok.TestDelete(ttSEMI) then
                  Break;
            end;
         until not FTok.HasTokens;
      finally
         CurrentProg.Table.Remove(Result);
      end;

      if not FTok.TestDelete(ttEND) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      if Result.Size=0 then
         FMsgs.AddCompilerError(FTok.HotPos, RTE_NoRecordFields)
      else;
      CheckNoPendingAttributes;

      Result.IsFullyDefined:=True;
   except
      OrphanObject(Result);
      raise;
   end;
end;

// ReadFieldsDecl
//
procedure TdwsCompiler.ReadFieldsDecl(struct : TStructuredTypeSymbol; visibility : TdwsVisibility;
                                      allowNonConstExpressions : Boolean; forceExternal : Boolean);
var
   x : Integer;
   names : TSimpleStringList;
   sym : TSymbol;
   member : TFieldSymbol;
   typ : TTypeSymbol;
   posArray : TScriptPosArray;
   expr : TTypedExpr;
   exprData : TData;
   exprDyn : TTypedExpr;
   detachTyp : Boolean;
   options : TdwsNameListOptions;
   factory : IdwsDataSymbolFactory;
begin
   names:=FStringListPool.Acquire;
   try
      options:=[];
      if struct.Name='' then
         Include(options, nloAllowStrings);
      if struct.IsExternal then
         Include(options, nloNoCheckSpecials);
      ReadNameList(names, posArray, options);

      if FTok.TestDelete(ttCOLON) then begin
         typ:=ReadType('', tcConstant);
      end else typ:=nil;

      exprDyn:=nil;
      if FTok.TestDeleteAny([ttEQ, ttASSIGN])<>ttNone then begin
         detachTyp:=False;
         if factory=nil then
            factory:=TCompositeTypeSymbolFactory.Create(Self, struct, cvPrivate);
         if (typ is TRecordSymbol) and FTok.Test(ttBLEFT) then begin
            exprData:=ReadConstRecord(TRecordSymbol(typ));
         end else begin
            expr:=factory.ReadExpr(typ);
            try
               if Assigned(typ) then begin
                  if not typ.IsCompatible(expr.typ) then
                     expr:=CompilerUtils.WrapWithImplicitConversion(FCompilerContext, expr, typ, FTok.HotPos);
               end else begin
                  typ:=expr.typ;
                  detachTyp:=(typ.Name='');
               end;
               if typ=FCompilerContext.TypNil then
                  if not (expr is TBogusConstExpr) then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_TypeCouldNotBeInferenced);

               if (typ=nil) or (expr=nil) then begin
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
                  if typ=nil then
                     typ:=FCompilerContext.TypVariant;
               end else if expr.IsConstant then begin
                  if not FMsgs.HasErrors then begin
                     SetLength(exprData, typ.Size);
                     case typ.Size of
                        0 : FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantInstruction);
                        1 : expr.EvalAsVariant(FExec, exprData[0]);
                     else
                        if expr.ClassType=TArrayConstantExpr then
                           TArrayConstantExpr(expr).Prepare(FCompilerContext, typ.Typ);
                        FExec.Stack.Push(typ.Size);
                        try
                           (expr as TDataExpr).DataPtr[FExec].CopyData(exprData, 0, typ.Size);
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
                  if not CurrentProg.Table.HasSymbol(typ) then
                     CurrentProg.Table.AddSymbol(typ);
                  expr.Typ:=nil;
               end;
               OrphanAndNil(expr);
            end;
         end;
      end else begin
         exprData:=nil;
         if typ=nil then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ColonExpected);
            typ:=FCompilerContext.TypVariant;
         end;
      end;
      if (typ=struct) and (typ.ClassType=TRecordSymbol) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_RecordTypeNotFullyDefined, [typ.Name]);

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
         if forceExternal then
            member.ExternalName := member.Name;

         // Add member symbols and positions
         RecordSymbolUse(member, posArray[x], [suDeclaration]);
      end;

      if FTok.TestDelete(ttSEMI) then begin

         if FTok.TestDelete(ttEXTERNAL) then begin

            if names.Count<>1 then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_OnlyOneFieldExpectedForExternal);

            if FTok.Test(ttStrVal) then begin
               if member<>nil then
                  member.ExternalName:=FTok.GetToken.AsString;
               FTok.KillToken;
            end else FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);

         end else FTok.SimulateToken(ttSEMI, FTok.HotPos);

      end;

   finally
      FStringListPool.Release(names);
   end;
end;

// ReadHelperDecl
//
function TdwsCompiler.ReadHelperDecl(const typeName : String; qualifierToken : TTokenType;
                                     isStrict : Boolean) : THelperSymbol;
var
   hotPos : TScriptPos;
   visibility : TdwsVisibility;
   tt : TTokenType;
   forType : TTypeSymbol;
   member : TSymbol;
begin
   if not FTok.TestDelete(ttFOR) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ForExpected);
   hotPos:=FTok.HotPos;
   forType:=ReadType('', tcHelper);
   if forType.AsFuncSymbol<>nil then
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

   Result:=THelperSymbol.Create(typeName, CurrentUnitSymbol, forType, CurrentProg.Table.Count);
   try
      Result.Strict:=isStrict;
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
            ttPROPERTY :
               ReadPropertyDecl(Result, visibility, False);
            ttFUNCTION, ttPROCEDURE, ttMETHOD :
               ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, False);
            ttCLASS : begin
               tt:=FTok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttVAR, ttCONST, ttPROPERTY]);
               case tt of
                  ttPROCEDURE, ttFUNCTION, ttMETHOD :
                     ReadMethodDecl(hotPos, Result, cTokenToFuncKind[tt], visibility, True);
                  ttVAR :
                     ReadClassVars(Result, visibility);
                  ttCONST :
                     ReadClassConst(Result, visibility);
                  ttPROPERTY :
                     ReadPropertyDecl(Result, visibility, True);
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

      for member in Result.Members do
         FHelperMemberNames.Add(member.Name);

   except
      OrphanObject(Result);
      raise;
   end;
end;

// ReadTry
//
function TdwsCompiler.ReadTry : TExceptionExpr;
var
   tryBlock : TProgramExpr;
   tt : TTokenType;
   wasExcept : Boolean;
   exceptExpr : TExceptExpr;
   finallyExpr : TFinallyExpr;
begin
   Result := nil;
   try
      wasExcept:=FIsExcept;
      FIsExcept:=False;
      try
         tryBlock:=ReadBlocks([ttFINALLY, ttEXCEPT], tt);
         if tt=ttEXCEPT then begin
            FIsExcept:=True;
            exceptExpr:=TExceptExpr.Create(tryBlock);
            Result:=exceptExpr;
            ReadExcept(exceptExpr, tt);
            tryBlock:=Result;
         end;
         if tt=ttFINALLY then begin
            finallyExpr:=TFinallyExpr.Create(tryBlock);
            Result:=finallyExpr;
            ReadFinally(finallyExpr);
         end;
      finally
         FIsExcept:=wasExcept;
      end;
   except
      if Result <> nil then
         Result.Orphan(FCompilerContext);
      raise;
   end;
end;

// ReadFinally
//
procedure TdwsCompiler.ReadFinally(finallyExpr : TFinallyExpr);
var
   tt : TTokenType;
begin
   FFinallyExprs.Push(True);
   try
      finallyExpr.HandlerExpr:=ReadBlocks([ttEND], tt);
   finally
      FFinallyExprs.Pop;
   end;
end;

// ReadExcept
//
procedure TdwsCompiler.ReadExcept(exceptExpr : TExceptExpr; var finalToken : TTokenType);
var
   doExpr : TExceptDoExpr;
   varName : String;
   classSym : TTypeSymbol;
begin
   if FTok.Test(ttON) then begin
      while FTok.TestDelete(ttON) do begin
         if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
         varName:=FTok.GetToken.AsString;
         FTok.KillToken;

         if not FTok.TestDelete(ttCOLON) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

         classSym:=ReadType('', tcExceptionClass);
         if not (classSym.BaseType is TClassSymbol) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassRefExpected);

         if not FTok.TestDelete(ttDO) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

         doExpr:=TExceptDoExpr.Create(FCompilerContext, FTok.HotPos);
         exceptExpr.AddDoExpr(DoExpr);

         doExpr.ExceptionTable.AddSymbol(TDataSymbol.Create(varName, ClassSym));
         CurrentProg.EnterSubTable(doExpr.ExceptionTable);
         try
            doExpr.DoBlockExpr:=ReadBlock;
         finally
            CurrentProg.LeaveSubTable;
         end;

         if FTok.TestAny([ttEND, ttELSE])=ttNone then
            ReadSemiColon;
      end;

      if FTok.TestDelete(ttELSE) then
         exceptExpr.ElseExpr:=ReadBlocks([ttEND, ttFINALLY], finalToken)
      else begin
         finalToken:=FTok.TestDeleteAny([ttEND, ttFINALLY]);
         if finalToken=ttNone then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      end;
   end else begin
      exceptExpr.HandlerExpr:=ReadBlocks([ttEND, ttFINALLY], finalToken);
   end;
end;

// ReadGenericParametersDecl
//
function TdwsCompiler.ReadGenericParametersDecl : IGenericParameters;
var
   name : String;
   hotPos : TScriptPos;
   tt : TTokenType;
   param : TGenericTypeParameterSymbol;
   constraintType : TTypeSymbol;
begin
   Result := TGenericParameters.Create;
   repeat
      if not FTok.TestDeleteNamePos(name, hotPos) then begin
         FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
         Break;
      end;
      if Result.Find(name) <> nil then
         FMsgs.AddCompilerErrorFmt(hotPos, CPE_NameAlreadyExists, [name]);
      param := TGenericTypeParameterSymbol.Create(nil, name);
      Result.Add(param);
      RecordSymbolUse(param, hotPos, [suDeclaration]);
      if FTok.TestDelete(ttCOLON) then begin
         repeat
            hotPos := FTok.HotPos;
            tt := FTok.TestDeleteAny([ttRECORD]);
            case tt of
               ttRECORD : begin
                  param.AddConstraint(TGenericConstraintRecord.Create(param));
               end;
            else
               constraintType := ReadType('', tcParameter);
               if constraintType <> nil then begin
                  param.AddConstraint(TGenericConstraintType.Create(param, constraintType));
               end else FMsgs.AddCompilerError(hotPos, CPE_UnsupportedGenericConstraint);
            end;
         until not FTok.TestDelete(ttCOMMA);
      end;
   until FTok.TestDeleteAny([ttSEMI, ttCOMMA]) = ttNONE;
   if Result.Count = 0 then
      Result := nil;
end;

// ReadSpecializedType
//
function TdwsCompiler.ReadSpecializedType(genericType : TGenericSymbol) : TTypeSymbol;
var
   value : TTypeSymbol;
   valueList : TUnSortedSymbolTable;
   valuePosList : TScriptPosArray;
   startPos, hotPos : TScriptPos;
   checkSuccessful : Boolean;
   n : Integer;
begin
   startPos := FTok.HotPos;
   if not FTok.TestDelete(ttLESS) then
      FMsgs.AddCompilerStop(startPos, CPE_GenericParametersListExpected);
   valueList := TUnSortedSymbolTable.Create;
   try
      checkSuccessful := True;
      repeat
         hotPos := FTok.HotPos;
         value := ReadType('', tcGeneric);
         if value <> nil then begin
            valueList.AddSymbol(value);
            n := Length(valuePosList);
            SetLength(valuePosList, n+1);
            valuePosList[n] := hotPos;
         end;
      until not FTok.TestDelete(ttCOMMA);
      if not FTok.TestDelete(ttGTR) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_GreaterExpected);
      if valueList.Count <> genericType.Parameters.Count then begin
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadNumberOfParameters,
                                   [genericType.Parameters.Count, valueList.Count]);
         checkSuccessful := False;
      end;

      if checkSuccessful then begin
         Result := genericType.SpecializationFor(
            startPos, CurrentUnitSymbol,
            valueList, valuePosList,
            FCompilerContext, FOperators
         );
      end else begin
         Result := genericType;
      end;
   finally
      valueList.Clear;
      valueList.Free;
   end;
end;

// CheckGenericParameters
//
procedure TdwsCompiler.CheckGenericParameters(genericType : TGenericSymbol);
var
   i : Integer;
   name : String;
   namePos : TScriptPos;
   p : TGenericTypeParameterSymbol;
begin
   if not FTok.TestDelete(ttLESS) then
      Msgs.AddCompilerError(FTok.HotPos, CPE_LesserExpected);
   for i := 0 to genericType.Parameters.Count-1 do begin
      if not FTok.TestDeleteNamePos(name, namePos) then begin
         Msgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
         Break;
      end;
      p := genericType.Parameters[i];
      if not SameText(p.Name, name) then begin
         Msgs.AddCompilerErrorFmt(namePos, CPE_X_ExpectedBut_Y_Found,
                                  [p.Name, name]);
      end else if p.Name <> name then begin
         FMsgs.AddCompilerHintFmt(namePos, CPH_CaseDoesNotMatchDeclaration,
                                  [name, p.Name], hlPedantic);
      end;
   end;
   if not FTok.TestDelete(ttGTR) then
      Msgs.AddCompilerError(FTok.HotPos, CPE_GreaterExpected);
end;

// ReadRaise
//
function TdwsCompiler.ReadRaise : TRaiseBaseExpr;
var
   exceptExpr : TTypedExpr;
   exceptObjTyp : TSymbol;
begin
   if FIsExcept and (FTok.TestAny([ttSEMI, ttEND, ttELSE, ttUNTIL, ttFINALLY, ttEXCEPT])<>ttNone) then
      Result:=TReraiseExpr.Create(FTok.HotPos)
   else begin
      exceptExpr:=ReadExpr;
      exceptObjTyp:=exceptExpr.Typ;
      if not (    (exceptObjTyp is TClassSymbol)
              and TClassSymbol(exceptObjTyp).IsOfType(FCompilerContext.TypException)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ExceptionObjectExpected);
      Result:=TRaiseExpr.Create(FCompilerContext, FTok.HotPos, exceptExpr);
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
      Result:=TExitExpr.Create(FTok.HotPos)
   else begin
      if not (CurrentProg is TdwsProcedure) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoResultRequired);
      gotParenthesis:=FTok.TestDelete(ttBLEFT);
      proc:=TdwsProcedure(CurrentProg);
      if proc.Func.Result=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoResultRequired);
      RecordSymbolUse(proc.Func.Result, exitPos, [suReference, suWrite]);
      leftExpr:=TVarExpr.CreateTyped(FCompilerContext, proc.Func.Result);
      try
         assignExpr:=ReadAssign(ttASSIGN, leftExpr) as TAssignExpr;
         try
            leftExpr:=nil;
            if gotParenthesis and not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
            Result:=TExitValueExpr.Create(FCompilerContext, exitPos, assignExpr);
         except
            assignExpr.Orphan(FCompilerContext);
            raise;
         end;
      except
         OrphanAndNil(leftExpr);
         raise;
      end;
   end;
end;

// ReadClassExpr
//
function TdwsCompiler.ReadClassExpr(ownerSymbol : TCompositeTypeSymbol; expecting : TTypeSymbol = nil) : TTypedExpr;
var
   exprTable : TExpressionSymbolTable;
   oldStructure : TCompositeTypeSymbol;
begin
   exprTable:=TExpressionSymbolTable.Create(ownerSymbol.Members);
   oldStructure:=FCurrentStructure;
   try
      FCurrentStructure:=ownerSymbol;
      exprTable.AddParent(CurrentProg.Table);
      CurrentProg.EnterSubTable(exprTable);
      try
         Result:=ReadExpr(expecting);
      finally
         CurrentProg.LeaveSubTable;
      end;
      // anonymous and implicit symbols might have been created,
      // transfer them to the regular table
      if exprTable.Count>0 then
         exprTable.TransferSymbolsTo(CurrentProg.Table);
   finally
      FCurrentStructure:=oldStructure;
      OrphanObject(exprTable);
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
         Result:=ReadClassDecl(typeName, flags, False)
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
   tt:=FTok.TestDeleteAny([ttARRAY, ttSET,
                           ttRECORD, ttCLASS, ttINTERFACE, ttHELPER,
                           ttBLEFT, ttENUM, ttFLAGS, ttPARTIAL, ttSTATIC, ttSTRICT,
                           ttPROCEDURE, ttFUNCTION, ttREFERENCE]);

   case tt of
      ttARRAY : begin
         Result:=ReadArrayType(typeName, typeContext);
         if typeName='' then
            RecordSymbolUse(Result, hotPos, [suReference, suImplicit]);
      end;

      ttSET : begin
         Result:=ReadSetOfType(typeName, typeContext);
         if typeName='' then
            RecordSymbolUse(Result, hotPos, [suReference, suImplicit]);
      end;

      ttRECORD :
         if FTok.TestDelete(ttHELPER) then
            Result:=ReadHelperDecl(typeName, ttRECORD, False)
         else begin
            Result:=ReadRecordDecl(typeName, False);
            if typeName='' then begin
               CurrentProg.Table.AddSymbol(Result);
               Result.IncRefCount;
            end;
            if typeName='' then
               RecordSymbolUse(Result, hotPos, [suReference, suImplicit]);
         end;

      ttCLASS : begin
         tt:=FTok.TestDeleteAny([ttOF, ttHELPER]);
         case tt of
            ttOF :
               Result:=ReadClassOf(typeName);
            ttHELPER :
               Result:=ReadHelperDecl(typeName, ttCLASS, False);
         else
            if typeContext=tcDeclaration then
               Result:=ReadClassDecl(typeName, [], False)
            else begin
               Result:=nil;
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);
            end;
         end;
         if typeName='' then
            RecordSymbolUse(Result, hotPos, [suReference, suImplicit]);
      end;

      ttPARTIAL, ttSTATIC : begin
         Result:=ReadClassFlags(tt);
      end;

      ttINTERFACE : begin
         hotPos:=FTok.HotPos;
         if FTok.TestDelete(ttHELPER) then
            Result:=ReadHelperDecl(typeName, ttINTERFACE, False)
         else begin
            if typeContext=tcDeclaration then
               Result:=ReadInterface(typeName)
            else begin
               Result:=nil;
               FMsgs.AddCompilerStop(hotPos, CPE_TypeExpected);
            end;
         end;
      end;

      ttHELPER : begin
         Result:=ReadHelperDecl(typeName, ttNone, False);
      end;

      ttSTRICT : begin
         if not FTok.TestDelete(ttHELPER) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_HelperExpected);
         Result:=ReadHelperDecl(typeName, ttNone, True);
      end;

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

      ttPROCEDURE, ttFUNCTION : begin
         Result:=ReadProcType(tt, hotPos);
      end;

   else

      if FTok.TestName then begin

         sym:=ReadAliasedNameSymbol(namePos);

         if not Assigned(sym) then begin
            // keep compiling
            Result:=FCompilerContext.TypVariant;
         end else if not sym.IsType then begin
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidType, [sym.Name]);
            Result:=FCompilerContext.TypVariant; // keep compiling
         end else if sym.ClassType = TGenericSymbol then begin
            Result := ReadSpecializedType(TGenericSymbol(sym));
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
               Result:=TConnectorSymbol(sym).SpecializeConnector(CurrentProg.Table, connectorQualifier);
               if Result=sym then
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConnectorCantBeSpecialized, [sym.Name])
               else if Result=nil then begin
                  FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConnectorInvalidSpecifier, [sym.Name, connectorQualifier]);
                  Result:=TTypeSymbol(sym);
               end;
            end;
         end else Result:=TTypeSymbol(sym);

         WarnDeprecatedType(hotPos, Result);

         // Create name symbol, e. g.: type a = integer;
         if typeName <> '' then
            Result:=TAliasSymbol.Create(typeName, Result);

         RecordSymbolUse(Result, namePos, [suReference]);

      end else begin

         FMsgs.AddCompilerError(FTok.HotPos, CPE_TypeExpected);
         // keep compiling
         Result:=TAliasSymbol.Create(typeName, FCompilerContext.TypVariant);

      end;

   end;

   // Ensure that unnamed symbols will be freed
   if (Result<>nil) and (Result.Name='') then
      CurrentProg.RootTable.AddToDestructionList(Result);
end;

// ReadTypeGenericDecl
//
function TdwsCompiler.ReadTypeGenericDecl(const typeName : String; typeContext : TdwsReadTypeContext;
                                          const genericParameters : IGenericParameters = nil) : TTypeSymbol;
var
   genericPos : TScriptPos;
   genericSymbol : TGenericSymbol;
   specializeMethod : TSpecializationMethod;
   genericExists : Boolean;
begin
   Result := nil;
   genericPos := FTok.HotPos;
   genericExists := (CurrentProg.Table.FindLocal(typeName) <> nil);
   Assert(not genericExists);

   genericSymbol := TGenericSymbol.Create(typeName, genericParameters);
   EnterGeneric(genericSymbol);
   try
      Result := ReadType(genericSymbol.Caption, typeContext);
   finally
      LeaveGeneric;
      if Result <> nil then begin
         specializeMethod := Result.SpecializeType;
         if TMethod(specializeMethod).Code = @TTypeSymbol.SpecializeType then
            FMsgs.AddCompilerErrorFmt(genericPos, CPE_GenericityNotSupportedYet, [Result.ClassName]);
      end;
      genericSymbol.GenericType := Result;
      Result := genericSymbol;
   end;
end;

// EnterGeneric
//
procedure TdwsCompiler.EnterGeneric(generic : TGenericSymbol);
begin
   CurrentProg.Table.AddSymbol(generic);   // auto-forward
   CurrentProg.Table.InsertParent(0, generic.Parameters.List);
   FGenericSymbol.Push(generic);
end;

// LeaveGeneric
//
procedure TdwsCompiler.LeaveGeneric;
var
   sym : TGenericSymbol;
begin
   sym := FGenericSymbol.Peek;
   FGenericSymbol.Pop;
   CurrentProg.Table.RemoveParent(sym.Parameters.List);
   CurrentProg.Table.Remove(sym);  // auto-forward
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
                                 ttIN, ttIS, ttIMPLEMENTS, ttIMPLIES]);
         case tt of
            ttNone :
               Break;
            ttIN :
               Result := ReadExprIn(Result);
         else

            hotPos := FTok.HotPos;

            // Read right argument
            right:=ReadExprAdd;
            if right=nil then
               rightTyp:=nil
            else rightTyp:=right.Typ;
            try
               case tt of
                  ttIS : begin

                     if not (Result.Typ is TClassSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                     else if not (rightTyp is TClassOfSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected)
                     else if not (   TClassSymbol(rightTyp.Typ).IsOfType(Result.Typ)
                                  or TClassSymbol(Result.Typ).IsOfType(rightTyp.Typ)) then
                        IncompatibleTypesWarn(hotPos, CPE_IncompatibleTypes, Result.Typ, rightTyp.Typ);
                     Result:=TIsOpExpr.Create(FCompilerContext, hotPos, tt, Result, right)

                  end;
                  ttIMPLEMENTS : begin

                     if not (rightTyp is TInterfaceSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                     if Result.Typ is TClassOfSymbol then
                        Result:=TClassImplementsIntfOpExpr.Create(FCompilerContext, hotPos, tt, Result, right)
                     else begin
                        if not (Result.Typ is TClassSymbol) then
                           FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
                        Result:=TImplementsIntfOpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                     end;

                  end;
               else
                  opExpr:=CreateTypedOperatorExpr(tt, hotPos, Result, right);
                  if opExpr=nil then begin
                     if     ((tt=ttEQ) or (tt=ttNOTEQ))
                        and (rightTyp<>nil)
                        and (
                                (Result.Typ is TClassSymbol)
                             or (Result.Typ is TInterfaceSymbol)
                             or (Result.Typ is TClassOfSymbol)
                             or (Result.Typ=FCompilerContext.TypNil)
                             ) then begin
                        if not ((rightTyp.ClassType=Result.Typ.ClassType) or (rightTyp=FCompilerContext.TypNil)) then begin
                           if Result.Typ is TClassSymbol then
                              FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                           else if Result.Typ is TClassOfSymbol then
                              FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected)
                           else FMsgs.AddCompilerError(hotPos, CPE_InterfaceExpected);
                        end;
                        if Result.Typ is TClassSymbol then
                           if tt=ttNOTEQ then
                              Result:=TObjCmpNotEqualExpr.Create(FCompilerContext, hotPos, tt, Result, right)
                           else Result:=TObjCmpEqualExpr.Create(FCompilerContext, hotPos, tt, Result, right)
                        else if Result.Typ is TClassOfSymbol then begin
                           Result:=TAssignedMetaClassExpr.Create(FCompilerContext, Result);
                           if tt=ttEQ then
                              Result:=TNotBoolExpr.Create(FCompilerContext, Result);
                           OrphanAndNil(right);
                        end else begin
                           Result:=TIntfCmpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                           if tt=ttNOTEQ then
                              Result:=TNotBoolExpr.Create(FCompilerContext, Result);
                        end;
                     end else if     ((tt=ttEQ) or (tt=ttNOTEQ))
                                 and (rightTyp=FCompilerContext.TypNil)
                                 and (Result.Typ.IsOfType(FCompilerContext.TypVariant)) then begin
                        if tt=ttEQ then
                           Result:=TRelVarEqualNilExpr.Create(FCompilerContext, Result)
                        else Result:=TRelVarNotEqualNilExpr.Create(FCompilerContext, Result);
                        OrphanAndNil(right);
                     end else if (Result<>nil) and (right<>nil) and (Result.IsGeneric or right.IsGeneric) then begin
                        Result := TGenericBinaryOpExpr.Create(
                           FCompilerContext, hotPos,
                           tt, Result, right
                        );
                     end else begin
                        FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                        Result:=TRelOpExpr.Create(FCompilerContext, hotPos, tt, Result, right); // keep going
                     end;
                  end else Result:=opExpr;
               end;
            except
               OrphanAndNil(right);
               raise;
            end;
         end;
      until False;
   except
      OrphanAndNil(Result);
      raise;
   end;
end;

// ReadExprAdd
//
function TdwsCompiler.ReadExprAdd(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
var
   right : TTypedExpr;
   tt : TTokenType;
   hotPos : TScriptPos;
   opExpr : TTypedExpr;
begin
   // Read left argument
   if leftExpr = nil then
      Result := ReadExprMult(expecting)
   else Result := leftExpr;
   try

      repeat
         tt := FTok.TestDeleteAny([ttPLUS, ttMINUS, ttOR, ttXOR, ttNOT]);
         if tt = ttNone then Break;

         hotPos := FTok.HotPos;

         if tt = ttNOT then begin

            if not FTok.TestDelete(ttIN) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_InExpected);
            Result:=ReadExprIn(Result);
            Result:=TNotBoolExpr.Create(FCompilerContext, Result);

         end else begin

            // Read right argument
            right := ReadExprMult;
            try
               // Generate function and add left and right argument
               if right=nil then begin
                  opExpr:=nil;
               end else if (Result.Typ=nil) or (right.Typ=nil) then begin
                  FMsgs.AddCompilerError(hotPos, CPE_IncompatibleOperands);
                  opExpr:=nil;
               end else begin
                  opExpr:=CreateTypedOperatorExpr(tt, hotPos, Result, right);
                  if opExpr=nil then begin
                     if     (tt=ttPLUS)
                        and Result.Typ.UnAliasedTypeIs(TArraySymbol)
                        and right.Typ.UnAliasedTypeIs(TArraySymbol) then begin
                        opExpr:=CompilerUtils.ArrayConcat(FCompilerContext, hotPos, Result, right);
                     end else if FGenericSymbol.Count > 0 then begin
                        opExpr := TGenericBinaryOpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                     end else begin
                        FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     end;
                  end;
               end;
               if opExpr=nil then begin
                  // fake result to keep compiler going and report further issues
                  Result := TBinaryOpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                  Result.Typ:=FCompilerContext.TypVariant;
               end else Result:=opExpr;
            except
               OrphanAndNil(right);
               raise;
            end;
         end;

         if Optimize then
            Result:=Result.OptimizeToTypedExpr(FCompilerContext, hotPos);
      until False;
   except
      OrphanAndNil(Result);
      raise;
   end;
end;

// ReadExprMult
//
function TdwsCompiler.ReadExprMult(expecting : TTypeSymbol = nil; leftExpr : TTypedExpr = nil) : TTypedExpr;
var
   right : TTypedExpr;
   tt : TTokenType;
   hotPos : TScriptPos;
   opExpr : TTypedExpr;
   rightTyp : TTypeSymbol;
begin
   // Read left argument
   if leftExpr = nil then
      Result := ReadTerm(False, expecting)
   else Result := leftExpr;
   try
      repeat
         tt := FTok.TestDeleteAny([ttTIMES, ttDIVIDE, ttMOD, ttDIV, ttAND,
                                   ttCARET, ttAS, ttLESSLESS, ttGTRGTR, ttQUESTIONQUESTION,
                                   ttSHL, ttSHR, ttSAR]);
         if tt = ttNone then Break;

         // Save position of the operator
         hotPos := FTok.HotPos;

         // Read right argument
         right := ReadTerm;
         try
            if (Result.Typ=nil) or (right=nil) or(right.Typ=nil) then
               FMsgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands);
            case tt of
               ttAS : begin
                  rightTyp:=right.Typ;
                  if Result.Typ is TInterfaceSymbol then begin
                     if rightTyp is TInterfaceSymbol then begin
                        Result:=TIntfAsIntfExpr.Create(FCompilerContext, hotPos, Result, TInterfaceSymbol(rightTyp));
                     end else begin
                        if not (rightTyp is TClassOfSymbol) then begin
                           FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                           rightTyp:=FCompilerContext.TypTObject.MetaSymbol;
                        end;
                        Result:=TIntfAsClassExpr.Create(FCompilerContext, hotPos, Result, TClassOfSymbol(rightTyp).Typ);
                     end;
                  end else if Result.Typ is TClassSymbol then begin
                     if rightTyp is TInterfaceSymbol then
                        Result:=TObjAsIntfExpr.Create(FCompilerContext, hotPos, Result, TInterfaceSymbol(rightTyp))
                     else begin
                        if not (rightTyp is TClassOfSymbol) then begin
                           FMsgs.AddCompilerError(hotPos, CPE_ClassRefExpected);
                           rightTyp:=FCompilerContext.TypTObject.MetaSymbol;
                        end;
                        Result:=TObjAsClassExpr.Create(FCompilerContext, hotPos, Result, TClassOfSymbol(rightTyp).Typ);
                     end;
                  end else begin
                     if not (Result.Typ is TClassOfSymbol) then
                        FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected)
                     else if not (rightTyp is TClassOfSymbol) then begin
                        FMsgs.AddCompilerStop(hotPos, CPE_ClassRefExpected);
                        rightTyp:=FCompilerContext.TypTObject.MetaSymbol;
                     end;
                     Result:=TClassAsClassExpr.Create(FCompilerContext, hotPos, Result, TClassOfSymbol(rightTyp));
                  end;
                  OrphanAndNil(right);
               end;
               ttQUESTIONQUESTION : begin
                  rightTyp:=right.Typ;
                  if not Result.Typ.IsCompatible(rightTyp) then begin
                     if Result.Typ.UnAliasedTypeIs(TClassSymbol) and right.Typ.UnAliasedTypeIs(TClassSymbol) then begin
                        Result := TCoalesceClassExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                        Result.Typ := (Result.Typ.UnAliasedType as TClassSymbol).CommonAncestor(rightTyp.UnAliasedType);
                        if Result.Typ = nil then begin
                           FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                           // keep compiling
                           Result.Typ := TCoalesceClassExpr(Result).Left.Typ;
                        end;
                     end else begin
                        IncompatibleTypes(hotPos, CPE_IncompatibleTypes, Result.Typ, rightTyp);
                        // fake result to keep compiler going and report further issues
                        Result:=TBinaryOpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                        Result.Typ:=TBinaryOpExpr(Result).Left.Typ;
                     end;
                  end else if Result.Typ.IsOfType(FCompilerContext.TypVariant) then begin
                     Result:=TCoalesceExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                     Result.Typ:=FCompilerContext.TypVariant;
                  end else if Result.Typ.IsOfType(FCompilerContext.TypString) then begin
                     Result:=TCoalesceStrExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                  end else if Result.Typ.UnAliasedType.ClassType=TClassSymbol then begin
                     Result:=TCoalesceClassExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                  end else if Result.Typ.UnAliasedType.ClassType=TDynamicArraySymbol then begin
                     Result:=TCoalesceDynArrayExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                  end else if Result.Typ.IsOfType(FCompilerContext.TypInteger) then begin
                     Result:=TCoalesceIntExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                  end else begin
                     FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     // fake result to keep compiler going and report further issues
                     Result:=TBinaryOpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                     Result.Typ:=TBinaryOpExpr(Result).Left.Typ;
                  end;
               end;
            else
               opExpr:=CreateTypedOperatorExpr(tt, hotPos, Result, right);
               if opExpr=nil then begin
                  FMsgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                  // fake result to keep compiler going and report further issues
                  Result:=TBinaryOpExpr.Create(FCompilerContext, hotPos, tt, Result, right);
                  Result.Typ:=right.Typ;
               end else Result:=opExpr;
            end;

         except
            OrphanAndNil(right);
            raise;
         end;

         if Optimize then
            Result:=Result.OptimizeToTypedExpr(FCompilerContext, hotPos);
      until False;
   except
      OrphanAndNil(Result);
      raise;
   end;
end;

// ReadBooleanExpr
//
function TdwsCompiler.ReadBooleanExpr : TTypedExpr;
var
   hotPos : TScriptPos;
begin
   hotPos := FTok.HotPos;
   Result := ReadExpr;
   if Result <> nil then begin
      if not (Result.IsOfType(FCompilerContext.TypBoolean) or Result.IsOfType(FCompilerContext.TypVariant)) then begin
         if not Result.IsGeneric then begin
            if Result.ScriptPos.Defined then
               hotPos := Result.ScriptPos;
            FMsgs.AddCompilerError(hotPos, CPE_BooleanExpected)
         end;
      end;
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
   classOpExpr : TFuncExprBase;
   argPosArray : TScriptPosArray;
begin
   hotPos:=FTok.HotPos;

   if FTok.TestDelete(ttALEFT) then begin

      Result:=ReadExprInConditions(left);

   end else begin

      setExpr:=ReadExpr;
      try

         if setExpr=nil then begin
            // keep compiling
            Result:=TConvVarToBoolExpr.Create(FCompilerContext, left);
            left:=nil;
            Exit;
         end;

         if setExpr.Typ is TDynamicArraySymbol then begin

            elementType:=TDynamicArraySymbol(setExpr.Typ).Typ;
            if (left.Typ=nil) or not left.Typ.IsOfType(elementType) then begin
               // attempt cast & typecheck harder
               if (left is TFuncExpr) and (TFuncExpr(left).Args.Count=0) then begin
                  if left is TFuncPtrExpr then
                     left:=TFuncPtrExpr(left).Extract
                  else left:=TFuncRefExpr.Create(FCompilerContext, TFuncExpr(left));
               end else begin
                  left:=TConvExpr.WrapWithConvCast(FCompilerContext, hotPos, elementType,
                                                   left, CPE_IncompatibleTypes);
               end;
            end;

            Result:=TArrayIndexOfExpr.Create(FCompilerContext, hotPos, setExpr, left, nil);
            Result:=TRelGreaterEqualIntExpr.Create(FCompilerContext, hotPos, ttIN, Result,
                                                   FUnifiedConstants.CreateInteger(0));

         end else if setExpr.Typ is TSetOfSymbol then begin

            elementType:=TDynamicArraySymbol(setExpr.Typ).Typ;
            if (left.Typ=nil) or not left.Typ.IsOfType(elementType) then
               IncompatibleTypes(hotPos, CPE_IncompatibleTypes,
                                 left.Typ, elementType);
            Result:=TSetOfInExpr.CreateOptimal(FCompilerContext, hotPos, left, setExpr as TDataExpr);

         end else if setExpr.Typ is TAssociativeArraySymbol then begin

            elementType := TAssociativeArraySymbol(setExpr.Typ).KeyType;
            if (left.Typ=nil) or not left.Typ.IsOfType(elementType) then begin
               // attempt cast & typecheck harder
               if (left is TFuncExpr) and (TFuncExpr(left).Args.Count=0) then begin
                  if left is TFuncPtrExpr then
                     left := TFuncPtrExpr(left).Extract
                  else left := TFuncRefExpr.Create(FCompilerContext, TFuncExpr(left));
               end else begin
                  left := TConvExpr.WrapWithConvCast(FCompilerContext, hotPos, elementType,
                                                     left, CPE_IncompatibleTypes);
               end;
            end;

            Result := TAssociativeArrayContainsKeyExpr.Create(FCompilerContext, hotPos, ttIN, left, setExpr);

         end else if not (setExpr is TDataExpr) then begin

            FMsgs.AddCompilerError(hotPos, CPE_ObjectExpected);
            // keep compiling
            OrphanAndNil(left);
            OrphanAndNil(setExpr);
            Result := FUnifiedConstants.CreateBoolean(False);

         end else begin

            if setExpr.Typ is TClassSymbol then begin
               classOpSymbol:=(setExpr.Typ as TClassSymbol).FindClassOperator(ttIN, left.Typ);
               if classOpSymbol<>nil then begin
                  classOpExpr:=GetMethodExpr(classOpSymbol.UsesSym, (setExpr as TDataExpr),
                                             rkObjRef, hotPos, []);
                  try
                     setExpr:=nil;
                     classOpExpr.AddArg(left);
                     left:=nil;
                     TypeCheckArguments(FCompilerContext, classOpExpr, argPosArray);
                  except
                     classOpExpr.Orphan(FCompilerContext);
                     raise;
                  end;
                  Result:=classOpExpr;
                  Exit;
               end;
            end;

            Result:=CreateTypedOperatorExpr(ttIN, hotPos, left, setExpr);
            if Result=nil then begin
               FMsgs.AddCompilerError(hotPos, CPE_IncompatibleOperands);
               // fake result to keep compiler going and report further issues
               Result:=TBinaryOpExpr.Create(FCompilerContext, hotPos, ttIN, left, setExpr);
               Result.Typ:=FCompilerContext.TypVariant;
            end;
            left:=nil;

         end;

      except
         OrphanAndNil(setExpr);
         raise;
      end;

   end;
   if Optimize then
      Result:=Result.OptimizeToTypedExpr(FCompilerContext, hotPos);
end;

// ReadExprInConditions
//
function TdwsCompiler.ReadExprInConditions(var left : TTypedExpr) : TInOpExpr;
var
   i : Integer;
   condList : TTightList;
   hotPos : TScriptPos;
begin
   hotPos:=FTok.HotPos;

   condList.Initialize;
   try
      if not FTok.TestDelete(ttARIGHT) then begin
         ReadCaseConditions(condList, left);
         if not FTok.TestDelete(ttARIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
      end;

      if left.IsOfType(FCompilerContext.TypString) then begin

         if     TCaseConditionsHelper.CanOptimizeToTyped(condList, TConstStringExpr)
            and condList.ItemsAllOfClass(TCompareConstStringCaseCondition) then begin

            Result:=TStringInOpStaticSetExpr.Create(FCompilerContext, left)

         end else begin

            Result:=TStringInOpExpr.Create(FCompilerContext, left);

         end;
      end else begin

         Result:=TInOpExpr.Create(FCompilerContext, left);

      end;

      left:=nil;

      // Add case conditions to TCaseExpr
      for i:=0 to condList.Count-1 do
         Result.AddCaseCondition(condList.List[i] as TCaseCondition);

      Result.Prepare;

      condList.Clear;
   finally
      condList.Clean;
   end;
end;

// ReadTerm
//
function TdwsCompiler.ReadTerm(isWrite : Boolean = False; expecting : TTypeSymbol = nil) : TTypedExpr;

   function ReadNotTerm : TUnaryOpExpr;
   var
      operand : TTypedExpr;
      hotPos : TScriptPos;
   begin
      hotPos:=FTok.HotPos;
      operand:=ReadTerm;
      if operand.IsOfType(FCompilerContext.TypBoolean) then
         Result:=TNotBoolExpr.Create(FCompilerContext, operand)
      else if operand.IsOfType(FCompilerContext.TypInteger) then
         Result:=TNotIntExpr.Create(FCompilerContext, operand)
      else begin
         if not operand.IsOfType(FCompilerContext.TypVariant) then
            FMsgs.AddCompilerError(hotPos, CPE_BooleanOrIntegerExpected);
         Result:=TNotVariantExpr.Create(FCompilerContext, operand);
      end;
   end;

   function ReadNull(expecting : TTypeSymbol) : TConstExpr;
   begin
      Result:=TConstExpr.CreateNull(expecting);
   end;

   procedure CheckForClosure(funcSym : TFuncSymbol);

      procedure CheckSubExprs(expr : TExprBase);
      var
         i : Integer;
         funcSym : TFuncSymbol;
      begin
         if expr = nil then Exit;
         if (expr is TVarParentExpr) or (expr is TVarParamParentExpr) then
            FMsgs.AddCompilerErrorFmt(expr.ScriptPos, CPE_SymbolCannotBeCaptured,
                                      [(expr as TVarExpr).DataSym.Name])
         else if expr is TFuncExprBase then begin
            funcSym := TFuncExprBase(expr).FuncSym;
            if Assigned(funcSym) and (funcSym.Level <> 1) then
               FMsgs.AddCompilerError(expr.ScriptPos, CPE_LocalFunctionAsDelegate);
         end;
         for i := 0 to expr.SubExprCount-1 do
            CheckSubExprs(expr.SubExpr[i]);
      end;

   var
      i : Integer;
   begin
      for i := 0 to funcSym.Executable.SubExprCount-1 do
         CheckSubExprs(funcSym.Executable.SubExpr(i));
   end;

   function ReadAnonymousMethod(funcType : TTokenType; const hotPos : TScriptPos) : TAnonymousFuncRefExpr;
   var
      funcSym : TFuncSymbol;
   begin
      funcSym:=ReadProcDecl(funcType, hotPos, [pdoAnonymous]);
      CurrentProg.Table.AddSymbol(funcSym);
      ReadProcBody(funcSym);
      Result:=TAnonymousFuncRefExpr.Create(FCompilerContext, GetFuncExpr(funcSym, nil));
      if not (coAllowClosures in Options) then
         CheckForClosure(funcSym);
   end;

   function ReadLambda(funcType : TTokenType; const hotPos : TScriptPos) : TAnonymousFuncRefExpr;
   var
      funcSym : TFuncSymbol;
      expectingFuncSym : TFuncSymbol;
      expectedType : TTypeSymbol;
      expectingParams : TParamsSymbolTable;
      resultExpr : TTypedExpr;
      resultVar : TVarExpr;
      proc : TdwsProcedure;
      procPos : TScriptPos;
      oldProg : TdwsProgram;
   begin
      expectingFuncSym:=expecting.AsFuncSymbol;
      if expectingFuncSym <> nil then begin
         expectingParams := expectingFuncSym.Params;
         expectedType := expectingFuncSym.Typ;
      end else begin
         expectingParams := nil;
         expectedType := nil;
      end;

      funcSym := ReadProcDecl(funcType, hotPos, [pdoAnonymous], expectingParams);
      CurrentProg.Table.AddSymbol(funcSym);

      if (funcSym.Typ=nil) and (expectingFuncSym<>nil) then
         if not (expectedType is TAnyTypeSymbol) then
            funcSym.Typ := expectedType;

      if FTok.TestDelete(ttEQGTR) then begin

         FTok.TestName;
         procPos:=FTok.HotPos;

         proc := TdwsProcedure.Create(CurrentProg);
         proc.SetBeginPos(procPos);
         proc.AssignTo(funcSym);

         oldProg:=CurrentProg;
         CurrentProg:=proc;
         try
            resultExpr:=ReadExpr(expectedType);
         finally
            CurrentProg:=oldProg;
         end;
         if funcSym.Typ = nil then
            funcSym.Typ := resultExpr.Typ;

         if funcSym.Typ=nil then begin
            FMsgs.AddCompilerError(procPos, CPE_UnexpectedEqGtrForLambdaStatement);
            proc.Expr:=resultExpr;
         end else begin
            resultVar:=TVarExpr.CreateTyped(FCompilerContext, proc.Func.Result);
            proc.Expr:=CreateAssign(procPos, ttASSIGN, resultVar, resultExpr);
         end;

      end else begin

         ReadProcBody(funcSym);

      end;
      Result:=TAnonymousFuncRefExpr.Create(FCompilerContext, GetFuncExpr(funcSym, nil));

      if not (coAllowClosures in Options) then
         CheckForClosure(funcSym);
   end;

   function ReadAnonymousRecord : TTypedExpr;
   var
      scriptPos : TScriptPos;
      recordType : TRecordSymbol;
      data : TData;
   begin
      scriptPos:=FTok.HotPos;
      recordType:=ReadRecordDecl('', True);
      CurrentProg.Table.AddSymbol(recordType);
      recordType.SetIsExternal;

      RecordSymbolUseImplicitReference(recordType, scriptPos, False);

      if recordType.IsDynamic then begin

         Result:=TDynamicRecordExpr.Create(FCompilerContext, scriptPos, recordType);

      end else begin

         SetLength(data, recordType.Size);
         recordType.InitData(data, 0);

         Result:=TConstExpr.CreateTyped(FCompilerContext, recordType, data);

      end;
   end;

   function ReadAnonymousClass : TTypedExpr;
   var
      scriptPos : TScriptPos;
      classType : TClassSymbol;
   begin
      scriptPos:=FTok.HotPos;
      classType:=ReadClassDecl('', [csfExternal], True);
      CurrentProg.Table.AddSymbol(classType);

      RecordSymbolUseImplicitReference(classType, scriptPos, False);

      Result:=TConstructorAnonymousExpr.Create(scriptPos, classType);
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
   const
      cNilIntf : IUnknown = nil;
   var
      hotPos : TScriptPos;
   begin
      hotPos:=FTok.HotPos;
      if expecting=nil then
         expecting:=FAnyFuncSymbol
      else if expecting.AsFuncSymbol=nil then
         FMsgs.AddCompilerError(hotPos, CPE_UnexpectedAt)
      else if expecting=FAnyFuncSymbol then
         FMsgs.AddCompilerStop(hotPos, CPE_UnexpectedAt);
      Result:=ReadTerm(isWrite, expecting);
      if Result = nil then begin
         // error was already reported
         Result := TBogusConstExpr.Create(FCompilerContext.TypNil, cNilIntf);
      end else if (Result.Typ=nil) or (Result.Typ.AsFuncSymbol=nil) then begin
         if (expecting=FAnyFuncSymbol) or (Result is TConstExpr) then
            FMsgs.AddCompilerError(hotPos, CPE_UnexpectedAt)
         else if Result <> nil then // if nil, error was already reported
            ReportIncompatibleAt(hotPos, Result);
         // keep compiling
         OrphanAndNil(Result);
         Result := TBogusConstExpr.Create(FCompilerContext.TypNil, cNilIntf);
      end;
   end;

   function ReadImmediate : TTypedExpr;
   var
      constExpr : TConstExpr;
      hasDot : Boolean;
   begin
      constExpr := ReadConstImmediateValue;
      if constExpr<>nil then begin
         try
            hasDot := FTok.Test(ttDOT);
         except
            constExpr.Orphan(FCompilerContext);
            raise;
         end;
         if hasDot then
            Result := (ReadSymbol(constExpr, isWrite) as TTypedExpr)
         else Result := constExpr;
      end else Result := nil;
   end;

var
   tt : TTokenType;
   nameExpr : TProgramExpr;
   hotPos : TScriptPos;
begin
   tt := FTok.TestAny([ttPLUS, ttMINUS, ttALEFT, ttNOT, ttBLEFT, ttAT,
                       ttTRUE, ttFALSE, ttNIL, ttIF,
                       ttFUNCTION, ttPROCEDURE, ttLAMBDA,
                       ttRECORD, ttCLASS,
                       ttBRIGHT]);
   if tt <> ttNone then begin
      // special logic for property write expressions
      if tt=ttBRIGHT then begin
         if FPendingSetterValueExpr<>nil then begin
            Result:=FPendingSetterValueExpr;
            FPendingSetterValueExpr:=nil;
         end else begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExpressionExpected);
            Result:=nil;
         end;
         Exit;
      end else FTok.KillToken;
   end;
   case tt of
      ttPLUS : begin
         FTok.TestName;
         hotPos:=FTok.HotPos;
         Result:=ReadTerm; // (redundant) plus sign
         if not (   Result.IsOfType(FCompilerContext.TypFloat)
                 or Result.IsOfType(FCompilerContext.TypInteger)
                 or Result.IsOfType(FCompilerContext.TypVariant)) then
            FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
      end;
      ttMINUS :
         Result:=ReadNegation;
      ttALEFT :
         Result:=ReadArrayConstant(ttARIGHT, expecting);
      ttNOT :
         Result:=ReadNotTerm;
      ttBLEFT : begin
         Result:=ReadBracket;
         if FTok.Test(ttDOT) then
            Result:=(ReadSymbol(Result, isWrite) as TTypedExpr);
      end;
      ttAT :
         Result:=ReadAt(expecting);
      ttTRUE :
         Result := FUnifiedConstants.CreateBoolean(True);
      ttFALSE :
         Result := FUnifiedConstants.CreateBoolean(False);
      ttNIL :
         Result := FUnifiedConstants.CreateNil;
      ttIF :
         Result:=ReadIfExpr;
      ttPROCEDURE, ttFUNCTION : begin
         Result:=ReadAnonymousMethod(tt, FTok.HotPos);
      end;
      ttLAMBDA : begin
         Result:=ReadLambda(tt, FTok.HotPos);
      end;
      ttRECORD :
         Result:=ReadAnonymousRecord;
      ttCLASS : begin
         if not (coAllowClosures in Options) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_AnonymousClassNotAllowed);
         Result:=ReadAnonymousClass;
      end;
   else
      if (FTok.TestAny([ttINHERITED, ttNEW])<>ttNone) or FTok.TestName then begin
         // Variable or Function
         nameExpr:=ReadName(isWrite, expecting);
         if (nameExpr<>nil) and not nameExpr.InheritsFrom(TTypedExpr) then begin
            OrphanAndNil(nameExpr);
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExpressionExpected);
            // keep compiling
            if expecting=nil then
               expecting:=FCompilerContext.TypVariant;
            Result:=ReadNull(expecting);
         end else Result:=TTypedExpr(nameExpr);
      end else begin // Constant values in the code
         Result:=ReadImmediate;
      end;
   end;

   // No expression found
   if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);
end;

// ReadBracket
//
function TdwsCompiler.ReadBracket(expecting : TTypeSymbol = nil) : TTypedExpr;
begin
   Result:=ReadExpr(expecting);
   if not FTok.TestDelete(ttBRIGHT) then begin
      OrphanAndNil(Result);
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
   end else if Optimize then
      Result:=Result.OptimizeToTypedExpr(FCompilerContext, Result.ScriptPos);
end;

// ReadNegation
//
function TdwsCompiler.ReadNegation : TTypedExpr;
var
   opSymbol : TOperatorSymbol;
   negExprClass : TUnaryOpExprClass;
   negTerm : TTypedExpr;
   hotPos : TScriptPos;
begin
   FTok.TestName; // make sure hotpos is on next token
   hotPos := FTok.HotPos;
   negTerm := ReadTerm;
   if negTerm = nil then begin
      Result := TErrorValueExpr.Create(FSystemTable.TypAnyType);
      Exit;
   end;

   // shortcut for common negations
   if negTerm.Typ = FCompilerContext.TypInteger then
      negExprClass := TNegIntExpr
   else if negTerm.Typ = FCompilerContext.TypFloat then
      negExprClass:=TNegFloatExpr
   else begin
      opSymbol := ResolveOperatorFor(CurrentProg, ttMINUS, nil, negTerm.Typ);
      if opSymbol = nil then begin
         FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
         negExprClass := TNegVariantExpr; // keep compiling
      end else begin
         Assert(opSymbol.OperatorExprClass.InheritsFrom(TUnaryOpExpr));
         negExprClass := TUnaryOpExprClass(opSymbol.OperatorExprClass);
      end;
   end;
   Result:=negExprClass.Create(FCompilerContext, negTerm);
   if Optimize or (negTerm is TConstExpr) then
      Result:=Result.OptimizeToTypedExpr(FCompilerContext, hotPos);
end;

// ReadIfExpr
//
function TdwsCompiler.ReadIfExpr(expecting : TTypeSymbol = nil) : TTypedExpr;
var
   hotPos : TScriptPos;
   boolExpr : TTypedExpr;
   trueExpr, falseExpr : TTypedExpr;
   trueTyp, falseTyp : TTypeSymbol;
   typ : TTypeSymbol;
begin
   hotPos:=FTok.HotPos;
   boolExpr:=ReadBooleanExpr;
   trueExpr:=nil;
   falseExpr:=nil;
   try
      if not FTok.TestDelete(ttTHEN) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ThenExpected);

      trueExpr:=ReadExpr(expecting);
      if trueExpr=nil then
         trueTyp:=nil
      else begin
         trueTyp:=trueExpr.Typ;
         if trueTyp=nil then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ExpressionExpected);
      end;

      if FTok.TestDelete(ttELSE) then begin

         falseExpr:=ReadExpr(expecting);
         if falseExpr=nil then
            falseTyp:=nil
         else begin
            falseTyp:=falseExpr.Typ;
            if falseTyp=nil then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ExpressionExpected);
         end;

      end else if trueTyp<>nil then begin

         falseExpr := CreateTypedDefault(trueTyp);
         falseTyp := trueTyp;

      end else falseTyp:=nil;

      if (trueTyp=nil) or (falseTyp=nil) then begin

         typ:=nil;

      end else begin

         if trueExpr.IsOfType(FCompilerContext.TypInteger) and falseExpr.IsOfType(FCompilerContext.TypFloat) then begin
            trueExpr:=TConvIntToFloatExpr.Create(FCompilerContext, trueExpr);
            trueTyp:=trueExpr.Typ;
         end else if trueExpr.IsOfType(FCompilerContext.TypFloat) and falseExpr.IsOfType(FCompilerContext.TypInteger) then begin
            falseExpr:=TConvIntToFloatExpr.Create(FCompilerContext, falseExpr);
            falseTyp:=falseExpr.Typ;
         end;

         if falseTyp.IsOfType(FCompilerContext.TypNil) then begin

            typ:=trueTyp;
            if not typ.IsCompatible(falseTyp) then
               FMsgs.AddCompilerError(hotPos, CPE_InvalidArgCombination);

         end else if trueTyp.IsOfType(FCompilerContext.TypNil) then begin

            typ:=falseTyp;
            if not typ.IsCompatible(trueTyp) then
               FMsgs.AddCompilerError(hotPos, CPE_InvalidArgCombination);

         end else if falseTyp.IsCompatible(trueTyp) then

            typ:=falseExpr.Typ

         else begin

            typ:=trueTyp;
            if not typ.IsCompatible(falseTyp) then
               FMsgs.AddCompilerError(hotPos, CPE_InvalidArgCombination);

         end;

      end;

      if (typ<>nil) and (typ.Size>1) then   // TODO !!!!!!!!!!!
         FMsgs.AddCompilerError(hotPos, 'Implementation currently limited to arguments of size 1');

      Result:=TIfThenElseValueExpr.Create(FCompilerContext, hotPos, typ, boolExpr, trueExpr, falseExpr);
   except
      OrphanAndNil(boolExpr);
      OrphanAndNil(trueExpr);
      OrphanAndNil(falseExpr);
      raise;
   end;
end;

// ReadConstImmediateValue
//
function TdwsCompiler.ReadConstImmediateValue: TConstExpr;
var
   tt : TTokenType;
   token : TToken;
begin
   Result:=nil;
   tt:=FTok.TestAny([ttStrVal, ttIntVal, ttFloatVal]);
   if tt<>ttNone then begin
      token:=FTok.GetToken;
      case tt of
         ttIntVal :
            Result := FUnifiedConstants.CreateInteger(token.FInteger);
         ttFloatVal :
            Result := FUnifiedConstants.CreateFloat(token.FFloat);
         ttStrVal :
            if token.EmptyString then
               Result := FUnifiedConstants.CreateEmptyString
            else Result := FUnifiedConstants.CreateString(token.AsString);
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
   memberTyp : TTypeSymbol;
   expr : TTypedExpr;
   constExpr : TConstExpr;
   exprPos : TScriptPos;
   factory : IdwsDataSymbolFactory;
begin
   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

   SetLength(memberSet, symbol.Size);

   SetLength(Result, symbol.Size);
   symbol.InitData(Result, 0);

   factory:=TCompositeTypeSymbolFactory.Create(Self, symbol, cvPublic);

   while not FTok.Test(ttBRIGHT) do begin
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      sym:=symbol.Members.FindLocal(FTok.GetToken.AsString);
      if not (sym is TFieldSymbol) then begin
         FMsgs.AddCompilerErrorFmt(FTok.GetToken.FScriptPos, CPE_UnknownMember, [FTok.GetToken.AsString]);
         sym:=nil;
      end;
      memberSym:=TFieldSymbol(sym);
      if memberSym<>nil then begin
         memberTyp:=memberSym.Typ;
         if memberSym.Visibility<cvPublic then
            FMsgs.AddCompilerErrorFmt(FTok.GetToken.FScriptPos, CPE_MemberSymbolNotVisible, [FTok.GetToken.AsString]);
         if memberSet[memberSym.Offset] then
            FMsgs.AddCompilerError(FTok.GetToken.FScriptPos, CPE_FieldAlreadySet);
         memberSet[memberSym.Offset]:=True;
         RecordSymbolUseReference(memberSym, FTok.GetToken.FScriptPos, True);
      end else begin
         memberTyp:=nil;
      end;
      FTok.KillToken;
      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
      exprPos:=FTok.HotPos;
      expr:=factory.ReadInitExpr(memberTyp);
      try
         if not (expr is TConstExpr) then begin
            if expr.IsConstant then
               expr:=expr.OptimizeToTypedExpr(FCompilerContext, exprPos)
            else begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
               OrphanAndNil(expr);
            end;
         end;
         if (expr<>nil) and (memberTyp<>nil) then begin
            constExpr:=TConstExpr(expr);
            if constExpr.Typ.IsOfType(FCompilerContext.TypInteger) and memberTyp.IsOfType(FCompilerContext.TypFloat) then
               Result[memberSym.Offset]:=constExpr.EvalAsFloat(FExec)
            else if not constExpr.Typ.IsCompatible(memberTyp) then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidConstTypeVsExpected, [constExpr.Typ.Caption, memberTyp.Caption])
            else begin
               constExpr.DataPtr[FExec].CopyData(result, memberSym.Offset, memberTyp.Size);
            end;
         end;
      finally
         OrphanAndNil(expr);
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
   names : TSimpleStringList;
   typSym : TTypeSymbol;
   isVarParam, isConstParam : Boolean;
   posArray : TScriptPosArray;
begin
   if FTok.TestDelete(ttARIGHT) then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_ParamsExpected);
      Exit;
   end;

   // At least one argument was found
   names:=FStringListPool.Acquire;
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
                  ArrayIndices.AddSymbol(CreateConstParamSymbol(names[i], typSym))
               else ArrayIndices.AddSymbol(TParamSymbol.Create(names[i], typSym));
            end;
         end;
      until not FTok.TestDelete(ttSEMI);

   finally
      FStringListPool.Release(names);
   end;

   if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
end;

// ReadParams
//
procedure TdwsCompiler.ReadParams(const hasParamMeth : THasParamSymbolMethod;
                                  const addParamMeth : TAddParamSymbolMethod;
                                  forwardedParams : TParamsSymbolTable;
                                  expectedLambdaParams : TParamsSymbolTable;
                                  var posArray : TScriptPosArray);

   procedure GenerateParam(const curName : String; const scriptPos : TScriptPos;
                           paramSemantics : TParamSymbolSemantics;
                           paramType : TTypeSymbol; const typScriptPos : TScriptPos;
                           var defaultExpr : TTypedExpr);
   var
      paramSym : TParamSymbol;
   begin
      case paramSemantics of
         pssLazy :
            paramSym := TLazyParamSymbol.Create(curName, paramType);
         pssVar :
            paramSym := TVarParamSymbol.Create(curName, paramType);
         pssConst :
            paramSym := CreateConstParamSymbol(curName, paramType);
      else
         if Assigned(defaultExpr) then begin
            if defaultExpr.ClassType=TArrayConstantExpr then begin
               paramSym:=TParamSymbolWithDefaultValue.Create(
                              curName, paramType, TArrayConstantExpr(defaultExpr).EvalAsTData(FExec));
            end else begin
               paramSym:=TParamSymbolWithDefaultValue.Create(
                              curName, paramType,
                              (defaultExpr as TConstExpr).Data);
            end;
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
         if not typScriptPos.Defined then
            RecordSymbolUse(paramType, scriptPos, [suReference, suImplicit])
//            RecordSymbolUse(paramType, typScriptPos, [suReference])
      end;
   end;

var
   i, paramIdx : Integer;
   names : TSimpleStringList;
   typ : TTypeSymbol;
   onlyDefaultParamsNow : Boolean;
   typScriptPos, exprPos : TScriptPos;
   defaultExpr : TTypedExpr;
   expectedParam : TParamSymbol;
   localPosArray : TScriptPosArray;
   paramSemantics : TParamSymbolSemantics;
begin
   if FTok.TestDelete(ttBLEFT) then begin

      if not FTok.TestDelete(ttBRIGHT) then begin
         // At least one argument was found
         names:=FStringListPool.Acquire;
         try
            paramIdx:=0;
            onlyDefaultParamsNow:=False;
            repeat
               case FTok.TestDeleteAny([ttLAZY, ttVAR, ttCONST]) of
                  ttLAZY : paramSemantics := pssLazy;
                  ttVAR : paramSemantics := pssVar;
                  ttCONST : paramSemantics := pssConst;
               else
                  paramSemantics := pssCopy;
               end;

               ReadNameList(names, localPosArray);
               ConcatScriptPosArray(posArray, localPosArray, names.Count);

               if not FTok.TestDelete(ttCOLON) then begin

                  if (expectedLambdaParams<>nil) and (paramIdx+names.Count-1<expectedLambdaParams.Count) then begin

                     defaultExpr:=nil;
                     for i:=0 to names.Count-1 do begin
                        expectedParam := expectedLambdaParams[paramIdx+i];
                        paramSemantics := expectedParam.Semantics;
                        GenerateParam(names[i], localPosArray[i], paramSemantics, expectedParam.Typ, cNullPos, defaultExpr);
                     end;

                  end else begin

                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)

                  end;

               end else begin

                  typ:=ReadType('', tcParameter);

                  defaultExpr:=nil;
                  try
                     typScriptPos:=FTok.HotPos;

                     if (paramSemantics<>pssConst) and (typ is TOpenArraySymbol) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_OpenArrayParamMustBeConst);
                     if (paramSemantics=pssLazy) and (typ.AsFuncSymbol<>nil) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantBeFunctionPointer);

                     if FTok.TestDelete(ttEQ) then begin
                        onlyDefaultParamsNow:=True;
                        case paramSemantics of
                           pssLazy :
                              FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantHaveDefaultValue);
                           pssVar :
                              FMsgs.AddCompilerError(FTok.HotPos, CPE_VarParamCantHaveDefaultValue);
                           pssConst :
                              FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstParamCantHaveDefaultValue);
                        end;

                        exprPos:=FTok.HotPos;
                        defaultExpr:=FStandardDataSymbolFactory.ReadInitExpr(typ);

                        if defaultExpr<>nil then begin
                           if (not defaultExpr.IsConstant) or (defaultExpr.Typ=nil) then begin
                              FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
                              OrphanAndNil(defaultExpr);
                           end else if not typ.IsCompatible(defaultExpr.Typ) then begin
                              defaultExpr:=CompilerUtils.WrapWithImplicitConversion(FCompilerContext, defaultExpr, Typ, exprPos);
                              if defaultExpr.ClassType=TConvInvalidExpr then
                                 OrphanAndNil(defaultExpr);
                           end;
                        end;
                     end else if onlyDefaultParamsNow then begin
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_DefaultValueRequired);
                     end;

                     if (defaultExpr<>nil) and not (defaultExpr is TConstExpr) then
                        defaultExpr:=defaultExpr.OptimizeToTypedExpr(FCompilerContext, exprPos);

                     for i:=0 to names.Count-1 do
                        GenerateParam(names[i], localPosArray[i], paramSemantics, typ, typScriptPos, defaultExpr);

                  finally
                     OrphanAndNil(defaultExpr);
                  end;

               end;

               Inc(paramIdx, names.Count);

            until not FTok.TestDelete(ttSEMI);

         finally
            FStringListPool.Release(names);
         end;

         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;

   end else if (expectedLambdaParams<>nil) and (expectedLambdaParams.Count>0) then begin

      // implicit anonmous lambda params
      defaultExpr:=nil;
      for i:=0 to expectedLambdaParams.Count-1 do begin
         expectedParam := expectedLambdaParams[i];
         paramSemantics := expectedParam.Semantics;
         GenerateParam('_implicit_'+expectedParam.Name, cNullPos,
                       paramSemantics, expectedParam.Typ, cNullPos, defaultExpr);
      end;

   end;
end;

// ReadProcCallQualifiers
//
procedure TdwsCompiler.ReadProcCallQualifiers(funcSymbol : TFuncSymbol);
var
   tt : TTokenType;
begin
   tt:=FTok.TestDeleteAny([ttSAFECALL, ttSTDCALL, ttCDECL, ttREGISTER, ttPASCAL]);
   if funcSymbol.IsExternal then
   begin
      if tt = ttNone then
         tt := ttREGISTER;
      funcSymbol.ExternalConvention := tt;
   end
   else if tt<>ttNone then begin
      FMsgs.AddCompilerHintFmt(FTok.HotPos, CPH_CallConventionIsNotSupportedAndIgnored,
                               [cTokenStrings[tt]], hlStrict);
      ReadSemiColon;
   end;
end;

// ReadSwitch
//
function TdwsCompiler.ReadSwitch(const switchName: String) : Boolean;
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
   name : String;
   scriptSource : String;
   i : Integer;
   conditionalTrue : Boolean;
   switchPos, condPos, fileNamePos : TScriptPos;
   condExpr : TTypedExpr;
   sourceFile : TSourceFile;
   includeSymbol : TIncludeSymbol;
   condInfo : TTokenizerConditionalInfo;
   filter : TdwsFilter;
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
            SkipUntilToken(ttCRIGHT);

         end else begin

            name:=FTok.GetToken.AsString;
            FTok.KillToken;

            if coSymbolDictionary in Options then begin
               includeSymbol:=TIncludeSymbol.Create(name);
               CurrentProg.Table.AddSymbol(includeSymbol);
               fileNamePos:=FTok.HotPos;
               fileNamePos.IncCol; // skip quote
               RecordSymbolUse(includeSymbol, fileNamePos, [suReference]);
            end;

            if (switch=siIncludeOnce) then begin
               sourceFile:=FMainProg.GetSourceFile(name);
               if (sourceFile<>nil) and (sourceFile.Name<>name) then
                  FMsgs.AddCompilerWarningFmt(switchPos, CPW_IncludeOnceWithDifferentCase,
                                              [name, sourceFile.Name]);
            end else sourceFile:=nil;
            if sourceFile=nil then begin
               try
                  if switch in [siFilterLong, siFilterShort] then begin
                     if not Assigned(FFilter) then
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoFilterAvailable);
                     // Include file is processed by the filter
                     scriptSource := GetIncludeScriptSource(name);
                     filter := FFilter;
                     if Assigned(FOnFilter) then
                        FOnFilter(Self, name, scriptSource, filter);
                     if filter <> nil then
                        scriptSource := filter.Process(scriptSource, FMsgs);
                  end else begin
                     scriptSource := GetIncludeScriptSource(name);
                  end;

                  if not FTok.TestDelete(ttCRIGHT) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

                  if StrContains(FTok.PathName, '\') then
                     name:=Copy(FTok.PathName, 1, LastDelimiter('\', FTok.PathName))+name;
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
            SkipUntilToken(ttCRIGHT);
         end else begin
            FTok.ConditionalDefines.Value.Add(FTok.GetToken.AsString);
            FTok.KillToken;
         end;

      end;
      siUndef : begin

         if not FTok.Test(ttNAME) then begin
            FMsgs.AddCompilerError(FTok.HotPos, CPE_NameExpected);
            // skip in attempt to recover from error
            SkipUntilToken(ttCRIGHT);
         end else begin
            i:=FTok.ConditionalDefines.Value.IndexOf(FTok.GetToken.AsString);
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
                  SkipUntilToken(ttCRIGHT);
               end else begin
                  conditionalTrue:=    (FTok.ConditionalDefines.Value.IndexOf(FTok.GetToken.AsString)>=0)
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
                     else if not condExpr.IsOfType(FCompilerContext.TypBoolean) then
                        FMsgs.AddCompilerError(condPos, CPE_BooleanExpected)
                     else conditionalTrue:=condExpr.EvalAsBoolean(FExec);
                  finally
                     OrphanAndNil(condExpr);
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
            FMsgs.AddCompilerError(switchPos, CPE_UnbalancedConditionalDirective)
         else begin
            if FTok.ConditionalDepth.Peek.Conditional=tcElse then
               FMsgs.AddCompilerStop(switchPos, CPE_UnfinishedConditionalDirective);

            FTok.ConditionalDepth.Pop;
            ReadUntilEndOrElseSwitch(False);
            if not FTok.HasTokens then
               FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective);
         end;
      end;
      siEndIf : begin

         if FTok.ConditionalDepth.Count=0 then
            FMsgs.AddCompilerStop(switchPos, CPE_UnbalancedConditionalDirective)
         else FTok.ConditionalDepth.Pop;
         // tolerate junk after endif before the curly right
         SkipUntilToken(ttCRIGHT);

      end;
      siResourceLong, siResourceShort : begin

         if not FTok.Test(ttStrVal) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
         if Assigned(FOnResource) then
            FOnResource(Self, FTok.GetToken.AsString);
         FTok.KillToken;

      end;
      siCodeGen : begin

         if not FTok.Test(ttStrVal) then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected);
         if Assigned(FOnCodeGen) then
            FOnCodeGen(Self, switchPos, FTok.GetToken.AsString);
         FTok.KillToken;

      end;
      siHint, siWarning, siError, siFatal : begin

         if not FTok.Test(ttStrVal) then
            if switch<>siFatal then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected)
            else FMsgs.AddCompilerStop(FTok.HotPos, CPE_StringExpected)
         else begin
            case switch of
               siHint    : FMsgs.AddCompilerHint(switchPos, FTok.GetToken.AsString);
               siWarning : FMsgs.AddCompilerWarning(switchPos, FTok.GetToken.AsString);
               siError   : FMsgs.AddCompilerError(switchPos, FTok.GetToken.AsString, TCompilerErrorMessage);
               siFatal   : FMsgs.AddCompilerStop(switchPos, FTok.GetToken.AsString, TCompilerErrorMessage);
            end;
            FTok.KillToken;
         end;

      end;
      siWarnings : begin

         if not FTok.TestDeleteNamePos(name, condPos) then
            name:='';
         conditionalTrue:=ASCIISameText(name, 'ON');
         if conditionalTrue or ASCIISameText(name, 'OFF') then
            FMsgs.WarningsDisabled:=not conditionalTrue
         else FMsgs.AddCompilerError(FTok.HotPos, CPE_OnOffExpected);

      end;
      siHints : begin

         if not FTok.TestDeleteNamePos(name, condPos) then
            name:='';
         if ASCIISameText(name, 'OFF') then
            FMsgs.HintsLevel:=hlDisabled
         else if ASCIISameText(name, 'ON') then
            FMsgs.HintsLevel:=FDefaultHintsLevel
         else if ASCIISameText(name, 'NORMAL') then
            FMsgs.HintsLevel:=hlNormal
         else if ASCIISameText(name, 'STRICT') then
            FMsgs.HintsLevel:=hlStrict
         else if ASCIISameText(name, 'PEDANTIC') then
            FMsgs.HintsLevel:=hlPedantic
         else FMsgs.AddCompilerError(FTok.HotPos, CPE_OnOffExpected);

      end;
      siRegion, siEndRegion : begin

         SkipUntilToken(ttCRIGHT);

      end;
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
   asNum : Boolean;
   name, value : String;
   numValue : Integer;
   hotPos : TScriptPos;
   funcSym : TFuncSymbol;
begin
   Result:=False;
   asNum:=False;
   numValue:=0;
   hotPos:=FTok.HotPos;

   hotPos:=FTok.HotPos;
   if FTok.TestDelete(ttPERCENT) then begin
      if FTok.TestAny([ttNAME, ttFUNCTION])<>ttNone then begin
         name:=FTok.GetToken.AsString;
         FTok.KillToken;
         if not FTok.TestDelete(ttPERCENT) then
            name:='';
      end;
   end;
   if name='' then
      FMsgs.AddCompilerError(hotPos, CPE_IncludeItemExpected)
   else if ASCIISameText(name, 'FILE') then
      value:=hotPos.SourceFile.Name
   else if ASCIISameText(name, 'MAINFILE') then
      value:=FMainProg.MainFileName
   else if ASCIISameText(name, 'LINE') then
      value:=IntToStr(hotPos.Line)
   else if ASCIISameText(name, 'LINENUM') then begin
      numValue:=hotPos.Line;
      asNum:=True;
   end else if ASCIISameText(name, 'DATE') then
      value:=FormatDateTime('yyyy-mm-dd', Date)
   else if ASCIISameText(name, 'TIME') then
      value:=FormatDateTime('hh:nn:ss', Time)
   else if ASCIISameText(name, 'FUNCTION') then begin
      if CurrentProg is TdwsProcedure then begin
         funcSym:=TdwsProcedure(CurrentProg).Func;
         if funcSym is TMethodSymbol then
            value:=TMethodSymbol(funcSym).StructSymbol.Name+'.'+funcSym.Name
         else value:=funcSym.Name;
      end else value:=MSG_MainFunction;
   end else FMsgs.AddCompilerErrorFmt(hotPos, CPE_IncludeItemUnknown, [name]);

   if not FTok.TestDelete(ttCRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_CurlyRightExpected);

   if asNum then
      FTok.SimulateIntegerToken(switchPos, numValue)
   else FTok.SimulateStringToken(switchPos, value);
end;

// SkipUntilToken
//
procedure TdwsCompiler.SkipUntilToken(tt : TTokenType);
begin
   while not FTok.Test(tt) do begin
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

      if not FTok.HasTokens then
         FMsgs.AddCompilerStop(startPos, CPE_UnbalancedConditionalDirective);

      startPos:=FTok.HotPos;
      switch:=StringToSwitchInstruction(FTok.GetToken.AsString);
      FTok.KillToken;

      case switch of

         siEndIf : begin

            // tolerate junk after endif
            SkipUntilToken(ttCRIGHT);
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
         siIfDef, siIfNDef, siIf : begin

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

   sym:=CurrentProg.FindLocal(name);

   if Assigned(sym) then
      FMsgs.AddCompilerErrorFmt(namePos, CPE_NameAlreadyExists, [name]);

   if CurrentProg.Table.ClassType=TUnitImplementationTable then Exit;

   for i:=0 to CurrentProg.SubTableDepth-1 do begin
      subTable:=CurrentProg.SubTable(i);
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
         'a', 'A' : if ASCIISameText(name, cSpecialKeywords[skAbs]) then Exit(skAbs);
         'd', 'D' : if ASCIISameText(name, cSpecialKeywords[skDec]) then Exit(skDec);
         'i', 'I' : if ASCIISameText(name, cSpecialKeywords[skInc]) then Exit(skInc);
         'l', 'L' : if ASCIISameText(name, cSpecialKeywords[skLow]) then Exit(skLow);
         'o', 'O' : if ASCIISameText(name, cSpecialKeywords[skOrd]) then Exit(skOrd);
      end;
      4 : case name[1] of
         'h', 'H' : if ASCIISameText(name, cSpecialKeywords[skHigh]) then Exit(skHigh);
         'p', 'P' : if ASCIISameText(name, cSpecialKeywords[skPred]) then Exit(skPred);
         's', 'S' : case name[2] of
            'u', 'U' : if ASCIISameText(name, cSpecialKeywords[skSucc]) then Exit(skSucc);
            'w', 'W' : if ASCIISameText(name, cSpecialKeywords[skSwap]) then Exit(skSwap);
         end;
      end;
      6 : case name[1] of
         'a', 'A' : if ASCIISameText(name, cSpecialKeywords[skAssert]) then Exit(skAssert);
         'l', 'L' : if ASCIISameText(name, cSpecialKeywords[skLength]) then Exit(skLength);
         's', 'S' : if ASCIISameText(name, cSpecialKeywords[skSizeOf]) then Exit(skSizeOf);
      end;
      7 : case name[1] of
         'd', 'D' :
            if ASCIISameText(name, cSpecialKeywords[skDefined]) then  Exit(skDefined)
            else if ASCIISameText(name, cSpecialKeywords[skDefault]) then  Exit(skDefault)
            ;
         'i', 'I' : if ASCIISameText(name, cSpecialKeywords[skInclude]) then Exit(skInclude);
         'e', 'E' : if ASCIISameText(name, cSpecialKeywords[skExclude]) then Exit(skExclude);
      end;
      8 : case name[1] of
         'a', 'A' : if ASCIISameText(name, cSpecialKeywords[skAssigned]) then Exit(skAssigned);
         'd', 'D' : if ASCIISameText(name, cSpecialKeywords[skDeclared]) then Exit(skDeclared);
      end;
      10 : case name[1] of
         'd', 'D' : if ASCIISameText(name, cSpecialKeywords[skDebugBreak]) then Exit(skDebugBreak);
      end;
      18 : case name[1] of
         'c', 'C' : if ASCIISameText(name, cSpecialKeywords[skConditionalDefined]) then Exit(skConditionalDefined);
      end;
   end;
   Result:=skNone;
end;

// CheckSpecialName
//
procedure TdwsCompiler.CheckSpecialName(const name : String);
begin
   case IdentifySpecialName(name) of
      skNone, skDefault : ;
   else
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NameIsReserved, [Name]);
   end;
end;

// CheckSpecialNameCase
//
procedure TdwsCompiler.CheckSpecialNameCase(const name : String; sk : TSpecialKeywordKind;
                                            const namePos : TScriptPos);
begin
   if name<>cSpecialKeywords[sk] then
      FMsgs.AddCompilerHintFmt(namePos, CPH_CaseDoesNotMatchDeclaration,
                               [name, cSpecialKeywords[sk]], hlPedantic);
end;

// OpenStreamForFile
//
function TdwsCompiler.OpenStreamForFile(const fileName : String) : TStream;
var
   i : Integer;
   fname : String;
begin
   if FScriptPaths.Count=0 then begin
      if FCompileFileSystem.FileExists(fileName) then
         Exit(FCompileFileSystem.OpenFileStream(fileName, fomFastSequentialRead));
   end else begin
      for i:=0 to FScriptPaths.Count-1 do begin
         if FScriptPaths[i]<>'' then
            fname:=IncludeTrailingPathDelimiter(FScriptPaths[i])+fileName
         else fname:=fileName;
         if FCompileFileSystem.FileExists(fname) then
            Exit(FCompileFileSystem.OpenFileStream(fname, fomFastSequentialRead));
      end;
   end;
   Result:=nil;
end;

// GetScriptSource
//
function TdwsCompiler.GetScriptSource(const scriptName : String) : String;
var
   stream : TStream;
begin
   stream:=OpenStreamForFile(scriptName);
   if stream=nil then
      Result:=''
   else begin
      try
         Result:=LoadTextFromStream(stream);
      finally
         stream.Free;
      end;
      if Result='' then
         Result:=' ';
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
function TdwsCompiler.GetVarExpr(const aScriptPos: TScriptPos; dataSym: TDataSymbol): TVarExpr;
begin
   if (dataSym.ClassType=TClassVarSymbol) and (TClassVarSymbol(dataSym).OwnerSymbol.IsExternal) then
      Result:=TExternalVarExpr.Create(dataSym)
   else if CurrentProg.Level=dataSym.Level then begin
      if FDataSymbolExprReuse<>nil then begin
         Result:=FDataSymbolExprReuse.GetValue(dataSym);
         if Result=nil then begin
            Result:=TVarExpr.CreateTyped(FCompilerContext, dataSym);
            FDataSymbolExprReuse.SetValue(dataSym, Result);
         end;
         Result.IncRefCount;
      end else Result:=TVarExpr.CreateTyped(FCompilerContext, dataSym);
   end else Result:=TVarParentExpr.Create(aScriptPos, dataSym);
end;

// GetLazyParamExpr
//
function TdwsCompiler.GetLazyParamExpr(dataSym: TLazyParamSymbol): TLazyParamExpr;
begin
   Result:=TLazyParamExpr.Create(FCompilerContext, dataSym);
end;

// GetVarParamExpr
//
function TdwsCompiler.GetVarParamExpr(dataSym: TVarParamSymbol): TByRefParamExpr;
begin
  if CurrentProg.Level=dataSym.Level then
      Result:=TVarParamExpr.Create(dataSym)
  else Result:=TVarParamParentExpr.Create(dataSym)
end;

// GetConstByRefParamExpr
//
function TdwsCompiler.GetConstByRefParamExpr(dataSym : TConstByRefParamSymbol) : TByRefParamExpr;
begin
   if CurrentProg.Level = dataSym.Level then
      Result := TConstParamExpr.Create(dataSym)
   else Result := TConstParamParentExpr.Create(dataSym)
end;

// GetConstParamExpr
//
function TdwsCompiler.GetConstParamExpr(const aScriptPos: TScriptPos; dataSym: TParamSymbol) : TVarExpr;
begin
   if dataSym is TConstByRefParamSymbol then
      Result := GetConstByRefParamExpr(TConstByRefParamSymbol(dataSym))
   else Result := GetVarExpr(aScriptPos, dataSym);
end;

// GetSelfParamExpr
//
function TdwsCompiler.GetSelfParamExpr(const aScriptPos: TScriptPos; selfSym : TDataSymbol) : TVarExpr;
var
   ct : TClass;
begin
   ct:=selfSym.ClassType;
   if ct=TConstByRefParamSymbol then
      Result:=GetConstByRefParamExpr(TConstByRefParamSymbol(selfSym))
   else if ct=TVarParamSymbol then
      Result:=GetVarParamExpr(TVarParamSymbol(selfSym))
   else begin
      Assert((ct=TSelfSymbol) or (ct=TParamSymbol));
      Result:=GetVarExpr(aScriptPos, selfSym);
   end;
end;

function TdwsCompiler.CheckParams(tableA, tableB : TParamsSymbolTable; checkNames : Boolean; skipB : Integer = 0) : Boolean;
var
   x : Integer;
   r : Boolean;
   paramA, paramB : TParamSymbol;
   semanticsA, semanticsB : TParamSymbolSemantics;
begin
   Result:=True;
   for x:=0 to tableA.Count-1 do begin
      r:=False;
      paramA:=tableA[x];
      paramB:=tableB[x+skipB];
      semanticsA := paramA.Semantics;
      semanticsB := paramB.Semantics;
      if checkNames and not UnicodeSameText(paramA.Name, paramB.Name) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterName, [x, tableA[x].Name])
      else if not paramA.Typ.IsCompatible(paramB.Typ) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterType,
                                   [x, paramA.Typ.Caption, paramB.Typ.Caption])
      else if (semanticsA=pssVar) and (semanticsB<>pssVar) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_VarParameterExpected, [x, paramA.Name])
      else if (semanticsA<>pssVar) and (semanticsB=pssVar) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ValueParameterExpected, [x, paramA.Name])
      else if (semanticsA=pssConst) and (semanticsB<>pssConst) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ConstParameterExpected, [x, paramA.Name])
      else if (semanticsA<>pssConst) and (semanticsB=pssConst) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ValueParameterExpected, [x, paramA.Name])
      else if     (paramA.ClassType<>TParamSymbolWithDefaultValue)
              and (paramB.ClassType=TParamSymbolWithDefaultValue) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MismatchingParameterDefaultValues, [x, paramA.Name])
      else if     (paramA.ClassType=TParamSymbolWithDefaultValue)
              and (paramB.ClassType=TParamSymbolWithDefaultValue)
              and not DWSSameData(TParamSymbolWithDefaultValue(paramA).DefaultValue,
                                  TParamSymbolWithDefaultValue(paramB).DefaultValue) then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_MismatchingParameterDefaultValues, [x, paramA.Name])
      else r:=True;
      Result:=Result and r;
   end;
end;

// CompareFuncKinds
//
procedure TdwsCompiler.CompareFuncKinds(a, b : TFuncKind);
const
   cErrorForFunkKind : array [TFuncKind] of String = (
      CPE_FunctionExpected, CPE_ProcedureExpected, CPE_ConstructorExpected,
      CPE_DestructorExpected, CPE_MethodExpected, CPE_LambdaExpected
   );
begin
   if a<>b then
      FMsgs.AddCompilerError(FTok.HotPos, cErrorForFunkKind[a]);
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
   if FCurrentStructure<>nil then
      Exit(FCurrentStructure);
   prog:=CurrentProg;
   while prog.ClassType=TdwsProcedure do begin
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
   for sym in CurrentProg.Table do begin
      if sym.ClassType=TDataSymbol then begin
         if (sym.Typ<>nil) and (sym.Typ.UnAliasedType.ClassType=TDynamicArraySymbol) then continue;
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

   DoHintUnusedPrivateSymbols(CurrentProg.Table);
   for i:=0 to CurrentProg.UnitMains.Count-1 do begin
      ums:=CurrentProg.UnitMains[i];
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

      if not isVirtual then begin

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
      Result:=TConnectorCallExpr.Create(FTok.HotPos, Name, BaseExpr, IsWrite);
      try
         ReadArguments(Result.AddArg, ttBLEFT, ttBRIGHT, argPosArray);
         if not Result.AssignConnectorSym(CurrentProg, connectorType) then begin
            Result.Orphan(FCompilerContext);
            Result:=nil;
         end;
      except
         Result.Orphan(FCompilerContext);
         raise;
      end;
  end;

var
   connWrite : TConnectorWriteMemberExpr;
   connRead : TConnectorReadMemberExpr;
   rightExpr : TTypedExpr;
begin
   if FTok.Test(ttALEFT) then begin

      Result:=ReadConnectorArray(name, baseExpr, connectorType, isWrite);

   end else if FTok.Test(ttBLEFT) then begin

      Result:=TryConnectorCall;
      if Result=nil then
         Result:=TConstExpr.CreateNull(FCompilerContext.TypVariant); // keep compiling

   end else if not isWrite then begin

      Result:=TConnectorReadMemberExpr.CreateNew(FTok.HotPos, name, baseExpr, connectorType);
      if Result=nil then begin
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                                  [name, connectorType.ConnectorCaption]);
      end;

   end else if FTok.TestDelete(ttASSIGN) then begin

      // An assignment of the form "connector.member := expr" was found
      // and is transformed into "connector.member(expr)"
      try
         rightExpr:=ReadExpr;
         connWrite:=TConnectorWriteMemberExpr.CreateNew(FCompilerContext, FTok.HotPos, name, baseExpr, rightExpr, connectorType);
         baseExpr:=nil;
         if connWrite=nil then begin
            connWrite.Orphan(FCompilerContext);
            connWrite:=nil;
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorMember,
                                  [name, connectorType.ConnectorCaption]);
         end;
         Result:=connWrite;
      except
         OrphanAndNil(baseExpr);
         raise;
      end;

   end else begin

      // It's possible that we should read a connector member or
      // call a connector function without arguments.
      connRead:=TConnectorReadMemberExpr.CreateNew(FTok.HotPos, name, baseExpr, connectorType);

      if connRead=nil then begin
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
function TdwsCompiler.ReadConnectorArray(const Name: String; var baseExpr: TTypedExpr;
            const ConnectorType: IConnectorType; IsWrite: Boolean): TConnectorCallExpr;
var
   argPosArray : TScriptPosArray;
begin
   Result:=TConnectorCallExpr.Create(FTok.HotPos, Name, baseExpr, IsWrite, True);
   try
      baseExpr := nil;
      ReadArguments(Result.AddArg, ttALEFT, ttARIGHT, argPosArray);

      if IsWrite then begin
         if FTok.TestDelete(ttASSIGN) then
            Result.AddArg(ReadExpr)
         else Result.IsWrite:=False;
      end;

      if not Result.AssignConnectorSym(CurrentProg, connectorType) then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorIndex, [ConnectorType.ConnectorCaption]);
   except
      Result.Orphan(FCompilerContext);
      raise;
   end;
end;

function TdwsCompiler.ReadStringArray(expr : TDataExpr; IsWrite: Boolean): TProgramExpr;
var
   indexExpr, valueExpr: TTypedExpr;
   scriptPos : TScriptPos;
   n : Integer;
begin
   scriptPos := FTok.HotPos;
   indexExpr := ReadExpr;
   try
      if not (indexExpr.IsOfType(FCompilerContext.TypInteger) or indexExpr.IsOfType(FCompilerContext.TypVariant)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);

      if FTok.TestDelete(ttASSIGN) and IsWrite then begin
         valueExpr:=ReadExpr;
         if valueExpr is TConstStringExpr then begin
            n:=Length(TConstStringExpr(valueExpr).Value);
            if n<>1 then
               FMsgs.AddCompilerErrorFmt(scriptPos, RTE_InvalidInputDataSize, [n, 1]);
         end;
         if Expr is TStrVarExpr then begin
            if (valueExpr is TMagicStringFuncExpr) and (TMagicStringFuncExpr(valueExpr).FuncSym.Name='Chr') then begin
               Result:=TVarStringArraySetChrExpr.Create(FCompilerContext, scriptPos, expr, indexExpr,
                                                        TMagicStringFuncExpr(valueExpr).Args[0] as TTypedExpr);
               TMagicStringFuncExpr(valueExpr).Args.Clear;
               OrphanAndNil(valueExpr);
            end else Result:=TVarStringArraySetExpr.Create(FCompilerContext, scriptPos, expr, indexExpr, valueExpr);
         end else begin
            if not expr.IsWritable then
               FMsgs.AddCompilerError(scriptPos, CPE_CantWriteToLeftSide);
            Result := TStringArraySetExpr.Create(FCompilerContext, scriptPos, expr, indexExpr, valueExpr);
         end;
      end else Result := TStringArrayOpExpr.CreatePos(FCompilerContext, scriptPos, expr, indexExpr);
   except
      OrphanAndNil(indexExpr);
      raise;
   end;
end;

// CreateProgram
//
function TdwsCompiler.CreateProgram(const systemTable : ISystemSymbolTable;
                                    resultType : TdwsResultType;
                                    const stackParams : TStackParameters;
                                    const mainFileName : String) : TdwsMainProgram;
begin
   Result:=TdwsMainProgram.Create(systemTable, resultType, stackParams, mainFileName);
end;

// ReadEnumeration
//
function TdwsCompiler.ReadEnumeration(const typeName : String;
                                      aStyle : TEnumerationSymbolStyle) : TEnumerationSymbol;
var
   name, deprecatedMsg : String;
   elemSym : TElementSymbol;
   constExpr : TTypedExpr;
   enumInt, enumIntPrev : Int64;
   namePos : TScriptPos;
   isUserDef, overflowed : Boolean;
begin
   Result := TEnumerationSymbol.Create(TypeName, FCompilerContext.TypInteger, aStyle);
   try
      if aStyle=enumFlags then
         enumInt:=1
      else enumInt:=0;
      overflowed:=False;

      repeat
         // Read a member of the enumeration
         if not FTok.TestDeleteNamePos(name, namePos) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         if FTok.Test(ttDEPRECATED) then
            deprecatedMsg := ReadDeprecatedMessage(False)
         else deprecatedMsg := '';

         // Member has a user defined value
         if FTok.TestDelete(ttEQ) then begin
            if aStyle=enumFlags then
               FMsgs.AddCompilerError(FTok.HotPos, CPE_FlagEnumerationCantHaveUserValues);
            constExpr:=ReadExpr;

            if constExpr<>nil then begin
               if not constExpr.IsConstant then begin
                  OrphanAndNil(constExpr);
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
               end else if not FCompilerContext.TypInteger.IsCompatible(constExpr.Typ) then begin
                  OrphanAndNil(constExpr);
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);
               end;
            end;

            if Assigned(constExpr) then begin
               if aStyle<>enumFlags then
                  enumInt:=constExpr.EvalAsInteger(FExec);
               OrphanAndNil(constExpr);
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

         if deprecatedMsg <> '' then
            elemSym.DeprecatedMessage := deprecatedMsg;

         enumIntPrev:=enumInt;
         if aStyle=enumFlags then
            enumInt:=enumInt*2
         else Inc(enumInt);
         overflowed:=(enumInt<enumIntPrev);

         // Add member symbol to table and enumeration type
         if aStyle=enumClassic then begin
            CurrentProg.Table.AddSymbol(elemSym);
            elemSym.IncRefCount;
         end;
         Result.AddElement(elemSym);

         // Add member symbol to Symbol Dictionary
         RecordSymbolUse(elemSym, namePos, [suDeclaration]);

      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

   except
      OrphanObject(Result);
      raise;
   end;
end;

// ReadUses
//
procedure TdwsCompiler.ReadUses;
var
   names : TSimpleStringList;
   x, y, z, u : Integer;
   rt, rtInterface : TSymbolTable;
   rSym : TSymbol;
   unitSymbol : TUnitSymbol;
   posArray : TScriptPosArray;
begin
   names:=FStringListPool.Acquire;
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
               rt:=CurrentProg.Table;
            rtInterface:=CurrentUnitSymbol.InterfaceTable;
            u:=1;
         end else rt:=CurrentUnitSymbol.InterfaceTable;
      end else rt:=CurrentProg.Root.RootTable;
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
            if (unitSymbol<>nil) and (unitSymbol.Main<>nil) then begin
               if unitSymbol.IsDeprecated then
                  WarnDeprecatedSymbol(posArray[x], unitSymbol.Main, unitSymbol.Main.DeprecatedMessage);
               z:=rt.IndexOfParent(unitSymbol.Table);
               if z>u then
                  rt.MoveParent(z, u);
            end;
         end;
         rSym:=CurrentProg.UnitMains.Find(names[x]);
         if rSym<>nil then begin
            if coSymbolDictionary in Options then
               RecordSymbolUse(rSym, posArray[x], [suReference]);
            if CurrentUnitSymbol<>nil then begin
               if rSym.ClassType=TUnitSymbol then
                  rSym:=TUnitSymbol(rSym).Main;
               Assert(rSym.ClassType=TUnitMainSymbol);
               CurrentUnitSymbol.AddDependency(TUnitMainSymbol(rSym));
            end;
         end;
      end;
   finally
      FStringListPool.Release(names);
   end;
end;

// ReadUnitHeader
//
function TdwsCompiler.ReadUnitHeader : TScriptSourceType;
var
   name, part : String;
   namePos, partPos : TScriptPos;
   contextFix : TdwsSourceContext;
begin
   if not FTok.TestDelete(ttUNIT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_UnitExpected);

   if FTok.TestDelete(ttNAMESPACE) then
      Result:=stUnitNamespace
   else Result:=stUnit;

   if not FTok.TestDeleteNamePos(name, namePos) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

   while FTok.TestDelete(ttDOT) do begin
      if not FTok.TestDeleteNamePos(part, partPos) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      name:=name+'.'+part;
   end;

   if CurrentUnitSymbol=nil then begin
      // special case of a unit compiled directly (not through a main program)
      FCurrentSourceUnit:=TSourceUnit.Create(name, CurrentProg.Root.RootTable, CurrentProg.UnitMains);
      CurrentSourceUnit.Symbol.InitializationRank:=FUnits.Count;
      FUnits.Add(FCurrentSourceUnit);
      FCurrentUnitSymbol:=CurrentSourceUnit.Symbol;
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
   if not (   UnicodeSameText(name, namePos.SourceFile.Name)
           or UnicodeSameText(MSG_MainModule, namePos.SourceFile.Name)) then
      FMsgs.AddCompilerWarning(namePos, CPE_UnitNameDoesntMatch);

   // usually deprecated statement follows after the semi
   // but for units, Delphi wants it before, this supports both forms
   if FTok.Test(ttDEPRECATED) then
      CurrentUnitSymbol.DeprecatedMessage:=ReadDeprecatedMessage
   else begin
      ReadSemiColon;
      CurrentUnitSymbol.DeprecatedMessage:=ReadDeprecatedMessage;
   end;

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
                                   left : TDataExpr; right : TTypedExpr) : TProgramExpr;
begin
   Result := CreateAssignExpr(FCompilerContext, scriptPos, token, left, right);
end;

// CreateArrayLow
//
function TdwsCompiler.CreateArrayLow(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
begin
   if typ is TStaticArraySymbol then
      Result := (FCompilerContext.CreateConstExpr(TStaticArraySymbol(typ).IndexType, TStaticArraySymbol(typ).LowBound) as TTypedExpr)
   else begin
      Assert(typ.ClassType=TDynamicArraySymbol);
      Result := FUnifiedConstants.CreateInteger(0)
   end;
   if captureBase then
      OrphanAndNil(baseExpr);
end;

// CreateArrayHigh
//
function TdwsCompiler.CreateArrayHigh(baseExpr : TProgramExpr; typ : TArraySymbol; captureBase : Boolean) : TTypedExpr;
begin
   Assert(baseExpr<>nil);
   if typ is TOpenArraySymbol then begin
      Result:=TOpenArrayLengthExpr.Create(FCompilerContext, baseExpr as TDataExpr, captureBase);
      TOpenArrayLengthExpr(Result).Delta:=-1;
   end else if typ is TDynamicArraySymbol then begin
      Result:=TArrayLengthExpr.Create(FCompilerContext, baseExpr as TTypedExpr, captureBase);
      TArrayLengthExpr(Result).Delta:=-1;
   end else begin
      if captureBase then
         OrphanAndNil(baseExpr);
      Assert(typ is TStaticArraySymbol);
      Result := (FCompilerContext.CreateConstExpr(TStaticArraySymbol(typ).IndexType, TStaticArraySymbol(typ).HighBound) as TTypedExpr)
   end;
end;

// CreateArrayLength
//
function TdwsCompiler.CreateArrayLength(baseExpr : TTypedExpr; typ : TArraySymbol) : TTypedExpr;
begin
   Assert(baseExpr<>nil);
   if typ is TOpenArraySymbol then begin
      Result:=TOpenArrayLengthExpr.Create(FCompilerContext, baseExpr as TDataExpr, True);
   end else if typ is TDynamicArraySymbol then begin
      Result:=TArrayLengthExpr.Create(FCompilerContext, baseExpr, True);
   end else begin
      Assert(typ is TStaticArraySymbol);
      OrphanAndNil(baseExpr);
      Result := FUnifiedConstants.CreateInteger(TStaticArraySymbol(typ).ElementCount);
   end;
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
         Result:=TOpenArrayExpr.Create(scriptPos, baseExpr, indexExpr,
                                       TOpenArraySymbol(baseType))
      else begin
         Result:=TStaticArrayExpr.Create(scriptPos, baseExpr, indexExpr,
                                         TStaticArraySymbol(baseType));
      end;

   end else begin

      Assert(baseType.ClassType=TDynamicArraySymbol);

      Result:=TDynamicArrayExpr.Create(scriptPos, baseExpr, indexExpr,
                                       TDynamicArraySymbol(baseType));

   end;
end;

// EnsureLoopVarExpr
//
function TdwsCompiler.EnsureLoopVarExpr(const loopPos : TScriptPos;
         const loopVarName : String; const loopVarNamePos : TScriptPos;
         var loopVarExpr : TVarExpr; loopVarTyp : TTypeSymbol) : TBlockExpr;
var
   loopVarSymbol : TDataSymbol;
begin
   if loopVarExpr<>nil then begin

      if (not loopVarExpr.Typ.IsCompatible(loopVarTyp) and (not loopVarExpr.Typ.IsOfType(loopVarTyp))) then begin
         IncompatibleTypes(loopVarNamePos, CPE_IncompatibleTypes, loopVarExpr.Typ, loopVarTyp);
         OrphanAndNil(loopVarExpr);
      end else Exit(nil);

   end;

   Result:=TBlockExpr.Create(FCompilerContext, loopPos);
   loopVarSymbol:=TDataSymbol.Create(loopVarName, loopVarTyp);
   Result.Table.AddSymbol(loopVarSymbol);
   RecordSymbolUse(loopVarSymbol, loopVarNamePos, [suDeclaration, suReference, suWrite]);
   loopVarExpr:=GetVarExpr(loopVarNamePos, loopVarSymbol);
   CurrentProg.InitExpr.AddStatement(TInitDataExpr.Create(FCompilerContext, loopVarNamePos, loopVarExpr));
   loopVarExpr.IncRefCount;
end;

// CreateTypedOperatorExpr
//
function TdwsCompiler.CreateTypedOperatorExpr(token : TTokenType; const scriptPos : TScriptPos;
                                              aLeft, aRight : TTypedExpr) : TTypedExpr;
var
   opSym : TOperatorSymbol;
   funcExpr : TFuncExprBase;
begin
   Result:=nil;
   if (aLeft=nil) or (aRight=nil) then Exit;
   opSym:=ResolveOperatorFor(CurrentProg, token, aLeft.Typ, aRight.Typ);
   if opSym=nil then Exit;

   if opSym.OperatorExprClass <> nil then begin
      Result := TBinaryOpExprClass(opSym.OperatorExprClass).Create(FCompilerContext, scriptPos, token, aLeft, aRight);
   end else if opSym.UsesSym<>nil then begin
      if opSym.UsesSym is TMethodSymbol then
         funcExpr:=CreateMethodExpr(FCompilerContext, TMethodSymbol(opSym.UsesSym), aLeft, rkObjRef, scriptPos, [])
      else begin
         funcExpr:=GetFuncExpr(opSym.UsesSym);
         funcExpr.AddArg(aLeft);
      end;
      funcExpr.AddArg(aRight);
      TypeCheckArguments(FCompilerContext, funcExpr, nil);
      Result:=funcExpr;
   end;
   if Optimize then
      Result:=Result.OptimizeToTypedExpr(FCompilerContext, scriptPos);
end;

// CreateAssignOperatorExpr
//
function TdwsCompiler.CreateAssignOperatorExpr(token : TTokenType; const scriptPos : TScriptPos;
                                               exec : TdwsExecution;
                                               aLeft : TDataExpr; aRight : TTypedExpr) : TProgramExpr;
begin
   Result := dwsCompilerUtils.CreateAssignExpr(
      FCompilerContext, scriptPos,
      token, aLeft, aRight
   );
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
   if coContextMap in Options then begin
      while     (FSourceContextMap.Current<>nil)
            and (FSourceContextMap.Current.StartPos.SourceFile=sourceFile) do begin
         FSourceContextMap.CloseContext(FTok.CurrentPos);
      end;
   end;
end;

// EnterUnit
//
procedure TdwsCompiler.EnterUnit(srcUnit : TSourceUnit; var oldSrcUnit : TSourceUnit);
begin
   oldSrcUnit:=FCurrentSourceUnit;
   FCurrentSourceUnit:=srcUnit;

   CurrentUnitSymbol.StoreParents;
   FCurrentUnitSymbol:=srcUnit.Symbol;
   CurrentUnitSymbol.RestoreParents;
end;

// LeaveUnit
//
procedure TdwsCompiler.LeaveUnit(oldSrcUnit : TSourceUnit);
begin
   CurrentUnitSymbol.StoreParents;
   if oldSrcUnit<>nil then
      FCurrentUnitSymbol:=oldSrcUnit.Symbol
   else FCurrentUnitSymbol:=nil;
   CurrentUnitSymbol.RestoreParents;
   FCurrentSourceUnit:=oldSrcUnit;
end;

// SwitchTokenizerToUnit
//
procedure TdwsCompiler.SwitchTokenizerToUnit(srcUnit : TSourceUnit; const sourceCode : String);
var
   sourceFile : TSourceFile;
   oldUnit : TSourceUnit;
begin
   sourceFile:=FMainProg.SourceList.Add(srcUnit.GetUnitName, sourceCode, stUnit);

   EnterUnit(srcUnit, oldUnit);
   CurrentProg.EnterSubTable(CurrentUnitSymbol.Table);
   FUnitsFromStack.Push(sourceFile.Name);
   try
      ReadScript(sourceFile, stUnit);
   finally
      FUnitsFromStack.Pop;
      LeaveUnit(oldUnit);
      CurrentProg.LeaveSubTable;
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
      Result:=(TDeclaredExpr.FindSymbol(CurrentProg.Root.RootTable, name)<>nil);
   end;

   function CreateDefault(argTyp : TTypeSymbol) : TExprBase;
   var
      data : TData;
   begin
      SetLength(data, argTyp.Size);
      argTyp.InitData(data, 0);
      if argTyp is TBaseSymbol then
         Result := FCompilerContext.CreateConstExpr(argTyp, data[0])
      else Result := TConstExpr.Create(argTyp, data);
   end;

var
   argExpr, msgExpr, operandExpr : TTypedExpr;
   argTyp : TTypeSymbol;
   argPos : TScriptPos;
begin
   msgExpr:=nil;

   if specialKind=skDebugBreak then begin

      argExpr:=nil;
      argTyp:=nil;
      if FTok.TestDelete(ttBLEFT) then
         if not FTok.TestDelete(ttBRIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

   end else begin

      if not FTok.TestDelete(ttBLEFT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

      FTok.HasTokens;  // get token in buffer for correct argPos below
      argPos:=FTok.HotPos;
      case specialKind of
         skAssigned :
            argExpr:=ReadExpr(FCompilerContext.TypNil);
         skInc, skDec :
            argExpr:=ReadTerm(True);
         skLow, skHigh, skDefault :
            argExpr:=ReadExpr(FCompilerContext.TypAnyType);
      else
         argExpr:=ReadExpr;
      end;
      if argExpr=nil then
         argTyp:=nil
      else begin
         argTyp:=argExpr.Typ;
         if argTyp<>nil then
            argTyp:=argTyp.UnAliasedType;
      end;

      if not Assigned(argTyp) then begin
         OrphanAndNil(argExpr);
         FMsgs.AddCompilerStop(argPos, CPE_InvalidOperands);
      end;

   end;

   try
      Result := nil;

      case specialKind of
         skAbs : begin
            if argTyp.IsOfType(FCompilerContext.TypInteger) then
               Result:=TAbsIntExpr.Create(FCompilerContext, argExpr)
            else if argTyp.IsOfType(FCompilerContext.TypFloat) then
               Result:=TAbsFloatExpr.Create(FCompilerContext, argExpr)
            else if argTyp.IsOfType(FCompilerContext.TypVariant) then
               Result:=TAbsVariantExpr.Create(FCompilerContext, argExpr)
            else begin
               Result:=dwsInternalUnit.HandleAbs(FCompilerContext, argExpr);
               if Result=nil then
                  FMsgs.AddCompilerError(argPos, CPE_NumericalExpected);
            end;
            argExpr:=nil;
         end;
         skAssert : begin
            if not argTyp.IsOfType(FCompilerContext.TypBoolean) then
               FMsgs.AddCompilerError(argPos, CPE_BooleanExpected);
            if FTok.TestDelete(ttCOMMA) then begin
               FTok.HasTokens;
               argPos:=FTok.HotPos;
               msgExpr:=ReadExpr;
               if (msgExpr=nil) or (not msgExpr.IsOfType(FCompilerContext.TypString)) then
                  FMsgs.AddCompilerError(argPos, CPE_StringExpected);
            end;
            if coAssertions in FOptions then begin
               Result:=TAssertExpr.Create(FCompilerContext, namePos, argExpr, msgExpr);
               argExpr:=nil;
               msgExpr:=nil;
            end else begin
               Result:=TNullExpr.Create(namePos);
               OrphanAndNil(argExpr);
               OrphanAndNil(msgExpr);
            end;
         end;
         skAssigned : begin
            if argTyp is TClassSymbol then
               Result:=TAssignedInstanceExpr.Create(FCompilerContext, argExpr)
            else if argTyp is TInterfaceSymbol then
               Result:=TAssignedInterfaceExpr.Create(FCompilerContext, argExpr)
            else if argTyp is TClassOfSymbol then
               Result:=TAssignedMetaClassExpr.Create(FCompilerContext, argExpr)
            else if argTyp.AsFuncSymbol<>nil then
               Result:=TAssignedFuncPtrExpr.Create(FCompilerContext, argExpr)
            else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
               OrphanAndNil(argExpr);
            end;
            argExpr:=nil;
         end;
         skHigh : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayHigh(argExpr, TArraySymbol(argTyp), True);
            end else if argTyp is TEnumerationSymbol then begin
               OrphanAndNil(argExpr);
               Result := TConstIntExpr.Create(argTyp, TEnumerationSymbol(argTyp).HighBound)
            end else if argTyp.IsOfType(FCompilerContext.TypString) and Assigned(argExpr) then begin
               Result := TStringLengthExpr.Create(FCompilerContext, argExpr);
            end else if argTyp=FCompilerContext.TypInteger then begin
               OrphanAndNil(argExpr);
               Result := FUnifiedConstants.CreateInteger(High(Int64));
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
               OrphanAndNil(argExpr);
            end;
            argExpr:=nil;
         end;
         skInc, skDec, skSucc, skPred : begin
            if     (specialKind in [skInc, skDec])
               and not (   (argExpr is TDataExpr)
                        and TDataExpr(argExpr).IsWritable) then
               FMsgs.AddCompilerErrorFmt(argPos, CPE_ConstVarParam, [0, 'a'])
            else if    argTyp.IsOfType(FCompilerContext.TypInteger)
                    or (argTyp is TEnumerationSymbol) then begin
               if specialKind in [skInc, skDec] then
                  WarnForVarUsage(TVarExpr(argExpr), argPos);
               if FTok.TestDelete(ttCOMMA) then begin
                  operandExpr:=ReadExpr;
                  if operandExpr=nil then
                     FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected)
                  else begin
                     if operandExpr.IsOfType(FCompilerContext.TypVariant) then
                        operandExpr:=TConvVarToIntegerExpr.Create(FCompilerContext, operandExpr);
                     if not operandExpr.IsOfType(FCompilerContext.TypInteger) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);
                  end;
               end else operandExpr := FUnifiedConstants.CreateInteger(1);
               case specialKind of
                  skInc : Result:=TIncVarFuncExpr.Create(FCompilerContext, argPos, argExpr, operandExpr);
                  skDec : Result:=TDecVarFuncExpr.Create(FCompilerContext, argPos, argExpr, operandExpr);
                  skSucc : Result:=TSuccFuncExpr.Create(FCompilerContext, argPos, argExpr, operandExpr);
                  skPred : Result:=TPredFuncExpr.Create(FCompilerContext, argPos, argExpr, operandExpr);
               end;
               argExpr:=nil;
            end else FMsgs.AddCompilerError(argPos, CPE_IntegerExpected);
         end;
         skLength : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayLength(argExpr, TArraySymbol(argTyp));
            end else if ((argTyp=FCompilerContext.TypString) or (argTyp.IsOfType(FCompilerContext.TypVariant))) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FCompilerContext, argExpr);
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
               OrphanAndNil(argExpr);
            end;
            argExpr:=nil;
         end;
         skLow : begin
            if argTyp is TArraySymbol then begin
               Result:=CreateArrayLow(argExpr, TArraySymbol(argTyp), True);
            end else begin
               OrphanAndNil(argExpr);
               if argTyp is TEnumerationSymbol then begin
                  Result := TConstIntExpr.Create(argTyp, TEnumerationSymbol(argTyp).LowBound);
               end else if argTyp=FCompilerContext.TypString then begin
                  Result := FUnifiedConstants.CreateInteger(1);
               end else if (argTyp=FCompilerContext.TypInteger) then begin
                  Result := FUnifiedConstants.CreateInteger(Low(Int64));
               end else begin
                  FMsgs.AddCompilerError(argPos, CPE_InvalidArgumentType);
               end;
            end;
            argExpr:=nil;
         end;
         skOrd : begin
            if argTyp.IsOfType(FCompilerContext.TypInteger) or argTyp.InheritsFrom(TEnumerationSymbol) then begin
               Result:=TOrdIntExpr.Create(FCompilerContext, argExpr);
            end else if argTyp=FCompilerContext.TypBoolean then begin
               Result:=TOrdBoolExpr.Create(FCompilerContext, argExpr);
            end else if argTyp=FCompilerContext.TypString then begin
               if argExpr is TStringArrayOpExpr then
                  Result:=TOrdIntExpr.Create(FCompilerContext, argExpr)
               else Result:=TOrdStrExpr.Create(FCompilerContext, argExpr);
            end else if argTyp=FCompilerContext.TypVariant then begin
               Result:=TOrdExpr.Create(FCompilerContext, argExpr);
            end else begin
               FMsgs.AddCompilerError(argPos, CPE_InvalidOperands);
               OrphanAndNil(argExpr);
            end;
            argExpr:=nil;
         end;
         skDefault : begin
            if argTyp <> nil then
               Result := CreateTypedDefault(argTyp) as TProgramExpr
            else begin
               FMsgs.AddCompilerError(argPos, CPE_TypeExpected);
               Result := TConstExpr.CreateNull(FCompilerContext.TypVariant);
            end;
            OrphanAndNil(argExpr);
         end;
         skSizeOf : begin
            Result := FUnifiedConstants.CreateInteger(argTyp.Size);
            OrphanAndNil(argExpr);
         end;
         skDefined, skDeclared : begin
            if FIsSwitch then begin
               if not argExpr.IsOfType(FCompilerContext.TypString) then
                  FMsgs.AddCompilerError(argPos, CPE_StringExpected);
               if not argExpr.IsConstant then
                  FMsgs.AddCompilerStop(argPos, CPE_ConstantExpressionExpected);
               try
                  case SpecialKind of
                     skDefined :
                        Result := FUnifiedConstants.CreateBoolean(EvaluateDefined(argExpr));
                     skDeclared :
                        Result := FUnifiedConstants.CreateBoolean(EvaluateDeclared(argExpr));
                  end;
               finally
                  OrphanAndNil(argExpr);
               end;
            end else begin
               case SpecialKind of
                  skDefined :
                     Result:=TDefinedExpr.Create(FCompilerContext, argExpr);
                  skDeclared : begin
                     if not argExpr.IsOfType(FCompilerContext.TypString) then
                        FMsgs.AddCompilerError(argPos, CPE_StringExpected);
                     Result:=TDeclaredExpr.Create(FCompilerContext, argExpr);
                  end;
               end;
               argExpr:=nil;
            end;
         end;
         skDebugBreak : begin
            Result:=TDebugBreakExpr.Create(namePos);
         end;
         skConditionalDefined : begin
            if not argExpr.IsOfType(FCompilerContext.TypString) then
               FMsgs.AddCompilerError(argPos, CPE_StringExpected);
            Result:=TConditionalDefinedExpr.Create(FCompilerContext, argExpr);
            argExpr:=nil;
         end;
         skInclude, skExclude : begin
            if not FTok.TestDelete(ttCOMMA) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_CommaExpected);
            Result:=ReadIncludeExclude(namePos, specialKind, argExpr, argPos);
         end;
         skSwap : begin
            if not ((argExpr is TDataExpr) and TDataExpr(argExpr).IsWritable) then begin
               FMsgs.AddCompilerError(argPos, CPE_VariableExpected);
               OrphanAndNil(argExpr);
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
                  OrphanAndNil(argExpr);
               end else if (argExpr<>nil) then begin
                  if (msgExpr=nil) or not (msgExpr.IsOfType(argTyp) and argTyp.IsOfType(msgExpr.Typ)) then
                     IncompatibleTypes(namePos, CPE_IncompatibleTypes, argTyp, msgExpr.Typ);
               end;
            end;
            Result:=TSwapExpr.Create(FCompilerContext, namePos, TDataExpr(argExpr), TDataExpr(msgExpr));
            argExpr:=nil;
            msgExpr:=nil;
         end;
      end;

      if argExpr<>nil then begin
         OrphanAndNil(argExpr);
      end;
      if Result=nil then begin
         // fake expression to keep compiling
         case SpecialKind of
            skDefined, skDeclared, skAssigned :
               Result := FUnifiedConstants.CreateBoolean(False);
         else
            Result := FUnifiedConstants.CreateInteger(0);
         end;
      end else if Optimize then
         Result:=Result.Optimize(FCompilerContext);

      if specialKind<>skDebugBreak then begin
         try
            if not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
         except
            OrphanAndNil(Result);
            raise;
         end;
      end;

   except
      OrphanAndNil(argExpr);
      OrphanAndNil(msgExpr);
      raise;
   end;
end;

// ReadIncludeExclude
//
function TdwsCompiler.ReadIncludeExclude(const namePos : TScriptPos; specialKind : TSpecialKeywordKind;
                                         var argExpr : TTypedExpr; const argPos : TScriptPos) : TProgramExpr;
var
   operandExpr : TTypedExpr;
   typ : TTypeSymbol;
   setType : TSetOfSymbol;
   operandPos : TScriptPos;
begin
   if not ((argExpr is TDataExpr) and TDataExpr(argExpr).IsWritable) then
      FMsgs.AddCompilerError(argPos, CPE_VariableExpected);

   typ:=argExpr.Typ.UnAliasedType;
   if typ is TSetOfSymbol then begin
      setType:=TSetOfSymbol(typ);
      typ:=setType.Typ;
   end else begin
      FMsgs.AddCompilerError(argPos, CPE_SetExpected);
      typ:=nil;
   end;

   operandPos:=FTok.HotPos;
   operandExpr:=ReadExpr(typ);
   if (typ<>nil) and (operandExpr<>nil) and not operandExpr.IsOfType(typ) then
      IncompatibleTypes(operandPos, CPE_IncompatibleParameterTypes, typ, operandExpr.Typ);

   if specialKind=skInclude then
      Result:=TSetOfIncludeExpr.Create(FCompilerContext, namePos, TDataExpr(argExpr), operandExpr)
   else Result:=TSetOfExcludeExpr.Create(FCompilerContext, namePos, TDataExpr(argExpr), operandExpr);
   argExpr:=nil;
end;

// ReadTypeCast
//
function TdwsCompiler.ReadTypeCast(const namePos : TScriptPos; typeSym : TTypeSymbol) : TTypedExpr;
var
   argExpr : TTypedExpr;
   argTyp : TTypeSymbol;
   hotPos : TScriptPos;
   connCast : IConnectorCast;
   casterExprClass : TTypedExprClass;
begin
   hotPos:=FTok.CurrentPos;
   argExpr:=ReadExpr;

   Result:=nil;
   try
      if not FTok.TestDelete(ttBRIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

      if argExpr=nil then begin

         Result := CreateTypedDefault(typeSym);
         Exit;

      end;

      argTyp:=argExpr.Typ;
      if argTyp<>nil then
         argTyp:=argTyp.UnAliasedType;

      casterExprClass := FOperators.FindCaster(typeSym, argTyp);
      if casterExprClass <> nil then begin

         if casterExprClass.InheritsFrom(TUnaryOpExpr) then
            Result := TUnaryOpExprClass(casterExprClass).Create(FCompilerContext, argExpr)
         else begin
            Assert(casterExprClass.InheritsFrom(TUnaryOpDataExpr));
            Result := TUnaryOpDataExprClass(casterExprClass).Create(FCompilerContext, argExpr);
         end;

      end else if typeSym.IsOfType(FCompilerContext.TypInteger) then begin

         // Cast Integer(...)
         if argTyp is TEnumerationSymbol then
            Result := TConvOrdToIntegerExpr.Create(FCompilerContext, argExpr)
         else if argExpr.IsOfType(FCompilerContext.TypBoolean) then
            Result := TOrdBoolExpr.Create(FCompilerContext, argExpr)
         else if argExpr.IsOfType(FCompilerContext.TypInteger) then
            if argExpr.Typ<>typeSym then
               Result := TConvOrdToIntegerExpr.Create(FCompilerContext, argExpr)
            else Result := argExpr
         else if argExpr.IsOfType(FCompilerContext.TypFloat) then
            Result := TConvVarToIntegerExpr.Create(FCompilerContext, argExpr)
         else if argTyp is TSetOfSymbol then begin
            if TSetOfSymbol(argTyp).CountValue>31 then
               FMsgs.AddCompilerError(hotPos, CPE_SetTooLargeForCastToInteger);
            Result := TConvSetOfToIntegerExpr.Create(FCompilerContext, argExpr)
         end else begin
            if not argExpr.IsOfType(FCompilerContext.TypVariant) then
               FMsgs.AddCompilerError(hotPos, CPE_IntegerCastInvalid);
            Result := TConvVarToIntegerExpr.Create(FCompilerContext, argExpr)
         end;
         Result.Typ:=typeSym;

      end else if typeSym = FCompilerContext.TypFloat then begin

         // Cast Float(...)
         if argExpr.IsOfType(FCompilerContext.TypFloat) then
            Result := argExpr
         else begin
            if not argExpr.IsOfType(FCompilerContext.TypVariant) then
               FMsgs.AddCompilerError(hotPos, CPE_NumericalExpected);
            Result := TConvVarToFloatExpr.Create(FCompilerContext, argExpr);
         end;

      end else if typeSym = FCompilerContext.TypString then begin

         // Cast String(...)
         if argExpr.IsOfType(FCompilerContext.TypString) then
            Result:=argExpr
         else begin
            if not argExpr.IsOfType(FCompilerContext.TypVariant) then
               FMsgs.AddCompilerError(hotPos, CPE_VariantExpected);
            Result:=TConvVarToStringExpr.Create(FCompilerContext, argExpr);
         end;

      end else if typeSym = FCompilerContext.TypBoolean then begin

         // Cast Boolean(...)
         if argExpr.IsOfType(FCompilerContext.TypInteger) then
            Result := TConvIntToBoolExpr.Create(FCompilerContext, argExpr)
         else if argExpr.IsOfType(FCompilerContext.TypFloat) then
            Result := TConvFloatToBoolExpr.Create(FCompilerContext, argExpr)
         else if argExpr.IsOfType(FCompilerContext.TypBoolean) then
            Result := argExpr
         else begin
            if not argExpr.IsOfType(FCompilerContext.TypVariant) then
               FMsgs.AddCompilerError(hotPos, CPE_BooleanOrIntegerExpected);
            Result := TConvVarToBoolExpr.Create(FCompilerContext, argExpr);
         end;

      end else if typeSym = FCompilerContext.TypVariant then

         // Cast Variant(...)
         Result := TConvVariantExpr.Create(FCompilerContext, argExpr)

      else if typeSym is TClassOfSymbol then begin

         // Cast Class(...)
         if argTyp is TClassSymbol then
            Result:=TObjAsClassExpr.Create(FCompilerContext, hotPos, argExpr, typeSym)
         else if argTyp is TClassOfSymbol then
            Result:=TClassAsClassExpr.Create(FCompilerContext, hotPos, argExpr, typeSym)
         else FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

      end else if typeSym is TSetOfSymbol then begin

         // Cast Set of ( ... )
         if argExpr.IsOfType(FCompilerContext.TypInteger) then begin

            if TSetOfSymbol(typeSym).CountValue>31 then
               FMsgs.AddCompilerError(hotPos, CPE_SetTooLargeForCastToInteger);
            Result:=TConvIntegerToSetOfExpr.Create(hotPos, argExpr, TSetOfSymbol(typeSym));

         end else FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

      end else if argExpr.Typ is TConnectorSymbol then begin

         connCast:=TConnectorSymbol(argExpr.Typ).ConnectorType.HasCast(typeSym);
         if connCast=nil then
            FMsgs.AddCompilerStop(hotPos, CPE_InvalidOperands);
         Result:=TConnectorCastExpr.CreateCast(FCompilerContext, argExpr, connCast);
         Result.Typ:=typeSym;

      end else FMsgs.AddCompilerStop(hotPos, CPE_InvalidTypeCast);

      if Optimize then
         Result:=Result.OptimizeToTypedExpr(FCompilerContext, hotPos);

   except
      OrphanAndNil(argExpr);
      raise;
   end;
end;

// ReadTypeExpr
//
function TdwsCompiler.ReadTypeExpr(const namePos : TScriptPos; typeSym : TTypeSymbol;
                                   isWrite : Boolean; expecting : TTypeSymbol = nil) : TProgramExpr;

   function CreateClassSymbolExpr(typeSym : TTypeSymbol) : TConstExpr;
   begin
      Result:=TConstExpr.Create(typeSym, Int64(TClassOfSymbol(typeSym).TypClassSymbol));
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
      if     (expecting<>FCompilerContext.TypAnyType)
         and not (   (typeSym.ClassType=TClassSymbol)
                  or (typeSym.ClassType=TClassOfSymbol)) then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_BrackLeftExpected);
      Result:=typeExpr;
   end;
end;

// ReadAttributes
//
procedure TdwsCompiler.ReadAttributes(tokenALEFTAlreadyDeleted : Boolean);
var
   expr : TProgramExpr;
   customAttribute : TClassSymbol;
   hotPos : TScriptPos;
begin
   if not (tokenALEFTAlreadyDeleted or FTok.TestDelete(ttALEFT)) then Exit;

   customAttribute:=FCompilerContext.SystemTable.TypCustomAttribute;

   repeat
      hotPos:=FTok.HotPos;
      expr:=ReadNew(customAttribute, True);
      if not ((expr is TMethodExpr) and (TMethodExpr(expr).MethSym.Kind=fkConstructor)) then
         FMsgs.AddCompilerError(hotPos, CPE_AttributeConstructorExpected);
      FPendingAttributes.Add(TdwsSymbolAttribute.Create(hotPos, TMethodExpr(expr)));
      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(hotPos, CPE_ArrayBracketRightExpected);
   until not FTok.TestDelete(ttALEFT);
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

// AddProcHelper
//
procedure TdwsCompiler.AddProcHelper(func : TFuncSymbol);
var
   name : String;
   namePos : TScriptPos;
   param : TParamSymbol;
begin
   if func.Params.Count=0 then begin
      FMsgs.AddCompilerError(FTok.HotPos, CPE_ParamsExpected);
      param:=nil;
   end else param:=func.Params[0];

   if not FTok.TestDeleteNamePos(name, namePos) then
      name:=func.Name;
   ReadSemiColon;

   if param=nil then Exit;

   CompilerUtils.AddProcHelper(name, CurrentProg.Table, func, CurrentUnitSymbol);
   FHelperMemberNames.Add(name);
end;

// EnumerateHelpers
//
function TdwsCompiler.EnumerateHelpers(typeSym : TTypeSymbol) : THelperSymbols;
begin
   Result:=THelperSymbols.Create;
   if typeSym.ClassType=THelperSymbol then
      Result.Add(THelperSymbol(typeSym))
   else CurrentProg.Table.EnumerateHelpers(typeSym, Result.AddHelper);
end;

// ReadTypeHelper
//
function TdwsCompiler.ReadTypeHelper(expr : TTypedExpr;
                                     const name : String; const namePos : TScriptPos;
                                     expecting : TTypeSymbol; isWrite : Boolean;
                                     killNameToken : Boolean) : TProgramExpr;
var
   i : Integer;
   helper, bestHelper : THelperSymbol;
   helpers : THelperSymbols;
   sym, candidate : TSymbol;
   bestDist, helperDist : Integer;
   meth : TMethodSymbol;
   meta : TStructuredTypeMetaSymbol;
   typeSym : TTypeSymbol;
begin
   Result:=nil;
   typeSym:=expr.Typ;
   bestDist:=MaxInt;

   helpers:=EnumerateHelpers(typeSym);
   try
      if helpers.Count=0 then Exit;

      sym:=nil;
      bestHelper:=nil;
      for i:=0 to helpers.Count-1 do begin
         helper:=helpers[i];
         helperDist:=expr.Typ.DistanceTo(helper.ForType);
         if helperDist<bestDist then begin
            candidate:=helper.Members.FindSymbol(name, cvPublic);
            if candidate<>nil then begin
               sym:=candidate;
               bestDist:=helperDist;
               bestHelper:=helper;
               if helperDist=0 then Break;
            end;
         end;
      end;

      if sym<>nil then begin
         if not (coHintsDisabled in FOptions) then
            CheckMatchingDeclarationCase(name, sym, namePos);

         if killNameToken then
            FTok.KillToken;

         RecordSymbolUseReference(sym, namePos, False);
         if sym.ClassType=TAliasMethodSymbol then
            RecordSymbolUseImplicitReference(TAliasMethodSymbol(sym).Alias, namePos, False);

         if sym.ClassType=TPropertySymbol then begin

            Assert(expr is TTypedExpr);
            Result:=ReadPropertyExpr(TTypedExpr(expr), TPropertySymbol(sym), isWrite);

         end else if sym.ClassType=TClassVarSymbol then begin

            OrphanAndNil(expr);
            Result:=GetVarExpr(namePos, TClassVarSymbol(sym));

         end else if sym is TConstSymbol then begin

            OrphanAndNil(expr);
            Result:=TConstExpr.CreateTyped(FCompilerContext, sym.Typ, TConstSymbol(sym));

         end else if sym is TMethodSymbol then begin

            meth:=TMethodSymbol(sym);
            if meth.IsClassMethod then begin

               if (bestHelper.ForType is TStructuredTypeSymbol) then begin
                  meta:=TStructuredTypeSymbol(bestHelper.ForType).MetaSymbol;
                  if meta<>nil then begin
                     if expr<>nil then begin
                        if expr.Typ is TStructuredTypeSymbol then
                           expr:=TObjToClassTypeExpr.Create(FCompilerContext, expr)
                     end else expr:=TConstExpr.Create(meta, Int64(bestHelper.ForType));
                  end else begin
                     OrphanAndNil(expr);
                  end;
               end;

            end else begin

               if typeSym.ClassType<>THelperSymbol then begin
                  if    (expr is TTypeReferenceExpr)
                     or (expr.Typ is TStructuredTypeMetaSymbol) then begin
                     FMsgs.AddCompilerError(namePos, CPE_ClassMethodExpected);
                     // keep compiling
                     expr:=TConvExpr.Create(FCompilerContext, expr);
                     expr.Typ:=meth.Params[0].Typ;
                  end;
               end;

            end;

            if (expr<>nil) and (expr.Typ is THelperSymbol) then begin
               OrphanAndNil(expr);
            end;

            if meth.IsOverloaded then
               Result:=ReadMethOverloaded(meth, expr, namePos, expecting)
            else Result:=ReadMethod(meth, expr, namePos, expecting);

         end;
      end;
   finally
      helpers.Free;
   end;
end;

// ReadSelfTypeHelper
//
function TdwsCompiler.ReadSelfTypeHelper(const name : TToken; const namePos : TScriptPos;
                                         expecting : TTypeSymbol) : TProgramExpr;
var
   progMeth : TMethodSymbol;
   structSym : TCompositeTypeSymbol;
   selfExpr : TTypedExpr;
begin
   progMeth:=CurrentProg.ContextMethodSymbol;

   if progMeth<>nil then begin
      if progMeth.IsStatic then begin
         structSym:=progMeth.StructSymbol;
         selfExpr:=TConstExpr.Create(structSym.MetaSymbol, Int64(structSym));
      end else if progMeth.SelfSym is TConstByRefParamSymbol then
         selfExpr:=GetConstParamExpr(namePos, TConstByRefParamSymbol(progMeth.SelfSym))
      else if progMeth.SelfSym=nil then
         Exit(nil)
      else selfExpr:=GetSelfParamExpr(namePos, progMeth.SelfSym);
      Result:=ReadTypeHelper(selfExpr, name.AsString, namePos, expecting, False, True);
      if Result=nil then
         OrphanAndNil(selfExpr);
   end else Result:=nil;
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
   FSystemSymbols:=nil;
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
   sysTable : TSystemSymbols;
begin
   sysTable:=TSystemSymbols.Create(nil);
   FSystemSymbols:=sysTable;

   // Create base data types
   sysTable.TypBoolean:=TBaseBooleanSymbol.Create;
   sysTable.AddSymbol(sysTable.TypBoolean);

   sysTable.TypInteger:=TBaseIntegerSymbol.Create;
   sysTable.AddSymbol(sysTable.TypInteger);
   sysTable.AddSymbol(TDynamicArraySymbol.Create('array of integer', sysTable.TypInteger, sysTable.TypInteger));

   sysTable.TypFloat:=TBaseFloatSymbol.Create;
   sysTable.AddSymbol(sysTable.TypFloat);
   sysTable.AddSymbol(TDynamicArraySymbol.Create('array of float', sysTable.TypFloat, sysTable.TypInteger));

   sysTable.TypString:=TBaseStringSymbol.Create;
   sysTable.AddSymbol(sysTable.TypString);
   sysTable.AddSymbol(TDynamicArraySymbol.Create(SYS_ARRAY_OF_STRING, sysTable.TypString, sysTable.TypInteger));

   if Assigned(FOnCreateBaseVariantSymbol) then
      FOnCreateBaseVariantSymbol(sysTable)
   else begin
      sysTable.TypVariant:=TBaseVariantSymbol.Create;
      sysTable.AddSymbol(sysTable.TypVariant);
   end;
   if sysTable.TypVariant<>nil then begin
      sysTable.AddSymbol(TConstSymbol.CreateValue('Null', sysTable.TypVariant, Null));
      sysTable.AddSymbol(TConstSymbol.CreateValue('Unassigned', sysTable.TypVariant, Unassigned));
      if sysTable.TypVariant.SupportsEmptyParam then
         sysTable.AddSymbol(TConstSymbol.CreateValue('EmptyParam', sysTable.TypVariant, EmptyParam));
      sysTable.AddSymbol(TOpenArraySymbol.Create('array of const', sysTable.TypVariant, sysTable.TypInteger));
   end;

   sysTable.TypNil:=TNilSymbol.Create;

   sysTable.TypInterface:=TInterfaceSymbol.Create(SYS_IINTERFACE, nil);
   sysTable.AddSymbol(sysTable.TypInterface);

   sysTable.TypAnyType:=TAnyTypeSymbol.Create(SYS_ANY_TYPE, nil);
   sysTable.AddSymbol(sysTable.TypAnyType);

   // Create "root" class Object
   sysTable.TypObject:=TClassSymbol.Create(SYS_OBJECT, nil);
   sysTable.AddSymbol(sysTable.TypObject);

   // Create "almost root" class TObject
   sysTable.TypTObject:=TClassSymbol.Create(SYS_TOBJECT, nil);
   sysTable.TypTObject.InheritFrom(sysTable.TypObject);
   sysTable.AddSymbol(sysTable.TypTObject);
   // Add constructor Create
   meth:=TMethodSymbol.Create(SYS_TOBJECT_CREATE, fkConstructor, sysTable.TypTObject, cvPublic, False);
   meth.Executable:=ICallable(TEmptyFunc.Create);
   meth.IsDefault:=True;
   sysTable.TypTObject.AddMethod(meth);
   // Add destructor Destroy
   TObjectDestroyMethod.Create(mkDestructor, [maVirtual], SYS_TOBJECT_DESTROY,
                               [], '', sysTable.TypTObject, cvPublic, sysTable);
   // Add procedure Free
   TObjectFreeMethod.Create(mkProcedure, [], SYS_TOBJECT_FREE,
                            [], '', sysTable.TypTObject, cvPublic, sysTable);
   // Add ClassName method
   TObjectClassNameMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSNAME,
                                 [], SYS_STRING, sysTable.TypTObject, cvPublic, sysTable);

   // Create "root" metaclass TClass
   sysTable.TypClass:=TClassOfSymbol.Create(SYS_TCLASS, sysTable.TypTObject);
   sysTable.AddSymbol(sysTable.TypClass);

   // Add ClassType method
   TObjectClassTypeMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSTYPE,
                                 [], SYS_TCLASS, sysTable.TypTObject, cvPublic, sysTable);
   // Add ClassParent method
   TObjectClassParentMethod.Create(mkClassFunction, [], SYS_TOBJECT_CLASSPARENT,
                                   [], SYS_TCLASS, sysTable.TypTObject, cvPublic, sysTable);

   // Create class Exception
   sysTable.TypException := TClassSymbol.Create(SYS_EXCEPTION, nil);
   sysTable.TypException.InheritFrom(sysTable.TypTObject);
   fldSym:=TFieldSymbol.Create(SYS_EXCEPTION_MESSAGE_FIELD, sysTable.TypString, cvProtected);
   sysTable.TypException.AddField(fldSym);
   propSym:=TPropertySymbol.Create(SYS_EXCEPTION_MESSAGE, sysTable.TypString, cvPublic, nil);
   propSym.ReadSym:=fldSym;
   propSym.WriteSym:=fldSym;
   sysTable.TypException.AddProperty(propSym);
   fldSym:=TFieldSymbol.Create(SYS_EXCEPTION_DEBUGGER_FIELD, sysTable.TypInteger, cvProtected);
   sysTable.TypException.AddField(fldSym);
   TExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
                                 ['Msg', SYS_STRING], '', sysTable.TypException, cvPublic, sysTable);
   TExceptionDestroyMethod.Create(mkDestructor, [maVirtual, maOverride], SYS_TOBJECT_DESTROY,
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
   propSym:=TPropertySymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS, sysTable.TypString, cvPublic, nil);
   propSym.ReadSym:=fldSym;
   propSym.WriteSym:=fldSym;
   clsDelphiException.AddProperty(propSym);
   TDelphiExceptionCreateMethod.Create(mkConstructor, [], SYS_TOBJECT_CREATE,
                                       ['Cls', SYS_STRING, 'Msg', SYS_STRING], '',
                                       clsDelphiException, cvPublic, sysTable);
   sysTable.AddSymbol(clsDelphiException);

   // Create TCustomAttribute
   sysTable.TypCustomAttribute := TClassSymbol.Create(SYS_TCUSTOMATTRIBUTE, nil);
   sysTable.TypCustomAttribute.InheritFrom(sysTable.TypTObject);
   sysTable.TypCustomAttribute.IsAttribute := True;
   sysTable.AddSymbol(sysTable.TypCustomAttribute);

   // ExceptObj function
   TExceptObjFunc.Create(sysTable, 'ExceptObject', [], SYS_EXCEPTION, []);

   // Runtime parameters
   if sysTable.TypVariant<>nil then
      TParamFunc.Create(sysTable, 'Param', ['Index', SYS_INTEGER], SYS_VARIANT, []);
   TParamStrFunc.Create(sysTable, 'ParamStr', ['Index', SYS_INTEGER], SYS_STRING, []);
   TParamCountFunc.Create(sysTable, 'ParamCount', [], SYS_INTEGER, []);

   // CompilerVersion
   sysTable.AddSymbol(TConstSymbol.CreateValue(SYS_COMPILER_VERSION, sysTable.TypFloat, cCompilerVersion));

   if Assigned(FOnCreateSystemSymbols) then
      FOnCreateSystemSymbols(sysTable);

   sysTable.FOperators:=TSystemOperators.Create(sysTable);
end;

// SetFilter
//
procedure TdwsConfiguration.SetFilter(const value : TdwsFilter);
begin
   if FFilter=value then Exit;

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
         // the same file system can be referred in two roles
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
   FSystemSymbols:=nil;
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

// GetSystemSymbols
//
function TdwsConfiguration.GetSystemSymbols : ISystemSymbols;
begin
   if FSystemSymbols=nil then
      InitSystemTable;
   Result:=FSystemSymbols;
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
// ------------------ TExceptObjFunc ------------------
// ------------------

procedure TExceptObjFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   if args.Exec.ExceptionObjectStack.Count>0 then
      VarCopySafe(result, args.Exec.ExceptionObjectStack.Peek)
   else VarCopySafe(result, IUnknown(nil));
end;

// ------------------
// ------------------ TParamFunc ------------------
// ------------------

procedure TParamFunc.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
var
   idx : Integer;
   progExec : TdwsProgramExecution;
begin
   progExec := (args.Exec as TdwsProgramExecution);
   idx := args.AsInteger[0];
   if Cardinal(idx) < Cardinal(Length(progExec.Parameters)) then
      VarCopySafe(result, progExec.Parameters[idx])
   else VarClearSafe(result);
end;

// ------------------
// ------------------ TParamStrFunc ------------------
// ------------------

procedure TParamStrFunc.DoEvalAsString(const args : TExprBaseListExec; var Result : String);
var
   idx : Integer;
   progExec : TdwsProgramExecution;
begin
   progExec := (args.Exec as TdwsProgramExecution);
   idx := args.AsInteger[0];
   if Cardinal(idx) < Cardinal(Length(progExec.Parameters)) then
      VariantToString(progExec.Parameters[idx], Result)
   else Result := '';
end;

// ------------------
// ------------------ TParamCountFunc ------------------
// ------------------

function TParamCountFunc.DoEvalAsInteger(const args : TExprBaseListExec) : Int64;
begin
   Result := Length((args.Exec as TdwsProgramExecution).Parameters);
end;

// ------------------
// ------------------ TdwsOptimizationMessageList ------------------
// ------------------

// AddMsg
//
procedure TdwsOptimizationMessageList.AddMessage(aMessage : TdwsMessage);
begin
   inherited;
   FCompileMsgs.AddMessage(aMessage);
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

// CallStackLastExpr
//
function TdwsCompilerExecution.CallStackLastExpr : TExprBase;
begin
   Result:=nil;
end;

// CallStackLastProg
//
function TdwsCompilerExecution.CallStackLastProg : TObject;
begin
   Result:=FCompiler.CurrentProg;
end;

// CallStackDepth
//
function TdwsCompilerExecution.CallStackDepth : Integer;
begin
   Result:=0;
end;

// DebuggerNotifyException
//
procedure TdwsCompilerExecution.DebuggerNotifyException(const exceptObj : IScriptObj);
begin
   // nothing
end;

// GetMsgs
//
function TdwsCompilerExecution.GetMsgs : TdwsRuntimeMessageList;
begin
   Result:=FOptimMsgs;
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
   context.SourceUnit:=compiler.CurrentSourceUnit;
   context.Tokenizer:=compiler.FTok;
   context.UnitSymbol:=compiler.CurrentUnitSymbol;
   context.Context:=compiler.FSourceContextMap.SuspendContext;
   Push(context);
end;

// PopContext
//
procedure TdwsCompilerUnitContextStack.PopContext(compiler : TdwsCompiler; var oldSourceUnit : TSourceUnit);
begin
   compiler.FTok:=Peek.Tokenizer;
   compiler.EnterUnit(Peek.SourceUnit, oldSourceUnit);
   compiler.FSourceContextMap.ResumeContext(Peek.Context);
   Pop;
end;

{ TTypeLookupData }

constructor TTypeLookupData.Create(event: TTypeConvertEvent; info: PTypeInfo);
begin
   self.event := event;
   self.info := info;
end;

end.
// D2009: if you after a build get:
// [DCC Fatal Error] dwsCompiler.pas: F2051 Unit dwsCompiler was compiled with a different version of dwsUtils.TSimpleObjectObjectHash`2.GetItemHashCode
// Just do a re-compile, and it should go away... - HV
