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
unit dwsComp;

{$I dws.inc}

interface

uses
  Variants, Classes, SysUtils, TypInfo,
  dwsCompiler, dwsExprs, dwsSymbols, dwsDataContext, dwsExprList, dwsScriptSource,
  dwsStack, dwsFunctions, dwsStrings, dwsLanguageExtension, dwsCompilerContext,
  dwsTokenizer, dwsUtils, dwsOperators, dwsUnitSymbols, dwsXPlatform, dwsUnicode,
  // Built-In functions
{$IFNDEF DWS_NO_BUILTIN_FUNCTIONS}
  dwsMathFunctions, dwsStringFunctions, dwsTimeFunctions, dwsVariantFunctions,
{$ENDIF}
  dwsMagicExprs, dwsErrors;

type
   TDelphiWebScript = class;

   // TdwsCustomLangageExtension
   //
   TdwsCustomLangageExtension = class (TComponent)
      private
         FExtension : TdwsLanguageExtension;
         FScript : TDelphiWebScript;

      protected
         function CreateExtension : TdwsLanguageExtension; virtual; abstract;
         procedure SetScript(const val : TDelphiWebScript);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         property Extension : TdwsLanguageExtension read FExtension write FExtension;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         property Script : TDelphiWebScript read FScript write SetScript;
   end;

   TdwsEmptyUnit = class(TComponent, IUnknown, IdwsUnit)
      private
         procedure BeforeAdditionTo(dwscript : TObject);
         function GetUnitName: UnicodeString;
         function GetDependencies : TUnicodeStringList;
         function GetUnitTable(systemTable : TSystemSymbolTable;
                               unitSyms : TUnitMainSymbols;
                               operators : TOperators;
                               rootTable : TSymbolTable) : TUnitSymbolTable;
         function GetUnitFlags : TIdwsUnitFlags;
         function GetDeprecatedMessage : UnicodeString;

      protected
         FUnitName: UnicodeString;
         FDependencies: TUnicodeStringList;
         procedure AddUnitSymbols(SymbolTable: TSymbolTable); virtual; abstract;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
   end;

   TdwsUnitComponent = class(TdwsEmptyUnit)
      private
         FScript: TDelphiWebScript;

      protected
         procedure SetScript(const Value: TDelphiWebScript);
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      public
         destructor Destroy; override;

      published
         property Script: TDelphiWebScript read FScript write SetScript;
   end;

   TNeedLocalizerEvent = function (Sender : TObject) : IdwsLocalizer;

   // TDelphiWebScript
   //
   TDelphiWebScript = class (TdwsEmptyUnit)
      private
         FCompiler : IdwsCompiler;
         FConfig : TdwsConfiguration;
         FExtensions : TdwsLanguageExtensionAggregator;
         FLock : TdwsCriticalSection;

      protected
         function GetOnInclude: TIncludeEvent;
         procedure SetOnInclude(const Value: TIncludeEvent);
         function GetOnExecutionStarted : TdwsExecutionEvent;
         procedure SetOnExecutionStarted(const val : TdwsExecutionEvent);
         function GetOnExecutionEnded : TdwsExecutionEvent;
         procedure SetOnExecutionEnded(const val : TdwsExecutionEvent);
         function GetVersion: UnicodeString;
         procedure SetVersion(const Value: UnicodeString);
         function GetOnNeedUnit : TdwsOnNeedUnitEvent;
         procedure SetOnNeedUnit(const val : TdwsOnNeedUnitEvent);
         function GetOnResource : TdwsResourceEvent;
         procedure SetOnResource(const val : TdwsResourceEvent);

         procedure SetConfig(const Value: TdwsConfiguration);
         procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure SetupExtensions;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure AddUnit(const aUnit : IdwsUnit); overload;
         function RemoveUnit(const aUnit : IdwsUnit): Boolean;

         function Compile(const text : UnicodeString; const mainFileName : UnicodeString = '') : IdwsProgram; virtual;
         procedure RecompileInContext(const prog : IdwsProgram; const text : UnicodeString); virtual;

         procedure AbortCompilation;

         procedure Lock;
         procedure UnLock;

         property Compiler : IdwsCompiler read FCompiler;
         property Extensions : TdwsLanguageExtensionAggregator read FExtensions;

      published
         property Config : TdwsConfiguration read FConfig write SetConfig stored True;
         property OnNeedUnit : TdwsOnNeedUnitEvent read GetOnNeedUnit write SetOnNeedUnit stored False;
         property OnInclude : TIncludeEvent read GetOnInclude write SetOnInclude stored False;
         property OnResource : TdwsResourceEvent read GetOnResource write SetOnResource;
         property OnExecutionStarted : TdwsExecutionEvent read GetOnExecutionStarted write SetOnExecutionStarted;
         property OnExecutionEnded : TdwsExecutionEvent read GetOnExecutionEnded write SetOnExecutionEnded;
         property Version : UnicodeString read GetVersion write SetVersion stored False;
   end;

   TLocalizeSymbolEvent = procedure (Sender : TObject; aResSymbol : TResourceStringSymbol; var result : UnicodeString) of object;
   TLocalizeStringEvent = procedure (Sender : TObject; const aString : UnicodeString; var result : UnicodeString) of object;
   TGetLocalizerEvent = procedure (Sender : TObject; var localizer : IdwsLocalizer) of object;

   TEventBasedLocalizer = class (TInterfacedSelfObject, IdwsLocalizer)
      private
         FSender : TObject;
         FOnLocalizeSymbol : TLocalizeSymbolEvent;
         FOnLocalizeString : TLocalizeStringEvent;

      public
         procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString);
         procedure LocalizeString(const aString : UnicodeString; var Result : UnicodeString);

         property Sender : TObject read FSender write FSender;
         property OnLocalizeSymbol : TLocalizeSymbolEvent read FOnLocalizeSymbol write FOnLocalizeSymbol;
         property OnLocalizeString : TLocalizeStringEvent read FOnLocalizeString write FOnLocalizeString;
   end;

   // TdwsCustomLocalizer
   //
   TdwsCustomLocalizer = class (TdwsLocalizerComponent)
      private
         FOnLocalizeSymbol : TLocalizeSymbolEvent;
         FOnLocalizeString : TLocalizeStringEvent;
         FOnGetLocalizer : TGetLocalizerEvent;

      public
         function GetLocalizer : IdwsLocalizer; override;

      published
         property OnLocalizeSymbol : TLocalizeSymbolEvent read FOnLocalizeSymbol write FOnLocalizeSymbol;
         property OnLocalizeString : TLocalizeStringEvent read FOnLocalizeString write FOnLocalizeString;
         property OnGetLocalizer : TGetLocalizerEvent read FOnGetLocalizer write FOnGetLocalizer;
   end;

   // TdwsAbstractUnit
   //
   TdwsAbstractUnit = class(TComponent, IUnknown, IdwsUnit)
      private
         FDependencies : TUnicodeStringList;
         FScript : TDelphiWebScript;
         FUnitName : UnicodeString;
         FDeprecatedMessage : UnicodeString;
         FImplicitUse : Boolean;

         function GetDependencies : TUnicodeStringList;
         procedure SetScript(const Value: TDelphiWebScript);
         procedure SetUnitName(const Value: UnicodeString);

      protected
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         procedure BeforeAdditionTo(dwscript : TObject);
         function GetUnitName: UnicodeString;
         function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators; rootTable : TSymbolTable) : TUnitSymbolTable; virtual; abstract;
         function GetUnitFlags : TIdwsUnitFlags;
         function GetDeprecatedMessage : UnicodeString;

         {$WARNINGS OFF}
         property UnitName: UnicodeString read FUnitName write SetUnitName;
         {$WARNINGS ON}

         property DeprecatedMessage : UnicodeString read FDeprecatedMessage write FDeprecatedMessage;
         property ImplicitUse : Boolean read FImplicitUse write FImplicitUse;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         property Script: TDelphiWebScript read FScript write SetScript;

   end;

   TSymbolTableType = (sttDefault, sttStatic, sttLinked);

   // TdwsAbstractStaticUnit
   //
   TdwsAbstractStaticUnit = class(TdwsAbstractUnit)
      private
         FStaticSymbols : Boolean;
         FStaticTable : IStaticSymbolTable;

      protected
         function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators; rootTable : TSymbolTable) : TUnitSymbolTable; override;
         function CreateUnitTable(parent, rootTable: TSymbolTable; tableType: TSymbolTableType): TUnitSymbolTable; virtual;
         procedure SetStaticSymbols(const Value: Boolean); // static symbols
         procedure InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 operators : TOperators; UnitTable: TUnitSymbolTable); virtual;
         procedure AddUnitSymbols(systemTable : TSystemSymbolTable; table : TSymbolTable; operators : TOperators); virtual; abstract;
         property StaticSymbols : Boolean read FStaticSymbols write SetStaticSymbols;
         property StaticTable : IStaticSymbolTable read FStaticTable;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure BeforeDestruction; override;

         function InitStaticSymbols(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols; operators : TOperators): Boolean;
         procedure ReleaseStaticSymbols;
   end;

   TDataType = UnicodeString;
   TdwsUnit = class;
   TdwsGlobal = class;

   TdwsSymbol = class(TCollectionItem)
      private
         FName: UnicodeString;
         FIsGenerating: Boolean;
         FUnit: TdwsUnit;

      protected
         procedure AssignTo(Dest: TPersistent); override;

         procedure CheckName(aTable : TSymbolTable; const aName : UnicodeString; overloaded : Boolean = False);
         function  GetDataType(systemTable : TSystemSymbolTable; aTable : TSymbolTable; const aName : UnicodeString) : TTypeSymbol;
         procedure Reset;

         procedure SetName(const val : UnicodeString);
         function Parse(const Value : UnicodeString): UnicodeString; virtual;

         property IsGenerating: Boolean read FIsGenerating write FIsGenerating;

      public
         constructor Create(Collection: TCollection); override;

         procedure Assign(Source: TPersistent); override;

         function Generate(systemTable : TSystemSymbolTable; table: TSymbolTable; parentSym: TSymbol = nil): TSymbol;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; virtual; abstract;
         function GetNamePath: String; override;
         function GetUnit: TdwsUnit;

      published
         property Name: UnicodeString read FName write SetName;
   end;

   TdwsSymbolArray = array of TdwsSymbol;

   TdwsSymbolClass = class of TdwsSymbol;

   TdwsCollection = class(TOwnedCollection)
      private
         FUnit: TdwsUnit;

      protected
         class function GetSymbolClass : TdwsSymbolClass; virtual; abstract;
         function GetSymbols(const Name: UnicodeString): TdwsSymbol;
         function GetItem(Index: Integer): TdwsSymbol;
         procedure SetItem(Index: Integer; Value: TdwsSymbol);
         procedure Reset;

      public
         constructor Create(AOwner: TPersistent);
         function GetOwner: TPersistent; override;
         function GetUnit: TdwsUnit;
         function IndexOf(const Name: UnicodeString): Integer;
         property Symbols[const Name: UnicodeString]: TdwsSymbol read GetSymbols;
         property Items[Index: Integer]: TdwsSymbol read GetItem write SetItem;
   end;

   TdwsVariable = class(TdwsSymbol)
      private
         FDataType: TDataType;

      protected
         function GetDisplayName: String; override;

      public
         procedure Assign(Source: TPersistent); override;

      published
         property DataType: TDataType read FDataType write FDataType;
   end;

   TdwsVariables = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
         function GetDisplayName: String;

      public
         function Add : TdwsGlobal; overload;
         function Add(const name, typName : UnicodeString) : TdwsGlobal; overload;
   end;

  TdwsVariablesClass = class of TdwsVariables;

   // TdwsParameter
   //
   TdwsParameter = class(TdwsVariable)
      private
         FIsVarParam : Boolean;
         FIsLazy : Boolean;
         FIsWritable : Boolean;
         FDefaultValue : Variant;
         FHasDefaultValue : Boolean;

      protected
         procedure SetIsVarParam(const Value: Boolean);
         procedure SetHasDefaultValue(const Value: Boolean);
         procedure SetIsWritable(const Value: Boolean);
         procedure SetIsLazy(const val : Boolean);
         procedure SetDefaultValue(const Value: Variant);
         function GetDisplayName: String; override;
         function Parse(const Value : UnicodeString): UnicodeString; override;

      public
         constructor Create(Collection: TCollection); override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
         // not supported here yet, experimental
         property IsLazy : Boolean read FIsLazy write SetIsLazy default False;

      published
         property IsVarParam : Boolean read FIsVarParam write SetIsVarParam default False;
         property IsWritable : Boolean read FIsWritable write SetIsWritable default True;
         property HasDefaultValue : Boolean read FHasDefaultValue write SetHasDefaultValue default False;
         property DefaultValue: Variant read FDefaultValue write SetDefaultValue;
  end;

   TdwsParameters = class(TdwsVariables)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsParameter; overload; inline;
         function Add(const name, typeName : UnicodeString) : TdwsParameter; overload;
   end;

   TFuncEvalEvent = procedure(info : TProgramInfo) of object;
   TFuncFastEvalEvent = function(const args : TExprBaseListExec) : Variant of object;
   TInitSymbolEvent = procedure(sender : TObject; symbol : TSymbol) of object;
   TInitExprEvent = procedure(sender : TObject; expr : TExprBase) of object;

   TdwsCallable = class(TInterfacedSelfObject, IExecutable, ICallable)
      private
         FOwner : TObject;
         FOnInitSymbol : TInitSymbolEvent;
         FOnInitExpr : TInitExprEvent;

      protected
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); virtual; abstract;
         procedure CompileTimeCheck(context : TdwsCompilerContext; expr : TFuncExprBase); virtual;
         procedure InitSymbol(symbol : TSymbol; const msgs : TdwsCompileMessageList);
         procedure InitExpression(expr : TExprBase);
         function SubExpr(i : Integer) : TExprBase;
         function SubExprCount : Integer;

         function Specialize(const context : ISpecializationContext) : IExecutable;

      public
         constructor Create(owner : TObject);

         property OnInitSymbol : TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
         property OnInitExpr : TInitExprEvent read FOnInitExpr write FOnInitExpr;
   end;

   TdwsFunctionCallable = class(TdwsCallable)
      private
         FOnEval : TFuncEvalEvent;

      protected
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;

      public
         property OnEval : TFuncEvalEvent read FOnEval write FOnEval;
   end;

   TdwsFunctionSymbol = class(TdwsSymbol)
      private
         FResultType : TDataType;
         FParameters : TdwsParameters;
         FDeprecated : UnicodeString;
         FCallable : TdwsCallable;
         FOverloaded : Boolean;

      protected
         function GetDisplayName: String; override;
         procedure SetResultType(const val : TDataType); virtual;
         procedure SetParameters(const Value: TdwsParameters);
         function GetOnInitExpr : TInitExprEvent;
         procedure SetOnInitExpr(const val : TInitExprEvent);
         function GetOnInitSymbol : TInitSymbolEvent;
         procedure SetOnInitSymbol(const val : TInitSymbolEvent);
         function StoreParameters : Boolean;
         function Parse(const Value : UnicodeString): UnicodeString; override;
         procedure SetMethodType(const value: TTokenType); virtual;

      public
         constructor Create(collection : TCollection); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;
         function GetParameters(systemTable : TSystemSymbolTable; Table: TSymbolTable): TParamArray;

      published
         property Parameters : TdwsParameters read FParameters write SetParameters stored StoreParameters;
         property ResultType : TDataType read FResultType write SetResultType;
         property OnInitSymbol : TInitSymbolEvent read GetOnInitSymbol write SetOnInitSymbol;
         property OnInitExpr : TInitExprEvent read GetOnInitExpr write SetOnInitExpr;
         property Deprecated : UnicodeString read FDeprecated write FDeprecated;
         property Overloaded : Boolean read FOverloaded write FOverloaded default False;
   end;

   TdwsFunction = class(TdwsFunctionSymbol)
      private
         FOnFastEval : TFuncFastEvalEvent;

      protected
         function GetOnEval : TFuncEvalEvent;
         procedure SetOnEval(const val : TFuncEvalEvent);

      public
         constructor Create(collection : TCollection); override;

         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

      published
         property OnEval : TFuncEvalEvent read GetOnEval write SetOnEval;
         property OnFastEval : TFuncFastEvalEvent read FOnFastEval write FOnFastEval;
   end;

   TdwsFunctions = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsFunction; overload; inline;
         function Add(const name : UnicodeString; const resultType : UnicodeString = '') : TdwsFunction; overload;
   end;

   TdwsFunctionsClass = class of TdwsFunctions;

  // It would have better sense to derive both TdwsFunctionSymbol and TdwsDelegate
  // from a common ancestor.
  TdwsDelegate = class(TdwsSymbol)
  private
    FResultType : TDataType;
    FParameters : TdwsParameters;
    FDeprecated : UnicodeString;

  protected
    function GetDisplayName: String; override;
    procedure SetResultType(const Value: TDataType); virtual;
    procedure SetParameters(const Value: TdwsParameters);
    function StoreParameters : Boolean;
    function Parse(const Value : UnicodeString): UnicodeString; override;

  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
    function GetParameters(systemTable : TSystemSymbolTable; Table: TSymbolTable): TParamArray;

  published
    property Parameters : TdwsParameters read FParameters write SetParameters stored StoreParameters;
    property ResultType : TDataType read FResultType write SetResultType;
    property Deprecated : UnicodeString read FDeprecated write FDeprecated;
  end;

  TdwsDelegates = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
  public
    function Add: TdwsDelegate; overload; inline;
    function Add(const Name: UnicodeString; const ResultType: UnicodeString = '') : TdwsDelegate; overload;
  end;

  TdwsDelegatesClass = class of TdwsDelegates;

   TdwsArray = class(TdwsSymbol)
      private
         FDataType: TDataType;
         FLowBound: Integer;
         FHighBound: Integer;

      protected
         procedure SetIsDynamic(const Value: Boolean);
         function GetIsDynamic: Boolean;
         function GetDisplayName: String; override;
         function GetBoundStored: Boolean;

      public
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
           override;
         procedure Assign(Source: TPersistent); override;

      published
         property DataType: TDataType read FDataType write FDataType;
         property LowBound: Integer read FLowBound write FLowBound stored GetBoundStored;
         property HighBound: Integer read FHighBound write FHighBound stored GetBoundStored;
         property IsDynamic: Boolean read GetIsDynamic write SetIsDynamic default False;
   end;

   TdwsArrays = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsArray;
   end;

   TdwsArraysClass = class of TdwsArrays;

   TdwsConstant = class(TdwsVariable)
      protected
         FValue: Variant;
         function GetDisplayName: String; override;

      public
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Value: Variant read FValue write FValue;
   end;

   TdwsConstants = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
         function GetValues(const name : UnicodeString) : Variant;
         procedure SetValues(const name : UnicodeString; const v : Variant);

      public
         function Add : TdwsConstant; inline;

         property Values[const name : UnicodeString] : Variant read GetValues write SetValues;
   end;

   TdwsConstantsClass = class of TdwsConstants;

   TdwsForward = class(TdwsSymbol)
      protected
         function GetDisplayName: String; override;

      public
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;
   end;

   TdwsForwards = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsForward; inline;
   end;

   TdwsForwardsClass = class of TdwsForwards;

   TdwsField = class(TdwsVariable)
      private
         FVisibility : TdwsVisibility;
         FDefaultValue : Variant;
         FHasDefaultValue : Boolean;

      protected
         function GetDisplayName: String; override;
         procedure SetDefaultValue(const Value: Variant);
         function GetHasDefaultValue : Boolean;
         function Parse(const Value : UnicodeString): UnicodeString; override;

      public
         constructor Create(Collection: TCollection); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property DefaultValue : Variant read FDefaultValue write SetDefaultValue stored GetHasDefaultValue;
         property HasDefaultValue : Boolean read FHasDefaultValue write FHasDefaultValue default False;
   end;

   TdwsFields = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsField;
   end;

   TdwsProperty = class(TdwsSymbol)
      private
         FDataType: TDataType;
         FReadAccess: UnicodeString;
         FWriteAccess: UnicodeString;
         FParameters: TdwsParameters;
         FDeprecated : UnicodeString;
         FIndexValue: Variant;
         FIsDefault: Boolean;
         FIndexType: TDataType;
         FVisibility : TdwsVisibility;

         procedure SetReadAccess(const Value: UnicodeString);
         procedure SetWriteAccess(const Value: UnicodeString);

      protected
         function GetDisplayName: String; override;
         function GetIsDefault: Boolean;
         procedure SetIsDefault(Value: Boolean);
         procedure SetParameters(const Value: TdwsParameters);
         function StoreParameters : Boolean;
         function Parse(const Value : UnicodeString): UnicodeString; override;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property DataType: TDataType read FDataType write FDataType;
         property Deprecated : UnicodeString read FDeprecated write FDeprecated;
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property ReadAccess: UnicodeString read FReadAccess write SetReadAccess;
         property WriteAccess: UnicodeString read FWriteAccess write SetWriteAccess;
         property Parameters: TdwsParameters read FParameters write SetParameters stored StoreParameters;
         property IsDefault: Boolean read GetIsDefault write SetIsDefault default False;
         property IndexType: TDataType read FIndexType write FIndexType;
         property IndexValue: Variant read FIndexValue write FIndexValue;
   end;

   TdwsProperties = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsProperty;
   end;

   TdwsClassOperator = class(TdwsSymbol)
      private
         FOperator: TTokenType;
         FDataType: TDataType;
         FUsesAccess: UnicodeString;

      protected
         function GetDisplayName: String; override;

      public
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property DataType: TDataType read FDataType write FDataType;
         property &Operator : TTokenType read FOperator write FOperator;
         property UsesAccess : UnicodeString read FUsesAccess write FUsesAccess;
   end;

   TdwsClassOperators = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
   end;

   TdwsTypeSymbol = class(TCollectionItem)
      private
         FName: UnicodeString;

      protected
         function GetDisplayName: String; override;

      public
         procedure Assign(Source: TPersistent); override;

      published
         property Name : UnicodeString read FName write FName;
   end;

   TdwsTypeSymbols = class(TOwnedCollection)
      public
         constructor Create(AOwner: TPersistent);
         function Add : TdwsTypeSymbol;
   end;

   TdwsOperator = class(TdwsSymbol)
      private
         FOperator: TTokenType;
         FResultType: TDataType;
         FParams : TdwsTypeSymbols;
         FUsesAccess: UnicodeString;

      protected
         function GetDisplayName: String; override;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property ResultType : TDataType read FResultType write FResultType;
         property Params : TdwsTypeSymbols read FParams write FParams;
         property &Operator : TTokenType read FOperator write FOperator;
         property UsesAccess : UnicodeString read FUsesAccess write FUsesAccess;
   end;

   TdwsOperators = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsOperator;
   end;

   TdwsOperatorsClass = class of TdwsOperators;

   TAssignExternalObjectEvent = procedure(Info: TProgramInfo; var ExtObject: TObject) of object;
   TMethodEvalEvent = procedure(Info: TProgramInfo; ExtObject: TObject) of object;

   TdwsMethodCallable = class(TdwsCallable)
      private
         FOnEval : TMethodEvalEvent;

      protected
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;

      public
         property OnEval : TMethodEvalEvent read FOnEval write FOnEval;
   end;

   TdwsMethod = class(TdwsFunctionSymbol)
      private
         FAttributes : TMethodAttributes;
         FKind : TMethodKind;
         FVisibility : TdwsVisibility;

      protected
         function GetDisplayName: String; override;
         function GetOnEval : TMethodEvalEvent;
         procedure SetOnEval(const val : TMethodEvalEvent);
         procedure SetResultType(const val : TDataType); override;
         procedure SetAttributes(const attribs : TMethodAttributes);
         procedure SetMethodType(const value: TTokenType); override;

      public
         constructor Create(collection : TCollection); override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Attributes: TMethodAttributes read FAttributes write SetAttributes default [];
         property OnEval : TMethodEvalEvent read GetOnEval write SetOnEval;
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property Kind: TMethodKind read FKind write FKind;
   end;

   TdwsMethods = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsMethod; overload; inline;
         function Add(const name : UnicodeString; const resultType : UnicodeString = '') : TdwsMethod; overload;
   end;

   TdwsConstructorCallable = class(TdwsCallable)
      private
         FOnEval : TAssignExternalObjectEvent;

      protected
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); override;

      public
         property OnEval : TAssignExternalObjectEvent read FOnEval write FOnEval;
   end;

   TdwsConstructor = class(TdwsFunctionSymbol)
      private
         FAttributes: TMethodAttributes;
         FVisibility : TdwsVisibility;

      protected
         function GetResultType: UnicodeString;
         function GetDisplayName: String; override;
         function GetOnEval : TAssignExternalObjectEvent;
         procedure SetOnEval(const val : TAssignExternalObjectEvent);
         procedure SetAttributes(const attribs : TMethodAttributes);

      public
         constructor Create(Collection: TCollection); override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property OnEval : TAssignExternalObjectEvent read GetOnEval write SetOnEval;
         property Attributes: TMethodAttributes read FAttributes write SetAttributes default [];
         property ResultType: UnicodeString read GetResultType;
   end;

   TdwsConstructors = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsConstructor;
   end;

   TdwsClassConstant = class(TdwsConstant)
      private
         FVisibility : TdwsVisibility;

      protected
         function GetDisplayName: String; override;

      public
         constructor Create(Collection: TCollection); override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
   end;

   TdwsClassConstants = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsClassConstant;
  end;

   // TdwsClass
   //
   TdwsClass = class(TdwsSymbol)
      private
         FAncestor : UnicodeString;
         FConstructors : TdwsConstructors;
         FFields : TdwsFields;
         FMethods : TdwsMethods;
         FOnObjectDestroy : TObjectDestroyEvent;
         FProperties : TdwsProperties;
         FOperators : TdwsClassOperators;
         FConstants : TdwsClassConstants;
         FHelperObject : TObject;
         FIsSealed : Boolean;
         FIsAbstract : Boolean;
         FIsStatic : Boolean;
         FDeprecated : UnicodeString;

      protected
         function GetDisplayName : String; override;
         function StoreConstructors : Boolean;
         function StoreFields : Boolean;
         function StoreMethods : Boolean;
         function StoreOperators : Boolean;
         function StoreConstants : Boolean;
         function StoreProperties : Boolean;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

         {: User-side helper object, freed by the TdwsClass. }
         property HelperObject : TObject read FHelperObject write FHelperObject;

      published
         property Ancestor : UnicodeString read FAncestor write FAncestor;
         property IsSealed : Boolean read FIsSealed write FIsSealed default False;
         property IsAbstract : Boolean read FIsAbstract write FIsAbstract default False;
         property IsStatic : Boolean read FIsStatic write FIsStatic default False;
         property Deprecated : UnicodeString read FDeprecated write FDeprecated;
         property Constructors : TdwsConstructors read FConstructors write FConstructors stored StoreConstructors;
         property Fields : TdwsFields read FFields write FFields stored StoreFields;
         property Methods : TdwsMethods read FMethods write FMethods stored StoreMethods;
         property Operators : TdwsClassOperators read FOperators write FOperators stored StoreOperators;
         property Constants : TdwsClassConstants read FConstants write FConstants stored StoreConstants;
         property Properties: TdwsProperties read FProperties write FProperties stored StoreProperties;
         property OnCleanUp : TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
   end;

   TdwsClasses = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsClass;
   end;

   TdwsClassesClass = class of TdwsClasses;

   // TdwsInterface
   //
   TdwsInterface = class(TdwsSymbol)
      private
         FAncestor : UnicodeString;
         FMethods : TdwsMethods;
         FProperties : TdwsProperties;

      protected
         function GetDisplayName : String; override;
         function StoreMethods : Boolean;
         function StoreProperties : Boolean;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Ancestor : UnicodeString read FAncestor write FAncestor;
         property Methods : TdwsMethods read FMethods write FMethods stored StoreMethods;
         property Properties: TdwsProperties read FProperties write FProperties stored StoreProperties;
   end;

   TdwsInterfaces = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsInterface;
   end;

   TdwsInterfacesClass = class of TdwsInterfaces;

   TdwsMember = class(TdwsVariable)
      private
         FVisibility : TdwsVisibility;

      public
         constructor Create(Collection: TCollection); override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
   end;

   TdwsMembers = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsMember; inline;
   end;

   TdwsRecord = class(TdwsSymbol)
      private
         FMembers : TdwsMembers;
         FProperties : TdwsProperties;

      protected
         function GetDisplayName: String; override;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Members : TdwsMembers read FMembers write FMembers;
         property Properties : TdwsProperties read FProperties write FProperties;
  end;

   TdwsRecords = class(TdwsCollection)
      protected
         class function GetSymbolClass: TdwsSymbolClass; override;

      public
         function Add : TdwsRecord; inline;
   end;

   TdwsRecordsClass = class of TdwsRecords;

   TdwsElement = class(TdwsSymbol)
      private
         FIsUserDef: Boolean;
         FUserDefValue: Integer;
         FDeprecated : UnicodeString;

         procedure SetUserDefValue(const Value: Integer);
         procedure SetIsUserDef(const Value: Boolean);

     protected
         function GetDisplayName: String; override;

      public
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property UserDefValue : Integer read FUserDefValue write SetUserDefValue default 0;
         property IsUserDef : Boolean read FIsUserDef write SetIsUserDef default False;
         property Deprecated : UnicodeString read FDeprecated write FDeprecated;
   end;

   TdwsElements = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsElement; inline;
   end;

   TdwsEnumeration = class(TdwsSymbol)
      private
         FElements: TdwsElements;
         FStyle : TEnumerationSymbolStyle;

      protected
         function GetDisplayName: String; override;
         function Parse(const Value : UnicodeString): UnicodeString; override;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Elements: TdwsElements read FElements write FElements;
         property Style : TEnumerationSymbolStyle read FStyle write FStyle default enumClassic;
   end;

   TdwsEnumerations = class(TdwsCollection)
      protected
         class function GetSymbolClass: TdwsSymbolClass; override;

      public
         function Add : TdwsEnumeration;
   end;

   TdwsEnumerationsClass = class of TdwsEnumerations;

   TdwsSet = class(TdwsSymbol)
      private
         FBaseType : TDataType;

      protected
         function GetDisplayName: String; override;
         function Parse(const Value : UnicodeString): UnicodeString; override;

      public
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;

      published
         property BaseType : TDataType read FBaseType write FBaseType;
   end;

   TdwsSets = class(TdwsCollection)
      protected
         class function GetSymbolClass: TdwsSymbolClass; override;

      public
         function Add : TdwsSet;
   end;

   TdwsSetsClass = class of TdwsSets;

   TdwsCustomInstance = class;

   TReadVarEvent = procedure (info: TProgramInfo; var value : Variant) of object;
   TWriteVarEvent = procedure (info: TProgramInfo; const value : Variant) of object;
   TInstantiateEvent = procedure (info: TProgramInfo; var ExtObject: TObject) of object;

   TdwsGlobal = class(TdwsVariable)
      private
         FOnReadVar: TReadVarEvent;
         FOnWriteVar: TWriteVarEvent;
      protected
         function Parse(const Value : UnicodeString): UnicodeString; override;
      public
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;
         procedure Assign(Source: TPersistent); override;
      published
         property OnReadVar: TReadVarEvent read FOnReadVar write FOnReadVar;
         property OnWriteVar: TWriteVarEvent read FOnWriteVar write FOnWriteVar;
   end;

   TdwsInstance = class;

   TdwsInstances = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsInstance;
   end;

   TdwsInstancesClass = class of TdwsInstances;

   TdwsCustomInstance = class(TdwsVariable)
      private
         FOnObjectDestroy: TObjectDestroyEvent;
         FOnInstantiate: TInstantiateEvent;
         FAutoDestroyExternalObject: Boolean;
         FOnInitSymbol: TInitSymbolEvent;
         FOnInitExpr: TInitExprEvent;

      protected
         procedure DoDestroy(ExternalObject: TObject); virtual;
         procedure DoInstantiate(info : TProgramInfo; var ExternalObject: TObject); virtual;
         procedure DoInitSymbol(Sender: TObject; Symbol: TSymbol); virtual;
         procedure DoInitExpr(Sender: TObject; Expr: TExprBase); virtual;

      public
         constructor Create(Collection: TCollection); override;
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;
         procedure Assign(Source: TPersistent); override;

         property AutoDestroyExternalObject: Boolean read FAutoDestroyExternalObject
            write FAutoDestroyExternalObject default False;
         property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy
            write FOnObjectDestroy;
         property OnInstantiate: TInstantiateEvent read FOnInstantiate write
            FOnInstantiate;
         property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol
            write FOnInitSymbol;
         property OnInitExpr: TInitExprEvent read FOnInitExpr write FOnInitExpr;
   end;

   TdwsInstance = class(TdwsCustomInstance)
      published
         property AutoDestroyExternalObject;
         property OnObjectDestroy;
         property OnInstantiate;
         property OnInitSymbol;
         property OnInitExpr;
   end;

   TdwsSynonym = class(TdwsVariable)
      public
         function DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                             ParentSym: TSymbol = nil): TSymbol; override;
   end;

   TdwsSynonyms = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsSynonym;
   end;

   TdwsSynonymsClass = class of TdwsSynonyms;

   TdwsParseName = (pnAtDesignTimeOnly, pnAlways, pnNever);

   // TdwsUnit
   //
   TdwsUnit = class(TdwsAbstractStaticUnit)
      private
        FArrays: TdwsArrays;
        FClasses: TdwsClasses;
        FConstants: TdwsConstants;
        FEnumerations: TdwsEnumerations;
        FSets: TdwsSets;
        FForwards: TdwsForwards;
        FFunctions: TdwsFunctions;
        FDelegates: TdwsDelegates;
        FInstances: TdwsInstances;
        FRecords: TdwsRecords;
        FInterfaces : TdwsInterfaces;
        FSynonyms: TdwsSynonyms;
        FVariables: TdwsVariables;
        FOperators : TdwsOperators;
        FTable: TUnitSymbolTable;
        FOnAfterInitUnitTable : TNotifyEvent;
        FParseName : TdwsParseName;
        FDependencies : TStrings;

      protected
        FCollections : array[0..13] of TdwsCollection;

        class function GetArraysClass : TdwsArraysClass; virtual;
        class function GetClassesClass : TdwsClassesClass; virtual;
        class function GetConstantsClass : TdwsConstantsClass; virtual;
        class function GetEnumerationsClass : TdwsEnumerationsClass; virtual;
        class function GetSetsClass : TdwsSetsClass; virtual;
        class function GetForwardsClass : TdwsForwardsClass; virtual;
        class function GetFunctionsClass : TdwsFunctionsClass; virtual;
        class function GetDelegatesClass : TdwsDelegatesClass; virtual;
        class function GetInstancesClass : TdwsInstancesClass; virtual;
        class function GetRecordsClass : TdwsRecordsClass; virtual;
        class function GetInterfacesClass : TdwsInterfacesClass; virtual;
        class function GetVariablesClass : TdwsVariablesClass; virtual;
        class function GetSynonymsClass : TdwsSynonymsClass; virtual;
        class function GetOperatorsClass : TdwsOperatorsClass; virtual;

        procedure SetArrays(const Value: TdwsArrays);
        procedure SetClasses(const Value: TdwsClasses);
        procedure SetConstants(const Value: TdwsConstants);
        procedure SetEnumerations(const Value: TdwsEnumerations);
        procedure SetSets(const Value: TdwsSets);
        procedure SetForwards(const Value: TdwsForwards);
        procedure SetFunctions(const Value: TdwsFunctions);
        procedure SetDelegates(const Value: TdwsDelegates);
        procedure SetRecords(const Value: TdwsRecords);
        procedure SetInterfaces(const value : TdwsInterfaces);
        procedure SetVariables(const Value: TdwsVariables);
        procedure SetInstances(const Value: TdwsInstances);
        procedure SetSynonyms(const Value: TdwsSynonyms);
        procedure SetOperators(const Value: TdwsOperators);

        function StoreArrays : Boolean;
        function StoreClasses : Boolean;
        function StoreConstants : Boolean;
        function StoreEnumerations : Boolean;
        function StoreSets : Boolean;
        function StoreForwards : Boolean;
        function StoreFunctions : Boolean;
        function StoreDelegates : Boolean;
        function StoreRecords : Boolean;
        function StoreInterfaces : Boolean;
        function StoreVariables : Boolean;
        function StoreInstances : Boolean;
        function StoreSynonyms : Boolean;
        function StoreOperators : Boolean;
        function StoreImplicitUse : Boolean;

        function InternalTypeDefined(const name : UnicodeString; visited: TStringList): Boolean;

      protected
        function GetSymbol(systemTable : TSystemSymbolTable; Table: TSymbolTable; const Name: UnicodeString): TSymbol;
        procedure AddCollectionSymbols(aCollection: TdwsCollection;
                                       systemTable : TSystemSymbolTable; Table: TSymbolTable;
                                       operators : TOperators); virtual;
        procedure AddUnitSymbols(systemTable : TSystemSymbolTable; Table: TSymbolTable; operators : TOperators); override;
        procedure InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                operators : TOperators; UnitTable: TUnitSymbolTable); override;

        // Method to support get/set property values for dynamicly registered classes
        procedure HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
        procedure HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);

        procedure SetDependencies(const val : TStrings);
        procedure DependenciesChanged(Sender : TObject);

      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        function ShouldParseName(const val : UnicodeString) : Boolean;
        function TypeDefined(const name : UnicodeString) : Boolean;

        procedure GetDataTypes(List: TStrings);
        procedure GetClassTypes(List: TStrings);
        procedure ExposeClassToUnit(AClass, AAncestor: TClass; ASearchProgram: TdwsProgram=nil; const ScriptAncestorType: UnicodeString='');
        procedure ExposeInstanceToUnit(const AName, AClassType: UnicodeString; AInstance: TObject);

        property Table: TUnitSymbolTable read FTable;

      published
        property Arrays: TdwsArrays read FArrays write SetArrays stored StoreArrays;
        property Classes: TdwsClasses read FClasses write SetClasses stored StoreClasses;
        property Constants: TdwsConstants read FConstants write SetConstants stored StoreConstants;
        property Dependencies: TStrings read FDependencies write SetDependencies;
        property Enumerations: TdwsEnumerations read FEnumerations write SetEnumerations stored StoreEnumerations;
        property Sets: TdwsSets read FSets write SetSets stored StoreSets;
        property Forwards: TdwsForwards read FForwards write SetForwards stored StoreForwards;
        property Functions: TdwsFunctions read FFunctions write SetFunctions stored StoreFunctions;
        property Delegates: TdwsDelegates read FDelegates write SetDelegates stored StoreDelegates;
        property Instances: TdwsInstances read FInstances write SetInstances stored StoreInstances;
        property Operators : TdwsOperators read FOperators write SetOperators stored StoreOperators;
        property Records : TdwsRecords read FRecords write SetRecords stored StoreRecords;
        property Interfaces : TdwsInterfaces read FInterfaces write SetInterfaces stored StoreInterfaces;
        property Synonyms: TdwsSynonyms read FSynonyms write SetSynonyms stored StoreSynonyms;
        property ParseName : TdwsParseName read FParseName write FParseName default pnAtDesignTimeOnly;
        property UnitName;
        property DeprecatedMessage;
        property ImplicitUse stored StoreImplicitUse;
        property Variables : TdwsVariables read FVariables write SetVariables stored StoreVariables;
        property StaticSymbols;

        property OnAfterInitUnitTable : TNotifyEvent read FOnAfterInitUnitTable write FOnAfterInitUnitTable;
   end;

   TCustomInstantiateFunc = class (TFunctionPrototype, IUnknown, ICallable)
      protected
         FClassSym : TClassSymbol;
         FDataSym : TDataSymbol;

      public
         function Specialize(const context : ISpecializationContext) : IExecutable;

         property ClassSym : TClassSymbol read FClassSym write FClassSym;
         property DataSym : TDataSymbol read FDataSym write FDataSym;
   end;

   TDynamicInstantiateFunc = class(TCustomInstantiateFunc)
      protected
         FExternalObject : TObject;

      public
         constructor Create(FuncSym: TFuncSymbol; AExternalObject: TObject); virtual;

         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;
   end;

   TInstantiateFunc = class(TCustomInstantiateFunc)
      private
         FOnInstantiate : TInstantiateEvent;
         FOnObjectDestroy : TObjectDestroyEvent;
         FOnInitSymbol : TInitSymbolEvent;
         FOnInitExpr : TInitExprEvent;

      public
         constructor Create(FuncSym: TFuncSymbol);

         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); override;

         procedure InitSymbol(symbol : TSymbol; const msgs : TdwsCompileMessageList); override;
         procedure InitExpression(expr : TExprBase); override;

         property OnInstantiate : TInstantiateEvent read FOnInstantiate write FOnInstantiate;
         property OnObjectDestroy : TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
         property OnInitSymbol : TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
         property OnInitExpr : TInitExprEvent read FOnInitExpr write FOnInitExpr;
   end;

   TReadVarEventFunc = class(TAnonymousFunction)
      private
         FOnReadVar: TReadVarEvent;

      public
         procedure Execute(info : TProgramInfo); override;
         property OnReadVar: TReadVarEvent read FOnReadVar write FOnReadVar;
   end;

   TWriteVarEventFunc = class(TAnonymousFunction)
      private
         FOnWriteVar: TWriteVarEvent;

      public
         procedure Execute(info : TProgramInfo); override;
         property OnWriteVar: TWriteVarEvent read FOnWriteVar write FOnWriteVar;
   end;

   TReadVarFunc = class(TAnonymousFunction)
      private
         FData : TData;
         FTyp : TTypeSymbol;

      public
         constructor Create(FuncSym: TFuncSymbol);
         procedure Execute(info : TProgramInfo); override;
         procedure SetValue(const data : TData; offset : Integer);
   end;

   TWriteVarFunc = class(TAnonymousFunction)
      private
         FReadVarFunc : TReadVarFunc;

      public
         constructor Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
         procedure Execute(info : TProgramInfo); override;
   end;

   TCustomInternalMagicProcedure = class(TInternalMagicProcedure)
      private
         FOnFastEval : TFuncFastEvalEvent;
      public
         procedure DoEvalProc(const args : TExprBaseListExec); override;
   end;

   TCustomInternalMagicFunction = class(TInternalMagicVariantFunction)
      private
         FOnFastEval : TFuncFastEvalEvent;
      public
         procedure DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant); override;
   end;

   TCustomInternalMagicDataFunction = class(TInternalMagicDataFunction)
      private
         FOnFastEval : TFuncFastEvalEvent;
         FSize : Integer;
      public
         procedure DoEval(const args : TExprBaseListExec; var result : IDataContext); override;
   end;

   EdwsInvalidUnitAddition = class (Exception);

// Return the external object for a variable name.
function GetExternalObjForID(Info: TProgramInfo; const AVarName: UnicodeString): TObject;

// Get or create the DWS object ID (like a pointer) for a Delphi object instance.
//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: UnicodeString = ''): Integer;

function GetParameters(Symbol: TdwsSymbol; Parameters: TdwsParameters;
                       systemTable : TSystemSymbolTable; Table: TSymbolTable): TParamArray;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
  dwsPascalTokenizer;

type
   EGenerationError = class(Exception);
   EHandledGenerationError = class(Exception);

// ValueToString
//
function ValueToString(const value : Variant) : UnicodeString;
begin
   case VarType(value) of
      varEmpty :
         Result := 'Unassigned';
      varNull :
         Result := 'Null';
      varString, varUString, varOleStr, varStrArg :
         Result := UnicodeFormat('''%s''', [VarToStr(value)]);
      varDate :
         Result := UnicodeFormat('DateTime(%f)', [TVarData(value).VDate]);
   else
      VariantToString(value, Result);
   end;
end;

// VariantTypeToDataType
//
function VariantTypeToDataType(const value : Variant) : TDataType;
begin
   //TODO: Extend with arrays, records, etc:
   // varEmpty
   // varNull
   // varDate
   // varVariant
   // varRecord
   // varStrArg
   // varObject
   // varUStrArg
   // varAny
   case VarType(value) of
      varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord,
      varInt64, varUInt64 :
         Result := SYS_INTEGER;
      varSingle, varDouble, varCurrency :
         Result := SYS_FLOAT;
      varOleStr, varString, varUString :
         Result := SYS_STRING;
      varBoolean :
         Result := SYS_BOOLEAN;
      varVariant :
         Result := SYS_VARIANT;
   else
      Result := '';
   end;
end;

function GetExternalObjForID(Info: TProgramInfo; const AVarName: UnicodeString): TObject;
begin
  // Get param "Source" as object in Source_Obj
  Result := IScriptObj(IUnknown(Info.ValueAsVariant[AVarName])).ExternalObject;
end;

{ TCustomInternalMagicProcedure }

// DoEvalProc
//
procedure TCustomInternalMagicProcedure.DoEvalProc(const args : TExprBaseListExec);
begin
   FOnFastEval(args);
end;

{ TCustomInternalMagicFunction }

// DoEvalAsVariant
//
procedure TCustomInternalMagicFunction.DoEvalAsVariant(const args : TExprBaseListExec; var result : Variant);
begin
   Result:=FOnFastEval(args);
end;

{ TCustomInternalMagicDataFunction }

// DoEval
//
procedure TCustomInternalMagicDataFunction.DoEval(const args : TExprBaseListExec; var result : IDataContext);
var
   tmp : Variant;
   pvd : PVarData;
begin
   tmp:=FOnFastEval(args);
   pvd:=@tmp;
   Assert(pvd.VType=varUnknown);
   result.WriteData(IDataContext(IUnknown(pvd.VUnknown)), FSize);
end;

{ TDelphiWebScript }

// Create
//
constructor TDelphiWebScript.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FUnitName:=SYS_DEFAULT;
   FCompiler:=TdwsCompiler.Create;
   FConfig:=TdwsConfiguration.Create(Self);
   AddUnit(Self);
   FExtensions:=TdwsLanguageExtensionAggregator.Create;
   FLock:=TdwsCriticalSection.Create;
end;

// Destroy
//
destructor TDelphiWebScript.Destroy;
begin
   inherited;
   FCompiler:=nil;
   FConfig.Free;
   FExtensions.Free;
   FLock.Free;
end;

function TDelphiWebScript.GetVersion: UnicodeString;
begin
  Result := '2.3';
end;

procedure TDelphiWebScript.SetVersion(const Value: UnicodeString);
begin
  // This method is needed to make the IDE show the version in
  // the object inspector
end;

// GetOnNeedUnit
//
function TDelphiWebScript.GetOnNeedUnit : TdwsOnNeedUnitEvent;
begin
   Result:=Config.OnNeedUnit;
end;

// SetOnNeedUnit
//
procedure TDelphiWebScript.SetOnNeedUnit(const val : TdwsOnNeedUnitEvent);
begin
   Config.OnNeedUnit:=val;
end;

// GetOnResource
//
function TDelphiWebScript.GetOnResource : TdwsResourceEvent;
begin
   Result:=Config.OnResource;
end;

// SetOnResource
//
procedure TDelphiWebScript.SetOnResource(const val : TdwsResourceEvent);
begin
   Config.OnResource:=val;
end;

// SetupExtensions
//
procedure TDelphiWebScript.SetupExtensions;
var
   c : TdwsCompiler;
begin
   c:=FCompiler.Compiler;
   if FExtensions.Count>0 then begin
      c.StaticExtensionSymbols:=FExtensions.StaticSymbols;
      c.OnCreateBaseVariantSymbol:=FExtensions.CreateBaseVariantSymbol;
      c.OnCreateSystemSymbols:=FExtensions.CreateSystemSymbols;
      c.OnReadInstr:=FExtensions.ReadInstr;
      c.OnReadInstrSwitch:=FExtensions.ReadInstrSwitch;
      c.OnFindUnknownName:=FExtensions.FindUnknownName;
      c.OnReadUnknownName:=FExtensions.ReadUnknownName;
      c.OnSectionChanged:=FExtensions.SectionChanged;
      c.OnReadScript:=FExtensions.ReadScript;
      c.OnGetDefaultEnvironment:=FExtensions.DefaultEnvironment;
      c.OnRootExternalClass:=FExtensions.RootExternalClass;
      c.OnApplyConditionalDefines:=FExtensions.ApplyConditionalDefines;
   end else begin
      c.StaticExtensionSymbols:=True;
      c.OnCreateBaseVariantSymbol:=nil;
      c.OnCreateSystemSymbols:=nil;
      c.OnReadInstr:=nil;
      c.OnReadInstrSwitch:=nil;
      c.OnFindUnknownName:=nil;
      c.OnReadUnknownName:=nil;
      c.OnSectionChanged:=nil;
      c.OnReadScript:=nil;
      c.OnGetDefaultEnvironment:=nil;
      c.OnRootExternalClass:=nil;
      c.OnApplyConditionalDefines:=nil;
   end;
end;

// Compile
//
function TDelphiWebScript.Compile(const Text: UnicodeString; const mainFileName : UnicodeString = ''): IdwsProgram;
begin
   Lock;
   try
      SetupExtensions;
      Result:=FCompiler.Compile(Text, FConfig, mainFileName);
   finally
      UnLock;
   end;
end;

// RecompileInContext
//
procedure TDelphiWebScript.RecompileInContext(const prog : IdwsProgram; const text : UnicodeString);
begin
   Lock;
   try
      SetupExtensions;
      FCompiler.RecompileInContext(prog, text, FConfig);
   finally
      UnLock;
   end;
end;

// AbortCompilation
//
procedure TDelphiWebScript.AbortCompilation;
begin
   FCompiler.AbortCompilation;
end;

// AddUnit
//
procedure TDelphiWebScript.AddUnit(const aUnit : IdwsUnit);
begin
   if not Assigned(aUnit) then Exit;

   aUnit.BeforeAdditionTo(Self);

   RemoveUnit(aUnit);
   FConfig.Units.Add(aUnit);
end;

// RemoveUnit
//
function TDelphiWebScript.RemoveUnit(const aUnit : IdwsUnit): Boolean;
var
   i : Integer;
begin
   i:=FConfig.Units.IndexOf(aUnit);
   Result:=(i>=0);
   if Result then begin
      FConfig.Units[i]:=nil;
      FConfig.Units.Extract(i);
   end;
end;

// Lock
//
procedure TDelphiWebScript.Lock;
begin
   FLock.Enter;
end;

// UnLock
//
procedure TDelphiWebScript.UnLock;
begin
   FLock.Leave;
end;

procedure TDelphiWebScript.SetConfig(const Value: TdwsConfiguration);
begin
  FConfig.Assign(Value);
end;

// Implementation of TdwsEmptyUnit.AddUnitSymbols
procedure TDelphiWebScript.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // The TDelphiWebScript component is the unit "Default"
  Config.ResultType.AddResultSymbols(SymbolTable);
end;

procedure TDelphiWebScript.SetOnInclude(const Value: TIncludeEvent);
begin
  Config.OnInclude := Value;
end;

// GetOnExecutionStarted
//
function TDelphiWebScript.GetOnExecutionStarted : TdwsExecutionEvent;
begin
   Result:=Config.OnExecutionStarted;
end;

// SetOnExecutionStarted
//
procedure TDelphiWebScript.SetOnExecutionStarted(const val : TdwsExecutionEvent);
begin
   Config.OnExecutionStarted:=val;
end;

// GetOnExecutionEnded
//
function TDelphiWebScript.GetOnExecutionEnded : TdwsExecutionEvent;
begin
   Result:=Config.OnExecutionEnded;
end;

// SetOnExecutionEnded
//
procedure TDelphiWebScript.SetOnExecutionEnded(const val : TdwsExecutionEvent);
begin
   Config.OnExecutionEnded:=val;
end;

function TDelphiWebScript.GetOnInclude: TIncludeEvent;
begin
  Result := Config.OnInclude;
end;

procedure TDelphiWebScript.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
   inherited;
   Config.Notification(AComponent, Operation);
   if Operation = opRemove then begin
      if AComponent is TdwsUnitComponent then
         Self.RemoveUnit(TdwsUnitComponent(AComponent))
      else if AComponent is TdwsAbstractUnit then
         Self.RemoveUnit(TdwsAbstractUnit(AComponent));
   end;
end;

{ TdwsCollection }

constructor TdwsCollection.Create;
begin
  inherited Create(AOwner, GetSymbolClass);
  if AOwner is TdwsUnit then
    FUnit := TdwsUnit(AOwner)
  else if AOwner is TdwsSymbol then
    FUnit := TdwsSymbol(AOwner).GetUnit
  else
    FUnit := nil;
end;

function TdwsCollection.GetOwner: TPersistent;
begin
  Result := inherited GetOwner;
end;

function TdwsCollection.GetUnit: TdwsUnit;
begin
  Result := FUnit;
end;

function TdwsCollection.GetItem(Index: Integer): TdwsSymbol;
begin
  Result := TdwsSymbol(inherited Items[Index]);
end;

procedure TdwsCollection.Reset;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Items[x].Reset;
end;

procedure TdwsCollection.SetItem(Index: Integer; Value: TdwsSymbol);
begin
  Items[Index].Assign(Value);
end;

function TdwsCollection.GetSymbols(const Name: UnicodeString): TdwsSymbol;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
  begin
    Result := Items[x];
    if UnicodeSameText(Result.Name,Name) then
      Exit;
  end;
  Result := nil;
end;

function TdwsCollection.IndexOf(const Name: UnicodeString): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to Self.Count - 1 do
    if UnicodeSameText(Name, Items[x].Name) then
    begin
      Result := x;
      Break;
    end;
end;

{ TdwsUnit }

constructor TdwsUnit.Create(AOwner: TComponent);
begin
   inherited;
   FArrays := GetArraysClass.Create(Self);
   FClasses := GetClassesClass.Create(Self);
   FConstants := GetConstantsClass.Create(Self);
   FEnumerations := GetEnumerationsClass.Create(Self);
   FSets := GetSetsClass.Create(Self);
   FForwards := GetForwardsClass.Create(Self);
   FFunctions := GetFunctionsClass.Create(Self);
   FDelegates := GetDelegatesClass.Create(Self);
   FRecords := GetRecordsClass.Create(Self);
   FInterfaces := GetInterfacesClass.Create(Self);
   FVariables := GetVariablesClass.Create(Self);
   FInstances := GetInstancesClass.Create(Self);
   FSynonyms := GetSynonymsClass.Create(Self);
   FOperators := TdwsOperators.Create(Self);
   FDependencies := TStringList.Create;
   TStringList(FDependencies).OnChange := DependenciesChanged;

   FCollections[0] := FForwards;
   FCollections[1] := FEnumerations;
   FCollections[2] := FArrays;
   FCollections[3] := FRecords;
   FCollections[4] := FInterfaces;
   FCollections[5] := FClasses;
   FCollections[6] := FSynonyms;
   FCollections[7] := FFunctions;
   FCollections[8] := FVariables;
   FCollections[9] := FConstants;
   FCollections[10] := FInstances;
   FCollections[11] := FOperators;
   FCollections[12] := FSets;
   FCollections[13] := FDelegates;

   FParseName := pnAtDesignTimeOnly;
end;

destructor TdwsUnit.Destroy;
var
   i : Integer;
begin
   FDependencies.Free;
   for i:=Low(FCollections) to High(FCollections) do
      FCollections[i].Free;
   inherited;
end;

// ShouldParseName
//
function TdwsUnit.ShouldParseName(const val : UnicodeString) : Boolean;
var
   i : Integer;
begin
   if csLoading in ComponentState then Exit(False);
   case ParseName of
      pnAlways : Result:=True;
      pnAtDesignTimeOnly : Result:=csDesigning in ComponentState;
   else
      Exit(False);
   end;
   if Result then begin
      for i:=1 to Length(val) do begin
         case val[i] of
            ':', '(', '=' : Exit;
         end;
      end;
      Result:=False;
   end;
end;

procedure TdwsUnit.AddCollectionSymbols(aCollection: TdwsCollection;
  systemTable : TSystemSymbolTable; Table: TSymbolTable; operators : TOperators);
var
  y: Integer;
  clsName : UnicodeString;
  collSym : TdwsSymbol;
  sym : TSymbol;
begin
   // add all classes as forwards automatically if they aren't there already
   for y:=0 to FClasses.Count-1 do begin
      clsName:=FClasses.Items[y].Name;
      if FForwards.IndexOf(clsName)<0 then
         Forwards.Add.Name:=clsName;
   end;

   for y := 0 to aCollection.Count - 1 do begin
      collSym:=TdwsSymbol(aCollection.Items[y]);
      if not collSym.IsGenerating then begin
         try
            sym:=collSym.Generate(systemTable, Table);
            if sym is TOperatorSymbol then
               operators.RegisterOperator(TOperatorSymbol(sym));
         except
            on e: Exception do
               raise EGenerationError.CreateFmt(UNT_UnitGenerationError,
                                                [UnitName, e.Message]);
         end;
      end;
   end;
end;

procedure TdwsUnit.AddUnitSymbols(systemTable : TSystemSymbolTable; Table: TSymbolTable; operators : TOperators);
var
  x: Integer;
begin
  for x := Low(FCollections) to High(FCollections) do
    FCollections[x].Reset;

  for x := Low(FCollections) to High(FCollections) do
    AddCollectionSymbols(FCollections[x], systemTable, Table, operators);
end;

procedure TdwsUnit.GetClassTypes(List: TStrings);
var
   x : Integer;
   sysTable : TSystemSymbolTable;
begin
   if not Assigned(List) then
      Exit;

   if Assigned(FScript) then begin
      sysTable:=FScript.Config.SystemSymbols.SymbolTable;
      for x:=0 to sysTable.Count - 1 do begin
         if sysTable[x] is TClassSymbol then
            List.Add(sysTable[x].Name);
      end;
   end;

   for x := 0 to FClasses.Count - 1 do
      List.Add(FClasses.Items[x].Name);
end;

procedure TdwsUnit.GetDataTypes(List: TStrings);
var
   x, y: Integer;
   coll: TdwsCollection;
   sysTable : TSystemSymbolTable;
begin
  if not Assigned(List) then
    Exit;

  if Assigned(FScript) then begin
    // Add all type symbols from the systemtable
    sysTable:=FScript.Config.SystemSymbols.SymbolTable;
    for x := 0 to sysTable.Count - 1 do
    begin
      if sysTable[x] is TTypeSymbol then
        List.Add(sysTable[x].Name);
    end;
  end;

  // Only return array-, record- and class symbols, synonyms and enums
  for x := 1 to 6 do
  begin
    coll := FCollections[x];
    for y := 0 to coll.Count - 1 do
      List.Add(coll.Items[y].Name);
  end;

  // ...and sets and delegates
  for x := 12 to 13 do
  begin
    coll := FCollections[x];
    for y := 0 to coll.Count - 1 do
      List.Add(coll.Items[y].Name);
  end;
end;

class function TdwsUnit.GetDelegatesClass: TdwsDelegatesClass;
begin
  Result := TdwsDelegates;
end;

function TdwsUnit.GetSymbol(systemTable : TSystemSymbolTable; Table: TSymbolTable; const Name: UnicodeString): TSymbol;

   procedure RaiseCircularReference(item : TdwsSymbol);
   begin
      raise Exception.CreateFmt(UNT_CircularReference, [item.ClassName+':'+Name]);
   end;

var
   x, y: Integer;
   item: TdwsSymbol;
   coll: TdwsCollection;
begin
   Result:=Table.FindSymbol(Name, cvMagic);
   if Assigned(Result) then Exit;

   for x := Low(FCollections) to High(FCollections) do begin
      // Check if the symbol is defined but not yet generated
      coll := FCollections[x];

      for y := 0 to coll.Count - 1 do begin

         item := coll.Items[y];
         if UnicodeSameText(item.Name, Name) then begin

            // Check for circular references
            if item.IsGenerating then
               RaiseCircularReference(item);

            // Generate the symbol now
            try
               Result := item.Generate(systemTable, Table);
            except
               on e: Exception do
                  raise EHandledGenerationError.Create(e.Message);
            end;

            Exit;
         end;
      end;
   end;
end;

procedure TdwsUnit.SetArrays(const Value: TdwsArrays);
begin
  FArrays.Assign(Value);
end;

procedure TdwsUnit.SetClasses(const Value: TdwsClasses);
begin
  FClasses.Assign(Value);
end;

procedure TdwsUnit.SetConstants(const Value: TdwsConstants);
begin
  FConstants.Assign(Value);
end;

procedure TdwsUnit.SetDelegates(const Value: TdwsDelegates);
begin
  FDelegates.Assign(Value);
end;

procedure TdwsUnit.SetForwards(const Value: TdwsForwards);
begin
  FForwards.Assign(Value);
end;

procedure TdwsUnit.SetFunctions(const Value: TdwsFunctions);
begin
  FFunctions.Assign(Value);
end;

procedure TdwsUnit.SetRecords(const Value: TdwsRecords);
begin
  FRecords.Assign(Value);
end;

// SetInterfaces
//
procedure TdwsUnit.SetInterfaces(const value : TdwsInterfaces);
begin
   FInterfaces.Assign(value);
end;

procedure TdwsUnit.SetVariables(const Value: TdwsVariables);
begin
  FVariables.Assign(Value);
end;

procedure TdwsUnit.SetEnumerations(const Value: TdwsEnumerations);
begin
  FEnumerations.Assign(Value);
end;

procedure TdwsUnit.SetSets(const Value: TdwsSets);
begin
  FSets.Assign(Value);
end;

procedure TdwsUnit.SetInstances(const Value: TdwsInstances);
begin
  FInstances.Assign(Value);
end;

class function TdwsUnit.GetFunctionsClass: TdwsFunctionsClass;
begin
  Result := TdwsFunctions;
end;

class function TdwsUnit.GetArraysClass: TdwsArraysClass;
begin
  Result := TdwsArrays;
end;

class function TdwsUnit.GetClassesClass: TdwsClassesClass;
begin
  Result := TdwsClasses;
end;

class function TdwsUnit.GetConstantsClass: TdwsConstantsClass;
begin
  Result := TdwsConstants;
end;

class function TdwsUnit.GetEnumerationsClass: TdwsEnumerationsClass;
begin
  Result := TdwsEnumerations;
end;

class function TdwsUnit.GetSetsClass: TdwsSetsClass;
begin
  Result := TdwsSets;
end;

class function TdwsUnit.GetForwardsClass: TdwsForwardsClass;
begin
  Result := TdwsForwards;
end;

class function TdwsUnit.GetInstancesClass: TdwsInstancesClass;
begin
  Result := TdwsInstances;
end;

class function TdwsUnit.GetRecordsClass: TdwsRecordsClass;
begin
  Result := TdwsRecords;
end;

// GetInterfacesClass
//
class function TdwsUnit.GetInterfacesClass : TdwsInterfacesClass;
begin
   Result:=TdwsInterfaces;
end;

class function TdwsUnit.GetVariablesClass: TdwsVariablesClass;
begin
  Result := TdwsVariables;
end;

class function TdwsUnit.GetSynonymsClass: TdwsSynonymsClass;
begin
  Result := TdwsSynonyms;
end;

// GetOperatorsClass
//
class function TdwsUnit.GetOperatorsClass: TdwsOperatorsClass;
begin
   Result:=TdwsOperators;
end;

procedure TdwsUnit.SetSynonyms(const Value: TdwsSynonyms);
begin
  FSynonyms.Assign(Value);
end;

// SetOperators
//
procedure TdwsUnit.SetOperators(const Value: TdwsOperators);
begin
   FOperators.Assign(Value);
end;

// StoreArrays
//
function TdwsUnit.StoreArrays : Boolean;
begin
   Result:=FArrays.Count>0;
end;

// StoreClasses
//
function TdwsUnit.StoreClasses : Boolean;
begin
   Result:=FClasses.Count>0;
end;

// StoreConstants
//
function TdwsUnit.StoreConstants : Boolean;
begin
   Result:=FConstants.Count>0;
end;

function TdwsUnit.StoreDelegates: Boolean;
begin
  Result := (FDelegates.Count > 0);
end;

// StoreEnumerations
//
function TdwsUnit.StoreEnumerations : Boolean;
begin
   Result:=FEnumerations.Count>0;
end;

// StoreSets
//
function TdwsUnit.StoreSets : Boolean;
begin
   Result:=FSets.Count>0;
end;

// StoreForwards
//
function TdwsUnit.StoreForwards : Boolean;
begin
   Result:=FForwards.Count>0;
end;

// StoreFunctions
//
function TdwsUnit.StoreFunctions : Boolean;
begin
   Result:=FFunctions.Count>0;
end;

// StoreRecords
//
function TdwsUnit.StoreRecords : Boolean;
begin
   Result:=FRecords.Count>0;
end;

// StoreInterfaces
//
function TdwsUnit.StoreInterfaces : Boolean;
begin
   Result:=(FInterfaces.Count>0);
end;

// StoreVariables
//
function TdwsUnit.StoreVariables : Boolean;
begin
   Result:=FVariables.Count>0;
end;

function TdwsUnit.InternalTypeDefined(const name: UnicodeString; visited: TStringList): Boolean;
var
   dep : String;
   depIdx: integer;
   list: TStringList;
begin
   visited.Add(self.UnitName);
   list := TStringList.Create;
   try
      self.GetDataTypes(list);
      result := list.IndexOf(name) > -1;
   finally
      list.Free;
   end;
   for dep in self.Dependencies do begin
      if visited.IndexOf(dep) > -1 then
         continue;
      depIdx := FScript.FConfig.Units.IndexOfName(dep);
      if depIdx > -1 then
         result := TdwsUnit(FScript.FConfig.Units.Items[depIdx]).InternalTypeDefined(name, visited);
      if result then
         exit;
   end;
end;

function TdwsUnit.TypeDefined(const name: UnicodeString): Boolean;
var
   list: TStringList;
begin
   list := TStringList.Create;
   try
      result := InternalTypeDefined(name, list);
   finally
      list.Free;
   end;
end;

// StoreInstances
//
function TdwsUnit.StoreInstances : Boolean;
begin
   Result:=FInstances.Count>0;
end;

// StoreSynonyms
//
function TdwsUnit.StoreSynonyms : Boolean;
begin
   Result:=FSynonyms.Count>0;
end;

// StoreOperators
//
function TdwsUnit.StoreOperators : Boolean;
begin
   Result:=FOperators.Count>0;
end;

// StoreImplicitUse
//
function TdwsUnit.StoreImplicitUse : Boolean;
begin
   Result:=ImplicitUse;
end;

procedure TdwsUnit.HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);
var
  propName: UnicodeString;
  param: TParamSymbol;
  setValue: Variant;
begin
  { NOTE: Special handling is required for Boolean types. At least with Delphi 5 }
  if (Info.FuncSym is TMethodSymbol) and Assigned(ExtObject) then
  begin
    propName := StrDeleteLeft(Info.FuncSym.Name, 3);  // get property name. Trim off Get/Set prefix
    case TMethodSymbol(Info.FuncSym).Kind of
    fkFunction  :   // function is a "Get" method
      begin
        { Return property value for property GetXXX function }
        // Class
        if Info.FuncSym.Typ is TClassSymbol then   // don't free the object instance returned
          Info.ResultAsVariant := Info.RegisterExternalObject(GetObjectProp(ExtObject, propName), False, False)  // wrap as best we can (find a match)
        // Boolean
        else if ASCIISameText(Info.FuncSym.Typ.Name, SYS_BOOLEAN) then
          Info.ResultAsBoolean := Boolean(GetOrdProp(ExtObject, propName))
        // All others
        else
          Info.ResultAsVariant := GetPropValue(ExtObject, propName);
      end;
    fkProcedure :   // procedure is a "Set" method
      begin
        // Set property value for property SetXXX function
        if Info.FuncSym.Params.Count > 0 then
        begin
          param := Info.FuncSym.Params.Symbols[0] as TParamSymbol;
          // fetch param value by name
          VarCopy(setValue, Info.Data[ param.Name ][0]);
          // Class
          if param.Typ is TClassSymbol then
            SetObjectProp(ExtObject, propName, Info.GetExternalObjForVar(param.Name))
          // Boolean
          else if VarType(setValue) = varBoolean then
            SetOrdProp(ExtObject, propName, Integer(setValue))
          // All others
          else
            SetPropValue(ExtObject, propName, setValue);
        end;
      end;
    end;
  end;
end;

// SetDependencies
//
procedure TdwsUnit.SetDependencies(const val : TStrings);
begin
   FDependencies.Assign(val);
end;

// DependenciesChanged
//
procedure TdwsUnit.DependenciesChanged(Sender : TObject);
begin
   inherited GetDependencies.AssignFromTStrings(FDependencies);
end;

{ AClass is the class to expose to the unit. All published properties of standard
  simple datatypes that are supported in DWS will be exposed that were introduced
  between AAncestor and AClass. The ScriptAncestorType is the type that will be
  used for the new Script class inherited class type. If none is provided then
  AAncestor.ClassName is used. }
procedure TdwsUnit.ExposeClassToUnit(AClass, AAncestor: TClass;  ASearchProgram: TdwsProgram; const ScriptAncestorType: UnicodeString);

    { Determine if the type is available to the program. If so, add the owning
       unit as a dependency. }
    function IsTypeSupported(const ATypeName: UnicodeString): Boolean;
    var
      x: Integer;
      list: TStringList;
    begin
      Result := False;
      // if given a compiled program to search through for type declarations
      if Assigned(ASearchProgram) then
      begin
        for x := 0 to ASearchProgram.Table.Count - 1 do
        begin
          if ASearchProgram.Table.Symbols[x].ClassType=TUnitSymbol then
            // unit has the type declared
            if TUnitSymbol(ASearchProgram.Table.Symbols[x]).Table.FindLocal(ATypeName) <> nil then
            begin
              Result := True;
              // add the declaring unit as a dependency
              if Self.Dependencies.IndexOf(ASearchProgram.Table.Symbols[x].Name) < 0 then
                Self.Dependencies.Add(ASearchProgram.Table.Symbols[x].Name);
            end;
        end;
      end
      // No compiled program provided. Look up type locally
      else
      begin
        list := TStringList.Create;
        try
          Self.GetDataTypes(list);
          Result := list.IndexOf(ATypeName) >= 0;
        finally
          list.Free;
        end;
      end;
    end;

var
  newForward: TdwsForward;
  useClass: TdwsClass;
  newCreate: TdwsConstructor;
  newMeth: TdwsMethod;
  TypeData: PTypeData;
  propTypeData: PTypeData;
  PropList: PPropList;
  PropertyName, PropertyType: UnicodeString;
  i: Integer;
  Include: Boolean;
  getMethName, setMethName: UnicodeString;
  propIsDefault: Boolean;
begin
  if not Assigned(AClass) then
    EXIT;

  // Look for the class. If found use it, otherwise create new.
  useClass := TdwsClass(Classes.Symbols[AClass.ClassName]);
  if not Assigned(useClass) then
  begin
    // Create the class declaration
    useClass := Classes.Add;
    if ScriptAncestorType <> '' then
      useClass.Ancestor := ScriptAncestorType
    else
      useClass.Ancestor := AAncestor.ClassName;
    useClass.Name := AClass.ClassName;
    newCreate := TdwsConstructor(useClass.Constructors.Add);
    newCreate.Name := 'Create';
    newCreate.OnEval := HandleDynamicCreate;

    // Create a forward for the class. Handles all issues with class nestings and orderings
    newForward := TdwsForward(Forwards.Add);
    newForward.Name := useClass.Name;
  end;

  { Adds the published property names in AClass which are declared from AAncestor }
  TypeData := GetTypeData(AClass.ClassInfo);
  New(PropList);
  try
    GetPropInfos(AClass.ClassInfo, PropList);
    for i := 0 to Pred(TypeData^.PropCount) do
    begin
      PropertyName := UnicodeString(PropList^[i]^.Name);
      propIsDefault := WordBool(PropList^[i]^.Default);
      {$ifdef FPC}
      propTypeData := GetTypeData(PropList^[i]^.PropType);
      {$else}
      propTypeData := GetTypeData(PropList^[i]^.PropType^);
      {$endif}

      Include := True;
      if IsTypeSupported(UnicodeString(PropList^[i]^.PropType^.Name)) then
        PropertyType := UnicodeString(PropList^[i]^.PropType^.Name)
      else
      begin
        { NOTE: Could attempt to use the actual type name (ex: TComponentName is a UnicodeString).
          This would require trying to find the type when it is not yet compiled
          or risking using the type name without the alias being declared.
          It is easiest and safest to just support the standard native types. }
        case PropList^[i]^.PropType^.Kind of
        tkInteger : PropertyType := SYS_INTEGER;
        tkFloat : PropertyType := SYS_FLOAT;
        tkString, tkLString, tkWString : PropertyType := SYS_STRING;
        tkVariant : PropertyType := SYS_VARIANT;
        tkEnumeration :    // Booleans are reported as enumerations. Only support booleans
          begin
            {$ifdef FPC}
            if propTypeData^.BaseType = TypeInfo(Boolean) then
            {$else}
            if propTypeData^.BaseType^ = TypeInfo(Boolean) then
            {$endif}
              PropertyType := SYS_BOOLEAN
            else
              Include := False;
          end;
        { TODO : How to support TDateTime? }
        //  CreateGetSetMethodsForType(newClass, SYS_DATETIME);
        else
          Include := False;
        end;
      end;

      { Include property if it does not exist in AAncestor class. }
      // NOTE: In D5, TObject.ClassInfo = nil... would cause AV errors. First test for a valid pointer
      if Include and (AAncestor.ClassInfo <> nil) then
        Include := (GetPropInfo(AAncestor, PropertyName) = nil);

      // if set to include and property not already added
      if Include then 
      begin
        getMethName := 'Get'+PropertyName;
        setMethName := 'Set'+PropertyName;
        // Don't add if already present
        if useClass.Methods.Symbols[getMethName] = nil then
        begin
          // read value
          newMeth := (useClass.Methods.Add as TdwsMethod);
          newMeth.Name := getMethName;
          newMeth.ResultType := PropertyType;
          newMeth.OnEval := HandleDynamicProperty;
          // write value
          newMeth := (useClass.Methods.Add as TdwsMethod);
          newMeth.Name := setMethName;
          newMeth.OnEval := HandleDynamicProperty;
          with newMeth.Parameters.Add do
          begin
            Name := 'Value';
            DataType := PropertyType;
          end;
        end;
        // Create the property that uses the methods
        with useClass.Properties.Add as TdwsProperty do
        begin
          Name := PropertyName;
          DataType := PropertyType;
          ReadAccess := getMethName;
          WriteAccess := setMethName;
          IsDefault := propIsDefault;
        end;
      end;{if Include}
    end;{for i}
  finally
    Dispose(PropList);
  end;
end;

procedure TdwsUnit.ExposeInstanceToUnit(const AName, AClassType: UnicodeString;
  AInstance: TObject);
var
  typSym: TTypeSymbol;
  instFunc: TDynamicInstantiateFunc;
  externalVar: TExternalVarSymbol;
  funcSym: TFuncSymbol;
begin
  { CheckName }
  if AName = '' then
    raise Exception.Create(UNT_NameIsEmpty);
  if Assigned(Table.FindLocal(AName)) then
    raise Exception.CreateFmt(UNT_NameAlreadyExists, [AName]);

  typSym := Table.FindTypeLocal(AClassType);
  // Get the type symbol of this variable
  if not (typSym is TTypeSymbol) then
    raise Exception.CreateFmt(UNT_DatatypeUnknown, [AClassType]);

  if typSym is TClassSymbol then
  begin
    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    instFunc := TDynamicInstantiateFunc.Create(funcSym, AInstance);
    instFunc.ClassSym := TClassSymbol(typSym);
    instFunc.DataSym := TDataSymbol.Create('', typSym);
    Table.AddSymbol(instFunc.DataSym);
    funcSym.Executable := ICallable(instFunc);

    externalVar := TExternalVarSymbol.Create(AName, typSym);
    externalVar.ReadFunc := funcSym;
    Table.AddSymbol(externalVar);
  end
  else
    raise Exception.CreateFmt(UNT_AutoInstantiateWithoutClass, [AClassType]);
end;

procedure TdwsUnit.InitUnitTable(SystemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols; operators : TOperators; UnitTable: TUnitSymbolTable);
begin
   FTable := UnitTable;
   try
      inherited InitUnitTable(SystemTable, UnitSyms, operators, UnitTable);
      if Assigned(FOnAfterInitUnitTable) then
         FOnAfterInitUnitTable(Self);
   finally
      FTable := nil;
   end;
end;

procedure TdwsUnit.HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
begin
  { TODO : If accepted, create a UnicodeString declaration in appropriate unit. }
  raise Exception.CreateFmt('Cannot create dynamic class "%s". Must be obtained from supported objects.', [Info.ScriptObj.GetClassSym.Name]);
end;

{ TdwsConstant }

procedure TdwsConstant.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsConstant then
    FValue := TdwsConstant(Source).Value;
end;

// DoGenerate
//
function TdwsConstant.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                                 ParentSym: TSymbol): TSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   Result:=TConstSymbol.CreateValue(Name, GetDataType(systemTable, Table, DataType), Value);
   GetUnit.Table.AddSymbol(Result);
end;

// GetDisplayName
//
function TdwsConstant.GetDisplayName: String;
var
   valAsString : UnicodeString;
begin
   valAsString:=VarToStr(Value);
   if ASCIISameText(DataType, SYS_STRING) then  // just for show
      valAsString:=''''+valAsString+'''';
   Result := Format('const %s: %s = %s;', [Name, DataType, valAsString]);
end;

{ TdwsVariable }

procedure TdwsVariable.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsVariable then
    FDataType := TdwsVariable(Source).DataType;
end;

function TdwsVariable.GetDisplayName: String;
begin
  Result := Name + ' : ' + DataType;
end;

{ TdwsVariables }

function TdwsVariables.GetDisplayName: String;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Items[0].GetDisplayName;
    for i := 1 to Count - 1 do
      Result := Result + '; ' + Items[i].GetDisplayName;
  end
  else
    Result := '';
end;

// Add
//
function TdwsVariables.Add : TdwsGlobal;
begin
   Result:=TdwsGlobal(inherited Add);
end;

// Add
//
function TdwsVariables.Add(const name, typName : UnicodeString) : TdwsGlobal;
begin
   Result:=Add;
   Result.Name:=name;
   Result.DataType:=typName;
end;

class function TdwsVariables.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsGlobal;
end;

{ TdwsGlobal }

function TdwsGlobal.Parse(const Value : UnicodeString): UnicodeString;
var
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   sourceFile : TSourceFile;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);
      if tok.TestName then begin
         Result := tok.GetToken.AsString;
         tok.KillToken;
      end else raise Exception.Create('name expected');

      // check whether data type is present. Otherwise skip parsing
      if not tok.TestDelete(ttCOLON) then
         Exit;

      // set data type
      if tok.TestName then begin
         DataType := tok.GetToken.AsString;
         tok.KillToken;
      end else raise Exception.Create('Data type expected');
      tok.EndSourceFile;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

function TdwsGlobal.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
                               ParentSym: TSymbol = nil) : TSymbol;
var
   typSym: TTypeSymbol;
   readEventFunc: TReadVarEventFunc;
   writeEventFunc: TWriteVarEventFunc;
   readFunc: TReadVarFunc;
   funcSym: TFuncSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   // Get the type symbol of this variable
   typSym := GetDataType(systemTable, Table, DataType);
   if typSym is TArraySymbol then
      raise EHandledGenerationError.CreateFmt('Globals of array type not supported: %s in %s', [Name, FUnit.Name]);
   if typSym is TRecordSymbol then
      raise EHandledGenerationError.CreateFmt('Globals of record type not supported: %s in %s', [Name, FUnit.Name]);

   if (Assigned(FOnReadVar) or Assigned(FOnWriteVar)) then begin
      Result := TExternalVarSymbol.Create(Name, typSym);

      if Assigned(FOnReadVar) then begin
         funcSym := TFuncSymbol.Create('', fkFunction, 1);
         funcSym.Typ := typSym;

         readEventFunc := TReadVarEventFunc.Create(funcSym);
         readEventFunc.OnReadVar := FOnReadVar;

         funcSym.Executable := ICallable(readEventFunc);

         TExternalVarSymbol(Result).ReadFunc := funcSym;
      end;

      if Assigned(FOnWriteVar) then begin
         funcSym := TFuncSymbol.Create('', fkProcedure, 1);
         funcSym.AddParam(TParamSymbol.Create('Value', typSym));

         writeEventFunc := TWriteVarEventFunc.Create(funcSym);
         writeEventFunc.OnWriteVar := FOnWriteVar;

         funcSym.Executable := ICallable(writeEventFunc);

         TExternalVarSymbol(Result).WriteFunc := funcSym;
      end;
   end else begin
      Result := TExternalVarSymbol.Create(Name, typSym);

      funcSym := TFuncSymbol.Create('', fkFunction, 1);
      funcSym.Typ := typSym;

      readFunc := TReadVarFunc.Create(funcSym);
      TExternalVarSymbol(Result).ReadFunc := funcSym;

      funcSym := TFuncSymbol.Create('', fkProcedure, 1);
      funcSym.AddParam(TParamSymbol.Create('Value', typSym));
      TWriteVarFunc.Create(funcSym, readFunc);
      TExternalVarSymbol(Result).WriteFunc := funcSym;
   end;

   GetUnit.Table.AddSymbol(Result);
end;

procedure TdwsGlobal.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsGlobal then
  begin
    FOnReadVar := TdwsGlobal(Source).OnReadVar;
    FOnWriteVar := TdwsGlobal(Source).OnWriteVar;
  end;
end;

// ------------------
// ------------------ TDynamicInstantiateFunc ------------------
// ------------------

constructor TDynamicInstantiateFunc.Create(FuncSym: TFuncSymbol;
  AExternalObject: TObject);
begin
   inherited Create;
   FuncSym.Executable := ICallable(Self);
   FExternalObject := AExternalObject;
end;

// Call
//
procedure TDynamicInstantiateFunc.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   v : Variant;
   scriptObj : TScriptObjInstance;
begin
   exec.Stack.ReadValue(DataSym.StackAddr, v);
   if VarIsEmpty(v) then begin
      // First access to this variable. Create object instance!
      scriptObj := TScriptObjInstance.Create(FClassSym);
      scriptObj.ExternalObject := FExternalObject;
      v := IScriptObj(scriptObj);
      exec.Stack.WriteValue(DataSym.StackAddr, v);
   end;
   exec.Stack.WriteValue(exec.Stack.StackPointer+func.Result.StackAddr, v)
end;

// ------------------
// ------------------ TInstantiateFunc ------------------
// ------------------

// Create
//
constructor TInstantiateFunc.Create(FuncSym: TFuncSymbol);
begin
   inherited Create;
   FuncSym.Executable := ICallable(Self);
end;

procedure TInstantiateFunc.InitSymbol(Symbol: TSymbol; const msgs : TdwsCompileMessageList);
begin
   inherited;
   if Assigned(FOnInitSymbol) then
      FOnInitSymbol(Self,Symbol);
end;

procedure TInstantiateFunc.InitExpression(Expr: TExprBase);
begin
  inherited;
  if Assigned(FOnInitExpr) then
    FOnInitExpr(Self,Expr);
end;

// Call
//
procedure TInstantiateFunc.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   p : PVariant;

   procedure DoInstantiate;
   var
      scriptObj : TScriptObjInstance;
      extObj : TObject;
      info : TProgramInfo;
   begin
      scriptObj := TScriptObjInstance.Create(ClassSym);
      scriptObj.OnObjectDestroy := OnObjectDestroy;
      p^ := IScriptObj(scriptObj);
      exec.Stack.WriteValue(DataSym.StackAddr, p^);

      extObj:=nil;
      if Assigned(FOnInstantiate) then begin
         info:=exec.AcquireProgramInfo(func);
         try
            FOnInstantiate(info, extObj);
         finally
            exec.ReleaseProgramInfo(info);
         end;
         if extObj=nil then begin
            p^ := IUnknown(nil);
            exec.Stack.WriteValue(DataSym.StackAddr, p^);
            Exit;
         end;
      end;

      scriptObj.ExternalObject := extObj;
   end;

begin
   p:=@exec.Stack.Data[DataSym.StackAddr];
   if VarIsEmpty(p^) then begin
      // First access to this variable. Create object instance!
      DoInstantiate;
   end;
   exec.Stack.WriteValue(exec.Stack.StackPointer+func.Result.StackAddr, p^)
end;

// ------------------
// ------------------ TdwsParameter ------------------
// ------------------

// Create
//
constructor TdwsParameter.Create(Collection: TCollection);
begin
   inherited;
   FIsWritable := True;
end;

procedure TdwsParameter.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsParameter then begin
      FIsVarParam := TdwsParameter(Source).IsVarParam;
      FIsLazy := TdwsParameter(Source).IsLazy;
      FIsWritable := TdwsParameter(Source).IsWritable;
      VarCopySafe(FDefaultValue, TdwsParameter(Source).DefaultValue);
      FHasDefaultValue := TdwsParameter(Source).HasDefaultValue;
   end;
end;

function TdwsParameter.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
   Result:=nil;
   Assert(False);  // shouldn't be used anymore
end;

function TdwsParameter.GetDisplayName: String;
begin
   Result:=inherited GetDisplayName;
   if IsVarParam then
      if IsWritable then
         Result:='var '+Result
      else Result:='const '+Result
   else if IsLazy then
      Result:='lazy '+Result;
   if HasDefaultValue then
      Result:=Result+Format(' = %s', [ValueToString(DefaultValue)]);
end;

procedure TdwsParameter.SetDefaultValue(const Value: Variant);
begin
  FDefaultValue := Value;
  FHasDefaultValue := not (FIsVarParam and FIsWritable);
end;

procedure TdwsParameter.SetHasDefaultValue(const Value: Boolean);
begin
  FHasDefaultValue := Value and not (FIsVarParam and FIsWritable);
end;

// SetIsVarParam
//
procedure TdwsParameter.SetIsVarParam(const Value: Boolean);
begin
   FIsVarParam := Value;
   if FIsVarParam and FIsWritable then
      FHasDefaultValue := False;
   if FIsVarParam then
      FIsLazy:=False;
end;

// SetIsWritable
//
procedure TdwsParameter.SetIsWritable(const Value: Boolean);
begin
   FIsWritable := Value;
   if FIsVarParam and FIsWritable then
      FHasDefaultValue := False;
   if FIsWritable then
      FIsLazy:=False;
end;

// SetIsLazy
//
procedure TdwsParameter.SetIsLazy(const val : Boolean);
begin
   FIsLazy:=val;
   if FIsLazy then begin
      IsVarParam:=False;
      IsWritable:=False;
   end;
end;

// Parse
//
function TdwsParameter.Parse(const Value : UnicodeString): UnicodeString;
var
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   sourceFile : TSourceFile;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);

      case tok.TestDeleteAny([ttVAR, ttCONST, ttLAZY]) of
         ttVAR : begin
            IsVarParam := True;
            IsWritable := True;
         end;
         ttCONST : begin
            IsVarParam := True;
            IsWritable := False;
         end;
         ttLAZY : begin
            IsLazy := True;
            IsWritable := False;
         end;
      end;

      if tok.TestName then begin
         Result := tok.GetToken.AsString;
         tok.KillToken;
      end else raise Exception.Create('Parameter name expected');

      // check whether data type is present. Otherwise skip parsing
      if not tok.TestDelete(ttCOLON) then
         Exit;

      // set data type
      if tok.TestName then begin
         DataType := tok.GetToken.AsString;
         tok.KillToken;
      end else raise Exception.Create('Data type expected');

      // check for default value
      if tok.TestDelete(ttEQ) then begin
         case tok.TestAny([ttStrVal, ttIntVal, ttFloatVal]) of
            ttStrVal : begin
               DefaultValue := tok.GetToken.AsString;
               tok.KillToken;
            end;
            ttIntVal : begin
               DefaultValue := tok.GetToken.FInteger;
               tok.KillToken;
            end;
            ttFloatVal : begin
               DefaultValue := tok.GetToken.FFloat;
               tok.KillToken;
            end;
         else
            if tok.TestName then begin
               DefaultValue := tok.GetToken.AsString;
               tok.KillToken;
            end else raise Exception.Create('Default value expected');
         end;
      end;

      tok.EndSourceFile;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

// ------------------
// ------------------ TdwsCallable ------------------
// ------------------

// Create
//
constructor TdwsCallable.Create(owner : TObject);
begin
   FOwner:=owner;
   _AddRef;
end;

// InitSymbol
//
procedure TdwsCallable.InitSymbol(symbol : TSymbol; const msgs : TdwsCompileMessageList);
begin
   if Assigned(FOnInitSymbol) then
      FOnInitSymbol(FOwner, symbol);
end;

// InitExpression
//
procedure TdwsCallable.InitExpression(expr : TExprBase);
begin
   if Assigned(FOnInitExpr) then
      FOnInitExpr(FOwner, expr);
end;

// SubExpr
//
function TdwsCallable.SubExpr(i : Integer) : TExprBase;
begin
   Result:=nil;
end;

// SubExprCount
//
function TdwsCallable.SubExprCount : Integer;
begin
   Result:=0;
end;

// Specialize
//
function TdwsCallable.Specialize(const context : ISpecializationContext) : IExecutable;
begin
   context.AddCompilerError('TdwsCallable cannot be specialized yet');
end;

// CompileTimeCheck
//
procedure TdwsCallable.CompileTimeCheck(context : TdwsCompilerContext; expr : TFuncExprBase);
begin
   // nothing yet
end;

// ------------------
// ------------------ TdwsFunctionCallable ------------------
// ------------------

// Call
//
procedure TdwsFunctionCallable.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
begin
   if Assigned(FOnEval) then begin
      info:=exec.AcquireProgramInfo(Func);
      try
         FOnEval(info);
      finally
         exec.ReleaseProgramInfo(info);
      end;
   end;
end;

// ------------------
// ------------------ TdwsFunctionSymbol ------------------
// ------------------

// Create
//
constructor TdwsFunctionSymbol.Create;
begin
   inherited;
   FParameters:=TdwsParameters.Create(Self);
end;

// Destroy
//
destructor TdwsFunctionSymbol.Destroy;
begin
   FCallable._Release;
   FCallable:=nil;
   FParameters.Free;
   inherited;
end;

// SetParameters
//
procedure TdwsFunctionSymbol.SetParameters(const Value: TdwsParameters);
begin
   FParameters.Assign(Value);
end;

// GetOnInitExpr
//
function TdwsFunctionSymbol.GetOnInitExpr : TInitExprEvent;
begin
   Result:=FCallable.OnInitExpr;
end;

// SetOnInitExpr
//
procedure TdwsFunctionSymbol.SetOnInitExpr(const val : TInitExprEvent);
begin
   FCallable.OnInitExpr:=val;
end;

// GetOnInitSymbol
//
function TdwsFunctionSymbol.GetOnInitSymbol : TInitSymbolEvent;
begin
   Result:=FCallable.OnInitSymbol;
end;

// SetOnInitSymbol
//
procedure TdwsFunctionSymbol.SetOnInitSymbol(const val : TInitSymbolEvent);
begin
   FCallable.OnInitSymbol:=val;
end;

// StoreParameters
//
function TdwsFunctionSymbol.StoreParameters : Boolean;
begin
   Result:=(FParameters.Count>0);
end;

// DoGenerate
//
function TdwsFunctionSymbol.DoGenerate(systemTable : TSystemSymbolTable; table : TSymbolTable; parentSym : TSymbol = nil) : TSymbol;
var
   funcSym : TFuncSymbol;
begin
   FIsGenerating:=True;
   CheckName(Table, Name, Overloaded);
   if ResultType<>'' then
      GetDataType(systemTable, Table, ResultType);

   funcSym:=TFuncSymbol.Generate(table, Name, GetParameters(systemTable, table), ResultType);
   try
      funcSym.Params.AddParent(table);

      funcSym.Executable:=FCallable;
      funcSym.DeprecatedMessage:=Deprecated;
      funcSym.IsOverloaded:=Overloaded;
      GetUnit.Table.AddSymbol(funcSym);
   except
      funcSym.Free;
      raise;
   end;
   Result:=funcSym;
end;

// GetParameters
//
function GetParameters(Symbol: TdwsSymbol; Parameters: TdwsParameters;
                       systemTable : TSystemSymbolTable; Table: TSymbolTable): TParamArray;
var
   i, j, elemValue: Integer;
   name, enumValue : UnicodeString;
   paramSym, elemSym : TSymbol;
   param : TdwsParameter;
   paramRec : PParamRec;
begin
   SetLength(Result, Parameters.Count);
   for i := 0 to Parameters.Count - 1 do begin
      param:=TdwsParameter(Parameters.Items[i]);
      name := param.Name;

      // Check whether parameter name is unique
      for j := i - 1 downto 0 do begin
         if UnicodeSameText(Result[j].ParamName, name) then
            raise Exception.CreateFmt(UNT_ParameterNameAlreadyExists, [name]);
      end;

      paramRec := @Result[i];

      paramRec.IsVarParam := param.IsVarParam and param.IsWritable;
      paramRec.IsConstParam := param.IsVarParam and not param.IsWritable;
      paramRec.ParamName := name;
      paramRec.ParamType := param.DataType;

      paramRec.HasDefaultValue := param.HasDefaultValue;
      if paramRec.HasDefaultValue then begin
         SetLength(paramRec.DefaultValue, 1);
         paramSym:=Symbol.GetDataType(systemTable, Table, paramRec.ParamType);
         if paramSym is TEnumerationSymbol then begin
            enumValue:=param.DefaultValue;
            if UnicodeSameText(StrBeforeChar(enumValue, '.'), paramSym.Name) then
               enumValue:=StrAfterChar(enumValue, '.');
            elemSym:=TEnumerationSymbol(paramSym).Elements.FindLocal(enumValue);
            if elemSym=nil then
               elemValue:=param.DefaultValue
            else elemValue:=TElementSymbol(elemSym).Value;
            paramRec.DefaultValue[0] := elemValue;
         end else if paramSym.IsPointerType and VarIsNull(param.DefaultValue) then
            paramRec.DefaultValue[0] := IUnknown(nil)
         else paramRec.DefaultValue[0] := param.DefaultValue;
      end else paramRec.DefaultValue := nil;

      Symbol.GetUnit.GetSymbol(systemTable, Table, paramRec.ParamType);
  end;
end;


function TdwsFunctionSymbol.GetParameters(systemTable : TSystemSymbolTable; Table: TSymbolTable): TParamArray;
begin
  Result := dwsComp.GetParameters(Self,Parameters, systemTable, Table);
end;

// GetDisplayName
//
function TdwsFunctionSymbol.GetDisplayName: String;
begin
   Result:=Parameters.GetDisplayName;
   if Result<>'' then
      Result:='('+Result+')';
   if ResultType='' then
      Result:=Format('procedure %s%s;', [Name, Result])
   else Result:=Format('function %s%s : %s;', [Name, Result, ResultType]);
   if Overloaded then
      Result:=Result+' overloaded;';
   if Deprecated<>'' then
      Result:=Result+' deprecated;';
end;

// SetResultType
//
procedure TdwsFunctionSymbol.SetResultType(const val : TDataType);
begin
   FResultType:=val;
end;

procedure TdwsFunctionSymbol.SetMethodType(const value : TTokenType);
begin
   raise Exception.Create('Only methods of a class can be constructors or destructors');
end;

// ParseFunctionName
//
function TdwsFunctionSymbol.Parse(const Value : UnicodeString): UnicodeString;
var
   param : TdwsParameter;
   params : array of TdwsParameter;
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   tokenType: TTokenType;
   methodType: TTokenType;
   sourceFile : TSourceFile;
   hasName: boolean;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);

      // check whether tokens are available at all
      if not tok.HasTokens then
         raise Exception.Create('Token expected');

      methodType := tok.TestDeleteAny([ttFUNCTION, ttPROCEDURE, ttMETHOD, ttCONSTRUCTOR, ttDESTRUCTOR]);
      if methodType in [ttCONSTRUCTOR, ttDestructor] then
         self.SetMethodType(methodType);
      hasName := tok.TestName;
      tokenType := tok.GetToken.FTyp;

      // check for name
      if not (hasName or (tokenType <> ttNone)) then
         raise Exception.Create('Name expected');

      // get name and kill token
      Result := tok.GetToken.AsString;
      tok.KillToken;

      // kill token and eventually ignore additional procedure / function
      if tokenType <> ttNone then begin
         // check if further tokens are available, if not accept name
         if not tok.HasTokens then begin
            Result := Value;
            Exit;
         end;
      end;

      // check for parameters
      if tok.TestDelete(ttBLEFT) then begin
         while not tok.TestDelete(ttBRIGHT) do begin
            param := Parameters.Add;

            case tok.TestDeleteAny([ttVAR, ttCONST, ttLAZY]) of
               ttVAR: begin
                  param.IsVarParam := True;
                  param.IsWritable := True;
               end;
               ttCONST: begin
                  param.IsVarParam := True;
                  param.IsWritable := False;
               end;
               ttLAZY: begin
                  param.IsLazy := True;
                  param.IsWritable := False;
               end;
            end;

            if tok.TestName then begin
               param.Name := tok.GetToken.AsString;
               tok.KillToken;
            end else raise Exception.Create('Parameter name expected');

            SetLength(params, 1);
            Params[0] := param;

            while tok.TestDelete(ttCOMMA) do
            begin
               SetLength(params, length(params) + 1);
               param := Parameters.Add;
               param.Assign(params[0]);
               if tok.TestName then begin
                  param.Name := tok.GetToken.AsString;
                  tok.KillToken;
                  params[high(params)] := param;
               end else raise Exception.Create('Parameter name expected');
            end;

            if not tok.TestDelete(ttCOLON) then
               raise Exception.Create('Colon expected');

            if tok.TestName then begin
               for param in params do
                  param.DataType := tok.GetToken.AsString;
               tok.KillToken;
            end else raise Exception.Create('Data type expected');

            // check for default value
            if tok.TestDelete(ttEQ) then begin
               if length(params) > 1 then
                  raise Exception.Create('Default value not allowed for a list of more than 1 parameter');
               case tok.TestAny([ttStrVal, ttIntVal, ttFloatVal]) of
                  ttStrVal: begin
                     params[0].DefaultValue := tok.GetToken.AsString;
                     tok.KillToken;
                  end;
                  ttIntVal: begin
                     params[0].DefaultValue := tok.GetToken.FInteger;
                     tok.KillToken;
                  end;
                  ttFloatVal: begin
                     params[0].DefaultValue := tok.GetToken.FFloat;
                     tok.KillToken;
                  end;
               else
                  if tok.TestName then begin
                     params[0].DefaultValue := tok.GetToken.AsString;
                     tok.KillToken;
                  end else raise Exception.Create('Default value expected');
               end;
            end;

            // eventually head over to next parameter
            if tok.TestDelete(ttSEMI) then
               Continue;
         end;
      end;

      // check for return type
      if tok.TestDelete(ttCOLON) then begin
         if tok.TestName then begin
            ResultType := tok.GetToken.AsString;
            tok.KillToken;
         end;
      end;

      tok.EndSourceFile;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

// Assign
//
procedure TdwsFunctionSymbol.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsFunctionSymbol then begin
      FResultType := TdwsFunctionSymbol(Source).ResultType;
      FParameters.Assign(TdwsFunctionSymbol(Source).Parameters);
      FDeprecated:=TdwsFunctionSymbol(Source).Deprecated;
      FOverloaded:=TdwsFunctionSymbol(Source).Overloaded;
   end;
end;

// ------------------
// ------------------ TdwsFunction ------------------
// ------------------

// Create
//
constructor TdwsFunction.Create;
begin
   inherited;
   FCallable:=TdwsFunctionCallable.Create(Self);
end;

// DoGenerate
//
function TdwsFunction.DoGenerate(systemTable : TSystemSymbolTable; table : TSymbolTable; parentSym : TSymbol = nil): TSymbol;
var
   func : TInternalMagicFunction;
   flags : TInternalFunctionFlags;
   resType : TTypeSymbol;
begin
   if not Assigned(FOnFastEval) then
      Exit(inherited DoGenerate(systemTable, Table, parentSym));

   FIsGenerating:=True;
   CheckName(table, Name);

   flags:=[];
   if Deprecated<>'' then
      Include(flags, iffDeprecated);

   if ResultType='' then begin
      func:=TCustomInternalMagicProcedure.Create(table, Name, GetParameters(systemTable, table),
                                                 ResultType, flags, nil, '');
      TCustomInternalMagicProcedure(func).FOnFastEval:=FOnFastEval;
   end else begin
      resType:=GetDataType(systemTable, table, ResultType);
      if resType.Size<>1 then begin
         func:=TCustomInternalMagicDataFunction.Create(table, Name, GetParameters(systemTable, table),
                                                       ResultType, flags, nil, '');
         TCustomInternalMagicDataFunction(func).FOnFastEval:=FOnFastEval;
         TCustomInternalMagicDataFunction(func).FSize:=resType.Size;
      end else begin
         func:=TCustomInternalMagicFunction.Create(table, Name, GetParameters(systemTable, table),
                                                   ResultType, flags, nil, '');
         TCustomInternalMagicFunction(func).FOnFastEval:=FOnFastEval;
      end;
   end;

   Result:=table.FindLocal(Name) as TMagicFuncSymbol;
end;

//
// GetOnEval
//
function TdwsFunction.GetOnEval : TFuncEvalEvent;
begin
   Result:=TdwsFunctionCallable(FCallable).OnEval;
end;

// SetOnEval
//
procedure TdwsFunction.SetOnEval(const val : TFuncEvalEvent);
begin
   TdwsFunctionCallable(FCallable).OnEval:=val;
end;

// ------------------
// ------------------ TdwsField ------------------
// ------------------

// Create
//
constructor TdwsField.Create(Collection: TCollection);
begin
   inherited;
   FVisibility:=cvPublic;
end;

function TdwsField.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
var
   data : TData;
begin
   FIsGenerating := True;
   CheckName(TClassSymbol(ParentSym).Members, Name);
   Result := TFieldSymbol.Create(Name, GetDataType(systemTable, Table, DataType), Visibility);
   if FHasDefaultValue then begin
      SetLength(data, 1);
      data[0]:=FDefaultValue;
      TFieldSymbol(Result).DefaultValue:=data;
   end;
end;

// GetDisplayName
//
function TdwsField.GetDisplayName: String;
begin
   Result:=TClassSymbol.VisibilityToString(Visibility)+' '+inherited GetDisplayName;
end;

// SetDefaultValue
//
procedure TdwsField.SetDefaultValue(const Value: Variant);
begin
   FDefaultValue:=Value;
   FHasDefaultValue:=True;
end;

// GetHasDefaultValue
//
function TdwsField.GetHasDefaultValue : Boolean;
begin
   Result:=FHasDefaultValue;
end;

function TdwsField.Parse(const Value : UnicodeString): UnicodeString;
var
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   sourceFile : TSourceFile;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);

      // check for visibility
      case tok.TestDeleteAny([ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED]) of
         ttPRIVATE:
            Visibility := cvPrivate;

         ttPROTECTED:
            Visibility := cvProtected;

         ttPUBLIC:
            Visibility := cvPublic;

         ttPUBLISHED:
            Visibility := cvPublished;
      end;

      if tok.TestName then
      begin
         Result := tok.GetToken.AsString;
         tok.KillToken;
      end;

      // check for return type
      if tok.TestDelete(ttCOLON) then begin
         if tok.TestName then
         begin
            DataType := tok.GetToken.AsString;
            tok.KillToken;
         end;
      end;

      // check for default value
      if tok.TestDelete(ttEQ) then begin
         case tok.TestAny([ttStrVal, ttIntVal, ttFloatVal]) of
            ttStrVal: begin
               DefaultValue := tok.GetToken.AsString;
               tok.KillToken;
            end;
            ttIntVal: begin
               DefaultValue := tok.GetToken.FInteger;
               tok.KillToken;
            end;
            ttFloatVal: begin
               DefaultValue := tok.GetToken.FFloat;
               tok.KillToken;
            end;
         else
            if tok.TestName then begin
               DefaultValue := tok.GetToken.AsString;
               tok.KillToken;
            end else raise Exception.Create('Default value expected');
         end;
      end;

      tok.EndSourceFile;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

// ------------------
// ------------------ TdwsMethodCallable ------------------
// ------------------

// Call
//
procedure TdwsMethodCallable.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
   isClassMethod : Boolean;
   methodSymbol : TMethodSymbol;
begin
   if Assigned(FOnEval) then begin
      info:=exec.AcquireProgramInfo(func);
      try
         methodSymbol:=(Func as TMethodSymbol);

         isClassMethod:=methodSymbol.IsClassMethod;
         if not isClassMethod then
            info.PrepareScriptObj;

         if Assigned(info.ScriptObj) then  begin
            FOnEval(info, info.ScriptObj.ExternalObject);
         end else if isClassMethod then
            FOnEval(info, nil)
         else raise Exception.Create('Object not instantiated');
      finally
         exec.ReleaseProgramInfo(info);
      end;
   end;
end;

// ------------------
// ------------------ TdwsMethod ------------------
// ------------------

// Create
//
constructor TdwsMethod.Create(Collection: TCollection);
begin
   inherited;
   FVisibility:=cvPublic;
   FCallable:=TdwsMethodCallable.Create(Self);
end;

// DoGenerate
//
function TdwsMethod.DoGenerate(systemTable : TSystemSymbolTable; table : TSymbolTable; parentSym : TSymbol = nil) : TSymbol;
var
   methSymbol : TMethodSymbol;
begin
   FIsGenerating := True;
   CheckName(TClassSymbol(parentSym).Members, Name, Overloaded);

   if ResultType <> '' then
      GetUnit.GetSymbol(systemTable, table, ResultType);

   methSymbol:=TMethodSymbol.Generate(table, Kind, Attributes, Name,
                                      GetParameters(systemTable, table), ResultType,
                                      TClassSymbol(parentSym), Visibility, Overloaded);
   try
      methSymbol.Params.AddParent(table);
      methSymbol.DeprecatedMessage:=Deprecated;
      methSymbol.IsOverloaded:=Overloaded;

      methSymbol.Executable:=FCallable;
   except
      methSymbol.Free;
      raise;
   end;

   Result:=methSymbol;
end;

// GetDisplayName
//
function TdwsMethod.GetDisplayName: String;
begin
   Result:=Parameters.GetDisplayName;

   if Result<>'' then
      Result:='('+Result+')';

   case FKind of
      mkProcedure:
         Result:=Format('procedure %s%s;', [Name, Result]);
      mkFunction:
         Result:=Format('function %s%s : %s;', [Name, Result, ResultType]);
      mkMethod:
         Result:=Format('method %s%s : %s;', [Name, Result, ResultType]);
      mkConstructor:
         Result:=Format('constructor %s%s;', [Name, Result]);
      mkDestructor:
         Result:=Format('destructor %s%s;', [Name, Result]);
      mkClassProcedure:
         Result:=Format('class procedure %s%s;', [Name, Result]);
      mkClassFunction:
         Result:=Format('class function %s%s : %s;', [Name, Result, ResultType]);
      mkClassMethod:
         Result:=Format('class method %s%s : %s;', [Name, Result, ResultType]);
   else
      Assert(false); // if triggered, this func needs upgrade !
   end;
   if maStatic in Attributes then
      Result:=Result+' static;';
   if Deprecated<>'' then
      Result:=Result+' deprecated;';
   Result:=TClassSymbol.VisibilityToString(Visibility)+' '+Result;
end;

// GetOnEval
//
function TdwsMethod.GetOnEval : TMethodEvalEvent;
begin
   Result:=TdwsMethodCallable(FCallable).OnEval;
end;

// SetOnEval
//
procedure TdwsMethod.SetOnEval(const val : TMethodEvalEvent);
begin
   TdwsMethodCallable(FCallable).OnEval:=val;
end;

// SetResultType
//
procedure TdwsMethod.SetResultType(const val: TDataType);
begin
   inherited;
   if val <> '' then begin
      case FKind of
         mkProcedure:
            FKind := mkFunction;
         mkClassProcedure:
            FKind := mkClassFunction;
      end
   end else begin
      case FKind of
         mkFunction:
            FKind := mkProcedure;
         mkClassFunction:
            FKind := mkClassProcedure;
      end;
   end;
end;

// SetAttributes
//
procedure TdwsMethod.SetAttributes(const attribs : TMethodAttributes);
begin
   FAttributes:=attribs;
   // normalize attributes
   if maOverride in attribs then
      Include(FAttributes, maVirtual);
end;

procedure TdwsMethod.SetMethodType(const value: TTokenType);
begin
   case value of
      ttCONSTRUCTOR: FKind := mkConstructor;
      ttDESTRUCTOR: FKind := mkDestructor;
   end;
end;

procedure TdwsMethod.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsMethod then
  begin
    FAttributes := TdwsMethod(Source).Attributes;
    FKind := TdwsMethod(Source).Kind;
    FResultType := TdwsMethod(Source).ResultType;
    FVisibility := TdwsMethod(Source).Visibility;
  end;
end;

// ------------------
// ------------------ TdwsConstructorCallable ------------------
// ------------------

// Call
//
procedure TdwsConstructorCallable.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   info : TProgramInfo;
   extObj : TObject;
begin
   info:=exec.AcquireProgramInfo(Func);
   try
      info.PrepareScriptObj;

      if Assigned(FOnEval) then begin
         if Assigned(info.ScriptObj) then begin
            extObj:=info.ScriptObj.ExternalObject; // may assigned by Info.GetConstructor()
            FOnEval(info, extObj);
            info.ScriptObj.ExternalObject:=extObj;
         end;
      end;
   finally
      exec.ReleaseProgramInfo(info);
   end;
end;

// ------------------
// ------------------ TdwsConstructor ------------------
// ------------------

// Create
//
constructor TdwsConstructor.Create(collection : TCollection);
begin
   inherited;
   // Name the first constructor "Create" by default
   if Collection.Count = 1 then
      FName := 'Create';
   FVisibility:=cvPublic;
   FCallable:=TdwsConstructorCallable.Create(Self);
end;

procedure TdwsConstructor.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsMethod then begin
    FAttributes := TdwsMethod(Source).Attributes;
    FVisibility := TdwsMethod(Source).Visibility;
  end;
end;

// DoGenerate
//
function TdwsConstructor.DoGenerate(systemTable : TSystemSymbolTable; table : TSymbolTable; parentSym : TSymbol) : TSymbol;
var
   methSymbol : TMethodSymbol;
begin
   FIsGenerating := True;
   CheckName(TClassSymbol(ParentSym).Members, Name, Overloaded);

   methSymbol := TMethodSymbol.Generate(Table, mkConstructor, Attributes, Name,
                                        GetParameters(systemTable, Table), '', TClassSymbol(ParentSym),
                                        Visibility, Overloaded);
   try
      methSymbol.Params.AddParent(Table);
      methSymbol.Executable := FCallable;
   except
      methSymbol.Free;
      raise;
   end;
   Result:=methSymbol;
end;

function TdwsConstructor.GetDisplayName: String;
begin
  Result := Parameters.GetDisplayName;

  if Result <> '' then
    Result := '(' + Result + ')';

  Result:=TClassSymbol.VisibilityToString(Visibility)+Format(' constructor %s%s;', [Name, Result]);
end;

// GetOnEval
//
function TdwsConstructor.GetOnEval : TAssignExternalObjectEvent;
begin
   Result:=TdwsConstructorCallable(FCallable).OnEval;
end;

// SetOnEval
//
procedure TdwsConstructor.SetOnEval(const val : TAssignExternalObjectEvent);
begin
   TdwsConstructorCallable(FCallable).OnEval:=val;
end;

// SetAttributes
//
procedure TdwsConstructor.SetAttributes(const attribs : TMethodAttributes);
begin
   FAttributes:=attribs;
   // normalize attributes
   if maOverride in attribs then
      Include(FAttributes, maVirtual);
end;

function TdwsConstructor.GetResultType: UnicodeString;
begin
  // Hides the property "ResultType" in the object inspector
  Result := '';
end;

// ------------------
// ------------------ TdwsClassConstant ------------------
// ------------------

// Create
//
constructor TdwsClassConstant.Create(Collection: TCollection);
begin
   inherited;
   FVisibility:=cvPublic;
end;

// Assign
//
procedure TdwsClassConstant.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsClassConstant then
      FVisibility:=TdwsClassConstant(Source).Visibility;
end;

// DoGenerate
//
function TdwsClassConstant.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   Result:=TClassConstSymbol.CreateValue(Name, GetDataType(systemTable, Table, DataType), Value);
   TClassConstSymbol(Result).Visibility:=Visibility;
end;

// GetDisplayName
//
function TdwsClassConstant.GetDisplayName: String;
begin
   Result:=TClassSymbol.VisibilityToString(Visibility)+' '+inherited GetDisplayName;
end;

// ------------------
// ------------------ TdwsClassConstants ------------------
// ------------------

// GetSymbolClass
//
class function TdwsClassConstants.GetSymbolClass : TdwsSymbolClass;
begin
   Result:=TdwsClassConstant;
end;

// Add
//
function TdwsClassConstants.Add : TdwsClassConstant;
begin
   Result:=TdwsClassConstant(inherited Add);
end;

// ------------------
// ------------------ TdwsClass ------------------
// ------------------

// Create
//
constructor TdwsClass.Create(Collection: TCollection);
begin
   inherited;
   FFields := TdwsFields.Create(Self);
   FConstructors := TdwsConstructors.Create(Self);
   FMethods := TdwsMethods.Create(Self);
   FProperties := TdwsProperties.Create(Self);
   FOperators := TdwsClassOperators.Create(Self);
   FConstants := TdwsClassConstants.Create(Self);
end;

// Destroy
//
destructor TdwsClass.Destroy;
begin
   FFields.Free;
   FConstructors.Free;
   FMethods.Free;
   FProperties.Free;
   FOperators.Free;
   FConstants.Free;
   FHelperObject.Free;
   inherited;
end;

// Assign
//
procedure TdwsClass.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsClass then begin
      FAncestor := TdwsClass(Source).Ancestor;
      FFields.Assign(TdwsClass(Source).Fields);
      FMethods.Assign(TdwsClass(Source).Methods);
      FProperties.Assign(TdwsClass(Source).Properties);
      FOperators.Assign(TdwsClass(Source).Operators);
      FConstants.Assign(TdwsClass(Source).Constants);
   end;
end;

// DoGenerate
//
function TdwsClass.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
var
   x : Integer;
   sym : TSymbol;
   ancestorSym, classSym : TClassSymbol;
begin
   FIsGenerating := True;

   classSym := nil;
   sym := GetUnit.Table.FindSymbol(Name, cvMagic);

   if Assigned(sym) then begin
      if sym is TClassSymbol then begin
         classSym:=TClassSymbol(sym);
         if not classSym.IsForwarded then
            raise Exception.Create(UNT_ClassAlreadyDefined);
      end else begin
         raise Exception.CreateFmt(UNT_ClassNameAlreadyDefined,
                                  [Name, sym.Caption]);
      end;
   end;

   if not Assigned(classSym) then
      classSym := TClassSymbol.Create(Name, nil);

   try

      classSym.OnObjectDestroy := FOnObjectDestroy;

      if FAncestor = '' then
         FAncestor := SYS_TOBJECT;

      classSym.IsStatic:=IsStatic;

      ancestorSym := TClassSymbol(GetUnit.GetSymbol(systemTable, Table, FAncestor));
      if ancestorSym = nil then
         raise Exception.CreateFmt(UNT_SuperClassUnknwon, [FAncestor]);

      if ancestorSym.IsSealed then
         raise Exception.CreateFmt(CPE_ClassIsSealed, [FAncestor]);

      classSym.InheritFrom(ancestorSym);

      classSym.IsSealed:=IsSealed;
      classSym.IsExplicitAbstract:=IsAbstract;

      classSym.DeprecatedMessage:=Self.Deprecated;

      for x := 0 to FFields.Count - 1 do
         classSym.AddField(TFieldSymbol(TdwsField(FFields.Items[x]).Generate(systemTable, Table, classSym)));

      for x := 0 to FConstructors.Count - 1 do
         classSym.AddMethod(TMethodSymbol(TdwsConstructor(FConstructors.Items[x]).Generate(systemTable, Table, classSym)));

      for x := 0 to FMethods.Count - 1 do
         classSym.AddMethod(TMethodSymbol(TdwsMethod(FMethods.Items[x]).Generate(systemTable, Table, classSym)));

      for x := 0 to FProperties.Count - 1 do
         classSym.AddProperty(TPropertySymbol(TdwsProperty(FProperties.Items[x]).Generate(systemTable, Table, classSym)));

      for x := 0 to FOperators.Count - 1 do
         classSym.AddOperator(TClassOperatorSymbol(TdwsClassOperator(FOperators.Items[x]).Generate(systemTable, Table, classSym)));

      for x := 0 to FConstants.Count - 1 do
         classSym.AddConst(TClassConstSymbol(TdwsConstant(FConstants.Items[x]).Generate(systemTable, Table, classSym)));

   except
      if not classSym.IsForwarded then
         classSym.Free;
      raise;
   end;

   if classSym.IsForwarded then
      classSym.ClearIsForwarded;

   Result:=classSym;
end;

function TdwsClass.GetDisplayName: String;
begin
   Result:=Name;
   if IsAbstract then
      Result:=Result+' abstract';
   if IsSealed then
      Result:=Result+' sealed';
   if IsStatic then
      Result:=Result+' static';
   if Ancestor<>'' then
      Result:=Result+' ('+Ancestor+')'
   else Result:=Result+' (TObject)';
end;

// StoreConstructors
//
function TdwsClass.StoreConstructors : Boolean;
begin
   Result:=(Constructors.Count>0);
end;

// StoreFields
//
function TdwsClass.StoreFields : Boolean;
begin
   Result:=(Fields.Count>0);
end;

// StoreMethods
//
function TdwsClass.StoreMethods : Boolean;
begin
   Result:=(Methods.Count>0);
end;

// StoreOperators
//
function TdwsClass.StoreOperators : Boolean;
begin
   Result:=(Operators.Count>0);
end;

// StoreConstants
//
function TdwsClass.StoreConstants : Boolean;
begin
   Result:=(Constants.Count>0);
end;

// StoreProperties
//
function TdwsClass.StoreProperties : Boolean;
begin
   Result:=(Properties.Count>0);
end;

{ TdwsMember }

// Create
//
constructor TdwsMember.Create(Collection: TCollection);
begin
   inherited;
   FVisibility:=cvPublic;
end;

function TdwsMember.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
  FIsGenerating := True;
  CheckName(TRecordSymbol(ParentSym).Members, Name);
  Result := TFieldSymbol.Create(Name, GetDataType(systemTable, Table, DataType), Visibility);
end;

// Assign
//
procedure TdwsMember.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsMember then
      FVisibility := TdwsMember(Source).Visibility;
end;

// ------------------
// ------------------ TdwsRecord ------------------
// ------------------

constructor TdwsRecord.Create;
begin
   inherited;
   FMembers:=TdwsMembers.Create(Self);
   FProperties:=TdwsProperties.Create(Self);
end;

destructor TdwsRecord.Destroy;
begin
   FProperties.Free;
   FMembers.Free;
   inherited;
end;

procedure TdwsRecord.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsRecord then begin
      FMembers.Assign(TdwsRecord(Source).Members);
      FProperties.Assign(TdwsRecord(Source).Properties);
   end;
end;

function TdwsRecord.DoGenerate;
var
   x : Integer;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   Result := TRecordSymbol.Create(Name, nil);
   try
      for x := 0 to FMembers.Count - 1 do
         TRecordSymbol(Result).AddField(TFieldSymbol(TdwsMember(FMembers.Items[x]).Generate(systemTable, Table, Result)));
      for x := 0 to FProperties.Count - 1 do
         TRecordSymbol(Result).AddProperty(TPropertySymbol(TdwsProperty(FProperties.Items[x]).Generate(systemTable, Table, Result)));
      GetUnit.Table.AddSymbol(Result);
   except
      Result.Free;
      raise;
   end;
end;

function TdwsRecord.GetDisplayName: String;
begin
  Result := 'Record ' + Name;
end;

// ------------------
// ------------------ TdwsInterface ------------------
// ------------------

// Create
//
constructor TdwsInterface.Create(Collection: TCollection);
begin
   inherited;
   FMethods:=TdwsMethods.Create(Self);
   FProperties:=TdwsProperties.Create(Self);
end;

// Destroy
//
destructor TdwsInterface.Destroy;
begin
   FMethods.Free;
   FProperties.Free;
   inherited;
end;

// Assign
//
procedure TdwsInterface.Assign(Source: TPersistent);
begin
   inherited;
   if Source is TdwsInterface then begin
      FMethods.Assign(TdwsInterface(Source).Methods);
      FProperties.Assign(TdwsInterface(Source).Properties);
   end;
end;

// DoGenerate
//
function TdwsInterface.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
var
   x : Integer;
   sym : TSymbol;
   intfSym, ancestorSym : TInterfaceSymbol;
begin
   FIsGenerating := True;

   intfSym := nil;
   sym := GetUnit.Table.FindSymbol(Name, cvMagic);

   if Assigned(sym) then begin
      if sym is TInterfaceSymbol then begin
         intfSym:=TInterfaceSymbol(sym);
         if not intfSym.IsForwarded then
            raise Exception.Create(UNT_InterfaceAlreadyDefined);
      end else begin
         raise Exception.CreateFmt(UNT_InterfaceNameAlreadyDefined,
                                  [Name, sym.Caption]);
      end;
   end;

   if not Assigned(intfSym) then
      intfSym := TInterfaceSymbol.Create(Name, nil);

   try
      ancestorSym := (GetUnit.GetSymbol(systemTable, Table, FAncestor) as TInterfaceSymbol);
      if ancestorSym <> nil then
         intfSym.InheritFrom(ancestorSym);

      GetUnit.Table.AddSymbol(intfSym);

      for x := 0 to FMethods.Count - 1 do
         intfSym.AddMethod(TMethodSymbol(TdwsMethod(FMethods.Items[x]).Generate(systemTable, Table, intfSym)));

      for x := 0 to FProperties.Count - 1 do
         intfSym.AddProperty(TPropertySymbol(TdwsProperty(FProperties.Items[x]).Generate(systemTable, Table, intfSym)));
   except
      if not intfSym.IsForwarded then
         intfSym.Free;
      raise;
   end;

   if intfSym.IsForwarded then
      intfSym.ClearIsForwarded;

   Result:=intfSym;
end;

// GetDisplayName
//
function TdwsInterface.GetDisplayName : String;
begin
   Result:='Interface '+Name;
end;

// StoreMethods
//
function TdwsInterface.StoreMethods : Boolean;
begin
   Result:=(FMethods.Count>0);
end;

// StoreProperties
//
function TdwsInterface.StoreProperties : Boolean;
begin
   Result:=(FProperties.Count>0);
end;

// ------------------
// ------------------ TdwsInterfaces ------------------
// ------------------

// GetSymbolClass
//
class function TdwsInterfaces.GetSymbolClass : TdwsSymbolClass;
begin
   Result:=TdwsInterface;
end;

// Add
//
function TdwsInterfaces.Add : TdwsInterface;
begin
   Result:=TdwsInterface(inherited Add);
end;

// ------------------
// ------------------ TdwsArray ------------------
// ------------------

procedure TdwsArray.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsArray then
  begin
    FDataType := TdwsArray(Source).DataType;
    FLowBound :=  TdwsArray(Source).LowBound;
    FHighBound :=  TdwsArray(Source).HighBound;
  end;
end;

function TdwsArray.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  if (LowBound = 0) and (HighBound = -1) then
    Result := TDynamicArraySymbol.Create(Name, GetDataType(systemTable, Table, DataType), systemTable.TypInteger)
  else
  begin
    if LowBound > HighBound then
      raise Exception.Create(UNT_InvalidArrayBounds);
    Result := TStaticArraySymbol.Create(Name, GetDataType(systemTable, Table, DataType),
                                        systemTable.TypInteger,
                                        LowBound, HighBound);
  end;
  GetUnit.Table.AddSymbol(Result);
end;

function TdwsArray.GetBoundStored: Boolean;
begin
  Result := not IsDynamic;
end;

function TdwsArray.GetDisplayName: String;
begin
  if IsDynamic then
    Result := Format('%s = array of %s', [Name, DataType])
  else
    Result := Format('%s = array [%d .. %d] of %s', [Name, LowBound, HighBound,
      DataType]);
end;

function TdwsArray.GetIsDynamic: Boolean;
begin
  Result := (FLowBound = 0) and (FHighBound = -1);
end;

procedure TdwsArray.SetIsDynamic(const Value: Boolean);
begin
  if Value then
  begin
    FLowBound := 0;
    FHighBound := -1;
  end
  else if IsDynamic then
    FHighBound := 0;
end;

{ TdwsProperty }

procedure TdwsProperty.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsProperty then
  begin
    FDataType := TdwsProperty(Source).DataType;
    FReadAccess := TdwsProperty(Source).ReadAccess;
    FWriteAccess := TdwsProperty(Source).WriteAccess;
    FParameters.Assign(TdwsProperty(Source).Parameters);
    FIsDefault := TdwsProperty(Source).IsDefault;
    FVisibility:=TdwsProperty(Source).Visibility;
  end;
end;

constructor TdwsProperty.Create(Collection: TCollection);
begin
  inherited;
  FParameters := TdwsParameters.Create(Self);
  FVisibility:=cvPublic;
end;

destructor TdwsProperty.Destroy;
begin
  FParameters.Free;
  inherited;
end;

// DoGenerate
//
function TdwsProperty.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil) : TSymbol;
var
   sym : TSymbol;
   propSym : TPropertySymbol;
   indexData : TData;
   parent : TCompositeTypeSymbol;
begin
   FIsGenerating := True;

   if DataType='' then
      raise Exception.CreateFmt(UNT_DatatypeNotSpecified, [Name, ParentSym.Name]);

   propSym:=TPropertySymbol.Create(Name, GetDataType(systemTable, Table, DataType), Visibility, nil);
   Result:=propSym;

   propSym.GenerateParams(Table, GetParameters(Self, Parameters, systemTable, Table));

   parent:=(ParentSym as TCompositeTypeSymbol);

   if FReadAccess <> '' then begin
      sym := parent.Members.FindLocal(FReadAccess);
      if not Assigned(sym) then
         raise Exception.CreateFmt(UNT_ReadAccessNotFound, [ReadAccess]);

      propSym.ReadSym := sym;
   end;

   if FWriteAccess <> '' then begin
      sym := parent.Members.FindLocal(FWriteAccess);
      if not Assigned(sym) then
         raise Exception.CreateFmt(UNT_WriteAccessNotFound, [WriteAccess]);

      propSym.WriteSym := sym;
   end;

   if FIndexType <> '' then begin
      SetLength(indexData,1);
      indexData[0] := FIndexValue;
      propSym.SetIndex(indexData, GetDataType(systemTable, Table, IndexType));
   end;

   if IsDefault then
      parent.DefaultProperty := propSym;

   propSym.DeprecatedMessage:=Deprecated;
end;

// GetDisplayName
//
function TdwsProperty.GetDisplayName: String;
begin
   Result:=TClassSymbol.VisibilityToString(Visibility)+' property '+Name;
   if FParameters.Count>0 then
      Result:=Result+'['+FParameters.GetDisplayName+']';
   Result:=Result+': '+DataType;
   if IndexType<>'' then
      Result:=Result+' index '+ValueToString(IndexValue);
   if ReadAccess<>'' then
      Result:=Result+' read '+ReadAccess;
   if WriteAccess<>'' then
      Result:=Result+' write '+WriteAccess;
   Result:=Result+';';
   if IsDefault then
      Result:=Result+' default;';
end;

// GetIsDefault
//
function TdwsProperty.GetIsDefault: Boolean;
begin
   Result:=FIsDefault and (Parameters.Count>0);
end;

function GetProperties(const Symbol: TObject): TdwsProperties;
begin
  if Symbol is TdwsInterface then
    Exit((Symbol as TdwsInterface).Properties);

  if Symbol is TdwsClass then
    Exit((Symbol as TdwsClass).Properties);

  Exit(nil);
end;

// SetIsDefault
//
procedure TdwsProperty.SetIsDefault(Value: Boolean);
var
  i: Integer;
  properties: TdwsProperties;
begin
  Value := Value and (Parameters.Count > 0);
  if IsDefault <> Value then
  begin
    FIsDefault := Value;
    if FIsDefault then
    begin
      properties := GetProperties(TdwsCollection(Collection).GetOwner); // GetOwner can return a TdwsClass as well as a TdwsInterface, so GetProperties is used to distinguish.
      for i := 0 to properties.Count - 1 do
        if properties.Items[i] <> Self then
          TdwsProperty(properties.Items[i]).FIsDefault := False;
    end;
  end;
end;

// Parse
//
function TdwsProperty.Parse(const Value : UnicodeString): UnicodeString;
var
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   param: TdwsParameter;
   sourceFile : TSourceFile;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);

      // check for visibility
      case tok.TestDeleteAny([ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED]) of
         ttPRIVATE:
            Visibility := cvPrivate;

         ttPROTECTED:
            Visibility := cvProtected;

         ttPUBLIC:
            Visibility := cvPublic;

         ttPUBLISHED:
            Visibility := cvPublished;

         else
            if not tok.HasTokens then
               Result := Value;
      end;

      // delete property or set name to 'property' if this is the only token
      if tok.TestDelete(ttPROPERTY) then
      begin
         if not tok.HasTokens then
            Result := Value;
      end;

      // get property name
      if tok.TestName then begin
         Result := tok.GetToken.AsString;
         tok.KillToken;
      end else raise Exception.Create('Property name expected');

      if tok.TestDelete(ttALEFT) then
      repeat
         if not tok.TestName then
            raise Exception.Create('Parameter name expected!');

         param := Parameters.Add;
         param.Name := tok.GetToken.AsString;
         tok.KillToken;

         if not tok.TestDelete(ttCOLON) then
            raise Exception.Create(''':'' expected!');

         if not tok.TestName then
            raise Exception.Create('Parameter datatype expected!');

         param.DataType := tok.GetToken.AsString;
         tok.KillToken;

         if not tok.HasTokens then
            raise Exception.Create(''']'' expected!');
      until tok.TestDelete(ttARIGHT);

      // check for data type
      if not tok.TestDelete(ttCOLON) then
         Exit;

      if tok.TestName then begin
         DataType := tok.GetToken.AsString;
         tok.KillToken;
      end else raise Exception.Create('Data type expected');

      if tok.TestDelete(ttINDEX) then
      begin
         case tok.TestAny([ttStrVal, ttIntVal, ttFloatVal]) of
            ttStrVal : begin
               IndexValue := tok.GetToken.AsString;
               IndexType := 'UnicodeString';
               tok.KillToken;
            end;
            ttIntVal : begin
               IndexValue := tok.GetToken.FInteger;
               IndexType := 'Integer';
               tok.KillToken;
            end;
            ttFloatVal : begin
               IndexValue := tok.GetToken.FFloat;
               IndexType := 'Float';
               tok.KillToken;
            end;
            else
            if tok.TestName then begin
               IndexValue := tok.GetToken.AsString;
               tok.KillToken;
            end else raise Exception.Create('Index value expected');
         end;
      end;

      if tok.TestDelete(ttREAD) then
      begin
         if tok.TestName then begin
            ReadAccess := tok.GetToken.AsString;
            tok.KillToken;
         end else raise Exception.Create('ReadAccess expected');
      end;

      if tok.TestDelete(ttWRITE) then
      begin
         if tok.TestName then begin
            WriteAccess := tok.GetToken.AsString;
            tok.KillToken;
         end else raise Exception.Create('WriteAccess expected');
      end;

      // eventually delete semicolon
      tok.TestDelete(ttSEMI);

      IsDefault := tok.TestDelete(ttDEFAULT);

      tok.EndSourceFile;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

// SetReadAccess
//
procedure TdwsProperty.SetReadAccess(const Value: UnicodeString);
//var
//   cs: TComponentState;
//   Obj: TdwsClass;
//   Meth: TdwsMethod;
begin
   FReadAccess := Value;

   // See SetWriteAccess comment for why this was deactivated
   (*
   cs := FUnit.ComponentState;
   if (csDesigning in cs) and not (csLoading in cs) then
   begin
      Assert(Collection is TdwsProperties);
      if TdwsProperties(Collection).Owner is TdwsClass then
      begin
         Obj := TdwsClass(TdwsProperties(Collection).Owner);
         if Obj.FMethods.GetSymbols(FReadAccess) = nil then
         begin
            Meth := Obj.FMethods.Add;
            Meth.FName := FReadAccess;
            Meth.Visibility := cvProtected;
            Meth.ResultType := DataType;
         end;
      end
   end;
   *)
end;

// SetWriteAccess
//
procedure TdwsProperty.SetWriteAccess(const Value: UnicodeString);
//var
//   cs: TComponentState;
//   Obj: TdwsClass;
//   Meth: TdwsMethod;
//   Param: TdwsParameter;
begin
   FWriteAccess := Value;

   // deactivated by EG because when editing the write access field manually
   // at design-time, it will create all "intermediate" methods
   // f.i. if you type "SetProp", it will create methods "S", "Se", "Set", "SetP", etc.
   // if anyone has a solution for that, let me know
   (*
   cs := FUnit.ComponentState;
   if (csDesigning in cs) and not (csLoading in cs) then
   begin
      Assert(Collection is TdwsProperties);
      if TdwsProperties(Collection).Owner is TdwsClass then
      begin
         Obj := TdwsClass(TdwsProperties(Collection).Owner);
         if Obj.FMethods.GetSymbols(FWriteAccess) = nil then
         begin
            Meth := Obj.FMethods.Add;
            Meth.FName := FWriteAccess;
            Meth.Visibility := cvPrivate;

            Param := Meth.Parameters.Add;
            Param.FName := 'value';
            Param.DataType := DataType;
            Param.IsVarParam := True;
            Param.IsWritable := False;
         end;
      end
   end;
   *)
end;

// SetParameters
//
procedure TdwsProperty.SetParameters(const Value: TdwsParameters);
begin
   FParameters.Assign(Value);
end;

// StoreParameters
//
function TdwsProperty.StoreParameters : Boolean;
begin
   Result:=(FParameters.Count>0);
end;

{ TdwsClassOperator }

procedure TdwsClassOperator.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsClassOperator then
  begin
    FDataType := TdwsClassOperator(Source).DataType;
    FUsesAccess := TdwsClassOperator(Source).UsesAccess;
    FOperator := TdwsClassOperator(Source).&Operator;
  end;
end;

function TdwsClassOperator.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
var
   opSymbol : TClassOperatorSymbol;
   sym : TSymbol;
begin
   FIsGenerating := True;

   opSymbol:=TClassOperatorSymbol.Create(FOperator);
   Result:=opSymbol;

   Result.Typ:=GetDataType(systemTable, Table, DataType);
   sym:=TClassSymbol(ParentSym).Members.FindLocal(FUsesAccess);
   if (sym=nil) or not (sym is TMethodSymbol) then
      raise Exception.CreateFmt(UNT_UsesAccessNotFound, [FUsesAccess]);
   opSymbol.UsesSym:=TMethodSymbol(sym);

   if opSymbol.UsesSym.Params.Count<>1 then
      raise Exception.Create(CPE_SingleParameterExpected);
   if opSymbol.UsesSym.Params[0].Typ<>Result.Typ then
      raise Exception.CreateFmt(CPE_InvalidParameterType, [opSymbol.UsesSym.Name]);
end;

function TdwsClassOperator.GetDisplayName: String;
begin
   Result:=Format('operator %s %s uses %s;', [cTokenStrings[FOperator], DataType, UsesAccess])
end;

{ TdwsSymbol }

constructor TdwsSymbol.Create(Collection: TCollection);
begin
  inherited;
  FUnit := TdwsCollection(Collection).GetUnit;
end;

function TdwsSymbol.GetUnit: TdwsUnit;
begin
  Result := FUnit;
end;

procedure TdwsSymbol.Reset;
begin
  FIsGenerating := False;
end;

procedure TdwsSymbol.SetName(const val : UnicodeString);
begin
   if FUnit.ShouldParseName(val) then
      FName:=Parse(val)
   else FName:=val;
end;

function TdwsSymbol.Parse(const Value: UnicodeString): UnicodeString;
begin
   Result := Value;
end;

function TdwsSymbol.GetNamePath: String;
begin
  if FName <> '' then
    Result := Collection.GetNamePath + FName
  else
    Result := Collection.GetNamePath + IntToStr(Index);
end;

// Generate
//
function TdwsSymbol.Generate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
   try
      Result := DoGenerate(systemTable, Table, ParentSym);
   except
      on e: EHandledGenerationError do
         raise;
      on e: Exception do
         raise Exception.CreateFmt(UNT_SymbolGenerationError, [ClassName, Name, e.Message]);
  end;
end;

// GetDataType
//
function TdwsSymbol.GetDataType(systemTable : TSystemSymbolTable; aTable : TSymbolTable; const aName : UnicodeString) : TTypeSymbol;
var
   sym : TSymbol;
begin
   sym := GetUnit.GetSymbol(systemTable, aTable, aName);
   if not (sym is TTypeSymbol) then
      raise Exception.CreateFmt(UNT_DatatypeUnknown, [aName]);
   Result := TTypeSymbol(sym);
end;

// CheckName
//
procedure TdwsSymbol.CheckName(aTable : TSymbolTable; const aName : UnicodeString;
                               overloaded : Boolean = False);
var
   sym : TSymbol;
   funcSym : TFuncSymbol;
begin
   if aName='' then
      raise Exception.Create(UNT_NameIsEmpty);

   sym:=aTable.FindLocal(aName);
   if Assigned(sym) then begin
      if overloaded then begin
         funcSym:=sym.AsFuncSymbol;
         if (funcSym=nil) or not funcSym.IsOverloaded then
            raise Exception.CreateFmt(UNT_PreviousNotOverloaded, [aName]);
      end else raise Exception.CreateFmt(UNT_NameAlreadyExists, [aName]);
   end;
end;

procedure TdwsSymbol.Assign(Source: TPersistent);
begin
  if Source is TdwsSymbol then
    FName := TdwsSymbol(Source).Name
  else
    inherited;
end;

procedure TdwsSymbol.AssignTo(Dest: TPersistent);
begin
  if Dest is TdwsSymbol then
    TdwsSymbol(Dest).Name := Name
  else
    inherited;
end;

{ TdwsForward }

function TdwsForward.DoGenerate;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   Result := TClassSymbol.Create(Name, nil);
   TClassSymbol(Result).SetForwardedPos(cNullPos);
   GetUnit.Table.AddSymbol(Result);
end;

function TdwsForward.GetDisplayName: String;
begin
  Result := Format('type %s = class;', [Name]);
end;

{ TReadVarEventFunc }

procedure TReadVarEventFunc.Execute(info : TProgramInfo);
var
  Value: Variant;
begin
  if Assigned(FOnReadVar) then
    FOnReadVar(info, Value);
  Info.ResultAsVariant := Value;
end;

{ TWriteVarEventFunc }

procedure TWriteVarEventFunc.Execute(info : TProgramInfo);
begin
  if Assigned(FOnWriteVar) then
    FOnWriteVar(Info, Info.ValueAsVariant['Value']);
end;

{ TReadVarFunc }

constructor TReadVarFunc.Create(FuncSym: TFuncSymbol);
begin
  inherited;
  FTyp := FuncSym.Typ;
  SetLength(FData, FTyp.Size);
  FTyp.InitData(FData, 0);
end;

procedure TReadVarFunc.Execute(info : TProgramInfo);
begin
  Info.Data[SYS_RESULT] := FData;
end;

procedure TReadVarFunc.SetValue(const data : TData; offset : Integer);
begin
   DWSCopyData(data, offset, FData, 0, FTyp.Size);
end;

{ TWriteVarFunc }

constructor TWriteVarFunc.Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
begin
  inherited Create(FuncSym);
  FReadVarFunc := ReadVarFunc;
end;

procedure TWriteVarFunc.Execute(info : TProgramInfo);
begin
  FReadVarFunc.SetValue(info.Execution.Stack.Data, info.Execution.Stack.StackPointer-1);
end;

{ TdwsComponent }

constructor TdwsAbstractUnit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependencies := TUnicodeStringList.Create;
end;

destructor TdwsAbstractUnit.Destroy;
begin
  Script := nil;
  FDependencies.Free;
  inherited;
end;

function TdwsAbstractUnit.GetDependencies: TUnicodeStringList;
begin
  Result := FDependencies;
end;

function TdwsAbstractUnit.GetUnitName: UnicodeString;
begin
  Result := FUnitName;
end;

// Notification
//
procedure TdwsAbstractUnit.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if (Operation=opRemove) and (AComponent=FScript) then
      SetScript(nil);
end;

// BeforeAdditionTo
//
procedure TdwsAbstractUnit.BeforeAdditionTo(dwscript : TObject);
begin
   if dwscript<>FScript then begin
      // at this point an interface was likeley acquired and its ref-counter will be off
      // so the folowing exception is likely to be hidden by another exception in the implicit
      // exception frame, since the code is kaput, which just detach ourselves which will
      // cause a memory leak but no other exception (code is incorrect anyway,
      // so no point in avoiding the leak)
      if Owner<>nil then
         Owner.RemoveComponent(Self);
      raise EdwsInvalidUnitAddition.Create('Do not use AddUnit method but Script property instead');
   end;
end;

// SetScript
//
procedure TdwsAbstractUnit.SetScript(const Value: TDelphiWebScript);
begin
   if Assigned(FScript) then begin
      FScript.RemoveUnit(Self);
      FScript.RemoveFreeNotification(Self);
   end;

   FScript := Value;

   if Assigned(FScript) then begin
      FScript.AddUnit(Self);
      FScript.FreeNotification(Self);
   end;
end;

procedure TdwsAbstractUnit.SetUnitName(const Value: UnicodeString);
begin
  if not (csDesigning in ComponentState) and Assigned(FScript)
     and not UnicodeSameText(Value, FUnitName) then
    raise Exception.Create(UNT_CantChangeUnitName)
  else
    FUnitName := Value;
end;

// GetUnitFlags
//
function TdwsAbstractUnit.GetUnitFlags : TIdwsUnitFlags;
begin
   if ImplicitUse then
      Result:=[ufImplicitUse]
   else Result:=[];
end;

// GetDeprecatedMessage
//
function TdwsAbstractUnit.GetDeprecatedMessage : UnicodeString;
begin
   Result:=FDeprecatedMessage;
end;

{ TdwsEmptyUnit }

constructor TdwsEmptyUnit.Create(AOwner: TComponent);
begin
  inherited;
  FDependencies := TUnicodeStringList.Create;
end;

destructor TdwsEmptyUnit.Destroy;
begin
  inherited;
  FDependencies.Free;
end;

function TdwsEmptyUnit.GetDependencies: TUnicodeStringList;
begin
  Result := FDependencies;
end;

function TdwsEmptyUnit.GetUnitName: UnicodeString;
begin
  Result := FUnitName;
end;

function TdwsEmptyUnit.GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                    operators : TOperators; rootTable : TSymbolTable) : TUnitSymbolTable;
var
   x : Integer;
   sym : TUnitMainSymbol;
begin
   Result:=TUnitSymbolTable.Create(systemTable);
   try
      // insert links to units this unit depends of
      for x:=0 to FDependencies.Count-1 do begin
         sym:=unitSyms.Find(FDependencies[x]);
         sym.ReferenceInSymbolTable(Result, False);
      end;

      // create the symbols of this unit
      AddUnitSymbols(Result);

   except
      Result.Free;
      raise;
   end;
end;

// GetUnitFlags
//
function TdwsEmptyUnit.GetUnitFlags : TIdwsUnitFlags;
begin
   Result:=[ufImplicitUse];
end;

// GetDeprecatedMessage
//
function TdwsEmptyUnit.GetDeprecatedMessage : UnicodeString;
begin
   Result:='';
end;

// BeforeAdditionTo
//
procedure TdwsEmptyUnit.BeforeAdditionTo(dwscript : TObject);
begin
   // nothing
end;

{ TdwsEmptyUnit }

destructor TdwsUnitComponent.Destroy;
begin
  Script := nil;
  inherited;
end;

procedure TdwsUnitComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FScript) then
    FScript := nil;
end;

procedure TdwsUnitComponent.SetScript(const Value: TDelphiWebScript);
begin
  if Assigned(FScript) then
  begin
    FScript.RemoveUnit(Self);
    FScript.RemoveFreeNotification(Self);
  end;

  FScript := Value;

  if Assigned(FScript) then
  begin
    FScript.AddUnit(Self);
    FScript.FreeNotification(Self);
  end;
end;

{ TdwsEnumeration }

procedure TdwsEnumeration.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsEnumeration then
    FElements.Assign(TdwsEnumeration(Source).Elements);
end;

constructor TdwsEnumeration.Create(Collection: TCollection);
begin
  inherited;
  FElements := TdwsElements.Create(Self);
  FStyle:=enumClassic;
end;

destructor TdwsEnumeration.Destroy;
begin
  FElements.Free;
  inherited;
end;

// DoGenerate
//
function TdwsEnumeration.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
var
   i : Integer;
   enumSymbol : TEnumerationSymbol;
   element : TElementSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   enumSymbol:=TEnumerationSymbol.Create(Name, systemTable.TypInteger, Style);
   try
      for i:=0 to FElements.Count-1 do begin
         element:=(Elements.Items[i] as TdwsElement).Generate(systemTable, table, enumSymbol) as TElementSymbol;
         enumSymbol.AddElement(element);
         if Style=enumClassic then begin
            Table.AddSymbol(element);
            element.IncRefCount;
         end;
      end;
   except
      enumSymbol.Free;
      raise;
   end;
   Table.AddSymbol(enumSymbol);
   Result:=enumSymbol;
end;

// GetDisplayName
//
function TdwsEnumeration.GetDisplayName: String;
var
   i : Integer;
begin
   Result:=Name+' =';
   case Style of
      enumScoped : Result:=Result+' enum';
      enumFlags : Result:=Result+' flags';
   end;
   Result:=Result+' (';
   for i:=0 to FElements.Count-1 do begin
      if i<>0 then
         Result:=Result + ', ';
      Result:=Result+TdwsElement(FElements.Items[i]).GetDisplayName;
   end;
   Result:=Result+');';
end;

// Parse
//
function TdwsEnumeration.Parse(const Value : UnicodeString): UnicodeString;
var
   element: TdwsElement;
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   tokenType: TTokenType;
   sourceFile : TSourceFile;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);

      // check whether tokens are available at all
      if not tok.HasTokens then
         raise Exception.Create('Token expected');

      tokenType := tok.GetToken.FTyp;

      // check for name
      if not (tok.TestName or (tokenType <> ttNone)) then
         raise Exception.Create('Name expected');

      // get name and kill token
      Result := tok.GetToken.AsString;
      tok.KillToken;

      // kill token and eventually ignore additional procedure / function
      if tokenType <> ttNone then begin
         // check if further tokens are available, if not accept name
         if not tok.HasTokens then begin
            Result := Value;
            Exit;
         end;
      end;

      // check for elements
      if tok.TestDelete(ttEQ) then
      begin
         FElements.Clear;
         if tok.TestDelete(ttBLEFT) then
         repeat

            if tok.TestName then begin
               element := FElements.Add;
               element.Name := tok.GetToken.AsString;
               tok.KillToken;
            end else raise Exception.Create('Element name expected');

         until not tok.TestDelete(ttCOMMA);
         if not tok.TestDelete(ttBRIGHT) then
            raise Exception.Create('")" expected');
      end;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

{ TdwsElement }

function TdwsElement.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
var
   enumInt: Integer;
   enumSym: TEnumerationSymbol;
begin
   FIsGenerating := True;
   enumSym := TEnumerationSymbol(ParentSym);

   CheckName(enumSym.Elements, Name);

   if FIsUserDef then
      enumInt := FUserDefValue
   else if enumSym.Style=enumFlags then begin
      if enumSym.Elements.Count > 0 then
         enumInt := TElementSymbol(enumSym.Elements[enumSym.Elements.Count - 1]).Value * 2
      else enumInt := 1;
   end else begin
      if enumSym.Elements.Count > 0 then
         enumInt := TElementSymbol(enumSym.Elements[enumSym.Elements.Count - 1]).Value + 1
      else enumInt := 0;
   end;

   Result := TElementSymbol.Create(Name, enumSym, enumInt, FIsUserDef);
   TElementSymbol(Result).DeprecatedMessage := FDeprecated;
end;

function TdwsElement.GetDisplayName: String;
begin
  if FIsUserDef then
    Result := Name + ' = ' + IntToStr(FUserDefValue)
  else
    Result := Name;
end;

procedure TdwsElement.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsElement then begin
    FIsUserDef := TdwsElement(Source).IsUserDef;
    FUserDefValue := TdwsElement(Source).UserDefValue;
  end;
end;

procedure TdwsElement.SetIsUserDef(const Value: Boolean);
begin
  FIsUserDef := Value;
  if not Value then
    FUserDefValue := 0;
end;

procedure TdwsElement.SetUserDefValue(const Value: Integer);
begin
  FIsUserDef := True;
  FUserDefValue := Value;
end;

// ------------------
// ------------------ TdwsCustomInstance ------------------
// ------------------

procedure TdwsCustomInstance.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsCustomInstance then
  begin
    FOnInstantiate := TdwsCustomInstance(Source).OnInstantiate;
    FOnObjectDestroy := TdwsCustomInstance(Source).OnObjectDestroy;
    FAutoDestroyExternalObject := TdwsCustomInstance(Source).AutoDestroyExternalObject;
  end;
end;

constructor TdwsCustomInstance.Create(Collection: TCollection);
begin
  inherited;
  FAutoDestroyExternalObject := False;
end;

procedure TdwsCustomInstance.DoDestroy(ExternalObject: TObject);
begin
  if Assigned(FOnObjectDestroy) then
    FOnObjectDestroy(ExternalObject);
  if FAutoDestroyExternalObject then
    ExternalObject.Free;
end;

function TdwsCustomInstance.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
var
  typSym: TTypeSymbol;
  instFunc: TInstantiateFunc;
  funcSym: TFuncSymbol;
  dataSym : TDataSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   if Table.AddrGenerator=nil then
      raise Exception.Create(UNT_InstancesNotSupportedInStaticUnits);

  // Get the type symbol of this variable
  typSym := GetDataType(systemTable, Table, DataType);

  if typSym is TClassSymbol then
  begin
    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    dataSym := TDataSymbol.Create('', typSym);
    Table.AddSymbol(dataSym);

    instFunc := TInstantiateFunc.Create(funcSym);
    instFunc.DataSym := dataSym;
    instFunc.FClassSym := TClassSymbol(typSym);
    instFunc.OnInstantiate := DoInstantiate;
    instFunc.OnObjectDestroy := DoDestroy;
    instFunc.OnInitSymbol := DoInitSymbol;
    instFunc.OnInitExpr := DoInitExpr;
    funcSym.Executable := ICallable(instFunc);

    Result := TExternalVarSymbol.Create(Name, typSym);
    TExternalVarSymbol(Result).ReadFunc := funcSym;
  end
  else
    raise Exception.CreateFmt(UNT_AutoInstantiateWithoutClass, [DataType]);

  GetUnit.Table.AddSymbol(Result);
end;

procedure TdwsCustomInstance.DoInitSymbol(Sender: TObject; Symbol: TSymbol);
begin
  if Assigned(FOnInitSymbol) then
    FOnInitSymbol(Self,Symbol)
end;

procedure TdwsCustomInstance.DoInitExpr(Sender: TObject; Expr: TExprBase);
begin
  if Assigned(FOnInitExpr) then
    FOnInitExpr(Self,Expr)
end;

procedure TdwsCustomInstance.DoInstantiate(info : TProgramInfo; var ExternalObject: TObject);
begin
  if Assigned(FOnInstantiate) then
    FOnInstantiate(info, ExternalObject);
end;

{ TdwsFunctions }

class function TdwsFunctions.GetSymbolClass: TdwsSymbolClass;
begin
   Result := TdwsFunction;
end;

// Add
//
function TdwsFunctions.Add : TdwsFunction;
begin
   Result:=TdwsFunction(inherited Add);
end;

// Add
//
function TdwsFunctions.Add(const name : UnicodeString; const resultType : UnicodeString = '') : TdwsFunction;
begin
   Result:=Add;
   Result.Name:=name;
   Result.ResultType:=resultType;
end;

{ TdwsForwards }

class function TdwsForwards.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsForward;
end;

// Add
//
function TdwsForwards.Add : TdwsForward;
begin
   Result:=TdwsForward(inherited Add);
end;

{ TdwsEnumerations }

class function TdwsEnumerations.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsEnumeration;
end;

// Add
//
function TdwsEnumerations.Add : TdwsEnumeration;
begin
   Result:=TdwsEnumeration(inherited Add);
end;

// ------------------
// ------------------ TdwsSet ------------------
// ------------------

// DoGenerate
//
function TdwsSet.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
var
   base: TTypeSymbol;
   eBase: TEnumerationSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   base := GetDataType(systemTable, Table, FBaseType);
   if not (base is TEnumerationSymbol) then
      raise Exception.CreateFmt('Type "%s" is not an enumeration', [FBaseType]);
   eBase := TEnumerationSymbol(base);
   result := TSetOfSymbol.Create(self.Name, eBase, 0, eBase.HighBound);
   Table.AddSymbol(result);
end;

// GetDisplayName
//
function TdwsSet.GetDisplayName: String;
begin
   Result:=Name+' = set of '+BaseType;
end;

function TdwsSet.Parse(const Value : UnicodeString): UnicodeString;
var
   rules : TPascalTokenizerStateRules;
   tok : TTokenizer;
   tokenType: TTokenType;
   sourceFile : TSourceFile;
begin
   rules := TPascalTokenizerStateRules.Create;
   tok := TTokenizer.Create(rules, nil);
   sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := Value;
      tok.BeginSourceFile(sourceFile);

      // check whether tokens are available at all
      if not tok.HasTokens then
         raise Exception.Create('Token expected');

      tokenType := tok.GetToken.FTyp;

      // check for name
      if not (tok.TestName or (tokenType <> ttNone)) then
         raise Exception.Create('Name expected');

      // get name and kill token
      Result := tok.GetToken.AsString;
      tok.KillToken;

      // kill token and eventually ignore additional procedure / function
      if tokenType <> ttNone then begin
         // check if further tokens are available, if not accept name
         if not tok.HasTokens then begin
            Result := Value;
            Exit;
         end;
      end;

      // check for base type
      if tok.TestDelete(ttEQ) then
      begin
         if tok.TestDelete(ttSET) and tok.TestDelete(ttOF) then
         begin
            if tok.TestName then
               FBaseType := tok.GetToken.AsString
            else raise Exception.Create('Base type name expected');
         end
         else raise Exception.Create('"set of" expected');
      end;
   finally
      sourceFile.Free;
      tok.Free;
      rules.Free;
   end;
end;

// ------------------
// ------------------ TdwsSets ------------------
// ------------------

// GetSymbolClass
//
class function TdwsSets.GetSymbolClass: TdwsSymbolClass;
begin
   result := TdwsSet;
end;

function TdwsSets.Add: TdwsSet;
begin
   result := inherited Add as TdwsSet;
end;

// ------------------
// ------------------ TdwsConstants ------------------
// ------------------

// GetSymbolClass
//
class function TdwsConstants.GetSymbolClass: TdwsSymbolClass;
begin
   Result := TdwsConstant;
end;

// GetValues
//
function TdwsConstants.GetValues(const name : UnicodeString) : Variant;
var
   i : Integer;
begin
   // returns Unassigned if not present
   i:=IndexOf(name);
   if i>=0 then
      Result:=TdwsConstant(Items[i]).Value;
end;

// SetValues
//
procedure TdwsConstants.SetValues(const name : UnicodeString; const v : Variant);
var
   i : Integer;
   c : TdwsConstant;
begin
   i:=IndexOf(name);
   if i<0 then begin
      c:=Add;
      c.Name:=name;
      c.DataType:=VariantTypeToDataType(v);
   end else c:=TdwsConstant(Items[i]);
   c.Value:=v;
end;

// Add
//
function TdwsConstants.Add : TdwsConstant;
begin
   Result:=TdwsConstant(inherited Add);
end;

// ------------------
// ------------------ TdwsClasses ------------------
// ------------------

// GetSymbolClass
//
class function TdwsClasses.GetSymbolClass: TdwsSymbolClass;
begin
   Result:=TdwsClass;
end;

// Add
//
function TdwsClasses.Add : TdwsClass;
begin
   Result:=TdwsClass(inherited Add);
end;

// ------------------
// ------------------ TdwsArrays ------------------
// ------------------

class function TdwsArrays.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsArray;
end;

function TdwsArrays.Add : TdwsArray;
begin
  Result := TdwsArray(inherited Add);
end;

// ------------------
// ------------------ TdwsRecords ------------------
// ------------------

class function TdwsRecords.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsRecord;
end;

// Add
//
function TdwsRecords.Add : TdwsRecord;
begin
   Result:=TdwsRecord(inherited Add);
end;

// ------------------
// ------------------ TdwsParameters ------------------
// ------------------

class function TdwsParameters.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsParameter;
end;

// Add
//
function TdwsParameters.Add : TdwsParameter;
begin
   Result:=TdwsParameter(inherited Add);
end;

// Add
//
function TdwsParameters.Add(const name, typeName : UnicodeString) : TdwsParameter;
begin
   Result:=Add;
   Result.Name:=name;
   Result.DataType:=typeName;
end;

{ TdwsInstances }

class function TdwsInstances.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsInstance;
end;

// Add
//
function TdwsInstances.Add : TdwsInstance;
begin
   Result:=TdwsInstance(inherited Add);
end;

{ TdwsFields }

class function TdwsFields.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsField;
end;

// Add
//
function TdwsFields.Add : TdwsField;
begin
   Result:=TdwsField(inherited Add);
end;

{ TdwsConstructors }

class function TdwsConstructors.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsConstructor;
end;

// Add
//
function TdwsConstructors.Add : TdwsConstructor;
begin
   Result:=TdwsConstructor(inherited Add);
end;

{ TdwsMethods }

class function TdwsMethods.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsMethod;
end;

// Add
//
function TdwsMethods.Add : TdwsMethod;
begin
   Result:=TdwsMethod(inherited Add);
end;

// Add
//
function TdwsMethods.Add(const name : UnicodeString; const resultType : UnicodeString = '') : TdwsMethod;
begin
   Result:=Add;
   Result.Name:=name;
   Result.ResultType:=resultType;
end;

{ TdwsProperties }

class function TdwsProperties.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsProperty;
end;

// Add
//
function TdwsProperties.Add : TdwsProperty;
begin
   Result:=TdwsProperty(inherited Add);
end;

{ TdwsClassOperators }

class function TdwsClassOperators.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsClassOperator;
end;

{ TdwsMembers }

class function TdwsMembers.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsMember;
end;

// Add
//
function TdwsMembers.Add : TdwsMember;
begin
   Result:=TdwsMember(inherited Add);
end;

{ TdwsElements }

class function TdwsElements.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsElement;
end;

// Add
//
function TdwsElements.Add : TdwsElement;
begin
   Result:=TdwsElement(inherited Add);
end;

{ TdwsSynonyms }

class function TdwsSynonyms.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsSynonym;
end;

// Add
//
function TdwsSynonyms.Add : TdwsSynonym;
begin
   Result := TdwsSynonym(inherited Add);
end;

{ TdwsSynonym }

function TdwsSynonym.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  Result := TAliasSymbol.Create(Name, GetDataType(systemTable, Table, DataType));
  GetUnit.Table.AddSymbol(Result);
end;

{ TdwsAbstractStaticUnit }

constructor TdwsAbstractStaticUnit.Create(AOwner: TComponent);
begin
  inherited;
  FStaticTable := nil;
  FStaticSymbols := False;
end;

function TdwsAbstractStaticUnit.CreateUnitTable(parent, rootTable: TSymbolTable; tableType: TSymbolTableType): TUnitSymbolTable;
begin
   case tableType of
      sttLinked :
         Result := TLinkedSymbolTable.Create(Parent as TStaticSymbolTable);
      sttStatic :
         Result := TStaticSymbolTable.Create(Parent as TStaticSymbolTable);
   else
      if rootTable=nil then
         Result := TUnitSymbolTable.Create(Parent)
      else Result := TUnitSymbolTable.Create(Parent, rootTable.AddrGenerator);
  end;
end;

procedure TdwsAbstractStaticUnit.BeforeDestruction;
begin
  ReleaseStaticSymbols;
  inherited;
end;

destructor TdwsAbstractStaticUnit.Destroy;
begin
  inherited;
end;

function TdwsAbstractStaticUnit.GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                             operators : TOperators; rootTable : TSymbolTable) : TUnitSymbolTable;
begin
   if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms, operators) then
      Result := CreateUnitTable(FStaticTable.SymbolTable, nil, sttLinked) as TLinkedSymbolTable // typecheck
   else begin
      Result := CreateUnitTable(SystemTable, rootTable, sttDefault);
      try
         InitUnitTable(SystemTable, UnitSyms, operators, Result);
      except
         Result.Free;
         raise;
      end;
   end;
end;

function TdwsAbstractStaticUnit.InitStaticSymbols(systemTable : TSystemSymbolTable;
               unitSyms : TUnitMainSymbols; operators : TOperators): Boolean;
var
  staticParent: TStaticSymbolTable;
begin
  if not Assigned(FStaticTable) then
  begin
    staticParent := (SystemTable as TStaticSymbolTable);

    if Assigned(staticParent) then
    begin
      FStaticTable := CreateUnitTable(staticParent, nil, sttStatic) as TStaticSymbolTable;
      try
        InitUnitTable(SystemTable, UnitSyms, operators, FStaticTable.SymbolTable);
      except
        ReleaseStaticSymbols;
        raise;
      end;
    end;
  end; // else check FSymbolTable = StaticTable
  Result := Assigned(FStaticTable);
end;

procedure TdwsAbstractStaticUnit.InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                    operators : TOperators; unitTable: TUnitSymbolTable);
var
   x : Integer;
   sym : TUnitMainSymbol;
begin
   if UnitName = '' then
      raise Exception.CreateFmt(UNT_UnitNameNotDefined, [Name]);

   for x := 0 to FDependencies.Count - 1 do begin
      sym := unitSyms.Find(FDependencies[x]);
      sym.ReferenceInSymbolTable(unitTable, False);
   end;

   AddUnitSymbols(systemTable, unitTable, operators);
end;

// ReleaseStaticSymbols
//
procedure TdwsAbstractStaticUnit.ReleaseStaticSymbols;
begin
   FStaticTable := nil;
end;

procedure TdwsAbstractStaticUnit.SetStaticSymbols(const Value: Boolean);
begin
  FStaticSymbols := Value;
  if not FStaticSymbols then
    ReleaseStaticSymbols;
end;

// ------------------
// ------------------ TdwsCustomLangageExtension ------------------
// ------------------

// Create
//
constructor TdwsCustomLangageExtension.Create(AOwner: TComponent);
begin
   inherited;
   FExtension:=CreateExtension;
end;

// Destroy
//
destructor TdwsCustomLangageExtension.Destroy;
begin
   inherited;
   Script:=nil;
   FExtension.Free;
end;

// SetScript
//
procedure TdwsCustomLangageExtension.SetScript(const val : TDelphiWebScript);
begin
   if FScript<>nil then begin
      FScript.FExtensions.Remove(FExtension);
      FScript.RemoveFreeNotification(Self);
   end;

   FScript:=val;

   if FScript<>nil then begin
      FScript.FExtensions.Add(FExtension);
      FScript.FreeNotification(Self);
   end;
end;

// Notification
//
procedure TdwsCustomLangageExtension.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if (Operation=opRemove) and (AComponent=FScript) then
      Script:=nil;
end;

// ------------------
// ------------------ TdwsTypeSymbol ------------------
// ------------------

// GetDisplayName
//
function TdwsTypeSymbol.GetDisplayName: String;
begin
   Result:=FName;
end;

// Assign
//
procedure TdwsTypeSymbol.Assign(Source: TPersistent);
begin
   if Source is TdwsTypeSymbol then begin
      FName := TdwsTypeSymbol(Source).FName;
   end else inherited;
end;

// ------------------
// ------------------ TdwsTypeSymbols ------------------
// ------------------

// Create
//
constructor TdwsTypeSymbols.Create(AOwner: TPersistent);
begin
   inherited Create(AOwner, TdwsTypeSymbol);
end;

// Add
//
function TdwsTypeSymbols.Add : TdwsTypeSymbol;
begin
   Result:=TdwsTypeSymbol(inherited Add);
end;

// ------------------
// ------------------ TdwsOperator ------------------
// ------------------

// Create
//
constructor TdwsOperator.Create(Collection: TCollection);
begin
   inherited;
   FParams:=TdwsTypeSymbols.Create(Self);
end;

// Destroy
//
destructor TdwsOperator.Destroy;
begin
   FParams.Free;
   inherited;
end;

// GetDisplayName
//
function TdwsOperator.GetDisplayName: String;
var
   i : Integer;
begin
   Result:='operator '+cTokenStrings[Operator]+' (';
   for i:=0 to Params.Count-1 do begin
      if i>0 then
         Result:=Result+', ';
      Result:=Result+Params.Items[i].DisplayName;
   end;
   Result:=Result+') : '+ResultType+' uses '+UsesAccess;
end;

// Assign
//
procedure TdwsOperator.Assign(Source: TPersistent);
begin
   if Source is TdwsOperator then begin
      FOperator:=TdwsOperator(Source).FOperator;
      FResultType:=TdwsOperator(Source).FResultType;
      FUsesAccess:=TdwsOperator(Source).FUsesAccess;
      FParams.Assign(TdwsOperator(Source).FParams);
   end else inherited;
end;

// DoGenerate
//
function TdwsOperator.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;

   procedure RaiseError;
   begin
      raise EGenerationError.CreateFmt(UNT_IncorrectOperatorOverload, [DisplayName]);
   end;

var
   op : TOperatorSymbol;
   i : Integer;
   typ : TTypeSymbol;
   sym : TSymbol;
begin
   op:=TOperatorSymbol.Create(Operator);
   try
      if Params.Count<>2 then
         RaiseError;
      for i:=0 to Params.Count-1 do begin
         typ:=Table.FindTypeSymbol(Params.Items[i].DisplayName, cvMagic);
         if (typ=nil) or not typ.IsType then
            RaiseError;
         op.AddParam(typ);
      end;
      typ:=Table.FindTypeSymbol(ResultType, cvMagic);
      if (typ=nil) or not typ.IsType then
         RaiseError;
      op.Typ:=typ;
      sym:=Table.FindTypeSymbol(UsesAccess, cvMagic);
      if (sym=nil) or sym.IsType or (sym.AsFuncSymbol=nil) then
         raise Exception.CreateFmt(UNT_UsesAccessNotFound, [UsesAccess]);
      op.UsesSym:=sym.AsFuncSymbol;
      Table.AddSymbol(op);
   except
      op.Free;
      raise;
   end;
   Result:=op;
end;

// ------------------
// ------------------ TdwsOperators ------------------
// ------------------

// GetSymbolClass
//
class function TdwsOperators.GetSymbolClass : TdwsSymbolClass;
begin
   Result:=TdwsOperator;
end;

// Add
//
function TdwsOperators.Add : TdwsOperator;
begin
   Result:=TdwsOperator(inherited Add);
end;

// ------------------
// ------------------ TEventBasedLocalizer ------------------
// ------------------

// LocalizeSymbol
//
procedure TEventBasedLocalizer.LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString);
begin
   if Assigned(FOnLocalizeSymbol) then
      FOnLocalizeSymbol(Sender, aResSymbol, Result)
   else LocalizeString(aResSymbol.Value, Result);
end;

// LocalizeString
//
procedure TEventBasedLocalizer.LocalizeString(const aString : UnicodeString; var Result : UnicodeString);
begin
   if Assigned(FOnLocalizeString) then
      FOnLocalizeString(Sender, aString, Result)
   else Result:=aString;
end;

// ------------------
// ------------------ TdwsCustomLocalizer ------------------
// ------------------

// GetLocalizer
//
function TdwsCustomLocalizer.GetLocalizer : IdwsLocalizer;
var
   loc : TEventBasedLocalizer;
begin
   loc:=TEventBasedLocalizer.Create;
   loc.Sender:=Self;
   loc.OnLocalizeSymbol:=OnLocalizeSymbol;
   loc.OnLocalizeString:=OnLocalizeString;
   Result:=loc;
   if Assigned(FOnGetLocalizer) then
      FOnGetLocalizer(Self, Result);
end;

// -----------------------------------------------------------------------------
//
//          TdwsDelegate
//
// -----------------------------------------------------------------------------

procedure TdwsDelegate.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TdwsDelegate) then
  begin
    FResultType := TdwsDelegate(Source).ResultType;
    FParameters.Assign(TdwsDelegate(Source).Parameters);
    FDeprecated := TdwsDelegate(Source).Deprecated;
  end;
end;

// -----------------------------------------------------------------------------

constructor TdwsDelegate.Create(Collection: TCollection);
begin
  inherited;
  FParameters := TdwsParameters.Create(Self);
end;

// -----------------------------------------------------------------------------

destructor TdwsDelegate.Destroy;
begin
  FParameters.Free;
  inherited;
end;

// DoGenerate
//
function TdwsDelegate.DoGenerate(systemTable : TSystemSymbolTable; Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
var
   funcSym : TFuncSymbol;
   funcKind : TFuncKind;
   params : TParamArray;
   typ : TTypeSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   if ResultType <> '' then begin
      typ := GetDataType(systemTable, Table, ResultType);
      funcKind := fkFunction;
   end else begin
      typ := nil;
      funcKind := fkProcedure;
   end;

   funcSym := TFuncSymbol.Create('', funcKind, -1);
   try
      funcSym.Typ := typ;

      params := GetParameters(systemTable, Table);
      funcSym.GenerateParams(Table, params);
      funcSym.params.AddParent(Table);

      funcSym.SetName(Name);
      funcSym.SetIsType;
      funcSym.DeprecatedMessage := Deprecated;
      GetUnit.Table.AddSymbol(funcSym);
   except
      funcSym.Free;
      raise;
   end;
   Result := funcSym;
end;

// -----------------------------------------------------------------------------

function TdwsDelegate.GetDisplayName: String;
var
  Params: string;
begin
  Params := Parameters.GetDisplayName;
  if (Params <> '') then
    Params := '(' + Params + ')';

  if (ResultType = '') then
    Result := Format('%s = procedure%s;', [Name, Params])
  else
    Result := Format('%s = function%s: %s;', [Name, Params, ResultType]);
  if (Deprecated <> '') then
    Result := Result + ' deprecated;';
end;

// -----------------------------------------------------------------------------

function TdwsDelegate.GetParameters(systemTable : TSystemSymbolTable; Table: TSymbolTable): TParamArray;
begin
  Result := dwsComp.GetParameters(Self, Parameters, systemTable, Table);
end;

// -----------------------------------------------------------------------------

function TdwsDelegate.Parse(const Value: UnicodeString): UnicodeString;
var
  param : TdwsParameter;
  params : array of TdwsParameter;
  rules : TPascalTokenizerStateRules;
  tok : TTokenizer;
  tokenType: TTokenType;
  sourceFile : TSourceFile;
begin
  (*
  ** identifier ( parameters ) [: returntype]
  *)
  rules := TPascalTokenizerStateRules.Create;
  tok := TTokenizer.Create(rules, nil);
  sourceFile := TSourceFile.Create;
  try
    sourceFile.Code := Value;
    tok.BeginSourceFile(sourceFile);

    // check whether tokens are available at all
    if not tok.HasTokens then
      raise Exception.Create('Token expected');

    tokenType := tok.GetToken.FTyp;

    // check for name
    if not (tok.TestName or (tokenType <> ttNone)) then
      raise Exception.Create('Name expected');

    // get name and kill token
    Result := tok.GetToken.AsString;
    tok.KillToken;

    // kill token and eventually ignore additional procedure / function
    if tokenType <> ttNone then
    begin
      // check if further tokens are available, if not accept name
      if not tok.HasTokens then begin
        Result := Value;
        Exit;
      end;
    end;

    // check for parameters
    if tok.TestDelete(ttBLEFT) then
    begin
      while not tok.TestDelete(ttBRIGHT) do
      begin
        param := Parameters.Add;

        case tok.TestDeleteAny([ttVAR, ttCONST, ttLAZY]) of
          ttVAR:
            begin
              param.IsVarParam := True;
              param.IsWritable := True;
            end;
          ttCONST:
            begin
              param.IsVarParam := True;
              param.IsWritable := False;
            end;
          ttLAZY:
            begin
              param.IsLazy := True;
              param.IsWritable := False;
            end;
        end;

        if tok.TestName then
        begin
          param.Name := tok.GetToken.AsString;
          tok.KillToken;
        end else
          raise Exception.Create('Parameter name expected');

        SetLength(params, 1);
        Params[0] := param;

        while tok.TestDelete(ttCOMMA) do
        begin
          SetLength(params, length(params) + 1);
          param := Parameters.Add;
          param.Assign(params[0]);
          if tok.TestName then
          begin
            param.Name := tok.GetToken.AsString;
            tok.KillToken;
            params[high(params)] := param;
          end else
            raise Exception.Create('Parameter name expected');
        end;

        if not tok.TestDelete(ttCOLON) then
          raise Exception.Create('Colon expected');

        if tok.TestName then
        begin
          for param in params do
            param.DataType := tok.GetToken.AsString;
          tok.KillToken;
        end else
          raise Exception.Create('Data type expected');


        // eventually head over to next parameter
        if tok.TestDelete(ttSEMI) then
         Continue;
      end;
    end;

    // check for return type
    if tok.TestDelete(ttCOLON) then
    begin
      if tok.TestName then
      begin
        ResultType := tok.GetToken.AsString;
        tok.KillToken;
      end;
    end;

    tok.EndSourceFile;
  finally
    sourceFile.Free;
    tok.Free;
    rules.Free;
  end;
end;

// -----------------------------------------------------------------------------

procedure TdwsDelegate.SetParameters(const Value: TdwsParameters);
begin
  FParameters.Assign(Value);
end;

// -----------------------------------------------------------------------------

procedure TdwsDelegate.SetResultType(const Value: TDataType);
begin
  FResultType := Value;
end;

// -----------------------------------------------------------------------------

function TdwsDelegate.StoreParameters: Boolean;
begin
  Result := (FParameters.Count > 0);
end;

// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//
//          TdwsDelegates
//
// -----------------------------------------------------------------------------
function TdwsDelegates.Add: TdwsDelegate;
begin
  Result := TdwsDelegate(inherited Add);
end;

// -----------------------------------------------------------------------------

function TdwsDelegates.Add(const Name, ResultType: UnicodeString): TdwsDelegate;
begin
  Result := Add;
  Result.Name := Name;
  Result.ResultType := ResultType;
end;

// -----------------------------------------------------------------------------

class function TdwsDelegates.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsDelegate;
end;

// ------------------
// ------------------ TCustomInstantiateFunc ------------------
// ------------------

// Specialize
//
function TCustomInstantiateFunc.Specialize(const context : ISpecializationContext) : IExecutable;
begin
   context.AddCompilerError('TCustomInstantiateFunc cannot be specialized yet');
end;

end.
