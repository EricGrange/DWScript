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
  Variants, Classes, SysUtils, TypInfo, dwsCompiler, dwsExprs, dwsSymbols,
  dwsStack, dwsFunctions, dwsStrings, dwsLanguageExtension,
  dwsTokenizer, dwsUtils, dwsOperators, dwsUnitSymbols, dwsXPlatform,
  // Built-In functions
{$IFNDEF DWS_NO_BUILTIN_FUNCTIONS}
  dwsMathFunctions, dwsStringFunctions, dwsTimeFunctions, dwsVariantFunctions,
{$ENDIF}
  dwsErrors;

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
    function GetUnitName: String;
    function GetDependencies: TStrings;
      function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                            operators : TOperators) : TUnitSymbolTable;
    function GetUnitFlags : TIdwsUnitFlags;
  protected
    FUnitName: String;
    FDependencies: TStrings;
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
         FCompiler : TdwsCompiler;
         FConfig : TdwsConfiguration;
         FExtensions : TdwsLanguageExtensionAggregator;
         FLock : TFixedCriticalSection;

      protected
         function GetOnInclude: TIncludeEvent;
         procedure SetOnInclude(const Value: TIncludeEvent);
         function GetVersion: String;
         procedure SetVersion(const Value: String);
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

         procedure AddUnit(const Un: IdwsUnit);
         function RemoveUnit(const Un: IdwsUnit): Boolean;

         function Compile(const Text: String): IdwsProgram; virtual;
         procedure RecompileInContext(const prog : IdwsProgram; const text : String); virtual;

         procedure Lock;
         procedure UnLock;

         property Extensions : TdwsLanguageExtensionAggregator read FExtensions;

      published
         property Config : TdwsConfiguration read FConfig write SetConfig stored True;
         property OnNeedUnit : TdwsOnNeedUnitEvent read GetOnNeedUnit write SetOnNeedUnit stored False;
         property OnInclude : TIncludeEvent read GetOnInclude write SetOnInclude stored False;
         property OnResource : TdwsResourceEvent read GetOnResource write SetOnResource;
         property Version : String read GetVersion write SetVersion stored False;
   end;

   TLocalizeSymbolEvent = procedure (Sender : TObject; aResSymbol : TResourceStringSymbol; var result : String) of object;
   TLocalizeStringEvent = procedure (Sender : TObject; const aString : String; var result : String) of object;
   TGetLocalizerEvent = procedure (Sender : TObject; var localizer : IdwsLocalizer) of object;

   TEventBasedLocalizer = class (TInterfacedSelfObject, IdwsLocalizer)
      private
         FSender : TObject;
         FOnLocalizeSymbol : TLocalizeSymbolEvent;
         FOnLocalizeString : TLocalizeStringEvent;

      public
         procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : String);
         procedure LocalizeString(const aString : String; var Result : String);

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

  TdwsAbstractUnit = class(TComponent, IUnknown, IdwsUnit)
  private
    FDependencies: TStrings;
    FScript: TDelphiWebScript;
    FUnitName: String;
    function GetDependencies: TStrings;
    procedure SetDependencies(const Value: TStrings);
    procedure SetScript(const Value: TDelphiWebScript);
    procedure SetUnitName(const Value: String);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetUnitName: String; virtual;
      function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                            operators : TOperators) : TUnitSymbolTable; virtual; abstract;
    function GetUnitFlags : TIdwsUnitFlags;

    property Dependencies: TStrings read FDependencies write SetDependencies;
    {$WARNINGS OFF}
    property UnitName: String read GetUnitName write SetUnitName;
    {$WARNINGS ON}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Script: TDelphiWebScript read FScript write SetScript;
  end;

   TSymbolTableType = (sttDefault, sttStatic, sttLinked);

   TdwsAbstractStaticUnit = class(TdwsAbstractUnit)
      private
         FStaticSymbols : Boolean;
         FStaticTable : IStaticSymbolTable;

      protected
         function GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                               operators : TOperators) : TUnitSymbolTable; override;
         function CreateUnitTable(Parent: TSymbolTable; Typ: TSymbolTableType = sttDefault): TUnitSymbolTable; virtual;
         procedure SetStaticSymbols(const Value: Boolean); // static symbols
         procedure InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                 operators : TOperators; UnitTable: TUnitSymbolTable); virtual;
         procedure AddUnitSymbols(table : TSymbolTable; operators : TOperators); virtual; abstract;
         property StaticSymbols : Boolean read FStaticSymbols write SetStaticSymbols;
         property StaticTable : IStaticSymbolTable read FStaticTable;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure BeforeDestruction; override;
         function InitStaticSymbols(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols; operators : TOperators): Boolean;
         procedure ReleaseStaticSymbols;
   end;

   TDataType = String;
   TdwsUnit = class;
   TdwsGlobal = class;

  TdwsSymbol = class(TCollectionItem)
  private
    FIsGenerating: Boolean;
    FUnit: TdwsUnit;
    FName: String;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CheckName(Table: TSymbolTable; Name: String);
    function GetDataType(Table: TSymbolTable; Name: String): TTypeSymbol;
    procedure Reset;
    property IsGenerating: Boolean read FIsGenerating;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function Generate(table: TSymbolTable; parentSym: TSymbol = nil): TSymbol;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; virtual; abstract;
    function GetNamePath: String; override;
    function GetUnit: TdwsUnit;
  published
    property Name: String read FName write FName;
  end;

  TdwsSymbolArray = array of TdwsSymbol;

  TdwsSymbolClass = class of TdwsSymbol;

  TdwsCollection = class(TOwnedCollection)
  private
    FUnit: TdwsUnit;
    FSortedSymbols: TdwsSymbolArray;
    function GetSortedItem(Index: Integer): TdwsSymbol;
  protected
    class function GetSymbolClass : TdwsSymbolClass; virtual; abstract;
    function GetSymbols(const Name: String): TdwsSymbol;
    function GetItem(Index: Integer): TdwsSymbol;
    procedure SetItem(Index: Integer; Value: TdwsSymbol);
    procedure Reset;
  public
    constructor Create(AOwner: TPersistent);
    function GetOwner: TPersistent; override;
    function GetUnit: TdwsUnit;
    function GetSortedItems: TdwsSymbolArray;
    function IndexOf(const Name: String): Integer;
    property Symbols[const Name: String]: TdwsSymbol read GetSymbols;
    property Items[Index: Integer]: TdwsSymbol read GetItem write SetItem;
    property SortedItems[Index: Integer]: TdwsSymbol read GetSortedItem;
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
         function Add(const name, typName : String) : TdwsGlobal; overload;
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
      public
         constructor Create(Collection: TCollection); override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
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
         function Add : TdwsParameter;
   end;

   TFuncEvalEvent = procedure(info : TProgramInfo) of object;
   TInitSymbolEvent = procedure(sender : TObject; symbol : TSymbol) of object;
   TInitExprEvent = procedure(sender : TObject; expr : TExprBase) of object;

   TdwsCallable = class(TInterfacedSelfObject, IExecutable, ICallable)
      private
         FOwner : TObject;
         FOnInitSymbol : TInitSymbolEvent;
         FOnInitExpr : TInitExprEvent;

      protected
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol); virtual; abstract;
         procedure InitSymbol(symbol : TSymbol);
         procedure InitExpression(expr : TExprBase);

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
         FFuncType : TDataType;
         FParameters : TdwsParameters;
         FDeprecated : String;
         FCallable : TdwsCallable;

      protected
         function GetDisplayName: String; override;
         procedure SetParameters(const Value: TdwsParameters);
         function GetOnInitExpr : TInitExprEvent;
         procedure SetOnInitExpr(const val : TInitExprEvent);
         function GetOnInitSymbol : TInitSymbolEvent;
         procedure SetOnInitSymbol(const val : TInitSymbolEvent);
         function StoreParameters : Boolean;

      public
         constructor Create(collection : TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
         function GetParameters(Table: TSymbolTable): TParamArray;

      published
         property Parameters : TdwsParameters read FParameters write SetParameters stored StoreParameters;
         property ResultType : TDataType read FFuncType write FFuncType;
         property OnInitSymbol : TInitSymbolEvent read GetOnInitSymbol write SetOnInitSymbol;
         property OnInitExpr : TInitExprEvent read GetOnInitExpr write SetOnInitExpr;
         property Deprecated : String read FDeprecated write FDeprecated;
   end;

   TdwsFunction = class(TdwsFunctionSymbol)
      protected
         function GetOnEval : TFuncEvalEvent;
         procedure SetOnEval(const val : TFuncEvalEvent);

      public
         constructor Create(collection : TCollection); override;

      published
         property OnEval : TFuncEvalEvent read GetOnEval write SetOnEval;
   end;

   TdwsFunctions = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsFunction;
   end;

  TdwsFunctionsClass = class of TdwsFunctions;

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
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
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
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  published
    property Value: Variant read FValue write FValue;
  end;

   TdwsConstants = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;

      public
         function Add : TdwsConstant;
  end;

  TdwsConstantsClass = class of TdwsConstants;

  TdwsForward = class(TdwsSymbol)
  protected
    function GetDisplayName: String; override;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

   TdwsForwards = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsForward;
   end;

  TdwsForwardsClass = class of TdwsForwards;

   TdwsField = class(TdwsVariable)
      private
         FVisibility : TdwsVisibility;

      protected
         function GetDisplayName: String; override;

      public
         constructor Create(Collection: TCollection); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
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
         FReadAccess: String;
         FWriteAccess: String;
         FParameters: TdwsParameters;
         FDeprecated : String;
         FIndexValue: Variant;
         FIsDefault: Boolean;
         FIndexType: TDataType;
         FVisibility : TdwsVisibility;

      protected
         function GetDisplayName: String; override;
         function GetIsDefault: Boolean;
         procedure SetIsDefault(Value: Boolean);
         procedure SetParameters(const Value: TdwsParameters);
         function StoreParameters : Boolean;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

      published
         property DataType: TDataType read FDataType write FDataType;
         property Deprecated : String read FDeprecated write FDeprecated;
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property ReadAccess: String read FReadAccess write FReadAccess;
         property WriteAccess: String read FWriteAccess write FWriteAccess;
         property Parameters: TdwsParameters read FParameters write SetParameters stored StoreParameters;
         property IsDefault: Boolean read GetIsDefault write SetIsDefault;
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
    FUsesAccess: String;
  protected
    function GetDisplayName: String; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property DataType: TDataType read FDataType write FDataType;
    property Operator : TTokenType read FOperator write FOperator;
    property UsesAccess : String read FUsesAccess write FUsesAccess;
  end;

  TdwsClassOperators = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
  end;

  TdwsTypeSymbol = class(TCollectionItem)
  private
    FName: String;
  protected
    function GetDisplayName: String; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name : String read FName write FName;
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
         FUsesAccess: String;

      protected
         function GetDisplayName: String; override;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

      published
         property ResultType : TDataType read FResultType write FResultType;
         property Params : TdwsTypeSymbols read FParams write FParams;
         property Operator : TTokenType read FOperator write FOperator;
         property UsesAccess : String read FUsesAccess write FUsesAccess;
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
         FResultType : TDataType;
         FVisibility : TdwsVisibility;

      protected
         function GetDisplayName: String; override;
         function GetOnEval : TMethodEvalEvent;
         procedure SetOnEval(const val : TMethodEvalEvent);
         procedure SetResultType(const Value: TDataType);

      public
         constructor Create(collection : TCollection); override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Attributes: TMethodAttributes read FAttributes write FAttributes default [];
         property OnEval : TMethodEvalEvent read GetOnEval write SetOnEval;
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property Kind: TMethodKind read FKind write FKind;
         property ResultType: TDataType read FResultType write SetResultType;
   end;

   TdwsMethods = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsMethod;
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
         function GetResultType: String;
         function GetDisplayName: String; override;
         function GetOnEval : TAssignExternalObjectEvent;
         procedure SetOnEval(const val : TAssignExternalObjectEvent);

      public
         constructor Create(Collection: TCollection); override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Visibility : TdwsVisibility read FVisibility write FVisibility default cvPublic;
         property OnEval : TAssignExternalObjectEvent read GetOnEval write SetOnEval;
         property Attributes: TMethodAttributes read FAttributes write FAttributes default [];
         property ResultType: String read GetResultType;
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
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
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
         FAncestor : String;
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
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

         {: User-side helper object, freed by the TdwsClass. }
         property HelperObject : TObject read FHelperObject write FHelperObject;

      published
         property Ancestor : String read FAncestor write FAncestor;
         property IsSealed : Boolean read FIsSealed write FIsSealed default False;
         property IsAbstract : Boolean read FIsAbstract write FIsAbstract default False;
         property IsStatic : Boolean read FIsStatic write FIsStatic default False;
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
         FAncestor : String;
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
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

      published
         property Ancestor : String read FAncestor write FAncestor;
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
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
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
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
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
    procedure SetUserDefValue(const Value: Integer);
    procedure SetIsUserDef(const Value: Boolean);
  protected
    function GetDisplayName: String; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property UserDefValue: Integer read FUserDefValue write SetUserDefValue;
    property IsUserDef: Boolean read FIsUserDef write SetIsUserDef;
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
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
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

  TdwsCustomInstance = class;

  TReadVarEvent = procedure (info: TProgramInfo; var value : Variant) of object;
  TWriteVarEvent = procedure (info: TProgramInfo; const value : Variant) of object;
  TInstantiateEvent = procedure (info: TProgramInfo; var ExtObject: TObject) of object;

  TdwsGlobal = class(TdwsVariable)
  private
    FOnReadVar: TReadVarEvent;
    FOnWriteVar: TWriteVarEvent;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
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
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
    procedure Assign(Source: TPersistent); override;
    property AutoDestroyExternalObject: Boolean read FAutoDestroyExternalObject
      write FAutoDestroyExternalObject default False;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write
      FOnObjectDestroy;
    property OnInstantiate: TInstantiateEvent read FOnInstantiate write
      FOnInstantiate;
    property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
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

  TdwsSynonyms = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
  end;

  TdwsSynonymsClass = class of TdwsSynonyms;

  TdwsSynonym = class(TdwsVariable)
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

   // TdwsUnit
   //
   TdwsUnit = class(TdwsAbstractStaticUnit)
      private
        FArrays: TdwsArrays;
        FClasses: TdwsClasses;
        FConstants: TdwsConstants;
        FEnumerations: TdwsEnumerations;
        FForwards: TdwsForwards;
        FFunctions: TdwsFunctions;
        FInstances: TdwsInstances;
        FRecords: TdwsRecords;
        FInterfaces : TdwsInterfaces;
        FSynonyms: TdwsSynonyms;
        FVariables: TdwsVariables;
        FOperators : TdwsOperators;
        FTable: TUnitSymbolTable;
        FOnAfterInitUnitTable : TNotifyEvent;

      protected
        FCollections : array[0..11] of TdwsCollection;

        class function GetArraysClass : TdwsArraysClass; virtual;
        class function GetClassesClass : TdwsClassesClass; virtual;
        class function GetConstantsClass : TdwsConstantsClass; virtual;
        class function GetEnumerationsClass : TdwsEnumerationsClass; virtual;
        class function GetForwardsClass : TdwsForwardsClass; virtual;
        class function GetFunctionsClass : TdwsFunctionsClass; virtual;
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
        procedure SetForwards(const Value: TdwsForwards);
        procedure SetFunctions(const Value: TdwsFunctions);
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
        function StoreForwards : Boolean;
        function StoreFunctions : Boolean;
        function StoreRecords : Boolean;
        function StoreInterfaces : Boolean;
        function StoreVariables : Boolean;
        function StoreInstances : Boolean;
        function StoreSynonyms : Boolean;
        function StoreOperators : Boolean;

      protected
        function GetSymbol(Table: TSymbolTable; const Name: String): TSymbol;
        procedure AddCollectionSymbols(Collection: TdwsCollection; Table: TSymbolTable; operators : TOperators); virtual;
        procedure AddUnitSymbols(Table: TSymbolTable; operators : TOperators); override;
        procedure InitUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                operators : TOperators; UnitTable: TUnitSymbolTable); override;

        // Method to support get/set property values for dynamicly registered classes
        procedure HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
        procedure HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);

      public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure GetDataTypes(List: TStrings);
        procedure GetClassTypes(List: TStrings);
        procedure ExposeClassToUnit(AClass, AAncestor: TClass; ASearchProgram: TdwsProgram=nil; const ScriptAncestorType: String='');
        procedure ExposeInstanceToUnit(const AName, AClassType: String; AInstance: TObject);
        property Table: TUnitSymbolTable read FTable;

      published
        property Arrays: TdwsArrays read FArrays write SetArrays stored StoreArrays;
        property Classes: TdwsClasses read FClasses write SetClasses stored StoreClasses;
        property Constants: TdwsConstants read FConstants write SetConstants stored StoreConstants;
        property Dependencies;
        property Enumerations: TdwsEnumerations read FEnumerations write SetEnumerations stored StoreEnumerations;
        property Forwards: TdwsForwards read FForwards write SetForwards stored StoreForwards;
        property Functions: TdwsFunctions read FFunctions write SetFunctions stored StoreFunctions;
        property Instances: TdwsInstances read FInstances write SetInstances stored StoreInstances;
        property Operators : TdwsOperators read FOperators write SetOperators stored StoreOperators;
        property Records : TdwsRecords read FRecords write SetRecords stored StoreRecords;
        property Interfaces : TdwsInterfaces read FInterfaces write SetInterfaces stored StoreInterfaces;
        property Synonyms: TdwsSynonyms read FSynonyms write SetSynonyms stored StoreSynonyms;
        property UnitName;
        property Variables: TdwsVariables read FVariables write SetVariables stored StoreVariables;
        property StaticSymbols;

        property OnAfterInitUnitTable : TNotifyEvent read FOnAfterInitUnitTable write FOnAfterInitUnitTable;
   end;

  TCustomInstantiateFunc = class(TAnonymousFunction, IObjectOwner)
  protected
    FClassSym: TClassSymbol;
    FScriptObj: IScriptObj;
  public
    procedure ReleaseObject;
    property ClassSym: TClassSymbol read FClassSym write FClassSym;
  end;

  TDynamicInstantiateFunc = class(TCustomInstantiateFunc)
  protected
    FExternalObject: TObject;
  public
    constructor Create(FuncSym: TFuncSymbol; AExternalObject: TObject); reintroduce; virtual;
    procedure Execute(info : TProgramInfo); override;
  end;

  TInstantiateFunc = class(TCustomInstantiateFunc)
  private
    FOnInstantiate: TInstantiateEvent;
    FOnObjectDestroy: TObjectDestroyEvent;
    FOnInitSymbol: TInitSymbolEvent;
    FOnInitExpr: TInitExprEvent;
  public
    procedure Execute(info : TProgramInfo); override;
    procedure InitSymbol(Symbol: TSymbol); override;
    procedure InitExpression(Expr: TExprBase); override;
    property OnInstantiate: TInstantiateEvent read FOnInstantiate write FOnInstantiate;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
    property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
    property OnInitExpr: TInitExprEvent read FOnInitExpr write FOnInitExpr;
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
    FData: TData;
    FTyp: TTypeSymbol;
  public
    constructor Create(FuncSym: TFuncSymbol);
    procedure Execute(info : TProgramInfo); override;
    procedure SetValue(const data : TData; offset : Integer);
  end;

  TWriteVarFunc = class(TAnonymousFunction)
  private
    FReadVarFunc: TReadVarFunc;
  public
    constructor Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
    procedure Execute(info : TProgramInfo); override;
  end;

// Return the external object for a variable name.
function GetExternalObjForID(Info: TProgramInfo; const AVarName: String): TObject;

// Get or create the DWS object ID (like a pointer) for a Delphi object instance.
//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: String = ''): Integer;

function GetParameters(Symbol: TdwsSymbol;
  Parameters: TdwsParameters; Table: TSymbolTable): TParamArray;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
  EGenerationError = class(Exception);
  EHandledGenerationError = class(Exception);

// ValueToString
//
function ValueToString(const value : Variant) : String;
begin
   case VarType(value) of
      varEmpty :
         Result := 'Unassigned';
      varNull :
         Result := 'Null';
      varString, varUString, varOleStr, varStrArg :
         Result := Format('''%s''', [VarToStr(value)]);
      varDate :
         Result := Format('DateTime(%f)', [TVarData(value).VDate]);
   else
      Result := VarToStr(value);
   end;
end;

function GetExternalObjForID(Info: TProgramInfo; const AVarName: String): TObject;
begin
  // Get param "Source" as object in Source_Obj
  Result := IScriptObj(IUnknown(Info.ValueAsVariant[AVarName])).ExternalObject;
end;

//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: String): Integer;
//var
//  ScriptObj: TScriptObj;
//begin
//  if Assigned(AObject) then                // if object was returned
//  begin
//    if AClassName = '' then
//      AClassName := AObject.ClassName;
//
//    // Find the Delphi object and return the Id
//    ScriptObj := Info.Caller.FindExternalObject(AObject);
//    if Assigned(ScriptObj) then            // if object found
//      Result := ScriptObj.Id               // return the object's Id
//    else                                   // if not found, register the object and return the Id
//      Result := Info.Vars[AClassName].GetConstructor('Create', AObject).Call.Value;
//  end
//  else                                     // no object returned
//    Result := 0;                           // return 'nil' Id
//end;

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
   FLock:=TFixedCriticalSection.Create;
end;

// Destroy
//
destructor TDelphiWebScript.Destroy;
begin
   inherited;
   FCompiler.Free;
   FConfig.Free;
   FExtensions.Free;
   FLock.Free;
end;

function TDelphiWebScript.GetVersion: String;
begin
  Result := '2.3';
end;

procedure TDelphiWebScript.SetVersion(const Value: String);
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
begin
   if FExtensions.Count>0 then begin
      FCompiler.OnCreateBaseVariantSymbol:=FExtensions.CreateBaseVariantSymbol;
      FCompiler.OnCreateSystemSymbols:=FExtensions.CreateSystemSymbols;
      FCompiler.OnReadInstr:=FExtensions.ReadInstr;
      FCompiler.OnReadInstrSwitch:=FExtensions.ReadInstrSwitch;
      FCompiler.OnFindUnknownName:=FExtensions.FindUnknownName;
      FCompiler.OnSectionChanged:=FExtensions.SectionChanged;
      FCompiler.OnReadScript:=FExtensions.ReadScript;
      FCompiler.OnGetDefaultEnvironment:=FExtensions.DefaultEnvironment;
      FCompiler.OnRootExternalClass:=FExtensions.RootExternalClass;
   end else begin
      FCompiler.OnCreateBaseVariantSymbol:=nil;
      FCompiler.OnCreateSystemSymbols:=nil;
      FCompiler.OnReadInstr:=nil;
      FCompiler.OnReadInstrSwitch:=nil;
      FCompiler.OnFindUnknownName:=nil;
      FCompiler.OnSectionChanged:=nil;
      FCompiler.OnReadScript:=nil;
      FCompiler.OnGetDefaultEnvironment:=nil;
      FCompiler.OnRootExternalClass:=nil;
   end;
end;

// Compile
//
function TDelphiWebScript.Compile(const Text: String): IdwsProgram;
begin
   Lock;
   try
      SetupExtensions;
      Result := FCompiler.Compile(Text, FConfig);
   finally
      UnLock;
   end;
end;

// RecompileInContext
//
procedure TDelphiWebScript.RecompileInContext(const prog : IdwsProgram; const text : String);
begin
   Lock;
   try
      SetupExtensions;
      FCompiler.RecompileInContext(prog, text, FConfig);
   finally
      UnLock;
   end;
end;

// AddUnit
//
procedure TDelphiWebScript.AddUnit(const Un: IdwsUnit);
begin
   RemoveUnit(Un);
   if Assigned(Un) then
      FConfig.Units.Add(Un);
end;

// RemoveUnit
//
function TDelphiWebScript.RemoveUnit(const Un: IdwsUnit): Boolean;
var
   i : Integer;
begin
   i := FConfig.Units.IndexOf(Un);
   if i >= 0 then begin
      FConfig.Units[i]:=nil;
      FConfig.Units.Extract(i);
   end;
   Result := i >= 0;
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

  FSortedSymbols := nil;
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
  setlength(FSortedSymbols,0);
  FSortedSymbols := nil;
end;

procedure TdwsCollection.SetItem(Index: Integer; Value: TdwsSymbol);
begin
  Items[Index].Assign(Value);
  setlength(FSortedSymbols,0);
  FSortedSymbols := nil;
end;

function TdwsCollection.GetSymbols(const Name: String): TdwsSymbol;
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

function TdwsCollection.GetSortedItems: TdwsSymbolArray;
var
  x: Integer;
  FSortedItems: TStringList;
begin
  if not assigned(FSortedSymbols) then
  begin
    FSortedItems := TStringList.Create;
    FSortedItems.Sorted := true;
    FSortedItems.Duplicates := dupAccept;

    for x := 0 to Count - 1 do
      FSortedItems.AddObject(Items[x].Name,Items[x]);

    SetLength(FSortedSymbols,FSortedItems.Count);
    for x := Count - 1 downto 0 do
    begin
      FSortedSymbols[x] := TdwsSymbol(FSortedItems.Objects[x]);
      FSortedItems.Objects[x] := nil;
    end;

    FSortedItems.Free;
  end;

  result := FSortedSymbols;
end;

function TdwsCollection.GetSortedItem(Index: Integer): TdwsSymbol;
begin
  result := GetSortedItems[Index];
end;

function TdwsCollection.IndexOf(const Name: String): Integer;
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
   FForwards := GetForwardsClass.Create(Self);
   FFunctions := GetFunctionsClass.Create(Self);
   FRecords := GetRecordsClass.Create(Self);
   FInterfaces := GetInterfacesClass.Create(Self);
   FVariables := GetVariablesClass.Create(Self);
   FInstances := GetInstancesClass.Create(Self);
   FSynonyms := GetSynonymsClass.Create(Self);
   FOperators := TdwsOperators.Create(Self);

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

end;

destructor TdwsUnit.Destroy;
var
   i : Integer;
begin
   for i:=Low(FCollections) to High(FCollections) do
      FCollections[i].Free;
   inherited;
end;

procedure TdwsUnit.AddCollectionSymbols(Collection: TdwsCollection;
  Table: TSymbolTable; operators : TOperators);
var
  y: Integer;
  clsName : String;
  collSym : TdwsSymbol;
  sym : TSymbol;
begin
   // add all classes as forwards automatically if they aren't there already
   for y:=0 to FClasses.Count-1 do begin
      clsName:=FClasses.Items[y].Name;
      if FForwards.IndexOf(clsName)<0 then
         Forwards.Add.Name:=clsName;
   end;

   for y := 0 to Collection.Count - 1 do begin
      collSym:=TdwsSymbol(Collection.Items[y]);
      if not collSym.IsGenerating then begin
         try
            sym:=collSym.Generate(Table);
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

procedure TdwsUnit.AddUnitSymbols(Table: TSymbolTable; operators : TOperators);
var
  x: Integer;
begin
  for x := Low(FCollections) to High(FCollections) do
    FCollections[x].Reset;

  for x := Low(FCollections) to High(FCollections) do
    AddCollectionSymbols(FCollections[x], Table, operators);
end;

procedure TdwsUnit.GetClassTypes(List: TStrings);
var
   x : Integer;
   sysTable : TSystemSymbolTable;
begin
   if not Assigned(List) then
      Exit;

   if Assigned(FScript) then begin
      sysTable:=FScript.Config.SystemTable.SymbolTable;
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
    sysTable:=FScript.Config.SystemTable.SymbolTable;
    for x := 0 to sysTable.Count - 1 do
    begin
      if sysTable[x] is TTypeSymbol then
        List.Add(sysTable[x].Name);
    end;
  end;

  // Only return array-, record- and class symbols, synonyms and enums
  for x := 1 to 5 do
  begin
    coll := FCollections[x];
    for y := 0 to coll.Count - 1 do
      List.Add(coll.Items[y].Name);
  end;
end;

function TdwsUnit.GetSymbol(Table: TSymbolTable; const Name: String): TSymbol;
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
               raise Exception.CreateFmt(UNT_CircularReference, [item.ClassName+':'+Name]);

            // Generate the symbol now
            try
               Result := item.Generate(Table);
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

// StoreEnumerations
//
function TdwsUnit.StoreEnumerations : Boolean;
begin
   Result:=FEnumerations.Count>0;
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

procedure TdwsUnit.HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);
var
  propName: String;
  param: TParamSymbol;
  setValue: Variant;
begin
  { NOTE: Special handling is required for Boolean types. At least with Delphi 5 }
  if (Info.FuncSym is TMethodSymbol) and Assigned(ExtObject) then
  begin
    propName := Copy(Info.FuncSym.Name, 4, Length(Info.FuncSym.Name));  // get property name. Trim off Get/Set prefix
    case TMethodSymbol(Info.FuncSym).Kind of
    fkFunction  :   // function is a "Get" method
      begin
        { Return property value for property GetXXX function }
        // Class
        if Info.FuncSym.Typ is TClassSymbol then   // don't free the object instance returned
          Info.ResultAsVariant := Info.RegisterExternalObject(GetObjectProp(ExtObject, propName), False, False)  // wrap as best we can (find a match)
        // Boolean
        else if SameText(Info.FuncSym.Typ.Name, SYS_BOOLEAN) then
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


{ AClass is the class to expose to the unit. All published properties of standard
  simple datatypes that are supported in DWS will be exposed that were introduced
  between AAncestor and AClass. The ScriptAncestorType is the type that will be
  used for the new Script class inherited class type. If none is provided then
  AAncestor.ClassName is used. }
procedure TdwsUnit.ExposeClassToUnit(AClass, AAncestor: TClass;  ASearchProgram: TdwsProgram; const ScriptAncestorType: String);

    { Determine if the type is available to the program. If so, add the owning
       unit as a dependency. }
    function IsTypeSupported(const ATypeName: String): Boolean;
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
  PropertyName, PropertyType: String;
  i: Integer;
  Include: Boolean;
  getMethName, setMethName: String;
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
      PropertyName := String(PropList^[i]^.Name);
      propIsDefault := WordBool(PropList^[i]^.Default);
      {$ifdef FPC}
      propTypeData := GetTypeData(PropList^[i]^.PropType);
      {$else}
      propTypeData := GetTypeData(PropList^[i]^.PropType^);
      {$endif}

      Include := True;
      if IsTypeSupported(String(PropList^[i]^.PropType^.Name)) then
        PropertyType := String(PropList^[i]^.PropType^.Name)
      else
      begin
        { NOTE: Could attempt to use the actual type name (ex: TComponentName is a String).
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

procedure TdwsUnit.ExposeInstanceToUnit(const AName, AClassType: String;
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
    raise Exception.CreateFmt(UNT_DatatypeUnknown, [Name]);

  if typSym is TClassSymbol then
  begin
    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    instFunc := TDynamicInstantiateFunc.Create(funcSym, AInstance);
    Table.AddObjectOwner(instFunc);
    instFunc.ClassSym := TClassSymbol(typSym);
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
  { TODO : If accepted, create a String declaration in appropriate unit. }
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
function TdwsConstant.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   Result:=TConstSymbol.Create(Name, GetDataType(Table, DataType), Value);
   GetUnit.Table.AddSymbol(Result);
end;

// GetDisplayName
//
function TdwsConstant.GetDisplayName: String;
var
   valAsString : String;
begin
   valAsString:=VarToStr(Value);
   if SameText(DataType, SYS_STRING) then  // just for show
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
function TdwsVariables.Add(const name, typName : String) : TdwsGlobal;
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

function TdwsGlobal.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
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
   typSym := GetDataType(Table, DataType);
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

{ TInstantiateFunc }

procedure TInstantiateFunc.Execute(info : TProgramInfo);
var
  scriptObj: TScriptObjInstance;
  extObj: TObject;
begin
  if Assigned(FScriptObj) then
    // Instance was already created
    Info.ResultAsVariant := FScriptObj
  else
  begin
    // First access to this variable. Create object instance!
    scriptObj := TScriptObjInstance.Create(FClassSym{, Info.Caller});
    scriptObj.OnObjectDestroy := FOnObjectDestroy;
    FScriptObj := scriptObj;

    FOnInstantiate(info, extObj);
    FScriptObj.ExternalObject := extObj;

    Info.ResultAsVariant := FScriptObj;
  end;
end;

procedure TInstantiateFunc.InitSymbol(Symbol: TSymbol);
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

{ TdwsParameter }

procedure TdwsParameter.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsParameter then
  begin
    FIsVarParam := TdwsParameter(Source).IsVarParam;
    FIsWritable := TdwsParameter(Source).IsWritable;
  end;
end;

constructor TdwsParameter.Create(Collection: TCollection);
begin
  inherited;
  FIsWritable := True;
  FIsVarParam := False;
  FDefaultValue := Unassigned;
  FHasDefaultValue := False;
end;

function TdwsParameter.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
//var
//   paramSym : TParamSymbol;
//   paramType : TTypeSymbol;
//   elemSym : TSymbol;
//   elemValue : Integer;
begin
   Result:=nil;
   Assert(False);  // shouldn't be used anymore (not sure yet)
//   FIsGenerating := True;
//   paramType := GetDataType(Table, DataType);
//   if IsLazy then
//      paramSym:=TLazyParamSymbol.Create(Name, paramType)
//   else if IsVarParam then begin
//      if IsWritable then
//         paramSym := TVarParamSymbol.Create(Name, paramType)
//      else paramSym := TConstParamSymbol.Create(Name, paramType)
//   end else if HasDefaultValue then begin
//      paramSym := TParamSymbolWithDefaultValue.Create(Name, paramType);
//      if paramType is TEnumerationSymbol then begin
//         elemSym:=TEnumerationSymbol(paramType).Elements.FindLocal(DefaultValue);
//         if elemSym=nil then
//            elemValue:=DefaultValue
//         else elemValue:=TElementSymbol(elemSym).UserDefValue;
//         TParamSymbolWithDefaultValue(paramSym).SetDefaultValue(elemValue);
//      end else TParamSymbolWithDefaultValue(paramSym).SetDefaultValue(DefaultValue);
//   end else begin
//      paramSym := TParamSymbol.Create(Name, paramType);
//   end;
//   Result := paramSym;
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

procedure TdwsParameter.SetIsVarParam(const Value: Boolean);
begin
   FIsVarParam := Value;
   if FIsVarParam and FIsWritable then
      FHasDefaultValue := False;
   if FIsVarParam then
      FIsLazy:=False;
end;

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
procedure TdwsCallable.InitSymbol(symbol : TSymbol);
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
function TdwsFunctionSymbol.DoGenerate(table : TSymbolTable; parentSym : TSymbol = nil) : TSymbol;
var
   funcSym : TFuncSymbol;
begin
   FIsGenerating:=True;
   CheckName(Table, Name);
   if ResultType<>'' then
      GetDataType(Table, ResultType);

   funcSym:=TFuncSymbol.Generate(table, Name, GetParameters(table), ResultType);
   try
      funcSym.Params.AddParent(table);

      funcSym.Executable:=FCallable;
      funcSym.DeprecatedMessage:=Deprecated;
      GetUnit.Table.AddSymbol(funcSym);
   except
      funcSym.Free;
      raise;
   end;
   Result:=funcSym;
end;

// GetParameters
//
function GetParameters(Symbol: TdwsSymbol; Parameters: TdwsParameters; Table: TSymbolTable): TParamArray;
var
   i, j, elemValue: Integer;
   name: String;
   paramSym, elemSym : TSymbol;
   param : TdwsParameter;
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

      Result[i].IsVarParam := param.IsVarParam and param.IsWritable;
      Result[i].IsConstParam := param.IsVarParam and not param.IsWritable;
      Result[i].ParamName := name;
      Result[i].ParamType := param.DataType;

      Result[i].HasDefaultValue := param.HasDefaultValue;
      if Result[i].HasDefaultValue then begin
         SetLength(Result[i].DefaultValue, 1);
         paramSym:=Symbol.GetDataType(Table, Result[i].ParamType);
         if paramSym is TEnumerationSymbol then begin
            elemSym:=TEnumerationSymbol(paramSym).Elements.FindLocal(param.DefaultValue);
            if elemSym=nil then
               elemValue:=param.DefaultValue
            else elemValue:=TElementSymbol(elemSym).Value;
            Result[i].DefaultValue[0] := elemValue;
         end else Result[i].DefaultValue[0] := param.DefaultValue;
      end else Result[i].DefaultValue := nil;

      Symbol.GetUnit.GetSymbol(Table, Result[i].ParamType);
  end;
end;


function TdwsFunctionSymbol.GetParameters(Table: TSymbolTable): TParamArray;
begin
  Result := dwsComp.GetParameters(Self,Parameters,Table);
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
   if Deprecated<>'' then
      Result:=Result+' deprecated;'
end;

procedure TdwsFunctionSymbol.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsFunctionSymbol then
  begin
    FFuncType := TdwsFunctionSymbol(Source).ResultType;
    FParameters.Assign(TdwsFunctionSymbol(Source).Parameters);
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

function TdwsField.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TClassSymbol(ParentSym).Members, Name);
  Result := TFieldSymbol.Create(Name, GetDataType(Table, DataType), Visibility);
end;

// GetDisplayName
//
function TdwsField.GetDisplayName: String;
begin
   Result:=TClassSymbol.VisibilityToString(Visibility)+' '+inherited GetDisplayName;
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
function TdwsMethod.DoGenerate(table : TSymbolTable; parentSym : TSymbol = nil) : TSymbol;
var
   methSymbol : TMethodSymbol;
begin
   FIsGenerating := True;
   CheckName(TClassSymbol(parentSym).Members, Name);

   if ResultType <> '' then
      GetUnit.GetSymbol(table, ResultType);

   methSymbol:=TMethodSymbol.Generate(table, Kind, Attributes, Name,
                                      GetParameters(table), ResultType,
                                      TClassSymbol(parentSym), Visibility);
   try
      methSymbol.Params.AddParent(table);
      methSymbol.DeprecatedMessage:=Deprecated;

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
      mkConstructor:
         Result:=Format('constructor %s%s;', [Name, Result]);
      mkDestructor:
         Result:=Format('destructor %s%s;', [Name, Result]);
      mkClassProcedure:
         Result:=Format('class procedure %s%s;', [Name, Result]);
      mkClassFunction:
         Result:=Format('class function %s%s : %s;', [Name, Result, ResultType]);
   else
      Assert(false); // if triggered, this func needs upgrade !
   end;
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

procedure TdwsMethod.SetResultType(const Value: TDataType);
begin
  FResultType := Value;
  if Value <> '' then
    case FKind of
      mkProcedure:
        FKind := mkFunction;
      mkClassProcedure:
        FKind := mkClassFunction;
    end
  else
    case FKind of
      mkFunction:
        FKind := mkProcedure;
      mkClassFunction:
        FKind := mkClassProcedure;
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
function TdwsConstructor.DoGenerate(table : TSymbolTable; parentSym : TSymbol) : TSymbol;
var
   methSymbol : TMethodSymbol;
begin
   FIsGenerating := True;
   CheckName(TClassSymbol(ParentSym).Members, Name);

   methSymbol := TMethodSymbol.Generate(Table, mkConstructor, Attributes, Name,
                                        GetParameters(Table), '', TClassSymbol(ParentSym), Visibility);
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

function TdwsConstructor.GetResultType: String;
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
function TdwsClassConstant.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   Result:=TClassConstSymbol.Create(Name, GetDataType(Table, DataType), Value);
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
function TdwsClass.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
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

      ancestorSym := TClassSymbol(GetUnit.GetSymbol(Table, FAncestor));
      if ancestorSym = nil then
         raise Exception.CreateFmt(UNT_SuperClassUnknwon, [FAncestor]);

      if ancestorSym.IsSealed then
         raise Exception.CreateFmt(CPE_ClassIsSealed, [FAncestor]);

      classSym.InheritFrom(ancestorSym);

      classSym.IsSealed:=IsSealed;
      classSym.IsExplicitAbstract:=IsAbstract;

      for x := 0 to FFields.Count - 1 do
         classSym.AddField(TFieldSymbol(TdwsField(FFields.Items[x]).Generate(Table, classSym)));

      for x := 0 to FConstructors.Count - 1 do
         classSym.AddMethod(TMethodSymbol(TdwsConstructor(FConstructors.Items[x]).Generate(Table, classSym)));

      for x := 0 to FMethods.Count - 1 do
         classSym.AddMethod(TMethodSymbol(TdwsMethod(FMethods.Items[x]).Generate(Table, classSym)));

      for x := 0 to FProperties.Count - 1 do
         classSym.AddProperty(TPropertySymbol(TdwsProperty(FProperties.Items[x]).Generate(Table, classSym)));

      for x := 0 to FOperators.Count - 1 do
         classSym.AddOperator(TClassOperatorSymbol(TdwsClassOperator(FOperators.Items[x]).Generate(Table, classSym)));

      for x := 0 to FConstants.Count - 1 do
         classSym.AddConst(TClassConstSymbol(TdwsConstant(FConstants.Items[x]).Generate(Table, classSym)));

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
  if Ancestor <> '' then
    Result := Name + ' (' + Ancestor + ')'
  else
    Result := Name + ' (TObject)';
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

function TdwsMember.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TRecordSymbol(ParentSym).Members, Name);
  Result := TFieldSymbol.Create(Name, GetDataType(Table, DataType), Visibility);
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
         TRecordSymbol(Result).AddField(TFieldSymbol(TdwsMember(FMembers.Items[x]).Generate(Table, Result)));
      for x := 0 to FProperties.Count - 1 do
         TRecordSymbol(Result).AddProperty(TPropertySymbol(TdwsProperty(FProperties.Items[x]).Generate(Table, Result)));
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
function TdwsInterface.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
   Result:=nil; // TODO
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

function TdwsArray.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  if (LowBound = 0) and (HighBound = -1) then
    Result := TDynamicArraySymbol.Create(Name, GetDataType(Table, DataType), GetDataType(Table, SYS_INTEGER))
  else
  begin
    if LowBound > HighBound then
      raise Exception.Create(UNT_InvalidArrayBounds);
    Result := TStaticArraySymbol.Create(Name, GetDataType(Table, DataType), GetDataType(Table, SYS_INTEGER),
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
function TdwsProperty.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil) : TSymbol;
var
   sym : TSymbol;
   propSym : TPropertySymbol;
   indexData : TData;
   parent : TCompositeTypeSymbol;
begin
   FIsGenerating := True;

   if DataType='' then
      raise Exception.CreateFmt(UNT_DatatypeNotSpecified, [Name, ParentSym.Name]);

   propSym := TPropertySymbol.Create(Name, GetDataType(Table, DataType), Visibility);
   Result := PropSym;

   propSym.GenerateParams(Table, GetParameters(Self, Parameters, Table));

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
      propSym.SetIndex(indexData, 0, GetDataType(Table, IndexType));
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
      properties := TdwsClass(TdwsCollection(Collection).GetOwner).Properties;
      for i := 0 to properties.Count - 1 do
        if properties.Items[i] <> Self then
          TdwsProperty(properties.Items[i]).FIsDefault := False;
    end;
  end;
end;

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

function TdwsClassOperator.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
var
   opSymbol : TClassOperatorSymbol;
   sym : TSymbol;
begin
   FIsGenerating := True;

   opSymbol:=TClassOperatorSymbol.Create(FOperator);
   Result:=opSymbol;

   Result.Typ:=GetDataType(Table, DataType);
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

function TdwsSymbol.GetNamePath: String;
begin
  if FName <> '' then
    Result := Collection.GetNamePath + FName
  else
    Result := Collection.GetNamePath + IntToStr(Index);
end;

// Generate
//
function TdwsSymbol.Generate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
   try
      Result := DoGenerate(Table, ParentSym);
   except
      on e: EHandledGenerationError do
         raise;
      on e: Exception do
         raise Exception.CreateFmt(UNT_SymbolGenerationError, [ClassName, Name, e.Message]);
  end;
end;

function TdwsSymbol.GetDataType(Table: TSymbolTable; Name: String): TTypeSymbol;
var sym : TSymbol;
begin
  sym := GetUnit.GetSymbol(Table, Name);
  if not (sym is TTypeSymbol) then
    raise Exception.CreateFmt(UNT_DatatypeUnknown, [Name]);
  Result := TTypeSymbol(sym);
end;

procedure TdwsSymbol.CheckName(Table: TSymbolTable;
  Name: String);
begin
  if Name = '' then
    raise Exception.Create(UNT_NameIsEmpty);

  if Assigned(Table.FindLocal(Name)) then
    raise Exception.CreateFmt(UNT_NameAlreadyExists, [Name]);
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
  VarClear(Value);
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
  FDependencies := TStringList.Create;
end;

destructor TdwsAbstractUnit.Destroy;
begin
  Script := nil;
  FDependencies.Free;
  inherited;
end;

function TdwsAbstractUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function TdwsAbstractUnit.GetUnitName: String;
begin
  Result := FUnitName;
end;

procedure TdwsAbstractUnit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FScript) then
    FScript := nil;
end;

procedure TdwsAbstractUnit.SetDependencies(const Value: TStrings);
begin
  FDependencies.Assign(Value);
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

procedure TdwsAbstractUnit.SetUnitName(const Value: String);
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
   Result:=[];
end;

{ TdwsEmptyUnit }

constructor TdwsEmptyUnit.Create(AOwner: TComponent);
begin
  inherited;
  FDependencies := TStringList.Create;
end;

destructor TdwsEmptyUnit.Destroy;
begin
  inherited;
  FDependencies.Free;
end;

function TdwsEmptyUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function TdwsEmptyUnit.GetUnitName: String;
begin
  Result := FUnitName;
end;

function TdwsEmptyUnit.GetUnitTable(systemTable : TSystemSymbolTable; unitSyms : TUnitMainSymbols;
                                    operators : TOperators) : TUnitSymbolTable;
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
function TdwsEnumeration.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
var
   i : Integer;
   enumSymbol : TEnumerationSymbol;
   element : TElementSymbol;
begin
   FIsGenerating := True;
   CheckName(Table, Name);

   enumSymbol:=TEnumerationSymbol.Create(Name, Table.FindTypeSymbol(SYS_INTEGER, cvMagic), Style);
   try
      for i:=0 to FElements.Count-1 do begin
         element:=(Elements.Items[i] as TdwsElement).Generate(table, enumSymbol) as TElementSymbol;
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

{ TdwsElement }

function TdwsElement.DoGenerate(Table: TSymbolTable;
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
  else if enumSym.Elements.Count > 0 then
    enumInt := TElementSymbol(enumSym.Elements[enumSym.Elements.Count - 1]).Value + 1
  else
    enumInt := 0;

  Result := TElementSymbol.Create(Name, enumSym, enumInt, FIsUserDef);
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

{ TdwsCustomInstance }

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

function TdwsCustomInstance.DoGenerate(Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
var
  typSym: TTypeSymbol;
  instFunc: TInstantiateFunc;
  funcSym: TFuncSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  // Get the type symbol of this variable
  typSym := GetDataType(Table, DataType);

  if typSym is TClassSymbol then
  begin
    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    instFunc := TInstantiateFunc.Create(funcSym);
    GetUnit.Table.AddObjectOwner(instFunc);
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

{ TdwsConstants }

class function TdwsConstants.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsConstant;
end;

// Add
//
function TdwsConstants.Add : TdwsConstant;
begin
   Result:=TdwsConstant(inherited Add);
end;

{ TdwsClasses }

class function TdwsClasses.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsClass;
end;

// Add
//
function TdwsClasses.Add : TdwsClass;
begin
   Result:=TdwsClass(inherited Add);
end;

{ TdwsArrays }

class function TdwsArrays.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsArray;
end;

function TdwsArrays.Add : TdwsArray;
begin
  Result := TdwsArray(inherited Add);
end;

{ TdwsRecords }

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

{ TdwsParameters }

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

{ TdwsSynonym }

function TdwsSynonym.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  Result := TAliasSymbol.Create(Name, GetDataType(Table, DataType));
  GetUnit.Table.AddSymbol(Result);
end;

{ TdwsAbstractStaticUnit }

constructor TdwsAbstractStaticUnit.Create(AOwner: TComponent);
begin
  inherited;
  FStaticTable := nil;
  FStaticSymbols := False;
end;

function TdwsAbstractStaticUnit.CreateUnitTable(Parent: TSymbolTable; Typ: TSymbolTableType): TUnitSymbolTable;
begin
  case Typ of
    sttLinked: Result := TLinkedSymbolTable.Create(Parent as TStaticSymbolTable);
    sttStatic: Result := TStaticSymbolTable.Create(Parent as TStaticSymbolTable);
  else
    Result := TUnitSymbolTable.Create(Parent);
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
                                             operators : TOperators) : TUnitSymbolTable;
begin
   if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms, operators) then
      Result := CreateUnitTable(FStaticTable.SymbolTable, sttLinked) as TLinkedSymbolTable // typecheck
   else begin
      Result := CreateUnitTable(SystemTable); // sttDefault
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
      FStaticTable := CreateUnitTable(staticParent, sttStatic) as TStaticSymbolTable;
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

   AddUnitSymbols(unitTable, operators);
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

{ TCustomInstantiateFunc }

procedure TCustomInstantiateFunc.ReleaseObject;
begin
  FScriptObj := nil;
end;

{ TDynamicInstantiateFunc }

constructor TDynamicInstantiateFunc.Create(FuncSym: TFuncSymbol;
  AExternalObject: TObject);
begin
  inherited Create(FuncSym);
  FExternalObject := AExternalObject;
end;

procedure TDynamicInstantiateFunc.Execute(info : TProgramInfo);
begin
  if Assigned(FScriptObj) then
    // Instance was already created
    Info.ResultAsVariant := FScriptObj
  else
  begin
    // First access to this variable. Create object instance!
    FScriptObj := TScriptObjInstance.Create(FClassSym);
    FScriptObj.ExternalObject := FExternalObject;
    Info.ResultAsVariant := FScriptObj;
  end;
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
function TdwsOperator.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;

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
      if (sym=nil) or sym.IsType or not (sym is TFuncSymbol) then
         raise Exception.CreateFmt(UNT_UsesAccessNotFound, [UsesAccess]);
      op.UsesSym:=TFuncSymbol(sym);
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
procedure TEventBasedLocalizer.LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : String);
begin
   if Assigned(FOnLocalizeSymbol) then
      FOnLocalizeSymbol(Sender, aResSymbol, Result)
   else LocalizeString(aResSymbol.Value, Result);
end;

// LocalizeString
//
procedure TEventBasedLocalizer.LocalizeString(const aString : String; var Result : String);
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

end.



