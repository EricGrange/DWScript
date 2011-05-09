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
unit dwsComp;

interface

uses
  Variants, Classes, SysUtils, TypInfo, dwsCompiler, dwsExprs, dwsSymbols,
  dwsStack, dwsFunctions, dwsStrings, dwsFileSystem, dwsLanguageExtension,
  dwsTokenizer, dwsUtils,
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

         property Script : TDelphiWebScript read FScript write SetScript;
   end;

  TdwsEmptyUnit = class(TComponent, IUnknown, IUnit)
  private
    function GetUnitName: string;
    function GetDependencies: TStrings;
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable;
  protected
    FUnitName: string;
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

   // TDelphiWebScript
   //
   TDelphiWebScript = class (TdwsEmptyUnit)
      private
         FCompiler : TdwsCompiler;
         FConfig : TdwsConfiguration;
         FExtensions : TdwsLanguageExtensionAggregator;

      protected
         function GetOnInclude: TIncludeEvent;
         function GetVersion: string;
         procedure SetVersion(const Value: string);
         procedure SetConfig(const Value: TdwsConfiguration);
         procedure SetOnInclude(const Value: TIncludeEvent);
         procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure AddUnit(const Un: IUnit);
         function Compile(const Text: string): IdwsProgram; virtual;
         function RemoveUnit(const Un: IUnit): Boolean;

      published
         property Config: TdwsConfiguration read FConfig write SetConfig stored True;
         property OnInclude: TIncludeEvent read GetOnInclude write SetOnInclude;
         property Version: string read GetVersion write SetVersion stored False;
   end;

  TdwsAbstractUnit = class(TComponent, IUnknown, IUnit)
  private
    FDependencies: TStrings;
    FScript: TDelphiWebScript;
    FUnitName: string;
    function GetDependencies: TStrings;
    procedure SetDependencies(const Value: TStrings);
    procedure SetScript(const Value: TDelphiWebScript);
    procedure SetUnitName(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetUnitName: string; virtual;
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable; virtual; abstract;
    property Dependencies: TStrings read FDependencies write SetDependencies;
    {$WARNINGS OFF}
    property UnitName: string read GetUnitName write SetUnitName;
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
    FStaticSymbols: Boolean;
    FStaticTable: TStaticSymbolTable;
  protected
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable; override;
    function CreateUnitTable(Parent: TSymbolTable; Typ: TSymbolTableType = sttDefault): TUnitSymbolTable; virtual;
    procedure SetStaticSymbols(const Value: Boolean); // static symbols
    procedure InitUnitTable(SystemTable, UnitSyms: TSymbolTable; UnitTable: TUnitSymbolTable); virtual;
    procedure AddUnitSymbols(Table: TSymbolTable); virtual; abstract;
    property StaticSymbols: Boolean read FStaticSymbols write SetStaticSymbols;
    property StaticTable: TStaticSymbolTable read FStaticTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function InitStaticSymbols(SystemTable: TSymbolTable; UnitSyms: TSymbolTable): Boolean;
    procedure ReleaseStaticSymbols;
  end;

  TDataType = string;
  TdwsUnit = class;
  TdwsGlobal = class;

  TdwsSymbol = class(TCollectionItem)
  private
    FIsGenerating: Boolean;
    FUnit: TdwsUnit;
    FName: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CheckName(Table: TSymbolTable; Name: string);
    function GetDataType(Table: TSymbolTable; Name: string): TTypeSymbol;
    procedure Reset;
    property IsGenerating: Boolean read FIsGenerating;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function Generate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; virtual; abstract;
    function GetNamePath: string; override;
    function GetUnit: TdwsUnit;
  published
    property Name: string read FName write FName;
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
    function IndexOf(const Name: string): Integer;
    property Symbols[const Name: String]: TdwsSymbol read GetSymbols;
    property Items[Index: Integer]: TdwsSymbol read GetItem write SetItem;
    property SortedItems[Index: Integer]: TdwsSymbol read GetSortedItem;
  end;

  TdwsVariable = class(TdwsSymbol)
  private
    FDataType: TDataType;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DataType: TDataType read FDataType write FDataType;
  end;

   TdwsVariables = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
         function GetDisplayName: string;
      public
         function Add : TdwsGlobal;
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
         function GetDisplayName: string; override;
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

  TdwsFunction = class;

  TFuncEvalEvent = procedure(Info: TProgramInfo) of object;
  TInitSymbolEvent = procedure(Sender: TObject; Symbol: TSymbol) of object;
  TInitExprEvent = procedure(Sender: TObject; Expr: TExprBase) of object;

  TdwsFunction = class(TdwsSymbol, IUnknown, ICallable)
  private
    FOnEval: TFuncEvalEvent;
    FFuncType: TDataType;
    FParameters: TdwsParameters;
    FOnInitSymbol: TInitSymbolEvent;
    FOnInitExpr: TInitExprEvent;
    FDeprecated : String;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  protected
    function GetDisplayName: string; override;
    procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol); virtual;
    procedure SetParameters(const Value: TdwsParameters);
    function StoreParameters : Boolean;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
    function GetParameters(Table: TSymbolTable): TParamArray;
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
  published
    property Parameters: TdwsParameters read FParameters write SetParameters stored StoreParameters;
    property ResultType: TDataType read FFuncType write FFuncType;
    property OnEval: TFuncEvalEvent read FOnEval write FOnEval;
    property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
    property OnInitExpr: TInitExprEvent read FOnInitExpr write FOnInitExpr;
    property Deprecated : String read FDeprecated write FDeprecated;
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
    function GetDisplayName: string; override;
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
  end;

  TdwsArraysClass = class of TdwsArrays;

  TdwsConstant = class(TdwsVariable)
  protected
    FValue: Variant;
    function GetDisplayName: string; override;
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
    function GetDisplayName: string; override;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

  TdwsForwards = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
  end;

  TdwsForwardsClass = class of TdwsForwards;

   TdwsField = class(TdwsVariable)
      private
         FVisibility : TClassVisibility;

      protected
         function GetDisplayName: string; override;

      public
         constructor Create(Collection: TCollection); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
         property Visibility : TClassVisibility read FVisibility write FVisibility default cvPublic;
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
    FReadAccess: string;
    FWriteAccess: string;
    FParameters: TdwsParameters;
    FIsDefault: Boolean;
    FIndexType: TDataType;
    FIndexValue: Variant;
    FVisibility : TClassVisibility;
  protected
    function GetDisplayName: string; override;
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
    property Visibility : TClassVisibility read FVisibility write FVisibility default cvPublic;
    property ReadAccess: string read FReadAccess write FReadAccess;
    property WriteAccess: string read FWriteAccess write FWriteAccess;
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
    FUsesAccess: string;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property DataType: TDataType read FDataType write FDataType;
    property Operator : TTokenType read FOperator write FOperator;
    property UsesAccess : string read FUsesAccess write FUsesAccess;
  end;

  TdwsClassOperators = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
  end;

  TAssignExternalObjectEvent = procedure(Info: TProgramInfo; var ExtObject: TObject) of object;
  TMethodEvalEvent = procedure(Info: TProgramInfo; ExtObject: TObject) of object;

  TdwsMethod = class(TdwsFunction)
  private
    FAttributes : TMethodAttributes;
    FKind : TMethodKind;
    FOnEval : TMethodEvalEvent;
    FResultType : TDataType;
    FVisibility : TClassVisibility;
    procedure SetResultType(const Value: TDataType);
  protected
    function GetDisplayName: string; override;
    procedure Call(Caller: TdwsProgramExecution; Func: TFuncSymbol); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property Attributes: TMethodAttributes read FAttributes write FAttributes default [];
    property Visibility : TClassVisibility read FVisibility write FVisibility default cvPublic;
    property Kind: TMethodKind read FKind write FKind;
    property OnEval: TMethodEvalEvent read FOnEval write FOnEval;
    property ResultType: TDataType read FResultType write SetResultType;
  end;

   TdwsMethods = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsMethod;
   end;

  TdwsConstructor = class(TdwsFunction)
  private
    FAttributes: TMethodAttributes;
    FOnAssignExternalObject: TAssignExternalObjectEvent;
    FVisibility : TClassVisibility;
    function GetResultType: string;
  protected
    function GetDisplayName: string; override;
    procedure Call(Caller: TdwsProgramExecution; Func: TFuncSymbol); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property Visibility : TClassVisibility read FVisibility write FVisibility default cvPublic;
    property Attributes: TMethodAttributes read FAttributes write FAttributes default [];
    property OnEval: TAssignExternalObjectEvent read FOnAssignExternalObject write FOnAssignExternalObject;
    property ResultType: string read GetResultType;
  end;

   TdwsConstructors = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsConstructor;
   end;

   TdwsClassConstant = class(TdwsConstant)
      private
         FVisibility : TClassVisibility;
      protected
         function GetDisplayName: string; override;
      public
         constructor Create(Collection: TCollection); override;
         procedure Assign(Source: TPersistent); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
      published
         property Visibility : TClassVisibility read FVisibility write FVisibility default cvPublic;
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
         FAncestor: string;
         FConstructors: TdwsConstructors;
         FFields: TdwsFields;
         FMethods: TdwsMethods;
         FOnObjectDestroy: TObjectDestroyEvent;
         FProperties: TdwsProperties;
         FOperators : TdwsClassOperators;
         FConstants : TdwsClassConstants;
         FHelperObject : TObject;
         FIsSealed : Boolean;
         FIsAbstract : Boolean;
         FIsStatic : Boolean;

      protected
         function GetDisplayName: string; override;

      public
         constructor Create(Collection: TCollection); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;
         function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;

         {: User-side helper object, freed by the TdwsClass. }
         property HelperObject : TObject read FHelperObject write FHelperObject;

      published
         property Ancestor: string read FAncestor write FAncestor;
         property IsSealed : Boolean read FIsSealed write FIsSealed;
         property IsAbstract : Boolean read FIsAbstract write FIsAbstract;
         property IsStatic : Boolean read FIsStatic write FIsStatic;
         property Constructors: TdwsConstructors read FConstructors write FConstructors;
         property Fields: TdwsFields read FFields write FFields;
         property Methods: TdwsMethods read FMethods write FMethods;
         property Operators : TdwsClassOperators read FOperators write FOperators;
         property Constants : TdwsClassConstants read FConstants write FConstants;
         property Properties: TdwsProperties read FProperties write FProperties;
         property OnCleanUp: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
   end;

   TdwsClasses = class(TdwsCollection)
      protected
         class function GetSymbolClass : TdwsSymbolClass; override;
      public
         function Add : TdwsClass;
   end;

  TdwsClassesClass = class of TdwsClasses;

  TdwsMember = class(TdwsVariable)
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

  TdwsMembers = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
  end;

  TdwsRecord = class(TdwsSymbol)
  private
    FMembers: TdwsMembers;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  published
    property Members: TdwsMembers read FMembers write FMembers;
  end;

  TdwsRecords = class(TdwsCollection)
  protected
    class function GetSymbolClass: TdwsSymbolClass; override;
  end;

  TdwsRecordsClass = class of TdwsRecords;

  TdwsElement = class(TdwsSymbol)
  private
    FIsUserDef: Boolean;
    FUserDefValue: Integer;
    procedure SetUserDefValue(const Value: Integer);
    procedure SetIsUserDef(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
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
  end;

  TdwsEnumeration = class(TdwsSymbol)
  private
    FElements: TdwsElements;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property Elements: TdwsElements read FElements write FElements;
  end;

  TdwsEnumerations = class(TdwsCollection)
  protected
    class function GetSymbolClass: TdwsSymbolClass; override;
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

  TdwsInstances = class(TdwsCollection)
  protected
    class function GetSymbolClass : TdwsSymbolClass; override;
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
        FSynonyms: TdwsSynonyms;
        FVariables: TdwsVariables;
        FTable: TUnitSymbolTable;

      protected
        FCollections: array[0..9] of TdwsCollection;

        class function GetArraysClass: TdwsArraysClass; virtual;
        class function GetClassesClass: TdwsClassesClass; virtual;
        class function GetConstantsClass: TdwsConstantsClass; virtual;
        class function GetEnumerationsClass: TdwsEnumerationsClass; virtual;
        class function GetForwardsClass: TdwsForwardsClass; virtual;
        class function GetFunctionsClass: TdwsFunctionsClass; virtual;
        class function GetInstancesClass: TdwsInstancesClass; virtual;
        class function GetRecordsClass: TdwsRecordsClass; virtual;
        class function GetVariablesClass: TdwsVariablesClass; virtual;
        class function GetSynonymsClass: TdwsSynonymsClass; virtual;

        procedure SetArrays(const Value: TdwsArrays);
        procedure SetClasses(const Value: TdwsClasses);
        procedure SetConstants(const Value: TdwsConstants);
        procedure SetEnumerations(const Value: TdwsEnumerations);
        procedure SetForwards(const Value: TdwsForwards);
        procedure SetFunctions(const Value: TdwsFunctions);
        procedure SetRecords(const Value: TdwsRecords);
        procedure SetVariables(const Value: TdwsVariables);
        procedure SetInstances(const Value: TdwsInstances);
        procedure SetSynonyms(const Value: TdwsSynonyms);

        function StoreArrays : Boolean;
        function StoreClasses : Boolean;
        function StoreConstants : Boolean;
        function StoreEnumerations : Boolean;
        function StoreFunctions : Boolean;
        function StoreRecords : Boolean;
        function StoreVariables : Boolean;
        function StoreInstances : Boolean;
        function StoreSynonyms : Boolean;

      protected
        function GetSymbol(Table: TSymbolTable; const Name: string): TSymbol;
        procedure AddCollectionSymbols(Collection: TdwsCollection; Table: TSymbolTable); virtual;
        procedure AddUnitSymbols(Table: TSymbolTable); override;
        procedure InitUnitTable(SystemTable, UnitSyms: TSymbolTable; UnitTable: TUnitSymbolTable); override;

        // Method to support get/set property values for dynamicly registered classes
        procedure HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
        procedure HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);

      public
        procedure GetDataTypes(List: TStrings);
        procedure GetClassTypes(List: TStrings);
        procedure ExposeClassToUnit(AClass, AAncestor: TClass; ASearchProgram: TdwsProgram=nil; const ScriptAncestorType: string='');
        procedure ExposeInstanceToUnit(const AName, AClassType: string; AInstance: TObject);
        property Table: TUnitSymbolTable read FTable;

      published
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        property Arrays: TdwsArrays read FArrays write SetArrays stored StoreArrays;
        property Classes: TdwsClasses read FClasses write SetClasses stored StoreClasses;
        property Constants: TdwsConstants read FConstants write SetConstants stored StoreConstants;
        property Dependencies;
        property Enumerations: TdwsEnumerations read FEnumerations write SetEnumerations stored StoreEnumerations;
        property Forwards: TdwsForwards read FForwards write SetForwards stored False;
        property Functions: TdwsFunctions read FFunctions write SetFunctions stored StoreFunctions;
        property Instances: TdwsInstances read FInstances write SetInstances stored StoreInstances;
        property Records: TdwsRecords read FRecords write SetRecords stored StoreRecords;
        property Synonyms: TdwsSynonyms read FSynonyms write SetSynonyms stored StoreSynonyms;
        property UnitName;
        property Variables: TdwsVariables read FVariables write SetVariables stored StoreVariables;
        property StaticSymbols;
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
    procedure SetValue(const Data: TData);
  end;

  TWriteVarFunc = class(TAnonymousFunction)
  private
    FReadVarFunc: TReadVarFunc;
  public
    constructor Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
    procedure Execute(info : TProgramInfo); override;
  end;

// Return the external object for a variable name.
function GetExternalObjForID(Info: TProgramInfo; const AVarName: string): TObject;

// Get or create the DWS object ID (like a pointer) for a Delphi object instance.
//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: string = ''): Integer;

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

function ValueToString(const Value : Variant) : String;
begin
  case VarType(Value) of
    varEmpty : Result := 'Unassigned';
    varNull : Result := 'Null';
    varString, varUString, varOleStr, varStrArg : Result := Format('''%s''', [VarToStr(Value)]);
    varDate : Result := Format('DateTime(%f)', [TVarData(Value).VDate]);
  else
    Result := VarToStr(Value);
  end;
end;

function GetExternalObjForID(Info: TProgramInfo; const AVarName: string): TObject;
begin
  // Get param "Source" as object in Source_Obj
  Result := IScriptObj(IUnknown(Info.ValueAsVariant[AVarName])).ExternalObject;
end;

//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: string): Integer;
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
   FUnitName := 'Default';
   FCompiler := TdwsCompiler.Create;
   FConfig := TdwsConfiguration.Create(Self);
   AddUnit(Self);
   FExtensions := TdwsLanguageExtensionAggregator.Create;
end;

// Destroy
//
destructor TDelphiWebScript.Destroy;
begin
   inherited;
   FCompiler.Free;
   FConfig.Free;
   FExtensions.Free;
end;

function TDelphiWebScript.GetVersion: string;
begin
  Result := '2.2';
end;

procedure TDelphiWebScript.SetVersion(const Value: string);
begin
  // This method is needed to make the IDE show the version in
  // the object inspector
end;

// Compile
//
function TDelphiWebScript.Compile(const Text: string): IdwsProgram;
begin
   if FExtensions.Count>0 then begin
      FCompiler.OnReadInstr:=FExtensions.ReadInstr;
   end else begin
      FCompiler.OnReadInstr:=nil;
   end;

   Result := FCompiler.Compile(Text, FConfig);
end;

procedure TDelphiWebScript.AddUnit(const Un: IUnit);
begin
  RemoveUnit(Un);
  if Assigned(Un) then
    FConfig.Units.AddObject(Un.GetUnitName, Pointer(Un));
end;

function TDelphiWebScript.RemoveUnit(const Un: IUnit): Boolean;
var
  x: Integer;
begin
  x := FConfig.Units.IndexOfObject(Pointer(Un));
  if x >= 0 then
    FConfig.Units.Delete(x);
  Result := x >= 0;
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

function TdwsCollection.IndexOf(const Name: string): Integer;
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

procedure TdwsUnit.AddCollectionSymbols(Collection: TdwsCollection;
  Table: TSymbolTable);
var
  y: Integer;
  clsName : String;
begin
   // add all classes as forwards automatically if they aren't there already
   for y:=0 to FClasses.Count-1 do begin
      clsName:=FClasses.Items[y].Name;
      if FForwards.IndexOf(clsName)<0 then
         TdwsForward(Forwards.Add).Name:=clsName;
   end;

  for y := 0 to Collection.Count - 1 do
  begin
    if not TdwsSymbol(Collection.Items[y]).IsGenerating then
    try
      TdwsSymbol(Collection.Items[y]).Generate(Table);
    except
      on e: Exception do
        raise EGenerationError.CreateFmt(UNT_UnitGenerationError, [UnitName,
          e.Message]);
    end;
  end;
end;

procedure TdwsUnit.AddUnitSymbols(Table: TSymbolTable);
var
  x: Integer;
begin
  for x := Low(FCollections) to High(FCollections) do
    FCollections[x].Reset;

  for x := Low(FCollections) to High(FCollections) do
    AddCollectionSymbols(FCollections[x], Table);
end;

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
  FVariables := GetVariablesClass.Create(Self);
  FInstances := GetInstancesClass.Create(Self);
  FSynonyms := GetSynonymsClass.Create(Self);
  FCollections[0] := FForwards;
  FCollections[1] := FEnumerations;
  FCollections[2] := FArrays;
  FCollections[3] := FRecords;
  FCollections[4] := FClasses;
  FCollections[5] := FSynonyms;
  FCollections[6] := FFunctions;
  FCollections[7] := FVariables;
  FCollections[8] := FConstants;
  FCollections[9] := FInstances;
end;

destructor TdwsUnit.Destroy;
begin
  FArrays.Free;
  FClasses.Free;
  FConstants.Free;
  FEnumerations.Free;
  FForwards.Free;
  FRecords.Free;
  FFunctions.Free;
  FVariables.Free;
  FInstances.Free;
  FSynonyms.Free;
  inherited;
end;

procedure TdwsUnit.GetClassTypes(List: TStrings);
var
  x: Integer;
begin
  if not Assigned(List) then
    Exit;

  if Assigned(FScript) then
    for x := 0 to FScript.Config.SystemTable.Count - 1 do
    begin
      if FScript.Config.SystemTable[x] is TClassSymbol then
        List.Add(Script.Config.SystemTable[x].Name);
    end;

  for x := 0 to FClasses.Count - 1 do
    List.Add(FClasses.Items[x].Name);
end;

procedure TdwsUnit.GetDataTypes(List: TStrings);
var
  x, y: Integer;
  coll: TdwsCollection;
begin
  if not Assigned(List) then
    Exit;

  if Assigned(FScript) then
    // Add all type symbols from the systemtable
    for x := 0 to FScript.Config.SystemTable.Count - 1 do
    begin
      if FScript.Config.SystemTable[x] is TTypeSymbol then
        List.Add(FScript.Config.SystemTable[x].Name);
    end;

  // Only return array-, record- and class symbols, synonyms and enums
  for x := 1 to 5 do
  begin
    coll := FCollections[x];
    for y := 0 to coll.Count - 1 do
      List.Add(coll.Items[y].Name);
  end;
end;

function TdwsUnit.GetSymbol(Table: TSymbolTable; const Name: string): TSymbol;
{var
  x, y: Integer;
  item: TdwsSymbol;
  coll: TdwsCollection; }
begin
   Result:=Table.FindSymbol(Name, cvMagic);
   if not Assigned(Result) then
      raise EHandledGenerationError.CreateFmt('Symbol not found: %s in %s', [Name, self.Name]);
//      if FIs

{    for x := Low(FCollections) to High(FCollections) do
    begin
      // Check if the symbol is defined but not yet generated
      coll := FCollections[x];
      for y := 0 to coll.Count - 1 do
        if SameText(coll.Items[y].Name, Name) then
        begin
          item := coll.Items[y];

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
    end; }
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

class function TdwsUnit.GetVariablesClass: TdwsVariablesClass;
begin
  Result := TdwsVariables;
end;

class function TdwsUnit.GetSynonymsClass: TdwsSynonymsClass;
begin
  Result := TdwsSynonyms;
end;

procedure TdwsUnit.SetSynonyms(const Value: TdwsSynonyms);
begin
  FSynonyms.Assign(Value);
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

procedure TdwsUnit.HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);
var
  propName: string;
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
procedure TdwsUnit.ExposeClassToUnit(AClass, AAncestor: TClass;  ASearchProgram: TdwsProgram; const ScriptAncestorType: string);

    { Determine if the type is available to the program. If so, add the owning
       unit as a dependency. }
    function IsTypeSupported(const ATypeName: string): Boolean;
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
  getMethName, setMethName: string;
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
      propTypeData := GetTypeData(PropList^[i]^.PropType^);

      Include := True;
      if IsTypeSupported(String(PropList^[i]^.PropType^.Name)) then
        PropertyType := String(PropList^[i]^.PropType^.Name)
      else
      begin
        { NOTE: Could attempt to use the actual type name (ex: TComponentName is a string).
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
            if propTypeData^.BaseType^ = TypeInfo(Boolean) then
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

procedure TdwsUnit.ExposeInstanceToUnit(const AName, AClassType: string;
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

    externalVar := TExternalVarSymbol.Create(Name, typSym);
    externalVar.ReadFunc := funcSym;
    Table.AddSymbol(externalVar);
  end
  else
    raise Exception.CreateFmt(UNT_AutoInstantiateWithoutClass, [AClassType]);
end;

procedure TdwsUnit.InitUnitTable(SystemTable, UnitSyms: TSymbolTable; UnitTable: TUnitSymbolTable);
begin
  FTable := UnitTable;
  try
    inherited InitUnitTable(SystemTable, UnitSyms, UnitTable);
  finally
    FTable := nil;
  end;
end;

procedure TdwsUnit.HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
begin
  { TODO : If accepted, create a string declaration in appropriate unit. }
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
function TdwsConstant.GetDisplayName: string;
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

function TdwsVariable.GetDisplayName: string;
begin
  Result := Name + ' : ' + DataType;
end;

{ TdwsVariables }

function TdwsVariables.GetDisplayName: string;
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

  if (Assigned(FOnReadVar) or Assigned(FOnWriteVar)) then
  begin
    Result := TExternalVarSymbol.Create(Name, typSym);

    if Assigned(FOnReadVar) then
    begin
      funcSym := TFuncSymbol.Create('', fkFunction, 1);
      funcSym.Typ := typSym;

      readEventFunc := TReadVarEventFunc.Create(funcSym);
      readEventFunc.OnReadVar := FOnReadVar;

      funcSym.Executable := ICallable(readEventFunc);

      TExternalVarSymbol(Result).ReadFunc := funcSym;
    end;

    if Assigned(FOnWriteVar) then
    begin
      funcSym := TFuncSymbol.Create('', fkProcedure, 1);
      funcSym.AddParam(TParamSymbol.Create('Value', typSym));

      writeEventFunc := TWriteVarEventFunc.Create(funcSym);
      writeEventFunc.OnWriteVar := FOnWriteVar;

      funcSym.Executable := ICallable(writeEventFunc);

      TExternalVarSymbol(Result).WriteFunc := funcSym;
    end;
  end
  else
  begin
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
  scriptObj: TScriptObj;
  extObj: TObject;
begin
  if Assigned(FScriptObj) then
    // Instance was already created
    Info.ResultAsVariant := FScriptObj
  else
  begin
    // First access to this variable. Create object instance!
    scriptObj := TScriptObj.Create(FClassSym{, Info.Caller});
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

function TdwsParameter.GetDisplayName: string;
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

{ TdwsFunction }

constructor TdwsFunction.Create;
begin
  inherited;
  FParameters := TdwsParameters.Create(Self);
end;

destructor TdwsFunction.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure TdwsFunction.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   info: TProgramInfo;
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

// SetParameters
//
procedure TdwsFunction.SetParameters(const Value: TdwsParameters);
begin
  FParameters.Assign(Value);
end;

// StoreParameters
//
function TdwsFunction.StoreParameters : Boolean;
begin
   Result:=(FParameters.Count>0);
end;

function TdwsFunction._AddRef: Integer;
begin
  Result := -1;
end;

function TdwsFunction._Release: Integer;
begin
  Result := -1;
end;

function TdwsFunction.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function TdwsFunction.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  if ResultType <> '' then
    GetDataType(Table, ResultType);

  Result := TFuncSymbol.Generate(Table, Name, GetParameters(Table), ResultType);
  try
    TFuncSymbol(Result).Params.AddParent(Table);

    // Connect TdwsFunction to TFuncSymbol
    TFuncSymbol(Result).Executable := ICallable(Self);
    TFuncSymbol(Result).DeprecatedMessage:=Deprecated;
    GetUnit.Table.AddSymbol(Result);
  except
    Result.Free;
    raise;
  end;
end;

// GetParameters
//
function GetParameters(Symbol: TdwsSymbol; Parameters: TdwsParameters; Table: TSymbolTable): TParamArray;
var
   i, j, elemValue: Integer;
   name: string;
   paramSym, elemSym : TSymbol;
   param : TdwsParameter;
begin
   SetLength(Result, Parameters.Count);
   for i := 0 to Parameters.Count - 1 do begin
      param:=TdwsParameter(Parameters.Items[i]);
      name := param.Name;

      // Check wether parameter name is unique
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
            else elemValue:=TElementSymbol(elemSym).UserDefValue;
            Result[i].DefaultValue[0] := elemValue;
         end else Result[i].DefaultValue[0] := param.DefaultValue;
      end else Result[i].DefaultValue := nil;

      Symbol.GetUnit.GetSymbol(Table, Result[i].ParamType);
  end;
end;


function TdwsFunction.GetParameters(Table: TSymbolTable): TParamArray;
begin
  Result := dwsComp.GetParameters(Self,Parameters,Table);
end;

// GetDisplayName
//
function TdwsFunction.GetDisplayName: string;
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

procedure TdwsFunction.InitSymbol(Symbol: TSymbol);
begin
  if Assigned(FOnInitSymbol) then
    FOnInitSymbol(Self,Symbol);
end;

procedure TdwsFunction.InitExpression(Expr: TExprBase);
begin
  if Assigned(FOnInitExpr) then
    FOnInitExpr(Self,Expr);
end;

procedure TdwsFunction.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsFunction then
  begin
    FFuncType := TdwsFunction(Source).ResultType;
    FParameters.Assign(TdwsFunction(Source).Parameters);
  end;
end;

{ TdwsField }

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
function TdwsField.GetDisplayName: string;
begin
   Result:=TClassSymbol.VisibilityToString(Visibility)+' '+inherited GetDisplayName;
end;

{ TdwsMethod }

function TdwsMethod.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TClassSymbol(ParentSym).Members, Name);

  if ResultType <> '' then
    GetUnit.GetSymbol(Table, ResultType);

  Result := TMethodSymbol.Generate(Table, Kind, Attributes, Name,
    GetParameters(Table), ResultType, TClassSymbol(ParentSym), Visibility);
  try
    TFuncSymbol(Result).Params.AddParent(Table);
    TFuncSymbol(Result).DeprecatedMessage:=Deprecated;

    TMethodSymbol(Result).Executable := ICallable(Self);
  except
    Result.Free;
    raise;
  end;
end;

// GetDisplayName
//
function TdwsMethod.GetDisplayName: string;
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

procedure TdwsMethod.Call(Caller: TdwsProgramExecution; Func: TFuncSymbol);
var
  info : TProgramInfo;
  isClassMethod : Boolean;
  methodSymbol : TMethodSymbol;
begin
   if Assigned(FOnEval) then begin
      info:=Caller.AcquireProgramInfo(func);
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
         Caller.ReleaseProgramInfo(info);
      end;
   end;
end;

// Create
//
constructor TdwsMethod.Create(Collection: TCollection);
begin
   inherited;
   FVisibility:=cvPublic;
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

{ TdwsConstructor }

procedure TdwsConstructor.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsMethod then begin
    FAttributes := TdwsMethod(Source).Attributes;
    FVisibility := TdwsMethod(Source).Visibility;
  end;
end;

procedure TdwsConstructor.Call(Caller: TdwsProgramExecution; Func: TFuncSymbol);
var
   info: TProgramInfo;
   extObj: TObject;
begin
   info := Caller.AcquireProgramInfo(Func);
   try
      info.PrepareScriptObj;

      if Assigned(FOnAssignExternalObject) then begin
         if Assigned(info.ScriptObj) then begin
            extObj := info.ScriptObj.ExternalObject; // may assigned by Info.GetConstructor()
            FOnAssignExternalObject(info, extObj);
            info.ScriptObj.ExternalObject := extObj;
         end;
      end;
   finally
      Caller.ReleaseProgramInfo(info);
   end;
end;

constructor TdwsConstructor.Create(Collection: TCollection);
begin
  inherited;
  // Name the first constructor "Create" by default
  if Collection.Count = 1 then
    FName := 'Create';
  FVisibility:=cvPublic;
end;

function TdwsConstructor.DoGenerate(Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
begin
  FIsGenerating := True;
  CheckName(TClassSymbol(ParentSym).Members, Name);

  Result := TMethodSymbol.Generate(Table, mkConstructor, Attributes, Name,
    GetParameters(Table), '', TClassSymbol(ParentSym), Visibility);
  try
    TFuncSymbol(Result).Params.AddParent(Table);
    TMethodSymbol(Result).Executable := ICallable(Self);
  except
    Result.Free;
    raise;
  end;
end;

function TdwsConstructor.GetDisplayName: string;
begin
  Result := Parameters.GetDisplayName;

  if Result <> '' then
    Result := '(' + Result + ')';

  Result:=TClassSymbol.VisibilityToString(Visibility)+Format(' constructor %s%s;', [Name, Result]);
end;

function TdwsConstructor.GetResultType: string;
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
function TdwsClassConstant.GetDisplayName: string;
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
      classSym := TClassSymbol.Create(Name);

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

function TdwsClass.GetDisplayName: string;
begin
  if Ancestor <> '' then
    Result := Name + ' (' + Ancestor + ')'
  else
    Result := Name + ' (TObject)';
end;

{ TdwsMember }

function TdwsMember.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TRecordSymbol(ParentSym).Members, Name);
  Result := TMemberSymbol.Create(Name, GetDataType(Table, DataType));
end;

{ TdwsRecord }

procedure TdwsRecord.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdwsRecord then
    FMembers.Assign(TdwsRecord(Source).Members);
end;

constructor TdwsRecord.Create;
begin
  inherited;
  FMembers := TdwsMembers.Create(Self);
end;

destructor TdwsRecord.Destroy;
begin
  FMembers.Free;
  inherited;
end;

function TdwsRecord.DoGenerate;
var
  x: Integer;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  Result := TRecordSymbol.Create(Name);
  try
    for x := 0 to FMembers.Count - 1 do
      TRecordSymbol(Result).AddMember(TMemberSymbol(TdwsMember(FMembers.Items[x]).Generate(Table, Result)));
    GetUnit.Table.AddSymbol(Result);
  except
    Result.Free;
    raise;
  end;
end;

function TdwsRecord.GetDisplayName: string;
begin
  Result := 'Record ' + Name;
end;

{ TdwsArray }

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
    Result := TDynamicArraySymbol.Create(Name, GetDataType(Table, DataType))
  else
  begin
    if LowBound > HighBound then
      raise Exception.Create(UNT_InvalidArrayBounds);
    Result := TStaticArraySymbol.Create(Name, GetDataType(Table, DataType), LowBound, HighBound);
  end;
  GetUnit.Table.AddSymbol(Result);
end;

function TdwsArray.GetBoundStored: Boolean;
begin
  Result := not IsDynamic;
end;

function TdwsArray.GetDisplayName: string;
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

function TdwsProperty.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
var
  sym: TSymbol;
  propSym: TPropertySymbol;
  indexData: TData;
begin
  FIsGenerating := True;

  if DataType='' then
    raise Exception.CreateFmt(UNT_DatatypeNotSpecified, [Name, ParentSym.Name]);

  propSym := TPropertySymbol.Create(Name, GetDataType(Table, DataType), Visibility);
  Result := PropSym;

  propSym.GenerateParams(Table,GetParameters(Self, Parameters, Table));

  if FReadAccess <> '' then
  begin
    // ReadAccess
    sym := TClassSymbol(ParentSym).Members.FindLocal(FReadAccess);

    if not Assigned(sym) then
      raise Exception.CreateFmt(UNT_ReadAccessNotFound, [ReadAccess]);

    propSym.ReadSym := sym;
  end;

  if FWriteAccess <> '' then
  begin
    // WriteAccess
    sym := TClassSymbol(ParentSym).Members.FindLocal(FWriteAccess);

    if not Assigned(sym) then
      raise Exception.CreateFmt(UNT_WriteAccessNotFound, [WriteAccess]);

    propSym.WriteSym := sym;
  end;

  if FIndexType <> '' then
  begin
    SetLength(indexData,1);
    indexData[0] := FIndexValue;
    propSym.SetIndex(indexData,0,GetDataType(Table, IndexType));
  end;

  if IsDefault then
    TClassSymbol(ParentSym).DefaultProperty := propSym;
end;

// GetDisplayName
//
function TdwsProperty.GetDisplayName: string;
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

function TdwsClassOperator.GetDisplayName: string;
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

function TdwsSymbol.GetNamePath: string;
begin
  if FName <> '' then
    Result := Collection.GetNamePath + FName
  else
    Result := Collection.GetNamePath + IntToStr(Index);
end;

function TdwsSymbol.Generate;
begin
  try
    Result := DoGenerate(Table, ParentSym);
  except
    on e: EHandledGenerationError do
      raise;
    on e: Exception do
      raise Exception.CreateFmt(UNT_SymbolGenerationError, [ClassName, Name,
        e.Message]);
  end;
end;

function TdwsSymbol.GetDataType(Table: TSymbolTable; Name: string): TTypeSymbol;
var sym : TSymbol;
begin
  sym := GetUnit.GetSymbol(Table, Name);
  if not (sym is TTypeSymbol) then
    raise Exception.CreateFmt(UNT_DatatypeUnknown, [Name]);
  Result := TTypeSymbol(sym);
end;

procedure TdwsSymbol.CheckName(Table: TSymbolTable;
  Name: string);
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

   Result := TClassSymbol.Create(Name);
   TClassSymbol(Result).SetForwardedPos(cNullPos);
   GetUnit.Table.AddSymbol(Result);
end;

function TdwsForward.GetDisplayName: string;
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

procedure TReadVarFunc.SetValue(const Data: TData);
begin
  DWSCopyData(Data, 0, FData, 0, FTyp.Size);
end;

{ TWriteVarFunc }

constructor TWriteVarFunc.Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
begin
  inherited Create(FuncSym);
  FReadVarFunc := ReadVarFunc;
end;

procedure TWriteVarFunc.Execute(info : TProgramInfo);
begin
  FReadVarFunc.SetValue(Info.Data['Value']);
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

function TdwsAbstractUnit.GetUnitName: string;
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

procedure TdwsAbstractUnit.SetScript(const Value: TDelphiWebScript);
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

procedure TdwsAbstractUnit.SetUnitName(const Value: string);
begin
  if not (csDesigning in ComponentState) and Assigned(FScript)
     and not UnicodeSameText(Value, FUnitName) then
    raise Exception.Create(UNT_CantChangeUnitName)
  else
    FUnitName := Value;
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

function TdwsEmptyUnit.GetUnitName: string;
begin
  Result := FUnitName;
end;

function TdwsEmptyUnit.GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable;
var
  x: Integer;
  sym: TSymbol;
begin
  Result := TUnitSymbolTable.Create(SystemTable);
  try
    // insert links to units this unit depends of
    for x := 0 to FDependencies.Count - 1 do
    begin
      sym := UnitSyms.FindSymbol(FDependencies[x], cvMagic);
      Result.AddParent(TUnitSymbol(sym).Table);
      Result.AddSymbol(TUnitSymbol.Create(TUnitSymbol(sym).Name,
        TUnitSymbol(sym).Table));
    end;

    // create the symbols of this unit
    AddUnitSymbols(Result);

  except
    Result.Free;
    raise;
  end;
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
end;

destructor TdwsEnumeration.Destroy;
begin
  FElements.Free;
  inherited;
end;

function TdwsEnumeration.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
var
  x: Integer;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  Result := TEnumerationSymbol.Create(Name, Table.FindSymbol(SYS_INTEGER, cvMagic) as TTypeSymbol);
  try
    for x := 0 to FElements.Count - 1 do
      TEnumerationSymbol(Result).AddElement(
        TElementSymbol(TdwsElement(FElements.Items[x]).Generate(Table, Result)));
    GetUnit.Table.AddSymbol(Result);
  except
    Result.Free;
    raise;
  end;
end;

// GetDisplayName
//
function TdwsEnumeration.GetDisplayName: string;
var
   i : Integer;
begin
   Result:=Name+' = (';
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
    enumInt := TElementSymbol(enumSym.Elements[enumSym.Elements.Count - 1]).Data[0] + 1
  else
    enumInt := 0;

  Result := TElementSymbol.Create(Name, enumSym, enumInt, FIsUserDef);
  GetUnit.Table.AddSymbol(Result);
end;

function TdwsElement.GetDisplayName: string;
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

{ TdwsEnumerations }

class function TdwsEnumerations.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsEnumeration;
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

{ TdwsRecords }

class function TdwsRecords.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsRecord;
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

{ TdwsElements }

class function TdwsElements.GetSymbolClass: TdwsSymbolClass;
begin
  Result := TdwsElement;
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

function TdwsAbstractStaticUnit.GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable;
begin
  if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms) then
    Result := CreateUnitTable(FStaticTable, sttLinked) as TLinkedSymbolTable // typecheck
  else
  begin
    Result := CreateUnitTable(SystemTable); // sttDefault
    try
      InitUnitTable(SystemTable, UnitSyms, Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function TdwsAbstractStaticUnit.InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable): Boolean;
var
  staticParent: TStaticSymbolTable;
begin
  if not Assigned(FStaticTable) then
  begin
    if SystemTable is TStaticSymbolTable then
      staticParent := TStaticSymbolTable(SystemTable)
    else if SystemTable is TLinkedSymbolTable then
      staticParent := TLinkedSymbolTable(SystemTable).Parent
    else
      staticParent := nil;
      
    if Assigned(staticParent) then
    begin
      FStaticTable := CreateUnitTable(staticParent, sttStatic) as TStaticSymbolTable;
      try
        InitUnitTable(SystemTable, UnitSyms, FStaticTable);
      except
        ReleaseStaticSymbols;
        raise;
      end;
    end;
  end; // else check FSymbolTable = StaticTable
  Result := Assigned(FStaticTable);
end;

procedure TdwsAbstractStaticUnit.InitUnitTable(SystemTable, UnitSyms: TSymbolTable; UnitTable: TUnitSymbolTable);
var
  x: Integer;
  sym: TSymbol;
begin
  if UnitName = '' then
    raise Exception.CreateFmt(UNT_UnitNameNotDefined, [Name]);

  for x := 0 to FDependencies.Count - 1 do
  begin
    sym := UnitSyms.FindSymbol(FDependencies[x], cvMagic);
    try
      UnitTable.AddParent(TUnitSymbol(sym).Table);
    except
      on e: Exception do
        raise Exception.CreateFmt(UNT_DependencyError,[UnitName, Sym.Name, e.Message]);
    end;
    UnitTable.AddSymbol(TUnitSymbol.Create(TUnitSymbol(sym).Name,
      TUnitSymbol(sym).Table));
  end;

  AddUnitSymbols(UnitTable);
end;

procedure TdwsAbstractStaticUnit.ReleaseStaticSymbols;
var
  s: TStaticSymbolTable;
begin
  if Assigned(FStaticTable) then
  begin
    s := FStaticTable;
    FStaticTable := nil;
    s._Release;
  end;
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
    FScriptObj := TScriptObj.Create(FClassSym);
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

end.



