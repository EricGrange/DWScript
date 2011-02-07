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
unit dwsExprs;

interface

uses Classes, Variants, SysUtils, TypInfo, dwsSymbols, dwsErrors, dwsUtils,
   dwsStrings, dwsStack, SyncObjs, dwsFileSystem, dwsTokenizer;

const
   C_DefaultStackChunkSize = 4096;

type
   TRelOps = (roEqual, roUnEqual, roLess, roLessEqual, roMore, roMoreEqual);

   TRefKind = (rkObjRef, rkClassOfRef);

   TNoPosExpr = class;
   TNoResultExpr = class;
   TBlockInitExpr = class;
   TExpr = class;
   TNoPosExprList = class;
   TdwsProgram = class;
   IdwsProgram = interface;
   TdwsProgramExecution = class;
   TSymbolPositionList = class;
   TFuncExprBase = class;
   TScriptObj = class;
   TSourceConditions = class;
   TSourcePreConditions = class;
   TSourcePostConditions = class;

   TVariantDynArray = array of Variant;

   TExprList = array[0..MaxListSize - 1] of TExpr;
   PExprList = ^TExprList;
   PExpr = ^TExpr;

  // Interface for units
  IUnit = interface
    ['{8D534D12-4C6B-11D5-8DCB-0000216D9E86}']
    function GetUnitName: string;
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TUnitSymbolTable;
    function GetDependencies: TStrings;
  end;

  TScriptSourceType = (stMain, stInclude{, stUnit}); // stUnit is left for the future

  // A specific ScriptSource entry. The text of the script contained in that unit.
  TScriptSourceItem = class
  private
    FNameReference: string;
    FSourceFile: TSourceFile;
    FSourceType: TScriptSourceType;
  public
    constructor Create(const ANameReference: string; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
    property NameReference: string read FNameReference write FNameReference;
    property SourceFile: TSourceFile read FSourceFile;
    property SourceType: TScriptSourceType read FSourceType;
  end;

  // Manage a list of all the different Script Texts (files) used in the program.
  TScriptSourceList = class
  private
    FSourceList: TList;
    FMainScript: TScriptSourceItem;
    function GetSourceItem(Index: Integer): TScriptSourceItem;
    procedure SetSourceItem(Index: Integer; SourceItem: TScriptSourceItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const ANameReference: string; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
    function FindScriptSourceItem(const ScriptPos: TScriptPos): TScriptSourceItem; overload;
    function FindScriptSourceItem(SourceFile: TSourceFile): TScriptSourceItem; overload;
    function FindScriptSourceItem(const SourceFileName: string): TScriptSourceItem; overload;
    function IndexOf(const AScriptPos: TScriptPos): Integer; overload;
    function IndexOf(ASourceFile: TSourceFile): Integer; overload;
    function IndexOf(const SourceFileName: string): Integer; overload;
    function Count: Integer;
    property Items[Index: Integer]: TScriptSourceItem read GetSourceItem write SetSourceItem; default;
    property MainScript: TScriptSourceItem read FMainScript;
  end;

  { Describe how the symbol at the position is being used. suReference would be
    a typical usage of the symbol. }
  TSymbolUsage = (suForward, suDeclaration, suImplementation, suReference);
  TSymbolUsages = set of TSymbolUsage;

  TSymbolPosition = class
  private
    FOwnerList: TSymbolPositionList; // pointer back to owning list
    FScriptPos: TScriptPos;     // location of symbol instance in script
    FSymUsages: TSymbolUsages;  // how symbol is used at this location (mutiple uses possible, Functions are Delcared/Implemented at same spot)
    function GetSymbol: TSymbol;// get symbol from parent
  public
    constructor Create(AOwningList: TSymbolPositionList; const AScriptPos: TScriptPos; AUsages: TSymbolUsages);
    property Symbol: TSymbol read GetSymbol;     // get owner symbol
    property ScriptPos: TScriptPos read FScriptPos;
    property SymbolUsages: TSymbolUsages read FSymUsages write FSymUsages;
  end;

  {Re-list every symbol (pointer to it) and every position it is in in the script }
  TSymbolPositionList = class
  private
    FSymbol: TSymbol;       // pointer to the symbol
    FPosList: TList;        // list of positions where symbol is declared and used
    function GetPosition(Index: Integer): TSymbolPosition;
    procedure SetPosition(Index: Integer; SymPos: TSymbolPosition);
  protected
    // Used by TSymbolDictionary. Not meaningful to make public (symbol is known).
    function FindSymbolAtPosition(ACol, ALine: Integer; const sourceFile : String): TSymbol; overload;
  public
    constructor Create(ASymbol: TSymbol);
    destructor Destroy; override;
    procedure Add(const Pos: TScriptPos; UseTypes: TSymbolUsages);
    function FindUsage(SymbolUse: TSymbolUsage): TSymbolPosition;
    function Count: Integer;
    property Items[Index: Integer]: TSymbolPosition read GetPosition write SetPosition; default;
    property Symbol: TSymbol read FSymbol;
  end;

  { List all symbols in the script. Each symbol list contains a list of the
    positions where it was used. }
  TSymbolDictionary = class
  protected
    FSymbolList: TList;
    function GetList(Index: Integer): TSymbolPositionList;
    procedure SetList(Index: Integer; PosList: TSymbolPositionList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;  // clear the lists
    procedure Add(Sym: TSymbol; const Pos: TScriptPos; UseTypes: TSymbolUsages=[suReference]);
    procedure Remove(Sym: TSymbol); // remove references to the symbol
    function FindSymbolAtPosition(ACol, ALine: Integer; const sourceFile : String): TSymbol; overload;
    function FindSymbolPosList(Sym: TSymbol): TSymbolPositionList; overload;  // return list of symbol
    function FindSymbolPosList(const SymName: string): TSymbolPositionList; overload;  // return list of symbol
    function FindSymbolPosListOfType(const SymName: string; SymbolType: TSymbolClass): TSymbolPositionList; // return list of symbol given the desired type
    function FindSymbolUsage(Symbol: TSymbol; SymbolUse: TSymbolUsage): TSymbolPosition; overload;
    function FindSymbolUsage(const SymName: string; SymbolUse: TSymbolUsage): TSymbolPosition; overload;
    function FindSymbolUsageOfType(const SymName: string; SymbolType: TSymbolClass; SymbolUse: TSymbolUsage): TSymbolPosition;
    function Count: Integer;
    property Items[Index: Integer]: TSymbolPositionList read GetList write SetList; default;
  end;

  // Context within the script. (A block of code) Can be nested
  TContext = class
  private
    FParentContext: TContext;
    FParentSymbol: TSymbol;     // a parent symbol would be a procedure/method, etc.
    FSubContexts: TList;        // contexts that are inside of this one
    FEndPos: TScriptPos;
    FStartPos: TScriptPos;
    FData: Pointer;             // pointer to some data element (for users)
    FLocalTable: TSymbolTable;  // symbol table associated with the context (begin..end blocks, TProcedures, etc)
  public
    constructor Create(AParent: TContext; const AStartPos: TScriptPos; AParentSymbol: TSymbol);
    destructor Destroy; override;
    function IsPositionInContext(ACol, ALine: Integer; SourceFile: TSourceFile=nil): Boolean;
    function HasParentSymbolOfClass(SymbolType: TSymbolClass; SearchParents: Boolean): Boolean;
    property Parent: TContext read FParentContext;
    property ParentSym: TSymbol read FParentSymbol;
    property SubContexts: TList read FSubContexts;
    property StartPos: TScriptPos read FStartPos;
    property EndPos: TScriptPos read FEndPos;
    property Data: Pointer read FData write FData;
    property LocalTable: TSymbolTable read FLocalTable write FLocalTable;
  end;

  // Map the various script contexts. (Code blocks)
  TContextMap = class
  private
    FScriptContexts: TList;     // list of top-level contexts
    FCurrentContext: TContext;  // current context (used when adding and leaving)
  public
    constructor Create;
    destructor Destroy; override;
    { Push a context on to the stack - procedures have a symbol context.
      Standard Begin..end blocks do not have a ParentSymbol. }
    procedure OpenContext(const AStartPos: TScriptPos; AParentSymbol: TSymbol);
    { Pop a context off the stack }
    procedure CloseContext(const AEndPos: TScriptPos);
    function FindContext(AParentSymbol: TSymbol): TContext; overload;// return the first context group based on its parent
    function FindContext(ACol, ALine: Integer; SourceFile: TSourceFile=nil): TContext; overload;
    function FindContext(const ScriptPos: TScriptPos): TContext; overload;
    property Contexts: TList read FScriptContexts;
    property Current: TContext read FCurrentContext; 
  end;

  TProgramEvent = procedure (Prog: TdwsProgram) of object;

  TdwsResultType = class;

  TdwsResult = class
  private
    FResultType: TdwsResultType;
  protected
    constructor Create(ResultType: TdwsResultType); virtual;
    procedure InitializeProgram(Prog: TdwsProgram); virtual;
    procedure FinalizeProgram(Prog: TdwsProgram); virtual;
    property ResultType: TdwsResultType read FResultType;
  public
    procedure AddString(const str : String); virtual;
  end;

  TdwsResultType = class(TComponent)
  private
    FOnInitializeProgram: TProgramEvent;
    FOnFinalizeProgram: TProgramEvent;
  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); virtual;
    function CreateProgResult: TdwsResult; virtual;
  published
    property OnInitializeProgram: TProgramEvent read FOnInitializeProgram write FOnInitializeProgram;
    property OnFinalizeProgram: TProgramEvent read FOnFinalizeProgram write FOnFinalizeProgram;
  end;

   // TTerminatorThread
   //
   // Stops the script after given time (Timeout)
   TTerminatorThread = class(TThread)
      private
         FExecutionContext : TdwsProgramExecution;
         FEvent : TEvent;
         FMillisecondsToLive : Integer;

      protected
         procedure Execute; override;
         procedure DoTerminate; override;

      public
         constructor Create(anExecutionContext : TdwsProgramExecution; aMilliSecToLive : Integer);
         destructor Destroy; override;
   end;

   TProgramInfo = class;

   IdwsProgramExecution = interface (IdwsExecution)
      ['{D0603CA6-40E3-4CBA-9C75-BD87C7A84650}']
      function GetInfo : TProgramInfo;
      function GetResult : TdwsResult;
      function GetObjectCount : Integer;
      function GetProg : IdwsProgram;

      procedure Execute(aTimeoutMilliSeconds : Integer = 0); overload;
      procedure ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0); overload;
      procedure ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0); overload;

      procedure BeginProgram;
      procedure RunProgram(aTimeoutMilliSeconds : Integer);
      procedure Stop;
      procedure EndProgram;

      function GetCallStack : TExprBaseArray;

      property Prog : IdwsProgram read GetProg;
      property Info : TProgramInfo read GetInfo;
      property Result : TdwsResult read GetResult;
      property ObjectCount : Integer read GetObjectCount;
   end;

   IdwsProgram = interface
      ['{AD513983-F033-44AF-9F2B-9CFFF94B9BB3}']
      function GetMsgs : TdwsMessageList;
      function GetConditionalDefines : TStringList;
      function GetLineCount : Integer;
      function GetTable : TSymbolTable;
      function GetTimeoutMilliseconds : Integer;
      procedure SetTimeoutMilliseconds(const val : Integer);
      function GetDefaultUserObject : TObject;
      procedure SetDefaultUserObject(const val : TObject);
      function GetSymbolDictionary : TSymbolDictionary;
      function GetContextMap : TContextMap;
      function GetSourceList : TScriptSourceList;
      function GetProgramObject : TdwsProgram;

      function CreateNewExecution : IdwsProgramExecution;
      function BeginNewExecution : IdwsProgramExecution;
      function Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
      function ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;
      function ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;

      property Table : TSymbolTable read GetTable;
      property Msgs : TdwsMessageList read GetMsgs;
      property ConditionalDefines : TStringList read GetConditionalDefines;
      property TimeoutMilliseconds : Integer read GetTimeoutMilliseconds write SetTimeoutMilliseconds;
      property DefaultUserObject : TObject read GetDefaultUserObject write SetDefaultUserObject;

      property SymbolDictionary : TSymbolDictionary read GetSymbolDictionary;
      property ContextMap : TContextMap read GetContextMap;
      property SourceList : TScriptSourceList read GetSourceList;
      property ProgramObject : TdwsProgram read GetProgramObject;
      property LineCount : Integer read GetLineCount;
   end;

   // holds execution context for a script
   TdwsProgramExecution = class (TdwsExecution, IdwsProgramExecution)
      private
         FProg : TdwsProgram;
         FCurrentProg : TdwsProgram;

         FFirstObject, FLastObject : TScriptObj;
         FObjectCount : Integer;
         FProgramInfo : TProgramInfo;
         FProgInfoPool : TProgramInfo;

         FParameters : TData;
         FResult : TdwsResult;
         FFileSystem : IdwsFileSystem;

         FMsgs : TdwsMessageList;

      protected
         procedure ReleaseObjects;

         procedure ScriptObjCreated(scriptObj: TScriptObj);
         procedure ScriptObjDestroyed(scriptObj: TScriptObj);
         procedure DestroyScriptObj(const scriptObj: IScriptObj);

         function GetMsgs : TdwsMessageList; override;

         // for interface only, script exprs use direct properties
         function GetProg : IdwsProgram;
         function GetInfo : TProgramInfo;
         function GetResult : TdwsResult;
         function GetObjectCount : Integer;

      public
         constructor Create(aProgram : TdwsProgram; const stackParams : TStackParameters);
         destructor Destroy; override;

         procedure Execute(aTimeoutMilliSeconds : Integer = 0); overload;
         procedure ExecuteParam(const Params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0); overload;
         procedure ExecuteParam(const Params : OleVariant; aTimeoutMilliSeconds : Integer = 0); overload;

         procedure BeginProgram;
         procedure RunProgram(aTimeoutMilliSeconds : Integer);
         procedure Stop;
         procedure EndProgram;

         function GetCallStack : TExprBaseArray;
         function CallStackToString(const callStack : TExprBaseArray) : String;
         procedure RaiseAssertionFailed(const msg : String; const scriptPos : TScriptPos);
         procedure RaiseAssertionFailedFmt(const fmt : String; const args : array of const; const scriptPos : TScriptPos);

         function AcquireProgramInfo(funcSym : TFuncSymbol) : TProgramInfo;
         procedure ReleaseProgramInfo(info : TProgramInfo);

         property Prog : TdwsProgram read FProg;
         property CurrentProg : TdwsProgram read FCurrentProg;
         property ProgramInfo : TProgramInfo read FProgramInfo;

         property Parameters : TData read FParameters;
         property Result : TdwsResult read FResult;
         property FileSystem : IdwsFileSystem read FFileSystem;

         property ObjectCount : Integer read FObjectCount;
         property Msgs : TdwsMessageList read FMsgs;
   end;

   TdwsProgramBaseTypes = record
      FTypBoolean: TTypeSymbol;
      FTypFloat: TTypeSymbol;
      FTypInteger: TTypeSymbol;
      FTypNil: TNilSymbol;
      FTypObject: TClassSymbol;
      FTypString: TTypeSymbol;
      FTypVariant: TTypeSymbol;
      FTypException: TClassSymbol;
   end;

   // A script executable program
   TdwsProgram = class (TInterfacedObject, IdwsProgram)
      private
         FContextMap: TContextMap;
         FExpr: TExpr;
         FInitExpr: TBlockInitExpr;
         FAddrGenerator: TAddrGeneratorRec;
         FGlobalAddrGenerator: TAddrGeneratorRec;
         FCompileMsgs: TdwsCompileMessageList;
         FSourceFiles : TTightList;
         FParent: TdwsProgram;
         FResultType: TdwsResultType;
         FRoot: TdwsProgram;
         FUnifiedConstList: TSortedList<TExprBase>;
         FRootTable: TProgramSymbolTable;
         FBinaryOperators : TObject;
         FSourceList: TScriptSourceList;
         FSymbolDictionary: TSymbolDictionary;
         FTable: TSymbolTable;
         FTimeoutMilliseconds: Integer;
         FCompiler : TObject;
         FRuntimeFileSystem : TdwsCustomFileSystem;
         FConditionalDefines : TStringList;
         FBaseTypes : TdwsProgramBaseTypes;
         FLineCount : Integer;
         FDefaultUserObject : TObject;

         FStackParameters : TStackParameters;

         FExecutions : TTightList;
         FExecutionsLock : TCriticalSection;

      protected
         function GetLevel: Integer; inline;
         procedure SetConditionalDefines(const val : TStringList);

         procedure NotifyExecutionDestruction(exec : TdwsProgramExecution);

         // for interface only, script exprs use direct properties
         function GetMsgs : TdwsMessageList;
         function GetConditionalDefines : TStringList;
         function GetLineCount : Integer;
         function GetTable : TSymbolTable;
         function GetTimeoutMilliseconds : Integer;
         procedure SetTimeoutMilliseconds(const val : Integer);
         function GetDefaultUserObject : TObject;
         procedure SetDefaultUserObject(const val : TObject);
         function GetSymbolDictionary : TSymbolDictionary;
         function GetContextMap : TContextMap;
         function GetSourceList : TScriptSourceList;
         function GetProgramObject : TdwsProgram;

      public
         constructor Create(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                            const stackParameters : TStackParameters);
         destructor Destroy; override;

         function CreateNewExecution : IdwsProgramExecution;
         function BeginNewExecution : IdwsProgramExecution;
         function Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
         function ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;
         function ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;

         function GetGlobalAddr(DataSize: Integer): Integer;
         function GetTempAddr(DataSize: Integer = -1): Integer;
         function NextStackLevel(level : Integer) : Integer;

         function RegisterSourceFile(const sourceFile : String; const sourceCode : String) : TSourceFile;
         function GetSourceFile(const aSourceFile : String) : TSourceFile;

         property Compiler: TObject read FCompiler write FCompiler;
         property RuntimeFileSystem : TdwsCustomFileSystem read FRuntimeFileSystem write FRuntimeFileSystem;
         property ConditionalDefines : TStringList read FConditionalDefines write SetConditionalDefines;
         property LineCount : Integer read FLineCount write FLineCount;
         property DefaultUserObject : TObject read FDefaultUserObject write FDefaultUserObject;

         property MaxRecursionDepth : Integer read FStackParameters.MaxRecursionDepth write FStackParameters.MaxRecursionDepth;
         property MaxDataSize : Integer read FStackParameters.MaxByteSize write FStackParameters.MaxByteSize;
         property StackChunkSize : Integer read FStackParameters.ChunkSize write FStackParameters.ChunkSize;

         property TimeoutMilliseconds : Integer read FTimeoutMilliseconds write FTimeoutMilliseconds;

         property Expr: TExpr read FExpr write FExpr;
         property InitExpr: TBlockInitExpr read FInitExpr;
         property Level: Integer read GetLevel;
         property CompileMsgs: TdwsCompileMessageList read FCompileMsgs write FCompileMsgs;
         property Parent: TdwsProgram read FParent;
         property Root: TdwsProgram read FRoot write FRoot;

         property RootTable: TProgramSymbolTable read FRootTable;
         property Table: TSymbolTable read FTable write FTable;
         property BinaryOperators : TObject read FBinaryOperators write FBinaryOperators;

         property UnifiedConstList: TSortedList<TExprBase> read FUnifiedConstList;

         property TypBoolean: TTypeSymbol read FBaseTypes.FTypBoolean;
         property TypFloat: TTypeSymbol read FBaseTypes.FTypFloat;
         property TypInteger: TTypeSymbol read FBaseTypes.FTypInteger;
         property TypNil: TNilSymbol read FBaseTypes.FTypNil;
         property TypObject: TClassSymbol read FBaseTypes.FTypObject;
         property TypString: TTypeSymbol read FBaseTypes.FTypString;
         property TypVariant: TTypeSymbol read FBaseTypes.FTypVariant;
         property TypException: TClassSymbol read FBaseTypes.FTypException;

         property SymbolDictionary: TSymbolDictionary read FSymbolDictionary;
         property ContextMap: TContextMap read FContextMap;
         property SourceList: TScriptSourceList read FSourceList;
   end;

   // Functions callable from a script program implement this interfaces
   ICallable = interface (IExecutable)
      ['{8D534D15-4C6B-11D5-8DCB-0000216D9E86}']
      procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol);
   end;

   // A script procedure
   TdwsProcedure = class (TdwsProgram, IUnknown, ICallable)
      private
         FFunc : TFuncSymbol;
         FPreConditions : TSourcePreConditions;
         FPostConditions : TSourcePostConditions;

      public
         constructor Create(Parent: TdwsProgram);
         destructor Destroy; override;

         procedure AssignTo(sym: TFuncSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
         procedure InitSymbol(Symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);

         property Func: TFuncSymbol read FFunc write FFunc;

         property PreConditions : TSourcePreConditions read FPreConditions write FPreConditions;
         property PostConditions : TSourcePostConditions read FPostConditions write FPostConditions;
   end;

   // Base class of all expressions attached to a program
   TNoPosExpr = class(TExprBase)
      private
         FProg : TdwsProgram;

      protected
         FTyp : TSymbol;

      public
         constructor Create(Prog: TdwsProgram);

         function IsBooleanValue : Boolean;
         function IsIntegerValue : Boolean;
         function IsFloatValue : Boolean;
         function IsNumberValue : Boolean;
         function IsStringValue : Boolean;
         function IsVariantValue : Boolean;

         function GetBaseType : TTypeSymbol;

         procedure EvalNoResult(exec : TdwsExecution); virtual;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
         procedure EvalAsString(exec : TdwsExecution; var Result : String); override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj); override;

         procedure AssignValue(exec : TdwsExecution; const value : Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value: String); override;

         procedure Initialize; virtual;
         procedure TypeCheckNoPos(const aPos : TScriptPos); virtual;
         function IsConstant : Boolean; virtual;
         function Optimize(exec : TdwsExecution) : TNoPosExpr; virtual;
         function OptimizeIntegerConstantToFloatConstant(exec : TdwsExecution) : TNoPosExpr;

         function ScriptPos : TScriptPos; override;
         function ScriptLocation : String; override;

         procedure RaiseScriptError(e : EScriptError); overload;
         procedure RaiseScriptError; overload;
         procedure RaiseScriptError(const msg : String); overload;
         procedure RaiseScriptError(exceptClass : EScriptErrorClass; const msg : String); overload;
         procedure RaiseScriptError(exceptClass : EScriptErrorClass; const msg : String;
                                    const args : array of const); overload;

         procedure RaiseUpperExceeded(index : Integer);
         procedure RaiseLowerExceeded(index : Integer);

         procedure CheckScriptObject(const scriptObj : IScriptObj); inline;
         procedure RaiseObjectNotInstantiated;
         procedure RaiseObjectAlreadyDestroyed;

         property Prog : TdwsProgram read FProg;
         property Typ: TSymbol read FTyp write FTyp;
         property BaseType: TTypeSymbol read GetBaseType;
   end;

   TNoPosExprClass = class of TNoPosExpr;

   // TExpr
   //
   TExpr = class (TNoPosExpr)
      protected
         FPos: TScriptPos;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);

         procedure TypeCheck;

         function ScriptPos : TScriptPos; override;

         property Pos: TScriptPos read FPos;
   end;

   TNoResultExpr = class(TExpr)
      function Eval(exec : TdwsExecution) : Variant; override;
      procedure EvalNoResult(exec : TdwsExecution); override;
      function OptimizeToNoResultExpr(exec : TdwsExecution) : TNoResultExpr;
   end;

   // Does nothing! E. g.: "for x := 1 to 10 do {TNullExpr};"
   TNullExpr = class(TNoResultExpr)
      procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // statement; statement; statement;
   TBlockExprBase = class(TNoResultExpr)
      protected
         FStatements : PExprList;
         FCount : Integer;

      public
         destructor Destroy; override;

         procedure AddStatement(expr : TExpr);
         procedure AddStatementFirst(expr : TExpr);

         procedure Initialize; override;
   end;

   // statement; statement; statement;
   TBlockInitExpr = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

  // Encapsulates data
   TDataExpr = class(TNoPosExpr)
      protected
         function GetAddr(exec : TdwsExecution) : Integer; virtual;
         function GetData(exec : TdwsExecution) : TData; virtual; abstract;

      public
         constructor Create(Prog: TdwsProgram; Typ: TSymbol);

         procedure AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer); virtual;
         procedure AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr); virtual;
         procedure AssignExpr(exec : TdwsExecution; Expr: TNoPosExpr); virtual;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value: String); override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function IsWritable: Boolean; virtual;

         property Addr[exec : TdwsExecution] : Integer read GetAddr;
         property Data[exec : TdwsExecution] : TData read GetData;
   end;

   // Encapsulates data
   TPosDataExpr = class(TDataExpr)
      protected
         FPos: TScriptPos;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Typ: TSymbol);

         procedure TypeCheck;

         procedure AddCompilerErrorFmt(const fmtText: string; const Args: array of const);
         procedure AddCompilerStop(const Text : String); overload;

         function ScriptPos : TScriptPos; override;

         property Pos: TScriptPos read FPos;
   end;

   // TFuncExprBase
   //
   TFuncExprBase = class(TPosDataExpr)
      protected
         FArgs: TExprBaseListRec;
         FFunc: TFuncSymbol;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TFuncSymbol);
         destructor Destroy; override;
         function AddArg(arg : TNoPosExpr) : TSymbol; virtual; abstract;
         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
         procedure Initialize; override;
         function GetArgs : TExprBaseList;
         function Optimize(exec : TdwsExecution) : TNoPosExpr; override;
         function IsConstant : Boolean; override;
         property FuncSym: TFuncSymbol read FFunc;
   end;

   TPushOperatorType = (potUnknown,
                        potAddr, potTempAddr, potTempArrayAddr, potTempArray,
                        potResult,
                        potResultInteger, potResultFloat, potResultBoolean,
                        potResultString, potResultConstString,
                        potData, potLazy, potInitResult);
   PPushOperator = ^TPushOperator;
   TPushOperator = packed record
      FStackAddr: Integer;
      FArgExpr: TNoPosExpr;
      FTypeParamSym: TSymbol;  // TSymbol / TPushOperatorType union

      procedure InitPushAddr(StackAddr: Integer; ArgExpr: TNoPosExpr);
      procedure InitPushTempAddr(StackAddr: Integer; ArgExpr: TNoPosExpr);
      procedure InitPushTempArrayAddr(StackAddr: Integer; ArgExpr: TNoPosExpr);
      procedure InitPushTempArray(StackAddr: Integer; ArgExpr: TNoPosExpr);
      procedure InitPushResult(StackAddr: Integer; ArgExpr: TNoPosExpr);
      procedure InitPushData(StackAddr: Integer; ArgExpr: TNoPosExpr; ParamSym: TSymbol);
      procedure InitPushInitResult(StackAddr: Integer; ArgExpr: TNoPosExpr);
      procedure InitPushLazy(StackAddr: Integer; ArgExpr: TNoPosExpr);

      procedure Execute(exec : TdwsExecution); inline;

      procedure ExecuteAddr(exec : TdwsExecution);
      procedure ExecuteTempAddr(exec : TdwsExecution);
      procedure ExecuteTempArrayAddr(exec : TdwsExecution);
      procedure ExecuteTempArray(exec : TdwsExecution);
      procedure ExecuteResult(exec : TdwsExecution);
      procedure ExecuteResultBoolean(exec : TdwsExecution);
      procedure ExecuteResultInteger(exec : TdwsExecution);
      procedure ExecuteResultFloat(exec : TdwsExecution);
      procedure ExecuteResultString(exec : TdwsExecution);
      procedure ExecuteResultConstString(exec : TdwsExecution);
      procedure ExecuteData(exec : TdwsExecution);
      procedure ExecuteInitResult(exec : TdwsExecution);
      procedure ExecuteLazy(exec : TdwsExecution);
   end;
   TPushOperatorArray = packed array [0..0] of TPushOperator;
   PPushOperatorArray = ^TPushOperatorArray;

   // Function call: func(arg0, arg1, ...);
   TFuncExpr = class (TFuncExprBase)
      private
         FPushExprs : PPushOperatorArray;
         FPushExprsCount : SmallInt;
         FLevel : SmallInt;
         FResultAddr : Integer;

      protected
         function PreCall(exec : TdwsExecution) : TFuncSymbol; virtual;
         function PostCall(exec : TdwsExecution) : Variant; virtual;

         procedure AddPushExprs;
         procedure EvalPushExprs(exec : TdwsExecution); inline;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TFuncSymbol;
                            exec : TdwsProgramExecution = nil);
         destructor Destroy; override;

         function AddArg(arg : TNoPosExpr) : TSymbol; override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;
         procedure Initialize; override;
         procedure SetResultAddr(exec : TdwsExecution; ResultAddr: Integer = -1);
         function IsWritable : Boolean; override;
   end;

  TMethodObjExpr = class(TPosDataExpr)
  private
    FBaseExpr : TDataExpr;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr);
    function GetData(exec : TdwsExecution) : TData; override;
    function GetAddr(exec : TdwsExecution) : Integer; override;
  end;

   TSourceCondition = class (TInterfacedObject, IBooleanEvalable, IStringEvalable)
      private
         FPos : TScriptPos;
         FTest : TNoPosExpr;
         FMsg : TNoPosExpr;

      public
         constructor Create(const pos : TScriptPos; aTest, aMsg : TNoPosExpr);
         destructor Destroy; override;

         procedure InitSymbol(Symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);

         function EvalAsBoolean(exec : TdwsExecution) : Boolean;
         procedure EvalAsString(exec : TdwsExecution; var Result : String);

         property Pos : TScriptPos read FPos write FPos;
         property Test : TNoPosExpr read FTest;
         property Msg : TNoPosExpr read FMsg write FMsg;
   end;

   TSourceConditions = class
      private
         FProg : TdwsProcedure;
         FItems : TTightList;
         FAncestor : TSourceConditions;

      public
         constructor Create(aProg : TdwsProcedure);
         destructor Destroy; override;

         procedure AddCondition(condition : TSourceCondition);

         procedure RaiseConditionFailed(exec : TdwsExecution; const scriptPos : TScriptPos;
                                        const msg : IStringEvalable); virtual; abstract;
         function  Test(exec : TdwsExecution) : TSourceCondition;
         procedure EvalNoResult(exec : TdwsExecution); virtual;

         property Ancestor : TSourceConditions read FAncestor write FAncestor;
   end;
   TSourceConditionsClass = class of TSourceConditions;

   TSourcePreConditions = class (TSourceConditions)
      public
         procedure RaiseConditionFailed(exec : TdwsExecution; const scriptPos : TScriptPos;
                                        const msg : IStringEvalable); override;
   end;

   TSourcePostConditions = class (TSourceConditions)
      public
         procedure RaiseConditionFailed(exec : TdwsExecution; const scriptPos : TScriptPos;
                                        const msg : IStringEvalable); override;
   end;

   TSourceMethodPreConditions = class (TSourcePreConditions)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;
   TSourceMethodPostConditions = class (TSourcePostConditions)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

  TConnectorCallExpr = class(TPosDataExpr)
  protected
    FArgs: TTightList;
    FBaseExpr: TNoPosExpr;
    FConnectorArgs: TConnectorArgs;
    FConnectorCall: IConnectorCall;
    FConnectorParams: TConnectorParamArray;
    FIsInstruction: Boolean;
    FIsWritable: Boolean;
    FIsIndex: Boolean;
    FName: string;
    FResultData: TData;
    function GetData(exec : TdwsExecution) : TData; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: string;
                       BaseExpr: TNoPosExpr; IsWritable: Boolean = True; IsIndex: Boolean = False);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    function AddArg(ArgExpr: TNoPosExpr) : TSymbol;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
    function Eval(exec : TdwsExecution) : Variant; override;
    procedure Initialize; override;
    function IsWritable : Boolean; override;
  end;

  TConnectorReadExpr = class(TPosDataExpr)
  protected
    FBaseExpr: TNoPosExpr;
    FConnectorMember: IConnectorMember;
    FName: string;
    FResultData: TData;
    function GetData(exec : TdwsExecution) : TData; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: string;
                       BaseExpr: TNoPosExpr);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    function Eval(exec : TdwsExecution) : Variant; override;
    procedure Initialize; override;
    property BaseExpr: TNoPosExpr write FBaseExpr;
  end;

  TConnectorWriteExpr = class(TNoResultExpr)
  private
    FBaseExpr: TNoPosExpr;
    FValueExpr: TNoPosExpr;
    FConnectorMember: IConnectorMember;
    FName: string;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: string;
                       BaseExpr, ValueExpr: TNoPosExpr);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    procedure EvalNoResult(exec : TdwsExecution); override;
    procedure Initialize; override;
  end;

   // Call of a method
   TMethodExpr = class abstract (TFuncExpr)
      private
         FBaseExpr: TDataExpr;
         FSelfAddr: Integer;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            BaseExpr: TDataExpr);
         destructor Destroy; override;

         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
         procedure Initialize; override;

         property BaseExpr: TDataExpr read FBaseExpr;
   end;

   // Call of static methods (not virtual)
   TMethodStaticExpr = class(TMethodExpr)
      protected
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;

      public
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Call to a virtual method
   TMethodVirtualExpr = class(TMethodStaticExpr)
      protected
         function FindVirtualMethod(classSym : TClassSymbol): TMethodSymbol; inline;
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;

      public
   end;

   // Class methods (non virtual)
   TClassMethodStaticExpr = class(TMethodStaticExpr)
      protected
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;
   end;

   // Call to a virtual class method
   TClassMethodVirtualExpr = class(TClassMethodStaticExpr)
      protected
         function FindVirtualMethod(exec : TdwsExecution) : TMethodSymbol;
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;
   end;

   TConstructorStaticExpr = class(TMethodStaticExpr)
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            Base: TDataExpr);
   end;

   TConstructorVirtualExpr = class(TMethodVirtualExpr)
      private
         FExternalObject: TObject;
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            Base: TDataExpr);
         property ExternalObject: TObject read FExternalObject write FExternalObject;
   end;

   TConstructorStaticObjExpr = class(TMethodStaticExpr)
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            BaseExpr: TDataExpr);
   end;

   TConstructorVirtualObjExpr = class(TMethodVirtualExpr)
      protected
         function PostCall(exec : TdwsExecution): Variant; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            Base: TDataExpr);
   end;

   TDestructorStaticExpr = class(TMethodStaticExpr)
   end;

   TDestructorVirtualExpr = class(TMethodVirtualExpr)
   end;

   TUnaryOpExpr = class(TNoPosExpr)
      protected
         FExpr: TNoPosExpr;
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         destructor Destroy; override;
         procedure Initialize; override;
         function IsConstant : Boolean; override;
         property Expr: TNoPosExpr read FExpr write FExpr;
   end;

   // bool unary result
   TUnaryOpBoolExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // int unary result
   TUnaryOpIntExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // float unary result
   TUnaryOpFloatExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // string unary result
   TUnaryOpStringExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval(exec : TdwsExecution) : Variant; override;
   end;


  TNoResultWrapperExpr = class(TNoResultExpr)
  protected
    FExpr: TNoPosExpr;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure EvalNoResult(exec : TdwsExecution); override;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
    function IsConstant : Boolean; override;
    property Expr: TNoPosExpr read FExpr write FExpr;
  end;

   // left "op" right
   TBinaryOpExpr = class(TNoPosExpr)
      protected
         FLeft : TNoPosExpr;
         FRight : TNoPosExpr;
      public
         constructor Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr); virtual;
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure Initialize; override;
         function IsConstant : Boolean; override;

         property Left : TNoPosExpr read FLeft write FLeft;
         property Right : TNoPosExpr read FRight write FRight;
   end;

   TBinaryOpExprClass = class of TBinaryOpExpr;

   // A list of no pos expressions
   TNoPosExprList = class
      protected
         FList : TTightList;

         function GetExpr(const x: Integer): TNoPosExpr;
         procedure SetExpr(const x: Integer; const Value: TNoPosExpr);
         function GetCount : Integer;

      public
         destructor Destroy; override;

         function AddExpr(AExpr: TNoPosExpr) : TSymbol;
         procedure Insert0(expr : TExprBase);
         procedure Delete(index : Integer);

         procedure Initialize;
         procedure TypeCheck(const pos : TScriptPos; ExpectedTyp : TSymbol);

         property Expr[const x: Integer]: TNoPosExpr read GetExpr write SetExpr; default;
         property Count : Integer read GetCount;
   end;

   // Helper object for access to symbols
   IInfo = interface
      ['{8D534D16-4C6B-11D5-8DCB-0000216D9E86}']
      function Call: IInfo; overload;
      function Call(const Params: array of Variant): IInfo; overload;
      function Element(const Indices: array of Integer): IInfo;
      function GetConstructor(const MethName: string; ExtObject: TObject): IInfo;
      function GetData : TData;
      function GetExternalObject: TObject;
      function GetMember(const s: string): IInfo;
      function GetMethod(const s: string): IInfo;
      function GetScriptObj: IScriptObj;
      function GetParameter(const s: string): IInfo;
      function GetTypeSym: TSymbol;
      function GetValue : Variant;
      function GetValueAsString : String;
      function GetValueAsDataString : RawByteString;
      function GetValueAsInteger : Int64;
      function GetValueAsFloat : Double;
      function GetInherited: IInfo;
      procedure SetData(const Data: TData);
      procedure SetExternalObject(ExtObject: TObject);
      procedure SetValue(const Value: Variant);

      property Data: TData read GetData write SetData;
      property ExternalObject: TObject read GetExternalObject write SetExternalObject;
      property Member[const s: string]: IInfo read GetMember;
      property Method[const s: string]: IInfo read GetMethod;
      property ScriptObj: IScriptObj read GetScriptObj;
      property Parameter[const s: string]: IInfo read GetParameter;
      property TypeSym: TSymbol read GetTypeSym;
      property Value: Variant read GetValue write SetValue;
      property ValueAsString : String read GetValueAsString;
      property ValueAsDataString : RawByteString read GetValueAsDataString;
      property ValueAsInteger : Int64 read GetValueAsInteger;
      property ValueAsFloat : Double read GetValueAsFloat;
   end;

   // Informations about the program in external procedures
   TProgramInfo = class
      private
         FExecution : TdwsProgramExecution;
         FFuncSym : TFuncSymbol;
         FLevel: Integer;
         FScriptObj: IScriptObj;
         FTable: TSymbolTable;

      protected
         function GetData(const s: string): TData;
         function GetFunc(const s: string): IInfo;
         procedure SetFuncSym(const Value: TFuncSymbol);
         function GetValueAsVariant(const s: string): Variant;
         function GetVars(const str: string): IInfo;
         procedure SetData(const s: string; const Value: TData);
         procedure SetValueAsVariant(const s: string; const Value: Variant);
         procedure SetResultAsVariant(const Value: Variant);
         function GetResultAsVariant: Variant;
         function GetResultVars: IInfo;

         function GetValueAsString(const s: string): String;
         procedure SetValueAsString(const s: string; const Value: String);
         function GetValueAsChar(const s: string): Char;
         function GetValueAsDataString(const s: string): RawByteString;
         procedure SetValueAsDataString(const s: string; const Value: RawByteString);
         function GetValueAsInteger(const s: string): Int64;
         procedure SetValueAsInteger(const s: string; const Value: Int64);
         function GetValueAsBoolean(const s: string): Boolean;
         procedure SetValueAsBoolean(const s: string; const Value: Boolean);
         function GetValueAsFloat(const s: string): Double;
         procedure SetValueAsFloat(const s: string; const Value: Double);
         function GetValueAsObject(const s: String): TObject;
         function GetValueAsClassSymbol(const s: String): TClassSymbol;
         function GetValueAsTStrings(const s: String): TStrings;
         procedure SetResultAsString(const value : String);
         function GetResultAsString : String;
         procedure SetResultAsDataString(const value : RawByteString);
         function GetResultAsDataString : RawByteString;
         procedure SetResultAsInteger(const value : Int64);
         function GetResultAsInteger : Int64;
         procedure SetResultAsBoolean(const value : Boolean);
         function GetResultAsBoolean : Boolean;
         procedure SetResultAsFloat(const value : Double);
         function GetResultAsFloat : Double;

         function GetParamAsPVariant(index : Integer) : PVariant;
         function GetParamAsVariant(index : Integer) : Variant;
         function GetParamAsInteger(index : Integer) : Int64;
         function GetParamAsString(index : Integer) : String;
         function GetParamAsFloat(index : Integer) : Double;
         function GetParamAsBoolean(index : Integer) : Boolean;

         function CreateUnitList : TList;
         function FindSymbolInUnits(AUnitList: TList; const Name: string): TSymbol; overload;

      public
         procedure PrepareScriptObj;

         function RegisterExternalObject(AObject: TObject; AutoFree: Boolean=False; ExactClassMatch: Boolean=True): Variant;
         function GetExternalObjForVar(const s: string): TObject;
         // cycle ancestry hierarchy and find the nearest matching type
         function FindClassMatch(AObject: TObject; ExactMatch: Boolean=True): TClassSymbol;
         function FindSymbolInUnits(const Name: string): TSymbol; overload;
         function GetTemp(const DataType: string): IInfo;

         property Table : TSymbolTable read FTable write FTable;
         property Execution : TdwsProgramExecution read FExecution write FExecution;
         property Level : Integer read FLevel write FLevel;
         property Data[const s: string]: TData read GetData write SetData;
         property Func[const s: string]: IInfo read GetFunc;
         property FuncSym: TFuncSymbol read FFuncSym write SetFuncSym;
         property Method[const s: string]: IInfo read GetFunc;
         property ScriptObj: IScriptObj read FScriptObj write FScriptObj;
         property ResultAsVariant: Variant read GetResultAsVariant write SetResultAsVariant;
         property ResultVars: IInfo read GetResultVars;
         property Vars[const s: string]: IInfo read GetVars;

         property ValueAsVariant[const s : String] : Variant read GetValueAsVariant write SetValueAsVariant;
         property ValueAsChar[const s : String] : Char read GetValueAsChar;
         property ValueAsString[const s : String] : String read GetValueAsString write SetValueAsString;
         property ValueAsDataString[const s : String] : RawByteString read GetValueAsDataString write SetValueAsDataString;
         property ValueAsInteger[const s : String] : Int64 read GetValueAsInteger write SetValueAsInteger;
         property ValueAsBoolean[const s : String] : Boolean read GetValueAsBoolean write SetValueAsBoolean;
         property ValueAsFloat[const s : String] : Double read GetValueAsFloat write SetValueAsFloat;
         property ValueAsObject[const s : String] : TObject read GetValueAsObject;
         property ValueAsClassSymbol[const s : String] : TClassSymbol read GetValueAsClassSymbol;
         property ValueAsTStrings[const s : String] : TStrings read GetValueAsTStrings;

         property ParamAsVariant[index : Integer] : Variant read GetParamAsVariant;
         property ParamAsInteger[index : Integer] : Int64 read GetParamAsInteger;
         property ParamAsString[index : Integer] : String read GetParamAsString;
         property ParamAsFloat[index : Integer] : Double read GetParamAsFloat;
         property ParamAsBoolean[index : Integer] : Boolean read GetParamAsBoolean;

         property ResultAsString : String read GetResultAsString write SetResultAsString;
         property ResultAsDataString : RawByteString read GetResultAsDataString write SetResultAsDataString;
         property ResultAsBoolean : Boolean read GetResultAsBoolean write SetResultAsBoolean;
         property ResultAsInteger : Int64 read GetResultAsInteger write SetResultAsInteger;
         property ResultAsFloat : Double read GetResultAsFloat write SetResultAsFloat;
  end;

   // An instance of a script class FClassSym. Instance data in FData,
   TScriptObj = class(TInterfacedObject, IScriptObj)
      private
         FClassSym : TClassSymbol;
         FData : TData;
         FExternalObj : TObject;
         FDestroyed : Boolean;
         FExecutionContext : TdwsProgramExecution;
         FOnObjectDestroy: TObjectDestroyEvent;
         FNextObject, FPrevObject : TScriptObj;

      protected
         { IScriptObj }
         function GetClassSym: TClassSymbol;
         function GetData : TData;
         function DataOfAddr(addr : Integer) : Variant;
         function DataOfAddrAsString(addr : Integer) : String;
         function DataOfAddrAsInteger(addr : Integer) : Int64;
         procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
         function GetDestroyed : Boolean;
         procedure SetDestroyed(const val : Boolean);
         function GetExternalObject: TObject;
         procedure SetExternalObject(Value: TObject);

      public
         constructor Create(aClassSym : TClassSymbol; executionContext : TdwsProgramExecution = nil);
         destructor Destroy; override;
         procedure BeforeDestruction; override;
         property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;

         property ExecutionContext : TdwsProgramExecution read FExecutionContext write FExecutionContext;
         property Destroyed : Boolean read FDestroyed write FDestroyed;
         property NextObject : TScriptObj read FNextObject write FNextObject;
         property PrevObject : TScriptObj read FPrevObject write FPrevObject;
   end;

   EdwsVariantTypeCastError = class(EVariantTypeCastError)
      public
         constructor Create(const v : Variant; const desiredType : String);
   end;

function CreateMethodExpr(prog: TdwsProgram; meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
                          const Pos: TScriptPos; ForceStatic : Boolean = False): TFuncExpr;

function RawByteStringToScriptString(const s : RawByteString) : String;
function ScriptStringToRawByteString(const s : String) : RawByteString;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsFunctions, dwsCoreExprs;

type
  IDataMaster = interface
    ['{8D534D17-4C6B-11D5-8DCB-0000216D9E86}']
    function GetCaption: string;
    function GetSize: Integer;
    procedure Read(exec : TdwsExecution; const Data: TData);
    procedure Write(exec : TdwsExecution; const Data: TData);
    property Caption: string read GetCaption;
    property Size: Integer read GetSize;
  end;

  // private implementation of IInfo
  TInfo = class(TInterfacedObject, IUnknown, IInfo)
  protected
    FExec : TdwsProgramExecution;
    FChild: IInfo;
    FData: TData;
    FOffset: Integer;
    FProgramInfo: TProgramInfo;
    FDataMaster: IDataMaster;
    FTypeSym: TSymbol;
    function GetData : TData; virtual;
    function GetExternalObject: TObject; virtual;
    function GetMember(const s: string): IInfo; virtual;
    function GetMethod(const s: string): IInfo; virtual;
    function GetScriptObj: IScriptObj; virtual;
    function GetParameter(const s: string): IInfo; virtual;
    function GetTypeSym: TSymbol;
    function GetValue: Variant; virtual;
    function GetValueAsString : String; virtual;
    function GetValueAsDataString : RawByteString; virtual;
    function GetValueAsInteger : Int64; virtual;
    function GetValueAsFloat : Double; virtual;
    function GetInherited: IInfo; virtual;
    procedure SetData(const Value: TData); virtual;
    procedure SetExternalObject(ExtObject: TObject); virtual;
    procedure SetValue(const Value: Variant); virtual;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
                       const Data: TData; Offset: Integer; const DataMaster: IDataMaster = nil);
    function Call: IInfo; overload; virtual;
    function Call(const Params: array of Variant): IInfo; overload; virtual;
    function Element(const Indices: array of Integer): IInfo; virtual;
    function GetConstructor(const MethName: string; ExtObject: TObject): IInfo; virtual;
    class procedure SetChild(var Result : IInfo; ProgramInfo: TProgramInfo; ChildTypeSym: TSymbol;
      const ChildData: TData; ChildOffset: Integer; const ChildDataMaster: IDataMaster = nil);
  end;

  TInfoConst = class(TInfo)
  private
    FData: TData;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Value: Variant);
    function GetValue: Variant; override;
    function GetData : TData; override;
  end;

  TInfoData = class(TInfo)
    function GetValue: Variant; override;
    function GetValueAsString : String; override;
    function GetValueAsInteger : Int64; override;
    function GetValueAsFloat : Double; override;
    function GetData : TData; override;
    procedure SetData(const Value: TData); override;
    procedure SetValue(const Value: Variant); override;
  end;

  TInfoClass = class(TInfoData)
    FScriptObj: IScriptObj;
    function GetConstructor(const MethName: string; ExtObject: TObject): IInfo; override;
    function GetMethod(const s: string): IInfo; override;
    function GetScriptObj: IScriptObj; override;
    function GetInherited: IInfo; override;
  end;

  TInfoClassObj = class(TInfoClass)
    function GetMember(const s: string): IInfo; override;
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; const DataMaster: IDataMaster = nil);
  end;

  TInfoClassOf = class(TInfoClass)
  end;

  TInfoRecord = class(TInfoData)
    function GetMember(const s: string): IInfo; override;
  end;

  TInfoStaticArray = class(TInfoData)
    function Element(const Indices: array of Integer): IInfo; override;
    function GetMember(const s: string): IInfo; override;
  end;

  TInfoDynamicArray = class(TInfoData)
    function Element(const Indices: array of Integer): IInfo; override;
    function GetMember(const s: string): IInfo; override;
  end;

  TTempParam = class(TParamSymbol)
  private
    FData: TData;
    FIsVarParam: Boolean;
  public
    constructor Create(ParamSym: TSymbol);
    property Data: TData read FData;
    property IsVarParam: Boolean read FIsVarParam;
  end;

  TInfoFunc = class(TInfo)
  protected
    FClassSym: TClassSymbol;
    FExternalObject: TObject;
    FScriptObj: IScriptObj;
    FParams: TSymbolTable;
    FParamSize: Integer;
    FResult: TData;
    FTempParams: TSymbolTable;
    FTempParamSize: Integer;
    FUsesTempParams: Boolean;
    FForceStatic: Boolean;
    procedure InitTempParams;
    function GetParameter(const s: string): IInfo; override;
    function GetExternalObject: TObject; override;
    procedure SetExternalObject(ExtObject: TObject); override;
    function GetInherited: IInfo; override;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; DataMaster: IDataMaster; ScriptObj: IScriptObj;
      ClassSym: TClassSymbol; ForceStatic: Boolean = False);
    destructor Destroy; override;
    function Call: IInfo; overload; override;
    function Call(const Params: array of Variant): IInfo; overload; override;
  end;

  TInfoProperty = class(TInfo)
  private
    FScriptObj: IScriptObj;
    FPropSym: TPropertySymbol;
    FTempParams: TSymbolTable;
    procedure AssignIndices(const Func: IInfo; FuncParams: TSymbolTable);
  protected
    procedure InitTempParams;
    function GetParameter(const s: string): IInfo; override;
    function GetValue: Variant; override;
    function GetData : TData; override;
    procedure SetData(const Value: TData); override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; PropSym: TPropertySymbol; const ScriptObj: IScriptObj);
    destructor Destroy; override;
  end;

  TInfoConnector = class(TInfoData)
    function GetMethod(const s: string): IInfo; override;
    function GetMember(const s: string): IInfo; override;
  end;

  TInfoConnectorCall = class(TInfo)
  protected
    FName: string;
    FConnectorType: IConnectorType;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; const ConnectorType: IConnectorType; const Name: string);
    function Call(const Params: array of Variant): IInfo; overload; override;
  end;

  TDataMaster = class(TInterfacedObject, IUnknown, IDataMaster)
  private
    FCaller: TdwsProgramExecution;
    FSym: TSymbol;
    function GetCaption: string;
    function GetSize: Integer;
  public
    constructor Create(Caller: TdwsProgramExecution; Sym: TSymbol);
    procedure Read(exec : TdwsExecution; const Data: TData); virtual;
    procedure Write(exec : TdwsExecution; const Data: TData); virtual;
  end;

  TExternalVarDataMaster = class(TDataMaster)
  public
    procedure Read(exec : TdwsExecution; const Data: TData); override;
    procedure Write(exec : TdwsExecution; const Data: TData); override;
  end;

  TConnectorMemberDataMaster = class(TDataMaster)
  private
    FBaseValue: Variant;
    FName: string;
  public
    constructor Create(Caller: TdwsProgramExecution; Sym: TSymbol; BaseValue: Variant; const Name: string);
    procedure Read(exec : TdwsExecution; const Data: TData); override;
    procedure Write(exec : TdwsExecution; const Data: TData); override;
  end;

  TCleanUpEvent = procedure(ScriptObj: IScriptObj; ExternalObject: TObject) of object;

{ TScriptObjectWrapper }

// wrapper to interact with an released script object
type
   TScriptObjectWrapper = class (TInterfacedObject, IScriptObj)
      private
         FScriptObj : TScriptObj;
      protected
         { IScriptObj }
         function GetClassSym: TClassSymbol;
         function GetData : TData;
         function DataOfAddr(addr : Integer) : Variant;
         function DataOfAddrAsString(addr : Integer) : String;
         function DataOfAddrAsInteger(addr : Integer) : Int64;
         procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
         function GetExternalObject: TObject;
         procedure SetExternalObject(Value: TObject);
         function GetDestroyed : Boolean;
         procedure SetDestroyed(const val : Boolean);
      public
         constructor Create(ScriptObj : TScriptObj);
   end;

constructor TScriptObjectWrapper.Create(ScriptObj: TScriptObj);
begin
  inherited Create;
  FScriptObj := ScriptObj;
end;

function TScriptObjectWrapper.GetClassSym: TClassSymbol;
begin
  Result := FScriptObj.FClassSym;
end;

function TScriptObjectWrapper.GetData : TData;
begin
  Result := FScriptObj.GetData;
end;

// DataOfAddr
//
function TScriptObjectWrapper.DataOfAddr(addr : Integer) : Variant;
begin
   Result:=FScriptObj.DataOfAddr(addr);
end;

// DataOfAddrAsString
//
function TScriptObjectWrapper.DataOfAddrAsString(addr : Integer) : String;
begin
   Result:=FScriptObj.DataOfAddrAsString(addr);
end;

// DataOfAddrAsInteger
//
function TScriptObjectWrapper.DataOfAddrAsInteger(addr : Integer) : Int64;
begin
   Result:=FScriptObj.DataOfAddrAsInteger(addr);
end;

// DataOfAddrAsScriptObj
//
procedure TScriptObjectWrapper.DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
begin
   FScriptObj.DataOfAddrAsScriptObj(addr, scriptObj);
end;

function TScriptObjectWrapper.GetExternalObject: TObject;
begin
  Result := FScriptObj.GetExternalObject;
end;

procedure TScriptObjectWrapper.SetExternalObject(Value: TObject);
begin
  FScriptObj.SetExternalObject(Value);
end;

// GetDestroyed
//
function TScriptObjectWrapper.GetDestroyed : Boolean;
begin
   Result:=FScriptObj.Destroyed
end;

// SetDestroyed
//
procedure TScriptObjectWrapper.SetDestroyed(const val : Boolean);
begin
   FScriptObj.Destroyed:=val;
end;

function ScriptStringToRawByteString(const s : String) : RawByteString;
var
   i, n : Integer;
   pSrc : PChar;
   pDest : PByteArray;
begin
   if s='' then Exit('');
   n:=Length(s);
   SetLength(Result, n);
   pSrc:=PChar(NativeUInt(s));
   pDest:=PByteArray(NativeUInt(Result));
   for i:=0 to n-1 do
      pDest[i]:=PByte(@pSrc[i])^;
end;

function RawByteStringToScriptString(const s : RawByteString) : String;
var
   i, n : Integer;
   pSrc : PByteArray;
   pDest : PWordArray;
begin
   if s='' then Exit('');
   n:=Length(s);
   SetLength(Result, n);
   pSrc:=PByteArray(NativeUInt(s));
   pDest:=PWordArray(NativeUInt(Result));
   for i:=0 to n-1 do
      pDest[i]:=Word(PByte(@pSrc[i])^);
end;

procedure RaiseVariableNotFound(const s : String);
begin
   raise Exception.CreateFmt(RTE_VariableNotFound, [s]);
end;

procedure RaiseIncorrectParameterIndex(i : Integer);
begin
   raise Exception.CreateFmt(RTE_IncorrectParameterIndex, [i]);
end;

procedure RaiseOnlyVarSymbols(sym : TSymbol);
begin
   raise Exception.CreateFmt(RTE_OnlyVarSymbols, [sym.Caption]);
end;

// GetFuncExpr
//
function GetFuncExpr(Prog: TdwsProgram; FuncSym: TFuncSymbol; scriptObj: IScriptObj;
  ClassSym: TClassSymbol; ForceStatic: Boolean = False; exec : TdwsProgramExecution = nil): TFuncExpr;
begin
   if FuncSym is TMethodSymbol then begin
      if Assigned(scriptObj) then begin
         Result := CreateMethodExpr(Prog, TMethodSymbol(funcSym),
                                    TConstExpr.Create(Prog, ClassSym, scriptObj),
                                                      rkObjRef, cNullPos, ForceStatic)
      end else begin
         Result := CreateMethodExpr(Prog, TMethodSymbol(funcSym),
                                    TConstExpr.Create(Prog, ClassSym.ClassOf, Int64(ClassSym)),
                                                      rkClassOfRef, cNullPos, ForceStatic)
      end;
   end else begin
      Result := TFuncExpr.Create(Prog, cNullPos, funcSym, exec);
   end;
end;

// CreateMethodExpr
//
function CreateMethodExpr(prog: TdwsProgram; meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
                          const Pos: TScriptPos; ForceStatic : Boolean = False): TFuncExpr;
begin
   // Create the correct TExpr for a method symbol
   Result := nil;

   if Expr.IsConstant and (Expr.Typ is TClassOfSymbol) and TClassOfSymbol(Expr.Typ).TypClassSymbol.IsAbstract then
      prog.CompileMsgs.AddCompilerError(Pos, RTE_InstanceOfAbstractClass);

   // Return the right expression
   case meth.Kind of
      fkFunction, fkProcedure, fkMethod:
         if meth.IsClassMethod then begin
            if not ForceStatic and meth.IsVirtual then
               Result := TClassMethodVirtualExpr.Create(Expr.Prog, Pos, meth, Expr)
            else Result := TClassMethodStaticExpr.Create(Expr.Prog, Pos, meth, Expr)
         end else begin
            Assert(RefKind = rkObjRef);
            if not ForceStatic and meth.IsVirtual then
               Result := TMethodVirtualExpr.Create(Expr.Prog, Pos, meth, Expr)
            else Result := TMethodStaticExpr.Create(Expr.Prog, Pos, meth, Expr);
         end;
      fkConstructor:
         if RefKind = rkClassOfRef then begin
            if not ForceStatic and meth.IsVirtual then
               Result := TConstructorVirtualExpr.Create(Expr.Prog, Pos, meth, Expr)
            else Result := TConstructorStaticExpr.Create(Expr.Prog, Pos, meth, Expr);
         end else begin
            if not ForceStatic and meth.IsVirtual then
               Result := TConstructorVirtualObjExpr.Create(Expr.Prog, Pos, meth, Expr)
            else Result := TConstructorStaticObjExpr.Create(Expr.Prog, Pos, meth, Expr);
         end;
      fkDestructor:
         begin
            Assert(RefKind = rkObjRef);
            if not ForceStatic and meth.IsVirtual then
               Result := TDestructorVirtualExpr.Create(Expr.Prog, Pos, meth, Expr)
            else Result := TDestructorStaticExpr.Create(Expr.Prog, Pos, meth, Expr)
         end;
   else
      Assert(False);
   end;
end;

// ------------------
// ------------------ TdwsProgramExecution ------------------
// ------------------

// Create
//
constructor TdwsProgramExecution.Create(aProgram : TdwsProgram; const stackParams : TStackParameters);
begin
   inherited Create(stackParams);

   FProg:=aProgram;
   FProg._AddRef;
   FMsgs:=TdwsMessageList.Create;

   if aProgram.CompileMsgs.HasErrors then
      FProgramState:=psUndefined
   else FProgramState:=psReadyToRun;
end;

// Destroy
//
destructor TdwsProgramExecution.Destroy;
begin
   if ProgramState=psRunning then
      raise Exception.Create(RTE_ScriptStillRunning);
   if ProgramState=psRunningStopped then
      EndProgram;

   FProg.NotifyExecutionDestruction(Self);

   ReleaseObjects;

   FProgramInfo.Free;
   FProgInfoPool.Free;
   FResult.Free;
   FMsgs.Free;

   FProg._Release;
   inherited;
end;

// Execute
//
procedure TdwsProgramExecution.Execute(aTimeoutMilliSeconds : Integer = 0);
begin
   BeginProgram;
   if ProgramState=psRunning then
      RunProgram(aTimeoutMilliSeconds);
   if ProgramState in [psRunning, psRunningStopped] then
      EndProgram;
end;

// ExecuteParam
//
procedure TdwsProgramExecution.ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0);
var
   x, index: Integer;
begin
   SetLength(FParameters, High(Params) - Low(Params) + 1);
   index := 0;
   for x := Low(Params) to High(Params) do begin
      FParameters[index] := Params[x];
      Inc(index);
   end;

   Execute(aTimeoutMilliSeconds);
end;

// ExecuteParam
//
procedure TdwsProgramExecution.ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0);
var
   x: Integer;
begin
   if VarIsArray(Params) then begin
      SetLength(FParameters, VarArrayHighBound(Params, 1) + 1);
      for x := 0 to VarArrayHighBound(Params, 1) do
         FParameters[x] := Params[x];
   end else begin
      SetLength(FParameters, 1);
      FParameters[0] := Params;
   end;

   Execute(aTimeoutMilliSeconds);
end;

// BeginProgram
//
procedure TdwsProgramExecution.BeginProgram;
begin
   // Check program state
   if FProgramState<>psReadyToRun then begin
      case FProgramState of
         psRunning, psRunningStopped :
            Msgs.AddError(RTE_ScriptAlreadyRunning);
         psUndefined :
            Msgs.AddError(RTE_CantRunScript);
      else
         Msgs.AddError(RTE_StateReadyToRunExpected);
      end;
      Exit;
   end;

   FProgramState:=psRunning;
   try
      Msgs.Clear;

      FCurrentProg:=FProg;

      // Stack
      Stack.Reset;

      FProgramInfo := TProgramInfo.Create;
      FProgramInfo.Table := FProg.FTable;
      FProgramInfo.Execution := Self;

      // allocate global stack space
      Stack.Push(FProg.FGlobalAddrGenerator.DataSize + FProg.FAddrGenerator.DataSize);
      Stack.PushBp(0, Stack.BasePointer);

      // Initialize Result
      FResult.Free;
      FResult:=FProg.FResultType.CreateProgResult;

      // Result
      FResult.InitializeProgram(FProg);

      // Debugger
      StartDebug;

      // Prepare FileSystem
      if FProg.RuntimeFileSystem<>nil then
         FFileSystem:=FProg.RuntimeFileSystem.AllocateFileSystem
      else FFileSystem:=TdwsOSFileSystem.Create;

      // Initialize global variables
      Status:=esrNone;
      FProg.FInitExpr.EvalNoResult(Self);

      DoStep(FProg.FExpr);

   except
      on e: EScriptError do begin
         FMsgs.AddError(e.Message);
         FProgramState:=psRunningStopped;
      end;
      on e: Exception do begin
         FMsgs.AddError(e.Message+LastScriptError.ScriptPos.AsInfo);
         FProgramState:=psRunningStopped;
      end;
   end;
end;

// RunProgram
//
procedure TdwsProgramExecution.RunProgram(aTimeoutMilliSeconds : Integer);

   function AppendCallStack(const msg : String; const callStack : TExprBaseArray) : String;
   begin
      if Length(callStack)>0 then
         Result:=msg+#13#10+CallStackToString(callStack)
      else Result:=msg;
   end;

var
   terminator : TTerminatorThread;
begin
   if FProgramState <> psRunning then begin
      Msgs.AddError('Program state psRunning expected');
      Exit;
   end;

   if aTimeoutMilliSeconds=0 then
      aTimeOutMilliseconds:=FProg.TimeoutMilliseconds;
   if aTimeoutMilliSeconds>0 then
      terminator:=TTerminatorThread.Create(Self, aTimeoutMilliSeconds)
   else terminator:=nil;

   try
      Status:=esrNone;
      try
         // Run the script
         FProg.FExpr.EvalNoResult(Self);

         if status<>esrNone then begin
            case status of
               esrBreak : Msgs.AddError(RTE_InvalidBreak);
               esrContinue : Msgs.AddError(RTE_InvalidContinue);
            end;
         end;
      except
         on e: EScriptStopped do
            Msgs.AddInfo(e.Message);
         on e: EScriptAssertionFailed do
            Msgs.AddError(AppendCallStack(e.Message, e.ScriptCallStack));
         on e: EScriptException do
            Msgs.AddError(AppendCallStack(e.Message+e.Pos.AsInfo, e.ScriptCallStack));
         on e: EScriptError do
            Msgs.AddError(AppendCallStack(e.Message, e.ScriptCallStack));
         on e: Exception do
            if (LastScriptError is TFuncExpr) then
               Msgs.AddError(AppendCallStack(e.Message+' in '+TFuncExpr(LastScriptError).FuncSym.QualifiedName
                                             +LastScriptError.ScriptPos.AsInfo, LastScriptCallStack))
            else if LastScriptError<>nil then
               Msgs.AddError(AppendCallStack(e.Message+LastScriptError.ScriptLocation, LastScriptCallStack))
            else Msgs.AddError(AppendCallStack(e.Message, LastScriptCallStack))
      end;

   finally
      if Assigned(terminator) then
         terminator.Terminate;
   end;

   ClearScriptError;
end;

// Stop
//
procedure TdwsProgramExecution.Stop;
begin
   if FProgramState=psRunning then
      FProgramState:=psRunningStopped;
end;

// EndProgram
//
procedure TdwsProgramExecution.EndProgram;
begin
   if not (FProgramState in [psRunning, psRunningStopped]) then
      raise Exception.Create('Program was not started!');

   FProgramState:=psTerminated;
   try
      // Stack
      Stack.PopBp(0);
      Stack.Pop(Stack.StackPointer); // FProg.FAddrGenerator.DataSize + FProg.FGlobalAddrGenerator.DataSize);

      // Object Cycles
      ReleaseObjects;

      // Result
      FResult.FinalizeProgram(FProg);

      // Debugger
      StopDebug;

      // FileSystem
      FFileSystem:=nil;

      FreeAndNil(FProgramInfo);

      FProgramState:=psReadyToRun;
   except
      on e: EScriptError do
         Msgs.AddError(e.Message);
      on e: Exception do
         if LastScriptError<>nil then
            Msgs.AddError(e.Message+LastScriptError.ScriptPos.AsInfo)
         else Msgs.AddError(e.Message);
   end;

end;

// GetCallStack
//
function TdwsProgramExecution.GetCallStack : TExprBaseArray;
var
   i : Integer;
begin
   SetLength(Result, CallStack.Count);
   for i:=0 to CallStack.Count-1 do
      Result[i]:=(TObject(CallStack.List[CallStack.Count-1-i]) as TExprBase);
end;

// CallStackToString
//
function TdwsProgramExecution.CallStackToString(const callStack : TExprBaseArray) : String;
var
   i : Integer;
   buffer : TWriteOnlyBlockStream;
begin
   buffer:=TWriteOnlyBlockStream.Create;
   try
      for i:=0 to High(callStack) do begin
         if i>0 then
            buffer.WriteString(#13#10);
         buffer.WriteString(callStack[i].ScriptLocation);
      end;
      Result:=buffer.ToString;
   finally
      buffer.Free;
   end;
end;

// RaiseAssertionFailed
//
procedure TdwsProgramExecution.RaiseAssertionFailed(const msg : String; const scriptPos : TScriptPos);
begin
   RaiseAssertionFailedFmt(RTE_AssertionFailed, [scriptPos.AsInfo, msg], scriptPos);
end;

// RaiseAssertionFailedFmt
//
procedure TdwsProgramExecution.RaiseAssertionFailedFmt(const fmt : String; const args : array of const; const scriptPos : TScriptPos);
var
   exceptObj : IScriptObj;
   fmtMsg : String;
begin
   fmtMsg:=Format(fmt, args);
   exceptObj:=IScriptObj(IUnknown(ProgramInfo.Vars[SYS_EASSERTIONFAILED].Method[SYS_TOBJECT_CREATE].Call([fmtMsg]).Value));
   raise EScriptAssertionFailed.Create(fmtMsg, exceptObj, scriptPos)
end;

// AcquireProgramInfo
//
function TdwsProgramExecution.AcquireProgramInfo(funcSym : TFuncSymbol) : TProgramInfo;
begin
   if FProgInfoPool=nil then begin
      Result:=TProgramInfo.Create;
      Result.Execution:=Self;
   end else begin
      Result:=FProgInfoPool;
      FProgInfoPool:=nil;
   end;
   Result.FuncSym:=funcSym;
   Result.FTable:=funcSym.Params;
end;

// ReleaseProgramInfo
//
procedure TdwsProgramExecution.ReleaseProgramInfo(info : TProgramInfo);
begin
   if FProgInfoPool=nil then begin
      FProgInfoPool:=info;
      info.ScriptObj:=nil;
   end else info.Free;
end;

// ReleaseObjects
//
procedure TdwsProgramExecution.ReleaseObjects;
var
   i : Integer;
   iter : TScriptObj;
   buffer : array of TScriptObj;
begin
   if FObjectCount=0 then Exit;

   // add refcount to keep all alive during cleanup
   // detach from execution, add to buffer
   SetLength(buffer, FObjectCount);
   i:=0;
   iter:=FFirstObject;
   while iter<>nil do begin
      buffer[i]:=iter;
      Inc(i);
      iter._AddRef;
      iter.ExecutionContext:=nil;
      iter:=iter.NextObject;
   end;

   // clear all datas, kill references
   for i:=0 to FObjectCount-1 do begin
      iter:=buffer[i];
      SetLength(buffer[i].FData, 0);
      iter.FClassSym:=nil;
      iter.PrevObject:=nil;
      iter.NextObject:=nil;
   end;

   // dec refcount
   for i:=0 to FObjectCount-1 do begin
      iter:=buffer[i];
      iter._Release;
   end;

   // all remaining objects should now be referred only outside of scripts
   // can't do anything about them without triggering crashes
   FFirstObject:=nil;
   FLastObject:=nil;
   FObjectCount:=0;
end;

// ScriptObjCreated
//
procedure TdwsProgramExecution.ScriptObjCreated(scriptObj: TScriptObj);
begin
   scriptObj.ExecutionContext:=Self;
   if FObjectCount=0 then begin
      FFirstObject:=scriptObj;
      FLastObject:=scriptObj;
   end else begin
      scriptObj.PrevObject:=FLastObject;
      FLastObject.NextObject:=scriptObj;
      FLastObject:=scriptObj;
   end;
   Inc(FObjectCount);
end;

// ScriptObjDestroyed
//
procedure TdwsProgramExecution.ScriptObjDestroyed(scriptObj: TScriptObj);
begin
   scriptObj.ExecutionContext:=nil;
   Dec(FObjectCount);

   if FObjectCount>0 then begin
      if scriptObj.PrevObject<>nil then
         scriptObj.PrevObject.NextObject:=scriptObj.NextObject
      else begin
         FFirstObject:=scriptObj.NextObject;
         FFirstObject.PrevObject:=nil;
      end;
      if scriptObj.NextObject<>nil then
         scriptObj.NextObject.PrevObject:=scriptObj.PrevObject
      else begin
         FLastObject:=scriptObj.PrevObject;
         FLastObject.NextObject:=nil;
      end;
   end else begin
      FFirstObject:=nil;
      FLastObject:=nil;
   end;
end;

// DestroyScriptObj
//
procedure TdwsProgramExecution.DestroyScriptObj(const scriptObj: IScriptObj);
var
   sym : TSymbol;
   func : TMethodSymbol;
   expr : TDestructorVirtualExpr;
   oldStatus : TExecutionStatusResult;
begin
   try
      sym := ScriptObj.ClassSym.Members.FindSymbol(SYS_TOBJECT_DESTROY, cvPublic);

      if sym is TMethodSymbol then begin
         func := TMethodSymbol(sym);
         if (func.Kind = fkDestructor) and (func.Params.Count = 0) then begin
            expr := TDestructorVirtualExpr.Create(FProg, cNullPos, func,
                                                  TConstExpr.Create(FProg, ScriptObj.ClassSym, ScriptObj));
            oldStatus:=Status;
            try
               Status:=esrNone;
               scriptObj.Destroyed:=False;
               expr.EvalNoResult(Self);
            finally
               scriptObj.Destroyed:=True;
               Status:=oldStatus;
               expr.Free;
            end;
         end;
      end;
   except
      on e: Exception do
         Msgs.AddError(e.Message);
   end;
end;

// GetInfo
//
function TdwsProgramExecution.GetInfo : TProgramInfo;
begin
   Result:=ProgramInfo;
end;

// GetResult
//
function TdwsProgramExecution.GetResult : TdwsResult;
begin
   Result:=FResult;
end;

// GetObjectCount
//
function TdwsProgramExecution.GetObjectCount : Integer;
begin
   Result:=FObjectCount;
end;

// GetMsgs
//
function TdwsProgramExecution.GetMsgs : TdwsMessageList;
begin
   Result:=FMsgs;
end;

// GetProg
//
function TdwsProgramExecution.GetProg : IdwsProgram;
begin
   Result:=FProg;
end;

// ------------------
// ------------------ TdwsProgram ------------------
// ------------------

// Create
//
constructor TdwsProgram.Create(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                               const stackParameters : TStackParameters);
begin
   FExecutionsLock:=TCriticalSection.Create;

   FConditionalDefines:=TStringList.Create;
   FConditionalDefines.Sorted:=True;
   FConditionalDefines.CaseSensitive:=False;
   FConditionalDefines.Duplicates:=dupIgnore;

   FResultType := ResultType;

   FCompileMsgs := TdwsCompileMessageList.Create;
   FRoot := Self;

   // Create the Symbol Dictionary
   FSymbolDictionary := TSymbolDictionary.Create;
   // Create Context Map
   FContextMap := TContextMap.Create;
   //Create Script Source List
   FSourceList := TScriptSourceList.Create;

   FStackParameters:=stackParameters;

   // Initialize address generators
   FAddrGenerator := TAddrGeneratorRec.CreatePositive(0);
   FGlobalAddrGenerator := TAddrGeneratorRec.CreatePositive(0);
   FStackParameters.MaxLevel:=1;

   FUnifiedConstList:=TUnifiedConstList.Create;

   // Initialize the system table
   FRootTable := TProgramSymbolTable.Create(SystemTable, @FAddrGenerator);
   FTable := FRootTable;

   FInitExpr := TBlockInitExpr.Create(Self, cNullPos);

   // Initialize shortcuts to often used symbols
   FBaseTypes.FTypBoolean := SystemTable.FindSymbol(SYS_BOOLEAN, cvMagic) as TTypeSymbol;
   FBaseTypes.FTypFloat := SystemTable.FindSymbol(SYS_FLOAT, cvMagic) as TTypeSymbol;
   FBaseTypes.FTypInteger := SystemTable.FindSymbol(SYS_INTEGER, cvMagic) as TTypeSymbol;
   FBaseTypes.FTypString := SystemTable.FindSymbol(SYS_STRING, cvMagic) as TTypeSymbol;
   FBaseTypes.FTypVariant := SystemTable.FindSymbol(SYS_VARIANT, cvMagic) as TTypeSymbol;
   FBaseTypes.FTypNil := TNilSymbol.Create;
   FBaseTypes.FTypObject := TClassSymbol(SystemTable.FindSymbol(SYS_TOBJECT, cvMagic));
   FBaseTypes.FTypException := SystemTable.FindSymbol(SYS_EXCEPTION, cvMagic) as TClassSymbol;
end;

// Destroy
//
destructor TdwsProgram.Destroy;
begin
   FExecutionsLock.Enter;
   try
      if FExecutions.Count>0 then
         raise Exception.CreateFmt(RTE_ScriptHasLiveExecutions, [FExecutions.Count]);
   finally
      FExecutionsLock.Leave;
   end;

   FBinaryOperators.Free;
   FExecutionsLock.Free;
   FExpr.Free;
   FInitExpr.Free;
   FRootTable.Free;
   FBaseTypes.FTypNil.Free;
   FCompileMsgs.Free;
   FSymbolDictionary.Free;
   FContextMap.Free;
   FSourceList.Free;
   FUnifiedConstList.Free;
   FConditionalDefines.Free;
   FSourceFiles.Clean;

   inherited;
end;

// CreateNewExecution
//
function TdwsProgram.CreateNewExecution : IdwsProgramExecution;
var
   exec : TdwsProgramExecution;
begin
   exec:=TdwsProgramExecution.Create(Self, FStackParameters);
   exec.UserObject:=DefaultUserObject;
   FExecutionsLock.Enter;
   try
      FExecutions.Add(exec);
   finally
      FExecutionsLock.Leave;
   end;
   Result:=exec;
end;

// BeginNewExecution
//
function TdwsProgram.BeginNewExecution : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.BeginProgram;
end;

// Execute
//
function TdwsProgram.Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.Execute(aTimeoutMilliSeconds);
end;

// ExecuteParam
//
function TdwsProgram.ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.ExecuteParam(params, aTimeoutMilliSeconds);
end;

// ExecuteParam
//
function TdwsProgram.ExecuteParam(const Params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.ExecuteParam(Params, aTimeoutMilliSeconds);
end;

// SetConditionalDefines
//
procedure TdwsProgram.SetConditionalDefines(const val : TStringList);
begin
   FConditionalDefines.Assign(val);
end;

// NotifyExecutionDestruction
//
procedure TdwsProgram.NotifyExecutionDestruction(exec : TdwsProgramExecution);
begin
   FExecutionsLock.Enter;
   try
      FExecutions.Remove(exec);
   finally
      FExecutionsLock.Leave;
   end;
end;

// RegisterSourceFile
//
function TdwsProgram.RegisterSourceFile(const sourceFile : String; const sourceCode : String) : TSourceFile;
var
   sf: TSourceFile;
begin
   sf:=GetSourceFile(SourceFile);
   if not Assigned(sf) or (sf.SourceCode <> SourceCode) then begin
      Result:=TSourceFile.Create;
      Result.SourceFile:=SourceFile;
      Result.SourceCode:=SourceCode;
      FSourceFiles.Add(Result);
   end else Result:=sf;
end;

// GetSourceFile
//
function TdwsProgram.GetSourceFile(const aSourceFile : String) : TSourceFile;
var
   i : Integer;
begin
   for i:=0 to FSourceFiles.Count-1 do begin
      Result:=TSourceFile(FSourceFiles.List[i]);
      if Result.SourceFile=aSourceFile then Exit;
   end;
   Result:=nil;
end;

// GetMsgs
//
function TdwsProgram.GetMsgs : TdwsMessageList;
begin
   Result:=FCompileMsgs;
end;

// GetConditionalDefines
//
function TdwsProgram.GetConditionalDefines : TStringList;
begin
   Result:=FConditionalDefines;
end;

// GetLineCount
//
function TdwsProgram.GetLineCount : Integer;
begin
   Result:=FLineCount;
end;

// GetTable
//
function TdwsProgram.GetTable : TSymbolTable;
begin
   Result:=FTable;
end;

// GetTimeoutMilliseconds
//
function TdwsProgram.GetTimeoutMilliseconds : Integer;
begin
   Result:=FTimeoutMilliseconds;
end;

// SetTimeoutMilliseconds
//
procedure TdwsProgram.SetTimeoutMilliseconds(const val : Integer);
begin
   FTimeoutMilliseconds:=val;
end;

// GetDefaultUserObject
//
function TdwsProgram.GetDefaultUserObject : TObject;
begin
   Result:=FDefaultUserObject;
end;

// SetDefaultUserObject
//
procedure TdwsProgram.SetDefaultUserObject(const val : TObject);
begin
   FDefaultUserObject:=val;
end;

// GetSymbolDictionary
//
function TdwsProgram.GetSymbolDictionary : TSymbolDictionary;
begin
   Result:=FSymbolDictionary;
end;

// GetContextMap
//
function TdwsProgram.GetContextMap : TContextMap;
begin
   Result:=FContextMap;
end;

// GetSourceList
//
function TdwsProgram.GetSourceList : TScriptSourceList;
begin
   Result:=FSourceList;
end;

// GetProgramObject
//
function TdwsProgram.GetProgramObject : TdwsProgram;
begin
   Result:=Self;
end;

function TdwsProgram.GetLevel: Integer;
begin
  Result := FAddrGenerator.Level;
end;

function TdwsProgram.GetGlobalAddr(DataSize: Integer): Integer;
begin
  Result := FRoot.FGlobalAddrGenerator.GetStackAddr(DataSize);
end;

function TdwsProgram.GetTempAddr(DataSize: Integer): Integer;
begin
  Result := FAddrGenerator.GetStackAddr(DataSize);
end;

// NextStackLevel
//
function TdwsProgram.NextStackLevel(level : Integer) : Integer;
begin
   Result:=level+1;
   if Result>FStackParameters.MaxLevel then
      FStackParameters.MaxLevel:=Result;
end;

// ------------------
// ------------------ TdwsProcedure ------------------
// ------------------

// Create
//
constructor TdwsProcedure.Create(Parent: TdwsProgram);
begin
  FParent := Parent;

  // Create a local symbol table and connect it to the parent symboltable
  FAddrGenerator := TAddrGeneratorRec.CreatePositive(Parent.Level + 1);
  FRootTable := TProgramSymbolTable.Create(Parent.Table, @FAddrGenerator);
  FTable := FRootTable;
  FCompileMsgs:=Parent.CompileMsgs;

  FInitExpr := TBlockInitExpr.Create(Self, cNullPos);

  // Connect the procedure to the root TdwsProgram
  FRoot := Parent.Root;
  FBaseTypes := FRoot.FBaseTypes;
  FSymbolDictionary := Parent.SymbolDictionary;
  FContextMap := Parent.ContextMap;
end;

destructor TdwsProcedure.Destroy;
begin
   FRootTable.Free;
   FExpr.Free;
   FInitExpr.Free;
   FPreConditions.Free;
   FPostConditions.Free;
end;

procedure TdwsProcedure.AssignTo(sym: TFuncSymbol);
begin
  // Add parameter symboltable into the symboltable chain
  FTable.InsertParent(0, sym.Params);
  sym.Executable := ICallable(Self);
  FFunc := sym;
end;

// Call
//
procedure TdwsProcedure.Call(exec: TdwsProgramExecution; func: TFuncSymbol);
var
   oldProg : TdwsProgram;
   stackSize : Integer;
begin
   oldProg:=exec.CurrentProg;
   exec.FCurrentProg:=Self;

   // Allocate stack space for local variables
   stackSize:=FAddrGenerator.DataSize;
   exec.Stack.Push(stackSize);

   // Run the procedure
   try
      exec.Status:=esrNone;

      if FPreConditions<>nil then
         FPreConditions.EvalNoresult(exec);

      FInitExpr.EvalNoResult(exec);

      exec.DoStep(FExpr);
      FExpr.EvalNoResult(exec);

      exec.Status:=esrNone;

      if FPostConditions<>nil then
         FPostConditions.EvalNoresult(exec);

   finally
      // Free stack space for local variables
      exec.Stack.Pop(stackSize);
      exec.FCurrentProg:=oldProg;
   end;
end;

procedure TdwsProcedure.InitSymbol(Symbol: TSymbol);
begin
   FTable.Initialize(CompileMsgs);
   FExpr.Initialize;
end;

procedure TdwsProcedure.InitExpression(Expr: TExprBase);
begin
end;

{ TdwsResultType }

procedure TdwsResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  // no symbols
end;

function TdwsResultType.CreateProgResult: TdwsResult;
begin
  Result := TdwsResult.Create(Self);
end;

{ TdwsResult }

constructor TdwsResult.Create(ResultType: TdwsResultType);
begin
  FResultType := ResultType;
end;

procedure TdwsResult.FinalizeProgram(Prog: TdwsProgram);
begin
  if Assigned(FResultType.FOnFinalizeProgram) then
    FResultType.FOnFinalizeProgram(Prog);
end;

// AddString
//
procedure TdwsResult.AddString(const str : String);
begin
   // ignore by default
end;

procedure TdwsResult.InitializeProgram(Prog: TdwsProgram);
begin
  if Assigned(FResultType.FOnInitializeProgram) then
    FResultType.FOnInitializeProgram(Prog);
end;

{ TTerminatorThread }

// Create
//
constructor TTerminatorThread.Create(anExecutionContext : TdwsProgramExecution; aMilliSecToLive : Integer);
begin
   FExecutionContext:=anExecutionContext;
   FEvent:=TEvent.Create(nil, False, False, '');
   FMillisecondsToLive:=aMilliSecToLive;
   FreeOnTerminate:=True;
   inherited Create(False);
   Priority:=tpTimeCritical;
end;

// Destroy
//
destructor TTerminatorThread.Destroy;
begin
   FEvent.Free;
   inherited;
end;

// Execute
//
procedure TTerminatorThread.Execute;
begin
   FEvent.WaitFor(FMillisecondsToLive);

   if (not Terminated) and Assigned(FExecutionContext) then
      FExecutionContext.Stop;

   // Wait until TdwsProgram terminates the thread
   while not Terminated do
      FEvent.WaitFor(1000);
end;

// DoTerminate
//
procedure TTerminatorThread.DoTerminate;
begin
   inherited;
   FEvent.SetEvent;
end;

{ TNoPosExpr }

constructor TNoPosExpr.Create(Prog: TdwsProgram);
begin
  inherited Create;
  FProg := Prog;
  FTyp := nil;
end;

procedure TNoPosExpr.Initialize;
begin
end;

// TypeCheckNoPos
//
procedure TNoPosExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   // nothing
end;

function IsType(Typ: TSymbol; BType: TBaseTypeId): Boolean;
begin
  Result := Assigned(Typ) and (Typ.BaseType is TBaseSymbol)
            and IsBaseTypeCompatible(TBaseSymbol(Typ.BaseType).Id, BType);
end;

// IsBooleanValue
//
function TNoPosExpr.IsBooleanValue : Boolean;
begin
   Result:=Assigned(Typ) and Typ.IsBaseTypeIDValue(typBooleanID);
end;

// IsFloatValue
//
function TNoPosExpr.IsFloatValue : Boolean;
begin
   Result:=Assigned(Typ) and Typ.IsBaseTypeIDValue(typFloatID);
end;

// IsIntegerValue
//
function TNoPosExpr.IsIntegerValue : Boolean;
begin
   Result:=Assigned(Typ) and Typ.IsBaseTypeIDValue(typIntegerID);
end;

// IsNumberValue
//
function TNoPosExpr.IsNumberValue : Boolean;
begin
   Result:=Assigned(Typ) and (Typ.BaseTypeID in [typFloatID, typIntegerID]);
end;

// IsStringValue
//
function TNoPosExpr.IsStringValue : Boolean;
begin
   Result:=Assigned(Typ) and Typ.IsBaseTypeIDValue(typStringID);
end;

// IsVariantValue
//
function TNoPosExpr.IsVariantValue : Boolean;
begin
   Result:=Assigned(Typ) and Typ.IsBaseTypeIDValue(typVariantID);
end;

// IsConstant
//
function TNoPosExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// Optimize
//
function TNoPosExpr.Optimize(exec : TdwsExecution) : TNoPosExpr;
begin
   Result:=Self;
end;

// OptimizeIntegerConstantToFloatConstant
//
function TNoPosExpr.OptimizeIntegerConstantToFloatConstant(exec : TdwsExecution) : TNoPosExpr;
begin
   if IsConstant and IsIntegerValue then begin
      Result:=TConstFloatExpr.CreateUnified(FProg, nil, EvalAsFloat(exec));
      Free;
   end else Result:=Self;
end;

// ScriptPos
//
function TNoPosExpr.ScriptPos : TScriptPos;
begin
   Result:=cNullPos;
end;

// ScriptLocation
//
function TNoPosExpr.ScriptLocation : String;
begin
   if Prog is TdwsProcedure then
      Result:=TdwsProcedure(Prog).Func.QualifiedName+ScriptPos.AsInfo
   else Result:=ScriptPos.AsInfo;
end;

// RaiseScriptError
//
procedure TNoPosExpr.RaiseScriptError(e : EScriptError);
begin
   e.Pos:=ScriptPos;
   e.RawMessage:=e.Message;
   e.Message:=e.Message+e.Pos.AsInfo;
   raise e;
end;

// RaiseScriptError
//
procedure TNoPosExpr.RaiseScriptError;
var
   exc : Exception;
   e : EScriptError;
begin
   Assert(ExceptObject is Exception);
   exc:=Exception(ExceptObject);
   e:=EScriptError.Create(exc.Message);
   e.RawClassName:=exc.ClassName;
   RaiseScriptError(e);
end;

// RaiseScriptError
//
procedure TNoPosExpr.RaiseScriptError(const msg : String);
begin
   RaiseScriptError(EScriptError.Create(msg));
end;

// RaiseScriptError
//
procedure TNoPosExpr.RaiseScriptError(exceptClass : EScriptErrorClass; const msg : String);
begin
   RaiseScriptError(exceptClass.Create(msg));
end;

// RaiseScriptError
//
procedure TNoPosExpr.RaiseScriptError(exceptClass : EScriptErrorClass; const msg : String;
                                      const args : array of const);
begin
   RaiseScriptError(exceptClass.CreateFmt(msg, args));
end;

// RaiseUpperExceeded
//
procedure TNoPosExpr.RaiseUpperExceeded(index : Integer);
begin
   RaiseScriptError(EScriptOutOfBounds.CreateFmt(RTE_ArrayUpperBoundExceeded, [index]));
end;

// RaiseLowerExceeded
//
procedure TNoPosExpr.RaiseLowerExceeded(index : Integer);
begin
   RaiseScriptError(EScriptOutOfBounds.CreateFmt(RTE_ArrayLowerBoundExceeded, [index]));
end;

// CheckScriptObject
//
procedure TNoPosExpr.CheckScriptObject(const scriptObj : IScriptObj);
begin
   if scriptObj=nil then
      RaiseObjectNotInstantiated
   else if scriptObj.Destroyed then
      RaiseObjectAlreadyDestroyed;
end;

// RaiseObjectNotInstantiated
//
procedure TNoPosExpr.RaiseObjectNotInstantiated;
begin
   RaiseScriptError(EScriptError.Create(RTE_ObjectNotInstantiated));
end;

// RaiseObjectAlreadyDestroyed
//
procedure TNoPosExpr.RaiseObjectAlreadyDestroyed;
begin
   RaiseScriptError(EScriptError.Create(RTE_ObjectAlreadyDestroyed));
end;

// GetBaseType
//
function TNoPosExpr.GetBaseType : TTypeSymbol;
begin
   if Assigned(Typ) then
      Result:=Typ.BaseType
   else Result:=nil;
end;

// EvalAsVariant
//
procedure TNoPosExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Result:=Eval(exec);
end;

// EvalAsScriptObj
//
procedure TNoPosExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
begin
   Result:=IScriptObj(IUnknown(Eval(exec)));
end;

// AssignValue
//
procedure TNoPosExpr.AssignValue(exec : TdwsExecution; const value : Variant);
begin
   raise EScriptError.CreateFmt('Cannot assign to %s', [ClassName]);
end;

// AssignValueAsInteger
//
procedure TNoPosExpr.AssignValueAsInteger(exec : TdwsExecution; const value : Int64);
begin
   AssignValue(exec, value);
end;

// AssignValueAsBoolean
//
procedure TNoPosExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   AssignValue(exec, value);
end;

// AssignValueAsFloat
//
procedure TNoPosExpr.AssignValueAsFloat(exec : TdwsExecution; const value : Double);
begin
   AssignValue(exec, value);
end;

// AssignValueAsString
//
procedure TNoPosExpr.AssignValueAsString(exec : TdwsExecution; const value: String);
begin
   AssignValue(exec, value);
end;

// EvalAsInteger
//
function TNoPosExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=v;
   except
      // workaround for RTL bug that will sometimes report a failed cast to Int64
      // as being a failed cast to Boolean
      on E : EVariantTypeCastError do begin
         raise EdwsVariantTypeCastError.Create(v, 'Integer');
      end else raise;
   end;
end;

// EvalAsBoolean
//
function TNoPosExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=v;
   except
      // standardize RTL message
      on E : EVariantTypeCastError do begin
         raise EdwsVariantTypeCastError.Create(v, 'Boolean');
      end else raise;
   end;
end;

// EvalAsFloat
//
function TNoPosExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=v;
   except
      // standardize RTL message
      on E : EVariantTypeCastError do begin
         raise EdwsVariantTypeCastError.Create(v, 'Float');
      end else raise;
   end;
end;

// EvalAsString
//
procedure TNoPosExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=String(v);
   except
      // standardize RTL message
      on E : EVariantTypeCastError do begin
         raise EdwsVariantTypeCastError.Create(v, 'String');
      end else raise;
   end;
end;

// EvalNoResult
//
procedure TNoPosExpr.EvalNoResult(exec : TdwsExecution);
begin
   Eval(exec);
end;

// ------------------
// ------------------ TExpr ------------------
// ------------------

// Create
//
constructor TExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   inherited Create(Prog);
   FPos:=Pos;
end;

// TypeCheck
//
procedure TExpr.TypeCheck;
begin
   TypeCheckNoPos(Pos);
end;

// ScriptPos
//
function TExpr.ScriptPos : TScriptPos;
begin
   Result:=FPos;
end;

// ------------------
// ------------------ TPosDataExpr ------------------
// ------------------

// Create
//
constructor TPosDataExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Typ: TSymbol);
begin
   inherited Create(Prog, Typ);
   FPos:=Pos;
end;

// TypeCheck
//
procedure TPosDataExpr.TypeCheck;
begin
   TypeCheckNoPos(Pos);
end;

// AddCompilerErrorFmt
//
procedure TPosDataExpr.AddCompilerErrorFmt(const fmtText: string; const Args: array of const);
begin
   Prog.CompileMsgs.AddCompilerErrorFmt(Pos, fmttext, Args);
end;

// AddCompilerStop
//
procedure TPosDataExpr.AddCompilerStop(const Text : String);
begin
   Prog.CompileMsgs.AddCompilerStop(Pos, Text);
end;

// ScriptPos
//
function TPosDataExpr.ScriptPos : TScriptPos;
begin
   Result:=FPos;
end;

// ------------------
// ------------------ TNoResultExpr ------------------
// ------------------

function TNoResultExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalNoResult(exec);
   Assert(exec.Status=esrNone);
end;

// EvalNoResult
//
procedure TNoResultExpr.EvalNoResult(exec : TdwsExecution);
begin
   //nothing
end;

// OptimizeToNoResultExpr
//
function TNoResultExpr.OptimizeToNoResultExpr(exec : TdwsExecution) : TNoResultExpr;
var
   optimized : TNoPosExpr;
begin
   optimized:=Optimize(exec);
   if optimized is TNoResultExpr then
      Result:=TNoResultExpr(optimized)
   else begin
      Result:=nil;
      Assert(False);
   end;
end;

{ TNullExpr }

procedure TNullExpr.EvalNoResult(exec : TdwsExecution);
begin
   //nothing
end;

// ------------------
// ------------------ TBlockExprBase ------------------
// ------------------

// Destroy
//
destructor TBlockExprBase.Destroy;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      FStatements[i].Free;
   FreeMem(FStatements);
   inherited;
end;

// AddStatement
//
procedure TBlockExprBase.AddStatement(expr : TExpr);
begin
   ReallocMem(FStatements, (FCount+1)*SizeOf(TExpr));
   FStatements[FCount]:=expr;
   Inc(FCount);
end;

// AddStatementFirst
//
procedure TBlockExprBase.AddStatementFirst(expr : TExpr);
begin
   ReallocMem(FStatements, (FCount+1)*SizeOf(TExpr));
   Move(FStatements[0], FStatements[1], FCount);
   FStatements[0]:=expr;
   Inc(FCount);
end;

// Initialize
//
procedure TBlockExprBase.Initialize;
var
   i : Integer;
begin
   for i:=0 to FCount-1 do
      FStatements[i].Initialize;
end;

// ------------------
// ------------------ TBlockInitExpr ------------------
// ------------------

// EvalNoResult
//
procedure TBlockInitExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   expr : PExpr;
begin
   expr:=@FStatements[0];
   for i:=1 to FCount do begin
      expr.EvalNoResult(exec);
      Inc(expr);
   end;
end;

// ------------------
// ------------------ TDataExpr ------------------
// ------------------

constructor TDataExpr.Create(Prog: TdwsProgram; Typ: TSymbol);
begin
   inherited Create(Prog);
   FTyp := Typ;
end;

function TDataExpr.Eval(exec : TdwsExecution) : Variant;
begin
  Result := Data[exec][Addr[exec]];
end;

// IsWritable
//
function TDataExpr.IsWritable: Boolean;
begin
   Result:=True;
end;

function TDataExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
  Result := 0;
end;

procedure TDataExpr.AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer);
begin
  Assert(IsWritable);
  CopyData(SourceData, SourceAddr, Data[exec], Addr[exec], Typ.Size);
end;

procedure TDataExpr.AssignValue(exec : TdwsExecution; const Value: Variant);
begin
  Assert(IsWritable);
  VarCopy(Data[exec][Addr[exec]], Value);
end;

// AssignValueAsInteger
//
procedure TDataExpr.AssignValueAsInteger(exec : TdwsExecution; const value : Int64);
begin
   AssignValue(exec, value);
end;

// AssignValueAsBoolean
//
procedure TDataExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   AssignValue(exec, value);
end;

// AssignValueAsFloat
//
procedure TDataExpr.AssignValueAsFloat(exec : TdwsExecution; const value : Double);
begin
   AssignValue(exec, value);
end;

// AssignValueAsString
//
procedure TDataExpr.AssignValueAsString(exec : TdwsExecution; const value: String);
begin
   AssignValue(exec, value);
end;

procedure TDataExpr.AssignExpr(exec : TdwsExecution; Expr: TNoPosExpr);
begin
  Assert(IsWritable);
  VarCopy(Data[exec][Addr[exec]], Expr.Eval(exec));
end;

procedure TDataExpr.AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr);
begin
  CopyData(DataExpr.Data[exec], DataExpr.Addr[exec], Data[exec], Addr[exec], Typ.Size);
end;

// ------------------
// ------------------ TFuncExprBase ------------------
// ------------------

// Create
//
constructor TFuncExprBase.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TFuncSymbol);
begin
   inherited Create(Prog, Pos, nil);
   FFunc := Func;
   FTyp := Func.Typ;
end;

// Destroy
//
destructor TFuncExprBase.Destroy;
begin
   FArgs.Clean;
   inherited;
end;

// TypeCheckNoPos
//
procedure TFuncExprBase.TypeCheckNoPos(const aPos : TScriptPos);
var
   arg : TNoPosExpr;
   x, paramCount, nbParamsToCheck : Integer;
   paramSymbol : TParamSymbol;
begin
   paramCount := FFunc.Params.Count;

   // Check number of arguments = number of parameters
   if FArgs.Count>paramCount then begin
      AddCompilerErrorFmt(CPE_TooManyArguments, []);
      while FArgs.Count>paramCount do begin
         FArgs.ExprBase[FArgs.Count-1].Free;
         FArgs.Delete(FArgs.Count-1);
      end;
   end;
   while FArgs.Count<paramCount do begin
      // Complete missing args by default values
      paramSymbol:=TParamSymbol(FFunc.Params[FArgs.Count]);
      if paramSymbol is TParamSymbolWithDefaultValue then
         FArgs.Add(TConstExpr.CreateTyped(Prog, paramSymbol.Typ,
                                          TParamSymbolWithDefaultValue(paramSymbol).DefaultValue))
      else begin
         AddCompilerErrorFmt(CPE_TooFewArguments, []);
         Break;
      end;
   end;

   if paramCount<FArgs.Count then
      nbParamsToCheck:=paramCount
   else nbParamsToCheck:=FArgs.Count;

   for x:=0 to nbParamsToCheck-1 do begin
      arg:=TNoPosExpr(FArgs.ExprBase[x]);
      paramSymbol:=TParamSymbol(FFunc.Params[x]);

      if arg is TArrayConstantExpr then
         TArrayConstantExpr(arg).Prepare(paramSymbol.Typ.Typ);

      // Expand integer arguments to float if necessary
      if (paramSymbol.Typ = FProg.TypFloat) and (arg.Typ = FProg.TypInteger) then
         arg := TConvFloatExpr.Create(FProg, arg);

      FArgs.ExprBase[x] := arg;

      if arg.Typ = nil then
         AddCompilerErrorFmt(CPE_WrongArgumentType, [x, paramSymbol.Typ.Name]);
      if paramSymbol.InheritsFrom(TVarParamSymbol) then begin
         if arg is TDataExpr then begin
            if not TDataExpr(arg).IsWritable then
               AddCompilerErrorFmt(CPE_ConstVarParam, [x, paramSymbol.Name]);
         end else AddCompilerErrorFmt(CPE_ConstVarParam, [x, paramSymbol.Name]);
      end;
      if arg.Typ=nil then
         AddCompilerErrorFmt(CPE_WrongArgumentType, [x, paramSymbol.Typ.Name])
      else if not paramSymbol.Typ.IsCompatible(arg.Typ) then
         AddCompilerErrorFmt(CPE_WrongArgumentType_Long, [x, paramSymbol.Typ.Name, arg.Typ.Name]);
   end;
end;

// Initialize
//
procedure TFuncExprBase.Initialize;
var
   i : Integer;
begin
   for i:=0 to FArgs.Count-1 do
      TNoPosExpr(FArgs.ExprBase[i]).Initialize;
end;

// GetArgs
//
function TFuncExprBase.GetArgs : TExprBaseList;
begin
   Result:=@FArgs;
end;

// Optimize
//
function TFuncExprBase.Optimize(exec : TdwsExecution) : TNoPosExpr;
begin
   Result:=Self;
   if IsConstant then begin
      Initialize;
      try
         Result:=TConstExpr.CreateTyped(Prog, Typ, Eval(exec));
      except
         on E: EScriptError do begin
            FProg.CompileMsgs.AddCompilerErrorFmt(E.Pos, CPE_FunctionOptimizationFailed,
                                                  [FuncSym.Name, E.RawClassName, E.RawMessage],
                                                   TCompilerErrorMessage);
         end;
         on E: Exception do begin
            FProg.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_FunctionOptimizationFailed,
                                                  [FuncSym.Name, E.ClassName, E.Message],
                                                   TCompilerErrorMessage);
         end;
      end;
      if Result<>Self then
         Free;
   end;
end;

// IsConstant
//
function TFuncExprBase.IsConstant : Boolean;
var
   i : Integer;
begin
   if not FuncSym.IsStateless then Exit(False);

   for i:=0 to FArgs.Count-1 do
      if not TNoPosExpr(FArgs.ExprBase[i]).IsConstant then
         Exit(False);

   Result:=True;
end;

// ------------------
// ------------------ TPushOperator ------------------
// ------------------

// InitPushAddr
//
procedure TPushOperator.InitPushAddr(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potAddr);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// InitPushAddr
//
procedure TPushOperator.InitPushTempAddr(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potTempAddr);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// InitPushTempArrayAddr
//
procedure TPushOperator.InitPushTempArrayAddr(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potTempArrayAddr);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr as TArrayConstantExpr;
end;

// InitPushTempArray
//
procedure TPushOperator.InitPushTempArray(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potTempArray);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr as TConstParamExpr;
end;

// InitPushResult
//
procedure TPushOperator.InitPushResult(StackAddr: Integer; ArgExpr: TNoPosExpr);
var
   typID : TBaseTypeID;
begin
   if ArgExpr.Typ<>nil then
      typID:=ArgExpr.Typ.BaseTypeID
   else typID:=typNoneID;
   case typID of
      typIntegerID : FTypeParamSym:=TSymbol(potResultInteger);
      typFloatID : FTypeParamSym:=TSymbol(potResultFloat);
      typBooleanID : FTypeParamSym:=TSymbol(potResultBoolean);
      typStringID :
         if ArgExpr.InheritsFrom(TConstStringExpr) then
            FTypeParamSym:=TSymbol(potResultConstString)
         else FTypeParamSym:=TSymbol(potResultString);
   else
      FTypeParamSym:=TSymbol(potResult);
   end;
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// InitPushData
//
procedure TPushOperator.InitPushData(StackAddr: Integer; ArgExpr: TNoPosExpr; ParamSym: TSymbol);
begin
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
   FTypeParamSym:=ParamSym;
end;

// InitPushInitResult
//
procedure TPushOperator.InitPushInitResult(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potInitResult);
   FStackAddr:=StackAddr;
   FArgExpr:=TNoPosExpr((ArgExpr as TFuncExpr).Typ); // dirty hack
end;

// InitPushLazy
//
procedure TPushOperator.InitPushLazy(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potLazy);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// Execute
//
procedure TPushOperator.Execute(exec : TdwsExecution);
begin
   case TPushOperatorType(FTypeParamSym) of
      potAddr : ExecuteAddr(exec);
      potTempAddr : ExecuteTempAddr(exec);
      potTempArrayAddr : ExecuteTempArrayAddr(exec);
      potTempArray : ExecuteTempArray(exec);
      potResultBoolean : ExecuteResultBoolean(exec);
      potResultInteger : ExecuteResultInteger(exec);
      potResultFloat : ExecuteResultFloat(exec);
      potResultString : ExecuteResultString(exec);
      potResultConstString : ExecuteResultConstString(exec);
      potResult : ExecuteResult(exec);
      potInitResult : ExecuteInitResult(exec);
      potLazy : ExecuteLazy(exec);
   else
      ExecuteData(exec);
   end;
end;

type

  TVarParamData = class (TInterfacedObject, IVarParamData)
  private
    FData: TData;
    FAddr: Integer;
  protected
    function GetData : TData;
    function GetAddr : Integer;
  public
    constructor Create(const Data: TData; Addr: Integer);
  end;

{ TVarParamData }

constructor TVarParamData.Create(const Data: TData; Addr: Integer);
begin
  inherited Create;
  FData := Data;
  FAddr := Addr;
end;

function TVarParamData.GetAddr : Integer;
begin
  Result := FAddr;
end;

function TVarParamData.GetData : TData;
begin
  Result := FData;
end;

// ExecuteAddr
//
procedure TPushOperator.ExecuteAddr(exec : TdwsExecution);
var
   vpd: IVarParamData;
begin
   vpd := TVarParamData.Create(TDataExpr(FArgExpr).Data[exec], TDataExpr(FArgExpr).Addr[exec]);
   exec.Stack.WriteValue(exec.Stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempAddr
//
procedure TPushOperator.ExecuteTempAddr(exec : TdwsExecution);
var
   vpd : IVarParamData;
   data : TData;
begin
   SetLength(data, 1);
   data[0]:=FArgExpr.Eval(exec);
   vpd := TVarParamData.Create(data, 0);
   exec.Stack.WriteValue(exec.Stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempArrayAddr
//
procedure TPushOperator.ExecuteTempArrayAddr(exec : TdwsExecution);
var
   vpd : IVarParamData;
   data : TData;
begin
   data:=TArrayConstantExpr(FArgExpr).EvalAsTData(exec);
   vpd:=TVarParamData.Create(data, 0);
   exec.Stack.WriteValue(exec.Stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteTempArray
//
procedure TPushOperator.ExecuteTempArray(exec : TdwsExecution);
var
   vpd : IVarParamData;
   data : TData;
begin
   data:=TConstParamExpr(FArgExpr).Data[exec];
   vpd:=TVarParamData.Create(data, 0);
   exec.Stack.WriteValue(exec.Stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteResult
//
procedure TPushOperator.ExecuteResult(exec : TdwsExecution);
begin
   exec.Stack.WriteValue(exec.Stack.StackPointer + FStackAddr, FArgExpr.Eval(exec));
end;

// ExecuteResultBoolean
//
procedure TPushOperator.ExecuteResultBoolean(exec : TdwsExecution);
begin
   exec.Stack.WriteBoolValue(exec.Stack.StackPointer + FStackAddr, FArgExpr.EvalAsBoolean(exec));
end;

// ExecuteResultInteger
//
procedure TPushOperator.ExecuteResultInteger(exec : TdwsExecution);
begin
   exec.Stack.WriteIntValue(exec.Stack.StackPointer + FStackAddr, FArgExpr.EvalAsInteger(exec));
end;

// ExecuteResultFloat
//
procedure TPushOperator.ExecuteResultFloat(exec : TdwsExecution);
begin
   exec.Stack.WriteFloatValue(exec.Stack.StackPointer + FStackAddr, FArgExpr.EvalAsFloat(exec));
end;

// ExecuteResultString
//
procedure TPushOperator.ExecuteResultString(exec : TdwsExecution);
var
   buf : String;
begin
   FArgExpr.EvalAsString(exec, buf);
   exec.Stack.WriteStrValue(exec.Stack.StackPointer + FStackAddr, buf);
end;

// ExecuteResultConstString
//
procedure TPushOperator.ExecuteResultConstString(exec : TdwsExecution);
begin
   exec.Stack.WriteStrValue(exec.Stack.StackPointer + FStackAddr,
                            TConstStringExpr(FArgExpr).Value);
end;

// ExecuteData
//
procedure TPushOperator.ExecuteData(exec : TdwsExecution);
begin
   exec.Stack.WriteData(TDataExpr(FArgExpr).Addr[exec], exec.Stack.StackPointer + FStackAddr,
                        FTypeParamSym.Typ.Size, TDataExpr(FArgExpr).Data[exec]);
end;

// ExecuteInitResult
//
procedure TPushOperator.ExecuteInitResult(exec : TdwsExecution);
begin
   TSymbol(FArgExpr).InitData(exec.Stack.Data, exec.Stack.StackPointer+FStackAddr);
end;

// ExecuteLazy
//
procedure TPushOperator.ExecuteLazy(exec : TdwsExecution);
begin
   exec.Stack.WriteIntValue(exec.Stack.StackPointer + FStackAddr,
                            Int64(FArgExpr)+(Int64(exec.Stack.BasePointer) shl 32));
end;

// ------------------
// ------------------ TFuncExpr ------------------
// ------------------

constructor TFuncExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TFuncSymbol;
                             exec : TdwsProgramExecution = nil);
begin
   inherited Create(Prog, Pos, Func);
   FLevel:=Prog.Level;
   FResultAddr:=-1;
end;

// Destroy
//
destructor TFuncExpr.Destroy;
begin
   FreeMem(FPushExprs);
   inherited;
end;

// AddArg
//
function TFuncExpr.AddArg(arg: TNoPosExpr) : TSymbol;
begin
   if FArgs.Count<FFunc.Params.Count then
      Result:=FFunc.Params[FArgs.Count]
   else Result:=nil;
   FArgs.Add(arg);
end;

// EvalPushExprs
//
procedure TFuncExpr.EvalPushExprs(exec : TdwsExecution);
var
   i : Integer;
   p : PPushOperator;
begin
   p:=PPushOperator(FPushExprs);
   for i:=1 to FPushExprsCount do begin
      p.Execute(exec);
      Inc(p);
   end;
end;

// Eval
//
function TFuncExpr.Eval(exec : TdwsExecution) : Variant;
var
   oldBasePointer : Integer;
   func : TFuncSymbol;
begin
   try
      // Allocate memory for parameters on the stack
      exec.Stack.Push(FFunc.ParamSize);
      try
         func:=PreCall(exec);

         EvalPushExprs(exec);

         oldBasePointer:=exec.Stack.SwitchFrame(FLevel);
         exec.EnterRecursion(Self);
         try
            ICallable(func.Executable).Call(TdwsProgramExecution(exec), func);
         finally
            exec.LeaveRecursion;
            exec.Stack.RestoreFrame(FLevel, oldBasePointer);
         end;

         Result:=PostCall(exec);
      finally
         // Remove parameters from stack
         exec.Stack.Pop(FFunc.ParamSize);
      end;
   except
      exec.SetScriptError(Self);
      raise;
   end;
end;

// PreCall
//
function TFuncExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
begin
   Result:=FFunc;
end;

// PostCall
//
function TFuncExpr.PostCall(exec : TdwsExecution) : Variant;
var
   sourceAddr, destAddr: Integer;
begin
   if Assigned(FTyp) then begin
      // Result.StackAddr is relative to BasePointer of the called function
      // But the frame is already restored so its relative to the stackpointer here
      sourceAddr:=exec.Stack.StackPointer+FFunc.Result.StackAddr;
      // Copy return value
      exec.Stack.ReadValue(sourceAddr, Result);

      if FResultAddr>=0 then begin
         destAddr:=exec.Stack.BasePointer+FResultAddr;
         exec.Stack.CopyData(sourceAddr, destAddr, FFunc.Typ.Size);
      end;
   end;
end;

function TFuncExpr.GetData(exec : TdwsExecution) : TData;
begin
   Eval(exec);
   Result := exec.Stack.Data;
end;

function TFuncExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
  Result := exec.Stack.BasePointer + FResultAddr;
end;

// AddPushExprs
//
procedure TFuncExpr.AddPushExprs;
var
   i : Integer;
   arg : TNoPosExpr;
   param : TParamSymbol;
   pushOperator : PPushOperator;
begin
   if Assigned(FFunc.Result) then
      FPushExprsCount:=FArgs.Count+1
   else FPushExprsCount:=FArgs.Count;
   ReallocMem(FPushExprs, FPushExprsCount*SizeOf(TPushOperator));

   for i := 0 to FArgs.Count - 1 do begin
      pushOperator:=@FPushExprs[i];
      arg := TNoPosExpr(FArgs.ExprBase[i]);
      param := TParamSymbol(FFunc.Params[i]);
      if param.InheritsFrom(TLazyParamSymbol) then
         pushOperator.InitPushLazy(param.StackAddr, arg)
      else if arg is TDataExpr then begin
         if param.Typ is TOpenArraySymbol then begin
            if arg is TArrayConstantExpr then
               pushOperator.InitPushTempArrayAddr(param.StackAddr, arg)
            else pushOperator.InitPushTempArray(param.StackAddr, arg);
         end else if param is TByRefParamSymbol then begin
            pushOperator.InitPushAddr(param.StackAddr, arg)
         end else if param.Size > 1 then
            pushOperator.InitPushData(param.StackAddr, TDataExpr(arg), param)
         else pushOperator.InitPushResult(param.StackAddr, arg)
      end else begin
         if param.InheritsFrom(TByRefParamSymbol) then
            pushOperator.InitPushTempAddr(param.StackAddr, arg)
         else pushOperator.InitPushResult(param.StackAddr, arg);
      end;
   end;

   if Assigned(FFunc.Result) then
      FPushExprs[FArgs.Count].InitPushInitResult(FFunc.Result.StackAddr, Self);

end;

// Initialize
//
procedure TFuncExpr.Initialize;
begin
   inherited;
   if Assigned(FFunc.Executable) then
      FFunc.Executable.InitExpression(Self);
   AddPushExprs;
end;

procedure TFuncExpr.SetResultAddr(exec : TdwsExecution; ResultAddr: Integer);
begin
  if ResultAddr = -1 then
  begin
    if (exec=nil) or (exec.ProgramState = psUndefined) then
      FResultAddr := Prog.GetTempAddr(FTyp.Size)
    else
      FResultAddr := -1; // TFuncExpr.Create called from TInfoFunc.Call
  end
  else
    FResultAddr := ResultAddr;
end;

// IsWritable
//
function TFuncExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

{ TNoPosExprList }

// Destroy
//
destructor TNoPosExprList.Destroy;
begin
   FList.Clean;
   inherited;
end;

function TNoPosExprList.AddExpr(AExpr: TNoPosExpr) : TSymbol;
begin
   FList.Add(AExpr);
   Result:=nil;
end;

// Insert0
//
procedure TNoPosExprList.Insert0(expr : TExprBase);
begin
   FList.Insert(0, expr);
end;

// Delete
//
procedure TNoPosExprList.Delete(index : Integer);
begin
   FList.Delete(index);
end;

function TNoPosExprList.GetExpr(const x: Integer): TNoPosExpr;
begin
  Result := TNoPosExpr(FList.List[x]);
end;

procedure TNoPosExprList.SetExpr(const x: Integer; const Value: TNoPosExpr);
begin
  FList.List[x] := Value;
end;

// GetCount
//
function TNoPosExprList.GetCount : Integer;
begin
   Result:=FList.Count;
end;

procedure TNoPosExprList.Initialize;
var
  x: Integer;
begin
  for x := 0 to FList.Count - 1 do
    TNoPosExpr(FList.List[x]).Initialize;
end;

procedure TNoPosExprList.TypeCheck(const pos : TScriptPos; ExpectedTyp: TSymbol);
var
  x: Integer;
  expr : TNoPosExpr;
begin
   for x := 0 to FList.Count - 1 do begin
      expr:=TNoPosExpr(FList.List[x]);
      expr.TypeCheckNoPos(pos);
      if not expr.Typ.IsCompatible(ExpectedTyp) then
         expr.Prog.CompileMsgs.AddCompilerErrorFmt(pos, CPE_AssignIncompatibleTypes,
                                                   [expr.Typ.Caption, ExpectedTyp.Caption]);
   end;
end;

{ TBinaryOpExpr }

constructor TBinaryOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TNoPosExpr);
begin
   inherited Create(Prog);
   FLeft := aLeft;
   FRight := aRight;
end;

destructor TBinaryOpExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

function TBinaryOpExpr.Eval(exec : TdwsExecution): Variant;
begin
   Assert(False);
end;

procedure TBinaryOpExpr.Initialize;
begin
  FLeft.Initialize;
  FRight.Initialize;
end;

// IsConstant
//
function TBinaryOpExpr.IsConstant : Boolean;
begin
   Result:=FLeft.IsConstant and FRight.IsConstant;
end;

{ TUnaryOpExpr }

constructor TUnaryOpExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
  inherited Create(Prog);
  FExpr := Expr;
end;

destructor TUnaryOpExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

procedure TUnaryOpExpr.Initialize;
begin
  FExpr.Initialize;
end;

// IsConstant
//
function TUnaryOpExpr.IsConstant : Boolean;
begin
   Result:=FExpr.IsConstant;
end;

// ------------------
// ------------------ TUnaryOpBoolExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpBoolExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
   inherited;
   Typ:=Prog.TypBoolean;
end;

// Eval
//
function TUnaryOpBoolExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsBoolean(exec);
end;

// ------------------
// ------------------ TUnaryOpIntExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpIntExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
   inherited;
   Typ:=Prog.TypInteger;
end;

// Eval
//
function TUnaryOpIntExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsInteger(exec);
end;

// ------------------
// ------------------ TUnaryOpFloatExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpFloatExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
   inherited;
   Typ:=Prog.TypFloat;
end;

// Eval
//
function TUnaryOpFloatExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsFloat(exec);
end;

// ------------------
// ------------------ TUnaryOpStringExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpStringExpr.Create(Prog: TdwsProgram; Expr: TNoPosExpr);
begin
   inherited;
   Typ:=Prog.TypString;
end;

// Eval
//
function TUnaryOpStringExpr.Eval(exec : TdwsExecution) : Variant;
var
   str : String;
begin
   EvalAsString(exec, str);
   Result:=str;
end;

// ------------------
// ------------------ TMethodExpr ------------------
// ------------------

// Create
//
constructor TMethodExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  Func: TMethodSymbol; BaseExpr: TDataExpr);
begin
   inherited Create(Prog, Pos, Func);
   FBaseExpr:=BaseExpr;
   FSelfAddr:=Func.SelfSym.StackAddr;
end;

// Destroy
//
destructor TMethodExpr.Destroy;
begin
   FBaseExpr.Free;
   inherited;
end;

// TypeCheckNoPos
//
procedure TMethodExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FBaseExpr.TypeCheckNoPos(aPos);
   inherited;
end;

// Initialize
//
procedure TMethodExpr.Initialize;
begin
   inherited;
   FBaseExpr.Initialize;
end;

// ------------------
// ------------------ TMethodStaticExpr ------------------
// ------------------

// Eval
//
function TMethodStaticExpr.Eval(exec : TdwsExecution) : Variant;
var
   scriptObj : Pointer;
   oldSelf : PIScriptObj;
begin
   scriptObj:=nil;
   oldSelf:=exec.SelfScriptObject;
   try
      exec.SelfScriptObject:=@scriptObj;
      Result:=inherited Eval(exec);
   finally
      exec.SelfScriptObject:=oldSelf;
      IScriptObj(scriptObj):=nil;
   end;
end;

// PreCall
//
function TMethodStaticExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
begin
   FBaseExpr.EvalAsScriptObj(exec, exec.SelfScriptObject^);
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FSelfAddr, exec.SelfScriptObject^);

   Result:=FFunc;
end;

// ------------------
// ------------------ TMethodVirtualExpr ------------------
// ------------------

// FindVirtualMethod
//
function TMethodVirtualExpr.FindVirtualMethod(ClassSym: TClassSymbol): TMethodSymbol;
begin
   Result:=ClassSym.VMTMethod(TMethodSymbol(FFunc).VMTIndex);
end;

// PreCall
//
function TMethodVirtualExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
begin
   // Find virtual method
   FBaseExpr.EvalAsScriptObj(exec, exec.SelfScriptObject^);
   CheckScriptObject(exec.SelfScriptObject^);
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FSelfAddr, exec.SelfScriptObject^);
   Result:=FindVirtualMethod(exec.SelfScriptObject^.ClassSym);
end;

// ------------------
// ------------------ TClassMethodStaticExpr ------------------
// ------------------

// PreCall
//
function TClassMethodStaticExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
var
   buf : Int64;
begin
   if FBaseExpr.Typ is TClassOfSymbol then
      buf:=FBaseExpr.EvalAsInteger(exec)
   else begin
      FBaseExpr.EvalAsScriptObj(exec, exec.SelfScriptObject^);
      CheckScriptObject(exec.SelfScriptObject^);
      buf:=Int64(exec.SelfScriptObject^.ClassSym);
      exec.SelfScriptObject^:=nil;
   end;
   exec.Stack.WriteIntValue(exec.Stack.StackPointer + FSelfAddr, buf);
   Result := FFunc;
end;

// ------------------
// ------------------ TClassMethodVirtualExpr ------------------
// ------------------

// FindVirtualMethod
//
function TClassMethodVirtualExpr.FindVirtualMethod(exec : TdwsExecution) : TMethodSymbol;
var
   clsInt : Int64;
   classSym : TClassSymbol;
begin
   clsInt:=exec.Stack.ReadIntValue(exec.Stack.StackPointer+FSelfAddr);
   classSym:=TClassSymbol(clsInt);
   if classSym=nil then
      RaiseScriptError(EScriptError.Create(RTE_ClassTypeIsNil));

   Result:=ClassSym.VMTMethod(TMethodSymbol(FFunc).VMTIndex);
end;

// PreCall
//
function TClassMethodVirtualExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
begin
   inherited PreCall(exec);

   Result:=FindVirtualMethod(exec);
end;

// ------------------
// ------------------ TConstructorStaticExpr ------------------
// ------------------

constructor TConstructorStaticExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
   Func: TMethodSymbol; Base: TDataExpr);
begin
  inherited Create(Prog, Pos, Func, Base);
  if Base.Typ is TClassOfSymbol then
    FTyp := Base.Typ.Typ
  else
    FTyp := Base.Typ;
end;

// PreCall
//
function TConstructorStaticExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
begin
   // Create object
   exec.SelfScriptObject^:=TScriptObj.Create(TClassSymbol(FTyp), exec as TdwsProgramExecution);
   exec.SelfScriptObject^.ExternalObject:=exec.ExternalObject;

   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FSelfAddr, exec.SelfScriptObject^);

   Result:=FFunc;
end;

// PostCall
//
function TConstructorStaticExpr.PostCall(exec : TdwsExecution) : Variant;
begin
   Assert(FResultAddr=-1);
   Result:=exec.SelfScriptObject^;
end;

{ TConstructorVirtualExpr }

constructor TConstructorVirtualExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
   Func: TMethodSymbol; Base: TDataExpr);
begin
  inherited Create(Prog, Pos, Func, Base);
  FTyp := Base.Typ.Typ;
end;

// PreCall
//
function TConstructorVirtualExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
var
  classInt : Int64;
  classSym : TClassSymbol;
begin
  // Get class symbol
  classInt := FBaseExpr.EvalAsInteger(exec);
  classSym := TClassSymbol(classInt);
  Assert(classSym <> nil);

  if classSym.IsAbstract then
    RaiseScriptError(RTE_InstanceOfAbstractClass);

  Result := FindVirtualMethod(classSym);

  // Create object
  exec.SelfScriptObject^ := TScriptObj.Create(classSym, exec as TdwsProgramExecution);
  exec.SelfScriptObject^.ExternalObject := ExternalObject;

  exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer + FSelfAddr, exec.SelfScriptObject^);
end;

// PostCall
//
function TConstructorVirtualExpr.PostCall(exec : TdwsExecution) : Variant;
begin
   // Return Self as Result
   Assert(FResultAddr=-1);
   Result:=exec.SelfScriptObject^;
end;

{ TProgramInfo }

function TProgramInfo.GetValueAsVariant(const s: string): Variant;
begin
  Result := GetVars(s).Value;
end;

function TProgramInfo.GetData(const s: string): TData;
begin
  Result := GetVars(s).Data;
end;

procedure TProgramInfo.SetValueAsVariant(const s: string; const Value: Variant);
begin
  GetVars(s).Value := Value;
end;

procedure TProgramInfo.SetData(const s: string; const Value: TData);
begin
  GetVars(s).Data := Value;
end;

function TProgramInfo.GetVars(const str : string): IInfo;

   procedure GetExternalVarSymbolInfo(sym : TSymbol; var Result : IInfo);
   var
      dat : TData;
      extVDM : TExternalVarDataMaster;
   begin
      SetLength(dat, sym.Typ.Size);
      extVDM := TExternalVarDataMaster.Create(Execution, TExternalVarSymbol(sym));
      if sym.Typ is TClassSymbol then
         extVDM.Read(Execution, dat); // initialize 'Self'-Object
      TInfo.SetChild(Result, Self, sym.Typ, dat, 0, extVDM);
   end;

   procedure GetTypeSymbolInfo(sym : TSymbol; var Result : IInfo);
   var
      dat : TData;
   begin
      if sym.BaseType is TClassSymbol then begin
         SetLength(dat, 1);
         VarClear(dat[0]);
         Result := TInfoClassObj.Create(Self, sym, dat, 0)
      end else Result:=nil;
   end;

   procedure GetVarParamVars(sym : TDataSymbol; basePointer : Integer; var Result : IInfo);
   var
      vpd : IVarParamData;
   begin
      vpd:=IVarParamData(IUnknown(Execution.Stack.Data[basePointer+sym.StackAddr]));
      TInfo.SetChild(Result, Self, sym.Typ, vpd.Data, vpd.Addr);
   end;

   procedure GetDataSymbol(sym : TDataSymbol; var Result : IInfo);
   var
      basePointer : Integer;
      pin : TProgramInfo;
      exec : TdwsExecution;
   begin
      pin:=Self;
      exec:=pin.Execution;
      if sym.Level=pin.FLevel then
         basePointer:=exec.Stack.BasePointer
      else basePointer:=exec.Stack.GetSavedBp(pin.Level);
      if sym is TVarParamSymbol then begin
         GetVarParamVars(sym, basePointer, Result);
      end else begin
         TInfo.SetChild(Result, pin, sym.Typ, exec.Stack.Data,
                        basePointer+sym.StackAddr);
      end;
   end;

   procedure GetFieldSymbol(sym : TFieldSymbol; var Result : IInfo);
   begin
      // Field of the Self object
      TInfo.SetChild(Result, Self, sym.Typ, FScriptObj.Data, sym.Offset)
   end;

   procedure GetConstSymbol(sym : TConstSymbol; var Result : IInfo);
   begin
      TInfo.SetChild(Result, Self, sym.Typ, sym.Data, 0)
   end;

var
   sym : TSymbol;
begin
   sym:=FTable.FindSymbol(str, cvMagic);

   if not Assigned(sym) then
      RaiseVariableNotFound(str)
   else if sym is TDataSymbol then
      GetDataSymbol(TDataSymbol(sym), Result)
   else if sym is TConstSymbol then
      GetConstSymbol(TConstSymbol(sym), Result)
   else if sym is TFieldSymbol then
      GetFieldSymbol(TFieldSymbol(sym), Result)
   else if sym is TExternalVarSymbol then
      GetExternalVarSymbolInfo(sym, Result)
   else if sym is TTypeSymbol then
      GetTypeSymbolInfo(sym, Result)
   else RaiseOnlyVarSymbols(sym);
end;

function TProgramInfo.GetFunc(const s: string): IInfo;
var
  sym: TSymbol;
begin
  sym := FTable.FindSymbol(s, cvMagic);

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_FunctionNotFound, [s]);

  if sym is TFuncSymbol then
  begin
    if Assigned(FScriptObj) then
      Result := TInfoFunc.Create(Self, sym, nil, 0, nil, FScriptObj, FScriptObj.ClassSym)
    else
      Result := TInfoFunc.Create(Self, sym, nil, 0, nil, nil, nil)
  end
  else
    raise Exception.CreateFmt(RTE_OnlyFuncSymbols, [sym.Caption]);
end;

function TProgramInfo.GetTemp(const DataType: string): IInfo;
var
  data: TData;
  typSym: TSymbol;
begin
  typSym := FTable.FindSymbol(DataType, cvMagic);

  if not Assigned(typSym) then
    raise Exception.CreateFmt(RTE_DatatypeNotFound, [DataType]);

  data := nil;
  SetLength(data, typSym.Size);
  typSym.InitData(data, 0);

  TInfo.SetChild(Result, Self, typSym, data, 0);
end;

procedure TProgramInfo.SetFuncSym(const Value: TFuncSymbol);
begin
  FFuncSym := Value;
  if Assigned(FFuncSym) then
    FLevel := FFuncSym.Level      // 1
  else
    FLevel := 0;
end;

procedure TProgramInfo.SetResultAsVariant(const Value: Variant);
begin
  GetResultVars.Value := Value;
end;

function TProgramInfo.GetResultAsVariant: Variant;
begin
  Result := GetResultVars.Value;
end;

// GetResultVars
//
function TProgramInfo.GetResultVars: IInfo;
begin
   Result:=GetVars(SYS_RESULT);
end;

function TProgramInfo.GetValueAsString(const s: string): String;
begin
  Result:=GetVars(s).ValueAsString;
end;

procedure TProgramInfo.SetValueAsString(const s: string; const Value: String);
begin
  GetVars(s).Value:=Value;
end;

// GetValueAsChar
//
function TProgramInfo.GetValueAsChar(const s: string): Char;
var
   buf : String;
begin
   buf:=GetVars(s).ValueAsString;
   if buf<>'' then
      Result:=buf[1]
   else Result:=#0;
end;

// GetValueAsDataString
//
function TProgramInfo.GetValueAsDataString(const s: string): RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetValueAsString(s));
end;

// SetValueAsDataString
//
procedure TProgramInfo.SetValueAsDataString(const s: string; const Value: RawByteString);
begin
   SetValueAsString(s, RawByteStringToScriptString(Value));
end;

function TProgramInfo.GetValueAsInteger(const s: String): Int64;
begin
  Result:=GetVars(s).ValueAsInteger;
end;

procedure TProgramInfo.SetValueAsInteger(const s: String; const Value: Int64);
begin
  GetVars(s).Value:=Value;
end;

function TProgramInfo.GetValueAsBoolean(const s: String): Boolean;
begin
  Result:=GetVars(s).Value;
end;

procedure TProgramInfo.SetValueAsBoolean(const s: String; const Value: Boolean);
begin
  GetVars(s).Value:=Value;
end;

function TProgramInfo.GetValueAsFloat(const s: String): Double;
begin
  Result:=GetVars(s).ValueAsFloat;
end;

procedure TProgramInfo.SetValueAsFloat(const s: String; const Value: Double);
begin
  GetVars(s).Value:=Value;
end;

function TProgramInfo.GetValueAsObject(const s: String): TObject;
var
   info : IInfo;
   scriptobj : IScriptObj;
begin
   info:=GetVars(s);
   if Assigned(info) then begin
      scriptobj:=info.ScriptObj;
      if Assigned(scriptobj) then
         Result:=scriptobj.ExternalObject
      else Result:=nil;
   end else Result:=nil;
end;

// GetValueAsClassSymbol
//
function TProgramInfo.GetValueAsClassSymbol(const s: String): TClassSymbol;
begin
   Result:=TClassSymbol(GetVars(s).ValueAsInteger);
end;

function TProgramInfo.GetValueAsTStrings(const s: String): TStrings;
var
   obj : TObject;
begin
   obj:=GetValueAsObject(s);
   if Assigned(obj) then
      Result:=obj as TStrings
   else Result:=nil;
end;

procedure TProgramInfo.SetResultAsString(const value : String);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

function TProgramInfo.GetResultAsString : String;
begin
   Result:=GetVars(SYS_RESULT).ValueAsString;
end;

// SetResultAsDataString
//
procedure TProgramInfo.SetResultAsDataString(const value : RawByteString);
begin
   SetResultAsString(RawByteStringToScriptString(value));
end;

// GetResultAsDataString
//
function TProgramInfo.GetResultAsDataString : RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetResultAsString);
end;

procedure TProgramInfo.SetResultAsInteger(const value : Int64);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

function TProgramInfo.GetResultAsInteger : Int64;
begin
   Result:=GetVars(SYS_RESULT).ValueAsInteger;
end;

procedure TProgramInfo.SetResultAsBoolean(const value : Boolean);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

function TProgramInfo.GetResultAsBoolean : Boolean;
begin
   Result:=GetVars(SYS_RESULT).Value;
end;

procedure TProgramInfo.SetResultAsFloat(const value : Double);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

function TProgramInfo.GetResultAsFloat : Double;
begin
   Result:=GetVars(SYS_RESULT).Value;
end;

// GetParamAsPVariant
//
function TProgramInfo.GetParamAsPVariant(index : Integer) : PVariant;

   function GetVarParam(stackAddr : Integer) : PVariant;
   var
      vpd : IVarParamData;
   begin
      vpd:=IVarParamData(IUnknown(Execution.Stack.Data[stackAddr]));
      Result:=@vpd.Data[vpd.Addr];
   end;

var
   ip : TSymbolTable;
   sym : TDataSymbol;
   stackAddr : Integer;
   exec : TdwsExecution;
begin
   ip:=FuncSym.Params;
   if Cardinal(index)>=Cardinal(ip.Count) then begin
      RaiseIncorrectParameterIndex(index);
      Result:=nil;
   end else begin
      sym:=TDataSymbol(ip[index]);
      Assert(sym.InheritsFrom(TDataSymbol));
      exec:=Execution;
      if sym.Level=FLevel then
         stackAddr:=sym.StackAddr+exec.Stack.BasePointer
      else stackAddr:=sym.StackAddr+exec.Stack.GetSavedBp(Level);
      if sym.InheritsFrom(TByRefParamSymbol) then
         Result:=GetVarParam(stackAddr)
      else Result:=@exec.Stack.Data[stackAddr];
   end;
end;

// GetParamAsVariant
//
function TProgramInfo.GetParamAsVariant(index : Integer) : Variant;
begin
   Result:=GetParamAsPVariant(index)^;
end;

// GetParamAsInteger
//
function TProgramInfo.GetParamAsInteger(index : Integer) : Int64;
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   if p^.VType=varInt64 then
      Result:=p.VInt64
   else Result:=PVariant(p)^;
end;

// GetParamAsString
//
function TProgramInfo.GetParamAsString(index : Integer) : String;
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   if p^.VType=varUString then
      Result:=String(p.VUString)
   else Result:=PVariant(p)^;
end;

// GetParamAsFloat
//
function TProgramInfo.GetParamAsFloat(index : Integer) : Double;
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   if p^.VType=varDouble then
      Result:=p.VDouble
   else Result:=PVariant(p)^;
end;

// GetParamAsBoolean
//
function TProgramInfo.GetParamAsBoolean(index : Integer) : Boolean;
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   if p^.VType=varBoolean then
      Result:=p.VBoolean
   else Result:=PVariant(p)^;
end;

function TProgramInfo.FindClassMatch(AObject: TObject; ExactMatch: Boolean): TClassSymbol;
var
  ParentRTTI: PPTypeInfo;
  unitList: TList;      // build the list once, may search for many symbols
  typeSym: TSymbol;
begin
  Result := nil;
  if AObject = nil then
    Exit;

  { Cycle the AObject class hierarchy and determine the DWS class type that most
    closely matches the AObject type. Return the IInfo for that matching
    class. It should *always* at least match at TObject. }
  unitList := CreateUnitList;
  try
    // Check current class type. If not found cycle object ancestry
    typeSym := FindSymbolInUnits(unitList, AObject.ClassName);
    // If no exact match found then look for supported ancestors
    if Assigned(typeSym) and (typeSym is TClassSymbol) then
      Result := TClassSymbol(typeSym);

    // Allowed to look through ancestor types
    if not ExactMatch then
    begin
      if AObject.ClassInfo <> nil then
      begin
        ParentRTTI := GetTypeData(AObject.ClassInfo).ParentInfo;
        repeat
          typeSym := FindSymbolInUnits(unitList, String(ParentRTTI^.Name));
          if Assigned(typeSym) and (typeSym is TClassSymbol) then       // match found, stop searching
          begin
            Result := TClassSymbol(typeSym);
            Break;
          end
          else                               // if no match found yet, try higher ancestor
            ParentRTTI := GetTypeData(ParentRTTI^)^.ParentInfo;
        until ParentRTTI = nil;
      end;{if Assigned}
    end;{if not ExactMatch}
    // If no matches found, error
    if Result = nil then
      raise Exception.CreateFmt(RTE_ClassMatchNotFound, [AObject.ClassName]);
  finally
    unitList.Free;
  end;
end;

function TProgramInfo.RegisterExternalObject(AObject: TObject; AutoFree: Boolean; ExactClassMatch: Boolean): Variant;
var
  NewScriptObj: IScriptObj;
  ClassSym: TClassSymbol;
  context: TdwsProgramExecution;
begin
  Assert(Assigned(Execution));
  { This will register an external object (known or not known to the system)
    with the DWS system. If an object that is already registered is passed in
    it will NOT point to the same script object. Currently it is too difficult
    (if not impossible) to obtain the IScriptObj for an existing
    registered external object. This is very useful for registering a new object
    like a TField (ie TDataset.FieldByName()) so the script can refer to the
    object and act on it. }
  ClassSym := FindClassMatch(AObject, ExactClassMatch) as TClassSymbol;
  if Assigned(ClassSym) and Assigned(AObject) then
  begin
    if AutoFree then
      context := Execution
    else
      context := nil;
    NewScriptObj := TScriptObj.Create(ClassSym, context);
    NewScriptObj.ExternalObject := AObject;
    Result := IScriptObj(NewScriptObj);
  end
  else                                     // no class returned or no object provided
    Result := Unassigned;                  // return 'nil' Id
end;

function TProgramInfo.GetExternalObjForVar(const s: string): TObject;
var
  sObj: IScriptObj;
begin
  sObj := Vars[s].ScriptObj;
  if Assigned(sObj) then
    Result := sObj.ExternalObject
  else
    Result := nil;
end;

function TProgramInfo.FindSymbolInUnits(AUnitList: TList; const Name: string): TSymbol;
var
  i: Integer;
begin
  // Search all units for the symbol
  Result := nil;
  for i := 0 to AUnitList.Count - 1 do
  begin
    Result := TUnitSymbol(AUnitList[i]).Table.FindLocal(Name);
    if Assigned(Result) then
      Break;
  end;
end;

// PrepareScriptObj
//
procedure TProgramInfo.PrepareScriptObj;
begin
   FScriptObj:=FExecution.SelfScriptObject^;
end;

function TProgramInfo.FindSymbolInUnits(const Name: string): TSymbol;
var
  list: TList;
begin
  list := CreateUnitList;
  try
    Result := FindSymbolInUnits(list, Name);
  finally
    list.Free;
  end;
end;

function TProgramInfo.CreateUnitList: TList;
var
  root: TSymbolTable;
  i: Integer;
begin
  // Find the root table for the full compiled program (not just the function)
  if Assigned(Execution) then
    root := Execution.Prog.RootTable
  else
  // if no caller provided, make a 'best effort' to find a root.
  begin
    root := FTable;
    while root.ParentCount > 0 do
      root := root.Parents[0];
  end;

  Result := TList.Create;                         // caller reponsible for freeing
  // Add all unit symbols to a list
  for i := 0 to root.Count - 1 do
    if root.Symbols[i] is TUnitSymbol then        // if a unit symbol
      if Result.IndexOf(root.Symbols[i]) < 0 then // and not already in list (units may reuse others)
        Result.Add(root.Symbols[i]);
end;

// ------------------
// ------------------ TScriptObj ------------------
// ------------------

// Create
//
constructor TScriptObj.Create(aClassSym : TClassSymbol; executionContext : TdwsProgramExecution);
var
   i : Integer;
   classSymIter : TClassSymbol;
   externalClass : TClassSymbol;
   fs : TFieldSymbol;
   member : TSymbol;
begin
   FClassSym:=aClassSym;

   if executionContext<>nil then
      executionContext.ScriptObjCreated(Self);

   SetLength(FData, aClassSym.ScriptInstanceSize);

   // initialize fields
   classSymIter:=aClassSym;
   while classSymIter<>nil do begin
      for i:=0 to classSymIter.Members.Count-1 do begin
         member:=classSymIter.Members[i];
         if member is TFieldSymbol then begin
            fs:=TFieldSymbol(member);
            fs.Typ.InitData(FData, fs.Offset);
         end;
      end;
      classSymIter := classSymIter.Parent;
   end;

   // initialize OnObjectDestroy
   externalClass:=aClassSym;
   while (externalClass<>nil) and not Assigned(externalClass.OnObjectDestroy) do
      externalClass:=externalClass.Parent;
   if externalClass<>nil then
      FOnObjectDestroy:=externalClass.OnObjectDestroy;
end;

// BeforeDestruction
//
procedure TScriptObj.BeforeDestruction;
var
   iso : IScriptObj;
begin
   if Assigned(FExecutionContext) then begin
      // we are released, so never do: Self as IScriptObj
      FDestroyed:=True;
      iso:=TScriptObjectWrapper.Create(Self);
      ExecutionContext.DestroyScriptObj(iso);
      ExecutionContext.ScriptObjDestroyed(Self);
   end;
   inherited;
end;

// Destroy
//
destructor TScriptObj.Destroy;
begin
   if Assigned(FOnObjectDestroy) then
      FOnObjectDestroy(FExternalObj);
   inherited;
end;

function TScriptObj.GetClassSym: TClassSymbol;
begin
  Result := FClassSym;
end;

function TScriptObj.GetData : TData;
begin
  Result := FData;
end;

// DataOfAddr
//
function TScriptObj.DataOfAddr(addr : Integer) : Variant;
begin
   Result:=FData[addr];
end;

// DataOfAddrAsString
//
function TScriptObj.DataOfAddrAsString(addr : Integer) : String;
begin
   Result:=FData[addr];
end;

// DataOfAddrAsInteger
//
function TScriptObj.DataOfAddrAsInteger(addr : Integer) : Int64;
begin
   Result:=FData[addr];
end;

// DataOfAddrAsScriptObj
//
procedure TScriptObj.DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
var
   varData : PVarData;
begin
   varData:=@FData[addr];
   if varData.VType=varUnknown then
      scriptObj:=IScriptObj(IUnknown(varData.VUnknown))
   else Assert(False);
//   scriptObj:=IScriptObj(IUnknown(FData[addr]));
end;

function TScriptObj.GetExternalObject: TObject;
begin
  Result := FExternalObj;
end;

// GetDestroyed
//
function TScriptObj.GetDestroyed : Boolean;
begin
   Result:=FDestroyed;
end;

// SetDestroyed
//
procedure TScriptObj.SetDestroyed(const val : Boolean);
begin
   FDestroyed:=True;
end;

procedure TScriptObj.SetExternalObject(Value: TObject);
begin
  FExternalObj := Value;
end;

{ TInfo }

constructor TInfo.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; const DataMaster: IDataMaster = nil);
begin
  FProgramInfo := ProgramInfo;
  if Assigned(ProgramInfo) then
    FExec := ProgramInfo.Execution;
  FTypeSym := TypeSym;
  FData := Data;
  FOffset := Offset;
  FDataMaster := DataMaster;
end;

function TInfo.Call(const Params: array of Variant): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Call', FTypeSym.Caption]);
end;

function TInfo.Call: IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Call', FTypeSym.Caption]);
end;

function TInfo.Element(const Indices: array of Integer): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Element', FTypeSym.Caption]);
end;

function TInfo.GetConstructor(const MethName: string; ExtObject: TObject): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['GetConstructor', FTypeSym.Caption]);
end;

function TInfo.GetData : TData;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Data', FTypeSym.Caption]);
end;

function TInfo.GetExternalObject: TObject;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['ExternalObject', FTypeSym.Caption]);
end;

function TInfo.GetMember(const s: string): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Member', FTypeSym.Caption]);
end;

function TInfo.GetMethod(const s: string): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Method', FTypeSym.Caption]);
end;

function TInfo.GetScriptObj: IScriptObj;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Obj', FTypeSym.Caption]);
end;

function TInfo.GetParameter(const s: string): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Parameter', FTypeSym.Caption]);
end;

function TInfo.GetTypeSym: TSymbol;
begin
  Result := FTypeSym;
end;

function TInfo.GetValue: Variant;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Value', FTypeSym.Caption]);
end;

function TInfo.GetValueAsString : String;
begin
   Result:=GetValue;
end;

// GetValueAsDataString
//
function TInfo.GetValueAsDataString : RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetValueAsString);
end;

// GetValueAsInteger
//
function TInfo.GetValueAsInteger : Int64;
begin
   Result:=GetValue;
end;

// GetValueAsFloat
//
function TInfo.GetValueAsFloat : Double;
begin
   Result:=GetValue;
end;

class procedure TInfo.SetChild(var Result : IInfo; ProgramInfo: TProgramInfo;
  ChildTypeSym: TSymbol; const ChildData: TData; ChildOffset: Integer;
  const ChildDataMaster: IDataMaster = nil);
var
   BaseType : TTypeSymbol;
begin
  Assert(Assigned(ChildTypeSym));
  BaseType := ChildTypeSym.BaseType;

  if    (BaseType is TBaseSymbol)
     or (BaseType is TEnumerationSymbol)
     or (BaseType is TConnectorSymbol) then
      Result := TInfoData.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
                                 ChildDataMaster)
  else if ChildTypeSym is TFuncSymbol then
    Result := TInfoFunc.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
                               ChildDataMaster, nil, nil)
  else if BaseType is TRecordSymbol then
    Result := TInfoRecord.Create(ProgramInfo, ChildTypeSym, ChildData,
                                 ChildOffset, ChildDataMaster)
  else if BaseType is TStaticArraySymbol then
    Result := TInfoStaticArray.Create(ProgramInfo, ChildTypeSym, ChildData,
                                      ChildOffset, ChildDataMaster)
  else if BaseType is TDynamicArraySymbol then
    Result := TInfoDynamicArray.Create(ProgramInfo, ChildTypeSym, ChildData,
                                       ChildOffset, ChildDataMaster)
  else if BaseType is TClassSymbol then
    Result := TInfoClassObj.Create(ProgramInfo, ChildTypeSym, ChildData,
                                   ChildOffset, ChildDataMaster)
  else if BaseType is TClassOfSymbol then
    Result := TInfoClassOf.Create(ProgramInfo, ChildTypeSym, ChildData,
                                  ChildOffset, ChildDataMaster)
  else
    Assert(False); // Shouldn't be ever executed
end;

procedure TInfo.SetData(const Value: TData);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Data', FTypeSym.Caption]);
end;

procedure TInfo.SetExternalObject(ExtObject: TObject);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['ExternalObject', FTypeSym.Caption]);
end;

procedure TInfo.SetValue(const Value: Variant);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Value', FTypeSym.Caption]);
end;

function TInfo.GetInherited: IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['GetInherited', FTypeSym.Caption]);
end;

{ TInfoData }

function TInfoData.GetData;
begin
  if Assigned(FDataMaster) then
    FDataMaster.Read(FExec, FData);

  SetLength(Result, FTypeSym.Size);
  CopyData(FData, FOffset, Result, 0, FTypeSym.Size);
end;

function TInfoData.GetValue: Variant;
begin
   if Assigned(FDataMaster) then
      FDataMaster.Read(FExec, FData);
   if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
      Result := FData[FOffset]
   else raise Exception.CreateFmt(RTE_CanNotReadComplexType, [FTypeSym.Caption]);
end;

// GetValueAsString
//
function TInfoData.GetValueAsString : String;
var
   varData : PVarData;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then begin
      varData:=@FData[FOffset];
      if varData.VType=varUString then
         Result:=String(varData.VUString)
      else Result:=PVariant(varData)^;
   end else Result:=inherited GetValueAsString;
end;

// GetValueAsInteger
//
function TInfoData.GetValueAsInteger : Int64;
var
   varData : PVarData;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then begin
      varData:=@FData[FOffset];
      if varData.VType=varInt64 then
         Result:=varData.VInt64
      else Result:=PVariant(varData)^;
   end else Result:=inherited GetValueAsInteger;
end;

// GetValueAsFloat
//
function TInfoData.GetValueAsFloat : Double;
var
   varData : PVarData;
begin
   if (FDataMaster=nil) and (FTypeSym<>nil) and (FTypeSym.Size=1) then begin
      varData:=@FData[FOffset];
      if varData.VType=varDouble then
         Result:=varData.VDouble
      else Result:=PVariant(varData)^;
   end else Result:=inherited GetValueAsFloat;
end;

procedure TInfoData.SetData(const Value: TData);
begin
  if Length(Value) <> FTypeSym.Size then
    raise Exception.CreateFmt(RTE_InvalidInputDataSize, [Length(Value), FTypeSym.Size]);
  CopyData(Value, 0, FData, FOffset, FTypeSym.Size);

  if Assigned(FDataMaster) then
  begin
    if FTypeSym.Size = FDataMaster.Size then
      FDataMaster.Write(FExec, FData)
    else
      raise Exception.CreateFmt(RTE_CanOnlyWriteBlocks, [FDataMaster.Caption, FTypeSym.Caption]);
  end;
end;

procedure TInfoData.SetValue(const Value: Variant);
begin
  if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
    FData[FOffset] := Value
  else
    raise Exception.CreateFmt(RTE_CanNotSetValueForType, [FTypeSym.Caption]);

  if Assigned(FDataMaster) then
    FDataMaster.Write(FExec, FData);
end;

{ TInfoClass }

function TInfoClass.GetConstructor(const MethName: string;
  ExtObject: TObject): IInfo;
begin
  Result := GetMethod(MethName);
  Result.ExternalObject := ExtObject;
end;

function TInfoClass.GetInherited: IInfo;
begin
  SetChild(Result, FProgramInfo,(FTypeSym as TClassSymbol).Parent,FData,
           FOffset,FDataMaster);
end;

function TInfoClass.GetMethod(const s: string): IInfo;
var
  sym: TSymbol;
begin
  if not (FTypeSym is TClassSymbol) then
    raise Exception.CreateFmt(RTE_NoClassNoMethod, [FTypeSym.Caption, s]);

  sym := TClassSymbol(FTypeSym).Members.FindSymbol(s, cvMagic);

  if not (sym is TMethodSymbol) then
    sym := nil;

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_MethodNotFoundInClass, [s, FTypeSym.Caption]);

  Result := TInfoFunc.Create(FProgramInfo, sym, nil, 0, nil, FScriptObj, TClassSymbol(FTypeSym));
end;

function TInfoClass.GetScriptObj: IScriptObj;
begin
  Result := FScriptObj;
end;

{ TInfoClassObj }

constructor TInfoClassObj.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; const DataMaster: IDataMaster);
begin
  inherited;
  if VarType(Data[Offset]) = varUnknown then
    FScriptObj := IScriptObj(IUnknown(Data[Offset]))
  else
    FScriptObj := nil;
end;

function TInfoClassObj.GetMember(const s: string): IInfo;
var
  member: TSymbol;
begin
  member := FScriptObj.ClassSym.Members.FindLocal(s);

  if member is TFieldSymbol then
    SetChild(Result, FProgramInfo, member.Typ, FScriptObj.Data,
             TFieldSymbol(member).Offset)
  else if member is TPropertySymbol then
  begin
    Result := TInfoProperty.Create(FProgramInfo,member.Typ,nil,0,
      TPropertySymbol(member),FScriptObj);
  end
  else
    raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

{ TTempParam }

constructor TTempParam.Create(ParamSym: TSymbol);
begin
  inherited Create(ParamSym.Name, ParamSym.Typ);
  FIsVarParam := ParamSym is TVarParamSymbol;
  SetLength(FData, Size);
  ParamSym.InitData(FData, 0);
end;

{ TInfoFunc }

constructor TInfoFunc.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; DataMaster: IDataMaster;
  ScriptObj: IScriptObj; ClassSym: TClassSymbol; ForceStatic: Boolean);
begin
  inherited Create(ProgramInfo, TypeSym, Data, Offset, DataMaster);
  FScriptObj := ScriptObj;
  FClassSym := ClassSym;
  FParams := TFuncSymbol(FTypeSym).Params;
  FParamSize := TFuncSymbol(FTypeSym).ParamSize;
  FTempParams := TSymbolTable.Create;
  FForceStatic := ForceStatic;

  if Assigned(TFuncSymbol(FTypeSym).Typ) then
    SetLength(FResult, TFuncSymbol(FTypeSym).Typ.Size)
  else if (FTypeSym is TMethodSymbol) and (TMethodSymbol(FTypeSym).Kind = fkConstructor) then
    SetLength(FResult, 1);
end;

destructor TInfoFunc.Destroy;
begin
  FTempParams.Free;
  inherited;
end;

function TInfoFunc.Call: IInfo;
var
   x: Integer;
   tp: TTempParam;
   funcExpr: TFuncExpr;
   resultAddr: Integer;
   resultData: TData;
begin
   resultData := nil;
   if not FUsesTempParams then
      InitTempParams;

   // Write the var-params as local variables to the stack.
   for x := 0 to FTempParams.Count - 1 do begin
      tp := TTempParam(FTempParams[x]);
      if tp.IsVarParam then begin
         FExec.Stack.Push(tp.Size);
         FExec.Stack.WriteData(0, FExec.Stack.StackPointer-tp.Size, tp.Size, tp.Data);
      end;
   end;

   try

      // Simulate the params of the functions as local variables
      FExec.Stack.Push(FParamSize);
      try
         // Create the TFuncExpr
         funcExpr := GetFuncExpr(FExec.Prog, TFuncSymbol(FTypeSym), FScriptObj,
                                 FClassSym, FForceStatic, FExec);
         FExec.ExternalObject:=FExternalObject;
         try

            for x := 0 to FTempParams.Count - 1 do begin
               tp := TTempParam(FTempParams[x]);
               if tp.IsVarParam then begin
                  funcExpr.AddArg(TVarExpr.Create(FExec.Prog, tp.Typ, tp));
               end else begin
                  funcExpr.AddArg(TConstExpr.CreateTyped(FExec.Prog, tp.Typ, tp.Data));
               end;
            end;
            funcExpr.Initialize;
            if Assigned(funcExpr.Typ) then begin
               if funcExpr.Typ.Size > 1 then begin
                  // Allocate space on the stack to store the Result value
                  FExec.Stack.Push(funcExpr.Typ.Size);
                  try
                     // Result-space is just behind the temporary-params
                     // (Calculated relative to the basepointer of the caller!)
                     funcExpr.SetResultAddr(FExec, FExec.Stack.StackPointer-funcExpr.Typ.Size);

                     // Execute function.
                     // Result is stored on the stack
                     resultData := funcExpr.GetData(FExec);
                     resultAddr := funcExpr.GetAddr(FExec);

                     // Copy Result
                     CopyData(resultData, resultAddr, FResult, 0, funcExpr.Typ.Size);
                  finally
                     FExec.Stack.Pop(funcExpr.Typ.Size);
                  end;
               end else VarCopy(FResult[0], funcExpr.Eval(FExec));
               SetChild(Result, FProgramInfo, funcExpr.Typ, FResult, 0);
            end else begin
               // Execute as procedure
               FExec.Status:=esrNone;
               funcExpr.EvalNoResult(FExec);
               Assert(FExec.Status=esrNone);
               Result := nil;
            end;
         finally
            FExec.ExternalObject:=nil;
            funcExpr.Free;
         end;
      finally
         FExec.Stack.Pop(FParamSize);
      end;
   finally
      // Copy back the Result of var-parameters
      for x := FTempParams.Count - 1 downto 0 do begin
         tp := TTempParam(FTempParams[x]);
         if tp.IsVarParam then begin
            FExec.Stack.ReadData(FExec.Stack.Stackpointer - tp.Size, 0, tp.Size, tp.Data);
            FExec.Stack.Pop(tp.Size);
         end;
      end;
   end;
end;

function TInfoFunc.Call(const Params: array of Variant): IInfo;
var
  x: Integer;
  funcSym: TFuncSymbol;
  dataSym: TDataSymbol;
  funcExpr: TFuncExpr;
  resultAddr: Integer;
  resultData: TData;
begin
  resultData := nil;
  funcSym := TFuncSymbol(FTypeSym);

  if Length(Params) <> funcSym.Params.Count then
    raise Exception.CreateFmt(RTE_InvalidNumberOfParams, [Length(Params),
      funcSym.Params.Count, FTypeSym.Caption]);

  // Create the TFuncExpr
  funcExpr := GetFuncExpr(FExec.Prog, funcSym, FScriptObj, FClassSym, FForceStatic, FExec);

  FExec.ExternalObject:=FExternalObject;

  try
    // Add arguments to the expression
    for x := Low(Params) to High(Params) do
    begin
      dataSym := TDataSymbol(FParams[x]);

      if dataSym.Size > 1 then
        raise Exception.CreateFmt(RTE_UseParameter, [dataSym.Caption,
          funcSym.Caption]);

      funcExpr.AddArg(TConstExpr.CreateTyped(FExec.Prog, dataSym.Typ, Params[x]));
    end;
    funcExpr.Initialize;
    if Assigned(funcExpr.Typ) then
    begin
      if funcExpr.Typ.Size > 1 then
      begin
        // Allocate space on the stack to store the Result value
        FExec.Stack.Push(funcExpr.Typ.Size);
        try
          // Result-space is just behind the temporary-params
          funcExpr.SetResultAddr(FExec, FParamSize);

          // Execute function.
          // Result is stored on the stack
          resultData := funcExpr.GetData(FExec);
          resultAddr := funcExpr.GetAddr(FExec);

          // Copy Result
          for x := 0 to funcExpr.Typ.Size - 1 do
            FResult[x] := resultData[resultAddr + x];
        finally
          FExec.Stack.Pop(funcExpr.Typ.Size);
        end;
      end
      else
        FResult[0] := funcExpr.Eval(FExec);

      SetChild(Result, FProgramInfo, funcExpr.Typ, FResult, 0);
    end
    else begin
      FExec.Status:=esrNone;
      funcExpr.EvalNoResult(FExec);
      Assert(FExec.Status=esrNone);
    end;
  finally
    FExec.ExternalObject:=nil;
    funcExpr.Free;
  end;
end;

function TInfoFunc.GetParameter(const s: string): IInfo;
var
  tp: TTempParam;
begin
  if not FUsesTempParams then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s, cvMagic));

  if Assigned(tp) then
    SetChild(Result, FProgramInfo, tp.Typ, tp.FData, 0)
  else
    raise Exception.CreateFmt(RTE_NoParameterFound, [s, FTypeSym.Caption]);
end;

procedure TInfoFunc.InitTempParams;
var
  x: Integer;
  tp: TTempParam;
begin
  FTempParamSize := 0;
  for x := 0 to FParams.Count - 1 do
  begin
    tp := TTempParam.Create(FParams[x]);
    FTempParams.AddSymbol(tp);
    if tp.FIsVarParam then
    begin
      tp.StackAddr := FTempParamSize + FExec.Stack.FrameSize;
      Inc(FTempParamSize, tp.Size);
    end;
  end;
  FUsesTempParams := True;
end;

function TInfoFunc.GetExternalObject: TObject;
begin
  Result := FExternalObject;
end;

procedure TInfoFunc.SetExternalObject(ExtObject: TObject);
begin
  FExternalObject := ExtObject;
end;

function TInfoFunc.GetInherited: IInfo;
begin
  if FTypeSym is TMethodSymbol then
    result := TInfoFunc.Create(FProgramInfo,TMethodSymbol(FTypeSym).ParentMeth,
      FData,FOffset,FDataMaster,FScriptObj,FClassSym.Parent,True)
  else
    result := inherited GetInherited;
end;

{ TInfoRecord }

function TInfoRecord.GetMember(const s: string): IInfo;
var
  sym: TSymbol;
begin
  sym := TRecordSymbol(FTypeSym).Members.FindLocal(s);

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_NoRecordMemberFound, [s, FTypeSym.Caption]);

  SetChild(Result, FProgramInfo, sym.Typ, FData,
           FOffset + TMemberSymbol(sym).Offset, FDataMaster);
end;

{ TInfoStaticArray }

function TInfoStaticArray.Element(const Indices: array of Integer): IInfo;
var
  x: Integer;
  elemTyp: TSymbol;
  arrTyp: TStaticArraySymbol;
  elemOff, elemIdx: Integer;
begin
  elemTyp := FTypeSym;
  elemOff := FOffset;
  for x := 0 to Length(Indices) - 1 do
  begin
    if Assigned(elemTyp) and (elemTyp.BaseType is TStaticArraySymbol) then
      arrTyp := TStaticArraySymbol(elemTyp.BaseType)
    else
      raise Exception.Create(RTE_TooManyIndices);

    if Indices[x] > arrTyp.HighBound then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded, [x]);

    if Indices[x] < arrTyp.LowBound then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded, [x]);

    elemTyp := arrTyp.Typ;
    elemIdx := Indices[x] - arrTyp.LowBound;
    elemOff := elemOff + elemIdx * elemTyp.Size;
  end;

  SetChild(Result, FProgramInfo, elemTyp, FData, elemOff, FDataMaster);
end;

function TInfoStaticArray.GetMember(const s: string): IInfo;
var
  h, l: Integer;
begin
  h := TStaticArraySymbol(FTypeSym).HighBound;
  l := TStaticArraySymbol(FTypeSym).LowBound;
  if SameText('length', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, h - l + 1)
  else if SameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, l)
  else if SameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, h)
  else
    raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

{ TInfoDynamicArray }

function TInfoDynamicArray.Element(const Indices: array of Integer): IInfo;
var
  x: Integer;
  elemTyp: TSymbol;
  elemOff: Integer;
begin
  elemTyp := FTypeSym;
  elemOff := FOffset;
  for x := 0 to Length(Indices) - 1 do
  begin
    if Assigned(elemTyp) and (elemTyp.BaseType is TDynamicArraySymbol) then
      elemTyp := elemTyp.BaseType.Typ
    else
      raise Exception.Create(RTE_TooManyIndices);

    elemOff := FData[elemOff];

    if Indices[x] >= FData[elemOff - 1] then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded,[x]);

    if Indices[x] < 0 then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded,[x]);

    elemOff := elemOff + Indices[x] * elemTyp.Size;
  end;

  SetChild(Result, FProgramInfo, elemTyp, FData, elemOff, FDataMaster);
end;

function TInfoDynamicArray.GetMember(const s: string): IInfo;
var
  l : Integer;
  elemOff: Integer;
begin
  elemOff := FData[FOffset];
  l := FData[elemOff - 1];
  if SameText('length', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, l)
  else if SameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, 0)
  else if SameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Execution.Prog.TypInteger, l - 1)
  else
    raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

{ TConnectorExpr }

constructor TConnectorCallExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  const Name: string; BaseExpr: TNoPosExpr; IsWritable: Boolean; IsIndex: Boolean);
begin
  inherited Create(Prog, Pos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
  FIsInstruction := IsWritable;
  FIsWritable := IsWritable;
  FIsIndex := IsIndex;
end;

destructor TConnectorCallExpr.Destroy;
begin
  FBaseExpr.Free;
  FArgs.Clean;
  inherited;
end;

function TConnectorCallExpr.AddArg(ArgExpr: TNoPosExpr) : TSymbol;
begin
   FArgs.Add(ArgExpr);
   Result:=nil;
end;

// TypeCheckNoPos
//
procedure TConnectorCallExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if FArgs.Count>64 then
      AddCompilerErrorFmt(CPE_ConnectorTooManyArguments, [FArgs.Count]);
end;

function TConnectorCallExpr.AssignConnectorSym(ConnectorType: IConnectorType):
  Boolean;
var
  x: Integer;
  typSym: TSymbol;
  arg : TNoPosExpr;
begin
  // Prepare the parameter information array to query the connector symbol
  SetLength(FConnectorParams, FArgs.Count);
  for x := 0 to FArgs.Count - 1 do
  begin
    arg:=TNoPosExpr(FArgs.List[x]);
    FConnectorParams[x].IsVarParam := (arg is TDataExpr) and TDataExpr(arg).IsWritable;
    FConnectorParams[x].TypSym := arg.Typ;
  end;

  // Ask the connector symbol if such a method exists
  if FIsIndex then
    FConnectorCall := ConnectorType.HasIndex(FName, FConnectorParams, typSym, FIsWritable)
  else begin
    FIsWritable := False;
    FConnectorCall := ConnectorType.HasMethod(FName, FConnectorParams, typSym);
  end;

  Result := Assigned(FConnectorCall);

  // Prepare the arguments for the method call
  if Result then
  begin
    SetLength(FConnectorArgs, FArgs.Count);
    for x := 0 to FArgs.Count - 1 do
      SetLength(FConnectorArgs[x], FConnectorParams[x].TypSym.Size);

    FTyp := typSym;
  end;
end;

function TConnectorCallExpr.Eval(exec : TdwsExecution): Variant;
var
  dataSource, dataDest: TData;
  addrSource: Integer;
  x: Integer;
  arg : TNoPosExpr;
begin
  if exec.IsDebugging then
    exec.Debugger.EnterFunc(exec, Self);

  // Call function
  try
    dataSource := nil;
    dataDest := nil;

    for x := 0 to Length(FConnectorArgs) - 1 do
    begin
      arg:=TNoPosExpr(FArgs.List[x]);
      if FConnectorParams[x].TypSym.Size = 1 then
        VarCopy(FConnectorArgs[x][0], arg.Eval(exec))
      else
      begin
        dataSource := TDataExpr(arg).Data[exec];
        addrSource := TDataExpr(arg).Addr[exec];
        dataDest := FConnectorArgs[x];
        CopyData(dataSource, addrSource, dataDest, 0, FConnectorParams[x].TypSym.Size);
      end;
    end;

    try
      // The call itself
      if FBaseExpr is TDataExpr then
        FResultData := FConnectorCall.Call(TDataExpr(FBaseExpr).Data[exec][TDataExpr(FBaseExpr).Addr[exec]], FConnectorArgs)
      else
        FResultData := FConnectorCall.Call(FBaseExpr.Eval(exec), FConnectorArgs);
    except
      on e: EScriptException do
        raise;
      on e: Exception do begin
        exec.SetScriptError(Self);
        raise;
      end;
    end;

    for x := 0 to Length(FConnectorArgs) - 1 do
      if FConnectorParams[x].IsVarParam then
        TDataExpr(FArgs.List[x]).AssignData(exec, FConnectorArgs[x], 0);

  finally
    if exec.IsDebugging then
      exec.Debugger.LeaveFunc(exec, Self);
  end;

  if Assigned(FResultData) then
    Result := FResultData[0]
  else
    VarClear(Result);
end;

function TConnectorCallExpr.GetData(exec : TdwsExecution) : TData;
begin
  Eval(exec);
  Result := FResultData;
end;

procedure TConnectorCallExpr.Initialize;
var
   i : Integer;
begin
	inherited;
   FBaseExpr.Initialize;
   for i:=0 to FArgs.Count-1 do
      TNoPosExpr(FArgs.List[0]).Initialize;
end;

// IsWritable
//
function TConnectorCallExpr.IsWritable : Boolean;
begin
   Result:=FIsWritable;
end;

{ TConnectorReadExpr }

function TConnectorReadExpr.AssignConnectorSym(
  ConnectorType: IConnectorType): Boolean;
begin
  FConnectorMember := ConnectorType.HasMember(FName, FTyp,False);
  Result := Assigned(FConnectorMember);
end;

constructor TConnectorReadExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  const Name: string; BaseExpr: TNoPosExpr);
begin
  inherited Create(Prog, Pos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
end;

destructor TConnectorReadExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

function TConnectorReadExpr.Eval(exec : TdwsExecution): Variant;
begin
  try
    if FBaseExpr is TDataExpr then
      FResultData := FConnectorMember.Read(TDataExpr(FBaseExpr).Data[exec][TDataExpr(FBaseExpr).Addr[exec]])
    else
      FResultData := FConnectorMember.Read(FBaseExpr.Eval(exec));
    Result := FResultData[0];
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

function TConnectorReadExpr.GetData(exec : TdwsExecution) : TData;
begin
  Eval(exec);
  Result := FResultData;
end;

procedure TConnectorReadExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
end;

{ TConnectorWriteExpr }

function TConnectorWriteExpr.AssignConnectorSym(
  ConnectorType: IConnectorType): Boolean;
begin
  FConnectorMember := ConnectorType.HasMember(FName, FTyp, True);
  Result := Assigned(FConnectorMember);
  if Result and not (Assigned(FTyp) and Assigned(FValueExpr.Typ) and
    FTyp.IsCompatible(FValueExpr.Typ)) then
    FProg.CompileMsgs.AddCompilerError(FPos, CPE_ConnectorTypeMismatch);
end;

constructor TConnectorWriteExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  const Name: string; BaseExpr, ValueExpr: TNoPosExpr);
begin
  inherited Create(Prog, Pos);
  FName := Name;
  FBaseExpr := BaseExpr;
  FValueExpr := ValueExpr;
end;

destructor TConnectorWriteExpr.Destroy;
begin
  FBaseExpr.Free;
  FValueExpr.Free;
  inherited;
end;

procedure TConnectorWriteExpr.EvalNoResult(exec : TdwsExecution);
var
  dat: TData;
  tmp: Variant;
  Base: pVariant;
begin
  if FBaseExpr is TDataExpr then
    Base := @TDataExpr(FBaseExpr).Data[exec][TDataExpr(FBaseExpr).Addr[exec]]
  else begin
    tmp := FBaseExpr.Eval(exec);
    Base := @tmp;
  end;

  if FValueExpr is TDataExpr then
    dat := TDataExpr(FValueExpr).GetData(exec)
  else
  begin
    SetLength(dat, 1);
    dat[0] := FValueExpr.Eval(exec);
  end;

  try
    FConnectorMember.Write(Base^, dat);
  except
    exec.SetScriptError(Self);
    raise;
  end;
end;

procedure TConnectorWriteExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
  FValueExpr.Initialize;
end;

{ TInfoConnector }

function TInfoConnector.GetMember(const s: string): IInfo;
begin
  TInfo.SetChild(Result, FProgramInfo, FTypeSym, FData, FOffset,
    TConnectorMemberDataMaster.Create(FExec, FTypeSym, s, FData[FOffset]));
end;

function TInfoConnector.GetMethod(const s: string): IInfo;
begin
  Result := TInfoConnectorCall.Create(FProgramInfo, FTypeSym,
    FData, FOffset, TConnectorSymbol(FTypeSym).ConnectorType, s);
end;

{ TInfoConnectorCall }

constructor TInfoConnectorCall.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const Data: TData; Offset: Integer;
  const ConnectorType: IConnectorType; const Name: string);
begin
  inherited Create(ProgramInfo, TypeSym, Data, Offset);
  FConnectorType := ConnectorType;
  FName := Name;
end;

function TInfoConnectorCall.Call(const Params: array of Variant): IInfo;
var
  x: Integer;
  expr: TConnectorCallExpr;
  resultData: TData;
begin
  expr := TConnectorCallExpr.Create(FExec.Prog, cNullPos, FName,
    TConstExpr.Create(FExec.Prog, FExec.Prog.TypVariant, FData[FOffset]));

  try
    for x := 0 to Length(Params) - 1 do
      expr.AddArg(TConstExpr.Create(FExec.Prog, FExec.Prog.TypVariant, Params[x]));

    if expr.AssignConnectorSym(FConnectorType) then
    begin
      if Assigned(expr.Typ) then
      begin
        SetLength(resultData, 1);
        resultData[0] := expr.Eval(FExec);
        TInfo.SetChild(Result, FProgramInfo, expr.Typ, resultData, 0);
      end
      else
      begin
        resultData := nil;
        FExec.Status:=esrNone;
        expr.EvalNoResult(FExec);
        Assert(FExec.Status=esrNone);
        Result := nil;
      end;
    end
    else
      raise Exception.CreateFmt(RTE_ConnectorCallFailed, [FName]);
  finally
    expr.Free;
  end;
end;

{ TDataMaster }

constructor TDataMaster.Create(Caller: TdwsProgramExecution; Sym: TSymbol);
begin
  FCaller := Caller;
  FSym := Sym;
end;

function TDataMaster.GetCaption: string;
begin
  Result := FSym.Caption;
end;

function TDataMaster.GetSize: Integer;
begin
  Result := FSym.Size;
end;

procedure TDataMaster.Read(exec : TdwsExecution; const Data: TData);
begin
end;

procedure TDataMaster.Write(exec : TdwsExecution; const Data: TData);
begin
end;

{ TExternalVarDataMaster }

procedure TExternalVarDataMaster.Read(exec : TdwsExecution; const Data: TData);
var
  x: Integer;
  resultData: TData;
  resultAddr: Integer;
  funcExpr: TFuncExpr;
begin
  resultData := nil;
  // Read an external var
  funcExpr := GetFuncExpr(FCaller.Prog, TExternalVarSymbol(FSym).ReadFunc, nil, nil);
  try
    funcExpr.Initialize;
    if funcExpr.Typ.Size > 1 then // !! > 1 untested !!
    begin
      funcExpr.SetResultAddr(exec, FCaller.Stack.FrameSize);
      // Allocate space on the stack to store the Result value
      FCaller.Stack.Push(funcExpr.Typ.Size);
      try
        // Execute function.
        resultData := funcExpr.GetData(exec);
        resultAddr := funcExpr.GetAddr(exec);
        // Copy Result
        for x := 0 to funcExpr.Typ.Size - 1 do
          Data[x] := resultData[resultAddr + x];
      finally
        FCaller.Stack.Pop(funcExpr.Typ.Size);
      end;
    end
    else
      VarCopy(Data[0], funcExpr.Eval(exec));
  finally
    funcExpr.Free;
  end;
end;

procedure TExternalVarDataMaster.Write(exec : TdwsExecution; const Data: TData);
var
  funcExpr: TFuncExpr;
begin
  funcExpr := GetFuncExpr(FCaller.Prog, TExternalVarSymbol(FSym).WriteFunc, nil, nil);
  try
    funcExpr.AddArg(TConstExpr.CreateTyped(FCaller.Prog, FSym.Typ, Data));
    funcExpr.AddPushExprs;
    exec.Status:=esrNone;
    funcExpr.EvalNoResult(exec);
    Assert(exec.Status=esrNone);
  finally
    funcExpr.Free;
  end;
end;

{ TConnectorMemberDataMaster }

constructor TConnectorMemberDataMaster.Create(Caller: TdwsProgramExecution;
  Sym: TSymbol; BaseValue: Variant; const Name: string);
begin
  inherited Create(Caller, Sym);
  FName := Name;
end;

procedure TConnectorMemberDataMaster.Read(exec : TdwsExecution; const Data: TData);
var
  readExpr: TConnectorReadExpr;
  dataSource: TData;
begin
  dataSource := nil;
  readExpr := TConnectorReadExpr.Create(FCaller.Prog, cNullPos, FName,
    TConstExpr.Create(FCaller.Prog, FCaller.Prog.TypVariant, FBaseValue));

  if readExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    dataSource := readExpr.GetData(exec);
    CopyData(dataSource, 0, Data, 0, readExpr.Typ.Size);
  end
  else
    raise Exception.Create(RTE_ConnectorReadError);
end;

procedure TConnectorMemberDataMaster.Write(exec : TdwsExecution; const Data: TData);
var
  writeExpr: TConnectorWriteExpr;
begin
  writeExpr := TConnectorWriteExpr.Create(FCaller.Prog, cNullPos, FName,
    TConstExpr.Create(FCaller.Prog, FCaller.Prog.TypVariant, FBaseValue),
    TConstExpr.Create(FCaller.Prog, FCaller.Prog.TypVariant, Data));

  if writeExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    exec.Status:=esrNone;
    writeExpr.EvalNoResult(exec);
    Assert(exec.Status=esrNone);
  end
  else
    raise Exception.Create(RTE_ConnectorWriteError);
end;

{ TInfoConst }

constructor TInfoConst.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Value: Variant);
begin
  inherited Create(ProgramInfo, TypeSym, FData, 0);
  SetLength(FData, TypeSym.Size);
  VarCopy(FData[0], Value);
end;

function TInfoConst.GetData : TData;
begin
  Result := FData;
end;

function TInfoConst.GetValue: Variant;
begin
  Result := FData[0];
end;

{ TSymbolDictionary }

procedure TSymbolDictionary.Add(Sym: TSymbol; const Pos: TScriptPos; UseTypes: TSymbolUsages);
var
  SymPosList: TSymbolPositionList;
begin
  if not Assigned(Sym) then
    Exit;   // don't add a nil pointer
  if Sym is TBaseSymbol then
    Exit;    // don't store references to base symbols

  { Check to see if symbol list already exists, if not create it }
  SymPosList := FindSymbolPosList(Sym);
  if SymPosList = nil then
  begin
    SymPosList := TSymbolPositionList.Create(Sym);
    FSymbolList.Add(SymPosList);      // add list for new symbol
  end;
  SymPosList.Add(Pos, UseTypes);      // add the instance of the symbol to the position list
end;

constructor TSymbolDictionary.Create;
begin
  FSymbolList := TList.Create;
end;

destructor TSymbolDictionary.Destroy;
begin
  Clear;
  FSymbolList.Free;
  inherited;
end;

function TSymbolDictionary.FindSymbolAtPosition(ACol, ALine: Integer; const sourceFile : String): TSymbol;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
  begin
    Result := TSymbolPositionList(FSymbolList[x]).FindSymbolAtPosition(ACol, ALine, sourceFile);
    if Assigned(Result) then
      Break;            // found symbol, stop searching
  end;
end;

function TSymbolDictionary.GetList(Index: Integer): TSymbolPositionList;
begin
  Result := TSymbolPositionList(FSymbolList[Index]);
end;

function TSymbolDictionary.Count: Integer;
begin
  Result := FSymbolList.Count;
end;

function TSymbolDictionary.FindSymbolPosList(Sym: TSymbol): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
    if TSymbolPositionList(FSymbolList[x]).Symbol = Sym then
    begin
      Result := TSymbolPositionList(FSymbolList[x]);
      Break;
    end;
end;

function TSymbolDictionary.FindSymbolPosList(const SymName: string): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
    if CompareText(TSymbolPositionList(FSymbolList[x]).Symbol.Name, SymName) = 0 then // same name (not case-sensitive)
    begin
      Result := TSymbolPositionList(FSymbolList[x]);
      Break;
    end;
end;

procedure TSymbolDictionary.Remove(Sym: TSymbol);
var
  idx, x: Integer;
  SymList: TSymbolPositionList;
begin
  // TFuncSymbol - remove params
  if Sym is TFuncSymbol then
  begin
    for x := 0 to TFuncSymbol(Sym).Params.Count - 1 do
      Remove(TFuncSymbol(Sym).Params[x]);   
  end
  // TClassSymbol - remove members (methods, fields, properties)
  else if Sym is TClassSymbol then
  begin
    for x := 0 to TClassSymbol(Sym).Members.Count - 1 do
      Remove(TClassSymbol(Sym).Members[x]);    
  end
  // TRecordSymbol - remove members
  else if Sym is TRecordSymbol then
  begin
    for x := 0 to TRecordSymbol(Sym).Members.Count - 1 do
      Remove(TRecordSymbol(Sym).Members[x]);
  end;

  // basic entry to remove
  SymList := FindSymbolPosList(Sym);
  if Assigned(SymList) then
  begin
    // remove SymList from internal list
    idx := FSymbolList.IndexOf(SymList);
    if idx >= 0 then
    begin
      FSymbolList.Delete(idx);     // delete entry from the list
      SymList.Free;                // free the object
    end;
  end;
end;

procedure TSymbolDictionary.Clear;
var
  x: Integer;
begin
  for x := 0 to FSymbolList.Count - 1 do
    try
      TSymbolPositionList(FSymbolList[x]).Free;
    except
      FSymbolList[x] := nil;   // initialized things *could* be left
    end;
end;

procedure TSymbolDictionary.SetList(Index: Integer; PosList: TSymbolPositionList);
begin
  FSymbolList[Index] := PosList;
end;

function TSymbolDictionary.FindSymbolUsage(Symbol: TSymbol;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(Symbol);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TSymbolDictionary.FindSymbolUsage(const SymName: string;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(SymName);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TSymbolDictionary.FindSymbolUsageOfType(const SymName: string;
  SymbolType: TSymbolClass; SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosListOfType(SymName, SymbolType);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TSymbolDictionary.FindSymbolPosListOfType(const SymName: string;
  SymbolType: TSymbolClass): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to Self.Count - 1 do
    if SameText(Self.Items[x].Symbol.Name, SymName) and (Self.Items[x].Symbol is SymbolType) then // same name (not case-sensitive)
    begin
      Result := Self.Items[x];
      Break;
    end;
end;

{ TSymbolPositionList }

procedure TSymbolPositionList.Add(const Pos: TScriptPos; UseTypes: TSymbolUsages);
var
  SymPos: TSymbolPosition;
begin
  if (Pos.Line <= 0) or (Pos.SourceFile = nil) then EXIT; // don't add invalid entry

  SymPos := TSymbolPosition.Create(Self, Pos, UseTypes);
  FPosList.Add(SymPos);              // add position information to the list
end;

constructor TSymbolPositionList.Create(ASymbol: TSymbol);
begin
  FSymbol := ASymbol;
  FPosList := TList.Create;
end;

destructor TSymbolPositionList.Destroy;
var
  x: Integer;
begin
  for x := 0 to FPosList.Count - 1 do
    TSymbolPosition(FPosList[x]).Free;
  FPosList.Free;
  inherited;
end;

function TSymbolPositionList.FindSymbolAtPosition(ACol, ALine: Integer; const sourceFile : String): TSymbol;
var
   x : Integer;
   symPos : TSymbolPosition;
begin
   for x := 0 to FPosList.Count - 1 do begin
      symPos := TSymbolPosition(FPosList[x]);
      if (sourceFile<>'') and (symPos.ScriptPos.SourceFile.SourceFile<>sourceFile) then continue;
      if (symPos.ScriptPos.Line = ALine) and (symPos.ScriptPos.Col = ACol) then begin
         Result := symPos.Symbol;
         Exit;    // found the symbol, stop searching
      end;
   end;
   Result := nil; // default to not found
end;

function TSymbolPositionList.GetPosition(Index: Integer): TSymbolPosition;
begin
  Result := TSymbolPosition(FPosList[Index]);
end;

function TSymbolPositionList.Count: Integer;
begin
  Result := FPosList.Count;
end;

procedure TSymbolPositionList.SetPosition(Index: Integer; SymPos: TSymbolPosition);
begin
  FPosList[Index] := SymPos;
end;

function TSymbolPositionList.FindUsage(SymbolUse: TSymbolUsage): TSymbolPosition;
var
  x: Integer;
begin
  Result := nil;          // default to not found
  for x := 0 to FPosList.Count - 1 do
    if SymbolUse in TSymbolPosition(FPosList[x]).SymbolUsages then
    begin
      Result := TSymbolPosition(FPosList[x]);
      Break;    // found the symbol, stop searching
    end;
end;

{ TSymbolPosition }

constructor TSymbolPosition.Create(AOwningList: TSymbolPositionList; const AScriptPos: TScriptPos; AUsages: TSymbolUsages);
begin
  FOwnerList := AOwningList;
  FScriptPos := AScriptPos;
  FSymUsages := AUsages;
end;

function TSymbolPosition.GetSymbol: TSymbol;
begin
  if Assigned(FOwnerList) then
    Result := FOwnerList.Symbol
  else
    Result := nil;
end;

{ TContext }

constructor TContext.Create(AParent: TContext; const AStartPos: TScriptPos;
  AParentSymbol: TSymbol);
begin
  FSubContexts := TList.Create;

  { Initialize variables }
  FParentContext := AParent;
  FParentSymbol  := AParentSymbol;
  FLocalTable    := nil;             // default to nil. Didn't pass in because uses didn't have access to that data when context is openned
  // starting position
  FStartPos := AStartPos;
  // invalid end position
  FEndPos.Line := 0;
  FEndPos.Col  := 0;
  FEndPos.SourceFile := nil;
end;

destructor TContext.Destroy;
var
  x: Integer;
begin
  for x := 0 to FSubContexts.Count - 1 do
    TContext(FSubContexts[x]).Free;
  FSubContexts.Free;
  inherited;
end;

function TContext.HasParentSymbolOfClass(SymbolType: TSymbolClass;
  SearchParents: Boolean): Boolean;
begin
  // Return if the context has a parent symbol of the specified type. Optionally
  // search up through other parent contexts
  Result := False;
  if Assigned(ParentSym) then
    Result := (ParentSym is SymbolType);
  // if not found and parents should be searched also, recurse until no more
  // parents or the symbol type is found.
  if (not Result) and SearchParents then
    if Assigned(Parent) then
      Result := Parent.HasParentSymbolOfClass(SymbolType, SearchParents);
end;

function TContext.IsPositionInContext(ACol, ALine: Integer; SourceFile: TSourceFile): Boolean;
begin
  // check if the position is in the same SourceFile
  if Assigned(SourceFile) then  // if not assigned, don't check it
    if SourceFile <> FStartPos.SourceFile then
    begin
      Result := False;
      Exit;
    end;

  // if inside a multi-line context
  Result := (ALine > FStartPos.Line) and (ALine < FEndPos.Line);
  if not Result then
  begin
    // if not, check for a one-line context (inside the context begin and end cols)
    if FStartPos.Line = FEndPos.Line then
      Result := (ALine = FStartPos.Line) and (ACol >= FStartPos.Col) and (ACol <= FEndPos.Col)
    else  // not a single-line context
      Result := ((ALine = FStartPos.Line) and (ACol >= FStartPos.Col)) or // on top line, inside start
                ((ALine = FEndPos.Line) and (ACol <= FEndPos.Col));       // on bottom line, inside end
  end;
end;

{ TContextMap }

procedure TContextMap.CloseContext(const AEndPos: TScriptPos);
begin
  FCurrentContext.FEndPos := AEndPos;       // close the current context
  { if the CurrentContext is not a top-level one, then pop the stack and make
    the new context the closed one's parent }
  FCurrentContext := FCurrentContext.Parent;
end;

constructor TContextMap.Create;
begin
  FScriptContexts := TList.Create;
  FCurrentContext := nil;
end;

destructor TContextMap.Destroy;
var
  x: Integer;
begin
  for x := 0 to FScriptContexts.Count - 1 do
    TContext(FScriptContexts[x]).Free;
  FScriptContexts.Free;
  inherited;
end;

function TContextMap.FindContext(AParentSymbol: TSymbol): TContext;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FScriptContexts.Count - 1 do
  begin
    if TContext(FScriptContexts[x]).FParentSymbol = AParentSymbol then
    begin
      Result := TContext(FScriptContexts[x]);
      BREAK;
    end;
  end;
end;

function TContextMap.FindContext(ACol, ALine: Integer; SourceFile: TSourceFile): TContext;
var
  ReturnContext: TContext;    // Gets set to the context found
  HitEnd: Boolean;            // Followed branch to end, stop searching

    function FoundContext(Context: TContext): Boolean;
    var
      x: Integer;
    begin
      Result := False;
      { Record that this context contains it and should be returned (provided it
        doesn't go deeper) }
      ReturnContext := Context;
      { Search sub-contexts }
      for x := 0 to Context.SubContexts.Count - 1 do
      begin
        if TContext(Context.SubContexts[x]).IsPositionInContext(ACol, ALine, SourceFile) then
          Result := FoundContext(TContext(Context.SubContexts[x]))
      end;
      { We got here because it was found. After all subContexts were checked,
        it wasn't found so we've hit the end. }
      if not Result then
        HitEnd := True;
    end;

var
  i: Integer;
begin
  { If this position is not in the top level contexts then it won't be in
    subcontexts. Use a recursive search to find the lowest context at which the
    position can be found. }

  ReturnContext := nil;
  HitEnd        := False;
  { Cycle all top level contexts. Burrow into each to find the lowest level that
    matches the criteria. }
  for i := 0 to FScriptContexts.Count - 1 do
  begin
    if HitEnd then
      BREAK;
    { If in top-level context, burrow into subcontexts }
    if TContext(FScriptContexts[i]).IsPositionInContext(ACol, ALine, SourceFile) then
      if not FoundContext(TContext(FScriptContexts[i])) then
        Break;
  end;
  Result := ReturnContext;
end;

function TContextMap.FindContext(const ScriptPos: TScriptPos): TContext;
begin
  Result := FindContext(ScriptPos.Col, ScriptPos.Line, ScriptPos.SourceFile);
end;

procedure TContextMap.OpenContext(const AStartPos: TScriptPos; AParentSymbol: TSymbol);
var
  NewContext: TContext;
begin
  { Uses a simple 'stack' concept. If currently in a context and a new context
    is openned then the new context is a sub context of the current context. }
  NewContext := TContext.Create(FCurrentContext, AStartPos, AParentSymbol);  // new context is owned by the current context
  { Add new context to the appropriate 'parent' context }
  if FCurrentContext = nil then           // if top-level,
    FScriptContexts.Add(NewContext)       // Add to top-level contexts
  else
    FCurrentContext.SubContexts.Add(NewContext);
  FCurrentContext := NewContext;
end;

{ TScriptSourceItem }

constructor TScriptSourceItem.Create(const ANameReference: string; ASourceFile: TSourceFile;
  ASourceType: TScriptSourceType);
begin
  FNameReference := ANameReference;
  FSourceFile := ASourceFile;
  FSourceType := ASourceType;
end;

{ TScriptSourceList }

procedure TScriptSourceList.Add(const ANameReference: string; ASourceFile: TSourceFile;
  ASourceType: TScriptSourceType);
var
  testItem: TScriptSourceItem;
begin
  { Determine if that script item alread exists, if not add it. }
  testItem := FindScriptSourceItem(ASourceFile);
  if not Assigned(testItem) then   // if not found, create a new one and add it to the list
  begin
    testItem := TScriptSourceItem.Create(ANameReference, ASourceFile, ASourceType);
    FSourceList.Add(testItem);
    // get a pointer to the 'main' script item
    if ASourceType = stMain then
      FMainScript := testItem;
  end;
end;

procedure TScriptSourceList.Clear;
var
  x: Integer;
begin
  for x := 0 to FSourceList.Count - 1 do
    try
      TScriptSourceItem(FSourceList[x]).Free;
    except
      FSourceList[x] := nil;   // initialized things *could* be left
    end;
end;

function TScriptSourceList.Count: Integer;
begin
  Result := FSourceList.Count;
end;

constructor TScriptSourceList.Create;
begin
  FSourceList := TList.Create;
  FMainScript := nil;
end;

destructor TScriptSourceList.Destroy;
begin
  Clear;
  FSourceList.Free;
  inherited;
end;

function TScriptSourceList.FindScriptSourceItem(const SourceFileName: string): TScriptSourceItem;
var
  x: Integer;
begin
  Result := nil;
  x := IndexOf(SourceFileName);
  if x > -1 then
    Result := TScriptSourceItem(FSourceList[x]);
end;

function TScriptSourceList.FindScriptSourceItem(SourceFile: TSourceFile): TScriptSourceItem;
var
  x: Integer;
begin
  Result :=  nil;
  x := IndexOf(SourceFile);
  if x > -1 then
    Result := TScriptSourceItem(FSourceList[x]);
end;

function TScriptSourceList.FindScriptSourceItem(const ScriptPos: TScriptPos): TScriptSourceItem;
var
  x: Integer;
begin
  Result :=  nil;
  x := IndexOf(ScriptPos);
  if x > -1 then
    Result := TScriptSourceItem(FSourceList[x]);
end;

function TScriptSourceList.GetSourceItem(Index: Integer): TScriptSourceItem;
begin
  Result := TScriptSourceItem(FSourceList[Index]);
end;

function TScriptSourceList.IndexOf(ASourceFile: TSourceFile): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to FSourceList.Count - 1 do
  begin
    // if they both point to the same TSourceFile then they match
    if TScriptSourceItem(FSourceList[x]).SourceFile = ASourceFile then
    begin
      Result := x;
      Break;           // found match, stop searching
    end;
  end;
end;

function TScriptSourceList.IndexOf(const SourceFileName: string): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to FSourceList.Count - 1 do
  begin
    // if both names match, consider it a match
    if CompareText(TScriptSourceItem(FSourceList[x]).SourceFile.SourceFile, SourceFileName) = 0 then
    begin
      Result := x;
      Break;           // found match, stop searching
    end;
  end;
end;

function TScriptSourceList.IndexOf(const AScriptPos: TScriptPos): Integer;
begin
  Result := IndexOf(AScriptPos.SourceFile);
end;

procedure TScriptSourceList.SetSourceItem(Index: Integer;
  SourceItem: TScriptSourceItem);
begin
  FSourceList[Index] := SourceItem;
end;

{ TMethodObjExpr }

constructor TMethodObjExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  BaseExpr: TDataExpr);
begin
  Assert(BaseExpr.Typ is TMethodSymbol);
  inherited Create(Prog,Pos,TMethodSymbol(BaseExpr.Typ).ClassSymbol);
  FBaseExpr := BaseExpr;
end;

function TMethodObjExpr.GetAddr(exec : TdwsExecution) : Integer;
begin
  Result := FBaseExpr.Addr[exec] + 1;
end;

function TMethodObjExpr.GetData(exec : TdwsExecution) : TData;
begin
  Result := FBaseExpr.Data[exec];
end;

{ TConstructorStaticObjExpr }

constructor TConstructorStaticObjExpr.Create(Prog: TdwsProgram;
  const Pos: TScriptPos; Func: TMethodSymbol; BaseExpr: TDataExpr);
begin
  inherited Create(Prog,Pos,Func,BaseExpr);
  Typ := BaseExpr.Typ;
end;

function TConstructorStaticObjExpr.PostCall(exec : TdwsExecution) : Variant;
begin
   Result := exec.SelfScriptObject^;
end;

{ TConstructorVirtualObjExpr }

constructor TConstructorVirtualObjExpr.Create(Prog: TdwsProgram;
  const Pos: TScriptPos; Func: TMethodSymbol; Base: TDataExpr);
begin
  inherited Create(Prog,Pos,Func,Base);
  Typ := Base.Typ;
end;

function TConstructorVirtualObjExpr.PostCall(exec : TdwsExecution): Variant;
begin
   Result := exec.SelfScriptObject^;
end;

{ TInfoProperty }

constructor TInfoProperty.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const Data: TData; Offset: Integer;
  PropSym: TPropertySymbol; const ScriptObj: IScriptObj);
begin
  inherited Create(ProgramInfo,TypeSym,Data,Offset);
  FPropSym := PropSym;
  FScriptObj := ScriptObj;
end;

destructor TInfoProperty.Destroy;
begin
  FTempParams.Free;
  inherited;
end;

function TInfoProperty.GetParameter(const s: string): IInfo;
var
  tp: TTempParam;
begin
  if not Assigned(FTempParams) then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s, cvMagic));

  if Assigned(tp) then
    SetChild(Result, FProgramInfo, tp.Typ, tp.FData, 0)
  else
    raise Exception.CreateFmt(RTE_NoIndexFound, [s, FPropSym.Name]);
end;

procedure TInfoProperty.InitTempParams;
var
  x: Integer;
  tp: TTempParam;
begin
  FTempParams := TSymbolTable.Create;
  for x := 0 to FPropSym.ArrayIndices.Count - 1 do
  begin
    tp := TTempParam.Create(FPropSym.ArrayIndices[x]);
    FTempParams.AddSymbol(tp);
  end;
end;

function TInfoProperty.GetValue: Variant;
begin
  result := IInfo(Self).Data[0];
end;

procedure TInfoProperty.AssignIndices(const Func: IInfo; FuncParams: TSymbolTable);
var
  paramName: String;
  x: Integer;
  destParam: IInfo;
begin
  if Assigned(FTempParams) then
    for x := 0 to FTempParams.Count - 1 do
    begin
      paramName := FuncParams[x].Name;
      destParam := Func.Parameter[paramName];
      destParam.Data := TTempParam(FTempParams[x]).Data;
    end;
end;

function TInfoProperty.GetData : TData;
var
  func : IInfo;
begin
  if FPropSym.ReadSym is TFuncSymbol then begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.ReadSym,FData,FOffset,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);
    AssignIndices(func,TFuncSymbol(FPropSym.ReadSym).Params);
    result := func.Call.Data;
  end
  else if FPropSym.ReadSym is TFieldSymbol then
  begin
    SetChild(func, FProgramInfo,FPropSym.ReadSym.Typ,FScriptObj.Data,
      TFieldSymbol(FPropSym.ReadSym).Offset);
    result := func.Data;
{
    fieldSym := TFieldSymbol(FPropSym.ReadSym); // var fieldSym : TFieldSymbol;
    SetLength(result,fieldSym.Typ.Size);
    CopyData(FScriptObj.Data,fieldSym.Offset,result,0,fieldSym.Typ.Size);
}
  end
  else
    raise Exception.Create(CPE_WriteOnlyProperty);
end;

procedure TInfoProperty.SetData(const Value: TData);
var
  func: IInfo;
  paramName: String;
  params: TSymbolTable;
begin
  if FPropSym.WriteSym is TFuncSymbol then
  begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.WriteSym,FData,FOffset,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);

    params := TFuncSymbol(FPropSym.WriteSym).Params;
    AssignIndices(func,params);

    paramName := params[params.Count - 1].Name;
    func.Parameter[paramName].Data := Value;

    func.Call;
  end
  else if FPropSym.WriteSym is TFieldSymbol then
  begin
    SetChild(func, FProgramInfo,FPropSym.WriteSym.Typ,FScriptObj.Data,
      TFieldSymbol(FPropSym.WriteSym).Offset);
    func.Data := Value;
  end
  else
    raise Exception.Create(CPE_ReadOnlyProperty);
end;

procedure TInfoProperty.SetValue(const Value: Variant);
var dat: TData;
begin
  SetLength(dat,1);
  dat[0] := Value;
  IInfo(Self).Data := dat;
end;

// ------------------
// ------------------ TNoResultWrapperExpr ------------------
// ------------------

// Create
//
constructor TNoResultWrapperExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
begin
   inherited Create(Prog, Pos);
   FExpr := Expr;
end;

// Destroy
//
destructor TNoResultWrapperExpr.Destroy;
begin
   FExpr.Free;
   inherited;
end;

// Initialize
//
procedure TNoResultWrapperExpr.Initialize;
begin
   FExpr.Initialize;
end;

// EvalNoResult
//
procedure TNoResultWrapperExpr.EvalNoResult(exec : TdwsExecution);
begin
   Expr.EvalNoResult(exec);
end;

// TypeCheckNoPos
//
procedure TNoResultWrapperExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   Expr.TypeCheckNoPos(aPos);
end;

// IsConstant
//
function TNoResultWrapperExpr.IsConstant : Boolean;
begin
   Result:=Expr.IsConstant;
end;

// ------------------
// ------------------ TSourceCondition ------------------
// ------------------

// Create
//
constructor TSourceCondition.Create(const pos : TScriptPos; aTest, aMsg : TNoPosExpr);
begin
   inherited Create;
   FPos:=pos;
   FTest:=aTest;
   FMsg:=aMsg;
end;

// Destroy
//
destructor TSourceCondition.Destroy;
begin
   inherited;
   FTest.Free;
   FMsg.Free;
end;

// InitSymbol
//
procedure TSourceCondition.InitSymbol(Symbol: TSymbol);
begin
   FTest.Initialize;
   FMsg.Initialize;
end;

// InitExpression
//
procedure TSourceCondition.InitExpression(Expr: TExprBase);
begin
   // nothing
end;

// EvalAsBoolean
//
function TSourceCondition.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FTest.EvalAsBoolean(exec);
end;

// EvalAsString
//
procedure TSourceCondition.EvalAsString(exec : TdwsExecution; var Result : String);
begin
   FMsg.EvalAsString(exec, Result);
end;

// ------------------
// ------------------ TSourceConditions ------------------
// ------------------

// Create
//
constructor TSourceConditions.Create(aProg : TdwsProcedure);
begin
   inherited Create;
   FProg:=aProg;
end;

// Destroy
//
destructor TSourceConditions.Destroy;
var
   i : Integer;
begin
   inherited;
   for i:=0 to FItems.Count-1 do
      TSourceCondition(FItems.List[i])._Release;
   FItems.Clear;
end;

// AddCondition
//
procedure TSourceConditions.AddCondition(condition : TSourceCondition);
begin
   condition._AddRef;
   FItems.Add(condition);
end;

// Test
//
function TSourceConditions.Test(exec : TdwsExecution) : TSourceCondition;
var
   i : Integer;
   ptrList : PPointerList;
begin
   ptrList:=FItems.List;
   for i:=0 to FItems.Count-1 do begin
      Result:=TSourceCondition(ptrList[i]);
      if not Result.EvalAsBoolean(exec) then Exit;
   end;
   Result:=nil;
end;

// EvalNoresult
//
procedure TSourceConditions.EvalNoresult(exec : TdwsExecution);
var
   failed : TSourceCondition;
begin
   failed:=Test(exec);
   if failed<>nil then
      RaiseConditionFailed(exec, failed.Pos, failed);
end;

// ------------------
// ------------------ TSourcePreConditions ------------------
// ------------------

// RaiseConditionFailed
//
procedure TSourcePreConditions.RaiseConditionFailed(exec : TdwsExecution;
                 const scriptPos : TScriptPos; const msg : IStringEvalable);
var
   msgStr : String;
begin
   msg.EvalAsString(exec, msgStr);
   (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(
      RTE_PreConditionFailed, [FProg.Func.QualifiedName, scriptPos.AsInfo, msgStr], scriptPos);
end;

// ------------------
// ------------------ TSourcePostConditions ------------------
// ------------------

// RaiseConditionFailed
//
procedure TSourcePostConditions.RaiseConditionFailed(exec : TdwsExecution;
                 const scriptPos : TScriptPos; const msg : IStringEvalable);
var
   msgStr : String;
begin
   msg.EvalAsString(exec, msgStr);
   (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(
      RTE_PostConditionFailed, [FProg.Func.QualifiedName, scriptPos.AsInfo, msgStr], scriptPos);
end;

// ------------------
// ------------------ TSourceMethodPreConditions ------------------
// ------------------

// EvalNoResult
//
procedure TSourceMethodPreConditions.EvalNoResult(exec : TdwsExecution);
var
   methSym : TMethodSymbol;
   current : TConditionSymbol;
   conds : TConditionsSymbolTable;
   i : Integer;
begin
   // for pre-conditions find the root and test against those
   methSym:=(FProg.Func as TMethodSymbol);
   if not methSym.IsOverride then begin
      inherited EvalNoResult(exec);
      Exit;
   end;

   while (methSym.ParentMeth<>nil) and (methSym.IsOverride) do
      methSym:=methSym.ParentMeth;
   if methSym.IsOverride then Exit;

   conds:=methSym.Conditions;
   for i:=0 to conds.Count-1 do begin
      current:=TConditionSymbol(conds[i]);
      if (current.ClassType=TPreConditionSymbol) and not current.Condition.EvalAsBoolean(exec) then
         RaiseConditionFailed(exec, current.ScriptPos, current.Message);
   end;
end;

// ------------------
// ------------------ TSourceMethodPostConditions ------------------
// ------------------

// EvalNoResult
//
procedure TSourceMethodPostConditions.EvalNoResult(exec : TdwsExecution);
var
   methSym : TMethodSymbol;
   current : TConditionSymbol;
   conds : TConditionsSymbolTable;
   i : Integer;
begin
   // for post-conditions, all must pass
   inherited EvalNoResult(exec);
   methSym:=(FProg.Func as TMethodSymbol);
   if not methSym.IsOverride then Exit;
   while methSym<>nil do begin
      conds:=methSym.Conditions;
      for i:=0 to conds.Count-1 do begin
         current:=TConditionSymbol(conds[i]);
         if (current.ClassType=TPostConditionSymbol) and not current.Condition.EvalAsBoolean(exec) then
            RaiseConditionFailed(exec, current.ScriptPos, current.Message);
      end;
      methSym:=methSym.ParentMeth;
   end;
end;

// ------------------
// ------------------ EdwsVariantTypeCastError ------------------
// ------------------

// Create
//
constructor EdwsVariantTypeCastError.Create(const v : Variant; const desiredType : String);
begin
   inherited CreateFmt(CPE_AssignIncompatibleTypes,
                       [VarTypeAsText(VarType(v)), desiredType])

end;

end.

