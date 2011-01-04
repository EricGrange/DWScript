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
  TSymbolPositionList = class;
  TFuncExprBase = class;
  TScriptObj = class;

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

  // Interface for external debuggers
  IDebugger = interface
    ['{8D534D14-4C6B-11D5-8DCB-0000216D9E86}']
    procedure StartDebug(MainProg: TdwsProgram);
    procedure DoDebug(Prog: TdwsProgram; Expr: TExpr);
    procedure StopDebug(MainProg: TdwsProgram);
    procedure EnterFunc(Prog: TdwsProgram; funcExpr: TNoPosExpr);
    procedure LeaveFunc(Prog: TdwsProgram; funcExpr: TNoPosExpr);
  end;

   // TTerminatorThread
   //
   // Stops the script after given time (Timeout)
   TTerminatorThread = class(TThread)
      private
         FProg : TdwsProgram;
         FEvent : TEvent;
         FMillisecondsToLive : Integer;

      protected
         procedure Execute; override;
         procedure DoTerminate; override;

      public
         constructor Create(aProgram : TdwsProgram; aMilliSecToLive : Integer);
         destructor Destroy; override;
   end;

  TProgramInfo = class;

  TProgramState = (psUndefined, psReadyToRun, psRunning, psRunningStopped, psTerminated);

  TExecutionStatusResult = (esrNone, esrExit, esrBreak, esrContinue);

  // A script executable program
  TdwsProgram = class(TInterfacedObject)
  private
    FFirstObject, FLastObject : TScriptObj;
    FObjectCount : Integer;
    FDebugger: IDebugger;
    FIsDebugging: Boolean;
    FContextMap: TContextMap;
    FExpr: TExpr;
    FInitExpr: TBlockInitExpr;
    FAddrGenerator: TAddrGeneratorRec;
    FGlobalAddrGenerator: TAddrGeneratorRec;
    FInfo: TProgramInfo;
    FMsgs: TdwsMessageList;
    FParameters: TData;
    FParent: TdwsProgram;
    FProgramState: TProgramState;
    FResult: TdwsResult;
    FResultType: TdwsResultType;
    FRoot: TdwsProgram;
    FUnifiedConstList: TSortedList<TExprBase>;
    FRootTable: TProgramSymbolTable;
    FSourceList: TScriptSourceList;
    FStack: TStack;
    FSymbolDictionary: TSymbolDictionary;
    FTable: TSymbolTable;
    FTimeoutMilliseconds: Integer;
    FTypBoolean: TTypeSymbol;
    FTypFloat: TTypeSymbol;
    FTypInteger: TTypeSymbol;
    FTypNil: TNilSymbol;
    FTypObject: TClassSymbol;
    FTypString: TTypeSymbol;
    FTypVariant: TTypeSymbol;
    FUserDef: TObject;
    FCompiler : TObject;
    FRuntimeFileSystem : TdwsCustomFileSystem;
    FFileSystem : IdwsFileSystem;
    FConditionalDefines : TStringList;
    FLineCount : Integer;
    FProgramInfo : TProgramInfo;

  protected
    function GetLevel: Integer;
    function GetResult: TdwsResult; virtual;
    function GetUserDef: TObject; virtual;
    procedure SetDebugger(const Value: IDebugger);
    procedure SetResult(const Value: TdwsResult); virtual;
    procedure SetUserDef(const Value: TObject); virtual;
    procedure Evaluate(var status : TExecutionStatusResult); virtual;
    procedure SetConditionalDefines(const val : TStringList);

  public
    constructor Create(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                       MaxRecursionDepth : Integer;
                       MaxDataSize, StackChunkSize: Integer);
    destructor Destroy; override;

    procedure DoStep(Expr: TExpr);

    procedure BeginProgram(IsRunningMainProgram: Boolean = True);
    procedure ScriptObjCreated(scriptObj: TScriptObj);
    procedure ScriptObjDestroyed(scriptObj: TScriptObj);
    procedure DestroyScriptObj(const ScriptObj: IScriptObj);
    procedure EndProgram;

    procedure Execute; overload; virtual;
    procedure Execute(aTimeoutMilliSeconds: Integer); overload;
    procedure ExecuteParam(const Params: array of Variant); overload;
    procedure ExecuteParam(const Params: array of Variant; aTimeoutMilliSeconds: Integer); overload;
    procedure ExecuteParam(const Params: OleVariant); overload;
    procedure ExecuteParam(const Params: OleVariant; aTimeoutMilliSeconds: Integer); overload;

    function GetGlobalAddr(DataSize: Integer): Integer;
    function GetTempAddr(DataSize: Integer = -1): Integer;
    procedure ReadyToRun;
    procedure RunProgram(aTimeoutMilliSeconds: Integer);
    procedure Stop; virtual;

    function AcquireProgramInfo(funcSym : TFuncSymbol) : TProgramInfo;
    procedure ReleaseProgramInfo(info : TProgramInfo);

    property Debugger: IDebugger read FDebugger write SetDebugger;
    property Compiler: TObject read FCompiler write FCompiler;
    property RuntimeFileSystem : TdwsCustomFileSystem read FRuntimeFileSystem write FRuntimeFileSystem;
    property FileSystem : IdwsFileSystem read FFileSystem write FFileSystem;
    property ConditionalDefines : TStringList read FConditionalDefines write SetConditionalDefines;
    property LineCount : Integer read FLineCount write FLineCount;
    property ObjectCount : Integer read FObjectCount write FObjectCount;
    property Expr: TExpr read FExpr write FExpr;
    property InitExpr: TBlockInitExpr read FInitExpr;
    property Info: TProgramInfo read FInfo;
    property IsDebugging: Boolean read FIsDebugging;
    property Level: Integer read GetLevel;
    property Msgs: TdwsMessageList read FMsgs write FMsgs;
    property Parameters: TData read FParameters;
    property Parent: TdwsProgram read FParent;
    property ProgramState: TProgramState read FProgramState;
    property Result: TdwsResult read GetResult write SetResult;
    property Root: TdwsProgram read FRoot write FRoot;
    property Stack: TStack read FStack;
    property RootTable: TProgramSymbolTable read FRootTable;
    property Table: TSymbolTable read FTable write FTable;
    property TimeoutMilliseconds : Integer read FTimeoutMilliseconds write FTimeoutMilliseconds;

    property UnifiedConstList: TSortedList<TExprBase> read FUnifiedConstList;

    property TypBoolean: TTypeSymbol read FTypBoolean;
    property TypFloat: TTypeSymbol read FTypFloat;
    property TypInteger: TTypeSymbol read FTypInteger;
    property TypNil: TNilSymbol read FTypNil;
    property TypObject: TClassSymbol read FTypObject;
    property TypString: TTypeSymbol read FTypString;
    property TypVariant: TTypeSymbol read FTypVariant;

    property UserDef: TObject read GetUserDef write SetUserDef;
    property SymbolDictionary: TSymbolDictionary read FSymbolDictionary;
    property ContextMap: TContextMap read FContextMap;
    property SourceList: TScriptSourceList read FSourceList;
  end;

  // Functions callable from a script program implement this interfaces
  ICallable = interface(IExecutable)
    ['{8D534D15-4C6B-11D5-8DCB-0000216D9E86}']
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
  end;

  // A script procedure
  TProcedure = class(TdwsProgram, IUnknown, ICallable)
  private
    FFunc: TFuncSymbol;
  protected
    function GetResult: TdwsResult; override;
    function GetUserDef: TObject; override;
    procedure SetResult(const Value: TdwsResult); override;
    procedure SetUserDef(const Value: TObject); override;
  public
    constructor Create(Parent: TdwsProgram);
    destructor Destroy; override;
    procedure AssignTo(sym: TFuncSymbol);
    procedure Call(Caller: TdwsProgram; Func: TFuncSymbol);
    procedure Execute; override;
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
    procedure Stop; override;
    property Func: TFuncSymbol read FFunc write FFunc;
  end;

  // Base class of all expressions attached to a program
  TNoPosExpr = class(TExprBase)
  private
    protected
      FProg: TdwsProgram;
      FTyp: TSymbol;

      function CreateEDelphiObj(const ClassName, Message: string): IScriptObj;

    public
      constructor Create(Prog: TdwsProgram);

      function IsBooleanValue : Boolean;
      function IsIntegerValue : Boolean;
      function IsFloatValue : Boolean;
      function IsNumberValue : Boolean;
      function IsStringValue : Boolean;
      function IsVariantValue : Boolean;

      function GetBaseType: TTypeSymbol;

      procedure EvalNoResult(var status : TExecutionStatusResult); virtual;
      function  EvalAsInteger : Int64; override;
      function  EvalAsBoolean : Boolean; override;
      procedure EvalAsFloat(var Result : Double); override;
      procedure EvalAsString(var Result : String); override;
      procedure EvalAsVariant(var Result : Variant); override;
      procedure EvalAsScriptObj(var Result : IScriptObj); override;

      procedure AssignValue(const value : Variant); override;
      procedure AssignValueAsInteger(const value : Int64); override;
      procedure AssignValueAsBoolean(const value : Boolean); override;
      procedure AssignValueAsFloat(var value : Double); override;
      procedure AssignValueAsString(const value: String); override;

      procedure Initialize; virtual;
      procedure TypeCheckNoPos(const aPos : TScriptPos); virtual;
      function IsConstant : Boolean; virtual;
      function Optimize : TNoPosExpr; virtual;
      function OptimizeIntegerConstantToFloatConstant : TNoPosExpr;

      property Prog: TdwsProgram read FProg;
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

         procedure AddCompilerWarning(const Text : String);
         procedure AddCompilerError(const Text : String);
         procedure AddCompilerErrorFmt(const fmtText: string; const Args: array of const);
         procedure AddCompilerStop(const Text : String);
         procedure AddExecutionStop(const Text : String);
         procedure AddExecutionStopFmt(const fmtText: string; const Args: array of const);

         property Pos: TScriptPos read FPos write FPos;
   end;

  TNoResultExpr = class(TExpr)
    function Eval: Variant; override;
    procedure EvalNoResult(var status : TExecutionStatusResult); override;
    function OptimizeToNoResultExpr : TNoResultExpr;
  end;

  // Does nothing! E. g.: "for x := 1 to 10 do {TNullExpr};"
  TNullExpr = class(TNoResultExpr)
    procedure EvalNoResult(var status : TExecutionStatusResult); override;
  end;

   // statement; statement; statement;
   TBlockExprBase = class(TNoResultExpr)
      protected
         FStatements : PExprList;
         FCount : Integer;

      public
         destructor Destroy; override;

         procedure AddStatement(expr : TExpr);

         procedure Initialize; override;
   end;

   // statement; statement; statement;
   TBlockInitExpr = class(TBlockExprBase)
      public
         procedure EvalNoResult(var status : TExecutionStatusResult); override;
   end;

  // Encapsulates data
  TDataExpr = class(TNoPosExpr)
  protected
    function GetAddr: Integer; virtual;
    function GetData: TData; virtual; abstract;
  public
    constructor Create(Prog: TdwsProgram; Typ: TSymbol);
    procedure AssignData(const SourceData: TData; SourceAddr: Integer); virtual;
    procedure AssignDataExpr(DataExpr: TDataExpr); virtual;
    procedure AssignExpr(Expr: TNoPosExpr); virtual;
    procedure AssignValue(const Value: Variant); override;
    procedure AssignValueAsInteger(const value : Int64); override;
    procedure AssignValueAsBoolean(const value : Boolean); override;
    procedure AssignValueAsFloat(var value : Double); override;
    procedure AssignValueAsString(const value: String); override;
    function Eval: Variant; override;
    property Addr: Integer read GetAddr;
    property Data: TData read GetData;
    function IsWritable: Boolean; virtual;
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
         procedure AddExecutionStop(const Text : String);
         procedure AddExecutionStopFmt(const fmtText: string; const Args: array of const);

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
         function AddArg(Arg: TNoPosExpr) : TSymbol; virtual; abstract;
         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
         procedure Initialize; override;
         function GetArgs : TExprBaseList;
         function Optimize : TNoPosExpr; override;
         function IsConstant : Boolean; override;
         property FuncSym: TFuncSymbol read FFunc;
   end;

   TPushOperatorType = (potUnknown,
                        potAddr, potTempAddr, potTempArrayAddr, potTempArray,
                        potResult,
                        potResultInteger, potResultFloat, potResultBoolean,
                        potResultString, potResultConstString,
                        potData, potLazy);
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
      procedure InitPushLazy(StackAddr: Integer; ArgExpr: TNoPosExpr);

      procedure Execute(stack : TStack);

      procedure ExecuteAddr(stack : TStack);
      procedure ExecuteTempAddr(stack : TStack);
      procedure ExecuteTempArrayAddr(stack : TStack);
      procedure ExecuteTempArray(stack : TStack);
      procedure ExecuteResult(stack : TStack);
      procedure ExecuteResultBoolean(stack : TStack);
      procedure ExecuteResultInteger(stack : TStack);
      procedure ExecuteResultFloat(stack : TStack);
      procedure ExecuteResultString(stack : TStack);
      procedure ExecuteResultConstString(stack : TStack);
//      procedure ExecuteResultArray(stack : TStack);
      procedure ExecuteData(stack : TStack);
      procedure ExecuteLazy(stack : TStack);
   end;

   TFuncExprState = (fesIsInstruction, fesIsWritable);
   TFuncExprStates = set of TFuncExprState;

   // Function call: func(arg0, arg1, ...);
   TFuncExpr = class (TFuncExprBase)
      private
         FInitResultExpr : TDataExpr;
         FStates : TFuncExprStates;
         FPushExprs : packed array of TPushOperator;
         FResultAddr : Integer;
         FCodeExpr : TDataExpr;

      protected
         function PostCall(const ScriptObj: IScriptObj): Variant; virtual;
         function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; virtual;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TFuncSymbol;
                            IsInstruction: Boolean = True; CodeExpr: TDataExpr = nil;
                            IsWritable: Boolean = False);
         destructor Destroy; override;
         function AddArg(Arg: TNoPosExpr) : TSymbol; override;
         procedure AddPushExprs;
         function Eval: Variant; override;
         function GetData: TData; override;
         function GetAddr: Integer; override;
         procedure GetCode(Func : TFuncSymbol; var result : ICallable); virtual;
         procedure Initialize; override;
         procedure SetResultAddr(ResultAddr: Integer = -1);
         function IsWritable : Boolean; override;
         property CodeExpr : TDataExpr read FCodeExpr;
   end;

  TFuncCodeExpr = class(TPosDataExpr)
  private
    FFuncExpr : TFuncExpr;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; FuncExpr: TFuncExpr);
    destructor Destroy; override;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
    procedure AssignDataExpr(Right: TDataExpr); override;
    function Eval: Variant; override;
    function GetData: TData; override;
    function GetAddr: Integer; override;
    property FuncExpr : TFuncExpr read FFuncExpr;
  end;

  TMethodObjExpr = class(TPosDataExpr)
  private
    FBaseExpr : TDataExpr;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr);
    function GetData: TData; override;
    function GetAddr: Integer; override;
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
    function GetData: TData; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: string;
                       BaseExpr: TNoPosExpr; IsWritable: Boolean = True; IsIndex: Boolean = False);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    function AddArg(ArgExpr: TNoPosExpr) : TSymbol;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function IsWritable : Boolean; override;
  end;

  TConnectorReadExpr = class(TPosDataExpr)
  protected
    FBaseExpr: TNoPosExpr;
    FConnectorMember: IConnectorMember;
    FName: string;
    FResultData: TData;
    function GetData: TData; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: string;
                       BaseExpr: TNoPosExpr);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    function Eval: Variant; override;
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
    procedure EvalNoResult(var status : TExecutionStatusResult); override;
    procedure Initialize; override;
  end;

  // Call of static methods (not virtual)
  TMethodStaticExpr = class(TFuncExpr)
  private
    FBaseExpr: TDataExpr;
    FSelfAddr: Integer;
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
      BaseExpr: TDataExpr; IsInstruction: Boolean = True;
      CodeExpr: TDataExpr = nil; IsWritable: Boolean = False);
    destructor Destroy; override;
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
    procedure Initialize; override;
    property BaseExpr: TDataExpr read FBaseExpr;
  end;

  // Class methods
  TClassMethodStaticExpr = class(TMethodStaticExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TConstructorStaticExpr = class(TMethodStaticExpr)
  private
    FExternalObject: TObject;
  protected
    function PostCall(const ScriptObj: IScriptObj): Variant; override;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                       Base: TDataExpr; IsInstruction: Boolean = True);
    procedure TypeCheckNoPos(const aPos : TScriptPos); override;
    property ExternalObject: TObject read FExternalObject write FExternalObject;
  end;

  TDestructorStaticExpr = class(TMethodStaticExpr)
  end;

  TMethodVirtualExpr = class(TMethodStaticExpr)
  private
    FMethName: string;
  protected
    function FindVirtualMethod(ClassSym: TClassSymbol): TMethodSymbol;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                       Base: TDataExpr; IsInstruction: Boolean = True);
    property MethName: string read FMethName;
  end;

  // Call to Class method with class reference: TMyClass.ClassMethod(..)
  TClassMethodVirtualExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TClassMethodVirtualNameExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  // Call to Class method with object reference: obj.ClassMethod(..)
  TClassMethodObjVirtualExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TClassMethodObjVirtualNameExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TConstructorVirtualExpr = class(TMethodVirtualExpr)
  private
    FExternalObject: TObject;
  protected
    function PostCall(const ScriptObj: IScriptObj): Variant; override;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                       Base: TDataExpr; IsInstruction: Boolean = True);
    property ExternalObject: TObject read FExternalObject write FExternalObject;
  end;

  TConstructorStaticObjExpr = class(TMethodStaticExpr)
  protected
    function PostCall(const ScriptObj: IScriptObj): Variant; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
      BaseExpr: TDataExpr; IsInstruction: Boolean = True;
      CodeExpr: TDataExpr = nil; IsWritable: Boolean = False);
  end;

  TConstructorVirtualObjExpr = class(TMethodVirtualExpr)
  protected
    function PostCall(const ScriptObj: IScriptObj): Variant; override;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol; Base:
      TDataExpr; IsInstruction: Boolean = True);
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
         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
         function IsConstant : Boolean; override;
         property Expr: TNoPosExpr read FExpr write FExpr;
   end;

   // bool unary result
   TUnaryOpBoolExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval : Variant; override;
   end;

   // int unary result
   TUnaryOpIntExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval : Variant; override;
   end;

   // float unary result
   TUnaryOpFloatExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval : Variant; override;
   end;

   // string unary result
   TUnaryOpStringExpr = class(TUnaryOpExpr)
      public
         constructor Create(Prog: TdwsProgram; Expr: TNoPosExpr);
         function Eval : Variant; override;
   end;


  TNoResultWrapperExpr = class(TNoResultExpr)
  protected
    FExpr: TNoPosExpr;
  public
    constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TNoPosExpr);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure EvalNoResult(var status : TExecutionStatusResult); override;
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

         function Eval: Variant; override;
         procedure Initialize; override;
         procedure TypeCheckNoPos(const aPos : TScriptPos); override;
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
    function GetData: TData;
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
    FCaller: TdwsProgram;
    FFuncSym: TFuncSymbol;
    FLevel: Integer;
    FScriptObj: IScriptObj;
    FTable: TSymbolTable;
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

  protected
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
    property Caller: TdwsProgram read FCaller write FCaller;
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

  // A instance of a script class FClassSym. Instance data in FData,
  TScriptObj = class(TInterfacedObject, IScriptObj)
  private
    FClassSym: TClassSymbol;
    FData: TData;
    FExternalObj: TObject;
    FProg: TdwsProgram;
    FOnObjectDestroy: TObjectDestroyEvent;
    FNextObject, FPrevObject : TScriptObj;
  protected
    { IScriptObj }
    function GetClassSym: TClassSymbol;
    function GetData: TData;
    function DataOfAddr(addr : Integer) : Variant;
    function DataOfAddrAsString(addr : Integer) : String;
    function DataOfAddrAsInteger(addr : Integer) : Int64;
    procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
    procedure SetData(const Dat: TData);
    function GetExternalObject: TObject;
    procedure SetExternalObject(Value: TObject);
  public
    constructor Create(ClassSym: TClassSymbol; Prog: TdwsProgram = nil);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;

    property Prog : TdwsProgram read FProg write FProg;
    property NextObject : TScriptObj read FNextObject write FNextObject;
    property PrevObject : TScriptObj read FPrevObject write FPrevObject;
  end;

function CreateMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
                          const Pos: TScriptPos; IsInstruction: Boolean; ForceStatic : Boolean = False): TFuncExpr;

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
    procedure Read(const Data: TData);
    procedure Write(const Data: TData);
    property Caption: string read GetCaption;
    property Size: Integer read GetSize;
  end;

  // private implementation of IInfo
  TInfo = class(TInterfacedObject, IUnknown, IInfo)
  protected
    FCaller: TdwsProgram;
    FChild: IInfo;
    FData: TData;
    FOffset: Integer;
    FProgramInfo: TProgramInfo;
    FDataMaster: IDataMaster;
    FTypeSym: TSymbol;
    function GetData: TData; virtual;
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
    function GetData: TData; override;
  end;

  TInfoData = class(TInfo)
    function GetValue: Variant; override;
    function GetValueAsString : String; override;
    function GetValueAsInteger : Int64; override;
    function GetValueAsFloat : Double; override;
    function GetData: TData; override;
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
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; const DataMaster: IDataMaster = nil);
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
    function GetData: TData; override;
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
    FCaller: TdwsProgram;
    FSym: TSymbol;
    function GetCaption: string;
    function GetSize: Integer;
  public
    constructor Create(Caller: TdwsProgram; Sym: TSymbol);
    procedure Read(const Data: TData); virtual;
    procedure Write(const Data: TData); virtual;
  end;

  TExternalVarDataMaster = class(TDataMaster)
  public
    procedure Read(const Data: TData); override;
    procedure Write(const Data: TData); override;
  end;

  TConnectorMemberDataMaster = class(TDataMaster)
  private
    FBaseValue: Variant;
    FName: string;
  public
    constructor Create(Caller: TdwsProgram; Sym: TSymbol; BaseValue: Variant; const Name: string);
    procedure Read(const Data: TData); override;
    procedure Write(const Data: TData); override;
  end;

  TCleanUpEvent = procedure(ScriptObj: IScriptObj; ExternalObject: TObject) of object;

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

function GetFuncExpr(Prog: TdwsProgram; FuncSym: TFuncSymbol; ScriptObj: IScriptObj;
  ClassSym: TClassSymbol; ForceStatic: Boolean = False): TFuncExpr;
begin
  if FuncSym is TMethodSymbol then
  begin
    if Assigned(ScriptObj) then
      Result := CreateMethodExpr(
        TMethodSymbol(funcSym),
        TConstExpr.Create(Prog, {ScriptObj.}ClassSym, ScriptObj),
                          rkObjRef, cNullPos, True, ForceStatic)
    else
      Result := CreateMethodExpr(
        TMethodSymbol(funcSym),
        TConstExpr.Create(Prog, ClassSym.ClassOf, ClassSym.Name),
                          rkClassOfRef, cNullPos, True, ForceStatic)
  end
  else
    Result := TFuncExpr.Create(Prog, cNullPos, TFuncSymbol(funcSym), True);
end;

// CreateMethodExpr
//
function CreateMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
                       const Pos: TScriptPos; IsInstruction: Boolean; ForceStatic : Boolean = False): TFuncExpr;
begin
  // Create the correct TExpr for a method symbol
  Result := nil;

  // Return the right expression
  case meth.Kind of
    fkFunction, fkProcedure, fkMethod:
      if meth.IsClassMethod then
      begin
        if not ForceStatic and meth.IsVirtual and (RefKind = rkClassOfRef) then
          if (Expr is TConstExpr) and (VarIsStr(Expr.Eval)) then
            Result := TClassMethodVirtualNameExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
          else
            Result := TClassMethodVirtualExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
        else if not ForceStatic and meth.IsVirtual and (RefKind = rkObjRef) then
          if (Expr is TConstExpr) and (VarIsStr(Expr.Eval)) then
            Result := TClassMethodObjVirtualNameExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
          else
            Result := TClassMethodObjVirtualExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
        else
          Result := TClassMethodStaticExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
      end
      else
      begin
        Assert(RefKind = rkObjRef);
        if not ForceStatic and meth.IsVirtual then
          Result := TMethodVirtualExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
        else
          Result := TMethodStaticExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction);
      end;
    fkConstructor:
      if RefKind = rkClassOfRef then
      begin
        if not ForceStatic and meth.IsVirtual then
          Result := TConstructorVirtualExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
        else
          Result := TConstructorStaticExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction);
      end
      else
      begin
        if not ForceStatic and meth.IsVirtual then
          Result := TConstructorVirtualObjExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
        else
          Result := TConstructorStaticObjExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction);
      end;
    fkDestructor:
      begin
        Assert(RefKind = rkObjRef);
        if not ForceStatic and meth.IsVirtual then
          Result := TDestructorVirtualExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
        else
          Result := TDestructorStaticExpr.Create(Expr.Prog, Pos, meth, Expr, IsInstruction)
      end;
  end;
end;

{ TdwsProgram }

constructor TdwsProgram.Create(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                               MaxRecursionDepth : Integer;
                               MaxDataSize, StackChunkSize: Integer);
begin
   FConditionalDefines:=TStringList.Create;
   FConditionalDefines.Sorted:=True;
   FConditionalDefines.CaseSensitive:=False;
   FConditionalDefines.Duplicates:=dupIgnore;

   FResultType := ResultType;
   FProgramState := psUndefined;

   FMsgs := TdwsMessageList.Create;
   FRoot := Self;

   // Create the Symbol Dictionary
   FSymbolDictionary := TSymbolDictionary.Create;
   // Create Context Map
   FContextMap := TContextMap.Create;
   //Create Script Source List
   FSourceList := TScriptSourceList.Create;

   // Create the program stack
   FStack := TStack.Create(StackChunkSize, MaxDataSize, MaxRecursionDepth);
   FStack.Reset;
   FAddrGenerator := TAddrGeneratorRec.CreatePositive(0);
   FGlobalAddrGenerator := TAddrGeneratorRec.CreatePositive(0);

   FUnifiedConstList:=TUnifiedConstList.Create;

   // Initialize the system table
   FRootTable := TProgramSymbolTable.Create(SystemTable, @FAddrGenerator);
   FTable := FRootTable;

   FInitExpr := TBlockInitExpr.Create(Self, cNullPos);

   // Initialize shortcuts to often used symbols
   FTypBoolean := SystemTable.FindSymbol(SYS_BOOLEAN) as TTypeSymbol;
   FTypFloat := SystemTable.FindSymbol(SYS_FLOAT) as TTypeSymbol;
   FTypInteger := SystemTable.FindSymbol(SYS_INTEGER) as TTypeSymbol;
   FTypString := SystemTable.FindSymbol(SYS_STRING) as TTypeSymbol;
   FTypVariant := SystemTable.FindSymbol(SYS_VARIANT) as TTypeSymbol;
   FTypNil := TNilSymbol.Create;
   FTypObject := TClassSymbol(SystemTable.FindSymbol(SYS_TOBJECT));
end;

destructor TdwsProgram.Destroy;
begin
   FProgramInfo.Free;
   FResult.Free;
   FExpr.Free;
   FInitExpr.Free;
   FRootTable.Free;
   FStack.Free;
   FTypNil.Free;
   FMsgs.Free;
   FSymbolDictionary.Free;
   FContextMap.Free;
   FSourceList.Free;
   FUnifiedConstList.Free;
   FConditionalDefines.Free;
   inherited;
end;

// Starts the program but does not terminate it.
// Use .RunProgram() to run the main program or .Info property to call procedures.
// Call EndProgram() to terminate the program
procedure TdwsProgram.BeginProgram;
var
   status : TExecutionStatusResult;
begin
  try
    // Program is already running
    if FProgramState = psRunning then
      Msgs.AddErrorStop(RTE_ScriptAlreadyRunning);

    // Compilation terminated with errors
    if FProgramState = psUndefined then
      Msgs.AddErrorStop(RTE_CantRunScript);

    if FProgramState <> psReadyToRun then
      Msgs.AddErrorStop('ProgramState should be "ReadyToRun"');

    // Initialize Result
    FResult.Free;
    FResult := FResultType.CreateProgResult;

    Msgs.Clear;

    // Stack
    FStack.Reset;

    FInfo := TProgramInfo.Create;
    FInfo.Table := FTable;
    FInfo.Caller := Self;

    // Result
    FResult.InitializeProgram(Self);

    // allocate global stack space
    FStack.Push(
      FGlobalAddrGenerator.DataSize +
      FAddrGenerator.DataSize);
    FStack.PushBp(0, FStack.BasePointer);

    FProgramState := psRunning;

    // Debugger
    FIsDebugging := Assigned(FDebugger);
    if FIsDebugging then
      FDebugger.StartDebug(Self);

    // Prepare FileSystem
    if FRuntimeFileSystem<>nil then
       FFileSystem:=FRuntimeFileSystem.AllocateFileSystem
    else FFileSystem:=TdwsOSFileSystem.Create;

    // Initialize global variables
    status:=esrNone;
    FInitExpr.EvalNoResult(status);

  except
    on e: EScriptError do
      ;
    on e: Exception do
      FMsgs.AddExecutionError(e.Message);
  end;
end;

// EndProgram
//
procedure TdwsProgram.EndProgram;

   procedure ReleaseObjects;
   var
      iter : TScriptObj;
   begin
      while FFirstObject<>nil do begin
         iter:=FFirstObject;
         ScriptObjDestroyed(iter);
         iter.FRefCount:=0;
         iter.Free;
      end;
   end;

begin
   if not (FProgramState in [psRunning, psRunningStopped]) then
      raise Exception.Create('Program was not started!');

   FProgramState := psTerminated;

   try
      // Result
      FResult.FinalizeProgram(Self);

      // Flags
      FIsDebugging := False;

      // Stack
      FStack.Pop(FAddrGenerator.DataSize + FGlobalAddrGenerator.DataSize);

      // Object Cycles
      if FFirstObject<>nil then
         ReleaseObjects;

      // FileSystem
      FFileSystem:=nil;

      // Debugger
      if Assigned(FDebugger) then
         FDebugger.StopDebug(Self);

      FProgramState := psReadyToRun;

      FreeAndNil(FInfo);
   except
      on e: EScriptError do
         ;
      on e: Exception do
         Msgs.AddExecutionError(e.Message);
   end;
end;

// Execute
//
procedure TdwsProgram.Execute;
begin
   Execute(0);
end;

procedure TdwsProgram.Execute(aTimeoutMilliSeconds: Integer);
begin
  BeginProgram;
  if ProgramState = psRunning then
    RunProgram(aTimeoutMilliSeconds);
  if ProgramState in [psRunning, psRunningStopped] then
    EndProgram;
end;

procedure TdwsProgram.Evaluate(var status : TExecutionStatusResult);
begin
    FExpr.EvalNoResult(status);
    if FRoot.ProgramState = psRunningStopped then
       Msgs.AddExecutionStop(Expr.Pos, RTE_ScriptStopped);
end;

// SetConditionalDefines
//
procedure TdwsProgram.SetConditionalDefines(const val : TStringList);
begin
   FConditionalDefines.Assign(val);
end;

procedure TdwsProgram.RunProgram(aTimeoutMilliSeconds: Integer);
var
   terminator: TTerminatorThread;
   status : TExecutionStatusResult;
begin
   try
      if FProgramState <> psRunning then
         raise Exception.Create('Program state psRunning expected');

      if aTimeoutMilliSeconds=0 then
         aTimeOutMilliseconds:=TimeoutMilliseconds;
      if aTimeoutMilliSeconds>0 then
         terminator:=TTerminatorThread.Create(Self, aTimeoutMilliSeconds)
      else terminator:=nil;

      try
         status:=esrNone;
         try
            // Run the script
            Evaluate(status);
            if status<>esrNone then begin
               case status of
                  esrBreak : Msgs.AddExecutionError(RTE_InvalidBreak);
                  esrContinue : Msgs.AddExecutionError(RTE_InvalidContinue);
               end;
            end;
         except
            on e: EScriptException do
               Msgs.AddExecutionError(e.Pos, e.Message);
            on e: EScriptError do
               Msgs.AddExecutionError(e.Pos, e.Message);
            on e: Exception do
               Msgs.AddExecutionError(e.Message);
         end;

      finally
         if Assigned(terminator) then
            terminator.Terminate;
      end;

      Msgs.SetLastScriptError(cNullPos);
    
   except
      on e: EScriptError do
         ; // Error message in FMsgs
      on e: Exception do
         Msgs.AddExecutionError(e.Message);
   end;
end;


procedure TdwsProgram.ExecuteParam(const Params: array of Variant);
begin
  ExecuteParam(Params, 0)
end;

procedure TdwsProgram.ExecuteParam(const Params: array of Variant; aTimeoutMilliSeconds: Integer);
var
  x, index: Integer;
begin
  SetLength(FParameters, High(Params) - Low(Params) + 1);
  index := 0;
  for x := Low(Params) to High(Params) do
  begin
    FParameters[index] := Params[x];
    Inc(index);
  end;

  Execute(aTimeoutMilliSeconds);
end;

procedure TdwsProgram.ExecuteParam(const Params: OleVariant);
begin
  ExecuteParam(Params, 0);
end;

procedure TdwsProgram.ExecuteParam(const Params: OleVariant; aTimeoutMilliSeconds: Integer);
var
  x: Integer;
begin
  if VarIsArray(Params) then
  begin
    SetLength(FParameters, VarArrayHighBound(Params, 1) + 1);
    for x := 0 to VarArrayHighBound(Params, 1) do
      FParameters[x] := Params[x];
  end
  else
  begin
    SetLength(FParameters, 1);
    FParameters[0] := Params;
  end;

  Execute(aTimeoutMilliSeconds);
end;

procedure TdwsProgram.DoStep(Expr: TExpr);
begin
   if FRoot.ProgramState = psRunningStopped then
      Msgs.AddExecutionStop(Expr.Pos, RTE_ScriptStopped)
   else if FRoot.IsDebugging then
      FRoot.Debugger.DoDebug(Self, Expr);
end;

procedure TdwsProgram.SetDebugger(const Value: IDebugger);
begin
  if FRoot = Self then
    FDebugger := Value
  else
    FRoot.Debugger := Value;
end;

procedure TdwsProgram.Stop;
begin
  if FProgramState = psRunning then
    FProgramState := psRunningStopped;
end;

// AcquireProgramInfo
//
function TdwsProgram.AcquireProgramInfo(funcSym : TFuncSymbol) : TProgramInfo;
begin
   if FProgramInfo=nil then begin
      Result:=TProgramInfo.Create;
      Result.Caller:=Self;
   end else begin
      Result:=FProgramInfo;
      FProgramInfo:=nil;
   end;
   Result.FuncSym:=funcSym;
   Result.FTable:=funcSym.Params;
end;

// ReleaseProgramInfo
//
procedure TdwsProgram.ReleaseProgramInfo(info : TProgramInfo);
begin
   if FProgramInfo=nil then begin
      FProgramInfo:=info;
      info.ScriptObj:=nil;
   end else info.Free;
end;

function TdwsProgram.GetLevel: Integer;
begin
  Result := FAddrGenerator.Level;
end;

function TdwsProgram.GetResult: TdwsResult;
begin
  Result := FResult;
end;

procedure TdwsProgram.SetResult(const Value: TdwsResult);
begin
  FResult := Value;
end;

function TdwsProgram.GetUserDef: TObject;
begin
  Result := FUserDef;
end;

procedure TdwsProgram.SetUserDef(const Value: TObject);
begin
  FUserDef := Value;
end;

// Called by the compiler if compilation has been finished successfully
procedure TdwsProgram.ReadyToRun;
begin
  if FProgramState = psUndefined then
    FProgramState := psReadyToRun;
end;

function TdwsProgram.GetGlobalAddr(DataSize: Integer): Integer;
begin
  Result := FRoot.FGlobalAddrGenerator.GetStackAddr(DataSize);
end;

// ScriptObjCreated
//
procedure TdwsProgram.ScriptObjCreated(scriptObj: TScriptObj);
begin
   scriptObj.Prog:=Self;
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
procedure TdwsProgram.ScriptObjDestroyed(scriptObj: TScriptObj);
begin
   scriptObj.Prog:=nil;
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
procedure TdwsProgram.DestroyScriptObj(const ScriptObj: IScriptObj);
var
   sym: TSymbol;
   func: TMethodSymbol;
   expr: TDestructorVirtualExpr;
   status : TExecutionStatusResult;
begin
   try
      sym := ScriptObj.ClassSym.Members.FindSymbol(SYS_TOBJECT_DESTROY);

      if sym is TMethodSymbol then begin
         func := TMethodSymbol(sym);
         if (func.Kind = fkDestructor) and (func.Params.Count = 0) then begin
            expr := TDestructorVirtualExpr.Create(Self, cNullPos, func,
                                                  TConstExpr.Create(Self, ScriptObj.ClassSym, ScriptObj));
            try
               status:=esrNone;
               expr.EvalNoResult(status);
            finally
               expr.Free;
            end;
         end;
      end;
   except
      on e: Exception do
         Msgs.AddError(e.Message);
   end;
end;

function TdwsProgram.GetTempAddr(DataSize: Integer): Integer;
begin
  Assert(Root.ProgramState = psUndefined);
  Result := FAddrGenerator.GetStackAddr(DataSize);
end;

{ TProcedure }

constructor TProcedure.Create(Parent: TdwsProgram);
begin
  FParent := Parent;

  // Create a local symbol table and connect it to the parent symboltable
  FAddrGenerator := TAddrGeneratorRec.CreatePositive(Parent.Level + 1);
  FRootTable := TProgramSymbolTable.Create(Parent.Table, @FAddrGenerator);
  FTable := FRootTable;

  FInitExpr := TBlockInitExpr.Create(Self, cNullPos);

  // Connect the procedure to the root TdwsProgram
  FRoot := Parent.Root;
  FMsgs := Parent.FMsgs;
  FTypBoolean := FRoot.TypBoolean;
  FTypFloat := FRoot.TypFloat;
  FTypInteger := FRoot.TypInteger;
  FTypNil := FRoot.TypNil;
  FTypString := FRoot.TypString;
  FTypVariant := FRoot.TypVariant;
  FTypObject := FRoot.TypObject;
  FStack := Root.Stack;
  FSymbolDictionary := Parent.SymbolDictionary;
  FContextMap := Parent.ContextMap;
end;

destructor TProcedure.Destroy;
begin
   FProgramInfo.Free;
   FRootTable.Free;
   FExpr.Free;
   FInitExpr.Free;
end;

procedure TProcedure.AssignTo(sym: TFuncSymbol);
begin
  // Add parameter symboltable into the symboltable chain
  FTable.InsertParent(0, sym.Params);
  sym.Executable := ICallable(Self);
  FFunc := sym;
end;

procedure TProcedure.Call(Caller: TdwsProgram; Func: TFuncSymbol);
begin
  if Caller.Root = Root then
    Execute
  else begin
    raise Exception.Create('Feature not supported!');
    // TODO
  end;
end;

procedure TProcedure.Execute;
var
   status : TExecutionStatusResult;
begin
   // Allocate stack space for local variables
   FRoot.Stack.Push(FAddrGenerator.DataSize);

   // Run the procedure
   try
      status:=esrNone;
      FInitExpr.EvalNoResult(status);
      FExpr.EvalNoResult(status);

      if status<>esrNone then begin
         case status of
            esrBreak : Msgs.AddExecutionError(RTE_InvalidBreak);
            esrContinue : Msgs.AddExecutionError(RTE_InvalidContinue);
         end;
      end;

   finally
      // Free stack space for local variables
      FRoot.Stack.Pop(FAddrGenerator.DataSize);
   end;
end;

function TProcedure.GetResult: TdwsResult;
begin
  Result := FRoot.Result;
end;

function TProcedure.GetUserDef: TObject;
begin
  Result := FRoot.UserDef;
end;

procedure TProcedure.InitSymbol(Symbol: TSymbol);
begin
  FTable.Initialize(msgs);
  FExpr.Initialize;
end;

procedure TProcedure.InitExpression(Expr: TExprBase);
begin
end;

procedure TProcedure.SetResult(const Value: TdwsResult);
begin
  Root.Result := Value;
end;

procedure TProcedure.SetUserDef(const Value: TObject);
begin
  FRoot.UserDef := Value;
end;

procedure TProcedure.Stop;
begin
  FRoot.Stop;
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
constructor TTerminatorThread.Create(aProgram : TdwsProgram; aMilliSecToLive : Integer);
begin
   FProg:=aProgram;
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

   if (not Terminated) and Assigned(FProg) then
      FProg.Stop;

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

function TNoPosExpr.CreateEDelphiObj(const ClassName, Message: string): IScriptObj;
var
   info: TProgramInfo;
begin
   info := FProg.Info;
   Result := IScriptObj(IUnknown(
      info.Vars[SYS_EDELPHI].Method[SYS_TOBJECT_CREATE].Call([
        ClassName, Message]).Value));
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
   Result:=Typ.IsBaseTypeIDValue(typBooleanID);
end;

// IsFloatValue
//
function TNoPosExpr.IsFloatValue : Boolean;
begin
   Result:=Typ.IsBaseTypeIDValue(typFloatID);
end;

// IsIntegerValue
//
function TNoPosExpr.IsIntegerValue : Boolean;
begin
   Result:=Typ.IsBaseTypeIDValue(typIntegerID);
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
   Result:=Typ.IsBaseTypeIDValue(typStringID);
end;

// IsVariantValue
//
function TNoPosExpr.IsVariantValue : Boolean;
begin
   Result:=Typ.IsBaseTypeIDValue(typVariantID);
end;

// IsConstant
//
function TNoPosExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// Optimize
//
function TNoPosExpr.Optimize : TNoPosExpr;
begin
   Result:=Self;
end;

// OptimizeIntegerConstantToFloatConstant
//
function TNoPosExpr.OptimizeIntegerConstantToFloatConstant : TNoPosExpr;
var
   temp : Double;
begin
   if IsConstant and IsIntegerValue then begin
      EvalAsFloat(temp);
      Result:=TConstFloatExpr.CreateUnified(FProg, nil, temp);
      Free;
   end else Result:=Self;
end;

function TNoPosExpr.GetBaseType: TTypeSymbol;
begin
  if Assigned(Typ) then
    result := Typ.BaseType
  else
    result := nil;
end;

// EvalAsVariant
//
procedure TNoPosExpr.EvalAsVariant(var Result : Variant);
begin
   Result:=Eval;
end;

// EvalAsScriptObj
//
procedure TNoPosExpr.EvalAsScriptObj(var Result : IScriptObj);
begin
   Result:=IScriptObj(IUnknown(Eval));
end;

// AssignValue
//
procedure TNoPosExpr.AssignValue(const value : Variant);
begin
   raise EScriptError.CreateFmt('Cannot assign to %s', [ClassName]);
end;

// AssignValueAsInteger
//
procedure TNoPosExpr.AssignValueAsInteger(const value : Int64);
begin
   AssignValue(value);
end;

// AssignValueAsBoolean
//
procedure TNoPosExpr.AssignValueAsBoolean(const value : Boolean);
begin
   AssignValue(value);
end;

// AssignValueAsFloat
//
procedure TNoPosExpr.AssignValueAsFloat(var value : Double);
begin
   AssignValue(value);
end;

// AssignValueAsString
//
procedure TNoPosExpr.AssignValueAsString(const value: String);
begin
   AssignValue(value);
end;

// EvalAsInteger
//
function TNoPosExpr.EvalAsInteger : Int64;
var
   v : Variant;
begin
   v:=Eval;
   try
      Result:=v;
   except
      // workaround for RTL bug that will sometimes report a failed cast to Int64
      // as being a failed cast to Boolean
      on E : EVariantTypeCastError do begin
         raise EVariantTypeCastError.CreateFmt(CPE_AssignIncompatibleTypes,
                                               [VarTypeAsText(VarType(v)), 'Integer'])
      end else raise;
   end;
end;

// EvalAsBoolean
//
function TNoPosExpr.EvalAsBoolean : Boolean;
var
   v : Variant;
begin
   v:=Eval;
   try
      Result:=v;
   except
      // standardize RTL message
      on E : EVariantTypeCastError do begin
         raise EVariantTypeCastError.CreateFmt(CPE_AssignIncompatibleTypes,
                                               [VarTypeAsText(VarType(v)), 'Boolean'])
      end else raise;
   end;
end;

// EvalAsFloat
//
procedure TNoPosExpr.EvalAsFloat(var Result : Double);
var
   v : Variant;
begin
   v:=Eval;
   try
      Result:=v;
   except
      // standardize RTL message
      on E : EVariantTypeCastError do begin
         raise EVariantTypeCastError.CreateFmt(CPE_AssignIncompatibleTypes,
                                               [VarTypeAsText(VarType(v)), 'Float'])
      end else raise;
   end;
end;

// EvalAsString
//
procedure TNoPosExpr.EvalAsString(var Result : String);
var
   v : Variant;
begin
   v:=Eval;
   try
      Result:=String(v);
   except
      // standardize RTL message
      on E : EVariantTypeCastError do begin
         raise EVariantTypeCastError.CreateFmt(CPE_AssignIncompatibleTypes,
                                               [VarTypeAsText(VarType(v)), 'String'])
      end else raise;
   end;
end;

// EvalNoResult
//
procedure TNoPosExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   Eval;
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

// AddCompilerWarning
//
procedure TExpr.AddCompilerWarning(const Text : String);
begin
   Prog.Msgs.AddCompilerWarning(Pos, Text);
end;

// AddCompilerError
//
procedure TExpr.AddCompilerError(const Text : String);
begin
   Prog.Msgs.AddCompilerError(Pos, Text);
end;

// AddCompilerErrorFmt
//
procedure TExpr.AddCompilerErrorFmt(const fmtText: string; const Args: array of const);
begin
   Prog.Msgs.AddCompilerErrorFmt(Pos, fmtText, Args);
end;

// AddCompilerStop
//
procedure TExpr.AddCompilerStop(const Text : String);
begin
   Prog.Msgs.AddCompilerStop(Pos, Text);
end;

// AddExecutionStop
//
procedure TExpr.AddExecutionStop(const Text : String);
begin
   Prog.Msgs.AddExecutionStop(Pos, Text);
end;

// AddExecutionStopFmt
//
procedure TExpr.AddExecutionStopFmt(const fmtText: string; const Args: array of const);
begin
   AddExecutionStop(Format(fmtText, Args));
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
   Prog.Msgs.AddCompilerErrorFmt(Pos, fmttext, Args);
end;

// AddCompilerStop
//
procedure TPosDataExpr.AddCompilerStop(const Text : String);
begin
   Prog.Msgs.AddCompilerStop(Pos, Text);
end;

// AddExecutionStop
//
procedure TPosDataExpr.AddExecutionStop(const Text : String);
begin
   Prog.Msgs.AddExecutionStop(Pos, Text);
end;

// AddExecutionStopFmt
//
procedure TPosDataExpr.AddExecutionStopFmt(const fmtText: string; const Args: array of const);
begin
   AddExecutionStop(Format(fmtText, Args));
end;

// ------------------
// ------------------ TNoResultExpr ------------------
// ------------------

function TNoResultExpr.Eval: Variant;
var
   status : TExecutionStatusResult;
begin
   status:=esrNone;
   EvalNoResult(status);
   Assert(status=esrNone);
end;

// EvalNoResult
//
procedure TNoResultExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   //nothing
end;

// OptimizeToNoResultExpr
//
function TNoResultExpr.OptimizeToNoResultExpr : TNoResultExpr;
var
   optimized : TNoPosExpr;
begin
   optimized:=Optimize;
   if optimized is TNoResultExpr then
      Result:=TNoResultExpr(optimized)
   else begin
      Result:=nil;
      Assert(False);
   end;
end;

{ TNullExpr }

procedure TNullExpr.EvalNoResult(var status : TExecutionStatusResult);
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
procedure TBlockInitExpr.EvalNoResult(var status : TExecutionStatusResult);
var
   i : Integer;
   expr : TExpr;
begin
   for i:=0 to FCount-1 do begin
      expr:=FStatements[i];
      expr.EvalNoResult(status);
      if status<>esrNone then Break;
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

function TDataExpr.Eval: Variant;
begin
  Result := Data[Addr];
end;

// IsWritable
//
function TDataExpr.IsWritable: Boolean;
begin
   Result:=True;
end;

function TDataExpr.GetAddr: Integer;
begin
  Result := 0;
end;

procedure TDataExpr.AssignData(const SourceData: TData; SourceAddr: Integer);
begin
  Assert(IsWritable);
  CopyData(SourceData, SourceAddr, Data, Addr, Typ.Size);
end;

procedure TDataExpr.AssignValue(const Value: Variant);
begin
  Assert(IsWritable);
  VarCopy(Data[Addr], Value);
end;

// AssignValueAsInteger
//
procedure TDataExpr.AssignValueAsInteger(const value : Int64);
begin
   AssignValue(value);
end;

// AssignValueAsBoolean
//
procedure TDataExpr.AssignValueAsBoolean(const value : Boolean);
begin
   AssignValue(value);
end;

// AssignValueAsFloat
//
procedure TDataExpr.AssignValueAsFloat(var value : Double);
begin
   AssignValue(value);
end;

// AssignValueAsString
//
procedure TDataExpr.AssignValueAsString(const value: String);
begin
   AssignValue(value);
end;

procedure TDataExpr.AssignExpr(Expr: TNoPosExpr);
begin
  Assert(IsWritable);
  VarCopy(Data[Addr], Expr.Eval);
end;

procedure TDataExpr.AssignDataExpr(DataExpr: TDataExpr);
begin
  CopyData(DataExpr.Data, DataExpr.Addr, Data, Addr, Typ.Size);
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
   if FArgs.Count>paramCount then
      AddCompilerErrorFmt(CPE_TooManyArguments, [])
   else begin
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
function TFuncExprBase.Optimize : TNoPosExpr;
begin
   Result:=Self;
   if IsConstant then begin
      Initialize;
      try
         Result:=TConstExpr.CreateTyped(Prog, Typ, Eval);
      except
         on E: Exception do begin
            FProg.Msgs.AddCompilerErrorFmt(Pos, CPE_FunctionOptimizationFailed,
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

// InitPushData
//
procedure TPushOperator.InitPushLazy(StackAddr: Integer; ArgExpr: TNoPosExpr);
begin
   FTypeParamSym:=TSymbol(potLazy);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// Execute
//
procedure TPushOperator.Execute(stack : TStack);
begin
   case TPushOperatorType(FTypeParamSym) of
      potAddr : ExecuteAddr(stack);
      potTempAddr : ExecuteTempAddr(stack);
      potTempArrayAddr : ExecuteTempArrayAddr(stack);
      potTempArray : ExecuteTempArray(stack);
      potResultBoolean : ExecuteResultBoolean(stack);
      potResultInteger : ExecuteResultInteger(stack);
      potResultFloat : ExecuteResultFloat(stack);
      potResultString : ExecuteResultString(stack);
      potResultConstString : ExecuteResultConstString(stack);
      potResult : ExecuteResult(stack);
      potLazy : ExecuteLazy(stack);
   else
      ExecuteData(stack);
   end;
end;

type

  TVarParamData = class (TInterfacedObject, IVarParamData)
  private
    FData: TData;
    FAddr: Integer;
  protected
    function GetData: TData;
    function GetAddr: Integer;
  public
    constructor Create(Data: TData; Addr: Integer);
  end;

{ TVarParamData }

constructor TVarParamData.Create(Data: TData; Addr: Integer);
begin
  inherited Create;
  FData := Data;
  FAddr := Addr;
end;

function TVarParamData.GetAddr: Integer;
begin
  Result := FAddr;
end;

function TVarParamData.GetData: TData;
begin
  Result := FData;
end;

// ExecuteAddr
//
procedure TPushOperator.ExecuteAddr(stack : TStack);
var
   vpd: IVarParamData;
begin
   vpd := TVarParamData.Create(TDataExpr(FArgExpr).Data, TDataExpr(FArgExpr).Addr);
   stack.WriteValue(stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempAddr
//
procedure TPushOperator.ExecuteTempAddr(stack : TStack);
var
   vpd : IVarParamData;
   data : TData;
begin
   SetLength(data, 1);
   data[0]:=FArgExpr.Eval;
   vpd := TVarParamData.Create(data, 0);
   stack.WriteValue(stack.StackPointer + FStackAddr, vpd);
end;

// ExecuteTempArrayAddr
//
procedure TPushOperator.ExecuteTempArrayAddr(stack : TStack);
var
   vpd : IVarParamData;
   data : TData;
begin
   data:=TArrayConstantExpr(FArgExpr).EvalAsTData;
   vpd:=TVarParamData.Create(data, 0);
   stack.WriteValue(stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteTempArray
//
procedure TPushOperator.ExecuteTempArray(stack : TStack);
var
   vpd : IVarParamData;
   data : TData;
begin
   data:=TConstParamExpr(FArgExpr).Data;
   vpd:=TVarParamData.Create(data, 0);
   stack.WriteValue(stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteResult
//
procedure TPushOperator.ExecuteResult(stack : TStack);
begin
   stack.WriteValue(stack.StackPointer + FStackAddr, FArgExpr.Eval);
end;

// ExecuteResultBoolean
//
procedure TPushOperator.ExecuteResultBoolean(stack : TStack);
begin
   stack.WriteBoolValue(stack.StackPointer + FStackAddr, FArgExpr.EvalAsBoolean);
end;

// ExecuteResultInteger
//
procedure TPushOperator.ExecuteResultInteger(stack : TStack);
begin
   stack.WriteIntValue(stack.StackPointer + FStackAddr, FArgExpr.EvalAsInteger);
end;

// ExecuteResultFloat
//
procedure TPushOperator.ExecuteResultFloat(stack : TStack);
var
   buf : Double;
begin
   FArgExpr.EvalAsFloat(buf);
   stack.WriteFloatValue(stack.StackPointer + FStackAddr, buf);
end;

// ExecuteResultString
//
procedure TPushOperator.ExecuteResultString(stack : TStack);
var
   buf : String;
begin
   FArgExpr.EvalAsString(buf);
   stack.WriteStrValue(stack.StackPointer + FStackAddr, buf);
end;

// ExecuteResultConstString
//
procedure TPushOperator.ExecuteResultConstString(stack : TStack);
begin
   stack.WriteStrValue(stack.StackPointer + FStackAddr,
                       TConstStringExpr(FArgExpr).Value);
end;

// ExecuteData
//
procedure TPushOperator.ExecuteData(stack : TStack);
begin
   stack.WriteData(TDataExpr(FArgExpr).Addr, stack.StackPointer + FStackAddr,
                   FTypeParamSym.Typ.Size, TDataExpr(FArgExpr).Data);
end;

// ExecuteLazy
//
procedure TPushOperator.ExecuteLazy(stack : TStack);
begin
   stack.WriteIntValue(stack.StackPointer + FStackAddr, Int64(FArgExpr)+(Int64(stack.BasePointer) shl 32));
end;

{ TFuncExpr }

constructor TFuncExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TFuncSymbol;
  IsInstruction : Boolean; CodeExpr: TDataExpr; IsWritable: Boolean);

   procedure CreateResultExpr;
   var
      initData: TData;
   begin
      // Initialize Result
      SetLength(initData, FTyp.Size);
      FTyp.InitData(initData, 0);
      FInitResultExpr := TConstExpr.CreateTyped(Prog, FTyp, initData);

      SetResultAddr;
   end;

begin
   inherited Create(Prog, Pos, Func);
   if IsInstruction then
      Include(FStates, fesIsInstruction);
   if IsWritable then
      Include(FStates, fesIsWritable);
   FResultAddr := -1;
   FCodeExpr := CodeExpr;

   if Assigned(FTyp) then
      CreateResultExpr;
end;

destructor TFuncExpr.Destroy;
begin
  FInitResultExpr.Free;
  FCodeExpr.Free;
  inherited;
end;

function TFuncExpr.AddArg(Arg: TNoPosExpr) : TSymbol;
begin
   if FArgs.Count<FFunc.Params.Count then begin
      Result:=FFunc.Params[FArgs.Count];
      if     (Arg is TFuncExpr)
         and (Result.Typ is TFuncSymbol) then
      Arg:=TFuncCodeExpr.Create(Prog, Pos, TFuncExpr(Arg));
   end else Result:=nil;

   FArgs.Add(Arg);
end;

function TFuncExpr.Eval : Variant;
var
   x : Integer;
   oldBasePointer : Integer;
   func : TFuncSymbol;
   scriptObj : IScriptObj;
   code : ICallable;
   stack : TStack;
begin
   try
      // Allocate memory for parameters on the stack
      stack:=FProg.Stack;
      stack.IncRecursion;
      stack.Push(FFunc.ParamSize);
      try

         // Special operations
         func := PreCall(scriptObj);

         // Push parameters
         for x := 0 to High(FPushExprs) do
           FPushExprs[x].Execute(stack);

         GetCode(func, code);
         if not Assigned(Code) then
           FProg.Msgs.AddExecutionStop(FPos, RTE_InvalidFunctionCall);

         // Switch frame
         stack.SwitchFrame(oldBasePointer);
         stack.PushBp(FProg.Level, oldBasePointer);

         if FProg.Root.IsDebugging then
            FProg.Root.Debugger.EnterFunc(FProg, Self);

         // Call function
         try
            try
               // The call itself
               code.Call(FProg, func);
            except
               on e: EScriptException do
                  raise;
            else
               FProg.Msgs.SetLastScriptError(FPos);
               raise;
            end;
         finally
            if FProg.Root.IsDebugging then
               FProg.Root.Debugger.LeaveFunc(FProg, Self);

            // Restore frame

            stack.RestoreFrame(oldBasePointer);
            stack.PopBp(FProg.Level);

            Result := PostCall(scriptObj);
         end;

      finally
         // Remove parameters from stack
         stack.Pop(FFunc.ParamSize);
         stack.DecRecursion;
      end;
   except
      FProg.Msgs.SetLastScriptError(FPos);
      raise;
   end;
end;

function TFuncExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;
end;

function TFuncExpr.PostCall(const ScriptObj: IScriptObj): Variant;
var
  sourceAddr, destAddr: Integer;
begin
  if Assigned(FInitResultExpr) then begin
    // Result.StackAddr is relative to BasePointer of the called function
    // But the frame is already restored so its relative to the stackpointer here
    sourceAddr := FProg.Stack.StackPointer + FFunc.Result.StackAddr;
    // Copy return value
    Result := FProg.Stack.ReadValue(sourceAddr);

    if FResultAddr >= 0 then
    begin
      destAddr := FProg.Stack.BasePointer + FResultAddr;
      FProg.Stack.CopyData(sourceAddr, destAddr, FFunc.Typ.Size);
    end;
  end;
end;

function TFuncExpr.GetData: TData;
begin
  Eval;
  Result := FProg.Stack.Data;
end;

function TFuncExpr.GetAddr: Integer;
begin
  Result := FProg.Stack.BasePointer + FResultAddr;
end;

procedure TFuncExpr.AddPushExprs;
var
   x: Integer;
   arg: TNoPosExpr;
   param: TParamSymbol;
   pushOperator : PPushOperator;
begin
   if Assigned(FInitResultExpr) then
      SetLength(FPushExprs, FArgs.Count+1)
   else SetLength(FPushExprs, FArgs.Count);

   for x := 0 to FArgs.Count - 1 do begin
      pushOperator:=@FPushExprs[x];
      arg := TNoPosExpr(FArgs.ExprBase[x]);
      param := TParamSymbol(FFunc.Params[x]);
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

   if Assigned(FInitResultExpr) then
      FPushExprs[FArgs.Count].InitPushData(FFunc.Result.StackAddr, FInitResultExpr, FFunc.Result);
end;

procedure TFuncExpr.Initialize;
begin
   inherited;
   if Assigned(FCodeExpr) then
      FCodeExpr.Initialize
   else if Assigned(FFunc.Executable) then
      FFunc.Executable.InitExpression(Self);
   AddPushExprs;
end;

procedure TFuncExpr.SetResultAddr(ResultAddr: Integer);
begin
  if ResultAddr = -1 then
  begin
    if FProg.Root.ProgramState = psUndefined then
      FResultAddr := FProg.GetTempAddr(FTyp.Size)
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
   Result:=(fesIsWritable in FStates);
end;

// GetCode
//
procedure TFuncExpr.GetCode(Func : TFuncSymbol; var result : ICallable);

   procedure GetCodeExpr(codeExpr : TDataExpr; var result : ICallable);
   begin
      Result:=ICallable(IUnknown(codeExpr.Eval));
   end;

begin
  if Assigned(FCodeExpr) then
      GetCodeExpr(codeExpr, result)
  else result:=ICallable(Func.Executable);
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
         expr.Prog.Msgs.AddCompilerErrorFmt(pos, CPE_AssignIncompatibleTypes,
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

function TBinaryOpExpr.Eval: Variant;
begin
  Result := FLeft.Eval + FRight.Eval;
end;

procedure TBinaryOpExpr.Initialize;
begin
  FLeft.Initialize;
  FRight.Initialize;
end;

// TypeCheckNoPos
//
procedure TBinaryOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  if (FLeft.Typ = FProg.TypInteger) and (FRight.Typ = FProg.TypFloat) then
    FLeft := TConvFloatExpr.Create(FProg, FLeft)
  else if (FLeft.Typ = FProg.TypFloat) and (FRight.Typ = FProg.TypInteger) then
    FRight := TConvFloatExpr.Create(FProg, FRight)
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

// TypeCheckNoPos
//
procedure TUnaryOpExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   // nothing by default
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
function TUnaryOpBoolExpr.Eval : Variant;
begin
   Result:=EvalAsBoolean;
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
function TUnaryOpIntExpr.Eval : Variant;
begin
   Result:=EvalAsInteger;
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
function TUnaryOpFloatExpr.Eval : Variant;
var
   dbl : Double;
begin
   EvalAsFloat(dbl);
   Result:=dbl;
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
function TUnaryOpStringExpr.Eval : Variant;
var
   str : String;
begin
   EvalAsString(str);
   Result:=str;
end;

{ TMethodStaticExpr }

constructor TMethodStaticExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  Func: TMethodSymbol; BaseExpr: TDataExpr; IsInstruction: Boolean;
  CodeExpr: TDataExpr; IsWritable: Boolean);
begin
  inherited Create(Prog, Pos, Func, IsInstruction, CodeExpr, IsWritable);
  FBaseExpr := BaseExpr;
  if not Func.IsClassMethod then
    FSelfAddr := TDataSymbol(Func.SelfSym).StackAddr;
end;

destructor TMethodStaticExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

// TypeCheckNoPos
//
procedure TMethodStaticExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   FBaseExpr.TypeCheckNoPos(aPos);
   inherited;
end;

function TMethodStaticExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
   FBaseExpr.EvalAsScriptObj(ScriptObj);
   FProg.Stack.WriteInterfaceValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);

   Result := FFunc;
end;

procedure TMethodStaticExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
end;

{ TClassMethodStaticExpr }

function TClassMethodStaticExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;
end;

{ TConstructorStaticExpr }

constructor TConstructorStaticExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
   Func: TMethodSymbol; Base: TDataExpr; IsInstruction: Boolean = True);
begin
  inherited Create(Prog, Pos, Func, Base, IsInstruction);
  if Base.Typ is TClassOfSymbol then
    FTyp := Base.Typ.Typ
  else
    FTyp := Base.Typ;
end;

function TConstructorStaticExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;

  // Create object
  ScriptObj := TScriptObj.Create(TClassSymbol(FTyp), FProg);
  ScriptObj.ExternalObject := ExternalObject;

  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);
end;

function TConstructorStaticExpr.PostCall(const ScriptObj: IScriptObj): Variant;
begin
  Assert(FResultAddr = -1);
  Result := ScriptObj;
end;

// TypeCheckNoPos
//
procedure TConstructorStaticExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
   inherited;
   if TClassSymbol(FTyp).IsAbstract then
      FProg.Msgs.AddCompilerError(FPos, RTE_InstanceOfAbstractClass);
end;

{ TMethodVirtualExpr }

constructor TMethodVirtualExpr.Create;
begin
  inherited Create(Prog, Pos, Func, Base, IsInstruction);
  FMethName := Func.Name;
end;

function TMethodVirtualExpr.FindVirtualMethod(ClassSym: TClassSymbol): TMethodSymbol;
begin
  Result := TMethodSymbol(ClassSym.Members.FindSymbol(FMethName));
  Assert(Result <> nil);

  while not TMethodSymbol(Result).IsVirtual and TMethodSymbol(Result).IsOverlap do
    Result := TMethodSymbol(Result).ParentMeth;
end;

function TMethodVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
   // Find virtual method
   ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
   if ScriptObj=nil then
      FProg.Msgs.AddExecutionStop(FPos, RTE_ObjectNotInstantiated);
   Result := FindVirtualMethod(ScriptObj.ClassSym);
   FProg.Stack.WriteValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);
end;

{ TClassMethodVirtualExpr }

function TClassMethodVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
   if ScriptObj=nil then
      AddExecutionStop(RTE_ObjectNotInstantiated);
  Result := FindVirtualMethod(ScriptObj.ClassSym);
end;

{ TClassMethodVirtualNameExpr }

function TClassMethodVirtualNameExpr.PreCall(
  var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FindVirtualMethod(FProg.Table.FindSymbol(FBaseExpr.Eval) as TClassSymbol);
end;

{ TClassMethodObjVirtualExpr }
function TClassMethodObjVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
  Result := FindVirtualMethod(ScriptObj.ClassSym);
end;

{ TClassMethodObjVirtualNameExpr }

function TClassMethodObjVirtualNameExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FindVirtualMethod(FProg.Table.FindSymbol(FBaseExpr.Eval) as TClassSymbol);
end;

{ TConstructorVirtualExpr }

constructor TConstructorVirtualExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
   Func: TMethodSymbol; Base: TDataExpr; IsInstruction: Boolean);
begin
  inherited Create(Prog, Pos, Func, Base, IsInstruction);
  FTyp := Base.Typ.Typ;
end;

function TConstructorVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
var
  className: string;
  classSym: TClassSymbol;
begin
  // Get class symbol
  className := FBaseExpr.Eval;
  classSym := TClassSymbol(FProg.Table.FindSymbol(className));
  Assert(classSym <> nil);

  if classSym.IsAbstract then

    AddExecutionStop(RTE_InstanceOfAbstractClass);

  Result := FindVirtualMethod(classSym);

  // Create object
  ScriptObj := TScriptObj.Create(classSym, FProg);
  ScriptObj.ExternalObject := ExternalObject;

  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);
end;

function TConstructorVirtualExpr.PostCall(const ScriptObj: IScriptObj): Variant;
begin
  // Return Self as Result
  Assert(FResultAddr = -1);
  Result := ScriptObj;
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
      extVDM := TExternalVarDataMaster.Create(FCaller, TExternalVarSymbol(sym));
      if sym.Typ is TClassSymbol then
         extVDM.Read(dat); // initialize 'Self'-Object
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
      vpd:=IVarParamData(IUnknown(FCaller.Stack.Data[basePointer+sym.StackAddr]));
      TInfo.SetChild(Result, Self, sym.Typ, vpd.Data, vpd.Addr);
   end;

   procedure GetDataSymbol(sym : TDataSymbol; var Result : IInfo);
   var
      basePointer : Integer;
      pin : TProgramInfo;
      stack : TStack;
   begin
      pin:=Self;
      stack:=pin.FCaller.Stack;
      if sym.Level=pin.FLevel then
         basePointer:=stack.BasePointer
      else basePointer:=stack.GetSavedBp(pin.FCaller.Level);
      if sym is TVarParamSymbol then begin
         GetVarParamVars(sym, basePointer, Result);
      end else begin
         TInfo.SetChild(Result, pin, sym.Typ, stack.Data,
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
   sym:=FTable.FindSymbol(str);

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
  sym := FTable.FindSymbol(s);

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
  typSym := FTable.FindSymbol(DataType);

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
    FLevel := 1
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
      vpd:=IVarParamData(IUnknown(FCaller.Stack.Data[stackAddr]));
      Result:=@vpd.Data[vpd.Addr];
   end;

var
   ip : TSymbolTable;
   sym : TDataSymbol;
   stack : TStack;
   stackAddr : Integer;
begin
   ip:=FuncSym.Params;
   if Cardinal(index)>=Cardinal(ip.Count) then begin
      RaiseIncorrectParameterIndex(index);
      Result:=nil;
   end else begin
      sym:=TDataSymbol(ip[index]);
      Assert(sym.InheritsFrom(TDataSymbol));
      stack:=Caller.Stack;
      if sym.Level=FLevel then
         stackAddr:=sym.StackAddr+stack.BasePointer
      else stackAddr:=sym.StackAddr+stack.GetSavedBp(FCaller.Level);
      if sym.InheritsFrom(TByRefParamSymbol) then
         Result:=GetVarParam(stackAddr)
      else Result:=@stack.Data[stackAddr];
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

{ TScriptObjectWrapper }

// wrapper to interact with an released script object
type
  TScriptObjectWrapper = class (TInterfacedObject, IScriptObj)
  private
    FScriptObj : TScriptObj;
  protected
    { IScriptObj }
    function GetClassSym: TClassSymbol;
    function GetData: TData;
    function DataOfAddr(addr : Integer) : Variant;
    function DataOfAddrAsString(addr : Integer) : String;
    function DataOfAddrAsInteger(addr : Integer) : Int64;
    procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
    procedure SetData(const Dat: TData);
    function GetExternalObject: TObject;
    procedure SetExternalObject(Value: TObject);
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

function TScriptObjectWrapper.GetData: TData;
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

procedure TScriptObjectWrapper.SetData(const Dat: TData);
begin
  FScriptObj.SetData(Dat);
end;

procedure TScriptObjectWrapper.SetExternalObject(Value: TObject);
begin
  FScriptObj.SetExternalObject(Value);
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
  prg: TdwsProgram;
begin
  Assert(Assigned(FCaller));
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
      prg := FCaller
    else
      prg := nil;
    NewScriptObj := TScriptObj.Create(ClassSym, prg);
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
type
   PIUnknown = ^IUnknown;
var
   stack : TStack;
begin
   stack:=Caller.Stack;
   stack.ReadInterfaceValue(stack.BasePointer+TDataSymbol(FuncSym.InternalParams[0]).StackAddr,
                            PIUnknown(@FScriptObj)^);
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
  if Assigned(FCaller) then
    root := FCaller.RootTable
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

{ TScriptObj }

constructor TScriptObj.Create(ClassSym: TClassSymbol; Prog: TdwsProgram);
var
   x: Integer;
   c: TClassSymbol;
begin
   FClassSym := ClassSym;

   if Prog<>nil then
      Prog.ScriptObjCreated(Self);

   SetLength(FData, ClassSym.InstanceSize);

   // Initialize fields
   c := TClassSymbol(ClassSym);
   while c <> nil do begin
      for x := 0 to c.Members.Count - 1 do
         if c.Members[x] is TFieldSymbol then
            with TFieldSymbol(c.Members[x]) do
               Typ.InitData(FData, Offset);
      c := c.Parent;
   end;

   FOnObjectDestroy := ClassSym.OnObjectDestroy;
end;

procedure TScriptObj.BeforeDestruction;
var
   iso : IScriptObj;
begin
   if Assigned(FProg) then begin
      // we are released, so never do: Self as IScriptObj
      iso:=TScriptObjectWrapper.Create(Self);
      FProg.DestroyScriptObj(iso);
      FProg.ScriptObjDestroyed(Self);
   end;
   inherited;
end;

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

function TScriptObj.GetData: TData;
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

procedure TScriptObj.SetData(const Dat: TData);
begin
  FData := Dat;
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
    FCaller := ProgramInfo.Caller;
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

function TInfo.GetData: TData;
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
    FDataMaster.Read(FData);

  SetLength(Result, FTypeSym.Size);
  CopyData(FData, FOffset, Result, 0, FTypeSym.Size);
end;

function TInfoData.GetValue: Variant;
begin
   if Assigned(FDataMaster) then
      FDataMaster.Read(FData);
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
      case varData.VType of
         varInt64 : Result:=varData.VInt64;
         varInteger : Result:=varData.VInteger;
         varSmallint : Result:=varData.VSmallInt;
         varShortInt : Result:=varData.VShortInt;
      else
         Result:=PVariant(varData)^;
      end;
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
      case varData.VType of
         varDouble : Result:=varData.VDouble;
         varSingle : Result:=varData.VSingle;
      else
         Result:=PVariant(varData)^;
      end;
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
      FDataMaster.Write(FData)
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
    FDataMaster.Write(FData);
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

  sym := TClassSymbol(FTypeSym).Members.FindSymbol(s);

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

{ TInfoClassOf }

constructor TInfoClassOf.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; const DataMaster: IDataMaster);
begin
  inherited;
  FTypeSym := FCaller.Table.FindSymbol(FData[FOffset]);
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
   status : TExecutionStatusResult;
begin
   resultData := nil;
   if not FUsesTempParams then
      InitTempParams;

   // Write the var-params as local variables to the stack.
   for x := 0 to FTempParams.Count - 1 do begin
      tp := TTempParam(FTempParams[x]);
      if tp.IsVarParam then begin
         FCaller.Stack.Push(tp.Size);
         FCaller.Stack.WriteData(0, FCaller.Stack.StackPointer-tp.Size, tp.Size, tp.Data);
      end;
   end;

   try

      // Simulate the params of the functions as local variables
      FCaller.Stack.Push(FParamSize);
      try
         // Create the TFuncExpr
         funcExpr := GetFuncExpr(FCaller, TFuncSymbol(FTypeSym), FScriptObj, FClassSym, FForceStatic);
         try
            if funcExpr is TConstructorVirtualExpr then
               TConstructorVirtualExpr(funcExpr).ExternalObject := FExternalObject
            else if funcExpr is TConstructorStaticExpr then
               TConstructorStaticExpr(funcExpr).ExternalObject := FExternalObject;

            for x := 0 to FTempParams.Count - 1 do begin
               tp := TTempParam(FTempParams[x]);
               if tp.IsVarParam then begin
                  funcExpr.AddArg(TVarExpr.Create(FCaller, tp.Typ, tp));
               end else begin
                  funcExpr.AddArg(TConstExpr.CreateTyped(FCaller, tp.Typ, tp.Data));
               end;
            end;
            funcExpr.Initialize;
            if Assigned(funcExpr.Typ) then begin
               if funcExpr.Typ.Size > 1 then begin
                  // Allocate space on the stack to store the Result value
                  FCaller.Stack.Push(funcExpr.Typ.Size);
                  try
                     // Result-space is just behind the temporary-params
                     // (Calculated relative to the basepointer of the caller!)
                     funcExpr.SetResultAddr(FCaller.Stack.StackPointer-funcExpr.Typ.Size);

                     // Execute function.
                     // Result is stored on the stack
                     resultData := funcExpr.GetData;
                     resultAddr := funcExpr.GetAddr;

                     // Copy Result
                     CopyData(resultData, resultAddr, FResult, 0, funcExpr.Typ.Size);
                  finally
                     FCaller.Stack.Pop(funcExpr.Typ.Size);
                  end;
               end else VarCopy(FResult[0], funcExpr.Eval);
               SetChild(Result, FProgramInfo, funcExpr.Typ, FResult, 0);
            end else begin
               // Execute as procedure
               status:=esrNone;
               funcExpr.EvalNoResult(status);
               Assert(status=esrNone);
               Result := nil;
            end;
         finally
            funcExpr.Free;
         end;
      finally
         FCaller.Stack.Pop(FParamSize);
      end;
   finally
      // Copy back the Result of var-parameters
      for x := FTempParams.Count - 1 downto 0 do begin
         tp := TTempParam(FTempParams[x]);
         if tp.IsVarParam then begin
            FCaller.Stack.ReadData(FCaller.Stack.Stackpointer - tp.Size, 0, tp.Size, tp.Data);
            FCaller.Stack.Pop(tp.Size);
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
  status : TExecutionStatusResult;
begin
  resultData := nil;
  funcSym := TFuncSymbol(FTypeSym);

  if Length(Params) <> funcSym.Params.Count then
    raise Exception.CreateFmt(RTE_InvalidNumberOfParams, [Length(Params),
      funcSym.Params.Count, FTypeSym.Caption]);

  // Create the TFuncExpr
  funcExpr := GetFuncExpr(FCaller, funcSym, FScriptObj, FClassSym, FForceStatic);

  if funcExpr is TConstructorVirtualExpr then
    TConstructorVirtualExpr(funcExpr).ExternalObject := FExternalObject
  else if funcExpr is TConstructorStaticExpr then
    TConstructorStaticExpr(funcExpr).ExternalObject := FExternalObject;

  try
    // Add arguments to the expression
    for x := Low(Params) to High(Params) do
    begin
      dataSym := TDataSymbol(FParams[x]);

      if dataSym.Size > 1 then
        raise Exception.CreateFmt(RTE_UseParameter, [dataSym.Caption,
          funcSym.Caption]);

      funcExpr.AddArg(TConstExpr.CreateTyped(FCaller, dataSym.Typ, Params[x]));
    end;
    funcExpr.Initialize;
    if Assigned(funcExpr.Typ) then
    begin
      if funcExpr.Typ.Size > 1 then
      begin
        // Allocate space on the stack to store the Result value
        FCaller.Stack.Push(funcExpr.Typ.Size);
        try
          // Result-space is just behind the temporary-params
          funcExpr.SetResultAddr(FParamSize);

          // Execute function.
          // Result is stored on the stack
          resultData := funcExpr.GetData;
          resultAddr := funcExpr.GetAddr;

          // Copy Result
          for x := 0 to funcExpr.Typ.Size - 1 do
            FResult[x] := resultData[resultAddr + x];
        finally
          FCaller.Stack.Pop(funcExpr.Typ.Size);
        end;
      end
      else
        FResult[0] := funcExpr.Eval;

      SetChild(Result, FProgramInfo, funcExpr.Typ, FResult, 0);
    end
    else begin
      status:=esrNone;
      funcExpr.EvalNoResult(status);
      Assert(status=esrNone);
    end;
  finally
    funcExpr.Free;
  end;
end;

function TInfoFunc.GetParameter(const s: string): IInfo;
var
  tp: TTempParam;
begin
  if not FUsesTempParams then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s));

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
      tp.StackAddr := FTempParamSize + FCaller.Stack.FrameSize;
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
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded,[x]);

    if Indices[x] < arrTyp.LowBound then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded,[x]);

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
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, h - l + 1)
  else if SameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, l)
  else if SameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, h)
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
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, l)
  else if SameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, 0)
  else if SameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, l - 1)
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

function TConnectorCallExpr.Eval: Variant;
var
  dataSource, dataDest: TData;
  addrSource: Integer;
  x: Integer;
  arg : TNoPosExpr;
begin
  if FProg.Root.IsDebugging then
    FProg.Debugger.EnterFunc(FProg, Self);

  // Call function
  try
    dataSource := nil;
    dataDest := nil;

    for x := 0 to Length(FConnectorArgs) - 1 do
    begin
      arg:=TNoPosExpr(FArgs.List[x]);
      if FConnectorParams[x].TypSym.Size = 1 then
        VarCopy(FConnectorArgs[x][0], arg.Eval)
      else
      begin
        dataSource := TDataExpr(arg).Data;
        addrSource := TDataExpr(arg).Addr;
        dataDest := FConnectorArgs[x];
        CopyData(dataSource, addrSource, dataDest, 0, FConnectorParams[x].TypSym.Size);
      end;
    end;

    try
      // The call itself
      if FBaseExpr is TDataExpr then
        FResultData := FConnectorCall.Call(TDataExpr(FBaseExpr).Data[TDataExpr(FBaseExpr).Addr], FConnectorArgs)
      else
        FResultData := FConnectorCall.Call(FBaseExpr.Eval, FConnectorArgs);
    except
      on e: EScriptException do
        raise;
      on e: Exception do begin
        FProg.Msgs.SetLastScriptError(FPos);
        raise;
      end;
    end;

    for x := 0 to Length(FConnectorArgs) - 1 do
      if FConnectorParams[x].IsVarParam then
        TDataExpr(FArgs.List[x]).AssignData(FConnectorArgs[x], 0);

  finally
    if FProg.Root.IsDebugging then
      FProg.Debugger.LeaveFunc(FProg, Self);
  end;

  if Assigned(FResultData) then
    Result := FResultData[0]
  else
    VarClear(Result);
end;

function TConnectorCallExpr.GetData: TData;
begin
  Eval;
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

function TConnectorReadExpr.Eval: Variant;
begin
  try
    if FBaseExpr is TDataExpr then
      FResultData := FConnectorMember.Read(TDataExpr(FBaseExpr).Data[TDataExpr(FBaseExpr).Addr])
    else
      FResultData := FConnectorMember.Read(FBaseExpr.Eval);
    Result := FResultData[0];
  except
    FProg.Msgs.SetLastScriptError(FPos);
    raise;
  end;
end;

function TConnectorReadExpr.GetData: TData;
begin
  Eval;
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
    FProg.Msgs.AddCompilerError(FPos, CPE_ConnectorTypeMismatch);
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

procedure TConnectorWriteExpr.EvalNoResult(var status : TExecutionStatusResult);
var
  dat: TData;
  tmp: Variant;
  Base: pVariant;
begin
  if FBaseExpr is TDataExpr then
    Base := @TDataExpr(FBaseExpr).Data[TDataExpr(FBaseExpr).Addr]
  else begin
    tmp := FBaseExpr.Eval;
    Base := @tmp;
  end;

  if FValueExpr is TDataExpr then
    dat := TDataExpr(FValueExpr).GetData
  else
  begin
    SetLength(dat, 1);
    dat[0] := FValueExpr.Eval;
  end;

  try
    FConnectorMember.Write(Base^, dat);
  except
    FProg.Msgs.SetLastScriptError(FPos);
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
    TConnectorMemberDataMaster.Create(FCaller, FTypeSym, s, FData[FOffset]));
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
  status : TExecutionStatusResult;
begin
  expr := TConnectorCallExpr.Create(FCaller, cNullPos, FName,
    TConstExpr.Create(FCaller, FCaller.TypVariant, FData[FOffset]));

  try
    for x := 0 to Length(Params) - 1 do
      expr.AddArg(TConstExpr.Create(FCaller, FCaller.TypVariant, Params[x]));

    if expr.AssignConnectorSym(FConnectorType) then
    begin
      if Assigned(expr.Typ) then
      begin
        SetLength(resultData, 1);
        resultData[0] := expr.Eval;
        TInfo.SetChild(Result, FProgramInfo, expr.Typ, resultData, 0);
      end
      else
      begin
        resultData := nil;
        status:=esrNone;
        expr.EvalNoResult(status);
        Assert(status=esrNone);
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

constructor TDataMaster.Create(Caller: TdwsProgram; Sym: TSymbol);
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

procedure TDataMaster.Read(const Data: TData);
begin
end;

procedure TDataMaster.Write(const Data: TData);
begin
end;

{ TExternalVarDataMaster }

procedure TExternalVarDataMaster.Read(const Data: TData);
var
  x: Integer;
  resultData: TData;
  resultAddr: Integer;
  funcExpr: TFuncExpr;
begin
  resultData := nil;
  // Read an external var
  funcExpr := GetFuncExpr(FCaller, TExternalVarSymbol(FSym).ReadFunc, nil, nil);
  try
    funcExpr.Initialize;
    if funcExpr.Typ.Size > 1 then // !! > 1 untested !!
    begin
      funcExpr.SetResultAddr(FCaller.Stack.FrameSize);
      // Allocate space on the stack to store the Result value
      FCaller.Stack.Push(funcExpr.Typ.Size);
      try
        // Execute function.
        resultData := funcExpr.GetData;
        resultAddr := funcExpr.GetAddr;
        // Copy Result
        for x := 0 to funcExpr.Typ.Size - 1 do
          Data[x] := resultData[resultAddr + x];
      finally
        FCaller.Stack.Pop(funcExpr.Typ.Size);
      end;
    end
    else
      VarCopy(Data[0],funcExpr.Eval);
  finally
    funcExpr.Free;
  end;
end;

procedure TExternalVarDataMaster.Write(const Data: TData);
var
  funcExpr: TFuncExpr;
  status : TExecutionStatusResult;
begin
  funcExpr := GetFuncExpr(FCaller, TExternalVarSymbol(FSym).WriteFunc, nil, nil);
  try
    funcExpr.AddArg(TConstExpr.CreateTyped(FCaller, FSym.Typ, Data));
    funcExpr.AddPushExprs;
    status:=esrNone;
    funcExpr.EvalNoResult(status);
    Assert(status=esrNone);
  finally
    funcExpr.Free;
  end;
end;

{ TConnectorMemberDataMaster }

constructor TConnectorMemberDataMaster.Create(Caller: TdwsProgram;
  Sym: TSymbol; BaseValue: Variant; const Name: string);
begin
  inherited Create(Caller, Sym);
  FName := Name;
end;

procedure TConnectorMemberDataMaster.Read(const Data: TData);
var
  readExpr: TConnectorReadExpr;
  dataSource: TData;
begin
  dataSource := nil;
  readExpr := TConnectorReadExpr.Create(FCaller, cNullPos, FName,
    TConstExpr.Create(FCaller, FCaller.TypVariant, FBaseValue));

  if readExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    dataSource := readExpr.GetData;
    CopyData(dataSource, 0, Data, 0, readExpr.Typ.Size);
  end
  else
    raise Exception.Create(RTE_ConnectorReadError);
end;

procedure TConnectorMemberDataMaster.Write(const Data: TData);
var
  writeExpr: TConnectorWriteExpr;
  status : TExecutionStatusResult;
begin
  writeExpr := TConnectorWriteExpr.Create(FCaller, cNullPos, FName,
    TConstExpr.Create(FCaller, FCaller.TypVariant, FBaseValue),
    TConstExpr.Create(FCaller, FCaller.TypVariant, Data));

  if writeExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    status:=esrNone;
    writeExpr.EvalNoResult(status);
    Assert(status=esrNone);
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

function TInfoConst.GetData: TData;
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

{ TFuncCodeExpr }

procedure TFuncCodeExpr.AssignDataExpr(Right: TDataExpr);
begin
  Assert(Right is TFuncCodeExpr);
  Assert(FFuncExpr is TMethodStaticExpr);
  Assert(TFuncCodeExpr(Right).FuncExpr is TMethodStaticExpr);
  FFuncExpr.CodeExpr.AssignValue(TFuncCodeExpr(Right).Eval);
  TMethodStaticExpr(FFuncExpr).BaseExpr.AssignValue(
    TMethodStaticExpr(TFuncCodeExpr(Right).FuncExpr).BaseExpr.Eval);
end;

constructor TFuncCodeExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  FuncExpr: TFuncExpr);
begin
  inherited Create(Prog,Pos,FuncExpr.FuncSym);
  FFuncExpr := FuncExpr;
end;

destructor TFuncCodeExpr.Destroy;
begin
  FFuncExpr.Free;
  inherited;
end;

// TypeCheckNoPos
//
procedure TFuncCodeExpr.TypeCheckNoPos(const aPos : TScriptPos);
begin
  if FFuncExpr.FArgs.Count > 0 then
    FProg.FMsgs.AddCompilerError(FPos, CPE_NoArgumentsExpected);
end;

function TFuncCodeExpr.Eval: Variant;
var
   callable : ICallable;
begin
   FFuncExpr.GetCode(FFuncExpr.FuncSym, callable);
   Result := callable;
end;

function TFuncCodeExpr.GetAddr: Integer;
begin
  Result := FFuncExpr.CodeExpr.Addr;
end;

function TFuncCodeExpr.GetData: TData;
begin
  Result := FFuncExpr.CodeExpr.Data;
end;

{ TMethodObjExpr }

constructor TMethodObjExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  BaseExpr: TDataExpr);
begin
  Assert(BaseExpr.Typ is TMethodSymbol);
  inherited Create(Prog,Pos,TMethodSymbol(BaseExpr.Typ).ClassSymbol);
  FBaseExpr := BaseExpr;
end;

function TMethodObjExpr.GetAddr: Integer;
begin
  Result := FBaseExpr.Addr + 1;
end;

function TMethodObjExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

{ TConstructorStaticObjExpr }

constructor TConstructorStaticObjExpr.Create(Prog: TdwsProgram;
  const Pos: TScriptPos; Func: TMethodSymbol; BaseExpr: TDataExpr;
  IsInstruction: Boolean; CodeExpr: TDataExpr; IsWritable: Boolean);
begin
  inherited Create(Prog,Pos,Func,BaseExpr,IsInstruction,CodeExpr,IsWritable);
  Typ := BaseExpr.Typ;
end;

function TConstructorStaticObjExpr.PostCall(const ScriptObj: IScriptObj): Variant;
begin
  result := ScriptObj;
end;

{ TConstructorVirtualObjExpr }

constructor TConstructorVirtualObjExpr.Create(Prog: TdwsProgram;
  const Pos: TScriptPos; Func: TMethodSymbol; Base: TDataExpr; IsInstruction: Boolean);
begin
  inherited Create(Prog,Pos,Func,Base,IsInstruction);
  Typ := Base.Typ;
end;

function TConstructorVirtualObjExpr.PostCall(const ScriptObj: IScriptObj): Variant;
begin
  result := ScriptObj;
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

  tp := TTempParam(FTempParams.FindSymbol(s));

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

function TInfoProperty.GetData: TData;
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
procedure TNoResultWrapperExpr.EvalNoResult(var status : TExecutionStatusResult);
begin
   Expr.EvalNoResult(status);
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

end.

