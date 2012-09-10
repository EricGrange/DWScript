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
unit dwsExprs;

{$I dws.inc}

interface

uses Classes, Variants, SysUtils, TypInfo, dwsSymbols, dwsErrors, dwsUtils,
   dwsStrings, dwsStack, SyncObjs, dwsFileSystem, dwsTokenizer, dwsUnitSymbols,
   dwsJSON, dwsXPlatform;

type
   TRelOps = (roEqual, roUnEqual, roLess, roLessEqual, roMore, roMoreEqual);

   TRefKind = (rkObjRef, rkClassOfRef);

   TTypedExpr = class;
   TNoResultExpr = class;
   TBlockInitExpr = class;
   TBlockFinalExpr = class;
   TTypedExprList = class;
   TdwsProgram = class;
   TdwsMainProgram = class;
   IdwsProgram = interface;
   TdwsProgramExecution = class;
   IdwsProgramExecution = interface;
   TSymbolPositionList = class;
   TFuncExprBase = class;
   TScriptObj = class;
   TSourceConditions = class;
   TSourcePreConditions = class;
   TSourcePostConditions = class;
   TBlockExprBase = class;

   TVariantDynArray = array of Variant;

   TNoResultExprList = array[0..MaxInt shr 4] of TNoResultExpr;
   PNoResultExprList = ^TNoResultExprList;
   PNoResultExpr = ^TNoResultExpr;

   TScriptSourceType = (stMain, stUnit, stInclude, stRecompile);

   // A specific ScriptSource entry. The text of the script contained in that unit.
   TScriptSourceItem = class (TRefCountedObject)
      private
         FNameReference : String;
         FSourceFile : TSourceFile;
         FSourceType : TScriptSourceType;

      public
         constructor Create(const ANameReference: String; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
         destructor Destroy; override;

         property NameReference : String read FNameReference write FNameReference;
         property SourceFile : TSourceFile read FSourceFile;
         property SourceType : TScriptSourceType read FSourceType;
   end;

   // Manage a list of all the different Script Texts (files) used in the program.
   TScriptSourceList = class
      private
         FSourceList : TTightList;
         FMainScript : TScriptSourceItem;

      protected
         function GetSourceItem(index : Integer) : TScriptSourceItem; inline;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;
         function Add(const nameReference, code: String; sourceType: TScriptSourceType) : TSourceFile;

         function FindScriptSourceItem(const SourceFileName: String): TScriptSourceItem; overload;

         function IndexOf(const SourceFileName: String): Integer; overload;

         property Count : Integer read FSourceList.FCount;

         property Items[Index: Integer] : TScriptSourceItem read GetSourceItem; default;
         property MainScript: TScriptSourceItem read FMainScript;
   end;

   { Describe how the symbol at the position is being used. suReference would be
     a typical usage of the symbol.
     suImplicit indicates that the symbol was only implicitly present }
   TSymbolUsage = (suForward, suDeclaration, suImplementation, suReference,
                   suRead, suWrite, suImplicit);
   TSymbolUsages = set of TSymbolUsage;

   // Records a symbol's position in source and usage at that position
   //
   TSymbolPositionRec = record
      private
         FScriptPos : TScriptPos;     // location of symbol instance in script
         FSymUsages : TSymbolUsages;  // how symbol is used at this location (mutiple uses possible, Functions are Delcared/Implemented at same spot)

      public
         property ScriptPos : TScriptPos read FScriptPos;
         property SymbolUsages : TSymbolUsages read FSymUsages write FSymUsages;
   end;
   TSymbolPosition = ^TSymbolPositionRec;

   {Re-list every symbol (pointer to it) and every position it is in in the script }
   TSymbolPositionList = class (TRefCountedObject)
      private
         FSymbol : TSymbol;                           // pointer to the symbol
         FPosList : TSimpleList<TSymbolPosition>;  // list of positions where symbol is declared and used

      protected
         function GetPosition(index : Integer) : TSymbolPosition; inline;

         // Used by TSymbolDictionary. Not meaningful to make public (symbol is known).
         function FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : String) : TSymbol; overload;

      public
         constructor Create(ASymbol: TSymbol);
         destructor Destroy; override;

         procedure Add(const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
         procedure Delete(index : Integer);
         procedure Clear;

         function FindUsage(const symbolUse : TSymbolUsage) : TSymbolPosition;
         function IndexOfPosition(const scriptPos : TScriptPos) : Integer;
         procedure RemoveInRange(const startPos, endPos : TScriptPos);

         property Items[index : Integer] : TSymbolPosition read GetPosition; default;
         function Count : Integer; inline;

         property Symbol: TSymbol read FSymbol;
   end;

   TSymbolPositionListList = class(TSortedList<TSymbolPositionList>)
      protected
         function Compare(const item1, item2 : TSymbolPositionList) : Integer; override;
   end;

   TdwsSymbolDictionaryProc = procedure (sym : TSymbol) of object;

   { List all symbols in the script. Each symbol list contains a list of the
     positions where it was used. }
   TdwsSymbolDictionary = class
      protected
         FSymbolList : TSymbolPositionListList;
         FSearchSymbolPositionList : TSymbolPositionList;

         function GetList(Index: Integer): TSymbolPositionList; inline;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Clear;  // clear the lists
         procedure AddSymbol(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);

         // remove all references to the symbol
         procedure Remove(sym : TSymbol);
         procedure RemoveInRange(const startPos, endPos : TScriptPos);
         procedure EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc);

         procedure ReplaceSymbolAt(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
         procedure ChangeUsageAt(const scriptPos : TScriptPos; const addUsages, removeUsages : TSymbolUsages);

         function FindSymbolAtPosition(aCol, aLine: Integer; const sourceFile : String): TSymbol; overload;
         function FindSymbolPosList(sym: TSymbol): TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosList(const symName: String): TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosListOfType(const symName: String; symbolType: TSymbolClass): TSymbolPositionList; // return list of symbol given the desired type
         function FindSymbolUsage(symbol: TSymbol; symbolUse: TSymbolUsage): TSymbolPosition; overload;
         function FindSymbolUsage(const symName: String; symbolUse: TSymbolUsage): TSymbolPosition; overload;
         function FindSymbolUsageOfType(const symName: String; symbolType: TSymbolClass; symbolUse: TSymbolUsage): TSymbolPosition;
         function FindSymbolByUsageAtLine(const scriptPos : TScriptPos; symbolUse: TSymbolUsage) : TSymbol;

         function Count : Integer; inline;
         property Items[Index: Integer] : TSymbolPositionList read GetList; default;
   end;

   TdwsSourceContext = class;
   TdwsSourceContextCallBack = procedure (context : TdwsSourceContext) of object;

   // Context within the script. (A block of code) Can be nested
   //
   TdwsSourceContext = class (TRefCountedObject)
      private
         FParentContext : TdwsSourceContext;
         FParentSymbol : TSymbol;     // a parent symbol would be a procedure/method, etc.
         FSubContexts : TTightList;   // contexts that are inside of this one
         FEndPos : TScriptPos;
         FStartPos : TScriptPos;
         FData : Pointer;             // pointer to some data element (for users)
         FLocalTable : TSymbolTable;  // symbol table associated with the context (begin..end blocks, TProcedures, etc)
         FToken : TTokenType;         // token associated with opening the context

      protected
         function GetSubContext(index : Integer) : TdwsSourceContext; inline;

      public
         constructor Create(aParent : TdwsSourceContext; const aStartPos : TScriptPos;
                            aParentSymbol : TSymbol; aToken : TTokenType);
         destructor Destroy; override;

         function IsPositionInContext(aCol, aLine : Integer; const sourceName : String) : Boolean;
         function HasParentSymbolOfClass(SymbolType: TSymbolClass; SearchParents: Boolean): Boolean;

         function FindContext(parentSymbol : TSymbol) : TdwsSourceContext;
         function FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
         procedure EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);

         procedure WriteToJSON(writer : TdwsJSONWriter);

         property Parent : TdwsSourceContext read FParentContext;
         property ParentSym : TSymbol read FParentSymbol write FParentSymbol;
         property Token : TTokenType read FToken write FToken;

         property SubContexts : TTightList read FSubContexts;
         property SubContext[index : Integer] : TdwsSourceContext read GetSubContext;
         property Count : Integer read FSubContexts.FCount;

         property StartPos : TScriptPos read FStartPos;
         property EndPos : TScriptPos read FEndPos;
         property Data : Pointer read FData write FData;
         property LocalTable : TSymbolTable read FLocalTable write FLocalTable;
   end;

   // Map the various script contexts. (Code blocks)
   TdwsSourceContextMap = class
      private
         FScriptContexts : TTightList; // list of top-level contexts
         FCurrentContext : TdwsSourceContext;   // current context (used when adding and leaving)

      protected
         function GetContext(index : Integer) : TdwsSourceContext; inline;

      public
         destructor Destroy; override;

         { Push a context on to the stack - procedures have a symbol context.
         Standard Begin..end blocks do not have a ParentSymbol. }
         procedure OpenContext(const startPos : TScriptPos; parentSymbol : TSymbol; token : TTokenType);
         { Pop a context off the stack }
         procedure CloseContext(const aEndPos : TScriptPos; onlyIfTokenType : TTokenType = ttNone);
         { Pops and close all opened contexts in the stack }
         procedure CloseAllContexts(const aEndPos : TScriptPos);

         { Suspends the current context and goes back to top level }
         function SuspendContext : TdwsSourceContext;
         { Sets specified context as current context }
         procedure ResumeContext(aContext : TdwsSourceContext);

         // return the first context group based on its parent
         function FindContext(AParentSymbol : TSymbol) : TdwsSourceContext; overload;
         function FindContext(aCol, aLine : Integer; sourceFile : TSourceFile) : TdwsSourceContext; overload;
         function FindContext(aCol, aLine : Integer; const sourceName : String) : TdwsSourceContext; overload;
         function FindContext(const ScriptPos : TScriptPos) : TdwsSourceContext; overload;
         function FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
         procedure EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);

         procedure WriteToJSON(writer : TdwsJSONWriter);

         property Contexts : TTightList read FScriptContexts;
         property Context[index : Integer] : TdwsSourceContext read GetContext;
         property Count : Integer read FScriptContexts.FCount;
         property Current : TdwsSourceContext read FCurrentContext;
   end;

   // Symbol attributes information
   TdwsSymbolAttribute = class (TRefCountedObject)
      private
         FSymbol : TSymbol;
         FScriptPos : TScriptPos;
         FAttributeConstructor : TFuncExprBase;

      protected

      public
         constructor Create(const aScriptPos : TScriptPos;
                            aConstructor : TFuncExprBase);
         destructor Destroy; override;

         property Symbol : TSymbol read FSymbol write FSymbol;
         property ScriptPos : TScriptPos read FScriptPos;
         property AttributeConstructor : TFuncExprBase read FAttributeConstructor;
   end;

   TdwsSymbolAttributeArray = array of TdwsSymbolAttribute;

   // Holds all symbol attributes
   TdwsSymbolAttributes = class (TObjectList<TdwsSymbolAttribute>)
      private

      protected

      public
         function AttributesFor(aSymbol : TSymbol) : TdwsSymbolAttributeArray;
   end;

   TProgramEvent = procedure (Prog: TdwsProgram) of object;

   TdwsResultType = class;

   TdwsResult = class
      private
         FResultType : TdwsResultType;

      protected
         property ResultType : TdwsResultType read FResultType;

      public
         constructor Create(resultType : TdwsResultType); virtual;

         procedure AddString(const str : String); virtual; abstract;
         procedure Clear; virtual; abstract;
   end;

   TdwsDefaultResult = class(TdwsResult)
      private
         FTextBuilder : TWriteOnlyBlockStream;
         function GetText : String; inline;

      public
         constructor Create(resultType : TdwsResultType); override;
         destructor Destroy; override;

         procedure AddString(const str : String); override;
         procedure Clear; override;
         function ToString : String; override;

         property Text: String read GetText;
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

   TdwsDefaultResultType = class(TdwsResultType)
      public
         procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
         function CreateProgResult: TdwsResult; override;
   end;

   TdwsGuardedExecution = class (TRefCountedObject)
      Exec : IdwsProgramExecution;
      TimeOutAt : TDateTime;
   end;

   TdwsGuardedExecutionList = class(TSortedList<TdwsGuardedExecution>)
      protected
         function Compare(const item1, item2 : TdwsGuardedExecution) : Integer; override;
   end;

   // TdwsGuardianThread
   //
   // Stops the script after given time (Timeout)
   TdwsGuardianThread = class(TThread)
      private
         FEvent : TEvent;
         FExecutions : TdwsGuardedExecutionList;
         FExecutionsLock : TFixedCriticalSection;

      protected
         procedure Execute; override;

         class var vThreadLock : Integer;
         class var vThread : TdwsGuardianThread;

      public
         constructor Create;
         destructor Destroy; override;

         class procedure Initialize; static;
         class procedure Finalize; static;

         class procedure GuardExecution(const exec : IdwsProgramExecution; aMilliSecToLive : Integer); static;
         class procedure ForgetExecution(const exec : IdwsProgramExecution); static;
   end;

   // Attached and owned by its program execution
   IdwsEnvironment = interface (IGetSelf)
      ['{CCAA438D-76F4-49C2-A3A2-82445BC2976A}']
   end;

   IdwsLocalizer = interface (IGetSelf)
      ['{2AFDC297-FF85-43F5-9913-45DE5C1330AB}']
      procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : String);
      procedure LocalizeString(const aString : String; var Result : String);
   end;

   TProgramInfo = class;

   IdwsProgramExecution = interface (IdwsExecution)
      ['{D0603CA6-40E3-4CBA-9C75-BD87C7A84650}']
      function GetInfo : TProgramInfo;
      function GetResult : TdwsResult;
      function GetObjectCount : Integer;
      function GetProg : IdwsProgram;
      function GetEnvironment : IdwsEnvironment;
      procedure SetEnvironment(const env : IdwsEnvironment);
      function GetLocalizer : IdwsLocalizer;
      procedure SetLocalizer(const loc : IdwsLocalizer);

      procedure Execute(aTimeoutMilliSeconds : Integer = 0); overload;
      procedure ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0); overload;
      procedure ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0); overload;

      procedure BeginProgram;
      procedure RunProgram(aTimeoutMilliSeconds : Integer);
      procedure Stop;
      procedure EndProgram;

      property Prog : IdwsProgram read GetProg;
      property Info : TProgramInfo read GetInfo;
      property Result : TdwsResult read GetResult;
      property ObjectCount : Integer read GetObjectCount;
      property Environment : IdwsEnvironment read GetEnvironment write SetEnvironment;
      property Localizer : IdwsLocalizer read GetLocalizer write SetLocalizer;
   end;

   IdwsProgram = interface
      ['{AD513983-F033-44AF-9F2B-9CFFF94B9BB3}']
      function GetMsgs : TdwsMessageList;
      function GetConditionalDefines : IAutoStrings;
      function GetLineCount : Integer;
      function GetTimeStamp : TDateTime;
      function GetCompileDuration : TDateTime;
      function GetTable : TSymbolTable;
      function GetTimeoutMilliseconds : Integer;
      procedure SetTimeoutMilliseconds(const val : Integer);
      function GetDefaultUserObject : TObject;
      procedure SetDefaultUserObject(const val : TObject);
      function GetSymbolDictionary : TdwsSymbolDictionary;
      function GetSourceContextMap : TdwsSourceContextMap;
      function GetSourceList : TScriptSourceList;
      function GetUnitMains : TUnitMainSymbols;
      function GetProgramObject : TdwsMainProgram;

      function CreateNewExecution : IdwsProgramExecution;
      function BeginNewExecution : IdwsProgramExecution;
      function Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
      function ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;
      function ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;

      procedure DropMapAndDictionary;

      property Table : TSymbolTable read GetTable;
      property Msgs : TdwsMessageList read GetMsgs;
      property ConditionalDefines : IAutoStrings read GetConditionalDefines;
      property TimeoutMilliseconds : Integer read GetTimeoutMilliseconds write SetTimeoutMilliseconds;
      property DefaultUserObject : TObject read GetDefaultUserObject write SetDefaultUserObject;

      property SymbolDictionary : TdwsSymbolDictionary read GetSymbolDictionary;
      property SourceContextMap : TdwsSourceContextMap read GetSourceContextMap;
      property SourceList : TScriptSourceList read GetSourceList;
      property UnitMains : TUnitMainSymbols read GetUnitMains;
      property ProgramObject : TdwsMainProgram read GetProgramObject;
      property LineCount : Integer read GetLineCount;
      property TimeStamp : TDateTime read GetTimeStamp;
      property CompileDuration : TDateTime read GetCompileDuration;
   end;

   // holds execution context for a script
   TdwsProgramExecution = class (TdwsExecution, IdwsProgramExecution)
      private
         FProg : TdwsMainProgram;
         FCurrentProg : TdwsProgram;

         FFirstObject, FLastObject : TScriptObj;
         FObjectCount : Integer;
         FProgramInfo : TProgramInfo;
         FProgInfoPool : TProgramInfo;

         FParameters : TData;
         FResult : TdwsResult;
         FFileSystem : IdwsFileSystem;
         FEnvironment : IdwsEnvironment;
         FLocalizer : IdwsLocalizer;
         FRTTIRawAttributes : IScriptObj;

         FMsgs : TdwsRuntimeMessageList;

      protected
         procedure ReleaseObjects;

         procedure ScriptObjCreated(scriptObj: TScriptObj);
         procedure ScriptObjDestroyed(scriptObj: TScriptObj);
         procedure DestroyScriptObj(const scriptObj: IScriptObj);

         function GetMsgs : TdwsRuntimeMessageList; override;
         function GetEnvironment : IdwsEnvironment;
         procedure SetEnvironment(const val : IdwsEnvironment);

         // for interface only, script exprs use direct properties
         function GetProg : IdwsProgram;
         function GetInfo : TProgramInfo;
         function GetResult : TdwsResult;
         function GetObjectCount : Integer;
         function GetLocalizer : IdwsLocalizer;
         procedure SetLocalizer(const val : IdwsLocalizer);

         procedure EnterRecursion(caller : TExprBase);
         procedure LeaveRecursion;
         procedure RaiseMaxRecursionReached;
         procedure SetCurrentProg(const val : TdwsProgram); inline;

      public
         constructor Create(aProgram : TdwsMainProgram; const stackParams : TStackParameters);
         destructor Destroy; override;

         procedure Execute(aTimeoutMilliSeconds : Integer = 0); overload;
         procedure ExecuteParam(const Params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0); overload;
         procedure ExecuteParam(const Params : OleVariant; aTimeoutMilliSeconds : Integer = 0); overload;

         procedure BeginProgram;
         procedure RunProgram(aTimeoutMilliSeconds : Integer);
         procedure Stop;
         procedure EndProgram;

         function CallStackDepth : Integer; override;
         function GetCallStack : TdwsExprLocationArray; override;
         function CallStackLastExpr : TExprBase;

         class function CallStackToString(const callStack : TdwsExprLocationArray) : String; static;
         procedure RaiseAssertionFailed(const msg : String; const scriptPos : TScriptPos);
         procedure RaiseAssertionFailedFmt(const fmt : String; const args : array of const; const scriptPos : TScriptPos);

         function AcquireProgramInfo(funcSym : TFuncSymbol) : TProgramInfo;
         procedure ReleaseProgramInfo(info : TProgramInfo);

         procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : String); override;
         procedure LocalizeString(const aString : String; var Result : String); override;

         property Prog : TdwsMainProgram read FProg;
         property CurrentProg : TdwsProgram read FCurrentProg write SetCurrentProg;
         property ProgramInfo : TProgramInfo read FProgramInfo;

         property Parameters : TData read FParameters;
         property Result : TdwsResult read FResult;
         property FileSystem : IdwsFileSystem read FFileSystem;
         property Environment : IdwsEnvironment read GetEnvironment write SetEnvironment;
         property Localizer : IdwsLocalizer read FLocalizer write FLocalizer;
         property RTTIRawAttributes : IScriptObj read FRTTIRawAttributes write FRTTIRawAttributes;

         property ObjectCount : Integer read FObjectCount;
   end;

   TdwsProgramBaseTypes = record
      FTypBoolean : TTypeSymbol;
      FTypFloat : TTypeSymbol;
      FTypInteger : TTypeSymbol;
      FTypNil : TNilSymbol;
      FTypObject : TClassSymbol;
      FTypString : TTypeSymbol;
      FTypVariant : TTypeSymbol;
      FTypException : TClassSymbol;
      FTypInterface : TInterfaceSymbol;
   end;

   // A script executable program
   TdwsProgram = class (TInterfacedSelfObject)
      private
         FExpr : TNoResultExpr;
         FInitExpr : TBlockInitExpr;
         FAddrGenerator : TAddrGeneratorRec;
         FCompileMsgs : TdwsCompileMessageList;
         FParent : TdwsProgram;
         FRoot : TdwsMainProgram;
         FRootTable : TProgramSymbolTable;
         FTable : TSymbolTable;
         FUnitMains : TUnitMainSymbols;
         FBaseTypes : TdwsProgramBaseTypes;
         FSubTables : TTightList;

      protected
         function GetLevel : Integer; inline;
         function GetUnitMains : TUnitMainSymbols;
         function GetAddrGeneratorDataSize : Integer; inline;

      public
         constructor Create(const systemTable : ISystemSymbolTable);
         destructor Destroy; override;

         function GetGlobalAddr(DataSize: Integer): Integer;
         function GetTempAddr(DataSize: Integer = -1): Integer;

         procedure ResetExprs;

         procedure EnterSubTable(subTable : TSymbolTable);
         procedure LeaveSubTable;
         function  SubTableDepth : Integer;
         function  SubTable(depth : Integer) : TSymbolTable;

         function ContextMethodSymbol : TMethodSymbol;

         property Expr : TNoResultExpr read FExpr write FExpr;
         property InitExpr : TBlockInitExpr read FInitExpr;
         property Level : Integer read GetLevel;
         property CompileMsgs : TdwsCompileMessageList read FCompileMsgs write FCompileMsgs;
         property Parent : TdwsProgram read FParent;
         property Root : TdwsMainProgram read FRoot write FRoot;
         property DataSize : Integer read GetAddrGeneratorDataSize;

         property RootTable : TProgramSymbolTable read FRootTable;
         property UnitMains : TUnitMainSymbols read FUnitMains;
         property Table : TSymbolTable read FTable;

         property TypBoolean: TTypeSymbol read FBaseTypes.FTypBoolean;
         property TypFloat: TTypeSymbol read FBaseTypes.FTypFloat;
         property TypInteger: TTypeSymbol read FBaseTypes.FTypInteger;
         property TypNil: TNilSymbol read FBaseTypes.FTypNil;
         property TypObject: TClassSymbol read FBaseTypes.FTypObject;
         property TypString: TTypeSymbol read FBaseTypes.FTypString;
         property TypVariant: TTypeSymbol read FBaseTypes.FTypVariant;
         property TypException: TClassSymbol read FBaseTypes.FTypException;
         property TypInterface : TInterfaceSymbol read FBaseTypes.FTypInterface;
   end;

   // A script main executable program
   TdwsMainProgram = class (TdwsProgram, IdwsProgram)
      private
         FFinalExpr : TBlockFinalExpr;

         FUnifiedConstList : TSortedExprBaseList;
         FResourceStringList : TResourceStringSymbolList;

         FDefaultUserObject : TObject;

         FStackParameters : TStackParameters;
         FGlobalAddrGenerator : TAddrGeneratorRec;

         FResultType : TdwsResultType;
         FRuntimeFileSystem : TdwsCustomFileSystem;
         FExecutions : TTightList;
         FExecutionsLock : TCriticalSection;
         FTimeoutMilliseconds : Integer;

         FSourceContextMap : TdwsSourceContextMap;
         FSymbolDictionary : TdwsSymbolDictionary;
         FAttributes : TdwsSymbolAttributes;

         FSystemTable : ISystemSymbolTable;
         FOperators : TObject;
         FConditionalDefines : IAutoStrings;
         FSourceList : TScriptSourceList;
         FLineCount : Integer;
         FTimeStamp : TDateTime;
         FCompileDuration : TDateTime;
         FCompiler : TObject;

         FDefaultEnvironment : IdwsEnvironment;
         FDefaultLocalizer : IdwsLocalizer;

      protected
         function GetConditionalDefines : IAutoStrings;
         function GetDefaultUserObject : TObject;
         procedure SetDefaultUserObject(const val : TObject);

         function GetSourceList : TScriptSourceList;
         function GetLineCount : Integer;
         function GetTimeStamp : TDateTime;
         function GetCompileDuration : TDateTime;

         procedure NotifyExecutionDestruction(exec : TdwsProgramExecution);

         // for interface only, script exprs use direct properties
         function GetMsgs : TdwsMessageList;
         function GetTable : TSymbolTable;
         function GetTimeoutMilliseconds : Integer;
         procedure SetTimeoutMilliseconds(const val : Integer);
         function GetSymbolDictionary : TdwsSymbolDictionary;
         function GetSourceContextMap : TdwsSourceContextMap;
         function GetProgramObject : TdwsMainProgram;

      public
         constructor Create(const systemTable : ISystemSymbolTable;
                            resultType : TdwsResultType;
                            const stackParameters : TStackParameters);
         destructor Destroy; override;

         function CreateNewExecution : IdwsProgramExecution;
         function BeginNewExecution : IdwsProgramExecution;
         function Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
         function ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;
         function ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;

         function GetSourceFile(const aSourceFile : String) : TSourceFile;

         function NextStackLevel(level : Integer) : Integer;

         procedure DropMapAndDictionary;

         function CollectAllPropertyAttributes : TSimplePropertySymbolList;

         property FinalExpr : TBlockFinalExpr read FFinalExpr write FFinalExpr;

         property TimeoutMilliseconds : Integer read FTimeoutMilliseconds write FTimeoutMilliseconds;
         property MaxRecursionDepth : Integer read FStackParameters.MaxRecursionDepth write FStackParameters.MaxRecursionDepth;
         property MaxExceptionDepth : Integer read FStackParameters.MaxExceptionDepth write FStackParameters.MaxExceptionDepth;
         property MaxDataSize : Integer read FStackParameters.MaxByteSize write FStackParameters.MaxByteSize;
         property StackChunkSize : Integer read FStackParameters.ChunkSize write FStackParameters.ChunkSize;
         property UnifiedConstList : TSortedExprBaseList read FUnifiedConstList;
         property ResourceStringList : TResourceStringSymbolList read FResourceStringList write FResourceStringList;
         property RuntimeFileSystem : TdwsCustomFileSystem read FRuntimeFileSystem write FRuntimeFileSystem;

         property SystemTable : ISystemSymbolTable read FSystemTable;
         property Operators : TObject read FOperators write FOperators;
         property ConditionalDefines : IAutoStrings read FConditionalDefines;
         property Compiler : TObject read FCompiler write FCompiler;
         property SourceContextMap : TdwsSourceContextMap read FSourceContextMap;
         property SymbolDictionary: TdwsSymbolDictionary read FSymbolDictionary;
         property Attributes : TdwsSymbolAttributes read FAttributes;
         property SourceList : TScriptSourceList read FSourceList;
         property LineCount : Integer read FLineCount write FLineCount;
         property TimeStamp : TDateTime read FTimeStamp write FTimeStamp;
         property CompileDuration : TDateTime read FCompileDuration write FCompileDuration;

         property DefaultEnvironment : IdwsEnvironment read FDefaultEnvironment write FDefaultEnvironment;
         property DefaultLocalizer : IdwsLocalizer read FDefaultLocalizer write FDefaultLocalizer;
         property DefaultUserObject : TObject read FDefaultUserObject write FDefaultUserObject;
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
         constructor Create(aParent : TdwsProgram);
         destructor Destroy; override;

         procedure AssignTo(sym: TFuncSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
         procedure InitSymbol(Symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);

         procedure OptimizeConstAssignments(blockExpr : TBlockExprBase);

         procedure SetBeginPos(const scriptPos : TScriptPos);

         property Func : TFuncSymbol read FFunc write FFunc;

         property PreConditions : TSourcePreConditions read FPreConditions write FPreConditions;
         property PostConditions : TSourcePostConditions read FPostConditions write FPostConditions;
   end;

   TdwsExceptionContext = class
      private
         FCallStack : TdwsExprLocationArray;

      public
         constructor Create(const aCallStack : TdwsExprLocationArray);
         procedure Skip(n : Integer);
         procedure ReplaceTop(expr : TExprBase);

         property CallStack : TdwsExprLocationArray read FCallStack;
   end;

   // Base class of all expressions attached to a program
   TProgramExpr = class(TExprBase)
      protected
         function GetType : TTypeSymbol; virtual;
         function GetBaseType : TTypeSymbol; virtual;

      public
         function  IsConstant : Boolean; virtual;
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; virtual;

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
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); override;

         procedure RaiseScriptError(exec : TdwsExecution; e : EScriptError); overload;
         procedure RaiseScriptError(exec : TdwsExecution); overload;
         procedure RaiseScriptError(exec : TdwsExecution; const msg : String); overload;
         procedure RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass; const msg : String); overload;
         procedure RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass; const msg : String;
                                    const args : array of const); overload;

         procedure RaiseUpperExceeded(exec : TdwsExecution; index : Integer);
         procedure RaiseLowerExceeded(exec : TdwsExecution; index : Integer);

         procedure CheckScriptObject(exec : TdwsExecution; const scriptObj : IScriptObj); inline;
         procedure RaiseObjectNotInstantiated(exec : TdwsExecution);
         procedure RaiseObjectAlreadyDestroyed(exec : TdwsExecution);

         function ScriptLocation(prog : TObject) : String; override;

         property Typ : TTypeSymbol read GetType;
         property BaseType : TTypeSymbol read GetBaseType;
   end;

   TProgramExprClass = class of TProgramExpr;

   // Base class of all typed expressions
   TTypedExpr = class(TProgramExpr)
      protected
         FTyp : TTypeSymbol;

         function GetType : TTypeSymbol; override;
         function GetBaseType : TTypeSymbol; override;

      public
         function OptimizeToTypedExpr(prog : TdwsProgram; exec : TdwsExecution; const hotPos : TScriptPos) : TTypedExpr;
         function OptimizeToFloatConstant(prog : TdwsProgram; exec : TdwsExecution) : TTypedExpr;

         function ScriptPos : TScriptPos; override;

         procedure CheckInterface(exec : TdwsExecution; const scriptObj : IScriptObj); inline;
         procedure RaiseInterfaceIsNil(exec : TdwsExecution);

         function IsOfType(typSym : TTypeSymbol) : Boolean;

         property Typ : TTypeSymbol read FTyp write FTyp;
   end;

   TTypedExprClass = class of TTypedExpr;

   // hosts a type reference
   TTypeReferenceExpr = class sealed (TTypedExpr)
      private
         FPos : TScriptPos;

      public
         constructor Create(aTyp : TTypeSymbol; const scriptPos : TScriptPos);

         function Eval(exec : TdwsExecution) : Variant; override;
         function ScriptPos : TScriptPos; override;
  end;

   // base class of expressions that return no result
   TNoResultExpr = class(TProgramExpr)
      public
         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         function OptimizeToNoResultExpr(prog : TdwsProgram; exec : TdwsExecution) : TNoResultExpr;

         function InterruptsFlow : Boolean; virtual;
   end;

   // base class of expressions that return no result
   TNoResultPosExpr = class(TNoResultExpr)
      protected
         FScriptPos : TScriptPos;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos);

         function ScriptPos : TScriptPos; override;
         procedure SetScriptPos(const aPos : TScriptPos);
   end;

   // Does nothing! E. g.: "for x := 1 to 10 do {TNullExpr};"
   TNullExpr = class(TNoResultPosExpr)
      procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // statement; statement; statement;
   TBlockExprBase = class(TNoResultPosExpr)
      protected
         FStatements : PNoResultExprList;
         FCount : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure AddStatement(expr : TNoResultExpr);
         procedure ReplaceStatement(index : Integer; expr : TNoResultExpr);
         function ExtractStatement(index : Integer) : TNoResultExpr;
   end;

   // statement; statement; statement;
   TBlockRawExpr = class(TBlockExprBase)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // var initialization + finalization block
   TBlockInitExpr = class(TBlockRawExpr)
   end;

   // finalization block
   TBlockFinalExpr = class(TBlockRawExpr)
   end;

  // Encapsulates data
   TDataExpr = class(TTypedExpr)
      protected
         function GetAddr(exec : TdwsExecution) : Integer; virtual;
         function GetData(exec : TdwsExecution) : TData; virtual; abstract;
         function GetDataPtr(exec : TdwsExecution) : TDataPtr; virtual;

      public
         constructor Create(Prog: TdwsProgram; Typ: TTypeSymbol);

         procedure AssignData(exec : TdwsExecution; const SourceData: TData; SourceAddr: Integer); virtual;
         procedure AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr); virtual;
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); virtual;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value: String); override;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function IsWritable: Boolean; virtual;

         property Addr[exec : TdwsExecution] : Integer read GetAddr;
         property Data[exec : TdwsExecution] : TData read GetData;

         property DataPtr[exec : TdwsExecution] : TDataPtr read GetDataPtr;
   end;

   // Encapsulates data
   TPosDataExpr = class(TDataExpr)
      protected
         FPos : TScriptPos;

      public
         constructor Create(Prog: TdwsProgram; const scriptPos : TScriptPos; Typ: TTypeSymbol);

         function ScriptPos : TScriptPos; override;

         property Pos : TScriptPos read FPos;
   end;

   // TExternalFuncHandler
   //
   TExternalFuncHandler = class(TInterfacedSelfObject, IUnknown, ICallable, IExecutable)
      public
         procedure InitSymbol(symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol);
   end;

   EdwsExternalFuncHandler = class (Exception);

   // TFuncExprBase
   //
   TFuncExprBase = class(TPosDataExpr)
      private
         FFunc : TFuncSymbol;

      protected
         FArgs : TExprBaseListRec;
         FResultAddr : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos; aFunc : TFuncSymbol);
         destructor Destroy; override;

         procedure AddArg(arg : TTypedExpr);
         procedure ClearArgs;
         function ExpectedArg : TParamSymbol; virtual; abstract;
         function GetArgType(idx : Integer) : TTypeSymbol;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         function IsConstant : Boolean; override;

         procedure Initialize(prog : TdwsProgram); virtual;

         procedure SetResultAddr(prog : TdwsProgram; exec : TdwsExecution; ResultAddr: Integer = -1);

         function ChangeFuncSymbol(aProg: TdwsProgram; newFuncSym : TFuncSymbol) : TFuncExprBase; virtual;

         property FuncSym : TFuncSymbol read FFunc;
         property Args : TExprBaseListRec read FArgs;
   end;

   TFuncExprOverloadsHelper = class
      private
         FExpr : TFuncExprBase;
         FOverloads : TFuncSymbolList;

      public
         constructor Create(expr : TFuncExprBase; overloads : TFuncSymbolList);
         function ExpectedArg : TParamSymbol;
   end;

   TPushOperatorType = (potUnknown,
                        potAddr, potPassAddr, potTempAddr, potTempArrayAddr, potTempArray,
                        potResult,
                        potResultInteger, potResultFloat, potResultBoolean,
                        potResultString, potResultConstString,
                        potData, potLazy, potInitResult);
   PPushOperator = ^TPushOperator;
   TPushOperator = packed record
      FStackAddr : Integer;
      FArgExpr : TTypedExpr;
      FTypeParamSym : TSymbol;  // TSymbol / TPushOperatorType union

      procedure InitPushAddr(stackAddr: Integer; argExpr: TTypedExpr);
      procedure InitPushTempAddr(stackAddr: Integer; argExpr: TTypedExpr);
      procedure InitPushTempArrayAddr(stackAddr: Integer; argExpr: TTypedExpr);
      procedure InitPushTempArray(stackAddr: Integer; argExpr: TTypedExpr);
      procedure InitPushResult(prog : TdwsProgram; stackAddr: Integer; argExpr: TTypedExpr);
      procedure InitPushData(stackAddr: Integer; argExpr: TTypedExpr; ParamSym: TSymbol);
      procedure InitPushInitResult(stackAddr: Integer; argExpr: TTypedExpr);
      procedure InitPushLazy(stackAddr: Integer; argExpr: TTypedExpr);

      procedure Execute(exec : TdwsExecution); inline;

      procedure ExecuteAddr(exec : TdwsExecution);
      procedure ExecutePassAddr(exec : TdwsExecution);
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
   TPushOperatorArray = packed array [0..MaxInt shr 5] of TPushOperator;
   PPushOperatorArray = ^TPushOperatorArray;

   // Function call: func(arg0, arg1, ...);
   TFuncExpr = class(TFuncExprBase)
      private
         FPushExprs : PPushOperatorArray;
         FCallerID : TFuncExpr;
         FPushExprsCount : SmallInt;
         FLevel : SmallInt;

      protected
         function PreCall(exec : TdwsExecution) : TFuncSymbol; virtual;
         function PostCall(exec : TdwsExecution) : Variant; virtual;

         procedure EvalPushExprs(exec : TdwsExecution); inline;

      public
         constructor Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TFuncSymbol);
         destructor Destroy; override;

         function ExpectedArg : TParamSymbol; override;

         procedure AddPushExprs(prog : TdwsProgram);

         function Eval(exec : TdwsExecution) : Variant; override;
         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;
         procedure Initialize(prog : TdwsProgram); override;
         function IsWritable : Boolean; override;

         property CallerID : TFuncExpr read FCallerID write FCallerID;
   end;

   IFuncPointer = interface
      function GetFuncExpr : TFuncExprBase;
      function SameFunc(const v : Variant) : Boolean;
   end;

   // Encapsulates a function or method pointer
   TFuncPointer = class(TInterfacedObject, IUnknown, IFuncPointer)
      private
         FFuncExpr : TFuncExprBase;

      public
         constructor Create(exec : TdwsExecution; funcExpr : TFuncExprBase);
         destructor Destroy; override;

         function GetFuncExpr : TFuncExprBase;
         function SameFunc(const v : Variant) : Boolean;
   end;

   // returns an IFuncPointer to the FuncExpr
   TAnonymousFuncRefExpr = class(TDataExpr)
      private
         FFuncExpr : TFuncExprBase;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetData(exec : TdwsExecution) : TData; override;

      public
         constructor Create(prog : TdwsProgram; funcExpr : TFuncExprBase);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;

         function Extract : TFuncExprBase; // also a destructor

         property FuncExpr : TFuncExprBase read FFuncExpr write FFuncExpr;
   end;

   TFuncRefExpr = class (TAnonymousFuncRefExpr)
   end;

   TFuncPtrExpr = class sealed (TFuncExpr)
      private
         FCodeExpr : TDataExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; const pos : TScriptPos; codeExpr : TDataExpr);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function IsConstant : Boolean; override;

         function Extract : TDataExpr; // also a destructor

         property CodeExpr : TDataExpr read FCodeExpr write FCodeExpr;
   end;

   TMethodObjExpr = class(TPosDataExpr)
      private
         FBaseExpr : TDataExpr;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; BaseExpr: TDataExpr);
         function GetData(exec : TdwsExecution) : TData; override;
         function GetAddr(exec : TdwsExecution) : Integer; override;
   end;

   TSourceCondition = class (TInterfacedSelfObject, IBooleanEvalable, IStringEvalable)
      private
         FPos : TScriptPos;
         FTest : TTypedExpr;
         FMsg : TTypedExpr;

      public
         constructor Create(const pos : TScriptPos; aTest, aMsg : TTypedExpr);
         destructor Destroy; override;

         procedure InitSymbol(symbol: TSymbol);
         procedure InitExpression(Expr: TExprBase);

         function EvalAsBoolean(exec : TdwsExecution) : Boolean;
         procedure EvalAsString(exec : TdwsExecution; var Result : String);

         property Pos : TScriptPos read FPos write FPos;
         property Test : TTypedExpr read FTest;
         property Msg : TTypedExpr read FMsg write FMsg;
   end;

   TSourceConditions = class
      private
         FProg : TdwsProcedure;
         FItems : TTightList;
         FAncestor : TSourceConditions;

         function GetConditions(idx : Integer) : TSourceCondition;

      public
         constructor Create(aProg : TdwsProcedure);
         destructor Destroy; override;

         procedure AddCondition(condition : TSourceCondition);

         procedure RaiseConditionFailed(exec : TdwsExecution; funcSym : TFuncSymbol;
                                        const scriptPos : TScriptPos;
                                        const msg : IStringEvalable); virtual; abstract;
         function  Test(exec : TdwsExecution) : TSourceCondition;
         procedure EvalNoResult(exec : TdwsExecution); virtual;

         property Ancestor : TSourceConditions read FAncestor write FAncestor;
         property Conditions[idx : Integer] : TSourceCondition read GetConditions; default;
         property Count : Integer read FItems.FCount;
   end;
   TSourceConditionsClass = class of TSourceConditions;

   TSourcePreConditions = class (TSourceConditions)
      public
         procedure RaiseConditionFailed(exec : TdwsExecution; funcSym : TFuncSymbol;
                                        const scriptPos : TScriptPos;
                                        const msg : IStringEvalable); override;
   end;

   TSourcePostConditions = class (TSourceConditions)
      public
         procedure RaiseConditionFailed(exec : TdwsExecution; funcSym : TFuncSymbol;
                                        const scriptPos : TScriptPos;
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
      private
         FArgs: TTightList;
         FBaseExpr: TTypedExpr;
         FConnectorArgs: TConnectorArgs;
         FConnectorCall: IConnectorCall;
         FConnectorParams: TConnectorParamArray;
         FIsInstruction: Boolean;
         FIsWritable: Boolean;
         FIsIndex: Boolean;
         FName: String;
         FResultData: TData;

      protected
         function GetData(exec : TdwsExecution) : TData; override;
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: String;
                            BaseExpr: TTypedExpr; IsWrite: Boolean = True; IsIndex: Boolean = False);
         destructor Destroy; override;

         function AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType) : Boolean;
         procedure AddArg(expr : TTypedExpr);
         function Eval(exec : TdwsExecution) : Variant; override;
         function IsWritable : Boolean; override;

         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;
         property IsWrite : Boolean read FIsWritable write FIsWritable;
         property IsIndex : Boolean read FIsIndex write FIsIndex;
         property ConnectorCall : IConnectorCall read FConnectorCall write FConnectorCall;
   end;

   TConnectorReadExpr = class sealed (TPosDataExpr)
      private
         FBaseExpr: TTypedExpr;
         FConnectorMember: IConnectorMember;
         FName: String;
         FResultData: TData;

      protected
         function GetData(exec : TdwsExecution) : TData; override;
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; const Name: String;
                            BaseExpr: TTypedExpr);
         destructor Destroy; override;

         function AssignConnectorSym(ConnectorType : IConnectorType) : Boolean;
         function Eval(exec : TdwsExecution) : Variant; override;

         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;

         property ConnectorMember : IConnectorMember read FConnectorMember write FConnectorMember;
  end;

   TConnectorWriteExpr = class sealed (TTypedExpr)
      private
         FBaseExpr: TTypedExpr;
         FValueExpr: TTypedExpr;
         FConnectorMember: IConnectorMember;
         FName: String;
         FScriptPos : TScriptPos;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const scriptPos: TScriptPos; const Name: String;
                            BaseExpr, ValueExpr: TTypedExpr);
         destructor Destroy; override;

         function ScriptPos : TScriptPos; override;
         function AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType) : Boolean;
         function  Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         property ConnectorMember : IConnectorMember read FConnectorMember write FConnectorMember;
         property BaseExpr : TTypedExpr read FBaseExpr write FBaseExpr;
         property ValueExpr : TTypedExpr read FValueExpr write FValueExpr;
   end;

   // Call of a method
   TMethodExpr = class abstract (TFuncExpr)
      private
         FBaseExpr : TTypedExpr;
         FSelfAddr : Integer;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const scriptPos : TScriptPos; Func: TMethodSymbol;
                            BaseExpr: TTypedExpr);
         destructor Destroy; override;

         function MethSym : TMethodSymbol; inline;

         function ChangeFuncSymbol(aProg: TdwsProgram; newFuncSym : TFuncSymbol) : TFuncExprBase; override;

         property BaseExpr : TTypedExpr read FBaseExpr;
   end;

   // Call of a record method
   TRecordMethodExpr = class (TFuncExpr)
   end;

   // Call of a helper method
   THelperMethodExpr = class (TFuncExpr)
   end;

   // Call of static methods (not virtual)
   TMethodStaticExpr = class(TMethodExpr)
      protected
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;

      public
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // Call of an interface methods
   TMethodInterfaceExpr = class(TMethodStaticExpr)
      protected
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;
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

   // Call to a static constructor
   TConstructorStaticExpr = class(TMethodStaticExpr)
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            Base: TTypedExpr);
   end;

   // Call to a virtual constructor
   TConstructorVirtualExpr = class(TMethodVirtualExpr)
      private
         FExternalObject: TObject;
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
         function PreCall(exec : TdwsExecution) : TFuncSymbol; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            Base: TTypedExpr);
         property ExternalObject: TObject read FExternalObject write FExternalObject;
   end;

   TConstructorStaticObjExpr = class(TMethodStaticExpr)
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            BaseExpr: TTypedExpr);
   end;

   TConstructorVirtualObjExpr = class(TMethodVirtualExpr)
      protected
         function PostCall(exec : TdwsExecution): Variant; override;
      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Func: TMethodSymbol;
                            Base: TTypedExpr);
   end;

   TDestructorStaticExpr = class(TMethodStaticExpr)
      protected
         function PostCall(exec : TdwsExecution): Variant; override;
   end;

   TDestructorVirtualExpr = class(TMethodVirtualExpr)
      protected
         function PostCall(exec : TdwsExecution) : Variant; override;
   end;

   TUnaryOpExpr = class(TTypedExpr)
      protected
         FExpr : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); virtual;
         destructor Destroy; override;

         function IsConstant : Boolean; override;
         property Expr : TTypedExpr read FExpr write FExpr;
   end;
   TUnaryOpExprClass = class of TUnaryOpExpr;

   // bool unary result
   TUnaryOpBoolExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // int unary result
   TUnaryOpIntExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // float unary result
   TUnaryOpFloatExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
         function Eval(exec : TdwsExecution) : Variant; override;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // String unary result
   TUnaryOpStringExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // variant unary result
   TUnaryOpVariantExpr = class(TUnaryOpExpr)
      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); override;
         function Eval(exec : TdwsExecution) : Variant; override;
   end;

   // wraps an expression with a result into a no-result one and discard the result
   TNoResultWrapperExpr = class(TNoResultExpr)
      protected
         FExpr : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;
         function  IsConstant : Boolean; override;

         function ScriptPos : TScriptPos; override;

         property Expr : TTypedExpr read FExpr write FExpr;
   end;

   // left "op" right
   TBinaryOpExpr = class(TTypedExpr)
      protected
         FLeft : TTypedExpr;
         FRight : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); virtual;
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function IsConstant : Boolean; override;

         property Typ : TTypeSymbol read FTyp write FTyp;
         property Left : TTypedExpr read FLeft write FLeft;
         property Right : TTypedExpr read FRight write FRight;
   end;

   TBinaryOpExprClass = class of TBinaryOpExpr;

   TVariantBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); override;
     function Eval(exec : TdwsExecution) : Variant; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TIntegerBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); override;
     function Eval(exec : TdwsExecution) : Variant; override;
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TStringBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); override;
     function Eval(exec : TdwsExecution) : Variant; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TFloatBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); override;
     function Eval(exec : TdwsExecution) : Variant; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TBooleanBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr); override;
     function Eval(exec : TdwsExecution) : Variant; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;

   // A list of typed expressions
   TTypedExprList = class
      protected
         FList : TTightList;
         FTable : TSymbolTable;
         FDefaultExpected : TParamSymbol;

         function GetExpr(const x: Integer): TTypedExpr;
         procedure SetExpr(const x: Integer; const Value: TTypedExpr);
         function GetCount : Integer;

      public
         destructor Destroy; override;

         procedure AddExpr(expr : TTypedExpr);
         function  ExpectedArg : TParamSymbol;

         procedure Insert0(expr : TExprBase);
         procedure Delete(index : Integer);
         procedure Clear; inline;

         property Expr[const x: Integer]: TTypedExpr read GetExpr write SetExpr; default;
         property Count : Integer read GetCount;
         property Table : TSymbolTable read FTable write FTable;
         property DefaultExpected : TParamSymbol read FDefaultExpected write FDefaultExpected;
   end;

   // Helper object for access to symbols
   IInfo = interface
      ['{8D534D16-4C6B-11D5-8DCB-0000216D9E86}']
      function Call: IInfo; overload;
      function Call(const Params: array of Variant): IInfo; overload;
      function Element(const Indices: array of Integer): IInfo;
      function GetConstructor(const MethName: String; ExtObject: TObject): IInfo;
      function GetData : TData;
      function GetExternalObject: TObject;
      function GetMember(const s: String): IInfo;
      function GetFieldMemberNames : TStrings;
      function GetMethod(const s: String): IInfo;
      function GetScriptObj: IScriptObj;
      function GetParameter(const s: String): IInfo;
      function GetTypeSym: TSymbol;
      function GetValue : Variant;
      function GetValueAsString : String;
      function GetValueAsDataString : RawByteString;
      function GetValueAsInteger : Int64;
      function GetValueAsBoolean : Boolean;
      function GetValueAsFloat : Double;
      function GetInherited: IInfo;
      procedure SetData(const Data: TData);
      procedure SetExternalObject(ExtObject: TObject);
      procedure SetValue(const Value: Variant);

      property Data: TData read GetData write SetData;
      property ExternalObject: TObject read GetExternalObject write SetExternalObject;
      property Member[const s: String]: IInfo read GetMember;
      property FieldMemberNames : TStrings read GetFieldMemberNames;
      property Method[const s: String]: IInfo read GetMethod;
      property ScriptObj: IScriptObj read GetScriptObj;
      property Parameter[const s: String]: IInfo read GetParameter;
      property TypeSym: TSymbol read GetTypeSym;
      property Value: Variant read GetValue write SetValue;
      property ValueAsString : String read GetValueAsString;
      property ValueAsDataString : RawByteString read GetValueAsDataString;
      property ValueAsInteger : Int64 read GetValueAsInteger;
      property ValueAsBoolean : Boolean read GetValueAsBoolean;
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
         function GetData(const s: String): TData;
         function GetFunc(const s: String): IInfo;
         procedure SetFuncSym(const Value: TFuncSymbol);
         function GetValueAsVariant(const s: String): Variant;
         function GetVars(const str: String): IInfo;
         procedure SetData(const s: String; const Value: TData);
         procedure SetValueAsVariant(const s: String; const Value: Variant);
         procedure SetResultAsVariant(const Value: Variant);
         function GetResultAsVariant: Variant;
         function GetResultVars: IInfo;

         function GetValueAsString(const s: String): String;
         procedure SetValueAsString(const s: String; const Value: String);
         function GetValueAsChar(const s: String): WideChar;
         function GetValueAsDataString(const s: String): RawByteString;
         procedure SetValueAsDataString(const s: String; const Value: RawByteString);
         function GetValueAsInteger(const s: String): Int64;
         procedure SetValueAsInteger(const s: String; const Value: Int64);
         function GetValueAsBoolean(const s: String): Boolean;
         procedure SetValueAsBoolean(const s: String; const Value: Boolean);
         function GetValueAsFloat(const s: String): Double;
         procedure SetValueAsFloat(const s: String; const Value: Double);
         function GetValueAsObject(const s: String): TObject;
         function GetValueAsClassSymbol(const s: String): TClassSymbol;
         function GetValueAsTStrings(const s: String): TStrings;
         procedure SetResultAsString(const value : String);
         procedure SetResultAsDataString(const value : RawByteString);
         procedure SetResultAsInteger(const value : Int64);
         procedure SetResultAsBoolean(const value : Boolean);
         procedure SetResultAsFloat(const value : Double);

         function GetParamAsPVariant(index : Integer) : PVariant;
         function GetParamAsVariant(index : Integer) : Variant;
         function GetParamAsInteger(index : Integer) : Int64;
         function GetParamAsString(index : Integer) : String;
         function GetParamAsFloat(index : Integer) : Double;
         function GetParamAsBoolean(index : Integer) : Boolean;
         function GetParamAsObject(index : Integer) : TObject;

         function CreateUnitList : TList;
         function FindSymbolInUnits(AUnitList: TList; const Name: String): TSymbol; overload;

      public
         procedure PrepareScriptObj;

         function RegisterExternalObject(AObject: TObject; AutoFree: Boolean=False; ExactClassMatch: Boolean=True): Variant;
         function GetExternalObjForVar(const s: String): TObject;
         // cycle ancestry hierarchy and find the nearest matching type
         function FindClassMatch(AObject: TObject; ExactMatch: Boolean=True): TClassSymbol;
         function FindSymbolInUnits(const Name: String): TSymbol; overload;
         function GetTemp(const DataType: String): IInfo;

         property Table : TSymbolTable read FTable write FTable;
         property Execution : TdwsProgramExecution read FExecution write FExecution;
         property Level : Integer read FLevel write FLevel;
         property Data[const s: String]: TData read GetData write SetData;
         property Func[const s: String]: IInfo read GetFunc;
         property FuncSym: TFuncSymbol read FFuncSym write SetFuncSym;
         property Method[const s: String]: IInfo read GetFunc;
         property ScriptObj: IScriptObj read FScriptObj write FScriptObj;
         property ResultAsVariant: Variant read GetResultAsVariant write SetResultAsVariant;
         property ResultVars: IInfo read GetResultVars;
         property Vars[const s: String]: IInfo read GetVars;

         property ValueAsVariant[const s : String] : Variant read GetValueAsVariant write SetValueAsVariant;
         property ValueAsChar[const s : String] : WideChar read GetValueAsChar;
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
         property ParamAsObject[index : Integer] : TObject read GetParamAsObject;

         property ResultAsString : String write SetResultAsString;
         property ResultAsDataString : RawByteString write SetResultAsDataString;
         property ResultAsBoolean : Boolean write SetResultAsBoolean;
         property ResultAsInteger : Int64 write SetResultAsInteger;
         property ResultAsFloat : Double write SetResultAsFloat;
  end;

   // An instance of a script class FClassSym. Instance data in FData,
   TScriptObj = class(TInterfacedObject, IScriptObj)
      private
         FNextObject, FPrevObject : TScriptObj;

      protected
         FData : TData;

         function GetClassSym : TClassSymbol; virtual;
         procedure SetClassSym(clsSym : TClassSymbol); virtual;

         function GetData : TData;
         function DataOfAddr(addr : Integer) : Variant;
         function DataOfAddrAsString(addr : Integer) : String;
         function DataOfAddrAsInteger(addr : Integer) : Int64;
         procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
         function GetDestroyed : Boolean; virtual;
         procedure SetDestroyed(const val : Boolean); virtual;
         function GetInternalObject: TObject;
         function GetExternalObject: TObject; virtual;
         procedure SetExternalObject(Value: TObject); virtual;
         procedure SetExecutionContext(exec : TdwsProgramExecution); virtual;

      public
         property NextObject : TScriptObj read FNextObject write FNextObject;
         property PrevObject : TScriptObj read FPrevObject write FPrevObject;
   end;

   TScriptObjInstance = class(TScriptObj)
      private
         FClassSym : TClassSymbol;
         FExternalObj : TObject;
         FExecutionContext : TdwsProgramExecution;
         FOnObjectDestroy: TObjectDestroyEvent;
         FDestroyed : Boolean;

      protected
         function GetClassSym: TClassSymbol; override;
         procedure SetClassSym(clsSym : TClassSymbol); override;
         function GetExternalObject: TObject; override;
         procedure SetExternalObject(Value: TObject); override;
         function GetDestroyed : Boolean; override;
         procedure SetDestroyed(const val : Boolean); override;
         procedure SetExecutionContext(exec : TdwsProgramExecution); override;

      public
         constructor Create(aClassSym : TClassSymbol; executionContext : TdwsProgramExecution = nil);
         destructor Destroy; override;
         procedure BeforeDestruction; override;

         property ExecutionContext : TdwsProgramExecution read FExecutionContext write FExecutionContext;
         property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
         property Destroyed : Boolean read FDestroyed write FDestroyed;
   end;

   TScriptDynamicArrayCompareFunc = function (idx1, idx2 : Integer) : Integer of object;

   TScriptDynamicArray = class(TScriptObj)
      private
         FElementTyp : TTypeSymbol;
         FElementSize : Integer;
         FLength : Integer;

      protected
         procedure SetLength(n : Integer);
         property GetLength : Integer read FLength;
         procedure SetData(const data : TData);

         procedure QuickSort(lo, hi : Integer; const compareFunc : TScriptDynamicArrayCompareFunc);
         function CompareFunc(idx1, idx2 : Integer) : Integer;

      public
         constructor Create(elemTyp : TTypeSymbol);

         procedure Delete(index, count : Integer);
         procedure Insert(index : Integer);
         procedure Swap(i1, i2 : Integer);
         procedure Reverse;
         procedure Sort(exec : TdwsExecution; compareExpr : TTypedExpr);
         procedure Copy(src : TScriptDynamicArray; index, count : Integer);
         procedure RawCopy(const src : TData; rawIndex, rawCount : Integer);
         procedure Concat(src : TScriptDynamicArray);
         function IndexOf(const item : TData; addr, fromIndex : Integer) : Integer; overload;
         function IndexOf(const item : Variant; fromIndex : Integer) : Integer; overload;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer; overload;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ElementSize : Integer read FElementSize;
         property Data : TData read FData write SetData;
         property Length : Integer read FLength write SetLength;
   end;

   TScriptInterface = class(TScriptObjInstance)
      private
         FTyp : TInterfaceSymbol;
         FInstance : IScriptObj;
         FVMT : TMethodSymbolArray;

      protected

      public
         constructor Create(const instance : IScriptObj;
                            const resolvedInterface : TResolvedInterface;
                            executionContext : TdwsProgramExecution = nil);
         procedure BeforeDestruction; override;

         property Typ : TInterfaceSymbol read FTyp;
         property Instance : IScriptObj read FInstance;
         property VMT : TMethodSymbolArray read FVMT write FVMT;
   end;

   EdwsVariantTypeCastError = class(EVariantTypeCastError)
      public
         constructor Create(const v : Variant; const desiredType : String;
                            originalException : Exception);
   end;

   EScriptStopped = class (EScriptError)
      public
         class procedure DoRaise(exec : TdwsExecution; stoppedOn : TExprBase); static;
   end;

function CreateFuncExpr(prog : TdwsProgram; funcSym: TFuncSymbol;
                        const scriptObj : IScriptObj; structSym : TCompositeTypeSymbol;
                        forceStatic : Boolean = False): TFuncExpr;
function CreateMethodExpr(prog: TdwsProgram; meth: TMethodSymbol; var expr : TTypedExpr; RefKind: TRefKind;
                          const scriptPos: TScriptPos; ForceStatic : Boolean = False): TFuncExpr;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsFunctions, dwsCoreExprs, dwsMagicExprs, dwsInfo;

type

   TPrintFunction = class(TInternalFunction)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

   TPrintLnFunction = class(TInternalFunction)
      public
         procedure Execute(info : TProgramInfo); override;
   end;

{ TScriptObjectWrapper }

// wrapper to interact with an released script object
type
   TScriptObjectWrapper = class (TInterfacedObject, IUnknown, IScriptObj)
      private
         FScriptObj : TScriptObjInstance;
      protected
         { IScriptObj }
         function GetClassSym: TClassSymbol;
         function GetData : TData;
         function DataOfAddr(addr : Integer) : Variant;
         function DataOfAddrAsString(addr : Integer) : String;
         function DataOfAddrAsInteger(addr : Integer) : Int64;
         procedure DataOfAddrAsScriptObj(addr : Integer; var scriptObj : IScriptObj);
         function GetInternalObject: TObject;
         function GetExternalObject: TObject;
         procedure SetExternalObject(Value: TObject);
         function GetDestroyed : Boolean;
         procedure SetDestroyed(const val : Boolean);
      public
         constructor Create(scriptObj : TScriptObjInstance);
   end;

// Create
//
constructor TScriptObjectWrapper.Create(scriptObj : TScriptObjInstance);
begin
   inherited Create;
   FScriptObj:=scriptObj;
end;

// GetClassSym
//
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

// GetInternalObject
//
function TScriptObjectWrapper.GetInternalObject: TObject;
begin
   Result:=FScriptObj;
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

// RaiseVariableNotFound
//
procedure RaiseVariableNotFound(const s : String);
begin
   raise Exception.CreateFmt(RTE_VariableNotFound, [s]);
end;

// RaiseIncorrectParameterIndex
//
procedure RaiseIncorrectParameterIndex(i : Integer);
begin
   raise Exception.CreateFmt(RTE_IncorrectParameterIndex, [i]);
end;

// RaiseOnlyVarSymbols
//
procedure RaiseOnlyVarSymbols(sym : TSymbol);
begin
   raise Exception.CreateFmt(RTE_OnlyVarSymbols, [sym.Caption]);
end;

// CreateFuncExpr
//
function CreateFuncExpr(prog : TdwsProgram; funcSym: TFuncSymbol;
                        const scriptObj : IScriptObj; structSym : TCompositeTypeSymbol;
                        forceStatic : Boolean = False): TFuncExpr;
var
   instanceExpr : TTypedExpr;
begin
   if FuncSym is TMethodSymbol then begin
      if Assigned(scriptObj) then begin
         instanceExpr:=TConstExpr.Create(Prog, structSym, scriptObj);
         Result:=CreateMethodExpr(prog, TMethodSymbol(funcSym),
                                  instanceExpr, rkObjRef, cNullPos, ForceStatic)
      end else if structSym<>nil then begin
         instanceExpr:=TConstExpr.Create(prog, (structSym as TClassSymbol).MetaSymbol, Int64(structSym));
         Result:=CreateMethodExpr(prog, TMethodSymbol(funcSym),
                                  instanceExpr, rkClassOfRef, cNullPos, ForceStatic)
      end else begin
         // static method
         structSym:=TMethodSymbol(funcSym).StructSymbol;
         if structSym is TStructuredTypeSymbol then begin
            instanceExpr:=TConstExpr.Create(prog, TStructuredTypeSymbol(structSym).MetaSymbol, Int64(structSym));
            Result:=CreateMethodExpr(prog, TMethodSymbol(funcSym),
                                     instanceExpr, rkClassOfRef, cNullPos, ForceStatic)
         end else begin
            Result:=nil;
            Assert(False, 'TODO');
         end;
      end;
   end else begin
      Result:=TFuncExpr.Create(Prog, cNullPos, funcSym);
   end;
end;

// CreateMethodExpr
//
function CreateMethodExpr(prog: TdwsProgram; meth: TMethodSymbol; var expr: TTypedExpr; RefKind: TRefKind;
                          const scriptPos: TScriptPos; ForceStatic : Boolean = False): TFuncExpr;
var
   helper : THelperSymbol;
begin
   // Create the correct TExpr for a method symbol
   Result := nil;

   if meth.StructSymbol is TInterfaceSymbol then begin

      Result:=TMethodInterfaceExpr.Create(prog, scriptPos, meth, expr);

   end else if meth.StructSymbol is TClassSymbol then begin

      if meth.IsStatic then begin

         Result:=TFuncExpr.Create(prog, scriptPos, meth);
         expr.Free;
         Exit;

      end else if (expr.Typ is TClassOfSymbol) then begin

         if expr.IsConstant and TClassOfSymbol(expr.Typ).TypClassSymbol.IsAbstract then begin
            if meth.Kind=fkConstructor then
               prog.CompileMsgs.AddCompilerError(scriptPos, RTE_InstanceOfAbstractClass)
            else prog.CompileMsgs.AddCompilerError(scriptPos, CPE_AbstractClassUsage);
         end;

      end;
      if (not meth.IsClassMethod) and meth.StructSymbol.IsStatic then
         prog.CompileMsgs.AddCompilerErrorFmt(scriptPos, CPE_ClassIsStatic, [meth.StructSymbol.Name]);

      // Return the right expression
      case meth.Kind of
         fkFunction, fkProcedure, fkMethod, fkLambda:
            if meth.IsClassMethod then begin
               if not ForceStatic and meth.IsVirtual then
                  Result := TClassMethodVirtualExpr.Create(prog, scriptPos, meth, expr)
               else Result := TClassMethodStaticExpr.Create(prog, scriptPos, meth, expr)
            end else begin
               if RefKind<>rkObjRef then
                  prog.CompileMsgs.AddCompilerError(scriptPos, CPE_StaticMethodExpected);
               if not ForceStatic and meth.IsVirtual then
                  Result := TMethodVirtualExpr.Create(prog, scriptPos, meth, expr)
               else Result := TMethodStaticExpr.Create(prog, scriptPos, meth, expr);
            end;
         fkConstructor:
            if RefKind = rkClassOfRef then begin
               if not ForceStatic and meth.IsVirtual then
                  Result := TConstructorVirtualExpr.Create(prog, scriptPos, meth, expr)
               else Result := TConstructorStaticExpr.Create(prog, scriptPos, meth, expr);
            end else begin
               if not ((prog is TdwsProcedure) and (TdwsProcedure(prog).Func.Kind=fkConstructor)) then
                  prog.CompileMsgs.AddCompilerWarning(scriptPos, CPE_UnexpectedConstructor);
               if not ForceStatic and meth.IsVirtual then
                  Result := TConstructorVirtualObjExpr.Create(prog, scriptPos, meth, expr)
               else Result := TConstructorStaticObjExpr.Create(prog, scriptPos, meth, expr);
            end;
         fkDestructor:
            begin
               if RefKind<>rkObjRef then
                  prog.CompileMsgs.AddCompilerError(scriptPos, CPE_UnexpectedDestructor);
               if not ForceStatic and meth.IsVirtual then
                  Result := TDestructorVirtualExpr.Create(prog, scriptPos, meth, expr)
               else Result := TDestructorStaticExpr.Create(prog, scriptPos, meth, expr)
            end;
      else
         Assert(False);
      end;

   end else if meth.StructSymbol is TRecordSymbol then begin

      if meth.IsClassMethod then begin

         Result:=TFuncExpr.Create(prog, scriptPos, meth);
         expr.Free;

      end else begin

         Result:=TRecordMethodExpr.Create(prog, scriptPos, meth);
         Result.AddArg(expr);

      end;

   end else if meth.StructSymbol is THelperSymbol then begin

      helper:=THelperSymbol(meth.StructSymbol);
      if     meth.IsClassMethod
         and (   (helper.ForType.ClassType=TInterfaceSymbol)
              or not (   (helper.ForType is TStructuredTypeSymbol)
                      or (helper.ForType is TStructuredTypeMetaSymbol))) then begin

         Result:=TFuncExpr.Create(prog, scriptPos, meth);
         expr.Free;

      end else begin

         Result:=THelperMethodExpr.Create(prog, scriptPos, meth);
         if expr<>nil then
            Result.AddArg(expr);

      end;

   end else Assert(False);

   expr:=nil;
end;

// ------------------
// ------------------ TdwsProgramExecution ------------------
// ------------------

// Create
//
constructor TdwsProgramExecution.Create(aProgram : TdwsMainProgram; const stackParams : TStackParameters);
begin
   inherited Create(stackParams);

   FProg:=aProgram;
   FProg._AddRef;

   FMsgs:=TdwsRuntimeMessageList.Create;

   if aProgram.CompileMsgs.HasErrors then
      FProgramState:=psUndefined
   else FProgramState:=psReadyToRun;

   FCallStack.Push(nil);
   FCallStack.Push(FProg);
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
   FEnvironment:=nil;

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
            Msgs.AddRuntimeError(RTE_ScriptAlreadyRunning);
         psUndefined :
            Msgs.AddRuntimeError(RTE_CantRunScript);
      else
         Msgs.AddRuntimeError(RTE_StateReadyToRunExpected);
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
      Stack.Push(FProg.FGlobalAddrGenerator.DataSize + FProg.DataSize);
      Stack.PushBp(0, Stack.BasePointer);

      // Initialize Result
      FResult.Free;
      FResult:=FProg.FResultType.CreateProgResult;

      // Debugger
      StartDebug;

      // Prepare FileSystem
      if FProg.RuntimeFileSystem<>nil then
         FFileSystem:=FProg.RuntimeFileSystem.AllocateFileSystem
      else FFileSystem:=TdwsOSFileSystem.Create;

      // Initialize global variables
      Status:=esrNone;
      FProg.FInitExpr.EvalNoResult(Self);

      if not (FProg.Expr is TBlockExprBase) then
         DoStep(FProg.FExpr);

   except
      on e: EScriptError do begin
         FMsgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
         FProgramState:=psRunningStopped;
      end;
      on e: Exception do begin
         FMsgs.AddRuntimeError(LastScriptError.ScriptPos, e.Message, LastScriptCallStack);
         FProgramState:=psRunningStopped;
      end;
   end;
end;

// RunProgram
//
procedure TdwsProgramExecution.RunProgram(aTimeoutMilliSeconds : Integer);

   procedure Handle_EScriptAssertionFailed(e : EScriptAssertionFailed);
   begin
      Msgs.AddRuntimeError(e.ScriptPos,
                            Copy(e.Message, 1, LastDelimiter('[', e.Message)-2)
                           +Copy(e.Message, Pos(']', e.Message)+1, MaxInt),
                           e.ScriptCallStack);
   end;

   procedure Handle_Exception(e : Exception);
   var
      debugPos : TScriptPos;
   begin
      if LastScriptError<>nil then
         if LastScriptError is TFuncExpr then
            Msgs.AddRuntimeError(LastScriptError.ScriptPos,
                                 e.Message+' in '+TFuncExpr(LastScriptError).FuncSym.QualifiedName,
                                 LastScriptCallStack)
         else Msgs.AddRuntimeError(LastScriptError.ScriptPos, e.Message,
                                   LastScriptCallStack)
      else if (Debugger<>nil) and (Debugger.LastDebugStepExpr<>nil) then begin
         debugPos:=Debugger.LastDebugStepExpr.ScriptPos;
         debugPos.Col:=0;
         Msgs.AddRuntimeError(debugPos, e.Message, LastScriptCallStack)
      end else Msgs.AddRuntimeError(cNullPos, e.Message, LastScriptCallStack);
   end;

var
   stackBaseReqSize : Integer;
begin
   if FProgramState<>psRunning then begin
      Msgs.AddRuntimeError('Program state psRunning expected');
      Exit;
   end;

   if aTimeoutMilliSeconds=0 then
      aTimeOutMilliseconds:=FProg.TimeoutMilliseconds;
   if aTimeoutMilliSeconds>0 then
      TdwsGuardianThread.GuardExecution(Self, aTimeoutMilliSeconds);

   stackBaseReqSize:=FProg.FGlobalAddrGenerator.DataSize+FProg.DataSize;
   if Stack.StackPointer<stackBaseReqSize then
      Stack.FixBaseStack(stackBaseReqSize);

   try
      Status:=esrNone;
      try
         // Run the script
         FProg.FExpr.EvalNoResult(Self);

         if status<>esrNone then begin
            case status of
               esrBreak : Msgs.AddRuntimeError(RTE_InvalidBreak);
               esrContinue : Msgs.AddRuntimeError(RTE_InvalidContinue);
            end;
         end;
      except
         on e: EScriptAssertionFailed do
            Handle_EScriptAssertionFailed(e);
         on e: EScriptException do
            Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
         on e: EScriptError do
            Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
         on e: EScriptStackException do
            Msgs.AddRuntimeError(LastScriptError.ScriptPos,
                                 e.Message,
                                 LastScriptCallStack);
         on e: Exception do
            Handle_Exception(e);
      end;

   finally
      if aTimeoutMilliSeconds>0 then
         TdwsGuardianThread.ForgetExecution(Self);
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

   if FProg.FinalExpr<>nil then
      FProg.FinalExpr.EvalNoResult(Self);

   FProgramState:=psTerminated;
   try
      // Stack
      Stack.PopBp(0);
      Stack.Pop(Stack.StackPointer); // FProg.FAddrGenerator.DataSize + FProg.FGlobalAddrGenerator.DataSize);

      // Object Cycles
      ReleaseObjects;

      // Debugger
      StopDebug;

      // FileSystem
      FFileSystem:=nil;

      FProgramInfo.Free;
      FProgramInfo:=nil;

      FProgramState:=psReadyToRun;
   except
      on e: EScriptError do
         Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
      on e: Exception do
         Msgs.AddRuntimeError(e.Message);
   end;

end;

// CallStackToString
//
class function TdwsProgramExecution.CallStackToString(const callStack : TdwsExprLocationArray) : String;
begin
   Result:=TExprBase.CallStackToString(callStack);
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
   (exceptObj.ExternalObject as TdwsExceptionContext).Skip(1); // temporary constructor expression
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
   if funcSym<>nil then
      Result.FTable:=funcSym.Params
   else Result.FTable:=FProg.Table;
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

// LocalizeSymbol
//
procedure TdwsProgramExecution.LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : String);
begin
   if Assigned(Localizer) then
      Localizer.LocalizeSymbol(aResSymbol, Result)
   else Result:=aResSymbol.Value;
end;

// LocalizeString
//
procedure TdwsProgramExecution.LocalizeString(const aString : String; var Result : String);
begin
   if Assigned(Localizer) then
      Localizer.LocalizeString(aString, Result)
   else Result:=aString;
end;

// ReleaseObjects
//
procedure TdwsProgramExecution.ReleaseObjects;
var
   i : Integer;
   iter : TScriptObj;
   buffer : array of TScriptObj;
begin
   FRTTIRawAttributes:=nil;

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
      iter.SetExecutionContext(nil);
      iter:=iter.NextObject;
   end;

   // clear all datas, kill references
   for i:=0 to FObjectCount-1 do begin
      iter:=buffer[i];
      SetLength(buffer[i].FData, 0);
      iter.SetClassSym(nil);
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
   scriptObj.SetExecutionContext(Self);
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
   scriptObj.SetExecutionContext(nil);
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
         Msgs.AddRuntimeError(e.Message);
   end;
end;

// GetInfo
//
function TdwsProgramExecution.GetInfo : TProgramInfo;
begin
   Assert(FProgramState<>psReadyToRun, 'Must call BeginProgram before accessing Info');
   if ProgramInfo<>nil then
      Result:=ProgramInfo
   else Result:=AcquireProgramInfo(nil);
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

// GetLocalizer
//
function TdwsProgramExecution.GetLocalizer : IdwsLocalizer;
begin
   Result:=FLocalizer;
end;

// SetLocalizer
//
procedure TdwsProgramExecution.SetLocalizer(const val : IdwsLocalizer);
begin
   FLocalizer:=val;
end;

// GetMsgs
//
function TdwsProgramExecution.GetMsgs : TdwsRuntimeMessageList;
begin
   Result:=FMsgs;
end;

// GetEnvironment
//
function TdwsProgramExecution.GetEnvironment : IdwsEnvironment;
begin
   Result:=FEnvironment;
end;

// SetEnvironment
//
procedure TdwsProgramExecution.SetEnvironment(const val : IdwsEnvironment);
begin
   FEnvironment:=val;
end;

// GetProg
//
function TdwsProgramExecution.GetProg : IdwsProgram;
begin
   Result:=FProg;
end;

// EnterRecursion
//
procedure TdwsProgramExecution.EnterRecursion(caller : TExprBase);
begin
   FCallStack.Push(caller);
   FCallStack.Push(FCurrentProg);
   if FCallStack.Count div 2>FStack.MaxRecursionDepth then
      RaiseMaxRecursionReached;

   if IsDebugging then
      Debugger.EnterFunc(Self, caller);
end;

// LeaveRecursion
//
procedure TdwsProgramExecution.LeaveRecursion;
begin
   if IsDebugging then
      Debugger.LeaveFunc(Self, TExprBase(FCallStack.Peek));

   FCurrentProg:=TdwsProgram(FCallStack.Peek);

   FCallStack.Pop;
   FCallStack.Pop;
end;

// SetCurrentProg
//
procedure TdwsProgramExecution.SetCurrentProg(const val : TdwsProgram);
begin
   FCurrentProg:=val;
end;

// RaiseMaxRecursionReached
//
procedure TdwsProgramExecution.RaiseMaxRecursionReached;
begin
   FCallStack.Pop;
   SetScriptError(TExprBase(FCallStack.Peek));
   FCallStack.Pop;
   raise EScriptStackOverflow.CreateFmt(RTE_MaximalRecursionExceeded, [FStack.MaxRecursionDepth]);
end;

// CallStackDepth
//
function TdwsProgramExecution.CallStackDepth : Integer;
begin
   Result:=(FCallStack.Count-2) div 2;
end;

// GetCallStack
//
function TdwsProgramExecution.GetCallStack : TdwsExprLocationArray;
var
   i, n : Integer;
begin
   n:=CallStackDepth;
   SetLength(Result, n);
   for i:=0 to n-1 do begin
      Result[n-1-i].Expr:=(TObject(FCallStack.List[i*2+2]) as TExprBase);
      Result[n-1-i].Prog:=TObject(FCallStack.List[i*2+3]);
   end;
end;

// CallStackLastExpr
//
function TdwsProgramExecution.CallStackLastExpr : TExprBase;
var
   n : Integer;
begin
   n:=FCallStack.Count-2;
   if n>=0 then
      Result:=(TObject(FCallStack.List[n]) as TExprBase)
   else Result:=nil;
end;

// ------------------
// ------------------ TdwsProgram ------------------
// ------------------

// Create
//
constructor TdwsProgram.Create(const systemTable : ISystemSymbolTable);
var
   sysTable : TSystemSymbolTable;
begin
   FCompileMsgs := TdwsCompileMessageList.Create;

   FAddrGenerator := TAddrGeneratorRec.CreatePositive(0);

   // Initialize the system table
   FRootTable := TProgramSymbolTable.Create(systemTable.SymbolTable, @FAddrGenerator);
   FTable := FRootTable;

   FInitExpr := TBlockInitExpr.Create(Self, cNullPos);

   // Initialize shortcuts to often used symbols
   sysTable:=systemTable.SymbolTable;
   FBaseTypes.FTypBoolean := sysTable.TypBoolean;
   FBaseTypes.FTypFloat := sysTable.TypFloat;
   FBaseTypes.FTypInteger := sysTable.TypInteger;
   FBaseTypes.FTypString := sysTable.TypString;
   FBaseTypes.FTypVariant := sysTable.TypVariant;
   FBaseTypes.FTypNil := TNilSymbol.Create;
   FBaseTypes.FTypObject := sysTable.TypObject;
   FBaseTypes.FTypException := sysTable.TypException;
   FBaseTypes.FTypInterface := sysTable.TypInterface;
end;

// Destroy
//
destructor TdwsProgram.Destroy;
begin
   FExpr.Free;
   FInitExpr.Free;
   FRootTable.Free;
   FUnitMains.Free;
   FBaseTypes.FTypNil.Free;
   FCompileMsgs.Free;
   FSubTables.Clear;

   inherited;
end;

function TdwsProgram.GetLevel: Integer;
begin
  Result := FAddrGenerator.Level;
end;

// GetUnitMains
//
function TdwsProgram.GetUnitMains : TUnitMainSymbols;
begin
   Result:=FUnitMains;
end;

// GetAddrGeneratorDataSize
//
function TdwsProgram.GetAddrGeneratorDataSize : Integer;
begin
   Result:=FAddrGenerator.DataSize;
end;

function TdwsProgram.GetGlobalAddr(DataSize: Integer): Integer;
begin
  Result := FRoot.FGlobalAddrGenerator.GetStackAddr(DataSize);
end;

function TdwsProgram.GetTempAddr(DataSize: Integer): Integer;
begin
  Result := FAddrGenerator.GetStackAddr(DataSize);
end;

// ResetExprs
//
procedure TdwsProgram.ResetExprs;
begin
   FExpr.Free;
   FExpr:=nil;
   FInitExpr.Free;
   FInitExpr:=TBlockInitExpr.Create(Self, cNullPos);
end;

// EnterSubTable
//
procedure TdwsProgram.EnterSubTable(subTable : TSymbolTable);
begin
   FSubTables.Add(Table);
   FTable:=subTable;
end;

// LeaveSubTable
//
procedure TdwsProgram.LeaveSubTable;
var
   n : Integer;
begin
   n:=FSubTables.Count-1;
   FTable:=TSymbolTable(FSubTables.List[n]);
   FSubTables.Delete(n);
end;

// SubTableDepth
//
function TdwsProgram.SubTableDepth : Integer;
begin
   Result:=FSubTables.Count;
end;

// SubTable
//
function TdwsProgram.SubTable(depth : Integer) : TSymbolTable;
begin
   Result:=TSymbolTable(FSubTables.List[depth]);
end;

// ContextMethodSymbol
//
function TdwsProgram.ContextMethodSymbol : TMethodSymbol;
var
   progIter : TdwsProgram;
begin
   progIter:=Self;
   while (progIter<>nil) and (progIter is TdwsProcedure) do begin
      if TdwsProcedure(progIter).Func is TMethodSymbol then begin
         Result:=TMethodSymbol(TdwsProcedure(progIter).Func);
         Exit;
      end;
      progIter:=progIter.Parent;
   end;
   Result:=nil;
end;

// ------------------
// ------------------ TdwsMainProgram ------------------
// ------------------

// Create
//
constructor TdwsMainProgram.Create(const systemTable : ISystemSymbolTable;
                                   resultType : TdwsResultType;
                                   const stackParameters : TStackParameters);
var
   systemUnitTable : TLinkedSymbolTable;
   systemUnit : TUnitMainSymbol;
   sl : TStringList;
begin
   inherited Create(systemTable);

   FResultType:=ResultType;

   FExecutionsLock:=TCriticalSection.Create;

   FStackParameters:=stackParameters;
   FStackParameters.MaxLevel:=1;

   FGlobalAddrGenerator:=TAddrGeneratorRec.CreatePositive(0);

   FSourceContextMap:=TdwsSourceContextMap.Create;

   FSymbolDictionary:=TdwsSymbolDictionary.Create;

   FAttributes:=TdwsSymbolAttributes.Create;

   sl:=TStringList.Create;
   sl.Sorted:=True;
   sl.CaseSensitive:=False;
   sl.Duplicates:=dupIgnore;
   FConditionalDefines:=TAutoStrings.Create(sl);

   FSourceList:=TScriptSourceList.Create;

   FRoot:=Self;

   FUnifiedConstList:=TUnifiedConstList.Create;
   TUnifiedConstList(FUnifiedConstList).Precharge(Self, systemTable.SymbolTable);

   FResourceStringList:=TResourceStringSymbolList.Create;

   FUnitMains:=TUnitMainSymbols.Create;

   FSystemTable := systemTable;
   systemUnitTable:=TLinkedSymbolTable.Create(systemTable.SymbolTable);
   systemUnit:=TUnitMainSymbol.Create(SYS_SYSTEM, systemUnitTable, FUnitMains);
   systemUnit.ReferenceInSymbolTable(FRootTable, True);

   FRoot:=Self;
end;

// Destroy
//
destructor TdwsMainProgram.Destroy;
begin
   FExecutionsLock.Enter;
   try
      if FExecutions.Count>0 then
         raise Exception.CreateFmt(RTE_ScriptHasLiveExecutions, [FExecutions.Count]);
   finally
      FExecutionsLock.Leave;
   end;
   FExecutionsLock.Free;

   inherited;

   FFinalExpr.Free;
   FOperators.Free;
   FSourceContextMap.Free;
   FSymbolDictionary.Free;
   FAttributes.Free;
   FUnifiedConstList.Free;
   FResourceStringList.Free;
   FSourceList.Free;
end;

// CreateNewExecution
//
function TdwsMainProgram.CreateNewExecution : IdwsProgramExecution;
var
   exec : TdwsProgramExecution;
begin
   if CompileMsgs.HasErrors then
      raise EScriptException.Create(RTE_CantRunScript);
   exec:=TdwsProgramExecution.Create(Self, FStackParameters);
   exec.UserObject:=DefaultUserObject;
   exec.Environment:=DefaultEnvironment;
   exec.Localizer:=DefaultLocalizer;
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
function TdwsMainProgram.BeginNewExecution : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.BeginProgram;
end;

// NotifyExecutionDestruction
//
procedure TdwsMainProgram.NotifyExecutionDestruction(exec : TdwsProgramExecution);
begin
   FExecutionsLock.Enter;
   try
      FExecutions.Remove(exec);
   finally
      FExecutionsLock.Leave;
   end;
end;

// Execute
//
function TdwsMainProgram.Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.Execute(aTimeoutMilliSeconds);
end;

// ExecuteParam
//
function TdwsMainProgram.ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.ExecuteParam(params, aTimeoutMilliSeconds);
end;

// ExecuteParam
//
function TdwsMainProgram.ExecuteParam(const Params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
begin
   Result:=CreateNewExecution;
   Result.ExecuteParam(Params, aTimeoutMilliSeconds);
end;

// GetMsgs
//
function TdwsMainProgram.GetMsgs : TdwsMessageList;
begin
   Result:=FCompileMsgs;
end;

// GetTable
//
function TdwsMainProgram.GetTable : TSymbolTable;
begin
   Result:=FTable;
end;

// GetTimeoutMilliseconds
//
function TdwsMainProgram.GetTimeoutMilliseconds : Integer;
begin
   Result:=FTimeoutMilliseconds;
end;

// SetTimeoutMilliseconds
//
procedure TdwsMainProgram.SetTimeoutMilliseconds(const val : Integer);
begin
   FTimeoutMilliseconds:=val;
end;

// GetSymbolDictionary
//
function TdwsMainProgram.GetSymbolDictionary : TdwsSymbolDictionary;
begin
   Result:=FSymbolDictionary;
end;

// GetSourceContextMap
//
function TdwsMainProgram.GetSourceContextMap : TdwsSourceContextMap;
begin
   Result:=FSourceContextMap;
end;

// GetProgramObject
//
function TdwsMainProgram.GetProgramObject : TdwsMainProgram;
begin
   Result:=Self;
end;

// GetSourceFile
//
function TdwsMainProgram.GetSourceFile(const aSourceFile : String) : TSourceFile;
var
   i : Integer;
begin
   i:=FSourceList.IndexOf(aSourceFile);
   if i>=0 then
      Result:=FSourceList[i].SourceFile
   else Result:=nil;
end;

// GetSourceList
//
function TdwsMainProgram.GetSourceList : TScriptSourceList;
begin
   Result:=FSourceList;
end;

// GetLineCount
//
function TdwsMainProgram.GetLineCount : Integer;
begin
   Result:=FLineCount;
end;

// GetTimeStamp
//
function TdwsMainProgram.GetTimeStamp : TDateTime;
begin
   Result:=FTimeStamp;
end;

// GetCompileDuration
//
function TdwsMainProgram.GetCompileDuration : TDateTime;
begin
   Result:=FCompileDuration;
end;

// GetDefaultUserObject
//
function TdwsMainProgram.GetDefaultUserObject : TObject;
begin
   Result:=FDefaultUserObject;
end;

// SetDefaultUserObject
//
procedure TdwsMainProgram.SetDefaultUserObject(const val : TObject);
begin
   FDefaultUserObject:=val;
end;

// NextStackLevel
//
function TdwsMainProgram.NextStackLevel(level : Integer) : Integer;
begin
   Result:=level+1;
   if Result>FStackParameters.MaxLevel then
      FStackParameters.MaxLevel:=Result;
end;

// DropMapAndDictionary
//
procedure TdwsMainProgram.DropMapAndDictionary;
begin
   FSymbolDictionary.Free;
   FSymbolDictionary:=nil;
   FSourceContextMap.Free;
   FSourceContextMap:=nil;
end;

// CollectAllPropertyAttributes
//
function TdwsMainProgram.CollectAllPropertyAttributes : TSimplePropertySymbolList;
var
   tableList : TSimpleRefCountedObjectHash;
begin
   Result:=TSimplePropertySymbolList.Create;
   tableList:=TSimpleRefCountedObjectHash.Create;
   try
      RootTable.CollectPropertyAttributes(tableList, Result);
   finally
      tableList.Free;
   end;
end;

// GetConditionalDefines
//
function TdwsMainProgram.GetConditionalDefines : IAutoStrings;
begin
   Result:=FConditionalDefines;
end;

// ------------------
// ------------------ TdwsProcedure ------------------
// ------------------

// Create
//
constructor TdwsProcedure.Create(aParent : TdwsProgram);
begin
   FParent:=aParent;

   // Create a local symbol table and connect it to the parent symboltable
   FAddrGenerator:=TAddrGeneratorRec.CreatePositive(Parent.Level + 1);
   FRootTable:=TProgramSymbolTable.Create(Parent.Table, @FAddrGenerator);
   FTable:=FRootTable;
//   FSystemTable:=Parent.SystemTable;
   FCompileMsgs:=Parent.CompileMsgs;
   FUnitMains:=Parent.UnitMains;

   FInitExpr := TBlockInitExpr.Create(Self, cNullPos);

   // Connect the procedure to the root TdwsProgram
   FRoot:=Parent.Root;
   FBaseTypes:=FRoot.FBaseTypes;
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
   stackSize : Integer;
   oldStatus : TExecutionStatusResult;
begin
   exec.CurrentProg:=Self;

   // Allocate stack space for local variables
   stackSize:=DataSize;
   exec.Stack.Push(stackSize);

   // Run the procedure
   oldStatus:=exec.Status;
   try
      exec.Status:=esrNone;

      if FPreConditions<>nil then
         FPreConditions.EvalNoresult(exec);

      exec.DoStep(FInitExpr);
      FInitExpr.EvalNoResult(exec);

      exec.DoStep(FExpr);
      FExpr.EvalNoResult(exec);

      if FPostConditions<>nil then
         FPostConditions.EvalNoresult(exec);

   finally
      exec.Status:=oldStatus;

      // Free stack space for local variables
      exec.Stack.Pop(stackSize);
   end;
end;

procedure TdwsProcedure.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TdwsProcedure.InitExpression(Expr: TExprBase);
begin
end;

// OptimizeConstAssignments
//
procedure TdwsProcedure.OptimizeConstAssignments(blockExpr : TBlockExprBase);
var
   i, j : Integer;
   subExpr, initSubExpr : TExprBase;
   assignExpr : TAssignExpr;
   assignExprSym : TDataSymbol;
   initAssign : TAssignConstExpr;
begin
   if (InitExpr.SubExprCount=0) or (blockExpr.SubExprCount=0) then Exit;

   // merges all initial constant assignments in blockExpr in InitExpr

   // valid only if InitExpr is only made of constant assignments
   for i:=0 to InitExpr.SubExprCount-1 do begin
      initSubExpr:=InitExpr.SubExpr[i];
      if not (initSubExpr is TAssignConstExpr) then Exit;
   end;

   i:=0;
   while i<blockExpr.SubExprCount do begin
      subExpr:=blockExpr.SubExpr[i];
      if subExpr is TAssignConstExpr then begin
         assignExpr:=TAssignExpr(subExpr);
         if assignExpr.Left is TVarExpr then begin
            assignExprSym:=TVarExpr(assignExpr.Left).DataSym;
            for j:=0 to InitExpr.SubExprCount-1 do begin
               initSubExpr:=InitExpr.SubExpr[j];
               if initSubExpr.ClassType=assignExpr.ClassType then begin
                  initAssign:=TAssignConstExpr(initSubExpr);
                  if (initAssign.Left is TVarExpr) and (TVarExpr(initAssign.Left).DataSym=assignExprSym) then begin
                     InitExpr.ReplaceStatement(j, blockExpr.ExtractStatement(i));
                     Dec(i);
                     Break;
                  end;
               end;
            end;
         end;
      end else Break;
      Inc(i);
   end;

   // the first assignment can be moved to the last of the InitExpr
   if i<blockExpr.SubExprCount then begin
      subExpr:=blockExpr.SubExpr[i];
      if (subExpr is TAssignExpr) and not (subExpr is TOpAssignExpr) then begin
         assignExpr:=TAssignExpr(subExpr);
         if assignExpr.Left is TVarExpr then begin
            assignExprSym:=TVarExpr(assignExpr.Left).DataSym;
            j:=InitExpr.SubExprCount-1;
            initSubExpr:=InitExpr.SubExpr[j];
            initAssign:=(initSubExpr as TAssignConstExpr);
            if (initAssign.Left is TVarExpr) and (TVarExpr(initAssign.Left).DataSym=assignExprSym) then begin
               InitExpr.ReplaceStatement(j, blockExpr.ExtractStatement(i));
            end;
         end;
      end;
   end;
end;

// SetBeginPos
//
procedure TdwsProcedure.SetBeginPos(const scriptPos : TScriptPos);
begin
   FInitExpr.FScriptPos:=scriptPos;
end;

// ------------------
// ------------------ TdwsExceptionContext ------------------
// ------------------

// Create
//
constructor TdwsExceptionContext.Create(const aCallStack : TdwsExprLocationArray);
begin
   FCallStack:=aCallStack
end;

// Skip
//
procedure TdwsExceptionContext.Skip(n : Integer);
var
   i : Integer;
begin
   for i:=0 to High(FCallStack)-n do
      FCallStack[i]:=FCallStack[i+n];
   SetLength(FCallStack, Length(CallStack)-n);
end;

// ReplaceTop
//
procedure TdwsExceptionContext.ReplaceTop(expr : TExprBase);
begin
   FCallStack[0].Expr:=expr;
end;

// ------------------
// ------------------ TdwsResultType ------------------
// ------------------

procedure TdwsResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  // no symbols
end;

function TdwsResultType.CreateProgResult: TdwsResult;
begin
  Result := TdwsDefaultResult.Create(Self);
end;

// ------------------
// ------------------ TdwsResult ------------------
// ------------------

constructor TdwsResult.Create(ResultType: TdwsResultType);
begin
  FResultType := ResultType;
end;

// ------------------
// ------------------ TdwsDefaultResultType ------------------
// ------------------

// CreateProgResult
//
function TdwsDefaultResultType.CreateProgResult: TdwsResult;
begin
   Result:=TdwsDefaultResult.Create(Self);
end;

// AddResultSymbols
//
procedure TdwsDefaultResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
   inherited;
   TPrintFunction.Create(SymbolTable, 'Print',  ['v', 'Variant'], '', []);
   TPrintLnFunction.Create(SymbolTable, 'PrintLn', ['v', 'Variant'], '', []);
end;

// ------------------
// ------------------ TPrintFunction ------------------
// ------------------

procedure TPrintFunction.Execute(info : TProgramInfo);
begin
   info.Execution.Result.AddString(info.ValueAsString['v']);
end;

// ------------------
// ------------------ TPrintLnFunction ------------------
// ------------------

procedure TPrintLnFunction.Execute(info : TProgramInfo);
var
   result : TdwsResult;
begin
   result:=info.Execution.Result;
   result.AddString(Info.ValueAsString['v']);
   result.AddString(#13#10);
end;

// ------------------
// ------------------ TdwsDefaultResult ------------------
// ------------------

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

// Clear
//
procedure TdwsDefaultResult.Clear;
begin
   FTextBuilder.Clear;
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

// ------------------
// ------------------ TdwsGuardedExecutionList ------------------
// ------------------

// Compare
//
function TdwsGuardedExecutionList.Compare(const item1, item2 : TdwsGuardedExecution) : Integer;
begin
   if item1.TimeOutAt<item2.TimeOutAt then
      Result:=1
   else if item1.TimeOutAt>item2.TimeOutAt then
      Result:=-1
   else Result:=0;
end;

// ------------------
// ------------------ TdwsGuardianThread ------------------
// ------------------

// Create
//
constructor TdwsGuardianThread.Create;
begin
   FEvent:=TEvent.Create(nil, False, False, '');
   FExecutionsLock:=TFixedCriticalSection.Create;
   FExecutions:=TdwsGuardedExecutionList.Create;
   FreeOnTerminate:=False;

   inherited Create(True);

   SetThreadName('DWScript Guardian');

   Priority:=tpTimeCritical;
end;

// Destroy
//
destructor TdwsGuardianThread.Destroy;
begin
   FExecutions.Clear;
   FExecutions.Free;
   FEvent.Free;
   FExecutionsLock.Free;
   inherited;
end;

// Initialize
//
class procedure TdwsGuardianThread.Initialize;
begin
   vThread:=TdwsGuardianThread.Create;
   vThread.Start;
end;

// Finalize
//
class procedure TdwsGuardianThread.Finalize;
begin
   if vThread<>nil then begin
      vThread.Terminate;
      vThread.FEvent.SetEvent;
      vThread.WaitFor;
      vThread.Destroy;
      vThread:=nil;
   end;
end;

// GuardExecution
//
class procedure TdwsGuardianThread.GuardExecution(const exec : IdwsProgramExecution; aMilliSecToLive : Integer);
var
   thread : TdwsGuardianThread;
   item : TdwsGuardedExecution;
begin
   thread:=vThread;
   item:=TdwsGuardedExecution.Create;
   item.Exec:=exec;
   item.TimeOutAt:=Now+aMilliSecToLive*(1/(86400*1000));
   thread.FExecutionsLock.Enter;
   try
      thread.FExecutions.Add(item);
      thread.FEvent.SetEvent;
   finally
      thread.FExecutionsLock.Leave;
   end;
end;

// ForgetExecution
//
class procedure TdwsGuardianThread.ForgetExecution(const exec : IdwsProgramExecution);
var
   i : Integer;
   thread : TdwsGuardianThread;
   execs : TdwsGuardedExecutionList;
   guarded : TdwsGuardedExecution;
begin
   thread:=vThread;
   thread.FExecutionsLock.Enter;
   try
      execs:=thread.FExecutions;
      for i:=0 to execs.Count-1 do begin
         guarded:=execs[i];
         if guarded.Exec=exec then begin
            guarded.Exec:=nil;
            Break;
         end;
      end;
   finally
      thread.FExecutionsLock.Leave;
   end;
end;

// Execute
//
procedure TdwsGuardianThread.Execute;
var
   currentTime : TDateTime;
   item : TdwsGuardedExecution;
   n, millisecs : Integer;
   timeLeft : Double;
begin
   while not Terminated do begin

      currentTime:=Now;
      item:=nil;

      FExecutionsLock.Enter;
      try
         while FExecutions.Count>0 do begin
            n:=FExecutions.Count;
            item:=FExecutions[n-1];
            if item.Exec=nil then begin
               FExecutions.ExtractAt(n-1);
               item.Free;
            end else begin
               if item.TimeOutAt<=currentTime then begin
                  item.Exec.Stop;
                  FExecutions.ExtractAt(n-1);
                  item.Free;
               end else Break;
            end;
         end;
      finally
         FExecutionsLock.Leave;
      end;

      if item=nil then
         FEvent.WaitFor(INFINITE)
      else begin
         timeLeft:=item.TimeOutAt-currentTime;
         if timeLeft<0 then
            millisecs:=0
         else millisecs:=Round(timeLeft*(86400*1000));
         FEvent.WaitFor(millisecs);
      end;

   end;
end;

// ------------------
// ------------------ TProgramExpr ------------------
// ------------------

// IsConstant
//
function TProgramExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// Optimize
//
function TProgramExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
end;

// GetType
//
function TProgramExpr.GetType : TTypeSymbol;
begin
   Result:=nil;
end;

// GetBaseType
//
function TProgramExpr.GetBaseType : TTypeSymbol;
begin
   Result:=nil;
end;

// EvalAsVariant
//
procedure TProgramExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
begin
   Result:=Eval(exec);
end;

// EvalAsScriptObj
//
procedure TProgramExpr.EvalAsScriptObj(exec : TdwsExecution; var Result : IScriptObj);
begin
   Result:=IScriptObj(IUnknown(Eval(exec)));
end;

// AssignValue
//
procedure TProgramExpr.AssignValue(exec : TdwsExecution; const value : Variant);
begin
   raise EScriptError.CreateFmt('Cannot assign to %s', [ClassName]);
end;

// AssignValueAsInteger
//
procedure TProgramExpr.AssignValueAsInteger(exec : TdwsExecution; const value : Int64);
begin
   AssignValue(exec, value);
end;

// AssignValueAsBoolean
//
procedure TProgramExpr.AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean);
begin
   AssignValue(exec, value);
end;

// AssignValueAsFloat
//
procedure TProgramExpr.AssignValueAsFloat(exec : TdwsExecution; const value : Double);
begin
   AssignValue(exec, value);
end;

// AssignValueAsString
//
procedure TProgramExpr.AssignValueAsString(exec : TdwsExecution; const value: String);
begin
   AssignValue(exec, value);
end;

// AssignValueAsScriptObj
//
procedure TProgramExpr.AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj);
begin
   AssignValue(exec, value);
end;

// EvalAsInteger
//
function TProgramExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=v;
   except
      // workaround for RTL bug that will sometimes report a failed cast to Int64
      // as being a failed cast to Boolean
      on E : EVariantError do begin
         raise EdwsVariantTypeCastError.Create(v, 'Integer', E);
      end else raise;
   end;
end;

// EvalAsBoolean
//
function TProgramExpr.EvalAsBoolean(exec : TdwsExecution) : Boolean;
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=v;
   except
      // standardize RTL message
      on E : EVariantError do begin
         raise EdwsVariantTypeCastError.Create(v, 'Boolean', E);
      end else raise;
   end;
end;

// EvalAsFloat
//
function TProgramExpr.EvalAsFloat(exec : TdwsExecution) : Double;
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=v;
   except
      // standardize RTL message
      on E : EVariantError do begin
         raise EdwsVariantTypeCastError.Create(v, 'Float', E);
      end else raise;
   end;
end;

// EvalAsString
//
procedure TProgramExpr.EvalAsString(exec : TdwsExecution; var Result : String);
var
   v : Variant;
begin
   v:=Eval(exec);
   try
      Result:=String(v);
   except
      // standardize RTL message
      on E : EVariantError do begin
         raise EdwsVariantTypeCastError.Create(v, 'String', E);
      end else raise;
   end;
end;

// EvalNoResult
//
procedure TProgramExpr.EvalNoResult(exec : TdwsExecution);
begin
   Eval(exec);
end;

// RaiseScriptError
//
procedure TProgramExpr.RaiseScriptError(exec : TdwsExecution; e : EScriptError);
begin
   e.ScriptPos:=ScriptPos;
   e.ScriptCallStack:=exec.GetCallStack;
   raise e;
end;

// RaiseScriptError
//
procedure TProgramExpr.RaiseScriptError(exec : TdwsExecution);
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
procedure TProgramExpr.RaiseScriptError(exec : TdwsExecution; const msg : String);
begin
   RaiseScriptError(exec, EScriptError, msg);
end;

// RaiseScriptError
//
procedure TProgramExpr.RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass; const msg : String);
begin
   RaiseScriptError(exec, exceptClass.Create(msg));
end;

// RaiseScriptError
//
procedure TProgramExpr.RaiseScriptError(exec : TdwsExecution; exceptClass : EScriptErrorClass;
                                        const msg : String; const args : array of const);
begin
   RaiseScriptError(exec, exceptClass.CreateFmt(msg, args));
end;

// RaiseUpperExceeded
//
procedure TProgramExpr.RaiseUpperExceeded(exec : TdwsExecution; index : Integer);
begin
   RaiseScriptError(exec, EScriptOutOfBounds.CreateFmt(RTE_ArrayUpperBoundExceeded, [index]));
end;

// RaiseLowerExceeded
//
procedure TProgramExpr.RaiseLowerExceeded(exec : TdwsExecution; index : Integer);
begin
   RaiseScriptError(exec, EScriptOutOfBounds.CreateFmt(RTE_ArrayLowerBoundExceeded, [index]));
end;

// CheckScriptObject
//
procedure TProgramExpr.CheckScriptObject(exec : TdwsExecution; const scriptObj : IScriptObj);
begin
   if scriptObj=nil then
      RaiseObjectNotInstantiated(exec)
   else if scriptObj.Destroyed then
      RaiseObjectAlreadyDestroyed(exec);
end;

// RaiseObjectNotInstantiated
//
procedure TProgramExpr.RaiseObjectNotInstantiated(exec : TdwsExecution);
begin
   RaiseScriptError(exec, EScriptError, RTE_ObjectNotInstantiated);
end;

// RaiseObjectAlreadyDestroyed
//
procedure TProgramExpr.RaiseObjectAlreadyDestroyed(exec : TdwsExecution);
begin
   RaiseScriptError(exec, EScriptError, RTE_ObjectAlreadyDestroyed);
end;

// ScriptLocation
//
function TProgramExpr.ScriptLocation(prog : TObject) : String;
begin
   if prog is TdwsProcedure then
      Result:=TdwsProcedure(prog).Func.QualifiedName+ScriptPos.AsInfo
   else Result:=ScriptPos.AsInfo;
end;

// ------------------
// ------------------ TProgramExpr ------------------
// ------------------

// OptimizeToTypedExpr
//
function TTypedExpr.OptimizeToTypedExpr(prog : TdwsProgram; exec : TdwsExecution; const hotPos : TScriptPos) : TTypedExpr;
var
   optimized : TProgramExpr;
begin
   try
      optimized:=Optimize(prog, exec);
      Assert(optimized is TTypedExpr);
      Result:=TTypedExpr(optimized);
   except
      on E: Exception do
         raise ECompileException.CreateFromException(hotPos, E)
   end;
end;

// OptimizeToFloatConstant
//
function TTypedExpr.OptimizeToFloatConstant(prog : TdwsProgram; exec : TdwsExecution) : TTypedExpr;
begin
   if IsConstant then begin
      if Typ.IsOfType(prog.TypInteger) or Typ.IsOfType(prog.TypFloat) then begin
         Result:=TConstFloatExpr.CreateUnified(prog, nil, EvalAsFloat(exec));
         Free;
      end else Result:=OptimizeToTypedExpr(prog, exec, ScriptPos);
   end else Result:=Self;
end;

// ScriptPos
//
function TTypedExpr.ScriptPos : TScriptPos;
begin
   Result:=cNullPos;
end;

// CheckInterface
//
procedure TTypedExpr.CheckInterface(exec : TdwsExecution; const scriptObj : IScriptObj);
begin
   if scriptObj=nil then
      RaiseInterfaceIsNil(exec)
end;

// RaiseInterfaceIsNil
//
procedure TTypedExpr.RaiseInterfaceIsNil(exec : TdwsExecution);
begin
   RaiseScriptError(exec, EScriptError, RTE_IntfIsNil);
end;

// IsOfType
//
function TTypedExpr.IsOfType(typSym : TTypeSymbol) : Boolean;
begin
   Result:=    (Self<>nil) and (Typ<>nil)
           and (Typ.IsOfType(typSym));
end;

// GetBaseType
//
function TTypedExpr.GetBaseType : TTypeSymbol;
begin
   if Assigned(Typ) then
      Result:=Typ.BaseType
   else Result:=nil;
end;

// GetType
//
function TTypedExpr.GetType : TTypeSymbol;
begin
   Result:=FTyp;
end;

// ------------------
// ------------------ TTypeReferenceExpr ------------------
// ------------------

// Create
//
constructor TTypeReferenceExpr.Create(aTyp : TTypeSymbol; const scriptPos : TScriptPos);
begin
   inherited Create;
   Typ:=aTyp;
   FPos:=scriptPos;
end;

// Eval
//
function TTypeReferenceExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Assert(False);
end;

// ScriptPos
//
function TTypeReferenceExpr.ScriptPos : TScriptPos;
begin
   Result:=FPos;
end;

// ------------------
// ------------------ TPosDataExpr ------------------
// ------------------

// Create
//
constructor TPosDataExpr.Create(Prog: TdwsProgram; const scriptPos : TScriptPos; Typ: TTypeSymbol);
begin
   inherited Create(Prog, Typ);
   FPos:=scriptPos;
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

// Eval
//
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
function TNoResultExpr.OptimizeToNoResultExpr(prog : TdwsProgram; exec : TdwsExecution) : TNoResultExpr;
var
   optimized : TProgramExpr;
begin
   optimized:=Optimize(prog, exec);
   Assert(optimized is TNoResultExpr);
   Result:=TNoResultExpr(optimized);
end;

// InterruptsFlow
//
function TNoResultExpr.InterruptsFlow : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TNoResultPosExpr ------------------
// ------------------

// Create
//
constructor TNoResultPosExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos);
begin
   FScriptPos:=Pos;
end;

// ScriptPos
//
function TNoResultPosExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// SetScriptPos
//
procedure TNoResultPosExpr.SetScriptPos(const aPos : TScriptPos);
begin
   FScriptPos:=aPos;
end;

// ------------------
// ------------------ TNullExpr ------------------
// ------------------

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
procedure TBlockExprBase.AddStatement(expr : TNoResultExpr);
begin
   ReallocMem(FStatements, (FCount+1)*SizeOf(TNoResultExpr));
   FStatements[FCount]:=expr;
   Inc(FCount);
end;

// ReplaceStatement
//
procedure TBlockExprBase.ReplaceStatement(index : Integer; expr : TNoResultExpr);
begin
   FStatements[index].Free;
   FStatements[index]:=expr;
end;

// ExtractStatement
//
function TBlockExprBase.ExtractStatement(index : Integer) : TNoResultExpr;
begin
   Result:=FStatements[index];
   if index<FCount-1 then
      Move(FStatements[index+1], FStatements[index], (FCount-index-1)*SizeOf(TNoResultExpr));
   Dec(FCount);
end;

// GetSubExpr
//
function TBlockExprBase.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FStatements[i];
end;

// GetSubExprCount
//
function TBlockExprBase.GetSubExprCount : Integer;
begin
   Result:=FCount;
end;

// ------------------
// ------------------ TBlockRawExpr ------------------
// ------------------

// EvalNoResult
//
procedure TBlockRawExpr.EvalNoResult(exec : TdwsExecution);
var
   i : Integer;
   expr : PNoResultExpr;
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

constructor TDataExpr.Create(Prog: TdwsProgram; Typ: TTypeSymbol);
begin
   FTyp := Typ;
end;

// Eval
//
function TDataExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=Data[exec][Addr[exec]];
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
  DWSCopyData(SourceData, SourceAddr, Data[exec], Addr[exec], Typ.Size);
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

// AssignValueAsScriptObj
//
procedure TDataExpr.AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj);
begin
   AssignValue(exec, value);
end;

// AssignExpr
//
procedure TDataExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   Expr.EvalAsVariant(exec, Data[exec][Addr[exec]]);
end;

procedure TDataExpr.AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr);
begin
   DWSCopyData(DataExpr.Data[exec], DataExpr.Addr[exec], Data[exec], Addr[exec], Typ.Size);
end;

// GetDataPtr
//
function TDataExpr.GetDataPtr(exec : TdwsExecution) : TDataPtr;
begin
   Result:=TDataPtr.Create(Data[exec], Addr[exec]);
end;

// ------------------
// ------------------ TFuncExprBase ------------------
// ------------------

// Create
//
constructor TFuncExprBase.Create(prog : TdwsProgram; const pos : TScriptPos; aFunc : TFuncSymbol);
begin
   inherited Create(Prog, Pos, nil);
   FFunc:=aFunc;
   if Assigned(aFunc) then
      FTyp:=aFunc.Typ;
end;

// Destroy
//
destructor TFuncExprBase.Destroy;
begin
   FArgs.Clean;
   inherited;
end;

// AddArg
//
procedure TFuncExprBase.AddArg(arg : TTypedExpr);
begin
   FArgs.Add(arg);
end;

// ClearArgs
//
procedure TFuncExprBase.ClearArgs;
begin
   FArgs.Clean;
end;

// Optimize
//
function TFuncExprBase.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   Result:=Self;
   if IsConstant then begin
      Initialize(prog);
      try
         if (Typ=nil) or (Typ.Size<=1) then
            Result:=TConstExpr.CreateTypedVariantValue(prog, typ, Eval(exec))
         else begin
            exec.Stack.Push(prog.DataSize);
            try
               Eval(exec);
               Result:=TConstExpr.CreateTyped(prog, typ, exec.Stack.Data, FResultAddr);
            finally
               exec.Stack.Pop(prog.DataSize);
            end;
         end;
      except
         on E: EScriptError do begin
            Prog.CompileMsgs.AddCompilerErrorFmt(E.ScriptPos, CPE_FunctionOptimizationFailed,
                                                 [FuncSym.Name, E.RawClassName, E.Message],
                                                  TCompilerErrorMessage);
         end;
         on E: Exception do begin
            Prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_FunctionOptimizationFailed,
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
   if (FuncSym<>nil) and (not FuncSym.IsStateless) then Exit(False);

   for i:=0 to FArgs.Count-1 do
      if not TTypedExpr(FArgs.ExprBase[i]).IsConstant then
         Exit(False);

   Result:=True;
end;

// SetResultAddr
//
procedure TFuncExprBase.SetResultAddr(prog : TdwsProgram; exec : TdwsExecution; ResultAddr: Integer = -1);
begin
   if ResultAddr=-1 then begin
      if (exec=nil) or (exec.ProgramState = psUndefined) then
         FResultAddr:=prog.GetTempAddr(FTyp.Size)
      else FResultAddr:=-1; // TFuncExpr.Create called from TInfoFunc.Call
   end else FResultAddr:=ResultAddr;
end;

// GetSubExpr
//
function TFuncExprBase.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FArgs.ExprBase[i];
end;

// GetSubExprCount
//
function TFuncExprBase.GetSubExprCount : Integer;
begin
   Result:=FArgs.Count;
end;

// ChangeFuncSymbol
//
function TFuncExprBase.ChangeFuncSymbol(aProg: TdwsProgram; newFuncSym : TFuncSymbol) : TFuncExprBase;
begin
   if (FuncSym is TMagicFuncSymbol) or (newFuncSym is TMagicFuncSymbol) then begin
      Result:=TMagicFuncExpr.CreateMagicFuncExpr(aProg, ScriptPos, TMagicFuncSymbol(newFuncSym));
      Result.Args.Assign(Args);
      Args.Clear;
      TMagicFuncExpr(Result).FResultAddr:=FResultAddr;
      Free;
   end else begin
      FFunc:=newFuncSym;
      if Assigned(newFuncSym) then begin
         // don't update type for a constructor as the "return type" of a constructor
         // isn't specified by the constructor symbol, but by the meta the constructor
         // is invoked upon
         if newFuncSym.Kind<>fkConstructor then
            FTyp:=newFuncSym.Typ
      end else FTyp:=nil;
      Result:=Self;
   end
end;

// Initialize
//
procedure TFuncExprBase.Initialize(prog : TdwsProgram);
begin
   if Assigned(FFunc) and Assigned(FFunc.Executable) then
      FFunc.Executable.InitExpression(Self);
end;

// GetArgType
//
function TFuncExprBase.GetArgType(idx : Integer) : TTypeSymbol;
begin
   if Cardinal(idx)<Cardinal(FArgs.Count) then
      Result:=TTypedExpr(FArgs[idx]).Typ
   else Result:=nil;
end;

// ------------------
// ------------------ TPushOperator ------------------
// ------------------

// InitPushAddr
//
procedure TPushOperator.InitPushAddr(stackAddr: Integer; argExpr: TTypedExpr);
begin
   if argExpr is TVarParamExpr then
      FTypeParamSym:=TSymbol(potPassAddr)
   else FTypeParamSym:=TSymbol(potAddr);
   FStackAddr:=stackAddr;
   FArgExpr:=argExpr;
end;

// InitPushTempAddr
//
procedure TPushOperator.InitPushTempAddr(stackAddr: Integer; argExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potTempAddr);
   FStackAddr:=stackAddr;
   FArgExpr:=argExpr;
end;

// InitPushTempArrayAddr
//
procedure TPushOperator.InitPushTempArrayAddr(stackAddr: Integer; argExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potTempArrayAddr);
   FStackAddr:=stackAddr;
   FArgExpr:=TArrayConstantExpr(argExpr);
end;

// InitPushTempArray
//
procedure TPushOperator.InitPushTempArray(stackAddr: Integer; argExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potTempArray);
   FStackAddr:=stackAddr;
   FArgExpr:=argExpr as TConstParamExpr;
end;

// InitPushResult
//
procedure TPushOperator.InitPushResult(prog : TdwsProgram; stackAddr : Integer; argExpr : TTypedExpr);
var
   argTyp : TTypeSymbol;
begin
   argTyp:=argExpr.Typ;
   if argTyp.IsOfType(prog.TypInteger) then
      FTypeParamSym:=TSymbol(potResultInteger)
   else if argTyp.IsOfType(prog.TypFloat) then
      FTypeParamSym:=TSymbol(potResultFloat)
   else if argTyp.IsOfType(prog.TypBoolean) then
      FTypeParamSym:=TSymbol(potResultBoolean)
   else if argTyp.IsOfType(prog.TypString) then
      if ArgExpr.InheritsFrom(TConstStringExpr) then
         FTypeParamSym:=TSymbol(potResultConstString)
      else FTypeParamSym:=TSymbol(potResultString)
   else FTypeParamSym:=TSymbol(potResult);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// InitPushData
//
procedure TPushOperator.InitPushData(StackAddr: Integer; ArgExpr: TTypedExpr; ParamSym: TSymbol);
begin
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
   FTypeParamSym:=ParamSym;
end;

// InitPushInitResult
//
procedure TPushOperator.InitPushInitResult(StackAddr: Integer; ArgExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potInitResult);
   FStackAddr:=StackAddr;
   FArgExpr:=TTypedExpr((ArgExpr as TFuncExpr).Typ); // dirty hack
end;

// InitPushLazy
//
procedure TPushOperator.InitPushLazy(StackAddr: Integer; ArgExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potLazy);
   FStackAddr:=StackAddr;
   FArgExpr:=ArgExpr;
end;

// Execute
//
procedure TPushOperator.Execute(exec : TdwsExecution);
begin
   case NativeInt(FTypeParamSym) of
      NativeInt(potAddr) : ExecuteAddr(exec);
      NativeInt(potPassAddr) : ExecutePassAddr(exec);
      NativeInt(potTempAddr) : ExecuteTempAddr(exec);
      NativeInt(potTempArrayAddr) : ExecuteTempArrayAddr(exec);
      NativeInt(potTempArray) : ExecuteTempArray(exec);
      NativeInt(potResultBoolean) : ExecuteResultBoolean(exec);
      NativeInt(potResultInteger) : ExecuteResultInteger(exec);
      NativeInt(potResultFloat) : ExecuteResultFloat(exec);
      NativeInt(potResultString) : ExecuteResultString(exec);
      NativeInt(potResultConstString) : ExecuteResultConstString(exec);
      NativeInt(potResult) : ExecuteResult(exec);
      NativeInt(potInitResult) : ExecuteInitResult(exec);
      NativeInt(potLazy) : ExecuteLazy(exec);
   else
      ExecuteData(exec);
   end;
end;

type

   TVarParamData = class (TInterfacedObject, IVarParamData)
      private
         FData : TData;
         FAddr : Integer;
      protected
         function GetData : TData;
         function GetAddr : Integer;
      public
         constructor Create(const Data: TData; Addr: Integer);
         function Eval : PVariant;
  end;

{ TVarParamData }

constructor TVarParamData.Create(const Data: TData; Addr: Integer);
begin
   inherited Create;
   FData:=Data;
   FAddr:=Addr;
end;

// Eval
//
function TVarParamData.Eval : PVariant;
begin
   Result:=@FData[FAddr];
end;

function TVarParamData.GetAddr : Integer;
begin
   Result:=FAddr;
end;

function TVarParamData.GetData : TData;
begin
  Result := FData;
end;

// ExecuteAddr
//
procedure TPushOperator.ExecuteAddr(exec : TdwsExecution);
var
   vpd : IVarParamData;
begin
   vpd:=TVarParamData.Create(TDataExpr(FArgExpr).Data[exec], TDataExpr(FArgExpr).Addr[exec]);
   exec.Stack.WriteValue(exec.Stack.StackPointer + FStackAddr, vpd);
end;

// ExecutePassAddr
//
procedure TPushOperator.ExecutePassAddr(exec : TdwsExecution);
var
   vpd : IVarParamData;
begin
   TVarParamExpr(FArgExpr).GetVarParamData(exec, vpd);
   exec.Stack.WriteValue(exec.Stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteTempAddr
//
procedure TPushOperator.ExecuteTempAddr(exec : TdwsExecution);
var
   vpd : IVarParamData;
   data : TData;
begin
   SetLength(data, 1);
   FArgExpr.EvalAsVariant(exec, data[0]);
   vpd:=TVarParamData.Create(data, 0);
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
var
   buf : Variant;
begin
   FArgExpr.EvalAsVariant(exec, buf);
   exec.Stack.WriteValue(exec.Stack.StackPointer + FStackAddr, buf);
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
var
   dataExpr : TDataExpr;
begin
   dataExpr:=TDataExpr(FArgExpr);
   exec.Stack.WriteData(dataExpr.Addr[exec], exec.Stack.StackPointer+FStackAddr,
                        FTypeParamSym.Typ.Size, dataExpr.Data[exec]);
end;

// ExecuteInitResult
//
procedure TPushOperator.ExecuteInitResult(exec : TdwsExecution);
begin
   TTypeSymbol(FArgExpr).InitData(exec.Stack.Data, exec.Stack.StackPointer+FStackAddr);
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

// Create
//
constructor TFuncExpr.Create(prog : TdwsProgram; const scriptPos : TScriptPos; func : TFuncSymbol);
begin
   inherited Create(Prog, scriptPos, Func);
   FCallerID:=Self;
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

// ExpectedArg
//
function TFuncExpr.ExpectedArg : TParamSymbol;
begin
(*   if FFunc.IsOverloaded then
      Result:=nil
   else*) if FArgs.Count<FFunc.Params.Count then
      Result:=(FFunc.Params[FArgs.Count] as TParamSymbol)
   else Result:=nil;
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
         TdwsProgramExecution(exec).EnterRecursion(CallerID);
         try
            ICallable(func.Executable).Call(TdwsProgramExecution(exec), func);
         finally
            TdwsProgramExecution(exec).LeaveRecursion;
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
   arg : TTypedExpr;
   param : TParamSymbol;
   pushOperator : PPushOperator;
begin
   if Assigned(FFunc.Result) then
      FPushExprsCount:=FArgs.Count+1
   else FPushExprsCount:=FArgs.Count;
   ReallocMem(FPushExprs, FPushExprsCount*SizeOf(TPushOperator));

   for i:=0 to FArgs.Count-1 do begin
      pushOperator:=@FPushExprs[i];
      arg:=TTypedExpr(FArgs.ExprBase[i]);
      param:=TParamSymbol(FFunc.Params[i]);
      if param.InheritsFrom(TLazyParamSymbol) then
         pushOperator.InitPushLazy(param.StackAddr, arg)
      else if arg is TDataExpr then begin
         if param.Typ is TOpenArraySymbol then begin
            if arg.ClassType=TArrayConstantExpr then
               pushOperator.InitPushTempArrayAddr(param.StackAddr, arg)
            else pushOperator.InitPushTempArray(param.StackAddr, arg);
         end else if param is TByRefParamSymbol then begin
            if arg is TFuncExprBase then
               pushOperator.InitPushTempAddr(param.StackAddr, arg)
            else pushOperator.InitPushAddr(param.StackAddr, arg);
         end else if param.Size>1 then
            pushOperator.InitPushData(param.StackAddr, TDataExpr(arg), param)
         else pushOperator.InitPushResult(prog, param.StackAddr, arg)
      end else begin
         if param.InheritsFrom(TByRefParamSymbol) then
            pushOperator.InitPushTempAddr(param.StackAddr, arg)
         else pushOperator.InitPushResult(prog, param.StackAddr, arg);
      end;
   end;

   if Assigned(FFunc.Result) then
      FPushExprs[FArgs.Count].InitPushInitResult(FFunc.Result.StackAddr, Self);
end;

// Initialize
//
procedure TFuncExpr.Initialize(prog : TdwsProgram);
begin
   inherited;
   AddPushExprs(prog);
end;

// IsWritable
//
function TFuncExpr.IsWritable : Boolean;
begin
   Result:=False;
end;

// ------------------
// ------------------ TFuncPointer ------------------
// ------------------

// Create
//
constructor TFuncPointer.Create(exec : TdwsExecution; funcExpr : TFuncExprBase);
var
   prog : TdwsMainProgram;
   baseExpr : TTypedExpr;
   scriptObj : IScriptObj;
   classSym : TClassSymbol;
   magicFuncSym : TMagicFuncSymbol;
   baseTyp : TTypeSymbol;
begin
   prog:=(exec as TdwsProgramExecution).Prog;
   if funcExpr is TMethodExpr then begin

      baseExpr:=TMethodExpr(funcExpr).BaseExpr;
      baseTyp:=baseExpr.Typ.UnAliasedType;
      if baseTyp is TClassOfSymbol then begin
         classSym:=TClassSymbol(baseExpr.EvalAsInteger(exec));
         FFuncExpr:=CreateFuncExpr(prog, funcExpr.FuncSym, nil, classSym);
      end else begin
         baseExpr.EvalAsScriptObj(exec, scriptObj);
         if baseTyp is TInterfaceSymbol then begin
            FFuncExpr:=CreateFuncExpr(prog, funcExpr.FuncSym, scriptObj, (scriptObj.InternalObject as TScriptInterface).Typ);
         end else begin
            FFuncExpr:=CreateFuncExpr(prog, funcExpr.FuncSym, scriptObj, scriptObj.ClassSym);
         end;
      end;

   end else if funcExpr is TMagicFuncExpr then begin

      magicFuncSym:=funcExpr.FuncSym as TMagicFuncSymbol;
      FFuncExpr:=TMagicFuncExpr.CreateMagicFuncExpr(prog, cNullPos, magicFuncSym);

   end else begin

      FFuncExpr:=CreateFuncExpr(prog, funcExpr.FuncSym, nil, nil);

   end;
end;

// Destroy
//
destructor TFuncPointer.Destroy;
begin
   inherited;
   FFuncExpr.Free;
end;

// GetFuncExpr
//
function TFuncPointer.GetFuncExpr : TFuncExprBase;
begin
   Result:=FFuncExpr;
end;

// SameFunc
//
function TFuncPointer.SameFunc(const v : Variant) : Boolean;
var
   ptr : IFuncPointer;
   expr : TFuncExprBase;
   c1, c2 : TConstExpr;
begin
   ptr:=IFuncPointer(IUnknown(v));
   if ptr=nil then
      Exit(FFuncExpr=nil);
   expr:=ptr.GetFuncExpr;
   Result:=    (expr.ClassType=FFuncExpr.ClassType)
           and (expr.FuncSym=FFuncExpr.FuncSym);
   if Result and (FFuncExpr is TMethodExpr) then begin
      c1:=TMethodExpr(FFuncExpr).BaseExpr as TConstExpr;
      c2:=TMethodExpr(expr).BaseExpr as TConstExpr;
      Result:=c1.SameValueAs(c2);
   end;
end;

// ------------------
// ------------------ TAnonymousFuncRefExpr ------------------
// ------------------

// Create
//
constructor TAnonymousFuncRefExpr.Create(prog : TdwsProgram; funcExpr : TFuncExprBase);
begin
   FFuncExpr:=funcExpr;
   Typ:=funcExpr.FuncSym;
end;

// Destroy
//
destructor TAnonymousFuncRefExpr.Destroy;
begin
   inherited;
   FFuncExpr.Free;
end;

// Extract
//
function TAnonymousFuncRefExpr.Extract : TFuncExprBase;
begin
   Result:=FFuncExpr;
   FFuncExpr:=nil;
   Free;
end;

// Eval
//
function TAnonymousFuncRefExpr.Eval(exec : TdwsExecution) : Variant;
var
   funcPtr : TFuncPointer;
begin
   if FFuncExpr is TFuncPtrExpr then
      TFuncPtrExpr(FFuncExpr).FCodeExpr.EvalAsVariant(exec, Result)
   else begin
      funcPtr:=TFuncPointer.Create(exec, FFuncExpr);
      Result:=IFuncPointer(funcPtr);
   end;
end;

// GetSubExpr
//
function TAnonymousFuncRefExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FFuncExpr
end;

// GetSubExprCount
//
function TAnonymousFuncRefExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// GetData
//
function TAnonymousFuncRefExpr.GetData(exec : TdwsExecution) : TData;
begin
   SetLength(Result, 1);
   EvalAsVariant(exec, Result[0]);
end;

// ------------------
// ------------------ TFuncPtrExpr ------------------
// ------------------

// Create
//
constructor TFuncPtrExpr.Create(prog : TdwsProgram; const pos : TScriptPos; codeExpr : TDataExpr);
begin
   inherited Create(prog, pos, (codeExpr.Typ as TFuncSymbol));
   FCodeExpr:=codeExpr;
end;

// Destroy
//
destructor TFuncPtrExpr.Destroy;
begin
   inherited;
   FCodeExpr.Free;
end;

// Extract
//
function TFuncPtrExpr.Extract : TDataExpr;
begin
   Result:=FCodeExpr;
   FCodeExpr:=nil;
   Free;
end;

// Eval
//
function TFuncPtrExpr.Eval(exec : TdwsExecution) : Variant;
var
   funcPointer : IFuncPointer;
   funcExprBase : TFuncExprBase;
   funcExpr : TFuncExpr;
   oldArgs : TExprBaseListRec;
   i : Integer;
   val : Variant;
begin
   FCodeExpr.EvalAsVariant(exec, val);
   funcPointer:=IFuncPointer(IUnknown(val));
   if funcPointer=nil then
      RaiseScriptError(exec, EScriptError, RTE_FuncPointerIsNil);
   funcExprBase:=funcPointer.GetFuncExpr;

   if funcExprBase is TMagicFuncExpr then begin

      oldArgs:=funcExprBase.FArgs;
      funcExprBase.FArgs:=FArgs;
      try
         funcExprBase.EvalAsVariant(exec, Result);
      finally
         funcExprBase.FArgs:=oldArgs;
      end;

   end else begin

      Assert(funcExprBase is TFuncExpr);

      funcExpr:=TFuncExpr(funcExprBase);

      funcExpr.ClearArgs;
      for i:=0 to FArgs.Count-1 do
         funcExpr.AddArg(FArgs.ExprBase[i] as TTypedExpr);
      funcExpr.AddPushExprs((exec as TdwsProgramExecution).Prog);
      funcExpr.CallerID:=Self;

      try
         funcExprBase.EvalAsVariant(exec, Result);
      finally
         for i:=0 to FArgs.Count-1 do
            funcExpr.FArgs.ExprBase[i]:=nil;
      end;

   end;
end;

// IsConstant
//
function TFuncPtrExpr.IsConstant : Boolean;
begin
   Result:=False;
end;

// GetSubExpr
//
function TFuncPtrExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FCodeExpr
   else Result:=inherited GetSubExpr(i-1);
end;

// GetSubExprCount
//
function TFuncPtrExpr.GetSubExprCount : Integer;
begin
   Result:=1+inherited GetSubExprCount;
end;

// ------------------
// ------------------ TTypedExprList ------------------
// ------------------

// Destroy
//
destructor TTypedExprList.Destroy;
begin
   FList.Clean;
   FDefaultExpected.Free;
   inherited;
end;

procedure TTypedExprList.AddExpr(expr : TTypedExpr);
begin
   FList.Add(expr);
end;

// ExpectedArg
//
function TTypedExprList.ExpectedArg : TParamSymbol;
begin
   if (FTable<>nil) and (FList.Count<FTable.Count) then
      Result:=FTable[FList.Count] as TParamSymbol
   else Result:=FDefaultExpected;
end;

// Insert0
//
procedure TTypedExprList.Insert0(expr : TExprBase);
begin
   FList.Insert(0, expr);
end;

// Delete
//
procedure TTypedExprList.Delete(index : Integer);
begin
   FList.Delete(index);
end;

// Clear
//
procedure TTypedExprList.Clear;
begin
   FList.Clear;
end;

function TTypedExprList.GetExpr(const x: Integer): TTypedExpr;
begin
  Result := TTypedExpr(FList.List[x]);
end;

procedure TTypedExprList.SetExpr(const x: Integer; const Value: TTypedExpr);
begin
  FList.List[x] := Value;
end;

// GetCount
//
function TTypedExprList.GetCount : Integer;
begin
   Result:=FList.Count;
end;

// ------------------
// ------------------ TBinaryOpExpr ------------------
// ------------------

constructor TBinaryOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
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

// IsConstant
//
function TBinaryOpExpr.IsConstant : Boolean;
begin
   Result:=FLeft.IsConstant and FRight.IsConstant;
end;

// GetSubExpr
//
function TBinaryOpExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FLeft
   else Result:=FRight;
end;

// GetSubExprCount
//
function TBinaryOpExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TVariantBinOpExpr ------------------
// ------------------

// Create
//
constructor TVariantBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypVariant;
end;

// Eval
//
function TVariantBinOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalAsVariant(exec, Result);
end;

// Optimize
//
function TVariantBinOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if IsConstant then begin
      Result:=TUnifiedConstExpr.CreateUnified(Prog, Prog.TypVariant, Eval(exec));
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TIntegerBinOpExpr ------------------
// ------------------

// Create
//
constructor TIntegerBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypInteger;
end;

// Eval
//
function TIntegerBinOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsInteger(exec);
end;

// EvalAsFloat
//
function TIntegerBinOpExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   Result:=EvalAsInteger(exec);
end;

// Optimize
//
function TIntegerBinOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if IsConstant then begin
      Result:=TConstIntExpr.CreateUnified(Prog, nil, EvalAsInteger(exec));
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TStringBinOpExpr ------------------
// ------------------

// Create
//
constructor TStringBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypString;
end;

// Eval
//
function TStringBinOpExpr.Eval(exec : TdwsExecution) : Variant;
var
   buf : String;
begin
   EvalAsString(exec, buf);
   Result:=buf;
end;

// Optimize
//
function TStringBinOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   buf : String;
begin
   if IsConstant then begin
      EvalAsString(exec, buf);
      Result:=TConstStringExpr.CreateUnified(Prog, nil, buf);
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TFloatBinOpExpr ------------------
// ------------------

// Create
//
constructor TFloatBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypFloat;
end;

// Eval
//
function TFloatBinOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsFloat(exec);
end;

// Optimize
//
function TFloatBinOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if IsConstant then begin
      Result:=TConstFloatExpr.CreateUnified(Prog, nil, EvalAsFloat(exec));
      Free;
   end else begin
      FLeft:=FLeft.OptimizeToFloatConstant(prog, exec);
      FRight:=FRight.OptimizeToFloatConstant(prog, exec);
      Result:=Self;
   end;
end;

// ------------------
// ------------------ TBooleanBinOpExpr ------------------
// ------------------

// Create
//
constructor TBooleanBinOpExpr.Create(Prog: TdwsProgram; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypBoolean;
end;

function TBooleanBinOpExpr.Eval(exec : TdwsExecution) : Variant;
begin
   Result:=EvalAsBoolean(exec);
end;

// Optimize
//
function TBooleanBinOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if IsConstant then begin
      Result:=TConstBooleanExpr.CreateUnified(Prog, nil, EvalAsBoolean(exec));
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TUnaryOpExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   FExpr:=Expr;
end;

// Destroy
//
destructor TUnaryOpExpr.Destroy;
begin
   FExpr.Free;
   inherited;
end;

// IsConstant
//
function TUnaryOpExpr.IsConstant : Boolean;
begin
   Result:=FExpr.IsConstant;
end;

// GetSubExpr
//
function TUnaryOpExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExpr;
end;

// GetSubExprCount
//
function TUnaryOpExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TUnaryOpBoolExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpBoolExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
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
constructor TUnaryOpIntExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
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

// Optimize
//
function TUnaryOpIntExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if IsConstant then begin
      Result:=TConstIntExpr.CreateUnified(Prog, Typ, EvalAsInteger(exec));
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TUnaryOpFloatExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpFloatExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
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

// Optimize
//
function TUnaryOpFloatExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
begin
   if IsConstant then begin
      Result:=TConstFloatExpr.CreateUnified(Prog, nil, EvalAsFloat(exec));
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TUnaryOpStringExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpStringExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   inherited;
   Typ:=Prog.TypString;
end;

// Eval
//
function TUnaryOpStringExpr.Eval(exec : TdwsExecution) : Variant;
var
   buf : String;
begin
   EvalAsString(exec, buf);
   Result:=buf;
end;

// ------------------
// ------------------ TUnaryOpVariantExpr ------------------
// ------------------

// Create
//
constructor TUnaryOpVariantExpr.Create(prog : TdwsProgram; expr : TTypedExpr);
begin
   inherited;
   Typ:=Prog.TypVariant;
end;

// Eval
//
function TUnaryOpVariantExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalAsVariant(exec, Result);
end;

// ------------------
// ------------------ TMethodExpr ------------------
// ------------------

// Create
//
constructor TMethodExpr.Create(Prog: TdwsProgram; const scriptPos: TScriptPos;
  Func: TMethodSymbol; BaseExpr: TTypedExpr);
begin
   inherited Create(Prog, scriptPos, Func);
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

// MethSym
//
function TMethodExpr.MethSym : TMethodSymbol;
begin
   Result:=TMethodSymbol(FuncSym);
end;

// ChangeFuncSymbol
//
function TMethodExpr.ChangeFuncSymbol(aProg: TdwsProgram; newFuncSym : TFuncSymbol) : TFuncExprBase;
var
   newMeth : TMethodSymbol;
   refKind : TRefKind;
begin
   newMeth:=(newFuncSym as TMethodSymbol);

   if BaseExpr.Typ is TStructuredTypeMetaSymbol then begin
      Assert(newMeth.IsClassMethod or (newMeth.Kind=fkConstructor));
      refKind:=rkClassOfRef;
   end else refKind:=rkObjRef;

   Result:=CreateMethodExpr(aProg, newMeth, Self.FBaseExpr, refKind, ScriptPos);
   Result.Args.Assign(Args);
   Self.FArgs.Clear;
   Self.Free;
end;

// GetSubExpr
//
function TMethodExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   case i of
      0 : Result:=FBaseExpr;
   else
      Result:=inherited GetSubExpr(i-1);
   end;
end;

// GetSubExprCount
//
function TMethodExpr.GetSubExprCount : Integer;
begin
   Result:=FArgs.Count+1;
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
var
   p : PIScriptObj;
begin
   p:=exec.SelfScriptObject;
   FBaseExpr.EvalAsScriptObj(exec, p^);
   if (p^<>nil) and p^.Destroyed then
      RaiseObjectAlreadyDestroyed(exec);
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FSelfAddr, p^);

   Result:=FFunc;
end;

// ------------------
// ------------------ TMethodInterfaceExpr ------------------
// ------------------

// PreCall
//
function TMethodInterfaceExpr.PreCall(exec : TdwsExecution) : TFuncSymbol;
var
   scriptObj : IScriptObj;
   intfObj : TScriptInterface;
begin
   FBaseExpr.EvalAsScriptObj(exec, scriptObj);
   CheckInterface(exec, scriptObj);
   intfObj:=TScriptInterface(scriptObj.InternalObject);
   exec.SelfScriptObject^:=intfObj.Instance;
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FSelfAddr, intfObj.Instance);
   Result:=intfObj.VMT[TMethodSymbol(FFunc).VMTIndex];
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
var
   p : PIScriptObj;
begin
   // Find virtual method
   p:=exec.SelfScriptObject;
   FBaseExpr.EvalAsScriptObj(exec, p^);
   CheckScriptObject(exec, p^);
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FSelfAddr, p^);
   Result:=FindVirtualMethod(p^.ClassSym);
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
      CheckScriptObject(exec, exec.SelfScriptObject^);
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
      RaiseScriptError(exec, RTE_ClassTypeIsNil);

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
   Func: TMethodSymbol; Base: TTypedExpr);
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
var
   classSym : TClassSymbol;
begin
   classSym:=TClassSymbol(BaseExpr.EvalAsInteger(exec));
   if classSym=nil then
      RaiseScriptError(exec, RTE_ClassTypeIsNil);

   // Create object
   exec.SelfScriptObject^:=TScriptObjInstance.Create(classSym, exec as TdwsProgramExecution);
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

// ------------------
// ------------------ TConstructorVirtualExpr ------------------
// ------------------

constructor TConstructorVirtualExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
   Func: TMethodSymbol; Base: TTypedExpr);
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
    RaiseScriptError(exec, RTE_InstanceOfAbstractClass);

  Result := FindVirtualMethod(classSym);

  // Create object
  exec.SelfScriptObject^ := TScriptObjInstance.Create(classSym, exec as TdwsProgramExecution);
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

function TProgramInfo.GetValueAsVariant(const s: String): Variant;
begin
  Result := GetVars(s).Value;
end;

function TProgramInfo.GetData(const s: String): TData;
begin
  Result := GetVars(s).Data;
end;

procedure TProgramInfo.SetValueAsVariant(const s: String; const Value: Variant);
begin
  GetVars(s).Value := Value;
end;

procedure TProgramInfo.SetData(const s: String; const Value: TData);
begin
  GetVars(s).Data := Value;
end;

function TProgramInfo.GetVars(const str : String): IInfo;

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
      if (sym.ClassType=TVarParamSymbol) or (sym.Typ is TOpenArraySymbol) then begin
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

function TProgramInfo.GetFunc(const s: String): IInfo;
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

function TProgramInfo.GetTemp(const DataType: String): IInfo;
var
  data: TData;
  typSym: TTypeSymbol;
begin
  typSym := FTable.FindTypeSymbol(DataType, cvMagic);

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

function TProgramInfo.GetValueAsString(const s: String): String;
begin
  Result:=GetVars(s).ValueAsString;
end;

procedure TProgramInfo.SetValueAsString(const s: String; const Value: String);
begin
  GetVars(s).Value:=Value;
end;

// GetValueAsChar
//
function TProgramInfo.GetValueAsChar(const s: String): WideChar;
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
function TProgramInfo.GetValueAsDataString(const s: String): RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetValueAsString(s));
end;

// SetValueAsDataString
//
procedure TProgramInfo.SetValueAsDataString(const s: String; const Value: RawByteString);
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

// SetResultAsString
//
procedure TProgramInfo.SetResultAsString(const value : String);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

// SetResultAsDataString
//
procedure TProgramInfo.SetResultAsDataString(const value : RawByteString);
begin
   SetResultAsString(RawByteStringToScriptString(value));
end;

// SetResultAsInteger
//
procedure TProgramInfo.SetResultAsInteger(const value : Int64);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

// SetResultAsBoolean
//
procedure TProgramInfo.SetResultAsBoolean(const value : Boolean);
begin
   GetVars(SYS_RESULT).Value:=value;
end;

// SetResultAsFloat
//
procedure TProgramInfo.SetResultAsFloat(const value : Double);
begin
   GetVars(SYS_RESULT).Value:=value;
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
   {$ifdef FPC}
   if p^.VType=varString then
      Result:=String(p.VString)
   {$else}
   if p^.VType=varUString then
      Result:=String(p.VUString)
   {$endif}
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

// GetParamAsObject
//
function TProgramInfo.GetParamAsObject(index : Integer) : TObject;
begin
   Result:=IScriptObj(IUnknown(GetParamAsPVariant(index)^)).ExternalObject;
end;

function TProgramInfo.FindClassMatch(AObject: TObject; ExactMatch: Boolean): TClassSymbol;
var
  {$ifdef FPC}
  ParentRTTI: PTypeInfo;
  {$else}
  ParentRTTI: PPTypeInfo;
  {$endif}
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
            {$ifdef FPC}
            ParentRTTI := GetTypeData(ParentRTTI)^.ParentInfo;
            {$else}
            ParentRTTI := GetTypeData(ParentRTTI^)^.ParentInfo;
            {$endif}
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
    NewScriptObj := TScriptObjInstance.Create(ClassSym, context);
    NewScriptObj.ExternalObject := AObject;
    Result := IScriptObj(NewScriptObj);
  end
  else                                     // no class returned or no object provided
    Result := Unassigned;                  // return 'nil' Id
end;

function TProgramInfo.GetExternalObjForVar(const s: String): TObject;
var
  sObj: IScriptObj;
begin
  sObj := Vars[s].ScriptObj;
  if Assigned(sObj) then
    Result := sObj.ExternalObject
  else
    Result := nil;
end;

function TProgramInfo.FindSymbolInUnits(AUnitList: TList; const Name: String): TSymbol;
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

function TProgramInfo.FindSymbolInUnits(const Name: String): TSymbol;
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
    if root.Symbols[i].ClassType=TUnitSymbol then        // if a unit symbol
      if Result.IndexOf(root.Symbols[i]) < 0 then // and not already in list (units may reuse others)
        Result.Add(root.Symbols[i]);
end;

// ------------------
// ------------------ TScriptObj ------------------
// ------------------

// GetClassSym
//
function TScriptObj.GetClassSym: TClassSymbol;
begin
   Result:=nil;
end;

// SetClassSym
//
procedure TScriptObj.SetClassSym(clsSym : TClassSymbol);
begin
   // ignore
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

// GetExternalObject
//
function TScriptObj.GetExternalObject: TObject;
begin
   Result:=nil;
end;

// SetExternalObject
//
procedure TScriptObj.SetExternalObject(Value: TObject);
begin
   Assert(False);
end;

// SetExecutionContext
//
procedure TScriptObj.SetExecutionContext(exec : TdwsProgramExecution);
begin
   // ignore
end;

// GetDestroyed
//
function TScriptObj.GetDestroyed : Boolean;
begin
   Result:=False;
end;

// SetDestroyed
//
procedure TScriptObj.SetDestroyed(const val : Boolean);
begin
   Assert(False);
end;

// GetInternalObject
//
function TScriptObj.GetInternalObject: TObject;
begin
   Result:=Self;
end;

// ------------------
// ------------------ TScriptObjInstance ------------------
// ------------------

// Create
//
constructor TScriptObjInstance.Create(aClassSym : TClassSymbol; executionContext : TdwsProgramExecution);
var
   i : Integer;
   classSymIter : TCompositeTypeSymbol;
   externalClass : TClassSymbol;
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
         if member.ClassType=TFieldSymbol then
            TFieldSymbol(member).InitData(FData, 0);
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

// Destroy
//
destructor TScriptObjInstance.Destroy;
begin
   if Assigned(FOnObjectDestroy) then
      FOnObjectDestroy(FExternalObj);
   inherited;
end;

// BeforeDestruction
//
procedure TScriptObjInstance.BeforeDestruction;
var
   iso : IScriptObj;
begin
   if Assigned(FExecutionContext) then begin
      // we are released, so never do: Self as IScriptObj
      if not FDestroyed then begin
         iso:=TScriptObjectWrapper.Create(Self);
         ExecutionContext.DestroyScriptObj(iso);
      end;
      ExecutionContext.ScriptObjDestroyed(Self);
   end;
   inherited;
end;

// GetClassSym
//
function TScriptObjInstance.GetClassSym: TClassSymbol;
begin
   Result:=FClassSym;
end;

// SetClassSym
//
procedure TScriptObjInstance.SetClassSym(clsSym : TClassSymbol);
begin
   FClassSym:=nil;
end;

// GetExternalObject
//
function TScriptObjInstance.GetExternalObject: TObject;
begin
   Result:=FExternalObj;
end;

// SetExternalObject
//
procedure TScriptObjInstance.SetExternalObject(Value: TObject);
begin
   FExternalObj:=Value;
end;

// GetDestroyed
//
function TScriptObjInstance.GetDestroyed : Boolean;
begin
   Result:=FDestroyed;
end;

// SetDestroyed
//
procedure TScriptObjInstance.SetDestroyed(const val : Boolean);
begin
   if Assigned(FOnObjectDestroy) then begin
      FOnObjectDestroy(FExternalObj);
      FOnObjectDestroy:=nil;
      FExternalObj:=nil;
   end;
   FDestroyed:=True;
end;

// SetExecutionContext
//
procedure TScriptObjInstance.SetExecutionContext(exec : TdwsProgramExecution);
begin
   FExecutionContext:=exec;
end;

// ------------------
// ------------------ TScriptDynamicArray ------------------
// ------------------

// Create
//
constructor TScriptDynamicArray.Create(elemTyp : TTypeSymbol);
begin
   FElementTyp:=elemTyp;
   FElementSize:=elemTyp.Size;
end;

// SetLength
//
procedure TScriptDynamicArray.SetLength(n : Integer);
var
   i : Integer;
begin
   System.SetLength(FData, n*ElementSize);
   for i:=FLength to n-1 do
      FElementTyp.InitData(FData, i*ElementSize);
   FLength:=n;
end;

// SetData
//
procedure TScriptDynamicArray.SetData(const data : TData);
begin
   FData:=data;
   FLength:=System.Length(data) div ElementSize;
end;

// Insert
//
procedure TScriptDynamicArray.Insert(index : Integer);
var
   n : Integer;
begin
   Inc(FLength);
   System.SetLength(FData, FLength*ElementSize);
   n:=(FLength-index-1)*ElementSize*SizeOf(Variant);
   if n>0 then
      Move(FData[index*ElementSize], FData[(index+1)*ElementSize], n);
   FillChar(FData[index*ElementSize], ElementSize*SizeOf(Variant), 0);
   FElementTyp.InitData(FData, index*ElementSize);
end;

// Delete
//
procedure TScriptDynamicArray.Delete(index, count : Integer);
var
   i, d : Integer;
begin
   Dec(FLength, count);
   index:=index*ElementSize;
   count:=count*ElementSize;
   for i:=index to index+count-1 do
      VarClear(FData[i]);
   d:=(FLength-1)*ElementSize+count-index;
   if d>0 then
      System.Move(FData[index+count], FData[index], d*SizeOf(Variant));
   System.FillChar(FData[FLength*ElementSize], count*SizeOf(Variant), 0);
   System.SetLength(FData, FLength*ElementSize);
end;

// Swap
//
procedure TScriptDynamicArray.Swap(i1, i2 : Integer);
var
   elem1, elem2 : PVarData;
   buf : TVarData;
begin
   elem1:=PVarData(@FData[i1*ElementSize]);
   elem2:=PVarData(@FData[i2*ElementSize]);
   buf:=elem1^;
   elem1^:=elem2^;
   elem2^:=buf;
end;

// Reverse
//
procedure TScriptDynamicArray.Reverse;
var
   i : Integer;
begin
   if Length>1 then begin
      for i:=0 to (Length div 2)-1 do
         Swap(i, Length-i-1);
   end;
end;

// QuickSort
//
procedure TScriptDynamicArray.QuickSort(lo, hi : Integer; const compareFunc : TScriptDynamicArrayCompareFunc);
var
   i, j, pivot : Integer;
begin
   repeat
      i:=lo;
      j:=hi;
      pivot:=(lo+hi) shr 1;
      repeat
         while compareFunc(i, pivot)<0 do
            Inc(i);
         while compareFunc(i, pivot)>0 do
            Dec(j);
         if i<=j then begin
            if i<>j then
               Swap(i, j);
            Inc(i);
            Dec(j);
         end;
      until i>j;
      if lo<j then
         QuickSort(lo, j, compareFunc);
      lo:=i;
   until i>=hi;
end;

// CompareFunc
//
function TScriptDynamicArray.CompareFunc(idx1, idx2 : Integer) : Integer;
begin
   Result:=0;
end;

// Sort
//
procedure TScriptDynamicArray.Sort(exec : TdwsExecution; compareExpr : TTypedExpr);
begin
   QuickSort(0, FLength-1, CompareFunc);
end;

// Copy
//
procedure TScriptDynamicArray.Copy(src : TScriptDynamicArray; index, count : Integer);
begin
   RawCopy(src.FData, index*ElementSize, count*ElementSize);
end;

// RawCopy
//
procedure TScriptDynamicArray.RawCopy(const src : TData; rawIndex, rawCount : Integer);
var
   i : Integer;
begin
   FLength:=rawCount div ElementSize;
   System.SetLength(FData, rawCount);
   for i:=rawIndex to rawIndex+rawCount-1 do
      FData[i-rawIndex]:=src[i];
end;

// Concat
//
procedure TScriptDynamicArray.Concat(src : TScriptDynamicArray);
var
   n, nSrc : Integer;
begin
   if src.Length>0 then begin
      n:=Length;
      nSrc:=src.Length;
      FLength:=n+nSrc;
      System.SetLength(FData, FLength*ElementSize);
      DWSCopyData(src.Data, 0, FData, n*ElementSize, nSrc*ElementSize);
   end;
end;

// IndexOf
//
function TScriptDynamicArray.IndexOf(const item : TData; addr, fromIndex : Integer) : Integer;
var
   i : Integer;
begin
   for i:=fromIndex to Length-1 do
      if DWSSameData(FData, item, i*ElementSize, addr, ElementSize) then
         Exit(i);
   Result:=-1;
end;

// IndexOf
//
function TScriptDynamicArray.IndexOf(const item : Variant; fromIndex : Integer) : Integer;
var
   i : Integer;
begin
   Assert(ElementSize=1);
   for i:=fromIndex to Length-1 do
      if DWSSameVariant(FData[i], item) then
         Exit(i);
   Result:=-1;
end;

// IndexOfFuncPtr
//
function TScriptDynamicArray.IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;
var
   i : Integer;
   itemFunc : IFuncPointer;
   p : PVarData;
begin
   itemFunc:=IFuncPointer(IUnknown(item));
   if itemFunc=nil then begin
      for i:=fromIndex to Length-1 do begin
         p:=@FData[i];
         if (p.VType=varUnknown) and (p.VUnknown=nil) then
            Exit(i);
      end;
   end else begin
      for i:=fromIndex to Length-1 do
         if itemFunc.SameFunc(FData[i]) then
            Exit(i);
   end;
   Result:=-1;
end;

// ------------------
// ------------------ TScriptInterface ------------------
// ------------------

// Create
//
constructor TScriptInterface.Create(const instance : IScriptObj;
                                    const resolvedInterface : TResolvedInterface;
                                    executionContext : TdwsProgramExecution = nil);
begin
   FInstance:=instance;
   FTyp:=resolvedInterface.IntfSymbol;
   FVMT:=resolvedInterface.VMT;

   if executionContext<>nil then begin
      executionContext.ScriptObjCreated(Self);
      FExecutionContext:=executionContext;
   end;
end;

// BeforeDestruction
//
procedure TScriptInterface.BeforeDestruction;
begin
   if Assigned(FExecutionContext) then
      ExecutionContext.ScriptObjDestroyed(Self);
   inherited;
end;

{ TConnectorExpr }

constructor TConnectorCallExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  const Name: String; BaseExpr: TTypedExpr; IsWrite: Boolean; IsIndex: Boolean);
begin
  inherited Create(Prog, Pos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
  FIsInstruction := IsWrite;
  FIsWritable := IsWrite;
  FIsIndex := IsIndex;
end;

destructor TConnectorCallExpr.Destroy;
begin
  FBaseExpr.Free;
  FArgs.Clean;
  inherited;
end;

// AddArg
//
procedure TConnectorCallExpr.AddArg(expr : TTypedExpr);
begin
   FArgs.Add(expr);
end;

// AssignConnectorSym
//
function TConnectorCallExpr.AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType): Boolean;
var
   i : Integer;
   typSym, paramTyp : TTypeSymbol;
   arg : TTypedExpr;
begin
  // Prepare the parameter information array to query the connector symbol
   if FArgs.Count>64 then
      prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_ConnectorTooManyArguments, [FArgs.Count]);

   SetLength(FConnectorParams, FArgs.Count);
   for i:=0 to FArgs.Count-1 do begin
      arg:=TTypedExpr(FArgs.List[i]);
      FConnectorParams[i].IsVarParam:=(arg is TDataExpr) and TDataExpr(arg).IsWritable
                                      and not (arg.Typ is TArraySymbol);
      FConnectorParams[i].TypSym:=arg.Typ;
   end;

   if not connectorType.AcceptsParams(FConnectorParams) then begin
      if FName<>'' then begin
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_MethodConnectorParams,
                                              [FName, connectorType.ConnectorCaption])
      end else begin
         prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_ConnectorParams,
                                              [connectorType.ConnectorCaption]);
      end;
   end;

   // Ask the connector symbol if such a method exists
   if FIsIndex then
      FConnectorCall := ConnectorType.HasIndex(FName, FConnectorParams, typSym, FIsWritable)
   else begin
      FIsWritable := False;
      FConnectorCall := ConnectorType.HasMethod(FName, FConnectorParams, typSym);
   end;

   Result := Assigned(FConnectorCall);
   if Result then begin
      // Prepare the arguments for the method call
      SetLength(FConnectorArgs, FArgs.Count);
      for i:=0 to FArgs.Count-1 do begin
         paramTyp:=FConnectorParams[i].TypSym;
         if paramTyp<>nil then
            SetLength(FConnectorArgs[i], paramTyp.Size);
      end;
      FTyp:=typSym;
   end else begin
      prog.CompileMsgs.AddCompilerErrorFmt(Pos, CPE_ConnectorCall,
                                           [FName, connectorType.ConnectorCaption])
   end;
end;

// Eval
//
function TConnectorCallExpr.Eval(exec : TdwsExecution): Variant;
var
   dataSource, dataDest : TData;
   addrSource : Integer;
   x : Integer;
   arg : TTypedExpr;
   argTyp : TTypeSymbol;
   buf : Variant;
   obj : IScriptObj;
begin
   if exec.IsDebugging then
      exec.Debugger.EnterFunc(exec, Self);

   // Call function
   try
      dataSource := nil;
      dataDest := nil;

      for x := 0 to Length(FConnectorArgs) - 1 do begin
         arg:=TTypedExpr(FArgs.List[x]);
         argTyp:=FConnectorParams[x].TypSym;
         if argTyp.Size = 1 then begin
            if argTyp is TDynamicArraySymbol then begin
               arg.EvalAsScriptObj(exec, obj);
               FConnectorArgs[x][0]:=VarArrayOf(TScriptDynamicArray(obj.InternalObject).Data);
            end else arg.EvalAsVariant(exec, FConnectorArgs[x][0]);
         end else begin
            dataSource := TDataExpr(arg).Data[exec];
            addrSource := TDataExpr(arg).Addr[exec];
            dataDest := FConnectorArgs[x];
            DWSCopyData(dataSource, addrSource, dataDest, 0, argTyp.Size);
         end;
      end;

      try
         // The call itself
         if FConnectorCall.NeedDirectReference then
            FResultData := FConnectorCall.Call(TDataExpr(FBaseExpr).Data[exec][TDataExpr(FBaseExpr).Addr[exec]], FConnectorArgs)
         else begin
            FBaseExpr.EvalAsVariant(exec, buf);
            FResultData := FConnectorCall.Call(buf, FConnectorArgs);
         end;
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

// GetSubExpr
//
function TConnectorCallExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=BaseExpr
   else Result:=TExprBase(FArgs.List[i-1]);
end;

// IsWritable
//
function TConnectorCallExpr.IsWritable : Boolean;
begin
   Result:=FIsWritable;
end;

// GetSubExprCount
//
function TConnectorCallExpr.GetSubExprCount : Integer;
begin
   Result:=FArgs.Count+1;
end;

{ TConnectorReadExpr }

function TConnectorReadExpr.AssignConnectorSym(
  ConnectorType: IConnectorType): Boolean;
begin
  FConnectorMember := ConnectorType.HasMember(FName, FTyp,False);
  Result := Assigned(FConnectorMember);
end;

constructor TConnectorReadExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  const Name: String; BaseExpr: TTypedExpr);
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
      FBaseExpr.EvalAsVariant(exec, Result);
      FResultData := FConnectorMember.Read(Result);
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

// GetSubExpr
//
function TConnectorReadExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FBaseExpr
end;

// GetSubExprCount
//
function TConnectorReadExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TConnectorWriteExpr ------------------
// ------------------

constructor TConnectorWriteExpr.Create(prog : TdwsProgram; const scriptPos: TScriptPos;
  const Name: String; BaseExpr, ValueExpr: TTypedExpr);
begin
   inherited Create;
   FScriptPos:=scriptPos;
   FName:=Name;
   FBaseExpr:=BaseExpr;
   FValueExpr:=ValueExpr;
end;

// Destroy
//
destructor TConnectorWriteExpr.Destroy;
begin
   FBaseExpr.Free;
   FValueExpr.Free;
   inherited;
end;

// ScriptPos
//
function TConnectorWriteExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// AssignConnectorSym
//
function TConnectorWriteExpr.AssignConnectorSym(prog : TdwsProgram; const connectorType : IConnectorType) : Boolean;
var
   memberTyp : TTypeSymbol;
begin
   FConnectorMember := ConnectorType.HasMember(FName, memberTyp, True);
   Result := Assigned(FConnectorMember);
   if Result and not (Assigned(memberTyp) and Assigned(FValueExpr.Typ) and memberTyp.IsCompatible(FValueExpr.Typ)) then
      Prog.CompileMsgs.AddCompilerError(FScriptPos, CPE_ConnectorTypeMismatch);
end;

// Eval
//
function TConnectorWriteExpr.Eval(exec : TdwsExecution) : Variant;
begin
   EvalNoResult(exec);
end;

// EvalNoResult
//
procedure TConnectorWriteExpr.EvalNoResult(exec : TdwsExecution);
var
   dat : TData;
   tmp : Variant;
   base : PVariant;
begin
   if (FBaseExpr is TVarExpr) or (FBaseExpr.Typ.Size>1) then
      base:=@TDataExpr(FBaseExpr).Data[exec][TDataExpr(FBaseExpr).Addr[exec]]
   else begin
      FBaseExpr.EvalAsVariant(exec, tmp);
      base:=@tmp;
   end;

//  if FValueExpr is TDataExpr then
//    dat := TDataExpr(FValueExpr).GetData(exec)
//  else
//  begin
    SetLength(dat, 1);
    FValueExpr.EvalAsVariant(exec, dat[0]);
//  end;

   try
      FConnectorMember.Write(base^, dat);
   except
      exec.SetScriptError(Self);
      raise;
   end;
end;

// GetSubExpr
//
function TConnectorWriteExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FBaseExpr
   else Result:=FValueExpr;
end;

// GetSubExprCount
//
function TConnectorWriteExpr.GetSubExprCount : Integer;
begin
   Result:=2;
end;

// ------------------
// ------------------ TdwsSymbolDictionary ------------------
// ------------------

// Create
//
constructor TdwsSymbolDictionary.Create;
begin
   FSymbolList:=TSymbolPositionListList.Create;
   FSearchSymbolPositionList:=TSymbolPositionList.Create(nil);
end;

// Destroy
//
destructor TdwsSymbolDictionary.Destroy;
begin
   Clear;
   FSymbolList.Free;
   FSearchSymbolPositionList.Free;
   inherited;
end;

// AddSymbol
//
procedure TdwsSymbolDictionary.AddSymbol(sym : TSymbol; const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
var
   symPosList : TSymbolPositionList;
begin
   if sym=nil then Exit;         // don't add a nil pointer
   if sym.IsBaseType then Exit;  // don't store references to base symbols

   // Check to see if symbol list already exists, if not create it
   symPosList:=FindSymbolPosList(sym);
   if symPosList=nil then begin
      symPosList:=TSymbolPositionList.Create(sym);
      FSymbolList.Add(symPosList);
   end;

   // add the instance of the symbol to the position list
   symPosList.Add(scriptPos, UseTypes);
end;

// FindSymbolAtPosition
//
function TdwsSymbolDictionary.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : String) : TSymbol;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to FSymbolList.Count-1 do begin
      Result:=FSymbolList[i].FindSymbolAtPosition(aCol, aLine, sourceFile);
      if Assigned(Result) then Break;
   end;
end;

// GetList
//
function TdwsSymbolDictionary.GetList(Index: Integer): TSymbolPositionList;
begin
   Result:=FSymbolList[Index];
end;

// Count
//
function TdwsSymbolDictionary.Count: Integer;
begin
  Result:=FSymbolList.Count;
end;

// FindSymbolPosList
//
function TdwsSymbolDictionary.FindSymbolPosList(Sym: TSymbol): TSymbolPositionList;
var
   i : Integer;
begin
   FSearchSymbolPositionList.FSymbol:=sym;
   if FSymbolList.Find(FSearchSymbolPositionList, i) then
      Result:=FSymbolList[i]
   else Result:=nil;
end;

// FindSymbolPosList
//
function TdwsSymbolDictionary.FindSymbolPosList(const symName : String) : TSymbolPositionList;
var
   i : Integer;
begin
   for i:=0 to FSymbolList.Count-1 do begin
      // same name (not case-sensitive)
      Result:=FSymbolList[i];
      if AnsiCompareText(Result.Symbol.Name, SymName)=0 then Exit;
   end;
   Result:=nil;
end;

// Remove
//
procedure TdwsSymbolDictionary.Remove(sym: TSymbol);
var
   idx, i : Integer;
   symList : TSymbolPositionList;
begin
   // TFuncSymbol - remove params
   if sym is TFuncSymbol then begin
      for i := 0 to TFuncSymbol(sym).Params.Count - 1 do
         Remove(TFuncSymbol(sym).Params[i]);
   // TPropertySymbol - remove array indices
   end else if sym is TPropertySymbol then begin
      for i := 0 to TPropertySymbol(sym).ArrayIndices.Count - 1 do
         Remove(TPropertySymbol(sym).ArrayIndices[i]);
   // TStructuredTypeSymbol - remove members (methods, fields, properties)
   end else if sym is TCompositeTypeSymbol then begin
      for i := 0 to TCompositeTypeSymbol(sym).Members.Count - 1 do
         Remove(TCompositeTypeSymbol(sym).Members[i]);
   end;

   // basic entry to remove
   symList := FindSymbolPosList(sym);
   if Assigned(symList) then begin
      // remove symList from internal list
      idx:=FSymbolList.Extract(symList);
      Assert(idx>=0);
      symList.Free;
   end;
end;

// RemoveInRange
//
procedure TdwsSymbolDictionary.RemoveInRange(const startPos, endPos : TScriptPos);
var
   i : Integer;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FSymbolList.Count-1 do
      FSymbolList[i].RemoveInRange(startPos, endPos);
end;

// EnumerateInRange
//
procedure TdwsSymbolDictionary.EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc);
var
   i, j : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FSymbolList.Count-1 do begin
      list:=FSymbolList[i];
      for j:=list.Count-1 downto 0 do begin
         symPos:=list[j];
         if     startPos.IsBeforeOrEqual(symPos.ScriptPos)
            and symPos.ScriptPos.IsBeforeOrEqual(endPos) then begin
            callBack(list.Symbol);
            Break;
         end;
      end;
   end;
end;

// ReplaceSymbolAt
//
procedure TdwsSymbolDictionary.ReplaceSymbolAt(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
var
   i : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   list:=FindSymbolPosList(oldSym);
   i:=list.IndexOfPosition(scriptPos);
   Assert(i>=0);
   symPos:=list[i];
   AddSymbol(newSym, scriptPos, symPos.SymbolUsages);
   list.Delete(i);
end;

// ChangeUsageAt
//
procedure TdwsSymbolDictionary.ChangeUsageAt(const scriptPos : TScriptPos; const addUsages, removeUsages : TSymbolUsages);
var
   i, k : Integer;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   for i:=0 to FSymbolList.Count-1 do begin
      symPosList:=FSymbolList[i];
      k:=symPosList.IndexOfPosition(scriptPos);
      if k>=0 then begin
         symPos:=symPosList[k];
         symPos.SymbolUsages:=symPos.SymbolUsages+addUsages-removeUsages;
      end;
   end;
end;

// Clear
//
procedure TdwsSymbolDictionary.Clear;
begin
   FSymbolList.Clean;
end;

// FindSymbolUsage
//
function TdwsSymbolDictionary.FindSymbolUsage(symbol : TSymbol; symbolUse : TSymbolUsage) : TSymbolPosition;
var
   list : TSymbolPositionList;
begin
   list:=FindSymbolPosList(Symbol);
   if Assigned(list) then
      Result:=list.FindUsage(SymbolUse)
   else Result:=nil;
end;

function TdwsSymbolDictionary.FindSymbolUsage(const SymName: String;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(SymName);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TdwsSymbolDictionary.FindSymbolUsageOfType(const SymName: String;
  SymbolType: TSymbolClass; SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosListOfType(SymName, SymbolType);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

// FindSymbolByUsageAtLine
//
function TdwsSymbolDictionary.FindSymbolByUsageAtLine(const scriptPos : TScriptPos; symbolUse: TSymbolUsage) : TSymbol;
var
   i, j : Integer;
   list : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   for i:=0 to FSymbolList.Count-1 do begin
      list:=FSymbolList[i];
      for j:=0 to list.Count-1 do begin
         symPos:=list[j];
         if     (symbolUse in symPos.SymbolUsages)
            and (symPos.ScriptPos.SourceFile=scriptPos.SourceFile)
            and (symPos.ScriptPos.Line=scriptPos.Line) then begin
            Exit(list.Symbol);
         end;
      end;
   end;
   Result:=nil;
end;

function TdwsSymbolDictionary.FindSymbolPosListOfType(const SymName: String;
  SymbolType: TSymbolClass): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to Self.Count - 1 do
    if UnicodeSameText(Self.Items[x].Symbol.Name, SymName) and (Self.Items[x].Symbol is SymbolType) then // same name (not case-sensitive)
    begin
      Result := Self.Items[x];
      Break;
    end;
end;

// ------------------
// ------------------ TSymbolPositionList ------------------
// ------------------

// Create
//
constructor TSymbolPositionList.Create(ASymbol: TSymbol);
begin
   FSymbol:=ASymbol;
   FPosList:=TSimpleList<TSymbolPosition>.Create;
end;

// Destroy
//
destructor TSymbolPositionList.Destroy;
begin
   Clear;
   FPosList.Free;
   inherited;
end;

// Add
//
procedure TSymbolPositionList.Add(const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
var
   symPos : TSymbolPosition;
begin
   if (scriptPos.Line<=0) or (scriptPos.SourceFile=nil) then Exit;

   New(symPos);
   symPos.FScriptPos:=scriptPos;
   symPos.FSymUsages:=useTypes;
   FPosList.Add(symPos);
end;

// Delete
//
procedure TSymbolPositionList.Delete(index : Integer);
begin
   Dispose(FPosList[index]);
   FPosList.Extract(index);
end;

// Clear
//
procedure TSymbolPositionList.Clear;
var
   i : Integer;
begin
   for i:=0 to FPosList.Count-1 do
      Dispose(FPosList[i]);
   FPosList.Clear;
end;

// FindSymbolAtPosition
//
function TSymbolPositionList.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : String): TSymbol;
var
   i : Integer;
   symPos : TSymbolPosition;
begin
   for i:=0 to FPosList.Count-1 do begin
      symPos:=FPosList[i];
      if     (symPos.ScriptPos.Line=ALine)
         and (symPos.ScriptPos.Col=ACol)
         and UnicodeSameText(symPos.ScriptPos.SourceFile.Name, sourceFile) then begin
         Exit(FSymbol);
      end;
   end;
   Result:=nil;
end;

// GetPosition
//
function TSymbolPositionList.GetPosition(Index: Integer): TSymbolPosition;
begin
   Result:=FPosList[Index];
end;

// Count
//
function TSymbolPositionList.Count: Integer;
begin
   Result:=FPosList.Count;
end;

// FindUsage
//
function TSymbolPositionList.FindUsage(const symbolUse : TSymbolUsage) : TSymbolPosition;
var
   i : Integer;
begin
   if Self<>nil then begin
      for i:=0 to Count-1 do begin
         Result:=FPosList[i];
         if SymbolUse in Result.SymbolUsages then Exit;
      end;
   end;
   Result:=nil;
end;

// IndexOfPosition
//
function TSymbolPositionList.IndexOfPosition(const scriptPos : TScriptPos) : Integer;
var
   i : Integer;
   symPos : TSymbolPosition;
begin
   for i:=0 to Count-1 do begin
      symPos:=FPosList[i];
      if symPos.ScriptPos.SamePosAs(scriptPos) then
         Exit(i);
   end;
   Result:=-1;
end;

// RemoveInRange
//
procedure TSymbolPositionList.RemoveInRange(const startPos, endPos : TScriptPos);
var
   i : Integer;
   symPos : TSymbolPosition;
begin
   for i:=FPosList.Count-1 downto 0 do begin
      symPos:=FPosList[i];
      if     startPos.IsBeforeOrEqual(symPos.ScriptPos)
         and symPos.ScriptPos.IsBeforeOrEqual(endPos) then begin
         Dispose(symPos);
         FPosList.Extract(i);
      end;
   end;
end;

// ------------------
// ------------------ TSymbolPositionListList ------------------
// ------------------

// Compare
//
function TSymbolPositionListList.Compare(const item1, item2 : TSymbolPositionList) : Integer;
begin
   if NativeInt(item1.Symbol)<NativeInt(item2.Symbol) then
      Result:=-1
   else if NativeInt(item1.Symbol)>NativeInt(item2.Symbol) then
      Result:=1
   else Result:=0;
end;

// ------------------
// ------------------ TdwsSourceContext ------------------
// ------------------

// Create
//
constructor TdwsSourceContext.Create(aParent : TdwsSourceContext; const aStartPos : TScriptPos;
                            aParentSymbol : TSymbol; aToken : TTokenType);
begin
   FParentContext := AParent;
   FParentSymbol  := AParentSymbol;
   FStartPos := AStartPos;
   FToken := aToken;
end;

// Destroy
//
destructor TdwsSourceContext.Destroy;
begin
   FSubContexts.Clean;
   inherited;
end;

function TdwsSourceContext.HasParentSymbolOfClass(SymbolType: TSymbolClass;
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

// FindContext
//
function TdwsSourceContext.FindContext(parentSymbol : TSymbol) : TdwsSourceContext;
var
   i : Integer;
begin
   if FParentSymbol=parentSymbol then Exit(Self);
   for i:=0 to Count-1 do begin
      Result:=SubContext[i].FindContext(parentSymbol);
      if Result<>nil then Exit;
   end;
   Result:=nil;
end;

// FindContextByToken
//
function TdwsSourceContext.FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Result:=SubContext[i];
      if Result.Token=aToken then Exit;
   end;
   Result:=nil;
end;

// EnumerateContextsOfSymbol
//
procedure TdwsSourceContext.EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);
var
   i : Integer;
begin
   if ParentSym=aParentSymbol then
      callBack(Self);
   for i:=0 to Count-1 do
      SubContext[i].EnumerateContextsOfSymbol(aParentSymbol, callBack);
end;

// WriteToJSON
//
procedure TdwsSourceContext.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginObject;

   writer.WriteName('Token');
   writer.WriteString(GetEnumName(TypeInfo(TTokenType), Ord(Token)));

   writer.WriteName('Symbol');
   if ParentSym=nil then
      writer.WriteNull
   else begin
      writer.BeginObject;
      writer.WriteName('Class');
      writer.WriteString(ParentSym.ClassName);
      writer.WriteName('Name');
      writer.WriteString(ParentSym.Name);
      writer.EndObject;
   end;

   writer.WriteName('SubContexts');
   writer.BeginArray;
   for i:=0 to Count-1 do
      SubContext[i].WriteToJSON(writer);
   writer.EndArray;

   writer.EndObject;
end;

// IsPositionInContext
//
function TdwsSourceContext.IsPositionInContext(aCol, aLine : Integer; const sourceName : String) : Boolean;
begin
   // check if the position is in the same SourceFile
   if sourceName<>'' then begin // if empty, don't check it
      if not UnicodeSameText(sourceName, FStartPos.SourceFile.Name) then begin
         Result:=False;
         Exit;
      end;
   end;

   // if inside a multi-line context
   Result := (aLine > FStartPos.Line) and (aLine < FEndPos.Line);
   if not Result then begin
      // if not, check for a one-line context (inside the context begin and end cols)
      if FStartPos.Line = FEndPos.Line then
         Result:=    (aLine = FStartPos.Line)
                 and (aCol >= FStartPos.Col)
                 and (aCol <= FEndPos.Col)
      else begin
         // not a single-line context
         if FEndPos.SourceFile=nil then begin
            // unclosed context (compiled with errors)
            Result :=    (aLine > FStartPos.Line)
                      or ((aLine = FStartPos.Line) and (aCol >= FStartPos.Col)) ;    // after start
         end else begin
            Result :=    ((aLine = FStartPos.Line) and (aCol >= FStartPos.Col)) // on top line, inside start
                      or ((aLine = FEndPos.Line) and (aCol <= FEndPos.Col));    // on bottom line, inside end
         end;
      end;
   end;
end;

// GetSubContext
//
function TdwsSourceContext.GetSubContext(index : Integer) : TdwsSourceContext;
begin
   Result:=TdwsSourceContext(FSubContexts.List[index]);
end;

// ------------------
// ------------------ TdwsSourceContextMap ------------------
// ------------------

// Destroy
//
destructor TdwsSourceContextMap.Destroy;
begin
   FScriptContexts.Clean;
   inherited;
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(aParentSymbol : TSymbol): TdwsSourceContext;
var
   x : Integer;
begin
   for x:=0 to FScriptContexts.Count-1 do begin
      Result:=TdwsSourceContext(FScriptContexts.List[x]);
      if Result.FParentSymbol=aParentSymbol then Exit;
   end;
   Result:=nil;
end;

// FindContextByToken
//
function TdwsSourceContextMap.FindContextByToken(aToken : TTokenType) : TdwsSourceContext;
var
   x : Integer;
begin
   for x:=0 to FScriptContexts.Count-1 do begin
      Result:=TdwsSourceContext(FScriptContexts.List[x]);
      if Result.Token=aToken then Exit;
   end;
   Result:=nil;
end;

// EnumerateContextsOfSymbol
//
procedure TdwsSourceContextMap.EnumerateContextsOfSymbol(aParentSymbol : TSymbol; const callBack : TdwsSourceContextCallBack);
var
   x : Integer;
begin
   for x:=0 to FScriptContexts.Count-1 do
      TdwsSourceContext(FScriptContexts.List[x]).EnumerateContextsOfSymbol(aParentSymbol, callBack);
end;

// WriteToJSON
//
procedure TdwsSourceContextMap.WriteToJSON(writer : TdwsJSONWriter);
var
   i : Integer;
begin
   writer.BeginArray;
   for i:=0 to FScriptContexts.Count-1 do
      TdwsSourceContext(FScriptContexts.List[i]).WriteToJSON(writer);
   writer.EndArray;
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(aCol, aLine : Integer; sourceFile : TSourceFile) : TdwsSourceContext;
begin
   Result:=FindContext(aCol, aLine, sourceFile.Name);
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(aCol, aLine : Integer; const sourceName : String) : TdwsSourceContext;
var
   returnContext : TdwsSourceContext;    // Gets set to the context found
   hitEnd : Boolean;            // Followed branch to end, stop searching

   function FoundContext(context : TdwsSourceContext) : Boolean;
   var
      x : Integer;
      subContext : TdwsSourceContext;
   begin
      Result := False;
      { Record that this context contains it and should be returned (provided it
        doesn't go deeper) }
      returnContext := context;
      { Search sub-contexts }
      for x := 0 to context.SubContexts.Count - 1 do begin
         subContext:=TdwsSourceContext(context.SubContexts.List[x]);
         if subContext.IsPositionInContext(aCol, aLine, sourceName) then
            Result := FoundContext(subContext)
      end;
      { We got here because it was found. After all subContexts were checked,
        it wasn't found so we've hit the end. }
      if not Result then
         hitEnd := True;
   end;

var
   i : Integer;
   context : TdwsSourceContext;
begin
   { If this position is not in the top level contexts then it won't be in
     subcontexts. Use a recursive search to find the lowest context at which the
     position can be found. }

   returnContext := nil;
   hitEnd        := False;
   { Cycle all top level contexts. Burrow into each to find the lowest level that
     matches the criteria. }
   for i := 0 to FScriptContexts.Count - 1 do begin
      if hitEnd then
         Break;
      { If in top-level context, burrow into subcontexts }
      context:=TdwsSourceContext(FScriptContexts.List[i]);
      if context.IsPositionInContext(aCol, aLine, sourceName) then
         if not FoundContext(context) then
            Break;
   end;
   Result := returnContext;
end;

// FindContext
//
function TdwsSourceContextMap.FindContext(const scriptPos : TScriptPos): TdwsSourceContext;
begin
   Result:=FindContext(scriptPos.Col, scriptPos.Line, scriptPos.SourceFile.Name);
end;

// OpenContext
//
procedure TdwsSourceContextMap.OpenContext(const startPos : TScriptPos; parentSymbol : TSymbol; token : TTokenType);
var
   newContext: TdwsSourceContext;
begin
   // Uses a simple 'stack' concept. If currently in a context and a new context
   // is opened then the new context is a sub context of the current context.
   // new context is owned by the current context
   newContext:=TdwsSourceContext.Create(FCurrentContext, startPos, parentSymbol, token);
   // Add new context to the appropriate 'parent' context
   if FCurrentContext=nil then           // if top-level,
      FScriptContexts.Add(newContext)      // Add to top-level contexts
   else FCurrentContext.SubContexts.Add(newContext);
   FCurrentContext:=newContext;
end;

// CloseContext
//
procedure TdwsSourceContextMap.CloseContext(const aEndPos : TScriptPos; onlyIfTokenType : TTokenType = ttNone);
begin
   if (onlyIfTokenType<>ttNone) and (FCurrentContext.Token<>onlyIfTokenType) then Exit;

   FCurrentContext.FEndPos := AEndPos;       // close the current context
   { if the CurrentContext is not a top-level one, then pop the stack and make
     the new context the closed one's parent }
   FCurrentContext := FCurrentContext.Parent;
end;

// CloseAllContexts
//
procedure TdwsSourceContextMap.CloseAllContexts(const aEndPos : TScriptPos);
begin
   while FCurrentContext<>nil do
      CloseContext(aEndPos);
end;

// SuspendContext
//
function TdwsSourceContextMap.SuspendContext : TdwsSourceContext;
begin
   Result:=FCurrentContext;
   FCurrentContext:=nil;
end;

// ResumeContext
//
procedure TdwsSourceContextMap.ResumeContext(aContext : TdwsSourceContext);
begin
   if FCurrentContext<>nil then
      Assert(FCurrentContext=nil);
   FCurrentContext:=aContext;
end;

// GetContext
//
function TdwsSourceContextMap.GetContext(index : Integer) : TdwsSourceContext;
begin
   Result:=TdwsSourceContext(FScriptContexts.List[index]);
end;

// ------------------
// ------------------ TScriptSourceItem ------------------
// ------------------

constructor TScriptSourceItem.Create(const ANameReference: String; ASourceFile: TSourceFile;
  ASourceType: TScriptSourceType);
begin
   FNameReference := ANameReference;
   FSourceFile := ASourceFile;
   FSourceType := ASourceType;
end;

// Destroy
//
destructor TScriptSourceItem.Destroy;
begin
   FSourceFile.Free;
   inherited;
end;

// ------------------
// ------------------ TScriptSourceList ------------------
// ------------------

// Create
//
constructor TScriptSourceList.Create;
begin
   inherited;
   FMainScript:=nil;
end;

// Destroy
//
destructor TScriptSourceList.Destroy;
begin
   Clear;
   FSourceList.Free;
   inherited;
end;

// Add
//
function TScriptSourceList.Add(const nameReference, code: String;
   sourceType: TScriptSourceType) : TSourceFile;
var
   srcItem : TScriptSourceItem;
begin
   srcItem:=FindScriptSourceItem(nameReference);
   if srcItem=nil then begin
      Result:=TSourceFile.Create;
      Result.Name:=nameReference;
      Result.Code:=code;
      srcItem:=TScriptSourceItem.Create(nameReference, Result, sourceType);
      FSourceList.Add(srcItem);
      // get a pointer to the 'main' script item
      if sourceType=stMain then
         FMainScript:=srcItem;
   end else begin
      Result:=srcItem.SourceFile;
   end;
end;

// Clear
//
procedure TScriptSourceList.Clear;
begin
   FSourceList.Clean;
   FMainScript:=nil;
end;

// GetSourceItem
//
function TScriptSourceList.GetSourceItem(Index: Integer): TScriptSourceItem;
begin
   Result := TScriptSourceItem(FSourceList.List[Index]);
end;

// FindScriptSourceItem
//
function TScriptSourceList.FindScriptSourceItem(const sourceFileName: String): TScriptSourceItem;
var
   x : Integer;
begin
   x:=IndexOf(SourceFileName);
   if x>=0 then
      Result:=Items[x]
   else Result:=nil;
end;

function TScriptSourceList.IndexOf(const SourceFileName: String): Integer;
var
   x: Integer;
begin
   for x := 0 to FSourceList.Count-1 do
      if UnicodeSameText(Items[x].NameReference, SourceFileName) then
         Exit(x);
   Result:=-1;
end;

// ------------------
// ------------------ TMethodObjExpr ------------------
// ------------------

constructor TMethodObjExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos;
  BaseExpr: TDataExpr);
begin
  Assert(BaseExpr.Typ is TMethodSymbol);
  inherited Create(Prog,Pos,TMethodSymbol(BaseExpr.Typ).StructSymbol);
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

// ------------------
// ------------------ TConstructorStaticObjExpr ------------------
// ------------------

constructor TConstructorStaticObjExpr.Create(Prog: TdwsProgram;
  const Pos: TScriptPos; Func: TMethodSymbol; BaseExpr: TTypedExpr);
begin
  inherited Create(Prog,Pos,Func,BaseExpr);
  Typ := BaseExpr.Typ;
end;

function TConstructorStaticObjExpr.PostCall(exec : TdwsExecution) : Variant;
begin
   Result := exec.SelfScriptObject^;
end;

// ------------------
// ------------------ TConstructorVirtualObjExpr ------------------
// ------------------

constructor TConstructorVirtualObjExpr.Create(Prog: TdwsProgram;
  const Pos: TScriptPos; Func: TMethodSymbol; Base: TTypedExpr);
begin
  inherited Create(Prog,Pos,Func,Base);
  Typ := Base.Typ;
end;

function TConstructorVirtualObjExpr.PostCall(exec : TdwsExecution): Variant;
begin
   Result := exec.SelfScriptObject^;
end;

// ------------------
// ------------------ TDestructorStaticExpr ------------------
// ------------------

// PostCall
//
function TDestructorStaticExpr.PostCall(exec : TdwsExecution) : Variant;
begin
   exec.SelfScriptObject^.Destroyed:=True;
end;

// ------------------
// ------------------ TDestructorVirtualExpr ------------------
// ------------------

// PostCall
//
function TDestructorVirtualExpr.PostCall(exec : TdwsExecution) : Variant;
begin
   exec.SelfScriptObject^.Destroyed:=True;
end;

// ------------------
// ------------------ TNoResultWrapperExpr ------------------
// ------------------

// Create
//
constructor TNoResultWrapperExpr.Create(Prog: TdwsProgram; const Pos: TScriptPos; Expr: TTypedExpr);
begin
   inherited Create;
   FExpr := Expr;
end;

// Destroy
//
destructor TNoResultWrapperExpr.Destroy;
begin
   FExpr.Free;
   inherited;
end;

// EvalNoResult
//
procedure TNoResultWrapperExpr.EvalNoResult(exec : TdwsExecution);
begin
   Expr.EvalNoResult(exec);
end;

// IsConstant
//
function TNoResultWrapperExpr.IsConstant : Boolean;
begin
   Result:=FExpr.IsConstant;
end;

// ScriptPos
//
function TNoResultWrapperExpr.ScriptPos : TScriptPos;
begin
   Result:=FExpr.ScriptPos;
end;

// GetSubExpr
//
function TNoResultWrapperExpr.GetSubExpr(i : Integer) : TExprBase;
begin
   Result:=FExpr;
end;

// GetSubExprCount
//
function TNoResultWrapperExpr.GetSubExprCount : Integer;
begin
   Result:=1;
end;

// ------------------
// ------------------ TSourceCondition ------------------
// ------------------

// Create
//
constructor TSourceCondition.Create(const pos : TScriptPos; aTest, aMsg : TTypedExpr);
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
procedure TSourceCondition.InitSymbol(symbol: TSymbol);
begin

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
   ptrList : PObjectTightList;
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
      RaiseConditionFailed(exec, FProg.FFunc, failed.Pos, failed);
end;

// GetConditions
//
function TSourceConditions.GetConditions(idx : Integer) : TSourceCondition;
begin
   Result:=TSourceCondition(FItems.List[idx]);
end;

// ------------------
// ------------------ TSourcePreConditions ------------------
// ------------------

// RaiseConditionFailed
//
procedure TSourcePreConditions.RaiseConditionFailed(exec : TdwsExecution;
   funcSym : TFuncSymbol; const scriptPos : TScriptPos; const msg : IStringEvalable);
var
   msgStr : String;
begin
   msg.EvalAsString(exec, msgStr);
   (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(
      RTE_PreConditionFailed, [funcSym.QualifiedName, scriptPos.AsInfo, msgStr], scriptPos);
end;

// ------------------
// ------------------ TSourcePostConditions ------------------
// ------------------

// RaiseConditionFailed
//
procedure TSourcePostConditions.RaiseConditionFailed(exec : TdwsExecution;
   funcSym : TFuncSymbol; const scriptPos : TScriptPos; const msg : IStringEvalable);
var
   msgStr : String;
begin
   msg.EvalAsString(exec, msgStr);
   (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(
      RTE_PostConditionFailed, [funcSym.QualifiedName, scriptPos.AsInfo, msgStr], scriptPos);
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
         RaiseConditionFailed(exec, methSym, current.ScriptPos, current.Message);
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
            RaiseConditionFailed(exec, methSym, current.ScriptPos, current.Message);
      end;
      methSym:=methSym.ParentMeth;
   end;
end;

// ------------------
// ------------------ EdwsVariantTypeCastError ------------------
// ------------------

// Create
//
constructor EdwsVariantTypeCastError.Create(const v : Variant;
      const desiredType : String; originalException : Exception);
begin
   inherited CreateFmt(RTE_VariantCastFailed,
                       [VarTypeAsText(VarType(v)), desiredType, originalException.ClassName])

end;

// ------------------
// ------------------ TExternalFuncHandler ------------------
// ------------------

// InitSymbol
//
procedure TExternalFuncHandler.InitSymbol(symbol: TSymbol);
begin
   // nothing
end;

// InitExpression
//
procedure TExternalFuncHandler.InitExpression(Expr: TExprBase);
begin
   // nothing
end;

// Call
//
procedure TExternalFuncHandler.Call(exec : TdwsProgramExecution; func : TFuncSymbol);
var
   locArray : TdwsExprLocationArray;
begin
   locArray:=exec.GetCallStack;
   raise EdwsExternalFuncHandler.CreateFmt(RTE_UnHandledExternalCall,
                                           [func.Name, locArray[High(locArray)].Location]);
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
// ------------------ TFuncExprOverloadsHelper ------------------
// ------------------

// Create
//
constructor TFuncExprOverloadsHelper.Create(expr : TFuncExprBase; overloads : TFuncSymbolList);
begin
   FExpr:=expr;
   FOverloads:=overloads;
end;

// ExpectedArg
//
function TFuncExprOverloadsHelper.ExpectedArg : TParamSymbol;
var
   i, n : Integer;
   func : TFuncSymbol;
begin
   Result:=nil;
   n:=FExpr.FArgs.Count;
   for i:=0 to FOverloads.Count-1 do begin
      func:=FOverloads[i];
      if n<func.Params.Count then begin
         if Result=nil then
            Result:=(func.Params[n] as TParamSymbol)
         else if not Result.Typ.IsOfType(func.Params[n].Typ) then
            Exit(nil);
      end else Exit(nil);
   end;
end;

// ------------------
// ------------------ TdwsSymbolAttribute ------------------
// ------------------

// Create
//
constructor TdwsSymbolAttribute.Create(const aScriptPos : TScriptPos;
                                       aConstructor : TFuncExprBase);
begin
   inherited Create;
   FScriptPos:=aScriptPos;
   FAttributeConstructor:=aConstructor;
end;

// Destroy
//
destructor TdwsSymbolAttribute.Destroy;
begin
   FAttributeConstructor.Free;
   inherited;
end;

// ------------------
// ------------------ TdwsSymbolAttributes ------------------
// ------------------

// AttributesFor
//
function TdwsSymbolAttributes.AttributesFor(aSymbol : TSymbol) : TdwsSymbolAttributeArray;
var
   i, n : Integer;
begin
   // slow and ugly, for testing until structure gets finalized
   SetLength(Result, 0);
   n:=0;
   for i:=0 to Count-1 do begin
      if Items[i].Symbol=aSymbol then begin
         SetLength(Result, n+1);
         Result[n]:=Items[i];
         Inc(n);
      end;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsGuardianThread.Initialize;

finalization

   TdwsGuardianThread.Finalize;

end.
