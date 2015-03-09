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

uses
   Classes, Variants, SysUtils, TypInfo, Math,
   dwsSymbols, dwsErrors, dwsUtils, dwsDataContext, dwsExprList,
   dwsStrings, dwsStack, SyncObjs, dwsFileSystem, dwsTokenizer, dwsUnitSymbols,
   dwsJSON, dwsXPlatform;

type
   TRelOps = (roEqual, roUnEqual, roLess, roLessEqual, roMore, roMoreEqual);

   TRefKind = (rkObjRef, rkIntfRef, rkClassOfRef);

   TTypedExpr = class;
   TNoResultExpr = class;
   TBlockInitExpr = class;
   TBlockFinalExpr = class;
   TTypedExprList = class;
   TdwsProgram = class;
   TdwsMainProgram = class;
   IdwsProgram = interface;
   TdwsProgramExecution = class;
   TdwsProgramExecutionClass = class of TdwsProgramExecution;
   IdwsProgramExecution = interface;
   TSymbolPositionList = class;
   TFuncExprBase = class;
   TScriptObj = class;
   TSourceConditions = class;
   TSourcePreConditions = class;
   TSourcePostConditions = class;
   TBlockExprBase = class;
   TProgramExpr = class;

   TVariantDynArray = array of Variant;

   TProgramExprList = array[0..MaxInt shr 4] of TProgramExpr;
   PProgramExprList = ^TProgramExprList;
   PProgramExpr = ^TProgramExpr;

   TdwsExecutionEvent = procedure (exec : TdwsProgramExecution) of object;

   TScriptSourceType = (stMain, stUnit, stUnitNamespace, stInclude, stRecompile);

   // A specific ScriptSource entry. The text of the script contained in that unit.
   TScriptSourceItem = class (TRefCountedObject)
      private
         FNameReference : UnicodeString;
         FSourceFile : TSourceFile;
         FSourceType : TScriptSourceType;

      public
         constructor Create(const ANameReference: UnicodeString; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
         destructor Destroy; override;

         property NameReference : UnicodeString read FNameReference write FNameReference;
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
         function Add(const nameReference, code: UnicodeString; sourceType: TScriptSourceType) : TSourceFile;

         function FindScriptSourceItem(const sourceFileName: UnicodeString): TScriptSourceItem; overload;

         function IndexOf(const sourceFileName: UnicodeString): Integer; overload;

         property Count : Integer read FSourceList.FCount;

         property Items[index: Integer] : TScriptSourceItem read GetSourceItem; default;
         property MainScript: TScriptSourceItem read FMainScript;
   end;

   { Describe how the symbol at the position is being used. suReference would be
     a typical usage of the symbol.
     suImplicit indicates that the symbol was only implicitly present
     suRTTI indicates explicit RTTI access of the symbol }
   TSymbolUsage = (suForward, suDeclaration, suImplementation, suReference,
                   suRead, suWrite, suImplicit, suRTTI );
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
      type
         TSymbolPositionListEnumerator = record
            Index : Integer;
            PosList : TSymbolPositionList;
            function MoveNext : Boolean; inline;
            function GetCurrent : TSymbolPosition; inline;
            property Current : TSymbolPosition read GetCurrent;
         end;

      private
         FSymbol : TSymbol;                        // pointer to the symbol
         FPosList : TSimpleList<TSymbolPosition>;  // list of positions where symbol is declared and used
         FSourceFile : TSourceFile; // not nil only if all positions are in that file

      protected
         function GetPosition(index : Integer) : TSymbolPosition; inline;

         // Used by TSymbolDictionary. Not meaningful to make public (symbol is known).
         function FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : UnicodeString) : TSymbol; overload;

      public
         constructor Create(ASymbol: TSymbol);
         destructor Destroy; override;

         procedure Add(const scriptPos : TScriptPos; const useTypes : TSymbolUsages);
         procedure Delete(index : Integer);
         procedure Clear;

         function FindUsage(const symbolUse : TSymbolUsage) : TSymbolPosition;
         function FindAnyUsage(const symbolUses : TSymbolUsages) : TSymbolPosition;
         function IndexOfPosition(const scriptPos : TScriptPos) : Integer;
         procedure RemoveInRange(const startPos, endPos : TScriptPos);

         function GetEnumerator : TSymbolPositionListEnumerator;

         property Items[index : Integer] : TSymbolPosition read GetPosition; default;
         function Count : Integer; inline;

         property Symbol: TSymbol read FSymbol;
   end;

   TSymbolPositionListList = class(TSortedList<TSymbolPositionList>)
      protected
         function Compare(const item1, item2 : TSymbolPositionList) : Integer; override;
   end;

   TdwsSymbolDictionaryProc = procedure (sym : TSymbol) of object;
   TdwsSymbolDictionaryRef = reference to procedure (sym : TSymbol);

   { List all symbols in the script. Each symbol list contains a list of the
     positions where it was used. }
   TdwsSymbolDictionary = class
      type
         TdwsSymbolDictionaryEnumerator = record
            Index : Integer;
            Dict : TdwsSymbolDictionary;
            function MoveNext : Boolean; inline;
            function GetCurrent : TSymbolPositionList; inline;
            property Current : TSymbolPositionList read GetCurrent;
         end;

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
         procedure EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryProc); overload;
         procedure EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryRef); overload;

         procedure ReplaceSymbolAt(oldSym, newSym : TSymbol; const scriptPos : TScriptPos);
         procedure ChangeUsageAt(const scriptPos : TScriptPos; const addUsages, removeUsages : TSymbolUsages);

         function FindSymbolAtPosition(aCol, aLine: Integer; const sourceFile : UnicodeString): TSymbol; overload;
         function FindSymbolAtPosition(const aScriptPos : TScriptPos) : TSymbol; overload;
         function FindSymbolPosList(sym : TSymbol) : TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosList(const symName : UnicodeString) : TSymbolPositionList; overload;  // return list of symbol
         function FindSymbolPosListOfType(const symName : UnicodeString; symbolType : TSymbolClass) : TSymbolPositionList; // return list of symbol given the desired type
         function FindSymbolUsage(symbol : TSymbol; symbolUse: TSymbolUsage) : TSymbolPosition; overload;
         function FindSymbolUsage(const symName : UnicodeString; symbolUse: TSymbolUsage) : TSymbolPosition; overload;
         function FindSymbolUsageOfType(const symName : UnicodeString; symbolType : TSymbolClass; symbolUse : TSymbolUsage) : TSymbolPosition;
         function FindSymbolByUsageAtLine(const scriptPos : TScriptPos; symbolUse : TSymbolUsage) : TSymbol;

         function GetEnumerator : TdwsSymbolDictionaryEnumerator;

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

         function IsPositionInContext(const aPos : TScriptPos) : Boolean; overload;
         function IsPositionInContext(aCol, aLine : Integer; const sourceName : UnicodeString) : Boolean; overload;
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
         function FindContext(aCol, aLine : Integer; const sourceName : UnicodeString) : TdwsSourceContext; overload;
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

         procedure AddString(const str : UnicodeString); overload; virtual; abstract;
         procedure AddString(const i : Int64); overload; virtual;
         procedure AddCRLF; virtual;
         procedure Clear; virtual; abstract;

         function ToUTF8String : UTF8String; virtual;
         function ToDataString : RawByteString; virtual;
   end;

   TdwsDefaultResult = class(TdwsResult)
      private
         FTextBuilder : TWriteOnlyBlockStream;
         function GetText : UnicodeString; inline;

      public
         constructor Create(resultType : TdwsResultType); override;
         destructor Destroy; override;

         procedure AddString(const str : UnicodeString); override;
         procedure AddString(const i : Int64); override;
         procedure AddCRLF; override;
         procedure Clear; override;
         function ToString : String; override;
         function ToDataString : RawByteString; override;

         property Text : UnicodeString read GetText;
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
      public
         Exec : IdwsProgramExecution;
         TimeOutAt : Int64;
         Next : TdwsGuardedExecution;
         procedure FreeAll;
   end;

   // TdwsGuardianThread
   //
   // Stops the script after given time (Timeout)
   TdwsGuardianThread = class(TdwsThread)
      private
         FEvent : TEvent;
         FExecutions : TdwsGuardedExecution;
         FExecutionsLock : TFixedCriticalSection;

      protected
         procedure Execute; override;

         class var vThread : TdwsGuardianThread;
         class var vExecutionsPool : TdwsGuardedExecution;

      public
         constructor Create;
         destructor Destroy; override;

         const PulseIntervalMilliSeconds : Integer = 2500;

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
      procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString);
      procedure LocalizeString(const aString : UnicodeString; var Result : UnicodeString);
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
      function GetLastScriptErrorExpr : TExprBase;

      procedure Execute(aTimeoutMilliSeconds : Integer = 0); overload;
      procedure ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0); overload;
      procedure ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0); overload;

      function  BeginProgram : Boolean;
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

   IdwsProgram = interface(IGetSelf)
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
      procedure SetExecutionsClass(aClass : TdwsProgramExecutionClass);

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
      property ExecutionsClass : TdwsProgramExecutionClass write SetExecutionsClass;
   end;

   TdwsCustomState = record
      Key : TGUID;
      Value : Variant;
   end;

   TdwsCustomStates = class (TSimpleHash<TdwsCustomState>)
      protected
         function SameItem(const item1, item2 : TdwsCustomState) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsCustomState) : Integer; override;

         function GetState(const index : TGUID) : Variant;
         procedure SetState(const index : TGUID; const v : Variant);

      public
         property States[const index : TGUID] : Variant read GetState write SetState; default;
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

         FResult : TdwsResult;
         FParameters : TData;
         FFileSystem : IdwsFileSystem;
         FEnvironment : IdwsEnvironment;
         FLocalizer : IdwsLocalizer;
         FRTTIRawAttributes : IScriptDynArray;

         FCustomStates : TdwsCustomStates;

         FOnExecutionStarted : TdwsExecutionEvent;
         FOnExecutionEnded : TdwsExecutionEvent;

         FRuntimeMsgs : TdwsRuntimeMessageList;

         FDebuggerFieldAddr : Integer;

      protected
         procedure ReleaseObjects;

         procedure ScriptObjCreated(scriptObj: TScriptObj);
         procedure ScriptObjDestroyed(scriptObj: TScriptObj);
         procedure DestroyScriptObj(const scriptObj: IScriptObj);

         function GetMsgs : TdwsRuntimeMessageList; override;
         function GetEnvironment : IdwsEnvironment;
         procedure SetEnvironment(const val : IdwsEnvironment);

         function GetCustomStates : TdwsCustomStates;

         // for interface only, script exprs use direct properties
         function GetProg : IdwsProgram;
         function GetInfo : TProgramInfo;
         function GetResult : TdwsResult;
         function GetObjectCount : Integer;
         function GetLocalizer : IdwsLocalizer;
         procedure SetLocalizer(const val : IdwsLocalizer);
         function GetLastScriptErrorExpr : TExprBase;

         procedure RaiseMaxRecursionReached;
         procedure SetCurrentProg(const val : TdwsProgram); inline;

         procedure RunProgramExpr(expr : TProgramExpr);

      public
         constructor Create(aProgram : TdwsMainProgram; const stackParams : TStackParameters); virtual;
         destructor Destroy; override;

         procedure Execute(aTimeoutMilliSeconds : Integer = 0); overload;
         procedure ExecuteParam(const Params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0); overload;
         procedure ExecuteParam(const Params : OleVariant; aTimeoutMilliSeconds : Integer = 0); overload;

         function  BeginProgram : Boolean; virtual;
         procedure RunProgram(aTimeoutMilliSeconds : Integer);
         procedure Stop;
         procedure EndProgram; virtual;

         procedure EnterRecursion(caller : TExprBase);
         procedure LeaveRecursion;

         function CallStackDepth : Integer; override;
         function GetCallStack : TdwsExprLocationArray; override;
         function CallStackLastExpr : TExprBase; override;
         function CallStackLastProg : TObject; override;

         function  DebuggerFieldAddr : Integer;
         procedure DebuggerNotifyException(const exceptObj : IScriptObj); override;

         class function CallStackToString(const callStack : TdwsExprLocationArray) : UnicodeString; static;
         procedure RaiseAssertionFailed(fromExpr : TExprBase; const msg : UnicodeString; const scriptPos : TScriptPos);
         procedure RaiseAssertionFailedFmt(fromExpr : TExprBase; const fmt : UnicodeString; const args : array of const; const scriptPos : TScriptPos);

         function CreateEDelphiObj(const ClassName : String;
                                   const Message : UnicodeString) : IScriptObj;

         procedure EnterExceptionBlock(var exceptObj : IScriptObj); override;

         function AcquireProgramInfo(funcSym : TFuncSymbol) : TProgramInfo;
         procedure ReleaseProgramInfo(info : TProgramInfo);

         procedure LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString); override;
         procedure LocalizeString(const aString : UnicodeString; var Result : UnicodeString); override;

         function ValidateFileName(const path : String) : String; override;

         property Prog : TdwsMainProgram read FProg;
         property CurrentProg : TdwsProgram read FCurrentProg write SetCurrentProg;
         property ProgramInfo : TProgramInfo read FProgramInfo;

         property Parameters : TData read FParameters;
         property Result : TdwsResult read FResult;
         property FileSystem : IdwsFileSystem read FFileSystem;
         property Environment : IdwsEnvironment read GetEnvironment write SetEnvironment;
         property CustomStates : TdwsCustomStates read GetCustomStates;
         property Localizer : IdwsLocalizer read FLocalizer write FLocalizer;
         property RTTIRawAttributes : IScriptDynArray read FRTTIRawAttributes write FRTTIRawAttributes;

         property ObjectCount : Integer read FObjectCount;

         property OnExecutionStarted : TdwsExecutionEvent read FOnExecutionStarted write FOnExecutionStarted;
         property OnExecutionEnded : TdwsExecutionEvent read FOnExecutionEnded write FOnExecutionEnded;
   end;

   TdwsProgramBaseTypes = record
      FTypBoolean : TTypeSymbol;
      FTypFloat : TTypeSymbol;
      FTypInteger : TTypeSymbol;
      FTypNil : TNilSymbol;
      FTypObject : TClassSymbol;
      FTypTObject : TClassSymbol;
      FTypString : TTypeSymbol;
      FTypVariant : TTypeSymbol;
      FTypException : TClassSymbol;
      FTypInterface : TInterfaceSymbol;
      FTypAnyType : TAnyTypeSymbol;
   end;

   TdwsProgramType = (ptScript, ptProgram, ptLibrary);

   // A script executable program
   TdwsProgram = class (TInterfacedSelfObject)
      private
         FExpr : TProgramExpr;
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
         function SubExpr(i : Integer) : TExprBase;
         function SubExprCount : Integer;

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

         property Expr : TProgramExpr read FExpr write FExpr;
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
         property TypTObject: TClassSymbol read FBaseTypes.FTypTObject;
         property TypString: TTypeSymbol read FBaseTypes.FTypString;
         property TypVariant: TTypeSymbol read FBaseTypes.FTypVariant;
         property TypException: TClassSymbol read FBaseTypes.FTypException;
         property TypInterface : TInterfaceSymbol read FBaseTypes.FTypInterface;
         property TypAnyType: TAnyTypeSymbol read FBaseTypes.FTypAnyType;
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
         FExecutionsLock : TFixedCriticalSection;
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
         FOrphanedObjects : TObjectList<TRefCountedObject>;

         FTypDefaultConstructor : TMethodSymbol;
         FTypDefaultDestructor : TMethodSymbol;

         FMainFileName : String;
         FDefaultEnvironment : IdwsEnvironment;
         FDefaultLocalizer : IdwsLocalizer;
         FOnExecutionStarted : TdwsExecutionEvent;
         FOnExecutionEnded : TdwsExecutionEvent;

         FExecutionsClass : TdwsProgramExecutionClass;

         FProgramType : TdwsProgramType;

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
         procedure SetExecutionsClass(aClass : TdwsProgramExecutionClass);

      public
         constructor Create(const systemTable : ISystemSymbolTable;
                            resultType : TdwsResultType;
                            const stackParameters : TStackParameters;
                            const mainFileName : String);
         destructor Destroy; override;

         function CreateNewExecution : IdwsProgramExecution;
         function BeginNewExecution : IdwsProgramExecution;
         function Execute(aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution;
         function ExecuteParam(const params : TVariantDynArray; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;
         function ExecuteParam(const params : OleVariant; aTimeoutMilliSeconds : Integer = 0) : IdwsProgramExecution; overload;

         function GetSourceFile(const aSourceFile : UnicodeString) : TSourceFile;

         function NextStackLevel(level : Integer) : Integer;

         procedure DropMapAndDictionary;

         function CollectAllPublishedSymbols(ignoreImplementationPublished : Boolean) : TSimpleSymbolList;

         procedure OrphanObject(obj : TRefCountedObject);

         property FinalExpr : TBlockFinalExpr read FFinalExpr write FFinalExpr;

         procedure AddFinalExpr(expr : TProgramExpr);

         property MainFileName : String read FMainFileName write FMainFileName;
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

         property TypDefaultConstructor : TMethodSymbol read FTypDefaultConstructor;
         property TypDefaultDestructor : TMethodSymbol read FTypDefaultDestructor;

         property DefaultEnvironment : IdwsEnvironment read FDefaultEnvironment write FDefaultEnvironment;
         property DefaultLocalizer : IdwsLocalizer read FDefaultLocalizer write FDefaultLocalizer;
         property DefaultUserObject : TObject read FDefaultUserObject write FDefaultUserObject;

         property OnExecutionStarted : TdwsExecutionEvent read FOnExecutionStarted write FOnExecutionStarted;
         property OnExecutionEnded : TdwsExecutionEvent read FOnExecutionEnded write FOnExecutionEnded;

         property ProgramType : TdwsProgramType read FProgramType write FProgramType;
   end;

   // Functions callable from a script program implement this interfaces
   ICallable = interface (IExecutable)
      ['{8D534D15-4C6B-11D5-8DCB-0000216D9E86}']
      procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol);
      procedure CompileTimeCheck(prog : TdwsProgram; expr : TFuncExprBase);
   end;

   IExternalRoutine = interface (ICallable)
      ['{1595278A-94F5-4B46-8173-C3604C93959C}']
      procedure SetExternalPointer(value : Pointer);
   end;

   TExternalRoutineFactory = function (funcSymbol : TFuncSymbol; mainProg : TdwsMainProgram) : IExternalRoutine;

   // A script procedure
   TdwsProcedure = class sealed (TdwsProgram, IUnknown, ICallable)
      private
         FFunc : TFuncSymbol;
         FPreConditions : TSourcePreConditions;
         FPostConditions : TSourcePostConditions;

      public
         constructor Create(aParent : TdwsProgram);
         destructor Destroy; override;

         procedure AssignTo(sym: TFuncSymbol);
         procedure Call(exec: TdwsProgramExecution; func: TFuncSymbol);
         procedure CompileTimeCheck(prog : TdwsProgram; expr : TFuncExprBase);
         procedure InitSymbol(Symbol: TSymbol; const msgs : TdwsCompileMessageList);
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
         function  Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; virtual;

         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsBoolean(exec : TdwsExecution) : Boolean; override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
         procedure EvalAsString(exec : TdwsExecution; var result : UnicodeString); override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
         procedure EvalAsScriptObj(exec : TdwsExecution; var result : IScriptObj); override;
         procedure EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface); override;
         procedure EvalAsScriptDynArray(exec : TdwsExecution; var result : IScriptDynArray); override;

         procedure AssignValue(exec : TdwsExecution; const value : Variant); override;
         procedure AssignValueAsInteger(exec : TdwsExecution; const value : Int64); override;
         procedure AssignValueAsBoolean(exec : TdwsExecution; const value : Boolean); override;
         procedure AssignValueAsFloat(exec : TdwsExecution; const value : Double); override;
         procedure AssignValueAsString(exec : TdwsExecution; const value: UnicodeString); override;
         procedure AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj); override;
         procedure AssignValueAsScriptDynArray(exec : TdwsExecution; const value : IScriptDynArray); override;

         procedure RaiseUpperExceeded(exec : TdwsExecution; index : Integer);
         procedure RaiseLowerExceeded(exec : TdwsExecution; index : Integer);

         function ScriptLocation(prog : TObject) : UnicodeString; override;

         function InterruptsFlow : Boolean; virtual;

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

         procedure CheckInterface(exec : TdwsExecution; const scriptObj : IScriptObj); overload; inline;
         procedure CheckInterface(exec : TdwsExecution; const intf : IScriptObjInterface); overload; inline;
         procedure RaiseInterfaceIsNil(exec : TdwsExecution);

         function IsOfType(typSym : TTypeSymbol) : Boolean;

         function SameDataExpr(expr : TTypedExpr) : Boolean; virtual;

         property Typ : TTypeSymbol read FTyp write FTyp;
   end;

   TTypedExprClass = class of TTypedExpr;

   // hosts a type reference
   TTypeReferenceExpr = class sealed (TTypedExpr)
      private
         FScriptPos : TScriptPos;

      public
         constructor Create(aTyp : TTypeSymbol; const aScriptPos : TScriptPos);

         function Eval(exec : TdwsExecution) : Variant; override;
         function ScriptPos : TScriptPos; override;
  end;

   // base class of expressions that return no result
   TNoResultExpr = class(TProgramExpr)
      protected
         FScriptPos : TScriptPos;

      public
         constructor Create(const aPos: TScriptPos);

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         function ScriptPos : TScriptPos; override;
         procedure SetScriptPos(const aPos : TScriptPos);
   end;

   // Does nothing! E. g.: "for x := 1 to 10 do {TNullExpr};"
   TNullExpr = class (TNoResultExpr)
      procedure EvalNoResult(exec : TdwsExecution); override;
   end;

   // invalid expression
   TErrorExpr = class (TNullExpr)
   end;

   // statement; statement; statement;
   TBlockExprBase = class(TNoResultExpr)
      protected
         FStatements : PProgramExprList;
         FCount : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         destructor Destroy; override;

         procedure AddStatement(expr : TProgramExpr);
         procedure ReplaceStatement(index : Integer; expr : TProgramExpr);
         function ExtractStatement(index : Integer) : TProgramExpr;

         property StatementCount : Integer read FCount;
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
         function GetDataPtrFunc(exec : TdwsExecution) : IDataContext; inline;

      public
         constructor Create(aTyp: TTypeSymbol);

         procedure AssignData(exec : TdwsExecution; const source : IDataContext); virtual;
         procedure AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr); virtual;
         procedure AssignExpr(exec : TdwsExecution; Expr: TTypedExpr); virtual;
         procedure AssignValue(exec : TdwsExecution; const Value: Variant); override;

         function Eval(exec : TdwsExecution) : Variant; override;
         function IsWritable: Boolean; virtual;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); virtual; abstract;

         property DataPtr[exec : TdwsExecution] : IDataContext read GetDataPtrFunc;
   end;

   // Encapsulates data
   TPosDataExpr = class(TDataExpr)
      protected
         FScriptPos : TScriptPos;

      public
         constructor Create(const scriptPos : TScriptPos; aTyp: TTypeSymbol);

         function ScriptPos : TScriptPos; override;
   end;

   // TExternalFuncHandler
   //
   TExternalFuncHandler = class(TInterfacedSelfObject, IUnknown, ICallable, IExecutable)
      public
         procedure InitSymbol(symbol: TSymbol; const msgs : TdwsCompileMessageList);
         procedure InitExpression(Expr: TExprBase);
         procedure Call(exec : TdwsProgramExecution; func : TFuncSymbol);
         procedure CompileTimeCheck(prog : TdwsProgram; expr : TFuncExprBase);
         function SubExpr(i : Integer) : TExprBase;
         function SubExprCount : Integer;
   end;

   EdwsExternalFuncHandler = class (Exception);

   // TFuncExprBase
   //
   TFuncExprBase = class(TPosDataExpr)
      private
         FFunc : TFuncSymbol;
         FParamSize : Integer;

      protected
         FArgs : TExprBaseListRec;
         FResultAddr : Integer;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetIsConstant : Boolean; override;

         property ParamSize : Integer read FParamSize;

      public
         constructor Create(const aScriptPos : TScriptPos; aFunc : TFuncSymbol);
         destructor Destroy; override;

         procedure AddArg(arg : TTypedExpr);
         procedure ClearArgs;
         function ExpectedArg : TParamSymbol; virtual; abstract;
         function GetArgType(idx : Integer) : TTypeSymbol;
         function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
         procedure CompileTimeCheck(prog : TdwsProgram); virtual;

         procedure Initialize(prog : TdwsProgram); virtual;

         procedure SetResultAddr(prog : TdwsProgram; exec : TdwsExecution; ResultAddr: Integer = -1);
         property ResultAddr : Integer read FResultAddr;

         function ChangeFuncSymbol(aProg: TdwsProgram; newFuncSym : TFuncSymbol) : TFuncExprBase; virtual;

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property FuncSym : TFuncSymbol read FFunc;
         property Args : TExprBaseListRec read FArgs write FArgs;
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
                        potAddr, potPassAddr, potTempAddr, potTempData, potTempArrayAddr, potTempArray,
                        potResult,
                        potResultInteger, potResultFloat, potResultBoolean,
                        potResultString, potResultConstString,
                        potData, potConstData,
                        potLazy, potInitResult);
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
      procedure ExecuteTempData(exec : TdwsExecution);
      procedure ExecuteTempArrayAddr(exec : TdwsExecution);
      procedure ExecuteTempArray(exec : TdwsExecution);
      procedure ExecuteResult(exec : TdwsExecution);
      procedure ExecuteResultBoolean(exec : TdwsExecution);
      procedure ExecuteResultInteger(exec : TdwsExecution);
      procedure ExecuteResultFloat(exec : TdwsExecution);
      procedure ExecuteResultString(exec : TdwsExecution);
      procedure ExecuteResultConstString(exec : TdwsExecution);
      procedure ExecuteData(exec : TdwsExecution);
      procedure ExecuteConstData(exec : TdwsExecution);
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
         procedure StaticPostCall(exec : TdwsExecution; var Result : Variant);
         procedure StaticPostCallInteger(exec : TdwsExecution; var Result : Int64);
         procedure StaticPostCallFloat(exec : TdwsExecution; var Result : Double);

         procedure EvalPushExprs(exec : TdwsExecution); inline;

         procedure DoEvalCall(exec : TdwsExecution; func : TFuncSymbol);

      public
         constructor Create(aProg : TdwsProgram; const scriptPos : TScriptPos; func : TFuncSymbol);
         destructor Destroy; override;

         function ExpectedArg : TParamSymbol; override;

         procedure AddPushExprs(prog : TdwsProgram);

         function Eval(exec : TdwsExecution) : Variant; override;

         procedure Initialize(prog : TdwsProgram); override;
         function IsWritable : Boolean; override;
         procedure CompileTimeCheck(prog : TdwsProgram); override;

         function FuncSymQualifiedName : UnicodeString; override;

         property CallerID : TFuncExpr read FCallerID write FCallerID;
         property Level : SmallInt read FLevel write FLevel;
   end;

   // A simple function/procedure (not a method, not a function pointer);
   TFuncSimpleExpr = class(TFuncExpr)
      public
         procedure EvalNoResult(exec : TdwsExecution); override;
         function  EvalAsInteger(exec : TdwsExecution) : Int64; override;
         function  EvalAsFloat(exec : TdwsExecution) : Double; override;
   end;

   IFuncPointer = interface
      function GetFuncExpr : TFuncExprBase;
      function SameFunc(const v : Variant) : Boolean;
      procedure EvalAsVariant(exec : TdwsExecution; caller : TFuncExpr; var result : Variant);
      function EvalAsInteger(exec : TdwsExecution; caller : TFuncExpr) : Int64;
      function EvalDataPtr(exec : TdwsExecution; caller : TFuncExpr) : IDataContext;
   end;

   TFuncPointerEvalAsVariant = procedure (exec : TdwsExecution; caller : TFuncExpr; var result : Variant) of object;
   TFuncPointerEvalAsInteger = procedure (exec : TdwsExecution; caller : TFuncExpr; var result : Int64) of object;

   // Encapsulates a function or method pointer
   TFuncPointer = class(TInterfacedObject, IUnknown, IFuncPointer)
      private
         FFuncExpr : TFuncExprBase;
         FDoEvalAsVariant : TFuncPointerEvalAsVariant;

      protected
         procedure EvalMagicAsVariant(exec : TdwsExecution; caller : TFuncExpr; var result : Variant);
         procedure EvalFuncAsVariant(exec : TdwsExecution; caller : TFuncExpr; var result : Variant);

      public
         constructor Create(exec : TdwsExecution; funcExpr : TFuncExprBase);
         destructor Destroy; override;

         function GetFuncExpr : TFuncExprBase;
         function SameFunc(const v : Variant) : Boolean;

         procedure EvalAsVariant(exec : TdwsExecution; caller : TFuncExpr; var result : Variant);
         function EvalAsInteger(exec : TdwsExecution; caller : TFuncExpr) : Int64;
         function EvalDataPtr(exec : TdwsExecution; caller : TFuncExpr) : IDataContext;
   end;

   // returns an IFuncPointer to the FuncExpr
   TAnonymousFuncRefExpr = class(TDataExpr)
      private
         FFuncExpr : TFuncExprBase;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

      public
         constructor Create(prog : TdwsProgram; funcExpr : TFuncExprBase);
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsVariant(exec : TdwsExecution; var Result : Variant); override;

         function Extract : TFuncExprBase; // also a destructor

         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;

         property FuncExpr : TFuncExprBase read FFuncExpr write FFuncExpr;
   end;

   TFuncRefExpr = class (TAnonymousFuncRefExpr)
   end;

   TFuncPtrExpr = class sealed (TFuncExpr)
      private
         FCodeExpr : TTypedExpr;

      protected
         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(prog : TdwsProgram; const aScriptPos : TScriptPos; codeExpr : TTypedExpr);
         destructor Destroy; override;

         procedure EvalAsFuncPointer(exec : TdwsExecution; var result : IFuncPointer); inline;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalNoResult(exec : TdwsExecution); override;

         function Extract : TTypedExpr; // also a destructor

         property CodeExpr : TTypedExpr read FCodeExpr write FCodeExpr;
   end;

   TMethodObjExpr = class(TPosDataExpr)
      private
         FBaseExpr : TDataExpr;

      public
         constructor Create(Prog: TdwsProgram; const aScriptPos: TScriptPos; BaseExpr: TDataExpr);
         procedure GetDataPtr(exec : TdwsExecution; var result : IDataContext); override;
   end;

   TSourceCondition = class (TInterfacedSelfObject, IBooleanEvalable, IStringEvalable)
      private
         FScriptPos : TScriptPos;
         FTest : TTypedExpr;
         FMsg : TTypedExpr;

      public
         constructor Create(const aScriptPos: TScriptPos; aTest, aMsg : TTypedExpr);
         destructor Destroy; override;

         procedure InitSymbol(symbol: TSymbol; const msgs : TdwsCompileMessageList);
         procedure InitExpression(Expr: TExprBase);
         function SubExpr(i : Integer) : TExprBase;
         function SubExprCount : Integer;

         function EvalAsBoolean(exec : TdwsExecution) : Boolean;
         procedure EvalAsString(exec : TdwsExecution; var Result : UnicodeString);

         property ScriptPos : TScriptPos read FScriptPos write FScriptPos;
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

   TUnaryOpExpr = class(TTypedExpr)
      protected
         FExpr : TTypedExpr;

         function GetSubExpr(i : Integer) : TExprBase; override;
         function GetSubExprCount : Integer; override;

         function GetIsConstant : Boolean; override;

      public
         constructor Create(prog : TdwsProgram; expr : TTypedExpr); virtual;
         destructor Destroy; override;

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

   // UnicodeString unary result
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

         function GetIsConstant : Boolean; override;

      public
         constructor Create(const aScriptPos: TScriptPos; Expr: TTypedExpr);
         destructor Destroy; override;

         procedure EvalNoResult(exec : TdwsExecution); override;

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

         function GetIsConstant : Boolean; override;

      public
         constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); virtual;
         destructor Destroy; override;

         function Eval(exec : TdwsExecution) : Variant; override;
         procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;

         procedure OptimizeConstantOperandsToFloats(prog : TdwsProgram; exec : TdwsExecution);

         procedure Swap;

         property Typ : TTypeSymbol read FTyp write FTyp;
         property Left : TTypedExpr read FLeft write FLeft;
         property Right : TTypedExpr read FRight write FRight;
   end;

   TBinaryOpExprClass = class of TBinaryOpExpr;

   TVariantBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TIntegerBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
     procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
     function EvalAsFloat(exec : TdwsExecution) : Double; override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TStringBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
     procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TFloatBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
     procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
     function Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr; override;
   end;
   TBooleanBinOpExpr = class(TBinaryOpExpr)
     constructor Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr); override;
     procedure EvalAsVariant(exec : TdwsExecution; var result : Variant); override;
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
      function GetConstructor(const MethName: UnicodeString; ExtObject: TObject): IInfo;
      function GetData : TData;
      function GetExternalObject: TObject;
      function GetMember(const s: UnicodeString): IInfo;
      function GetFieldMemberNames : TStrings;
      function GetMethod(const s: UnicodeString): IInfo;
      function GetScriptObj: IScriptObj;
      function GetScriptDynArray: IScriptDynArray;
      function GetParameter(const s: UnicodeString): IInfo;
      function GetTypeSym: TSymbol;
      function GetValue : Variant;
      function GetValueAsString : UnicodeString;
      function GetValueAsDataString : RawByteString;
      function GetValueAsInteger : Int64;
      function GetValueAsBoolean : Boolean;
      function GetValueAsFloat : Double;
      function GetInherited: IInfo;
      function GetExec : IdwsProgramExecution;
      procedure SetData(const Data: TData);
      procedure SetExternalObject(ExtObject: TObject);
      procedure SetValue(const Value: Variant);
      procedure SetValueAsInteger(const value : Int64);
      procedure SetValueAsString(const value : UnicodeString);

      property Data: TData read GetData write SetData;
      property ExternalObject: TObject read GetExternalObject write SetExternalObject;
      property Member[const s : UnicodeString]: IInfo read GetMember;
      property FieldMemberNames : TStrings read GetFieldMemberNames;
      property Method[const s : UnicodeString]: IInfo read GetMethod;

      property Exec: IdwsProgramExecution read GetExec;
      property ScriptObj: IScriptObj read GetScriptObj;
      property ScriptDynArray: IScriptDynArray read GetScriptDynArray;
      property Parameter[const s: UnicodeString]: IInfo read GetParameter;
      property TypeSym: TSymbol read GetTypeSym;
      property Value: Variant read GetValue write SetValue;
      property ValueAsString : UnicodeString read GetValueAsString write SetValueAsString;
      property ValueAsDataString : RawByteString read GetValueAsDataString;
      property ValueAsInteger : Int64 read GetValueAsInteger write SetValueAsInteger;
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
         function GetData(const s: UnicodeString): TData;
         function GetFunc(const s: UnicodeString): IInfo;
         function GetFuncBySym(funcSym : TFuncSymbol): IInfo;
         procedure SetFuncSym(const Value: TFuncSymbol);
         function GetValueAsVariant(const s: UnicodeString): Variant;
         procedure GetSymbolInfo(sym : TSymbol; var info : IInfo);
         function GetVars(const str: UnicodeString): IInfo;
         function GetParams(const Index: Integer): IInfo;
         procedure SetData(const s: UnicodeString; const Value: TData);
         procedure SetValueAsVariant(const s: UnicodeString; const Value: Variant);
         function GetResultAsVariant: Variant;
         function GetResultVars: IInfo;

         function GetValueAsString(const s: UnicodeString): UnicodeString;
         procedure SetValueAsString(const s: UnicodeString; const Value: UnicodeString);
         function GetValueAsChar(const s: UnicodeString): WideChar;
         function GetValueAsDataString(const s: UnicodeString): RawByteString;
         procedure SetValueAsDataString(const s: UnicodeString; const Value: RawByteString);
         function GetValueAsInteger(const s: UnicodeString): Int64;
         procedure SetValueAsInteger(const s: UnicodeString; const Value: Int64);
         function GetValueAsBoolean(const s: UnicodeString): Boolean;
         procedure SetValueAsBoolean(const s: UnicodeString; const Value: Boolean);
         function GetValueAsFloat(const s: UnicodeString): Double;
         procedure SetValueAsFloat(const s: UnicodeString; const Value: Double);
         function GetValueAsObject(const s: UnicodeString): TObject;
         function GetValueAsClassSymbol(const s: UnicodeString): TClassSymbol;
         function GetValueAsTStrings(const s: UnicodeString): TStrings;

         function GetResultAsPVariant : PVariant;

         procedure SetResultAsVariant(const Value: Variant);
         procedure SetResultAsString(const value : UnicodeString);
         procedure SetResultAsDataString(const value : RawByteString);
         procedure SetResultAsInteger(const value : Int64);
         procedure SetResultAsBoolean(const value : Boolean);
         procedure SetResultAsFloat(const value : Double);

         function GetParamAsPVariant(index : Integer) : PVariant;
         function GetParamAsVariant(index : Integer) : Variant;
         procedure SetParamAsVariant(index : Integer; const v : Variant);
         function GetParamAsInteger(index : Integer) : Int64;
         procedure SetParamAsInteger(index : Integer; const v : Int64);
         function GetParamAsString(index : Integer) : UnicodeString;
         procedure SetParamAsString(index : Integer; const v : UnicodeString);
         function GetParamAsDataString(index : Integer) : RawByteString;
         procedure SetParamAsDataString(index : Integer; const v : RawByteString);
         function GetParamAsFloat(index : Integer) : Double;
         function GetParamAsBoolean(index : Integer) : Boolean;
         function GetParamAsObject(index : Integer) : TObject;

         function CreateUnitList : TUnitSymbolRefList;
         function FindSymbolInUnits(aUnitList: TUnitSymbolRefList; const aName: UnicodeString) : TSymbol; overload;
         function GetSystemTable : TSystemSymbolTable;

      public
         procedure PrepareScriptObj;

         function RegisterExternalObject(AObject: TObject; AutoFree: Boolean=False; ExactClassMatch: Boolean=True): IScriptObj;
         function GetExternalObjForVar(const s: UnicodeString): TObject;
         // cycle ancestry hierarchy and find the nearest matching type
         function FindClassMatch(AObject: TObject; ExactMatch: Boolean=True): TClassSymbol;
         function FindSymbolInUnits(const aName : UnicodeString) : TSymbol; overload;
         function GetTemp(const DataType: UnicodeString): IInfo;

         procedure RaiseExceptObj(const msg : UnicodeString; const obj : IScriptObj);

         property Table : TSymbolTable read FTable write FTable;
         property SystemTable : TSystemSymbolTable read GetSystemTable;
         property Execution : TdwsProgramExecution read FExecution write FExecution;
         property Level : Integer read FLevel write FLevel;
         property Data[const s: UnicodeString]: TData read GetData write SetData;
         property Func[const s: UnicodeString]: IInfo read GetFunc;
         property FuncBySym[funcSym: TFuncSymbol]: IInfo read GetFuncBySym;
         property FuncSym: TFuncSymbol read FFuncSym write SetFuncSym;
         property Method[const s: UnicodeString]: IInfo read GetFunc;
         property ScriptObj: IScriptObj read FScriptObj write FScriptObj;
         property ResultAsVariant: Variant read GetResultAsVariant write SetResultAsVariant;
         property ResultVars: IInfo read GetResultVars;
         property Vars[const s: UnicodeString]: IInfo read GetVars;
         property Params[const Index: Integer]: IInfo read GetParams;

         property ValueAsVariant[const s : UnicodeString] : Variant read GetValueAsVariant write SetValueAsVariant;
         property ValueAsChar[const s : UnicodeString] : WideChar read GetValueAsChar;
         property ValueAsString[const s : UnicodeString] : UnicodeString read GetValueAsString write SetValueAsString;
         property ValueAsDataString[const s : UnicodeString] : RawByteString read GetValueAsDataString write SetValueAsDataString;
         property ValueAsInteger[const s : UnicodeString] : Int64 read GetValueAsInteger write SetValueAsInteger;
         property ValueAsBoolean[const s : UnicodeString] : Boolean read GetValueAsBoolean write SetValueAsBoolean;
         property ValueAsFloat[const s : UnicodeString] : Double read GetValueAsFloat write SetValueAsFloat;
         property ValueAsObject[const s : UnicodeString] : TObject read GetValueAsObject;
         property ValueAsClassSymbol[const s : UnicodeString] : TClassSymbol read GetValueAsClassSymbol;
         property ValueAsTStrings[const s : UnicodeString] : TStrings read GetValueAsTStrings;

         property ParamAsPVariant[index : Integer] : PVariant read GetParamAsPVariant;
         property ParamAsVariant[index : Integer] : Variant read GetParamAsVariant write SetParamAsVariant;
         property ParamAsInteger[index : Integer] : Int64 read GetParamAsInteger write SetParamAsInteger;
         property ParamAsString[index : Integer] : UnicodeString read GetParamAsString write SetParamAsString;
         property ParamAsDataString[index : Integer] : RawByteString read GetParamAsDataString write SetParamAsDataString;
         property ParamAsFloat[index : Integer] : Double read GetParamAsFloat;
         property ParamAsBoolean[index : Integer] : Boolean read GetParamAsBoolean;
         property ParamAsObject[index : Integer] : TObject read GetParamAsObject;

         property ResultAsString : UnicodeString write SetResultAsString;
         property ResultAsDataString : RawByteString write SetResultAsDataString;
         property ResultAsBoolean : Boolean write SetResultAsBoolean;
         property ResultAsInteger : Int64 write SetResultAsInteger;
         property ResultAsFloat : Double write SetResultAsFloat;
  end;

   // An instance of a script class FClassSym. Instance data in FData,
   TScriptObj = class (TDataContext)
      private
         FNextObject, FPrevObject : TScriptObj;

      protected
         procedure SetExecutionContext(exec : TdwsProgramExecution); virtual;

      public
         property NextObject : TScriptObj read FNextObject write FNextObject;
         property PrevObject : TScriptObj read FPrevObject write FPrevObject;
   end;

   TScriptObjInstance = class (TScriptObj, IScriptObj)
      private
         FClassSym : TClassSymbol;
         FExternalObject : TObject;
         FExecutionContext : TdwsProgramExecution;
         FOnObjectDestroy: TObjectDestroyEvent;
         FDestroyed : Boolean;

      protected
         function GetClassSym: TClassSymbol;
         function GetExternalObject: TObject;
         procedure SetExternalObject(Value: TObject);
         function GetDestroyed : Boolean;
         procedure SetDestroyed(const val : Boolean);
         procedure SetExecutionContext(exec : TdwsProgramExecution); override;

      public
         constructor Create(aClassSym : TClassSymbol; executionContext : TdwsProgramExecution = nil);
         destructor Destroy; override;
         procedure BeforeDestruction; override;

         function ToString : String; override;

         procedure ClearData; override;

         property ClassSym : TClassSymbol read FClassSym;
         property ExecutionContext : TdwsProgramExecution read FExecutionContext write FExecutionContext;
         property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
         property Destroyed : Boolean read FDestroyed write FDestroyed;
         property ExternalObject : TObject read FExternalObject write FExternalObject;
   end;

   TScriptDynamicArray = class abstract (TScriptObj, IScriptDynArray)
      private
         FElementTyp : TTypeSymbol;
         FElementSize : Integer;
         FArrayLength : Integer;

      protected
         function GetElementSize : Integer;
         procedure SetArrayLength(n : Integer);
         function GetArrayLength : Integer;

      public
         class function CreateNew(elemTyp : TTypeSymbol) : TScriptDynamicArray; static;

         procedure Delete(index, count : Integer);
         procedure Insert(index : Integer);
         procedure Swap(i1, i2 : Integer); virtual; abstract;
         procedure Reverse;
         procedure Copy(src : TScriptDynamicArray; index, count : Integer);
         procedure RawCopy(const src : TData; rawIndex, rawCount : Integer);
         procedure Concat(src : TScriptDynamicArray);

         function IndexOfData(const item : IDataContext; fromIndex : Integer) : Integer;
         function IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
         function IndexOfString(const item : UnicodeString; fromIndex : Integer) : Integer;
         function IndexOfInteger(const item : Int64; fromIndex : Integer) : Integer;
         function IndexOfFuncPtr(const item : Variant; fromIndex : Integer) : Integer;

         function ToString : String; override;
         function ToStringArray : TStringDynArray;

         procedure ReplaceData(const newData : TData); override;

         property ElementTyp : TTypeSymbol read FElementTyp;
         property ElementSize : Integer read FElementSize;
         property ArrayLength : Integer read FArrayLength write SetArrayLength;
   end;

   TScriptDynamicDataArray = class (TScriptDynamicArray)
      public
         procedure Swap(i1, i2 : Integer); override;
   end;

   TScriptDynamicValueArray = class (TScriptDynamicArray)
      public
         procedure Swap(i1, i2 : Integer); override;

         function CompareString(i1, i2 : Integer) : Integer;
         function CompareInteger(i1, i2 : Integer) : Integer;
         function CompareFloat(i1, i2 : Integer) : Integer;
   end;

   TScriptInterface = class(TScriptObj, IScriptObjInterface)
      private
         FTyp : TInterfaceSymbol;
         FInstance : IScriptObj;
         FVMT : TMethodSymbolArray;
         FExecutionContext : TdwsProgramExecution;

      protected
         function GetScriptObj : IScriptObj;

      public
         constructor Create(const instance : IScriptObj;
                            const resolvedInterface : TResolvedInterface;
                            executionContext : TdwsProgramExecution = nil);
         procedure BeforeDestruction; override;

         function ToString : String; override;

         property Typ : TInterfaceSymbol read FTyp;
         property Instance : IScriptObj read FInstance;
         property VMT : TMethodSymbolArray read FVMT write FVMT;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsFunctions, dwsCoreExprs, dwsMagicExprs, dwsMethodExprs,
   dwsInfo, dwsCompilerUtils, dwsConstExprs, dwsResultFunctions;

{ TScriptObjectWrapper }

// wrapper to interact with an released script object
type
   TScriptObjectWrapper = class (TDataContext, IUnknown, IScriptObj)
      private
         FScriptObj : TScriptObjInstance;
      protected
         { IScriptObj }
         function GetClassSym: TClassSymbol;
         function GetSelf : TObject;
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
   ReplaceData(scriptObj.AsData);
end;

// GetClassSym
//
function TScriptObjectWrapper.GetClassSym: TClassSymbol;
begin
   Result := FScriptObj.FClassSym;
end;

// GetSelf
//
function TScriptObjectWrapper.GetSelf: TObject;
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
procedure RaiseVariableNotFound(const s : UnicodeString);
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
   FRuntimeMsgs.Free;
   FEnvironment:=nil;

   FProg._Release;
   inherited;
end;

// Execute
//
procedure TdwsProgramExecution.Execute(aTimeoutMilliSeconds : Integer = 0);
begin
   if BeginProgram then begin
      if ProgramState=psRunning then
         RunProgram(aTimeoutMilliSeconds);
   end;
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
function TdwsProgramExecution.BeginProgram : Boolean;
begin
   Result:=False;

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

   if Assigned(FOnExecutionStarted) then
      FOnExecutionStarted(Self);

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
      Stack.Push(FProg.FGlobalAddrGenerator.DataSize+FProg.DataSize);
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
      RunProgramExpr(FProg.FInitExpr);

      if not (FProg.Expr is TBlockExprBase) then
         DoStep(FProg.FExpr);

      Result:=True;
   except
      // we should never end up here
      FProgramState:=psRunningStopped;
      raise;
   end;
end;

// RunProgramExpr
//
procedure TdwsProgramExecution.RunProgramExpr(expr : TProgramExpr);

   procedure Handle_EScriptAssertionFailed(e : EScriptAssertionFailed);
   begin
      if IsDebugging then
         Debugger.NotifyException(Self, e.ExceptionObj);
      Msgs.AddRuntimeError(e.ScriptPos,
                            Copy(e.Message, 1, LastDelimiter('[', e.Message)-2)
                           +StrAfterChar(e.Message, ']'),
                           e.ScriptCallStack);
   end;

   procedure Handle_Exception(e : Exception);
   var
      debugPos : TScriptPos;
      exceptObj : IScriptObj;
   begin
      EnterExceptionBlock(exceptObj);
      try
         if LastScriptError<>nil then begin
            if LastScriptError is TFuncExpr then
               Msgs.AddRuntimeError(LastScriptError.ScriptPos,
                                    e.Message+' in '+TFuncExpr(LastScriptError).FuncSym.QualifiedName,
                                    LastScriptCallStack)
            else Msgs.AddRuntimeError(LastScriptError.ScriptPos, e.Message,
                                      LastScriptCallStack)
         end else if (Debugger<>nil) and (Debugger.LastDebugStepExpr<>nil) then begin
            debugPos:=Debugger.LastDebugStepExpr.ScriptPos;
            debugPos.Col:=0;
            Msgs.AddRuntimeError(debugPos, e.Message, LastScriptCallStack)
         end else Msgs.AddRuntimeError(cNullPos, e.Message, LastScriptCallStack);
      finally
         LeaveExceptionBlock;
      end;
   end;

begin
   Status:=esrNone;
   try
      // Run the script
      expr.EvalNoResult(Self);

      if status<>esrNone then begin
         case status of
            esrBreak : Msgs.AddRuntimeError(RTE_InvalidBreak);
            esrContinue : Msgs.AddRuntimeError(RTE_InvalidContinue);
         end;
      end;
   except
      on e: EScriptAssertionFailed do
         Handle_EScriptAssertionFailed(e);
      on e: EScriptException do begin
         if IsDebugging then
            Debugger.NotifyException(Self, e.ExceptionObj);
         Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
      end;
      on e: EScriptError do
         Msgs.AddRuntimeError(e.ScriptPos, e.Message, e.ScriptCallStack);
      on e: EScriptStackException do
         Msgs.AddRuntimeError(LastScriptError.ScriptPos,
                              e.Message,
                              LastScriptCallStack);
      on e: Exception do
         Handle_Exception(e);
   end;
end;

// RunProgram
//
procedure TdwsProgramExecution.RunProgram(aTimeoutMilliSeconds : Integer);
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
      RunProgramExpr(FProg.FExpr);
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
      RunProgramExpr(FProg.FFinalExpr);

   FProgramState:=psTerminated;
   try
      // Stack
      Stack.PopBp(0);
      Stack.Pop(Stack.StackPointer); // FProg.FAddrGenerator.DataSize + FProg.FGlobalAddrGenerator.DataSize);

      // Object Cycles
      ReleaseObjects;

      // Custom states
      FCustomStates.Free;
      FCustomStates:=nil;

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

   if Assigned(FOnExecutionEnded) then
      FOnExecutionEnded(Self);
end;

// CallStackToString
//
class function TdwsProgramExecution.CallStackToString(const callStack : TdwsExprLocationArray) : UnicodeString;
begin
   Result:=TExprBase.CallStackToString(callStack);
end;

// RaiseAssertionFailed
//
procedure TdwsProgramExecution.RaiseAssertionFailed(fromExpr : TExprBase; const msg : UnicodeString; const scriptPos : TScriptPos);
begin
   RaiseAssertionFailedFmt(fromExpr, RTE_AssertionFailed, [scriptPos.AsInfo, msg], scriptPos);
end;

// RaiseAssertionFailedFmt
//
procedure TdwsProgramExecution.RaiseAssertionFailedFmt(fromExpr : TExprBase; const fmt : UnicodeString; const args : array of const; const scriptPos : TScriptPos);
var
   exceptObj : IScriptObj;
   fmtMsg : UnicodeString;
begin
   SetScriptError(fromExpr);
   fmtMsg:=Format(fmt, args);
   exceptObj:=IScriptObj(IUnknown(ProgramInfo.Vars[SYS_EASSERTIONFAILED].Method[SYS_TOBJECT_CREATE].Call([fmtMsg]).Value));
   (exceptObj.ExternalObject as TdwsExceptionContext).Skip(1); // temporary constructor expression
   if IsDebugging then
      DebuggerNotifyException(exceptObj);
   raise EScriptAssertionFailed.Create(fmtMsg, exceptObj, scriptPos)
end;

// CreateEDelphiObj
//
function TdwsProgramExecution.CreateEDelphiObj(const ClassName : String;
                                   const Message : UnicodeString) : IScriptObj;
begin
   Result := IScriptObj(IUnknown(
      ProgramInfo.Vars[SYS_EDELPHI].Method[SYS_TOBJECT_CREATE].Call([ClassName, Message]).Value));
   (Result.ExternalObject as TdwsExceptionContext).ReplaceTop(LastScriptError); // temporary constructor expression
end;

// EnterExceptionBlock
//
procedure TdwsProgramExecution.EnterExceptionBlock(var exceptObj : IScriptObj);
var
   mainException : Exception;
   err : EScriptError;
   msg : UnicodeString;
begin
   if ExceptionObjectStack.Count>Stack.MaxExceptionDepth then
      raise EScriptExceptionOverflow.CreateFmt(RTE_MaximalExceptionDepthExceeded,
                                               [ExceptionObjectStack.Count]);

   {$ifdef FPC}
   mainException:=SysUtils.ExceptObject as Exception;
   {$else}
   mainException:=System.ExceptObject as Exception;
   {$endif}

   if mainException is EScriptException then begin
      // a raise-statement created an Exception object
      exceptObj:=EScriptException(mainException).ExceptionObj
   end else if mainException is EScriptError then begin
      msg:=mainException.Message;
      err:=EScriptError(mainException);
      if Length(err.ScriptCallStack)>0 then
         msg:=msg+' in '+(err.ScriptCallStack[High(err.ScriptCallStack)].Expr as TFuncExpr).FuncSym.QualifiedName;
      if EScriptError(mainException).ScriptPos.Defined then
         msg:=msg+EScriptError(mainException).ScriptPos.AsInfo;
      exceptObj:=CreateEDelphiObj(mainException.ClassName, msg);
   end else if mainException is EScriptStackOverflow then begin
      exceptObj:=nil
   end else begin
      // A Delphi exception. Transform it to a EDelphi-dws exception
      exceptObj:=CreateEDelphiObj(mainException.ClassName, mainException.Message);
   end;

   inherited EnterExceptionBlock(exceptObj);
   if IsDebugging then
      DebuggerNotifyException(exceptObj);
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
procedure TdwsProgramExecution.LocalizeSymbol(aResSymbol : TResourceStringSymbol; var Result : UnicodeString);
begin
   if Assigned(Localizer) then
      Localizer.LocalizeSymbol(aResSymbol, Result)
   else Result:=aResSymbol.Value;
end;

// LocalizeString
//
procedure TdwsProgramExecution.LocalizeString(const aString : UnicodeString; var Result : UnicodeString);
begin
   if Assigned(Localizer) then
      Localizer.LocalizeString(aString, Result)
   else Result:=aString;
end;

// ValidateFileName
//
function TdwsProgramExecution.ValidateFileName(const path : String) : String;
begin
   if Assigned(FileSystem) then
      Result:=FileSystem.ValidateFileName(path)
   else Result:='';
   if Result='' then
      Result:=inherited ValidateFileName(path);
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
      iter.ClearData;
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
   destroySym : TMethodSymbol;
   expr : TDestructorVirtualExpr;
   oldStatus : TExecutionStatusResult;
   caller : TExprBase;
begin
   if scriptObj.ClassSym.IsExternalRooted then Exit;
   try
      destroySym:=Prog.TypDefaultDestructor;
      expr := TDestructorVirtualExpr.Create(FProg, cNullPos, destroySym,
                                            TConstExpr.Create(FProg, ScriptObj.ClassSym, ScriptObj));

      caller:=CallStackLastExpr;
      if caller<>nil then begin
         // called from script
         expr.Level:=(caller as TFuncExpr).Level;
         if expr.Level=0 then
            expr.Level:=1;
      end else begin
         // called from Delphi-side outside of script
         expr.Level:=0;
      end;

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

// GetLastScriptErrorExpr
//
function TdwsProgramExecution.GetLastScriptErrorExpr : TExprBase;
begin
   Result:=LastScriptError;
end;

// GetMsgs
//
function TdwsProgramExecution.GetMsgs : TdwsRuntimeMessageList;
begin
   if FRuntimeMsgs=nil then
      FRuntimeMsgs:=TdwsRuntimeMessageList.Create;
   Result:=FRuntimeMsgs;
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

// GetCustomStates
//
function TdwsProgramExecution.GetCustomStates : TdwsCustomStates;
begin
   if FCustomStates=nil then
      FCustomStates:=TdwsCustomStates.Create;
   Result:=FCustomStates;
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
   if (FCallStack.Count shr 1)>FStack.MaxRecursionDepth then
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

// CallStackLastProg
//
function TdwsProgramExecution.CallStackLastProg : TObject;
var
   n : Integer;
begin
   n:=FCallStack.Count-1;
   if n>=0 then
      Result:=TObject(FCallStack.List[n])
   else Result:=nil;
end;

// DebuggerFieldAddr
//
function TdwsProgramExecution.DebuggerFieldAddr : Integer;
var
   field : TFieldSymbol;
begin
   if FDebuggerFieldAddr=0 then begin
      field:=TFieldSymbol(Prog.TypException.Members.FindSymbol(SYS_EXCEPTION_DEBUGGER_FIELD, cvMagic, TFieldSymbol));
      Assert(field<>nil);
      FDebuggerFieldAddr:=field.Offset;
   end;
   Result:=FDebuggerFieldAddr;
end;

// DebuggerNotifyException
//
procedure TdwsProgramExecution.DebuggerNotifyException(const exceptObj : IScriptObj);
var
   addr, i : Integer;
begin
   if not IsDebugging then Exit;
   addr:=DebuggerFieldAddr;
   i:=exceptObj.AsInteger[addr];
   exceptObj.AsInteger[addr]:=i+1;
   if i=0 then
      Debugger.NotifyException(Self, exceptObj);
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

   FInitExpr := TBlockInitExpr.Create(cNullPos);

   // Initialize shortcuts to often used symbols
   sysTable:=systemTable.SymbolTable;
   FBaseTypes.FTypBoolean := sysTable.TypBoolean;
   FBaseTypes.FTypFloat := sysTable.TypFloat;
   FBaseTypes.FTypInteger := sysTable.TypInteger;
   FBaseTypes.FTypString := sysTable.TypString;
   FBaseTypes.FTypVariant := sysTable.TypVariant;
   FBaseTypes.FTypNil := TNilSymbol.Create;
   FBaseTypes.FTypObject := sysTable.TypObject;
   FBaseTypes.FTypTObject := sysTable.TypTObject;
   FBaseTypes.FTypException := sysTable.TypException;
   FBaseTypes.FTypInterface := sysTable.TypInterface;
   FBaseTypes.FTypAnyType := sysTable.TypAnyType;
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

// SubExpr
//
function TdwsProgram.SubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=InitExpr
   else Result:=Expr;
end;

// SubExprCount
//
function TdwsProgram.SubExprCount : Integer;
begin
   Result:=2;
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
   FInitExpr:=TBlockInitExpr.Create(cNullPos);
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
                                   const stackParameters : TStackParameters;
                                   const mainFileName : String);
var
   systemUnitTable : TLinkedSymbolTable;
   systemUnit : TUnitMainSymbol;
begin
   inherited Create(systemTable);

   FResultType:=ResultType;

   FMainFileName:=mainFileName;

   FExecutionsLock:=TFixedCriticalSection.Create;

   FStackParameters:=stackParameters;
   FStackParameters.MaxLevel:=1;

   FGlobalAddrGenerator:=TAddrGeneratorRec.CreatePositive(0);

   FSourceContextMap:=TdwsSourceContextMap.Create;

   FSymbolDictionary:=TdwsSymbolDictionary.Create;

   FAttributes:=TdwsSymbolAttributes.Create;

   FConditionalDefines:=TAutoStrings.Create;

   FSourceList:=TScriptSourceList.Create;

   FRoot:=Self;

   FUnifiedConstList:=TUnifiedConstList.Create;
   TUnifiedConstList(FUnifiedConstList).Precharge(Self, systemTable.SymbolTable);

   FResourceStringList:=TResourceStringSymbolList.Create;

   FUnitMains:=TUnitMainSymbols.Create;

   FSystemTable:=systemTable;
   systemUnitTable:=TLinkedSymbolTable.Create(systemTable.SymbolTable);
   systemUnit:=TUnitMainSymbol.Create(SYS_SYSTEM, systemUnitTable, FUnitMains);
   systemUnit.ReferenceInSymbolTable(FRootTable, True);

   FTypDefaultConstructor:=TypTObject.Members.FindSymbol(SYS_TOBJECT_CREATE, cvPublic) as TMethodSymbol;
   FTypDefaultDestructor:=TypTObject.Members.FindSymbol(SYS_TOBJECT_DESTROY, cvPublic) as TMethodSymbol;

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

   FOrphanedObjects.Free;
   FFinalExpr.Free;
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
   if FExecutionsClass=nil then
      exec:=TdwsProgramExecution.Create(Self, FStackParameters)
   else exec:=FExecutionsClass.Create(Self, FStackParameters);
   exec.UserObject:=DefaultUserObject;
   exec.Environment:=DefaultEnvironment;
   exec.Localizer:=DefaultLocalizer;
   exec.OnExecutionStarted:=OnExecutionStarted;
   exec.OnExecutionEnded:=OnExecutionEnded;
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

// SetExecutionsClass
//
procedure TdwsMainProgram.SetExecutionsClass(aClass : TdwsProgramExecutionClass);
begin
   FExecutionsClass:=aClass;
end;

// GetSourceFile
//
function TdwsMainProgram.GetSourceFile(const aSourceFile : UnicodeString) : TSourceFile;
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

// CollectAllPublishedSymbols
//
function TdwsMainProgram.CollectAllPublishedSymbols(ignoreImplementationPublished : Boolean) : TSimpleSymbolList;
begin
   Result:=TSimpleSymbolList.Create;
   UnitMains.CollectPublishedSymbols(Result, ignoreImplementationPublished);
   FRootTable.CollectPublishedSymbols(Result);
end;

// OrphanObject
//
procedure TdwsMainProgram.OrphanObject(obj : TRefCountedObject);
begin
   if FOrphanedObjects=nil then
      FOrphanedObjects:=TObjectList<TRefCountedObject>.Create;
   FOrphanedObjects.Add(obj);
end;

// AddFinalExpr
//
procedure TdwsMainProgram.AddFinalExpr(expr : TProgramExpr);
begin
   if FFinalExpr=nil then
      FFinalExpr:=TBlockFinalExpr.Create(cNullPos);
   FFinalExpr.AddStatement(expr);
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
   FCompileMsgs:=Parent.CompileMsgs;
   FUnitMains:=Parent.UnitMains;

   FInitExpr := TBlockInitExpr.Create(cNullPos);

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

      if FInitExpr.StatementCount>0 then begin
         exec.DoStep(FInitExpr);
         FInitExpr.EvalNoResult(exec);
      end;

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

// CompileTimeCheck
//
procedure TdwsProcedure.CompileTimeCheck(prog : TdwsProgram; expr : TFuncExprBase);
begin
   // nothing yet
end;

procedure TdwsProcedure.InitSymbol(Symbol: TSymbol; const msgs : TdwsCompileMessageList);
begin
   FTable.Initialize(msgs);
end;

procedure TdwsProcedure.InitExpression(Expr: TExprBase);
begin
end;

// OptimizeConstAssignments
//
procedure TdwsProcedure.OptimizeConstAssignments(blockExpr : TBlockExprBase);
{$ifdef COALESCE_VAR_INITIALIZATION}
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
   // if its expression doesn't use the variable itself
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
               if not assignExpr.Right.ReferencesVariable(assignExprSym) then
                  InitExpr.ReplaceStatement(j, blockExpr.ExtractStatement(i));
            end;
         end;
      end;
   end;
{$else}
begin
{$endif}
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

// ToDataString
//
function TdwsResult.ToDataString : RawByteString;
begin
   Result:=ScriptStringToRawByteString(ToString);
end;

// ToUTF8String
//
function TdwsResult.ToUTF8String : UTF8String;
begin
   Result:=UTF8Encode(ToString);
end;

// AddCRLF
//
procedure TdwsResult.AddCRLF;
begin
   AddString(#13#10);
end;

// AddString
//
procedure TdwsResult.AddString(const i : Int64);
begin
   AddString(IntToStr(i));
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
   RegisterStandardResultFunctions(SymbolTable);
end;

// ------------------
// ------------------ TdwsDefaultResult ------------------
// ------------------

// Create
//
constructor TdwsDefaultResult.Create(resultType: TdwsResultType);
begin
   inherited;
   FTextBuilder:=TWriteOnlyBlockStream.AllocFromPool;
end;

// Destroy
//
destructor TdwsDefaultResult.Destroy;
begin
   inherited;
   FTextBuilder.ReturnToPool;
end;

// AddString
//
procedure TdwsDefaultResult.AddString(const str : UnicodeString);
begin
   FTextBuilder.WriteString(str);
end;

// AddString
//
procedure TdwsDefaultResult.AddString(const i : Int64);
begin
   FTextBuilder.WriteString(i);
end;

// AddCRLF
//
procedure TdwsDefaultResult.AddCRLF;
begin
   FTextBuilder.WriteCRLF;
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

// ToDataString
//
function TdwsDefaultResult.ToDataString : RawByteString;
begin
   Result:=ScriptStringToRawByteString(FTextBuilder.ToString);
end;

// GetText
//
function TdwsDefaultResult.GetText : UnicodeString;
begin
   Result:=FTextBuilder.ToString;
end;

// ------------------
// ------------------ TdwsGuardedExecution ------------------
// ------------------

// FreeAll
//
procedure TdwsGuardedExecution.FreeAll;
begin
   if Assigned(Self) then begin
      Next.FreeAll;
      Destroy;
   end;
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
   FreeOnTerminate:=False;

   inherited Create(True);

   Priority:=tpTimeCritical;
end;

// Destroy
//
destructor TdwsGuardianThread.Destroy;
begin
   FExecutions.FreeAll;
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
var
   guardian : TdwsGuardianThread;
begin
   if vThread<>nil then begin
      guardian:=vThread;
      vThread:=nil;
      guardian.Terminate;
      guardian.FEvent.SetEvent;
      guardian.WaitFor;
      guardian.Destroy;
   end;
end;

// GuardExecution
//
class procedure TdwsGuardianThread.GuardExecution(const exec : IdwsProgramExecution; aMilliSecToLive : Integer);
var
   thread : TdwsGuardianThread;
   item, prev, iter : TdwsGuardedExecution;
   timeOutAt : Int64;
begin
   thread:=vThread;
   timeOutAt:=GetSystemMilliseconds+aMilliSecToLive;
   thread.FExecutionsLock.Enter;
   try
      if vExecutionsPool<>nil then begin
         item:=vExecutionsPool;
         vExecutionsPool:=item.Next;
         item.Next:=nil;
      end else item:=TdwsGuardedExecution.Create;
      item.Exec:=exec;
      item.TimeOutAt:=timeOutAt;

      iter:=thread.FExecutions;
      if iter=nil then
         thread.FExecutions:=item
      else if iter.TimeOutAt>=item.TimeOutAt then begin
         item.Next:=thread.FExecutions;
         thread.FExecutions:=item
      end else begin
         repeat
            prev:=iter;
            iter:=iter.Next;
         until (iter=nil) or (iter.TimeOutAt>=item.TimeOutAt);
         item.Next:=iter;
         prev.Next:=item;
      end;
   finally
      thread.FExecutionsLock.Leave;
   end;
   if aMilliSecToLive<PulseIntervalMilliSeconds then
      thread.FEvent.SetEvent;
end;

// ForgetExecution
//
class procedure TdwsGuardianThread.ForgetExecution(const exec : IdwsProgramExecution);
var
   thread : TdwsGuardianThread;
   iter, prev : TdwsGuardedExecution;
begin
   thread:=vThread;
   if thread=nil then Exit;
   thread.FExecutionsLock.Enter;
   try
      iter:=thread.FExecutions;
      if iter<>nil then begin
         if iter.Exec=exec then
            thread.FExecutions:=iter.Next
         else begin
            repeat
               prev:=iter;
               iter:=iter.Next;
            until (iter=nil) or (iter.Exec=exec);
            if iter<>nil then
               prev.Next:=iter.Next;
         end;
         if iter<>nil then begin
            iter.Exec:=nil;
            iter.Next:=vExecutionsPool;
            vExecutionsPool:=iter;
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
   currentTime : Int64;
   item : TdwsGuardedExecution;
   millisecs : Cardinal;
   timeLeft : Int64;
begin
   SetThreadName('DWScript Guardian');

   while not Terminated do begin

      timeLeft:=INFINITE;

      if FExecutions<>nil then begin

         currentTime:=GetSystemMilliseconds;

         FExecutionsLock.Enter;
         try
            item:=FExecutions;
            while (item<>nil) and (item.TimeOutAt<=currentTime) do begin
               item.Exec.Stop;
               FExecutions:=item.Next;
               item.Exec:=nil;
               item.Next:=vExecutionsPool;
               vExecutionsPool:=item;
               item:=FExecutions;
            end;
            item:=FExecutions;
            if item<>nil then
               timeLeft:=item.TimeOutAt-currentTime;
         finally
            FExecutionsLock.Leave;
         end;

      end;

      Assert(timeLeft>0);
      if timeLeft>PulseIntervalMilliSeconds then
         millisecs:=PulseIntervalMilliSeconds
      else millisecs:=timeLeft;
      FEvent.WaitFor(millisecs);

   end;
end;

// ------------------
// ------------------ TProgramExpr ------------------
// ------------------

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
   Result:=(IUnknown(Eval(exec)) as IScriptObj);
end;

// EvalAsScriptObjInterface
//
procedure TProgramExpr.EvalAsScriptObjInterface(exec : TdwsExecution; var result : IScriptObjInterface);
begin
   Result:=(IUnknown(Eval(exec)) as IScriptObjInterface);
end;

// EvalAsScriptDynArray
//
procedure TProgramExpr.EvalAsScriptDynArray(exec : TdwsExecution; var Result : IScriptDynArray);
begin
   Result:=(IUnknown(Eval(exec)) as IScriptDynArray);
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
procedure TProgramExpr.AssignValueAsString(exec : TdwsExecution; const value: UnicodeString);
begin
   AssignValue(exec, value);
end;

// AssignValueAsScriptObj
//
procedure TProgramExpr.AssignValueAsScriptObj(exec : TdwsExecution; const value : IScriptObj);
begin
   AssignValue(exec, value);
end;

// AssignValueAsScriptDynArray
//
procedure TProgramExpr.AssignValueAsScriptDynArray(exec : TdwsExecution; const value : IScriptDynArray);
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
   VariantToInt64(v, Result);
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
procedure TProgramExpr.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
var
   v : Variant;
   p : PVarData;
begin
   v:=Eval(exec);
   try
      p:=PVarData(@v);
      {$ifdef FPC}
      if p^.VType=varString then
         Result:=UnicodeString(p.VString)
      {$else}
      if p^.VType=varUString then
         Result:=UnicodeString(p.VUString)
      {$endif}
      else VariantToString(v, Result);
   except
      // standardize RTL message
      on E : EVariantError do begin
         raise EdwsVariantTypeCastError.Create(v, SYS_STRING, E);
      end else raise;
   end;
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

// ScriptLocation
//
function TProgramExpr.ScriptLocation(prog : TObject) : UnicodeString;
begin
   if prog is TdwsProcedure then
      Result:=TdwsProcedure(prog).Func.QualifiedName+ScriptPos.AsInfo
   else Result:=ScriptPos.AsInfo;
end;

// InterruptsFlow
//
function TProgramExpr.InterruptsFlow : Boolean;
begin
   Result:=False;
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
      if optimized<>Self then begin
         Assert(optimized is TTypedExpr);
         Result:=TTypedExpr(optimized);
      end else Result:=Self;
   except
      on E: Exception do begin
         prog.CompileMsgs.AddCompilerException(hotPos, E);
         Result:=Self;
      end;
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

// CheckInterface
//
procedure TTypedExpr.CheckInterface(exec : TdwsExecution; const intf : IScriptObjInterface);
begin
   if intf=nil then
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

// SameDataExpr
//
function TTypedExpr.SameDataExpr(expr : TTypedExpr) : Boolean;
begin
   Result:=False;
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
constructor TTypeReferenceExpr.Create(aTyp : TTypeSymbol; const aScriptPos : TScriptPos);
begin
   inherited Create;
   Typ:=aTyp;
   FScriptPos:=aScriptPos;
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
   Result:=FScriptPos;
end;

// ------------------
// ------------------ TPosDataExpr ------------------
// ------------------

// Create
//
constructor TPosDataExpr.Create(const scriptPos : TScriptPos; aTyp: TTypeSymbol);
begin
   inherited Create(aTyp);
   FScriptPos:=scriptPos;
end;

// ScriptPos
//
function TPosDataExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// ------------------
// ------------------ TNoResultExpr ------------------
// ------------------

// Create
//
constructor TNoResultExpr.Create(const aPos: TScriptPos);
begin
   FScriptPos:=aPos;
end;

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

// ScriptPos
//
function TNoResultExpr.ScriptPos : TScriptPos;
begin
   Result:=FScriptPos;
end;

// SetScriptPos
//
procedure TNoResultExpr.SetScriptPos(const aPos : TScriptPos);
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
procedure TBlockExprBase.AddStatement(expr : TProgramExpr);
begin
   ReallocMem(FStatements, (FCount+1)*SizeOf(TNoResultExpr));
   FStatements[FCount]:=expr;
   Inc(FCount);
end;

// ReplaceStatement
//
procedure TBlockExprBase.ReplaceStatement(index : Integer; expr : TProgramExpr);
begin
   FStatements[index].Free;
   FStatements[index]:=expr;
end;

// ExtractStatement
//
function TBlockExprBase.ExtractStatement(index : Integer) : TProgramExpr;
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
   expr : PProgramExpr;
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

constructor TDataExpr.Create(aTyp: TTypeSymbol);
begin
   FTyp := aTyp;
end;

// Eval
//
function TDataExpr.Eval(exec : TdwsExecution) : Variant;
begin
   DataPtr[exec].EvalAsVariant(0, Result);
end;

// IsWritable
//
function TDataExpr.IsWritable: Boolean;
begin
   Result:=True;
end;

// GetDataPtrFunc
//
function TDataExpr.GetDataPtrFunc(exec : TdwsExecution) : IDataContext;
begin
   GetDataPtr(exec, Result);
end;

// AssignData
//
procedure TDataExpr.AssignData(exec : TdwsExecution; const source : IDataContext);
begin
  Assert(IsWritable);
  if Typ.Size>0 then
     DataPtr[exec].WriteData(source, Typ.Size);
end;

// AssignValue
//
procedure TDataExpr.AssignValue(exec : TdwsExecution; const Value: Variant);
begin
  Assert(IsWritable);
  DataPtr[exec][0]:=Value;
end;

// AssignExpr
//
procedure TDataExpr.AssignExpr(exec : TdwsExecution; Expr: TTypedExpr);
begin
   Expr.EvalAsVariant(exec, DataPtr[exec].AsPVariant(0)^);
end;

procedure TDataExpr.AssignDataExpr(exec : TdwsExecution; DataExpr: TDataExpr);
begin
   DataPtr[exec].WriteData(DataExpr.DataPtr[exec], Typ.Size);
end;

// ------------------
// ------------------ TFuncExprBase ------------------
// ------------------

// Create
//
constructor TFuncExprBase.Create(const aScriptPos: TScriptPos; aFunc : TFuncSymbol);
begin
   inherited Create(aScriptPos, nil);
   FFunc:=aFunc;
   if Assigned(aFunc) then begin
      FTyp:=aFunc.Typ;
      FParamSize:=aFunc.ParamSize;
   end;
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
            Prog.CompileMsgs.AddCompilerErrorFmt(ScriptPos, CPE_FunctionOptimizationFailed,
                                                 [FuncSym.Name, E.ClassName, E.Message],
                                                  TCompilerErrorMessage);
         end;
      end;
      if Result<>Self then
         Free;
   end;
end;

// CompileTimeCheck
//
procedure TFuncExprBase.CompileTimeCheck(prog : TdwsProgram);
begin
   // nothing here
end;

// GetIsConstant
//
function TFuncExprBase.GetIsConstant : Boolean;
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
      FParamSize:=newFuncSym.ParamSize;
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

// GetDataPtr
//
procedure TFuncExprBase.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   Eval(exec);
   exec.DataContext_CreateBase(FResultAddr, result);
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
var
   expr : TTypedExpr;
begin
   if Cardinal(idx)<Cardinal(FArgs.Count) then begin
      expr:=TTypedExpr(FArgs[idx]);
      if expr<>nil then
         Result:=expr.Typ
      else Result:=nil;
   end else Result:=nil;
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
var
   typ : TTypeSymbol;
begin
   typ:=argExpr.Typ;
   if (typ<>nil) and (typ.Size>1) then
      FTypeParamSym:=TSymbol(potTempData)
   else FTypeParamSym:=TSymbol(potTempAddr);
   FStackAddr:=stackAddr;
   FArgExpr:=argExpr;
end;

// InitPushTempArrayAddr
//
procedure TPushOperator.InitPushTempArrayAddr(stackAddr: Integer; argExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potTempArrayAddr);
   FStackAddr:=stackAddr;
   FArgExpr:=argExpr;
end;

// InitPushTempArray
//
procedure TPushOperator.InitPushTempArray(stackAddr: Integer; argExpr: TTypedExpr);
begin
   FTypeParamSym:=TSymbol(potTempArray);
   FStackAddr:=stackAddr;
   if argExpr is TConstParamExpr then
      FArgExpr:=argExpr
   else FArgExpr:=nil;  // error caught earlier, if not, ensure runtime crash
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
   if argExpr is TConstExpr then
      FTypeParamSym:=TSymbol(potConstData)
   else FTypeParamSym:=ParamSym;
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
      NativeInt(potTempData) : ExecuteTempData(exec);
      NativeInt(potTempArrayAddr) : ExecuteTempArrayAddr(exec);
      NativeInt(potTempArray) : ExecuteTempArray(exec);
      NativeInt(potResultBoolean) : ExecuteResultBoolean(exec);
      NativeInt(potResultInteger) : ExecuteResultInteger(exec);
      NativeInt(potResultFloat) : ExecuteResultFloat(exec);
      NativeInt(potResultString) : ExecuteResultString(exec);
      NativeInt(potResultConstString) : ExecuteResultConstString(exec);
      NativeInt(potResult) : ExecuteResult(exec);
      NativeInt(potConstData) : ExecuteConstData(exec);
      NativeInt(potInitResult) : ExecuteInitResult(exec);
      NativeInt(potLazy) : ExecuteLazy(exec);
   else
      ExecuteData(exec);
   end;
end;

// ExecuteAddr
//
procedure TPushOperator.ExecuteAddr(exec : TdwsExecution);
begin
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FStackAddr, TDataExpr(FArgExpr).DataPtr[exec]);
end;

// ExecutePassAddr
//
procedure TPushOperator.ExecutePassAddr(exec : TdwsExecution);
var
   vpd : Pointer;
begin
   vpd:=TVarParamExpr(FArgExpr).GetVarParamDataAsPointer(exec);
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FStackAddr, IUnknown(vpd));
end;

// ExecuteTempAddr
//
procedure TPushOperator.ExecuteTempAddr(exec : TdwsExecution);
var
   vpd : IDataContext;
   data : TData;
begin
   SetLength(data, 1);
   FArgExpr.EvalAsVariant(exec, data[0]);
   exec.DataContext_Create(data, 0, vpd);
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteTempData
//
procedure TPushOperator.ExecuteTempData(exec : TdwsExecution);
var
   vpd : IDataContext;
   dataExpr : TDataExpr;
   data : TData;
begin
   SetLength(data, FArgExpr.Typ.Size);

   dataExpr:=TDataExpr(FArgExpr);
   exec.DataContext_Create(data, 0, vpd);

   vpd.WriteData(dataExpr.DataPtr[exec], FArgExpr.Typ.Size);

   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteTempArrayAddr
//
procedure TPushOperator.ExecuteTempArrayAddr(exec : TdwsExecution);
var
   ace : TArrayConstantExpr;
   vpd : IDataContext;
   data : TData;
begin
   ace:=TArrayConstantExpr(FArgExpr);
   SetLength(data, ace.Size);

   exec.DataContext_Create(data, 0, vpd);

   ace.EvalAsTData(exec, vpd.AsPData^);

   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FStackAddr, vpd);
end;

// ExecuteTempArray
//
procedure TPushOperator.ExecuteTempArray(exec : TdwsExecution);
begin
   exec.Stack.WriteInterfaceValue(exec.Stack.StackPointer+FStackAddr, TConstParamExpr(FArgExpr).DataPtr[exec]);
end;

// ExecuteResult
//
procedure TPushOperator.ExecuteResult(exec : TdwsExecution);
begin
   FArgExpr.EvalAsVariant(exec, exec.Stack.Data[exec.Stack.StackPointer+FStackAddr]);
end;

// ExecuteResultBoolean
//
procedure TPushOperator.ExecuteResultBoolean(exec : TdwsExecution);
begin
   exec.Stack.WriteBoolValue(exec.Stack.StackPointer+FStackAddr,
                             FArgExpr.EvalAsBoolean(exec));
end;

// ExecuteResultInteger
//
procedure TPushOperator.ExecuteResultInteger(exec : TdwsExecution);
begin
   exec.Stack.WriteIntValue(exec.Stack.StackPointer+FStackAddr,
                            FArgExpr.EvalAsInteger(exec));
end;

// ExecuteResultFloat
//
procedure TPushOperator.ExecuteResultFloat(exec : TdwsExecution);
begin
   exec.Stack.WriteFloatValue(exec.Stack.StackPointer+FStackAddr,
                              FArgExpr.EvalAsFloat(exec));
end;

// ExecuteResultString
//
procedure TPushOperator.ExecuteResultString(exec : TdwsExecution);
var
   buf : UnicodeString;
begin
   FArgExpr.EvalAsString(exec, buf);
   exec.Stack.WriteStrValue(exec.Stack.StackPointer+FStackAddr,
                            buf);
end;

// ExecuteResultConstString
//
procedure TPushOperator.ExecuteResultConstString(exec : TdwsExecution);
begin
   exec.Stack.WriteStrValue(exec.Stack.StackPointer+FStackAddr,
                            TConstStringExpr(FArgExpr).Value);
end;

// ExecuteData
//
procedure TPushOperator.ExecuteData(exec : TdwsExecution);
var
   dataExpr : TDataExpr;
begin
   dataExpr:=TDataExpr(FArgExpr);
   dataExpr.DataPtr[exec].CopyData(exec.Stack.Data, exec.Stack.StackPointer+FStackAddr,
                                   FTypeParamSym.Typ.Size);
end;

// ExecuteConstData
//
procedure TPushOperator.ExecuteConstData(exec : TdwsExecution);
var
   constExpr : TConstExpr;
begin
   constExpr:=TConstExpr(FArgExpr);
   exec.Stack.WriteData(0, exec.Stack.StackPointer+FStackAddr, Length(constExpr.Data), constExpr.Data);
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
constructor TFuncExpr.Create(aProg : TdwsProgram; const scriptPos : TScriptPos; func : TFuncSymbol);
begin
   inherited Create(scriptPos, Func);
   FCallerID:=Self;
   FLevel:=aProg.Level;
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
   if FArgs.Count<FFunc.Params.Count then
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

// DoEvalCall
//
procedure TFuncExpr.DoEvalCall(exec : TdwsExecution; func : TFuncSymbol);
var
   oldBasePointer : Integer;
begin
   EvalPushExprs(exec);

   oldBasePointer:=exec.Stack.SwitchFrame(Level);
   TdwsProgramExecution(exec).EnterRecursion(CallerID);
   try
      ICallable(func.Executable).Call(TdwsProgramExecution(exec), func);
   finally
      TdwsProgramExecution(exec).LeaveRecursion;
      exec.Stack.RestoreFrame(Level, oldBasePointer);
   end;
end;

// Eval
//
function TFuncExpr.Eval(exec : TdwsExecution) : Variant;
begin
   try
      // Allocate memory for parameters on the stack
      exec.Stack.Push(ParamSize);
      try
         DoEvalCall(exec, FuncSym);

         if Typ<>nil then
            StaticPostCall(exec, Result);
      finally
         // Remove parameters from stack
         exec.Stack.Pop(ParamSize);
      end;
   except
      exec.SetScriptError(Self);
      raise;
   end;
end;

// StaticPostCall
//
procedure TFuncExpr.StaticPostCall(exec : TdwsExecution; var Result : Variant);
var
   sourceAddr, destAddr: Integer;
begin
   // Result.StackAddr is relative to BasePointer of the called function
   // But the frame is already restored so its relative to the stackpointer here
   sourceAddr:=exec.Stack.StackPointer+FuncSym.Result.StackAddr;
   // Copy return value
   exec.Stack.ReadValue(sourceAddr, Result);

   if ResultAddr>=0 then begin
      destAddr:=exec.Stack.BasePointer+ResultAddr;
      exec.Stack.CopyData(sourceAddr, destAddr, FFunc.Typ.Size);
   end;
end;

// StaticPostCallInteger
//
procedure TFuncExpr.StaticPostCallInteger(exec : TdwsExecution; var Result : Int64);
var
   sourceAddr, destAddr: Integer;
begin
   sourceAddr:=exec.Stack.StackPointer+FuncSym.Result.StackAddr;
   Result:=exec.Stack.ReadIntValue(sourceAddr);

   if ResultAddr>=0 then begin
      destAddr:=exec.Stack.BasePointer+ResultAddr;
      exec.Stack.CopyData(sourceAddr, destAddr, FFunc.Typ.Size);
   end;
end;

// StaticPostCallFloat
//
procedure TFuncExpr.StaticPostCallFloat(exec : TdwsExecution; var Result : Double);
var
   sourceAddr, destAddr: Integer;
begin
   sourceAddr:=exec.Stack.StackPointer+FuncSym.Result.StackAddr;
   Result:=exec.Stack.ReadFloatValue(sourceAddr);

   if ResultAddr>=0 then begin
      destAddr:=exec.Stack.BasePointer+ResultAddr;
      exec.Stack.CopyData(sourceAddr, destAddr, FFunc.Typ.Size);
   end;
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
      if param.ClassType=TLazyParamSymbol then
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

// CompileTimeCheck
//
procedure TFuncExpr.CompileTimeCheck(prog : TdwsProgram);
begin
   if FuncSym.Executable<>nil then
      ICallable(FuncSym.Executable).CompileTimeCheck(prog, Self);
end;

// FuncSymQualifiedName
//
function TFuncExpr.FuncSymQualifiedName : UnicodeString;
begin
   Result:=FuncSym.QualifiedName;
end;

// ------------------
// ------------------ TFuncSimpleExpr ------------------
// ------------------

// EvalNoResult
//
procedure TFuncSimpleExpr.EvalNoResult(exec : TdwsExecution);
begin
   try
      exec.Stack.Push(ParamSize);
      try
         DoEvalCall(exec, FuncSym);
      finally
         exec.Stack.Pop(ParamSize);
      end;
   except
      exec.SetScriptError(Self);
      raise;
   end;
end;

// EvalAsInteger
//
function TFuncSimpleExpr.EvalAsInteger(exec : TdwsExecution) : Int64;
begin
   try
      exec.Stack.Push(ParamSize);
      try
         DoEvalCall(exec, FuncSym);
         StaticPostCallInteger(exec, Result);
      finally
         exec.Stack.Pop(ParamSize);
      end;
   except
      exec.SetScriptError(Self);
      raise;
   end;
end;

// EvalAsFloat
//
function TFuncSimpleExpr.EvalAsFloat(exec : TdwsExecution) : Double;
begin
   try
      exec.Stack.Push(ParamSize);
      try
         DoEvalCall(exec, FuncSym);
         StaticPostCallFloat(exec, Result);
      finally
         exec.Stack.Pop(ParamSize);
      end;
   except
      exec.SetScriptError(Self);
      raise;
   end;
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
   scriptObjIntf : IScriptObjInterface;
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
         if baseTyp is TInterfaceSymbol then begin
            baseExpr.EvalAsScriptObjInterface(exec, scriptObjIntf);
            FFuncExpr:=CreateIntfExpr(prog, funcExpr.FuncSym, scriptObjIntf);
         end else begin
            baseExpr.EvalAsScriptObj(exec, scriptObj);
            FFuncExpr:=CreateFuncExpr(prog, funcExpr.FuncSym, scriptObj, scriptObj.ClassSym);
         end;
      end;

   end else if funcExpr is TMagicFuncExpr then begin

      magicFuncSym:=funcExpr.FuncSym as TMagicFuncSymbol;
      FFuncExpr:=TMagicFuncExpr.CreateMagicFuncExpr(prog, cNullPos, magicFuncSym);

   end else begin

      FFuncExpr:=CreateFuncExpr(prog, funcExpr.FuncSym, nil, nil);

   end;

   if FFuncExpr is TMagicFuncExpr then
      FDoEvalAsVariant:=EvalMagicAsVariant
   else begin
      Assert(FFuncExpr is TFuncExpr);
      FDoEvalAsVariant:=EvalFuncAsVariant;
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

// EvalMagicAsVariant
//
procedure TFuncPointer.EvalMagicAsVariant(exec : TdwsExecution; caller : TFuncExpr; var result : Variant);
var
   oldArgs : TExprBaseListRec;
begin
   oldArgs:=FFuncExpr.Args;
   FFuncExpr.Args:=caller.Args;
   try
      FFuncExpr.EvalAsVariant(exec, Result);
   finally
      FFuncExpr.Args:=oldArgs;
   end;
end;

// EvalFuncAsVariant
//
procedure TFuncPointer.EvalFuncAsVariant(exec : TdwsExecution; caller : TFuncExpr; var result : Variant);
var
   funcExpr : TFuncExpr;
   i : Integer;
begin
   funcExpr:=TFuncExpr(FFuncExpr);

   funcExpr.ClearArgs;
   for i:=0 to caller.Args.Count-1 do
      funcExpr.AddArg(caller.Args.ExprBase[i] as TTypedExpr);
   funcExpr.AddPushExprs((exec as TdwsProgramExecution).Prog);
   funcExpr.CallerID:=caller;

   try
      funcExpr.EvalAsVariant(exec, Result);
   finally
      for i:=0 to caller.Args.Count-1 do
         funcExpr.Args.ExprBase[i]:=nil;
   end;
end;

// EvalAsVariant
//
procedure TFuncPointer.EvalAsVariant(exec : TdwsExecution; caller : TFuncExpr;
                                     var result : Variant);
begin
   FDoEvalAsVariant(exec, caller, result);
end;

// EvalAsInteger
//
function TFuncPointer.EvalAsInteger(exec : TdwsExecution; caller : TFuncExpr) : Int64;
var
   v : TVarData;
begin
   v.VType:=varInt64;
   FDoEvalAsVariant(exec, caller, Variant(v));
   if v.VType=varInt64 then
      Result:=v.VInt64
   else begin
      Result:=Variant(v);
      VarClear(Variant(v));
   end;
end;

// EvalDataPtr
//
function TFuncPointer.EvalDataPtr(exec : TdwsExecution; caller : TFuncExpr) : IDataContext;
var
   funcExpr : TFuncExpr;
   i : Integer;
begin
   funcExpr:=TFuncExpr(FFuncExpr);

   funcExpr.ClearArgs;
   for i:=0 to caller.Args.Count-1 do
      funcExpr.AddArg(caller.Args.ExprBase[i] as TTypedExpr);
   funcExpr.AddPushExprs((exec as TdwsProgramExecution).Prog);
   funcExpr.CallerID:=caller;

   try
      Result:=funcExpr.DataPtr[exec];
   finally
      for i:=0 to caller.Args.Count-1 do
         funcExpr.Args.ExprBase[i]:=nil;
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
begin
   EvalAsVariant(exec, Result);
end;

// EvalAsVariant
//
procedure TAnonymousFuncRefExpr.EvalAsVariant(exec : TdwsExecution; var Result : Variant);
var
   funcPtr : TFuncPointer;
begin
   if FFuncExpr.ClassType=TFuncPtrExpr then
      TFuncPtrExpr(FFuncExpr).CodeExpr.EvalAsVariant(exec, Result)
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

// GetDataPtr
//
procedure TAnonymousFuncRefExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
var
   data : TData;
begin
   SetLength(data, 1);
   EvalAsVariant(exec, data[0]);
   exec.DataContext_Create(data, 0, result);
end;

// ------------------
// ------------------ TFuncPtrExpr ------------------
// ------------------

// Create
//
constructor TFuncPtrExpr.Create(prog : TdwsProgram; const aScriptPos : TScriptPos; codeExpr : TTypedExpr);
begin
   inherited Create(prog, aScriptPos, (codeExpr.Typ.UnAliasedType as TFuncSymbol));
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
function TFuncPtrExpr.Extract : TTypedExpr;
begin
   Result:=FCodeExpr;
   FCodeExpr:=nil;
   Free;
end;

// EvalAsFuncPointer
//
procedure TFuncPtrExpr.EvalAsFuncPointer(exec : TdwsExecution; var result : IFuncPointer);
var
   val : Variant;
begin
   FCodeExpr.EvalAsVariant(exec, val);
   result:=IFuncPointer(IUnknown(val));
end;

// Eval
//
function TFuncPtrExpr.Eval(exec : TdwsExecution) : Variant;
var
   funcPointer : IFuncPointer;
begin
   EvalAsFuncPointer(exec, funcPointer);
   if funcPointer=nil then
      RaiseScriptError(exec, EScriptError, RTE_FuncPointerIsNil);
   funcPointer.EvalAsVariant(exec, Self, Result);
end;

// EvalNoResult
//
procedure TFuncPtrExpr.EvalNoResult(exec : TdwsExecution);
begin
   Eval(exec);
end;

// GetIsConstant
//
function TFuncPtrExpr.GetIsConstant : Boolean;
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

constructor TBinaryOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
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
   EvalAsVariant(exec, Result);
end;

// EvalAsVariant
//
procedure TBinaryOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
begin
   Assert(False);
end;

// GetIsConstant
//
function TBinaryOpExpr.GetIsConstant : Boolean;
begin
   Result:=FLeft.IsConstant and FRight.IsConstant;
end;

// OptimizeConstantOperandsToFloats
//
procedure TBinaryOpExpr.OptimizeConstantOperandsToFloats(prog : TdwsProgram; exec : TdwsExecution);
begin
   FLeft:=FLeft.OptimizeToFloatConstant(prog, exec);
   FRight:=FRight.OptimizeToFloatConstant(prog, exec);
end;

// Swap
//
procedure TBinaryOpExpr.Swap;
var
   t : TTypedExpr;
begin
   t:=FLeft;
   FLeft:=FRight;
   FRight:=t;
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
constructor TVariantBinOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypVariant;
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
constructor TIntegerBinOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   if aLeft.Typ=aRight.Typ then
      FTyp:=aLeft.Typ
   else FTyp:=Prog.TypInteger;
end;

// EvalAsVariant
//
procedure TIntegerBinOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
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
      Result:=TConstIntExpr.CreateUnified(Prog, Typ, EvalAsInteger(exec));
      Free;
   end else Result:=Self;
end;

// ------------------
// ------------------ TStringBinOpExpr ------------------
// ------------------

// Create
//
constructor TStringBinOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypString;
end;

// EvalAsVariant
//
procedure TStringBinOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
var
   buf : UnicodeString;
begin
   EvalAsString(exec, buf);
   Result:=buf;
end;

// Optimize
//
function TStringBinOpExpr.Optimize(prog : TdwsProgram; exec : TdwsExecution) : TProgramExpr;
var
   buf : UnicodeString;
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
constructor TFloatBinOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypFloat;
end;

// EvalAsVariant
//
procedure TFloatBinOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
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
      OptimizeConstantOperandsToFloats(prog, exec);
      Result:=Self;
   end;
end;

// ------------------
// ------------------ TBooleanBinOpExpr ------------------
// ------------------

// Create
//
constructor TBooleanBinOpExpr.Create(Prog: TdwsProgram; const aScriptPos : TScriptPos; aLeft, aRight : TTypedExpr);
begin
   inherited;
   FTyp:=Prog.TypBoolean;
end;

// EvalAsVariant
//
procedure TBooleanBinOpExpr.EvalAsVariant(exec : TdwsExecution; var result : Variant);
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

// GetIsConstant
//
function TUnaryOpExpr.GetIsConstant : Boolean;
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
   buf : UnicodeString;
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

{ TProgramInfo }

function TProgramInfo.GetValueAsVariant(const s: UnicodeString): Variant;
begin
  Result := GetVars(s).Value;
end;

function TProgramInfo.GetData(const s: UnicodeString): TData;
begin
  Result := GetVars(s).Data;
end;

procedure TProgramInfo.SetValueAsVariant(const s: UnicodeString; const Value: Variant);
begin
  GetVars(s).Value := Value;
end;

procedure TProgramInfo.SetData(const s: UnicodeString; const Value: TData);
begin
  GetVars(s).Data := Value;
end;

// GetSymbolInfo
//
procedure TProgramInfo.GetSymbolInfo(sym : TSymbol; var info : IInfo);

   procedure GetExternalVarSymbolInfo(sym : TSymbol; var Result : IInfo);
   var
      dat : TData;
      extVDM : TExternalVarDataMaster;
      locData : IDataContext;
   begin
      SetLength(dat, sym.Typ.Size);
      extVDM := TExternalVarDataMaster.Create(Execution, TExternalVarSymbol(sym));
      if sym.Typ is TClassSymbol then
         extVDM.Read(Execution, dat); // initialize 'Self'-Object
      Execution.DataContext_Create(dat, 0, locData);
      TInfo.SetChild(Result, Self, sym.Typ, locData, extVDM);
   end;

   procedure GetTypeSymbolInfo(sym : TSymbol; var Result : IInfo);
   var
      dat : TData;
      locData : IDataContext;
   begin
      if sym.BaseType is TClassSymbol then begin
         SetLength(dat, 1);
         VarClear(dat[0]);
         Execution.DataContext_Create(dat, 0, locData);
         Result := TInfoClassObj.Create(Self, sym, locData);
      end else Result:=nil;
   end;

   procedure GetVarParamVars(sym : TDataSymbol; basePointer : Integer; var Result : IInfo);
   var
      dp : IDataContext;
   begin
      dp:=IDataContext(IUnknown(Execution.Stack.Data[basePointer+sym.StackAddr]));
      TInfo.SetChild(Result, Self, sym.Typ, dp);
   end;

   procedure GetDataSymbol(sym : TDataSymbol; var Result : IInfo);
   var
      basePointer : Integer;
      pin : TProgramInfo;
      exec : TdwsExecution;
      locData : IDataContext;
   begin
      pin:=Self;
      exec:=pin.Execution;
      if sym.Level=pin.FLevel then
         basePointer:=exec.Stack.BasePointer
      else basePointer:=exec.Stack.GetSavedBp(pin.Level);
      if    (sym is TByRefParamSymbol)
         or (sym.Typ is TOpenArraySymbol)
         or ((sym is TSelfSymbol) and (sym.Typ is TRecordSymbol))
         then begin
         GetVarParamVars(sym, basePointer, Result);
      end else begin
         exec.DataContext_CreateBase(sym.StackAddr, locData);
         TInfo.SetChild(Result, pin, sym.Typ, locData);
      end;
   end;

   procedure GetFieldSymbol(sym : TFieldSymbol; var Result : IInfo);
   var
      locData : IDataContext;
   begin
      // Field of the Self object
      if sym.StructSymbol is TRecordSymbol then begin
         Execution.DataContext_Create(Self.GetData(SYS_SELF), sym.Offset, locData);
      end else begin
         Execution.DataContext_Create(FScriptObj.AsData, sym.Offset, locData);
      end;
      TInfo.SetChild(Result, Self, sym.Typ, locData);
   end;

   procedure GetConstSymbol(sym : TConstSymbol; var Result : IInfo);
   var
      locData : IDataContext;
   begin
      Execution.DataContext_Create(sym.Data, 0, locData);
      TInfo.SetChild(Result, Self, sym.Typ, locData);
   end;

begin
   if sym is TDataSymbol then
      GetDataSymbol(TDataSymbol(sym), info)
   else if sym is TConstSymbol then
      GetConstSymbol(TConstSymbol(sym), info)
   else if sym is TFieldSymbol then
      GetFieldSymbol(TFieldSymbol(sym), info)
   else if sym is TExternalVarSymbol then
      GetExternalVarSymbolInfo(sym, info)
   else if sym is TTypeSymbol then
      GetTypeSymbolInfo(sym, info)
   else RaiseOnlyVarSymbols(sym);
end;

// GetVars
//
function TProgramInfo.GetVars(const str : UnicodeString): IInfo;

var
   sym : TSymbol;
begin
   sym:=FTable.FindSymbol(str, cvMagic);

   if not Assigned(sym) then
      RaiseVariableNotFound(str)
   else GetSymbolInfo(sym, Result);
end;

// GetParams
//
function TProgramInfo.GetParams(const Index: Integer): IInfo;
var
   ip : TSymbolTable;
   sym: TSymbol;
begin
   ip:=FuncSym.Params;
   if Cardinal(index)>=Cardinal(ip.Count) then begin
      RaiseIncorrectParameterIndex(index);
      Result:=nil;
   end else begin
      sym:=ip[index];
      if not Assigned(sym) then
         RaiseVariableNotFound(ip[index].Name)
      else GetSymbolInfo(sym, Result);
   end;
end;

// GetFunc
//
function TProgramInfo.GetFunc(const s: UnicodeString): IInfo;
var
   sym : TSymbol;
   funcSym : TFuncSymbol;
begin
   sym := FTable.FindSymbol(s, cvMagic);

   if not Assigned(sym) then
      raise Exception.CreateFmt(RTE_FunctionNotFound, [s]);

   funcSym := sym.AsFuncSymbol;
   if funcSym <> nil then
      Result := GetFuncBySym(funcSym)
   else raise Exception.CreateFmt(RTE_OnlyFuncSymbols, [sym.Caption]);
end;

// GetFuncBySym
//
function TProgramInfo.GetFuncBySym(funcSym : TFuncSymbol): IInfo;
begin
   if Assigned(FScriptObj) then begin
      Result := TInfoFunc.Create(Self, funcSym, Execution.DataContext_Nil,
                                 nil, FScriptObj, FScriptObj.ClassSym)
   end else begin
      Result := TInfoFunc.Create(Self, funcSym, Execution.DataContext_Nil, nil, nil, nil)
   end;
end;

function TProgramInfo.GetTemp(const DataType: UnicodeString): IInfo;
var
  data: TData;
  typSym: TTypeSymbol;
  locData : IDataContext;
begin
  typSym := FTable.FindTypeSymbol(DataType, cvMagic);

  if not Assigned(typSym) then
    raise Exception.CreateFmt(RTE_DatatypeNotFound, [DataType]);

  data := nil;
  SetLength(data, typSym.Size);
  typSym.InitData(data, 0);

  Execution.DataContext_Create(data, 0, locData);
  TInfo.SetChild(Result, Self, typSym, locData);
end;

// RaiseExceptObj
//
procedure TProgramInfo.RaiseExceptObj(const msg : UnicodeString; const obj : IScriptObj);
begin
   raise EScriptException.Create(msg, obj, cNullPos);
end;

procedure TProgramInfo.SetFuncSym(const Value: TFuncSymbol);
begin
  FFuncSym := Value;
  if Assigned(FFuncSym) then
    FLevel := FFuncSym.Level      // 1
  else
    FLevel := 0;
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

function TProgramInfo.GetValueAsString(const s: UnicodeString): UnicodeString;
begin
  Result:=GetVars(s).ValueAsString;
end;

procedure TProgramInfo.SetValueAsString(const s: UnicodeString; const Value: UnicodeString);
begin
  GetVars(s).Value:=Value;
end;

// GetValueAsChar
//
function TProgramInfo.GetValueAsChar(const s: UnicodeString): WideChar;
var
   buf : UnicodeString;
begin
   buf:=GetVars(s).ValueAsString;
   if buf<>'' then
      Result:=buf[1]
   else Result:=#0;
end;

// GetValueAsDataString
//
function TProgramInfo.GetValueAsDataString(const s: UnicodeString): RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetValueAsString(s));
end;

// SetValueAsDataString
//
procedure TProgramInfo.SetValueAsDataString(const s: UnicodeString; const Value: RawByteString);
begin
   SetValueAsString(s, RawByteStringToScriptString(Value));
end;

function TProgramInfo.GetValueAsInteger(const s: UnicodeString): Int64;
begin
  Result:=GetVars(s).ValueAsInteger;
end;

procedure TProgramInfo.SetValueAsInteger(const s: UnicodeString; const Value: Int64);
begin
  GetVars(s).Value:=Value;
end;

function TProgramInfo.GetValueAsBoolean(const s: UnicodeString): Boolean;
begin
  Result:=GetVars(s).Value;
end;

procedure TProgramInfo.SetValueAsBoolean(const s: UnicodeString; const Value: Boolean);
begin
  GetVars(s).Value:=Value;
end;

function TProgramInfo.GetValueAsFloat(const s: UnicodeString): Double;
begin
  Result:=GetVars(s).ValueAsFloat;
end;

procedure TProgramInfo.SetValueAsFloat(const s: UnicodeString; const Value: Double);
begin
  GetVars(s).Value:=Value;
end;

function TProgramInfo.GetValueAsObject(const s: UnicodeString): TObject;
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
function TProgramInfo.GetValueAsClassSymbol(const s: UnicodeString): TClassSymbol;
begin
   Result:=TClassSymbol(GetVars(s).ValueAsInteger);
end;

function TProgramInfo.GetValueAsTStrings(const s: UnicodeString): TStrings;
var
   obj : TObject;
begin
   obj:=GetValueAsObject(s);
   if Assigned(obj) then
      Result:=obj as TStrings
   else Result:=nil;
end;

// GetResultAsPVariant
//
function TProgramInfo.GetResultAsPVariant : PVariant;
var
   sym : TDataSymbol;
   stackAddr : Integer;
   exec : TdwsExecution;
begin
   sym:=FuncSym.Result;
   if sym=nil then
      RaiseVariableNotFound(SYS_RESULT);
   Assert(sym.InheritsFrom(TDataSymbol));
   exec:=Execution;
   if sym.Level=FLevel then
      stackAddr:=sym.StackAddr+exec.Stack.BasePointer
   else stackAddr:=sym.StackAddr+exec.Stack.GetSavedBp(Level);
   Result:=@exec.Stack.Data[stackAddr];
end;

procedure TProgramInfo.SetResultAsVariant(const Value: Variant);
begin
   GetResultAsPVariant^:=value;
end;

// SetResultAsString
//
procedure TProgramInfo.SetResultAsString(const value : UnicodeString);
begin
   GetResultAsPVariant^:=value;
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
   GetResultAsPVariant^:=value;
end;

// SetResultAsBoolean
//
procedure TProgramInfo.SetResultAsBoolean(const value : Boolean);
begin
   GetResultAsPVariant^:=value;
end;

// SetResultAsFloat
//
procedure TProgramInfo.SetResultAsFloat(const value : Double);
begin
   GetResultAsPVariant^:=value;
end;

// GetParamAsPVariant
//
function TProgramInfo.GetParamAsPVariant(index : Integer) : PVariant;

   function GetVarParam(stackAddr : Integer) : PVariant;
   var
      vpd : IDataContext;
   begin
      vpd:=IDataContext(IUnknown(Execution.Stack.Data[stackAddr]));
      Result:=vpd.AsPVariant(0);
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

// SetParamAsVariant
//
procedure TProgramInfo.SetParamAsVariant(index : Integer; const v : Variant);
var
   p : PVariant;
begin
   p:=GetParamAsPVariant(index);
   p^:=v;
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

// SetParamAsInteger
//
procedure TProgramInfo.SetParamAsInteger(index : Integer; const v : Int64);
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   if p^.VType=varInt64 then
      p.VInt64:=v
   else PVariant(p)^:=v;
end;

// GetParamAsString
//
function TProgramInfo.GetParamAsString(index : Integer) : UnicodeString;
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   {$ifdef FPC}
   if p^.VType=varString then
      Result:=UnicodeString(p.VString)
   {$else}
   if p^.VType=varUString then
      Result:=UnicodeString(p.VUString)
   {$endif}
   else VariantToString(PVariant(p)^, Result);
end;

// SetParamAsString
//
procedure TProgramInfo.SetParamAsString(index : Integer; const v : UnicodeString);
begin
   GetParamAsPVariant(index)^:=v;
end;

// GetParamAsDataString
//
function TProgramInfo.GetParamAsDataString(index : Integer) : RawByteString;
begin
   Result:=ScriptStringToRawByteString(GetParamAsString(index));
end;

// SetParamAsDataString
//
procedure TProgramInfo.SetParamAsDataString(index : Integer; const v : RawByteString);
begin
   GetParamAsPVariant(index)^:=RawByteStringToScriptString(v);
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
var
   p : PVarData;
begin
   p:=PVarData(GetParamAsPVariant(index));
   Assert(p.VType=varUnknown);
   if p.VUnknown<>nil then
      Result:=(IUnknown(p.VUnknown) as IScriptObj).ExternalObject
   else Result:=nil;
end;

function TProgramInfo.FindClassMatch(AObject: TObject; ExactMatch: Boolean): TClassSymbol;
var
  {$ifdef FPC}
  ParentRTTI: PTypeInfo;
  {$else}
  ParentRTTI: PPTypeInfo;
  {$endif}
  unitList: TUnitSymbolRefList;      // build the list once, may search for many symbols
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
    if not (ExactMatch or Assigned(typeSym)) then
    begin
      if AObject.ClassInfo <> nil then
      begin
        ParentRTTI := GetTypeData(AObject.ClassInfo).ParentInfo;
        repeat
          typeSym := FindSymbolInUnits(unitList, UnicodeString(ParentRTTI^.Name));
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

function TProgramInfo.RegisterExternalObject(AObject: TObject; AutoFree: Boolean; ExactClassMatch: Boolean): IScriptObj;
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
    Result := NewScriptObj;
  end
  else                              // no class returned or no object provided
    Result := nil;                  // return 'nil' Id
end;

function TProgramInfo.GetExternalObjForVar(const s: UnicodeString): TObject;
var
  sObj: IScriptObj;
begin
  sObj := Vars[s].ScriptObj;
  if Assigned(sObj) then
    Result := sObj.ExternalObject
  else
    Result := nil;
end;

function TProgramInfo.FindSymbolInUnits(aUnitList: TUnitSymbolRefList; const aName: UnicodeString) : TSymbol;
var
   i : Integer;
   table : TUnitSymbolTable;
begin
   // Search all units for the symbol
   for i := 0 to aUnitList.Count-1 do begin
      table := aUnitList[i].Table;
      if table <> nil then begin
         Result := table.FindLocal(aName);
         if Assigned(Result) then
            Exit;
      end;
   end;
   Result := nil;
end;

// GetSystemTable
//
function TProgramInfo.GetSystemTable : TSystemSymbolTable;
begin
   Result:=Execution.Prog.SystemTable.SymbolTable;
end;

// PrepareScriptObj
//
procedure TProgramInfo.PrepareScriptObj;
begin
   FScriptObj:=FExecution.SelfScriptObject^;
end;

// FindSymbolInUnits
//
function TProgramInfo.FindSymbolInUnits(const aName : UnicodeString) : TSymbol;
var
   list : TUnitSymbolRefList;
begin
   list := CreateUnitList;
   try
      Result := FindSymbolInUnits(list, aName);
   finally
      list.Free;
   end;
end;

// CreateUnitList
//
function TProgramInfo.CreateUnitList : TUnitSymbolRefList;
var
   root : TSymbolTable;
   sym : TSymbol;
   unitSym : TUnitSymbol;
   i : Integer;
begin
   // Find the root table for the full compiled program (not just the function)
   if Assigned(Execution) then
      root := Execution.Prog.RootTable
   else begin
      // if no caller provided, make a 'best effort' to find a root.
      root := FTable;
      while root.ParentCount > 0 do
         root := root.Parents[0];
   end;

   // caller reponsible for freeing
   Result := TUnitSymbolRefList.Create;

   // Add all unit symbols to a list
   for i := 0 to root.Count-1 do begin
      sym := root.Symbols[i];
      if sym.ClassType = TUnitSymbol then begin
         unitSym := TUnitSymbol(sym);
         if Result.IndexOf(unitSym) < 0 then  // (units may reuse others)
            Result.Add(unitSym);
      end;
   end;
end;

// ------------------
// ------------------ TScriptObj ------------------
// ------------------

// SetExecutionContext
//
procedure TScriptObj.SetExecutionContext(exec : TdwsProgramExecution);
begin
   // ignore
end;

// ------------------
// ------------------ TScriptObjInstance ------------------
// ------------------

// Create
//
constructor TScriptObjInstance.Create(aClassSym : TClassSymbol; executionContext : TdwsProgramExecution);
var
   externalClass : TClassSymbol;
   fieldIter : TFieldSymbol;
   instanceData : PData;
begin
   FClassSym:=aClassSym;
   if aClassSym=nil then Exit;

   if executionContext<>nil then
      executionContext.ScriptObjCreated(Self);

   SetDataLength(aClassSym.ScriptInstanceSize);

   // initialize fields
   instanceData:=AsPData;
   fieldIter:=aClassSym.FirstField;
   while fieldIter<>nil do begin
      fieldIter.InitData(instanceData^, 0);
      fieldIter:=fieldIter.NextField;
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
      FOnObjectDestroy(FExternalObject);
   inherited;
end;

// BeforeDestruction
//
procedure TScriptObjInstance.BeforeDestruction;

   procedure CallDestructor;
   var
      iso : IScriptObj;
   begin
      iso:=TScriptObjectWrapper.Create(Self);
      ExecutionContext.DestroyScriptObj(iso);
   end;

var
   destroySym : TMethodSymbol;
begin
   if Assigned(FExecutionContext) then begin
      // we are released, so never do: Self as IScriptObj
      if not FDestroyed then begin
         destroySym:=ExecutionContext.Prog.TypDefaultDestructor;
         if destroySym=ClassSym.VMTMethod(destroySym.VMTIndex) then
            Destroyed:=True
         else CallDestructor;
      end;
      ExecutionContext.ScriptObjDestroyed(Self);
   end;
   inherited;
end;

// ToString
//
function TScriptObjInstance.ToString : String;
begin
   Result:=FClassSym.Name;
end;

// ClearData
//
procedure TScriptObjInstance.ClearData;
begin
   inherited;
   FClassSym:=nil;
end;

// GetClassSym
//
function TScriptObjInstance.GetClassSym: TClassSymbol;
begin
   Result:=FClassSym;
end;

// GetExternalObject
//
function TScriptObjInstance.GetExternalObject: TObject;
begin
   Result:=FExternalObject;
end;

// SetExternalObject
//
procedure TScriptObjInstance.SetExternalObject(Value: TObject);
begin
   FExternalObject:=Value;
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
      FOnObjectDestroy(FExternalObject);
      FOnObjectDestroy:=nil;
      FExternalObject:=nil;
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

// CreateNew
//
class function TScriptDynamicArray.CreateNew(elemTyp : TTypeSymbol) : TScriptDynamicArray;
var
   size : Integer;
begin
   if elemTyp<>nil then
      size:=elemTyp.Size
   else size:=0;
   if size=1 then
      Result:=TScriptDynamicValueArray.Create
   else Result:=TScriptDynamicDataArray.Create;
   Result.FElementTyp:=elemTyp;
   Result.FElementSize:=size;
end;

// TScriptDynamicArray_InitData
//
procedure TScriptDynamicArray_InitData(elemTyp : TTypeSymbol; var result : Variant);
begin
   result:=IScriptDynArray(TScriptDynamicArray.CreateNew(elemTyp));
end;

// SetArrayLength
//
procedure TScriptDynamicArray.SetArrayLength(n : Integer);
var
   i : Integer;
   p : PData;
begin
   SetDataLength(n*ElementSize);
   p:=AsPData;
   for i:=FArrayLength to n-1 do
      FElementTyp.InitData(p^, i*ElementSize);
   FArrayLength:=n;
end;

// GetArrayLength
//
function TScriptDynamicArray.GetArrayLength : Integer;
begin
   Result:=FArrayLength;
end;

// ReplaceData
//
procedure TScriptDynamicArray.ReplaceData(const newData : TData);
begin
   inherited;
   FArrayLength:=System.Length(newData) div ElementSize;
end;

// Insert
//
procedure TScriptDynamicArray.Insert(index : Integer);
var
   n : Integer;
begin
   Inc(FArrayLength);
   SetDataLength(FArrayLength*ElementSize);
   n:=(FArrayLength-index-1)*ElementSize*SizeOf(Variant);
   if n>0 then
      Move(AsData[index*ElementSize], AsData[(index+1)*ElementSize], n);
   FillChar(AsData[index*ElementSize], ElementSize*SizeOf(Variant), 0);
   FElementTyp.InitData(AsData, index*ElementSize);
end;

// Delete
//
procedure TScriptDynamicArray.Delete(index, count : Integer);
var
   i, d : Integer;
begin
   if count<=0 then Exit;
   Dec(FArrayLength, count);
   index:=index*ElementSize;
   count:=count*ElementSize;
   for i:=index to index+count-1 do
      VarClear(AsPVariant(i)^);
   d:=(FArrayLength-1)*ElementSize+count-index;
   if d>0 then
      System.Move(AsData[index+count], AsData[index], d*SizeOf(Variant));
   System.FillChar(AsData[FArrayLength*ElementSize], count*SizeOf(Variant), 0);
   SetDataLength(FArrayLength*ElementSize);
end;

// Reverse
//
procedure TScriptDynamicArray.Reverse;
var
   t, b : Integer;
begin
   t:=ArrayLength-1;
   b:=0;
   while t>b do begin
      Swap(t, b);
      Dec(t);
      Inc(b);
   end;
end;

// Copy
//
procedure TScriptDynamicArray.Copy(src : TScriptDynamicArray; index, count : Integer);
begin
   RawCopy(src.AsData, index*ElementSize, count*ElementSize);
end;

// RawCopy
//
procedure TScriptDynamicArray.RawCopy(const src : TData; rawIndex, rawCount : Integer);
var
   i : Integer;
begin
   FArrayLength:=rawCount div ElementSize;
   SetDataLength(rawCount);
   for i:=rawIndex to rawIndex+rawCount-1 do
      AsVariant[i-rawIndex]:=src[i];
end;

// Concat
//
procedure TScriptDynamicArray.Concat(src : TScriptDynamicArray);
var
   n, nSrc : Integer;
begin
   if src.ArrayLength>0 then begin
      n:=ArrayLength;
      nSrc:=src.ArrayLength;
      FArrayLength:=n+nSrc;
      SetDataLength(FArrayLength*ElementSize);
      DWSCopyData(src.AsData, 0, AsData, n*ElementSize, nSrc*ElementSize);
   end;
end;

// IndexOfData
//
function TScriptDynamicArray.IndexOfData(const item : IDataContext; fromIndex : Integer) : Integer;
var
   i : Integer;
   data : PData;
begin
   data:=AsPData;
   for i:=fromIndex to ArrayLength-1 do
      if item.SameData(0, data^, i*ElementSize, ElementSize) then
         Exit(i);
   Result:=-1;
end;

// IndexOfValue
//
function TScriptDynamicArray.IndexOfValue(const item : Variant; fromIndex : Integer) : Integer;
var
   i : Integer;
   data : PData;
begin
   Assert(ElementSize=1);
   data:=AsPData;
   for i:=fromIndex to ArrayLength-1 do
      if DWSSameVariant(data^[i], item) then
         Exit(i);
   Result:=-1;
end;

// IndexOfString
//
function TScriptDynamicArray.IndexOfString(const item : UnicodeString; fromIndex : Integer) : Integer;
var
   i : Integer;
   varData : PVarData;
begin
   if fromIndex<ArrayLength then begin
      varData:=@AsPData^[fromIndex];
      for i:=fromIndex to ArrayLength-1 do begin
         Assert(varData^.VType=varUString);
         if UnicodeString(varData^.VString)=item then
            Exit(i);
         Inc(varData);
      end;
   end;
   Result:=-1;
end;

// IndexOfInteger
//
function TScriptDynamicArray.IndexOfInteger(const item : Int64; fromIndex : Integer) : Integer;
var
   i : Integer;
   varData : PVarData;
begin
   if fromIndex<ArrayLength then begin
      varData:=@AsPData^[fromIndex];
      for i:=fromIndex to ArrayLength-1 do begin
         Assert(varData^.VType=varInt64);
         if varData^.VInt64=item then
            Exit(i);
         Inc(varData);
      end;
   end;
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
      for i:=fromIndex to ArrayLength-1 do begin
         p:=PVarData(AsPVariant(i));
         if (p.VType=varUnknown) and (p.VUnknown=nil) then
            Exit(i);
      end;
   end else begin
      for i:=fromIndex to ArrayLength-1 do
         if itemFunc.SameFunc(AsPVariant(i)^) then
            Exit(i);
   end;
   Result:=-1;
end;

// ToString
//
function TScriptDynamicArray.ToString : String;
begin
   Result:='array of '+FElementTyp.Name;
end;

// ToStringArray
//
function TScriptDynamicArray.ToStringArray : TStringDynArray;
var
   i : Integer;
begin
   Assert(FElementTyp.BaseType.ClassType=TBaseStringSymbol);

   System.SetLength(Result, ArrayLength);
   for i:=0 to ArrayLength-1 do
      EvalAsString(i, Result[i]);
end;

// GetElementSize
//
function TScriptDynamicArray.GetElementSize : Integer;
begin
   Result:=FElementSize;
end;

// ------------------
// ------------------ TScriptDynamicValueArray ------------------
// ------------------

// Swap
//
procedure TScriptDynamicValueArray.Swap(i1, i2 : Integer);
var
   elem1, elem2 : PVarData;
   buf : TVarData;
begin
   elem1:=@DirectData[i1];
   elem2:=@DirectData[i2];
   buf.VType:=elem1^.VType;
   buf.VInt64:=elem1^.VInt64;
   elem1^.VType:=elem2^.VType;
   elem1^.VInt64:=elem2^.VInt64;
   elem2^.VType:=buf.VType;
   elem2^.VInt64:=buf.VInt64;
end;

// CompareString
//
function TScriptDynamicValueArray.CompareString(i1, i2 : Integer) : Integer;
var
   p : PVarDataArray;
   v1, v2 : PVarData;
begin
   p:=@DirectData[0];
   v1:=@p[i1];
   v2:=@p[i2];
   Assert((v1.VType=varUString) and (v2.VType=varUString));
   Result:=UnicodeCompareStr(UnicodeString(v1.VString), UnicodeString(v2.VString));
end;

// CompareInteger
//
function TScriptDynamicValueArray.CompareInteger(i1, i2 : Integer) : Integer;
var
   p : PVarDataArray;
   v1, v2 : PVarData;
begin
   p:=@DirectData[0];
   v1:=@p[i1];
   v2:=@p[i2];
   Assert((v1.VType=varInt64) and (v2.VType=varInt64));
   if v1.VInt64<v2.VInt64 then
      Result:=-1
   else Result:=Ord(v1.VInt64>v2.VInt64);
end;

// CompareFloat
//
function TScriptDynamicValueArray.CompareFloat(i1, i2 : Integer) : Integer;
var
   p : PVarDataArray;
   v1, v2 : PVarData;
begin
   p:=@DirectData[0];
   v1:=@p[i1];
   v2:=@p[i2];
   Assert((v1.VType=varDouble) and (v2.VType=varDouble));
   if v1.VDouble<v2.VDouble then
      Result:=-1
   else Result:=Ord(v1.VDouble>v2.VDouble);
end;

// ------------------
// ------------------ TScriptDynamicDataArray ------------------
// ------------------

// Swap
//
procedure TScriptDynamicDataArray.Swap(i1, i2 : Integer);
var
   i : Integer;
   elem1, elem2 : PVarData;
   buf : TVarData;
begin
   elem1:=@DirectData[i1*ElementSize];
   elem2:=@DirectData[i2*ElementSize];
   for i:=1 to ElementSize do begin
      buf:=elem1^;
      elem1^:=elem2^;
      elem2^:=buf;
      Inc(elem1);
      Inc(elem2);
   end;
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
      FExecutionContext.ScriptObjDestroyed(Self);
   inherited;
end;

// ToString
//
function TScriptInterface.ToString : String;
begin
   Result:=FTyp.ClassName;
end;

// GetScriptObj
//
function TScriptInterface.GetScriptObj : IScriptObj;
begin
   Result:=Instance;
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
function TdwsSymbolDictionary.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : UnicodeString) : TSymbol;
var
   i : Integer;
begin
   Result:=nil;
   for i:=0 to FSymbolList.Count-1 do begin
      Result:=FSymbolList[i].FindSymbolAtPosition(aCol, aLine, sourceFile);
      if Assigned(Result) then Break;
   end;
end;

// FindSymbolAtPosition
//
function TdwsSymbolDictionary.FindSymbolAtPosition(const aScriptPos: TScriptPos): TSymbol;
begin
   Result:=FindSymbolAtPosition(aScriptPos.Col, aScriptPos.Line, aScriptPos.SourceName);
end;

// GetEnumerator
//
function TdwsSymbolDictionary.GetEnumerator: TdwsSymbolDictionaryEnumerator;
begin
   if Self=nil then begin
      Result.Dict:=nil;
      Result.Index:=0;
   end else begin
      Result.Dict:=Self;
      Result.Index:=Count;
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
function TdwsSymbolDictionary.FindSymbolPosList(const symName : UnicodeString) : TSymbolPositionList;
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
   funcSym : TFuncSymbol;
begin
   // TFuncSymbol - remove params
   funcSym:=sym.AsFuncSymbol;
   if funcSym<>nil then begin
      for i := 0 to funcSym.Params.Count - 1 do
         Remove(funcSym.Params[i]);
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
      SuppressH2077ValueAssignedToVariableNeverUsed(idx);
      symList.Free;
   end;
end;

// RemoveInRange
//
procedure TdwsSymbolDictionary.RemoveInRange(const startPos, endPos : TScriptPos);
var
   i : Integer;
   symPosList : TSymbolPositionList;
begin
   if startPos.SourceFile<>endPos.SourceFile then Exit;

   for i:=0 to FSymbolList.Count-1 do begin
      symPosList:=FSymbolList[i];
      if symPosList.FSourceFile=startPos.SourceFile then
         FSymbolList[i].RemoveInRange(startPos, endPos);
   end;
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

// EnumerateInRange
//
procedure TdwsSymbolDictionary.EnumerateInRange(const startPos, endPos : TScriptPos; const callBack : TdwsSymbolDictionaryRef);
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

function TdwsSymbolDictionary.FindSymbolUsage(const SymName: UnicodeString;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(SymName);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TdwsSymbolDictionary.FindSymbolUsageOfType(const SymName: UnicodeString;
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

function TdwsSymbolDictionary.FindSymbolPosListOfType(const SymName: UnicodeString;
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

function TdwsSymbolDictionary.TdwsSymbolDictionaryEnumerator.GetCurrent: TSymbolPositionList;
begin
   Result:=Dict[Index];
end;

function TdwsSymbolDictionary.TdwsSymbolDictionaryEnumerator.MoveNext: Boolean;
begin
   Dec(Index);
   Result:=(Index>=0);
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

   if FPosList.Count=0 then
      FSourceFile:=scriptPos.SourceFile
   else if FSourceFile<>scriptPos.SourceFile then
      FSourceFile:=nil;

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
function TSymbolPositionList.FindSymbolAtPosition(aCol, aLine : Integer; const sourceFile : UnicodeString): TSymbol;
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
         if symbolUse in Result.SymbolUsages then Exit;
      end;
   end;
   Result:=nil;
end;

// FindAnyUsage
//
function TSymbolPositionList.FindAnyUsage(const symbolUses : TSymbolUsages) : TSymbolPosition;
var
   i : Integer;
begin
   if Self<>nil then begin
      for i:=0 to Count-1 do begin
         Result:=FPosList[i];
         if (symbolUses*Result.SymbolUsages)<>[] then Exit;
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

// GetEnumerator
//
function TSymbolPositionList.GetEnumerator: TSymbolPositionListEnumerator;
begin
   if Self=nil then begin
      Result.PosList:=nil;
      Result.Index:=0;
   end else begin
      Result.PosList:=Self;
      Result.Index:=Count;
   end;
end;

// TSymbolPositionListEnumerator.GetCurrent
//
function TSymbolPositionList.TSymbolPositionListEnumerator.GetCurrent: TSymbolPosition;
begin
   Result:=PosList[Index];
end;

// TSymbolPositionListEnumerator.MoveNext
//
function TSymbolPositionList.TSymbolPositionListEnumerator.MoveNext: Boolean;
begin
   Dec(Index);
   Result:=(Index>=0);
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

// GetSubContext
//
function TdwsSourceContext.GetSubContext(index : Integer) : TdwsSourceContext;
begin
   Result:=TdwsSourceContext(FSubContexts.List[index]);
end;

// HasParentSymbolOfClass
//
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
function TdwsSourceContext.IsPositionInContext(const aPos : TScriptPos) : Boolean;
begin
   Result := IsPositionInContext(aPos.Col, aPos.Line, aPos.SourceFile.Name);
end;

// IsPositionInContext
//
function TdwsSourceContext.IsPositionInContext(aCol, aLine : Integer; const sourceName : UnicodeString) : Boolean;
begin
   // check if the position is in the same SourceFile
   if sourceName<>'' then begin // if empty, don't check it
      if not FStartPos.IsSourceFile(sourceName) then begin
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
function TdwsSourceContextMap.FindContext(aCol, aLine : Integer; const sourceName : UnicodeString) : TdwsSourceContext;
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
   Result:=FindContext(scriptPos.Col, scriptPos.Line, scriptPos.SourceName);
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
   Assert((FCurrentContext=nil) or (FCurrentContext.Token=ttUNIT));
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

constructor TScriptSourceItem.Create(const ANameReference: UnicodeString; ASourceFile: TSourceFile;
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
function TScriptSourceList.Add(const nameReference, code: UnicodeString;
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
function TScriptSourceList.FindScriptSourceItem(const sourceFileName: UnicodeString): TScriptSourceItem;
var
   x : Integer;
begin
   x:=IndexOf(SourceFileName);
   if x>=0 then
      Result:=Items[x]
   else Result:=nil;
end;

function TScriptSourceList.IndexOf(const SourceFileName: UnicodeString): Integer;
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

constructor TMethodObjExpr.Create(Prog: TdwsProgram; const aScriptPos: TScriptPos;
                                  BaseExpr: TDataExpr);
begin
   Assert(BaseExpr.Typ is TMethodSymbol);
   inherited Create(aScriptPos, TMethodSymbol(BaseExpr.Typ).StructSymbol);
   FBaseExpr := BaseExpr;
end;

// GetDataPtr
//
procedure TMethodObjExpr.GetDataPtr(exec : TdwsExecution; var result : IDataContext);
begin
   FBaseExpr.DataPtr[exec].CreateOffset(1, result);
end;

// ------------------
// ------------------ TNoResultWrapperExpr ------------------
// ------------------

// Create
//
constructor TNoResultWrapperExpr.Create(const aScriptPos: TScriptPos; Expr: TTypedExpr);
begin
   inherited Create(aScriptPos);
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

// GetIsConstant
//
function TNoResultWrapperExpr.GetIsConstant : Boolean;
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
constructor TSourceCondition.Create(const aScriptPos: TScriptPos; aTest, aMsg : TTypedExpr);
begin
   inherited Create;
   FScriptPos:=aScriptPos;
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
procedure TSourceCondition.InitSymbol(symbol: TSymbol; const msgs : TdwsCompileMessageList);
begin
end;

// InitExpression
//
procedure TSourceCondition.InitExpression(Expr: TExprBase);
begin
   // nothing
end;

// SubExpr
//
function TSourceCondition.SubExpr(i : Integer) : TExprBase;
begin
   if i=0 then
      Result:=FTest
   else Result:=FMsg;
end;

// SubExprCount
//
function TSourceCondition.SubExprCount : Integer;
begin
   Result:=2;
end;

// EvalAsBoolean
//
function TSourceCondition.EvalAsBoolean(exec : TdwsExecution) : Boolean;
begin
   Result:=FTest.EvalAsBoolean(exec);
end;

// EvalAsString
//
procedure TSourceCondition.EvalAsString(exec : TdwsExecution; var Result : UnicodeString);
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
      RaiseConditionFailed(exec, FProg.FFunc, failed.ScriptPos, failed);
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
   msgStr : UnicodeString;
begin
   msg.EvalAsString(exec, msgStr);
   (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(nil,
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
   msgStr : UnicodeString;
begin
   msg.EvalAsString(exec, msgStr);
   (exec as TdwsProgramExecution).RaiseAssertionFailedFmt(nil,
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
// ------------------ TExternalFuncHandler ------------------
// ------------------

// InitSymbol
//
procedure TExternalFuncHandler.InitSymbol(symbol: TSymbol; const msgs : TdwsCompileMessageList);
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

// CompileTimeCheck
//
procedure TExternalFuncHandler.CompileTimeCheck(prog : TdwsProgram; expr : TFuncExprBase);
begin
   // nothing yet
end;

// SubExpr
//
function TExternalFuncHandler.SubExpr(i : Integer) : TExprBase;
begin
   Result:=nil;
end;

// SubExprCount
//
function TExternalFuncHandler.SubExprCount : Integer;
begin
   Result:=0;
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
      func := FOverloads.Items[i];
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

// ------------------
// ------------------ TdwsCustomStates ------------------
// ------------------

// SameItem
//
function TdwsCustomStates.SameItem(const item1, item2 : TdwsCustomState) : Boolean;
begin
   Result:=IsEqualGUID(item1.Key, item2.Key);
end;

// GetItemHashCode
//
function TdwsCustomStates.GetItemHashCode(const item1 : TdwsCustomState) : Integer;
type
   TInteger4 = array [0..3] of Integer;
   PInteger4 = ^TInteger4;
var
   p : PInteger4;
begin
   p:=PInteger4(@item1.Key);
   Result:=p[0] xor p[1] xor p[2] xor p[3];
end;

// GetState
//
function TdwsCustomStates.GetState(const index : TGUID) : Variant;
var
   s : TdwsCustomState;
begin
   s.Key:=index;
   if Match(s) then
      Result:=s.Value
   else Result:=Unassigned;
end;

// SetState
//
procedure TdwsCustomStates.SetState(const index : TGUID; const v : Variant);
var
   s : TdwsCustomState;
begin
   s.Key:=index;
   s.Value:=v;
   Replace(s);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TdwsGuardianThread.Initialize;
   TDynamicArraySymbol.SetInitDynamicArrayProc(TScriptDynamicArray_InitData);

finalization

   TdwsGuardianThread.Finalize;
   TdwsGuardianThread.vExecutionsPool.FreeAll;

end.
