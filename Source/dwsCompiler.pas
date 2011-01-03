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
  dwsMagicExprs, dwsRelExprs;

type
   TCompilerOption = (coOptimize, coSymbolDictionary, coContextMap);
   TCompilerOptions = set of TCompilerOption;

const
   cDefaultCompilerOptions = [coOptimize];
   cDefaultMaxRecursionDepth = 1024;

type
  TIncludeEvent = procedure(const scriptName: string; var scriptSource: string) of object;

  TdwsCompiler = class;
  TCompilerReadInstrEvent = function (compiler : TdwsCompiler) : TNoResultExpr of object;

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
    FOwner: TComponent;
    FResultType: TdwsResultType;
    FScriptPaths: TStrings;
    FConditionals: TStringList;
    FStackChunkSize: Integer;
    FSystemTable: TSymbolTable;
    FTimeoutMilliseconds: Integer;
    FUnits: TStrings;
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
    property OnInclude: TIncludeEvent read FOnInclude write FOnInclude;
    property SystemTable: TSymbolTable read FSystemTable write FSystemTable;
    property Units: TStrings read FUnits write FUnits;

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

   TAddArgFunction = function (ArgExpr: TNoPosExpr) : TSymbol of object;

   TSpecialKeywordKind = (skNone, skAssigned, skHigh, skLength, skLow,
                          skOrd, skSizeOf, skDefined, skDeclared, skSqr);

   TSwitchInstruction = (siNone,
                         siIncludeLong, siIncludeShort,
                         siFilterLong, siFilterShort,
                         siDefine, siUndef,
                         siIfDef, siIfNDef, siIf, siEndIf, siElse,
                         siHint, siWarning, siError, siFatal );

   TLoopExitable = (leNotExitable, leBreak, leExit);

   // TdwsCompiler
   //
   TdwsCompiler = class
   private
      FCompilerOptions : TCompilerOptions;
      FMsgs : TdwsMessageList;
      FProg : TdwsProgram;
      FTok : TTokenizer;
      FBinaryOperators : TBinaryOperators;
      FLoopExprs : TSimpleStack<TNoResultExpr>;
      FLoopExitable : TSimpleStack<TLoopExitable>;
      FConditionalDepth : TSimpleStack<TSwitchInstruction>;

      FConnectors : TStrings;
      FCompileFileSystem : IdwsFileSystem;
      FOnInclude : TIncludeEvent;
      FScriptPaths : TStrings;
      FFilter : TdwsFilter;
      FIsExcept : Boolean;
      FIsSwitch : Boolean;
      FLineCount : Integer;

      FOnReadInstr : TCompilerReadInstrEvent;

      function Optimize : Boolean;

      function CheckFuncParams(ParamsA, ParamsB: TSymbolTable; IndexSym: TSymbol = nil;
                               TypSym: TSymbol = nil): Boolean;
      procedure CheckName(const Name: string);
      function IdentifySpecialName(const name: string) : TSpecialKeywordKind;
      procedure CheckSpecialName(const name: string);
      function CheckParams(A, B: TSymbolTable; CheckNames: Boolean): Boolean;
      procedure CompareFuncSymbols(A, B: TFuncSymbol; IsCheckingParameters: Boolean);

      function OpenStreamForFile(const scriptName : String) : TStream;
      function GetScriptSource(const scriptName : String) : String;

      function GetVarExpr(dataSym: TDataSymbol): TVarExpr;

      function GetLazyParamExpr(dataSym: TLazyParamSymbol): TLazyParamExpr;
      function GetVarParamExpr(dataSym: TVarParamSymbol): TVarParamExpr;
      function GetConstParamExpr(dataSym: TConstParamSymbol): TVarParamExpr;

      function ReadAssign(token : TTokenType; Left: TDataExpr): TNoResultExpr;
      function ReadArray(const TypeName: string): TTypeSymbol;
      function ReadArrayConstant: TArrayConstantExpr;
      function ReadCase: TCaseExpr;
      function ReadCaseConditions(condList : TList; valueExpr : TNoPosExpr) : Integer;
      function ReadClassOf(const TypeName: string): TClassOfSymbol;
      function ReadClass(const TypeName: string): TClassSymbol;
      procedure ReadClassFields(const classSymbol : TClassSymbol);
      function ReadConnectorSym(const Name: string; var BaseExpr: TNoPosExpr;
                                const ConnectorType: IConnectorType; IsWrite: Boolean): TNoPosExpr;
      function ReadConnectorArray(const Name: String; var BaseExpr: TNoPosExpr;
                                  const ConnectorType: IConnectorType; IsWrite: Boolean): TConnectorCallExpr;
      procedure ReadConstDecl;
      function ReadConstValue: TConstExpr;
      function ReadBlock: TNoResultExpr;
      function ReadBlocks(const endTokens: TTokenTypes; var finalToken: TTokenType): TNoResultExpr;
      function ReadEnumeration(const TypeName: string): TEnumerationSymbol;
      function ReadExcept(TryExpr: TExpr): TExceptExpr;
      function ReadExit: TNoResultExpr;
      function ReadExpr: TNoPosExpr;
      function ReadExprAdd: TNoPosExpr;
      function ReadExprMult: TNoPosExpr;
      function ReadExprIn(var left : TNoPosExpr) : TInOpExpr;
      function ReadExternalVar(Sym: TExternalVarSymbol; IsWrite: Boolean): TFuncExpr;
      function ReadField(Expr: TDataExpr; Sym: TFieldSymbol): TNoPosExpr;
      function ReadFor: TForExpr;
      function ReadStaticMethod(methodSym: TMethodSymbol; IsWrite: Boolean): TFuncExpr;
      function ReadFunc(FuncSym: TFuncSymbol; IsWrite: Boolean; CodeExpr: TDataExpr = nil): TNoPosExpr;
      procedure ReadFuncArgs(const AddArgProc: TAddArgFunction; LDelim: TTokenType = ttBLEFT; RDelim: TTokenType = ttBRIGHT);
      function ReadIf: TIfExpr;
      function ReadInherited(IsWrite: Boolean): TNoPosExpr;
      function ReadInstr: TNoResultExpr;
      function ReadInstrSwitch(semiPending : Boolean): TNoResultExpr;
      function ReadExprSwitch : TNoPosExpr;
      function ReadUntilEndOrElseSwitch(allowElse : Boolean) : Boolean;
      function ReadMethodDecl(ClassSym: TClassSymbol; FuncKind: TFuncKind; IsClassMethod: Boolean): TMethodSymbol;
      function ReadMethodImpl(ClassSym: TClassSymbol; FuncKind: TFuncKind; IsClassMethod: Boolean): TMethodSymbol;
      procedure ReadDeprecated(funcSym : TFuncSymbol);
      procedure WarnDeprecated(funcSym : TFuncSymbol);
      function ReadName(IsWrite: Boolean = False): TNoPosExpr;
      function ReadNameInherited(IsWrite: Boolean): TNoPosExpr;
      // Created overloaded ReadNameList to deal with script positions
      procedure ReadNameList(Names: TStrings); overload;
      procedure ReadNameList(Names: TStrings; out PosArray: TScriptPosArray); overload;
      procedure ReadArrayParams(ArrayIndices: TSymbolTable);
      // Don't want to add param symbols to dictionary when a method implementation (they get thrown away)
      procedure ReadParams(Proc: TFuncSymbol; ParamsToDictionary: Boolean=True);
      function ReadProcDecl(FuncKind: TFuncKind; ClassSym: TClassSymbol;
                            IsClassMethod: Boolean = False; IsType : Boolean = False): TFuncSymbol;
      procedure ReadProcBody(Proc: TFuncSymbol);
      function ReadClassOperatorDecl(ClassSym: TClassSymbol) : TClassOperatorSymbol;
      function ReadProperty(ClassSym: TClassSymbol): TPropertySymbol;
      function ReadPropertyExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol; IsWrite: Boolean): TNoPosExpr;
      function ReadPropertyReadExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol): TNoPosExpr;
      function ReadPropertyWriteExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol): TNoPosExpr;
      function ReadRecord(const TypeName: string): TTypeSymbol;
      function ReadRaise: TRaiseBaseExpr;
      function ReadRepeat: TRepeatExpr;
      function ReadRootStatement: TExpr;
      function ReadScript(const AName: string=''; ScriptType: TScriptSourceType=stMain): TBlockExpr;  // AName might be the name of an INCLUDEd script
      function ReadSpecialFunction(const NamePos: TScriptPos; SpecialKind: TSpecialKeywordKind): TNoPosExpr;
      function ReadStatement: TExpr;
      function ReadStringArray(Expr: TDataExpr; IsWrite: Boolean): TNoPosExpr;
      function ReadSwitch(const SwitchName: string): Boolean;
      function ReadSymbol(Expr: TNoPosExpr; IsWrite: Boolean = False): TNoPosExpr;
      function ReadTerm : TNoPosExpr;
      function ReadNegation : TNoPosExpr;
      function ReadTry: TExceptionExpr;
      function ReadType(const TypeName: string = ''): TTypeSymbol;
      function ReadTypeCast(const NamePos: TScriptPos; TypeSym: TSymbol): TNoPosExpr;
      procedure ReadTypeDecl;
      procedure ReadUses;
      function ReadVarDecl: TNoResultExpr;
      function ReadWhile: TWhileExpr;
      function ResolveUnitReferences(Units: TStrings): TInterfaceList;

   protected
      procedure EnterLoop(loopExpr : TNoResultExpr);
      procedure MarkLoopExitable(level : TLoopExitable);
      procedure LeaveLoop;

      function GetMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
                             const Pos: TScriptPos; IsInstruction: Boolean; ForceStatic : Boolean = False): TFuncExpr;

      function CreateProgram(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                             MaxDataSize, StackChunkSize: Integer;
                             MaxRecursionDepth : Integer): TdwsProgram; virtual;
      function CreateProcedure(Parent : TdwsProgram) : TProcedure; virtual;
      function CreateAssign(const pos : TScriptPos; token : TTokenType; left : TDataExpr; right : TNoPosExpr) : TNoResultExpr;

   public
      constructor Create;
      destructor Destroy; override;

      function Compile(const aCodeText : String; Conf: TdwsConfiguration): TdwsProgram;

      class function Evaluate(AContext: TdwsProgram; const AExpression: string): TNoPosExpr;

      procedure WarnForVarUsage(varExpr : TVarExpr; const pos : TScriptPos);

      property CurrentProg : TdwsProgram read FProg write FProg;
      property Tokenizer : TTokenizer read FTok write FTok;

      property OnReadInstr : TCompilerReadInstrEvent read FOnReadInstr write FOnReadInstr;
   end;

  TdwsDefaultResult = class(TdwsResult)
  private
    FTextBuilder: TWriteOnlyBlockStream;
    function GetText: String;
  public
    constructor Create(resultType : TdwsResultType); override;
    destructor Destroy; override;
    procedure AddString(const str : String); override;
    property Text: String read GetText;
  end;

  TdwsDefaultResultType = class(TdwsResultType)
  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
    function CreateProgResult: TdwsResult; override;
  end;

  TPrintFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TPrintLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
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
      SWI_INCLUDE_LONG, SWI_INCLUDE_SHORT, SWI_FILTER_LONG, SWI_FILTER_SHORT,
      SWI_DEFINE, SWI_UNDEF,
      SWI_IFDEF, SWI_IFNDEF, SWI_IF, SWI_ENDIF, SWI_ELSE,
      SWI_HINT, SWI_WARNING, SWI_ERROR, SWI_FATAL
      );
   cAssignmentTokens : TTokenTypes = [ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN,
                                      ttTIMES_ASSIGN, ttDIVIDE_ASSIGN];

type
  TExceptionCreateMethod = class(TInternalMethod)
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TDelphiExceptionCreateMethod = class(TInternalMethod)
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TParamFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TParamStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TParamCountFunc = class(TInternalFunction)
    procedure Execute; override;
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

// Create
//
constructor TdwsCompiler.Create;
begin
   inherited;
   FLoopExprs:=TSimpleStack<TNoResultExpr>.Create;
   FLoopExitable:=TSimpleStack<TLoopExitable>.Create;
   FConditionalDepth:=TSimpleStack<TSwitchInstruction>.Create;
end;

// Destroy
//
destructor TdwsCompiler.Destroy;
begin
   FConditionalDepth.Free;
   FLoopExitable.Free;
   FLoopExprs.Free;
   inherited;
end;

function TdwsCompiler.ResolveUnitReferences(Units: TStrings): TInterfaceList;
var
  x, y, z: Integer;
  deps: TStrings;
  refCount: array of Integer;
  changed: Boolean;
  unitName: string;
begin
  // initialize reference count vector
  SetLength(refCount, Units.Count);

  // Calculate number of outgoing references
  for x := 0 to Units.Count - 1 do
  begin
    deps := IUnit(Pointer(Units.Objects[x])).GetDependencies;
    for y := 0 to deps.Count - 1 do
    begin
      if Units.IndexOf(deps[y]) < 0 then
        FMsgs.AddCompilerStopFmt(cNullPos, CPE_UnitNotFound, [deps[y], Units[x]]);
    end;
    refCount[x] := deps.Count;
  end;

  Result := TInterfaceList.Create;
  try

    // Resolve references
    changed := True;
    while changed do
    begin
      changed := False;
      for x := 0 to Units.Count - 1 do
        // Find unit that is not referencing other units
        if refCount[x] = 0 then
        begin
          Result.Add(IUnit(Pointer(Units.Objects[x])));

          // Remove the references to this unit from all other units
          unitName := Units[x];
          for y := 0 to Units.Count - 1 do
          begin
            deps := IUnit(Pointer(Units.Objects[y])).GetDependencies;
            for z := 0 to deps.Count - 1 do
              if SameText(deps[z], unitName) then
                Dec(refCount[y]);
          end;

          refCount[x] := -1;
          changed := True;
        end;
    end;

    if Result.Count <> Units.Count then
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
      FProg.Msgs.AddCompilerWarning(FLoopExprs.Peek.Pos, CPW_InfiniteLoop);

   FLoopExprs.Pop;
   FLoopExitable.Pop;
end;

// GetMethodExpr
//
function TdwsCompiler.GetMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
             const Pos: TScriptPos; IsInstruction: Boolean; ForceStatic : Boolean = False): TFuncExpr;
begin
   WarnDeprecated(meth);
   Result:=CreateMethodExpr(meth, Expr, RefKind, Pos, IsInstruction, ForceStatic);
end;

// Compile
//
function TdwsCompiler.Compile(const aCodeText : String; Conf: TdwsConfiguration): TdwsProgram;
var
   x : Integer;
   stackChunkSize: Integer;
   maxDataSize: Integer;
   unitsResolved: TInterfaceList;
   unitsTable: TSymbolTable;
   unitTables: TList;
   unitTable: TSymbolTable;
   codeText : String;
   unitSymbol : TUnitSymbol;
begin
   FIsExcept := False;
   FFilter := Conf.Filter;
   FConnectors := Conf.Connectors;
   FCompilerOptions := Conf.CompilerOptions;
   FOnInclude := Conf.OnInclude;
   FScriptPaths := Conf.ScriptPaths;

   if Conf.CompileFileSystem<>nil then
      FCompileFileSystem := Conf.CompileFileSystem.AllocateFileSystem
   else FCompileFileSystem := TdwsOSFileSystem.Create;

   FLoopExprs.Clear;
   FLoopExitable.Clear;
   FConditionalDepth.Clear;

   maxDataSize := Conf.MaxDataSize;
   if maxDataSize = 0 then
     maxDataSize := MaxInt;

   StackChunkSize := Conf.StackChunkSize;
   if StackChunkSize <= 0 then
     StackChunkSize := 1;

   FLineCount:=0;

   // Create the TdwsProgram
   FProg := CreateProgram(Conf.SystemTable, Conf.ResultType, maxDataSize, stackChunkSize, Conf.MaxRecursionDepth);
   Result := FProg;
   FMsgs := FProg.Msgs;
   FProg.Compiler := Self;
   FProg.TimeoutMilliseconds := Conf.TimeoutMilliseconds;
   FProg.RuntimeFileSystem := Conf.RuntimeFileSystem;
   FProg.ConditionalDefines:=conf.Conditionals;

   FBinaryOperators:=TBinaryOperators.Create(FProg.Table);
   try
      // Check for missing units
      if Assigned(FFilter) then begin
         for x := 0 to FFilter.Dependencies.Count - 1 do begin
            if Conf.Units.IndexOf(FFilter.Dependencies[x]) = -1 then
               FMsgs.AddCompilerErrorFmt(cNullPos, CPE_FilterDependsOnUnit, [FFilter.ClassName, FFilter.Dependencies[x]]);
         end;
      end;

      // Handle unit dependencies
      unitsResolved := ResolveUnitReferences(Conf.Units);
      try
         unitTables := TList.Create;
         unitsTable := TSymbolTable.Create;
         try
            try
               // Get the symboltables of the units
               for x := 0 to unitsResolved.Count - 1 do begin
                  unitTable := IUnit(unitsResolved[x]).GetUnitTable(Conf.SystemTable, unitsTable);
                  unitTables.Add(unitTable);
                  unitsTable.AddSymbol(TUnitSymbol.Create(IUnit(unitsResolved[x]).GetUnitName, unitTable));
               end;
            except
               on e: Exception do begin
                  for x:=0 to unitTables.Count-1 do
                     TObject(unitTables[x]).Free;
                  raise;
               end;
            end;

            // Add the units to the program-symboltable
            for x := 0 to unitsTable.Count - 1 do begin
               unitSymbol:=TUnitSymbol(unitsTable[x]);

               FProg.Table.AddSymbol(TUnitSymbol.Create( unitSymbol.Name, unitSymbol.Table, True));
               FProg.Table.AddParent(unitSymbol.Table);
            end;

            unitSymbol:=FProg.Table.FindSymbol(SYS_INTERNAL) as TUnitSymbol;
            FProg.Table.AddSymbol(TUnitSymbol.Create( SYS_SYSTEM, unitSymbol.Table, False ));

         finally
            unitsTable.Free;
            unitTables.Free;
         end;

      finally
         unitsResolved.Free;
      end;

      // Filter stuff
      if Assigned(FFilter) then
         codeText := FFilter.Process(aCodeText, FMsgs)
      else codeText := aCodeText;

      // Initialize tokenizer
      FTok := TTokenizer.Create(codeText, MSG_MainModule, FProg.Msgs);
      try
         FTok.SwitchHandler := ReadSwitch;

         // Start compilation
         FProg.Expr := ReadScript('', stMain);

         if FConditionalDepth.Count>0 then
            FProg.Msgs.AddCompilerError(FTok.HotPos, CPE_UnbalancedConditionalDirective);

         // Initialize symbol table
         FProg.Table.Initialize(FMsgs);

         // Initialize the expressions
         FProg.Expr.Initialize;

         // Every thing is done, set program state to "prepared"
         FProg.ReadyToRun;
      finally
         Inc(FLineCount, FTok.CurrentPos.Line-2);
         FTok.Free;
      end;
   except
      on e: ECompileError do
         ;
      on e: Exception do
         FMsgs.AddCompilerError(cNullPos, e.Message);
   end;

   FreeAndNil(FBinaryOperators);

   FCompileFileSystem := nil;

   FProg.LineCount:=FLineCount;
   FProg.Compiler:=nil;
end;

// Optimize
//
function TdwsCompiler.Optimize : Boolean;
begin
   Result:=(coOptimize in FCompilerOptions) and (not FMsgs.HasErrors);
end;

// ReadScript
//
function TdwsCompiler.ReadScript(const AName: string; ScriptType: TScriptSourceType): TBlockExpr;
var
   stmt : TExpr;
begin
   Result := TBlockExpr.Create(FProg, FTok.DefaultPos);
   try
      FProg.SourceList.Add(AName, FTok.HotPos.SourceFile, ScriptType);
      while FTok.HasTokens do begin
         Stmt := ReadRootStatement;
         if Assigned(Stmt) then
            TBlockExpr(Result).AddStatement(Stmt);

         if not FTok.TestDelete(ttSEMI) then begin
            while FTok.HasTokens and FTok.Test(ttSWITCH) do begin
               ReadInstrSwitch(True);
               FTok.KillToken;
            end;
            if FTok.HasTokens then
               FMsgs.AddCompilerStop(FTok.CurrentPos, CPE_SemiExpected);
         end;
      end;
  except
      Result.Free;
      raise;
  end;
end;

// ReadRootStatement
//
function TdwsCompiler.ReadRootStatement: TExpr;
var
   token : TTokenType;
begin
   Result:=nil;
   token:=FTok.TestDeleteAny([ttTYPE, ttPROCEDURE, ttFUNCTION,
                              ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttCLASS]);
   case token of
      ttTYPE :
         ReadTypeDecl;
      ttPROCEDURE :
         ReadProcBody(ReadProcDecl(fkProcedure, nil));
      ttFUNCTION :
         ReadProcBody(ReadProcDecl(fkFunction, nil));
      ttCONSTRUCTOR :
         ReadProcBody(ReadProcDecl(fkConstructor, nil));
      ttDESTRUCTOR :
         ReadProcBody(ReadProcDecl(fkDestructor, nil));
      ttMETHOD :
         ReadProcBody(ReadProcDecl(fkMethod, nil));
      ttCLASS : begin
         token:=FTok.TestDeleteAny([ttPROCEDURE, ttFUNCTION, ttMETHOD]);
         case token of
            ttPROCEDURE :
               ReadProcBody(ReadProcDecl(fkProcedure, nil, True));
            ttFUNCTION :
               ReadProcBody(ReadProcDecl(fkFunction, nil, True));
            ttMETHOD :
               ReadProcBody(ReadProcDecl(fkMethod, nil, True));
         else
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
         end;
      end;
   else
      Result:=ReadStatement;
   end;
end;

// ReadStatement
//
function TdwsCompiler.ReadStatement: TExpr;
var
   token : TTokenType;
begin
   Result:=nil;
   token:=Ftok.TestDeleteAny([ttVAR, ttCONST, ttUSES]);
   case token of
      ttVAR :
         Result:=ReadVarDecl;
      ttCONST :
         ReadConstDecl;
      ttUSES :
         ReadUses
   else
      Result:=ReadBlock;
   end;
end;

class function TdwsCompiler.Evaluate(AContext: TdwsProgram; const AExpression: string): TNoPosExpr;
var
  OldProgMsgs: TdwsMessageList;
begin
  { This will evaluate an expression by tokenizing it evaluating it in the
    Context provided. }

  Result := nil;

  with Self.Create do
    try
      FProg := AContext;
      try
        OldProgMsgs := FProg.Msgs;

        FMsgs := TdwsMessageList.Create;
        FProg.Msgs := FMsgs;
        try
          FTok := TTokenizer.Create(AExpression, MSG_MainModule, FMsgs);
          try
            try
              Result := ReadExpr;
              try
                Result.Initialize;
              except
                FreeAndNil(Result);
                raise;
              end;
            except
              on E: EScriptError do
              begin
                if FMsgs.Count > 0 then
                begin
                  E.Message := FMsgs[0].AsInfo;
                  raise;    // change the message and re-raise the EScriptError exception
                end;
              end;
            end;
          finally
            FreeAndNil(FTok);
          end;
        finally
          FProg.Msgs := OldProgMsgs;
          FreeAndNil(FMsgs);
        end;
      finally
        FProg := nil;
      end;
    finally
      Free;
    end;
end;

// ReadVarDecl
//
function TdwsCompiler.ReadVarDecl : TNoResultExpr;
var
   x : Integer;
   names : TStringList;
   sym : TDataSymbol;
   typ : TSymbol;
   pos : TScriptPos;
   posArray : TScriptPosArray;
   initData : TData;
   initExpr : TNoPosExpr;
   assignExpr : TAssignExpr;
   constExpr : TConstExpr;
   varExpr : TVarExpr;
begin
   Result := nil;

   names := TStringList.Create;
   initExpr := nil;
   try
      // Conditionally pass in dynamic array
      if coSymbolDictionary in FCompilerOptions then
         ReadNameList(names, posArray)     // use overloaded version
      else ReadNameList(names);

      pos := FTok.HotPos;

      if FTok.TestDelete(ttCOLON) then begin

         // explicit typing
         //    var myVar : type
         //    var myVar : type = expr
         //    var myVar : type := expr
         typ := ReadType('');
         if names.Count = 1 then begin
            if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then
               initExpr := ReadExpr
         end;

      end else if FTok.TestDelete(ttEQ) or FTok.TestDelete(ttASSIGN) then begin

         // inferred typing
         //    var myVar = expr
         //    var myVar := expr
         if names.Count <> 1 then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);
         initExpr := ReadExpr;
         typ := initExpr.Typ;

      end else begin

         typ := nil;
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      end;

      for x := 0 to names.Count - 1 do begin
         CheckName(names[x]);
         sym := TDataSymbol.Create(names[x], typ);
         FProg.Table.AddSymbol(sym);
         if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(sym, posArray[x], [suDeclaration]);   // entry for variable

         varExpr:=GetVarExpr(sym);
         if Assigned(initExpr) then begin

            // Initialize with an expression
            Result:=CreateAssign(pos, ttASSIGN, varExpr, initExpr);
            initExpr:=nil;

         end else begin

            if sym.Typ is TArraySymbol then begin

               // TODO: if Sym.DynamicInit?
               FProg.InitExpr.AddStatement(
                  TInitDataExpr.Create(FProg, Pos, varExpr));

            end else begin

               // Initialize with default value
               if varExpr.Typ=FProg.TypInteger then
                  assignExpr:=TAssignConstToIntegerVarExpr.CreateVal(FProg, pos, varExpr, 0)
               else if varExpr.Typ=FProg.TypFloat then
                  assignExpr:=TAssignConstToFloatVarExpr.CreateVal(FProg, pos, varExpr, 0)
               else if varExpr.Typ=FProg.TypString then
                  assignExpr:=TAssignConstToStringVarExpr.CreateVal(FProg, pos, varExpr, '')
               else begin
                  initData := nil;
                  SetLength(initData, sym.Typ.Size);
                  TDataSymbol(sym).initData(initData, 0);

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
procedure TdwsCompiler.ReadConstDecl;
var
   name : String;
   expr: TNoPosExpr;
   typ : TSymbol;
   constPos : TScriptPos;
   sym : TSymbol;
begin
   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected)
   else begin
      name:=FTok.GetToken.FString;
      constPos := FTok.HotPos;
      FTok.KillToken;

      CheckName(name);

      if FTok.TestDelete(ttCOLON) then
         typ := ReadType('')
      else typ := nil;

      if typ is TFuncSymbol then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InvalidConstType, [typ.Caption]);

      if not FTok.TestDelete(ttEQ) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

      expr:=ReadExpr;
      try
         expr.TypeCheckNoPos(FTok.HotPos);
         if not expr.IsConstant then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);

         if Assigned(typ) then begin
            if not typ.IsCompatible(expr.typ) then
               FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_AssignIncompatibleTypes,
                                        [expr.typ.Caption, typ.Caption]);
         end else typ := expr.typ;

         if typ is TArraySymbol then begin
            sym := TStaticArraySymbol.Create('', typ, 0, TArraySymbol(typ).typ.Size-1);
            FProg.Table.AddSymbol(sym);
            sym := TConstSymbol.Create(name, sym, (expr as TArrayConstantExpr).EvalAsTData, 0);
         end else if typ.Size>1 then
            sym := TConstSymbol.Create(name, typ, TConstExpr(expr).Data, TConstExpr(expr).Addr)
         else sym := TConstSymbol.Create(name, typ, expr.Eval);
         FProg.Table.AddSymbol(sym);
         if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(sym, constPos, [suDeclaration]);
      finally
         expr.Free;
      end;
   end;
end;

procedure TdwsCompiler.ReadTypeDecl;
var
  Name: string;
  typNew, typOld: TSymbol;
  typePos: TScriptPos;
  oldSymPos: TSymbolPosition; // Mark *where* the old declaration was
begin
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected)
  else
  begin
    Name := FTok.GetToken.FString;
    typePos := FTok.HotPos;
    FTok.KillToken;

    if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

    typOld := FProg.Table.FindSymbol(Name);
    oldSymPos := nil;
    if coSymbolDictionary in FCompilerOptions then
    begin
      if Assigned(typOld) then
        oldSymPos := FProg.SymbolDictionary.FindSymbolUsage(typOld, suDeclaration);  // may be nil
    end;

    typNew := ReadType(Name);

    // Wrap whole type declarations in a context.
    if coContextMap in FCompilerOptions then
      FProg.ContextMap.OpenContext(typePos, typNew);

    try
      try
        // typOld = typNew if a forwarded class declaration was overwritten
        if typOld <> typNew then
        begin
          CheckName(Name);
          FProg.Table.AddSymbol(typNew);
        end
        // Handle overwriting forwards in Dictionary
        // Original symbol was a forward. Update symbol entry
        else
        begin
          // If the type is in the SymbolDictionary (disabled dictionary would leave pointer nil),
          if Assigned(oldSymPos) then              // update original position information
            oldSymPos.SymbolUsages := [suForward]; // update old postion to reflect that the type was forwarded
        end;

        // Add symbol position as being the type being declared (works for forwards too)
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(typNew, typePos, [suDeclaration]);
      except
        typNew.Free;
        raise;
      end;
    finally
      if coContextMap in FCompilerOptions then
        FProg.ContextMap.CloseContext(FTok.CurrentPos);
    end;
  end;
end;

// ReadProcDecl
//
function TdwsCompiler.ReadProcDecl(FuncKind: TFuncKind; ClassSym: TClassSymbol;
                                   IsClassMethod: Boolean; IsType : Boolean): TFuncSymbol;
var
   Name: string;
   sym: TSymbol;
   funcPos: TScriptPos;
   forwardedSym: TFuncSymbol;
   forwardedSymPos: TSymbolPosition;
   methSym: TMethodSymbol;
   i: Integer;
begin
   if not IsType then begin
      // Find Symbol for Functionname
      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      Name := FTok.GetToken.FString;
      CheckSpecialName(Name);
      funcPos := FTok.HotPos;
      FTok.KillToken;

      sym := FProg.Table.FindSymbol(Name);

      // Open context for procedure declaration. Closed in ReadProcBody.
      if coContextMap in FCompilerOptions then
         FProg.ContextMap.OpenContext(funcPos, sym);
   end else begin
      sym := nil;
      Name := '';
   end;

   // Name is the name of class -> Method
   if sym is TClassSymbol then begin
      // Store reference to class in dictionary
      if coSymbolDictionary in FCompilerOptions then
         FProg.SymbolDictionary.Add(sym, funcPos);
      Result := ReadMethodImpl(TClassSymbol(sym), FuncKind, IsClassMethod);
   end else begin
      // Read normal procedure/function declaration
      if IsClassMethod or (FuncKind in [fkConstructor, fkDestructor, fkMethod]) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ImplClassNameExpected);

      if (sym is TFuncSymbol) and TFuncSymbol(sym).IsForwarded then
         // There was already a (forward) declaration
         forwardedSym := TFuncSymbol(sym)
      else forwardedSym := nil;

      if not Assigned(forwardedSym) then
         CheckName(Name);

      if IsType then
         Result := TSourceFuncSymbol.Create('', FuncKind, -1)
      else Result := TSourceFuncSymbol.Create(Name, FuncKind, FProg.Stack.NextLevel(FProg.Level));
      try
         ReadParams(Result, forwardedSym=nil);  // Don't add params to dictionary when function is forwarded. It is already declared.

         if FuncKind in [fkFunction, fkMethod] then begin
            if FTok.TestDelete(ttCOLON) then
               Result.Typ := ReadType('')
            else if FuncKind = fkFunction then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionTypeExpected);
         end;

         if not IsType then begin
            if Assigned(forwardedSym) then
               CompareFuncSymbols(forwardedSym, Result, True);

            // forward declarations
            if not Assigned(forwardedSym) then begin
               if FTok.Test(ttSEMI) then begin
                  FTok.KillToken; // SEMI
                  if FTok.Test(ttFORWARD) then begin
                     Result.SetForwardedPos(funcPos);
                     FTok.TestDelete(ttFORWARD);
                  end;
               end;
            end else if not FTok.TestDelete(ttSEMI) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

            ReadDeprecated(Result);

            if Assigned(forwardedSym) then begin
               // Get forwarded position in script. If compiled without symbols it will just return from empty list (could optimize here to prevent the push/pop of call stack
               forwardedSymPos := FProg.SymbolDictionary.FindSymbolUsage(forwardedSym, suDeclaration);  // may be nil
               // Adapt dictionary entry to reflect that it was a forward
               // If the record is in the SymbolDictionary (disabled dictionary would leave pointer nil)
               if Assigned(forwardedSymPos) then
                  forwardedSymPos.SymbolUsages := [suForward];  // update old postion to reflect that the type was forwarded

               Result.Free;
               Result := forwardedSym;
               Result.ClearIsForwarded;
            end else FProg.Table.AddSymbol(Result);
         end else if FTok.TestDelete(ttOF) then begin
            if not FTok.TestDelete(ttOBJECT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectExpected);
            methSym := TSourceMethodSymbol.Create('',FuncKind, FProg.TypObject,-1);
            methSym.Typ := Result.Typ;
            for i := 0 to Result.Params.Count - 1 do
               methSym.Params.AddSymbol(Result.Params[i]);
            Result.Params.Clear;
            Result.Free;
            Result := methSym;
         end;

         // Procedure is both Declared and Implemented here
         if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(Result, funcPos, [suDeclaration, suImplementation]);
      except
         // Remove reference to symbol (gets freed)
         if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Remove(Result);
         Result.Free;
         raise;
      end;
   end;
end;

function TdwsCompiler.ReadMethodDecl(ClassSym: TClassSymbol; FuncKind: TFuncKind;
  IsClassMethod: Boolean): TMethodSymbol;

   function ParamsCheck(newMeth, oldMeth: TMethodSymbol): Boolean;
   var
      x: Integer;
   begin
      Result := False;
      if newMeth.Params.Count = oldMeth.Params.Count then begin
         for x := 0 to newMeth.Params.Count - 1 do
            if not newMeth.Params[x].Typ.IsCompatible(oldMeth.Params[x].Typ) then
               exit;
         Result := True;
      end;
   end;

var
   Name: string;
   meth: TSymbol;
   IsReintroduced: Boolean;
   methPos: TScriptPos;
   qualifier : TTokenType;
begin
   // Find Symbol for Functionname
   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
   Name := FTok.GetToken.FString;
   FTok.KillToken;

   methPos := FTok.HotPos;

   // Check if name is already used
   meth := ClassSym.Members.FindSymbol(Name);
   if meth is TFieldSymbol then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_FieldRedefined, [Name])
   else if meth is TPropertySymbol then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_PropertyRedefined, [Name])
   else if meth is TMethodSymbol then begin
      if TMethodSymbol(meth).ClassSymbol = ClassSym then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_MethodRedefined, [Name]);
   end;

   // Read declaration of method implementation
   if IsClassMethod then
      Result := TSourceMethodSymbol.Create(Name, FuncKind, ClassSym.ClassOf)
   else Result := TSourceMethodSymbol.Create(Name, FuncKind, ClassSym);
   Result.DeclarationPos:=methPos;

   try
      if meth is TMethodSymbol then begin
         Result.SetOverlap(TMethodSymbol(meth));
         IsReintroduced := TMethodSymbol(meth).IsVirtual;
      end else IsReintroduced := False;

      ReadParams(Result);

      if FuncKind in [fkFunction, fkMethod] then begin
         if FTok.TestDelete(ttCOLON) then
            Result.Typ := ReadType('')
         else if FuncKind = fkFunction then
            FMsgs.AddCompilerStop(methPos, CPE_FunctionTypeExpected);
      end;
      if not FTok.TestDelete(ttSEMI) then
         FMsgs.AddCompilerStop(methPos, CPE_SemiExpected);

      qualifier:=FTok.TestDeleteAny([ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttABSTRACT]);
      if qualifier<>ttNone then begin
         case qualifier of
            ttVIRTUAL : begin
               TMethodSymbol(Result).IsVirtual := True;
               if FTok.Test(ttSEMI) and FTok.NextTest(ttABSTRACT) then begin
                  FTok.KillToken;
                  FTok.TestDelete(ttABSTRACT);
                  TMethodSymbol(Result).IsAbstract := True;
               end;
            end;
            ttOVERRIDE : begin
               if not Assigned(meth) or not (meth is TMethodSymbol) then
                  FMsgs.AddCompilerStopFmt(methPos, CPE_CantOverrideNotInherited, [Name])
               else if not TMethodSymbol(meth).IsVirtual then
                  FMsgs.AddCompilerStopFmt(methPos, CPE_CantOverrideNotVirtual, [Name])
               else begin
                  if not ParamsCheck(TMethodSymbol(Result), TMethodSymbol(meth)) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_CantOverrideWrongParameterList);
                  TMethodSymbol(Result).SetOverride(TMethodSymbol(meth));
                  IsReintroduced := False;
               end;
            end;
            ttREINTRODUCE : begin
               if not IsReintroduced then
                  FMsgs.AddCompilerStopFmt(methPos, CPE_CantReintroduce, [Name]);
               IsReintroduced := False;
            end;
            ttABSTRACT : begin
               FMsgs.AddCompilerError(FTok.HotPos, CPE_NonVirtualAbstract);
            end;
         end;

         if not FTok.TestDelete(ttSEMI) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
      end;
      ReadDeprecated(Result);

      if IsReintroduced then
         FMsgs.AddCompilerWarningFmt(methPos, CPE_ReintroduceWarning, [Name]);

      // Added as last step. OnExcept, won't need to be freed.
      if coSymbolDictionary in FCompilerOptions then
         FProg.SymbolDictionary.Add(Result, methPos, [suDeclaration]);
   except
      Result.Free;
      raise;
   end;
end;

function TdwsCompiler.ReadMethodImpl(ClassSym: TClassSymbol;
  FuncKind: TFuncKind; IsClassMethod: Boolean): TMethodSymbol;
var
  methName: string;
  meth: TSymbol;
  methPos: TScriptPos;
begin
  if not (FTok.TestDelete(ttDOT) and FTok.TestName) then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

  methName := FTok.GetToken.FString;
  methPos := FTok.HotPos;
  FTok.KillToken;
  FTok.Test(ttBLEFT);

  meth := ClassSym.Members.FindSymbol(methName);

  if not (meth is TMethodSymbol) then
    FMsgs.AddCompilerStop(methPos, CPE_ImplNotAMethod);

  if TMethodSymbol(meth).ClassSymbol <> ClassSym then
    FMsgs.AddCompilerStopFmt(methPos, CPE_ImplInvalidClass, [methName, ClassSym.Name]);

  if TMethodSymbol(meth).IsAbstract then
    FMsgs.AddCompilerErrorFmt(methPos, CPE_ImplAbstract, [ClassSym.Name, methName]);

  if TMethodSymbol(meth).IsClassMethod and not IsClassMethod then
    FMsgs.AddCompilerStop(methPos, CPE_ImplClassExpected)
  else if not TMethodSymbol(meth).IsClassMethod and IsClassMethod then
    FMsgs.AddCompilerStop(methPos, CPE_ImplNotClassExpected);

  Result := TSourceMethodSymbol.Create(methName, FuncKind, ClassSym);
  try
    if not FTok.TestDelete(ttSEMI) then
    begin
      ReadParams(Result, False);  // Don't store these params to Dictionary. They will become invalid when the method is freed.

      if FuncKind in [fkFunction, fkMethod] then begin
        if FTok.TestDelete(ttCOLON) then
          Result.Typ := ReadType('')
        else if FuncKind = fkFunction then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionTypeExpected);
      end;

      if not FTok.TestDelete(ttSEMI) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

      CompareFuncSymbols(TMethodSymbol(meth), Result, True);
    end
    else
      CompareFuncSymbols(TMethodSymbol(meth), Result, False);
  finally
    Result.Free;
    Result := TMethodSymbol(meth);
    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(Result, methPos, [suImplementation]);
  end;
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
      if not FTok.TestDelete(ttSEMI) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
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
procedure TdwsCompiler.ReadProcBody(Proc: TFuncSymbol);
var
   oldprog: TdwsProgram;
   stmt: TExpr;
   assignExpr : TNoResultExpr;
   sectionType : TTokenType;
begin
   // Stop if declaration was forwarded or external
   if (Proc.IsForwarded) then begin
      // Closed context of procedure (was only a forward)
      if coContextMap in FCompilerOptions then
         FProg.ContextMap.CloseContext(FTok.HotPos);
      Exit;
   end;

   if Proc.Executable<>nil then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_MethodRedefined, [Proc.Name]);

   // Open context of full procedure body (may include a 'var' section)
   if coContextMap in FCompilerOptions then
      FProg.ContextMap.OpenContext(FTok.CurrentPos, Proc);   // attach to symbol that it belongs to (perhaps a class)

   Proc.SourcePosition:=FTok.HotPos;

   try
      // Funktion Body
      oldprog := FProg;
      FProg := CreateProcedure(FProg);
      try
         FProg.Compiler := Self;
         TProcedure(FProg).AssignTo(Proc);
         // Set the current context's LocalTable to be the table of the new procedure
         if coContextMap in FCompilerOptions then
            FProg.ContextMap.Current.LocalTable := FProg.Table;

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
               end else if sectionType=ttCONST then
                  ReadConstDecl;

               if not FTok.TestDelete(ttSEMI) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

            until FTok.Test(ttBEGIN);
         end;

         if coContextMap in FCompilerOptions then
            FProg.ContextMap.OpenContext(FTok.CurrentPos, nil);
         try
            // Read procedure body
            if not FTok.TestDelete(ttBEGIN) then begin
               if FTok.Test(ttFORWARD) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_FuncForwardAlreadyExists)
               else FMsgs.AddCompilerStop(FTok.HotPos, CPE_BeginExpected);
            end;

            // Read Statements enclosed in "begin" and "end"
            FProg.Expr := TBlockExpr.Create(FProg, FTok.HotPos);
            while not FTok.TestDelete(ttEND) do begin
               stmt := ReadRootStatement;
               if Assigned(stmt) then
                  TBlockExpr(FProg.Expr).AddStatement(Stmt);
               if not FTok.TestDelete(ttSEMI) then begin
                  if not FTok.Test(ttEND) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
               end;
            end;
         finally
            if coContextMap in FCompilerOptions then
               FProg.ContextMap.CloseContext(FTok.CurrentPos);  // close with inside procedure end
         end;
      finally
         FProg.Compiler := nil;
         FProg := oldprog;
      end;
   finally
      // Closed procedure body and procedure implementation (from declaration to body)
      if coContextMap in FCompilerOptions then begin
         FProg.ContextMap.CloseContext(FTok.CurrentPos);  // closed begin..end body (may include 'var' section)
         FProg.ContextMap.CloseContext(FTok.CurrentPos);  // closed from declaration through implementation
      end;
   end;
end;

// ReadBlocks
//
function TdwsCompiler.ReadBlocks(const endTokens: TTokenTypes; var finalToken: TTokenType): TNoResultExpr;
var
   stmt : TExpr;
   oldTable : TSymbolTable;
   x : Integer;
   token : TToken;
   closePos : TScriptPos; // Position at which the ending token was found (for context)
   blockExpr : TBlockExpr;
begin
   // Read a block of instructions enclosed in "begin" and "end"
   blockExpr:=TBlockExpr.Create(FProg, FTok.HotPos);
   try
      if coContextMap in FCompilerOptions then begin
         FProg.ContextMap.OpenContext(FTok.CurrentPos, nil);
         closePos:=FTok.CurrentPos;     // default to close context where it opened (used on errors)
      end;

      oldTable:=FProg.Table;
      FProg.Table:=blockExpr.Table;
      try
         // Add local table to context for the new block
         if coContextMap in FCompilerOptions then
            FProg.ContextMap.Current.LocalTable:=FProg.Table;

         while True do begin

            if not FTok.HasTokens then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndOfBlockExpected);

            if FTok.GetToken.FTyp in EndTokens then begin
               finalToken:=FTok.GetToken.FTyp;
               closePos:=FTok.GetToken.FPos;    // get start position of ending token
               FTok.KillToken;
               Break;
            end;

            stmt := ReadStatement;
            if Assigned(stmt) then
               blockExpr.AddStatement(stmt);

            if not FTok.TestDelete(ttSEMI) then begin
               token:=FTok.GetToken;
               if (token=nil) or (not (token.FTyp in EndTokens)) then
                  FMsgs.AddCompilerStop(FTok.CurrentPos, CPE_SemiExpected);
            end;

         end;

      finally
         FProg.Table:=oldTable;
         if coContextMap in FCompilerOptions then
            FProg.ContextMap.CloseContext(closePos);   // get to end of block
      end;

      if Optimize then
         Result:=blockExpr.OptimizeToNoResultExpr
      else Result:=blockExpr;

   except
      // Remove any symbols in the expression's table. Table will be freed.
      if coSymbolDictionary in FCompilerOptions then
         for x:=0 to blockExpr.Table.Count - 1 do
            FProg.SymbolDictionary.Remove(blockExpr.Table[x]);
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

function TdwsCompiler.ReadInstr: TNoResultExpr;
var
   token : TTokenType;
   locExpr : TNoPosExpr;
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
            FMsgs.AddCompilerError(FTok.HotPos, CPE_BreakOutsideOfLoop);
         Result := TBreakExpr.Create(FProg, FTok.HotPos);
         MarkLoopExitable(leBreak);
      end;
      ttEXIT : begin
         Result := ReadExit;
         MarkLoopExitable(leExit);
      end;
      ttTRY :
         Result := ReadTry;
      ttCONTINUE : begin
         if FLoopExprs.Count=0 then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_ContinueOutsideOfLoop);
         Result := TContinueExpr.Create(FProg, FTok.HotPos);
      end;
      ttRAISE :
         Result := ReadRaise;
   else
      // Try to read a function call, method call or an assignment
      if FTok.Test(ttSWITCH) then
         Result := ReadInstrSwitch(False)
      else if FTok.Test(ttBLEFT) or FTok.Test(ttINHERITED) or FTok.TestName then begin // !! TestName must be the last !!
         hotPos:=FTok.HotPos;
         if FTok.Test(ttBLEFT) then // (X as TY)
            locExpr := ReadSymbol(ReadTerm)
         else locExpr := ReadName(True);
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
            end else if locExpr is TConnectorWriteExpr then
               Result:=TConnectorWriteExpr(locExpr)
            else if locExpr is TStringArraySetExpr then
               Result:=TStringArraySetExpr(locExpr)
            else if locExpr is TConstExpr then begin
               Result:=TNullExpr.Create(FProg, hotPos);
               FMsgs.AddCompilerHint(hotPos, CPE_ConstantInstruction);
            end else if locExpr is TNullExpr then begin
               Result:=TNullExpr(locExpr);
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

   if Assigned(Result) then begin
      try
         if Result is TExpr then
            TExpr(Result).TypeCheck
         else Result.TypeCheckNoPos(FTok.HotPos);
      except
         Result.Free;
         raise;
      end;
   end;
end;

function TdwsCompiler.ReadInherited(IsWrite: Boolean): TNoPosExpr;
var
  name: string;
  sym: TSymbol;
  methSym: TMethodSymbol;
  classSym, parentSym: TClassSymbol;
  varExpr: TDataExpr;
begin
  Result := nil;
  if not ((FProg is TProcedure) and (TProcedure(FProg).Func is TMethodSymbol)) then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedOnlyInMethodsAllowed);

  methSym := TMethodSymbol(TProcedure(FProg).Func);
  classSym := methSym.ClassSymbol;
  parentSym := ClassSym.Parent;
  sym := nil;

  if FTok.TestName then
  begin
    name := FTok.GetToken.FString;
    FTok.KillToken;

    sym := ParentSym.Members.FindSymbol(name);
  end
  else if not methSym.IsOverride then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName)
  else
    sym := methSym.ParentMeth;

  if Assigned(sym) then
  begin
    if sym is TMethodSymbol then
    begin
      if methSym.IsClassMethod then
        varExpr := TConstExpr.CreateTyped(FProg, parentSym.ClassOf, parentSym.Name)
      else
        varExpr := TVarExpr.CreateTyped(FProg, parentSym, methSym.SelfSym);
      try
        if methSym.IsClassMethod then
          Result := GetMethodExpr(TMethodSymbol(sym),varExpr,rkClassOfRef,FTok.HotPos,True,True)
        else
          Result := GetMethodExpr(TMethodSymbol(sym),varExpr,rkObjRef,FTok.HotPos,True,True);
      except
        varExpr.Free;
        raise;
      end;
      try
        ReadFuncArgs(TFuncExpr(Result).AddArg);
        if TMethodSymbol(sym).Kind = fkConstructor then
          Result.Typ := methSym.ClassSymbol.Parent;
      except
        Result.Free;
        raise;
      end;
    end
    else if sym is TPropertySymbol then
    begin
      varExpr := TVarExpr.CreateTyped(FProg, parentSym, methSym.SelfSym);
      try
        Result := ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
      except
        varExpr.Free;
        raise;
      end;
    end
    else
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);
  end
  else
    FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InheritedMethodNotFound, [Name]);
end;

// ReadName
//
function TdwsCompiler.ReadName(IsWrite: Boolean): TNoPosExpr;
var
   sym: TSymbol;
   nameToken : TToken;
   namePos : TScriptPos;
   varExpr : TDataExpr;
   fieldExpr, constExpr : TNoPosExpr;
   convExpr : TConvClassExpr;
   progMeth : TMethodSymbol;
   baseType : TTypeSymbol;
   sk : TSpecialKeywordKind;
begin
   if FTok.TestDelete(ttINHERITED) then
      Exit(ReadNameInherited(IsWrite));

   // Get name
   FTok.TestName;
   nameToken := FTok.GetToken;
   namePos := FTok.HotPos;

   // Test for special functions
   sk:=IdentifySpecialName(nameToken.FString);
   if sk<>skNone then begin
      FTok.KillToken;
      Exit(ReadSpecialFunction(namePos, sk));
   end;

   // Find name in symboltable
   sym := FProg.Table.FindSymbol(nameToken.FString);

   if not Assigned(sym) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownName, [nameToken.FString]);

   FTok.KillToken;

   // Add the symbol usage to Dictionary
   if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(sym, namePos);

   Result := nil;
   try
      baseType := sym.BaseType;

      if baseType<>nil then begin

         // Unit prefix found
         if baseType.InheritsFrom(TUnitSymbol) then begin
            if not FTok.TestDelete(ttDOT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
            if not FTok.TestName then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
            namePos := FTok.HotPos;   // reuse token pos variable
            sym := TUnitSymbol(baseType).Table.FindLocal(FTok.GetToken.FString);
            FTok.KillToken;
            // Already added symbol usage of the unit. Now add for the unit's specified symbol.
            if coSymbolDictionary in FCompilerOptions then
               FProg.SymbolDictionary.Add(sym, namePos);
         end;

         if baseType.InheritsFrom(TEnumerationSymbol) then
            baseType := TEnumerationSymbol(baseType).Typ.BaseType;

      end;

      // "Variables"

      if sym.InheritsFrom(TLazyParamSymbol) then begin
         Result := ReadSymbol(GetLazyParamExpr(TLazyParamSymbol(sym)), IsWrite);
         Exit;
      end;

      if sym.InheritsFrom(TByRefParamSymbol) then begin
         if sym.InheritsFrom(TVarParamSymbol) then
            Result := ReadSymbol(GetVarParamExpr(TVarParamSymbol(sym)), IsWrite)
         else if sym.InheritsFrom(TConstParamSymbol) then begin
            Result := ReadSymbol(GetConstParamExpr(TConstParamSymbol(sym)), IsWrite)
         end else Assert(False); // compiler bug
         Exit;
      end;

      if sym.InheritsFrom(TConstSymbol) then begin
         if sym.Typ.Typ is TArraySymbol then begin
            Result := ReadSymbol(TConstExpr.CreateTyped(FProg, sym.Typ.Typ, TConstSymbol(sym)), IsWrite)
         end else begin
            Result := ReadSymbol(TConstExpr.CreateTyped(FProg, sym.Typ, TConstSymbol(sym)), IsWrite)
         end;
         Exit;
      end;

      if sym.InheritsFrom(TDataSymbol) then begin
         if sym.Typ is TFuncSymbol then
            Result := ReadFunc(TFuncSymbol(sym.Typ), IsWrite, GetVarExpr(TDataSymbol(sym)))
         else Result := ReadSymbol(GetVarExpr(TDataSymbol(sym)), IsWrite);
      end else if sym.InheritsFrom(TExternalVarSymbol) then
         Result := ReadSymbol(ReadExternalVar(TExternalVarSymbol(sym), IsWrite), IsWrite)

      // OOP related stuff

      else if baseType is TClassSymbol then begin

         if FTok.TestDelete(ttBLEFT) then begin
            // Cast
            FTok.TestName;
            namePos:=FTok.HotPos;
            Result:=ReadExpr;
            if    (not (Result.Typ is TClassSymbol))
               or (
                     (not TClassSymbol(Result.Typ).IsOfType(baseType))
                     and (not TClassSymbol(baseType).IsOfType(Result.Typ))
                  ) then
               FMsgs.AddCompilerErrorFmt(namePos, CPE_IncompatibleTypes,
                                         [Result.Typ.Name, baseType.Name]);
            if not (FTok.TestDelete(ttBRIGHT)) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
            convExpr:=TConvClassExpr.Create(FProg, TClassSymbol(baseType), Result);
            Result:=nil; // protect ReadSymbol exception
            Result:=ReadSymbol(convExpr, IsWrite);
         end else begin
            constExpr:=TConstExpr.CreateTyped(FProg, TClassSymbol(baseType).ClassOf, baseType.Name);
            Result:=ReadSymbol(constExpr, IsWrite);
         end;

      end else if sym.InheritsFrom(TFieldSymbol) then begin

         progMeth := TMethodSymbol(TProcedure(FProg).Func);
         if progMeth.IsClassMethod then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
         varExpr := TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym);
         try
            fieldExpr:=ReadField(varExpr, TFieldSymbol(sym));
         except
            varExpr.Free;
            raise;
         end;
         Result := ReadSymbol(fieldExpr, IsWrite);

      end else if sym.InheritsFrom(TPropertySymbol) then begin
         progMeth := TMethodSymbol(TProcedure(FProg).Func);
         if progMeth.IsClassMethod then
            varExpr := TConstExpr.CreateTyped(FProg, progMeth.ClassSymbol, nil)
         else varExpr := TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym);
         try
            Result := ReadSymbol(ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite), IsWrite);
         except
            varExpr.Free;
            raise;
         end;

      end else if sym.InheritsFrom(TMethodSymbol) then
         Result:=ReadStaticMethod(TMethodSymbol(sym), IsWrite)

      // Functions/Procedures

      else if sym.InheritsFrom(TFuncSymbol) then
         Result := ReadSymbol(ReadFunc(TFuncSymbol(sym), IsWrite), IsWrite)
      // Type casts
      else if sym.InheritsFrom(TTypeSymbol) then
         Result := ReadTypeCast(namePos, sym)
      else begin
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownType, [sym.Name]);
      end;

   except
      Result.Free;
      raise;
   end;
end;

// ReadNameInherited
//
function TdwsCompiler.ReadNameInherited(IsWrite: Boolean): TNoPosExpr;
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

function TdwsCompiler.ReadField(Expr: TDataExpr; Sym: TFieldSymbol): TNoPosExpr;
begin
   Result := TFieldExpr.Create(FProg, FTok.HotPos, Sym.Typ, Sym, Expr);
end;

// Parses statements like "property[i, j, k] := expr" and "expr := property[i, j, k]"
function TdwsCompiler.ReadPropertyExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol; IsWrite: Boolean): TNoPosExpr;
begin
   if IsWrite then
      Result:=ReadPropertyWriteExpr(Expr, PropertySym)
   else Result:=ReadPropertyReadExpr(Expr, PropertySym);
end;

// ReadPropertyReadExpr
//
function TdwsCompiler.ReadPropertyReadExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol): TNoPosExpr;
var
   sym : TSymbol;
   arrayArgs : TNoPosExprList;
   aPos : TScriptPos;
begin
   Result := nil;
   aPos:=FTok.HotPos;
   arrayArgs := TNoPosExprList.Create;
   try
      if PropertySym.ArrayIndices.Count > 0 then
         ReadFuncArgs(arrayArgs.AddExpr, ttALEFT, ttARIGHT);

      sym := PropertySym.ReadSym;

      // No ReadSym
      if sym = nil then

         FMsgs.AddCompilerStop(FTok.HotPos, CPE_WriteOnlyProperty)

      else if sym is TFieldSymbol then begin

         // ReadSym is a field
         if Expr.Typ is TClassSymbol then
            Result := TReadOnlyFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, TFieldSymbol(sym),
                                                TDataExpr(Expr))
         else FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);

      end else if sym is TMethodSymbol then begin

         // ReadSym is a method
         if Expr.Typ is TClassOfSymbol then
            Result := GetMethodExpr(TMethodSymbol(sym), TDataExpr(Expr), rkClassOfRef, aPos, False)
         else Result := GetMethodExpr(TMethodSymbol(sym), TDataExpr(Expr), rkObjRef, aPos, False);

         try
            // Add array indizes if any
            while ArrayArgs.Count > 0 do begin
               TFuncExpr(Result).AddArg(ArrayArgs[0]);
               ArrayArgs.Delete(0);
            end;

            if Assigned(PropertySym.IndexSym) then
               TFuncExpr(Result).AddArg(TConstExpr.CreateTyped(FProg,PropertySym.IndexSym,PropertySym.IndexValue));
         except
            Result.Free;
            raise;
         end;

      end else Assert(False);

   finally
      arrayArgs.Free;
   end;
end;

// ReadPropertyWriteExpr
//
function TdwsCompiler.ReadPropertyWriteExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol): TNoPosExpr;
var
   sym : TSymbol;
   arrayArgs : TNoPosExprList;
   aPos : TScriptPos;
   fieldExpr : TFieldExpr;
   tokenType : TTokenType;
begin
   Result := nil;
   aPos:=FTok.HotPos;
   arrayArgs := TNoPosExprList.Create;
   try
      if PropertySym.ArrayIndices.Count > 0 then
         ReadFuncArgs(arrayArgs.AddExpr, ttALEFT, ttARIGHT);

      tokenType:=FTok.TestDeleteAny(cAssignmentTokens);

      if tokenType<>ttNone then begin

         if tokenType<>ttASSIGN then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_CantUseCombinedAssignmentOnProperty);

         sym := PropertySym.WriteSym;

         // No WriteSym
         if sym = nil then

            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ReadOnlyProperty)

         else if sym is TFieldSymbol then begin

            // WriteSym is a Field
            if Expr.Typ is TClassOfSymbol then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
            fieldExpr := TFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, TFieldSymbol(sym), TDataExpr(Expr));
            Result := ReadAssign(ttASSIGN, fieldExpr);

         end else if sym is TMethodSymbol then begin

            // WriteSym is a Method
            // Convert an assignment to a function call f := x  -->  f(x)

            if Expr.Typ is TClassOfSymbol then begin
               // Class properties
               if not TMethodSymbol(sym).IsClassMethod then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticPropertyWriteExpected);
               Result := GetMethodExpr(TMethodSymbol(sym), Expr, rkClassOfRef, aPos, True);
            end else Result := GetMethodExpr(TMethodSymbol(sym), Expr, rkObjRef, aPos, True);

            try
               Expr := nil; // is part of Result

               // Add array indizes (if any)
               while arrayArgs.Count > 0 do begin
                  TFuncExpr(Result).AddArg(arrayArgs[0]);
                  arrayArgs.Delete(0);
               end;

               if Assigned(PropertySym.IndexSym) then
                  TFuncExpr(Result).AddArg(TConstExpr.CreateTyped(FProg, PropertySym.IndexSym,
                                                                  PropertySym.IndexValue));

               // Add right side of assignment
               TFuncExpr(Result).AddArg(ReadExpr);
            except
               Result.Free;
               raise;
            end;
         end;

      end else begin

         if FTok.Test(ttDOT) then begin

            Result:=ReadSymbol(ReadPropertyReadExpr(Expr, PropertySym), True);

         end else begin
            FMsgs.AddCompilerError(aPos, CPE_InvalidInstruction);
            // fake to keep going
            FreeAndNil(Expr);
            Result:=TNullExpr.Create(FProg, aPos);
         end;

      end;

   finally
      arrayArgs.Free;
   end;
end;

// ReadSymbol
//
function TdwsCompiler.ReadSymbol(Expr: TNoPosExpr; IsWrite: Boolean): TNoPosExpr;

   function GetDefaultProperty(cls: TClassSymbol): TPropertySymbol;
   begin
      while Assigned(cls) and not Assigned(cls.DefaultProperty) do
         cls := cls.Parent;

      if Assigned(cls) then
         Result := cls.DefaultProperty
      else Result := nil;
   end;

   function ReadArrayExpr(var baseExpr : TDataExpr) : TArrayExpr;
   var
      indexExpr : TNoPosExpr;
      baseType : TTypeSymbol;
      arraySymbol : TStaticArraySymbol;
   begin
      FTok.KillToken;

      Result := nil;

      if FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

      // There is at one index expression
      repeat
         indexExpr := ReadExpr;
         baseType := baseExpr.BaseType;

         try
            if baseType is TStaticArraySymbol then begin
               arraySymbol:=TStaticArraySymbol(baseType);
               if arraySymbol is TOpenArraySymbol then begin
                  Result := TOpenArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr)
               end else begin
                  Result := TStaticArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr,
                                                    arraySymbol.LowBound, arraySymbol.HighBound)
               end;
            end else if baseType is TDynamicArraySymbol then
               Result := TDynamicArrayExpr.Create(FProg, FTok.HotPos, baseExpr, indexExpr)
            else FMsgs.AddCompilerStop(FTok.HotPos, RTE_TooManyIndices);
         except
            indexExpr.Free;
            raise;
         end;

         baseExpr := Result;
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
   end;

var
   name: string;
   member: TSymbol;
   defaultProperty: TPropertySymbol;
   symPos: TScriptPos;
   baseType: TTypeSymbol;
   dataExpr : TDataExpr;
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
                  if coSymbolDictionary in FCompilerOptions then
                     FProg.SymbolDictionary.Add(member, symPos);

                  if Assigned(member) then
                     Result := TRecordExpr.Create(FProg, FTok.HotPos, TDataExpr(Result), TMemberSymbol(member))
                  else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownMember, [Name]);
                  Expr := nil;
               end
               // Class
               else if baseType is TClassSymbol then begin
                  member := TClassSymbol(baseType).Members.FindSymbol(Name);
                  if coSymbolDictionary in FCompilerOptions then
                     FProg.SymbolDictionary.Add(member, symPos);

                  if member is TMethodSymbol then begin
                     // Member is a method
                     if Assigned(TMethodSymbol(member).SelfSym) then
                        Result := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result), rkObjRef, symPos, IsWrite)
                     else Result := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result), rkClassOfRef, symPos, IsWrite);
                     ReadFuncArgs(TFuncExpr(Result).AddArg);
                  end else if member is TFieldSymbol then
                     // Member is a field
                     Result := TFieldExpr.Create(FProg, FTok.HotPos, member.Typ,
                                                 TFieldSymbol(member), TDataExpr(Result))
                  else if member is TPropertySymbol then
                     // Member is a property
                     Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite)
                  else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownMember, [Name]);
               end
               // Class Of
               else if baseType is TClassOfSymbol then begin
                  member := TClassSymbol(baseType.Typ).Members.FindSymbol(Name);
                  if coSymbolDictionary in FCompilerOptions then
                     FProg.SymbolDictionary.Add(member, FTok.HotPos);

                  // Class method
                  if member is TMethodSymbol then begin
                     case TMethodSymbol(member).Kind of
                        fkFunction, fkProcedure, fkMethod:
                           if not TMethodSymbol(member).IsClassMethod then
                              FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
                        fkDestructor:
                           FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
                     end;
                     Result := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result),
                                             rkClassOfRef, symPos, IsWrite);
                     ReadFuncArgs(TFuncExpr(Result).AddArg);
                  end
                  // Static property
                  else if member is TPropertySymbol then
                     Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite)
                  else FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
               end
               // Connector symbol
               else if baseType is TConnectorSymbol then begin
                  Result := ReadConnectorSym(Name, Result,
                                             TConnectorSymbol(baseType).ConnectorType, IsWrite)
               end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMemberExpected);
            end else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
         end
         // Arrays
         else if FTok.Test(ttALEFT) then begin
            if Assigned(Result) then begin
               if baseType is TClassSymbol then begin
                  // array property
                  DefaultProperty := GetDefaultProperty(TClassSymbol(baseType));
                  if Assigned(DefaultProperty) then
                     Result := ReadPropertyExpr(TDataExpr(Result), DefaultProperty, IsWrite)
                  else FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NoDefaultProperty, [Result.Typ.Name]);
               end else begin
                  // Type "array"
                  dataExpr:=(Result as TDataExpr);
                  if baseType is TArraySymbol then
                     Result := ReadArrayExpr(dataExpr)
                  else if baseType is TConnectorSymbol then
                     Result := ReadConnectorArray('', Result, TConnectorSymbol(baseType).ConnectorType, IsWrite)
                  else begin
                     FTok.KillToken;
                     Result := ReadStringArray(dataExpr, IsWrite)
                  end;
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

function TdwsCompiler.ReadExternalVar;
begin
  Result := nil;
  try
    if IsWrite then
    begin
      if FTok.TestDelete(ttASSIGN) then
      begin
        if not Assigned(Sym.WriteFunc) then
          FProg.Msgs.AddCompilerStop(FTok.HotPos,CPE_CantWriteToLeftSide);
        // Transform a := b into a(b)
        Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.WriteFunc, True);
        Result.AddArg(ReadExpr);
      end
      else if (Sym.Typ is TClassSymbol) or (Sym.Typ is TClassOfSymbol) then
      begin
        if not Assigned(Sym.ReadFunc) then
          FProg.Msgs.AddCompilerStop(FTok.HotPos,CPE_RightSideNeedsReturnType);
        Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc, False)
      end
      else
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
    end
    else if Assigned(Sym.ReadFunc) then
      Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc, False)
    else
      FProg.Msgs.AddCompilerStop(FTok.HotPos,CPE_WriteOnlyProperty); // ??
  except
    Result.Free;
    raise;
  end;
end;

// ReadFor
//
function TdwsCompiler.ReadFor: TForExpr;
var
   expr : TNoPosExpr;
   loopVarExpr : TIntVarExpr;
   fromExpr, toExpr, stepExpr : TNoPosExpr;
   sym : TSymbol;
   forPos, enumPos, stepPos : TScriptPos;
   forExprClass : TForExprClass;
begin
   Result:=nil;
   loopVarExpr:=nil;
   fromExpr:=nil;
   toExpr:=nil;
   stepExpr:=nil;
   try
      forPos:=FTok.HotPos;

      expr:=ReadName;
      try
         if not (expr is TVarExpr) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_VariableExpected);
         if not expr.IsIntegerValue then
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
         sym:=FProg.Table.FindSymbol(FTok.GetToken.FString);
         if not Assigned(sym) then
            FMsgs.AddCompilerStopFmt(enumPos, CPE_UnknownName, [FTok.GetToken.FString]);
         FTok.KillToken;

         if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(sym, enumPos);

         if sym.InheritsFrom(TEnumerationSymbol) then begin

            if loopVarExpr.Typ<>sym then
               FMsgs.AddCompilerStop(enumPos, CPE_IncompatibleOperands);

            fromExpr:=TConstExpr.CreateTyped(FProg, loopVarExpr.Typ, TEnumerationSymbol(sym).LowBound);
            toExpr:=TConstExpr.CreateTyped(FProg, loopVarExpr.Typ, TEnumerationSymbol(sym).HighBound);

         end else FMsgs.AddCompilerStop(enumPos, CPE_EnumerationExpected);

      end else begin

         if not FTok.TestDelete(ttASSIGN) then
           FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

         fromExpr:=ReadExpr;
         if not fromExpr.IsIntegerValue then
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
         if not toExpr.IsIntegerValue then
            FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpected);

      end;

      if FTok.Test(ttNAME) and SameText(FTok.GetToken.FString, 'step') then begin
         FTok.KillToken;
         FTok.Test(ttNone);
         stepPos:=FTok.HotPos;
         stepExpr:=ReadExpr;
         if not stepExpr.IsIntegerValue then
            FMsgs.AddCompilerError(stepPos, CPE_IntegerExpected);
         if stepExpr.InheritsFrom(TConstIntExpr) and (TConstIntExpr(stepExpr).EvalAsInteger<=0) then
            FMsgs.AddCompilerErrorFmt(stepPos, RTE_ForLoopStepShouldBeStrictlyPositive,
                                      [TConstIntExpr(stepExpr).EvalAsInteger]);
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

function TdwsCompiler.ReadIf: TIfExpr;
begin
  Result := TIfExpr.Create(FProg, FTok.HotPos);
  try
    Result.FCond := ReadExpr;

    if not FTok.TestDelete(ttTHEN) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ThenExpected);

    if not FTok.Test(ttELSE) then // if () then else;
      Result.FThen:=ReadBlock;

    if not Assigned(Result.FThen) then  // if () then <EOF>
      Result.FThen := TNullExpr.Create(FProg,FTok.HotPos);

    if FTok.TestDelete(ttELSE) then
      Result.FElse:=ReadBlock;
  except
    Result.Free;
    raise;
  end;
end;

// ReadCase
//
function TdwsCompiler.ReadCase;
var
   expr : TExpr;
   condList: TList;
   tt: TTokenType;
   x: Integer;
begin
   condList := TList.Create;
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
               try
                  ReadCaseConditions(condList, Result.ValueExpr);

                  if not FTok.TestDelete(ttCOLON) then
                     FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

                  Expr := ReadBlock;

               except
                  for x := 0 to condList.Count - 1 do
                     TCaseCondition(condList[x]).Free;
                  raise;
               end;

               // Add case conditions to TCaseExpr
               for x := 0 to condList.Count - 1 do begin
                  TCaseCondition(condList[x]).TrueExpr := Expr;
                  if x = 0 then
                     TCaseCondition(condList[0]).OwnsTrueExpr := True;
                  Result.AddCaseCondition(condList[x]);
               end;
               condList.Clear;

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
function TdwsCompiler.ReadCaseConditions(condList : TList; valueExpr : TNoPosExpr) : Integer;
var
   hotPos : TScriptPos;
   exprFrom, exprTo : TNoPosExpr;
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
            condList.Add(TRangeCaseCondition.Create(hotPos, valueExpr, exprFrom, exprTo));
         end else begin
            // compare condition e. g. 123:
            condList.Add(TCompareCaseCondition.Create(hotPos, valueExpr, exprFrom));
         end;
      except
         exprFrom.Free;
         raise;
      end;

   until not FTok.TestDelete(ttCOMMA);

   Result:=condList.Count;
end;

// ReadWhile
//
function TdwsCompiler.ReadWhile: TWhileExpr;
begin
   Result := TWhileExpr.Create(FProg, FTok.HotPos);
   EnterLoop(Result);
   try
      Result.CondExpr := ReadExpr;

      if not FTok.TestDelete(ttDO) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

      if    (not Result.CondExpr.IsConstant)
         or (not Result.CondExpr.IsBooleanValue)
         or (not Result.CondExpr.EvalAsBoolean) then
         MarkLoopExitable(leBreak);

      Result.LoopExpr := ReadBlock;
   except
      Result.Free;
      raise;
   end;
   LeaveLoop;
end;

// ReadRepeat
//
function TdwsCompiler.ReadRepeat: TRepeatExpr;
var
   tt: TTokenType;
begin
   Result := TRepeatExpr.Create(FProg, FTok.HotPos);
   EnterLoop(Result);
   try
      Result.LoopExpr := ReadBlocks([ttUNTIL], tt);
      Result.CondExpr := ReadExpr;
      if (not Result.CondExpr.IsConstant) or Result.CondExpr.EvalAsBoolean then
         MarkLoopExitable(leBreak);
   except
      Result.Free;
      raise;
   end;
   LeaveLoop;
end;

function TdwsCompiler.ReadAssign(token : TTokenType; Left: TDataExpr): TNoResultExpr;
var
   pos : TScriptPos;
   right : TNoPosExpr;
begin
   pos := FTok.HotPos;
   right := ReadExpr;
   try
      if Left.InheritsFrom(TFuncExpr) and TFuncExpr(Left).IsWritable then begin
         Left := TFuncCodeExpr.Create(FProg, FTok.HotPos, TFuncExpr(Left));
         if right.Typ = FProg.TypNil then begin
            right.Free;
            Result := TInitDataExpr.Create(FProg, FTok.HotPos, Left);
            Exit;
         end else if right.InheritsFrom(TFuncExpr) then
            right := TFuncCodeExpr.Create(FProg, FTok.HotPos, TFuncExpr(right));
      end;
      Result:=CreateAssign(pos, token, left, right);
   except
      right.Free;
      raise;
   end;
end;

// ReadStaticMethod
//
function TdwsCompiler.ReadStaticMethod(methodSym: TMethodSymbol; IsWrite: Boolean): TFuncExpr;
var
   progMeth: TMethodSymbol;
begin
   progMeth := TMethodSymbol(TProcedure(FProg).Func);
   if not progMeth.IsClassMethod then
      Result := GetMethodExpr(methodSym,
                              TVarExpr.CreateTyped(FProg, progMeth.SelfSym.Typ, progMeth.SelfSym),
                              rkObjRef, FTok.HotPos, IsWrite)
   else if (methodSym.Kind = fkConstructor) or (methodSym.IsClassMethod) then
      Result := GetMethodExpr(methodSym,
                              TConstExpr.CreateTyped(FProg, progMeth.ClassSymbol, nil),
                              rkClassOfRef, FTok.HotPos, IsWrite, True)
   else begin
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
      Exit(nil);
   end;

   ReadFuncArgs(TFuncExpr(Result).AddArg);
   Result := (ReadSymbol(Result, IsWrite) as TFuncExpr);
end;

// ReadFunc
//
function TdwsCompiler.ReadFunc(FuncSym: TFuncSymbol; IsWrite: Boolean;
                               CodeExpr: TDataExpr = nil): TNoPosExpr;
var
   internalFunc : TObject;
   magicFuncSym : TMagicFuncSymbol;
begin
   WarnDeprecated(FuncSym);

   if FuncSym.InheritsFrom(TMethodSymbol) and not (TMethodSymbol(FuncSym).IsClassMethod) then begin

      Result := TMethodStaticExpr.Create(FProg, FTok.HotPos, TMethodSymbol(FuncSym),
                                         TMethodObjExpr.Create(FProg, FTok.HotPos, CodeExpr),
                                         True, CodeExpr,
                                         IsWrite and Assigned(CodeExpr));

   end else if FuncSym.InheritsFrom(TMagicFuncSymbol) then begin

      magicFuncSym:=TMagicFuncSymbol(FuncSym);
      internalFunc:=magicFuncSym.InternalFunction;
      if internalFunc.InheritsFrom(TInternalMagicIntFunction) then
         Result:=TMagicIntFuncExpr.Create(FProg, FTok.HotPos, magicFuncSym)
      else if internalFunc.InheritsFrom(TInternalMagicFloatFunction) then
         Result:=TMagicFloatFuncExpr.Create(FProg, FTok.HotPos, magicFuncSym)
      else if internalFunc.InheritsFrom(TInternalMagicStringFunction) then
         Result:=TMagicStringFuncExpr.Create(FProg, FTok.HotPos, magicFuncSym)
      else if internalFunc.InheritsFrom(TInternalMagicProcedure) then
         Result:=TMagicProcedureExpr.Create(FProg, FTok.HotPos, magicFuncSym)
      else Result:=TMagicVariantFuncExpr.Create(FProg, FTok.HotPos, magicFuncSym);

   end else begin

      Result:=TFuncExpr.Create(FProg, FTok.HotPos, FuncSym, True, CodeExpr,
                               IsWrite and Assigned(CodeExpr));

   end;

   try
      ReadFuncArgs(TFuncExprBase(Result).AddArg);
      TFuncExprBase(Result).TypeCheck;
   except
      Result.Free;
      raise;
   end;

   if Optimize then
      Result:=Result.Optimize;
end;

// ReadFuncArgs
//
procedure TdwsCompiler.ReadFuncArgs(const AddArgProc: TAddArgFunction; LDelim: TTokenType; RDelim: TTokenType);
var
   arg : TNoPosExpr;
   argSym : TSymbol;
   argPos : TScriptPos;
begin
   if FTok.TestDelete(LDelim) then begin
      if not FTok.TestDelete(RDelim) then begin
         // At least one argument was found
         repeat
            argPos:=FTok.HotPos;
            arg:=ReadExpr;
            argSym:=AddArgProc(arg);
            if (argSym is TVarParamSymbol) and (arg is TVarExpr) then
               WarnForVarUsage(TVarExpr(arg), argPos);
         until not FTok.TestDelete(ttCOMMA);
         if not FTok.TestDelete(RDelim) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end;
   end;
end;

// ReadArray
//
function TdwsCompiler.ReadArray(const TypeName: String): TTypeSymbol;
var
   x: Integer;
   min, max: TNoPosExprList;
   typ: TSymbol;
   hotPos : TScriptPos;
begin
   min := TNoPosExprList.Create;
   max := TNoPosExprList.Create;
   try

      if FTok.TestDelete(ttALEFT) then begin

         repeat
            // Lower bound
            hotPos:=FTok.HotPos;
            min.Insert0(ReadExpr);

            if not (min[0].IsConstant) then
               FMsgs.AddCompilerStop(hotPos, CPE_ArrayBoundNotAConstant);

            if not (min[0].Typ = FProg.TypInteger) then
               FMsgs.AddCompilerStop(hotPos, CPE_ArrayBoundNotInteger);

            if not FTok.TestDelete(ttDOTDOT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotDotExpected);

            // Upper bound
            hotPos:=FTok.HotPos;
            max.Insert0(ReadExpr);

            if not (max[0].IsConstant) then
               FMsgs.AddCompilerStop(hotPos, CPE_ArrayBoundNotAConstant);

            if not (max[0].Typ = FProg.TypInteger) then
               FMsgs.AddCompilerStop(hotPos, CPE_ArrayBoundNotInteger);

            if max[0].EvalAsInteger < min[0].EvalAsInteger then
               FMsgs.AddCompilerStop(hotPos, CPE_LowerBoundBiggerThanUpperBound);

            if FTok.Test(ttARIGHT) then
               Break;
         until not FTok.TestDelete(ttCOMMA);

         if not FTok.TestDelete(ttARIGHT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
      end;

      if not FTok.TestDelete(ttOF) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_OfExpected);

      if FTok.TestDelete(ttCONST) then begin

         Result := TOpenArraySymbol.Create(TypeName, FProg.TypVariant);

      end else begin

         typ := ReadType('');

         if min.Count > 0 then begin
            // initialize innermost array
            Result := TStaticArraySymbol.Create('', typ, min[0].EvalAsInteger, max[0].EvalAsInteger);
            try
               // add outer arrays
               Assert(FProg.Table is TProgramSymbolTable);
               for x := 1 to min.Count - 1 do begin
                  TProgramSymbolTable(FProg.Table).AddToDestructionList(Result);
                  Result := TStaticArraySymbol.Create('', Result, min[x].EvalAsInteger, max[x].EvalAsInteger);
               end;

               // only outermost array is named
               Result.SetName(TypeName);
            except
               Result.Free;
               raise;
            end;
         end else begin
            Result := TDynamicArraySymbol.Create(TypeName, typ);
         end;

      end;

   finally
      min.Free;
      max.Free;
   end;
end;

function TdwsCompiler.ReadArrayConstant: TArrayConstantExpr;
begin
  Result := TArrayConstantExpr.Create(FProg);
  try
    if not FTok.TestDelete(ttARIGHT) then
    begin
      // At least one argument was found
      repeat
        TArrayConstantExpr(Result).AddElementExpr(ReadExpr);
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
    end;
    if Optimize then
      Result := Result.Optimize as TArrayConstantExpr;
  except
    Result.Free;
    raise;
  end;
end;

procedure TdwsCompiler.ReadNameList(Names: TStrings);
begin
  Names.Clear;
  repeat
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    Names.Add(FTok.GetToken.FString);
    CheckSpecialName(FTok.GetToken.FString);
    FTok.KillToken;
  until not FTok.TestDelete(ttCOMMA);
end;

procedure TdwsCompiler.ReadNameList(Names: TStrings; out PosArray: TScriptPosArray);
begin
  Names.Clear;
  repeat
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    // Added HotPos positions to PosArray. Used for dictionary
    if coSymbolDictionary in FCompilerOptions then
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
function TdwsCompiler.ReadClassOf(const TypeName: string): TClassOfSymbol;
var
   name : String;
   typ : TSymbol;
begin
   // Declaration of a class reference
   if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
   name := FTok.GetToken.FString;
   FTok.KillToken;

   typ := FProg.Table.FindSymbol(name);
   if not Assigned(typ) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownClass, [name]);
   if not (typ is TClassSymbol) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NotAClass, [name]);

   if TypeName <> '' then begin
      Result := TClassOfSymbol.Create(TypeName, TClassSymbol(typ));
      // Add reference of class type to Dictionary
      if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(typ, FTok.HotPos);
   end else Result := TClassSymbol(typ).ClassOf;
end;

// ReadClass
//
function TdwsCompiler.ReadClass(const TypeName: string): TClassSymbol;
var
   name: string;
   sym, Typ: TSymbol;
   propSym: TPropertySymbol;
   defProp: Boolean;
   isInSymbolTable: Boolean;
begin
   // Check for a forward declaration of this class
   sym:=FProg.Table.FindSymbol(TypeName);
   Result:=nil;

   if Assigned(sym) then begin
      if sym is TClassSymbol then begin
         if TClassSymbol(sym).IsForwarded then
            Result:=TClassSymbol(sym)
      end else begin
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameAlreadyExists, [sym.Caption]);
      end;
   end;

   isInSymbolTable := Assigned(Result);

   if not Assigned(Result) then
      Result := TClassSymbol.Create(TypeName);

   // forwarded declaration
   if FTok.Test(ttSEMI) then begin
      if Result.IsForwarded then
         FMsgs.AddCompilerError(FTok.HotPos, CPE_ClassForwardAlreadyExists);
      Result.SetForwardedPos(FTok.HotPos);
      Exit;
   end else Result.ClearIsForwarded;

   if not isInSymbolTable then
      FProg.Table.AddSymbol(Result);   // auto-forward
   try
      try
         // inheritance
         if FTok.TestDelete(ttBLEFT) then begin
            if not FTok.TestName then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

            Name := FTok.GetToken.FString;
            FTok.KillToken;

            Typ := FProg.Table.FindSymbol(Name);
            if not (Typ is TClassSymbol) then begin
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NotAClass, [Name]);
               Typ:=FProg.TypObject;
            end;

            if TClassSymbol(Typ).IsForwarded then
               FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassNotImplementedYet, [Name]);

            Result.InheritFrom(TClassSymbol(Typ));

            if not FTok.TestDelete(ttBRIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
         end else Result.InheritFrom(FProg.TypObject);

         // standard class definition
         while not FTok.Test(ttEND) do begin

            // Read methods and properties
            if FTok.TestDelete(ttFUNCTION) then
               Result.AddMethod(ReadMethodDecl(Result, fkFunction, False))
            else if FTok.TestDelete(ttPROCEDURE) then
               Result.AddMethod(ReadMethodDecl(Result, fkProcedure, False))
            else if FTok.TestDelete(ttCONSTRUCTOR) then
               Result.AddMethod(ReadMethodDecl(Result, fkConstructor, False))
            else if FTok.TestDelete(ttDESTRUCTOR) then
               Result.AddMethod(ReadMethodDecl(Result, fkDestructor, False))
            else if FTok.TestDelete(ttMETHOD) then
               Result.AddMethod(ReadMethodDecl(Result, fkMethod, False))
            else if FTok.TestDelete(ttCLASS) then begin

               if FTok.TestDelete(ttPROCEDURE) then
                  Result.AddMethod(ReadMethodDecl(Result, fkProcedure, True))
               else if FTok.TestDelete(ttFUNCTION) then
                  Result.AddMethod(ReadMethodDecl(Result, fkFunction, True))
               else if FTok.TestDelete(ttMETHOD) then
                  Result.AddMethod(ReadMethodDecl(Result, fkMethod, True))
               else if FTok.TestDelete(ttOPERATOR) then
                  Result.AddOperator(ReadClassOperatorDecl(Result))
               else FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);

            end else if FTok.TestDelete(ttPROPERTY) then begin

               propSym := ReadProperty(Result);
               defProp := False;
               // Array-Prop can be default
               if propSym.ArrayIndices.Count > 0 then begin
                  defProp := FTok.TestDelete(ttDEFAULT);
                  if defProp then begin
                     if not FTok.TestDelete(ttSEMI) then
                        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
                     if Assigned(Result.DefaultProperty) then
                        FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_MultipleDefaultProperties, [Result.Name]);
                  end;
               end;
               Result.AddProperty(propSym);
               if defProp then
                  Result.DefaultProperty := propSym;

            end else if    FTok.Test(ttPRIVATE) or FTok.Test(ttPROTECTED)
                        or FTok.Test(ttPUBLIC) or FTok.Test(ttPUBLISHED) then begin
               // visibility ignored
               FTok.KillToken;
            end else if FTok.TestName then begin
               ReadClassFields(Result);
               if not (FTok.TestDelete(ttSEMI) or FTok.Test(ttEND)) then
                  Break;
            end else Break;

         end; // while

         if not FTok.TestDelete(ttEND) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      except
         on E: EClassIncompleteError do
            ; // leave it handled
         else begin
            // if not ClassIncompleteError then free the class and re-raise error
            if not isInSymbolTable then begin
               if coSymbolDictionary in FCompilerOptions then
                  FProg.SymbolDictionary.Remove(Result);
               Result.Free;
            end;
            raise;
         end;
      end; {except}
   finally
      if not isInSymbolTable then
         FProg.Table.Remove(Result);  // auto-forward
   end;
end;

// ReadClassFields
//
procedure TdwsCompiler.ReadClassFields(const classSymbol : TClassSymbol);
var
   i : Integer;
   sym, typ : TSymbol;
   fieldSym : TFieldSymbol;
   names : TStringList;
   fields : TList;
   posArray : TScriptPosArray;    // positions of items pulled from ReadNameList call
begin
   names:=TStringList.Create;
   fields:=TList.Create;
   try
      // Conditionally pass in dynamic array
      if coSymbolDictionary in FCompilerOptions then
         ReadNameList(Names, PosArray)     // use overloaded version
      else ReadNameList(Names);

      if not FTok.TestDelete(ttCOLON) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

      Typ := ReadType('');
      for i := 0 to Names.Count - 1 do begin
         // Check if name isn't already used
         sym := classSymbol.Members.FindLocal(Names[i]);
         if Assigned(sym) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameAlreadyExists, [Names[i]]);

         // Create Internal Field
         fieldSym := TFieldSymbol.Create(Names[i], Typ);
         Fields.Add(sym);
         classSymbol.AddField(fieldSym);

         // Enter Field symbol in dictionary
         if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(sym, PosArray[i], [suDeclaration]);
      end;
   finally
      names.Free;
      fields.Free;
   end;
end;

function TdwsCompiler.CheckFuncParams(ParamsA, ParamsB: TSymbolTable;
  IndexSym: TSymbol; TypSym: TSymbol): Boolean;
begin
  Result := False;

  if Assigned(IndexSym) then
  begin
    if Assigned(TypSym) then
    begin
      if ParamsB.Count <> ParamsA.Count + 2 then
        Exit;
      if ParamsB[ParamsA.Count + 1].Typ <> TypSym then
        Exit;
      if ParamsB[ParamsA.Count].Typ <> IndexSym then
        Exit;
    end
    else if ParamsB.Count <> ParamsA.Count + 1 then
      Exit
    else
      if ParamsB[ParamsA.Count].Typ <> IndexSym then
        Exit;
  end
  else
  begin
    if Assigned(TypSym) then
    begin
      if ParamsB.Count <> ParamsA.Count + 1 then
        Exit;
      if ParamsB[ParamsA.Count].Typ <> TypSym then
        Exit;
    end
    else if ParamsA.Count <> ParamsB.Count then
      Exit;
  end;

  Result := CheckParams(ParamsA,ParamsB,False);
end;


// ReadClassOperatorDecl
//
function TdwsCompiler.ReadClassOperatorDecl(ClassSym: TClassSymbol) : TClassOperatorSymbol;
var
   tt : TTokenType;
   usesName : String;
   usesPos : TScriptPos;
   sym : TSymbol;
begin
   tt:=FTok.TestDeleteAny([ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN]);
   if tt=ttNone then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_OverloadableOperatorExpected);

   Result:=TClassOperatorSymbol.Create(tt);
   try
      Result.Typ:=ReadType('');

      if ClassSym.FindClassOperatorStrict(tt, Result.Typ, False)<>nil then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_ClassOperatorRedefined, [Result.Typ.Name]);

      if not FTok.TestDelete(ttUSES) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_UsesExpected);

      if not FTok.TestName then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      usesName:=FTok.GetToken.FString;
      usesPos:=FTok.HotPos;
      FTok.KillToken;

      sym:=ClassSym.Members.FindSymbol(usesName);

      if    (not Assigned(sym))
         or (not (sym is TMethodSymbol)) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureMethodExpected);

      Result.UsesSym:=TMethodSymbol(sym);
      if coSymbolDictionary in FCompilerOptions then
         FProg.SymbolDictionary.Add(sym, usesPos);

      if Result.UsesSym.Params.Count<>1 then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_SingleParameterExpected);
      if Result.UsesSym.Params[0].Typ<>Result.Typ then
         FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_InvalidParameterType, [Result.UsesSym.Name]);

      if not FTok.TestDelete(ttSEMI) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
   except
      // Remove reference to symbol (gets freed)
      if coSymbolDictionary in FCompilerOptions then
         FProg.SymbolDictionary.Remove(Result);
      Result.Free;
      raise;
   end;
end;

function TdwsCompiler.ReadProperty(ClassSym: TClassSymbol): TPropertySymbol;
var
  x: Integer;
  name: string;
  sym: TSymbol;
  arrayIndices: TSymbolTable;
  propPos: TScriptPos;
  accessPos: TScriptPos;  // Position where either a Read or Write symbol is found
  indexExpr: TNoPosExpr;
  indexTyp: TSymbol;
begin
  // Read property name
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
  name := FTok.GetToken.FString;
  propPos := FTok.HotPos;
  FTok.KillToken;

  // Check if property name is free
  sym := ClassSym.Members.FindSymbol(name);
  if Assigned(sym) then
    if sym is TPropertySymbol then
    begin
      if TPropertySymbol(sym).ClassSymbol = ClassSym then
        FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameAlreadyExists, [Name]);
    end
    else
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameAlreadyExists, [Name]);

  arrayIndices := TSymbolTable.Create;
  try
    // Check if it is an array property
    ReadArrayParams(arrayIndices);

    if not FTok.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

    sym := ReadType('');
    Result := TPropertySymbol.Create(name, sym);
    try
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(Result, propPos, [suDeclaration]);

      if FTok.TestDelete(ttINDEX) then
      begin
        indexExpr := ReadExpr;
        if not (indexExpr is TConstExpr) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);
        indexTyp := indexExpr.Typ;
        Result.SetIndex(TConstExpr(indexExpr).Data, TConstExpr(indexExpr).Addr,
          indexTyp);
      end
      else
        indexTyp := nil;

      // Generates a suggestion of how to fix it for class completion
      if FTok.TestDelete(ttREAD) then
      begin
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
        Name := FTok.GetToken.FString;
        accessPos := FTok.HotPos;
        FTok.KillToken;

        sym := ClassSym.Members.FindSymbol(name);

        if not Assigned(sym) or (sym is TPropertySymbol) then
        begin
          { Register the error and break the compilation process }
          FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);
          raise EClassPropertyIncompleteError.Create('');
        end;

        if sym is TMethodSymbol then
        begin
          if not CheckFuncParams(arrayIndices, TMethodSymbol(sym).Params, indexTyp) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_IncompatibleType, [name]);
        end
        else if arrayIndices.Count > 0 then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionMethodExpected);

        if Result.Typ <> sym.Typ then
          FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_IncompatibleType, [name]);

        Result.ReadSym := sym;
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(sym, accessPos)
      end;

      // Generates a suggestion of how to fix it for class completion
      if FTok.TestDelete(ttWRITE) then
      begin
        // Read name
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
        Name := FTok.GetToken.FString;
        accessPos := FTok.HotPos;
        FTok.KillToken;

        // Check if symbol exists
        sym := ClassSym.Members.FindSymbol(Name);

        if not Assigned(sym) or (sym is TPropertySymbol) then
        begin
          { Register the error and break the compilation process }
          FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_FieldMethodUnknown, [name]);
          raise EClassPropertyIncompleteError.Create('');
        end;

        if sym is TFuncSymbol then
        begin
          if TFuncSymbol(sym).Kind <> fkProcedure then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureMethodExpected);
          if not CheckFuncParams(arrayIndices, TFuncSymbol(sym).Params, indexTyp, Result.Typ) then
            FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_IncompatibleType, [name]);
        end
        else if Result.Typ <> sym.Typ then
          FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_IncompatibleWriteSymbol, [Name]);

        Result.WriteSym := sym;
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(sym, accessPos)
      end;

      if (Result.ReadSym = nil) and (Result.WriteSym = nil) then
        FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ReadOrWriteExpected, [name]);

      if not FTok.TestDelete(ttSEMI) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

      // Add array indices to property symbol (if any)
       for x := 0 to arrayIndices.Count - 1 do
         Result.ArrayIndices.AddSymbol(arrayIndices[x]);
       arrayIndices.Clear;

    except
      // Remove reference to symbol (gets freed)
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Remove(Result);
      Result.Free;
      raise;
    end;
  finally
    arrayIndices.Free;
  end;

end;

function TdwsCompiler.ReadRecord(const TypeName: string): TTypeSymbol;
var
  x: Integer;
  Names: TStringList;
  member,
  Typ: TSymbol;
  PosArray: TScriptPosArray;
begin
  Result := TRecordSymbol.Create(TypeName);
  try
    Names := TStringList.Create;
    try
      repeat

        if FTok.Test(ttEND) then
          break;

        if coSymbolDictionary in FCompilerOptions then
          ReadNameList(names, posArray)     // use overloaded version
        else
          ReadNameList(names);

        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

        Typ := ReadType('');
        for x := 0 to Names.Count - 1 do
        begin
          if TRecordSymbol(Result).Members.FindLocal(Names[x]) <> nil then
            FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_NameAlreadyExists, [Names[x]]);

          member := TMemberSymbol.Create(Names[x], Typ);
          TRecordSymbol(Result).AddMember(TMemberSymbol(member));

          // Add member symbols and positions
          if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(member, PosArray[x], [suDeclaration]);
        end;

      until not FTok.TestDelete(ttSEMI) or FTok.Test(ttEND);
    finally
      Names.Free;
    end;

    if not FTok.TestDelete(ttEND) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
  except
    // Removed added record symbols. Destroying object
    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Remove(Result);
    Result.Free;
    raise;
  end;
end;

function TdwsCompiler.ReadTry: TExceptionExpr;
var
   tryBlock : TExpr;
   tt : TTokenType;
   wasExcept : Boolean;
begin
   wasExcept := FIsExcept;
   FIsExcept := False;
   try
      tryBlock := ReadBlocks([ttFINALLY, ttEXCEPT], tt);
      tryBlock.Pos:=FTok.HotPos;
      if tt = ttEXCEPT then begin
         FIsExcept := True;
         Result := ReadExcept(tryBlock);
         // tryBlock is freed by ReadExcept in case of exception
      end else begin
         Result := TFinallyExpr.Create(FProg, FTok.HotPos);
         TExceptionExpr(Result).TryExpr := tryBlock;
         try
            TExceptionExpr(Result).HandlerExpr := ReadBlocks([ttEND], tt);
         except
            Result.Free;
            raise;
         end;
      end;
   finally
      FIsExcept := wasExcept;
   end;
end;

function TdwsCompiler.ReadRaise: TRaiseBaseExpr;
var
   exceptExpr : TNoPosExpr;
begin
  if FIsExcept and (FTok.Test(ttSEMI) or FTok.Test(ttEND)) then
    Result := TReraiseExpr.Create(FProg, FTok.HotPos)
  else
  begin
    exceptExpr := ReadExpr;
    try
      if not Assigned(exceptExpr.Typ) then
        FProg.Msgs.AddCompilerError(FTok.HotPos,CPE_TypeExpected);
      Result := TRaiseExpr.Create(FProg, FTok.HotPos, exceptExpr);
    except
      exceptExpr.Free;
      raise;
    end;
  end;
end;

function TdwsCompiler.ReadExcept(TryExpr: TExpr): TExceptExpr;
var
  tt: TTokenType;
  DoExpr: TExceptDoExpr;
  varName: string;
  ClassSym: TSymbol;
begin
  Result := TExceptExpr.Create(FProg, FTok.HotPos);
  try
    Result.TryExpr := TryExpr;
    if FTok.Test(ttON) then
    begin
      while FTok.TestDelete(ttON) do
      begin
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
        varName := FTok.GetToken.FString;
        FTok.KillToken;

        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

        ClassSym := ReadType('');
        if not FTok.TestDelete(ttDO) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

        DoExpr := TExceptDoExpr.Create(FProg, FTok.HotPos);
        try
          DoExpr.ExceptionVar := TDataSymbol.Create(varName, ClassSym);

          FProg.Table.AddSymbol(DoExpr.ExceptionVar);
          try
            DoExpr.DoBlockExpr := ReadBlock;
          finally
            FProg.Table.Remove(DoExpr.ExceptionVar);
          end;
        except
          DoExpr.Free;
          raise;
        end;

        Result.AddDoExpr(DoExpr);

        if not FTok.Test(ttEND) then
          if not FTok.TestDelete(ttSEMI) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
      end;

      if FTok.TestDelete(ttELSE) then
        Result.ElseExpr := ReadBlocks([ttEND], tt)
      else if not FTok.TestDelete(ttEND) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
    end
    else
      Result.HandlerExpr := ReadBlocks([ttEND], tt);
  except
    Result.Free;
    raise;
  end;
end;

// ReadExit
//
function TdwsCompiler.ReadExit: TNoResultExpr;
var
   gotParenthesis : Boolean;
   leftExpr : TDataExpr;
   assignExpr : TNoResultExpr;
   proc : TProcedure;
   exitPos : TScriptPos;
begin
   exitPos:=FTok.HotPos;
   if FTok.TestAny([ttEND, ttSEMI, ttELSE, ttUNTIL])<>ttNone then
      Result:=TExitExpr.Create(FProg, FTok.HotPos)
   else begin
      if not (FProg is TProcedure) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoResultRequired);
      gotParenthesis:=FTok.TestDelete(ttBLEFT);
      proc:=TProcedure(FProg);
      if proc.Func.Result=nil then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoResultRequired);
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

function TdwsCompiler.ReadType(const TypeName: string): TTypeSymbol;
var
  name: string;
  namePos: TScriptPos;
  sym : TSymbol;
begin
  if FTok.TestDelete(ttRECORD) then
    Result := ReadRecord(TypeName)
  else if FTok.TestDelete(ttARRAY) then
    Result := ReadArray(TypeName)
  else if FTok.TestDelete(ttCLASS) then begin
    if FTok.TestDelete(ttOF) then
      Result:=ReadClassOf(TypeName)
    else Result:=ReadClass(TypeName)
  end else if FTok.TestDelete(ttBLEFT) then
    Result := ReadEnumeration(TypeName)
  else if FTok.TestDelete(ttPROCEDURE) then
  begin
    Result := ReadProcDecl(fkProcedure,nil,False,True);
    Result.SetName(TypeName);
  end
  else if FTok.TestDelete(ttFUNCTION) then
  begin
    Result := ReadProcDecl(fkFunction,nil,False,True);
    Result.SetName(TypeName);
  end
  else if FTok.TestDelete(ttMETHOD) then
  begin
    Result := ReadProcDecl(fkMethod,nil,False,True);
    Result.SetName(TypeName);
  end
  else if FTok.TestName then
  begin
    name := FTok.GetToken.FString;
    namePos := FTok.HotPos;        // get the position before token is deleted
    FTok.KillToken;
    sym := FProg.Table.FindSymbol(name);
    Result := nil;

    if sym is TUnitSymbol then
    begin
      if not FTok.TestDelete(ttDOT) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
      if not FTok.TestName then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      name := FTok.GetToken.FString;
      FTok.KillToken;
      sym := TUnitSymbol(sym).Table.FindLocal(name);
    end;

    if not Assigned(sym) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_TypeUnknown, [name])
    else if not (sym is TTypeSymbol) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_InvalidType, [name])
    else
      Result := TTypeSymbol(sym); // TTypeSymbol(sym).BaseType ??
    // Create name symbol, e. g.: type a = integer;
    if TypeName <> '' then
      Result := TAliasSymbol.Create(TypeName, Result);

    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(Result, namePos);
  end
  else
  begin
    Result := nil;
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);
  end;

   // Ensure that unnamed symbols will be freed
   if Result.Name = '' then begin
      Assert(FProg.Table is TProgramSymbolTable);
      TProgramSymbolTable(FProg.Table).AddToDestructionList(Result);
   end;
end;

// ReadExpr
//
function TdwsCompiler.ReadExpr: TNoPosExpr;
var
   r: TNoPosExpr;
   tt: TTokenType;
   hotPos: TScriptPos;
   roeClass : TRelOpExprClass;
begin
   // Read left argument
   hotPos:=FTok.HotPos;
   Result := ReadExprAdd;
   try
      // Read operator
      while True do begin
         tt:=FTok.TestDeleteAny([ttEQ, ttNOTEQ, ttLESS, ttLESSEQ, ttGTR, ttGTREQ, ttIS, ttAS]);
         if tt=ttNone then Break;

         hotPos := FTok.HotPos;

         // Read right argument
         r:=ReadExprAdd;
         try
            if (Result.Typ is TClassSymbol) or (Result.Typ=FProg.TypNil) then begin
               case tt of
                  ttEQ, ttNOTEQ:
                     Result := TObjCmpExpr.CreateCmp(FProg, Result, r, tt = ttEQ);
                  ttIS: Result := TIsOpExpr.Create(FProg, Result, r);
                  ttAS: Result := TAsOpExpr.Create(FProg, Result, r);
               else
                  FProg.Msgs.AddCompilerStop(hotPos, CPE_InvalidOperands);
               end;
            end else begin
               roeClass:=FBinaryOperators.RelOperatorClassFor(tt, Result.Typ, r.Typ);
               if roeClass<>nil then
                  Result:=roeClass.Create(FProg, Result, r)
               else begin
                  FProg.Msgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                  // keep going
                  Result:=TRelOpExpr.Create(FProg, Result, r);
               end;
            end;
         except
            r.Free;
            raise;
         end;
         Result.TypeCheckNoPos(hotPos);
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExprAdd
//
function TdwsCompiler.ReadExprAdd: TNoPosExpr;
var
   right: TNoPosExpr;
   tt: TTokenType;
   hotPos: TScriptPos;
   exprClass : TBinaryOpExprClass;
begin
   // Read left argument
   Result := ReadExprMult;
   try

      while True do begin
         tt:=FTok.TestDeleteAny([ttPLUS, ttMINUS, ttOR, ttAND, ttXOR, ttSHL, ttSHR,
                                 ttIN, ttNOT]);
         if tt=ttNone then Break;

         hotPos := FTok.HotPos;

         if tt = ttNOT then begin

            if not FTok.TestDelete(ttIN) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_InExpected);
            Result := ReadExprIn(Result);
            Result := TNotExpr.Create(FProg, Result);

         end else if tt = ttIN then

            Result := ReadExprIn(Result)

         else begin
            // Read right argument
            right := ReadExprMult;
            try
               // Generate function and add left and right argument
               if (Result.Typ=nil) or (right.Typ=nil) then
                  FProg.Msgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands)
               else begin
                  exprClass:=FBinaryOperators.BinaryOperatorClassFor(tt, Result.Typ, right.Typ);
                  if exprClass=nil then begin
                     FProg.Msgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                     // fake result to keep compiler going and report further issues
                     Result:=TBinaryOpExpr.Create(FProg, Result, right);
                     Result.Typ:=FProg.TypVariant;
                  end else Result:=exprClass.Create(FProg, Result, right);
               end;
            except
               right.Free;
               raise;
            end;
         end;

         Result.TypeCheckNoPos(hotPos);
         if Optimize then
            Result:=Result.Optimize;
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExprMult
//
function TdwsCompiler.ReadExprMult: TNoPosExpr;
var
   right: TNoPosExpr;
   tt: TTokenType;
   hotPos: TScriptPos;
   exprClass : TBinaryOpExprClass;
begin
   // Read left argument
   Result := ReadTerm;
   try
      while True do begin
         tt:=FTok.TestDeleteAny([ttTIMES, ttDIVIDE, ttMOD, ttDIV]);
         if tt=ttNone then Break;

         // Save position of the operator
         hotPos := FTok.HotPos;

         // Read right argument
         right := ReadTerm;
         try

            // Generate function and add left and right argument
            if (Result.Typ=nil) or (right.Typ=nil) then
               FProg.Msgs.AddCompilerStop(hotPos, CPE_IncompatibleOperands)
            else begin
               exprClass:=FBinaryOperators.BinaryOperatorClassFor(tt, Result.Typ, right.Typ);
               if exprClass=nil then begin
                  FProg.Msgs.AddCompilerError(hotPos, CPE_InvalidOperands);
                  // fake result to keep compiler going and report further issues
                  Result:=TBinaryOpExpr.Create(FProg, Result, right);
                  Result.Typ:=right.Typ;
               end else Result:=exprClass.Create(FProg, Result, right);
            end;

         except
            right.Free;
            raise;
         end;

         Result.TypeCheckNoPos(hotPos);
         if Optimize then
            Result:=Result.Optimize;
      end;
   except
      Result.Free;
      raise;
   end;
end;

// ReadExprIn
//
function TdwsCompiler.ReadExprIn(var left : TNoPosExpr) : TInOpExpr;
var
   i : Integer;
   condList : TList;
   hotPos : TScriptPos;
begin
   if not FTok.TestDelete(ttALEFT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketLeftExpected);

   hotPos:=FTok.HotPos;

   Result:=nil;
   condList:=TList.Create;
   try
      try
         if not FTok.TestDelete(ttARIGHT) then begin
            ReadCaseConditions(condList, left);
            if not FTok.TestDelete(ttARIGHT) then
               FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
         end;

         Result:=TInOpExpr.Create(FProg, hotPos, left);
         left:=nil;

         // Add case conditions to TCaseExpr
         for i:=0 to condList.Count-1 do
            Result.AddCaseCondition(condList[i]);
         condList.Clear;

      except
         for i:=0 to condList.Count-1 do
            TCaseCondition(condList[i]).Free;
         raise;
      end;
   finally
      condList.Free;
   end;
end;

// ReadTerm
//
function TdwsCompiler.ReadTerm: TNoPosExpr;

   function ReadNilTerm : TNoPosExpr;
   const
      cNilIntf : IUnknown = nil;
   begin
      Result:=TConstExpr.CreateTyped(FProg, FProg.TypNil, cNilIntf);
   end;

   function ReadNotTerm : TNotExpr;
   begin
      Result:=TNotExpr.Create(FProg, ReadTerm);
      try
         Result.TypeCheckNoPos(FTok.HotPos);
      except
         Result.Free;
         raise;
      end;
   end;

var
   tt : TTokenType;
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
         Result:=ReadArrayConstant;
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
            Result:=ReadSymbol(Result);
      end;
      ttTRUE, ttFALSE :
         Result:=TConstBooleanExpr.CreateUnified(FProg, nil, (tt=ttTRUE));
      ttNIL :
         Result:=ReadNilTerm;
      ttSWITCH :
         Result:=ReadExprSwitch;
   else
      if FTok.Test(ttINHERITED) or FTok.TestName  then
         // Variable or Function
         Result := ReadName
      else // Constant values in the code
         Result := ReadConstValue;
   end;

   // No expression found
   if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);
end;

// ReadNegation
//
function TdwsCompiler.ReadNegation: TNoPosExpr;
var
   negExprClass : TNegExprClass;
   negTerm : TNoPosExpr;
begin
   negTerm:=ReadTerm;
   if negTerm.IsIntegerValue then
      negExprClass:=TNegIntExpr
   else if negTerm.IsFloatValue then
      negExprClass:=TNegFloatExpr
   else if negTerm.IsVariantValue then
      negExprClass:=TNegVariantExpr
   else negExprClass:=TNegExpr;
   Result:=negExprClass.Create(FProg, negTerm);
   try
      Result.TypeCheckNoPos(FTok.HotPos);
      if Optimize then
         Result:=Result.Optimize;
   except
      Result.Free;
      raise;
   end;
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

procedure TdwsCompiler.ReadArrayParams(ArrayIndices: TSymbolTable);
var
  x: Integer;
  names: TStringList;
  typSym: TSymbol;
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
          typSym := ReadType('');
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
procedure TdwsCompiler.ReadParams(Proc: TFuncSymbol; ParamsToDictionary: Boolean);
var
   i : Integer;
   names : TStringList;
   typ : TSymbol;
   lazyParam, varParam, constParam : Boolean;
   posArray : TScriptPosArray;
   sym : TParamSymbol;
   defaultExpr : TNoPosExpr;
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
               if ParamsToDictionary and (coSymbolDictionary in FCompilerOptions) then
                  ReadNameList(names, posArray)     // use overloaded version
               else ReadNameList(names);

               if not FTok.TestDelete(ttCOLON) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)
               else begin
                  defaultExpr := nil;
                  typ := ReadType('');
                  try
                     if (not constParam) and (typ is TOpenArraySymbol) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_OpenArrayParamMustBeConst);
                     if (typ is TDynamicArraySymbol) then
                        FMsgs.AddCompilerError(FTok.HotPos, CPE_OpenArrayParamElementsMustBeConst);

                     if FTok.TestDelete(ttEQ) then begin
                        if lazyParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_LazyParamCantHaveDefaultValue);
                        if varParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_VarParamCantHaveDefaultValue);
                        if constParam then
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstParamCantHaveDefaultValue);

                        defaultExpr := ReadExpr;

                        if not (defaultExpr is TConstExpr) then begin
                           FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
                           FreeAndNil(defaultExpr);
                        end;

                        if not Typ.IsCompatible(defaultExpr.Typ) then begin
                           FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_IncompatibleTypes,
                                                     [Typ.Caption,defaultExpr.Typ.Caption]);
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
                              TParamSymbolWithDefaultValue(sym).SetDefaultValue(TConstExpr(defaultExpr).Data,
                                                                                TConstExpr(defaultExpr).Addr);
                           end else begin
                              sym := TParamSymbol.Create(names[i], Typ);
                           end;
                        end;

                        Proc.AddParam(sym);

                        // Enter Field symbol in dictionary
                        if ParamsToDictionary and (coSymbolDictionary in FCompilerOptions) then begin
                           FProg.SymbolDictionary.Add(sym, posArray[i], [suDeclaration]);  // add variable symbol
                           FProg.SymbolDictionary.Add(Typ, FTok.HotPos);  // add type symbol
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
   oldTok : TTokenizer;
   i : Integer;
   conditionalTrue : Boolean;
   switchPos, condPos : TScriptPos;
   condExpr : TNoPosExpr;
begin
   Result := nil;

   switchPos:=FTok.HotPos;

   switch:=StringToSwitchInstruction(FTok.GetToken.FString);
   FTok.KillToken;

   case switch of
      siIncludeLong, siIncludeShort, siFilterLong, siFilterShort : begin

         if semiPending then
            FProg.Msgs.AddCompilerStop(switchPos, CPE_SemiExpected);

         if not FTok.Test(ttStrVal) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_IncludeFileExpected);
         name := FTok.GetToken.FString;
         FTok.KillToken;

         try
            oldTok := FTok;
            scriptSource := GetScriptSource(name);

            if switch in [siFilterLong, siFilterShort] then begin
               if Assigned(FFilter) then
                  // Include file is processed by the filter
                  FTok := TTokenizer.Create(FFilter.Process(scriptSource, FMsgs), name, FProg.Msgs)
               else FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoFilterAvailable);
            end else begin
               // Include file is included as-is
               FTok := TTokenizer.Create(scriptSource, name, FProg.Msgs);
            end;

            try
               FTok.SwitchHandler := ReadSwitch;
               Result := ReadScript(name, stInclude);
            finally
               Inc(FLineCount, FTok.CurrentPos.Line-2);
               FTok.Free;
               FTok := oldTok;
            end;
         except
            on e: EScriptError do
               raise;
            on e: Exception do
               FMsgs.AddCompilerStop(FTok.HotPos, e.Message);
         end;

      end;
      siDefine : begin

         if not FTok.Test(ttNAME) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         FProg.ConditionalDefines.Add(FTok.GetToken.FString);
         FTok.KillToken;

      end;
      siUndef : begin

         if not FTok.Test(ttNAME) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

         i:=FProg.ConditionalDefines.IndexOf(FTok.GetToken.FString);
         if i>=0 then
            FProg.ConditionalDefines.Delete(i);
         FTok.KillToken;

      end;
      siIfDef, siIfNDef, siIf : begin

         case switch of
            siIfDef, siIfNDef : begin
               if not FTok.Test(ttNAME) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
               conditionalTrue:=    (FProg.ConditionalDefines.IndexOf(FTok.GetToken.FString)>=0)
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
                     if not condExpr.IsBooleanValue then
                        FMsgs.AddCompilerStop(condPos, CPE_BooleanExpected);

                     conditionalTrue:=condExpr.EvalAsBoolean;
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
            FMsgs.AddCompilerError(FTok.HotPos, CPE_StringExpected)
         else begin
            case switch of
               siHint    : FMsgs.AddCompilerHint(switchPos, FTok.GetToken.FString);
               siWarning : FMsgs.AddCompilerWarning(switchPos, FTok.GetToken.FString);
               siError   : FMsgs.AddCompilerError(switchPos, FTok.GetToken.FString, TCompilerErrorMessage);
               siFatal   : FMsgs.AddCompilerStop(switchPos, FTok.GetToken.FString, TCompilerErrorMessage);
            end;
            FTok.KillToken;
         end;

      end
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
function TdwsCompiler.ReadExprSwitch : TNoPosExpr;
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
            value:=hotPos.SourceFile.SourceFile
         else if SameText(name, 'LINE') then
            value:=IntToStr(hotPos.Line)
         else if SameText(name, 'DATE') then
            value:=FormatDateTime('yyyy-mm-dd', Date)
         else if SameText(name, 'TIME') then
            value:=FormatDateTime('hh:nn:ss', Time)
         else if SameText(name, 'FUNCTION') then begin
            if FProg is TProcedure then begin
               funcSym:=TProcedure(FProg).Func;
               if funcSym is TMethodSymbol then
                  value:=TMethodSymbol(funcSym).ClassSymbol.Name+'.'+funcSym.Name
               else value:=funcSym.Name;
            end else value:='*Main*';
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

   if not Assigned(sym) and (FProg is TProcedure) then
      sym := TProcedure(FProg).Func.Params.FindLocal(Name);

   if Assigned(sym) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_NameAlreadyExists, [Name])
   else CheckSpecialName(Name);
end;

// IdentifySpecialName
//
function TdwsCompiler.IdentifySpecialName(const name: string) : TSpecialKeywordKind;
var
   ch : Char;
begin
   ch:=name[1];
   if (ch>='A') and (ch<='Z') then
      ch:=Char(Word(ch) or $0020);
   Result:=skNone;
   case ch of
      'a' :
         if SameText(name, 'assigned') then Result:=skAssigned;
      'd' :
         if SameText(name, 'defined') then Result:=skDefined
         else if SameText(name, 'declared') then Result:=skDeclared;
      'h' :
         if SameText(name, 'high') then Result:=skHigh;
      'l' :
         if SameText(name, 'low') then Result:=skLow
         else if SameText(name, 'length') then Result:=skLength;
      'o' :
         if SameText(name, 'ord') then Result:=skOrd;
      's' :
         if SameText(name, 'sizeof') then Result:=skSizeOf
         else if SameText(name, 'sqr') then Result:=skSqr;
   end;
end;

// CheckSpecialName
//
procedure TdwsCompiler.CheckSpecialName(const name: string);
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
    if CheckNames and not SameText(A[x].Name, B[x].Name) then
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
//    else if (A[x] is TVarParamSymbol) and (B[x] is TVarParamSymbol) and
//      (TVarParamSymbol(A[x]).IsWritable <> TVarParamSymbol(B[x]).IsWritable) then
//      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadParameterType,
//                                [x, A[x].Description, B[x].Description])
    else r := True;
    Result := Result and r;
  end;
end;

procedure TdwsCompiler.CompareFuncSymbols(A, B: TFuncSymbol; IsCheckingParameters:
  Boolean);
begin
  if A.Kind <> B.Kind then
  begin
    case A.Kind of
      fkFunction: FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionExpected);
      fkProcedure: FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureExpected);
      fkConstructor: FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstructorExpected);
      fkDestructor: FMsgs.AddCompilerStop(FTok.HotPos, CPE_DestructorExpected);
      fkMethod: FMsgs.AddCompilerStop(FTok.HotPos, CPE_MethodExpected);
    else
      Assert(False);
    end;
  end;

  if IsCheckingParameters then
  begin
    if Assigned(A.Typ) and not A.Typ.IsCompatible(B.Typ) then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadResultType, [A.Typ.Caption]);

    if A.Params.Count <> B.Params.Count then
      FMsgs.AddCompilerErrorFmt(FTok.HotPos, CPE_BadNumberOfParameters,
                                [A.Params.Count, B.Params.Count])
    else
      CheckParams(A.Params, B.Params, True);
  end;
end;

function TdwsCompiler.ReadConnectorSym(const Name: string;
  var BaseExpr: TNoPosExpr; const ConnectorType: IConnectorType; IsWrite: Boolean): TNoPosExpr;

  function TryConnectorCall: TConnectorCallExpr;
  begin
    // Try to read the call of a connector function
    Result := TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr,
      IsWrite);

    BaseExpr := nil;

    ReadFuncArgs(TConnectorCallExpr(Result).AddArg);

    if not TConnectorCallExpr(Result).AssignConnectorSym(ConnectorType) then
      FreeAndNil(Result);
  end;

begin
  if FTok.Test(ttALEFT) then begin
    Result := ReadConnectorArray(Name,BaseExpr,ConnectorType,IsWrite);
  end
  else if FTok.Test(ttBLEFT) then
  begin
    // Brackets -> always a function
    Result := TryConnectorCall;

    if not Assigned(Result) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorCall,
                               [Name, ConnectorType.ConnectorCaption]);
  end
  else if not IsWrite then
  begin
    // The assignment ":=" was already read.
    Result := TConnectorReadExpr.Create(FProg, FTok.HotPos, Name, BaseExpr);

    BaseExpr := nil;

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

    BaseExpr := nil;

    if not TConnectorWriteExpr(Result).AssignConnectorSym(ConnectorType) then
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

function TdwsCompiler.ReadConnectorArray(const Name: String; var BaseExpr: TNoPosExpr;
            const ConnectorType: IConnectorType; IsWrite: Boolean): TConnectorCallExpr;
begin
  Result := TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr, IsWrite, True);
  try
    BaseExpr := nil; // on Exception BaseExpr is freed by our Result!

    ReadFuncArgs(Result.AddArg,ttALEFT,ttARIGHT);

    if IsWrite and FTok.TestDelete(ttASSIGN) then
      Result.AddArg(ReadExpr);

    if not Result.AssignConnectorSym(ConnectorType) then
      FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_ConnectorIndex, [ConnectorType.ConnectorCaption]);
  except
    Result.Free;
    raise;
  end;
end;

{ TdwsConfiguration }

constructor TdwsConfiguration.Create(Owner: TComponent);
begin
  inherited Create;
  FOwner := Owner;
  FSystemTable := TStaticSymbolTable.Create;
  FConnectors := TStringList.Create;
  FScriptPaths := TStringList.Create;
  FConditionals := TStringList.Create;
  FUnits := TStringList.Create;
  InitSystemTable;
  FUnits.AddObject(SYS_INTERNAL, Pointer(IUnit(dwsInternalUnit)));
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
   clsObject, clsException, clsDelphiException : TClassSymbol;
   clsMeta : TClassOfSymbol;
   meth : TMethodSymbol;
   varSym : TBaseSymbol;
begin
   // Create base data types
   SystemTable.AddSymbol(TBaseSymbol.Create(SYS_BOOLEAN, typBooleanID, False));
   SystemTable.AddSymbol(TBaseSymbol.Create(SYS_FLOAT, typFloatID, 0.0));
   SystemTable.AddSymbol(TBaseSymbol.Create(SYS_INTEGER, typIntegerID, VarAsType(0, varInteger)));
   SystemTable.AddSymbol(TBaseSymbol.Create(SYS_STRING, typStringID, ''));

   varSym := TBaseSymbol.Create(SYS_VARIANT, typVariantID, Unassigned);
   SystemTable.AddSymbol(varSym);
   SystemTable.AddSymbol(TConstSymbol.Create('Null', varSym, Null));
   SystemTable.AddSymbol(TConstSymbol.Create('Unassigned', varSym, Unassigned));

   SystemTable.AddSymbol(TOpenArraySymbol.Create('array of const', varSym));

   // Create "root" class TObject
   clsObject := TClassSymbol.Create(SYS_TOBJECT);
   // Add constructor Create
   meth := TSourceMethodSymbol.Create(SYS_TOBJECT_CREATE, fkConstructor, clsObject);
   meth.Executable := ICallable(TEmptyFunc.Create);
   clsObject.AddMethod(meth);
   // Add destructor Destroy
   meth := TSourceMethodSymbol.Create(SYS_TOBJECT_DESTROY, fkDestructor, clsObject);
   meth.IsVirtual := True;
   meth.Executable := ICallable(TEmptyFunc.Create);
   clsObject.AddMethod(meth);
   // Add destructor Free
   meth := TSourceMethodSymbol.Create('Free', fkDestructor, clsObject);
   meth.Executable := ICallable(TEmptyFunc.Create);
   clsObject.AddMethod(meth);
   SystemTable.AddSymbol(clsObject);

   // Create "root" metaclass TObject
   clsMeta:=TClassOfSymbol.Create(SYS_TCLASS, clsObject);
   SystemTable.AddSymbol(clsMeta);

   // Create class Exception
   clsException := TClassSymbol.Create(SYS_EXCEPTION);
   clsException.InheritFrom(clsObject);
   clsException.AddField(TFieldSymbol.Create(SYS_EXCEPTION_MESSAGE,
    SystemTable.FindSymbol(SYS_STRING)));
   TExceptionCreateMethod.Create(mkConstructor, [], 0, SYS_TOBJECT_CREATE,
                                 ['Msg', SYS_STRING], '', clsException, SystemTable);
   SystemTable.AddSymbol(clsException);

   // Create class EDelphi
   clsDelphiException := TClassSymbol.Create(SYS_EDELPHI);
   clsDelphiException.InheritFrom(clsException);
   clsDelphiException.AddField(TFieldSymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS,
                                                   SystemTable.FindSymbol(SYS_STRING)));
   TDelphiExceptionCreateMethod.Create(mkConstructor, [], 0, SYS_TOBJECT_CREATE,
                                       ['Cls', SYS_STRING, 'Msg', SYS_STRING], '', clsDelphiException, SystemTable);
   SystemTable.AddSymbol(clsDelphiException);

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

{ TExceptionCreateMethod }

procedure TExceptionCreateMethod.Execute;
begin
  Info.ValueAsString[SYS_EXCEPTION_MESSAGE] := Info.ValueAsString['Msg'];
end;

{ TDelphiExceptionCreateMethod }

procedure TDelphiExceptionCreateMethod.Execute(var ExternalObject: TObject);
begin
  Info.ValueAsString[SYS_EXCEPTION_MESSAGE] := Info.ValueAsString['Msg'];
  Info.ValueAsVariant[SYS_EDELPHI_EXCEPTIONCLASS] := Info.ValueAsVariant['Cls']
end;

{ TParamFunc }

procedure TParamFunc.Execute;
begin
  Info.ResultAsVariant := Info.Caller.Parameters[Info.ValueAsInteger['Index']];
end;

{ TParamStrFunc }

procedure TParamStrFunc.Execute;
begin
  Info.ResultAsString := Info.Caller.Parameters[Info.ValueAsInteger['Index']];
end;

{ TParamCount }

procedure TParamCountFunc.Execute;
begin
  Info.ResultAsInteger := Length(Info.Caller.Parameters);
end;

// GetScriptSource
//
function TdwsCompiler.GetScriptSource(const scriptName : String) : String;
var
   stream : TStream;
   sl : TStringList;
begin
   Result:='';

   if Assigned(FOnInclude) then begin
      FOnInclude(ScriptName, Result);
      if Result<>'' then Exit;
   end;

   stream:=OpenStreamForFile(scriptName);
   try
      if stream=nil then
         FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_IncludeFileNotFound,
                                  [scriptName], TCompilerErrorMessage)
      else begin
         sl:=TStringList.Create;
         try
            sl.LoadFromStream(stream);
            Result:=sl.Text;
         finally
            sl.Free;
         end;
      end;
   finally
      stream.Free;
   end;
end;

function TdwsCompiler.ReadStringArray(Expr: TDataExpr; IsWrite: Boolean): TNoPosExpr;
var
   indexExpr, valueExpr: TNoPosExpr;
   pos: TScriptPos;
begin
   pos := FTok.HotPos;
   indexExpr := ReadExpr;
   try
      if not FTok.TestDelete(ttARIGHT) then
         FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);

      if FTok.TestDelete(ttASSIGN) and IsWrite then begin
         valueExpr:=ReadExpr;
         if Expr is TStrVarExpr then
            if valueExpr is TChrExpr then begin
               Result:=TVarStringArraySetChrExpr.Create(FProg, pos, Expr, indexExpr, TChrExpr(valueExpr).Expr);
               TChrExpr(valueExpr).Expr:=nil;
               valueExpr.Free;
            end else Result:=TVarStringArraySetExpr.Create(FProg, pos, Expr, indexExpr, valueExpr)
         else Result := TStringArraySetExpr.Create(FProg, pos, Expr, indexExpr, valueExpr);
      end else Result := TStringArrayOpExpr.CreatePos(FProg, pos, TDataExpr(Expr), indexExpr);
   except
      indexExpr.Free;
      raise;
   end;
end;

function TdwsCompiler.CreateProgram(SystemTable: TSymbolTable; ResultType: TdwsResultType;
                                    MaxDataSize, StackChunkSize: Integer;
                                    MaxRecursionDepth : Integer): TdwsProgram;
begin
  Result := TdwsProgram.Create(SystemTable, ResultType, MaxRecursionDepth,
                               MaxDataSize, StackChunkSize);
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

procedure TPrintFunction.Execute;
begin
   Info.Caller.Result.AddString(Info.ValueAsString['v']);
end;

{ TPrintLnFunction }

procedure TPrintLnFunction.Execute;
var
   result : TdwsResult;
begin
   result:=Info.Caller.Result;
   result.AddString(Info.ValueAsString['v']);
   result.AddString(#13#10);
end;

{ TdwsCompiler }

function TdwsCompiler.ReadEnumeration(const TypeName: string): TEnumerationSymbol;
var
  name: string;
  elemSym: TElementSymbol;
  constExpr: TNoPosExpr;
  enumInt: Integer;
  namePos: TScriptPos;
  isUserDef: Boolean;
begin
  Result := TEnumerationSymbol.Create(TypeName, FProg.TypInteger);
  try
    enumInt := 0;

    repeat
      // Read a member of the enumeration
      if not FTok.TestName then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      name := FTok.GetToken.FString;
      namePos := FTok.HotPos;
      FTok.KillToken;

      // Member has a user defined value
      if FTok.TestDelete(ttEQ) then
      begin
        constExpr := ReadExpr;

        if not(constExpr is TConstExpr) then
        begin
          FreeAndNil(constExpr);
          FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
        end
        else if not(constExpr.Typ = FProg.TypInteger) then
        begin
          FreeAndNil(constExpr);
          FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);
        end;

        if Assigned(constExpr) then
          enumInt := constExpr.Eval;

        isUserDef := True;
      end
      else
        isUserDef := False;

      // Create member symbol
      elemSym := TElementSymbol.Create(name, Result, enumInt, isUserDef);

      Inc(enumInt);

      // Add member symbol to table and enumeration type
      FProg.Table.AddSymbol(elemSym);
      Result.AddElement(elemSym);

      // Add member symbol to Symbol Dictionary
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(elemSym, namePos, [suDeclaration]);

    until not FTok.TestDelete(ttCOMMA);

    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

  except
    Result.Free;
    raise;
  end;
end;

procedure TdwsCompiler.ReadUses;
var
  Names : TStringList;
  x, y, z, u : Integer;
begin
  Names := TStringList.Create;
  try
    ReadNameList(Names);
    u := 0;
    for x := 0 to Names.Count - 1 do
    begin
      y := 0;
      z := -1;
      while (y < FProg.Root.RootTable.Count) do
      begin
        if (FProg.Root.RootTable[y] is TUnitSymbol) and SameText(FProg.Root.RootTable[y].Name,Names[x]) then
        begin
          z := FProg.Root.RootTable.IndexOfParent(TUnitSymbol(FProg.Root.RootTable[y]).Table);
          if z >= u then // uses A,B,A,C => uses A,B,C
          begin
            FProg.Root.RootTable.MoveParent(z,u);
            Inc(u);
          end;
          Break;
        end;
        Inc(y);
      end;
      if z < 0 then
        FMsgs.AddCompilerStopFmt(FTok.HotPos, CPE_UnknownUnit, [Names[x]]);
    end;
  finally
    Names.Free;
  end;
end;

function TdwsCompiler.CreateProcedure(Parent : TdwsProgram): TProcedure;
begin
  Result := TProcedure.Create(Parent);
end;

// CreateAssign
//
function TdwsCompiler.CreateAssign(const pos : TScriptPos; token : TTokenType;
                                   left : TDataExpr; right : TNoPosExpr) : TNoResultExpr;
var
   exprClass : TAssignExprClass;
   classOpSymbol : TClassOperatorSymbol;
   classOpExpr : TFuncExpr;
begin
   if Assigned(right.Typ) then begin

      case token of
         ttASSIGN : begin
            if left.Typ is TClassOfSymbol then begin
               Result := TAssignClassOfExpr.Create(FProg, pos, left, right);
            end else if (right is TDataExpr) and ((right.Typ.Size<>1) or (right.Typ is TArraySymbol)) then begin
               if right is TFuncExpr then
                  TFuncExpr(right).SetResultAddr;
               if right is TArrayConstantExpr then
                  Result := TAssignArrayConstantExpr.Create(FProg, pos, left, TArrayConstantExpr(right))
               else Result := TAssignDataExpr.Create(FProg, pos, left, right)
            end else begin
               Result:=TAssignExpr.Create(FProg, pos, left, right);
            end;
         end;
         ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN : begin
            if left.Typ is TClassSymbol then begin

               classOpSymbol:=(left.Typ as TClassSymbol).FindClassOperator(token, right.Typ);
               if classOpSymbol=nil then
                  FMsgs.AddCompilerStop(pos, CPE_IncompatibleOperands);
               classOpExpr:=GetMethodExpr(classOpSymbol.UsesSym, left, rkObjRef, pos, False);
               try
                  TFuncExpr(classOpExpr).AddArg(right);
               except
                  classOpExpr.Free;
                  raise;
               end;
               Result:=TNoResultWrapperExpr.Create(FProg, pos, classOpExpr);

            end else begin

               exprClass:=FBinaryOperators.AssignmentOperatorClassFor(token, left.Typ, right.Typ);
               if exprClass=nil then begin
                  FMsgs.AddCompilerError(pos, CPE_IncompatibleOperands);
                  exprClass:=TAssignExpr; // fake to keep compiler going
               end;
               Result:=exprClass.Create(FProg, pos, left, right);

            end;
         end;
      else
         Result:=nil;
         Assert(False);
      end;

      Result.TypeCheck;
      if Optimize then
         Result:=Result.OptimizeToNoResultExpr;

   end else begin

      FMsgs.AddCompilerStop(Pos, CPE_RightSideNeedsReturnType);
      Result:=nil;

   end;
end;

// ReadSpecialFunction
//
function TdwsCompiler.ReadSpecialFunction(const NamePos: TScriptPos; SpecialKind: TSpecialKeywordKind): TNoPosExpr;

   function EvaluateDefined(argExpr : TNoPosExpr) : Boolean;
   var
      name : String;
   begin
      argExpr.EvalAsString(name);
      Result:=(FProg.ConditionalDefines.IndexOf(name)>=0);
   end;

   function EvaluateDeclared(argExpr : TNoPosExpr) : Boolean;
   var
      name : String;
   begin
      argExpr.EvalAsString(name);
      Result:=(TDeclaredExpr.FindSymbol(FProg.Root.Table, name)<>nil);
   end;

var
   argExpr: TNoPosExpr;
   argTyp: TSymbol;
begin
   if not FTok.TestDelete(ttBLEFT) then
      FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

   // Test for statements like "Low(Integer)"
   if FTok.Test(ttName) and FTok.NextTest(ttBRIGHT) then
      argTyp := FProg.Table.FindSymbol(FTok.GetToken.FString)
   else argTyp := nil;

   if Assigned(argTyp) and (argTyp is TTypeSymbol) then begin
      argExpr := nil;
      FTok.KillToken;
      FTok.KillToken;
   end else begin
      argExpr := ReadExpr;
      argTyp := argExpr.BaseType;
   end;

   try
      if Assigned(argExpr) then
         argExpr.TypeCheckNoPos(FTok.HotPos);

      if not Assigned(argTyp) then
         FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);

      Result := nil;

      case SpecialKind of
         skAssigned: begin
            if argTyp is TClassSymbol then
               Result:=TAssignedInstanceExpr.Create(FProg, argExpr)
            else if argTyp is TClassOfSymbol then
               Result:=TAssignedMetaClassExpr.Create(FProg, argExpr)
            else FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
            argExpr:=nil;
         end;
         skHigh: begin
            if argTyp is TOpenArraySymbol then begin
               if argExpr=nil then
                  FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
               Result:=TOpenArrayLengthExpr.Create(FProg, TDataExpr(argExpr), -1);
               argExpr:=nil;
            end else if argTyp is TEnumerationSymbol then begin
               FreeAndNil(argExpr);
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TEnumerationSymbol(argTyp).HighBound)
            end else if argTyp is TDynamicArraySymbol and Assigned(argExpr) then begin
               Result:=TArrayLengthExpr.Create(FProg, TDataExpr(argExpr), -1);
               argExpr:=nil;
            end else if (argTyp = FProg.TypString) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp is TStaticArraySymbol then begin
               FreeAndNil(argExpr);
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TStaticArraySymbol(argTyp).HighBound);
            end else if argTyp=FProg.TypInteger then begin
               FreeAndNil(argExpr);
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, High(Int64));
            end else FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
         end;
         skLength: begin
            if argTyp is TOpenArraySymbol then begin
               if argExpr=nil then
                  FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
               Result:=TOpenArrayLengthExpr.Create(FProg, TDataExpr(argExpr), 0);
               argExpr:=nil;
            end else if (argTyp is TDynamicArraySymbol) and Assigned(argExpr) then begin
               Result:=TArrayLengthExpr.Create(FProg, TDataExpr(argExpr), 0);
               argExpr:=nil;
            end else if ((argTyp=FProg.TypString) or (argTyp=FProg.TypVariant)) and Assigned(argExpr) then begin
               Result:=TStringLengthExpr.Create(FProg, argExpr);
               argExpr:=nil;
            end else if argTyp is TStaticArraySymbol then begin
               FreeAndNil(argExpr);
               Result := TConstExpr.CreateTyped(FProg, FProg.TypInteger,
                                                TStaticArraySymbol(argTyp).ElementCount);
            end else FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
         end;
         skLow: begin
               FreeAndNil(argExpr);
            if argTyp is TStaticArraySymbol then
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TStaticArraySymbol(argTyp).LowBound)
            else if argTyp is TEnumerationSymbol then
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, TEnumerationSymbol(argTyp).LowBound)
            else if argTyp=FProg.TypString then
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, 1)
            else if (argTyp=FProg.TypInteger) then begin
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, Low(Int64))
            end else if (argTyp is TDynamicArraySymbol) then
               Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, 0)
            else FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
         end;
         skSqr : begin
            case argTyp.BaseTypeID of
               typIntegerID : begin
                  Result:=TSqrIntExpr.Create(FProg, argExpr);
                  argExpr:=nil;
               end;
               typFloatID : begin
                  if argExpr is TFloatVarExpr then
                     Result:=TSqrFloatVarExpr.Create(FProg, argExpr)
                  else Result:=TSqrFloatExpr.Create(FProg, argExpr);
                  argExpr:=nil;
               end;
            else
               FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_NumericalExpected);
            end;
         end;
         skOrd: begin
            case argTyp.BaseTypeID of
               typIntegerID, typBooleanID : begin
                  Result:=TOrdIntExpr.Create(FProg, argExpr);
                  argExpr:=nil;
               end;
               typStringID : begin
                  Result:=TOrdStrExpr.Create(FProg, argExpr);
                  argExpr:=nil;
               end;
               typVariantID : begin
                  Result:=TOrdExpr.Create(FProg, argExpr);
                  argExpr:=nil;
               end
            else
               FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
            end;
         end;
         skSizeOf : begin
             Result:=TConstExpr.CreateTyped(FProg, FProg.TypInteger, argTyp.Size);
             FreeAndNil(argExpr);
         end;
         skDefined, skDeclared : begin
            if not argExpr.IsStringValue then
               FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_StringExpected);
            if FIsSwitch then begin
               if not argExpr.IsConstant then
                  FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);
               try
                  case SpecialKind of
                     skDefined :
                        Result:=TConstBooleanExpr.CreateUnified(FProg, FProg.TypBoolean, EvaluateDefined(argExpr));
                     skDeclared :
                        Result:=TConstBooleanExpr.CreateUnified(FProg, FProg.TypBoolean, EvaluateDeclared(argExpr));
                  else
                     Assert(False);
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
               else
                  Assert(False);
               end;
               argExpr:=nil;
            end;
         end;
      end;

      try
         if not FTok.TestDelete(ttBRIGHT) then
            FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      except
         Result.Free;
         raise;
      end;

   except
      argExpr.Free;
      raise;
   end;
end;

// ReadTypeCast
//
function TdwsCompiler.ReadTypeCast(const NamePos: TScriptPos; TypeSym: TSymbol): TNoPosExpr;
var
   argExpr: TNoPosExpr;
   hotPos : TScriptPos;
begin
   if not FTok.TestDelete(ttBLEFT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

   hotPos:=FTok.CurrentPos;
   argExpr:=ReadExpr;

   Result:=nil;
   try
      if not FTok.TestDelete(ttBRIGHT) then
         FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

      if TypeSym = FProg.TypInteger then
         Result := TConvIntegerExpr.Create(FProg, argExpr)
      // Cast Float(...)
      else if TypeSym = FProg.TypFloat then
         Result := TConvFloatExpr.Create(FProg, argExpr)
      // Cast Variant(...)
      else if TypeSym = FProg.TypVariant then
         Result := TConvVariantExpr.Create(FProg, argExpr)
      else
         FProg.Msgs.AddCompilerStop(hotPos, CPE_InvalidOperands);

      argExpr:=nil;
      try
         Result.TypeCheckNoPos(hotPos);
      except
         Result.Free;
         raise;
      end;

  except
    argExpr.Free;
    raise;
  end;
end;

end.

