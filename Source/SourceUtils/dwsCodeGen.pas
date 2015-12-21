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
{    Eric Grange                                                       }
{                                                                      }
{**********************************************************************}
unit dwsCodeGen;

{$I dws.inc}

interface

uses
   Classes, SysUtils,
   dwsUtils, dwsSymbols, dwsExprs, dwsCoreExprs, dwsDataContext, dwsConstExprs,
   dwsStrings, dwsUnitSymbols, dwsErrors, dwsRTTIFunctions;

   // experimental codegen support classes for DWScipt

type

   TdwsCodeGen = class;
   TdwsExprCodeGen = class;

   TdwsMappedSymbol = record
      Symbol : TSymbol;
      Name : String;
   end;

   TdwsMappedSymbolHash = class(TSimpleHash<TdwsMappedSymbol>)
      protected
         function SameItem(const item1, item2 : TdwsMappedSymbol) : Boolean; override;
         function GetItemHashCode(const item1 : TdwsMappedSymbol) : Integer; override;
   end;

   TdwsCodeGenSymbolScope = (cgssGlobal, cgssClass, cgssLocal, cgssNoMap);

   TdwsCodeGenSymbolMaps = class;

   TdwsCodeGenSymbolMap = class (TRefCountedObject)
      private
         FParent : TdwsCodeGenSymbolMap;
         FSymbol : TSymbol;
         FHash : TdwsMappedSymbolHash;
         FMaps : TdwsCodeGenSymbolMaps;
         FNames : TSimpleNameObjectHash<TSymbol>;
         FLookup : TdwsMappedSymbol;
         FReservedSymbol : TSymbol;
         FPrefix : String;
         FCodeGen : TdwsCodeGen;

      protected
         function DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : String; virtual;

         procedure RaiseAlreadyDefined(sym, existing : TSymbol);

      public
         constructor Create(aCodeGen : TdwsCodeGen; aParent : TdwsCodeGenSymbolMap; aSymbol : TSymbol);
         destructor Destroy; override;

         function SymbolToName(symbol : TSymbol) : String;
         function NameToSymbol(const name : String; scope : TdwsCodeGenSymbolScope) : TSymbol;

         procedure ReserveName(const name : String); inline;
         procedure ReserveExternalName(sym : TSymbol);

         function IsReserved(const name : String) : Boolean; inline;
         function IsMapped(const sym : TSymbol) : Boolean;

         function MapSymbol(symbol : TSymbol; scope : TdwsCodeGenSymbolScope; canObfuscate : Boolean) : String;

         property CodeGen : TdwsCodeGen read FCodeGen;
         property Maps : TdwsCodeGenSymbolMaps read FMaps write FMaps;
         property Parent : TdwsCodeGenSymbolMap read FParent;
         property Prefix : String read FPrefix write FPrefix;
         property Symbol : TSymbol read FSymbol;
   end;

   TdwsCodeGenSymbolMaps = class(TObjectList<TdwsCodeGenSymbolMap>)
      private

      protected

      public
         function MapOf(symbol : TSymbol) : TdwsCodeGenSymbolMap;
   end;

   TdwsRegisteredCodeGen = class (TRefCountedObject)
      public
         Expr : TExprBaseClass;
         CodeGen : TdwsExprCodeGen;

         destructor Destroy; override;
   end;

   TdwsRegisteredCodeGenList = class(TSortedList<TdwsRegisteredCodeGen>)
      protected
         function Compare(const item1, item2 : TdwsRegisteredCodeGen) : Integer; override;
   end;

   TdwsCodeGenOption = (cgoNoRangeChecks, cgoNoCheckInstantiated, cgoNoCheckLoopStep,
                        cgoNoConditions, cgoNoInlineMagics, cgoObfuscate, cgoNoSourceLocations,
                        cgoOptimizeForSize, cgoSmartLink, cgoDeVirtualize,
                        cgoNoRTTI, cgoNoFinalizations, cgoIgnorePublishedInImplementation );
   TdwsCodeGenOptions = set of TdwsCodeGenOption;

   TdwsCodeGenOutputVerbosity = (cgovNone, cgovNormal, cgovVerbose);

   TdwsCodeGenDependencies = class
      private
         FList : TStringList;

      public
         constructor Create;
         destructor Destroy; override;

         procedure Add(const dep : String);
         procedure Clear;

         function Contains(const dep : String) : Boolean;

         property List : TStringList read FList;
   end;

   TdwsCustomCodeGenEvent = function (expr : TExprBase) : TdwsExprCodeGen of object;

   TdwsCodeGen = class
      private
         FCodeGenList : TdwsRegisteredCodeGenList;
         FOutput : TWriteOnlyBlockStream;
         FOutputLine : Integer;
         FDependencies : TdwsCodeGenDependencies;
         FFlushedDependencies : TStringList;
         FTempReg : TdwsRegisteredCodeGen;

         FLocalTable : TSymbolTable;
         FTableStack : TTightStack;

         FSymbolDictionary, FSuspendedDictionary : TdwsSymbolDictionary;
         FSourceContextMap : TdwsSourceContextMap;

         FContext : TdwsProgram;
         FContextStack : TTightStack;
         FContextSymbolDictionary : TdwsSymbolDictionary;

         FLocalVarSymbolMap : TStringList;
         FLocalVarSymbolMapStack : TTightStack;

         FRootSymbolMap : TdwsCodeGenSymbolMap;
//         FSymbolMap : TdwsCodeGenSymbolMap;
//         FSymbolMaps : TdwsCodeGenSymbolMaps;
//         FSymbolMapStack : TTightStack;
         FMappedUnits : TTightList;
         FOutputLineOffset : Integer;

         FTempSymbolCounter : Integer;
         FCompiledClasses : TTightList;
         FCompiledUnits : TTightList;
         FIndent : Integer;
         FIndentString : String;
         FNeedIndent : Boolean;
         FIndentSize : Integer;
         FOptions : TdwsCodeGenOptions;
         FVerbosity : TdwsCodeGenOutputVerbosity;
         FTryDepth : Integer;

         FDataContextPool : IDataContextPool;

         FOnCustomCodeGen : TdwsCustomCodeGenEvent;

      protected
         property SymbolDictionary : TdwsSymbolDictionary read FSymbolDictionary;

         procedure EnterContext(proc : TdwsProgram); virtual;
         procedure EnumerateOnEnterContext(parent, expr : TExprBase; var abort : Boolean);
         procedure LeaveContext; virtual;

         function CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap; virtual;

//         procedure EnterScope(symbol : TSymbol);
//         procedure LeaveScope;
//         function  EnterStructScope(struct : TCompositeTypeSymbol) : Integer;
//         procedure LeaveScopes(n : Integer);
//         function  IsScopeLevel(symbol : TSymbol) : Boolean;

         procedure RaiseUnknowExpression(expr : TExprBase);

         function  SmartLink(symbol : TSymbol) : Boolean; virtual;

         function  SmartLinkMethod(meth : TMethodSymbol) : Boolean; virtual;

         procedure SmartLinkFilterOutSourceContext(context : TdwsSourceContext);
         procedure SmartLinkFilterSymbolTable(table : TSymbolTable; var changed : Boolean); virtual;
         procedure SmartLinkUnaliasSymbolTable(table : TSymbolTable); virtual;
         procedure SmartLinkFilterStructSymbol(structSymbol : TCompositeTypeSymbol; var changed : Boolean); virtual;
         procedure SmartLinkFilterInterfaceSymbol(intfSymbol : TInterfaceSymbol; var changed : Boolean); virtual;
         procedure SmartLinkFilterMemberFieldSymbol(fieldSymbol : TFieldSymbol; var changed : Boolean); virtual;

         procedure SuspendSmartLink;
         procedure ResumeSmartLink;

         procedure DeVirtualize;

         procedure DoCompileCompositeSymbolInternals(struct : TCompositeTypeSymbol); virtual;
         procedure DoCompileHelperSymbol(helper : THelperSymbol); virtual;
         procedure DoCompileRecordSymbol(rec : TRecordSymbol); virtual;
         procedure DoCompileClassSymbol(cls : TClassSymbol); virtual;
         procedure DoCompileFuncSymbol(func : TFuncSymbol; deAnonymize : Boolean = False); virtual;
         procedure DoCompileUnitSymbol(un : TUnitMainSymbol); virtual;

         procedure CompileRTTIRawAttributes(attributes : TdwsSymbolAttributes); virtual;

         procedure MapStructuredSymbol(structSym : TCompositeTypeSymbol; canObfuscate : Boolean);

         function DoCustomCodeGen(expr : TExprBase) : TdwsExprCodeGen; virtual;

         property Output : TWriteOnlyBlockStream read FOutput write FOutput;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
         function  FindCodeGen(expr : TExprBase) : TdwsExprCodeGen; overload;
         function  FindCodeGen(exprClass : TExprBaseClass) : TdwsExprCodeGen; overload;
         function  FindSymbolAtStackAddr(stackAddr, level : Integer) : TDataSymbol; deprecated;
         function  SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String; virtual;

         procedure NotifyCompileExpr(expr : TExprBase); virtual;

         procedure Compile(expr : TExprBase);
         procedure CompileStatement(expr : TExprBase);
         procedure CompileNoWrap(expr : TTypedExpr);
         procedure CompileValue(expr : TTypedExpr); virtual;

         procedure CompileSymbolTable(table : TSymbolTable); virtual;
         procedure CompileUnitSymbol(un : TUnitMainSymbol);
         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); virtual;
         procedure CompileFuncSymbol(func : TFuncSymbol; deAnonymize : Boolean = False);
         procedure CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                     preConds : Boolean); virtual;
         procedure CompileHelperSymbol(helper : THelperSymbol);
         procedure CompileRecordSymbol(rec : TRecordSymbol);
         procedure CompileClassSymbol(cls : TClassSymbol);
         procedure CompileClassSymbolIfNeeded(cls : TClassSymbol);
         procedure CompileInterfaceSymbol(intf : TInterfaceSymbol);
         procedure BeforeCompileProgram(table, systemTable : TSymbolTable; unitSyms : TUnitMainSymbols);
         procedure CompileProgram(const prog : IdwsProgram); virtual;
         procedure CompileProgramInSession(const prog : IdwsProgram); virtual;
         procedure CompileProgramBody(expr : TProgramExpr); virtual;

         procedure BeginProgramSession(const prog : IdwsProgram); virtual;
         procedure EndProgramSession; virtual;

         procedure ReserveSymbolNames; virtual;
         procedure MapInternalSymbolNames(progTable, systemTable : TSymbolTable); virtual;
         procedure MapPrioritySymbolNames(table : TSymbolTable); virtual;
         procedure MapNormalSymbolNames(table : TSymbolTable); virtual;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;
         procedure CompileResourceStrings(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;

         procedure WriteIndent;
         procedure WriteIndentIfNeeded;
         procedure Indent(needIndent : Boolean = True);
         procedure UnIndent(needIndent : Boolean = True);

         procedure WriteString(const s : String); overload;
         procedure WriteString(const c : WideChar); overload;
         procedure WriteStringLn(const s : String);
         procedure WriteLineEnd;
         procedure WriteStatementEnd; virtual;
         procedure WriteBlockBegin(const prefix : String); virtual;
         procedure WriteBlockEnd; virtual;
         procedure WriteBlockEndLn;
         procedure WriteLiteralString(const s : String); virtual; abstract;
         procedure WriteFloat(const v : Double; const fmt : TFormatSettings); virtual; abstract;
         procedure WriteInteger(v : Int64);

         procedure WriteSymbolName(sym : TSymbol; scope : TdwsCodeGenSymbolScope = cgssGlobal);

         procedure WriteSymbolVerbosity(sym : TSymbol); virtual;

         function LocationString(e : TExprBase) : String;
         function IncTempSymbolCounter : Integer;
         function GetNewTempSymbol : String; virtual;

         procedure EnterTry; inline;
         procedure LeaveTry; inline;
         property TryDepth : Integer read FTryDepth;

         procedure WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;
         function  CompiledOutput(const prog : IdwsProgram) : String;
         procedure FlushDependencies;

         procedure Clear; virtual;
         procedure ClearOutput; virtual;

         procedure CreateDataContext(const data : TData; addr : Integer; var result : IDataContext);

         property Context : TdwsProgram read FContext;
         property ContextSymbolDictionary : TdwsSymbolDictionary read FContextSymbolDictionary;

         property LocalTable : TSymbolTable read FLocalTable write FLocalTable;
         property SymbolMap : TdwsCodeGenSymbolMap read FRootSymbolMap;
         property OutputLineOffset : Integer read FOutputLineOffset write FOutputLineOffset;
         property OutputLine : Integer read FOutputLine;

         property IndentSize : Integer read FIndentSize write FIndentSize;
         property Options : TdwsCodeGenOptions read FOptions write FOptions;
         property Verbosity : TdwsCodeGenOutputVerbosity read FVerbosity write FVerbosity;

         property Dependencies : TdwsCodeGenDependencies read FDependencies;
         property FlushedDependencies : TStringList read FFlushedDependencies;

         property OnCustomCodeGen : TdwsCustomCodeGenEvent read FOnCustomCodeGen write FOnCustomCodeGen;
   end;

   TdwsExprCodeGen = class abstract
      private
         FOwner : TdwsCodeGen;

      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); virtual;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); virtual;

         class function ExprIsConstantInteger(expr : TExprBase; value : Integer) : Boolean; static;

         property Owner : TdwsCodeGen read FOwner;
   end;

   ECodeGenException = class (Exception);
   ECodeGenUnknownExpression = class (ECodeGenException);
   ECodeGenUnsupportedSymbol = class (ECodeGenException);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsRegisteredCodeGen ------------------
// ------------------

// Destroy
//
destructor TdwsRegisteredCodeGen.Destroy;
begin
   inherited;
   CodeGen.Free;
end;

// ------------------
// ------------------ TdwsRegisteredCodeGenList ------------------
// ------------------

// Compare
//
function TdwsRegisteredCodeGenList.Compare(const item1, item2 : TdwsRegisteredCodeGen) : Integer;
var
   i1, i2 : Integer;
begin
   i1:=NativeInt(item1.Expr);
   i2:=NativeInt(item2.Expr);
   if i1<i2 then
      Result:=-1
   else if i1=i2 then
      Result:=0
   else Result:=1;
end;

// ------------------
// ------------------ TdwsCodeGen ------------------
// ------------------

// Create
//
constructor TdwsCodeGen.Create;
begin
   inherited;
   FCodeGenList:=TdwsRegisteredCodeGenList.Create;
   FOutput:=TWriteOnlyBlockStream.Create;
   FOutputLine:=1;
   FDependencies:=TdwsCodeGenDependencies.Create;
   FFlushedDependencies:=TStringList.Create;
   FFlushedDependencies.Sorted:=True;
   FFlushedDependencies.Duplicates:=dupIgnore;
   FTempReg:=TdwsRegisteredCodeGen.Create;
//   FSymbolMaps:=TdwsCodeGenSymbolMaps.Create;
   FIndentSize:=3;
   FDataContextPool:=TDataContextPool.Create;
end;

// Destroy
//
destructor TdwsCodeGen.Destroy;
begin
   Clear;
//   FSymbolMapStack.Free;
//   FMappedUnits.Free;
//   FSymbolMaps.Free;
   FRootSymbolMap.Free;
   FTempReg.Free;
   FDependencies.Free;
   FFlushedDependencies.Free;
   FOutput.Free;
   FCodeGenList.Clean;
   FCodeGenList.Free;
   FTableStack.Free;
   FContextStack.Free;
   FCompiledClasses.Free;
   FCompiledUnits.Free;
   FLocalVarSymbolMapStack.Free;
   inherited;
end;

// RegisterCodeGen
//
procedure TdwsCodeGen.RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
var
   reg : TdwsRegisteredCodeGen;
begin
   reg:=TdwsRegisteredCodeGen.Create;
   reg.Expr:=expr;
   reg.CodeGen:=codeGen;
   FCodeGenList.Add(reg);
   codeGen.FOwner:=Self;
end;

// FindCodeGen
//
function TdwsCodeGen.FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
begin
   Result:=DoCustomCodeGen(expr);
   if Result<>nil then Exit;

   Result:=FindCodeGen(TExprBaseClass(expr.ClassType));
end;

// FindCodeGen
//
function TdwsCodeGen.FindCodeGen(exprClass : TExprBaseClass) : TdwsExprCodeGen;
var
   i : Integer;
begin
   FTempReg.Expr:=exprClass;
   if FCodeGenList.Find(FTempReg, i) then
      Result:=FCodeGenList.Items[i].CodeGen
   else Result:=nil;
end;

// FindSymbolAtStackAddr
//
function TdwsCodeGen.FindSymbolAtStackAddr(stackAddr, level : Integer) : TDataSymbol;
var
   i : Integer;
   funcSym : TFuncSymbol;
   dataSym : TDataSymbol;
begin
   if (Context is TdwsProcedure) then begin
      funcSym:=TdwsProcedure(Context).Func;
      Result:=funcSym.Result;
      if (Result<>nil) and (Result.StackAddr=stackAddr) then
         Exit;
   end;

   for i:=0 to FLocalVarSymbolMap.Count-1 do begin
      dataSym:=TDataSymbol(FLocalVarSymbolMap.Objects[i]);
      if (dataSym.StackAddr=stackAddr) and (dataSym.Level=level) then begin
         Result:=dataSym;
         Exit;
      end;
   end;

   if FLocalTable=nil then Exit(nil);
   Result:=FLocalTable.FindSymbolAtStackAddr(stackAddr, level);
end;

// SymbolMappedName
//
function TdwsCodeGen.SymbolMappedName(sym : TSymbol; scope : TdwsCodeGenSymbolScope) : String;
var
//   i : Integer;
   meth : TMethodSymbol;
   funcSym : TFuncSymbol;
   fieldSym : TFieldSymbol;
begin
   funcSym:=sym.AsFuncSymbol;
   if funcSym<>nil then begin
      if funcSym.IsExternal or funcSym.HasExternalName then
         Exit(funcSym.ExternalName);
      if funcSym is TMethodSymbol then begin
         meth:=TMethodSymbol(funcSym);
         while meth.IsOverride do
            meth:=meth.ParentMeth;
         if meth.StructSymbol.IsExternalRooted then
            Exit(meth.ExternalName)
         else sym:=meth;
      end;
   end else if sym is TDataSymbol then begin
      if TDataSymbol(sym).HasExternalName then
         Exit(TDataSymbol(sym).ExternalName);
   end else if sym is TClassSymbol then begin
      if TClassSymbol(sym).ExternalRoot<>nil then
         Exit(TClassSymbol(sym).ExternalName);
   end else if sym is TFieldSymbol then begin
      fieldSym:=TFieldSymbol(sym);
      if fieldSym.HasExternalName or fieldSym.StructSymbol.IsExternalRooted then
         Exit(TFieldSymbol(sym).ExternalName);
   end else if sym is TBaseSymbol then begin
      if sym.ClassType=TBaseIntegerSymbol then
         Result:='Number'
      else if sym.ClassType=TBaseFloatSymbol then
         Result:='Number'
      else if sym.ClassType=TBaseStringSymbol then
         Result:='String'
      else if sym.ClassType=TBaseBooleanSymbol then
         Result:='Boolean'
      else if sym.ClassType=TBaseVariantSymbol then
         Result:='Object'
      else Assert(False);
      Exit;
   end;
   Result:=FRootSymbolMap.SymbolToName(sym);
   if Result<>'' then Exit;
//   for i:=0 to FSymbolMaps.Count-1 do begin
//      Result:=FSymbolMaps[i].SymbolToName(sym);
//      if Result<>'' then Exit;
//   end;
   Result:=FRootSymbolMap.MapSymbol(sym, scope, True);
end;

// NotifyCompileExpr
//
procedure TdwsCodeGen.NotifyCompileExpr(expr : TExprBase);
begin
   // nothing
end;

// Clear
//
procedure TdwsCodeGen.Clear;
begin
   ClearOutput;
   FDependencies.Clear;
   FFlushedDependencies.Clear;

   FLocalTable:=nil;
   FTableStack.Clear;
   FContext:=nil;
   FContextStack.Clear;
   FCompiledClasses.Clear;
   FCompiledUnits.Clear;
   FMappedUnits.Clear;

//   FSymbolMapStack.Clear;
//   FSymbolMap:=nil;
//   FSymbolMaps.Clear;
//   EnterScope(nil);
   FreeAndNil(FRootSymbolMap);

   FIndent:=0;
   FIndentString:='';
   FNeedIndent:=False;

   FTempSymbolCounter:=0;

   FDataContextPool.Cleanup;
end;

// ClearOutput
//
procedure TdwsCodeGen.ClearOutput;
begin
   FOutputLine:=1;
   FOutput.Clear;
end;

// CreateDataContext
//
procedure TdwsCodeGen.CreateDataContext(const data : TData; addr : Integer; var result : IDataContext);
begin
   result:=FDataContextPool.Create(data, addr);
end;

// Compile
//
procedure TdwsCodeGen.Compile(expr : TExprBase);
var
   cg : TdwsExprCodeGen;
   oldTable : TSymbolTable;
begin
   if expr=nil then Exit;
   cg:=FindCodeGen(expr);
   if cg=nil then
      RaiseUnknowExpression(expr);

   NotifyCompileExpr(expr);

   if expr.InheritsFrom(TBlockExpr) then begin
      FTableStack.Push(FLocalTable);
      oldTable:=FLocalTable;
      FLocalTable:=TBlockExpr(expr).Table;
      try
         cg.CodeGen(Self, expr);
      finally
         FLocalTable:=oldTable;
         FTableStack.Pop;
      end;
   end else begin
      cg.CodeGen(Self, expr);
   end;
end;

// CompileStatement
//
procedure TdwsCodeGen.CompileStatement(expr : TExprBase);
begin
   Compile(expr);
   if expr is TNoResultExpr then
   else WriteStatementEnd;
end;

// CompileNoWrap
//
procedure TdwsCodeGen.CompileNoWrap(expr : TTypedExpr);
var
   cg : TdwsExprCodeGen;
begin
   cg:=FindCodeGen(expr);
   if cg=nil then
      RaiseUnknowExpression(expr);

   cg.CodeGenNoWrap(Self, expr)
end;

// CompileValue
//
procedure TdwsCodeGen.CompileValue(expr : TTypedExpr);
begin
   Compile(expr);
end;

// CompileSymbolTable
//
procedure TdwsCodeGen.CompileSymbolTable(table : TSymbolTable);
var
   sym : TSymbol;
begin
   for sym in table do begin
      if sym is TSourceFuncSymbol then begin
         if sym.Name<>'' then
            CompileFuncSymbol(TSourceFuncSymbol(sym))
      end else if sym is TEnumerationSymbol then
         CompileEnumerationSymbol(TEnumerationSymbol(sym))
      else if sym is TRecordSymbol then
         CompileRecordSymbol(TRecordSymbol(sym))
      else if sym is TClassSymbol then begin
         if FCompiledClasses.IndexOf(sym)<0 then
            CompileClassSymbol(TClassSymbol(sym));
      end else if sym is THelperSymbol then begin
         CompileHelperSymbol(THelperSymbol(sym))
      end else if sym is TInterfaceSymbol then begin
         CompileInterfaceSymbol(TInterfaceSymbol(sym))
      end;
   end;
end;

// CompileUnitSymbol
//
procedure TdwsCodeGen.CompileUnitSymbol(un : TUnitMainSymbol);
begin
   if FCompiledUnits.IndexOf(un)>=0 then Exit;
   FCompiledUnits.Add(un);

//   EnterScope(un);
   DoCompileUnitSymbol(un);
//   LeaveScope;
end;

// DoCompileUnitSymbol
//
procedure TdwsCodeGen.DoCompileUnitSymbol(un : TUnitMainSymbol);
var
   oldTable : TSymbolTable;
begin
   CompileSymbolTable(un.Table);

   oldTable:=FLocalTable;
   FLocalTable:=un.ImplementationTable;
   try
      CompileSymbolTable(un.ImplementationTable);
   finally
      FLocalTable:=oldTable;
   end;
end;

// CompileRTTIRawAttributes
//
procedure TdwsCodeGen.CompileRTTIRawAttributes(attributes : TdwsSymbolAttributes);
begin
   // nothing
end;

// CompileEnumerationSymbol
//
procedure TdwsCodeGen.CompileEnumerationSymbol(enum : TEnumerationSymbol);
begin
   // nothing
end;

// CompileFuncSymbol
//
procedure TdwsCodeGen.CompileFuncSymbol(func : TFuncSymbol; deAnonymize : Boolean = False);
var
   execSelf : TObject;
   proc : TdwsProcedure;
begin
   // nil executable means it's a function pointer type
   if func.Executable=nil then Exit;
   execSelf:=func.Executable.GetSelf;
   if not (execSelf is TdwsProcedure) then Exit;

   if     (func.Name<>'')
      and (not func.IsExport)
      and not SmartLink(func) then Exit;

   proc:=TdwsProcedure(execSelf);

//   EnterScope(func);
   EnterContext(proc);
   try
      DoCompileFuncSymbol(func, deAnonymize);
   finally
      LeaveContext;
//      LeaveScope;
   end;
end;

// DoCompileFuncSymbol
//
procedure TdwsCodeGen.DoCompileFuncSymbol(func : TFuncSymbol; deAnonymize : Boolean = False);
var
   proc : TdwsProcedure;
begin
   proc:=(func.Executable.GetSelf as TdwsProcedure);

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PreConditions, True);

   Assert(func.SubExprCount=2);
   Compile(func.SubExpr[0]);
   Compile(func.SubExpr[1]);

   if not (cgoNoConditions in Options) then
      CompileConditions(func, proc.PostConditions, False);
end;

// CompileConditions
//
procedure TdwsCodeGen.CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                        preConds : Boolean);
begin
   // nothing
end;

// CompileHelperSymbol
//
procedure TdwsCodeGen.CompileHelperSymbol(helper : THelperSymbol);
//var
//   changed : Boolean;
begin
   if helper.IsExternal then Exit;

//...   SmartLinkFilterStructSymbol(helper, changed);

   DoCompileCompositeSymbolInternals(helper);

   DoCompileHelperSymbol(helper);
end;

// CompileRecordSymbol
//
procedure TdwsCodeGen.CompileRecordSymbol(rec : TRecordSymbol);
var
   changed : Boolean;
begin
   if rec.IsExternal then Exit;

   SmartLinkFilterStructSymbol(rec, changed);

   if SmartLink(rec) then
      DoCompileCompositeSymbolInternals(rec);

   DoCompileRecordSymbol(rec);
end;

// CompileClassSymbol
//
procedure TdwsCodeGen.CompileClassSymbol(cls : TClassSymbol);
var
   changed : Boolean;
begin
   if cls.IsExternal then Exit;

   SmartLinkFilterStructSymbol(cls, changed);

   if SmartLink(cls) then
      DoCompileCompositeSymbolInternals(cls);

//   EnterScope(cls);
   try
      DoCompileClassSymbol(cls);
   finally
//      LeaveScope;
   end;
end;

// CompileClassSymbolIfNeeded
//
procedure TdwsCodeGen.CompileClassSymbolIfNeeded(cls : TClassSymbol);
begin
   if FCompiledClasses.IndexOf(cls)<0 then
      CompileClassSymbol(cls);
end;

// CompileInterfaceSymbol
//
procedure TdwsCodeGen.CompileInterfaceSymbol(intf : TInterfaceSymbol);
var
   sym : TSymbol;
   funcSym : TFuncSymbol;
begin
   for sym in intf.Members do begin
      if sym.Name='' then begin
         funcSym:=sym.AsFuncSymbol;
         if funcSym<>nil then
            CompileFuncSymbol(funcSym, True);
      end;
   end;
end;

// BeforeCompileProgram
//
procedure TdwsCodeGen.BeforeCompileProgram(table, systemTable : TSymbolTable; unitSyms : TUnitMainSymbols);
var
   i : Integer;
   changed : Boolean;
begin
   if FRootSymbolMap=nil then
      FRootSymbolMap:=CreateSymbolMap(nil, nil);

   ReserveSymbolNames;
   MapInternalSymbolNames(table, systemTable);

   if FSymbolDictionary<>nil then begin

      SmartLinkUnaliasSymbolTable(table);
      for i:=0 to unitSyms.Count-1 do begin
         SmartLinkUnaliasSymbolTable(unitSyms[i].Table);
         SmartLinkUnaliasSymbolTable(unitSyms[i].ImplementationTable);
      end;

      DeVirtualize;

      SmartLinkFilterSymbolTable(table, changed);

      repeat
         changed:=False;
         for i:=0 to unitSyms.Count-1 do begin
            SmartLinkFilterSymbolTable(unitSyms[i].Table, changed);
            SmartLinkFilterSymbolTable(unitSyms[i].ImplementationTable, changed);
         end;
      until not changed;
   end;

   for i:=0 to unitSyms.Count-1 do
      MapPrioritySymbolNames(unitSyms[i].Table);

   MapPrioritySymbolNames(table);
   MapNormalSymbolNames(table);
end;

// DoCompileCompositeSymbolInternals
//
procedure TdwsCodeGen.DoCompileCompositeSymbolInternals(struct : TCompositeTypeSymbol);
begin
   // nothing by default
end;

// DoCompileHelperSymbol
//
procedure TdwsCodeGen.DoCompileHelperSymbol(helper : THelperSymbol);
begin
   // nothing by default
end;

// DoCompileRecordSymbol
//
procedure TdwsCodeGen.DoCompileRecordSymbol(rec : TRecordSymbol);
begin
   // nothing by default
end;

// DoCompileClassSymbol
//
procedure TdwsCodeGen.DoCompileClassSymbol(cls : TClassSymbol);
begin
   if FCompiledClasses.IndexOf(cls.Parent)<0 then begin
      if     (cls.Parent.Name<>'TObject')
         and (cls.Parent.Name<>'Exception')
         and (cls.Parent.Name<>'RTTIPropertyAttribute') then
         CompileClassSymbol(cls.Parent);
   end;
   FCompiledClasses.Add(cls);
end;

// CompileProgram
//
procedure TdwsCodeGen.CompileProgram(const prog : IdwsProgram);
var
   p : TdwsMainProgram;
begin
   p:=prog.ProgramObject;

   if (cgoSmartLink in Options) and (prog.SymbolDictionary.Count>0) then begin
      FSymbolDictionary:=prog.SymbolDictionary;
      FSourceContextMap:=prog.SourceContextMap;
   end else begin
      FSymbolDictionary:=nil;
      FSourceContextMap:=nil;
   end;

   p.ResourceStringList.ComputeIndexes;

   BeginProgramSession(prog);
   try
      BeforeCompileProgram(prog.Table, p.SystemTable.SymbolTable, p.UnitMains);

      CompileProgramInSession(prog);
   finally
      EndProgramSession;
   end;
end;

// CompileProgramInSession
//
procedure TdwsCodeGen.CompileProgramInSession(const prog : IdwsProgram);
var
   p : TdwsMainProgram;
   i : Integer;
   rttiCompiled : Boolean;
begin
   p:=prog.ProgramObject;

   for i:=0 to p.UnitMains.Count-1 do
      CompileUnitSymbol(p.UnitMains[i]);

   CompileSymbolTable(p.Table);

   if Dependencies.Contains(SYS_RTTIRAWATTRIBUTES) then begin
      rttiCompiled:=True;
      CompileRTTIRawAttributes(prog.ProgramObject.Attributes);
   end else rttiCompiled:=False;

   Compile(p.InitExpr);

   if (not rttiCompiled) and Dependencies.Contains(SYS_RTTIRAWATTRIBUTES) then
      CompileRTTIRawAttributes(prog.ProgramObject.Attributes);

   if not (p.Expr is TNullExpr) then
      CompileProgramBody(p.Expr);

   if (p.FinalExpr<>nil) and not (cgoNoFinalizations in Options) then
      CompileProgramBody(p.FinalExpr);
end;

// BeginProgramSession
//
procedure TdwsCodeGen.BeginProgramSession(const prog : IdwsProgram);
var
   p : TdwsProgram;
begin
   FContextSymbolDictionary:=prog.SymbolDictionary;

//   if FSymbolMap=nil then
//      EnterScope(nil);

   p:=prog.ProgramObject;
   EnterContext(p);
end;

// EndProgramSession
//
procedure TdwsCodeGen.EndProgramSession;
begin
   LeaveContext;
   FContextSymbolDictionary:=nil;
end;

// ReserveSymbolNames
//
procedure TdwsCodeGen.ReserveSymbolNames;
begin
   // nothing
end;

// MapStructuredSymbol
//
procedure TdwsCodeGen.MapStructuredSymbol(structSym : TCompositeTypeSymbol; canObfuscate : Boolean);
var
   sym : TSymbol;
//   n : Integer;
   changed : Boolean;
begin
//   if FSymbolMaps.MapOf(structSym)<>nil then Exit;
   if FRootSymbolMap.IsMapped(structSym) then Exit;

   SmartLinkFilterStructSymbol(structSym, changed);

   if structSym.Parent<>nil then
      MapStructuredSymbol(structSym.Parent, canObfuscate);

//   if (structSym.UnitSymbol<>nil) and not IsScopeLevel(structSym.UnitSymbol) then begin
//      EnterScope(structSym.UnitSymbol);
//      SymbolMap.MapSymbol(structSym, cgssGlobal, canObfuscate);
//      LeaveScope;
//   end else SymbolMap.MapSymbol(structSym, cgssGlobal, canObfuscate);
   FRootSymbolMap.MapSymbol(structSym, cgssGlobal, canObfuscate);

//   n:=EnterStructScope(structSym);
   if structSym is TRecordSymbol then begin
      for sym in structSym.Members do begin
         if (sym is TMethodSymbol) and (TMethodSymbol(sym).IsClassMethod) then
            FRootSymbolMap.MapSymbol(sym, cgssGlobal, canObfuscate)
         else FRootSymbolMap.MapSymbol(sym, cgssGlobal, canObfuscate);
      end;
   end else begin
      for sym in structSym.Members do
         FRootSymbolMap.MapSymbol(sym, cgssGlobal, canObfuscate);
   end;
//   LeaveScopes(n);
end;

// DoCustomCodeGen
//
function TdwsCodeGen.DoCustomCodeGen(expr : TExprBase) : TdwsExprCodeGen;
begin
   if Assigned(FOnCustomCodeGen) then begin
      Result:=FOnCustomCodeGen(expr);
      if Result<>nil then Exit;
   end else Result:=nil;
end;

// MapInternalSymbolNames
//
procedure TdwsCodeGen.MapInternalSymbolNames(progTable, systemTable : TSymbolTable);

   procedure MapSymbolTable(table : TSymbolTable);
   var
      i : Integer;
      sym : TSymbol;
      funcSym : TFuncSymbol;
   begin
      if table.ClassType=TLinkedSymbolTable then
         table:=TLinkedSymbolTable(table).ParentSymbolTable;
      for i:=0 to table.Count-1 do begin
         sym:=table.Symbols[i];
         if sym is TStructuredTypeSymbol then begin
            if (sym is TClassSymbol) or (sym is TRecordSymbol) then begin
               MapStructuredSymbol(TStructuredTypeSymbol(sym), False);
            end else if sym is TInterfaceSymbol then
               FRootSymbolMap.MapSymbol(sym, cgssGlobal, False)
            else Assert(False);
         end else begin
            funcSym:=sym.AsFuncSymbol;
            if funcSym<>nil then
               FRootSymbolMap.MapSymbol(funcSym, cgssGlobal, False)
            else if sym is TDataSymbol then
               FRootSymbolMap.MapSymbol(sym, cgssGlobal, False);
         end;
      end;
   end;

var
   u : TUnitSymbol;
begin
   MapSymbolTable(systemTable);
   u:=TUnitSymbol(progTable.FindSymbol(SYS_INTERNAL, cvMagic, TUnitSymbol));
   if u<>nil then
      MapSymbolTable(u.Table);
   u:=TUnitSymbol(progTable.FindSymbol(SYS_DEFAULT, cvMagic, TUnitSymbol));
   if u<>nil then
      MapSymbolTable(u.Table);
end;

// MapPrioritySymbolNames
//
procedure TdwsCodeGen.MapPrioritySymbolNames(table : TSymbolTable);
var
   sym : TSymbol;
   unitSym : TUnitSymbol;
   funcSym : TFuncSymbol;
begin
   for sym in table do begin
      if sym is TUnitSymbol then begin
         unitSym:=TUnitSymbol(sym);
         if unitSym.Table is TStaticSymbolTable then
            MapPrioritySymbolNames(unitSym.Table);
      end else if sym is TClassSymbol then begin
         if TClassSymbol(sym).IsExternal then begin
            FRootSymbolMap.ReserveExternalName(sym);
         end;
      end else begin
         funcSym:=sym.AsFuncSymbol;
         if funcSym<>nil then
            if funcSym.IsExternal then
               FRootSymbolMap.ReserveExternalName(funcSym);
      end;
   end;
end;

// MapNormalSymbolNames
//
procedure TdwsCodeGen.MapNormalSymbolNames(table : TSymbolTable);
var
   sym : TSymbol;
   unitSym : TUnitMainSymbol;
   funcSym : TFuncSymbol;
begin
   for sym in table do begin
      if sym is TUnitSymbol then begin
         unitSym:=TUnitSymbol(sym).Main;
         if unitSym=nil then continue;
         if not (unitSym.Table is TStaticSymbolTable) then begin
            if FMappedUnits.IndexOf(unitSym)<0 then begin
               FMappedUnits.Add(unitSym);
//               EnterScope(unitSym);
               MapNormalSymbolNames(unitSym.Table);
               MapNormalSymbolNames(unitSym.ImplementationTable);
//               LeaveScope;
            end;
         end;
      end else if (sym is TClassSymbol) or (sym is TRecordSymbol) then begin
         MapStructuredSymbol(TStructuredTypeSymbol(sym), True);
      end else if sym is TInterfaceSymbol then begin
         FRootSymbolMap.MapSymbol(sym, cgssGlobal, True);
      end else begin
         funcSym:=sym.AsFuncSymbol;
         if funcSym<>nil then begin
            FRootSymbolMap.MapSymbol(funcSym, cgssGlobal, True);
            if     (funcSym.Executable<>nil)
               and (funcSym.Executable.GetSelf is TdwsProcedure) then
               MapNormalSymbolNames((funcSym.Executable.GetSelf as TdwsProcedure).Table);
         end else if sym is TDataSymbol then begin
            FRootSymbolMap.MapSymbol(sym, cgssGlobal, True);
         end;
      end;
   end;
end;

// CompileProgramBody
//
procedure TdwsCodeGen.CompileProgramBody(expr : TProgramExpr);
begin
   Compile(expr);
end;

// CompileDependencies
//
procedure TdwsCodeGen.CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   // nothing
end;

// CompileResourceStrings
//
procedure TdwsCodeGen.CompileResourceStrings(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   // nothing
end;

// WriteIndent
//
procedure TdwsCodeGen.WriteIndent;
begin
   FOutput.WriteString(FIndentString);
end;

// WriteIndentIfNeeded
//
procedure TdwsCodeGen.WriteIndentIfNeeded;
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
end;

// Indent
//
procedure TdwsCodeGen.Indent(needIndent : Boolean = True);
begin
   Inc(FIndent);
   FIndentString:=StringOfChar(' ', FIndent*FIndentSize);
   FNeedIndent:=needIndent;
end;

// UnIndent
//
procedure TdwsCodeGen.UnIndent(needIndent : Boolean = True);
begin
   Dec(FIndent);
   FIndentString:=StringOfChar(' ', FIndent*FIndentSize);
   FNeedIndent:=needIndent;
end;

// WriteString
//
procedure TdwsCodeGen.WriteString(const s : String);
var
   i : Integer;
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
   for i:=1 to Length(s) do
      if s[i]=#10 then
         Inc(FOutputLine);
   FOutput.WriteString(s);
end;

// WriteString
//
procedure TdwsCodeGen.WriteString(const c : WideChar);
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
   if c=#10 then
      Inc(FOutputLine);
   FOutput.WriteChar(c);
end;

// WriteStringLn
//
procedure TdwsCodeGen.WriteStringLn(const s : String);
begin
   WriteString(s);
   WriteLineEnd;
end;

// WriteLineEnd
//
procedure TdwsCodeGen.WriteLineEnd;
begin
   Inc(FOutputLine);
   FOutput.WriteString(#13#10);
   FNeedIndent:=True;
end;

// WriteStatementEnd
//
procedure TdwsCodeGen.WriteStatementEnd;
begin
   WriteStringLn(';');
end;

// WriteBlockBegin
//
procedure TdwsCodeGen.WriteBlockBegin(const prefix : String);
begin
   WriteString(prefix);
   WriteStringLn('{');
   Indent;
end;

// WriteBlockEnd
//
procedure TdwsCodeGen.WriteBlockEnd;
begin
   UnIndent;
   WriteString('}');
end;

// WriteBlockEndLn
//
procedure TdwsCodeGen.WriteBlockEndLn;
begin
   WriteBlockEnd;
   WriteLineEnd;
end;

// WriteSymbolName
//
procedure TdwsCodeGen.WriteSymbolName(sym : TSymbol; scope : TdwsCodeGenSymbolScope = cgssGlobal);
begin
   WriteString(SymbolMappedName(sym, scope));
end;

// WriteSymbolVerbosity
//
procedure TdwsCodeGen.WriteSymbolVerbosity(sym : TSymbol);
begin
   // nothing by default
end;

// LocationString
//
function TdwsCodeGen.LocationString(e : TExprBase) : String;
begin
   if Context is TdwsMainProgram then
      Result:=e.ScriptPos.AsInfo
   else Result:=' in '+e.ScriptLocation(Context);
end;

// IncTempSymbolCounter
//
function TdwsCodeGen.IncTempSymbolCounter : Integer;
begin
   Inc(FTempSymbolCounter);
   Result:=FTempSymbolCounter;
end;

// GetNewTempSymbol
//
function TdwsCodeGen.GetNewTempSymbol : String;
begin
   Inc(FTempSymbolCounter);
   Result:=IntToStr(FTempSymbolCounter);
end;

// EnterTry
//
procedure TdwsCodeGen.EnterTry;
begin
   Inc(FTryDepth);
end;

// LeaveTry
//
procedure TdwsCodeGen.LeaveTry;
begin
   Dec(FTryDepth);
   Assert(FTryDepth>=0);
end;

// WriteCompiledOutput
//
procedure TdwsCodeGen.WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   CompileResourceStrings(dest, prog);
   CompileDependencies(dest, prog);

   FOutputLineOffset:=StrCountChar(dest.ToString, #10);

   dest.WriteString(FOutput.ToString);
end;

// CompiledOutput
//
function TdwsCodeGen.CompiledOutput(const prog : IdwsProgram) : String;
var
   buf : TWriteOnlyBlockStream;
begin
   buf:=TWriteOnlyBlockStream.Create;
   try
      WriteCompiledOutput(buf, prog);
      Result:=buf.ToString;
   finally
      buf.Free;
   end;
end;

// FlushDependencies
//
procedure TdwsCodeGen.FlushDependencies;
begin
   FFlushedDependencies.Assign(FDependencies.List);
   FDependencies.Clear;
end;

// EnterContext
//
procedure TdwsCodeGen.EnterContext(proc : TdwsProgram);
var
   i : Integer;
   sym : TSymbol;
begin
   FTableStack.Push(FLocalTable);
   FContextStack.Push(FContext);
   FLocalTable:=proc.Table;
   FContext:=proc;

   FLocalVarSymbolMapStack.Push(TRefCountedObject(FLocalVarSymbolMap));
   FLocalVarSymbolMap:=TStringList.Create;
   for i:=0 to FLocalTable.Count-1 do begin
      sym:=FLocalTable.Symbols[i];
      if sym is TDataSymbol then
         FLocalVarSymbolMap.AddObject(sym.Name, sym);
   end;
   proc.Expr.RecursiveEnumerateSubExprs(EnumerateOnEnterContext);
end;

// EnumerateOnEnterContext
//
procedure TdwsCodeGen.EnumerateOnEnterContext(parent, expr : TExprBase; var abort : Boolean);
var
   i, k, n : Integer;
   sym : TSymbol;
   locName : String;
begin
   if not (expr is TBlockExpr) then Exit;
   for i:=0 to TBlockExpr(expr).Table.Count-1 do begin
      sym:=TBlockExpr(expr).Table.Symbols[i];
      if sym is TDataSymbol then begin
         if FLocalVarSymbolMap.IndexOf(sym.Name)>=0 then begin
            n:=1;
            repeat
               locName:=Format('%s_%d', [sym.Name, n]);
               k:=FLocalVarSymbolMap.IndexOf(locName);
               Inc(n);
            until k<0;
            FLocalVarSymbolMap.AddObject(locName, sym);
            //FSymbolMap.AddObject(locName, sym);
         end else begin
            FLocalVarSymbolMap.AddObject(sym.Name, sym);
         end;
      end;
   end;
end;

// LeaveContext
//
procedure TdwsCodeGen.LeaveContext;
begin
   FLocalTable:=TSymbolTable(FTableStack.Peek);
   FTableStack.Pop;
   FContext:=TdwsProgram(FContextStack.Peek);
   FContextStack.Pop;

   FLocalVarSymbolMap.Free;
   FLocalVarSymbolMap:=TStringList(FLocalVarSymbolMapStack.Peek);
   FLocalVarSymbolMapStack.Pop;
end;

// CreateSymbolMap
//
function TdwsCodeGen.CreateSymbolMap(parentMap : TdwsCodeGenSymbolMap; symbol : TSymbol) : TdwsCodeGenSymbolMap;
begin
   Result:=TdwsCodeGenSymbolMap.Create(Self, FRootSymbolMap, symbol);
end;

// EnterScope
//
//procedure TdwsCodeGen.EnterScope(symbol : TSymbol);
//var
//   map : TdwsCodeGenSymbolMap;
//begin
//   FSymbolMapStack.Push(FSymbolMap);
//   if symbol is TUnitSymbol then
//      symbol:=TUnitSymbol(symbol).Main;
//   map:=FSymbolMaps.MapOf(symbol);
//   if map=nil then begin
//      FSymbolMap:=CreateSymbolMap(FSymbolMap, symbol);
//      FSymbolMap.Maps:=FSymbolMaps;
//      FSymbolMaps.Add(FSymbolMap);
//   end else begin
//      map.FParent:=FSymbolMap;
//      FSymbolMap:=map;
//   end;
//end;

// LeaveScope
//
//procedure TdwsCodeGen.LeaveScope;
//begin
//   Assert(FSymbolMap<>nil);
//   FSymbolMap.FParent:=nil;
//   FSymbolMap:=TdwsCodeGenSymbolMap(FSymbolMapStack.Peek);
//   FSymbolMapStack.Pop;
//end;

// EnterStructScope
//
//function TdwsCodeGen.EnterStructScope(struct : TCompositeTypeSymbol) : Integer;
//begin
//   if struct<>nil then begin
//      Result:=EnterStructScope(struct.Parent)+1;
//      EnterScope(struct);
//   end else Result:=0;
//end;

// LeaveScopes
//
//procedure TdwsCodeGen.LeaveScopes(n : Integer);
//begin
//   while n>0 do begin
//      LeaveScope;
//      Dec(n);
//   end;
//end;

// IsScopeLevel
//
//function TdwsCodeGen.IsScopeLevel(symbol : TSymbol) : Boolean;
//var
//   m : TdwsCodeGenSymbolMap;
//begin
//   m:=FSymbolMap;
//   while m<>nil do begin
//      if m.Symbol=symbol then Exit(True);
//      m:=m.Parent;
//   end;
//   Result:=False;
//end;

// RaiseUnknowExpression
//
procedure TdwsCodeGen.RaiseUnknowExpression(expr : TExprBase);
begin
   raise ECodeGenUnknownExpression.CreateFmt('%s: unknown expression class %s at %s',
                                             [ClassName, expr.ClassName, expr.ScriptLocation(Context)]);
end;

// SmartLink
//
function TdwsCodeGen.SmartLink(symbol : TSymbol): Boolean;

   function IsReferenced(symbol : TSymbol) : Boolean;
   var
      list : TSymbolPositionList;
   begin
      list:=FSymbolDictionary.FindSymbolPosList(symbol);
      Result:=(list<>nil) and (list.FindAnyUsage([suReference, suRTTI])<>nil);
   end;

begin
   Result:=(FSymbolDictionary=nil) or IsReferenced(symbol);
end;

// SmartLinkMethod
//
//  TSub = class (TBase)
//
function TdwsCodeGen.SmartLinkMethod(meth : TMethodSymbol) : Boolean;
var
   i : Integer;
   symPos : TSymbolPositionList;
   lookup : TMethodSymbol;
   isUsed : Boolean;
begin
   Result:=SmartLink(meth);
   if Result then Exit;

   // interfaces aren't smart-linked yet
   if meth.IsInterfaced then Exit(True);
   // constructors/destructors aren't smart-linked yet
   if meth.Kind in [fkConstructor, fkDestructor] then Exit(True);
   // virtual class methods aren't smart-linked yet
   if meth.IsClassMethod and meth.IsVirtual then Exit(True);
   // published methods stay unless NoRTTI is set
   if (meth.Visibility=cvPublished) and not (cgoNoRTTI in Options) then Exit(True);

   // regular resolution works for non-virtual methods
   if not meth.IsVirtual then Exit;

   // the virtual method should be included if itself or any
   // of its overrides are used
   // filtering of unused subclasses is assumed to have been made already
   for i:=0 to FSymbolDictionary.Count-1 do begin
      symPos:=FSymbolDictionary.Items[i];
      // only check methods
      if not (symPos.Symbol is TMethodSymbol) then continue;

      lookup:=TMethodSymbol(symPos.Symbol);
      // quick filter on vmt index
      if (not lookup.IsVirtual) or (lookup.VMTIndex<>meth.VMTIndex) then continue;

      // is it an override of our method?
      while (lookup<>meth) and (lookup<>nil) do begin
         lookup:=lookup.ParentMeth;
      end;
      if (lookup=nil) then continue;

      // is it used anywhere?
      isUsed:=(symPos.FindUsage(suReference)<>nil);
      lookup:=TMethodSymbol(symPos.Symbol);
      while (lookup<>nil) and (not isUsed) do begin
         isUsed:=   lookup.IsInterfaced
                 or (FSymbolDictionary.FindSymbolUsage(lookup, suReference)<>nil);
         lookup:=lookup.ParentMeth;
      end;

      if isUsed then Exit(True);
   end;
   WriteStringLn('// IGNORED: '+meth.StructSymbol.Name+'.'+meth.Name);
   Result:=False;
end;

// SmartLinkFilterOutSourceContext
//
procedure TdwsCodeGen.SmartLinkFilterOutSourceContext(context : TdwsSourceContext);
begin
   FSymbolDictionary.RemoveInRange(context.StartPos, context.EndPos);
end;

// SmartLinkFilterSymbolTable
//
procedure TdwsCodeGen.SmartLinkFilterSymbolTable(table : TSymbolTable; var changed : Boolean);

   procedure RemoveReferencesInContextMap(symbol : TSymbol);
   begin
      if FSourceContextMap=nil then Exit;
      FSourceContextMap.EnumerateContextsOfSymbol(symbol, SmartLinkFilterOutSourceContext);
   end;

var
   sym : TSymbol;
   localChanged : Boolean;
   funcSym : TFuncSymbol;
begin
   if FSymbolDictionary=nil then Exit;

   repeat
      localChanged:=False;
      for sym in table do begin
         if sym is TStructuredTypeSymbol then begin

            if sym is TInterfaceSymbol then
               SmartLinkFilterInterfaceSymbol(TInterfaceSymbol(sym), localChanged)
            else if not (sym is THelperSymbol) then
               SmartLinkFilterStructSymbol(TStructuredTypeSymbol(sym), localChanged);

         end else begin

            funcSym:=sym.AsFuncSymbol;
            if funcSym<>nil then begin

               if funcSym.IsExternal or funcSym.IsType or funcSym.IsExport then continue;
               if not SmartLink(funcSym) then begin
                  if FSymbolDictionary.FindSymbolPosList(funcSym)<>nil then begin
                     RemoveReferencesInContextMap(funcSym);
                     FSymbolDictionary.Remove(funcSym);
                     localChanged:=True;
                  end;
               end;

            end;

         end;
      end;
      changed:=changed or localChanged;
   until not localChanged;
end;

// SmartLinkUnaliasSymbolTable
//
procedure TdwsCodeGen.SmartLinkUnaliasSymbolTable(table : TSymbolTable);
var
   sym : TSymbol;
   unaliased : TTypeSymbol;
   symPosListAlias, symPosListUnAliased : TSymbolPositionList;
   symPos : TSymbolPosition;
   i : Integer;
begin
   if FSymbolDictionary=nil then Exit;

   for sym in table do begin
      if sym is TAliasSymbol then begin

         unaliased:=TAliasSymbol(sym).UnAliasedType;

         symPosListAlias:=FSymbolDictionary.FindSymbolPosList(sym);
         if symPosListAlias.Count>0 then begin
            symPosListUnAliased:=FSymbolDictionary.FindSymbolPosList(unaliased);
            if symPosListUnAliased<>nil then begin
               for i:=0 to symPosListAlias.Count-1 do begin
                  symPos:=symPosListAlias[i];
                  if suReference in symPos.SymbolUsages then
                     symPosListUnAliased.Add(symPos.ScriptPos, symPos.SymbolUsages);
               end;
               symPosListAlias.Clear;
            end;
         end;

      end;
   end;
end;

// SmartLinkFilterStructSymbol
//
procedure TdwsCodeGen.SmartLinkFilterStructSymbol(structSymbol : TCompositeTypeSymbol; var changed : Boolean);

   procedure RemoveReferencesInContextMap(symbol : TSymbol);
   begin
      if FSourceContextMap=nil then Exit;
      FSourceContextMap.EnumerateContextsOfSymbol(symbol, SmartLinkFilterOutSourceContext);
   end;

var
   i : Integer;
   member : TSymbol;
   method : TMethodSymbol;
   prop : TPropertySymbol;
   localChanged : Boolean;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
   selfReferencedOnly, foundSelf : Boolean;
   srcContext : TdwsSourceContext;
begin
   if FSymbolDictionary=nil then Exit;

   symPosList:=FSymbolDictionary.FindSymbolPosList(structSymbol);
   if symPosList=nil then Exit;

   // remove unused field members
   for member in structSymbol.Members do begin
      if member is TFieldSymbol then begin
         if FSymbolDictionary.FindSymbolPosList(member)<>nil then
            SmartLinkFilterMemberFieldSymbol(TFieldSymbol(member), changed);
      end;
   end;

   // is symbol only referenced by its members?
   selfReferencedOnly:=True;
   for i:=0 to symPosList.Count-1 do begin
      symPos:=symPosList[i];
      if suReference in symPos.SymbolUsages then begin
         srcContext:=FSourceContextMap.FindContext(symPos.ScriptPos);
         foundSelf:=False;
         while srcContext<>nil do begin
            if srcContext.ParentSym=structSymbol then begin
               foundSelf:=True;
               Break;
            end;
            if     (srcContext.ParentSym is TMethodSymbol)
               and (TMethodSymbol(srcContext.ParentSym).StructSymbol=structSymbol) then begin
               foundSelf:=True;
               Break;
            end;
            srcContext:=srcContext.Parent;
         end;
         if not foundSelf then begin
            selfReferencedOnly:=False;
            Break;
         end;
      end;
   end;
   if selfReferencedOnly then begin
      FSymbolDictionary.Remove(structSymbol);
      RemoveReferencesInContextMap(structSymbol);
      for member in structSymbol.Members do begin
         RemoveReferencesInContextMap(member);
         FSymbolDictionary.Remove(member);
      end;
      changed:=True;
      Exit;
   end;

   // remove members cross-references
   repeat
      localChanged:=False;
      for member in structSymbol.Members do begin

         if member is TPropertySymbol then begin

            prop:=TPropertySymbol(member);
            if prop.Visibility=cvPublished then continue;

         end else if member is TMethodSymbol then begin

            method:=TMethodSymbol(member);
            if    method.IsVirtual or method.IsInterfaced
               or (method.Kind=fkConstructor) then continue;

         end else continue;

         if not SmartLink(member) then begin
            if FSymbolDictionary.FindSymbolPosList(member)<>nil then begin
               RemoveReferencesInContextMap(member);
               FSymbolDictionary.Remove(member);
               localChanged:=True;
            end;
         end;

      end;
      changed:=changed or localChanged;
   until not localChanged;
end;

// SmartLinkFilterInterfaceSymbol
//
procedure TdwsCodeGen.SmartLinkFilterInterfaceSymbol(intfSymbol : TInterfaceSymbol; var changed : Boolean);
var
   i : Integer;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
begin
   if FSymbolDictionary=nil then Exit;

   symPosList:=FSymbolDictionary.FindSymbolPosList(intfSymbol);
   if symPosList=nil then Exit;

   for i:=0 to symPosList.Count-1 do begin
      symPos:=symPosList.Items[i];
      if symPos.SymbolUsages=[suDeclaration] then continue;
      Exit;
   end;

   FSymbolDictionary.Remove(intfSymbol);
end;

// SmartLinkFilterMemberFieldSymbol
//
procedure TdwsCodeGen.SmartLinkFilterMemberFieldSymbol(fieldSymbol : TFieldSymbol; var changed : Boolean);
var
   fieldType : TTypeSymbol;
   fieldDeclarationPos : TSymbolPosition;
   typeReferencePos : TSymbolPosition;
   typeReferencePosList : TSymbolPositionList;
   i : Integer;
begin
   if SmartLink(fieldSymbol) then Exit;

   fieldType:=fieldSymbol.Typ;

   fieldDeclarationPos:=FSymbolDictionary.FindSymbolUsage(fieldSymbol, suDeclaration);
   if fieldDeclarationPos<>nil then begin
      typeReferencePosList:=FSymbolDictionary.FindSymbolPosList(fieldType);
      if typeReferencePosList<>nil then begin
         for i:=0 to typeReferencePosList.Count-1 do begin
            typeReferencePos:=typeReferencePosList.Items[i];
            if     (typeReferencePos.SymbolUsages=[suReference])
               and (typeReferencePos.ScriptPos.SourceFile=fieldDeclarationPos.ScriptPos.SourceFile)
               and (typeReferencePos.ScriptPos.Line=fieldDeclarationPos.ScriptPos.Line)
               and (typeReferencePos.ScriptPos.Col>fieldDeclarationPos.ScriptPos.Col) then begin
               typeReferencePosList.Delete(i);
               Break;
            end;
         end;
      end;
      FSymbolDictionary.Remove(fieldSymbol);
      changed:=True;
   end;
end;

// SuspendSmartLink
//
procedure TdwsCodeGen.SuspendSmartLink;
begin
   Assert(FSuspendedDictionary=nil);
   FSuspendedDictionary:=FSymbolDictionary;
   FSymbolDictionary:=nil;
end;

// ResumeSmartLink
//
procedure TdwsCodeGen.ResumeSmartLink;
begin
   Assert(FSymbolDictionary=nil);
   FSymbolDictionary:=FSuspendedDictionary;
   FSuspendedDictionary:=nil;
end;

// DeVirtualize
//
procedure TdwsCodeGen.DeVirtualize;
var
   sym : TSymbol;
   methSym : TMethodSymbol;
   methods : TObjectList<TMethodSymbol>;
   marks : array of Boolean;
   i, k : Integer;
begin
   if FSymbolDictionary=nil then Exit;
   if not (cgoDeVirtualize in Options) then Exit;

   methods:=TObjectList<TMethodSymbol>.Create;
   try
      // collect all method symbols that are virtual
      for i:=0 to FSymbolDictionary.Count-1 do begin
         sym:=FSymbolDictionary.Items[i].Symbol;
         if sym is TMethodSymbol then begin
            methSym:=TMethodSymbol(sym);
            if methSym.IsVirtual then
               methods.Add(methSym);
         end;
      end;
      // then mark all the overrides
      SetLength(marks, methods.Count);
      for i:=0 to methods.Count-1 do begin
         methSym:=methods[i];
         if methSym.IsOverride then begin
            marks[i]:=True;
            k:=methods.IndexOf(methSym.ParentMeth);
            if k>=0 then
               marks[k]:=True;
         end;
      end;
      // unmarked methods are never overridden and can be devirtualized
      for i:=0 to methods.Count-1 do begin
         if marks[i] then continue;
         methSym:=methods[i];
         if methSym.StructSymbol=FContext.TypTObject then continue;
         methSym.IsVirtual:=False;
      end;
   finally
      methods.ExtractAll;
      methods.Free;
   end;
end;

// WriteInteger
//
procedure TdwsCodeGen.WriteInteger(v : Int64);
var
   s : String;
begin
   FastInt64ToStr(v, s);
   WriteString(s);
end;

// ------------------
// ------------------ TdwsExprCodeGen ------------------
// ------------------

// CodeGen
//
procedure TdwsExprCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   if expr is TTypedExpr then begin
      codeGen.WriteString('(');
      CodeGenNoWrap(codeGen, TTypedExpr(expr));
      codeGen.WriteString(')');
   end;
end;

// CodeGenNoWrap
//
procedure TdwsExprCodeGen.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   Self.CodeGen(codeGen, expr);
end;

// ExprIsConstantInteger
//
class function TdwsExprCodeGen.ExprIsConstantInteger(expr : TExprBase; value : Integer) : Boolean;
begin
   Result:=    (expr<>nil)
           and (expr.ClassType=TConstIntExpr)
           and (expr.EvalAsInteger(nil)=value);
end;

// ------------------
// ------------------ TdwsMappedSymbolHash ------------------
// ------------------

// SameItem
//
function TdwsMappedSymbolHash.SameItem(const item1, item2 : TdwsMappedSymbol) : Boolean;
begin
   Result:=(item1.Symbol=item2.Symbol);
end;

// GetItemHashCode
//
function TdwsMappedSymbolHash.GetItemHashCode(const item1 : TdwsMappedSymbol) : Integer;
begin
   Result:=NativeInt(item1.Symbol) shr 3;
   Result:=Result xor (Result shr 15);
end;

// ------------------
// ------------------ TdwsCodeGenSymbolMap ------------------
// ------------------

// Create
//
constructor TdwsCodeGenSymbolMap.Create(aCodeGen : TdwsCodeGen; aParent : TdwsCodeGenSymbolMap; aSymbol : TSymbol);
begin
   inherited Create;
   if (aParent<>nil) and (aParent.FParent=nil) and (aParent.FHash=nil) then
      FCodeGen:=aCodeGen;

   FCodeGen:=aCodeGen;
   FParent:=aParent;
   FSymbol:=aSymbol;
   if aParent=nil then begin
      FHash:=TdwsMappedSymbolHash.Create;
      FNames:=TSimpleNameObjectHash<TSymbol>.Create;
      FReservedSymbol:=TSymbol.Create('', nil);
   end;
   if aSymbol is TUnitSymbol then
      FPrefix:=aSymbol.Name+'_';
end;

// Destroy
//
destructor TdwsCodeGenSymbolMap.Destroy;
begin
   FReservedSymbol.Free;
   FHash.Free;
   FNames.Free;
   inherited;
end;

// SymbolToName
//
function TdwsCodeGenSymbolMap.SymbolToName(symbol : TSymbol) : String;
begin
   if Parent<>nil then
      Exit(FParent.SymbolToName(symbol));

   FLookup.Symbol:=symbol;
   if FHash.Match(FLookup) then
      Result:=FLookup.Name
   else Result:='';

(*
   FLookup.Symbol:=symbol;
   if FHash.Match(FLookup) then
      Result:=FLookup.Name
   else if Parent<>nil then
      Result:=Parent.SymbolToName(symbol)
   else Result:='';
*)
end;

// NameToSymbol
//
function TdwsCodeGenSymbolMap.NameToSymbol(const name : String; scope : TdwsCodeGenSymbolScope) : TSymbol;
//var
//   i : Integer;
//   iter : TdwsCodeGenSymbolMap;
//   skip : Boolean;
//   rootMap : TdwsCodeGenSymbolMap;
begin
   if Parent<>nil then
      Exit(FParent.NameToSymbol(name, scope));
   Assert(FHash<>nil);
   Result:=FNames[name];

//   Result:=FNames[name];
//   if Result=nil then begin
//      case scope of
//         cgssGlobal : if Parent<>nil then
//            Result:=Parent.NameToSymbol(name, scope);
//         cgssClass : begin
//            if (Parent<>nil) and (Parent.Symbol is TClassSymbol) then
//               Result:=Parent.NameToSymbol(name, scope)
//            else if Parent<>nil then begin
//               // check for root reserved names
//               rootMap:=Parent;
//               while rootMap.Parent<>nil do
//                  rootMap:=rootMap.Parent;
//               Result:=rootMap.NameToSymbol(name, cgssLocal);
//            end;
//         end;
//      end;
//      if (Result=nil) and (scope=cgssGlobal) then begin
//         for i:=0 to Maps.Count-1 do begin
//            iter:=Maps[i];
//            repeat
//               skip:=(iter=Self);
//               iter:=iter.Parent;
//            until (iter=nil) or skip;
//            if not skip then begin
//               iter:=Maps[i];
//               Result:=iter.NameToSymbol(name, cgssLocal);
//               if Result<>nil then Break;
//            end;
//         end;
//      end;
//   end;
end;

// ReserveName
//
procedure TdwsCodeGenSymbolMap.ReserveName(const name : String);
begin
   if Parent<>nil then
      Parent.ReserveName(name)
   else begin
      Assert(FHash<>nil);
      FNames.Objects[name]:=FReservedSymbol;
   end;
end;

// RaiseAlreadyDefined
//
procedure TdwsCodeGenSymbolMap.RaiseAlreadyDefined(sym, existing : TSymbol);
var
   locationString : String;
   existingPos, symPos : TSymbolPosition;
   symDict : TdwsSymbolDictionary;
begin
   locationString:='';
   symDict:=CodeGen.ContextSymbolDictionary;
   if symDict<>nil then begin
      existingPos:=symDict.FindSymbolUsage(existing, suDeclaration);
      symPos:=symDict.FindSymbolUsage(sym, suDeclaration);
      if (existingPos<>nil) and (symPos<>nil) then
         locationString:= ', declarations at'+symPos.ScriptPos.AsInfo
                         +' and'+existingPos.ScriptPos.AsInfo+')';
   end;
   raise ECodeGenException.CreateFmt('External symbol "%s" already defined%s',
                                     [sym.Name, locationString])
end;

// ReserveExternalName
//
procedure TdwsCodeGenSymbolMap.ReserveExternalName(sym : TSymbol);
var
   n : String;
   existing : TSymbol;
begin
   if Parent<>nil then begin
      Parent.ReserveExternalName(sym);
      Exit;
   end;

   if sym is TFuncSymbol then
      n:=TFuncSymbol(sym).ExternalName
   else n:=sym.Name;
   if not FNames.AddObject(n, sym) then begin
      existing:=FNames[n];
      if (existing<>FReservedSymbol) and (existing<>sym) then begin
         // ignore duplicate external, the raise is mostly a debugging facility at the moment
         // RaiseAlreadyDefined(sym, existing)
      end else FNames.Objects[n]:=sym;
   end;
end;

// IsReserved
//
function TdwsCodeGenSymbolMap.IsReserved(const name : String) : Boolean;
begin
   if FParent<>nil then
      Result:=FParent.IsReserved(name)
   else begin
      Assert(FHash<>nil);
      Result:=(FNames.Objects[name]<>nil);
   end;
end;

// IsMapped
//
function TdwsCodeGenSymbolMap.IsMapped(const sym : TSymbol) : Boolean;
begin
   FLookup.Symbol:=symbol;
   Result:=FHash.Match(FLookup);
end;

// MapSymbol
//
function TdwsCodeGenSymbolMap.MapSymbol(symbol : TSymbol; scope : TdwsCodeGenSymbolScope; canObfuscate : Boolean) : String;

   function NewName : String;
   var
      i : Integer;
   begin
      i:=0;
      Result:=DoNeedUniqueName(symbol, i, canObfuscate);
      while NameToSymbol(Result, scope)<>nil do begin
         Inc(i);
         Result:=DoNeedUniqueName(symbol, i, canObfuscate);
      end;
      FNames.AddObject(Result, symbol);
      FLookup.Name:=Result;
      FLookup.Symbol:=symbol;
      FHash.Add(FLookup);
   end;

begin
   if Parent<>nil then
      Exit(Parent.MapSymbol(symbol, scope, canObfuscate));

   Assert(FHash<>nil);
   Result:=SymbolToName(symbol);
   if Result='' then
      if scope<>cgssNoMap then
         Result:=NewName;
end;

// DoNeedUniqueName
//
function TdwsCodeGenSymbolMap.DoNeedUniqueName(symbol : TSymbol; tryCount : Integer; canObfuscate : Boolean) : String;
begin
   if FParent<>nil then
      Exit(Parent.DoNeedUniqueName(symbol, tryCount, canObfuscate));

   Assert(FHash<>nil);
   if symbol.Name='' then begin
      if tryCount=0 then
         Result:='a$'
      else Result:=Format('a$%d', [tryCount]);
   end else begin;
      if tryCount=0 then
         if Prefix='' then
            Result:=symbol.Name
         else Result:=Prefix+symbol.Name
      else Result:=Format('%s%s$%d', [Prefix, symbol.Name, tryCount]);
   end;
end;

// ------------------
// ------------------ TdwsCodeGenSymbolMaps ------------------
// ------------------

// MapOf
//
function TdwsCodeGenSymbolMaps.MapOf(symbol : TSymbol) : TdwsCodeGenSymbolMap;
var
   i : Integer;
begin
   for i:=0 to Count-1 do begin
      Result:=Items[i];
      if Result.Symbol=symbol then Exit;
   end;
   Result:=nil;
end;

// ------------------
// ------------------ TdwsCodeGenDependencies ------------------
// ------------------

// Create
//
constructor TdwsCodeGenDependencies.Create;
begin
   inherited;
   FList:=TStringList.Create;
   FList.Sorted:=True;
   FList.Duplicates:=dupIgnore;
end;

// Destroy
//
destructor TdwsCodeGenDependencies.Destroy;
begin
   FList.Free;
   inherited;
end;

// Add
//
procedure TdwsCodeGenDependencies.Add(const dep : String);
var
   p : Integer;
   s : String;
begin
   s:=dep;
   while True do begin
      p:=Pos(',', s);
      if p>0 then begin
         List.Add(Copy(s, 1, p-1));
         s:=StrDeleteLeft(s, p);
      end else break;
   end;
   List.Add(s);
end;

// Clear
//
procedure TdwsCodeGenDependencies.Clear;
begin
   List.Clear;
end;

// Contains
//
function TdwsCodeGenDependencies.Contains(const dep : String) : Boolean;
begin
   Result:=(List.IndexOf(dep)>=0);
end;

end.
