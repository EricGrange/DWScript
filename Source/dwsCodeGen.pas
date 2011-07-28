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

interface

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsExprs, dwsCoreExprs, dwsJSON;

   // experimental codegen support classes for DWScipt

type

   TdwsExprCodeGen = class;

   TdwsRegisteredCodeGen = class
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
                        cgoNoConditions, cgoNoInlineMagics);
   TdwsCodeGenOptions = set of TdwsCodeGenOption;

   TdwsCodeGen = class
      private
         FCodeGenList : TdwsRegisteredCodeGenList;
         FOutput : TWriteOnlyBlockStream;
         FDependencies : TStringList;
         FFlushedDependencies : TStringList;
         FTempReg : TdwsRegisteredCodeGen;

         FLocalTable : TSymbolTable;
         FTableStack : TTightStack;

         FContext : TdwsProgram;
         FContextStack : TTightStack;

         FLocalVarSymbolMap : TStringList;
         FLocalVarSymbolMapStack : TTightStack;

         FSymbolMap : TStringList;

         FTempSymbolCounter : Integer;
         FCompiledClasses : TTightList;
         FIndent : Integer;
         FIndentString : String;
         FNeedIndent : Boolean;
         FIndentSize : Integer;
         FOptions : TdwsCodeGenOptions;

      protected
         procedure EnterContext(proc : TdwsProgram); virtual;
         procedure LeaveContext; virtual;

         procedure RaiseUnknowExpression(expr : TExprBase);

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
         function  FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
         function  FindSymbolAtStackAddr(stackAddr, level : Integer) : TDataSymbol;
         function  SymbolMappedName(sym : TSymbol) : String;

         procedure Compile(expr : TExprBase);
         procedure CompileNoWrap(expr : TTypedExpr);

         procedure CompileSymbolTable(table : TSymbolTable); virtual;
         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); virtual;
         procedure CompileFuncSymbol(func : TSourceFuncSymbol); virtual;
         procedure CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                     preConds : Boolean); virtual;
         procedure CompileRecordSymbol(rec : TRecordSymbol); virtual;
         procedure CompileClassSymbol(cls : TClassSymbol); virtual;
         procedure CompileProgram(const prog : IdwsProgram); virtual;
         procedure CompileProgramInSession(const prog : IdwsProgram); virtual;
         procedure CompileProgramBody(expr : TNoResultExpr); virtual;

         procedure BeginProgramSession(const prog : IdwsProgram); virtual;
         procedure EndProgramSession; virtual;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;

         procedure WriteIndent;
         procedure Indent;
         procedure UnIndent;

         procedure WriteString(const s : String); overload;
         procedure WriteString(const c : Char); overload;
         procedure WriteStringLn(const s : String);
         procedure WriteLineEnd;

         procedure WriteSymbolName(sym : TSymbol);

         function LocationString(e : TExprBase) : String;
         function GetNewTempSymbol : String; virtual;

         procedure WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram); virtual;
         function CompiledOutput(const prog : IdwsProgram) : String;
         procedure FushDependencies;

         procedure Clear; virtual;

         property Context : TdwsProgram read FContext;
         property LocalTable : TSymbolTable read FLocalTable;

         property IndentSize : Integer read FIndentSize write FIndentSize;
         property Options : TdwsCodeGenOptions read FOptions write FOptions;

         property Output : TWriteOnlyBlockStream read FOutput;
         property Dependencies : TStringList read FDependencies;
         property FlushedDependencies : TStringList read FFlushedDependencies;
   end;

   TdwsExprCodeGen = class abstract
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); virtual;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); virtual;
   end;

   TdwsExprGenericCodeGen = class(TdwsExprCodeGen)
      private
         FTemplate : array of TVarRec;
         FStatement : Boolean;
         FUnWrapable : Boolean;

      protected
         procedure DoCodeGen(codeGen : TdwsCodeGen; expr : TExprBase; start, stop : Integer);

      public
         constructor Create(const template : array of const; statement : Boolean = False); overload;

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
         procedure CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr); override;
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
   FDependencies:=TStringList.Create;
   FDependencies.Sorted:=True;
   FDependencies.Duplicates:=dupIgnore;
   FFlushedDependencies:=TStringList.Create;
   FFlushedDependencies.Sorted:=True;
   FFlushedDependencies.Duplicates:=dupIgnore;
   FTempReg:=TdwsRegisteredCodeGen.Create;
   FSymbolMap:=TStringList.Create;
   FIndentSize:=3;
end;

// Destroy
//
destructor TdwsCodeGen.Destroy;
begin
   inherited;
   FSymbolMap.Free;
   FTempReg.Free;
   FDependencies.Free;
   FFlushedDependencies.Free;
   FOutput.Free;
   FCodeGenList.Clean;
   FCodeGenList.Free;
   FTableStack.Free;
   FContextStack.Free;
   FCompiledClasses.Free;
   FLocalVarSymbolMapStack.Free;
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
end;

// FindCodeGen
//
function TdwsCodeGen.FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
var
   i : Integer;
begin
   FTempReg.Expr:=TExprBaseClass(expr.ClassType);
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
function TdwsCodeGen.SymbolMappedName(sym : TSymbol) : String;
var
   i : Integer;
begin
   i:=FSymbolMap.IndexOfObject(sym);
   if i>=0 then
      Result:=FSymbolMap[i]
   else Result:=sym.Name;
end;

// Clear
//
procedure TdwsCodeGen.Clear;
begin
   FOutput.Clear;
   FDependencies.Clear;
   FFlushedDependencies.Clear;

   FLocalTable:=nil;
   FTableStack.Clear;
   FContext:=nil;
   FContextStack.Clear;
   FCompiledClasses.Clear;
   FSymbolMap.Clear;

   FIndent:=0;
   FIndentString:='';
   FNeedIndent:=False;

   FTempSymbolCounter:=0;
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

// CompileSymbolTable
//
procedure TdwsCodeGen.CompileSymbolTable(table : TSymbolTable);
var
   i : Integer;
   sym : TSymbol;
begin
   for i:=0 to table.Count-1 do begin
      sym:=table.Symbols[i];
      if sym is TSourceFuncSymbol then
         CompileFuncSymbol(TSourceFuncSymbol(sym))
      else if sym is TEnumerationSymbol then
         CompileEnumerationSymbol(TEnumerationSymbol(sym))
      else if sym is TRecordSymbol then
         CompileRecordSymbol(TRecordSymbol(sym))
      else if sym is TClassSymbol then begin
         if FCompiledClasses.IndexOf(sym)<0 then
            CompileClassSymbol(TClassSymbol(sym));
      end;
   end;
end;

// CompileEnumerationSymbol
//
procedure TdwsCodeGen.CompileEnumerationSymbol(enum : TEnumerationSymbol);
begin
   // nothing
end;

// CompileFuncSymbol
//
procedure TdwsCodeGen.CompileFuncSymbol(func : TSourceFuncSymbol);
var
   proc : TdwsProcedure;
begin
   proc:=(func.Executable as TdwsProcedure);
   // nil executable means it's a function pointer type
   if proc<>nil then begin
      EnterContext(proc);
      try
         if not (cgoNoConditions in Options) then
            CompileConditions(func, proc.PreConditions, True);

         Assert(func.SubExprCount=2);
         Compile(func.SubExpr[0]);
         Compile(func.SubExpr[1]);

         if not (cgoNoConditions in Options) then
            CompileConditions(func, proc.PostConditions, False);
      finally
         LeaveContext;
      end;
   end;
end;

// CompileConditions
//
procedure TdwsCodeGen.CompileConditions(func : TFuncSymbol; conditions : TSourceConditions;
                                        preConds : Boolean);
begin
   // nothing
end;

// CompileRecordSymbol
//
procedure TdwsCodeGen.CompileRecordSymbol(rec : TRecordSymbol);
begin
   // nothing here
end;

// CompileClassSymbol
//
procedure TdwsCodeGen.CompileClassSymbol(cls : TClassSymbol);
begin
   if FCompiledClasses.IndexOf(cls.Parent)<0 then begin
      if     (cls.Parent.Name<>'TObject')
         and (cls.Parent.Name<>'Exception') then
         CompileClassSymbol(cls.Parent);
   end;
   FCompiledClasses.Add(cls);
end;

// CompileProgram
//
procedure TdwsCodeGen.CompileProgram(const prog : IdwsProgram);
begin
   BeginProgramSession(prog);
   try
      CompileProgramInSession(prog);
   finally
      EndProgramSession;
   end;
end;

// CompileProgramInSession
//
procedure TdwsCodeGen.CompileProgramInSession(const prog : IdwsProgram);
var
   p : TdwsProgram;
begin
   p:=(prog as TdwsProgram);

   Compile(p.InitExpr);

   CompileSymbolTable(p.Table);

   if not (p.Expr is TNullExpr) then
      CompileProgramBody(p.Expr);
end;

// BeginProgramSession
//
procedure TdwsCodeGen.BeginProgramSession(const prog : IdwsProgram);
var
   p : TdwsProgram;
begin
   p:=(prog as TdwsProgram);
   EnterContext(p);
end;

// EndProgramSession
//
procedure TdwsCodeGen.EndProgramSession;
begin
   LeaveContext;
end;

// CompileProgramBody
//
procedure TdwsCodeGen.CompileProgramBody(expr : TNoResultExpr);
begin
   Compile(expr);
end;

// CompileDependencies
//
procedure TdwsCodeGen.CompileDependencies(destStream : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   // nothing
end;

// WriteIndent
//
procedure TdwsCodeGen.WriteIndent;
begin
   Output.WriteString(FIndentString);
end;

// Indent
//
procedure TdwsCodeGen.Indent;
begin
   Inc(FIndent);
   FIndentString:=StringOfChar(' ', FIndent*FIndentSize);
   FNeedIndent:=True;
end;

// UnIndent
//
procedure TdwsCodeGen.UnIndent;
begin
   Dec(FIndent);
   FIndentString:=StringOfChar(' ', FIndent*FIndentSize);
   FNeedIndent:=True;
end;

// WriteString
//
procedure TdwsCodeGen.WriteString(const s : String);
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
   Output.WriteString(s);
end;

// WriteString
//
procedure TdwsCodeGen.WriteString(const c : Char);
begin
   if FNeedIndent then begin
      WriteIndent;
      FNeedIndent:=False;
   end;
   Output.WriteChar(c);
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
   Output.WriteString(#13#10);
   FNeedIndent:=True;
end;

// WriteSymbolName
//
procedure TdwsCodeGen.WriteSymbolName(sym : TSymbol);
begin
   WriteString(SymbolMappedName(sym));
end;

// LocationString
//
function TdwsCodeGen.LocationString(e : TExprBase) : String;
begin
   if Context is TdwsMainProgram then
      Result:=e.ScriptPos.AsInfo
   else Result:=' in '+e.ScriptLocation(Context);
end;

// GetNewTempSymbol
//
function TdwsCodeGen.GetNewTempSymbol : String;
begin
   Inc(FTempSymbolCounter);
   Result:=IntToStr(FTempSymbolCounter);
end;

// WriteCompiledOutput
//
procedure TdwsCodeGen.WriteCompiledOutput(dest : TWriteOnlyBlockStream; const prog : IdwsProgram);
begin
   CompileDependencies(dest, prog);
   dest.WriteString(Output.ToString);
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

// FushDependencies
//
procedure TdwsCodeGen.FushDependencies;
begin
   FFlushedDependencies.Assign(FDependencies);
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

   FLocalVarSymbolMapStack.Push(FLocalVarSymbolMap);
   FLocalVarSymbolMap:=TStringList.Create;
   for i:=0 to FLocalTable.Count-1 do begin
      sym:=FLocalTable.Symbols[i];
      if sym is TDataSymbol then
         FLocalVarSymbolMap.AddObject(sym.Name, sym);
   end;
   proc.Expr.RecursiveEnumerateSubExprs(
      procedure (parent, expr : TExprBase; var abort : Boolean)
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
                  FSymbolMap.AddObject(locName, sym);
               end else begin
                  FLocalVarSymbolMap.AddObject(sym.Name, sym);
               end;
            end;
         end;
      end);
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

// RaiseUnknowExpression
//
procedure TdwsCodeGen.RaiseUnknowExpression(expr : TExprBase);
begin
   raise ECodeGenUnknownExpression.CreateFmt('%s: unknown expression class %s:%s',
                                             [ClassName, expr.ClassName, expr.ScriptLocation(Context)]);
end;

// ------------------
// ------------------ TdwsExprGenericCodeGen ------------------
// ------------------

// Create
//
constructor TdwsExprGenericCodeGen.Create(const template : array of const; statement : Boolean = False);
var
   i : Integer;
begin
   inherited Create;
   FStatement:=statement;
   SetLength(FTemplate, Length(template));
   for i:=0 to High(template) do
      FTemplate[i]:=template[i];
   if not FStatement then begin
      i:=High(template);
      FUnWrapable:=    (FTemplate[0].VType=vtWideChar) and (FTemplate[0].VWideChar='(')
                   and (FTemplate[i].VType=vtWideChar) and (FTemplate[i].VWideChar=')');
   end else FUnWrapable:=False;
end;

// CodeGen
//
procedure TdwsExprGenericCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
begin
   DoCodeGen(codeGen, expr, 0, High(FTemplate));
end;

// CodeGenNoWrap
//
procedure TdwsExprGenericCodeGen.CodeGenNoWrap(codeGen : TdwsCodeGen; expr : TTypedExpr);
begin
   if FUnWrapable then
      DoCodeGen(codeGen, expr, 1, High(FTemplate)-1)
   else DoCodeGen(codeGen, expr, 0, High(FTemplate));
end;

// DoCodeGen
//
procedure TdwsExprGenericCodeGen.DoCodeGen(codeGen : TdwsCodeGen; expr : TExprBase; start, stop : Integer);
var
   i : Integer;
   c : Char;
begin
   for i:=start to stop do begin
      case FTemplate[i].VType of
         vtInteger :
            codeGen.Compile(expr.SubExpr[FTemplate[i].VInteger]);
         vtUnicodeString :
            codeGen.WriteString(String(FTemplate[i].VUnicodeString));
         vtWideChar : begin
            c:=FTemplate[i].VWideChar;
            case c of
               #9 : begin
                  codeGen.WriteLineEnd;
                  codeGen.Indent;
               end;
               #8 : codeGen.UnIndent;
            else
               codeGen.WriteString(FTemplate[i].VWideChar);
            end;
         end;
      else
         Assert(False);
      end;
   end;
   if FStatement then
      codeGen.WriteLineEnd;
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

end.
