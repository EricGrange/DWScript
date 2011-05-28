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

   TdwsCodeGen = class
      private
         FCodeGenList : TdwsRegisteredCodeGenList;
         FOutput : TWriteOnlyBlockStream;
         FDependencies : TStringList;
         FTempReg : TdwsRegisteredCodeGen;
         FLocalTable : TSymbolTable;
         FContext : TdwsProgram;
         FTableStack : TTightStack;
         FContextStack : TTightStack;
         FCompiledClasses : TTightList;
         FIndent : Integer;
         FIndentString : String;
         FNeedIndent : Boolean;

      protected
         procedure EnterContext(proc : TdwsProgram); virtual;
         procedure LeaveContext; virtual;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
         function FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
         function FindSymbolAtStackAddr(stackAddr, level : Integer) : TDataSymbol;

         procedure Compile(expr : TExprBase); virtual;
         procedure CompileSymbolTable(table : TSymbolTable); virtual;
         procedure CompileEnumerationSymbol(enum : TEnumerationSymbol); virtual;
         procedure CompileFuncSymbol(func : TSourceFuncSymbol); virtual;
         procedure CompileRecordSymbol(rec : TRecordSymbol); virtual;
         procedure CompileClassSymbol(cls : TClassSymbol); virtual;
         procedure CompileProgram(const prog : IdwsProgram); virtual;

         procedure CompileDependencies(destStream : TWriteOnlyBlockStream); virtual;

         procedure WriteIndent;
         procedure Indent;
         procedure UnIndent;

         procedure WriteString(const s : String);
         procedure WriteStringLn(const s : String);
         procedure WriteLineEnd;

         function CompiledOutput : String; virtual;

         procedure Clear; virtual;

         property Context : TdwsProgram read FContext;
         property LocalTable : TSymbolTable read FLocalTable;

         property Output : TWriteOnlyBlockStream read FOutput;
         property Dependencies : TStringList read FDependencies;
   end;

   TdwsExprCodeGen = class abstract
      public
         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); virtual; abstract;
   end;

   TdwsExprGenericCodeGen = class(TdwsExprCodeGen)
      private
         FTemplate : array of TVarRec;
         FStatement : Boolean;
      public
         constructor Create(const template : array of const; statement : Boolean = False); overload;

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
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
   FTempReg:=TdwsRegisteredCodeGen.Create;
end;

// Destroy
//
destructor TdwsCodeGen.Destroy;
begin
   inherited;
   FTempReg.Free;
   FDependencies.Free;
   FOutput.Free;
   FCodeGenList.Clean;
   FCodeGenList.Free;
   FTableStack.Free;
   FContextStack.Free;
   FCompiledClasses.Free;
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
   funcSym : TFuncSymbol;
begin
   if (Context is TdwsProcedure) then begin
      funcSym:=TdwsProcedure(Context).Func;
      Result:=funcSym.Result;
      if (Result<>nil) and (Result.StackAddr=stackAddr) then
         Exit;
   end;

   if FLocalTable=nil then Exit(nil);
   Result:=FLocalTable.FindSymbolAtStackAddr(stackAddr, level);
end;

// Clear
//
procedure TdwsCodeGen.Clear;
begin
   FOutput.Clear;
   FDependencies.Clear;

   FLocalTable:=nil;
   FTableStack.Clear;
   FContext:=nil;
   FContextStack.Clear;
   FCompiledClasses.Clear;

   FIndent:=0;
   FIndentString:='';
   FNeedIndent:=False;
end;

// Compile
//
procedure TdwsCodeGen.Compile(expr : TExprBase);

   procedure RaiseUnknown(expr : TExprBase);
   begin
      raise ECodeGenUnknownExpression.CreateFmt('%s: unknown expression class %s',
                                                [ClassName, expr.ClassName]);
   end;

var
   cg : TdwsExprCodeGen;
   oldTable : TSymbolTable;
begin
   cg:=FindCodeGen(expr);
   if cg=nil then
      RaiseUnknown(expr);

   oldTable:=FLocalTable;
   if expr is TBlockExpr then
      FLocalTable:=TBlockExpr(expr).Table;

   cg.CodeGen(Self, expr);

   FLocalTable:=oldTable;
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

      Assert(func.SubExprCount=2);
      Compile(func.SubExpr[0]);
      Compile(func.SubExpr[1]);

      LeaveContext;
   end;
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
      if cls.Parent.Name<>'TObject' then
         CompileClassSymbol(cls.Parent);
   end;
   FCompiledClasses.Add(cls);
end;

// CompileProgram
//
procedure TdwsCodeGen.CompileProgram(const prog : IdwsProgram);
var
   p : TdwsProgram;
begin
   p:=(prog as TdwsProgram);

   EnterContext(p);

   Compile(p.InitExpr);

   CompileSymbolTable(p.Table);

   Compile(p.Expr);

   LeaveContext;
end;

// CompileDependencies
//
procedure TdwsCodeGen.CompileDependencies(destStream : TWriteOnlyBlockStream);
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
   Inc(FIndent, 3);
   FIndentString:=StringOfChar(' ', FIndent);
   FNeedIndent:=True;
end;

// UnIndent
//
procedure TdwsCodeGen.UnIndent;
begin
   Dec(FIndent, 3);
   FIndentString:=StringOfChar(' ', FIndent);
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

// CompiledOutput
//
function TdwsCodeGen.CompiledOutput : String;
var
   buf : TWriteOnlyBlockStream;
begin
   buf:=TWriteOnlyBlockStream.Create;
   try
      CompileDependencies(buf);
      buf.WriteString(Output.ToString);
      Result:=buf.ToString;
   finally
      buf.Free;
   end;
end;

// EnterContext
//
procedure TdwsCodeGen.EnterContext(proc : TdwsProgram);
begin
   FTableStack.Push(FLocalTable);
   FContextStack.Push(FContext);
   FLocalTable:=proc.Table;
   FContext:=proc;
end;

// LeaveContext
//
procedure TdwsCodeGen.LeaveContext;
begin
   FLocalTable:=TSymbolTable(FTableStack.Peek);
   FTableStack.Pop;
   FContext:=TdwsProgram(FContextStack.Peek);
   FContextStack.Pop;
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
end;

// CodeGen
//
procedure TdwsExprGenericCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
   c : Char;
begin
   for i:=0 to High(FTemplate) do begin
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

end.
