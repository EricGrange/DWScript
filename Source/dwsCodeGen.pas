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

uses Classes, SysUtils, dwsUtils, dwsSymbols, dwsExprs, dwsCoreExprs;

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

      protected

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure RegisterCodeGen(expr : TExprBaseClass; codeGen : TdwsExprCodeGen);
         function FindCodeGen(expr : TExprBase) : TdwsExprCodeGen;
         function FindSymbolAtStackAddr(stackAddr : Integer) : TDataSymbol;

         procedure Compile(expr : TExprBase); virtual;
         procedure CompileFuncSymbol(func : TSourceFuncSymbol); virtual;
         procedure CompileProgram(const prog : IdwsProgram); virtual;

         procedure Clear; virtual;

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
      public
         constructor Create(const template : array of const);

         procedure CodeGen(codeGen : TdwsCodeGen; expr : TExprBase); override;
   end;

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
   FCodeGenList.Free;
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
function TdwsCodeGen.FindSymbolAtStackAddr(stackAddr : Integer) : TDataSymbol;
begin
   if FLocalTable=nil then Exit(nil);
   Result:=FLocalTable.FindSymbolAtStackAddr(stackAddr);
end;

// Clear
//
procedure TdwsCodeGen.Clear;
begin
   FLocalTable:=nil;
   FOutput.Clear;
   FDependencies.Clear;
end;

// Compile
//
procedure TdwsCodeGen.Compile(expr : TExprBase);

   procedure RaiseUnknown(expr : TExprBase);
   begin
      raise Exception.CreateFmt('%s: unknown expression class %s',
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

// CompileFuncSymbol
//
procedure TdwsCodeGen.CompileFuncSymbol(func : TSourceFuncSymbol);
var
   oldTable : TSymbolTable;
begin
   oldTable:=FLocalTable;
   FLocalTable:=(func.Executable as TdwsProcedure).Table;

   Assert(func.SubExprCount=2);
   Compile(func.SubExpr[0]);
   Compile(func.SubExpr[1]);

   FLocalTable:=oldTable;
end;

// CompileProgram
//
procedure TdwsCodeGen.CompileProgram(const prog : IdwsProgram);
var
   p : TdwsProgram;
var
   oldTable : TSymbolTable;
begin
   p:=(prog as TdwsProgram);

   oldTable:=FLocalTable;
   FLocalTable:=p.Table;

   Compile(p.InitExpr);
   Compile(p.Expr);

   FLocalTable:=oldTable;
end;

// ------------------
// ------------------ TdwsExprGenericCodeGen ------------------
// ------------------

// Create
//
constructor TdwsExprGenericCodeGen.Create(const template : array of const);
var
   i : Integer;
begin
   inherited Create;
   SetLength(FTemplate, Length(template));
   for i:=0 to High(template) do
      FTemplate[i]:=template[i];
end;

// CodeGen
//
procedure TdwsExprGenericCodeGen.CodeGen(codeGen : TdwsCodeGen; expr : TExprBase);
var
   i : Integer;
begin
   for i:=0 to High(FTemplate) do begin
      case FTemplate[i].VType of
         vtInteger :
            codeGen.Compile(expr.SubExpr[FTemplate[i].VInteger]);
         vtString :
            codeGen.Output.WriteString(String(FTemplate[i].VString^));
         vtAnsiString :
            codeGen.Output.WriteString(String(AnsiString(FTemplate[i].VAnsiString)));
         vtUnicodeString :
            codeGen.Output.WriteString(String(FTemplate[i].VUnicodeString));
         vtWideChar :
            codeGen.Output.WriteString(FTemplate[i].VWideChar);
      else
         Assert(False);
      end;
   end;
end;

end.
