unit UCornerCasesTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols;

type

   TCornerCasesTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;
         procedure DoOnInclude(const scriptName : String; var scriptSource : String);

      published
         procedure EmptyTokenBuffer;
         procedure IgnoreDecimalSeparator;
         procedure TokenizerSpecials;
         procedure TimeOutTestFinite;
         procedure TimeOutTestInfinite;
         procedure IncludeViaEvent;
         procedure IncludeViaFile;
         procedure IncludeViaFileRestricted;
         procedure StackMaxRecursion;
         procedure StackOverFlow;
         procedure Assertions;
         procedure ScriptVersion;
         procedure ExecuteParams;
         procedure CallFuncThatReturnsARecord;
         procedure ConfigAssign;
         procedure DestructorCall;
         procedure SubExprTest;
         procedure RecompileInContext;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   // TTokenBufferWrapper
   //
   TTokenBufferWrapper = class
      Buffer : TTokenBuffer;
   end;

// ------------------
// ------------------ TCornerCasesTests ------------------
// ------------------

// SetUp
//
procedure TCornerCasesTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TCornerCasesTests.TearDown;
begin
   FCompiler.Free;
end;

// EmptyTokenBuffer
//
procedure TCornerCasesTests.EmptyTokenBuffer;
var
   w : TTokenBufferWrapper;
   s : String;
begin
   w:=TTokenBufferWrapper.Create;
   try
      CheckEquals('', w.Buffer.ToStr, 'ToStr function');
      s:='dummy';
      w.Buffer.ToStr(s);
      CheckEquals('', s, 'ToStr procedure');
      s:='dummy';
      w.Buffer.ToUpperStr(s);
      CheckEquals('', s, 'ToUpperStr');
      CheckEquals(#0, w.Buffer.LastChar, 'LastChar');
   finally
      w.Free;
   end;
end;

// IgnoreDecimalSeparator
//
procedure TCornerCasesTests.IgnoreDecimalSeparator;
var
   w : TTokenBufferWrapper;
   dc : Char;
begin
   w:=TTokenBufferWrapper.Create;
   dc:=GetDecimalSeparator;
   try
      w.Buffer.AppendChar('1');
      w.Buffer.AppendChar('.');
      w.Buffer.AppendChar('5');

      SetDecimalSeparator('.');
      CheckEquals(1.5, w.Buffer.ToFloat, 'With dot');
      SetDecimalSeparator(',');
      CheckEquals(1.5, w.Buffer.ToFloat, 'With comma');
      SetDecimalSeparator('P');
      CheckEquals(1.5, w.Buffer.ToFloat, 'With P');

   finally
      SetDecimalSeparator(dc);
      w.Free;
   end;
end;

// TokenizerSpecials
//
procedure TCornerCasesTests.TokenizerSpecials;
var
   t : TTokenizer;
   msgs : TdwsCompileMessageList;
   sourceFile : TSourceFile;
begin
   msgs:=TdwsCompileMessageList.Create;
   sourceFile:=TSourceFile.Create;
   sourceFile.Code:='@ @= %= ^ ^= $(';
   t:=TTokenizer.Create(sourceFile, msgs);
   try
      CheckTrue(t.TestDelete(ttAT), '@');
      CheckTrue(t.TestDelete(ttAT_ASSIGN), '@=');
      CheckTrue(t.TestDelete(ttPERCENT_ASSIGN), '%=');
      CheckTrue(t.TestDelete(ttCARET), '^');
      CheckTrue(t.TestDelete(ttCARET_ASSIGN), '^=');
      CheckTrue(t.TestDelete(ttDOLLAR), '$');
      CheckTrue(t.TestDelete(ttBLEFT), '(');

      CheckTrue(t.TestAny([ttNAME])=ttNone, 'Any at end');
      CheckTrue(t.TestDeleteAny([ttNAME])=ttNone, 'DeleteAny at end');

   finally
      sourceFile.Free;
      t.Free;
      msgs.Free;
   end;
end;

// TimeOutTestFinite
//
procedure TCornerCasesTests.TimeOutTestFinite;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile('while false do;');

   prog.TimeoutMilliseconds:=1000;
   prog.Execute;
end;

// TimeOutTestInfinite
//
procedure TCornerCasesTests.TimeOutTestInfinite;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile('while true do;');

   prog.TimeoutMilliseconds:=100;
   prog.Execute;
end;

// DoOnInclude
//
procedure TCornerCasesTests.DoOnInclude(const scriptName : String; var scriptSource : String);
begin
   CheckEquals('test.dummy', scriptName, 'DoOnInclude');
   scriptSource:='Print(''hello'');';
end;

// IncludeViaEvent
//
procedure TCornerCasesTests.IncludeViaEvent;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FCompiler.OnInclude:=nil;
   FCompiler.Config.ScriptPaths.Clear;

   prog:=FCompiler.Compile('{$include}');

   CheckEquals('Syntax Error: Name of include file expected [line: 1, column: 10]'#13#10,
               prog.Msgs.AsInfo, 'include missing');

   prog:=FCompiler.Compile('{$include ''test.dummy''}');

   CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
               prog.Msgs.AsInfo, 'include forbidden');

   FCompiler.OnInclude:=DoOnInclude;
   prog:=FCompiler.Compile('{$include ''test.dummy''}');

   CheckEquals('', prog.Msgs.AsInfo, 'include via event');
   exec:=prog.Execute;
   CheckEquals('hello', exec.Result.ToString, 'exec include via event');
end;

// IncludeViaFile
//
procedure TCornerCasesTests.IncludeViaFile;

   function GetTemporaryFilesPath : String;
   var
      n: Integer;
   begin
      SetLength(Result, MAX_PATH);
      n:=GetTempPath(MAX_PATH-1, PChar(Result));
      SetLength(Result, n);
   end;

var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   sl : TStringList;
   tempDir : String;
   tempFile : String;
begin
   FCompiler.OnInclude:=nil;

   tempDir:=GetTemporaryFilesPath;
   tempFile:=tempDir+'test.dummy';

   sl:=TStringList.Create;
   try
      sl.Add('Print(''world'');');
      sl.SaveToFile(tempFile);
   finally
      sl.Free;
   end;

   FCompiler.Config.ScriptPaths.Clear;
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
               prog.Msgs.AsInfo, 'include via file no paths');

   FCompiler.Config.ScriptPaths.Add(tempDir);
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   CheckEquals('', prog.Msgs.AsInfo, 'include via file');
   exec:=prog.Execute;
   CheckEquals('world', exec.Result.ToString, 'exec include via file');

   FCompiler.Config.ScriptPaths.Clear;
   DeleteFile(tempFile);
end;

// IncludeViaFileRestricted
//
procedure TCornerCasesTests.IncludeViaFileRestricted;

   function GetTemporaryFilesPath : String;
   var
      n: Integer;
   begin
      SetLength(Result, MAX_PATH);
      n:=GetTempPath(MAX_PATH-1, PChar(Result));
      SetLength(Result, n);
   end;

var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   sl : TStringList;
   tempDir : String;
   tempFile : String;
   restricted : TdwsRestrictedFileSystem;
begin
   restricted:=TdwsRestrictedFileSystem.Create(nil);
   FCompiler.OnInclude:=nil;
   FCompiler.Config.CompileFileSystem:=restricted;

   tempDir:=GetTemporaryFilesPath;
   tempFile:=tempDir+'test.dummy';

   sl:=TStringList.Create;
   try
      sl.Add('Print(''world'');');
      sl.SaveToFile(tempFile);
   finally
      sl.Free;
   end;

   restricted.Paths.Text:=tempDir+'\nothing';
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
               prog.Msgs.AsInfo, 'include via file no paths');

   restricted.Paths.Text:=tempDir;

   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   CheckEquals('Compile Error: Couldn''t find file "test.dummy" on input paths [line: 1, column: 11]'#13#10,
               prog.Msgs.AsInfo, 'include via file restricted - no paths');

   FCompiler.Config.ScriptPaths.Add('.');
   prog:=FCompiler.Compile('{$include ''test.dummy''}');
   CheckEquals('', prog.Msgs.AsInfo, 'include via file restricted - dot path');
   exec:=prog.Execute;
   CheckEquals('world', exec.Result.ToString, 'exec include via file');

   DeleteFile(tempFile);
   restricted.Free;

   CheckTrue(FCompiler.Config.CompileFileSystem=nil, 'Notification release');
end;

// StackMaxRecursion
//
procedure TCornerCasesTests.StackMaxRecursion;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FCompiler.Config.MaxRecursionDepth:=5;

   prog:=FCompiler.Compile('procedure Dummy; begin Dummy; end; Dummy;');
   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   exec:=prog.Execute;
   CheckEquals('Runtime Error: Maximal recursion exceeded (5 calls) in Dummy [line: 1, column: 24]'#13#10
               +'Dummy [line: 1, column: 24]'#13#10
               +'Dummy [line: 1, column: 24]'#13#10
               +'Dummy [line: 1, column: 24]'#13#10
               +' [line: 1, column: 36]'#13#10,
               exec.Msgs.AsInfo, 'stack max recursion');

   FCompiler.Config.MaxDataSize:=cDefaultMaxRecursionDepth;
end;

// StackOverFlow
//
procedure TCornerCasesTests.StackOverFlow;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   FCompiler.Config.MaxDataSize:=32;

   prog:=FCompiler.Compile('procedure Dummy; var i : Integer; begin Dummy; end; Dummy;');
   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   exec:=prog.Execute;
   CheckEquals('Runtime Error: Maximal data size exceeded (2 Variants) in Dummy [line: 1, column: 41]'#13#10
               +'Dummy [line: 1, column: 41]'#13#10
               +' [line: 1, column: 53]'#13#10,
               exec.Msgs.AsInfo, 'stack overflow');

   FCompiler.Config.MaxDataSize:=0;
end;

// Assertions
//
procedure TCornerCasesTests.Assertions;

   procedure CheckCase(options : TCompilerOptions; const expected, testName : String);
   var
      prog : IdwsProgram;
      exec : IdwsProgramExecution;
   begin
      FCompiler.Config.CompilerOptions:=options;
      prog:=FCompiler.Compile('Assert(False);');
      exec:=prog.Execute;
      CheckEquals(expected, Trim(exec.Msgs.AsInfo), testName);
   end;

begin
   try
      CheckCase([coOptimize, coAssertions], 'Runtime Error: Assertion failed [line: 1, column: 1]', 'assertions optimization');
      CheckCase([coAssertions], 'Runtime Error: Assertion failed [line: 1, column: 1]', 'assertions');
      CheckCase([coOptimize], '', 'optimization');
      CheckCase([], '', 'neither');
   finally
      FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions;
   end;
end;

// ScriptVersion
//
procedure TCornerCasesTests.ScriptVersion;
var
   v : String;
begin
   v:=FCompiler.Version;
   FCompiler.Version:='???';
   CheckEquals(v, FCompiler.Version);
end;

// ExecuteParams
//
procedure TCornerCasesTests.ExecuteParams;
var
   prog : IdwsProgram;
   params : TVariantDynArray;
begin
   prog:=FCompiler.Compile('PrintLn(ParamCount);'
                           +'var i : Integer;'
                           +'for i:=0 to ParamCount-1 do PrintLn(ParamStr(i));');

   CheckEquals('1'#13#10'hello world'#13#10, prog.ExecuteParam('hello world').Result.ToString);
   CheckEquals('2'#13#10'hello'#13#10'world'#13#10, prog.ExecuteParam(VarArrayOf(['hello','world'])).Result.ToString);


   SetLength(params, 0);
   CheckEquals('0'#13#10, prog.ExecuteParam(params).Result.ToString);
   SetLength(params, 1);
   params[0]:='hello';
   CheckEquals('1'#13#10'hello'#13#10, prog.ExecuteParam(params).Result.ToString);
   SetLength(params, 2);
   params[1]:=123;
   CheckEquals('2'#13#10'hello'#13#10'123'#13#10, prog.ExecuteParam(params).Result.ToString);
end;

// CallFuncThatReturnsARecord
//
procedure TCornerCasesTests.CallFuncThatReturnsARecord;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   result : IInfo;
begin
   prog:=FCompiler.Compile('type TMyRec = record x, y : Integer; end;'
                           +'function Hello : TMyRec;'
                           +'begin Result.x:=1; Result.y:=2; end;');

   exec:=prog.BeginNewExecution;
   try
      result:=exec.Info.Func['Hello'].Call;
      CheckEquals(1, result.Member['x'].ValueAsInteger, 'x');
      CheckEquals(2, result.Member['y'].ValueAsInteger, 'y');
   finally
      exec.EndProgram;
   end;
end;

// ConfigAssign
//
procedure TCornerCasesTests.ConfigAssign;
var
   mds : Integer;
begin
   mds:=FCompiler.Config.MaxDataSize;
   FCompiler.Config:=FCompiler.Config;
   CheckEquals(mds, FCompiler.Config.MaxDataSize);
   FCompiler.Config.ResultType:=FCompiler.Config.ResultType;
end;

// DestructorCall
//
procedure TCornerCasesTests.DestructorCall;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   info : IInfo;
begin
   prog:=FCompiler.Compile( 'type TScriptClass = class'#13#10
                           +'constructor Create;'#13#10
                           +'destructor Destroy; override;'#13#10
                           +'end;'#13#10
                           +'constructor TScriptClass.Create; begin Print(''create''); end;'#13#10
                           +'destructor TScriptClass.Destroy; begin Print(''-destroy''); inherited; end;');

   exec:=prog.BeginNewExecution;
   try
      info:=exec.Info.Vars['TScriptClass'].GetConstructor('Create', nil).Call;
      info.Method['Free'].Call;
      CheckEquals('create-destroy', exec.Result.ToString);
   finally
      exec.EndProgram;
   end;
end;

// SubExprTest
//
procedure TCornerCasesTests.SubExprTest;

   procedure SubExprTree(output : TStringBuilder; const expr : TExprBase; indent : Integer);
   var
      i : Integer;
   begin
      output.Append(StringOfChar(#9, indent));
      if expr=nil then
         output.Append('nil')
      else output.Append(expr.ClassName);
      output.AppendLine;
      if expr<>nil then
         for i:=0 to expr.SubExprCount-1 do
            SubExprTree(output, expr.SubExpr[i], indent+1);
   end;

   function MakeSubExprTree(const expr : TExprBase) : String;
   var
      sb : TStringBuilder;
   begin
      sb:=TStringBuilder.Create;
      try
         SubExprTree(sb, expr, 0);
         Result:=sb.ToString;
      finally
         sb.Free;
      end;
   end;

var
   prog : IdwsProgram;
   testFuncSym : TSourceFuncSymbol;
begin
   prog:=FCompiler.Compile( 'function Test(a : Integer) : Integer;'#13#10
                           +'begin'#13#10
                           +'Result:=a+1;'#13#10
                           +'end;'#13#10
                           +'var s := "Hello";'#13#10
                           +'Print(s);'#13#10
                           +'if s<>"" then Print(Test(5));');


   testFuncSym:=(prog.Table.FindSymbol('Test', cvMagic) as TSourceFuncSymbol);
   CheckEquals(2, testFuncSym.SubExprCount, 'Test SubExprCount');
   CheckEquals('TBlockInitExpr'#13#10,
               MakeSubExprTree(testFuncSym.SubExpr[0]), 'Test InitExpr');
   CheckEquals('TAssignExpr'#13#10
                  +#9'TIntVarExpr'#13#10
                  +#9'TAddIntExpr'#13#10
                     +#9#9'TIntVarExpr'#13#10
                     +#9#9'TConstIntExpr'#13#10,
               MakeSubExprTree(testFuncSym.SubExpr[1]), 'Test Expr');

   CheckEquals('TBlockInitExpr'#13#10,
               MakeSubExprTree((prog as TdwsProgram).InitExpr), 'Main InitExpr');
   CheckEquals('TBlockExprNoTable3'#13#10
                  +#9'TAssignConstToStringVarExpr'#13#10
                     +#9#9'TStrVarExpr'#13#10
                     +#9#9'nil'#13#10
                  +#9'TNoResultWrapperExpr'#13#10
                     +#9#9'TFuncExpr'#13#10
                        +#9#9#9'TStrVarExpr'#13#10
                  +#9'TIfThenExpr'#13#10
                     +#9#9'TRelNotEqualStringExpr'#13#10
                        +#9#9#9'TStrVarExpr'#13#10
                        +#9#9#9'TConstStringExpr'#13#10
                     +#9#9'TNoResultWrapperExpr'#13#10
                        +#9#9#9'TFuncExpr'#13#10
                           +#9#9#9#9'TFuncExpr'#13#10
                              +#9#9#9#9#9'TConstIntExpr'#13#10,
               MakeSubExprTree((prog as TdwsProgram).Expr), 'Main Expr');
end;

// RecompileInContext
//
procedure TCornerCasesTests.RecompileInContext;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile('const hello = "world"; Print("hello");');

   CheckEquals(0, prog.Msgs.Count, 'Compile: '+prog.Msgs.AsInfo);
   exec:=prog.Execute;
   CheckEquals('hello', exec.Result.ToString, 'Compile Result');

   FCompiler.RecompileInContext(prog, 'Print(hello);');

   CheckEquals(0, prog.Msgs.Count, 'Recompile: '+prog.Msgs.AsInfo);

   exec:=prog.Execute;
   CheckEquals('world', exec.Result.ToString, 'Recompile Result');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('CornerCasesTests', TCornerCasesTests.Suite);

end.
