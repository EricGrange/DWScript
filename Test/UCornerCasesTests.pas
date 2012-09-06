unit UCornerCasesTests;

interface

uses Windows, Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsStack, dwsJSON;

type

   TCornerCasesTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FLastResource : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;
         procedure DoOnInclude(const scriptName : String; var scriptSource : String);
         procedure DoOnResource(compiler : TdwsCompiler; const resName : String);

      published
         procedure EmptyTokenBuffer;
         procedure IgnoreDecimalSeparator;
         procedure TokenizerSpecials;
         procedure TimeOutTestFinite;
         procedure TimeOutTestInfinite;
         procedure TimeOutTestSequence;
         procedure IncludeViaEvent;
         procedure IncludeViaFile;
         procedure IncludeViaFileRestricted;
         procedure StackMaxRecursion;
         procedure StackOverFlow;
         procedure StackOverFlowOnFuncPtr;
         procedure Assertions;
         procedure ScriptVersion;
         procedure ExecuteParams;
         procedure CallFuncThatReturnsARecord;
         procedure ConfigAssign;
         procedure DestructorCall;
         procedure SubExprTest;
         procedure RecompileInContext;
         procedure RecompileInContext2;
         procedure RecompileInContext3;
         procedure RecompileInContext4;
         procedure ScriptPos;
         procedure MonkeyTest;
         procedure SameVariantTest;
         procedure SectionContextMaps;
         procedure ResourceTest;
         procedure LongLineTest;
         procedure TryExceptLoop;
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

   TScriptThread = class (TThread)
      FProg : IdwsProgram;
      FTimeOut : Integer;
      FTimeStamp : TDateTime;
      constructor Create(const prog : IdwsProgram; timeOut : Integer);
      procedure Execute; override;
   end;

// Create
//
constructor TScriptThread.Create(const prog : IdwsProgram; timeOut : Integer);
begin
   inherited Create(True);
   FProg:=prog;
   FTimeOut:=timeOut;
end;

// Execute
//
procedure TScriptThread.Execute;
begin
   FProg.Execute(FTimeOut);
   FTimeStamp:=Now;
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
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
   msgs : TdwsCompileMessageList;
   sourceFile : TSourceFile;
begin
   msgs:=TdwsCompileMessageList.Create;
   sourceFile:=TSourceFile.Create;
   sourceFile.Code:='@ @= %= ^ ^= $( ?';
   rules:=TPascalTokenizerStateRules.Create;
   t:=rules.CreateTokenizer(msgs);
   try
      t.BeginSourceFile(sourceFile);

      CheckTrue(t.TestDelete(ttAT), '@');
      CheckTrue(t.TestDelete(ttAT_ASSIGN), '@=');
      CheckTrue(t.TestDelete(ttPERCENT_ASSIGN), '%=');
      CheckTrue(t.TestDelete(ttCARET), '^');
      CheckTrue(t.TestDelete(ttCARET_ASSIGN), '^=');
      CheckTrue(t.TestDelete(ttDOLLAR), '$');
      CheckTrue(t.TestDelete(ttBLEFT), '(');
      CheckTrue(t.TestDelete(ttQUESTION), '?');

      CheckTrue(t.TestAny([ttNAME])=ttNone, 'Any at end');
      CheckTrue(t.TestDeleteAny([ttNAME])=ttNone, 'DeleteAny at end');

      t.EndSourceFile;
   finally
      sourceFile.Free;
      t.Free;
      msgs.Free;
      rules.Free;
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

// TimeOutTestSequence
//
procedure TCornerCasesTests.TimeOutTestSequence;
var
   prog : IdwsProgram;
   threads : array [1..3] of TScriptThread;
   i : Integer;
begin
   prog:=FCompiler.Compile('while true do;');

   for i:=1 to 3 do
      threads[i]:=TScriptThread.Create(prog, i*80);
   for i:=1 to 3 do
      threads[i].Start;
   while (threads[1].FTimeStamp=0) or (threads[2].FTimeStamp=0) or (threads[3].FTimeStamp=0) do
      Sleep(10);

   try
      Check(threads[1].FTimeStamp<threads[2].FTimeStamp, '1 < 2');
      Check(threads[2].FTimeStamp<threads[3].FTimeStamp, '2 < 3');
   finally
      for i:=1 to 3 do
         threads[i].Free;
   end;

   for i:=1 to 3 do
      threads[i]:=TScriptThread.Create(prog, 250-i*80);
   for i:=1 to 3 do
      threads[i].Start;
   while (threads[1].FTimeStamp=0) or (threads[2].FTimeStamp=0) or (threads[3].FTimeStamp=0) do
      Sleep(10);

   try
      Check(threads[1].FTimeStamp>threads[2].FTimeStamp, '1 > 2');
      Check(threads[2].FTimeStamp>threads[3].FTimeStamp, '2 > 3');
   finally
      for i:=1 to 3 do
         threads[i].Free;
   end;
end;

// DoOnInclude
//
procedure TCornerCasesTests.DoOnInclude(const scriptName : String; var scriptSource : String);
begin
   CheckEquals('test.dummy', scriptName, 'DoOnInclude');
   scriptSource:='Print(''hello'');';
end;

// DoOnResource
//
procedure TCornerCasesTests.DoOnResource(compiler : TdwsCompiler; const resName : String);
begin
   FLastResource:=resName;
   if resName='missing' then
      compiler.Msgs.AddCompilerError(compiler.Tokenizer.HotPos, 'Missing resource');
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

   prog:=FCompiler.Compile('{$include ''test.dummy''}print(" world");');

   CheckEquals('', prog.Msgs.AsInfo, 'include via event followup');
   exec:=prog.Execute;
   CheckEquals('hello world', exec.Result.ToString, 'exec include via event followup');
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

   CheckEquals(2, prog.SourceList.Count, 'source list count');
   CheckEquals(MSG_MainModule, prog.SourceList[0].NameReference, 'source list 0');
   CheckEquals('test.dummy', prog.SourceList[1].NameReference, 'source list 1');

   prog:=FCompiler.Compile('{$include ''test.dummy''}print(" happy");');
   CheckEquals('', prog.Msgs.AsInfo, 'include via file followup');
   exec:=prog.Execute;
   CheckEquals('world happy', exec.Result.ToString, 'exec include via file followup');

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

// StackOverFlowOnFuncPtr
//
procedure TCornerCasesTests.StackOverFlowOnFuncPtr;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   buf, buf2 : String;
begin
   FCompiler.Config.MaxRecursionDepth:=1500;
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coOptimize];

   prog:=FCompiler.Compile( 'type TObj = Class'#13#10
                           +' Procedure Proc;'#13#10
                           +' Begin'#13#10
                           +'  var p := @Proc;'#13#10
                           +'  p;'#13#10
                           +' End;'#13#10
                           +'End;'#13#10
                           +'TObj.Create.Proc;'#13#10);
   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   exec:=prog.Execute;
   buf:='TObj.Proc [line: 5, column: 4]'#13#10' [line: 8, column: 13]'#13#10;
   buf2:=exec.Msgs.AsInfo;
   CheckEquals(buf, Copy(buf2, Length(buf2)-Length(buf)+1, MaxInt), 'stack overflow');

   FCompiler.Config.MaxRecursionDepth:=1024;
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

   procedure SubExprTree(output : TWriteOnlyBlockStream; const expr : TExprBase; indent : Integer);
   var
      i : Integer;
   begin
      output.WriteString(StringOfChar(#9, indent));
      if expr=nil then
         output.WriteString('nil')
      else output.WriteString(expr.ClassName);
      output.WriteString(#13#10);
      if expr<>nil then
         for i:=0 to expr.SubExprCount-1 do
            SubExprTree(output, expr.SubExpr[i], indent+1);
   end;

   function MakeSubExprTree(const expr : TExprBase) : String;
   var
      sb : TWriteOnlyBlockStream;
   begin
      sb:=TWriteOnlyBlockStream.Create;
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

// RecompileInContext2
//
procedure TCornerCasesTests.RecompileInContext2;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'type TTest = class '
                              +'constructor Create;'
                              +'procedure Test;'
                           +'end;'
                           +'var t : TTest;'
                           +'constructor TTest.Create; begin t:=Self; end;'
                           +'procedure TTest.Test; begin Print(ClassName); end;'
                           +'TTest.Create;'
                           );

   CheckEquals(0, prog.Msgs.Count, 'Compile: '+prog.Msgs.AsInfo);

   exec:=prog.BeginNewExecution;
   try

      exec.RunProgram(0);

      CheckEquals('', exec.Result.ToString, 'Compile Result');

      FCompiler.RecompileInContext(prog, 't.Test;');

      CheckEquals(0, prog.Msgs.Count, 'Recompile: '+prog.Msgs.AsInfo);

      exec.RunProgram(0);

      CheckEquals('TTest', exec.Result.ToString, 'Recompile Result');

   finally
      exec.EndProgram;
   end;
end;

// RecompileInContext3
//
procedure TCornerCasesTests.RecompileInContext3;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'Print("Hello");'
                           +'var x: Integer;');

   exec:=prog.BeginNewExecution;

   exec.RunProgram(0);
   CheckEquals('Hello', exec.Result.ToString, 'compile');

   FCompiler.RecompileInContext(prog, 'x := 2; Print(x);');
   exec.RunProgram(0);
   CheckEquals('Hello2', exec.Result.ToString, 're compile');

   FCompiler.RecompileInContext(prog, 'x:=x+1; Print(x);');
   exec.RunProgram(0);
   CheckEquals('Hello23', exec.Result.ToString, 're re compile');

   exec.EndProgram;
end;

// RecompileInContext4
//
procedure TCornerCasesTests.RecompileInContext4;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'Print("Hello");');

   exec:=prog.BeginNewExecution;
   try

      FCompiler.RecompileInContext(prog, 'var x := 2');
      exec.RunProgram(0);
      CheckEquals('', exec.Result.ToString, 'compile');

      FCompiler.RecompileInContext(prog, 'Print(x)');
      exec.RunProgram(0);
      CheckEquals('2', exec.Result.ToString, 're compile');

      FCompiler.RecompileInContext(prog, 'var y := x+1; Print(y);');
      exec.RunProgram(0);
      CheckEquals('23', exec.Result.ToString, 're re compile');

   finally
      exec.EndProgram;
   end;
end;

// ScriptPos
//
procedure TCornerCasesTests.ScriptPos;
var
   p : TScriptPos;
begin
   p:=TScriptPos.Create(nil, 1, 2);

   CheckFalse(p.IsSourceFile('test'));
   CheckEquals('', p.AsInfo);

   CheckEquals(1, p.Line);
   CheckEquals(2, p.Col);

   p.IncCol;

   CheckEquals(1, p.Line);
   CheckEquals(3, p.Col);

   p.NewLine;

   CheckEquals(2, p.Line);
   CheckEquals(1, p.Col);
end;

// MonkeyTest
//
procedure TCornerCasesTests.MonkeyTest;
var
   i, n : Integer;
   s : String;
   prog : IdwsProgram;
   tt : TTokenType;
begin
   RandSeed:=0;

   for i:=1 to 2000 do begin
      n:=Random(10)+2;
      s:='';
      while n>0 do begin
         tt:=TTokenType(Random(Ord(High(TTokenType))+1));
         s:=s+cTokenStrings[tt]+' ';
         Dec(n);
      end;
      prog:=FCompiler.Compile(s);
   end;
end;

// SameVariantTest
//
procedure TCornerCasesTests.SameVariantTest;
var
   v : Variant;
begin
   CheckFalse(DWSSameVariant('hello', 123), '"hello" 123');

   CheckFalse(DWSSameVariant('123', 123), '"123" 123');
   CheckFalse(DWSSameVariant(123, '123'), '123 "123"');

   CheckFalse(DWSSameVariant(True, False), 'True False');
   CheckFalse(DWSSameVariant(False, True), 'False True');
   CheckTrue(DWSSameVariant(True, True), 'True True');
   CheckTrue(DWSSameVariant(False, False), 'True True');

   v:=1.5;
   CheckTrue(DWSSameVariant(v, v), 'v v');

   v:=Null;
   CheckTrue(DWSSameVariant(v, Null), 'Null Null');
   CheckFalse(DWSSameVariant(v, 1), 'Null 1');
end;

// SectionContextMaps
//
procedure TCornerCasesTests.SectionContextMaps;
var
   prog : IdwsProgram;
   unitContext, context : TdwsSourceContext;
   writer : TdwsJSONBeautifiedWriter;
   wobs : TWriteOnlyBlockStream;
begin
   FCompiler.Config.CompilerOptions:=[coContextMap];
   prog:=FCompiler.Compile( 'unit dummy;'#13#10
                           +'interface;'#13#10
                           +'uses Internal;'#13#10
                           +'implementation;'#13#10);
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions;

   unitContext:=prog.SourceContextMap.FindContextByToken(ttUNIT);
   CheckNotNull(unitContext, 'unit map');
   CheckNotNull(unitContext.ParentSym, 'unit parentsym');
   CheckEquals('dummy', unitContext.ParentSym.Name, 'unit parentsym name');

   context:=unitContext.FindContextByToken(ttINTERFACE);
   CheckEquals(' [line: 2, column: 1]', context.StartPos.AsInfo, 'intf start');
   CheckEquals(' [line: 4, column: 1]', context.EndPos.AsInfo, 'intf end');
   CheckEquals(1, context.Count, 'intf sub count');

   context:=unitContext.SubContext[0].SubContext[0];
   CheckEquals(Ord(ttUSES), Ord(context.Token), 'uses token');
   CheckEquals(' [line: 3, column: 1]', context.StartPos.AsInfo, 'uses start');
   CheckEquals(' [line: 3, column: 14]', context.EndPos.AsInfo, 'uses end');

   context:=unitContext.FindContextByToken(ttIMPLEMENTATION);
   CheckEquals(' [line: 4, column: 1]', context.StartPos.AsInfo, 'implem start');
   CheckEquals(' [line: 6, column: 1]', context.EndPos.AsInfo, 'implem end');
   CheckEquals(0, context.Count, 'implem sub count');

   wobs:=TWriteOnlyBlockStream.Create;
   writer:=TdwsJSONBeautifiedWriter.Create(wobs, 0, 1);
   try
      prog.SourceContextMap.WriteToJSON(writer);
      CheckEquals( '['#13#10
                  +#9'{'#13#10
                  +#9#9'"Token" : "ttUNIT",'#13#10
                  +#9#9'"Symbol" : {'#13#10
                  +#9#9#9'"Class" : "TUnitMainSymbol",'#13#10
                  +#9#9#9'"Name" : "dummy"'#13#10
                  +#9#9'},'#13#10
                  +#9#9'"SubContexts" : ['#13#10
                  +#9#9#9'{'#13#10
                  +#9#9#9#9'"Token" : "ttINTERFACE",'#13#10
                  +#9#9#9#9'"Symbol" : {'#13#10
                  +#9#9#9#9#9'"Class" : "TUnitMainSymbol",'#13#10
                  +#9#9#9#9#9'"Name" : "dummy"'#13#10
                  +#9#9#9#9'},'#13#10
                  +#9#9#9#9'"SubContexts" : ['#13#10
                  +#9#9#9#9#9'{'#13#10
                  +#9#9#9#9#9#9'"Token" : "ttUSES",'#13#10
                  +#9#9#9#9#9#9'"Symbol" : null,'#13#10
                  +#9#9#9#9#9#9'"SubContexts" : [ ]'#13#10
                  +#9#9#9#9#9'}'#13#10
                  +#9#9#9#9']'#13#10
                  +#9#9#9'},'#13#10
                  +#9#9#9'{'#13#10
                  +#9#9#9#9'"Token" : "ttIMPLEMENTATION",'#13#10
                  +#9#9#9#9'"Symbol" : {'#13#10
                  +#9#9#9#9#9'"Class" : "TUnitMainSymbol",'#13#10
                  +#9#9#9#9#9'"Name" : "dummy"'#13#10
                  +#9#9#9#9'},'#13#10
                  +#9#9#9#9'"SubContexts" : [ ]'#13#10
                  +#9#9#9'}'#13#10
                  +#9#9']'#13#10
                  +#9'}'#13#10
                  +']',
                  wobs.ToString, 'JSON map');
   finally
      writer.Free;
      wobs.Free;
   end;
end;

// ResourceTest
//
procedure TCornerCasesTests.ResourceTest;
var
   prog : IdwsProgram;
begin
   FLastResource:='';
   prog:=FCompiler.Compile('{$R "hello"}');
   CheckEquals('', FLastResource);

   FCompiler.OnResource:=DoOnResource;

   prog:=FCompiler.Compile('{$R "hello"}');
   CheckEquals('hello', FLastResource);

   prog:=FCompiler.Compile('{$RESOURCE ''world''}');
   CheckEquals('world', FLastResource);

   prog:=FCompiler.Compile('{$R "hello'#13#10'world"}');
   CheckEquals('hello'#13#10'world', FLastResource);

   prog:=FCompiler.Compile('{$R "missing"}');
   CheckEquals('Syntax Error: Missing resource [line: 1, column: 5]'#13#10, prog.Msgs.AsInfo);

   FCompiler.OnResource:=nil;
end;

// LongLineTest
//
procedure TCornerCasesTests.LongLineTest;
var
   s : String;
   prog : IdwsProgram;
begin
   s:='var s:="'+StringOfChar('a', 6000)+'";bug';

   prog:=FCompiler.Compile(s);
   CheckEquals('Syntax Error: Unknown name "bug" [line: 1, column: 6011]'#13#10, prog.Msgs.AsInfo);
end;

// TryExceptLoop
//
procedure TCornerCasesTests.TryExceptLoop;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=FCompiler.Compile( 'Procedure Proc;'#13#10
                           +'Begin'#13#10
                           +' Try'#13#10
                           +'  Raise Exception.Create("");'#13#10
                           +' Except'#13#10
                           +'  Proc;'#13#10
                           +' End;'#13#10
                           +'End;'#13#10
                           +'Proc;');

   exec:=prog.CreateNewExecution;
   try
      exec.Execute;
      CheckTrue(StrBeginsWith(exec.Msgs.AsInfo,
         'Runtime Error: Maximal exception depth exceeded (11 nested exceptions) in Proc [line: 6, column: 3]'),
         'exception depth');
   finally
      exec:=nil;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('CornerCasesTests', TCornerCasesTests);

end.
