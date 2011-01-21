unit UCornerCasesTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils;

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
   sourceFile.SourceCode:='@ @= %= ^ ^=';
   t:=TTokenizer.Create(sourceFile, msgs);
   try
      CheckTrue(t.TestDelete(ttAT), '@');
      CheckTrue(t.TestDelete(ttAT_ASSIGN), '@=');
      CheckTrue(t.TestDelete(ttPERCENT_ASSIGN), '%=');
      CheckTrue(t.TestDelete(ttCARET), '^');
      CheckTrue(t.TestDelete(ttCARET_ASSIGN), '^=');

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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('CornerCasesTests', TCornerCasesTests.Suite);

end.
