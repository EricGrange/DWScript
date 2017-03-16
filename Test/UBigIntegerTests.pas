unit UBigIntegerTests;

interface

uses
   Windows, Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsBigIntegerFunctions.GMP, dwsXPlatform, dwsSymbols, dwsUtils,
   dwsMPIR.Bundle, dwsCompilerContext;

type

   TBigIntegerTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;

      published
         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;

         procedure MultiThreadedExecution;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TBigIntegerTests ------------------
// ------------------

// SetUp
//
procedure TBigIntegerTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'BigInteger'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TBigIntegerTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TBigIntegerTests.Compilation;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

      end;

   finally
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TBigIntegerTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TBigIntegerTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TBigIntegerTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TBigIntegerTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

type
   TCheckEquals = procedure (const expected, effective, msg : String) of object;

procedure RunTests(testList : TStrings; compiler : TDelphiWebScript;
                   checkEquals :  TCheckEquals; rounds : Integer);
var
   source, expectedResult : TStringList;
   i, j : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultText, resultsFileName, expectedResultText : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to testList.Count-1 do begin

         source.LoadFromFile(testList[i]);

         prog:=compiler.Compile(source.Text);

         checkEquals('', prog.Msgs.AsInfo, testList[i]);

         resultsFileName:=ChangeFileExt(testList[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            expectedResultText := expectedResult.Text;
         end else expectedResultText := '';

         for j := 1 to rounds do begin
            exec:=prog.Execute;

            resultText:=exec.Result.ToString;
            if exec.Msgs.Count>0 then
               resultText:=resultText+#13#10'>>>> Error(s): '#13#10+exec.Msgs.AsInfo;

            checkEquals(expectedResultText, resultText, testList[i]);
            checkEquals('', exec.Msgs.AsInfo, testList[i]);
         end;

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// Execution
//
procedure TBigIntegerTests.Execution;
begin
   RunTests(FTests, FCompiler, CheckEquals, 1);
end;

type
   TThreadRunner = class(TThread)
      FTests : TBigIntegerTests;
      FMessage : String;
      procedure CheckEquals(const expected, effective, msg : String);
      procedure Execute; override;

   end;

// Execute
//
procedure TThreadRunner.Execute;
begin
   RunTests(FTests.FTests, FTests.FCompiler, CheckEquals, 20);
end;

// CheckEquals
//
procedure TThreadRunner.CheckEquals(const expected, effective, msg : String);
begin
   if expected <> effective then
      FMessage := Format('expected << %s >> but got << %s >> (%s)', [expected, effective, msg]);
end;

// MultiThreadedExecution
//
procedure TBigIntegerTests.MultiThreadedExecution;
var
   i : Integer;
   t : array [0..3] of TThreadRunner;
begin
   for i := 0 to High(t) do begin
      t[i] := TThreadRunner.Create(True);
      t[i].FTests := Self;
      t[i].Start;
   end;
   for i := 0 to High(t) do t[i].WaitFor;
   try
      for i := 0 to High(t) do
         if t[i].FMessage <> '' then
            Check(False, t[i].FMessage);
   finally
      for i := 0 to High(t) do t[i].Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('BigIntegerTests', TBigIntegerTests);

end.
