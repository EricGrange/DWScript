unit UAlgorithmsTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsXPlatform, dwsUtils;

type

   TAlgorithmsTests = class (TTestCase)
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

         procedure ExecutionThreaded;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type

   TThreadedRunner = class(TdwsThread)
      Script : String;
      Exec : IdwsProgramExecution;
      Count : Integer;
      ExpectedResult : String;
      ActualResult : String;

      procedure Execute; override;
   end;

// Execute
//
procedure TThreadedRunner.Execute;
begin
   while Count>0 do begin
      try
         Exec.Execute;
      except
         on E: Exception do begin
            ActualResult:=E.ClassName+': '+E.Message;
            Break;
         end;
      end;
      ActualResult:=Exec.Result.ToString;
      if Copy(ActualResult, 1, 5)='Swaps' then
         ActualResult:=Copy(ActualResult, Pos(#13#10, ActualResult)+2, MaxInt);
      if ActualResult<>ExpectedResult then
         Break;
      Dec(Count);
   end;
end;

// ------------------
// ------------------ TAlgorithmsTests ------------------
// ------------------

// SetUp
//
procedure TAlgorithmsTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TAlgorithmsTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TAlgorithmsTests.Compilation;
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
procedure TAlgorithmsTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TAlgorithmsTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TAlgorithmsTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TAlgorithmsTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// ExecutionThreaded
//
procedure TAlgorithmsTests.ExecutionThreaded;
const
   cRunsPerThread = 15;
   cThreadsPerScript = 3;
var
   source, expectedResult : TStringList;
   i, j : Integer;
   prog : IdwsProgram;
   threads : array of TThreadedRunner;
   runner : TThreadedRunner;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];

   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      SetLength(threads, FTests.Count*cThreadsPerScript);
      for i:=0 to FTests.Count-1 do begin
         source.LoadFromFile(FTests[i]);
         prog:=FCompiler.Compile(source.Text);

         expectedResult.LoadFromFile(ChangeFileExt(FTests[i], '.txt'));
         if Copy(expectedResult[0], 1, 5)='Swaps' then
            expectedResult.Delete(0); // variable part because of randomization

         // prepare threads
         for j:=0 to cThreadsPerScript-1 do begin
            runner:=TThreadedRunner.Create(True);
            runner.FreeOnTerminate:=False;
            runner.Count:=cRunsPerThread;
            runner.Script:=Format('%s [%d]', [ExtractFileName(FTests[i]), j]);
            runner.Exec:=prog.CreateNewExecution;
            runner.ExpectedResult:=expectedResult.Text;
            threads[i*cThreadsPerScript+j]:=runner;
         end;

         // unleash threads
         for j:=0 to cThreadsPerScript-1 do
            threads[i*cThreadsPerScript+j].Start;

      end;

   finally
      source.Free;
      expectedResult.Free;
   end;

   // wait for completion and check for failures
   try
      for i:=0 to High(threads) do begin
         runner:=threads[i];
         runner.WaitFor;
         CheckEquals(runner.ExpectedResult, runner.ActualResult, 'Thread failure for '+runner.Script);
      end;
   finally
      for i:=0 to High(threads) do
         threads[i].Free;
   end;
end;

// Execution
//
procedure TAlgorithmsTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, exec.Result.ToString, FTests[i]);
         end else CheckEquals('', exec.Result.ToString, FTests[i]);
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('AlgorithmsTests', TAlgorithmsTests.Suite);

end.
