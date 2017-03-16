unit UAlgorithmsTests;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsXPlatform, dwsUtils, dwsSymbols, dwsCompilerContext;

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
   TEnumeratorEmptyCallBack = class
      procedure EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
   end;

procedure TEnumeratorEmptyCallBack.EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
begin
   // just used for detecting crashes in subexpr tree navigation
end;

type

   TThreadedRunner = class(TdwsThread)
      public
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
   SetDecimalSeparator('.');

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

         (prog.GetSelf as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(TEnumeratorEmptyCallBack(nil).EmptyCallBack);
         (prog.GetSelf as TdwsProgram).Expr.RecursiveEnumerateSubExprs(TEnumeratorEmptyCallBack(nil).EmptyCallBack);

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
   cMaxScriptsAtATime = 8;
var
   source, expectedResult : TStringList;
   i, j, k : Integer;
   prog : IdwsProgram;
   threads : array of TThreadedRunner;
   runner : TThreadedRunner;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];

   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      SetLength(threads, cMaxScriptsAtATime*cThreadsPerScript);

      i:=0;
      while i<FTests.Count do begin

         for k:=0 to cMaxScriptsAtATime-1 do begin

            if i>=FTests.Count then begin
               for j:=0 to cThreadsPerScript-1 do
                  threads[k*cThreadsPerScript+j]:=nil;
               continue;
            end;

            source.LoadFromFile(FTests[i]);
            prog:=FCompiler.Compile(source.Text);

            if prog.Msgs.HasErrors then continue;

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
               threads[k*cThreadsPerScript+j]:=runner;
            end;

            // unleash threads
            for j:=0 to cThreadsPerScript-1 do
               threads[k*cThreadsPerScript+j].Start;

            Inc(i);

         end;

         // wait for completion and check for failures
         try
            for k:=0 to High(threads) do begin
               runner:=threads[k];
               if runner<>nil then begin
                  runner.WaitFor;
                  CheckEquals(runner.ExpectedResult, runner.ActualResult, 'Thread failure for '+runner.Script);
               end;
            end;
         finally
            for k:=0 to High(threads) do
               FreeAndNil(threads[k]);
         end;

      end;

   finally
      source.Free;
      expectedResult.Free;
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
   resultsFileName, output : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         output:=exec.Result.ToString;
         if exec.Msgs.Count>0 then
            output:=output+#13#10+'>>> Runtime Error: '+exec.Msgs.AsInfo;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, output, FTests[i]);
         end else CheckEquals('', output, FTests[i]);
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

   RegisterTest('AlgorithmsTests', TAlgorithmsTests);

end.
