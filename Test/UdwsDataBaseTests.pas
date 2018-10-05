unit UdwsDataBaseTests;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsDataBaseLibModule, dwsXPlatform, dwsSymbols, dwsUtils,
   dwsGUIDDatabase, dwsSynSQLiteDatabase, dwsCompilerContext,
   dwsWMIDatabase, dwsTDataset;

type

   TdwsDataBaseTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FDataBaseLib : TdwsDatabaseLib;

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
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsDataBaseTests ------------------
// ------------------

// SetUp
//
procedure TdwsDataBaseTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'DatabaseLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FDataBaseLib:=TdwsDatabaseLib.Create(nil);
   FDataBaseLib.Script:=FCompiler;
end;

// TearDown
//
procedure TdwsDataBaseTests.TearDown;
begin
   FDataBaseLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsDataBaseTests.Compilation;
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
procedure TdwsDataBaseTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsDataBaseTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsDataBaseTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsDataBaseTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// Execution
//
procedure TdwsDataBaseTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultText, resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;

         resultText:=exec.Result.ToString;
         if exec.Msgs.Count>0 then
            resultText:=resultText+#13#10'>>>> Error(s): '#13#10+exec.Msgs.AsInfo;

         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, resultText, FTests[i]);
         end else CheckEquals('', resultText, FTests[i]);
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

   RegisterTest('LibModules', TdwsDataBaseTests);

end.
