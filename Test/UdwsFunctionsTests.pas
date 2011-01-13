unit UdwsFunctionsTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsSymbols, dwsXPlatform, dwsUtils,
   dwsMathFunctions, dwsTimeFunctions, dwsGlobalVarsFunctions, dwsVariantFunctions;

type

   TdwsFunctionsTestsBase = class (TTestCase)
      private
         FFolder : String;
         FTests : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Compilation;
         procedure Execution;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
   end;

   TdwsFuncFunctionsTestsMath = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsTime = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsString = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsVariant = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

   TdwsFuncFunctionsTestsGlobalVars = class (TdwsFunctionsTestsBase)
      public
         procedure SetUp; override;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsFunctionsTestsBase ------------------
// ------------------

// SetUp
//
procedure TdwsFunctionsTestsBase.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);

   FTests:=TStringList.Create;
   CollectFiles(ExtractFilePath(ParamStr(0))+FFolder+PathDelim, '*.pas', FTests);
end;

// TearDown
//
procedure TdwsFunctionsTestsBase.TearDown;
begin
   FTests.Free;

   FCompiler.Free;
end;

// Compilation
//
procedure TdwsFunctionsTestsBase.Compilation;
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

// Execution
//
procedure TdwsFunctionsTestsBase.Execution;
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
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, exec.Result.ToString, FTests[i]);
         end else CheckEquals('', exec.Result.ToString, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TdwsFunctionsTestsBase.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsFunctionsTestsBase.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsMath ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsMath.SetUp;
begin
   FFolder:='FunctionsMath';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsTime ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsTime.SetUp;
begin
   FFolder:='FunctionsTime';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsString ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsString.SetUp;
begin
   FFolder:='FunctionsString';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsVariant ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsVariant.SetUp;
begin
   FFolder:='FunctionsVariant';
   inherited;
end;

// ------------------
// ------------------ TdwsFuncFunctionsTestsGlobalVars ------------------
// ------------------

// SetUp
//
procedure TdwsFuncFunctionsTestsGlobalVars.SetUp;
begin
   FFolder:='FunctionsGlobalVars';
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('FunctionsMath', TdwsFuncFunctionsTestsMath.Suite);
   TestFramework.RegisterTest('FunctionsTime', TdwsFuncFunctionsTestsTime.Suite);
   TestFramework.RegisterTest('FunctionsString', TdwsFuncFunctionsTestsString.Suite);
   TestFramework.RegisterTest('FunctionsVariant', TdwsFuncFunctionsTestsVariant.Suite);
   TestFramework.RegisterTest('FunctionsGlobalVars', TdwsFuncFunctionsTestsGlobalVars.Suite);

end.
