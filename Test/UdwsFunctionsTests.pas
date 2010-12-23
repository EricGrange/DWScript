unit UdwsFunctionsTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsSymbols, dwsMathFunctions, dwsTimeFunctions,
   dwsVariantFunctions, dwsXPlatform;

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
   prog : TdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         finally
            prog.Free;
         end;

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
   prog : TdwsProgram;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
            prog.Execute;
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, (prog.Result as TdwsDefaultResult).Text, FTests[i]);
            end else CheckEquals('', (prog.Result as TdwsDefaultResult).Text, FTests[i]);
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         finally
            prog.Free;
         end;

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
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsFunctionsTestsBase.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
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

end.
