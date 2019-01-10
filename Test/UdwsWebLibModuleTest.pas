unit UdwsWebLibModuleTest;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsXPlatform, dwsSymbols, dwsUtils, dwsCompilerContext,
   dwsWebLibModule, dwsJSONConnector, dwsWebEnvironment;

type

   TdwsWebLibModuleTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FWebLib : TdwsWebLib;
         FJSONLib : TdwsJSONLibModule;

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
// ------------------ TdwsWebLibModuleTests ------------------
// ------------------

// SetUp
//
procedure TdwsWebLibModuleTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'WebLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FJSONLib := TdwsJSONLibModule.Create(nil);
   FJSONLib.Script := FCompiler;

   FWebLib:=TdwsWebLib.Create(nil);
   FWebLib.dwsWeb.Script:=FCompiler;
end;

// TearDown
//
procedure TdwsWebLibModuleTests.TearDown;
begin
   FWebLib.Free;
   FJSONLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsWebLibModuleTests.Compilation;
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
procedure TdwsWebLibModuleTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsWebLibModuleTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsWebLibModuleTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsWebLibModuleTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// Execution
//
procedure TdwsWebLibModuleTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultText, resultsFileName : String;
   webEnv : TWebEnvironment;
   ewr : TEmptyWebRequest;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   ewr := TEmptyWebRequest.Create;
   try
      ewr.Headers.Text := 'Hello=World';
      ewr.DirectMethod := 'GET';

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

         exec := prog.CreateNewExecution;
         try
            webEnv := TWebEnvironment.Create;
            webEnv.WebRequest := ewr;
            exec.Environment := webEnv;

            exec.Execute(0);

            resultText:=exec.Result.ToString;
            if exec.Msgs.Count>0 then
               resultText:=resultText+#13#10'>>>> Error(s): '#13#10+exec.Msgs.AsInfo;

            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, resultText, FTests[i]);
            end else CheckEquals('', resultText, FTests[i]);
            CheckEquals('', exec.Msgs.AsInfo, FTests[i]);

         finally
            exec := nil;
         end;

      end;

   finally
      ewr.Free;
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

   RegisterTest('LibModules', TdwsWebLibModuleTests);

end.
