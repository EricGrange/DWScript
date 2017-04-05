unit UJSONConnectorTests;

interface

uses
   Windows, Classes, SysUtils, Variants,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsXPlatform, dwsUtils, dwsJSONConnector, dwsCompilerContext;

type

   TJSONConnectorTests = class (TTestCase)
      private
         FTests : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;
         FConnector : TdwsJSONLibModule;

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
         procedure CompilationFailure;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJSONConnectorTests ------------------
// ------------------

// SetUp
//
procedure TJSONConnectorTests.SetUp;
const
   cMask = '*.pas';
begin
   FTests:=TStringList.Create;
   FFailures:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'JSONConnectorPass'+PathDelim, cMask, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'JSONConnectorFail'+PathDelim, cMask, FFailures);

   FCompiler:=TDelphiWebScript.Create(nil);
   FConnector:=TdwsJSONLibModule.Create(nil);
   FConnector.Script:=FCompiler;
end;

// TearDown
//
procedure TJSONConnectorTests.TearDown;
begin
   FConnector.Free;
   FCompiler.Free;

   FFailures.Free;
   FTests.Free;
end;

// Compilation
//
procedure TJSONConnectorTests.Compilation;
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
procedure TJSONConnectorTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TJSONConnectorTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TJSONConnectorTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TJSONConnectorTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// CompilationFailure
//
procedure TJSONConnectorTests.CompilationFailure;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
   expectedError : TStringList;
   expectedErrorsFileName : String;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   source:=TStringList.Create;
   expectedError:=TStringList.Create;
   try

      for i:=0 to FFailures.Count-1 do begin

         source.LoadFromFile(FFailures[i]);

         prog:=FCompiler.Compile(source.Text);

         expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.txt');
         if FileExists(expectedErrorsFileName) then begin
            expectedError.LoadFromFile(expectedErrorsFileName);
            CheckEquals(expectedError.Text, prog.Msgs.AsInfo, FFailures[i]);
         end else Check(prog.Msgs.AsInfo<>'', FFailures[i]+': undetected error');

      end;

   finally
      expectedError.Free;
      source.Free;
   end;
end;

// Execution
//
procedure TJSONConnectorTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultsFileName : String;
   output : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         if exec.Msgs.Count=0 then
            output:=exec.Result.ToUnicodeString
         else begin
            output:= 'Errors >>>>'#13#10
                    +exec.Msgs.AsInfo
                    +'Result >>>>'#13#10
                    +exec.Result.ToUnicodeString;
         end;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, output, FTests[i]);
         end else CheckEquals('', output, FTests[i]);

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

   RegisterTest('JSON', TJSONConnectorTests);

end.
