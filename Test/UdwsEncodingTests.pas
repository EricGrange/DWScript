unit UdwsEncodingTests;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsEncodingLibModule, dwsXPlatform;

type

   TdwsEncodingTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FEncodingLib : TdwsEncodingLib;

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
// ------------------ TdwsEncodingTests ------------------
// ------------------

// SetUp
//
procedure TdwsEncodingTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'EncodingLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FEncodingLib:=TdwsEncodingLib.Create(nil);
   FEncodingLib.dwsEncoding.Script:=FCompiler;
end;

// TearDown
//
procedure TdwsEncodingTests.TearDown;
begin
   FEncodingLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsEncodingTests.Compilation;
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
procedure TdwsEncodingTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsEncodingTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsEncodingTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsEncodingTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// Execution
//
procedure TdwsEncodingTests.Execution;
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

   RegisterTest('LibModules', TdwsEncodingTests);

end.
