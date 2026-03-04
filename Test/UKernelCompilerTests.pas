unit UKernelCompilerTests;

{$I dws.inc}

interface

uses
   System.Classes, System.SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsKernelCompilerLibModule, dwsXPlatform, dwsCompilerContext,
   dwsKernelCompilerCommon, dwsKernelCompiler;

type

   TKernelCompilerTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FKCLLib : TdwsKernelCompilerLib;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution(const compilerClass : String = '');
         procedure DoCompilation;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
         procedure ExecutionReference;
         procedure ExecutionSSE2;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TKernelCompilerTests ------------------
// ------------------

// SetUp
//
procedure TKernelCompilerTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'KernelCompilerLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FKCLLib:=TdwsKernelCompilerLib.Create(nil);
   FKCLLib.Script:=FCompiler;
end;

// TearDown
//
procedure TKernelCompilerTests.TearDown;
begin
   FKCLLib.Free;
   FCompiler.Free;
   FTests.Free;
end;

// DoCompilation
//
procedure TKernelCompilerTests.DoCompilation;
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
procedure TKernelCompilerTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   DoCompilation;
end;

// CompilationWithMapAndSymbols
//
procedure TKernelCompilerTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   DoCompilation;
end;

// ExecutionNonOptimized
//
procedure TKernelCompilerTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TKernelCompilerTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

procedure TKernelCompilerTests.ExecutionReference;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution('TKCLReferenceCompiler');
end;

procedure TKernelCompilerTests.ExecutionSSE2;
begin
{$IFDEF WIN64_ASM}
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution('TKCLSSE2Compiler');
{$ENDIF}
end;

// Execution
//
procedure TKernelCompilerTests.Execution(const compilerClass : String);
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultText, resultsFileName : String;
   scriptSource : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin
         source.LoadFromFile(FTests[i]);
         scriptSource := source.Text;
         if compilerClass <> '' then
            scriptSource := 'type TKCLKernelCompiler = ' + compilerClass + '; ' + scriptSource;

         prog:=FCompiler.Compile(scriptSource);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         try
            exec:=prog.Execute;
         except
            on E: EdwsKCLException do begin
               CheckEquals('', 'EdwsKCLException: ' + E.Message, FTests[i]);
               Continue;
            end;
            on E: Exception do begin
               CheckEquals('', 'Exception: ' + E.Message, FTests[i]);
               Continue;
            end;
         end;

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

   RegisterTest('LibModules', TKernelCompilerTests);

end.
