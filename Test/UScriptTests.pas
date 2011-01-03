unit UScriptTests;

interface

uses Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs, dwsXPlatform;

type

   TScriptTests = class (TTestCase)
      private
         FTests : TStringList;
         FAlgos : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: string; var scriptSource: string);

         procedure Compilation;
         procedure Execution;

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
// ------------------ TScriptTests ------------------
// ------------------

// SetUp
//
procedure TScriptTests.SetUp;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;
   FAlgos:=TStringList.Create;
   FFailures:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'SimpleScripts'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FailureScripts'+PathDelim, '*.pas', FFailures);
   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, '*.pas', FAlgos);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
end;

// TearDown
//
procedure TScriptTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
   FAlgos.Free;
   FFailures.Free;
end;

// DoInclude
//
procedure TScriptTests.DoInclude(const scriptName: string; var scriptSource: string);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.LoadFromFile('SimpleScripts\'+scriptName);
      scriptSource:=sl.Text;
   finally
      sl.Free;
   end;
end;

// Compilation
//
procedure TScriptTests.Compilation;
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
procedure TScriptTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : TdwsProgram;
   resultsFileName : String;
   output : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals(False, prog.Msgs.HasErrors, FTests[i]);
            prog.Execute;
            if prog.Msgs.Count=0 then
               output:=(prog.Result as TdwsDefaultResult).Text
            else begin
               output:= 'Errors >>>>'#13#10
                       +prog.Msgs.AsInfo
                       +'Result >>>>'#13#10
                       +(prog.Result as TdwsDefaultResult).Text;
            end;
            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, output, FTests[i]);
            end else CheckEquals('', output, FTests[i]);
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
procedure TScriptTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TScriptTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TScriptTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   Execution;
end;

// ExecutionOptimized
//
procedure TScriptTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Execution;
end;

// CompilationFailure
//
procedure TScriptTests.CompilationFailure;
var
   source : TStringList;
   i : Integer;
   prog : TdwsProgram;
   expectedError : TStringList;
   expectedErrorsFileName : String;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   source:=TStringList.Create;
   expectedError:=TStringList.Create;
   try

      for i:=0 to FFailures.Count-1 do begin

         source.LoadFromFile(FFailures[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.txt');
            if FileExists(expectedErrorsFileName) then begin
               expectedError.LoadFromFile(expectedErrorsFileName);
               CheckEquals(expectedError.Text, prog.Msgs.AsInfo, FFailures[i]);
            end else Check(prog.Msgs.AsInfo<>'', FFailures[i]+': undetected error');
         finally
            prog.Free;
         end;

      end;

   finally
      expectedError.Free;
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

   TestFramework.RegisterTest('ScriptTests', TScriptTests.Suite);

end.
