unit UScriptTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsSymbols, dwsErrors;

type

   TScriptTests = class (TTestCase)
      private
         FTests : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: string; var scriptSource: string);

         procedure Compilation;
         procedure Execution;
         procedure CompilationFailure;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
         procedure FailuresNonOptimized;
         procedure FailuresOptimized;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

procedure EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
begin
   // just used for detecting crashes in subexpr tree navigation
end;

// ------------------
// ------------------ TScriptTests ------------------
// ------------------

// SetUp
//
procedure TScriptTests.SetUp;
const
   cFilter = '*.pas';
var
   basePath : String;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;
   FFailures:=TStringList.Create;

   basePath:=ExtractFilePath(ParamStr(0));

   CollectFiles(basePath+'SimpleScripts'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'InterfacesPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'OverloadsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'HelpersPass'+PathDelim, cFilter, FTests);

   CollectFiles(basePath+'FailureScripts'+PathDelim, cFilter, FFailures);
   CollectFiles(basePath+'InterfacesFail'+PathDelim, cFilter, FFailures);
   CollectFiles(basePath+'OverloadsFail'+PathDelim, cFilter, FFailures);
   CollectFiles(basePath+'HelpersFail'+PathDelim, cFilter, FFailures);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
   FCompiler.Config.HintsLevel:=hlPedantic;
end;

// TearDown
//
procedure TScriptTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
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
   prog : IdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);

         (prog as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
         (prog as TdwsProgram).Expr.RecursiveEnumerateSubExprs(EmptyCallBack);

         prog:=nil;

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
   prog : IdwsProgram;
   resultsFileName : String;
   output : String;
   exec : IdwsProgramExecution;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);
         try
            exec:=prog.Execute;
         except
            on E: Exception do begin
               CheckEquals('', E.Message, FTests[i]);
            end;
         end;
         if prog.Msgs.Count+exec.Msgs.Count=0 then
            output:=exec.Result.ToString
         else begin
            output:= 'Errors >>>>'#13#10
                    +prog.Msgs.AsInfo
                    +exec.Msgs.AsInfo
                    +'Result >>>>'#13#10
                    +exec.Result.ToString;
         end;

         if coOptimize in FCompiler.Config.CompilerOptions then begin
            resultsFileName:=ChangeFileExt(FTests[i], '.optimized.txt');
            if not FileExists(resultsFileName) then
               resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         end else resultsFileName:=ChangeFileExt(FTests[i], '.txt');

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
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TScriptTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize];
   Execution;
end;

// ExecutionOptimized
//
procedure TScriptTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize];
   Execution;
end;

// FailuresNonOptimized
//
procedure TScriptTests.FailuresNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize]+[coSymbolDictionary, coContextMap];
   CompilationFailure;
end;

// FailuresOptimized
//
procedure TScriptTests.FailuresOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize, coSymbolDictionary]-[coAssertions];
   CompilationFailure;
end;

// CompilationFailure
//
procedure TScriptTests.CompilationFailure;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
   expectedError : TStringList;
   expectedErrorsFileName : String;
begin
   source:=TStringList.Create;
   expectedError:=TStringList.Create;
   try

      for i:=0 to FFailures.Count-1 do begin

         source.LoadFromFile(FFailures[i]);

         try
            prog:=FCompiler.Compile(source.Text);
         except
            on E : Exception do begin
               Check(False, FFailures[i]+', during compile '+E.ClassName+': '+E.Message);
            end;
         end;

         if coOptimize in FCompiler.Config.CompilerOptions then begin
            expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.optimized.txt');
            if not FileExists(expectedErrorsFileName) then
               expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.txt');
         end else expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.txt');

         if FileExists(expectedErrorsFileName) then begin
            expectedError.LoadFromFile(expectedErrorsFileName);
            try
               CheckEquals(expectedError.Text, prog.Msgs.AsInfo, FFailures[i]);
            except
               on E: Exception do begin
                  Check(False, FFailures[i]+', '+E.ClassName+': '+E.Message);
               end;
            end;
         end else Check(prog.Msgs.AsInfo<>'', FFailures[i]+': undetected error');

         (prog as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(EmptyCallBack);
         (prog as TdwsProgram).Expr.RecursiveEnumerateSubExprs(EmptyCallBack);

         try
            prog:=nil;
         except
            on E : Exception do begin
               Check(False, FFailures[i]+', during cleanup '+E.ClassName+': '+E.Message);
            end;
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

   RegisterTest('ScriptTests', TScriptTests);

end.
