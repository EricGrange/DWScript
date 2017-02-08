unit UScriptTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsSymbols, dwsErrors, Clipbrd;

type

   TScriptTests = class (TTestCase)
      private
         FTests : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);

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

type
   TEnumeratorEmptyCallBack = class
      procedure EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
   end;

procedure TEnumeratorEmptyCallBack.EmptyCallBack(parent, expr : TExprBase; var abort : Boolean);
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

   CollectFiles(basePath+'FailureScripts'+PathDelim, cFilter, FFailures);
   CollectFiles(basePath+'AttributesFail'+PathDelim, cFilter, FFailures);
   CollectFiles(basePath+'LambdaFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'InterfacesPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'InterfacesFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'OverloadsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'OverloadsFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'HelpersPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'HelpersFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'PropertyExpressionsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'PropertyExpressionsFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'SetOfPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'SetOfFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'AssociativePass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'AssociativeFail'+PathDelim, cFilter, FFailures);

   CollectFiles(basePath+'GenericsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'GenericsFail'+PathDelim, cFilter, FFailures);

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
procedure TScriptTests.DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
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

         {$ifdef FPC}
         // triggers a GDB bug which crashes Lazarus
         if Copy(ExtractFileName(FTests[i]), 1, 11)='div_by_zero' then continue;
         {$endif}

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text, 'Test\'+ExtractFileName(FTests[i]));

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);

         (prog.GetSelf as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(TEnumeratorEmptyCallBack(nil).EmptyCallBack);
         (prog.GetSelf as TdwsProgram).Expr.RecursiveEnumerateSubExprs(TEnumeratorEmptyCallBack(nil).EmptyCallBack);

         try
            prog := nil;
         except
            on E: Exception do
               Check(False, FTests[i]+#13#10+E.ClassName+': '+E.Message);
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
   prog : IdwsProgram;
   resultsFileName : String;
   output : String;
   exec : IdwsProgramExecution;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         {$ifdef FPC}
         // triggers a GDB bug which crashes Lazarus
         if Copy(ExtractFileName(FTests[i]), 1, 11)='div_by_zero' then continue;
         // need FPC Unicode fixes
         if Copy(ExtractFileName(FTests[i]), 1, 10)='for_in_str' then continue;
         if Copy(ExtractFileName(FTests[i]), 1, 13)='unicode_const' then continue;
         if Copy(ExtractFileName(FTests[i]), 1, 19)='unicode_identifiers' then continue;
         {$endif}

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text, 'Test\'+ExtractFileName(FTests[i]));

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
         {$ifdef FPC}
         if FileExists(ChangeFileExt(resultsFileName, '.fpctxt')) then
            resultsFileName:=ChangeFileExt(resultsFileName, '.fpctxt');
         {$endif}

         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            {$ifdef FPC}
            if expectedResult.Count>0 then
               if Copy(expectedResult[0], 1, 3)=#$EF#$BB#$BF then
                  expectedResult[0]:=Copy(expectedResult[0], 4, MaxInt);
            {$endif}
            CheckEquals(expectedResult.Text, output, FTests[i]);
         end else CheckEquals('', output, FTests[i]);

         try
            prog := nil;
         except
            on E: Exception do
               Check(False, FTests[i]+#13#10+E.ClassName+': '+E.Message);
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
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize]-[coAssertions];
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

         (prog.GetSelf as TdwsProgram).InitExpr.RecursiveEnumerateSubExprs(TEnumeratorEmptyCallBack(nil).EmptyCallBack);
         (prog.GetSelf as TdwsProgram).Expr.RecursiveEnumerateSubExprs(TEnumeratorEmptyCallBack(nil).EmptyCallBack);

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
