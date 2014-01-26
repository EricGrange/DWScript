unit UExternalFunctionTests;

interface

uses
   Classes,
   dwsXPlatformTests, dwsComp, dwsErrors, dwsExprList;

type
   TExternalFunctionTests = class(TTestCase)
      private
         FTests: TStringList;
         FCompiler: TDelphiWebScript;
         procedure RegisterExternalRoutines(compiler: TDelphiWebScript);
      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;
      published
         procedure Test;
   end;

implementation
uses
   SysUtils,
   dwsXPlatform, dwsExprs,
   dwsSymbols, dwsUtils;

{ TExternalFunctionTests }

procedure TExternalFunctionTests.Compilation;
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

procedure Blank;
begin
end;

procedure Ints3(a, b, c: integer);
begin
end;

procedure TestString(a: integer; b: string);
begin
end;

procedure TestStringExc(a: integer; b: string);
begin
   Abort;
end;

procedure TExternalFunctionTests.RegisterExternalRoutines(compiler: TDelphiWebScript);
begin
   compiler.RegisterExternalFunction('Blank', @Blank);
   compiler.RegisterExternalFunction('Ints3', @Ints3);
   compiler.RegisterExternalFunction('TestString', @TestString);
   compiler.RegisterExternalFunction('TestStringExc', @TestStringExc);
end;

procedure TExternalFunctionTests.Execution;
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
         RegisterExternalRoutines(FCompiler);
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

procedure TExternalFunctionTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'External'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
end;

procedure TExternalFunctionTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

procedure TExternalFunctionTests.Test;
begin
   Compilation;
   Execution;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('dwsExternalFunctionTests', TExternalFunctionTests);

end.
