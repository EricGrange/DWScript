unit ULinqJsonTests;

interface
uses
   Classes,
   dwsXPlatformTests, dwsComp, dwsLinq, dwsJSONConnector, dwsErrors;

type
   TLinqJsonTests = class(TTestCase)
      private
         FTests: TStringList;
         FCompiler: TDelphiWebScript;
         FLinq: TdwsLinqFactory;
         FJSONLib: TdwsJSONLibModule;
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
   dwsXPlatform, dwsExprs, dwsLinqJson;

{ TLinqJsonTests }

procedure TLinqJsonTests.Compilation;
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
         prog := nil;
      end;

   finally
      source.Free;
   end;
end;

procedure TLinqJsonTests.Execution;
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

procedure TLinqJsonTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'LinqJson'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FLinq := TdwsLinqFactory.Create(FCompiler);
   FLinq.Script := FCompiler;
   FJSONLib:=TdwsJSONLibModule.Create(FCompiler);
   FJSONLib.Script:=FCompiler;
   dwsLinqJson.TLinqJsonExtension.Create(FCompiler).LinqFactory := FLinq;
end;

procedure TLinqJsonTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

procedure TLinqJsonTests.Test;
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

   RegisterTest('dwsLinqLibTests', TLinqJsonTests);

end.
