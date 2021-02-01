unit UJSFilterTests;

interface

uses
  Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
  UJSTestChromium,
  dwsJSFilter, dwsHtmlFilter, dwsXPlatform, dwsUtils,
  dwsJSLibModule,
  StrUtils, dwsFunctions, dwsCodeGen, dwsUnitSymbols, dwsCompilerContext, dwsErrors;

type

   TJSFilterTests = class(TTestCase)
      private
         FTests : TStringList;
         FTestFailures : TStringList;
         FMainCompiler: TDelphiWebScript;
         FJSCompiler: TDelphiWebScript;
         FJSFilter: TdwsJSFilter;
         FASMModule : TdwsJSLibModule;
         FHtmlFilter : TdwsHtmlFilter;
         FHtmlUnit : TdwsHtmlUnit;
         FChromium : ITestChromium;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoInclude(const scriptName: string; var scriptSource: string);
         function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

         function BrowserLoadAndWait(const src : String) : String;

      published
         procedure TestScripts;
         procedure ReleaseScripts;
         procedure FailureScripts;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJSFilterTests ------------------
// ------------------

// SetUp
//
procedure TJSFilterTests.SetUp;
const
   cFilter = '*.dws';
begin
   FTests := TStringList.Create;
   FTestFailures := TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0)) + 'JSFilterScripts' + PathDelim, cFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0)) + 'JSFilterScriptsFail' + PathDelim, cFilter, FTestFailures);

   FMainCompiler := TDelphiWebScript.Create(nil);
   FMainCompiler.OnInclude := DoInclude;

   FJSCompiler := TDelphiWebScript.Create(nil);
   FJSCompiler.Config.CompilerOptions :=
            FJSCompiler.Config.CompilerOptions
          + [ coVariablesAsVarOnly, coAllowClosures, coAllowAsyncAwait,
              coSymbolDictionary ];
   FJSCompiler.OnInclude := DoInclude;
   FJSCompiler.OnNeedUnit := DoNeedUnit;

   FJSFilter := TdwsJSFilter.Create(nil);
   FJSFilter.PatternOpen:='<%pas2js';
   FJSFilter.PatternClose:='%>';
   FJSFilter.CodeGenPrefix:='';
   FJSFilter.CodeGenPostfix:='';
   FJSFilter.Compiler := FJSCompiler;

   FHtmlFilter :=  TdwsHtmlFilter.Create(nil);
   FHtmlFilter.PatternOpen:='<?pas';
   FHtmlFilter.PatternClose:='?>';
   FHtmlFilter.SubFilter:=FJSFilter;

   FHtmlUnit := TdwsHtmlUnit.Create(nil);
   FHtmlUnit.Script := FMainCompiler;

   FMainCompiler.Config.Filter := FHtmlFilter;

   FASMModule:=TdwsJSLibModule.Create(nil);
   FASMModule.Script:=FJSCompiler;

   if FChromium = nil then
      FChromium := CreateTestChromium;
end;

// TearDown
//
procedure TJSFilterTests.TearDown;
begin
   FTestFailures.Free;
   FASMModule.Free;
   FJSFilter.Free;
   FHtmlFilter.Free;
   FMainCompiler.Free;
   FJSCompiler.Free;
   FHtmlUnit.Free;
   FTests.Free;
end;

// DoInclude
//
procedure TJSFilterTests.DoInclude(const scriptName: string; var scriptSource: string);
var
   sl : TStringList;
begin
   sl := TStringList.Create;
   try
      sl.LoadFromFile('SimpleScripts\' + scriptName);
      scriptSource := sl.Text;
   finally
      sl.Free;
   end;
end;

// DoNeedUnit
//
function TJSFilterTests.DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
var
   sl : TStringList;
begin
   sl := TStringList.Create;
   try
      sl.LoadFromFile('JSFilterScripts\' + unitName + '.pas');
      unitSource := sl.Text;
   finally
      sl.Free;
   end;
   Result:=nil;
end;

// BrowserLoadAndWait
//
function TJSFilterTests.BrowserLoadAndWait(const src : String) : String;
var
   i : Integer;
begin
   for i := 1 to 100 do
      if not FChromium.Initialized then begin
         FChromium.LoadURL('about:blank');
         Application.ProcessMessages;
         Sleep(10);
      end;
   Assert(FChromium.Initialized);

   FChromium.ClearLastResult;

   FChromium.LoadAndWait(src+'<script>console.log(document.body ? document.body.innerText : "!done")</script>', 'http://localhost');

   Result := FChromium.LastResult;
   if Result = '!done' then
      Result := '';
   FChromium.ClearLastResult;
end;

// TestScripts
//
procedure TJSFilterTests.TestScripts;
var
   s: string;
   resultFileName, output : String;
   prog: IdwsProgram;
   sl : TStringList;
   exec : IdwsProgramExecution;
begin
   sl:=TStringList.Create;
   try
      for s in FTests do begin
         sl.LoadFromFile(s);
         prog := FMainCompiler.Compile(sl.Text);

         if prog.Msgs.HasErrors then
            CheckEquals('', prog.Msgs.AsInfo, 'compile '+s);
         exec:=prog.Execute;

         CheckEquals('', exec.Msgs.AsInfo, 'exec '+s);

         FChromium.ClearLastResult;

         output := BrowserLoadAndWait(exec.Result.ToString);

         if prog.Msgs.Count > 0 then begin
            output:=  'Errors >>>>'#10
                    + ReplaceStr(prog.Msgs.AsInfo, #13#10, #10)
                    + 'Result >>>>'#10
                    + output;
         end;

         resultFileName:=ChangeFileExt(s, '.txt');
         if FileExists(resultFileName) then
            sl.LoadFromFile(ChangeFileExt(resultFileName, '.txt'))
         else sl.Clear;

         sl.LineBreak:=#10;
         CheckEquals(sl.Text, output, s+#13#10+exec.Result.ToString);
         sl.LineBreak:=#13#10;
      end;
   finally
      sl.Free;
   end;
end;

// ReleaseScripts
//
procedure TJSFilterTests.ReleaseScripts;
begin
   FJSCompiler.Config.CompilerOptions:=FJSCompiler.Config.CompilerOptions+[coOptimize, coSymbolDictionary, coContextMap];
   FJSFilter.CodeGenOptions:=[Low(TdwsCodeGenOption)..High(TdwsCodeGenOption)];
   try
      TestScripts;
   finally
      FJSCompiler.Config.CompilerOptions:=FJSCompiler.Config.CompilerOptions-[coOptimize, coSymbolDictionary, coContextMap];
      FJSFilter.CodeGenOptions:=[];
   end;
end;

// FailureScripts
//
procedure TJSFilterTests.FailureScripts;
var
   s: string;
   resultFileName, output : String;
   prog: IdwsProgram;
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      for s in FTestFailures do begin
         sl.LoadFromFile(s);
         prog := FMainCompiler.Compile(sl.Text);

         output:=ReplaceStr(prog.Msgs.AsInfo, #13#10, #10);

         resultFileName:=ChangeFileExt(s, '.txt');
         if FileExists(resultFileName) then
            sl.LoadFromFile(ChangeFileExt(resultFileName, '.txt'))
         else sl.Clear;

         sl.LineBreak:=#10;
         CheckEquals(sl.Text, output, s);
         sl.LineBreak:=#13#10;
      end;
   finally
      sl.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFrameWork.RegisterTest('JSFilterTests', TJSFilterTests.Suite);

end.
