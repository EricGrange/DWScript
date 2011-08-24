unit UJSCodeGenTests;

interface

uses Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsCodeGen, dwsJSCodeGen, cef, ceflib, dwsJSLibModule, dwsFunctions;

type

   TJSCodeGenTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FCodeGen : TdwsJSCodeGen;
         FASMModule : TdwsJSLibModule;
         FChromium : TChromium;
         FChromiumForm : TForm;
         FLastJSResult : String;
         FConsole : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoJSAlert(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
                             const message: ustring; out Result: Boolean);
         procedure DoConsoleMessage(Sender: TObject; const browser: ICefBrowser; message, source: ustring;
                                    line: Integer; out Result: Boolean);
         procedure DoInclude(const scriptName: string; var scriptSource: string);
         function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

         function GetExpectedResult(const fileName : String) : String;

         procedure Compilation;
         procedure Execution;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionNonOptimizedWithInlineMagics;
         procedure ExecutionOptimized;
         procedure ExecutionOptimizedWithInlineMagics;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJSCodeGenTests ------------------
// ------------------

// SetUp
//
procedure TJSCodeGenTests.SetUp;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'SimpleScripts'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'BuildScripts'+PathDelim, '*.dws', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'InterfacesPass'+PathDelim, '*.pas', FTests);

   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsMath'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsString'+PathDelim, '*.pas', FTests);
//   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsTime'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
   FCompiler.OnNeedUnit:=DoNeedUnit;

   FCodeGen:=TdwsJSCodeGen.Create;

   FASMModule:=TdwsJSLibModule.Create(nil);
   FASMModule.Script:=FCompiler;

   FChromiumForm:=TForm.Create(nil);
   FChromiumForm.Show;

   FChromium:=TChromium.Create(nil);
   FChromium.OnJsAlert:=DoJSAlert;
   FChromium.OnConsoleMessage:=DoConsoleMessage;
   FChromium.Parent:=FChromiumForm;
   FChromium.Load('about:blank');
end;

// TearDown
//
procedure TJSCodeGenTests.TearDown;
begin
   FChromium.Free;
   FChromiumForm.Free;

   FASMModule.Free;

   FCodeGen.Free;

   FCompiler.Free;

   FTests.Free;
end;

// DoJSAlert
//
procedure TJSCodeGenTests.DoJSAlert(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
                                    const message: ustring; out Result: Boolean);
begin
   FLastJSResult:=message;
   Result:=True;
end;

// DoConsoleMessage
//
procedure TJSCodeGenTests.DoConsoleMessage(Sender: TObject; const browser: ICefBrowser; message, source: ustring;
                                           line: Integer; out Result: Boolean);
begin
   FConsole:=FConsole+Format('Line %d: ', [line])+message+#13#10;
   Result:=True;
end;

// DoInclude
//
procedure TJSCodeGenTests.DoInclude(const scriptName: string; var scriptSource: string);
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

// DoNeedUnit
//
function TJSCodeGenTests.DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;
var
   sl : TStringList;
   fName : String;
begin
   fName:='BuildScripts\' + unitName + '.pas';
   if not FileExists(fName) then Exit(nil);
   sl := TStringList.Create;
   try
      sl.LoadFromFile(fName);
      unitSource := sl.Text;
   finally
      sl.Free;
   end;
   Result:=nil;
end;

// Compilation
//
procedure TJSCodeGenTests.Compilation;
var
   source : TStringList;
   i, ignored : Integer;
   prog : IdwsProgram;
   diagnostic : TStringList;
begin
   ignored:=0;
   diagnostic:=TStringList.Create;
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         if prog.Msgs.HasErrors then begin
            CheckEquals(GetExpectedResult(FTests[i]),
                         'Errors >>>>'#13#10
                        +prog.Msgs.AsInfo,
                        FTests[i]);
            Inc(ignored);
            continue;
         end;

         FCodeGen.Clear;
         try
            FCodeGen.CompileProgram(prog);
         except
            on e: Exception do begin
               if Pos('Variant', e.Message)>0 then
                  Inc(ignored)
               else if Pos('TOpenArrayExpr', e.Message)>0 then
                  Inc(ignored)
//               else if Pos('TDestructor', e.Message)>0 then
//                  Inc(ignored)
               else diagnostic.Add(ExtractFileName(FTests[i])+': '+e.Message);
            end;
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed (%d ignored)'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored, ignored,
                          diagnostic.Text]));
   finally
      diagnostic.Free;
      source.Free;
   end;
end;

// Execution
//
procedure TJSCodeGenTests.Execution;
var
   source : TStringList;
   i, ignored : Integer;
   prog : IdwsProgram;
   jscode : String;
   output, expectedResult : String;
   diagnostic : TStringList;
begin
   ignored:=0;
   diagnostic:=TStringList.Create;
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         if prog.Msgs.HasErrors then begin
            CheckEquals(GetExpectedResult(FTests[i]),
                         'Errors >>>>'#13#10
                        +prog.Msgs.AsInfo,
                        FTests[i]);
            Inc(ignored);
            continue;
         end;

         FCodeGen.Clear;
         try
            FCodeGen.CompileProgram(prog);

            FCodeGen.FlushedDependencies.Add('Print');
            FCodeGen.FlushedDependencies.Add('PrintLn');

            jsCode:= 'var $testResult = [];'#13#10
                    +'function Print(s) { if (s===true) $testResult.push("True"); '
                                        +'else if (s===false) $testResult.push("False"); '
                                        +'else $testResult.push(s); };'#13#10
                    +'function PrintLn(s) { Print(s); $testResult.push("\r\n"); };'#13#10
                    +'try {'#13#10
                    +#13#10
                    +FCodeGen.CompiledOutput(prog)
                    +#13#10
                    +'} catch(e) {$testResult.splice(0,0,"Errors >>>>\r\nRuntime Error: "+((e.ClassType)?e.FMessage:e.message)+"\r\nResult >>>>\r\n")};'#13#10
                    +'alert($testResult.join(""));';

            FLastJSResult:='*no result*';
            FConsole:='';
            FChromium.Browser.MainFrame.ExecuteJavaScript(jsCode, 'about:blank', 0);
            Application.ProcessMessages;

            if prog.Msgs.Count=0 then
               output:=FConsole+FLastJSResult
            else begin
               output:= 'Errors >>>>'#13#10
                       +prog.Msgs.AsInfo
                       +'Result >>>>'#13#10
                       +FConsole+FLastJSResult;
            end;

            expectedResult:=GetExpectedResult(FTests[i]);
            if not (expectedResult=output) then begin
               diagnostic.Add( ExtractFileName(FTests[i])
                              +': expected <'+expectedResult
                              +'> but got <'+output+'>');//+jsCode);
            end;
         except
            on e : Exception do begin
               if Pos('Variant', e.Message)>0 then
                  Inc(ignored)
               else if Pos('TOpenArrayExpr', e.Message)>0 then
                  Inc(ignored)
//               else if Pos('TDestructor', e.Message)>0 then
//                  Inc(ignored)
               else diagnostic.Add(ExtractFileName(FTests[i])+': '+e.Message);
            end;
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed (%d ignored)'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored, ignored,
                          diagnostic.Text]));
   finally
      diagnostic.Free;
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TJSCodeGenTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TJSCodeGenTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coSymbolDictionary, coContextMap];
   FCodeGen.Options:=FCodeGen.Options+[cgoNoInlineMagics];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TJSCodeGenTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize];
   FCodeGen.Options:=FCodeGen.Options+[cgoNoInlineMagics];
   Execution;
end;

// ExecutionNonOptimizedWithInlineMagics
//
procedure TJSCodeGenTests.ExecutionNonOptimizedWithInlineMagics;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics];
   Execution;
end;

// ExecutionOptimized
//
procedure TJSCodeGenTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize];
   FCodeGen.Options:=FCodeGen.Options+[cgoNoInlineMagics];
   Execution;
end;

// ExecutionOptimizedWithInlineMagics
//
procedure TJSCodeGenTests.ExecutionOptimizedWithInlineMagics;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize];
   FCodeGen.Options:=FCodeGen.Options-[cgoNoInlineMagics];
   Execution;
end;

// GetExpectedResult
//
function TJSCodeGenTests.GetExpectedResult(const fileName : String) : String;
var
   expectedResult : TStringList;
   resultsFileName : String;
begin
   expectedResult:=TStringList.Create;
   try
      resultsFileName:=ChangeFileExt(fileName, '.jstxt');
      if FileExists(resultsFileName) then
         expectedResult.LoadFromFile(resultsFileName)
      else begin
         resultsFileName:=ChangeFileExt(fileName, '.txt');
         if FileExists(resultsFileName) then
            expectedResult.LoadFromFile(resultsFileName)
         else expectedResult.Clear;
      end;
      Result:=expectedResult.Text;
   finally
      expectedResult.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('JSCodeGenTests', TJSCodeGenTests.Suite);

end.
