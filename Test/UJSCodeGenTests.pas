unit UJSCodeGenTests;

interface

uses Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs, dwsUtils,
   dwsXPlatform, dwsCodeGen, dwsJSCodeGen, cef, ceflib;

type

   TJSCodeGenTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FCodeGen : TdwsJSCodeGen;
         FChromium : TChromium;
         FChromiumForm : TForm;
         FLastJSResult : String;
         FConsole : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoJSAlert(Sender: TCustomChromium; const browser: ICefBrowser; const frame: ICefFrame;
                             const message: ustring; out Result: TCefRetval);
         procedure DoConsoleMessage(Sender: TCustomChromium; const browser: ICefBrowser; message, source: ustring;
                                    line: Integer; out Result: TCefRetval);
         procedure DoInclude(const scriptName: string; var scriptSource: string);

         procedure Compilation;
         procedure Execution;

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

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;

   FCodeGen:=TdwsJSCodeGen.Create;

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

   FCodeGen.Free;

   FCompiler.Free;

   FTests.Free;
end;

// DoJSAlert
//
procedure TJSCodeGenTests.DoJSAlert(Sender: TCustomChromium; const browser: ICefBrowser; const frame: ICefFrame;
                                    const message: ustring; out Result: TCefRetval);
begin
   FLastJSResult:=message;
   Result:=RV_HANDLED;
end;

// DoConsoleMessage
//
procedure TJSCodeGenTests.DoConsoleMessage(Sender: TCustomChromium; const browser: ICefBrowser; message, source: ustring;
                                           line: Integer; out Result: TCefRetval);
begin
   FConsole:=FConsole+Format('Line %d: ', [line])+message+#13#10;
   Result:=RV_HANDLED;
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

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);

         FCodeGen.Clear;
         try
            FCodeGen.CompileProgram(prog);
         except
            on e: Exception do begin
               if Pos('TBaseVariantSymbol', e.Message)>0 then
                  Inc(ignored)
               else diagnostic.Add(ExtractFileName(FTests[i])+': '+e.Message);
            end;
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored,
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
   source, expectedResult : TStringList;
   i, k, ignored : Integer;
   prog : IdwsProgram;
   resultsFileName : String;
   jscode : String;
   output : String;
   diagnostic : TStringList;
begin
   ignored:=0;
   diagnostic:=TStringList.Create;
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);

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
                    +'} catch(e) {PrintLn("Errors >>>>\r\nRuntime Error: "+((e.ClassType)?e.FMessage:e.message)+"\r\nResult >>>>")};'#13#10
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
            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then
               expectedResult.LoadFromFile(resultsFileName)
            else expectedResult.Clear;
            if not (expectedResult.Text=output) then
               diagnostic.Add( ExtractFileName(FTests[i])
                              +': expected <'+expectedResult.Text
                              +'> but got <'+output+'>');
         except
            on e : Exception do begin
               if Pos('TBaseVariantSymbol', e.Message)>0 then
                  Inc(ignored)
               else diagnostic.Add(ExtractFileName(FTests[i])+': '+e.Message);
            end;
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored,
                          diagnostic.Text]));
   finally
      diagnostic.Free;
      expectedResult.Free;
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TJSCodeGenTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TJSCodeGenTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TJSCodeGenTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions-[coOptimize];
   Execution;
end;

// ExecutionOptimized
//
procedure TJSCodeGenTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=cDefaultCompilerOptions+[coOptimize];
   Execution;
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
