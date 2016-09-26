unit UJSFilterTests;

interface

uses
  Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
  dwsJSFilter, dwsHtmlFilter, dwsXPlatform, dwsUtils, cefvcl, ceflib, dwsJSLibModule,
  StrUtils, dwsFunctions, dwsCodeGen;

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
         FChromium : TChromium;
         FChromiumForm : TForm;
         FLastJSResult : String;
         FConsole : String;
         FLoadEnded : Boolean;
         FJSDone : Boolean;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoJSDialog(
            Sender: TObject; const browser: ICefBrowser; const originUrl, acceptLang: ustring;
            dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
            callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean);
         procedure DoConsoleMessage(Sender: TObject; const browser: ICefBrowser;
            const message, source: ustring; line: Integer; out Result: Boolean);
         procedure DoLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);

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

type
   TCefStringVisitor = class(TCefStringVisitorOwn)
      Str : String;
      procedure Visit(const astr: ustring); override;
   end;

// Visit
//
procedure TCefStringVisitor.Visit(const astr: ustring);
begin
   Str:=astr;
end;

// ------------------
// ------------------ TJSFilterTests ------------------
// ------------------

// SetUp
//
procedure TJSFilterTests.SetUp;
const
   cFilter = 'type*.dws';
begin
   FTests := TStringList.Create;
   FTestFailures := TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0)) + 'JSFilterScripts' + PathDelim, cFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0)) + 'JSFilterScriptsFail' + PathDelim, cFilter, FTestFailures);

   FMainCompiler := TDelphiWebScript.Create(nil);
   FMainCompiler.OnInclude := DoInclude;

   FJSCompiler := TDelphiWebScript.Create(nil);
   FJSCompiler.Config.CompilerOptions:=FJSCompiler.Config.CompilerOptions+[coVariablesAsVarOnly, coAllowClosures, coSymbolDictionary];
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

   FChromiumForm:=TForm.Create(nil);
   FChromiumForm.Show;

   FChromium:=TChromium.Create(nil);
   FChromium.OnJsdialog:=DoJSDialog;
   FChromium.OnConsoleMessage:=DoConsoleMessage;
   FChromium.OnLoadEnd:=DoLoadEnd;
   FChromium.Parent:=FChromiumForm;
   FChromium.Load('about:blank');
end;

// TearDown
//
procedure TJSFilterTests.TearDown;
begin
   FTestFailures.Free;
   FChromium.Free;
   FChromiumForm.Free;
   FASMModule.Free;
   FJSFilter.Free;
   FHtmlFilter.Free;
   FMainCompiler.Free;
   FJSCompiler.Free;
   FHtmlUnit.Free;
   FTests.Free;
end;

// DoJSDialog
//
procedure TJSFilterTests.DoJSDialog(
            Sender: TObject; const browser: ICefBrowser; const originUrl, acceptLang: ustring;
            dialogType: TCefJsDialogType; const messageText, defaultPromptText: ustring;
            callback: ICefJsDialogCallback; out suppressMessage: Boolean; out Result: Boolean);
begin
   FLastJSResult:=messageText;
   Result:=True;
end;

// DoConsoleMessage
//
procedure TJSFilterTests.DoConsoleMessage(Sender: TObject; const browser: ICefBrowser;
            const message, source: ustring; line: Integer; out Result: Boolean);
begin
   if message='!done' then
      FJSDone:=True
   else FConsole:=FConsole+Format('Line %d: ', [line])+message+#13#10;
   Result:=True;
end;

// DoLoadEnd
//
procedure TJSFilterTests.DoLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame; httpStatusCode: Integer);
begin
   FLoadEnded:=True;
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
   v : TCefStringVisitor;
   iv : ICefStringVisitor;
   t : Integer;
begin
   // CEF3 is asynchronous, this is a quick & dirty synchronous-ification
   // as unit tests are not asynchronous

   FJSDone:=False;
   FLoadEnded:=False;
   FChromium.Browser.MainFrame.LoadString(src+'<script>console.log("!done")</script>', 'http://localhost');
   while not FLoadEnded do begin
      Sleep(1);
      Application.ProcessMessages;
   end;

   // give JS time to complete
   // JS state cannot be queried (?), so we wait a bit with a timeout
   t:=200;
   while (t>0) and not FJSDone do begin
      Sleep(1);
      Application.ProcessMessages;
      Dec(t);
   end;
   if t=0 then
      FChromium.Browser.StopLoad;

   v:=TCefStringVisitor.Create;
   iv:=v;
   v.Str:=#0;
   FChromium.Browser.MainFrame.GetText(iv);
   while v.Str=#0 do begin
      Sleep(1);
      Application.ProcessMessages;
   end;
   Result:=v.Str;
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

         FLastJSResult:='*no result*';
         FConsole:='';

         FLastJSResult:=BrowserLoadAndWait(exec.Result.ToString);

         if prog.Msgs.Count=0 then
            output:=FConsole+FLastJSResult
         else begin
            output:= 'Errors >>>>'#10
                    +ReplaceStr(prog.Msgs.AsInfo, #13#10, #10)
                    +'Result >>>>'#10
                    +FConsole+FLastJSResult;
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
