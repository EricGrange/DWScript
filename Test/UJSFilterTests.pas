unit UJSFilterTests;

interface

uses
  Forms, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
  dwsJSFilter, dwsHtmlFilter, dwsXPlatform, dwsUtils, cef, ceflib, dwsJSLibModule,
  StrUtils, dwsFunctions;

type

   TJSFilterTests = class(TTestCase)
      private
         FTests: TStringList;
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

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoJSAlert(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
                             const message: ustring; out Result: Boolean);
         procedure DoConsoleMessage(Sender: TObject; const browser: ICefBrowser; message, source: ustring;
                                    line: Integer; out Result: Boolean);
         procedure DoLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
                             httpStatusCode: Integer; out Result: Boolean);

         procedure DoInclude(const scriptName: string; var scriptSource: string);
         function  DoNeedUnit(const unitName : String; var unitSource : String) : IdwsUnit;

         procedure BrowserLoadAndWait(const src : String);

      published
         procedure TestScripts;
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
begin
   FTests := TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0)) + 'JSFilterScripts' + PathDelim, '*.dws', FTests);

   FMainCompiler := TDelphiWebScript.Create(nil);
   FMainCompiler.OnInclude := DoInclude;

   FJSCompiler := TDelphiWebScript.Create(nil);
   FJSCompiler.OnInclude := DoInclude;
   FJSCompiler.OnNeedUnit := DoNeedUnit;

   FJSFilter := TdwsJSFilter.Create(nil);
   FJSFilter.PatternOpen:='<%pas2js';
   FJSFilter.PatternClose:='%>';
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
   FChromium.OnJsAlert:=DoJSAlert;
   FChromium.OnConsoleMessage:=DoConsoleMessage;
   FChromium.OnLoadEnd:=DoLoadEnd;
   FChromium.Parent:=FChromiumForm;
   FChromium.Load('about:blank');
end;

// TearDown
//
procedure TJSFilterTests.TearDown;
begin
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

// DoJSAlert
//
procedure TJSFilterTests.DoJSAlert(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
                                   const message: ustring; out Result: Boolean);
begin
   FLastJSResult:=message;
   Result:=True;
end;

// DoConsoleMessage
//
procedure TJSFilterTests.DoConsoleMessage(Sender: TObject; const browser: ICefBrowser; message, source: ustring;
                                          line: Integer; out Result: Boolean);
begin
   FConsole:=FConsole+Format('Line %d: ', [line])+message+#13#10;
   Result:=True;
end;

// DoLoadEnd
//
procedure TJSFilterTests.DoLoadEnd(Sender: TObject; const browser: ICefBrowser; const frame: ICefFrame;
                             httpStatusCode: Integer; out Result: Boolean);
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
procedure TJSFilterTests.BrowserLoadAndWait(const src : String);
begin
   FLoadEnded:=False;
   FChromium.Browser.MainFrame.LoadString(src, 'dummy');
   while not FLoadEnded do
      Application.ProcessMessages;
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

         CheckEquals(False, prog.Msgs.HasErrors, s);
         exec:=prog.Execute;

         CheckEquals('', exec.Msgs.AsInfo, 'exec '+s);

         FLastJSResult:='*no result*';
         FConsole:='';

         BrowserLoadAndWait(exec.Result.ToString);
         FLastJSResult:=FChromium.Browser.MainFrame.Text;

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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFrameWork.RegisterTest('JSFilterTests', TJSFilterTests.Suite);

end.
