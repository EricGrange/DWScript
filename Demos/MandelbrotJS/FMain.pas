unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cef, ceflib, dwsJSFilter, StdCtrls, dwsComp, dwsCompiler,
  dwsHtmlFilter, dwsExprs, ComCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    DWSMain: TDelphiWebScript;
    DWSJS: TDelphiWebScript;
    HtmlFilter: TdwsHtmlFilter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Chromium: TChromium;
    MEOutput: TMemo;
    TabSheet3: TTabSheet;
    MESource: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Filter : TdwsJSFilter;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
   Filter:=TdwsJSFilter.Create(Self);
   Filter.Compiler:=DWSJS;

   HtmlFilter.SubFilter:=Filter;

   DWSMain.AddUnit(TdwsHTMLUnit.Create(Self));

   PageControl1.ActivePageIndex:=0;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   output : String;
begin
   prog:=DWSMain.Compile(MESource.Lines.Text);
   if prog.Msgs.HasErrors then begin
      MEOutput.Lines.Text:=prog.Msgs.AsInfo;
      PageControl1.ActivePageIndex:=2;
      Exit;
   end;

   exec:=prog.Execute;

   if exec.Msgs.Count>0 then begin
      MEOutput.Lines.Text:=prog.Msgs.AsInfo;
      PageControl1.ActivePageIndex:=2;
      Exit;
   end;

   output:=exec.Result.ToString;
   MEOutput.Lines.Text:=output;
   PageControl1.ActivePageIndex:=1;
   Chromium.Browser.MainFrame.LoadString(output, 'test');
end;

initialization

   // place Chromium binaries in dws root directory alongside Demos, Libraries, Source...
   CefLibrary:=ExtractFilePath(ParamStr(0))+'..\..\Chromium\libcef.dll';

end.
