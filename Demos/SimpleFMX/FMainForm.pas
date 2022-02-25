unit FMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Menus,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  dwsFMXGUIFunctions, dwsExprs, FMX.StdCtrls, dwsComp;

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    MLoad: TMenuItem;
    MSave: TMenuItem;
    MRun: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    MESourceCode: TMemo;
    MEResult: TMemo;
    Splitter1: TSplitter;
    DelphiWebScript: TDelphiWebScript;
    procedure MLoadClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
    procedure MRunClick(Sender: TObject);
  private
    { Private declarations }
    FFileName : String;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.MLoadClick(Sender: TObject);
begin
   OpenDialog.FileName := FFileName;
   if OpenDialog.Execute then begin
      //break;
      FFileName:=OpenDialog.FileName;
      MESourceCode.Lines.LoadFromFile(FFileName);
      Caption:='Simple - '+FFileName;
   end;
end;

procedure TMainForm.MSaveClick(Sender: TObject);
begin
   SaveDialog.FileName:=FFileName;
   if SaveDialog.Execute then begin
      FFileName:=SaveDialog.FileName;
      MESourceCode.Lines.SaveToFile(FFileName);
      Caption:='Simple - '+FFileName;
   end;
end;

procedure TMainForm.MRunClick(Sender: TObject);
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   prog:=DelphiWebScript.Compile(MESourceCode.Lines.Text);

   if prog.Msgs.Count>0 then
      MEResult.Lines.Text:=prog.Msgs.AsInfo
   else begin
      MEResult.Lines.Clear;
      try
         exec:=prog.Execute;
         MEResult.Lines.Text:=exec.Result.ToString;
      except
         on E: Exception do begin
            MEResult.Lines.Text:=E.ClassName+': '+E.Message;
         end;
      end;
   end;
end;

end.
