unit PreviewUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw;

type
  TFrmPreview = class(TForm)
    WebBrowser: TWebBrowser;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  end;

var
  FrmPreview: TFrmPreview;

implementation

{$R *.dfm}

uses MainUnit;

procedure TFrmPreview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FrmBasic.AcnViewPreview.Checked := False;
end;

end.

