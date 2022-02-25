program SimpleFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMainForm in 'FMainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
