program CustomClasses;

uses
  Forms,
  FMainForm in 'FMainForm.pas' {MainForm},
  UPlanets in 'UPlanets.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
