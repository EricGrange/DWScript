program Mandelbrot;

uses
  Forms,
  FMain in 'FMain.pas' {MainForm},
  dwsBaseExprs in '..\..\Source\dwsBaseExprs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
