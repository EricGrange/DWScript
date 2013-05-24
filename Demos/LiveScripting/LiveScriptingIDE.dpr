program LiveScriptingIDE;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmBasic};

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmBasic, FrmBasic);
  Application.Run;
end.

