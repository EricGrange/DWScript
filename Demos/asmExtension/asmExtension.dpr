program asmExtension;

uses
  Forms,
  FMain in 'FMain.pas' {asmExtensionTest},
  dwsLanguageExtension in '..\..\Source\dwsLanguageExtension.pas',
  dwsAsmLibModule in '..\..\Libraries\asmLib\dwsAsmLibModule.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TasmExtensionTest, asmExtensionTest);
  Application.Run;
end.
