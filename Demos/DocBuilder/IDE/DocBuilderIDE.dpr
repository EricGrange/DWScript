program DocBuilderIDE;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FrmBasic},
  dwsClasses in '..\..\..\Libraries\ClassesLib\dwsClasses.pas',
  dwsHashtables in '..\..\..\Libraries\ClassesLib\dwsHashtables.pas',
  dwsClassesLibModule in '..\..\..\Libraries\ClassesLib\dwsClassesLibModule.pas',
  dwsSymbolsLibModule in '..\..\..\Libraries\SymbolsLib\dwsSymbolsLibModule.pas',
  dwsDocBuilder in '..\..\..\Libraries\DocBuilder\dwsDocBuilder.pas';

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmBasic, FrmBasic);
  Application.Run;
end.

