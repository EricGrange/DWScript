program LanguageTests;

uses
  Classes,
  Forms,
  TestFrameWork,
  GUITestRunner,
  UScriptTests in 'UScriptTests.pas',
  UAlgorithmsTests in 'UAlgorithmsTests.pas';

{$R *.res}

begin
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
