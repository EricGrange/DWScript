program LanguageTests;

uses
  Classes,
  Forms,
  TestFrameWork,
  GUITestRunner,
  UScriptTests in 'UScriptTests.pas',
  UAlgorithmsTests in 'UAlgorithmsTests.pas',
  UHTMLFilterTests in 'UHTMLFilterTests.pas';

{$R *.res}

begin
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
