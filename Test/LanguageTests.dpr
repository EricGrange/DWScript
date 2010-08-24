program LanguageTests;

uses
  Classes,
  Forms,
  TestFrameWork,
  GUITestRunner,
  UScriptTests in 'UScriptTests.pas';

{$R *.res}

begin
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
