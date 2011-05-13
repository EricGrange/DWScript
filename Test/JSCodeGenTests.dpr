program JSCodeGenTests;

{$IFNDEF VER200}
{$WEAKLINKRTTI ON}
{.$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  Classes,
  Forms,
  TestFrameWork,
  GUITestRunner,
  UJSCodeGenTests in 'UJSCodeGenTests.pas',
  dwsJSCodeGen in '..\Libraries\JSCodeGen\dwsJSCodeGen.pas';

{$R *.res}

begin
   ReportMemoryLeaksOnShutdown:=True;
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
