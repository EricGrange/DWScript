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
  ceflib,
  SysUtils,
  UJSCodeGenTests in 'UJSCodeGenTests.pas',
  dwsJSCodeGen in '..\Libraries\JSCodeGen\dwsJSCodeGen.pas',
  dwsJSFilter in '..\Libraries\JSCodeGen\dwsJSFilter.pas',
  UJSFilterTests in 'UJSFilterTests.pas';

{$R *.res}

begin
   CefLibrary:=ExtractFilePath(ParamStr(0))+'..\Chromium\libcef.dll';
   ReportMemoryLeaksOnShutdown:=True;
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
