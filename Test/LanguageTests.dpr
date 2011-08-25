program LanguageTests;

{$IFNDEF VER200}
{.$WEAKLINKRTTI ON}
{.$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  Classes,
  Forms,
  TestFrameWork,
  GUITestRunner,
  SysUtils,
  UScriptTests in 'UScriptTests.pas',
  UAlgorithmsTests in 'UAlgorithmsTests.pas',
  UdwsUnitTests in 'UdwsUnitTests.pas',
  UHTMLFilterTests in 'UHTMLFilterTests.pas',
  UCornerCasesTests in 'UCornerCasesTests.pas',
  UdwsClassesTests in 'UdwsClassesTests.pas',
  dwsClasses in '..\Libraries\ClassesLib\dwsClasses.pas',
  dwsClassesLibModule in '..\Libraries\ClassesLib\dwsClassesLibModule.pas' {dwsClassesLib: TDataModule},
  dwsHashtables in '..\Libraries\ClassesLib\dwsHashtables.pas',
  UdwsFunctionsTests in 'UdwsFunctionsTests.pas',
  UCOMConnectorTests in 'UCOMConnectorTests.pas',
  UTestDispatcher in 'UTestDispatcher.pas',
  UDebuggerTests in 'UDebuggerTests.pas',
  UdwsUtilsTests in 'UdwsUtilsTests.pas',
  UMemoryTests in 'UMemoryTests.pas',
  dwsMathComplexFunctions in '..\Source\dwsMathComplexFunctions.pas',
  dwsOperators in '..\Source\dwsOperators.pas',
  UBuildTests in 'UBuildTests.pas',
  URTTIExposeTests in 'URTTIExposeTests.pas';

{$R *.res}

begin
   FormatSettings.DecimalSeparator:='.';
   ReportMemoryLeaksOnShutdown:=True;
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
