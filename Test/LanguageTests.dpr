program LanguageTests;

{$SetPEFlags $0001}

{$IFNDEF VER200}
{.$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  Classes,
  Forms,
  Windows,
  TestFrameWork,
  GUITestRunner,
  SysUtils,
  dwsXPlatform,
  UScriptTests in 'UScriptTests.pas',
  UAlgorithmsTests in 'UAlgorithmsTests.pas',
  UdwsUnitTests in 'UdwsUnitTests.pas',
  UHTMLFilterTests in 'UHTMLFilterTests.pas',
  UCornerCasesTests in 'UCornerCasesTests.pas',
  UdwsClassesTests in 'UdwsClassesTests.pas',
  dwsClasses in '..\Libraries\ClassesLib\dwsClasses.pas',
  dwsClassesLibModule in '..\Libraries\ClassesLib\dwsClassesLibModule.pas' {dwsClassesLib: TDataModule},
  UdwsFunctionsTests in 'UdwsFunctionsTests.pas',
  UCOMConnectorTests in 'UCOMConnectorTests.pas',
  UTestDispatcher in 'UTestDispatcher.pas',
  UDebuggerTests in 'UDebuggerTests.pas',
  UdwsUtilsTests in 'UdwsUtilsTests.pas',
  UMemoryTests in 'UMemoryTests.pas',
  dwsMathComplexFunctions in '..\Source\dwsMathComplexFunctions.pas',
  dwsMath3DFunctions in '..\Source\dwsMath3DFunctions.pas',
  UBuildTests in 'UBuildTests.pas',
  URTTIExposeTests in 'URTTIExposeTests.pas',
  USourceUtilsTests in 'USourceUtilsTests.pas',
  ULocalizerTests in 'ULocalizerTests.pas',
  dwsRTTIFunctions,
  UJSONTests in 'UJSONTests.pas';

{$R *.res}

begin
   SetProcessAffinityMask(GetCurrentProcessId, Cardinal(-1));
   SetDecimalSeparator('.');
   ReportMemoryLeaksOnShutdown:=True;
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
