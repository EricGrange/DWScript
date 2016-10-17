program LanguageTestsLAZ;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  Classes,
  fpcunit,
//  Forms,
//  TestFrameWork,
//  GUITestRunner,
  SysUtils,
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
  UBuildTests in 'UBuildTests.pas',
  URTTIExposeTests in 'URTTIExposeTests.pas',
  USourceUtilsTests in 'USourceUtilsTests.pas';

begin
   FormatSettings.DecimalSeparator:='.';
   ReportMemoryLeaksOnShutdown:=True;
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
