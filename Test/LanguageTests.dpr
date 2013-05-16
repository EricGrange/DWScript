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
  UdwsUnitTestsStatic in 'UdwsUnitTestsStatic.pas',
  UHTMLFilterTests in 'UHTMLFilterTests.pas',
  UCornerCasesTests in 'UCornerCasesTests.pas',
  UdwsClassesTests in 'UdwsClassesTests.pas',
  dwsClasses in '..\Libraries\ClassesLib\dwsClasses.pas',
  UdwsDataBaseTests in 'UdwsDataBaseTests.pas',
  UdwsFunctionsTests in 'UdwsFunctionsTests.pas',
  UCOMConnectorTests in 'UCOMConnectorTests.pas',
  UTestDispatcher in 'UTestDispatcher.pas',
  UDebuggerTests in 'UDebuggerTests.pas',
  UdwsUtilsTests in 'UdwsUtilsTests.pas',
  UMemoryTests in 'UMemoryTests.pas',
  dwsMathComplexFunctions in '..\Source\dwsMathComplexFunctions.pas',
  dwsMath3DFunctions in '..\Source\dwsMath3DFunctions.pas',
  dwsDebugFunctions in '..\Source\dwsDebugFunctions.pas',
  UBuildTests in 'UBuildTests.pas',
  URTTIExposeTests in 'URTTIExposeTests.pas',
  USourceUtilsTests in 'USourceUtilsTests.pas',
  ULocalizerTests in 'ULocalizerTests.pas',
  dwsRTTIFunctions,
  UJSONTests in 'UJSONTests.pas',
  UJSONConnectorTests in 'UJSONConnectorTests.pas',
  UTokenizerTests in 'UTokenizerTests.pas',
  ULanguageExtensionTests in 'ULanguageExtensionTests.pas',
  UJITTests in 'UJITTests.pas',
  UJITx86Tests in 'UJITx86Tests.pas',
  dwsLinq,
  dwsLinqSql in '..\Libraries\LinqLib\dwsLinqSql.pas',
  ULinqTests in 'ULinqTests.pas',
  dwsSynSQLiteDatabase in '..\Libraries\DatabaseLib\dwsSynSQLiteDatabase.pas',
  dwsLinqJson in '..\Libraries\LinqLib\dwsLinqJson.pas',
  ULinqJsonTests in 'ULinqJsonTests.pas';

{$R *.res}

begin
   DirectSet8087CW($133F);
   SetProcessAffinityMask(GetCurrentProcessId, Cardinal(-1));
   SetDecimalSeparator('.');
   ReportMemoryLeaksOnShutdown:=True;
   Application.Initialize;
   GUITestRunner.RunRegisteredTests;
end.
