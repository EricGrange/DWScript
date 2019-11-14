program LanguageTests;

{$SetPEFlags $0020}

{$IFNDEF VER200}
{.$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  FastMM4Debug,
  Classes,
  Forms,
  Windows,
  TestFrameWork,
  GUITestRunner,
  TextTestRunner,
  SysUtils,
  Math,
  dwsXPlatform,
  dwsMathComplexFunctions in '..\Source\dwsMathComplexFunctions.pas',
  dwsMath3DFunctions in '..\Source\dwsMath3DFunctions.pas',
  dwsDebugFunctions in '..\Source\dwsDebugFunctions.pas',
  dwsLinq,
  dwsLinqSql in '..\Libraries\LinqLib\dwsLinqSql.pas',
  dwsLinqJson in '..\Libraries\LinqLib\dwsLinqJson.pas',
  dwsDocBuilder in '..\Libraries\DocBuilder\dwsDocBuilder.pas',
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
  UdwsXPlatformTests,
  UCOMConnectorTests in 'UCOMConnectorTests.pas',
  UTestDispatcher in 'UTestDispatcher.pas',
  UDebuggerTests in 'UDebuggerTests.pas',
  UdwsUtilsTests in 'UdwsUtilsTests.pas',
  UMemoryTests in 'UMemoryTests.pas',
  UBuildTests in 'UBuildTests.pas',
  USourceUtilsTests in 'USourceUtilsTests.pas',
  ULocalizerTests in 'ULocalizerTests.pas',
  dwsRTTIFunctions,
  dwsRTTIConnector,
  dwsStringResult,
  dwsMetrics,
  UJSONTests in 'UJSONTests.pas',
  UJSONConnectorTests in 'UJSONConnectorTests.pas',
  UTokenizerTests in 'UTokenizerTests.pas',
  ULanguageExtensionTests in 'ULanguageExtensionTests.pas',
  UJITTests in 'UJITTests.pas',
  UJITx86Tests in 'UJITx86Tests.pas',
  UdwsCryptoTests in 'UdwsCryptoTests.pas',
  UdwsGraphicsTests in 'UdwsGraphicsTests.pas',
  dwsSymbolsLibModule in '..\Libraries\SymbolsLib\dwsSymbolsLibModule.pas',
  UExternalFunctionTests in 'UExternalFunctionTests.pas',
  UdwsEncodingTests in 'UdwsEncodingTests.pas',
  UInstantiateTests in 'UInstantiateTests.pas',
  UdwsWebUtilsTests in 'UdwsWebUtilsTests.pas',
  UBigIntegerTests in 'UBigIntegerTests.pas',
  dwsMPIR.Bundle in '..\Libraries\BigIntegersLib\dwsMPIR.Bundle.pas',
  USymbolDictionaryTests in 'USymbolDictionaryTests.pas',
  UdwsSystemInfoTests in 'UdwsSystemInfoTests.pas',
  UdwsWebLibModuleTest in 'UdwsWebLibModuleTest.pas',
  BeaEngineDelphi in '..\Tools\BeaEngine\BeaEngineDelphi.pas',
  UdwsDateTimeTests in 'UdwsDateTimeTests.pas';

{$R *.res}

var
{$IF RTLVersion >= 23}
   procAffinity, systAffinity : NativeUInt;
{$ELSE}
   procAffinity, systAffinity : DWORD;
{$IFEND}
begin
   ReportMemoryLeaksOnShutdown:=True;
   {$IF RTLVersion >= 23}
   SetExceptionMask(GetExceptionMask + [TArithmeticException.exZeroDivide, TArithmeticException.exInvalidOp]);
   {$else}
   DirectSet8087CW($133F);
   {$IFEND}
   GetProcessAffinityMask(GetCurrentProcess, procAffinity, systAffinity);
   SetProcessAffinityMask(GetCurrentProcess, systAffinity);
   Application.Initialize;
   Application.UpdateFormatSettings := False;
   SetDecimalSeparator('.');
   if ParamStr(1)='/text' then begin
      AllocConsole;
      TextTestRunner.RunRegisteredTests
   end else GUITestRunner.RunRegisteredTests;
end.

