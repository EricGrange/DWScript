unit UdwsTimeSeriesTests;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsXPlatform, dwsSymbols, dwsUtils,
   dwsCompilerContext, dwsTimeSeriesLibModule, dwsJSONConnector;

type

   TdwsTimeSeriesTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FTimeSeriesLib : TdwsTimeSeriesLib;
         FJSONLib : TdwsJSONLibModule;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;

      published
         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;

         procedure DeltaPackTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsTimeSeries;

// ------------------
// ------------------ TdwsTimeSeriesTests ------------------
// ------------------

// SetUp
//
procedure TdwsTimeSeriesTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'TimeSeriesLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FTimeSeriesLib := TdwsTimeSeriesLib.Create(nil);
   FTimeSeriesLib.Script := FCompiler;
   FJSONLib := TdwsJSONLibModule.Create(nil);
   FJSONLib.Script :=  FCompiler;
end;

// TearDown
//
procedure TdwsTimeSeriesTests.TearDown;
begin
   FTimeSeriesLib.Free;
   FJSONLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsTimeSeriesTests.Compilation;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);

      end;

   finally
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TdwsTimeSeriesTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsTimeSeriesTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsTimeSeriesTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsTimeSeriesTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// Execution
//
procedure TdwsTimeSeriesTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultText, resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;

         resultText:=exec.Result.ToString;
         if exec.Msgs.Count>0 then
            resultText:=resultText+#13#10'>>>> Error(s): '#13#10+exec.Msgs.AsInfo;

         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, resultText, FTests[i]);
         end else CheckEquals('', resultText, FTests[i]);
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// DeltaPackTest
//
procedure TdwsTimeSeriesTests.DeltaPackTest;
const
   cTestDoubles : array [ 0..10 ]  of Double = (
      1.0, 1.0, -1.0, 3.5, -1.5, 20.0,
      -100.5, 1000.5, 1000.0, -100000.0, 0
   );
var
   packedData : Pointer;
   temp : array [0..High(cTestDoubles)] of Double;
begin
   packedData := nil;
   for var i := 0 to High(cTestDoubles) do begin
      var n := DeltaPackValues(@cTestDoubles[0], i, 10, packedData);
      DeltaUnpackValues(packedData, i, 0.1, @temp[0]);
      for var k := 0 to i-1 do
         CheckEquals(cTestDoubles[k], temp[k], 'Pass ' + IntToStr(i) + ' offset ' + IntToStr(k));
   end;
   FreeMemory(packedData);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('LibModules', TdwsTimeSeriesTests);

end.
