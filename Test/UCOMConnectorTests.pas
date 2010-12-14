unit UCOMConnectorTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsComConnector, Variants, ActiveX, ComObj, dwsXPlatform;

type

   TCOMConnectorTests = class (TTestCase)
      private
         FCW : Word;
         FTests : TStringList;
         FFailures : TStringList;
         FCompiler : TDelphiWebScript;
         FConnector : TdwsComConnector;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure ReCreateTestMDB;

         procedure Execution;
         procedure Compilation;

      published

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;
         procedure CompilationFailure;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TCOMConnectorTests ------------------
// ------------------

// SetUp
//
procedure TCOMConnectorTests.SetUp;
begin
   FTests:=TStringList.Create;
   FFailures:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'COMConnector'+PathDelim, '*.pas', FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'COMConnectorFailure'+PathDelim, '*.pas', FFailures);

   FCompiler:=TDelphiWebScript.Create(nil);
   FConnector:=TdwsComConnector.Create(nil);
   FConnector.Script:=FCompiler;

   FCW:=Get8087CW;

   ReCreateTestMDB;
end;

// TearDown
//
procedure TCOMConnectorTests.TearDown;
begin
   FConnector.Free;
   FCompiler.Free;

   FFailures.Free;
   FTests.Free;

   Set8087CW(FCW);
end;

// ReCreateTestMDB
//
procedure TCOMConnectorTests.ReCreateTestMDB;
var
   cat : OleVariant;
   conn : OleVariant;
begin
   DeleteFile('Data\Db.mdb');

   cat := CreateOleObject('ADOX.Catalog');
   cat.Create('Provider=Microsoft.Jet.OLEDB.4.0;Data Source=Data\Db.mdb;');

   conn := CreateOleObject('ADODB.Connection');
   conn.ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=Data\Db.mdb;Persist Security Info=False';
   conn.Open;

   conn.Execute('create table test (intCol INT, charCol VARCHAR(50), memoCol MEMO, dateCol DATE)');
   conn.Execute('insert into test values (10, ''ten'', ''value ten'', cdate(''2010-10-10''))');
   conn.Execute('insert into test values (20, ''twenty'', ''value twenty'', cdate(''2020-10-20''))');

   conn.Close;
end;

// Compilation
//
procedure TCOMConnectorTests.Compilation;
var
   source : TStringList;
   i : Integer;
   prog : TdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         finally
            prog.Free;
         end;

      end;

   finally
      source.Free;
   end;
end;

// CompilationNormal
//
procedure TCOMConnectorTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TCOMConnectorTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TCOMConnectorTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[];
   Execution;
end;

// ExecutionOptimized
//
procedure TCOMConnectorTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Execution;
end;

// CompilationFailure
//
procedure TCOMConnectorTests.CompilationFailure;
var
   source : TStringList;
   i : Integer;
   prog : TdwsProgram;
   expectedError : TStringList;
   expectedErrorsFileName : String;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   source:=TStringList.Create;
   expectedError:=TStringList.Create;
   try

      for i:=0 to FFailures.Count-1 do begin

         source.LoadFromFile(FFailures[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.txt');
            if FileExists(expectedErrorsFileName) then begin
               expectedError.LoadFromFile(expectedErrorsFileName);
               CheckEquals(expectedError.Text, prog.Msgs.AsInfo, FFailures[i]);
            end else Check(prog.Msgs.AsInfo<>'', FFailures[i]+': undetected error');
         finally
            prog.Free;
         end;

      end;

   finally
      expectedError.Free;
      source.Free;
   end;
end;

// Execution
//
procedure TCOMConnectorTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : TdwsProgram;
   resultsFileName : String;
   output : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);
         try
            CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
            prog.Execute;
            if prog.Msgs.Count=0 then
               output:=(prog.Result as TdwsDefaultResult).Text
            else begin
               output:= 'Errors >>>>'#13#10
                       +prog.Msgs.AsInfo
                       +'Result >>>>'#13#10
                       +(prog.Result as TdwsDefaultResult).Text;
            end;
            resultsFileName:=ChangeFileExt(FTests[i], '.txt');
            if FileExists(resultsFileName) then begin
               expectedResult.LoadFromFile(resultsFileName);
               CheckEquals(expectedResult.Text, output, FTests[i]);
            end else CheckEquals('', output, FTests[i]);
         finally
            prog.Free;
         end;

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('COMConnectorTests', TCOMConnectorTests.Suite);

end.
