unit UCOMConnectorTests;

{$I dws.inc}

interface

uses
   Winapi.Windows, System.Classes, System.SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsComConnector, System.Variants, Winapi.ActiveX, System.Win.ComObj, dwsXPlatform, dwsUtils,
   dwsEncodingLibModule, dwsCompilerContext;

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
   TdwsEncodingLib.Create(FCompiler).dwsEncoding.Script := FCompiler;

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

const
   cJetProvider = 'Microsoft.Jet.OLEDB.4.0';
   cACEProvider = 'Microsoft.ACE.OLEDB.12.0';

// ReCreateTestMDB
//
procedure TCOMConnectorTests.ReCreateTestMDB;
var
   cat: OleVariant;
   conn: OleVariant;
   provider: String;
begin
   DeleteFile('Data\Db.mdb');

   {$ifdef WIN32}
   provider := cJetProvider;
   {$else}
   provider := cACEProvider;
   {$endif}

   try
      cat := CreateOleObject('ADOX.Catalog');
   except
      on E: Exception do
         raise Exception.Create('Could not create ADOX.Catalog. Please ensure Microsoft ADO and Access Database Engine are installed.'#13#10 + E.Message);
   end;

   try
      cat.Create('Provider=' + provider + ';Data Source=Data\Db.mdb;');
   except
      on E: Exception do
         raise Exception.Create('Could not create database with provider ' + provider + '. Please ensure the Microsoft Access Database Engine (64-bit for Win64) is installed.'#13#10 + E.Message);
   end;

   conn := CreateOleObject('ADODB.Connection');
   conn.ConnectionString := 'Provider=' + provider + ';Data Source=Data\Db.mdb;Persist Security Info=False';
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
procedure TCOMConnectorTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TCOMConnectorTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TCOMConnectorTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TCOMConnectorTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// CompilationFailure
//
procedure TCOMConnectorTests.CompilationFailure;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
   expectedError : TStringList;
   expectedErrorsFileName : String;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   source:=TStringList.Create;
   expectedError:=TStringList.Create;
   try

      for i:=0 to FFailures.Count-1 do begin

         source.LoadFromFile(FFailures[i]);

         prog:=FCompiler.Compile(source.Text);

         expectedErrorsFileName:=ChangeFileExt(FFailures[i], '.txt');
         if FileExists(expectedErrorsFileName) then begin
            expectedError.LoadFromFile(expectedErrorsFileName);
            CheckEquals(expectedError.Text, prog.Msgs.AsInfo, FFailures[i]);
         end else Check(prog.Msgs.AsInfo<>'', FFailures[i]+': undetected error');

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
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultsFileName : String;
   output : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         if exec.Msgs.Count=0 then
            output:=exec.Result.ToString
         else begin
            output:= 'Errors >>>>'#13#10
                    +exec.Msgs.AsInfo
                    +'Result >>>>'#13#10
                    +exec.Result.ToString;
         end;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, output, FTests[i]);
         end else CheckEquals('', output, FTests[i]);

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

   RegisterTest('COMConnectorTests', TCOMConnectorTests);

end.
