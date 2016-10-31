unit UdwsEncodingTests;

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsEncodingLibModule, dwsXPlatform, dwsEncoding;

type

   TdwsEncodingTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FEncodingLib : TdwsEncodingLib;

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

         procedure Base64Test;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsEncodingTests ------------------
// ------------------

// SetUp
//
procedure TdwsEncodingTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'EncodingLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FEncodingLib:=TdwsEncodingLib.Create(nil);
   FEncodingLib.dwsEncoding.Script:=FCompiler;
end;

// TearDown
//
procedure TdwsEncodingTests.TearDown;
begin
   FEncodingLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsEncodingTests.Compilation;
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
procedure TdwsEncodingTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsEncodingTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsEncodingTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsEncodingTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// Execution
//
procedure TdwsEncodingTests.Execution;
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

// Base64Test
//
procedure TdwsEncodingTests.Base64Test;
begin
   CheckEquals('', Base64Encode(''));
   CheckEquals('MA==', Base64Encode('0'));
   CheckEquals('MDE=', Base64Encode('01'));
   CheckEquals('MDEy', Base64Encode('012'));
   CheckEquals('MDEyMw==', Base64Encode('0123'));
   CheckEquals('MDEyMzQ=', Base64Encode('01234'));
   CheckEquals('MDEyMzQ1', Base64Encode('012345'));
   CheckEquals('MDEyMzQ1Ng==', Base64Encode('0123456'));

   CheckEquals('', Base64Decode(''));
   CheckEquals('0', Base64Decode('MA=='));
   CheckEquals('01', Base64Decode('MDE='));
   CheckEquals('012', Base64Decode('MDEy'));
   CheckEquals('0123', Base64Decode('MDEyMw=='));
   CheckEquals('01234', Base64Decode('MDEyMzQ='));
   CheckEquals('012345', Base64Decode('MDEyMzQ1'));
   CheckEquals('0123456', Base64Decode('MDEyMzQ1Ng=='));

   CheckEquals('012345', Base64Decode('MDEy MzQ1'), 'whitespace');
   CheckEquals('012345', Base64Decode('MDEy'#13#10'MzQ1'), 'crlf');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('LibModules', TdwsEncodingTests);

end.
