unit UBigIntegerTests;

interface

uses
   Windows, Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsErrors,
   dwsBigIntegerFunctions, dwsXPlatform, dwsSymbols, dwsUtils,
   Velthuis.BigIntegers;

type

   TBigIntegerTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure Execution;
         procedure Compilation;

      published
         procedure BasicTest32;
         procedure BasicTest64;

         procedure CompilationNormal;
         procedure CompilationWithMapAndSymbols;
         procedure ExecutionNonOptimized;
         procedure ExecutionOptimized;

         procedure MultiThreadedExecution;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TBigIntegerTests ------------------
// ------------------

// SetUp
//
procedure TBigIntegerTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'BigInteger'+PathDelim, 'rabin*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
end;

// TearDown
//
procedure TBigIntegerTests.TearDown;
begin
   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TBigIntegerTests.Compilation;
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
procedure TBigIntegerTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TBigIntegerTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TBigIntegerTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TBigIntegerTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

type
   TCheckEquals = procedure (const expected, effective, msg : String) of object;

procedure RunTests(testList : TStrings; compiler : TDelphiWebScript;
                   checkEquals :  TCheckEquals; rounds : Integer);
var
   source, expectedResult : TStringList;
   i, j : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultText, resultsFileName, expectedResultText : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to testList.Count-1 do begin

         source.LoadFromFile(testList[i]);

         prog:=compiler.Compile(source.Text);

         checkEquals('', prog.Msgs.AsInfo, testList[i]);

         resultsFileName:=ChangeFileExt(testList[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            expectedResultText := expectedResult.Text;
         end else expectedResultText := '';

         for j := 1 to rounds do begin
            exec:=prog.Execute;

            resultText:=exec.Result.ToString;
            if exec.Msgs.Count>0 then
               resultText:=resultText+#13#10'>>>> Error(s): '#13#10+exec.Msgs.AsInfo;

            checkEquals(expectedResultText, resultText, testList[i]);
            checkEquals('', exec.Msgs.AsInfo, testList[i]);
         end;

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// Execution
//
procedure TBigIntegerTests.Execution;
begin
   RunTests(FTests, FCompiler, CheckEquals, 1);
end;

// BasicTest32
//
procedure TBigIntegerTests.BasicTest32;
var
   a, b, c : BigInteger;
begin
   a := 1;
   CheckEquals('1', a.ToHexString, '1');
   b := -1;
   CheckEquals('-1', b.ToHexString, '-1');

   CheckTrue(a=a, '1 = 1');
   CheckFalse(a<>a, '1 <> 1');
   CheckTrue(a<>b, '1 <> -1');
   CheckTrue(b=b, '-1 = -1');
   CheckFalse(b<>b, '-1 <> -1');
   CheckTrue(b<>a, '-1 <> 1');

   c:=a+b;
   CheckEquals('0', c.ToHexString, '1+(-1)');
   c:=a-b;
   CheckEquals('2', c.ToHexString, '1-(-1)');

   c:=b+a;
   CheckEquals('0', c.ToHexString, '(-1)+1');
   c:=b-a;
   CheckEquals('-2', c.ToHexString, '(-1)-1');
end;

// BasicTest64
//
procedure TBigIntegerTests.BasicTest64;
var
   a, b, c : BigInteger;
begin
   a := High(Int64);
   CheckEquals('7FFFFFFFFFFFFFFF', a.ToHexString, 'hi');
   b := Low(Int64);
   CheckEquals('-8000000000000000', b.ToHexString, 'lo');

   CheckTrue(a=a, 'hi = hi');
   CheckFalse(a<>a, 'hi <> hi');
   CheckTrue(a<>b, 'hi <> lo');
   CheckTrue(b=b, 'lo = lo');
   CheckFalse(b<>b, 'lo <> lo');
   CheckTrue(b<>a, 'lo <> hi');

   c:=a+b;
   CheckEquals('-1', c.ToHexString, '1+(-1)');
   c:=a-b;
   CheckEquals('FFFFFFFFFFFFFFFF', c.ToHexString, '1-(-1)');

   c:=b+a;
   CheckEquals('-1', c.ToHexString, '(-1)+1');
   c:=b-a;
   CheckEquals('-FFFFFFFFFFFFFFFF', c.ToHexString, '(-1)-1');
end;

type
   TThreadRunner = class(TThread)
      FTests : TBigIntegerTests;
      FMessage : String;
      procedure CheckEquals(const expected, effective, msg : String);
      procedure Execute; override;

   end;

// Execute
//
procedure TThreadRunner.Execute;
begin
   RunTests(FTests.FTests, FTests.FCompiler, CheckEquals, 20);
end;

// CheckEquals
//
procedure TThreadRunner.CheckEquals(const expected, effective, msg : String);
begin
   if expected <> effective then
      FMessage := Format('expected << %s >> but got << %s >> (%s)', [expected, effective, msg]);
end;

// MultiThreadedExecution
//
procedure TBigIntegerTests.MultiThreadedExecution;
var
   i : Integer;
   t : array [0..3] of TThreadRunner;
begin
   for i := 0 to High(t) do begin
      t[i] := TThreadRunner.Create(True);
      t[i].FTests := Self;
      t[i].Start;
   end;
   for i := 0 to High(t) do t[i].WaitFor;
   try
      for i := 0 to High(t) do
         if t[i].FMessage <> '' then
            Check(False, t[i].FMessage);
   finally
      for i := 0 to High(t) do t[i].Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('BigIntegerTests', TBigIntegerTests);

end.
