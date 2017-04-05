unit UJITTests;

interface

uses
   Classes, SysUtils, Variants,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsXPlatform,
   dwsTokenizer, dwsErrors, dwsUtils, dwsSymbols, dwsFunctions,
   dwsJITFixups, dwsJITx86, dwsJITx86Intrinsics, dwsCompilerContext;

type

   TJITTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FStream : TWriteOnlyBlockStream;
         FFixups : TFixupLogic;

      protected
         procedure DoInclude(const scriptName : UnicodeString; var scriptSource: UnicodeString);
         function  DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;

         function GetStreamPosition : Integer;

         function GetExpectedResult(const fileName : UnicodeString) : UnicodeString;

         procedure Execution;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure FixupSimpleTests;
         procedure FixupConditionalBackwardJump;
         procedure FixupConditionalForwardJump;
         procedure FixupConditionalForwardJumpSpaced;

         procedure ExecutionOptimized;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TJITTests ------------------
// ------------------

// SetUp
//
procedure TJITTests.SetUp;
const
   cBaseFilter = '*';
var
   pasFilter : String;
   dwsFilter : String;
begin
   SetDecimalSeparator('.');

   FTests:=TStringList.Create;

   pasFilter:=cBaseFilter+'.pas';
   dwsFilter:=cBaseFilter+'.dws';

   CollectFiles(ExtractFilePath(ParamStr(0))+'Algorithms'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'SimpleScripts'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'BuildScripts'+PathDelim, dwsFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'InterfacesPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'OverloadsPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'HelpersPass'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'PropertyExpressionsPass'+PathDelim, pasFilter, FTests);

   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsMath'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsString'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsTime'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsVariant'+PathDelim, pasFilter, FTests);
   CollectFiles(ExtractFilePath(ParamStr(0))+'FunctionsRTTI'+PathDelim, pasFilter, FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.OnInclude:=DoInclude;
   FCompiler.OnNeedUnit:=DoNeedUnit;
   FCompiler.Config.HintsLevel:=hlPedantic;

   FStream:=TWriteOnlyBlockStream.Create;
   FFixups:=TFixupLogic.Create;
   FFixups.OnNeedLocation:=GetStreamPosition;
end;

// TearDown
//
procedure TJITTests.TearDown;
begin
   FStream.Free;
   FCompiler.Free;
   FFixups.Free;
   FTests.Free;
end;

// DoInclude
//
procedure TJITTests.DoInclude(const scriptName: UnicodeString; var scriptSource: UnicodeString);
var
   fileName : UnicodeString;
begin
   fileName := 'SimpleScripts\'+scriptName;
   if not FileExists(fileName) then
      fileName := 'BuildScripts\'+scriptName;
   scriptSource := LoadTextFromFile(fileName);
end;

// DoNeedUnit
//
function TJITTests.DoNeedUnit(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;
var
   sl : TStringList;
   fName : UnicodeString;
begin
   fName:='BuildScripts\' + unitName + '.pas';
   if not FileExists(fName) then Exit(nil);
   sl := TStringList.Create;
   try
      sl.LoadFromFile(fName);
      unitSource := sl.Text;
   finally
      sl.Free;
   end;
   Result:=nil;
end;

// GetStreamPosition
//
function TJITTests.GetStreamPosition : Integer;
begin
   Result:=FStream.Position;
end;

// GetExpectedResult
//
function TJITTests.GetExpectedResult(const fileName : UnicodeString) : UnicodeString;
var
   expectedResult : TStringList;
   resultsFileName : String;
begin
   expectedResult:=TStringList.Create;
   try
      resultsFileName:=ChangeFileExt(fileName, '.optimized.txt');
      if FileExists(resultsFileName) then
         expectedResult.LoadFromFile(resultsFileName)
      else begin
         resultsFileName:=ChangeFileExt(fileName, '.txt');
         if FileExists(resultsFileName) then
            expectedResult.LoadFromFile(resultsFileName)
         else expectedResult.Clear;
      end;
      Result:=expectedResult.Text;
   finally
      expectedResult.Free;
   end;
end;

// Execution
//
procedure TJITTests.Execution;
var
   source : TStringList;
   i, ignored : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   output, expectedResult : String;
   diagnostic : TStringList;
   jit : TdwsJITx86;
begin
   ignored:=0;
   diagnostic:=TStringList.Create;
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         if    (Pos('Algorithms', FTests[i])>1)
            or (Pos('Functions', FTests[i])>1)
            or (Pos('Lambda', FTests[i])>1) then
            FCompiler.Config.HintsLevel:=hlStrict
         else FCompiler.Config.HintsLevel:=hlPedantic;

         prog:=FCompiler.Compile(source.Text);

         if prog.Msgs.HasErrors then begin
            CheckEquals(GetExpectedResult(FTests[i]),
                         'Errors >>>>'#13#10
                        +prog.Msgs.AsInfo,
                        FTests[i]);
            Inc(ignored);
            continue;
         end;

         OutputDebugString(FTests[i]);

         try
            jit:=TdwsJITx86.Create;
            try
               jit.GreedyJIT(prog.ProgramObject);
            finally
               jit.Free;
            end;

            exec:=prog.Execute;
         except
            on E: Exception do
               raise Exception.Create(FTests[i]+', '+E.ClassName+': '+E.Message)
         end;

         if prog.Msgs.Count+exec.Msgs.Count=0 then
            output:=exec.Result.ToUnicodeString
         else begin
            output:= 'Errors >>>>'#13#10
                    +prog.Msgs.AsInfo
                    +exec.Msgs.AsInfo
                    +'Result >>>>'#13#10
                    +exec.Result.ToUnicodeString;
         end;

         expectedResult:=GetExpectedResult(FTests[i]);
         if not (expectedResult=output) then begin
            diagnostic.Add( ExtractFileName(FTests[i])
                           +': expected <'+expectedResult
                           +'> but got <'+output+'>');
         end;

      end;

      CheckEquals(0, diagnostic.Count,
                  Format('%d / %d tests passed (%d ignored)'#13#10'%s',
                         [FTests.Count-diagnostic.Count-ignored, FTests.Count-ignored, ignored,
                          diagnostic.Text]));
   finally
      diagnostic.Free;
      source.Free;
   end;
end;

// FixupSimpleTests
//
procedure TJITTests.FixupSimpleTests;
var
   rel : TFixupRelativeOffset;
   outbuf : TBytes;
begin
   FStream.Clear;

   FStream.WriteByte(Ord('a'));
   rel:=FFixups.NewRelativeOffset(1);
   FStream.WriteByte(Ord('b'));
   rel.NewTarget(False);
   FStream.WriteByte(Ord('c'));

   FFixups.FlushFixups(FStream.ToBytes, FStream);
   outBuf:=FStream.ToBytes;

   CheckEquals(4, Length(outbuf));
   CheckEquals(Ord('a'), outbuf[0], '0');
   CheckEquals(2, outbuf[1], '1');
   CheckEquals(Ord('b'), outbuf[2], '2');
   CheckEquals(Ord('c'), outbuf[3], '3');
end;

// FixupConditionalBackwardJump
//
procedure TJITTests.FixupConditionalBackwardJump;
var
   target : TFixupTarget;
   jump : TFixupJump;
   outbuf : TBytes;
begin
   FStream.Clear;

   target:=FFixups.NewTarget(False);
   FStream.WriteByte($90);

   jump:=TFixupJump.Create(flagsZ);
   jump.Target:=target;
   FFixups.AddFixup(jump);

   jump:=TFixupJump.Create(flagsNZ);
   jump.Target:=target;
   FFixups.AddFixup(jump);

   FFixups.FlushFixups(FStream.ToBytes, FStream);
   outbuf:=FStream.ToBytes;

   CheckEquals(5, Length(outbuf));
   CheckEquals($90, outbuf[0], '0');
   CheckEquals($74, outbuf[1], '1');
   CheckEquals($FD, outbuf[2], '2');
   CheckEquals($75, outbuf[3], '3');
   CheckEquals($FB, outbuf[4], '4');
end;

// FixupConditionalForwardJump
//
procedure TJITTests.FixupConditionalForwardJump;
var
   target : TFixupTarget;
   jump0, jump1 : TFixupJump;
   outbuf : TBytes;
begin
   FStream.Clear;

   jump0:=TFixupJump.Create(flagsZ);
   FFixups.AddFixup(jump0);

   jump1:=TFixupJump.Create(flagsNZ);
   FFixups.AddFixup(jump1);

   FStream.WriteByte($90);

   target:=FFixups.NewTarget(False);
   jump0.Target:=target;
   jump1.Target:=target;

   FFixups.FlushFixups(FStream.ToBytes, FStream);
   outbuf:=FStream.ToBytes;

   CheckEquals(5, Length(outbuf));
   CheckEquals($74, outbuf[0], '0');
   CheckEquals($03, outbuf[1], '1');
   CheckEquals($75, outbuf[2], '2');
   CheckEquals($01, outbuf[3], '3');
   CheckEquals($90, outbuf[4], '4');
end;

// FixupConditionalForwardJumpSpaced
//
procedure TJITTests.FixupConditionalForwardJumpSpaced;
var
   target : TFixupTarget;
   jump0, jump1 : TFixupJump;
   outbuf : TBytes;
begin
   FStream.Clear;

   jump0:=TFixupJump.Create(flagsZ);
   FFixups.AddFixup(jump0);

   FStream.WriteByte($11);

   jump1:=TFixupJump.Create(flagsNZ);
   FFixups.AddFixup(jump1);

   FStream.WriteByte($90);

   target:=FFixups.NewTarget(False);
   jump0.Target:=target;
   jump1.Target:=target;

   FStream.WriteByte($22);

   FFixups.FlushFixups(FStream.ToBytes, FStream);
   outbuf:=FStream.ToBytes;

   CheckEquals(7, Length(outbuf));
   CheckEquals($74, outbuf[0], '0');
   CheckEquals($04, outbuf[1], '1');
   CheckEquals($11, outbuf[2], '2');
   CheckEquals($75, outbuf[3], '3');
   CheckEquals($01, outbuf[4], '4');
   CheckEquals($90, outbuf[5], '5');
   CheckEquals($22, outbuf[6], '6');
end;

// ExecutionOptimized
//
procedure TJITTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('jitTests', TJITTests);

end.
