unit UdwsClassesTests;

interface

uses Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs,
   dwsClassesLibModule, dwsXPlatform, dwsSymbols, dwsUtils, dwsInfo;

type

   TdwsClassesTests = class (TTestCase)
      private
         FTests : TStringList;
         FCompiler : TDelphiWebScript;
         FClassesLib : TdwsClassesLib;

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

         procedure SymbolDescriptions;

         procedure CallAfterExec;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsClassesTests ------------------
// ------------------

// SetUp
//
procedure TdwsClassesTests.SetUp;
begin
   FTests:=TStringList.Create;

   CollectFiles(ExtractFilePath(ParamStr(0))+'ClassesLib'+PathDelim, '*.pas', FTests);

   FCompiler:=TDelphiWebScript.Create(nil);

   FClassesLib:=TdwsClassesLib.Create(nil);
   FClassesLib.Script:=FCompiler;
end;

// TearDown
//
procedure TdwsClassesTests.TearDown;
begin
   FClassesLib.Free;

   FCompiler.Free;

   FTests.Free;
end;

// Compilation
//
procedure TdwsClassesTests.Compilation;
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
procedure TdwsClassesTests.CompilationNormal;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize];
   Compilation;
end;

// CompilationWithMapAndSymbols
//
procedure TdwsClassesTests.CompilationWithMapAndSymbols;
begin
   FCompiler.Config.CompilerOptions:=[coSymbolDictionary, coContextMap, coAssertions];
   Compilation;
end;

// ExecutionNonOptimized
//
procedure TdwsClassesTests.ExecutionNonOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coAssertions];
   Execution;
end;

// ExecutionOptimized
//
procedure TdwsClassesTests.ExecutionOptimized;
begin
   FCompiler.Config.CompilerOptions:=[coOptimize, coAssertions];
   Execution;
end;

// SymbolDescriptions
//
procedure TdwsClassesTests.SymbolDescriptions;
var
   prog : IdwsProgram;
   stringsSymbol : TClassSymbol;
begin
   prog:=FCompiler.Compile('');
   stringsSymbol:=prog.Table.FindSymbol('TStrings', cvMagic) as TClassSymbol;
   CheckEquals('property Strings[x: Integer]: String read GetStrings write SetStrings; default;',
               stringsSymbol.Members.FindSymbol('Strings', cvPublic).Description, 'Strings Description');
end;

// Execution
//
procedure TdwsClassesTests.Execution;
var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   resultsFileName : String;
begin
   source:=TStringList.Create;
   expectedResult:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog:=FCompiler.Compile(source.Text);

         CheckEquals('', prog.Msgs.AsInfo, FTests[i]);
         exec:=prog.Execute;
         resultsFileName:=ChangeFileExt(FTests[i], '.txt');
         if FileExists(resultsFileName) then begin
            expectedResult.LoadFromFile(resultsFileName);
            CheckEquals(expectedResult.Text, exec.Result.ToString, FTests[i]);
         end else CheckEquals('', exec.Result.ToString, FTests[i]);
         CheckEquals('', exec.Msgs.AsInfo, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// CallAfterExec
//
procedure TdwsClassesTests.CallAfterExec;
var
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
   func : IInfo;
begin
   prog:=FCompiler.Compile( 'var s = TStrings.Create;'#13#10
                           +'s.Add("Line 1");'#13#10
                           +'s.Add("Line 2");'#13#10
                           +'procedure MyProc;'#13#10
                           +'begin'#13#10
                           +'  Print(s.Count)'#13#10
                           +'end;'#13#10
                           +'Print(s.Text)');
   CheckEquals('', prog.Msgs.AsInfo);

   exec:=prog.BeginNewExecution;
   try
      exec.RunProgram(0);

      CheckEquals('Line 1'#13#10'Line 2'#13#10, exec.Result.ToString);

      func:=exec.Info.Func['MyProc'];
      func.Call([]);

      CheckEquals('Line 1'#13#10'Line 2'#13#10'2', exec.Result.ToString);
   finally
      exec.EndProgram;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('LibModules', TdwsClassesTests);

end.
