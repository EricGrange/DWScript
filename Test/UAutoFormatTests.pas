unit UAutoFormatTests;

{$I ..\Source\dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsXPlatform,
   dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsErrors, dwsUtils, Variants, dwsSymbols, dwsSuggestions,
   dwsFunctions, dwsCaseNormalizer, dwsScriptSource, dwsSymbolDictionary,
   dwsCompilerContext, dwsUnicode, dwsJSONConnector, dwsUnitSymbols,
   dwsAutoFormat, dwsPascalTokenizer, dwsCodeDOMParser, dwsCodeDOMPascalParser;

type

   TAutoFormatTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FTests : TStringList;
         FAutoFormat : TdwsAutoFormat;
         FPascalRules : TdwsCodeDOMPascalParser;
         FTokRules : TTokenizerRules;

         procedure DoInclude(const scriptName: String; var scriptSource: String);

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure CodeStillCompiles;
         procedure CodeStillExecutes;

         procedure SimpleNewLines;
         procedure SimpleBeginEndBlocks;
         procedure SimpleRepeat;
         procedure SimpleWhile;
         procedure SimpleIfThenElse;
         procedure SimpleFuncs;
         procedure SimpleStrings;
         procedure SkipComments;
         procedure SimpleClass;
         procedure Conditionals;
         procedure CaseOf;
         procedure ForLoop;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TAutoFormatTests ------------------
// ------------------

// SetUp
//
procedure TAutoFormatTests.SetUp;
const
   cFilter = '*.pas';
var
   basePath : String;
begin
   FTests:=TStringList.Create;

   basePath:=ExtractFilePath(ParamStr(0));

   CollectFiles(basePath+'SimpleScripts'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'ArrayPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'LambdaPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'InterfacesPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'OperatorOverloadPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'OverloadsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'HelpersPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'PropertyExpressionsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'SetOfPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'AssociativePass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'GenericsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'InnerClassesPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'Algorithms'+PathDelim, cFilter, FTests);

   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.Config.CompilerOptions:=FCompiler.Config.CompilerOptions+[coSymbolDictionary, coContextMap];
   FCompiler.OnInclude:=DoInclude;

   FPascalRules := TdwsCodeDOMPascalParser.Create;
   FTokRules := TPascalTokenizerStateRules.Create;

   FAutoFormat := TdwsAutoFormat.Create(
      TdwsParser.Create(FTokRules.CreateTokenizer(nil, nil), FPascalRules.CreateRules)
   );
end;

// TearDown
//
procedure TAutoFormatTests.TearDown;
begin
   FAutoFormat.Free;
   FTokRules.Free;
   FPascalRules.Free;
   FCompiler.Free;
   FTests.Free;
end;

// DoInclude
//
procedure TAutoFormatTests.DoInclude(const scriptName: String; var scriptSource: String);
begin
   scriptSource := LoadTextFromFile('SimpleScripts\'+scriptName);
end;

// CodeStillCompiles
//
procedure TAutoFormatTests.CodeStillCompiles;
var
   source : TStringList;
   i : Integer;
   prog : IdwsProgram;
begin
   source:=TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);
         var processed := FAutoFormat.Process(source.Text);
         prog := FCompiler.Compile(
            processed,
            'Test\'+ExtractFileName(FTests[i])
         );

         CheckEquals(
            False, prog.Msgs.HasErrors,
            FTests[i] + #13#10 + prog.Msgs.AsInfo + #13#10 + processed
         );
      end;

   finally
      source.Free;
   end;
end;

// CodeStillExecutes
//
procedure TAutoFormatTests.CodeStillExecutes;

   function AsInfoWithoutPosition(msgs : TdwsMessageList) : String;
   begin
      if msgs.Count = 0 then Exit;
      Result := msgs[0].Text + #10;
      for var i := 1 to msgs.Count-1 do
         Result := #0 + msgs[i].Text;
   end;

   function FilterLocations(const s : String) : String;
   begin
      Result := s;
      repeat
         var p := Pos('[line', Result);
         if p <= 0 then Exit;
         var p2 := Pos(']', Result, p+5);
         if p2 <= 0 then Exit;
         Result := Copy(Result, 1, p) + Copy(Result, p2);
      until False;
   end;

var
   source, expectedResult : TStringList;
   i : Integer;
   prog : IdwsProgram;
   exec : IdwsProgramExecution;
begin
   source:=TStringList.Create;
   expectedResult := TStringList.Create;
   try

      for i:=0 to FTests.Count-1 do begin

         source.LoadFromFile(FTests[i]);

         prog := FCompiler.Compile(
            source.Text,
            'Test\'+ExtractFileName(FTests[i])
         );

         CheckEquals(
            False, prog.Msgs.HasErrors,
            FTests[i] + ' fail pre-processed compilation'#13#10 + prog.Msgs.AsInfo
         );

         exec := prog.Execute;
         var originalOutput := exec.Result.ToString;
         if exec.Msgs.Count > 0 then
            originalOutput := originalOutput+#13#10+'>>> Runtime Error: '+AsInfoWithoutPosition(exec.Msgs);

         var processed := FAutoFormat.Process(source.Text);

         prog := FCompiler.Compile(
            processed,
            'Test\'+ExtractFileName(FTests[i])
         );

         CheckEquals(
            False, prog.Msgs.HasErrors,
            FTests[i] + ' fails post-formatting' + #13#10 + prog.Msgs.AsInfo + #13#10 + processed
         );

         exec := prog.Execute;
         var output := exec.Result.ToString;
         if exec.Msgs.Count>0 then
            output:=output+#13#10+'>>> Runtime Error: '+AsInfoWithoutPosition(exec.Msgs);

         originalOutput := FilterLocations(originalOutput);
         output := FilterLocations(output);

         CheckEquals(originalOutput, output, FTests[i]);

      end;

   finally
      expectedResult.Free;
      source.Free;
   end;
end;

// SimpleNewLines
//
procedure TAutoFormatTests.SimpleNewLines;
begin
   CheckEquals(
      'var i := 1;'#10'i := i + 1;'#10,
      FAutoFormat.Process(#9#9'var'#9'i:=1;i:=i   +   1   ;    '#10)
   );
   CheckEquals(
      'i := a + b;'#10,
      FAutoFormat.Process('i'#9' := '#9' a+'#9#9'b'#9'  ;'#9#9)
   );
   CheckEquals(
      'abc := +2;'#10,
      FAutoFormat.Process(' abc:=+2;')
   );
   CheckEquals(
      'ab := 0.1e+10;'#10'cd := -45e-1;'#10,
      FAutoFormat.Process(' ab:=0.1e+10;cd:=-45e-1;'#10)
   );
end;

// SimpleBeginEndBlocks
//
procedure TAutoFormatTests.SimpleBeginEndBlocks;
begin
   CheckEquals(
      'begin'#10#9'i := 2;'#10'end;'#10,
      FAutoFormat.Process('Begin i:=2;END;')
   );
   CheckEquals(
      'begin'#10#9'var i := 1;'#10#9'begin'#10#9#9'i := 2;'#10#9'end;'#10'end;'#10,
      FAutoFormat.Process('beGin var i := 1;Begin i:=2;END; end;')
   );
   CheckEquals(
      'beginning := 1;'#10'begin'#10#9'ending := 2;'#10'end;'#10,
      FAutoFormat.Process('beginning:=1;begin ending:=2;end;'#10)
   );
end;

// SimpleRepeat
//
procedure TAutoFormatTests.SimpleRepeat;
begin
   CheckEquals(
      'repeat'#10#9'i += 1;'#10'until i >= 10;'#10,
      FAutoFormat.Process('rePeat i+=1; until i>=10;')
   );
end;

// SimpleWhile
//
procedure TAutoFormatTests.SimpleWhile;
begin
   CheckEquals(
      'while i > 0 do'#10#9'i -= 1;'#10'Done()'#10,
      FAutoFormat.Process('While i>0 do i-=1;Done()')
   );
   CheckEquals(
      'while i > 0 do begin'#10#9'i -= 1;'#10'end;'#10'Done()'#10,
      FAutoFormat.Process('While i>0 do begin i-=1;end;Done()')
   );
end;

// SimpleIfThenElse
//
procedure TAutoFormatTests.SimpleIfThenElse;
begin
//   CheckEquals(
//      'if b then'#10#9'doit()'#10'else dont();'#10'done()'#10,
//      FAutoFormat.Process('if b then doit() else dont();done()')
//   );
   CheckEquals(
      'if b then begin'#10#9'if not b then begin'#10#9'end else begin'#10#9'end'#10'end'#10,
      FAutoFormat.Process('if b then begin if not b then begin end else begin end end')
   );
end;

// SimpleFuncs
//
procedure TAutoFormatTests.SimpleFuncs;
begin
   CheckEquals(
      'procedure Hello;'#10'begin'#10#9'i := 1'#10'end;'#10,
      FAutoFormat.Process('procedure Hello;begin i:=1 end;')
   );
   CheckEquals(
      'procedure PrintBool(v : Variant);'#10'begin'#10#9'PrintLn(if v then ''True'' else ''False'');'#10'end;'#10,
      FAutoFormat.Process('procedure PrintBool(v:Variant);begin PrintLn(if v then''True'' else''False'');end;')
   );
end;

// SimpleStrings
//
procedure TAutoFormatTests.SimpleStrings;
begin
   CheckEquals(
      's := '''';'#10'b := "Hello";'#10,
      FAutoFormat.Process('s:='''';b:="Hello";')
   );
   CheckEquals(
      's := #9"abc"#10'#10,
      FAutoFormat.Process('s:=#9"abc"#10')
   );
end;

// SkipComments
//
procedure TAutoFormatTests.SkipComments;
begin
   CheckEquals(
      'i := 1; // begin'#10'i := 2;'#10'// end'#10,
      FAutoFormat.Process('i:=1;// begin'#10'i:=2;'#10'// end')
   );
   CheckEquals(
      '(* /* *)'#10'i := 1; // begin i:=1'#10'i *= 2 /*end*/ 1;'#10,
      FAutoFormat.Process('(* /* *)'#10'i:=1; // begin i:=1'#10'i*=2/*end*/1;'#10)
   )
end;

// SimpleClass
//
procedure TAutoFormatTests.SimpleClass;
begin
   CheckEquals(
      'type TMy = class end;'#10'type TMyClass = class of TMy;'#10,
      FAutoFormat.Process('type TMy=class end;type TMyClass=class of TMy;')
   );
end;

// Conditionals
//
procedure TAutoFormatTests.Conditionals;
begin
   CheckEquals(
      '{$ifdef A}'#10'b;'#10'{$endif}'#10,
      FAutoFormat.Process('{$ifdef A}'#10'b;'#10'{$endif}'#10)
   );
   CheckEquals(
      '{$ifdef A}b;{$endif}'#10,
      FAutoFormat.Process('{$ifdef A}b;{$endif}'#10)
   );
end;

// CaseOf
//
procedure TAutoFormatTests.CaseOf;
begin
   CheckEquals(
      'case a of'#10#9'-1..+1 : b'#10'end'#10,
      FAutoFormat.Process('case a of -1..+1:b end'#10)
   );
   CheckEquals(
      'case a of'#10#9'1, 2 : b;'#10#9'3 : c;'#10'else'#10#9'd()'#10'end;'#10,
      FAutoFormat.Process('case a of 1,2:b;3:c;else d()end;')
   );
end;

// ForLoop
//
procedure TAutoFormatTests.ForLoop;
begin
   CheckEquals(
      'for i := 1 to 9 do'#10#9'PrintLn(i);'#10,
      FAutoFormat.Process('for i:=1 to 9 do PrintLn(i);'#10)
   );
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('AutoFormatTests', TAutoFormatTests);

end.
