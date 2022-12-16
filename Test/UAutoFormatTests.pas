unit UAutoFormatTests;

{$I ..\Source\dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsXplatform,
   dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsErrors, dwsUtils, Variants, dwsSymbols, dwsSuggestions,
   dwsFunctions, dwsCaseNormalizer, dwsScriptSource, dwsSymbolDictionary,
   dwsCompilerContext, dwsUnicode, dwsJSONConnector, dwsUnitSymbols,
   dwsAutoFormat, dwsPascalTokenizer;

type

   TAutoFormatTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FTests : TStringList;
         FAutoFormat : TdwsAutoFormat;

         procedure DoInclude(const scriptName: String; var scriptSource: String);

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure CodeStillCompiles;

         procedure SimpleNewLines;
         procedure SimpleBeginEndBlocks;
         procedure SimpleRepeat;
         procedure SimpleFuncs;
         procedure SimpleStrings;
         procedure SkipComments;
         procedure SimpleClass;

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

   FAutoFormat := TdwsAutoFormat.Create(TPascalTokenizerStateRules.Create);
end;

// TearDown
//
procedure TAutoFormatTests.TearDown;
begin
   FAutoFormat.Free;
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
         prog := FCompiler.Compile(
            FAutoFormat.Process(source.Text),
            'Test\'+ExtractFileName(FTests[i])
         );

         CheckEquals(False, prog.Msgs.HasErrors, FTests[i]+#13#10+prog.Msgs.AsInfo);
      end;

   finally
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
      'type TMy = class'#10'end;'#10'type TMyClass = class of TMy;'#10,
      FAutoFormat.Process('type TMy=class end;type TMyClass=class of TMy;')
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
