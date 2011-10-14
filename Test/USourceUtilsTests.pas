unit USourceUtilsTests;

interface

uses Windows, Classes, SysUtils, TestFrameWork, dwsComp, dwsCompiler, dwsExprs,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsSuggestions;

type

   TSourceUtilsTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure BasicSuggestTest;
         procedure ObjectCreateTest;
         procedure ObjectSelfTest;
         procedure UnitDotTest;
         procedure MetaClassTest;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSourceUtilsTests ------------------
// ------------------

// SetUp
//
procedure TSourceUtilsTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.Config.CompilerOptions:=FCompiler.Config.CompilerOptions+[coSymbolDictionary, coContextMap];
end;

// TearDown
//
procedure TSourceUtilsTests.TearDown;
begin
   FCompiler.Free;
end;

// BasicSuggestTest
//
procedure TSourceUtilsTests.BasicSuggestTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'var printit : Boolean;'#13#10
                           +'PrintL');

   CheckTrue(prog.Msgs.HasErrors, 'compiled with errors');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 1);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>0, 'all suggestions');

   scriptPos.Col:=2;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>2, 'column 2');
   CheckEquals('printit', sugg.Code[0], 'sugg 2, 0');
   CheckEquals('Param', sugg.Code[1], 'sugg 2, 1');

   scriptPos.Col:=3;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(3, sugg.Count, 'column 3');
   CheckEquals('printit', sugg.Code[0], 'sugg 3, 0');
   CheckEquals('Print', sugg.Code[1], 'sugg 3, 1');
   CheckEquals('PrintLn', sugg.Code[2], 'sugg 3, 2');

   scriptPos.Col:=7;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 7');
   CheckEquals('PrintLn', sugg.Code[0], 'sugg 7, 0');
end;

// ObjectCreateTest
//
procedure TSourceUtilsTests.ObjectCreateTest;
const
   cBase = 'type TMyClass = class constructor CreateIt; class function CrDummy : Integer; method CrStuff; end;'#13#10;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile(cBase+'TObject.Create');
   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 10);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(3, sugg.Count, 'TObject.Create 10');
   CheckEquals('ClassName', sugg.Code[0], 'TObject.Create 10,0');
   CheckEquals('ClassType', sugg.Code[1], 'TObject.Create 10,1');
   CheckEquals('Create', sugg.Code[2], 'TObject.Create 10,2');
   scriptPos.Col:=11;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'TObject.Create 11');
   CheckEquals('Create', sugg.Code[0], 'TObject.Create 11,0');

   prog:=FCompiler.Compile(cBase+'TMyClass.Create');
   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 12);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(3, sugg.Count, 'TMyClass.Create 12');
   CheckEquals('CrDummy', sugg.Code[0], 'TMyClass.Create 12,0');
   CheckEquals('Create', sugg.Code[1], 'TMyClass.Create 12,1');
   CheckEquals('CreateIt', sugg.Code[2], 'TMyClass.Create 12,2');
   scriptPos.Col:=13;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count, 'TMyClass.Create 13');
   CheckEquals('Create', sugg.Code[0], 'TMyClass.Create 13,0');
   CheckEquals('CreateIt', sugg.Code[1], 'TMyClass.Create 13,1');

   prog:=FCompiler.Compile(cBase+'new TObject');
   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 6);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(3, sugg.Count, 'new TObject 6');
   CheckEquals('TMyClass', sugg.Code[0], 'new TObject 6,0');
   CheckEquals('TClass', sugg.Code[1], 'new TObject 6,1');
   CheckEquals('TObject', sugg.Code[2], 'new TObject 6,2');
   scriptPos.Col:=7;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'new TObject 7');
   CheckEquals('TObject', sugg.Code[0], 'new TObject 7,0');
end;

// ObjectSelfTest
//
procedure TSourceUtilsTests.ObjectSelfTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TMyClass = class constructor Create; procedure Test; end;'#13#10
                           +'procedure TMyClass.Test;begin'#13#10
                           +'Self.Create');
   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 9);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'Create 9');
   CheckEquals('Create', sugg.Code[0], 'Create 9,0');
   CheckEquals('TMyClass', (sugg.Symbols[0] as TMethodSymbol).StructSymbol.Name, 'Create 9,0 struct');

   prog:=FCompiler.Compile( 'type TMyClass = Class(TObject) Field : TMyClass; procedure first; procedure second; procedure third; End; '
                           +'procedure TMyClass.first; begin '#13#10
                           +'Self.Field.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 12);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>=9, 'Self 12');
   CheckEquals('Field', sugg.Code[0], 'Self 12,0');
   CheckEquals('first', sugg.Code[1], 'Self 12,1');
   CheckEquals('second', sugg.Code[2], 'Self 12,2');
   CheckEquals('third', sugg.Code[3], 'Self 12,3');
   CheckEquals('ClassName', sugg.Code[4], 'Self 12,4');
   CheckEquals('ClassType', sugg.Code[5], 'Self 12,5');
   CheckEquals('Create', sugg.Code[6], 'Self 12,6');
end;

// UnitDotTest
//
procedure TSourceUtilsTests.UnitDotTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('Internal.PrintL');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 11);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>2, 'column 11');
   CheckEquals('Pi', sugg.Code[0], 'sugg 11, 0');
   CheckEquals('Pos', sugg.Code[1], 'sugg 11, 1');

   scriptPos.Col:=12;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(0, sugg.Count, 'column 12');

   prog:=FCompiler.Compile('System.TObject');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 8);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>10, 'column 8');
   CheckEquals('Boolean', sugg.Code[0], 'sugg 8, 0');
   CheckEquals('EAssertionFailed', sugg.Code[1], 'sugg 8, 1');

   scriptPos.Col:=9;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(3, sugg.Count, 'column 12');
   CheckEquals('TClass', sugg.Code[0], 'sugg 12, 0');
   CheckEquals('TComplex', sugg.Code[1], 'sugg 12, 1');
   CheckEquals('TObject', sugg.Code[2], 'sugg 12, 2');
end;

// MetaClassTest
//
procedure TSourceUtilsTests.MetaClassTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('TClass.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 8);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count=0, 'TClass.');

   prog:=FCompiler.Compile('var v : TClass;'#13#10'v.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count=3, 'v.');
   CheckEquals('ClassName', sugg.Code[0], 'v. 0');
   CheckEquals('ClassType', sugg.Code[1], 'v. 1');
   CheckEquals('Create', sugg.Code[2], 'v. 2');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   TestFramework.RegisterTest('SourceUtilsTests', TSourceUtilsTests.Suite);

end.
