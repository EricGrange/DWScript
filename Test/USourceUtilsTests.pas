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
         procedure EmptyOptimizedLocalTable;
         procedure StringTest;
         procedure StaticArrayTest;
         procedure DynamicArrayTest;
         procedure HelperSuggestTest;
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
   CheckEquals(5, sugg.Count, 'column 5');
   CheckEquals('printit', sugg.Code[0], 'sugg 5, 0');
   CheckEquals('Print', sugg.Code[1], 'sugg 5, 1');
   CheckEquals('PrintLn', sugg.Code[2], 'sugg 5, 2');
   CheckEquals('procedure', sugg.Code[3], 'sugg 5, 2');
   CheckEquals('property', sugg.Code[4], 'sugg 5, 2');

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
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
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
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckEquals(0, sugg.Count, 'column 12');

   prog:=FCompiler.Compile('System.TObject');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 8);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>10, 'column 8');
   CheckEquals('Boolean', sugg.Code[0], 'sugg 8, 0');
   CheckEquals('EAssertionFailed', sugg.Code[1], 'sugg 8, 1');

   scriptPos.Col:=9;
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckEquals(4, sugg.Count, 'column 9');
   CheckEquals('TClass', sugg.Code[0], 'sugg 9, 0');
   CheckEquals('TComplex', sugg.Code[1], 'sugg 9, 1');
   CheckEquals('TObject', sugg.Code[2], 'sugg 9, 2');
   CheckEquals('TVector', sugg.Code[3], 'sugg 9, 3');
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
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=0, 'TClass.');

   prog:=FCompiler.Compile('var v : TClass;'#13#10'v.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=3, 'v.');
   CheckEquals('ClassName', sugg.Code[0], 'v. 0');
   CheckEquals('ClassType', sugg.Code[1], 'v. 1');
   CheckEquals('Create', sugg.Code[2], 'v. 2');
end;

// EmptyOptimizedLocalTable
//
procedure TSourceUtilsTests.EmptyOptimizedLocalTable;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   FCompiler.Config.CompilerOptions:=FCompiler.Config.CompilerOptions+[coOptimize];

   prog:=FCompiler.Compile('procedure Dummy;'#13#10'begin begin'#13#10#13#10'end end'#13#10);

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 3);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);

   FCompiler.Config.CompilerOptions:=FCompiler.Config.CompilerOptions-[coOptimize];
end;

// StringTest
//
procedure TSourceUtilsTests.StringTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('var s:='''';'#13#10's.h');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckTrue(sugg.Count=3, 's.');
   CheckEquals('High', sugg.Code[0], 's. 0');
   CheckEquals('Length', sugg.Code[1], 's. 1');
   CheckEquals('Low', sugg.Code[2], 's. 2');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=1, 's.h');
   CheckEquals('High', sugg.Code[0], 's.h 0');
end;

// StaticArrayTest
//
procedure TSourceUtilsTests.StaticArrayTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('var s : array [0..2] of Integer;'#13#10's.h');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckTrue(sugg.Count=3, 's.');
   CheckEquals('High', sugg.Code[0], 's. 0');
   CheckEquals('Length', sugg.Code[1], 's. 1');
   CheckEquals('Low', sugg.Code[2], 's. 2');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=1, 's.h');
   CheckEquals('High', sugg.Code[0], 's.h 0');
end;

// DynamicArrayTest
//
procedure TSourceUtilsTests.DynamicArrayTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('var d : array of Integer;'#13#10'd.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckTrue(sugg.Count=13, 'd.');
   CheckEquals('Add', sugg.Code[0], 'd. 0');
   CheckEquals('Clear', sugg.Code[1], 'd. 1');
   CheckEquals('Copy', sugg.Code[2], 'd. 2');
   CheckEquals('Delete', sugg.Code[3], 'd. 3');
   CheckEquals('High', sugg.Code[4], 'd. 4');
   CheckEquals('IndexOf', sugg.Code[5], 'd. 5');
   CheckEquals('Insert', sugg.Code[6], 'd. 6');
   CheckEquals('Length', sugg.Code[7], 'd. 7');
   CheckEquals('Low', sugg.Code[8], 'd. 8');
   CheckEquals('Push', sugg.Code[9], 'd. 9');
   CheckEquals('Reverse', sugg.Code[10], 'd. 10');
   CheckEquals('SetLength', sugg.Code[11], 'd. 11');
   CheckEquals('Swap', sugg.Code[12], 'd. 12');
end;

// HelperSuggestTest
//
procedure TSourceUtilsTests.HelperSuggestTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TIntegerHelper = helper for Integer const Hello = 123; function Next : Integer; begin Result:=Self+1; end; end;'#13#10
                           +'var d : Integer;'#13#10
                           +'d.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(2, sugg.Count, 'd.');
   CheckEquals('Hello', sugg.Code[0], 'd. 0');
   CheckEquals('Next', sugg.Code[1], 'd. 0');
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
