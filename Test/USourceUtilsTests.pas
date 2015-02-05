unit USourceUtilsTests;

{$I ..\Source\dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsErrors, dwsUtils, Variants, dwsSymbols, dwsSuggestions,
   dwsFunctions;

type

   TSourceUtilsTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;

         function NeedUnitHandler(const unitName : UnicodeString; var unitSource : UnicodeString) : IdwsUnit;

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
         procedure ObjectArrayTest;
         procedure HelperSuggestTest;
         procedure SuggestInUsesSection;
         procedure SuggestAfterCall;
         procedure SuggestAcrossLines;
         procedure SymDictFunctionForward;
         procedure SymDictInherited;
         procedure SymDictParamExplicit;
         procedure SymDictParamImplicit;
         procedure ReferencesVars;
         procedure InvalidExceptSuggest;
         procedure EnumerationNamesAndValues;
         procedure BigEnumerationNamesAndValues;
         procedure EnumerationSuggest;
         procedure StaticClassSuggest;
         procedure SuggestInBlockWithError;
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
   FCompiler.OnNeedUnit:=NeedUnitHandler;
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
   CheckEquals(6, sugg.Count, 'column 6');
   CheckEquals('printit', sugg.Code[0], 'sugg 6, 0');
   CheckEquals('Print', sugg.Code[1], 'sugg 6, 1');
   CheckEquals('PrintLn', sugg.Code[2], 'sugg 6, 2');
   CheckEquals('procedure', sugg.Code[3], 'sugg 6, 3');
   CheckEquals('property', sugg.Code[4], 'sugg 6, 4');
   CheckEquals('Pred', sugg.Code[5], 'sugg 6, 5');

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
   CheckEquals(4, sugg.Count, 'TObject.Create 10');
   CheckEquals('ClassName', sugg.Code[0], 'TObject.Create 10,0');
   CheckEquals('ClassParent', sugg.Code[1], 'TObject.Create 10,1');
   CheckEquals('ClassType', sugg.Code[2], 'TObject.Create 10,2');
   CheckEquals('Create', sugg.Code[3], 'TObject.Create 10,3');
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
   CheckEquals(4, sugg.Count, 'new TObject 6');
   CheckEquals('TMyClass', sugg.Code[0], 'new TObject 6,0');
   CheckEquals('TClass', sugg.Code[1], 'new TObject 6,1');
   CheckEquals('TCustomAttribute', sugg.Code[2], 'new TCustomAttribute 6,2');
   CheckEquals('TObject', sugg.Code[3], 'new TObject 6,3');
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
   CheckEquals('ClassParent', sugg.Code[5], 'Self 12,5');
   CheckEquals('ClassType', sugg.Code[6], 'Self 12,6');
   CheckEquals('Create', sugg.Code[7], 'Self 12,7');
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
   CheckEquals('PixmapToJPEGData', sugg.Code[1], 'sugg 11, 1');

   scriptPos.Col:=12;
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckEquals(0, sugg.Count, 'column 12');

   prog:=FCompiler.Compile('System.TObject');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 8);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>10, 'column 8');
   CheckEquals('Boolean', sugg.Code[0], 'sugg 8, 0');
   CheckEquals('CompilerVersion', sugg.Code[1], 'sugg 8, 1');
   CheckEquals('EAssertionFailed', sugg.Code[2], 'sugg 8, 2');

   scriptPos.Col:=9;
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckEquals(9, sugg.Count, 'column 9');
   CheckEquals('TClass', sugg.Code[0], 'sugg 9, 0');
   CheckEquals('TComplex', sugg.Code[1], 'sugg 9, 1');
   CheckEquals('TCustomAttribute', sugg.Code[2], 'sugg 9, 2');
   CheckEquals('TObject', sugg.Code[3], 'sugg 9, 3');
   CheckEquals('TRTTIRawAttribute', sugg.Code[4], 'sugg 9, 4');
   CheckEquals('TRTTIRawAttributes', sugg.Code[5], 'sugg 9, 5');
   CheckEquals('TRTTITypeInfo', sugg.Code[6], 'sugg 9, 6');
   CheckEquals('TSourceCodeLocation', sugg.Code[7], 'sugg 9, 7');
   CheckEquals('TVector', sugg.Code[8], 'sugg 9, 8');
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
   CheckTrue(sugg.Count=4, 'v.');
   CheckEquals('ClassName', sugg.Code[0], 'v. 0');
   CheckEquals('ClassParent', sugg.Code[1], 'v. 1');
   CheckEquals('ClassType', sugg.Code[2], 'v. 2');
   CheckEquals('Create', sugg.Code[3], 'v. 3');
end;

function TSourceUtilsTests.NeedUnitHandler(const unitName: UnicodeString;
  var unitSource: UnicodeString): IdwsUnit;
begin
  CheckEquals('SomeUnit', unitName, 'Only the unit ''SomeUnit'' is handled properly!');
  unitSource := 'unit SomeUnit;';
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

   CheckTrue(sugg.Count>4, 's.');
   CheckEquals('After', sugg.Code[0], 's. 0');
   CheckEquals('Before', sugg.Code[1], 's. 1');
   CheckEquals('CompareText', sugg.Code[2], 's. 2');
   CheckEquals('CompareTo', sugg.Code[3], 's. 3');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=2, 's.h');
   CheckEquals('HexToInteger', sugg.Code[0], 's.h 0');
   CheckEquals('High', sugg.Code[1], 's.h 1');
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

   CheckEquals(19, sugg.Count, 'd.');
   CheckEquals('Add', sugg.Code[0], 'd. 0');
   CheckEquals('Clear', sugg.Code[1], 'd. 1');
   CheckEquals('Copy', sugg.Code[2], 'd. 2');
   CheckEquals('Count', sugg.Code[3], 'd. 3');
   CheckEquals('Delete', sugg.Code[4], 'd. 4');
   CheckEquals('High', sugg.Code[5], 'd. 5');
   CheckEquals('IndexOf', sugg.Code[6], 'd. 6');
   CheckEquals('Insert', sugg.Code[7], 'd. 7');
   CheckEquals('Length', sugg.Code[8], 'd. 8');
   CheckEquals('Low', sugg.Code[9], 'd. 9');
   CheckEquals('Map', sugg.Code[10], 'd. 10');
   CheckEquals('Peek', sugg.Code[11], 'd. 11');
   CheckEquals('Pop', sugg.Code[12], 'd. 12');
   CheckEquals('Push', sugg.Code[13], 'd. 13');
   CheckEquals('Remove', sugg.Code[14], 'd. 14');
   CheckEquals('Reverse', sugg.Code[15], 'd. 15');
   CheckEquals('SetLength', sugg.Code[16], 'd. 16');
   CheckEquals('Sort', sugg.Code[17], 'd. 17');
   CheckEquals('Swap', sugg.Code[18], 'd. 18');
end;

// ObjectArrayTest
//
procedure TSourceUtilsTests.ObjectArrayTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TObj = class X : Integer; end; var a : array of TObj;'#13#10
                           +'a[0].');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 6);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(7, sugg.Count, 'a[0].');
   CheckEquals('ClassName', sugg.Code[0], 'a[0]. 0');
   CheckEquals('ClassParent', sugg.Code[1], 'a[0]. 1');
   CheckEquals('ClassType', sugg.Code[2], 'a[0]. 2');
   CheckEquals('Create', sugg.Code[3], 'a[0]. 3');
   CheckEquals('Destroy', sugg.Code[4], 'a[0]. 4');
   CheckEquals('Free', sugg.Code[5], 'a[0]. 5');
   CheckEquals('X', sugg.Code[6], 'a[0]. 6');
end;

// HelperSuggestTest
//
procedure TSourceUtilsTests.HelperSuggestTest;
const
   cSugg : array [0..13] of String = (
      'Clamp', 'Factorial', 'Hello', 'IsPrime', 'LeastFactor', 'Max',
      'Min', 'Next', 'Sign', 'Sqr', 'ToBin', 'ToHexString', 'ToString',
      'Unsigned32'
      );

var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
   i : Integer;
begin
   prog:=FCompiler.Compile( 'type TIntegerHelper = helper for Integer const Hello = 123; '
                              +'function Next : Integer; begin Result:=Self+1; end; end;'#13#10
                           +'var d : Integer;'#13#10
                           +'d.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(Length(cSugg), sugg.Count, 'd.');
   for i:=0 to High(cSugg) do
      CheckEquals(cSugg[i], sugg.Code[i], 'd. '+IntToStr(i));
end;

// SuggestAfterCall
//
procedure TSourceUtilsTests.SuggestAfterCall;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('function T(i : Integer) : String; forward;'#13#10
                           +'T(1).L');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 7);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(4, sugg.Count, '.L');
   CheckEquals('Left', sugg.Code[0], '.L 0');
   CheckEquals('Length', sugg.Code[1], '.L 1');
   CheckEquals('Low', sugg.Code[2], '.L 2');
   CheckEquals('LowerCase', sugg.Code[3], '.L 3');

   prog:=FCompiler.Compile('function T(i : Integer) : String; forward;'#13#10
                           +'T(Ord(IntToStr(1)[1]+"])([")).Le');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 33);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(2, sugg.Count, '.Le');
   CheckEquals('Left', sugg.Code[0], '.Le 0');
   CheckEquals('Length', sugg.Code[1], '.Le 1');
end;

procedure TSourceUtilsTests.SuggestInUsesSection;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('uses SomeUnit;'#13#10'So');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 6);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(4, sugg.Count, 'There should be four units in the suggestions');
   CheckEquals('Default', sugg.Code[0], 'Unit ''Default'' not found');
   CheckEquals('Internal', sugg.Code[1], 'Unit ''Internal'' not found');
   CheckEquals('SomeUnit', sugg.Code[2], 'Unit ''SomeUnit'' not found');
   CheckEquals('System', sugg.Code[3], 'Unit ''System'' not found');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(1, sugg.Count, 'Should be only one suggestion');
   CheckEquals('SomeUnit', sugg.Code[0], 'The suggestion should be the unit ''SomeUnit''');

   // now check the same example without including units at all
   prog:=FCompiler.Compile('uses SomeUnit;'#13#10'So');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 6);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords, soNoUnits]);

   CheckEquals(0, sugg.Count, 'There shouldn''t be units in the suggestions at all');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords, soNoUnits]);

   CheckEquals(0, sugg.Count, 'There shouldn''t be units in the suggestions at all');
end;

// SuggestAcrossLines
//
procedure TSourceUtilsTests.SuggestAcrossLines;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('function T(i : Integer) : String; forward;'#13#10
                           +'T('#13#10
                           +'1'#13#10
                           +')'#13#10
                           +'.LO');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 5, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(2, sugg.Count, '.Lo');
   CheckEquals('Low', sugg.Code[0], '.Lo 0');
   CheckEquals('LowerCase', sugg.Code[1], '.L 1');
end;

// SymDictFunctionForward
//
procedure TSourceUtilsTests.SymDictFunctionForward;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile( 'procedure Test; begin end;');

   Check(prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suForward)=nil, 'Forward');
   CheckEquals(1, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suDeclaration).ScriptPos.Line,
               'a Declaration');
   CheckEquals(1, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suImplementation).ScriptPos.Line,
               'a Implementation');

   prog:=FCompiler.Compile( 'procedure Test; forward;'#13#10
                           +'procedure Test; begin end;');

   CheckEquals(1, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suForward).ScriptPos.Line,
               'b Forward');
   CheckEquals(1, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suDeclaration).ScriptPos.Line,
               'b Declaration');
   CheckEquals(2, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suImplementation).ScriptPos.Line,
               'b Implementation');

   prog:=FCompiler.Compile( 'unit Test; interface'#13#10
                           +'procedure Test;'#13#10
                           +'implementation'#13#10
                           +'procedure Test; begin end;');

   CheckEquals(2, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suForward).ScriptPos.Line,
               'c Forward');
   CheckEquals(2, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suDeclaration).ScriptPos.Line,
               'c Declaration');
   CheckEquals(4, prog.SymbolDictionary.FindSymbolUsageOfType('Test', TFuncSymbol, suImplementation).ScriptPos.Line,
               'c Implementation');
end;

// SymDictInherited
//
procedure TSourceUtilsTests.SymDictInherited;
var
   prog : IdwsProgram;
   symPosList : TSymbolPositionList;
   sym : TSymbol;
begin
   prog:=FCompiler.Compile( 'type TBaseClass = class procedure Foo; virtual; end;'#13#10
                           +'type TDerivedClass = class(TBaseClass) procedure Foo; override; end;'#13#10
                           +'procedure TDerivedClass.Foo; begin'#13#10
                           +'inherited;'#13#10
                           +'inherited Foo;'#13#10
                           +'end;');

   // base method

   sym:=prog.Table.FindSymbol('TBaseClass', cvMagic);
   sym:=(sym as TClassSymbol).Members.FindSymbol('Foo', cvMagic);

   symPosList:=prog.SymbolDictionary.FindSymbolPosList(sym);

   CheckEquals(4, symPosList.Count);

   CheckEquals(1, symPosList[0].ScriptPos.Line, 'TBaseClass Line 1');
   Check(symPosList[0].SymbolUsages=[suDeclaration], 'TBaseClass Line 1 usage');

   CheckEquals(2, symPosList[1].ScriptPos.Line, 'TBaseClass Line 2');
   Check(symPosList[1].SymbolUsages=[suReference, suImplicit], 'TBaseClass Line 2 usage');

   CheckEquals(4, symPosList[2].ScriptPos.Line, 'TBaseClass Line 4');
   Check(symPosList[2].SymbolUsages=[suReference, suImplicit], 'TBaseClass Line 4 usage');

   CheckEquals(5, symPosList[3].ScriptPos.Line, 'TBaseClass Line 5');
   Check(symPosList[3].SymbolUsages=[suReference], 'TBaseClass Line 5 usage');

   // derived method

   sym:=prog.Table.FindSymbol('TDerivedClass', cvMagic);
   sym:=(sym as TClassSymbol).Members.FindSymbol('Foo', cvMagic);

   symPosList:=prog.SymbolDictionary.FindSymbolPosList(sym);

   CheckEquals(2, symPosList.Count);

   CheckEquals(2, symPosList[0].ScriptPos.Line, 'TDerivedClass Line 2');
   Check(symPosList[0].SymbolUsages=[suDeclaration], 'TDerivedClass Line 2 usage');

   CheckEquals(3, symPosList[1].ScriptPos.Line, 'TDerivedClass Line 3');
   Check(symPosList[1].SymbolUsages=[suImplementation], 'TDerivedClass Line 3 usage');
end;

// SymDictParamExplicit
//
procedure TSourceUtilsTests.SymDictParamExplicit;
var
   prog : IdwsProgram;
   sym : TTypeSymbol;
   spl : TSymbolPositionList;
begin
   prog:=FCompiler.Compile( 'type TTest = class end;'#13#10
                           +'procedure Test(a : TTest); begin end;'#13#10
                           +'Test(nil);'#13#10);
   CheckEquals('', prog.Msgs.AsInfo);

   sym := prog.Table.FindTypeSymbol('TTest', cvMagic);

   spl := prog.SymbolDictionary.FindSymbolPosList(sym);

   CheckEquals(2, spl.Count, 'TTest');
   CheckEquals(' [line: 1, column: 6]', spl.Items[0].ScriptPos.AsInfo);
   CheckEquals(' [line: 2, column: 20]', spl.Items[1].ScriptPos.AsInfo);

   spl := prog.SymbolDictionary.FindSymbolPosList('a');

   CheckEquals(1, spl.Count, 'a');
   CheckEquals(' [line: 2, column: 16]', spl.Items[0].ScriptPos.AsInfo);

   spl := prog.SymbolDictionary.FindSymbolPosList('Test');

   CheckEquals(2, spl.Count, 'Test');
   CheckEquals(' [line: 2, column: 11]', spl.Items[0].ScriptPos.AsInfo);
   CheckEquals(' [line: 3, column: 1]', spl.Items[1].ScriptPos.AsInfo);
end;

// SymDictParamImplicit
//
procedure TSourceUtilsTests.SymDictParamImplicit;
var
   prog : IdwsProgram;
   sym : TTypeSymbol;
   spl : TSymbolPositionList;
begin
   prog:=FCompiler.Compile( 'type TTest = class end;'#13#10
                           +'procedure Test(a : TTest = nil); begin end;'#13#10
                           +'Test();'#13#10);
   CheckEquals('', prog.Msgs.AsInfo);

   sym := prog.Table.FindTypeSymbol('TTest', cvMagic);

   spl := prog.SymbolDictionary.FindSymbolPosList(sym);

   CheckEquals(2, spl.Count, 'TTest');
   CheckEquals(' [line: 1, column: 6]', spl.Items[0].ScriptPos.AsInfo);
   CheckEquals(' [line: 2, column: 20]', spl.Items[1].ScriptPos.AsInfo);

   spl := prog.SymbolDictionary.FindSymbolPosList('a');

   CheckEquals(1, spl.Count, 'a');
   CheckEquals(' [line: 2, column: 16]', spl.Items[0].ScriptPos.AsInfo);

   spl := prog.SymbolDictionary.FindSymbolPosList('Test');

   CheckEquals(2, spl.Count, 'Test');
   CheckEquals(' [line: 2, column: 11]', spl.Items[0].ScriptPos.AsInfo);
   CheckEquals(' [line: 3, column: 1]', spl.Items[1].ScriptPos.AsInfo);
end;

// ReferencesVars
//
procedure TSourceUtilsTests.ReferencesVars;
var
   prog : IdwsProgram;
   sym : TDataSymbol;
   funcSym : TSymbol;
   funcExec : IExecutable;
begin
   prog:=FCompiler.Compile( 'var i : Integer;'#13#10
                           +'if i>0 then Inc(i);'#13#10
                           +'function Test : Integer;'#13#10
                           +'begin var i:=1; result:=i; end;'#13#10);
   CheckEquals('', prog.Msgs.AsInfo);

   sym:=TDataSymbol(prog.Table.FindSymbol('i', cvMagic, TDataSymbol));
   CheckEquals('TDataSymbol', sym.ClassName, 'i class');

   CheckTrue(prog.ProgramObject.Expr.ReferencesVariable(sym), 'referenced in program');

   funcSym:=prog.Table.FindSymbol('Test', cvMagic);
   CheckEquals('TSourceFuncSymbol', funcSym.ClassName, 'Test class');

   funcExec:=(funcSym as TFuncSymbol).Executable;
   CheckFalse((funcExec.GetSelf as TdwsProgram).Expr.ReferencesVariable(sym), 'not referenced in test');
end;

// InvalidExceptSuggest
//
procedure TSourceUtilsTests.InvalidExceptSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'try'#13#10
                           +'except'#13#10
                           +'on e : Exception do'#13#10
                           +'e.s'#13#10);

   CheckTrue(prog.Msgs.HasErrors, 'compiled with errors');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 4, 4);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 4');
   CheckEquals('StackTrace', sugg.Code[0], 'sugg 2, 0');
end;

// EnumerationNamesAndValues
//
procedure TSourceUtilsTests.EnumerationNamesAndValues;
var
   prog : IdwsProgram;
   enum : TEnumerationSymbol;
begin
   prog:=FCompiler.Compile( 'type TContinuous = (c1, c2, c3);'#13#10
                           +'type TContinuous2 = (d1 = 1, d2 = 2, d3 = 3);'#13#10
                           +'type TDiscontinuous = (e1 = 1, e2 = 3, e3 = 4);');

   CheckEquals('', prog.Msgs.AsInfo, 'compiled with errors');

   enum:=(prog.Table.FindTypeSymbol('TContinuous', cvPublic) as TEnumerationSymbol);
   CheckEquals('c1', enum.ElementByValue(0).Name);
   CheckEquals('c2', enum.ElementByValue(1).Name);
   CheckEquals('c3', enum.ElementByValue(2).Name);
   CheckNull(enum.ElementByValue(3), 'continuous');

   enum:=(prog.Table.FindTypeSymbol('TContinuous2', cvPublic) as TEnumerationSymbol);
   CheckNull(enum.ElementByValue(0), 'continuous2');
   CheckEquals('d1', enum.ElementByValue(1).Name);
   CheckEquals('d2', enum.ElementByValue(2).Name);
   CheckEquals('d3', enum.ElementByValue(3).Name);

   enum:=(prog.Table.FindTypeSymbol('TDiscontinuous', cvPublic) as TEnumerationSymbol);
   CheckEquals('e1', enum.ElementByValue(1).Name);
   CheckNull(enum.ElementByValue(2), 'discontinuous');
   CheckEquals('e2', enum.ElementByValue(3).Name);
   CheckEquals('e3', enum.ElementByValue(4).Name);
end;

// BigEnumerationNamesAndValues
//
procedure TSourceUtilsTests.BigEnumerationNamesAndValues;
var
   i : Integer;
   s : String;
   prog : IdwsProgram;
   enum : TEnumerationSymbol;
begin
   s:='type TTest = (';
   for i:=1 to 100 do begin
      if i>1 then
         s:=s+',';
      s:=s+'v'+IntToStr(i);
   end;
   prog:=FCompiler.Compile(s+');');

   CheckEquals('', prog.Msgs.AsInfo, 'compiled with errors');

   enum:=(prog.Table.FindTypeSymbol('TTest', cvPublic) as TEnumerationSymbol);

   for i:=1 to 100 do begin
      CheckEquals(i-1, (enum.Elements.FindLocal('v'+IntToStr(i)) as TElementSymbol).Value, 'value of '+IntToStr(i-1));
      CheckEquals('v'+IntToStr(i), enum.ElementByValue(i-1).Name, 'name of '+IntToStr(i-1));
   end;
end;

// EnumerationSuggest
//
procedure TSourceUtilsTests.EnumerationSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TTest = (One);'#13#10
                           +'Print(TTest.One.Name);'#13#10
                           +'var i : TTest;'#13#10
                           +'Print(i.Value);'#13#10);

   CheckEquals('', prog.Msgs.AsInfo, 'compiled with errors');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 13);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 13');
   CheckEquals('One', sugg.Code[0], 'sugg 2, 13, 0');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 18);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 18');
   CheckEquals('Name', sugg.Code[0], 'sugg 2, 18, 0');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 4, 10);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 10');
   CheckEquals('Value', sugg.Code[0], 'sugg 4, 10, 0');
end;

// StaticClassSuggest
//
procedure TSourceUtilsTests.StaticClassSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TTest = class static public'#13#10
                           +'class function GetTest(i : Integer) : String; begin Result:=""; end;'#13#10
                           +'property Test[i : Integer] : String read GetTest;'#13#10
                           +'end;'#13#10
                           +'Print(TTest.GetTest(1));'#13#10
                           +'Print(TTest.Test[2]);'#13#10);

   CheckEquals('', prog.Msgs.AsInfo, 'compiled with errors');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 5, 13);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(5, sugg.Count, 'column 13');
   CheckEquals('ClassName', sugg.Code[0], 'sugg 5, 13, 0');
   CheckEquals('ClassParent', sugg.Code[1], 'sugg 5, 13, 1');
   CheckEquals('ClassType', sugg.Code[2], 'sugg 5, 13, 2');
   CheckEquals('GetTest', sugg.Code[3], 'sugg 5, 13, 3');
   CheckEquals('Test', sugg.Code[4], 'sugg 5, 13, 4');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 5, 14);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 5,14');
   CheckEquals('GetTest', sugg.Code[0], 'sugg 5, 14, 0');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 6, 14);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 6,10');
   CheckEquals('Test', sugg.Code[0], 'sugg 6, 14, 0');
end;

// SuggestInBlockWithError
//
procedure TSourceUtilsTests.SuggestInBlockWithError;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'begin'#13#10
                           +'var xyz := "";'#13#10
                           +'x');

   CheckNotEquals('', prog.Msgs.AsInfo, 'should have compiled with errors');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 2);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count, 'line 3 col 2');
   CheckEquals('xyz', sugg.Code[0], '3,2,0');
   CheckEquals('xor', sugg.Code[1], '3,2,1');

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('SourceUtilsTests', TSourceUtilsTests);

end.
