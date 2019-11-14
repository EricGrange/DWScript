unit USourceUtilsTests;

{$I ..\Source\dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext,
   dwsTokenizer, dwsErrors, dwsUtils, Variants, dwsSymbols, dwsSuggestions,
   dwsFunctions, dwsCaseNormalizer, dwsScriptSource, dwsSymbolDictionary,
   dwsCompilerContext, dwsUnicode, dwsJSONConnector, dwsUnitSymbols;

type

   TSourceUtilsTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FTestUnit : String;

         function NeedUnitHandler(const unitName : String; var unitSource : String) : IdwsUnit;

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
         procedure StaticArrayRecordTest;
         procedure DynamicArrayTest;
         procedure ObjectArrayTest;
         procedure AssociativeArrayTest;
         procedure HelperSuggestTest;
         procedure FunctionReturningArrayTest;
         procedure JSONVariantSuggestTest;
         procedure SuggestInUsesSection;
         procedure SuggestAfterCall;
         procedure SuggestAcrossLines;
         procedure ForVariable;
         procedure ReferencesVars;
         procedure InvalidExceptSuggest;
         procedure EnumerationNamesAndValues;
         procedure BigEnumerationNamesAndValues;
         procedure EnumerationSuggest;
         procedure SetOfSuggest;
         procedure StaticClassSuggest;
         procedure ClassFieldSuggest;
         procedure RecordConstSuggest;
         procedure SuggestInBlockWithError;
         procedure NormalizeOverload;
         procedure NormalizeImplicit;
         procedure NormalizeTypes;
         procedure NormalizeMagics;
         procedure NormalizeForVarIn;
         procedure NormalizeKeywords;
         procedure OptimizedIfThenBlockSymbol;
         procedure MemberVisibilities;
         procedure UnitNamesSuggest;
         procedure OverloadSuggest;
         procedure LengthDotSuggest;
         procedure PropertyDescription;
         procedure ImplementationSuggest;
         procedure ParameterSuggest;
         procedure FunctionCaptionDescription;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsEncodingLibModule;

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
   CheckTrue(sugg.Count>3, 'column 2');
   CheckEquals('printit', sugg.Code[0], 'sugg 2, 0');
   CheckEquals('PadLeft', sugg.Code[1], 'sugg 2, 1');
   CheckEquals('PadRight', sugg.Code[2], 'sugg 2, 2');
   CheckEquals('Param', sugg.Code[3], 'sugg 2, 3');

   scriptPos.Col:=3;
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(7, sugg.Count, 'column 7');
   CheckEquals('printit', sugg.Code[0], 'sugg 7, 0');
   CheckEquals('Print', sugg.Code[1], 'sugg 7, 1');
   CheckEquals('PrintLn', sugg.Code[2], 'sugg 7, 2');
   CheckEquals('PrivateVarsNames', sugg.Code[3], 'sugg 7, 3');
   CheckEquals('procedure', sugg.Code[4], 'sugg 7, 4');
   CheckEquals('property', sugg.Code[5], 'sugg 7, 5');
   CheckEquals('Pred', sugg.Code[6], 'sugg 7, 6');

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
   CheckTrue(sugg.Count>5, 'column 11');
   CheckEquals('PadLeft', sugg.Code[0], 'sugg 11, 0');
   CheckEquals('PadRight', sugg.Code[1], 'sugg 11, 1');
   CheckEquals('ParseDateTime', sugg.Code[2], 'sugg 11, 2');
   CheckEquals('Pi', sugg.Code[3], 'sugg 11, 3');
   CheckEquals('PixmapToJPEGData', sugg.Code[4], 'sugg 11, 4');

   scriptPos.Col:=12;
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckEquals(1, sugg.Count, 'column 12');
   CheckEquals('PrivateVarsNames', sugg.Code[0], 'sugg 12, 0');

   prog:=FCompiler.Compile('System.TObject');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 8);
   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckTrue(sugg.Count>10, 'column 8');
   CheckEquals('BigInteger', sugg.Code[0], 'sugg 8, 0');
   CheckEquals('Boolean', sugg.Code[1], 'sugg 8, 1');
   CheckEquals('ByteBuffer', sugg.Code[2], 'sugg 8, 1');
   CheckEquals('CompilerVersion', sugg.Code[3], 'sugg 8, 2');
   CheckEquals('DateTimeZone', sugg.Code[4], 'sugg 8, 3');

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

function TSourceUtilsTests.NeedUnitHandler(const unitName: String;
  var unitSource: String): IdwsUnit;
begin
  CheckTrue((unitName = 'SomeUnit') or (unitName = 'Test'), 'Only the units ''SomeUnit'' & ''Test'' are handled properly!');
  if unitName = 'SomeUnit' then
     unitSource := 'unit SomeUnit;'
  else unitSource := FTestUnit;
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
   CheckEquals('AfterLast', sugg.Code[1], 's. 1');
   CheckEquals('Before', sugg.Code[2], 's. 2');
   CheckEquals('BeforeLast', sugg.Code[3], 's. 3');
   CheckEquals('Between', sugg.Code[4], 's. 4');
   CheckEquals('CompareText', sugg.Code[5], 's. 5');
   CheckEquals('CompareTo', sugg.Code[6], 's. 6');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=3, 's.h');
   CheckEquals('HexToBigInteger', sugg.Code[0], 's.h 0');
   CheckEquals('HexToInteger', sugg.Code[1], 's.h 1');
   CheckEquals('High', sugg.Code[2], 's.h 2');
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

   CheckTrue(sugg.Count=4, 's.');
   CheckEquals('Count', sugg.Code[0], 's. 0');
   CheckEquals('High', sugg.Code[1], 's. 1');
   CheckEquals('Length', sugg.Code[2], 's. 2');
   CheckEquals('Low', sugg.Code[3], 's. 3');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckTrue(sugg.Count=1, 's.h');
   CheckEquals('High', sugg.Code[0], 's.h 0');
end;

// StaticArrayRecordTest
//
procedure TSourceUtilsTests.StaticArrayRecordTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'const s : array [1..2] of record xy : Integer end = [('#13#10
                           +' )];');

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 1);
   sugg := TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckTrue(sugg.Count > 0, 'rec field');
   CheckEquals('xy', sugg.Code[0], 'sugg 0');
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

   var expected := [
      'Add', 'Clear', 'Copy', 'Count', 'Delete', 'Filter', 'High',
      'IndexOf', 'Insert', 'Length', 'Low', 'Map', 'Move',
      'Peek', 'Pop', 'Push', 'Remove', 'Reverse',
      'SetLength', 'Sort', 'Swap'
   ];
   CheckEquals(Length(expected), sugg.Count, 'd. Length');
   for var i := 0 to High(expected) do
      CheckEquals(expected[i], sugg.Code[i], 'd. ' + IntToStr(i));
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

// AssociativeArrayTest
//
procedure TSourceUtilsTests.AssociativeArrayTest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('var d : array [Integer] of Integer;'#13#10'd.');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(5, sugg.Count, 'd.');
   CheckEquals('Clear', sugg.Code[0], 'd. 0');
   CheckEquals('Count', sugg.Code[1], 'd. 1');
   CheckEquals('Delete', sugg.Code[2], 'd. 2');
   CheckEquals('Keys', sugg.Code[3], 'd. 3');
   CheckEquals('Length', sugg.Code[4], 'd. 4');
end;

// HelperSuggestTest
//
procedure TSourceUtilsTests.HelperSuggestTest;
const
   cSugg : array [0..19] of String = (
      'Abs', 'Clamp', 'Compare', 'Compare', 'Factorial', 'Hello', 'IsOdd', 'IsPrime',
      'LeastFactor', 'Max', 'Min', 'Next', 'PopCount', 'Sign', 'Sqr', 'TestBit',
      'ToBin', 'ToHexString', 'ToString', 'Unsigned32'
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
                           +'d.Nex');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 3);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(Length(cSugg), sugg.Count, 'd.');
   for i:=0 to High(cSugg) do
      CheckEquals(cSugg[i], sugg.Code[i], 'd. '+IntToStr(i));

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 6);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);
   CheckEquals(1, sugg.Count, 'd.Nex');
   CheckEquals('Next', sugg.Code[0]);
   CheckEquals(Ord(scFunction), Ord(sugg.Category[0]), 'scFunction');
   CheckEquals('Next () : Integer', sugg.Caption[0]);
end;

// FunctionReturningArrayTest
//
procedure TSourceUtilsTests.FunctionReturningArrayTest;

   procedure CheckIsJoin(const code : String);
   var
      prog : IdwsProgram;
      sugg : IdwsSuggestions;
      scriptPos : TScriptPos;
   begin
      prog := FCompiler.Compile(code);

      scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 2, Length(code)-Pos(#10, code)+1);
      sugg := TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

      CheckEquals(1, sugg.Count, code);
      CheckEquals('Join', sugg.Code[0], code);
   end;

begin
//   CheckIsJoin('function Test : array of String; begin end;'#13#10'Test.Jo');
//   CheckIsJoin('function Test : array of String; begin end;'#13#10'Test().Jo');
//   CheckIsJoin('function Test : array of String; begin end;'#13#10'StrSplit("Test").Jo');
   CheckIsJoin('function Test : array of String; begin end;'#13#10'Test.Map(Trim).Jo');
end;

// JSONVariantSuggestTest
//
procedure TSourceUtilsTests.JSONVariantSuggestTest;
const
   cSugg : array [0..12] of String = (
      'Add', 'Clone', 'Defined', 'Delete', 'ElementName', 'Extend', 'High', 'Length',
      'Low', 'Push', 'Swap', 'ToString', 'TypeName'
      );

var
   module : TdwsJSONLibModule;
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
   i : Integer;
begin
   module := TdwsJSONLibModule.Create(nil);
   try
      module.Script := FCompiler;

      prog:=FCompiler.Compile( 'var v : JSONVariant;'#13#10
                              +'v.');

      scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 3);
      sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

      CheckEquals(Length(cSugg), sugg.Count, 'd.');
      for i:=0 to High(cSugg) do
         CheckEquals(cSugg[i], sugg.Code[i], 'd. '+IntToStr(i));
   finally
      module.Free;
   end;
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

// ForVariable
//
procedure TSourceUtilsTests.ForVariable;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile('var a : array of String;'#10
                           +'for var test1 := 0 to 10 do'#10
                           +'for var test2 in a do begin'#10
                           +'teS'#10
                           +'end;');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 4, 4);
   sugg:=TdwsSuggestions.Create(prog, scriptPos, [soNoReservedWords]);

   CheckEquals(3, sugg.Count, 'tes');
   CheckEquals('test1', sugg.Code[0], 'tes 0');
   CheckEquals('test2', sugg.Code[1], 'tes 1');
   CheckEquals('TestBit', sugg.Code[2], 'tes 2');
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
   CheckEquals(TVarDataSymbol.ClassName, sym.ClassName, 'i class');

   CheckTrue(prog.ProgramObject.Expr.ReferencesVariable(sym), 'referenced in program');

   funcSym:=prog.Table.FindSymbol('Test', cvMagic);
   CheckEquals(TSourceFuncSymbol.ClassName, funcSym.ClassName, 'Test class');

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
      s:=s+'v'+FastInt64ToStr(i);
   end;
   prog:=FCompiler.Compile(s+');');

   CheckEquals('', prog.Msgs.AsInfo, 'compiled with errors');

   enum:=(prog.Table.FindTypeSymbol('TTest', cvPublic) as TEnumerationSymbol);

   for i:=1 to 100 do begin
      CheckEquals(i-1, (enum.Elements.FindLocal('v'+FastInt64ToStr(i)) as TElementSymbol).Value, 'value of '+IntToStr(i-1));
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
                           +'Print(i.Value);'#13#10
                           +'Print(i.QualifiedName);'#13#10);

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
   CheckEquals(1, sugg.Count, 'column 10 v');
   CheckEquals('Value', sugg.Code[0], 'sugg 4, 10, 0');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 5, 10);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count, 'column 10 q');
   CheckEquals('QualifiedName', sugg.Code[0], 'sugg 5, 10, 0');
end;

// SetOfSuggest
//
procedure TSourceUtilsTests.SetOfSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TTest = (One);'#13#10
                           +'type TSet = set of TTest;'#13#10
                           +'var i : TSet;'#13#10
                           +'i.');

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 4, 3);

   sugg := TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count, 'set of methods');
   CheckEquals('Exclude', sugg.Code[0], 'sugg 0');
   CheckEquals('Include', sugg.Code[1], 'sugg 1');
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

// ClassFieldSuggest
//
procedure TSourceUtilsTests.ClassFieldSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TTest = class private fb : Boolean; protected fi : Integer; fs : String; public'#13#10
                           +'class const fc = 123;'#13#10
                           +'property F : Integer read '#13#10
                           +'f');

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 4, 2);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   Check(sugg.Count > 4);
   CheckEquals('fb', sugg.Code[0]);
   CheckEquals('fc', sugg.Code[1]);
   CheckEquals('fi', sugg.Code[2]);
   CheckEquals('fs', sugg.Code[3]);
   CheckEquals('Factorial', sugg.Code[4]);
end;

// RecordConstSuggest
//
procedure TSourceUtilsTests.RecordConstSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'type TTest = record xyz, abc : Integer; function Foo : String; end;'#13#10
                           +'const c : array of TTest = [('#13#10
                           +'x');

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 2);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count);
   CheckEquals('xyz', sugg.Code[0]);
   CheckEquals('xor', sugg.Code[1]);

   prog:=FCompiler.Compile( 'type TTest = record xyz, abc : Integer; function Foo : String; end;'#13#10
                           +'const c : array of TTest = [('#13#10);

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 1);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   Check(sugg.Count > 2);
   CheckEquals('abc', sugg.Code[0]);
   CheckEquals('xyz', sugg.Code[1]);
end;

// UnitNamesSuggest
//
procedure TSourceUtilsTests.UnitNamesSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
   encodingLib : TdwsEncodingLib;
begin
   encodingLib := TdwsEncodingLib.Create(nil);
   try
      encodingLib.dwsEncoding.Script := FCompiler;

      prog := FCompiler.Compile('uses System.En');
      try
         scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 10);

         sugg:=TdwsSuggestions.Create(prog, scriptPos);
         CheckEquals(2, sugg.Count, 'Syst');
         CheckEquals('System', sugg.Code[0]);
         CheckEquals('System.Encoding', sugg.Code[1]);

         scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 13);

         sugg:=TdwsSuggestions.Create(prog, scriptPos);
         CheckEquals(4, sugg.Count, 'System.');
         CheckEquals('Default', sugg.Code[0]);
         CheckEquals('Internal', sugg.Code[1]);
         CheckEquals('System', sugg.Code[2]);
         CheckEquals('System.Encoding', sugg.Code[3]);

         scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 1, 15);

         sugg:=TdwsSuggestions.Create(prog, scriptPos);
         CheckEquals(3, sugg.Count, 'System.En');
         CheckEquals('EncodeDate', sugg.Code[0]);
         CheckEquals('Encoder', sugg.Code[1]);
         CheckEquals('EncodeTime', sugg.Code[2]);
      finally
         sugg := nil;
         prog := nil;
      end;
   finally
      encodingLib.Free;
   end;
end;

// OverloadSuggest
//
procedure TSourceUtilsTests.OverloadSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog:=FCompiler.Compile( 'function Toto(s: string): string; overload; begin Result := s; end;'#13#10
                           +'function Toto(i: integer): string; overload; begin Result := i.ToString; end;'#13#10
                           +'toto');

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 4);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count);
   CheckEquals('Toto (i: Integer) : String', sugg.Caption[0]);
   CheckEquals('Toto (s: String) : String', sugg.Caption[1]);
end;

// LengthDotSuggest
//
procedure TSourceUtilsTests.LengthDotSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog := FCompiler.Compile( 'var s : String;'#13#10
                            + 'PrintLn(s.Length.tos');

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 2, 21);

   sugg := TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(1, sugg.Count);
   CheckEquals('ToString () : String', sugg.Caption[0]);
end;

// PropertyDescription
//
procedure TSourceUtilsTests.PropertyDescription;
var
   prog : IdwsProgram;
   cls, prop : TSymbol;
begin
   prog := FCompiler.Compile(
         'type TTest = class '
         + 'function Func : String; begin Result := '''' ; end; '
         + 'property Hello : String read Func description "world"; '
       + 'end;'
   );
   CheckEquals(0, prog.Msgs.Count, prog.Msgs.AsInfo);

   cls := prog.Table.FindSymbol('TTest', cvMagic, TClassSymbol);
   Check(cls <> nil, 'TTest missing');

   prop := (cls as TClassSymbol).Members.FindLocal('Hello', TPropertySymbol);
   Check(prop <> nil, 'TTest.Hello missing');

   CheckEquals('world', (prop as TPropertySymbol).UserDescription);
end;

// ImplementationSuggest
//
procedure TSourceUtilsTests.ImplementationSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   FTestUnit :=   'unit test; interface implementation'#13#10
                + 'var zxy : String;'#13#10
                + 'procedure zzz; begin '#13#10
                + 'zzz; end;';

   prog := FCompiler.Compile(FTestUnit);

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 4, 2);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count);
   CheckEquals('zxy : String', sugg.Caption[0]);
   CheckEquals('zzz ()', sugg.Caption[1]);

   prog := FCompiler.Compile(  'uses Test;'#13#10
                             + 'var za := 0;'#13#10
                             + 'za := 1;');

   CheckEquals('*MainModule*', prog.SourceList[0].SourceFile.Name);
   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 2);
   sugg := TdwsSuggestions.Create(prog, scriptPos);

   CheckEquals(1, sugg.Count);
   CheckEquals('za : Integer', sugg.Caption[0]);

   CheckEquals('Test', prog.SourceList[1].SourceFile.Name);
   scriptPos := TScriptPos.Create(prog.SourceList[1].SourceFile, 4, 2);
   sugg := TdwsSuggestions.Create(prog, scriptPos);

   CheckEquals(2, sugg.Count);
   CheckEquals('zxy : String', sugg.Caption[0]);
   CheckEquals('zzz ()', sugg.Caption[1]);
end;

// ParameterSuggest
//
procedure TSourceUtilsTests.ParameterSuggest;
var
   prog : IdwsProgram;
   sugg : IdwsSuggestions;
   scriptPos : TScriptPos;
begin
   prog := FCompiler.Compile(
          'var myvariable: string;'#10
         +#10
         +'procedure foo(const att: Boolean);'#10
         +'var'#10
         +'  lcl: Boolean;'#10
         +'begin'#10
         +'  {here}'#10
         +'end;'#10
   );
   CheckFalse(prog.Msgs.HasErrors, prog.Msgs.AsInfo);

   scriptPos := TScriptPos.Create(prog.SourceList[0].SourceFile, 7, 3);
   sugg := TdwsSuggestions.Create(prog, scriptPos);

   CheckTrue(sugg.Count > 2);
   CheckEquals('att : Boolean', sugg.Caption[0]);
   CheckEquals('lcl : Boolean', sugg.Caption[1]);
end;

// FunctionCaptionDescription
//
procedure TSourceUtilsTests.FunctionCaptionDescription;
var
   prog : IdwsProgram;
begin
   prog := FCompiler.Compile(
          'function  Test1(a, b : Integer; const c : String; var d : Boolean; e : type Boolean) : String; begin end;'#10
         +'function  Test2(const a, b : String; c : String; var d : Boolean; var e : Boolean) : String; begin end;'#10
         +'procedure Test3(a, b : Integer; c : type Integer; d : Boolean; var e : Boolean); begin end;'#10
         +'procedure Test4(var a, b : Integer; const c : Integer; d : Boolean; e : Boolean); begin end;'#10
   );
   CheckFalse(prog.Msgs.HasErrors, prog.Msgs.AsInfo);

   CheckEquals('function Test1(a: Integer; b: Integer; const c: String; var d: Boolean; e: type Boolean): String',
               prog.Table.FindSymbol('Test1', cvMagic).Description, 'Test1');
   CheckEquals('function Test2(const a: String; const b: String; c: String; var d: Boolean; var e: Boolean): String',
               prog.Table.FindSymbol('Test2', cvMagic).Description, 'Test2');
   CheckEquals('procedure Test3(a: Integer; b: Integer; c: type Integer; d: Boolean; var e: Boolean)',
               prog.Table.FindSymbol('Test3', cvMagic).Description, 'Test3');
   CheckEquals('procedure Test4(var a: Integer; var b: Integer; const c: Integer; d: Boolean; e: Boolean)',
               prog.Table.FindSymbol('Test4', cvMagic).Description, 'Test4');
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

   CheckNotEquals(String(''), prog.Msgs.AsInfo, 'should have compiled with errors');

   scriptPos:=TScriptPos.Create(prog.SourceList[0].SourceFile, 3, 2);

   sugg:=TdwsSuggestions.Create(prog, scriptPos);
   CheckEquals(2, sugg.Count, 'line 3 col 2');
   CheckEquals('xyz', sugg.Code[0], '3,2,0');
   CheckEquals('xor', sugg.Code[1], '3,2,1');

end;

// NormalizeOverload
//
type
   TTestNormalizer = class (TStringList)
      procedure Normalize(line, col : Integer; const name : String);
   end;
procedure TTestNormalizer.Normalize(line, col : Integer; const name : String);
begin
   Add(Format('%d, %d, %s', [line, col, name]));
end;
procedure TSourceUtilsTests.NormalizeOverload;
var
   prog : IdwsProgram;
   lines : TStringList;
   normalizer : TTestNormalizer;
begin
   lines := TStringList.Create;
   try
      lines.Text:= 'unit Unit1;'#13#10
                  +'interface'#13#10
                  +'procedure Test(const A, Blah: String; const C: string); overload;'#13#10
                  +'procedure Test; overload;'#13#10
                  +'implementation'#13#10
                  +'procedure test(const A, Blah: string; const C: String);'#13#10
                  +'begin end;'#13#10
                  +'procedure test;'#13#10
                  +'begin end;';

      prog:=FCompiler.Compile(lines.Text);

      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors');

      normalizer:=TTestNormalizer.Create;
      try
         NormalizeSymbolsCase(lines, prog.SourceList[0].SourceFile, prog.SymbolDictionary,
                              normalizer.Normalize);
         CheckEquals('3, 48, String'#13#10'6, 11, Test'#13#10'6, 31, String'#13#10'8, 11, Test'#13#10, normalizer.Text);
      finally
         normalizer.Free;
      end;
   finally
      lines.Free;
   end;
end;

// NormalizeImplicit
//
procedure TSourceUtilsTests.NormalizeImplicit;
var
   prog : IdwsProgram;
   lines : TStringList;
   normalizer : TTestNormalizer;
begin
   lines := TStringList.Create;
   try
      lines.Text:= 'function Test(a : Integer) : Integer;'#13#10
                  +'begin'#13#10
                  +'Exit(a);'#13#10
                  +'end;';

      prog:=FCompiler.Compile(lines.Text);

      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors');

      normalizer:=TTestNormalizer.Create;
      try
         NormalizeSymbolsCase(lines, prog.SourceList[0].SourceFile, prog.SymbolDictionary,
                              normalizer.Normalize);
         CheckEquals('', normalizer.Text);
      finally
         normalizer.Free;
      end;
   finally
      lines.Free;
   end;
end;

// NormalizeTypes
//
procedure TSourceUtilsTests.NormalizeTypes;
var
   prog : IdwsProgram;
   lines : TStringList;
   normalizer : TTestNormalizer;
begin
   lines := TStringList.Create;
   try
      lines.Text:= 'type TProc = procedure;'#13#10
                  +'type TFunc = function(proc : tproc): tProc;'#13#10
                  +'var f : TFunC;'#13#10;

      prog:=FCompiler.Compile(lines.Text);

      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors');

      normalizer:=TTestNormalizer.Create;
      try
         NormalizeSymbolsCase(lines, prog.SourceList[0].SourceFile, prog.SymbolDictionary,
                              normalizer.Normalize);
         CheckEquals(
              'type TProc = procedure;'#13#10
            + 'type TFunc = function(proc : TProc): TProc;'#13#10
            + 'var f : TFunc;'#13#10,
            lines.Text
         );
      finally
         normalizer.Free;
      end;
   finally
      lines.Free;
   end;
end;

// NormalizeMagics
//
procedure TSourceUtilsTests.NormalizeMagics;
var
   prog : IdwsProgram;
   lines : TStringList;
   normalizer : TTestNormalizer;
begin
   lines := TStringList.Create;
   try
      lines.Text := 'var s : String; var a : array of String;'#13#10
                  + 'PrintLn(s.length + s.high + s.low + a.length + a.high + a.low);'#13#10;

      prog := FCompiler.Compile(lines.Text);

      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors');

      normalizer := TTestNormalizer.Create;
      try
         NormalizeSymbolsCase(lines, prog.SourceList[0].SourceFile, prog.SymbolDictionary,
                              normalizer.Normalize);
         CheckEquals(
              'var s : String; var a : array of String;'#13#10
            + 'PrintLn(s.Length + s.High + s.Low + a.Length + a.High + a.Low);'#13#10,
            lines.Text
         );
      finally
         normalizer.Free;
      end;
   finally
      lines.Free;
   end;
end;

// NormalizeForVarIn
//
procedure TSourceUtilsTests.NormalizeForVarIn;
var
   prog : IdwsProgram;
   lines : TStringList;
   normalizer : TTestNormalizer;
begin
   lines := TStringList.Create;
   try
      lines.Text := 'var i : integer; type TEnum = (One, Two); var a := [1, 2];'#13#10
                  + 'for var e in tenum do PrintLn(integer(E));'#13#10
                  + 'for var j in A do PrintLn(A[J]);'#13#10
                  + 'for I in A do PrintLn(A[I].tostring);'#13#10;

      prog := FCompiler.Compile(lines.Text);

      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors');

      normalizer := TTestNormalizer.Create;
      try
         NormalizeSymbolsCase(lines, prog.SourceList[0].SourceFile, prog.SymbolDictionary,
                              normalizer.Normalize);
         CheckEquals(
              'var i : Integer; type TEnum = (One, Two); var a := [1, 2];'#13#10
            + 'for var e in TEnum do PrintLn(Integer(e));'#13#10
            + 'for var j in a do PrintLn(a[j]);'#13#10
            + 'for i in a do PrintLn(a[i].ToString);'#13#10,
            lines.Text
         );
      finally
         normalizer.Free;
      end;
   finally
      lines.Free;
   end;
end;

// NormalizeKeywords
//
procedure TSourceUtilsTests.NormalizeKeywords;
var
   prog : IdwsProgram;
   lines : TStringList;
   normalizer : TTestNormalizer;
begin
   lines := TStringList.Create;
   try
      lines.Text := 'Var b := TRUE; begIn IF nOt B tHEN b := false ELSE b := NIL <> nIL; enD;'#13#10
                  + 'WHILE b do BREAK;'#13#10
                  + 'FOR VAR i := 0 To 1 dO CASE I OF 1 : ; eLSE END;'#13#10;

      FCompiler.Config.CompilerOptions := FCompiler.Config.CompilerOptions + [ coHintKeywordCaseMismatch ];
      FCompiler.Config.HintsLevel := hlPedantic;
      try
         prog := FCompiler.Compile(lines.Text);
      finally
         FCompiler.Config.HintsLevel := hlStrict;
         FCompiler.Config.CompilerOptions := FCompiler.Config.CompilerOptions - [ coHintKeywordCaseMismatch ];
      end;

      Check(prog.Msgs.Count > 0, 'should have compiled with hints');

      normalizer := TTestNormalizer.Create;
      try
         NormalizeKeywordCase(lines, prog.SourceList[0].SourceFile, prog.Msgs,
                              normalizer.Normalize);
         CheckEquals(
              'var b := True; begin if not B then b := False else b := nil <> nil; end;'#13#10
            + 'while b do break;'#13#10
            + 'for var i := 0 to 1 do case I of 1 : ; else end;'#13#10,
            lines.Text
         );
      finally
         normalizer.Free;
      end;
   finally
      lines.Free;
   end;
end;

// OptimizedIfThenBlockSymbol
//
procedure TSourceUtilsTests.OptimizedIfThenBlockSymbol;

   procedure CheckSymbols(const prefix : String; dic : TdwsSymbolDictionary);
   begin
      CheckEquals(2, dic.Count, prefix+'nb');
      Check(dic.FindSymbolPosList('xyz') <> nil, prefix+'exists');
      CheckEquals(1, dic.FindSymbolPosList('xyz').Count, prefix+'usage');
      CheckEquals(1, dic.FindSymbolPosList('String').Count, prefix+'string');
   end;

var
   prog : IdwsProgram;
   options : TCompilerOptions;
begin
   options:=FCompiler.Config.CompilerOptions;
   try
      FCompiler.Config.CompilerOptions := options + [coOptimize];

      prog:=FCompiler.Compile( 'if False then begin'#13#10
                              +'var xyz := "";'#13#10
                              +'end;');
      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors 1');

      CheckSymbols('a.', prog.SymbolDictionary);

      FCompiler.Config.CompilerOptions := options;

      prog:=FCompiler.Compile( 'if False then begin'#13#10
                              +'var xyz := "";'#13#10
                              +'end;');
      CheckEquals('', prog.Msgs.AsInfo, 'should have compiled without errors 2');

      CheckSymbols('b.', prog.SymbolDictionary);
   finally
      FCompiler.Config.CompilerOptions := options;
   end;
end;

// MemberVisibilities
//
procedure TSourceUtilsTests.MemberVisibilities;
var
   prog : IdwsProgram;
   sym : TTypeSymbol;
begin
   prog:=FCompiler.Compile(
       'type TPrivate = class private field : Integer; end;'#13#10
      +'type TPrivateRec = record private field : Integer; end;'#13#10
      +'type TPublished = class published function func : Integer; begin Result := 1; end; end;'#13#10
      +'type TPublicProtected = class (Object) protected field : Integer; public property prop : Integer read field; end;');

   sym := prog.Table.FindTypeLocal('TPrivate');
   CheckTrue((sym as TCompositeTypeSymbol).MembersVisibilities = [cvPrivate, cvPublic], 'private class');

   sym := prog.Table.FindTypeLocal('TPrivateRec');
   CheckTrue((sym as TCompositeTypeSymbol).MembersVisibilities = [cvPrivate], 'private record');

   sym := prog.Table.FindTypeLocal('TPublished');
   CheckTrue((sym as TCompositeTypeSymbol).MembersVisibilities = [cvPublic, cvPublished], 'published class');

   sym := prog.Table.FindTypeLocal('TPublicProtected');
   CheckTrue((sym as TCompositeTypeSymbol).MembersVisibilities = [cvPublic, cvProtected], 'public protected class');
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
