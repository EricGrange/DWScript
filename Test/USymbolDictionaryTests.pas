unit USymbolDictionaryTests;

interface

uses
   Windows, Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext, dwsInfo,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsJSON, dwsFunctions,
   dwsFilter, dwsScriptSource, dwsSymbolDictionary, dwsContextMap,
   dwsCompilerContext;

type

   TSymbolDictionaryTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;

      public
         procedure SetUp; override;
         procedure TearDown; override;
         procedure DoOnInclude(const scriptName : String; var scriptSource : String);

      published
         procedure BasicForward;
         procedure BasicForward2;
         procedure BasicForward2Overloaded;
         procedure BasicForward3;
         procedure BasicForward3Overloaded;

         procedure BasicMethod;
         procedure BasicClassMethod;

         procedure BasicOverride;

         procedure PartialClass;
         procedure PartialClassMethod;

         procedure PseudoMethod;

         procedure OverloadForwardDictionary;
         procedure OverloadMethodDictionary;
         procedure ClassForwardDictionary;
         procedure ClassForwardPartialDictionary;

         procedure SymDictFunctionForward;
         procedure SymDictInherited;
         procedure SymDictParamExplicit;
         procedure SymDictParamImplicit;

         procedure EnumCastPos;
   end;

   ETestException = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TSymbolDictionaryTests ------------------
// ------------------

// SetUp
//
procedure TSymbolDictionaryTests.SetUp;
begin
   FCompiler:=TDelphiWebScript.Create(nil);
   FCompiler.Config.CompilerOptions := FCompiler.Config.CompilerOptions + [coSymbolDictionary];

   FUnit:=TdwsUnit.Create(nil);
   FUnit.UnitName:='CornerCases';
   FUnit.Script:=FCompiler;
end;

// TearDown
//
procedure TSymbolDictionaryTests.TearDown;
begin
   FUnit.Free;
   FCompiler.Free;
end;

// DoOnInclude
//
procedure TSymbolDictionaryTests.DoOnInclude(const scriptName : String; var scriptSource : String);
begin
   if scriptName='comment.inc' then
      scriptSource:='{'
   else if scriptName='string.inc' then
      scriptSource:='"he'
   else if scriptName='define.test' then
      scriptSource:='{$DEFINE TEST}'
   else begin
      CheckEquals('test.dummy', scriptName, 'DoOnInclude');
      scriptSource:='Print(''hello'');';
   end;
end;

// BasicForward
//
procedure TSymbolDictionaryTests.BasicForward;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'procedure Test(a1 : Integer); forward;'#13#10
      +'procedure Test(a1 : Integer); begin end;'#13#10
      );
   CheckEquals(
      '[{"symbol":{"name":"a1","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 2, column: 16]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 1, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 2, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);

   prog:=FCompiler.Compile(
       'procedure Test(a1 : Integer); overload; forward;'#13#10
      +'procedure Test(a1 : Integer); begin end;'#13#10
      );
   CheckEquals(
      '[{"symbol":{"name":"a1","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 2, column: 16]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 1, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 2, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicForward2
//
procedure TSymbolDictionaryTests.BasicForward2;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'procedure Test1(a1 : Integer); forward;'#13#10
      +'procedure Test2(a2 : Integer); forward;'#13#10
      +'procedure Test1(a1 : Integer); begin end;'#13#10
      +'procedure Test2(a2 : Integer); begin end;'#13#10
      );
   CheckEquals(
      '[{"symbol":{"name":"a1","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 17]"},'
         +'{"usages":["suReference"],"position":" [line: 3, column: 17]"}]},'
      +'{"symbol":{"name":"a2","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 17]"},'
         +'{"usages":["suReference"],"position":" [line: 4, column: 17]"}]},'
      +'{"symbol":{"name":"Test1","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 1, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 3, column: 11]"}]},'
      +'{"symbol":{"name":"Test2","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 2, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 4, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicForward2Overloaded
//
procedure TSymbolDictionaryTests.BasicForward2Overloaded;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'procedure Test(a1 : Integer); overload; forward;'#13#10
      +'procedure Test(a2 : Integer); overload; forward;'#13#10
      +'procedure Test(a1 : Integer); begin end;'#13#10
      +'procedure Test(a2 : Integer); begin end;'#13#10
      );
   CheckEquals(
      '[{"symbol":{"name":"a1","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 3, column: 16]"}]},'
      +'{"symbol":{"name":"a2","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 4, column: 16]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 1, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 3, column: 11]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 2, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 4, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicForward3
//
procedure TSymbolDictionaryTests.BasicForward3;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'procedure Test1(a1 : Integer); forward;'#13#10
      +'procedure Test2(a2 : Integer); forward;'#13#10
      +'procedure Test3(a3 : Integer); forward;'#13#10
      +'procedure Test1(a1 : Integer); begin end;'#13#10
      +'procedure Test2(a2 : Integer); begin end;'#13#10
      +'procedure Test3(a3 : Integer); begin end;'#13#10
      );
   CheckEquals(
      '[{"symbol":{"name":"a1","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 17]"},'
         +'{"usages":["suReference"],"position":" [line: 4, column: 17]"}]},'
      +'{"symbol":{"name":"a2","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 17]"},'
         +'{"usages":["suReference"],"position":" [line: 5, column: 17]"}]},'
      +'{"symbol":{"name":"a3","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 3, column: 17]"},'
         +'{"usages":["suReference"],"position":" [line: 6, column: 17]"}]},'
      +'{"symbol":{"name":"Test1","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 1, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 4, column: 11]"}]},'
      +'{"symbol":{"name":"Test2","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 2, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 5, column: 11]"}]},'
      +'{"symbol":{"name":"Test3","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 3, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 6, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicForward3Overloaded
//
procedure TSymbolDictionaryTests.BasicForward3Overloaded;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'procedure Test(a1 : Integer); overload; forward;'#13#10
      +'procedure Test(a2 : Integer); overload; forward;'#13#10
      +'procedure Test(a3 : Integer); overload; forward;'#13#10
      +'procedure Test(a1 : Integer); begin end;'#13#10
      +'procedure Test(a2 : Integer); begin end;'#13#10
      +'procedure Test(a3 : Integer); begin end;'#13#10
      );
   CheckEquals(
       '[{"symbol":{"name":"a1","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 4, column: 16]"}]},'
      +'{"symbol":{"name":"a2","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 5, column: 16]"}]},'
      +'{"symbol":{"name":"a3","class":"TParamSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 3, column: 16]"},'
         +'{"usages":["suReference"],"position":" [line: 6, column: 16]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 1, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 4, column: 11]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 2, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 5, column: 11]"}]},'
      +'{"symbol":{"name":"Test","class":"TSourceFuncSymbol"},"positions":['
         +'{"usages":["suForward","suDeclaration"],"position":" [line: 3, column: 11]"},'
         +'{"usages":["suImplementation"],"position":" [line: 6, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicMethod
//
procedure TSymbolDictionaryTests.BasicMethod;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'type TTest = class procedure Test; end;'#13#10
      +'procedure TTest.Test; begin end;'#13#10
      );
   CheckEquals(
       '[{"symbol":{"name":"Test","class":"TSourceMethodSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 30]"},'
         +'{"usages":["suImplementation"],"position":" [line: 2, column: 17]"}]},'
      +'{"symbol":{"name":"TTest","class":"TClassSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 6]"},'
         +'{"usages":["suReference"],"position":" [line: 2, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicClassMethod
//
procedure TSymbolDictionaryTests.BasicClassMethod;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'type TTest = class class procedure Test; end;'#13#10
      +'class procedure TTest.Test; begin end;'#13#10
      );
   CheckEquals(
       '[{"symbol":{"name":"Test","class":"TSourceMethodSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 36]"},'
         +'{"usages":["suImplementation"],"position":" [line: 2, column: 23]"}]},'
      +'{"symbol":{"name":"TTest","class":"TClassSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 6]"},'
         +'{"usages":["suReference"],"position":" [line: 2, column: 17]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// BasicOverride
//
procedure TSymbolDictionaryTests.BasicOverride;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'type TTest = class destructor Destroy; override; begin end; end;'#13#10
      );
   CheckEquals(
       '[{"symbol":{"name":"Destroy","class":"TMethodSymbol"},"positions":['
         +'{"usages":["suImplicit"],"position":" [line: 1, column: 31]"}]},'
      +'{"symbol":{"name":"Destroy","class":"TSourceMethodSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 31]"}]},'
      +'{"symbol":{"name":"TTest","class":"TClassSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 6]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// PartialClass
//
procedure TSymbolDictionaryTests.PartialClass;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'type TTest = class partial end;'#13#10
      +'type TTest = class partial end;'#13#10
      );
   CheckEquals(
       '[{"symbol":{"name":"TTest","class":"TClassSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 6]"},'
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 6]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// PartialClassMethod
//
procedure TSymbolDictionaryTests.PartialClassMethod;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
       'type TTest = class partial end;'#13#10
      +'type TTest = class partial procedure Test; end;'#13#10
      +'procedure TTest.Test; begin end;'#13#10
      );
   CheckEquals(
        '[{"symbol":{"name":"Test","class":"TSourceMethodSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 38]"},'
         +'{"usages":["suImplementation"],"position":" [line: 3, column: 17]"}]},'
       +'{"symbol":{"name":"TTest","class":"TClassSymbol"},"positions":['
         +'{"usages":["suDeclaration"],"position":" [line: 1, column: 6]"},'
         +'{"usages":["suDeclaration"],"position":" [line: 2, column: 6]"},'
         +'{"usages":["suReference"],"position":" [line: 3, column: 11]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// PseudoMethod
//
procedure TSymbolDictionaryTests.PseudoMethod;
var
   prog : IdwsProgram;
begin
   prog:=FCompiler.Compile(
      'StrSplit('', '').Map(Trim);'
      );
   CheckEquals(
          '[{"symbol":{"name":"Map","class":"TPseudoMethodSymbol"},"positions":[{"usages":["suReference"],"position":" [line: 1, column: 16]"}]},'
        + '{"symbol":{"name":"StrSplit","class":"TMagicFuncSymbol"},"positions":[{"usages":["suReference","suWrite"],"position":" [line: 1, column: 1]"}]},'
        + '{"symbol":{"name":"Trim","class":"TMagicFuncSymbol"},"positions":[{"usages":["suReference","suRead"],"position":" [line: 1, column: 20]"}]}]',
      prog.SymbolDictionary.ToJSON);
end;

// OverloadForwardDictionary
//
procedure TSymbolDictionaryTests.OverloadForwardDictionary;
var
   prog : IdwsProgram;
   symPosList : TSymbolPositionList;
begin
   prog:=FCompiler.Compile(
       'procedure Test(a1 : Integer); overload; forward;'#13#10
      +'procedure Test(a2 : String); overload; forward;'#13#10
      +'procedure Test(a3 : Boolean); overload; forward;'#13#10
      +'procedure Test(a1 : Integer); begin end;'#13#10
      +'procedure Test(a2 : String); begin end;'#13#10
      +'procedure Test(a3 : Boolean); begin end;'#13#10
      );

   CheckEquals('', prog.Msgs.AsInfo, 'compile A');
   prog.SymbolDictionary.Pack;
   CheckEquals(6, prog.SymbolDictionary.Count, 'nb symbols A');
   for symPosList in prog.SymbolDictionary do
      CheckEquals(2, symPosList.Count,
                   'A declared + reference for '+symPosList.Symbol.Name);

   prog:=nil;

   prog:=FCompiler.Compile(
       'procedure Test(a1, a2 : Integer); overload; forward;'#13#10
      +'procedure Test(a2, a1 : String); overload; forward;'#13#10
      +'procedure Test(a1, a2 : Integer); overload; begin end;'#13#10
      +'procedure Test(a2, a1 : String); overload; begin end;'#13#10
      );

   CheckEquals('', prog.Msgs.AsInfo, 'compile B');
   prog.SymbolDictionary.Pack;
   CheckEquals(6, prog.SymbolDictionary.Count, 'nb symbols B');
   for symPosList in prog.SymbolDictionary do
      CheckEquals(2, symPosList.Count,
                   'B declared + reference for '+symPosList.Symbol.Name);
end;

// OverloadMethodDictionary
//
procedure TSymbolDictionaryTests.OverloadMethodDictionary;
var
   prog : IdwsProgram;
   symPosList : TSymbolPositionList;
begin
   prog:=FCompiler.Compile(
       'type TTest = class'#13#10
      +'procedure Test(a1 : Integer); overload; '#13#10
      +'procedure Test(a2 : String); overload;'#13#10
      +'procedure Test(a3 : Boolean); overload;'#13#10
      +'end;'#13#10
      +'procedure TTest.Test(a1 : Integer); begin end;'#13#10
      +'procedure TTest.Test(a2 : String); begin end;'#13#10
      +'procedure TTest.Test(a3 : Boolean); begin end;'#13#10
      );

   CheckEquals('', prog.Msgs.AsInfo, 'compile A');
   prog.SymbolDictionary.Pack;
   CheckEquals(7, prog.SymbolDictionary.Count, 'nb symbols A');
   for symPosList in prog.SymbolDictionary do begin
      if symPosList.Symbol.Name='TTest' then
         CheckEquals(4, symPosList.Count, 'TTest A')
      else CheckEquals(2, symPosList.Count,
                       'A declared + reference for '+symPosList.Symbol.Name);
   end;

   prog:=nil;

   prog:=FCompiler.Compile(
       'type TTest = class'#13#10
      +'procedure Test(a1, a2 : Integer); overload;'#13#10
      +'procedure Test(a2, a1 : String); overload;'#13#10
      +'end;'#13#10
      +'procedure TTest.Test(a1, a2 : Integer); begin end;'#13#10
      +'procedure TTest.Test(a2, a1 : String); begin end;'#13#10
      );

   CheckEquals('', prog.Msgs.AsInfo, 'compile B');
   prog.SymbolDictionary.Pack;
   CheckEquals(7, prog.SymbolDictionary.Count, 'nb symbols B');
   for symPosList in prog.SymbolDictionary do begin
      if symPosList.Symbol.Name='TTest' then
         CheckEquals(3, symPosList.Count, 'TTest B')
      else CheckEquals(2, symPosList.Count,
                       'B declared + reference for '+symPosList.Symbol.Name);
   end;
end;

// ClassForwardDictionary
//
procedure TSymbolDictionaryTests.ClassForwardDictionary;
var
   prog : IdwsProgram;
begin
   prog := FCompiler.Compile( 'type TTest = class;'#13#10
                             +'type TTest = class end;');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   CheckEquals(1, prog.SymbolDictionary.FindSymbolUsage('TTest', suForward).ScriptPos.Line, 'forward');
   CheckEquals(2, prog.SymbolDictionary.FindSymbolUsage('TTest', suDeclaration).ScriptPos.Line, 'declaration');
end;

// ClassForwardPartialDictionary
//
procedure TSymbolDictionaryTests.ClassForwardPartialDictionary;
var
   prog : IdwsProgram;
   symPosList : TSymbolPositionList;
begin
   prog := FCompiler.Compile( 'type TTest = class partial;'#13#10
                             +'type TTest = class partial end;'
                             +'type TTest = class partial end;');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   symPosList:=prog.SymbolDictionary.FindSymbolPosList('TTest');
   CheckTrue(symPosList.Items[0].SymbolUsages=[suForward], 'forward');
   CheckTrue(symPosList.Items[1].SymbolUsages=[suDeclaration], 'declaration 1');
   CheckTrue(symPosList.Items[2].SymbolUsages=[suDeclaration], 'declaration 2');
end;

// SymDictFunctionForward
//
procedure TSymbolDictionaryTests.SymDictFunctionForward;
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
procedure TSymbolDictionaryTests.SymDictInherited;
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
   Check(symPosList[1].SymbolUsages=[suImplicit], 'TBaseClass Line 2 usage');

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
procedure TSymbolDictionaryTests.SymDictParamExplicit;
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
procedure TSymbolDictionaryTests.SymDictParamImplicit;
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

// EnumCastPos
//
procedure TSymbolDictionaryTests.EnumCastPos;
var
   prog : IdwsProgram;
   sym : TTypeSymbol;
   spl : TSymbolPositionList;
begin
   prog:=FCompiler.Compile( 'type TTest = (one, two);'#13#10
                           +'var a := 1;'#13#10
                           +'var b := TTest(a);'#13#10);
   CheckEquals('', prog.Msgs.AsInfo);

   sym := prog.Table.FindTypeSymbol('TTest', cvMagic);

   spl := prog.SymbolDictionary.FindSymbolPosList(sym);

   CheckEquals(3, spl.Count, 'TTest');
   CheckEquals(' [line: 1, column: 6]', spl.Items[0].ScriptPos.AsInfo);
   CheckEquals(' [line: 3, column: 10]', spl.Items[1].ScriptPos.AsInfo);
   CheckEquals(' [line: 3, column: 7]', spl.Items[2].ScriptPos.AsInfo);
   Check(suImplicit in spl.Items[2].SymbolUsages, '3rd use is implicit');
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('SymbolDictionaryTests', TSymbolDictionaryTests);

end.
