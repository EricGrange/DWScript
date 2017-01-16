unit USymbolDictionaryTests;

interface

uses
   Windows, Classes, SysUtils,
   dwsXPlatformTests, dwsComp, dwsCompiler, dwsExprs, dwsDataContext, dwsInfo,
   dwsTokenizer, dwsXPlatform, dwsFileSystem, dwsErrors, dwsUtils, Variants,
   dwsSymbols, dwsPascalTokenizer, dwsStrings, dwsJSON, dwsFunctions,
   dwsFilter, dwsScriptSource, dwsSymbolDictionary, dwsContextMap;

type

   TSymbolDictionaryTests = class (TTestCase)
      private
         FCompiler : TDelphiWebScript;
         FUnit : TdwsUnit;

      public
         procedure SetUp; override;
         procedure TearDown; override;
         procedure DoOnInclude(const scriptName : UnicodeString; var scriptSource : UnicodeString);

      published
         procedure BasicForward;
         procedure BasicForward2;
         procedure BasicForward2Overloaded;
         procedure BasicForward3;
         procedure BasicForward3Overloaded;

         procedure OverloadForwardDictionary;
         procedure OverloadMethodDictionary;
         procedure ClassForwardDictionary;
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
procedure TSymbolDictionaryTests.DoOnInclude(const scriptName : UnicodeString; var scriptSource : UnicodeString);
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
   symPosList : TSymbolPositionList;
begin
   prog := FCompiler.Compile( 'type TTest = class;'#13#10
                             +'type TTest = class end;');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   CheckEquals(1, prog.SymbolDictionary.FindSymbolUsage('TTest', suForward).ScriptPos.Line, 'forward');
   CheckEquals(2, prog.SymbolDictionary.FindSymbolUsage('TTest', suDeclaration).ScriptPos.Line, 'declaration');


   prog := FCompiler.Compile( 'type TTest = class partial;'#13#10
                             +'type TTest = class partial end;'
                             +'type TTest = class partial end;');

   CheckEquals('', prog.Msgs.AsInfo, 'compile');
   symPosList:=prog.SymbolDictionary.FindSymbolPosList('TTest');
   CheckTrue(symPosList.Items[0].SymbolUsages=[suForward], 'forward');
   CheckTrue(symPosList.Items[1].SymbolUsages=[suDeclaration], 'declaration 1');
   CheckTrue(symPosList.Items[2].SymbolUsages=[suDeclaration], 'declaration 2');
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
