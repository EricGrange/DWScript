unit UCodeDOMTests;

{$I ..\Source\dws.inc}

interface

uses
   System.Classes, System.SysUtils,
   dwsXPlatformTests, dwsXPlatform, dwsJSON, dwsUtils,
   dwsCodeDOM, dwsCodeDOMParser, dwsCodeDOMPascalParser,
   dwsTokenizer, dwsPascalTokenizer, dwsScriptSource, dwsErrors;

type

   TOutlineMode = ( omCompact, omVerbose, omErrorsOnly );

   TCodeDOMTests = class (TTestCase)
      private
         FDOMTests : TStringList;
         FTests : TStringList;
         FPascalRules : TdwsCodeDOMPascalParser;
         FTokRules : TTokenizerRules;
         FParser : TdwsParser;

         function ToOutline(const code : String; mode : TOutlineMode = omCompact) : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure ParsePascal;
         procedure ParseWithoutErrors;

         procedure SimpleAssignment;
         procedure LiteralString;
         procedure LiteralFloats;
         procedure TailComment;
         procedure IfThenElse;
         procedure SimpleClassDecl;
         procedure SimpleClassScope;
         procedure ClassOfDecl;
         procedure Conditionals;
         procedure ArrayTypes;
         procedure CaseOf;
         procedure ForLoop;
         procedure EscapedNames;
         procedure Enums;
         procedure FunctionDecl;
         procedure FunctionType;
         procedure FunctionCall;
         procedure ArrayIndex;
         procedure BinOps;
         procedure Booleans;
         procedure TryFinally;
         procedure DotOperator;
         procedure UsesClause;
         procedure MultiLineBinaryOp;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TCodeDOMTests ------------------
// ------------------

// SetUp
//
procedure TCodeDOMTests.SetUp;
const
   cFilter = '*.pas';
begin
   FPascalRules := TdwsCodeDOMPascalParser.Create;
   FTokRules := TPascalTokenizerStateRules.Create;

   FParser := TdwsParser.Create(FTokRules.CreateTokenizer(nil, nil), FPascalRules.CreateRules);

   var basePath := ExtractFilePath(ParamStr(0));

   FDOMTests := TStringList.Create;
   CollectFiles(basePath + 'DOMParser' + PathDelim, cFilter, FDOMTests);

   FTests := TStringList.Create;
   CollectFiles(basePath+'SimpleScripts'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'ArrayPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'AssociativePass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'HelpersPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'InnerClassesPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'InterfacesPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'LambdaPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'OperatorOverloadPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'OverloadsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'PropertyExpressionsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'GenericsPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'SetOfPass'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'Algorithms'+PathDelim, cFilter, FTests);
   CollectFiles(basePath+'BuildScripts'+PathDelim, cFilter, FTests);

   // these tests require conditionals & switch processing or have expected failure
   for var i := FTests.Count-1 downto 0 do begin
      if StrEndsWith(FTests[i], 'conditionals_ifndef.pas') then FTests.Delete(i);
      if StrEndsWith(FTests[i], 'include_expr.pas') then FTests.Delete(i);
      if StrEndsWith(FTests[i], 'Sections.pas') then FTests.Delete(i);
      if StrEndsWith(FTests[i], 'UnfinishedDirective.pas') then FTests.Delete(i);
   end;
end;

// TearDown
//
procedure TCodeDOMTests.TearDown;
begin
   FTests.Free;
   FDOMTests.Free;
   FParser.Free;
   FTokRules.Free;
   FPascalRules.Free;
end;

// ToOutline
//
function TCodeDOMTests.ToOutline(const code : String; mode : TOutlineMode = omCompact) : String;

   function CompactLine(const line : String) : String;
   begin
      var nb := Length(line);
      Result := TrimLeft(line);
      nb := nb - Length(Result);
      if nb > 1 then
         Result := IntToStr(nb div 3) + Result;
   end;

begin
   var sourceFile := TSourceFile.Create;
   try
      sourceFile.Code := code;
      var dom := FParser.Parse(sourceFile);
      try
         var wobs := TWriteOnlyBlockStream.AllocFromPool;
         try
            dom.Root.WriteToOutline(wobs, 0);
            if mode in [ omVerbose, omCompact ] then
               Result := wobs.ToString;
         finally
            wobs.ReturnToPool;
         end;
      finally
         dom.Free;
      end;
      if (mode = omCompact) and (Result <> '') then begin
         var list := TStringList.Create;
         try
            list.Text := Result;
            Result := CompactLine(list[0]);
            for var i := 1 to list.Count-1 do
               Result := Result + ',' + CompactLine(list[i]);
         finally
            list.Free;
         end;
      end;
      if FParser.Messages.Count > 0 then
         Result := FParser.Messages.AsInfo + Result;
   finally
      sourceFile.Free;
   end;
end;

// ParsePascal
//
procedure TCodeDOMTests.ParsePascal;
begin
   for var i := 0 to FDOMTests.Count-1 do begin
      var code := LoadTextFromFile(FDOMTests[i]);
      var expected := LoadTextFromFile(ChangeFileExt(FDOMTests[i], '.txt'));
      expected := TrimRight(StringReplace(expected, #13#10, #10, [ rfReplaceAll ]));
      var actual := ToOutline(code, omVerbose);
      actual := TrimRight(StringReplace(actual, #13#10, #10, [ rfReplaceAll ]));
      CheckEquals(expected, actual, FDOMTests[i]);
   end;
end;

// ParseWithoutErrors
//
procedure TCodeDOMTests.ParseWithoutErrors;
begin
   for var i := 0 to FTests.Count-1 do begin
      var code := LoadTextFromFile(FTests[i]);
      var outline := ToOutline(code, omErrorsOnly);
      CheckEquals('', outline, FTests[i]+#13#10+outline);
   end;
end;

// SimpleAssignment
//
procedure TCodeDOMTests.SimpleAssignment;
begin
   CheckEquals(
      'Main,1StatementList,2VarSection,3Token var,3VarDeclaration,4Token name <<a>>,4Token :=,4Token Integer Literal <<1>>,2Token ;',
      ToOutline('var a := 1;')
   );
end;

// LiteralString
//
procedure TCodeDOMTests.LiteralString;
begin
   CheckEquals(
      'Main,1StatementList,2Assignment,3Reference,4Token name <<a>>,3Token :=,3Token UnicodeString Literal <<''1''>>,2Token ;',
      ToOutline('a := ''1'';')
   );
   CheckEquals(
      'Main,1Assignment,2Reference,3Token name <<a>>,2Token :=,2LiteralStr,3Token UnicodeString Literal <<#9>>,3Token UnicodeString Literal <<"abc">>,3Token UnicodeString Literal <<#10>>',
      ToOutline('a:=#9"abc"#10')
   );
end;

// LiteralFloats
//
procedure TCodeDOMTests.LiteralFloats;
begin
   CheckEquals(
      'Main,1Call,2Reference,3Token name <<a>>,2Parameters,3Token (,3Token Float Literal <<1.0>>,3Token )',
      ToOutline('a(1.0)')
   );
end;

// TailComment
//
procedure TCodeDOMTests.TailComment;
begin
   CheckEquals(
      'Main,1Comment,2Token comment <</* a>> [LF]',
      ToOutline('/* a')
   );
   CheckEquals(
      'Main,1StatementList,2Call,3Reference,4Token name <<a>>,3Parameters,4Token (,4Token ),2Token ;,3Comment,4Token comment <<// here>> [LF]',
      ToOutline('a(); // here')
   );
   CheckEquals(
      'Main,1Reference,2Token name <<a>> [LF],1Comment,2Token comment <</* bla,bla */>> [PL] [LF]',
      ToOutline('a'#10#10'/* bla'#10'bla */')
   );
end;

// IfThenElse
//
procedure TCodeDOMTests.IfThenElse;
begin
   CheckEquals(
      'Main,1StatementList,2IfThenElseStmt,3Token if,3Reference,4Token name <<b>>,3Token then,3Call,4Reference,5Token name <<doit>>,4Parameters,5Token (,5Token ),3Token else,3Call,4Reference,5Token name <<dont>>,4Parameters,5Token (,5Token ),2Token ;',
      ToOutline('if b then doit() else dont();')
   );
end;

// SimpleClassDecl
//
procedure TCodeDOMTests.SimpleClassDecl;
begin
   CheckEquals(
      'Main,1StatementList,2TypeInline,3Token type,3TypeDecl,4Reference,5Token name <<TTest>>,4Token =,4ClassFwd,5Token class,2Token ;',
      ToOutline('type TTest = class;')
   );
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassBody,5Token end',
      ToOutline('type TTest = class end')
   );
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassInh,5Token (,5Reference,6Token name <<TParent>>,5Token ,,5Reference,6Token name <<IInterface>>,5Token )',
      ToOutline('type TTest = class (TParent, IInterface)')
   );
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassBody,5TypeInnerDecl,6FieldDeclaration,7NameList,8Token name <<Field>>,7Token :,7Reference,8Token name <<Integer>>,5Token end',
      ToOutline('type TTest=class Field : Integer end')
   );
   CheckEquals(
       'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassBody,5TypeInnerDecl,6FieldDeclaration,7NameList,8Token name <<Field>>,7Token :,7Reference,8Token name <<Integer>>,'
      +'7Node,8Token ;,8Token readonly,6Token ;,5Token end',
      ToOutline('type TTest=class Field : Integer; readonly; end')
   );
end;

// SimpleClassScope
//
procedure TCodeDOMTests.SimpleClassScope;
begin
   CheckEquals(
      'Main,1StatementList,2TypeInline,3Token type,3TypeDecl,4Reference,5Token name <<TTest>>,4Token =,4ClassDecl,5ClassFwd,6Token class,5ClassBody,6TypeVisibilitySection,7Token private,6Token end,2Token ;',
      ToOutline('type TTest = class private end;')
   );
end;

// ClassOfDecl
//
procedure TCodeDOMTests.ClassOfDecl;
begin
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<TClass>>,3Token =,3ClassOfDecl,4Token class,5Token of,4Reference,5Token name <<TObject>>',
      ToOutline('type TClass = class of TObject')
   );
end;

// Conditionals
//
procedure TCodeDOMTests.Conditionals;
begin
   CheckEquals(
      'Main,1Switch,2Token switch <<{$ifdef>>,2Token name <<a>>,2Token },1StatementList,2Reference,3Token name <<b>>,2Token ;,3Switch,4Token switch <<{$endif>>,4Token }',
      ToOutline('{$ifdef a}b;{$endif}')
   );
   CheckEquals(
      'Main,1Switch,2Token switch <<{$ifdef>>,2Token name <<a>>,2Token },1Switch,2Token switch <<{$define>>,2Token name <<a>>,2Token },1Switch,2Token switch <<{$endif>>,2Token }',
      ToOutline('{$ifdef a}{$define a}{$endif}')
   );
   CheckEquals(
      'Main,1StatementList,2Assignment,3Reference,4Token name <<a>>,3Token :=,4Switch,5Token switch <<{$ifdef>>,5Token name <<TEST>>,5Token },4Switch,5Token switch <<{$endif>>,5Token },3Token Integer Literal <<1>>,2Token ;',
      ToOutline('a := {$ifdef TEST}{$endif}1;')
   );
end;

// ArrayTypes
//
procedure TCodeDOMTests.ArrayTypes;
begin
   CheckEquals(
      'Main,1VarSection,2Token var,2VarDeclaration,3NameList,4Token name <<a>>,3Token :,3ArrayDecl,4Token array,4Token of,4Reference,5Token name <<String>>',
      ToOutline('var a : array of String')
   );
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<t>>,3Token =,3ArrayDecl,4Token array,4ArrayRange,5Token [,5Reference,6Token name <<Integer>>,5Token ],4Token of,4Reference,5Token name <<String>>',
      ToOutline('type t = array[Integer]of String')
   );
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<t>>,3Token =,3ArrayDecl,4Token array,4ArrayRange,5Token [,5Range,6Token Integer Literal <<0>>,6Token ..,6Token Integer Literal <<1>>,5Token ],4Token of,4Reference,5Token name <<Byte>>',
      ToOutline('type t = array [0 .. 1] of Byte')
   );
end;

// CaseOf
//
procedure TCodeDOMTests.CaseOf;
begin
   CheckEquals(
      'Main,1CaseOf,2Token case,2Reference,3Token name <<a>>,2Token of,2CaseOfAlternatives,3CaseOfAlternative,'
      +'4CaseOfAlternativeCases,5Range,6UnaryOperator -,7Token -,7Token Integer Literal <<1>>,6Token ..,6UnaryOperator +,7Token +,7Token Integer Literal <<1>>,4Token :,4Reference,5Token name <<b>>,2Token end',
      ToOutline('case a of -1..+1 : b end')
   );
end;

// ForLoop
//
procedure TCodeDOMTests.ForLoop;
begin
   CheckEquals(
      'Main,1StatementList,2ForLoop,3Token for,3Token name <<a>>,3Token :=,3Token Integer Literal <<1>>,3Token to,3Token Integer Literal <<2>>,3Token do,2Token ;',
      ToOutline('for a := 1 to 2 do ;')
   );
   CheckEquals(
      'Main,1ForIn,2Token for,2Token name <<a>>,2Token in,2Reference,3Token name <<b>>,2Token do,2BeginEnd,3Token begin,3Token end',
      ToOutline('for a in b do begin end')
   );
end;

// EscapedNames
//
procedure TCodeDOMTests.EscapedNames;
begin
   CheckEquals(
      'Main,1StatementList,2VarSection,3Token var,3VarDeclaration,4Token name <<&begin>>,4Token :=,4Token Integer Literal <<1>>,2Token ;',
      ToOutline('var &begin := 1;')
   );
end;

// Enums
//
procedure TCodeDOMTests.Enums;
begin
   CheckEquals(
      'Main,1TypeInline,2Token type,2TypeDecl,3Reference,4Token name <<e>>,3Token =,3EnumDecl,4Token (,4EnumElements,5Token name <<a>>,4Token )',
      ToOutline('type e = (a)')
   );
end;

// FunctionDecl
//
procedure TCodeDOMTests.FunctionDecl;
begin
   CheckEquals(
      'Main,1StatementList,2FunctionImpl,3FunctionDecl,4Token procedure,4Reference,5Token name <<Test>>,3Token ;,3FunctionBlock,4Token begin,4Token end,2Token ;',
      ToOutline('procedure Test; begin end;')
   );
   CheckEquals(
      'Main,1StatementList,2FunctionDecl,3Token procedure,3Reference,4Token name <<Test>>,3FunctionQualifier,4Token ;,4Token forward,2Token ;',
      ToOutline('procedure Test; forward;')
   );
end;

// FunctionType
//
procedure TCodeDOMTests.FunctionType;
begin
   CheckEquals(
      'Main,1StatementList,2VarSection,3Token var,3VarDeclaration,4NameList,5Token name <<a>>,4Token :,4FunctionDecl,5Token procedure,2Token ;',
      ToOutline('var a : procedure;')
   );
end;

// FunctionCall
//
procedure TCodeDOMTests.FunctionCall;
begin
   CheckEquals(
      'Main,1StatementList,2Call,3Call,4Reference,5Token name <<f>>,4Parameters,5Token (,5Token ),3Parameters,4Token (,4Token ),2Token ;',
      ToOutline('f()();')
   );
   CheckEquals(
      'Main,1StatementList,2Index,3Call,4Reference,5Token name <<a>>,4Parameters,5Token (,5Token ),3Indexes,4Token [,4Token Integer Literal <<0>>,4Token ],2Token ;',
      ToOutline('a()[0];')
   );
   CheckEquals(
      'Main,1StatementList,2Field,3Call,4Reference,5Token name <<a>>,4Parameters,5Token (,5Token ),3Token .,3Reference,4Token name <<b>>,2Token ;',
      ToOutline('a().b;')
   );
end;

// ArrayIndex
//
procedure TCodeDOMTests.ArrayIndex;
begin
   CheckEquals(
      'Main,1Assignment,2Index,3Reference,4Token name <<a>>,3Indexes,4Token [,4Token Integer Literal <<1>>,4Token ],2Token :=,2Token Integer Literal <<0>>',
      ToOutline('a[1] := 0')
   );
   CheckEquals(
      'Main,1Index,2Index,3Reference,4Token name <<a>>,3Indexes,4Token [,4Token Integer Literal <<1>>,4Token ],2Indexes,3Token [,3Token Integer Literal <<2>>,3Token ]',
      ToOutline('a[1][2]')
   );
end;

// BinOps
//
procedure TCodeDOMTests.BinOps;
begin
   CheckEquals(
      'Main,1Assignment,2Reference,3Token name <<a>>,2Token :=,2BinaryOperator,3Reference,4Token name <<b>>,3Token *,3BinaryOperator,4Reference,5Token name <<c>>,4Token *,4Reference,5Token name <<d>>',
      ToOutline('a := b * c * d')
   );
   CheckEquals(
      'Main,1Assignment,2Reference,3Token name <<a>>,2Token :=,2BinaryOperator,3Reference,4Token name <<b>>,3Token +,3BinaryOperator,4Reference,5Token name <<c>>,4Token +,4Reference,5Token name <<d>>',
      ToOutline('a := b + c + d')
   );
   CheckEquals(
      'Main,1Call,2Reference,3Token name <<a>>,2Parameters,3Token (,3BinaryOperator,4Reference,5Token name <<b>>,4Token +,4BinaryOperator,5Token Integer Literal <<1>>,5Token -,5Reference,6Token name <<c>>,3Token )',
      ToOutline('a(b + 1 - c)')
   );
   CheckEquals(
      'Main,1Assignment,2Reference,3Token name <<a>>,2Token :=,2BinaryOperator,3Reference,4Token name <<b>>,3Token not,4Token in,3Reference,4Token name <<c>>',
      ToOutline('a := b not in c')
   );
end;

// Booleans
//
procedure TCodeDOMTests.Booleans;
begin
   CheckEquals(
      'Main,1VarSection,2Token var,2VarDeclaration,3Token name <<a>>,3Token =,3UnaryOperator not,4Token not,4Token True',
      ToOutline('var a = not True')
   );
end;

// TryFinally
//
procedure TCodeDOMTests.TryFinally;
begin
   CheckEquals(
      'Main,1Try,2Token try,2TryFinally,3Token finally,2Token end',
      ToOutline('try finally end')
   );
end;

// DotOperator
//
procedure TCodeDOMTests.DotOperator;
begin
   CheckEquals(
      'Main,1Field,2Call,3Reference,4Token name <<a>>,3Parameters,4Token (,4Token ),2Token .,2Reference,3Token name <<b>>',
      ToOutline('a().b')
   );
   CheckEquals(
      'Main,1Call,2Reference,3Token name <<a>>,3Token .,3Token name <<b>>,2Parameters,3Token (,3Token )',
      ToOutline('a.b()')
   );
   CheckEquals(
      'Main,1Field,2Index,3Reference,4Token name <<a>>,3Indexes,4Token [,4Token Integer Literal <<1>>,4Token ],2Token .,2Reference,3Token name <<b>>',
      ToOutline('a[1].b')
   );
   CheckEquals(
      'Main,1Field,2Index,3Reference,4Token name <<a>>,3Indexes,4Token [,4Token Integer Literal <<1>>,4Token ],2Token .,2Index,3Reference,4Token name <<b>>,3Indexes,4Token [,4Token Integer Literal <<2>>,4Token ]',
      ToOutline('a[1].b[2]')
   );
end;

// UsesClause
//
procedure TCodeDOMTests.UsesClause;
begin
   CheckEquals(
      'Main,1Comment,2Token comment <<// hello>> [LF],1StatementList,2Uses,3Token uses [PL],3Reference,4Token name <<foo>>,2Token ;',
      ToOutline('// hello'#10#10'uses foo;')
   );
end;

// MultiLineBinaryOp
//
procedure TCodeDOMTests.MultiLineBinaryOp;
begin
   CheckEquals(
      'Main,1Assignment,2Reference,3Token name <<a>>,2Token := [LF],2BinaryOperator,3Reference,4Token name <<hello>> [LF],3Token and,3Reference,4Token name <<world>>',
      ToOutline('a :='#10'hello'#10'and world')
   );
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('CodeDOMTests', TCodeDOMTests);

end.
