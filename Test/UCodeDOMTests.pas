unit UCodeDOMTests;

{$I ..\Source\dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsXPlatform, dwsJSON, dwsUtils,
   dwsCodeDOM, dwsCodeDOMParser, dwsCodeDOMPascalParser,
   dwsTokenizer, dwsPascalTokenizer, dwsScriptSource, dwsErrors;

type

   TCodeDOMTests = class (TTestCase)
      private
         FTests : TStringList;
         FPascalRules : TdwsCodeDOMPascalParser;
         FTokRules : TTokenizerRules;
         FParser : TdwsParser;

         function ToOutline(const code : String; compact : Boolean = False) : String;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure ParsePascal;
         procedure SimpleAssignment;
         procedure LiteralString;
         procedure TailComment;
         procedure IfThenElse;
         procedure SimpleClassDecl;
         procedure Conditionals;
         procedure ArrayTypes;
         procedure CaseOf;
         procedure ForLoop;
         procedure EscapedNames;
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
begin
   FPascalRules := TdwsCodeDOMPascalParser.Create;
   FTokRules := TPascalTokenizerStateRules.Create;

   FParser := TdwsParser.Create(FTokRules.CreateTokenizer(nil, nil), FPascalRules.CreateRules);

   FTests := TStringList.Create;
   CollectFiles(ExtractFilePath(ParamStr(0)) + 'DOMParser' + PathDelim, '*.pas', FTests);
end;

// TearDown
//
procedure TCodeDOMTests.TearDown;
begin
   FTests.Free;
   FParser.Free;
   FTokRules.Free;
   FPascalRules.Free;
end;

// ToOutline
//
function TCodeDOMTests.ToOutline(const code : String; compact : Boolean = False) : String;

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
            Result := wobs.ToString;
         finally
            wobs.ReturnToPool;
         end;
      finally
         dom.Free;
      end;
      if compact and (Result <> '') then begin
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
   for var i := 0 to FTests.Count-1 do begin
      var code := LoadTextFromFile(FTests[i]);
      var expected := LoadTextFromFile(ChangeFileExt(FTests[i], '.txt'));
      CheckEquals(TrimRight(expected), TrimRight(ToOutline(code)), FTests[i]);
   end;
end;


// SimpleAssignment
//
procedure TCodeDOMTests.SimpleAssignment;
begin
   CheckEquals(
      'Main,1StatementList,2VarSection,3Token var,3VarDeclaration,4Token name <<a>>,4Token :=,4Token Integer Literal <<1>>,2Token ;',
      ToOutline('var a := 1;', True)
   );
end;

// LiteralString
//
procedure TCodeDOMTests.LiteralString;
begin
   CheckEquals(
      'Main,1StatementList,2Assignment,3Reference,4Token name <<a>>,3Token :=,3Token UnicodeString Literal <<''1''>>,2Token ;',
      ToOutline('a := ''1'';', True)
   );
   CheckEquals(
      'Main,1Assignment,2Reference,3Token name <<a>>,2Token :=,2LiteralStr,3Token UnicodeString Literal <<#9>>,3Token UnicodeString Literal <<"abc">>,3Token UnicodeString Literal <<#10>>',
      ToOutline('a:=#9"abc"#10', True)
   );
end;

// TailComment
//
procedure TCodeDOMTests.TailComment;
begin
   CheckEquals(
      'Main,1StatementList,2Call,3Reference,4Token name <<a>>,3Token (,3Token ),2Token ;,3Comment,4Token comment <<// here>> [LF]',
      ToOutline('a(); // here', True)
   );
end;

// IfThenElse
//
procedure TCodeDOMTests.IfThenElse;
begin
   CheckEquals(
      'Main,1StatementList,2IfThenElseStmt,3Token if,3Reference,4Token name <<b>>,3Token then,3Call,4Reference,5Token name <<doit>>,4Token (,4Token ),3Token else,3Call,4Reference,5Token name <<dont>>,4Token (,4Token ),2Token ;',
      ToOutline('if b then doit() else dont();', True)
   );
end;

// SimpleClassDecl
//
procedure TCodeDOMTests.SimpleClassDecl;
begin
   CheckEquals(
      'Main,1StatementList,2TypeSection,3Token type,3TypeDecl,4Token name <<TTest>>,4Token =,4ClassFwd,5Token class,2Token ;',
      ToOutline('type TTest = class;', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2TypeDecl,3Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassBody,5Token end',
      ToOutline('type TTest = class end', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2TypeDecl,3Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassInh,5Token (,5Reference,6Token name <<TParent>>,5Token ,,5Reference,6Token name <<IInterface>>,5Token )',
      ToOutline('type TTest = class (TParent, IInterface)', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2TypeDecl,3Token name <<TTest>>,3Token =,3ClassDecl,4ClassFwd,5Token class,4ClassBody,5ClassBody,6VarDeclaration,7NameList,8Token name <<Field>>,7Token :,7Reference,8Token name <<Integer>>,5Token end',
      ToOutline('type TTest=class Field : Integer end', True)
   );
end;

// Conditionals
//
procedure TCodeDOMTests.Conditionals;
begin
   CheckEquals(
      'Main,1Switch,2Token switch <<ifdef>>,2Token name <<a>>,2Token },1StatementList,2Reference,3Token name <<b>>,2Token ;,3Switch,4Token switch <<endif>>,4Token }',
      ToOutline('{$ifdef a}b;{$endif}', True)
   );
   CheckEquals(
      'Main,1Switch,2Token switch <<ifdef>>,2Token name <<a>>,2Token },1Switch,2Token switch <<define>>,2Token name <<a>>,2Token },1Switch,2Token switch <<endif>>,2Token }',
      ToOutline('{$ifdef a}{$define a}{$endif}', True)
   );
end;

// ArrayTypes
//
procedure TCodeDOMTests.ArrayTypes;
begin
   CheckEquals(
      'Main,1VarSection,2Token var,2VarDeclaration,3NameList,4Token name <<a>>,3Token :,3ArrayDecl,4Token array,4Token of,4Reference,5Token name <<String>>',
      ToOutline('var a : array of String', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2TypeDecl,3Token name <<t>>,3Token =,3ArrayDecl,4Token array,4ArrayRangeType,5Token [,5Reference,6Token name <<Integer>>,5Token ],4Token of,4Reference,5Token name <<String>>',
      ToOutline('type t = array[Integer]of String', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2TypeDecl,3Token name <<t>>,3Token =,3ArrayDecl,4Token array,4ArrayRangeNum,5Token [,5Range,6Token Integer Literal <<0>>,6Token ..,6Token Integer Literal <<1>>,5Token ],4Token of,4Reference,5Token name <<Byte>>',
      ToOutline('type t = array [0 .. 1] of Byte', True)
   );
end;

// CaseOf
//
procedure TCodeDOMTests.CaseOf;
begin
   CheckEquals(
      'Main,1CaseOf,2Token case,2Reference,3Token name <<a>>,2Token of,2CaseOfAlternatives,3CaseOfAlternative,'
      +'4CaseOfAlternativeCases,5Range,6UnaryOperator -,7Token -,7Token Integer Literal <<1>>,6Token ..,6UnaryOperator +,7Token +,7Token Integer Literal <<1>>,4Token :,4Reference,5Token name <<b>>,2Token end',
      ToOutline('case a of -1..+1 : b end', True)
   );
end;

// ForLoop
//
procedure TCodeDOMTests.ForLoop;
begin
   CheckEquals(
      'Main,1StatementList,2ForLoop,3Token for,3Token name <<a>>,3Token :=,3Token Integer Literal <<1>>,3Token to,3Token Integer Literal <<2>>,3Token do,2Token ;',
      ToOutline('for a := 1 to 2 do ;', True)
   );
end;

// EscapedNames
//
procedure TCodeDOMTests.EscapedNames;
begin
   CheckEquals(
      'Main,1StatementList,2VarSection,3Token var,3VarDeclaration,4Token name <<&begin>>,4Token :=,4Token Integer Literal <<1>>,2Token ;',
      ToOutline('var &begin := 1;', True)
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
