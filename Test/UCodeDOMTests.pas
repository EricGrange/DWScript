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
end;

// TailComment
//
procedure TCodeDOMTests.TailComment;
begin
   CheckEquals(
      'Main,1StatementList,2Call,3Reference,4Token name <<a>>,3Token (,3Token ),2Token ;,3Token comment <<// here>> [LF]',
      ToOutline('a(); // here', True)
   );
end;

// IfThenElse
//
procedure TCodeDOMTests.IfThenElse;
begin
   CheckEquals(
      'Main,1StatementList,2IfThenElseStmt,3Reference,4Token name <<b>>,3Token then,3Call,4Reference,5Token name <<doit>>,4Token (,4Token ),3Token else,3Call,4Reference,5Token name <<dont>>,4Token (,4Token ),2Token ;',
      ToOutline('if b then doit() else dont();', True)
   );
end;

// SimpleClassDecl
//
procedure TCodeDOMTests.SimpleClassDecl;
begin
   CheckEquals(
      'Main,1StatementList,2TypeSection,3Token type,3ClassFwd,4Token name <<TTest>>,4Token =,4Token class,2Token ;',
      ToOutline('type TTest = class;', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2ClassDecl,3ClassFwd,4Token name <<TTest>>,4Token =,4Token class,3ClassBody,4Token end',
      ToOutline('type TTest = class end', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2ClassDecl,3ClassFwd,4Token name <<TTest>>,4Token =,4Token class,3ClassInh,4Token (,4Reference,5Token name <<TParent>>,4Token ,,4Reference,5Token name <<IInterface>>,4Token )',
      ToOutline('type TTest = class (TParent, IInterface)', True)
   );
   CheckEquals(
      'Main,1TypeSection,2Token type,2ClassDecl,3ClassFwd,4Token name <<TTest>>,4Token =,4Token class,3ClassBody,4ClassBody,5VarDeclaration,6NameList,7Token name <<Field>>,6Token :,6Reference,7Token name <<Integer>>,4Token end',
      ToOutline('type TTest=class Field : Integer end', True)
   );
end;

// Conditionals
//
procedure TCodeDOMTests.Conditionals;
begin
   CheckEquals(
      'Main,1Switch,2Token switch <<ifdef>>,2Token name <<a>>,2Token },1Switch,2Token switch <<define>>,2Token name <<a>>,2Token },1Switch,2Token switch <<endif>>,2Token }',
      ToOutline('{$ifdef a}{$define a}{$endif}', True)
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
