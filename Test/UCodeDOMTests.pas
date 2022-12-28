unit UCodeDOMTests;

{$I ..\Source\dws.inc}

interface

uses
   Classes, SysUtils,
   dwsXPlatformTests, dwsXPlatform, dwsJSON, dwsUtils,
   dwsCodeDOM, dwsCodeDOMParser, dwsCodeDOMPascalParser,
   dwsTokenizer, dwsPascalTokenizer, dwsScriptSource;

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
         procedure SimpleClassDecl;
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
   finally
      sourceFile.Free;
   end;
   if compact and (Result <> '') then begin
      var list := TStringList.Create;
      try
         list.Text := Result;
         Result := Trim(list[0]);
         for var i := 1 to list.Count-1 do
            Result := Result + ',' + Trim(list[i]);
      finally
         list.Free;
      end;
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
      'Main,StatementList,VarSection,Token var,VarDeclaration,Token name <<a>>,Token :=,Token Integer Literal <<1>>,Token ;',
      ToOutline('var a := 1;', True)
   );
end;

// LiteralString
//
procedure TCodeDOMTests.LiteralString;
begin
   CheckEquals(
      'Main,StatementList,Assignment,Reference,Token name <<a>>,Token :=,Token UnicodeString Literal <<''1''>>,Token ;',
      ToOutline('a := ''1'';', True)
   );
end;

// TailComment
//
procedure TCodeDOMTests.TailComment;
begin
   CheckEquals(
      'Main,StatementList,Call,Reference,Token name <<a>>,Token (,Token ),Token ;,Token comment <<// here>> [LF]',
      ToOutline('a(); // here', True)
   );
end;

// SimpleClassDecl
//
procedure TCodeDOMTests.SimpleClassDecl;
begin
   CheckEquals(
      'Main,StatementList,TypeSection,Token type,ClassFwd,Token name <<TTest>>,Token =,Token class,Token ;',
      ToOutline('type TTest = class;', True)
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
