unit UTokenizerTests;

interface

uses Windows, Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsScriptSource,
   dwsTokenizer, dwsXPlatform, dwsErrors, dwsUtils, dwsPascalTokenizer, TypInfo;

type

   TTokenizerTests = class (TTestCase)
      private
         FMsgs : TdwsCompileMessageList;
         FSourceFile : TSourceFile;

      public
         procedure SetUp; override;
         procedure TearDown; override;

      published
         procedure EmptyTokenBuffer;
         procedure IgnoreDecimalSeparator;
         procedure TokenizerSpecials;
         procedure NoCurlyComments;
         procedure DollarNames;
         procedure NoBreakSpace;
         procedure EqualsTokens;
         procedure PlusMinus;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   // TTokenBufferWrapper
   //
   TTokenBufferWrapper = class
      public
         Buffer : TTokenBuffer;
   end;

// ------------------
// ------------------ TTokenizerTests ------------------
// ------------------

// SetUp
//
procedure TTokenizerTests.SetUp;
begin
   FMsgs:=TdwsCompileMessageList.Create;
   FSourceFile:=TSourceFile.Create;
end;

// TearDown
//
procedure TTokenizerTests.TearDown;
begin
   FSourceFile.Free;
   FMsgs.Free;
end;

// EmptyTokenBuffer
//
procedure TTokenizerTests.EmptyTokenBuffer;
var
   w : TTokenBufferWrapper;
   s : String;
begin
   w:=TTokenBufferWrapper.Create;
   try
      CheckEquals('', w.Buffer.ToStr, 'ToStr function');
      s:='dummy';
      w.Buffer.ToStr(s);
      CheckEquals('', s, 'ToStr procedure');
      s:='dummy';
      w.Buffer.ToUpperStr(s);
      CheckEquals('', s, 'ToUpperStr');
      CheckEquals(#0, w.Buffer.LastChar, 'LastChar');
   finally
      w.Free;
   end;
end;

// IgnoreDecimalSeparator
//
procedure TTokenizerTests.IgnoreDecimalSeparator;
var
   w : TTokenBufferWrapper;
   dc : Char;
begin
   w:=TTokenBufferWrapper.Create;
   dc:=GetDecimalSeparator;
   try
      w.Buffer.AppendChar('1');
      w.Buffer.AppendChar('.');
      w.Buffer.AppendChar('5');

      SetDecimalSeparator('.');
      CheckEquals(1.5, w.Buffer.ToFloat, 'With dot');
      SetDecimalSeparator(',');
      CheckEquals(1.5, w.Buffer.ToFloat, 'With comma');
      SetDecimalSeparator('P');
      CheckEquals(1.5, w.Buffer.ToFloat, 'With P');

   finally
      SetDecimalSeparator(dc);
      w.Free;
   end;
end;

// TokenizerSpecials
//
procedure TTokenizerTests.TokenizerSpecials;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FMsgs.Clear;
   FSourceFile.Code:='@ @= %= ^ ^= $( ? ?? ?. | || & && ~ ~=';
   rules:=TPascalTokenizerStateRules.Create;
   t:=rules.CreateTokenizer(FMsgs, nil);
   try
      t.BeginSourceFile(FSourceFile);

      CheckTrue(t.TestDelete(ttAT), '@');
      CheckTrue(t.TestDelete(ttAT_ASSIGN), '@=');
      CheckTrue(t.TestDelete(ttPERCENT_ASSIGN), '%=');
      CheckTrue(t.TestDelete(ttCARET), '^');
      CheckTrue(t.TestDelete(ttCARET_ASSIGN), '^=');
      CheckTrue(t.TestDelete(ttDOLLAR), '$');
      CheckTrue(t.TestDelete(ttBLEFT), '(');
      CheckTrue(t.TestDelete(ttQUESTION), '?');
      CheckTrue(t.TestDelete(ttQUESTION_QUESTION), '??');
      CheckTrue(t.TestDelete(ttQUESTION_DOT), '?.');
      CheckTrue(t.TestDelete(ttPIPE), '|');
      CheckTrue(t.TestDelete(ttPIPE_PIPE), '||');
      CheckTrue(t.TestDelete(ttAMP), '&');
      CheckTrue(t.TestDelete(ttAMP_AMP), '&&');
      CheckTrue(t.TestDelete(ttTILDE), '~');
      CheckTrue(t.TestDelete(ttTILDE_ASSIGN), '~=');

      CheckTrue(t.TestAny([ttNAME])=ttNone, 'Any at end');
      CheckTrue(t.TestDeleteAny([ttNAME])=ttNone, 'DeleteAny at end');

      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// NoCurlyComments
//
procedure TTokenizerTests.NoCurlyComments;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code:='{ /* { */ }';
   rules:=TPascalTokenizerStateRules.Create;
   t:=rules.CreateTokenizer(FMsgs, nil);
   try
      CheckTrue(rules.CurlyComments, 'curly comments by default');

      rules.CurlyComments:=False;
      CheckFalse(rules.CurlyComments, 'curly comments cleared');

      t.BeginSourceFile(FSourceFile);
      CheckTrue(t.TestDelete(ttCLEFT), '{');
      CheckTrue(t.TestDelete(ttCRIGHT), '}');
      t.EndSourceFile;

      rules.CurlyComments:=True;
      CheckTrue(rules.CurlyComments, 'curly comments reset');

      t.BeginSourceFile(FSourceFile);
      CheckFalse(t.HasTokens, 'skip curly comment');
      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// DollarNames
//
procedure TTokenizerTests.DollarNames;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code:='$a a$ $a$';
   rules:=TPascalTokenizerStateRules.Create;
   t:=rules.CreateTokenizer(FMsgs, nil);
   try
      CheckFalse(rules.DollarNames, 'dollar names by default');

      rules.DollarNames:=True;
      CheckTrue(rules.DollarNames, 'dollar names set');

      t.BeginSourceFile(FSourceFile);
      CheckTrue(t.TestName, '$a');
      CheckEquals(t.GetToken.AsString, UnicodeString('$a'));
      t.KillToken;

      CheckTrue(t.TestName, 'a$');
      CheckEquals(t.GetToken.AsString, UnicodeString('a$'));
      t.KillToken;

      CheckTrue(t.TestName, '$a$');
      CheckEquals(t.GetToken.AsString, UnicodeString('$a$'));
      t.KillToken;

      t.EndSourceFile;

      rules.DollarNames:=False;
      CheckFalse(rules.DollarNames, 'dollar names set');
   finally
      t.Free;
      rules.Free;
   end;
end;

// NoBreakSpace
//
procedure TTokenizerTests.NoBreakSpace;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code:='" "'#$00A0'"'#$00A0'"';
   rules:=TPascalTokenizerStateRules.Create;
   t:=rules.CreateTokenizer(FMsgs, nil);
   try
      t.BeginSourceFile(FSourceFile);

      CheckTrue(t.Test(ttStrVal), '1st string');
      CheckEquals(' ', t.GetToken.AsString, '1st string value');
      t.KillToken;

      CheckTrue(t.Test(ttStrVal), '2nd string');
      CheckEquals(#$00A0, t.GetToken.AsString, '2nd string value');
      t.KillToken;

      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// EqualsTokens
//
procedure TTokenizerTests.EqualsTokens;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code := '= == === =>';
   rules := TPascalTokenizerStateRules.Create;
   t := rules.CreateTokenizer(FMsgs, nil);
   try
      t.BeginSourceFile(FSourceFile);

      CheckTrue(t.Test(ttEQ), '=');
      t.KillToken;

      CheckTrue(t.Test(ttEQ_EQ), '==');
      t.KillToken;

      CheckTrue(t.Test(ttEQ_EQ_EQ), '===');
      t.KillToken;

      CheckTrue(t.Test(ttEQ_GTR), '=>');
      t.KillToken;

      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// PlusMinus
//
procedure TTokenizerTests.PlusMinus;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code := '+ - ++ -- += -= ** +-';
   rules := TPascalTokenizerStateRules.Create;
   t := rules.CreateTokenizer(FMsgs, nil);
   try
      t.BeginSourceFile(FSourceFile);

      CheckTrue(t.Test(ttPLUS), '+');
      t.KillToken;

      CheckTrue(t.Test(ttMINUS), '-');
      t.KillToken;

      CheckTrue(t.Test(ttPLUS_PLUS), '++');
      t.KillToken;

      CheckTrue(t.Test(ttMINUS_MINUS), '--');
      t.KillToken;

      CheckTrue(t.Test(ttPLUS_ASSIGN), '+=');
      t.KillToken;

      CheckTrue(t.Test(ttMINUS_ASSIGN), '-=');
      t.KillToken;

      CheckTrue(t.Test(ttTIMES_TIMES), '**');
      t.KillToken;

      CheckTrue(t.Test(ttPLUS), '+ in +-');
      t.KillToken;
      CheckTrue(t.Test(ttMINUS), '- in +-');
      t.KillToken;

      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('TokenizerTests', TTokenizerTests);

end.
