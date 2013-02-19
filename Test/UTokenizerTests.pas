unit UTokenizerTests;

interface

uses Windows, Classes, SysUtils, dwsXPlatformTests, dwsComp,
   dwsTokenizer, dwsXPlatform, dwsErrors, dwsUtils, dwsPascalTokenizer;

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
   FSourceFile.Code:='@ @= %= ^ ^= $( ? | || & &&';
   rules:=TPascalTokenizerStateRules.Create;
   t:=rules.CreateTokenizer(FMsgs);
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
      CheckTrue(t.TestDelete(ttPIPE), '|');
      CheckTrue(t.TestDelete(ttPIPEPIPE), '||');
      CheckTrue(t.TestDelete(ttAMP), '&');
      CheckTrue(t.TestDelete(ttAMPAMP), '&&');

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
   t:=rules.CreateTokenizer(FMsgs);
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterTest('TokenizerTests', TTokenizerTests);

end.
