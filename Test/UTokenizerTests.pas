unit UTokenizerTests;

interface

uses Windows, Classes, SysUtils, dwsXPlatformTests, dwsComp, dwsScriptSource,
   dwsTokenizer, dwsTokenTypes, dwsXPlatform, dwsErrors, dwsUtils, dwsPascalTokenizer, TypInfo;

type

   TTokenizerTests = class (TTestCase)
      private
         FMsgs : TdwsCompileMessageList;
         FSourceFile : TSourceFile;
         FActions : TStringList;

      public
         procedure SetUp; override;
         procedure TearDown; override;

         procedure DoActionStream(Sender : TTokenizer; action : TConvertAction);
         procedure TokenizeToActions(tok : TTokenizer; const code : String);

      published
         procedure EmptyTokenBuffer;
         procedure IgnoreDecimalSeparator;
         procedure TokenizerSpecials;
         procedure NoCurlyComments;
         procedure DollarNames;
         procedure NoBreakSpace;
         procedure EqualsTokens;
         procedure PlusMinus;
         procedure TripleApos;
         procedure UnderscoreInNumbers;
         procedure TokenTypesToStringTest;

         procedure ActionStream;
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
   FActions := TStringList.Create;
end;

// TearDown
//
procedure TTokenizerTests.TearDown;
begin
   FSourceFile.Free;
   FMsgs.Free;
   FActions.Free;
end;

// DoActionStream
//
procedure TTokenizerTests.DoActionStream(Sender : TTokenizer; action : TConvertAction);
var
   tt : String;
begin
   var tok := Sender.GetToken;
   if action = caName then
      tt := ' (' + GetEnumName(TypeInfo(TTokenType), Ord(Sender.RawTokenBufferNameToType)) + ')';

   FActions.Add(
      GetEnumName(TypeInfo(TConvertAction), Ord(action))
         + ' at ' + Format('%d,%d', [ Sender.CurrentPos.Line, Sender.CurrentPos.Col ])
         + ' from ' + Format('%d,%d', [ tok.FScriptPos.Line, tok.FScriptPos.Col ])
         + ' on <<' + Sender.RawTokenBuffer + '>>' + tt
      );
end;

// TokenizeToActions
//
procedure TTokenizerTests.TokenizeToActions(tok : TTokenizer; const code : String);
begin
   FActions.Clear;
   FSourceFile.Code := code;

   tok.OnBeforeAction := DoActionStream;

   tok.BeginSourceFile(FSourceFile);
   while tok.HasTokens do
      tok.KillToken;
   tok.EndSourceFile;

   tok.OnBeforeAction := nil;
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
   FSourceFile.Code:='@ @= %= ^ ^= $( ? ?? ?. | || & && ~ ~= \ a&&b';
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
      CheckTrue(t.TestDelete(ttBACKSLASH), '\');

      CheckTrue(t.TestDelete(ttNAME), 'a');
      CheckTrue(t.TestDelete(ttAMP_AMP), '&&');
      CheckTrue(t.TestDelete(ttNAME), 'b');

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

// TripleApos
//
procedure TTokenizerTests.TripleApos;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code := ''''''''#13#10'hello'#13#10''''''';'
                     + ''''''''#10#9#9'world'#9#10#9''''''';'
                     + ''''''''#10#10'      space'#10#10'   '''''';'
                     ;
   rules := TPascalTokenizerStateRules.Create;
   t := rules.CreateTokenizer(FMsgs, nil);
   try
      t.BeginSourceFile(FSourceFile);

      CheckTrue(t.Test(ttStrVal), '1st string');
      CheckEquals('hello', t.GetToken.AsString, '1st string value');
      t.KillToken;

      CheckTrue(t.TestDelete(ttSEMI), '1st semi');

      CheckTrue(t.Test(ttStrVal), '2nd string');
      CheckEquals(#9'world'#9, t.GetToken.AsString, '2nd string value');
      t.KillToken;

      CheckTrue(t.TestDelete(ttSEMI), '2nd semi');

      CheckTrue(t.Test(ttStrVal), '3rd string');
      CheckEquals(#10'   space'#10, t.GetToken.AsString, '3rd string value');

      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// UnderscoreInNumbers
//
procedure TTokenizerTests.UnderscoreInNumbers;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code := '1_ 1_2 1_2_3 4_.5 4_5.6 4_5_6.7';
   rules := TPascalTokenizerStateRules.Create;
   t := rules.CreateTokenizer(FMsgs, nil);
   try
      t.BeginSourceFile(FSourceFile);

      CheckTrue(t.Test(ttIntVal), '1_');
      t.KillToken;

      CheckTrue(t.Test(ttIntVal), '1_2');
      t.KillToken;

      CheckTrue(t.Test(ttIntVal), '1_2_3');
      t.KillToken;

      CheckTrue(t.Test(ttFloatVal), '4_.5');
      t.KillToken;

      CheckTrue(t.Test(ttFloatVal), '4_5.6');
      t.KillToken;

      CheckTrue(t.Test(ttFloatVal), '4_5_6.7');
      t.KillToken;

      t.EndSourceFile;
   finally
      t.Free;
      rules.Free;
   end;
end;

// TokenTypesToStringTest
//
procedure TTokenizerTests.TokenTypesToStringTest;
var
  emptySet : TTokenTypes;
begin
  emptySet := [];
  CheckEquals('', TokenTypesToString(emptySet), 'Empty set should return empty string');

  CheckEquals('"lazy"', TokenTypesToString([ttLAZY]), 'Single word token');
  CheckEquals('"+"', TokenTypesToString([ttPLUS]), 'Single operator token');

  CheckEquals('"var" or "const"', TokenTypesToString([ttVAR, ttCONST]), 'Multiple word tokens');

  CheckEquals('Integer Literal or Float Literal or "+"', TokenTypesToString([ttIntVal, ttFloatVal, ttPLUS]), 'Mixed token types');
end;

// ActionStream
//
procedure TTokenizerTests.ActionStream;
var
   rules : TPascalTokenizerStateRules;
   t : TTokenizer;
begin
   FSourceFile.Code := 'var i:=1; // hello'#10'i += 1; (* world *) begin'#10#10'PrintLn(i);end;';
   rules := TPascalTokenizerStateRules.Create;
   t := rules.CreateTokenizer(FMsgs, nil);
   try
      TokenizeToActions(t, 'var i:=1; // hello'#10'i += 1; (* world *) begin'#10#10'PrintLn(i);end;');

      CheckEquals(
           'caName at 1,4 from 1,1 on <<var>> (ttVAR)'#13#10
         + 'caName at 1,6 from 1,5 on <<i>> (ttNAME)'#13#10
         + 'caName at 1,8 from 1,6 on <<:=>> (ttASSIGN)'#13#10
         + 'caInteger at 1,9 from 1,8 on <<1>>'#13#10
         + 'caName at 1,10 from 1,9 on <<;>> (ttSEMI)'#13#10
         + 'caClear at 2,1 from 1,11 on <</>>'#13#10
         + 'caName at 2,2 from 2,1 on <<i>> (ttNAME)'#13#10
         + 'caName at 2,5 from 2,3 on <<+=>> (ttPLUS_ASSIGN)'#13#10
         + 'caInteger at 2,7 from 2,6 on <<1>>'#13#10
         + 'caName at 2,8 from 2,7 on <<;>> (ttSEMI)'#13#10
         + 'caClear at 2,20 from 2,9 on <<(>>'#13#10
         + 'caName at 2,26 from 2,21 on <<begin>> (ttBEGIN)'#13#10
         + 'caName at 4,8 from 4,1 on <<PrintLn>> (ttNAME)'#13#10
         + 'caName at 4,9 from 4,8 on <<(>> (ttBLEFT)'#13#10
         + 'caName at 4,10 from 4,9 on <<i>> (ttNAME)'#13#10
         + 'caName at 4,11 from 4,10 on <<)>> (ttBRIGHT)'#13#10
         + 'caName at 4,12 from 4,11 on <<;>> (ttSEMI)'#13#10
         + 'caName at 4,15 from 4,12 on <<end>> (ttEND)'#13#10
         + 'caName at 4,16 from 4,15 on <<;>> (ttSEMI)'#13#10,
         FActions.Text
      );
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
