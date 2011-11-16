{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsPascalTokenizer;

{$I dws.inc}

interface

uses dwsTokenizer, dwsStrings;

type
   TPascalTokenizerStateRules = class(TTokenizerRules)
      private
         sStart, sComment, sCommentF, sSlashComment, sSlashComment0 : TState;
         sBracketLeft, sBlockCommentBracket, sBlockCommentBracket1 : TState;
         sBlockCommentSlash, sBlockCommentSlash1 : TState;
         sSwitch, sSwitchNameF, sChar0, sCharF, sCharHex, sCharHexF : TState;
         sNameF, sNameEscapedF, sNameEscapedS: TState;
         sIntF, sIntPoint, sIntPointF, sIntExp, sIntExp0, sIntExpF, sHex, sHexF: TState;
         sString0, sStringF, sAssign0: TState;
         sString1, sStringF1 : TState;
         sGreaterF, sSmallerF, sDotDot: TState;

      protected
         function StartState : TState; override;

      public
         constructor Create; override;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cOPS = ['+', '-', '*', '/', '=', '<', '>', '@', '%', '^'];
   cSPACE = [' ', #9, #13, #10, #0];
   cSPEC = ['(', ')', ',', ';', '[', ']', '}'];
   cSTOP = cSPEC + cOPS + cSPACE + [':', '.', '{'];
   cANYCHAR = [#0..#255];
   cNAM = ['A'..'Z', 'a'..'z', '_', #127];
   cINT = ['0'..'9'];
   cHEX = cINT + ['A'..'F', 'a'..'f'];
   cStart = ['''', '"', '#', ':', '$', '.'] + cNAM + cINT + cOPS;

// ------------------
// ------------------ TPascalTokenizerStateRules ------------------
// ------------------

// Create
//
constructor TPascalTokenizerStateRules.Create;
begin
   inherited;

   sStart:=CreateState;
   sComment:=CreateState;
   sCommentF:=CreateState;
   sSwitch:=CreateState;
   sSwitchNameF:=CreateState;
   sSlashComment0:=CreateState;
   sSlashComment:=CreateState;
   sBracketLeft:=CreateState;
   sBlockCommentBracket:=CreateState;
   sBlockCommentBracket1:=CreateState;
   sBlockCommentSlash:=CreateState;
   sBlockCommentSlash1:=CreateState;
   sChar0:=CreateState;
   sCharF:=CreateState;
   sCharHex:=CreateState;
   sCharHexF:=CreateState;
   sNameF:=CreateState;
   sNameEscapedS:=CreateState;
   sNameEscapedF:=CreateState;
   sIntF:=CreateState;
   sIntPoint:=CreateState;
   sIntPointF:=CreateState;
   sIntExp:=CreateState;
   sIntExp0:=CreateState;
   sIntExpF:=CreateState;
   sHex:=CreateState;
   sHexF:=CreateState;
   sString0:=CreateState;
   sStringF:=CreateState;
   sString1:=CreateState;
   sStringF1:=CreateState;
   sAssign0:=CreateState;
   sGreaterF:=CreateState;
   sSmallerF:=CreateState;
   sDotDot:=CreateState;

   sStart.AddTransition(cSPACE, TSeekTransition.Create(sStart, [], caNone));
   sStart.AddTransition(cNAM, TConsumeTransition.Create(sNameF, [toStart], caNone));
   sStart.AddTransition(['&'], TSeekTransition.Create(sNameEscapedS, [toStart], caNone));
   sStart.AddTransition(cINT, TConsumeTransition.Create(sIntF, [toStart], caNone));
   sStart.AddTransition([''''], TSeekTransition.Create(sString0, [toStart], caNone));
   sStart.AddTransition(['"'], TSeekTransition.Create(sString1, [toStart], caNone));
   sStart.AddTransition(['#'], TSeekTransition.Create(sChar0, [toStart], caNone));
   sStart.AddTransition([':', '+', '-', '*', '@', '%', '^'], TConsumeTransition.Create(sAssign0, [toStart], caNone));
   sStart.AddTransition(['='], TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
   sStart.AddTransition(cSPEC-['('], TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
   sStart.AddTransition(['('], TConsumeTransition.Create(sBracketLeft, [toStart], caNone));
   sStart.AddTransition(['/'], TConsumeTransition.Create(sSlashComment0, [toStart], caNone));
   sStart.AddTransition(['<'], TConsumeTransition.Create(sSmallerF, [toStart], caNone));
   sStart.AddTransition(['>'], TConsumeTransition.Create(sGreaterF, [toStart], caNone));
   sStart.AddTransition(['.'], TConsumeTransition.Create(sDotDot, [toStart], caNone));
   sStart.AddTransition(['{'], TSeekTransition.Create(sComment, [], caNone));
   sStart.AddTransition(['$'], TConsumeTransition.Create(sHex, [toStart], caNone));
   sStart.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sComment.AddTransition(['}'], TSeekTransition.Create(sStart, [], caClear));
   sComment.AddTransition(['$'], TSeekTransition.Create(sSwitch, [], caNone));
   sComment.SetElse(TSeekTransition.Create(sCommentF, [], caNone));

   sCommentF.AddTransition(['}'], TSeekTransition.Create(sStart, [], caClear));
   sCommentF.SetElse(TSeekTransition.Create(sCommentF, [], caNone));

   sSwitch.AddTransition(cNAM, TConsumeTransition.Create(sSwitchNameF, [toStart], caNone));
   sSwitch.SetElse(TErrorTransition.Create(TOK_NameOfSwitchExpected));

   sSwitchNameF.AddTransition(cNAM + cINT, TConsumeTransition.Create(sSwitchNameF, [], caNone));
   sSwitchNameF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caSwitch));
   sSwitchNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sSlashComment0.AddTransition(['/'], TSeekTransition.Create(sSlashComment, [], caNone));
   sSlashComment0.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSlashComment0.AddTransition(['*'], TConsumeTransition.Create(sBlockCommentSlash, [], caNone));
   sSlashComment0.SetElse(TCheckTransition.Create(sStart, [toFinal], caName));

   sSlashComment.AddTransition([#10], TSeekTransition.Create(sStart, [], caClear));
   sSlashComment.SetElse(TSeekTransition.Create(sSlashComment, [], caNone));

   sBracketLeft.AddTransition(['*'], TSeekTransition.Create(sBlockCommentBracket, [], caNone));
   sBracketLeft.SetElse(TCheckTransition.Create(sStart, [toFinal], caName));

   sBlockCommentBracket.AddTransition(['*'], TSeekTransition.Create(sBlockCommentBracket1, [], caNone));
   sBlockCommentBracket.SetElse(TSeekTransition.Create(sBlockCommentBracket, [], caNone));

   sBlockCommentBracket1.AddTransition([')'], TSeekTransition.Create(sStart, [], caClear));
   sBlockCommentBracket1.SetElse(TSeekTransition.Create(sBlockCommentBracket, [], caNone));

   sBlockCommentSlash.AddTransition(['*'], TSeekTransition.Create(sBlockCommentSlash1, [], caNone));
   sBlockCommentSlash.SetElse(TSeekTransition.Create(sBlockCommentSlash, [], caNone));

   sBlockCommentSlash1.AddTransition(['/'], TSeekTransition.Create(sStart, [], caClear));
   sBlockCommentSlash1.SetElse(TSeekTransition.Create(sBlockCommentSlash, [], caNone));

   sChar0.AddTransition(cINT, TConsumeTransition.Create(sCharF, [], caNone));
   sChar0.AddTransition(['$'], TConsumeTransition.Create(sCharHex, [], caNone));
   sChar0.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sCharF.AddTransition(cINT, TConsumeTransition.Create(sCharF, [], caNone));
   sCharF.AddTransition(['''', '"'], TCheckTransition.Create(sStart, [], caChar));
   sCharF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caChar));
   sCharF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caChar));
   sCharF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sCharHex.AddTransition(cHEX, TConsumeTransition.Create(sCharHexF, [], caNone));
   sCharHex.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sCharHexF.AddTransition(cHEX, TConsumeTransition.Create(sCharHexF, [], caNone));
   sCharHexF.AddTransition(['''', '"'], TCheckTransition.Create(sStart, [], caCharHex));
   sCharHexF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caCharHex));
   sCharHexF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caCharHex));
   sCharHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sNameF.AddTransition(cNAM + cINT, TConsumeTransition.Create(sNameF, [], caNone));
   sNameF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sNameEscapedS.AddTransition(cNAM, TConsumeTransition.Create(sNameEscapedF, [toStart], caNone));
   sNameEscapedS.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sNameEscapedF.AddTransition(cNAM+cINT, TConsumeTransition.Create(sNameEscapedF, [], caNone));
   sNameEscapedF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caNameEscaped));
   sNameEscapedF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sIntF.AddTransition(cINT, TConsumeTransition.Create(sIntF, [], caNone));
   sIntF.AddTransition(['.'], TConsumeTransition.Create(sIntPoint, [], caNone));
   sIntF.AddTransition(['E', 'e'], TConsumeTransition.Create(sIntExp, [], caNone));
   sIntF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sIntF.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

   sIntPoint.AddTransition(['.'], TCheckTransition.Create(sDotDot, [toFinal], caInteger));
   sIntPoint.AddTransition(cINT, TConsumeTransition.Create(sIntPointF, [], caNone));
   sIntPoint.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sIntPointF.AddTransition(cINT, TConsumeTransition.Create(sIntPointF, [], caNone));
   sIntPointF.AddTransition(['E', 'e'], TConsumeTransition.Create(sIntExp, [], caNone));
   sIntPointF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caFloat));
   sIntPointF.SetElse(TErrorTransition.Create(TOK_NumberExponentExpected));

   sIntExp.AddTransition(['+', '-'], TConsumeTransition.Create(sIntExp0, [], caNone));
   sIntExp.AddTransition(cINT, TConsumeTransition.Create(sIntExpF, [], caNone));
   sIntExp.SetElse(TErrorTransition.Create(TOK_NumberSignExpected));

   sIntExp0.AddTransition(cINT, TConsumeTransition.Create(sIntExpF, [], caNone));
   sIntExp0.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sIntExpF.AddTransition(cINT, TConsumeTransition.Create(sIntExpF, [], caNone));
   sIntExpF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caFloat));
   sIntExpF.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sHex.AddTransition(cHEX, TConsumeTransition.Create(sHexF, [], caNone));
   sHex.AddTransition(['('], TCheckTransition.Create(sStart, [toFinal], caName));
   sHex.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sHexF.AddTransition(cHEX, TConsumeTransition.Create(sHexF, [], caNone));
   sHexF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caHex));
   sHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sString0.AddTransition(cANYCHAR - ['''', #13, #10], TConsumeTransition.Create(sString0, [], caNone));
   sString0.AddTransition([''''], TSeekTransition.Create(sStringF, [], caNone));
   sString0.SetElse(TErrorTransition.Create(TOK_StringTerminationError));

   sStringF.AddTransition([''''], TConsumeTransition.Create(sString0, [], caNone));
   sStringF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caString));
   sStringF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caString));
   sStringF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sString1.AddTransition(cANYCHAR - ['"', #13, #10], TConsumeTransition.Create(sString1, [], caNone));
   sString1.AddTransition(['"'], TSeekTransition.Create(sStringF1, [], caNone));
   sString1.SetElse(TErrorTransition.Create(TOK_StringTerminationError));

   sStringF1.AddTransition(['"'], TConsumeTransition.Create(sString1, [], caNone));
   sStringF1.AddTransition(['#'], TCheckTransition.Create(sStart, [], caString));
   sStringF1.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caString));
   sStringF1.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sAssign0.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sAssign0.AddTransition(cStart + cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sAssign0.SetElse(TErrorTransition.Create(TOK_EqualityExpected));

   sGreaterF.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sGreaterF.AddTransition(['>'], TConsumeTransition.Create(sStart, [toFinal], caName));
   sGreaterF.AddTransition(cStart + cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sGreaterF.SetElse(TErrorTransition.Create(TOK_EqualityExpected));

   sSmallerF.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSmallerF.AddTransition(['>'], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSmallerF.AddTransition(['<'], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSmallerF.AddTransition(cStart + cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sSmallerF.SetElse(TErrorTransition.Create(TOK_GreaterEqualityExpected));

   sDotDot.AddTransition(['.'], TConsumeTransition.Create(sStart, [toFinal], caDotDot));
   sDotDot.SetElse(TCheckTransition.Create(sStart, [toFinal], caName));

   PrepareStates;
end;

// StartState
//
function TPascalTokenizerStateRules.StartState : TState;
begin
   Result:=sStart;
end;

end.
