{**********************************************************************}
{                                                                      }
{    The contents of this file are subject to the GNU General          }
{    Public License version 3 (the "License") as published by          }
{    the Free Software Foundation. You may obtain a copy of            }
{    the License at https://www.gnu.org/licenses/gpl-3.0.txt           }
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
unit dwsWebIDLTokenizer;

{$I dws.inc}

interface

uses dwsTokenizer, dwsStrings;

type
   TWebIdlTokenizerStateRules = class(TTokenizerRules)
      private
         sStart, sSlashComment, sSlashComment0 : TState;
         sNameF: TState;
         sHexS, sHexF, sIntS, sIntF: TState;
         sStringDouble, sStringDoubleF : TState;
         sBlockCommentSlash, sBlockCommentSlash1 : TState;
         sIntPoint, sIntPointF, sIntExp, sIntExp0, sIntExpF : TState;
         sStringIndentDouble, sStringIndentDoubleF : TState;
      protected
         function StartState : TState; override;

      public
         constructor Create; override;

   end;

const
   cWebIdlSymbolTokens : TTokenTypes = [
      ttStrVal, ttIntVal, ttFloatVal,
      ttDOT, ttEQ, ttSEMI, ttCOMMA, ttCOLON, ttBLEFT, ttBRIGHT, ttALEFT,
      ttARIGHT, ttCRIGHT, ttCLEFT
   ];

   cWebIdlReservedNames : TTokenTypes = [
      ttCONST, ttFALSE, ttTRUE
   ];

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cOPS = ['='];
   cSPACE = [' ', #9, #13, #10, #0];
   cSPEC = ['(', ')', ',', '.', ':', ';', '[', ']', '<', '>', '{', '}', '?'];
   cSTOP = cSPEC + cOPS + cSPACE + [':', '.'];
   cANYCHAR = [#0..#255];
   cNAM = ['A'..'Z', 'a'..'z', '_', #127];
   cZERO = ['0'];
   cDIGIT = ['1'..'9'];
   cINT = cZERO + cDIGIT;
   cHEX = cINT + ['A'..'F', 'a'..'f', '_'];
   cBIN = ['0', '1', '_'];

// ------------------
// ------------------ TWebIdlTokenizerStateRules ------------------
// ------------------

// Create
//
constructor TWebIdlTokenizerStateRules.Create;
begin
   inherited;

   CaseSensitive:=True;

   SymbolTokens:=cWebIdlSymbolTokens;
   ReservedNames:=cWebIdlReservedNames;

   sStart:=CreateState;
   sSlashComment0:=CreateState;
   sSlashComment:=CreateState;
   sStringDouble:=CreateState;
   sStringDoubleF:=CreateState;
   sHexS:=CreateState;
   sHexF:=CreateState;
   sIntS:=CreateState;
   sIntF:=CreateState;
   sBlockCommentSlash:=CreateState;
   sBlockCommentSlash1:=CreateState;
   sNameF:=CreateState;
   sIntPoint:=CreateState;
   sIntPointF:=CreateState;
   sIntExp:=CreateState;
   sIntExp0:=CreateState;
   sIntExpF:=CreateState;
   sStringIndentDouble:=CreateState;
   sStringIndentDoubleF:=CreateState;

   sStart.AddTransition(cSPACE, TSeekTransition.Create(sStart, [], caNone));
   sStart.AddTransition(cNAM, TConsumeTransition.Create(sNameF, [toStart], caNone));
   sStart.AddTransition(cZERO, TConsumeTransition.Create(sHexS, [toStart], caNone));
   sStart.AddTransition(cDIGIT, TConsumeTransition.Create(sIntS, [toStart], caNone));
   sStart.AddTransition(['-', '+'], TConsumeTransition.Create(sIntS, [toStart], caNone));
   sStart.AddTransition(['"'], TSeekTransition.Create(sStringDouble, [toStart], caNone));
   sStart.AddTransition(['/'], TConsumeTransition.Create(sSlashComment0, [toStart], caNone));
   sStart.AddTransition(cSPEC + cOPS, TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
   sStart.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sSlashComment0.AddTransition(['/'], TSeekTransition.Create(sSlashComment, [], caNone));
   sSlashComment0.AddTransition(['*'], TConsumeTransition.Create(sBlockCommentSlash, [], caNone));
   sSlashComment0.SetElse(TCheckTransition.Create(sStart, [toFinal], caName));

   sBlockCommentSlash.AddTransition(['*'], TSeekTransition.Create(sBlockCommentSlash1, [], caNone));
   sBlockCommentSlash.SetElse(TSeekTransition.Create(sBlockCommentSlash, [], caNone));

   sBlockCommentSlash1.AddTransition(['/'], TSeekTransition.Create(sStart, [], caClear));
   sBlockCommentSlash1.AddTransition(['*'], TSeekTransition.Create(sBlockCommentSlash1, [], caNone));
   sBlockCommentSlash1.SetElse(TSeekTransition.Create(sBlockCommentSlash, [], caNone));

   sSlashComment.AddTransition([#10], TSeekTransition.Create(sStart, [], caClear));
   sSlashComment.SetElse(TSeekTransition.Create(sSlashComment, [], caNone));

   sNameF.AddTransition(cNAM + cINT, TConsumeTransition.Create(sNameF, [], caNone));
   sNameF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sHexS.AddTransition(cINT, TConsumeTransition.Create(sIntF, [], caNone));
   sHexS.AddTransition(['x'], TConsumeTransition.Create(sHexF, [], caNone));
   sHexS.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sHexS.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sHexF.AddTransition(cHEX, TConsumeTransition.Create(sHexF, [], caNone));
   sHexF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caHex));
   sHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sIntS.AddTransition(cINT, TConsumeTransition.Create(sIntF, [], caNone));
   sIntS.AddTransition(['.'], TConsumeTransition.Create(sIntPoint, [], caNone));
   sIntS.AddTransition(['E', 'e'], TConsumeTransition.Create(sIntExp, [], caNone));
   sIntS.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sIntS.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

   sIntF.AddTransition(cINT, TConsumeTransition.Create(sIntF, [], caNone));
   sIntF.AddTransition(['.'], TConsumeTransition.Create(sIntPoint, [], caNone));
   sIntF.AddTransition(['E', 'e'], TConsumeTransition.Create(sIntExp, [], caNone));
   sIntF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sIntF.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

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

   sStringDouble.AddTransition(cANYCHAR - ['"', #0], TConsumeTransition.Create(sStringDouble, [], caNone));
   sStringDouble.AddTransition(['"'], TSeekTransition.Create(sStringDoubleF, [], caNone));
   sStringDouble.AddTransition([#0], TErrorTransition.Create(TOK_HereDocTerminationError));

   sStringDoubleF.AddTransition(['"'], TConsumeTransition.Create(sStringDouble, [], caNone));
   sStringDoubleF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caString));
   sStringDoubleF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caString));
   sStringDoubleF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sStringIndentDouble.AddTransition(cANYCHAR - ['"', #0], TConsumeTransition.Create(sStringIndentDouble, [], caNone));
   sStringIndentDouble.AddTransition(['"'], TSeekTransition.Create(sStringIndentDoubleF, [], caNone));
   sStringIndentDouble.AddTransition([#0], TErrorTransition.Create(TOK_HereDocTerminationError));

   sStringIndentDoubleF.AddTransition(['"'], TConsumeTransition.Create(sStringIndentDouble, [], caNone));
   sStringIndentDoubleF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caMultiLineString));
   sStringIndentDoubleF.AddTransition(cSTOP, TCheckTransition.Create(sStart, [toFinal], caMultiLineString));
   sStringIndentDoubleF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   PrepareStates;
end;

// StartState
//
function TWebIdlTokenizerStateRules.StartState : TState;
begin
   Result := sStart;
end;

end.
