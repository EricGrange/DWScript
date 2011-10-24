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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsTokenizer;

{$I dws.inc}

interface

uses
  SysUtils, Classes, TypInfo, dwsErrors, dwsStrings, dwsXPlatform, dwsUtils,
  Character;

type

   TTokenType =
     (ttNone, ttStrVal, ttIntVal, ttFloatVal, ttNAME, ttSWITCH,
     ttLAZY, ttVAR, ttCONST, ttTYPE, ttRECORD, ttARRAY, ttSET, ttDOT, ttDOTDOT, ttOF,
     ttTRY, ttEXCEPT, ttRAISE, ttFINALLY, ttON, ttREAD, ttWRITE, ttPROPERTY,
     ttFUNCTION, ttPROCEDURE, ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttOPERATOR,
     ttCLASS, ttNIL, ttIS, ttAS, ttIMPLEMENTS, ttINDEX, ttOBJECT,
     ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttINHERITED, ttFINAL, ttNEW,
     ttABSTRACT, ttSEALED, ttSTATIC, ttDEPRECATED,
     ttEXTERNAL, ttFORWARD, ttIN,
     ttENSURE, ttREQUIRE, ttINVARIANTS, ttOLD,
     ttINTERFACE, ttIMPLEMENTATION,
     ttBEGIN, ttEND, ttBREAK, ttCONTINUE, ttEXIT,
     ttIF, ttTHEN, ttELSE, ttWHILE, ttREPEAT, ttUNTIL, ttFOR, ttTO, ttDOWNTO, ttDO,
     ttCASE,
     ttTRUE, ttFALSE,
     ttAND, ttOR, ttXOR, ttIMPLIES, ttDIV, ttMOD, ttNOT, ttSHL, ttSHR,
     ttPLUS, ttMINUS,
     ttTIMES, ttDIVIDE, ttPERCENT, ttCARET, ttAT, ttDOLLAR,
     ttEQ, ttNOTEQ, ttGTR, ttGTREQ, ttLESS, ttLESSEQ,
     ttLESSLESS, ttGTRGTR,
     ttSEMI, ttCOMMA, ttCOLON,
     ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN,
     ttPERCENT_ASSIGN, ttCARET_ASSIGN, ttAT_ASSIGN,
     ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCRIGHT,
     ttDEFAULT, ttUSES, ttUNIT,

     // Tokens for compatibility to Delphi
     ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
     ttREGISTER, ttPASCAL, ttCDECL, ttSTDCALL, ttFASTCALL);

   TTokenTypes = set of TTokenType;

   // TTokenBuffer
   //
   TTokenBuffer = record
      Len : Integer;
      Capacity : Integer;
      Buffer : array of Char;
      procedure AppendChar(c : Char);
      procedure Grow;
      function LastChar : Char;
      function ToStr : UnicodeString; overload; inline;
      procedure ToStr(var result : UnicodeString); overload;
      procedure AppendToStr(var result : UnicodeString);
      procedure ToUpperStr(var result : UnicodeString); overload;
      function UpperFirstChar : Char;
      function UpperMatchLen(const str : UnicodeString) : Boolean;
      function ToInt64 : Int64;
      function ToFloat : Double;
      function ToType : TTokenType;
      function ToAlphaType : TTokenType;

      class function StringToTokenType(const str : UnicodeString) : TTokenType; static;
   end;

   TToken = ^TTokenRecord;
   TTokenRecord = record
     FTyp: TTokenType;
     FPos: TScriptPos;
     FString: UnicodeString;
     FFloat: Double;
     FInteger: Int64;
   end;

   TCharsType = set of AnsiChar;
   TTransition = class;

   TState = class
     FOwnedTransitions : TTightList;
     FTransitions : array [0..127] of TTransition;
     destructor Destroy; override;
     function FindTransition(c : Char) : TTransition;
     procedure AddTransition(const chrs : TCharsType; o : TTransition);
     procedure SetElse(o : TTransition);
   end;

   TConvertAction = (caNone, caClear, caName, caHex, caInteger, caFloat, caChar,
                     caCharHex, caString, caSwitch, caDotDot);
   TTransitionOptions = set of (toStart, toFinal);

   TTransition = class
     NextState: TState;
     Start: Boolean; // Marks the begin of a Token
     Final: Boolean; // Marks the end of a Token
     Action: TConvertAction;
     constructor Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
   end;

   TElseTransition = class(TTransition)
     constructor Create(actn: TConvertAction);
   end;

   TErrorTransition = class(TTransition)
     ErrorMessage: UnicodeString;
     constructor Create(const msg: UnicodeString);
   end;

   TCheckTransition = class(TTransition);
   TSeekTransition = class(TCheckTransition); // Transition, next char
   TConsumeTransition = class(TSeekTransition);
   // Transition, consume char, next char

   TSwitchHandler = function(const SwitchName: UnicodeString): Boolean of object;

   TTokenizer = class;

   TTokenizerRules = class
      private
         FStates : TObjectList<TState>;

      protected
         function CreateState : TState;
         function StartState : TState; virtual; abstract;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         function CreateTokenizer(sourceFile : TSourceFile; msgs : TdwsCompileMessageList) : TTokenizer;
   end;

   TTokenizer = class
      private
         FTokenBuf : TTokenBuffer;
         FDefaultPos : TScriptPos;
         FHotPos : TScriptPos;
         FMsgs : TdwsCompileMessageList;
         FNextToken : TToken;
         FPos : TScriptPos;
         FPosPtr : PChar;
         FRules : TTokenizerRules;
         FStartState : TState;
         FSwitchHandler : TSwitchHandler;
         FText : UnicodeString;
         FToken : TToken;

         FTokenStore : array of TToken;
         FTokenStoreCount : Integer;

         function AllocateToken : TToken;
         procedure ReleaseToken(aToken : TToken);

         procedure HandleChar(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleInteger(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleFloat(var tokenBuf : TTokenBuffer; var result : TToken);

         function ConsumeToken : TToken;

         procedure ReadToken;
         procedure ReadNextToken;
         procedure AddCompilerStopFmtTokenBuffer(const formatString : UnicodeString);

      public
         constructor Create(rules : TTokenizerRules; sourceFile : TSourceFile; msgs : TdwsCompileMessageList);
         destructor Destroy; override;

         function GetToken : TToken; inline;
         function HasTokens : Boolean;
         procedure KillToken;

         function NextTest(t : TTokenType) : Boolean;
         function Test(t : TTokenType) : Boolean;
         function TestAny(const t : TTokenTypes) : TTokenType;
         function TestDelete(t : TTokenType) : Boolean;
         function TestDeleteAny(const t : TTokenTypes) : TTokenType;
         function TestName : Boolean;

         function TestDeleteNamePos(var aName : UnicodeString; var aPos : TScriptPos) : Boolean; inline;

         property PosPtr : PChar read FPosPtr;
         property Text : UnicodeString read FText;
         property DefaultPos : TScriptPos read FDefaultPos;
         property HotPos : TScriptPos read FHotPos;
         property CurrentPos : TScriptPos read FPos;
         property SwitchHandler : TSwitchHandler read FSwitchHandler write FSwitchHandler;
   end;

const
   cTokenStrings : array [TTokenType] of UnicodeString = (
     '', 'StrVal', 'IntVal', 'FloatVal', 'NAME', 'SWITCH',
     'LAZY', 'VAR', 'CONST', 'TYPE', 'RECORD', 'ARRAY', 'SET', '.', '..', 'OF',
     'TRY', 'EXCEPT', 'RAISE', 'FINALLY', 'ON', 'READ', 'WRITE', 'PROPERTY',
     'FUNCTION', 'PROCEDURE', 'CONSTRUCTOR', 'DESTRUCTOR', 'METHOD', 'OPERATOR',
     'CLASS', 'NIL', 'IS', 'AS', 'IMPLEMENTS', 'INDEX', 'OBJECT',
     'VIRTUAL', 'OVERRIDE', 'REINTRODUCE', 'INHERITED', 'FINAL', 'NEW',
     'ABSTRACT', 'SEALED', 'STATIC', 'DEPRECATED',
     'EXTERNAL', 'FORWARD', 'IN',
     'ENSURE', 'REQUIRE', 'INVARIANTS', 'OLD',
     'INTERFACE', 'IMPLEMENTATION',
     'BEGIN', 'END', 'BREAK', 'CONTINUE', 'EXIT',
     'IF', 'THEN', 'ELSE', 'WHILE', 'REPEAT', 'UNTIL', 'FOR', 'TO', 'DOWNTO', 'DO',
     'CASE',
     'TRUE', 'FALSE',
     'AND', 'OR', 'XOR', 'IMPLIES', 'DIV', 'MOD', 'NOT', 'SHL', 'SHR',
     '+', '-',
     '*', '/', '%', '^', '@', '$',
     '=', '<>', '>', '>=', '<', '<=',
     '<<', '>>',
     ';', ',', ':',
     ':=', '+=', '-=', '*=', '/=',
     '%=', '^=', '@=',
     '(', ')', '[', ']', '}',
     'DEFAULT', 'USES', 'UNIT',
     'PRIVATE', 'PROTECTED', 'PUBLIC', 'PUBLISHED',
     'REGISTER', 'PASCAL', 'CDECL', 'STDCALL', 'FASTCALL'
     );

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const cReservedNames : TTokenTypes = [
   ttStrVal, ttIntVal, ttFloatVal, ttDOT, ttDOTDOT,

   ttPLUS, ttMINUS,
   ttTIMES, ttDIVIDE, ttPERCENT, ttCARET, ttAT, ttDOLLAR,
   ttEQ, ttNOTEQ, ttGTR, ttGTREQ, ttLESS, ttLESSEQ,
   ttLESSLESS, ttGTRGTR,
   ttSEMI, ttCOMMA, ttCOLON,
   ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN,
   ttPERCENT_ASSIGN, ttCARET_ASSIGN, ttAT_ASSIGN,
   ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCRIGHT,

   ttAND, ttARRAY, ttAS, ttBEGIN, ttCASE, ttCLASS, ttCONST, ttCONSTRUCTOR,
   ttDESTRUCTOR, ttDIV, ttDO, ttDOWNTO, ttELSE, ttEND, ttEXCEPT, ttEXIT,
   ttFALSE, ttFINALLY, ttFOR, ttFUNCTION, ttIF, ttIMPLEMENTATION, ttIMPLIES,
   ttIN, ttINHERITED, ttINTERFACE, ttIS, ttMOD, ttNEW, ttNIL, ttNOT, ttOBJECT,
   ttOF, ttOPERATOR, ttOR, ttPROCEDURE, ttPROPERTY, ttRAISE, ttRECORD,
   ttREINTRODUCE, ttREPEAT, ttSET, ttSHL, ttSHR, ttTHEN, ttTRUE, ttTRY,
   ttTYPE, ttUNIT, ttUNTIL, ttUSES, ttVAR, ttWHILE, ttXOR
   ];

const
   cFormatSettings : TFormatSettings = ( DecimalSeparator : '.' );

// AppendChar
//
procedure TTokenBuffer.AppendChar(c : Char);
begin
   if Len>=Capacity then Grow;
   Buffer[Len]:=c;
   Inc(Len);
end;

// Grow
//
procedure TTokenBuffer.Grow;
begin
   if Capacity=0 then
      Capacity:=256
   else Capacity:=Capacity*2;
   SetLength(Buffer, Capacity);
end;

// LastChar
//
function TTokenBuffer.LastChar : Char;
begin
   if Len>0 then
      Result:=Buffer[Len-1]
   else Result:=#0;
end;

// ToStr
//
function TTokenBuffer.ToStr : UnicodeString;
begin
   ToStr(Result);
end;

// ToStr
//
procedure TTokenBuffer.ToStr(var result : UnicodeString);
begin
   if Len=0 then
      result:=''
   else begin
      SetLength(result, Len);
      Move(Buffer[0], Pointer(NativeInt(result))^, Len*SizeOf(Char));
   end;
end;

// ToStr
//
procedure TTokenBuffer.AppendToStr(var result : UnicodeString);
var
   n : Integer;
begin
   if Len>0 then begin
      n:=Length(result);
      SetLength(result, n+Len);
      Move(Buffer[0], PChar(NativeInt(result))[n], Len*SizeOf(Char));
   end;
end;

// ToUpperStr
//
procedure TTokenBuffer.ToUpperStr(var result : UnicodeString);
var
   i : Integer;
   ch : Char;
   pResult : PChar;
begin
   if Len=0 then
      result:=''
   else begin
      SetLength(result, Len);
      pResult:=PChar(result);
      for i:=0 to Len-1 do begin
         ch:=Buffer[i];
         case ch of
            'a'..'z' : pResult[i]:=Char(Word(ch) xor $0020)
         else
            pResult[i]:=ch;
         end;
      end;
   end;
end;

// UpperFirstChar
//
function TTokenBuffer.UpperFirstChar : Char;
begin
   if Len=0 then
      Result:=#0
   else begin
      Result:=Buffer[0];
      case Result of
         'a'..'z' : Result:=Char(Word(Result) xor $0020)
      end;
   end;
end;

// ToInt64
//
function TTokenBuffer.ToInt64 : Int64;

   function ComplexToInt64(var buffer : TTokenBuffer) : Int64;
   begin
      Result:=StrToInt64(ToStr);
   end;

var
   i, i2 : Integer;
begin
   case Len of
      1 : begin
         i:=Ord(Buffer[0])-Ord('0');
         if Cardinal(i)<Cardinal(10) then Exit(i);
      end;
      2 : begin
         i:=Ord(Buffer[0])-Ord('0');
         if Cardinal(i)<Cardinal(10) then begin
            i2:=Ord(Buffer[1])-Ord('0');
            if Cardinal(i2)<Cardinal(10) then
               Exit(i*10+i2);
         end;
      end;
   end;
   Result:=ComplexToInt64(Self);
end;

// ToFloat
//
function TTokenBuffer.ToFloat : Double;
var
   buf : Extended;
begin
   AppendChar(#0);
   if not TextToFloat(PChar(@Buffer[0]), buf, fvExtended, cFormatSettings) then
      raise EConvertError.Create('');
   Result:=buf;
end;

// ToType
//
function TTokenBuffer.ToType : TTokenType;
begin
   Result := ttNAME;
   if Len=0 then Exit;

   case Buffer[0] of
     '/':
       if Len=1 then
         Result := ttDIVIDE
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttDIVIDE_ASSIGN; // '/='
     '*':
       if Len=1 then
         Result := ttTIMES
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttTIMES_ASSIGN; // '*='
     '+':
       if Len=1 then
         Result := ttPLUS
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttPLUS_ASSIGN; // '+='
     '-':
       if Len=1 then
         Result := ttMINUS
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttMINUS_ASSIGN; // '-='
     '@':
       if Len=1 then
         Result := ttAT
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttAT_ASSIGN; // '@='
     '%':
       if Len=1 then
         Result := ttPERCENT
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttPERCENT_ASSIGN; // '%='
     '^':
       if Len=1 then
         Result := ttCARET
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttCARET_ASSIGN; // '^='
     ';': Result := ttSEMI;
     '(': Result := ttBLEFT;
     ')': Result := ttBRIGHT;
     '[': Result := ttALEFT;
     ']': Result := ttARIGHT;
     '=': Result := ttEQ;
     '<':
       if Len=1 then // '<'
         Result := ttLESS
       else if Len=2 then case Buffer[1] of
         '=' : Result := ttLESSEQ;     // '<='
         '>' : Result := ttNOTEQ;      // '<>'
         '<' : Result := ttLESSLESS;   // '<<'
       end;
     '>':
       if Len=1 then // '>'
         Result := ttGTR
       else if Len=2 then case Buffer[1] of
         '=' : Result := ttGTREQ;      // '>='
         '>' : Result := ttGTRGTR;     // '>>'
       end;
     ':':
       if Len=1 then // ':'
         Result := ttCOLON
       else if Len=2 then
         if Buffer[1]='=' then
            Result := ttASSIGN; // ':='
     ',': Result := ttCOMMA;
     '}': Result := ttCRIGHT;
     '.':
       if Len=1 then
         Result := ttDOT;
     '$':
       if Len=1 then
         Result := ttDOLLAR;
   else
      Result:=ToAlphaType;
   end;
end;

// ToAlphaType
//
const
   cAlphaTypeTokens : array [0..84] of TTokenType = (
      ttAND, ttARRAY, ttABSTRACT, ttAS,
      ttBEGIN, ttBREAK,
      ttCONST, ttCLASS, ttCONSTRUCTOR, ttCASE, ttCDECL, ttCONTINUE,
      ttDO, ttDOWNTO, ttDIV, ttDEFAULT, ttDESTRUCTOR, ttDEPRECATED,
      ttEND, ttENSURE, ttELSE, ttEXCEPT, ttEXIT, ttEXTERNAL,
      ttFOR, ttFALSE, ttFINAL, ttFINALLY, ttFORWARD, ttFUNCTION,
      ttIF, ttIMPLIES, ttIMPLEMENTS, ttIN, ttINVARIANTS, ttIS, ttINHERITED, ttINDEX,
      ttINTERFACE, ttIMPLEMENTATION,
      ttLAZY,
      ttMETHOD, ttMOD,
      ttNEW, ttNOT, ttNIL,
      ttOBJECT, ttOF, ttOLD, ttON, ttOPERATOR, ttOR, ttOVERRIDE,
      ttPROCEDURE, ttPROPERTY, ttPASCAL, ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
      ttREPEAT, ttREQUIRE, ttRECORD, ttREAD, ttRAISE, ttREINTRODUCE, ttREGISTER,
      ttSEALED, ttSHL, ttSHR, ttSTATIC, ttSTDCALL,
      ttTHEN, ttTO, ttTRUE, ttTRY, ttTYPE,
      ttUNIT, ttUNTIL, ttUSES,
      ttVAR, ttVIRTUAL,
      ttWHILE, ttWRITE,
      ttXOR );
type
   TTokenAlphaLookup = record
      Alpha : UnicodeString;
      Token : TTokenType;
   end;
   TTokenAlphaLookups = array of TTokenAlphaLookup;
   PTokenAlphaLookups = ^TTokenAlphaLookups;
var
   vAlphaToTokenType : array [2..14] of array ['A'..'X'] of TTokenAlphaLookups;

procedure PrepareAlphaToTokenType;
var
   i, n, len : Integer;
   tokenName : UnicodeString;
begin
   for i:=Low(cAlphaTypeTokens) to High(cAlphaTypeTokens) do begin
      tokenName:=GetEnumName(TypeInfo(TTokenType), Integer(cAlphaTypeTokens[i]));
      len:=Length(tokenName)-2;
      Assert(len<=14);
      n:=Length(vAlphaToTokenType[len][tokenName[3]]);
      SetLength(vAlphaToTokenType[len][tokenName[3]], n+1);
      with vAlphaToTokenType[len][tokenName[3]][n] do begin
         Alpha:=Copy(tokenName, 3, MaxInt);
         Token:=cAlphaTypeTokens[i];
      end;
   end;
end;

// ------------------
// ------------------ TTokenBuffer ------------------
// ------------------

// UpperMatchLen
//
function TTokenBuffer.UpperMatchLen(const str : UnicodeString) : Boolean;
var
   i : Integer;
   p : PChar;
   ch : Char;
begin
   p:=PChar(Pointer(str));
   for i:=1 to Len-1 do begin
      ch:=Buffer[i];
      case ch of
         'a'..'z' : if Char(Word(ch) xor $0020)<>p[i] then Exit(False);
      else
         if ch<>p[i] then Exit(False);
      end;
   end;
   Result:=True;
end;

// ToAlphaType
//
function TTokenBuffer.ToAlphaType : TTokenType;
var
   ch : Char;
   i : Integer;
   lookups : PTokenAlphaLookups;
begin
   if (Len<2) or (Len>14) then Exit(ttNAME);
   ch:=Buffer[0];
   case ch of
      'a'..'x' : lookups:=@vAlphaToTokenType[Len][Char(Word(ch) xor $0020)];
      'A'..'X' : lookups:=@vAlphaToTokenType[Len][ch];
   else
      Exit(ttNAME);
   end;
   for i:=0 to High(lookups^) do begin
      if UpperMatchLen(lookups^[i].Alpha) then
         Exit(lookups^[i].Token);
   end;
   Result:=ttNAME;
end;

// StringToTokenType
//
class function TTokenBuffer.StringToTokenType(const str : UnicodeString) : TTokenType;
var
   c : Char;
   buffer : TTokenBuffer;
begin
   if str='' then Exit(ttNone);

   buffer.Capacity:=0;
   buffer.Len:=0;
   for c in str do
      buffer.AppendChar(c);

   Result:=buffer.ToType;
end;

// ------------------
// ------------------ TState ------------------
// ------------------

// Destroy
//
destructor TState.Destroy;
begin
   FOwnedTransitions.Clean;
   inherited Destroy;
end;

// FindTransition
//
function TState.FindTransition(c : Char) : TTransition;
var
   oc : Integer;
begin
   oc:=Ord(c);
   if oc<127 then
      Result:=FTransitions[oc]
   else Result:=FTransitions[127];
end;

// AddTransition
//
procedure TState.AddTransition(const chrs : TCharsType; o : TTransition);
var
   c : AnsiChar;
begin
   for c:=#0 to #127 do
      if c in chrs then begin
         if FTransitions[Ord(c)]=nil then
            FTransitions[Ord(c)]:=o;
      end;
   FOwnedTransitions.Add(o);
end;

// SetElse
//
procedure TState.SetElse(o : TTransition);
var
   c : AnsiChar;
begin
   for c:=#0 to #127 do
      if FTransitions[Ord(c)]=nil then
        FTransitions[Ord(c)]:=o;
  FOwnedTransitions.Add(o);
end;

// ------------------
// ------------------ TTransition ------------------
// ------------------

// Create
//
constructor TTransition.Create;
begin
   NextState:=nstate;
   Start:=toStart in opts;
   Final:=toFinal in opts;
   Action:=actn;
end;

// ------------------
// ------------------ TElseTransition ------------------
// ------------------

// Create
//
constructor TElseTransition.Create(actn: TConvertAction);
begin
   NextState:=nil;
   Start:=False;
   Action:=actn;
end;

// ------------------
// ------------------ TErrorTransition ------------------
// ------------------

constructor TErrorTransition.Create(const msg : UnicodeString);
begin
   ErrorMessage:=msg;
end;

// ------------------
// ------------------ TTokenizer ------------------
// ------------------

// Create
//
constructor TTokenizer.Create(rules : TTokenizerRules; sourceFile : TSourceFile; msgs : TdwsCompileMessageList);
begin
   FText := sourceFile.Code + (cLineTerminator+#0);
   FMsgs := Msgs;
   FDefaultPos := cNullPos;
   FDefaultPos.SourceFile := sourceFile;
   FHotPos := FDefaultPos;
   FPos := FDefaultPos;
   FPosPtr := PChar(FText);
   FPos.Line := 1;
   FPos.Col := 1;
   FTokenBuf.Grow;
   FRules := rules;
   FStartState := FRules.StartState;

   SetLength(FTokenStore, 8);
end;

// Destroy
//
destructor TTokenizer.Destroy;
begin
   if FToken<>nil then ReleaseToken(FToken);
   if FNextToken<>nil then ReleaseToken(FNextToken);

   while FTokenStoreCount>0 do begin
      Dec(FTokenStoreCount);
      Dispose(FTokenStore[FTokenStoreCount]);
   end;

   inherited;
end;

// GetToken
//
function TTokenizer.GetToken : TToken;
begin
   Result:=FToken;
end;

// ReadToken
//
procedure TTokenizer.ReadToken;
begin
   KillToken;

   if Assigned(FNextToken) then begin
      FToken:=FNextToken;
      FNextToken:=nil;
   end else FToken:=ConsumeToken;
end;

// ReadNextToken
//
procedure TTokenizer.ReadNextToken;
begin
   if not Assigned(FNextToken) then
      FNextToken:=ConsumeToken;
end;

// AddCompilerStopFmtTokenBuffer
//
procedure TTokenizer.AddCompilerStopFmtTokenBuffer(const formatString : UnicodeString);
var
   buf : UnicodeString;
begin
   buf:=FTokenBuf.ToStr;
   FMsgs.AddCompilerStopFmt(FPos, formatString, [buf]);
end;

// Test
//
function TTokenizer.Test(t: TTokenType): Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(False);
   end;

   Result:=(FToken.FTyp=t);
   FHotPos.LineCol:=FToken.FPos.LineCol;
end;

// TestAny
//
function TTokenizer.TestAny(const t: TTokenTypes) : TTokenType;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(ttNone);
   end;

   FHotPos.LineCol:=FToken.FPos.LineCol;
   if (FToken.FTyp in t) then
      Result:=FToken.FTyp
   else Result:=ttNone;
end;

// TestDelete
//
function TTokenizer.TestDelete(t: TTokenType): Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(False);
   end;

   FHotPos.LineCol:=FToken.FPos.LineCol;
   if FToken.FTyp=t then begin
      KillToken;
      Result:=True;
   end else Result:=False;
end;

// TestDeleteAny
//
function TTokenizer.TestDeleteAny(const t: TTokenTypes) : TTokenType;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(ttNone);
   end;

   FHotPos.LineCol:=FToken.FPos.LineCol;
   if FToken.FTyp in t then begin
      Result:=FToken.FTyp;
      KillToken;
   end else Result:=ttNone;
end;

// NextTest
//
function TTokenizer.NextTest(t : TTokenType) : Boolean;
begin
   Result:=False;
   ReadNextToken;
   if Assigned(FNextToken) then
      Result:=(FNextToken.FTyp=t);
end;

// TestName
//
function TTokenizer.TestName : Boolean;
begin
   Result:=False;
   if not Assigned(FToken) then
      ReadToken;
   if Assigned(FToken) then begin
      Result:=(FToken.FString<>'') and not (FToken.FTyp in cReservedNames);
      FHotPos:=FToken.FPos;
   end;
end;

// TestDeleteNamePos
//
function TTokenizer.TestDeleteNamePos(var aName : UnicodeString; var aPos : TScriptPos) : Boolean;
begin
   if not TestName then
      Result:=False
   else begin
      aName:=GetToken.FString;
      aPos:=HotPos;
      KillToken;
      Result:=True;
   end;
end;

// HasTokens
//
function TTokenizer.HasTokens: Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if FToken<>nil then
         FHotPos.LineCol:=FToken.FPos.LineCol;
   end;
   Result:=(FToken<>nil);
end;

// HandleChar
//
procedure TTokenizer.HandleChar(var tokenBuf : TTokenBuffer; var result : TToken);
var
   tokenIntVal, n : Integer;
begin
   tokenIntVal:=FTokenBuf.ToInt64;
   if Cardinal(tokenIntVal)>Cardinal($FFFF) then
      AddCompilerStopFmtTokenBuffer(TOK_InvalidCharConstant)
   else begin
      n:=Length(result.FString)+1;
      SetLength(result.FString, n);
      result.FString[n]:=Char(tokenIntVal);
      result.FTyp:=ttStrVal;
   end;
end;

// HandleHexa
//
procedure TTokenizer.HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      result.FInteger:=tokenBuf.ToInt64;
      result.FTyp:=ttIntVal;
   except
      on e : Exception do
         AddCompilerStopFmtTokenBuffer(TOK_InvalidHexConstant);
   end;
end;

// HandleInteger
//
procedure TTokenizer.HandleInteger(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      if tokenBuf.LastChar='.' then
         Dec(tokenBuf.Len);
      result.FInteger:=tokenBuf.ToInt64;
      result.FTyp:=ttIntVal;
   except
      on e : Exception do
         AddCompilerStopFmtTokenBuffer(TOK_InvalidIntegerConstant);
   end;
end;

// HandleFloat
//
procedure TTokenizer.HandleFloat(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      result.FFloat:=tokenBuf.ToFloat;
      result.FTyp:=ttFloatVal;
   except
      on e : EConvertError do
         AddCompilerStopFmtTokenBuffer(TOK_InvalidFloatConstant);
   end;
end;

// ConsumeToken
//
function TTokenizer.ConsumeToken : TToken;
var
   state : TState;
   trns : TTransition;
   trnsClassType : TClass;
   ch : Char;
begin
   Result:=AllocateToken;

   state:=FStartState;
   FTokenBuf.Len:=0;

   try

      // Look for the next token in FText
      while Assigned(state) do begin

         // Next character
         ch:=FPosPtr^;
         if ch=#0 then Break;

         // Find next state
         trns:=state.FindTransition(ch);
         trnsClassType:=trns.ClassType;

         // Handle Errors
         if trnsClassType=TErrorTransition then
            FMsgs.AddCompilerStopFmt(FPos, '%s (found "%s")', [TErrorTransition(trns).ErrorMessage, ch]);

         // A new token begins
         if trns.Start and (Result.FPos.Line<=0) then
            Result.FPos:=FPos;

         // Add actual character to s
         if trnsClassType=TConsumeTransition then
            FTokenBuf.AppendChar(ch);

         // Proceed to the next character
         if (trnsClassType=TSeekTransition) or (trnsClassType=TConsumeTransition) then begin
            Inc(FPosPtr);
            if ch=#10 then
               FPos.NewLine
            else FPos.IncCol;
         end;

         // The characters in 's' have to be converted
         if trns.Action<>caNone then begin
            case trns.Action of
               caClear : begin
                  FTokenBuf.Len:=0;
                  Result.FPos:=DefaultPos;
               end;

               // Convert name to token
               caName : begin
                  Result.FTyp:=FTokenBuf.ToType;
                  FTokenBuf.ToStr(Result.FString);
               end;

               // converts ASCII code to character (decimal or hex)
               caChar, caCharHex :
                  HandleChar(FTokenBuf, Result);

               // Concatenates the parts of a UnicodeString constant
               caString : begin
                  FTokenBuf.AppendToStr(Result.FString);
                  Result.FTyp:=ttStrVal;
               end;

               // Converts hexadecimal number to integer
               caHex :
                  HandleHexa(FTokenBuf, Result);

               // Converts integer constants
               caInteger :
                  HandleInteger(FTokenBuf, Result);

               // Converts Floating Point numbers
               caFloat :
                  HandleFloat(FTokenBuf, Result);

               caSwitch :
                  if Assigned(FSwitchHandler) then begin
                     FHotPos:=Result.FPos;

                     // Ask parser if we should create a token or not
                     FTokenBuf.ToUpperStr(Result.FString);
                     if FSwitchHandler(Result.FString) then begin
                        Result.FTyp:=ttSWITCH;
                     end else begin
                        Result.FString:='';
                        state:=FRules.StartState;
                        FTokenBuf.Len:=0;
                        Continue;
                     end;
                  end;

               caDotDot : begin
                  Result.FPos:=FPos;
                  Result.FPos.Col:=Result.FPos.Col-1;
                  Result.FTyp:=ttDOTDOT;
               end;
            end;
            FTokenBuf.Len:=0;
         end;

         // If the token is complete then exit
         if trns.Final then begin
            FStartState:=trns.NextState;
            Exit;
         end else state:=trns.NextState;
      end;

   except
      ReleaseToken(Result);
      raise;
   end;

   // Couldn't read a token (end of FText reached)
   ReleaseToken(Result);
   Result:=nil;
end;

// KillToken
//
procedure TTokenizer.KillToken;
begin
   if FToken<>nil then begin
      ReleaseToken(FToken);
      FToken:=nil;
   end;
end;

// AllocateToken
//
function TTokenizer.AllocateToken : TToken;
begin
   if FTokenStoreCount>0 then begin
      Dec(FTokenStoreCount);
      Result:=FTokenStore[FTokenStoreCount];
   end else New(Result);
   Result.FTyp:=ttNone;
   Result.FPos:=cNullPos;
end;

// ReleaseToken
//
procedure TTokenizer.ReleaseToken(aToken : TToken);
begin
   FTokenStore[FTokenStoreCount]:=aToken;
   Inc(FTokenStoreCount);
   if aToken.FString<>'' then
      aToken.FString:='';
end;

// ------------------
// ------------------ TTokenizerRules ------------------
// ------------------

// Create
//
constructor TTokenizerRules.Create;
begin
   FStates:=TObjectList<TState>.Create;
end;

// Destroy
//
destructor TTokenizerRules.Destroy;
begin
   FStates.Free;
end;

// CreateState
//
function TTokenizerRules.CreateState : TState;
begin
   Result:=TState.Create;
   FStates.Add(Result);
end;

// CreateTokenizer
//
function TTokenizerRules.CreateTokenizer(sourceFile : TSourceFile; msgs : TdwsCompileMessageList) : TTokenizer;
begin
   Result:=TTokenizer.Create(Self, sourceFile, msgs);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   PrepareAlphaToTokenType;

end.
