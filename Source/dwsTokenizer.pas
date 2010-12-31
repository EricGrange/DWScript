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
{$I dws.inc}
unit dwsTokenizer;

interface

uses
  SysUtils, Classes, TypInfo, dwsErrors, dwsStrings, dwsXPlatform, dwsUtils;

type

   TTokenType =
     (ttNone, ttStrVal, ttIntVal, ttFloatVal, ttNAME, ttSWITCH,
     ttLAZY, ttVAR, ttCONST, ttTYPE, ttRECORD, ttARRAY, ttDOT, ttDOTDOT, ttOF,
     ttTRY, ttEXCEPT, ttRAISE, ttFINALLY, ttON, ttREAD, ttWRITE, ttPROPERTY,
     ttPROCEDURE, ttFUNCTION, ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttOPERATOR,
     ttCLASS, ttNIL, ttIS,
     ttAS, ttINDEX, ttOBJECT,
     ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttINHERITED, ttABSTRACT, ttDEPRECATED,
     ttEXTERNAL, ttFORWARD, ttIN,
     ttBEGIN, ttEND, ttBREAK, ttCONTINUE, ttEXIT,
     ttIF, ttTHEN, ttELSE, ttWHILE, ttREPEAT, ttUNTIL, ttFOR, ttTO, ttDOWNTO, ttDO,
     ttCASE,
     ttTRUE, ttFALSE,
     ttAND, ttOR, ttXOR, ttDIV, ttMOD, ttNOT, ttSHL, ttSHR,
     ttPLUS, ttMINUS,
     ttTIMES, ttDIVIDE, ttPERCENT, ttCARET, ttAT,
     ttEQ, ttNOTEQ, ttGTR, ttGTREQ, ttLESS, ttLESSEQ,
     ttSEMI, ttCOMMA, ttCOLON,
     ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN,
     ttPERCENT_ASSIGN, ttCARET_ASSIGN, ttAT_ASSIGN,
     ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCRIGHT,
     ttDEFAULT, ttUSES,

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
      function ToStr : String; overload;
      procedure ToStr(var result : String); overload;
      procedure AppendToStr(var result : String);
      procedure ToUpperStr(var result : String); overload;
      function UpperFirstChar : Char;
      function UpperMatchLen(const str : String) : Boolean;
      function ToInt64 : Int64;
      function ToInt32Def(aDef : Integer) : Integer;
      function ToFloat : Double;
      function ToType : TTokenType;
      function ToAlphaType : TTokenType;
   end;

   TToken = ^TTokenRecord;
   TTokenRecord = record
     FTyp: TTokenType;
     FPos: TScriptPos;
     FString: string;
     FFloat: Double;
     FInteger: Int64;
   end;

   TCharsType = set of AnsiChar;
   TTransition = class;

   TState = class
     FOwnedTransitions : TTightList;
     FTransitions : array [0..127] of TTransition;
     destructor Destroy; override;
     function FindTransition(c: char): TTransition;
     procedure AddTransition(const chrs: TCharsType; o: TTransition);
     procedure SetElse(o: TTransition);
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
     ErrorMessage: string;
     constructor Create(const msg: string);
   end;

   TCheckTransition = class(TTransition);
   TSeekTransition = class(TCheckTransition); // Transition, next char
   TConsumeTransition = class(TSeekTransition);
   // Transition, consume char, next char

   TSwitchHandler = function(const SwitchName: string): Boolean of object;

   TTokenizer = class
   private
     FTokenBuf : TTokenBuffer;
     FDefaultPos: TScriptPos;
     FHotPos: TScriptPos;
     FMsgs: TdwsMessageList;
     FNextToken: TToken;
     FPos: TScriptPos;
     FPosPtr : PChar;
     FStartState: TState;
     FSwitchHandler: TSwitchHandler;
     FText: string;
     FToken: TToken;

     FTokenStore : array of TToken;
     FTokenStoreCount : Integer;

     function AllocateToken : TToken;
     procedure ReleaseToken(aToken : TToken);

     function ConsumeToken: TToken;
     procedure ReadToken;
     procedure ReadNextToken;
     procedure AddCompilerStopFmtTokenBuffer(const formatString : String);

   public
     constructor Create(const Text, SourceFile: string; Msgs: TdwsMessageList);
     destructor Destroy; override;
     function GetToken: TToken; inline;
     function HasTokens: Boolean;
     procedure KillToken;
     function NextTest(t: TTokenType): Boolean;
     function Test(t: TTokenType): Boolean;
     function TestAny(const t: TTokenTypes) : TTokenType;
     function TestDelete(t: TTokenType): Boolean;
     function TestDeleteAny(const t: TTokenTypes) : TTokenType;
     function NextTestName: Boolean;
     function TestName: Boolean;
     function TestDeleteName: Boolean;

     property PosPtr : PChar read FPosPtr;
     property Text : string read FText;
     property DefaultPos: TScriptPos read FDefaultPos;
     property HotPos: TScriptPos read FHotPos;
     property CurrentPos: TScriptPos read FPos;
     property SwitchHandler: TSwitchHandler read FSwitchHandler write FSwitchHandler;
   end;

const
   cTokenStrings : array [TTokenType] of String = (
     '', 'StrVal', 'IntVal', 'FloatVal', 'NAME', 'SWITCH',
     'LAZY', 'VAR', 'CONST', 'TYPE', 'RECORD', 'ARRAY', '.', '..', 'OF',
     'TRY', 'EXCEPT', 'RAISE', 'FINALLY', 'ON', 'READ', 'WRITE', 'PROPERTY',
     'PROCEDURE', 'FUNCTION', 'CONSTRUCTOR', 'DESTRUCTOR', 'METHOD', 'OPERATOR',
     'CLASS', 'NIL', 'IS',
     'AS', 'INDEX', 'OBJECT',
     'VIRTUAL', 'OVERRIDE', 'REINTRODUCE', 'INHERITED', 'ABSTRACT', 'DEPRECATED',
     'EXTERNAL', 'FORWARD', 'IN',
     'BEGIN', 'END', 'BREAK', 'CONTINUE', 'EXIT',
     'IF', 'THEN', 'ELSE', 'WHILE', 'REPEAT', 'UNTIL', 'FOR', 'TO', 'DOWNTO', 'DO',
     'CASE',
     'TRUE', 'FALSE',
     'AND', 'OR', 'XOR', 'DIV', 'MOD', 'NOT', 'SHL', 'SHR',
     '+', '-',
     '*', '/', '%', '^', '@',
     '=', '<>', '>', '>=', '<', '<=',
     ';', ',', ':',
     ':=', '+=', '-=', '*=', '/=',
     '%=', '^=', '@=',
     '(', ')', '[', ']', '}',
     'DEFAULT', 'USES',
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
   ttStrVal, ttSWITCH, ttSEMI, ttDIVIDE, ttTIMES, ttPLUS, ttMINUS, ttAT, ttSEMI,
   ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttEQ, ttLESS, ttLESSEQ, ttNOTEQ, ttGTR,
   ttGTREQ, ttCOLON,
   ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN,
   ttCOMMA, ttCRIGHT, ttDOT ];

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
function TTokenBuffer.ToStr : String;
begin
   if Len=0 then
      Result:=''
   else SetString(Result, PChar(@Buffer[0]), Len);
end;

// ToStr
//
procedure TTokenBuffer.ToStr(var result : String);
begin
   if Len=0 then
      result:=''
   else SetString(result, PChar(@Buffer[0]), Len);
end;

// ToStr
//
procedure TTokenBuffer.AppendToStr(var result : String);
var
//   n : Integer;
   s : String;
begin
   if Len>0 then begin
      ToStr(s);
      result:=result+s;
   end;
end;

// ToUpperStr
//
procedure TTokenBuffer.ToUpperStr(var result : String);
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

// ToInt32Def
//
function TTokenBuffer.ToInt32Def(aDef : Integer) : Integer;
begin
   Result:=StrToIntDef(ToStr, aDef);
end;

// ToFloat
//
function TTokenBuffer.ToFloat : Double;
begin
   Result:=StrToFloat(ToStr, cFormatSettings);
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
       else if Len=2 then
         if Buffer[1] = '=' then // '<='
            Result := ttLESSEQ
         else if Buffer[1] = '>' then // '<>'
            Result := ttNOTEQ;
     '>':
       if Len=1 then // '>'
         Result := ttGTR
       else if Len=2 then
         if Buffer[1] = '=' then // '>='
            Result := ttGTREQ;
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
   else
      Result:=ToAlphaType;
   end;
end;

// ToAlphaType
//
const
   cAlphaTypeTokens : array [0..71] of TTokenType = (
      ttAND, ttARRAY, ttABSTRACT, ttAS,
      ttBEGIN, ttBREAK,
      ttCONST, ttCLASS, ttCONSTRUCTOR, ttCASE, ttCDECL, ttCONTINUE,
      ttDO, ttDOWNTO, ttDIV, ttDEFAULT, ttDESTRUCTOR, ttDEPRECATED,
      ttEND, ttELSE, ttEXCEPT, ttEXIT, ttEXTERNAL,
      ttFOR, ttFALSE, ttFUNCTION, ttFINALLY, ttFORWARD,
      ttIF, ttIN, ttIS, ttINHERITED, ttINDEX,
      ttLAZY,
      ttMETHOD, ttMOD,
      ttNOT, ttNIL,
      ttOBJECT, ttOF, ttON, ttOPERATOR, ttOR, ttOVERRIDE,
      ttPROCEDURE, ttPROPERTY, ttPASCAL, ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
      ttREPEAT, ttRECORD, ttREAD, ttRAISE, ttREINTRODUCE, ttREGISTER,
      ttSHL, ttSHR, ttSTDCALL,
      ttTHEN, ttTO, ttTRUE, ttTRY, ttTYPE,
      ttUNTIL, ttUSES,
      ttVAR, ttVIRTUAL,
      ttWHILE, ttWRITE,
      ttXOR );
type
   TTokenAlphaLookup = record
      Alpha : String;
      Token : TTokenType;
   end;
   TTokenAlphaLookups = array of TTokenAlphaLookup;
   PTokenAlphaLookups = ^TTokenAlphaLookups;
var
   vAlphaToTokenType : array [2..11] of array ['A'..'X'] of TTokenAlphaLookups;

procedure PrepareAlphaToTokenType;
var
   i, n, len : Integer;
   tokenName : String;
begin
   for i:=Low(cAlphaTypeTokens) to High(cAlphaTypeTokens) do begin
      tokenName:=GetEnumName(TypeInfo(TTokenType), Integer(cAlphaTypeTokens[i]));
      len:=Length(tokenName)-2;
      Assert(len<=11);
      n:=Length(vAlphaToTokenType[len][tokenName[3]]);
      SetLength(vAlphaToTokenType[len][tokenName[3]], n+1);
      with vAlphaToTokenType[len][tokenName[3]][n] do begin
         Alpha:=Copy(tokenName, 3, MaxInt);
         Token:=cAlphaTypeTokens[i];
      end;
   end;
end;

// UpperMatchLen
//
function TTokenBuffer.UpperMatchLen(const str : String) : Boolean;
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
   if (Len<2) or (Len>11) then Exit(ttNAME);
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

{ TState }

destructor TState.Destroy;
begin
   FOwnedTransitions.Clean;
   inherited Destroy;
end;

function TState.FindTransition(c : Char): TTransition;
var
   oc : Integer;
begin
   oc:=Ord(c);
   if oc<128 then
      Result:=FTransitions[oc]
   else Result:=FTransitions[127];
end;

procedure TState.AddTransition(const chrs: TCharsType; o: TTransition);
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

procedure TState.SetElse(o: TTransition);
var
   c : AnsiChar;
begin
   for c:=#0 to #127 do
      if FTransitions[Ord(c)]=nil then
        FTransitions[Ord(c)]:=o;
  FOwnedTransitions.Add(o);
end;

{ TTransition }

constructor TTransition.Create;
begin
  NextState := nstate;
  Start := toStart in opts;
  Final := toFinal in opts;
  Action := actn;
end;

{ TElseTransition }

constructor TElseTransition.Create(actn: TConvertAction);
begin
  NextState := nil;
  Start := false;
  Action := actn;
end;

{ TErrorTransition }

constructor TErrorTransition.Create(const msg: string);
begin
  ErrorMessage := msg;
end;

var
  sStart, sSpace, sComment, sCommentF, sSlashComment, sSlashComment0: TState;
  sSwitch, sSwitchNameF, sChar0, sCharF, sCharHex, sCharHexF: TState;
  sNameF: TState;
  sIntF, sIntPoint, sIntPointF, sIntExp, sIntExp0, sIntExpF, sHex, sHexF: TState;
  sString0, sStringF, sAssign0: TState;
  sGreaterF, sSmallerF, sDotDot: TState;

{ TTokenizer }

constructor TTokenizer.Create(const Text, SourceFile: string; Msgs: TdwsMessageList);
begin
   FText := Text + (cLineTerminator+#0);
   FToken := nil;
   FMsgs := Msgs;
   FNextToken := nil;
   FDefaultPos := cNullPos;
   FDefaultPos.SourceFile := FMsgs.RegisterSourceFile(SourceFile, Text);
   FHotPos := FDefaultPos;
   FPos := FDefaultPos;
   FPosPtr := PChar(FText);
   FPos.Line := 1;
   FPos.Col := 1;
   FStartState := sStart;
   FTokenBuf.Grow;

   SetLength(FTokenStore, 8);
end;

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

procedure TTokenizer.ReadToken;
begin
  KillToken;

  if Assigned(FNextToken) then
  begin
    FToken := FNextToken;
    FNextToken := nil;
  end
  else
    FToken := ConsumeToken;
end;

procedure TTokenizer.ReadNextToken;
begin
  if not Assigned(FNextToken) then
    FNextToken := ConsumeToken;
end;

// AddCompilerStopFmtTokenBuffer
//
procedure TTokenizer.AddCompilerStopFmtTokenBuffer(const formatString : String);
var
   buf : String;
begin
   buf:=FTokenBuf.ToStr;
   FMsgs.AddCompilerStopFmt(FPos, formatString, [buf]);
end;

function TTokenizer.GetToken: TToken;
begin
  Result := FToken;
end;

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

function TTokenizer.NextTest(t: TTokenType): Boolean;
begin
  Result := false;
  ReadNextToken;
  if Assigned(FNextToken) then
    Result := (FNextToken.FTyp = t);
end;

function TTokenizer.TestName: Boolean;
begin
  Result := false;
  if not Assigned(FToken) then
    ReadToken;
  if Assigned(FToken) then
  begin
    Result := (FToken.FString <> '') and not (FToken.FTyp in cReservedNames);
    FHotPos := FToken.FPos;
  end;
end;

function TTokenizer.TestDeleteName: Boolean;
begin
  Result := TestName;
  if Result then
    KillToken;
end;

function TTokenizer.NextTestName: Boolean;
begin
  Result := false;
  ReadNextToken;
  if Assigned(FNextToken) then
    Result := (FNextToken.FString <> '') and not (FToken.FTyp in cReservedNames);
end;

function TTokenizer.HasTokens: Boolean;
begin
  if not Assigned(FToken) then
    ReadToken;
  Result := FToken <> nil;
end;

function TTokenizer.ConsumeToken: TToken;

   procedure HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
   begin
      try
         result.FInteger := tokenBuf.ToInt64;
         result.FTyp := ttIntVal;
      except
         on e: Exception do
            AddCompilerStopFmtTokenBuffer(TOK_InvalidHexConstant);
      end;
   end;

   procedure HandleInteger(var tokenBuf : TTokenBuffer; var result : TToken);
   begin
      try
         if tokenBuf.LastChar='.' then
            Dec(tokenBuf.Len);
         result.FInteger := tokenBuf.ToInt64;
         result.FTyp := ttIntVal;
      except
         on e: Exception do
            AddCompilerStopFmtTokenBuffer(TOK_InvalidIntegerConstant);
      end;
   end;

   procedure HandleFloat(var tokenBuf : TTokenBuffer; var result : TToken);
   begin
      try
         result.FFloat := tokenBuf.ToFloat;
         result.FTyp := ttFloatVal;
      except
         on e: Exception do
            AddCompilerStopFmtTokenBuffer(TOK_InvalidFloatConstant);
      end;
   end;

var
   state : TState;
   trns : TTransition;
   trnsClassType : TClass;
   tokenIntVal : Integer;
   n : Integer;
   ch : char;
begin
   Result:=AllocateToken;

   Result.FTyp:=ttNone;
   Result.FPos:=cNullPos;
   state := FStartState;
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
            FMsgs.AddCompilerStopFmt(FPos, '%s ("%s")', [TErrorTransition(trns).ErrorMessage, ch]);

         // A new token begins
         if trns.Start and (Result.FPos.Line<=0) then
            Result.FPos:=FPos;

         // Add actual character to s
         if trnsClassType=TConsumeTransition then begin
            FTokenBuf.AppendChar(ch);
         end;

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
               caClear: begin
                  FTokenBuf.Len:=0;
                  Result.FPos:=DefaultPos;
               end;

               // Convert name to token
               caName: begin
                  Result.FTyp:=FTokenBuf.ToType;
                  FTokenBuf.ToStr(Result.FString);
               end;

               // converts ASCII code to character (decimal or hex)
               caChar, caCharHex: begin
                  tokenIntVal:=FTokenBuf.ToInt32Def(-1);
                  if Cardinal(tokenIntVal)>Cardinal($FFFF) then
                     AddCompilerStopFmtTokenBuffer(TOK_InvalidCharConstant)
                  else begin
                     n:=Length(Result.FString)+1;
                     SetLength(Result.FString, n);
                     Result.FString[n]:= Char(tokenIntVal);
                     Result.FTyp:=ttStrVal;
                  end;
               end;

               // Concatenates the parts of a string constant
               caString: begin
                  FTokenBuf.AppendToStr(Result.FString);
                  Result.FTyp:=ttStrVal;
               end;

               // Converts hexadecimal number to integer
               caHex:
                  HandleHexa(FTokenBuf, Result);

               // Converts integer constants
               caInteger :
                  HandleInteger(FTokenBuf, Result);

               // Converts Floating Point numbers
               caFloat:
                  HandleFloat(FTokenBuf, Result);

               caSwitch:
                  if Assigned(FSwitchHandler) then begin
                     FHotPos := Result.FPos;

                     // Ask parser if we should create a token or not
                     FTokenBuf.ToUpperStr(Result.FString);
                     if FSwitchHandler(Result.FString) then begin
                        Result.FTyp := ttSWITCH;
                     end else begin
                        Result.FString :='';
                        state := sStart;
                        FTokenBuf.Len:=0;
                        Continue;
                     end;
                  end;

               caDotDot: begin
                  Result.FPos:=FPos;
                  Result.FPos.Col:=Result.FPos.Col-1;
                  Result.FTyp:=ttDOTDOT;
               end;
            end;
            FTokenBuf.Len:=0;
         end;

         // If the token is complete then exit
         if trns.Final then begin
            FStartState := trns.NextState;
            Exit;
         end else state := trns.NextState;
      end;
   except
      ReleaseToken(Result);
      raise;
   end;

   // Couldn't read a token (end of FText reached)
   ReleaseToken(Result);
   Result := nil;
end;

const
  OPS = ['+', '-', '*', '/', '=', '<', '>', '@', '%', '^'];
  SPACE = [' ', #9, #13, #10, #0];
  SPEC = ['(', ')', ',', ';', '[', ']', '}'];
  STOP = SPEC + OPS + SPACE + [':', '.', '{'];
  ANYCHAR = [#0..#255];
  NAM = ['A'..'Z', 'a'..'z', '_'];
  INT = ['0'..'9'];
  HEX = INT + ['A'..'F', 'a'..'f'];
  Start = ['''', '#', ':', '$', '.'] + NAM + INT + OPS;

procedure TTokenizer.KillToken;
begin
   if FToken<>nil then begin
      ReleaseToken(FToken);
      FToken := nil;
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

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   PrepareAlphaToTokenType;

   sStart := TState.Create;
   sComment := TState.Create;
   sCommentF := TState.Create;
   sSwitch := TState.Create;
   sSwitchNameF := TState.Create;
   sSlashComment0 := TState.Create;
   sSlashComment := TState.Create;
   sChar0 := TState.Create;
   sCharF := TState.Create;
   sCharHex := TState.Create;
   sCharHexF := TState.Create;
   sNameF := TState.Create;
   sIntF := TState.Create;
   sIntPoint := TState.Create;
   sIntPointF := TState.Create;
   sIntExp := TState.Create;
   sIntExp0 := TState.Create;
   sIntExpF := TState.Create;
   sHex := TState.Create;
   sHexF := TState.Create;
   sString0 := TState.Create;
   sStringF := TState.Create;
   sAssign0 := TState.Create;
   sGreaterF := TState.Create;
   sSmallerF := TState.Create;
   sDotDot := TState.Create;

   sStart.AddTransition(SPACE, TSeekTransition.Create(sStart, [], caNone));
   sStart.AddTransition(NAM, TConsumeTransition.Create(sNameF, [toStart], caNone));
   sStart.AddTransition(INT, TConsumeTransition.Create(sIntF, [toStart], caNone));
   sStart.AddTransition([''''], TSeekTransition.Create(sString0, [toStart], caNone));
   sStart.AddTransition(['#'], TSeekTransition.Create(sChar0, [toStart], caNone));
   sStart.AddTransition([':', '+', '-', '*', '@', '%', '^'], TConsumeTransition.Create(sAssign0, [toStart], caNone));
   sStart.AddTransition(['='], TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
   sStart.AddTransition(SPEC, TConsumeTransition.Create(sStart, [toStart, toFinal], caName));
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

   sSwitch.AddTransition(NAM, TConsumeTransition.Create(sSwitchNameF, [toStart], caNone));
   sSwitch.SetElse(TErrorTransition.Create(TOK_NameOfSwitchExpected));

   sSwitchNameF.AddTransition(NAM + INT, TConsumeTransition.Create(sSwitchNameF, [], caNone));
   sSwitchNameF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caSwitch));
   sSwitchNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sSlashComment0.AddTransition(['/'], TSeekTransition.Create(sSlashComment, [], caNone));
   sSlashComment0.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSlashComment0.SetElse(TCheckTransition.Create(sStart, [toFinal], caName));

   sSlashComment.AddTransition([#0, #10], TSeekTransition.Create(sStart, [], caClear));
   sSlashComment.SetElse(TSeekTransition.Create(sSlashComment, [], caNone));

   sChar0.AddTransition(INT, TConsumeTransition.Create(sCharF, [], caNone));
   sChar0.AddTransition(['$'], TConsumeTransition.Create(sCharHex, [], caNone));
   sChar0.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sCharF.AddTransition(INT, TConsumeTransition.Create(sCharF, [], caNone));
   sCharF.AddTransition([''''], TCheckTransition.Create(sStart, [], caChar));
   sCharF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caChar));
   sCharF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caChar));
   sCharF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sCharHex.AddTransition(HEX, TConsumeTransition.Create(sCharHexF, [], caNone));
   sCharHex.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sCharHexF.AddTransition(HEX, TConsumeTransition.Create(sCharHexF, [], caNone));
   sCharHexF.AddTransition([''''], TCheckTransition.Create(sStart, [], caCharHex));
   sCharHexF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caCharHex));
   sCharHexF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caCharHex));
   sCharHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sNameF.AddTransition(NAM + INT, TConsumeTransition.Create(sNameF, [], caNone));
   sNameF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sIntF.AddTransition(INT, TConsumeTransition.Create(sIntF, [], caNone));
   sIntF.AddTransition(['.'], TConsumeTransition.Create(sIntPoint, [], caNone));
   sIntF.AddTransition(['E', 'e'], TConsumeTransition.Create(sIntExp, [], caNone));
   sIntF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caInteger));
   sIntF.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

   sIntPoint.AddTransition(['.'], TCheckTransition.Create(sDotDot, [toFinal], caInteger));
   sIntPoint.AddTransition(INT, TConsumeTransition.Create(sIntPointF, [], caNone));
   sIntPoint.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sIntPointF.AddTransition(INT, TConsumeTransition.Create(sIntPointF, [], caNone));
   sIntPointF.AddTransition(['E', 'e'], TConsumeTransition.Create(sIntExp, [], caNone));
   sIntPointF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caFloat));
   sIntPointF.SetElse(TErrorTransition.Create(TOK_NumberExponentExpected));

   sIntExp.AddTransition(['+', '-'], TConsumeTransition.Create(sIntExp0, [], caNone));
   sIntExp.AddTransition(INT, TConsumeTransition.Create(sIntExpF, [], caNone));
   sIntExp.SetElse(TErrorTransition.Create(TOK_NumberSignExpected));

   sIntExp0.AddTransition(INT, TConsumeTransition.Create(sIntExpF, [], caNone));
   sIntExp0.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sIntExpF.AddTransition(INT, TConsumeTransition.Create(sIntExpF, [], caNone));
   sIntExpF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caFloat));
   sIntExpF.SetElse(TErrorTransition.Create(TOK_NumberExpected));

   sHex.AddTransition(HEX, TConsumeTransition.Create(sHexF, [], caNone));
   sHex.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sHexF.AddTransition(HEX, TConsumeTransition.Create(sHexF, [], caNone));
   sHexF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caHex));
   sHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

   sString0.AddTransition(ANYCHAR - ['''', #13, #10], TConsumeTransition.Create(sString0, [], caNone));
   sString0.AddTransition([''''], TSeekTransition.Create(sStringF, [], caNone));
   sString0.SetElse(TErrorTransition.Create(TOK_StringTerminationError));

   sStringF.AddTransition([''''], TConsumeTransition.Create(sString0, [], caNone));
   sStringF.AddTransition(['#'], TCheckTransition.Create(sStart, [], caString));
   sStringF.AddTransition(STOP, TCheckTransition.Create(sStart, [toFinal], caString));
   sStringF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

   sAssign0.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sAssign0.AddTransition(Start + STOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sAssign0.SetElse(TErrorTransition.Create(TOK_EqualityExpected));

   sGreaterF.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sGreaterF.AddTransition(Start + STOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sGreaterF.SetElse(TErrorTransition.Create(TOK_EqualityExpected));

   sSmallerF.AddTransition(['='], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSmallerF.AddTransition(['>'], TConsumeTransition.Create(sStart, [toFinal], caName));
   sSmallerF.AddTransition(Start + STOP, TCheckTransition.Create(sStart, [toFinal], caName));
   sSmallerF.SetElse(TErrorTransition.Create(TOK_GreaterEqualityExpected));

   sDotDot.AddTransition(['.'], TConsumeTransition.Create(sStart, [toFinal], caDotDot));
   sDotDot.AddTransition(NAM, TCheckTransition.Create(sStart, [toFinal], caName));
   sDotDot.SetElse(TErrorTransition.Create(TOK_DotExpected));

finalization

   sStart.Free;
   sSpace.Free;
   sComment.Free;
   sCommentF.Free;
   sSwitch.Free;
   sSwitchNameF.Free;
   sSlashComment0.Free;
   sSlashComment.Free;
   sChar0.Free;
   sCharF.Free;
   sCharHex.Free;
   sCharHexF.Free;
   sNameF.Free;
   sIntF.Free;
   sIntPoint.Free;
   sIntPointF.Free;
   sIntExp.Free;
   sIntExp0.Free;
   sIntExpF.Free;
   sHex.Free;
   sHexF.Free;
   sString0.Free;
   sStringF.Free;
   sAssign0.Free;
   sGreaterF.Free;
   sSmallerF.Free;
   sDotDot.Free;

end.
