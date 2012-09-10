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
  SysUtils, Classes, TypInfo, dwsErrors, dwsStrings, dwsXPlatform, dwsUtils
  {$ifdef FPC},lazutf8{$endif};

type

   TTokenType =
     (ttNone, ttStrVal, ttIntVal, ttFloatVal, ttNAME, ttSWITCH,
     ttLAZY, ttVAR, ttCONST, ttRESOURCESTRING,
     ttTYPE, ttRECORD, ttARRAY, ttSET, ttDOT, ttDOTDOT, ttOF, ttENUM, ttFLAGS,
     ttTRY, ttEXCEPT, ttRAISE, ttFINALLY, ttON, ttREAD, ttWRITE, ttPROPERTY,
     ttFUNCTION, ttPROCEDURE, ttCONSTRUCTOR, ttDESTRUCTOR, ttMETHOD, ttLAMBDA, ttOPERATOR,
     ttCLASS, ttNIL, ttIS, ttAS, ttIMPLEMENTS, ttINDEX, ttOBJECT,
     ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttINHERITED, ttFINAL, ttNEW,
     ttABSTRACT, ttSEALED, ttSTATIC, ttPARTIAL, ttDEPRECATED, ttOVERLOAD,
     ttEXTERNAL, ttFORWARD, ttEMPTY, ttIN,
     ttENSURE, ttREQUIRE, ttINVARIANTS, ttOLD,
     ttINTERFACE, ttIMPLEMENTATION, ttINITIALIZATION, ttFINALIZATION, ttHELPER,
     ttBEGIN, ttEND, ttBREAK, ttCONTINUE, ttEXIT,
     ttIF, ttTHEN, ttELSE, ttWHILE, ttREPEAT, ttUNTIL, ttFOR, ttTO, ttDOWNTO, ttDO,
     ttCASE,
     ttTRUE, ttFALSE,
     ttAND, ttOR, ttXOR, ttIMPLIES, ttDIV, ttMOD, ttNOT, ttSHL, ttSHR, ttSAR,
     ttPLUS, ttMINUS,
     ttTIMES, ttDIVIDE, ttPERCENT, ttCARET, ttAT, ttDOLLAR, ttEXCLAMATION, ttQUESTION,
     ttEQ, ttNOTEQ, ttGTR, ttGTREQ, ttLESS, ttLESSEQ, ttEQGTR,
     ttLESSLESS, ttGTRGTR,
     ttSEMI, ttCOMMA, ttCOLON,
     ttASSIGN, ttPLUS_ASSIGN, ttMINUS_ASSIGN, ttTIMES_ASSIGN, ttDIVIDE_ASSIGN,
     ttPERCENT_ASSIGN, ttCARET_ASSIGN, ttAT_ASSIGN,
     ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCRIGHT,
     ttDEFAULT, ttUSES, ttUNIT,
     ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
     ttPROGRAM,

     // Tokens for compatibility to Delphi
     ttREGISTER, ttPASCAL, ttCDECL, ttSTDCALL, ttFASTCALL, ttREFERENCE);

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
      function ToStr : String; overload; inline;
      procedure ToStr(var result : String); overload;
      procedure AppendMultiToStr(var result : String);
      procedure AppendToStr(var result : String);
      procedure ToUpperStr(var result : String); overload;
      function UpperFirstChar : Char;
      function UpperMatchLen(const str : String) : Boolean;
      function BinToInt64 : Int64;
      function HexToInt64 : Int64;
      function ToInt64 : Int64;
      function ToFloat : Double;
      function ToType : TTokenType;
      function ToAlphaType : TTokenType;

      class function StringToTokenType(const str : String) : TTokenType; static;
   end;

   TToken = ^TTokenRecord;
   TTokenRecord = record
      FScriptPos : TScriptPos;
      FString : String;
      FFloat : Double;
      FInteger : Int64;
      FTyp : TTokenType;
   end;

   TCharsType = set of AnsiChar;
   TTransition = class;

   TState = class (TRefCountedObject)
      private
         FOwnedTransitions : TTightList;
         FTransitions : array [#0..#127] of TTransition;

      public
         destructor Destroy; override;

         function FindTransition(c : Char) : TTransition; inline;
         procedure AddTransition(const chrs : TCharsType; o : TTransition);
         procedure SetElse(o : TTransition);
   end;

   TConvertAction = (caNone, caClear, caName, caNameEscaped,
                     caBin, caHex, caInteger, caFloat,
                     caChar, caCharHex, caString, caMultiLineString,
                     caSwitch, caDotDot);
   TTransitionOptions = set of (toStart, toFinal);

   TTransition = class (TRefCountedObject)
      private
         NextState : TState;
         Start : Boolean; // Marks the begin of a Token
         Final : Boolean; // Marks the end of a Token
         Action : TConvertAction;

      public
         constructor Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
   end;

   TElseTransition = class(TTransition)
     constructor Create(actn : TConvertAction);
   end;

   TErrorTransition = class(TTransition)
      private
         ErrorMessage : String;

      public
         constructor Create(const msg : String);
   end;

   TCheckTransition = class(TTransition);
   TSeekTransition = class(TCheckTransition); // Transition, next Char
   TConsumeTransition = class(TSeekTransition); // Transition, consume Char, next Char

   TSwitchHandler = function(const switchName : String) : Boolean of object;

   TTokenizer = class;

   TTokenizerRules = class
      private
         FStates : TObjectList<TState>;
         FEOFTransition : TErrorTransition;
         FReservedNames : TTokenTypes;
         FSymbolTokens : TTokenTypes;
         FReservedTokens : TTokenTypes;

      protected
         function CreateState : TState;
         function StartState : TState; virtual; abstract;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure PrepareStates;

         function CreateTokenizer(msgs : TdwsCompileMessageList) : TTokenizer;

         property ReservedNames : TTokenTypes read FReservedNames write FReservedNames;
         property SymbolTokens : TTokenTypes read FSymbolTokens write FSymbolTokens;
         property ReservedTokens : TTokenTypes read FReservedTokens;
   end;

   TTokenizerSourceInfo = record
      FText : String;
      FDefaultPos : TScriptPos;
      FHotPos : TScriptPos;
      FCurPos : TScriptPos;
      FPosPtr : PChar;
   end;

   TTokenizerConditional = (tcIf, tcElse);
   TTokenizerConditionalInfo = record
      Conditional : TTokenizerConditional;
      ScriptPos : TScriptPos;
   end;

   TTokenizerEndSourceFileEvent = procedure (sourceFile : TSourceFile) of object;

   TTokenizer = class
      private
         FTokenBuf : TTokenBuffer;
         FNextToken : TToken;
         FRules : TTokenizerRules;
         FStartState : TState;
         FToken : TToken;
         FSource : TTokenizerSourceInfo;
         FSwitchHandler : TSwitchHandler;
         FSwitchProcessor : TSwitchHandler;
         FMsgs : TdwsCompileMessageList;
         FConditionalDefines : IAutoStrings;
         FConditionalDepth : TSimpleStack<TTokenizerConditionalInfo>;

         FTokenStore : array of TToken;
         FTokenStoreCount : Integer;

         FSourceStack : array of TTokenizerSourceInfo;
         FOnEndSourceFile : TTokenizerEndSourceFileEvent;

         procedure AllocateToken;
         procedure ReleaseToken;

         procedure HandleChar(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleBin(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleInteger(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleFloat(var tokenBuf : TTokenBuffer; var result : TToken);

         procedure ConsumeToken;

         procedure ReadToken;
         procedure AddCompilerStopFmtTokenBuffer(const formatString : String);

      public
         constructor Create(rules : TTokenizerRules; msgs : TdwsCompileMessageList);
         destructor Destroy; override;

         procedure BeginSourceFile(sourceFile : TSourceFile);
         procedure EndSourceFile;

         function GetToken : TToken; inline;
         function HasTokens : Boolean;
         procedure KillToken; inline;

         function Test(t : TTokenType) : Boolean;
         function TestAny(const t : TTokenTypes) : TTokenType;
         function TestDelete(t : TTokenType) : Boolean;
         function TestDeleteAny(const t : TTokenTypes) : TTokenType;
         function TestName : Boolean;
         function TestAnyName : Boolean;

         function TestDeleteNamePos(var aName : String; var aPos : TScriptPos) : Boolean; inline;
         function TestDeleteAnyNamePos(var aName : String; var aPos : TScriptPos) : Boolean; inline;

         procedure SimulateToken(t : TTokenType; const scriptPos : TScriptPos);
         procedure SimulateStringToken(const scriptPos : TScriptPos; const str : String);
         procedure SimulateNameToken(const scriptPos : TScriptPos; const name : String);

         property PosPtr : PChar read FSource.FPosPtr;
         property Text : String read FSource.FText;
         property DefaultPos : TScriptPos read FSource.FDefaultPos;
         property HotPos : TScriptPos read FSource.FHotPos;
         property CurrentPos : TScriptPos read FSource.FCurPos;

         property ConditionalDepth : TSimpleStack<TTokenizerConditionalInfo> read FConditionalDepth;

         property SwitchHandler : TSwitchHandler read FSwitchHandler write FSwitchHandler;
         property SwitchProcessor : TSwitchHandler read FSwitchProcessor write FSwitchProcessor;
         property ConditionalDefines : IAutoStrings read FConditionalDefines write FConditionalDefines;
         property OnEndSourceFile : TTokenizerEndSourceFileEvent read FOnEndSourceFile write FOnEndSourceFile;
   end;

const
   cTokenStrings : array [TTokenType] of String = (
     '', 'StrVal', 'IntVal', 'FloatVal', 'NAME', 'SWITCH',
     'LAZY', 'VAR', 'CONST', 'RESOURCESTRING',
     'TYPE', 'RECORD', 'ARRAY', 'SET', '.', '..', 'OF', 'ENUM', 'FLAGS',
     'TRY', 'EXCEPT', 'RAISE', 'FINALLY', 'ON', 'READ', 'WRITE', 'PROPERTY',
     'FUNCTION', 'PROCEDURE', 'CONSTRUCTOR', 'DESTRUCTOR', 'METHOD', 'LAMBDA', 'OPERATOR',
     'CLASS', 'NIL', 'IS', 'AS', 'IMPLEMENTS', 'INDEX', 'OBJECT',
     'VIRTUAL', 'OVERRIDE', 'REINTRODUCE', 'INHERITED', 'FINAL', 'NEW',
     'ABSTRACT', 'SEALED', 'STATIC', 'PARTIAL', 'DEPRECATED', 'OVERLOAD',
     'EXTERNAL', 'FORWARD', 'EMPTY', 'IN',
     'ENSURE', 'REQUIRE', 'INVARIANTS', 'OLD',
     'INTERFACE', 'IMPLEMENTATION', 'INITIALIZATION', 'FINALIZATION', 'HELPER',
     'BEGIN', 'END', 'BREAK', 'CONTINUE', 'EXIT',
     'IF', 'THEN', 'ELSE', 'WHILE', 'REPEAT', 'UNTIL', 'FOR', 'TO', 'DOWNTO', 'DO',
     'CASE',
     'TRUE', 'FALSE',
     'AND', 'OR', 'XOR', 'IMPLIES', 'DIV', 'MOD', 'NOT', 'SHL', 'SHR', 'SAR',
     '+', '-',
     '*', '/', '%', '^', '@', '$', '!', '?',
     '=', '<>', '>', '>=', '<', '<=', '=>',
     '<<', '>>',
     ';', ',', ':',
     ':=', '+=', '-=', '*=', '/=',
     '%=', '^=', '@=',
     '(', ')', '[', ']', '}',
     'DEFAULT', 'USES', 'UNIT',
     'PRIVATE', 'PROTECTED', 'PUBLIC', 'PUBLISHED',
     'PROGRAM',
     'REGISTER', 'PASCAL', 'CDECL', 'STDCALL', 'FASTCALL', 'REFERENCE'
     );

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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
   ToStr(Result);
end;

// ToStr
//
procedure TTokenBuffer.ToStr(var result : String);
begin
   if Len=0 then
      result:=''
   else begin
      SetLength(result, Len);
      Move(Buffer[0], Pointer(result)^, Len*SizeOf(Char));
   end;
end;

// AppendToStr
//
procedure TTokenBuffer.AppendToStr(var result : String);
var
   n : Integer;
begin
   if Len>0 then begin
      n:=Length(result);
      SetLength(result, n+Len);
      Move(Buffer[0], PChar(Pointer(result))[n], Len*SizeOf(Char));
   end;
end;

// AppendMultiToStr
//
procedure TTokenBuffer.AppendMultiToStr(var result : String);
var
   i, n, k, minWhite, white : Integer;
   leftWhite, firstIsCRLF, firstLine : Boolean;
begin
   if Len=0 then Exit;
   // count nb lines and minimum whitespace, also detect if first line is whitespace + CRLF
   n:=0;
   minWhite:=MaxInt;
   leftWhite:=True;
   white:=0;
   firstIsCRLF:=False;
   firstLine:=True;
   for i:=0 to Len-1 do begin
      case Buffer[i] of
         ' ' : if leftWhite then Inc(white);
         #13 : ;
         #10 : begin
            if firstLine then begin
               if leftWhite then
                  firstIsCRLF:=True;
               firstLine:=False;
            end;
            if not leftWhite then begin
               if white<minWhite then
                  minWhite:=white;
               leftWhite:=True;
            end;
            white:=0;
            Inc(n);
         end;
      else
         leftWhite:=False;
      end;
   end;
   // single line with no indent, use simple AppendToStr
   if (n=0) and (minWhite=0) then begin
      AppendToStr(result);
      Exit;
   end;

   // ok now collect and remove indents
   k:=Length(result);
   SetLength(result, k+Len); // allocate for worst case
   i:=0;
   n:=Len;

   // do we have to remove indents?
   if firstIsCRLF then begin
      // skip first line
      repeat
         Inc(i);
      until Buffer[i]=#10;
      Inc(i);
   end;

   leftWhite:=(minWhite>0);
   white:=0;
   while i<n do begin
      case Buffer[i] of
         ' ' : begin
            if leftWhite and (white<minWhite) then
               Inc(white)
            else begin
               Inc(k);
               result[k]:=' ';
            end;
         end;
         #10 : begin
            leftWhite:=(minWhite>0);
            white:=0;
            Inc(k);
            result[k]:=Buffer[i];
         end
      else
         leftWhite:=False;
         Inc(k);
         result[k]:=Buffer[i];
      end;
      Inc(i);
   end;

   SetLength(result, k);
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

// BinToInt64
//
function TTokenBuffer.BinToInt64 : Int64;

   procedure RaiseInvalid;
   begin
      raise EIntOverflow.CreateFmt(TOK_InvalidIntegerConstant, [ToStr]);
   end;

var
   i : Integer;
begin
   if Len>64+2 then
      RaiseInvalid;
   Result:=0;
   for i:=2 to Len-1 do begin
      if Buffer[i]='1' then
         Result:=(Result shl 1) or 1
      else Result:=(Result shl 1);
   end;
end;

// BinToInt64
//
function TTokenBuffer.HexToInt64 : Int64;
begin
   Result:=StrToInt64(ToStr);
end;

// ToInt64
//
function TTokenBuffer.ToInt64 : Int64;

   function ComplexToInt64(var buffer : TTokenBuffer) : Int64;
   begin
      Result:=StrToInt64(buffer.ToStr);
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
   if not TryTextToFloat(PChar(@Buffer[0]), buf, cFormatSettings) then
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
     '!': Result := ttEXCLAMATION;
     '?': Result := ttQUESTION;
     '=': if Len=1 then
            Result := ttEQ
         else if Len=2 then
            if Buffer[1]='>' then
               Result := ttEQGTR;
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
   cAlphaTypeTokens : TTokenTypes = [
      ttAND, ttARRAY, ttABSTRACT, ttAS,
      ttBEGIN, ttBREAK,
      ttCONST, ttCLASS, ttCONSTRUCTOR, ttCASE, ttCDECL, ttCONTINUE,
      ttDO, ttDOWNTO, ttDIV, ttDEFAULT, ttDESTRUCTOR, ttDEPRECATED,
      ttELSE, ttEMPTY, ttEND, ttENSURE, ttENUM, ttEXCEPT, ttEXIT, ttEXTERNAL,
      ttFALSE, ttFINAL, ttFINALIZATION, ttFINALLY, ttFLAGS, ttFOR,
      ttFORWARD, ttFUNCTION, ttHELPER,
      ttIF, ttIMPLIES, ttIMPLEMENTS, ttIN, ttINITIALIZATION, ttINVARIANTS,
      ttIS, ttINHERITED, ttINDEX, ttINTERFACE, ttIMPLEMENTATION,
      ttLAMBDA, ttLAZY,
      ttMETHOD, ttMOD,
      ttNEW, ttNOT, ttNIL,
      ttOBJECT, ttOF, ttOLD, ttON, ttOPERATOR, ttOR, ttOVERLOAD, ttOVERRIDE,
      ttPARTIAL, ttPROCEDURE, ttPROPERTY, ttPASCAL, ttPROGRAM,
      ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
      ttRECORD, ttREAD, ttRAISE, ttREINTRODUCE, ttREFERENCE, ttREGISTER,
      ttREPEAT, ttREQUIRE, ttRESOURCESTRING,
      ttSAR, ttSEALED, ttSHL, ttSHR, ttSTATIC, ttSTDCALL,
      ttTHEN, ttTO, ttTRUE, ttTRY, ttTYPE,
      ttUNIT, ttUNTIL, ttUSES,
      ttVAR, ttVIRTUAL,
      ttWHILE, ttWRITE,
      ttXOR ];
type
   TTokenAlphaLookup = record
      Alpha : String;
      Token : TTokenType;
   end;
   TTokenAlphaLookups = array of TTokenAlphaLookup;
   PTokenAlphaLookups = ^TTokenAlphaLookups;
var
   vAlphaToTokenType : array [2..14] of array ['A'..'X'] of TTokenAlphaLookups;

procedure PrepareAlphaToTokenType;
var
   n, len : Integer;
   tokenName : String;
   tt : TTokenType;
begin
   for tt in cAlphaTypeTokens do begin
      tokenName:=GetEnumName(TypeInfo(TTokenType), Ord(tt));
      len:=Length(tokenName)-2;
      Assert(len<=14);
      n:=Length(vAlphaToTokenType[len][tokenName[3]]);
      SetLength(vAlphaToTokenType[len][tokenName[3]], n+1);
      with vAlphaToTokenType[len][tokenName[3]][n] do begin
         Alpha:=Copy(tokenName, 3, MaxInt);
         Token:=tt;
      end;
   end;
end;

// ------------------
// ------------------ TTokenBuffer ------------------
// ------------------

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
class function TTokenBuffer.StringToTokenType(const str : String) : TTokenType;
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
begin
   if c>#127 then
      c:=#127;
   Result:=FTransitions[c];
end;

// AddTransition
//
procedure TState.AddTransition(const chrs : TCharsType; o : TTransition);
var
   c : AnsiChar;
begin
   for c:=#0 to #127 do
      if c in chrs then begin
         if FTransitions[c]=nil then
            FTransitions[c]:=o;
      end;
   FOwnedTransitions.Add(o);
end;

// SetElse
//
procedure TState.SetElse(o : TTransition);
var
   c : AnsiChar;
begin
   for c:=#1 to #127 do
      if FTransitions[c]=nil then
        FTransitions[c]:=o;
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

constructor TErrorTransition.Create(const msg : String);
begin
   ErrorMessage:=msg;
end;

// ------------------
// ------------------ TTokenizer ------------------
// ------------------

// Create
//
constructor TTokenizer.Create(rules : TTokenizerRules; msgs : TdwsCompileMessageList);
begin
   FMsgs := Msgs;
   FTokenBuf.Grow;
   FRules := rules;
   FStartState := FRules.StartState;

   SetLength(FTokenStore, 8);

   FConditionalDepth:=TSimpleStack<TTokenizerConditionalInfo>.Create;
end;

// Destroy
//
destructor TTokenizer.Destroy;
begin
   if FToken<>nil then Dispose(FToken);
   if FNextToken<>nil then Dispose(FNextToken);

   while FTokenStoreCount>0 do begin
      Dec(FTokenStoreCount);
      Dispose(FTokenStore[FTokenStoreCount]);
   end;

   FConditionalDepth.Free;

   inherited;
end;

// BeginSourceFile
//
procedure TTokenizer.BeginSourceFile(sourceFile : TSourceFile);
var
   n : Integer;
begin
   if DefaultPos.SourceFile<>nil then begin
      n:=Length(FSourceStack);
      SetLength(FSourceStack, n+1);
      FSourceStack[n]:=FSource;
   end;

   FSource.FText := sourceFile.Code + (cLineTerminator+#0);
   FSource.FDefaultPos := cNullPos;
   FSource.FDefaultPos.SourceFile := sourceFile;
   FSource.FHotPos := DefaultPos;
   FSource.FCurPos := DefaultPos;
   FSource.FPosPtr := PChar(Text);
   FSource.FCurPos.Line := 1;
   FSource.FCurPos.Col := 1;
end;

// EndSourceFile
//
procedure TTokenizer.EndSourceFile;
var
   n : Integer;
begin
   n:=Length(FSourceStack);
   if n>0 then begin
      if Assigned(FOnEndSourceFile) then
         FOnEndSourceFile(FSource.FDefaultPos.SourceFile);
      FSource:=FSourceStack[n-1];
      SetLength(FSourceStack, n-1);
   end else begin
      Assert(DefaultPos.SourceFile<>nil);
      FSource.FText:='';
      FSource.FDefaultPos:=cNullPos;
      FSource.FHotPos:=cNullPos;
      FSource.FCurPos:=cNullPos;
      FSource.FPosPtr:=nil;
   end;
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
   end else ConsumeToken;
end;

// AddCompilerStopFmtTokenBuffer
//
procedure TTokenizer.AddCompilerStopFmtTokenBuffer(const formatString : String);
var
   buf : String;
begin
   buf:=FTokenBuf.ToStr;
   FMsgs.AddCompilerStopFmt(CurrentPos, formatString, [buf]);
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
   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
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

   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
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

   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
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

   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   if FToken.FTyp in t then begin
      Result:=FToken.FTyp;
      KillToken;
   end else Result:=ttNone;
end;

// TestName
//
function TTokenizer.TestName : Boolean;
begin
   Result:=False;
   if not Assigned(FToken) then
      ReadToken;
   if Assigned(FToken) then begin
      Result:=(FToken.FString<>'') and not (FToken.FTyp in FRules.ReservedTokens);
      FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   end;
end;

// TestAnyName
//
function TTokenizer.TestAnyName : Boolean;
begin
   Result:=False;
   if not Assigned(FToken) then
      ReadToken;
   if Assigned(FToken) then begin
      Result:=(FToken.FString<>'') and not (FToken.FTyp in FRules.SymbolTokens);
      FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   end;
end;

// TestDeleteNamePos
//
function TTokenizer.TestDeleteNamePos(var aName : String; var aPos : TScriptPos) : Boolean;
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

// TestDeleteAnyNamePos
//
function TTokenizer.TestDeleteAnyNamePos(var aName : String; var aPos : TScriptPos) : Boolean;
begin
   if not TestAnyName then
      Result:=False
   else begin
      aName:=GetToken.FString;
      aPos:=HotPos;
      KillToken;
      Result:=True;
   end;
end;

// SimulateToken
//
procedure TTokenizer.SimulateToken(t : TTokenType; const scriptPos : TScriptPos);
begin
   Assert(FNextToken=nil);
   FNextToken:=FToken;
   AllocateToken;
   FToken.FTyp:=t;
   FToken.FScriptPos:=scriptPos;
end;

// SimulateStringToken
//
procedure TTokenizer.SimulateStringToken(const scriptPos : TScriptPos; const str : String);
begin
   SimulateToken(ttStrVal, scriptPos);
   FToken.FString:=str;
end;

// SimulateNameToken
//
procedure TTokenizer.SimulateNameToken(const scriptPos : TScriptPos; const name : String);
begin
   SimulateToken(ttNAME, scriptPos);
   FToken.FString:=name;
end;

// HasTokens
//
function TTokenizer.HasTokens: Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if FToken<>nil then
         FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   end;
   Result:=(FToken<>nil);
end;

// HandleChar
//
procedure TTokenizer.HandleChar(var tokenBuf : TTokenBuffer; var result : TToken);
var
   tokenIntVal, n : Integer;
   {$ifdef FPC}
   us : UnicodeString;
   {$endif}
begin
   tokenIntVal:=FTokenBuf.ToInt64;
   case tokenIntVal of
      0..$FFFF : begin
         {$ifdef FPC}
         result.FString:=result.FString+UTF8Encode(WideChar(tokenIntVal));
         {$else}
         n:=Length(result.FString)+1;
         SetLength(result.FString, n);
         result.FString[n]:=WideChar(tokenIntVal);
         {$endif}
         result.FTyp:=ttStrVal;
      end;
      $10000..$10FFFF : begin
         {$ifdef FPC}
         result.FString:=result.FString+UnicodeToUTF8(tokenIntVal);
         {$else}
         tokenIntVal:=tokenIntVal-$10000;
         n:=Length(result.FString)+2;
         SetLength(result.FString, n);
         result.FString[n-1]:=WideChar($D800+(tokenIntVal shr 10));
         result.FString[n]:=WideChar($DC00+(tokenIntVal and $3FF));
         {$endif}
         result.FTyp:=ttStrVal;
      end;
   else
      AddCompilerStopFmtTokenBuffer(TOK_InvalidCharConstant)
   end;
end;

// HandleBin
//
procedure TTokenizer.HandleBin(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   result.FInteger:=tokenBuf.BinToInt64;
   result.FTyp:=ttIntVal;
end;

// HandleHexa
//
procedure TTokenizer.HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      result.FInteger:=tokenBuf.HexToInt64;
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
procedure TTokenizer.ConsumeToken;

   // don't trigger error for EOF
   procedure DoErrorTransition(trns : TErrorTransition; ch : Char);
   begin
      if trns.ErrorMessage<>'' then begin
         case ch of
            #0 :
               FMsgs.AddCompilerError(CurrentPos, trns.ErrorMessage);
            #1..#31 :
               FMsgs.AddCompilerStopFmt(CurrentPos, '%s (found #%x)', [trns.ErrorMessage, Ord(ch)]);
         else
            FMsgs.AddCompilerStopFmt(CurrentPos, '%s (found "%s")', [trns.ErrorMessage, ch])
         end;
      end;
   end;

   // return True to reset state and continue to next token
   function DoAction(action : TConvertAction) : Boolean;
   begin
      try
         case action of
            caClear : begin
               FTokenBuf.Len:=0;
               FToken.FScriptPos:=DefaultPos;
            end;

            // Convert name to token
            caName : begin
               FToken.FTyp:=FTokenBuf.ToType;
               FTokenBuf.ToStr(FToken.FString);
            end;

            // Convert escaped name to token
            caNameEscaped : begin
               FToken.FTyp:=ttNAME;
               FTokenBuf.ToStr(FToken.FString);
            end;

            // converts ASCII code to character (decimal or hex)
            caChar, caCharHex :
               HandleChar(FTokenBuf, FToken);

            // Concatenates the parts of a String constant
            caString : begin
               FTokenBuf.AppendToStr(FToken.FString);
               FToken.FTyp:=ttStrVal;
            end;

            caMultiLineString : begin
               FTokenBuf.AppendMultiToStr(FToken.FString);
               FToken.FTyp:=ttStrVal;
            end;

            // Converts binary number to integer
            caBin :
               HandleBin(FTokenBuf, FToken);

            // Converts hexadecimal number to integer
            caHex :
               HandleHexa(FTokenBuf, FToken);

            // Converts integer constants
            caInteger :
               HandleInteger(FTokenBuf, FToken);

            // Converts Floating Point numbers
            caFloat :
               HandleFloat(FTokenBuf, FToken);

            caSwitch :
               if Assigned(FSwitchHandler) then begin
                  FSource.FHotPos.SetLineCol(FToken.FScriptPos);

                  // Ask parser if we should create a token or not
                  FTokenBuf.ToUpperStr(FToken.FString);
                  if FSwitchHandler(FToken.FString) then begin
                     FToken.FTyp:=ttSWITCH;
                  end else begin
                     if FToken=nil then
                        AllocateToken;
                     FTokenBuf.Len:=0;
                     Exit(True);
                  end;
               end;

            caDotDot : begin
               FToken.FScriptPos:=CurrentPos;
               FToken.FScriptPos.Col:=FToken.FScriptPos.Col-1;
               FToken.FTyp:=ttDOTDOT;
            end;
         end;
         FTokenBuf.Len:=0;
         Result:=False;
      except
         ReleaseToken;
         raise;
      end;
   end;

   // process switch instruction
   procedure DoSwitchProcessor;
   begin
      if Assigned(FSwitchProcessor) then begin
         try
            if FSwitchProcessor(FToken.FString) then begin
               ReleaseToken;
               ConsumeToken;
            end;
         except
            ReleaseToken;
            raise;
         end;
      end;
   end;

var
   state : TState;
   trns : TTransition;
   trnsClassType : TClass;
   pch : PChar;
begin
   AllocateToken;

   state:=FStartState;
   FTokenBuf.Len:=0;

   // Next character
   pch:=PosPtr;

   // Look for the next token in FText
   while Assigned(state) do begin

      // Find next state
      //trns:=state.FindTransition(pch^);
      if pch^>#127 then
         trns:=state.FTransitions[#127]
      else trns:=state.FTransitions[pch^];

      trnsClassType:=trns.ClassType;

      // Handle Errors
      if trnsClassType=TErrorTransition then begin
         // tokenizer errors will raise exceptions, EOF won't
         DoErrorTransition(TErrorTransition(trns), pch^);
         if FSourceStack<>nil then begin
            EndSourceFile;
            pch:=PosPtr;
            continue;
         end;
         Break;
      end;

      // A new token begins
      if trns.Start and (FToken.FScriptPos.Col=0) then
         FToken.FScriptPos:=CurrentPos;

      // Add actual character to s
      if trnsClassType=TConsumeTransition then
         FTokenBuf.AppendChar(pch^);

      // Proceed to the next character
      if (trnsClassType=TSeekTransition) or (trnsClassType=TConsumeTransition) then begin
         Inc(FSource.FPosPtr);
         if pch^=#10 then
            FSource.FCurPos.NewLine
         else FSource.FCurPos.IncCol;
         Inc(pch);
      end;

      // The characters in 's' have to be converted
      if trns.Action<>caNone then begin
         if DoAction(trns.Action) then begin
            state:=FRules.StartState;
            pch:=PosPtr;
            continue;
         end;
      end;

      // If the token is complete then exit
      if trns.Final then begin
         FStartState:=trns.NextState;
         if FToken.FTyp=ttSWITCH then
            DoSwitchProcessor;
         Exit;
      end else state:=trns.NextState;

   end;

   // Couldn't read a token (end of FText reached)
   ReleaseToken;
end;

// KillToken
//
procedure TTokenizer.KillToken;
begin
   ReleaseToken;
end;

// AllocateToken
//
procedure TTokenizer.AllocateToken;
begin
   if FTokenStoreCount>0 then begin
      Dec(FTokenStoreCount);
      FToken:=FTokenStore[FTokenStoreCount];
   end else New(FToken);
   FToken.FTyp:=ttNone;
   FToken.FScriptPos:=cNullPos;
end;

// ReleaseToken
//
procedure TTokenizer.ReleaseToken;
begin
   if FToken<>nil then begin
      FTokenStore[FTokenStoreCount]:=FToken;
      Inc(FTokenStoreCount);
      if FToken.FString<>'' then
         FToken.FString:='';
      FToken:=nil;
   end;
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
   FEOFTransition.Free;
end;

// PrepareStates
//
procedure TTokenizerRules.PrepareStates;
var
   i : Integer;
   state : TState;
begin
   FReservedTokens:=FSymbolTokens+FReservedNames;

   FEOFTransition:=TErrorTransition.Create('');
   for i:=0 to FStates.Count-1 do begin
      state:=FStates[i];
      if (state.FTransitions[#0]=nil) or not (state.FTransitions[#0] is TErrorTransition) then
         state.FTransitions[#0]:=FEOFTransition;
   end;
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
function TTokenizerRules.CreateTokenizer(msgs : TdwsCompileMessageList) : TTokenizer;
begin
   Result:=TTokenizer.Create(Self, msgs);
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
