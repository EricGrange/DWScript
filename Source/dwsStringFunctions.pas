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
unit dwsStringFunctions;

interface

uses Classes, SysUtils, Variants, StrUtils, dwsFunctions, dwsSymbols, dwsStrings,
   dwsUtils, dwsExprs, dwsCoreExprs;

type

  TChrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TIntToStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TStrToIntFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TStrToIntDefFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TIntToHexFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  THexToIntFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TFloatToStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TStrToFloatFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
  end;

  TStrToFloatDefFunc = class(TInternalMagicFloatFunction)
    procedure DoEvalAsFloat(args : TExprBaseList; var Result : Double); override;
  end;

  TFormatFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TCharAtFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TSetCharAtFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCopyFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TLeftStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TRightStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TSubStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TSubStringFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TDeleteFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TInsertFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TLowerCaseFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TAnsiLowerCaseFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TUpperCaseFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TAnsiUpperCaseFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TPosFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TPosExFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TRevPosFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TSetLengthFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TTrimLeftFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TTrimRightFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TTrimFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TSameTextFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
  end;

  TCompareTextFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TAnsiCompareTextFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TCompareStrFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TAnsiCompareStrFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TIsDelimiterFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
  end;

  TLastDelimiterFunc = class(TInternalMagicIntFunction)
    function DoEvalAsInteger(args : TExprBaseList) : Int64; override;
  end;

  TQuotedStrFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TStringOfCharFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TStringOfStringFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TStrBeginsWithFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
  end;

  TStrEndsWithFunc = class(TInternalMagicBoolFunction)
    function DoEvalAsBoolean(args : TExprBaseList) : Boolean; override;
  end;

  TStrAfterFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

  TStrBeforeFunc = class(TInternalMagicStringFunction)
    procedure DoEvalAsString(args : TExprBaseList; var Result : String); override;
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const // type constants
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';
  cVariant = 'Variant';

{ TChrFunc }

// DoEvalAsString
//
procedure TChrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=Char(args.AsInteger[0]);
end;

{ TIntToStrFunc }

// DoEvalAsString
//
procedure TIntToStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=IntToStr(args.AsInteger[0]);
end;

{ TStrToIntFunc }

function TStrToIntFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=StrToInt64(args.AsString[0]);
end;

{ TStrToIntDefFunc }

function TStrToIntDefFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=StrToInt64Def(args.AsString[0], args.AsInteger[1]);
end;

{ TIntToHexFunc }

// DoEvalAsString
//
procedure TIntToHexFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=SysUtils.IntToHex(args.AsInteger[0], args.AsInteger[1]);
end;

{ THexToIntFunc }

function THexToIntFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=StrToInt64('$'+args.AsString[0]);
end;

{ TFloatToStrFunc }

// DoEvalAsString
//
procedure TFloatToStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=FloatToStr(args.AsFloat[0]);
end;

{ TStrToFloatFunc }

procedure TStrToFloatFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=StrToFloat(args.AsString[0]);
end;

{ TStrToFloatDefFunc }

procedure TStrToFloatDefFunc.DoEvalAsFloat(args : TExprBaseList; var Result : Double);
begin
   Result:=StrToFloatDef(args.AsString[0], args.AsFloat[1]);
end;

{ TCopyFunc }

// DoEvalAsString
//
procedure TCopyFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=Copy(args.AsString[0], args.AsInteger[1], args.AsInteger[2]);
end;

{ TLeftStrFunc }

// DoEvalAsString
//
procedure TLeftStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=Copy(args.AsString[0], 1, args.AsInteger[1]);
end;

{ TRightStrFunc }

// DoEvalAsString
//
procedure TRightStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   buf : String;
   n : Integer;
begin
   buf:=args.AsString[0];
   n:=args.AsInteger[1];
   Result:=Copy(buf, Length(buf)+1-n, n);
end;

{ TSubStrFunc }

// DoEvalAsString
//
procedure TSubStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=Copy(args.AsString[0], args.AsInteger[1], MaxInt);
end;

{ TSubStringFunc }

// DoEvalAsString
//
procedure TSubStringFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   s, e : Integer;
begin
   s:=args.AsInteger[1];
   e:=args.AsInteger[2];
   if s<1 then s:=1;
   Result:=Copy(args.AsString[0], s, e-s);
end;

{ TDeleteFunc }

procedure TDeleteFunc.Execute;
var
  s: string;
begin
  s := Info.ValueAsString['S'];
  Delete(s, Info.ValueAsInteger['Index'], Info.ValueAsInteger['Len']);
  Info.ValueAsString['S'] := s;
end;

{ TInsertFunc }

procedure TInsertFunc.Execute;
var
  s: string;
begin
  s := Info.ValueAsString['S'];
  Insert(Info.ValueAsString['src'], s, Info.ValueAsInteger['Index']);
  Info.ValueAsString['S'] := s;
end;

{ TLowerCaseFunc }

// DoEvalAsString
//
procedure TLowerCaseFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=LowerCase(args.AsString[0]);
end;

{ TAnsiLowerCaseFunc }

// DoEvalAsString
//
procedure TAnsiLowerCaseFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=AnsiLowerCase(args.AsString[0]);
end;

{ TUpperCaseFunc }

// DoEvalAsString
//
procedure TUpperCaseFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=UpperCase(args.AsString[0]);
end;

{ TAnsiUpperCaseFunc }

// DoEvalAsString
//
procedure TAnsiUpperCaseFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=AnsiUpperCase(args.AsString[0]);
end;

{ TPosFunc }

function TPosFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=Pos(args.AsString[0], args.AsString[1]);
end;

{ TPosExFunc }

function TPosExFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=PosEx(args.AsString[0], args.AsString[1], args.AsInteger[2]);
end;

{ TRevPosFunc }

function TRevPosFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;

   function StrRevFind(const stringSearched, stringToFind : String; startPos : Integer = 0) : Integer;
   var
      i : Integer;
   begin
      if (stringToFind='') or (stringSearched='') then begin
         Result:=0;
         Exit;
      end;
      if startPos<=0 then
         startPos:=Length(stringSearched);
      for i:=startPos-Length(stringToFind)+1 downto 1 do begin
         if stringSearched[i]=stringToFind[1] then begin
            if CompareMem(@stringSearched[i], @stringToFind[1], Length(stringToFind)*SizeOf(Char)) then begin
               Result:=i;
               Exit;
            end;
         end;
      end;
      Result:=0;
   end;

begin
   Result:=StrRevFind(args.AsString[1], args.AsString[0]);
end;

{ TTrimLeftFunc }

// DoEvalAsString
//
procedure TTrimLeftFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=TrimLeft(args.AsString[0]);
end;

{ TTrimRightFunc }

// DoEvalAsString
//
procedure TTrimRightFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=TrimRight(args.AsString[0]);
end;

{ TTrimFunc }

// DoEvalAsString
//
procedure TTrimFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=Trim(args.AsString[0]);
end;

{ TSameTextFunc }

function TSameTextFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
begin
   Result:=SameText(args.AsString[0], args.AsString[1]);
end;

{ TCompareTextFunc }

function TCompareTextFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=CompareText(args.AsString[0], args.AsString[1]);
end;

{ TAnsiCompareTextFunc }

function TAnsiCompareTextFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=AnsiCompareText(args.AsString[0], args.AsString[1]);
end;

{ TCompareStrFunc }

function TCompareStrFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=CompareStr(args.AsString[0], args.AsString[1]);
end;

{ TAnsiCompareStrFunc }

function TAnsiCompareStrFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=AnsiCompareStr(args.AsString[0], args.AsString[1]);
end;

{ TIsDelimiterFunc }

function TIsDelimiterFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
begin
   Result:=IsDelimiter(args.AsString[0], args.AsString[1], args.AsInteger[2]);
end;

{ TLastDelimiterFunc }

function TLastDelimiterFunc.DoEvalAsInteger(args : TExprBaseList) : Int64;
begin
   Result:=LastDelimiter(args.AsString[0], args.AsString[1]);
end;

{ TQuotedStrFunc }

// DoEvalAsString
//
procedure TQuotedStrFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
begin
   Result:=QuotedStr(args.AsString[0]);
end;

{ TCharAtFunc }

// DoEvalAsString
//
procedure TCharAtFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   buf : String;
   n : Integer;
begin
   buf:=args.AsString[0];
   n:=args.AsInteger[1];
   if (n>0) and (n<=Length(buf)) then
      Result:=buf[n]
   else Result:='';
end;

{ TSetCharAtFunc }

procedure TSetCharAtFunc.Execute;
var
   buf : String;
   n : Integer;
begin
   buf:=Info.ValueAsString['s'];
   n:=Info.ValueAsInteger['x'];
   if n<=0 then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded, [n])
   else if n>Length(buf) then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded, [n])
   else begin
      buf[n]:=Info.ValueAsChar['c'];
      Info.ValueAsString['s']:=buf;
   end;
end;

{ TSetLengthFunc }

procedure TSetLengthFunc.Execute;
var
   i, n : Integer;
   s : String;
begin
   //procedure SetLength(var S : String; NewLength : Integer);
   s := Info.ValueAsString['S'];

   i:=Length(s)+1;
   n:=Info.ValueAsInteger['NewLength'];
   SetLength(s, n);
   while i<=n do begin
      s[i]:=' ';
      Inc(i);
   end;

   Info.ValueAsString['S'] := s;              // re-assign 'var' value
end;

{ TStringOfCharFunc }

// DoEvalAsString
//
procedure TStringOfCharFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   ch : String;
begin
   ch:=args.AsString[0];
   if Length(ch)<1 then
      Result:=StringOfChar(' ', args.AsInteger[1]) // default to blank if an empty string
   else Result:=StringOfChar(ch[1], args.AsInteger[1]);
end;

{ TStringOfStringFunc }

// DoEvalAsString
//
procedure TStringOfStringFunc.DoEvalAsString(args : TExprBaseList; var Result : String);

   function StringOfString(const str : String; count : Integer) : String;
   var
      ls : Integer;
   begin
      if (str='') or (count<=0) then Exit('');
      ls:=Length(str);
      count:=ls*count;
      SetLength(Result, count);
      while count>0 do begin
         Dec(count, ls);
         Move(str[1], Result[count+1], ls*SizeOf(Char));
      end;
   end;

begin
   Result:=StringOfString(args.AsString[0], args.AsInteger[1]);
end;

{ TStrBeginsWithFunc }

function TStrBeginsWithFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
var
   str, beginStr : String;
begin
   str:=args.AsString[0];
   beginStr:=args.AsString[1];
   if Length(str)<Length(beginStr) then
      Result:=False
   else begin
      Result:=CompareMem(PChar(str), PChar(beginStr),
                         Length(beginStr)*SizeOf(Char));
   end;
end;

{ TStrEndsWithFunc }

function TStrEndsWithFunc.DoEvalAsBoolean(args : TExprBaseList) : Boolean;
var
   str, endStr : String;
begin
   str:=args.AsString[0];
   endStr:=args.AsString[1];
   if Length(str)<Length(endStr) then
      Result:=False
   else begin
      Result:=CompareMem(@str[Length(str)-Length(endStr)+1], PChar(endStr),
                         Length(endStr)*SizeOf(Char));
   end;
end;

{ TStrAfterFunc }

// DoEvalAsString
//
procedure TStrAfterFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   p : Integer;
   str, delimiter : String;
begin
   str:=args.AsString[0];
   delimiter:=args.AsString[1];
   p:=Pos(delimiter, str);
   if p>0 then
      Result:=Copy(str, p+Length(delimiter), MaxInt)
   else Result:='';
end;

{ TStrBeforeFunc }

// DoEvalAsString
//
procedure TStrBeforeFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   p : Integer;
   str, delimiter : String;
begin
   str:=args.AsString[0];
   delimiter:=args.AsString[1];
   p:=Pos(delimiter, str);
   if p>0 then
      Result:=Copy(str, 1, p-1)
   else Result:=str;
end;

{ TFormatFunc }

// DoEvalAsString
//
procedure TFormatFunc.DoEvalAsString(args : TExprBaseList; var Result : String);
var
   expr : TExprBase;
   varRecs : TVarRecArrayContainer;
begin
   varRecs:=nil;
   expr:=args.ExprBase[1];
   if expr is TArrayConstantExpr then
      varRecs:=TArrayConstantExpr(expr).EvalAsVarRecArray
   else if expr is TVarParamExpr then begin
      if TVarParamExpr(expr).Typ is TOpenArraySymbol then
         varRecs:=TVarRecArrayContainer.Create(TVarParamExpr(expr).Data)
   end;
   // current implementation, limitations may be relaxed later
   if varRecs=nil then
      raise EScriptException.Create('Constant expression or open array expected');
   try
      Result:=Format(args.AsString[0], varRecs.VarRecArray);
   finally
      varRecs.Free;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterInternalStringFunction(TChrFunc, 'Chr', ['i', cInteger], True);

   RegisterInternalStringFunction(TIntToStrFunc, 'IntToStr', ['i', cInteger], True);
   RegisterInternalIntFunction(TStrToIntFunc, 'StrToInt', ['str', cString], True);
   RegisterInternalIntFunction(TStrToIntDefFunc, 'StrToIntDef', ['str', cString, 'def', cInteger], True);
   RegisterInternalIntFunction(TStrToIntDefFunc, 'VarToIntDef', ['val', cVariant, 'def', cInteger], True);

   RegisterInternalStringFunction(TIntToHexFunc, 'IntToHex', ['v', cInteger, 'digits', cInteger], True);
   RegisterInternalIntFunction(THexToIntFunc, 'HexToInt', ['hexa', cString], True);

   RegisterInternalStringFunction(TFloatToStrFunc, 'FloatToStr', ['f', cFloat], True);
   RegisterInternalFloatFunction(TStrToFloatFunc, 'StrToFloat', ['str', cString], True);
   RegisterInternalFloatFunction(TStrToFloatDefFunc, 'StrToFloatDef', ['str', cString, 'def', cFloat], True);
   RegisterInternalFloatFunction(TStrToFloatDefFunc, 'VarToFloatDef', ['val', cVariant, 'def', cFloat], True);

   RegisterInternalStringFunction(TFormatFunc, 'Format', ['fmt', cString, 'args', 'array of const'], True);

   RegisterInternalStringFunction(TCharAtFunc, 'CharAt', ['s', cString, 'x', cInteger], True);
   RegisterInternalFunction(TSetCharAtFunc, 'SetCharAt', ['@s', cString, 'x', cInteger, 'c', cString], '');

   RegisterInternalFunction(TDeleteFunc, 'Delete', ['@S', cString, 'index', cInteger, 'Len', cInteger], '');
   RegisterInternalFunction(TInsertFunc, 'Insert', ['src', cString, '@S', cString, 'index', cInteger], '');

   RegisterInternalStringFunction(TLowerCaseFunc, 'LowerCase', ['str', cString]);
   RegisterInternalStringFunction(TAnsiLowerCaseFunc, 'AnsiLowerCase', ['str', cString]);
   RegisterInternalStringFunction(TUpperCaseFunc, 'UpperCase', ['str', cString]);
   RegisterInternalStringFunction(TAnsiUpperCaseFunc, 'AnsiUpperCase', ['str', cString]);

   RegisterInternalIntFunction(TPosFunc, 'Pos', ['subStr', cString, 'str', cString], True);
   RegisterInternalIntFunction(TPosExFunc, 'PosEx', ['subStr', cString, 'str', cString, 'offset', cInteger], True);
   RegisterInternalIntFunction(TRevPosFunc, 'RevPos', ['subStr', cString, 'str', cString], True);

   RegisterInternalFunction(TSetLengthFunc, 'SetLength', ['@S', cString, 'NewLength', cInteger], '');

   RegisterInternalStringFunction(TTrimLeftFunc, 'TrimLeft', ['str', cString], True);
   RegisterInternalStringFunction(TTrimRightFunc, 'TrimRight', ['str', cString], True);
   RegisterInternalStringFunction(TTrimFunc, 'Trim', ['str', cString], True);

   RegisterInternalBoolFunction(TSameTextFunc, 'SameText', ['str1', cString, 'str2', cString], True);
   RegisterInternalIntFunction(TCompareTextFunc, 'CompareText', ['str1', cString, 'str2', cString], True);
   RegisterInternalIntFunction(TAnsiCompareTextFunc, 'AnsiCompareText', ['str1', cString, 'str2', cString], True);
   RegisterInternalIntFunction(TCompareStrFunc, 'CompareStr', ['str1', cString, 'str2', cString], True);
   RegisterInternalIntFunction(TAnsiCompareStrFunc, 'AnsiCompareStr', ['str1', cString, 'str2', cString], True);

   RegisterInternalBoolFunction(TIsDelimiterFunc, 'IsDelimiter', ['delims', cString, 'str', cString, 'index', cInteger], True);
   RegisterInternalIntFunction(TLastDelimiterFunc, 'LastDelimiter', ['delims', cString, 'str', cString], True);

   RegisterInternalStringFunction(TQuotedStrFunc, 'QuotedStr', ['str', cString], True);

   RegisterInternalStringFunction(TCopyFunc, 'Copy', ['str', cString, 'index', cInteger, 'Len', cInteger], True);

   RegisterInternalStringFunction(TLeftStrFunc, 'LeftStr', ['str', cString, 'count', cInteger], True);
   RegisterInternalStringFunction(TRightStrFunc, 'RightStr', ['str', cString, 'count', cInteger], True);
   RegisterInternalStringFunction(TCopyFunc, 'MidStr', ['str', cString, 'start', cInteger, 'count', cInteger], True);
   RegisterInternalStringFunction(TSubStrFunc, 'SubStr', ['str', cString, 'start', cInteger], True);
   RegisterInternalStringFunction(TSubStringFunc, 'SubString', ['str', cString, 'start', cInteger, 'end', cInteger], True);

   RegisterInternalStringFunction(TStringOfCharFunc, 'StringOfChar', ['ch', cString, 'count', cInteger], False);
   RegisterInternalStringFunction(TStringOfStringFunc, 'StringOfString', ['str', cString, 'count', cInteger], False);

   RegisterInternalBoolFunction(TStrBeginsWithFunc, 'StrBeginsWith', ['str', cString, 'beginStr', cString], True);
   RegisterInternalBoolFunction(TStrEndsWithFunc, 'StrEndsWith', ['str', cString, 'endStr', cString], True);

   RegisterInternalStringFunction(TStrAfterFunc, 'StrAfter', ['str', cString, 'delimiter', cString], True);
   RegisterInternalStringFunction(TStrBeforeFunc, 'StrBefore', ['str', cString, 'delimiter', cString], True);

end.
