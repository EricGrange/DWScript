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
unit dwsWebUtils;

interface

uses
   Classes, SysUtils,
   dwsUtils;

type

   WebUtils = class
      public
         class procedure ParseURLEncoded(const data : RawByteString; dest : TStrings); static;
         class function DecodeURLEncoded(const src : RawByteString; start, count : Integer) : String; overload; static;
         class function DecodeURLEncoded(const src : RawByteString; start : Integer) : String; overload; static;
         class function EncodeURLEncoded(const src : String) : String; static;

         class function DecodeHex2(p : PAnsiChar) : Integer; static;
         class function HasFieldName(const list : TStrings; const name : String) : Boolean; static;

         class function EncodeEncodedWord(const s : String) : String; static;

         class function DateTimeToRFC822(const dt : TDateTime) : String; static;
         class function RFC822ToDateTime(const str : String) : TDateTime; static;

         class function HTMLTextEncode(const s : String) : String; static;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cToHex : String = '0123456789ABCDEF';

// ------------------
// ------------------ WebUtils ------------------
// ------------------

// ParseURLEncoded
//
class procedure WebUtils.ParseURLEncoded(const data : RawByteString; dest : TStrings);
var
   base, next, last : Integer;
begin
   last:=Length(data);
   base:=1;
   while True do begin
      next:=base;
      repeat
         if next>last then begin
            next:=-1;
            break;
         end else case data[next] of
            '&', ';' :
               break
         else
            Inc(next);
         end;
      until False;
      if next>base then begin
         dest.Add(DecodeURLEncoded(data, base, next-base));
         base:=next+1;
      end else begin
         if base<Length(data) then
            dest.Add(DecodeURLEncoded(data, base));
         Break;
      end;
   end;
end;

// DecodeURLEncoded
//
class function WebUtils.DecodeURLEncoded(const src : RawByteString; start, count : Integer) : String;
var
   raw : UTF8String;
   pSrc, pDest : PAnsiChar;
   c : AnsiChar;
begin
   SetLength(raw, count);
   pSrc:=@src[start];
   pDest:=PAnsiChar(Pointer(raw));
   while count>0 do begin
      Dec(count);
      c:=AnsiChar(pSrc^);
      case c of
         '+' :
            pDest^:=' ';
         '%' : begin
            if count<2 then break;
            pDest^:=AnsiChar(DecodeHex2(@pSrc[1]));
            Inc(pSrc, 2);
            Dec(count, 2);
         end;
      else
         pDest^:=c;
      end;
      Inc(pDest);
      Inc(pSrc);
   end;
   SetLength(raw, NativeUInt(pDest)-NativeUInt(Pointer(raw)));
   Result:=UTF8ToUnicodeString(raw);
end;

// EncodeURLEncoded
//
class function WebUtils.EncodeURLEncoded(const src : String) : String;
var
   raw : UTF8String;
   pSrc : PAnsiChar;
   pDest : PChar;
begin
   if src='' then Exit('');

   raw := UTF8Encode(src);
   SetLength(Result, Length(src)*3); // worst-case all special chars

   pSrc := Pointer(raw);
   pDest := Pointer(Result);

   // we are slightly more aggressive on the special characters than strictly required
   repeat
      case pSrc^ of
         #0 : break;
         #1..'/',  '['..']', ':'..'@' : begin
            pDest[0] := '%';
            pDest[1] := cToHex[1+(Ord(pSrc^) shr 4)];
            pDest[2] := cToHex[1+(Ord(pSrc^) and 15)];
            Inc(pDest, 3);
         end;
      else
         pDest^ := Char(pSrc^);
         Inc(pDest);
      end;
      Inc(pSrc);
   until False;

   SetLength(Result, (NativeUInt(PDest)-NativeUInt(Pointer(Result))) div SizeOf(Char));
end;

// DecodeURLEncoded
//
class function WebUtils.DecodeURLEncoded(const src : RawByteString; start : Integer) : String;
var
   n : Integer;
begin
   n:=Length(src)-start+1;
   if n>=0 then
      Result:=DecodeURLEncoded(src, start, n)
   else Result:='';
end;

// DecodeHex2
//
class function WebUtils.DecodeHex2(p : PAnsiChar) : Integer;
var
   c : AnsiChar;
begin
   c:=p[0];
   case c of
      '0'..'9' : Result:=Ord(c)-Ord('0');
      'A'..'F' : Result:=Ord(c)+(10-Ord('A'));
      'a'..'f' : Result:=Ord(c)+(10-Ord('a'));
   else
      Exit(-1);
   end;
   c:=p[1];
   case c of
      '0'..'9' : Result:=(Result shl 4)+Ord(c)-Ord('0');
      'A'..'F' : Result:=(Result shl 4)+Ord(c)+(10-Ord('A'));
      'a'..'f' : Result:=(Result shl 4)+Ord(c)+(10-Ord('a'));
   else
      Exit(-1);
   end;
end;


// HasFieldName
//
class function WebUtils.HasFieldName(const list : TStrings; const name : String) : Boolean;
var
   i, n : Integer;
   elem : String;
begin
   for i:=0 to list.Count-1 do begin
      elem:=list[i];
      if StrBeginsWith(elem, name) then begin
         n:=Length(name);
         if (Length(elem)=n) or (elem[n+1]='=') then
            Exit(True);
      end;
   end;
   Result:=False;
end;

// EncodeEncodedWord
//
class function WebUtils.EncodeEncodedWord(const s : String) : String;
var
   p, n : Integer;
   line : array [0..100] of Char;
   buf : String;
   c : AnsiChar;

   procedure FlushLine;
   begin
      SetString(buf, PChar(@line[0]), p);
      Result:=Result+'=?utf-8?Q?'+buf+'?='#13#10#9;
   end;

begin
   Result:='';
   p:=0;
   n:=0;
   for c in UTF8Encode(s) do begin
      case Ord(c) of
         Ord(' ') : begin
            line[p]:='_';
         end;
         Ord('?'), Ord('='), 128..255 : begin
            line[p]:='=';
            line[p+1]:=cToHex[(Ord(c) shr 4)+1];
            line[p+2]:=cToHex[(Ord(c) and 15)+1];
            Inc(p, 2);
            Inc(n, 2);
         end;
      else
         line[p]:=Char(c);
      end;
      Inc(p);
      if n>64 then begin
         FlushLine;
         p:=0;
         n:=0;
      end else Inc(n);
   end;
   if p>0 then
      FlushLine;
   if StrEndsWith(Result, #13#10#9) then
      SetLength(Result, Length(Result)-3);
end;

const
   cRFC822Months : array[1..12] of String = (
      'Jan','Feb','Mar','Apr', 'May','Jun','Jul','Aug', 'Sep','Oct','Nov','Dec'
   );
   cRFC822Days : array[1..7] of String = (
      'Sun','Mon','Tue','Wed','Thu', 'Fri','Sat'
   );

// DateTimeToRFC822
//
class function WebUtils.DateTimeToRFC822(const dt : TDateTime) : String;
var
   a, m, j, hh, mn, ss, ms : Word;
   dow : Integer;
begin
   DecodeDate(dt, a, m, j);
   DecodeTime(dt, hh, mn, ss, ms);
   dow:=DayOfWeek(dt);
   Result:=Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
                  [cRFC822Days[dow], j, cRFC822Months[m], a, hh, mn, ss]);
end;

// RFC822ToDateTime
//
class function WebUtils.RFC822ToDateTime(const str : String) : TDateTime;
const
   cMaxItems = 6;
type
   TStringArray = array of String;
var
   list : array [0..cMaxItems+1] of String;
   count : Integer;
   y, mo, d : Word;
   h, mi, s : Word;
   deltaHours, deltaDays, p : Integer;
   deltaTime : TDateTime;

   procedure SplitStr(const str : String; const delim : Char; start : Integer);
   var
      lookup : integer;
   begin
      count:=0;
      if str='' then Exit;
      lookup:=start;
      while lookup<=Length(str) do begin
         if str[lookup]=delim then begin
            if lookup>start then begin
               list[count]:=Copy(str, start, lookup-start);
               Inc(count);
               if count>=cMaxItems then break;
            end;
            start:=lookup+1;
         end;
         Inc(lookup);
      end;
      if lookup>start then begin
         list[count]:=Copy(str, start, lookup-start);
         Inc(count);
      end;
   end;

   function ParseTwoDigits(p : PChar; offset : Integer) : Integer; inline;
   begin
      Result:=Ord(p[offset])*10+Ord(p[offset+1])-11*Ord('0')
   end;

   function ParseFourDigits(p : PChar; offset : Integer) : Integer; inline;
   begin
      Result:=ParseTwoDigits(p, 0)*100+ParseTwoDigits(p, 2);
   end;

   procedure ParseHMS(const str : String);
   var
      p : PChar;
   begin
      p:=PChar(Pointer(str));
      h:=65535;
      case Length(str) of
         5 : begin // hh:nn
            if p[2]<>':' then exit;
            h:=ParseTwoDigits(p, 0);
            mi:=ParseTwoDigits(p, 3);
            s:=0;
         end;
         8 : begin // hh:nn:ss
            if p[2]<>':' then exit;
            if p[5]<>':' then exit;
            h:=ParseTwoDigits(p, 0);
            mi:=ParseTwoDigits(p, 3);
            s:=ParseTwoDigits(p, 6);
         end;
      end;
   end;

   procedure ParseYear(const str : String);
   begin
      case Length(str) of
         2 : y:=ParseTwoDigits(Pointer(str), 0)+2000;
         4 : y:=ParseFourDigits(Pointer(str), 0);
      else
         y:=65535;
      end;
   end;

   procedure ParseMonth(const str : String);
   begin
      mo:=1;
      while (mo<=12) and not SameText(str, cRFC822Months[mo]) do
         Inc(mo);
   end;

begin
   Result:=0;
   if str='' then Exit;

   p:=Pos(',', str);
   if p>0 then
      SplitStr(str, ' ', p+1)
   else SplitStr(str, ' ', 1);
   if count<5 then // invalid date
      Exit;
   if (count>5) and (Pos(':', list[4])>0) and StrBeginsWith(list[5], 'GMT+') then begin
      // Thu Oct 08 2009 00:00:00 GMT+0200 (Romance Daylight Time)
      ParseMonth(list[1]);
      if Length(list[2])=2 then
         d:=ParseTwoDigits(Pointer(list[2]), 0)
      else d:=0;
      ParseYear(list[3]);
      ParseHMS(list[4]);
      deltaHours:=0;
      deltaDays:=0;
   end else begin
      // Thu, 08 Oct 2009 00:00:00 GMT
      if Length(list[0])=2 then
         d:=ParseTwoDigits(Pointer(list[0]), 0)
      else d:=0;
      ParseMonth(list[1]);
      ParseYear(list[2]);
      ParseHMS(list[3]);
      deltaHours:=StrToIntDef(list[4], 0);
      deltaDays:=0;
      while h>=24 do begin
         Dec(h, 24);
         Inc(deltaDays);
      end;
   end;
   if not TryEncodeDate(y, mo, d, Result) then
      Result:=0
   else if TryEncodeTime(h, mi, s, 0, deltaTime) then
      Result:=Result+deltaTime-deltaHours*(1/100/24)+deltaDays
   else Result:=0;
end;

// HTMLTextEncode
//
class function WebUtils.HTMLTextEncode(const s : String) : String;
var
   capacity : Integer;
   pSrc, pDest : PChar;

   procedure Grow;
   var
      nr, dnr : Integer;
      k : NativeUInt;
   begin
      k := NativeUInt(pDest)-NativeUInt(Pointer(Result));
      nr := Length(Result);
      dnr := (nr div 4) + 8;
      SetLength(Result, nr + dnr);
      Inc(capacity, dnr);
      pDest := Pointer(NativeUInt(Pointer(Result))+k);
   end;

   procedure Append(const a : String);
   var
      n : Integer;
   begin
      n := Length(a);
      if n>capacity then Grow;
      System.Move(Pointer(a)^, pDest^, n*SizeOf(Char));
      Inc(pDest, n);
      Dec(capacity, n);
   end;

begin
   if s='' then exit;
   capacity:=Length(s);
   SetLength(Result, capacity);
   pSrc:=Pointer(s);
   pDest:=Pointer(Result);
   repeat
      case pSrc^ of
         #0 : break;
         '<' : Append('&lt;');
         '>' : Append('&gt;');
         '&' : Append('&amp;');
         '"' : Append('&quot;');
      else
         if capacity=0 then
            Grow;
         pDest^ := pSrc^;
         Inc(pDest);
         Dec(capacity);
      end;
      Inc(pSrc);
   until False;
   if capacity>0 then
      SetLength(Result, Length(Result)-capacity);
end;

end.
