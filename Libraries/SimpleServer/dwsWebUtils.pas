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
   Classes,
   dwsUtils;

type

   WebUtils = class
      public
         class procedure ParseURLEncoded(const data : RawByteString; dest : TStrings); static;
         class function DecodeURLEncoded(const src : RawByteString; start, count : Integer) : String; overload; static;
         class function DecodeURLEncoded(const src : RawByteString; start : Integer) : String; overload; static;
         class function DecodeHex2(p : PAnsiChar) : Integer; static;
         class function HasFieldName(const list : TStrings; const name : String) : Boolean; static;

         class function EncodeEncodedWord(const s : String) : String; static;
   end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

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
         end else if data[next]='&' then
            break
         else Inc(next);
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
const
   cToHex : String = '0123456789ABCDEF';
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

end.
