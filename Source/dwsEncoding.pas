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
unit dwsEncoding;

{$I dws.inc}
{$R-}

interface

uses SysUtils;

type
   TBase64Alphabet = array [0..63] of Char;

function Base58Encode(const data : RawByteString) : String; overload; inline;
function Base58Encode(data : Pointer; len : Integer) : String; overload;
function Base58Decode(const data : String) : RawByteString;

// RFC 4648 without padding
function Base32Encode(const data : RawByteString) : String; overload; inline;
function Base32Encode(data : Pointer; len : Integer) : String; overload;
function Base32Decode(const data : String) : RawByteString;

function Base64Encode(const data : RawByteString) : String; overload; inline;
function Base64Encode(data : Pointer; len : Integer) : String; overload; inline;
function Base64Encode(data : Pointer; len : Integer; const alphabet : TBase64Alphabet) : String; overload;
function Base64EncodeToBytes(data : Pointer; len : Integer; const alphabet : TBase64Alphabet) : TBytes;
function Base64Decode(const data : String) : RawByteString;

function Base64EncodeURI(const data : RawByteString) : String; overload; inline;
function Base64EncodeURI(data : Pointer; len : Integer) : String; overload; inline;
function Base64DecodeURI(const data : String) : RawByteString;

const
   cBase58 : String = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
   cBase64 : TBase64Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
   cBase64URI : TBase64Alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

function DivBy58(var v : Integer) : Integer;
const
   cMagic : Cardinal = 2369637129;
{$ifdef WIN32_ASM}
asm
   mov ecx, eax
   mov eax, dword ptr [eax]
   mul eax, cMagic
   mov eax, edx
   shr eax, 5
   mov edx, eax
   imul edx, 58
   sub dword ptr [ecx], edx
{$else}
begin
   Result := UInt64(cMagic)*UInt64(v) shr 37;
   v := v - Result*58;
{$endif}
end;

// Base58Encode
//
function Base58Encode(const data : RawByteString) : String;
begin
   Result := Base58Encode(Pointer(data), Length(data));
end;

// Base58Encode
//
function Base58Encode(data : Pointer; len : Integer) : String;
var
   i, j, carry, nextCarry, n : Integer;
   digits : array of Integer;
begin
   if len <= 0 then exit;

   Result := '';
   n := -1;
   for j := 1 to len do begin
      if PByte(data)[j-1] = 0 then
         Result := Result + cBase58[1]
      else begin
         SetLength(digits, 1);
         digits[0] := 0;
         n := 0;
         break;
      end;
   end;

   for i := Length(Result) to len-1 do begin

      digits[0] := (digits[0] shl 8) + PByte(data)[i];
      carry := DivBy58(digits[0]);

      for j := 1 to n do begin
         digits[j] := (digits[j] shl 8) + carry;
         carry := DivBy58(digits[j]);
      end;

      while carry > 0 do begin
         Inc(n);
         SetLength(digits, n+1);
         nextCarry := carry div 58;
         digits[n] := carry - nextCarry*58;
         carry := nextCarry;
      end;
   end;

   for j := n downto 0 do
      Result := Result + cBase58[digits[j]+1];
end;

// Base58Decode
//
function Base58Decode(const data : String) : RawByteString;
var
   i, j, carry, n, d : Integer;
   bytes : array of Integer;
begin
   if data = '' then exit;

   Result := '';
   n := -1;
   for j := 1 to Length(data) do begin
      if data[j] = '1' then
         Result := Result + #0
      else begin
         SetLength(bytes, 1);
         bytes[0] := 0;
         n := 0;
         break;
      end;
   end;

   for i := Length(Result)+1 to Length(data) do begin
      d := Pos(data[i], cBase58)-1;
      if d<0 then
         raise Exception.Create('Non-base58 character');

      for j := 0 to n do
         bytes[j] := bytes[j]*58;

      bytes[0] := bytes[0]+d;

      carry := 0;
      for j := 0 to n do begin
         bytes[j] := bytes[j] + carry;
         carry := bytes[j] shr 8;
         bytes[j] := bytes[j] and $FF;
      end;

      while carry > 0 do begin
         Inc(n);
         SetLength(bytes, n+1);
         bytes[n] := carry and $FF;
         carry := carry shr 8;
      end;
   end;

   for j := n downto 0 do
      Result := Result + AnsiChar(bytes[j]);
end;

const
   cBase32 : array [0..31] of Char = (
      'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
      'Q','R','S','T','U','V','W','X','Y','Z','2','3','4','5','6','7'
   );

function Base32Encode(data : Pointer; len : Integer) : String;
var
   i, n, c, b : Integer;
   pIn : PByteArray;
   pOut : PChar;
begin
   if (len = 0) or (data = nil) then Exit('');
   n := len;
   SetLength(Result, ((n div 5)+1)*8);
   c := 0;
   b := 0;
   pIn := data;
   pOut := Pointer(Result);
   for i := 0 to n-1 do begin
      c := (c shl 8) or pIn[i];
      Inc(b, 8);
      while b >= 5 do begin
         Dec(b, 5);
         pOut^ := cBase32[(c shr b) and $1F];
         Inc(pOut);
      end;
   end;
   if b > 0 then begin
      pOut^ := cBase32[(c shl (5-b)) and $1F];
      Inc(pOut);
   end;
   n := (NativeUInt(pOut)-NativeUInt(Pointer(Result))) div SizeOf(Char);
   SetLength(Result, n);
end;

// Base32Encode
//
function Base32Encode(const data : RawByteString) : String;
begin
   Result:=Base32Encode(Pointer(data), Length(data));
end;

// Base32Decode
//
var
   vBase32DecodeTable : array [#0..'z'] of Byte;
function Base32Decode(const data : String) : RawByteString;

   procedure PrepareTable;
   var
      c : Char;
   begin
      for c := #0 to High(vBase32DecodeTable) do begin
         case c of
            'A'..'Z' : vBase32DecodeTable[c] := Ord(c)-Ord('A');
            'a'..'z' : vBase32DecodeTable[c] := Ord(c)-Ord('a');
            '2'..'7' : vBase32DecodeTable[c] := Ord(c)+(26-Ord('2'));
            '0' : vBase32DecodeTable[c] := Ord('O')-Ord('A');
         else
            vBase32DecodeTable[c] := 255;
         end;
      end;
   end;

var
   c, b, i, n, d : Integer;
   pIn : PChar;
   pOut : PByte;
begin
   if data = '' then Exit('');
   if vBase32DecodeTable['z']=0 then PrepareTable;

   n := Length(data);
   SetLength(Result, ((n div 8)+1)*5);
   pIn := Pointer(data);
   pOut := Pointer(Result);
   c := 0;
   b := 0;
   for i := 0 to n-1 do begin
      d := vBase32DecodeTable[pIn[i]];
      if d = 255 then begin
         if pIn[i] = '=' then break;
         raise Exception.CreateFmt('Invalid character (#%d) in Base32', [Ord(pIn[i])]);
      end;
      c := (c shl 5) or d;
      Inc(b, 5);
      if b >= 8 then begin
         Dec(b, 8);
         pOut^ := c shr b;
         Inc(pOut);
      end;
   end;
   n := NativeUInt(pOut)-NativeUInt(Pointer(Result));
   SetLength(Result, n);
end;

type
   TBase64Block = record
      v0, skip0 : Byte;
      v1, skip1 : Byte;
      v2, skip2 : Byte;
      v3, skip3 : Byte;
   end;
   PBase64Block = ^TBase64Block;

var
   vBase64Decode : array [0..255] of Integer;

// PrepareBase64DecodeTable
//
procedure PrepareBase64DecodeTable;
var
   i : Integer;
begin
   // < 0 is for non-base64 characters
   // -1 is for invalid characters
   // -2 is for characters that are allowed (and ignored) between blocks
   // table should be filled sequentially so preparation does not need a threading lock
   for i := 0 to High(vBase64Decode) do begin
      case Char(i) of
         #1..#32 : vBase64Decode[i] := -2;
         'A'..'Z' : vBase64Decode[i] := i - Ord('A');
         'a'..'z' : vBase64Decode[i] := i + (26 - Ord('a'));
         '0'..'9' : vBase64Decode[i] := i + (52 - Ord('0'));
         '+', '-' : vBase64Decode[i] := 62;
         '/', '_' : vBase64Decode[i] := 63;
      else
         vBase64Decode[i] := -1;
      end;
   end;
end;

// Base64Encode
//
function Base64Encode(const data : RawByteString) : String;
begin
   Result := Base64Encode(Pointer(data), Length(data), cBase64);
end;

// Base64Encode
//
function Base64Encode(data : Pointer; len : Integer) : String;
begin
   Result := Base64Encode(data, len, cBase64);
end;

// Base64Encode
//
function Base64Encode(data : Pointer; len : Integer; const alphabet : TBase64Alphabet) : String; overload;
var
   outLen, blocks, tail, i : Integer;
   dest : PChar;
   src : PByte;
   c : Cardinal;
begin
   outLen := ((len+2) div 3)*4;
   SetLength(Result, outLen);
   if outLen = 0 then Exit;
   blocks := len div 3;
   tail := len - blocks*3;
   dest := Pointer(Result);
   src := PByte(data);
   for i := 1 to blocks do begin
      c := (src[0] shl 16) + (src[1] shl 8) + src[2];
      dest[0] := alphabet[(c shr 18) and $3f];
      dest[1] := alphabet[(c shr 12) and $3f];
      dest[2] := alphabet[(c shr 6) and $3f];
      dest[3] := alphabet[c and $3f];
      Inc(dest, 4);
      Inc(src, 3);
   end;
   case tail of
      1 : begin
         c := src[0] shl 4;
         dest[0] := alphabet[(c shr 6) and $3f];
         dest[1] := alphabet[c and $3f];
         dest[2] := '=';
         dest[3] := '=';
      end;
      2 : begin
         c := (src[0] shl 10) + (src[1] shl 2);
         dest[0] := alphabet[(c shr 12) and $3f];
         dest[1] := alphabet[(c shr 6) and $3f];
         dest[2] := alphabet[c and $3f];
         dest[3] := '=';
      end;
   end;
end;

// Base64EncodeToBytes
//
function Base64EncodeToBytes(data : Pointer; len : Integer; const alphabet : TBase64Alphabet) : TBytes;
var
   outLen, blocks, tail, i : Integer;
   dest : PByte;
   src : PByte;
   c : Cardinal;
begin
   outLen := ((len+2) div 3)*4;
   SetLength(Result, outLen);
   if outLen = 0 then Exit;
   blocks := len div 3;
   tail := len - blocks*3;
   dest := Pointer(Result);
   src := PByte(data);
   for i := 1 to blocks do begin
      c := (src[0] shl 16) + (src[1] shl 8) + src[2];
      dest[0] := Ord(alphabet[(c shr 18) and $3f]);
      dest[1] := Ord(alphabet[(c shr 12) and $3f]);
      dest[2] := Ord(alphabet[(c shr 6) and $3f]);
      dest[3] := Ord(alphabet[c and $3f]);
      Inc(dest, 4);
      Inc(src, 3);
   end;
   case tail of
      1 : begin
         c := src[0] shl 4;
         dest[0] := Ord(alphabet[(c shr 6) and $3f]);
         dest[1] := Ord(alphabet[c and $3f]);
         dest[2] := Ord('=');
         dest[3] := Ord('=');
      end;
      2 : begin
         c := (src[0] shl 10) + (src[1] shl 2);
         dest[0] := Ord(alphabet[(c shr 12) and $3f]);
         dest[1] := Ord(alphabet[(c shr 6) and $3f]);
         dest[2] := Ord(alphabet[c and $3f]);
         dest[3] := Ord('=');
      end;
   end;
end;

// Base64Decode
//
function Base64Decode(const data : String) : RawByteString;

   function Base64Decode(src, srcLast : PBase64Block; dest : PByte) : PByte;
   var
      c, ch : Integer;
   begin
      c := 0;
      while NativeUInt(src) <= NativeUInt(srcLast) do begin
         ch := vBase64Decode[src.v0];
         if ch >= 0 then begin
            c := ch shl 6;
            ch := vBase64Decode[src.v1];
            if ch >= 0 then begin
               c := (c or ch) shl 6;
               ch := vBase64Decode[src.v2];
               if ch >= 0 then begin
                  c := (c or ch) shl 6;
                  ch := vBase64Decode[src.v3];
                  if ch >= 0 then begin
                     c := c or ch;
                     dest[2] := c;
                     c := c shr 8;
                     dest[1] := c;
                     c := c shr 8;
                     dest[0] := c;
                     Inc(dest,3);
                     Inc(src);
                     Continue;
                  end else begin
                     c := c shr 8;
                     dest[1] := c;
                     dest[0] := c shr 8;
                     Inc(dest, 2);
                     Break;
                  end;
               end;
            end;
         end else if ch = -2 then begin
            src := @PChar(src)[1];
            Continue;
         end;
         dest[0] := c shr 10;
         Inc(dest);
         Break;
      end;
      Result := dest;
   end;

var
   len, outLen : Integer;
   decodedPtr : PByte;
begin
   if data = '' then Exit;
   if vBase64Decode[High(vBase64Decode)] = 0 then
      PrepareBase64DecodeTable;
   len := Length(data);
   while (data[len] <= ' ') and (len > 1) do Dec(len);
   outLen := (len shr 2)*3;
   if data[len] = '=' then begin
      if (len > 1) and (data[len-1] = '=') then
         Dec(outLen, 2)
      else Dec(outLen);
   end;
   SetLength(Result, outLen);
   decodedPtr := Base64Decode(Pointer(data), Pointer(@data[len-3]), Pointer(Result));
   if decodedPtr <> @PByte(Result)[outLen] then
      SetLength(Result, NativeUInt(decodedPtr) - NativeUInt(Pointer(Result)));
end;

// Base64EncodeURI
//
function Base64EncodeURI(const data : RawByteString) : String;
begin
   Result := Base64EncodeURI(Pointer(data), Length(data));
end;

// Base64DecodeURI
//
function Base64DecodeURI(const data : String) : RawByteString;
var
   n : Integer;
begin
   n := Length(data);
   case n and 3 of
      1 : Result := Base64Decode(data+'=');
      2 : Result := Base64Decode(data+'==');
      3 : Result := Base64Decode(data+'===');
   else
      Result := Base64Decode(data);
   end;
end;

// Base64EncodeURI
//
function Base64EncodeURI(data : Pointer; len : Integer) : String;
var
   n : Integer;
begin
   Result := Base64Encode(data, len, cBase64URI);
   len := Length(Result);
   n := len;
   while (n > 0) and (Result[n] = '=') do
      Dec(n);
   if n <> len then
      SetLength(Result, n);
end;

end.
