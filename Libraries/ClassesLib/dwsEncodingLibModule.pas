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
unit dwsEncodingLibModule;

{$I dws.inc}
{$R-}

interface

uses
  SysUtils, Classes,
  dwsExprs, dwsComp, dwsWebUtils, dwsUtils,
  SynCommons;

type
  TdwsEncodingLib = class(TDataModule)
    dwsEncoding: TdwsUnit;
    procedure dwsEncodingClassesUTF8EncoderMethodsEncodeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsEncodingClassesUTF8EncoderMethodsDecodeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsEncodingClassesURLEncodedEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesURLEncodedEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase64EncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase64EncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLTextEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase58EncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase58EncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLAttributeEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLAttributeEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase32EncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase32EncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16BigEndianEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16BigEndianEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16LittleEndianEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16LittleEndianEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Base58Encode(const data : RawByteString) : String;
function Base58Decode(const data : String) : RawByteString;

// RFC 4648 without padding
function Base32Encode(data : Pointer; len : Integer) : String; overload;
function Base32Encode(const data : RawByteString) : String; overload;
function Base32Decode(const data : String) : RawByteString;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

const
   cBase58 : String = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

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
   Result := UInt64(cMagic)*v shr 37;
   v := v - Result*58;
{$endif}
end;

function Base58Encode(const data : RawByteString) : String;
var
   i, j, carry, nextCarry, n : Integer;
   digits : array of Integer;
begin
   if data = '' then exit;

   Result := '';
   n := -1;
   for j := 1 to Length(data) do begin
      if data[j] = #0 then
         Result := Result + cBase58[1]
      else begin
         SetLength(digits, 1);
         digits[0] := 0;
         n := 0;
         break;
      end;
   end;

   for i := Length(Result)+1 to Length(data) do begin

      digits[0] := (digits[0] shl 8) + Ord(data[i]);
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


procedure TdwsEncodingLib.dwsEncodingClassesBase32EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=Base32Decode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase32EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Base32Encode(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase58EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := Base58Decode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase58EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := Base58Encode(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := Base64ToBin(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := BinToBase64(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := dwsUtils.HexToBin(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := dwsUtils.BinToHex(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLAttributeEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLAttributeDecode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLAttributeEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLAttributeEncode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLTextDecode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLTextEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLTextEncode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesURLEncodedEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : RawByteString;
begin
   buf := Info.ParamAsDataString[0];
   Info.ResultAsString := WebUtils.DecodeURLEncoded(buf, 1, Length(buf));
end;

procedure TdwsEncodingLib.dwsEncodingClassesURLEncodedEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.EncodeURLEncoded(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16BigEndianEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringWordsToBytes(buf, True);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16BigEndianEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringBytesToWords(buf, True);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16LittleEndianEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringWordsToBytes(buf, False);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16LittleEndianEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringBytesToWords(buf, False);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF8EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := UTF8ToUnicodeString(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF8EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := UnicodeStringToUtf8(Info.ParamAsString[0]);
end;

end.
