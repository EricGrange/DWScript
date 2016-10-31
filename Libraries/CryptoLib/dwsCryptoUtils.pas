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
unit dwsCryptoUtils;

interface

uses
   SysUtils, SynCrypto, SynZip,
   dwsRipeMD160, dwsCryptProtect, dwsSHA3, dwsUtils, dwsXPlatform;

type
   THashFunction = function (const data : RawByteString) : RawByteString;

function HMAC(const key, msg : RawByteString; h : THashFunction; blockSize : Integer) : String;

function HashSHA3_256(const data : RawByteString) : RawByteString;
function HashSHA256(const data : RawByteString) : RawByteString;
function HashRIPEMD160(const data : RawByteString) : RawByteString;
function HashSHA1(const data : RawByteString) : RawByteString;
function HashMD5(const data : RawByteString) : RawByteString;
function HashCRC32(const data : RawByteString) : RawByteString;

function AES_SHA3_CTR(const data, key : RawByteString; encrypt : Boolean) : RawByteString;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

function HMAC(const key, msg : RawByteString; h : THashFunction; blockSize : Integer) : String;
var
   n : Integer;
   oPad, iPad : AnsiString;
begin
   n:=Length(key);
   if n>blockSize then begin
      // shorten
      iPad:=h(key);
      n:=Length(iPad);
   end else iPad:=key;

   if n<blockSize then begin
      // zero pad to the right
      SetLength(iPad, blockSize);
      FillChar(iPad[n+1], blockSize-n, 0);
   end;
   oPad:=iPad;
   UniqueString(iPad);
   UniqueString(oPad);

   for n:=1 to blockSize do begin
      oPad[n]:=AnsiChar(Ord(oPad[n]) xor $5C);
      iPad[n]:=AnsiChar(Ord(iPad[n]) xor $36);
   end;

   Result := BinToHex(h(oPad+h(iPad+msg)));
end;

function HashSHA256(const data : RawByteString) : RawByteString;
var
   SHA : TSHA256;
   digest : TSHA256Digest;
begin
   SHA.Full(Pointer(data), Length(data), digest);
   SetLength(Result, SizeOf(digest));
   System.Move(digest, Result[1], SizeOf(digest));
end;

function HashMD5(const data : RawByteString) : RawByteString;
var
   digest : TMD5Digest;
begin
   digest:=MD5Buf(Pointer(data)^, Length(data));
   SetLength(Result, SizeOf(digest));
   System.Move(digest, Result[1], SizeOf(digest));
end;

function HashSHA1(const data : RawByteString) : RawByteString;
var
   SHA : TSHA1;
   digest : TSHA1Digest;
begin
   SHA.Full(Pointer(data), Length(data), digest);
   SetLength(Result, SizeOf(digest));
   System.Move(digest, Result[1], SizeOf(digest));
end;

function HashRIPEMD160(const data : RawByteString) : RawByteString;
var
   digest : TRipe160Digest;
   remaining : Integer;
   p : PRipe160Block;
begin
   p := PRipe160Block(data);
   remaining := Length(data);

   RipeMD160Init(digest);
   while remaining >= SizeOf(TRipe160Block) do begin
      RipeMD160(digest, p);
      Inc(p);
      Dec(remaining, SizeOf(TRipe160Block));
   end;
   RipeMD160Final(digest, p, remaining, Length(data));

   SetLength(Result, SizeOf(digest));
   System.Move(digest, Result[1], SizeOf(digest));
end;

function HashSHA3_256(const data : RawByteString) : RawByteString;
var
   sponge : TSpongeState;
begin
   SetLength(Result, SizeOf(TSHA3_256_Hash));
   if data<>'' then begin
      sponge.Init(SHA3_256);
      sponge.Update(Pointer(data), Length(data));
      sponge.FinalHash(Pointer(Result));
   end else begin
      System.Move(cSHA3_256_EmptyString, Pointer(Result)^, SizeOf(TSHA3_256_Hash));
   end;
end;

function HashCRC32(const data : RawByteString) : RawByteString;
begin
   SetLength(Result, 4);
   PCardinal(Result)^:=CRC32string(data);
end;

function AES_SHA3_CTR(const data, key : RawByteString; encrypt : Boolean) : RawByteString;
var
   aes : TAESCTR;
   passwordHash, dataHash : TSHA3_256_Hash;
   n : Integer;
   dataBuf : RawByteString;
begin
   Result := '';
   if encrypt then begin

      if data = '' then Exit;

   end else begin

      n := Length(data);
      if n < 3*SizeOf(TAESBlock) then Exit;
      if (n and (SizeOf(TAESBlock)-1)) <> 0 then Exit;

      Dec(n, SizeOf(dataHash));
      if n <= 0 then Exit;
      dataBuf := Copy(data, 1, n);
      dataHash := HashSHA3_256_Digest(key + dataBuf);

      if not CompareMem(@dataHash, @PByte(data)[n], SizeOf(dataHash)) then Exit;

   end;

   passwordHash := HashSHA3_256_Digest(Pointer(key), Length(key));

   aes := TAESCTR.Create(passwordHash, SizeOf(passwordHash)*8);
   try
      if encrypt then begin
         Result := aes.EncryptPKCS7(data, True);
         dataHash := HashSHA3_256_Digest(key + Result);
         Result := Result + BytesToRawByteString(@dataHash, SizeOf(dataHash));
      end else begin
         try
            Result := aes.DecryptPKCS7(dataBuf, True);
         except
            Exit;
         end;
      end;
   finally
      aes.Free;
   end;
end;


end.
