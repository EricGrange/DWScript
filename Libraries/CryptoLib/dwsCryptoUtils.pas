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

type
   THashFunction = function (const data : RawByteString) : RawByteString;

function HMAC(const key, msg : RawByteString; h : THashFunction; blockSize : Integer) : String;

function ConstantTimeCompareMem(p1, p2 : PByte; nbBytes : Integer) : Boolean;

function HashSHA3_256(const data : RawByteString) : RawByteString;
function HashSHA256(const data : RawByteString) : RawByteString;
function HashSHA256_P(p : Pointer; sizeBytes : Integer) : RawByteString;
function HashSHA512(const data : RawByteString) : RawByteString;
function HashRIPEMD160(const data : RawByteString) : RawByteString;

type
   TSHA1Digest = array[0..19] of byte;
procedure HashSHA1p(p : Pointer; nbBytes : Integer; var digest : TSHA1Digest);
function HashSHA1(const data : RawByteString) : RawByteString;

function HashMD5(const data : RawByteString) : RawByteString;
function HashCRC32(const data : RawByteString) : RawByteString;

// authenticated encryption, key hashing, PKCS7 padding
function AES_SHA3_CTR(const data, key : RawByteString; encrypt : Boolean) : RawByteString;

// raw nist-copatible encryption, no padding, no key hashing
function AES_nist_CTR(const data, key, iv : RawByteString; encrypt : Boolean) : RawByteString;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   System.SysUtils, System.ZLib,
   SynCrypto,
   dwsUtils, dwsXPlatform, dwsSHA3, dwsRipeMD160, dwsSHA512;

// HMAC
//
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

// ConstantTimeCompareMem
//
function ConstantTimeCompareMem(p1, p2 : PByte; nbBytes : Integer) : Boolean;
begin
   var r : UInt32 := 0;
   while nbBytes > 4 do begin
      r := r or (PUInt32(p1)^ xor PUInt32(p2)^);
      Inc(p1, 4);
      Inc(p2, 4);
   end;
   while nbBytes > 0 do begin
      r := r or (p1^ xor p2^);
      Inc(p1);
      Inc(p2);
   end;
   Result := (r = 0);
end;

// HashSHA256_P
//
function HashSHA256_P(p : Pointer; sizeBytes : Integer) : RawByteString;
var
   SHA : TSHA256;
   digest : TSHA256Digest;
begin
   SHA.Full(p, sizeBytes, digest);
   SetLength(Result, SizeOf(digest));
   System.Move(digest, Result[1], SizeOf(digest));
end;

// HashSHA256
//
function HashSHA256(const data : RawByteString) : RawByteString;
begin
   Result := HashSHA256_P(Pointer(data), Length(data));
end;

// HashSHA512
//
function HashSHA512(const data : RawByteString) : RawByteString;
var
   dcp : TSHA512State;
   digest : TSHA512Digest;
begin
   dcp.Init;
   dcp.Update(Pointer(data), Length(data));
   dcp.FinalHash(digest);
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

procedure HashSHA1p(p : Pointer; nbBytes : Integer; var digest : TSHA1Digest);
var
   SHA : TSHA1;
begin
   SHA.Full(p, nbBytes, SynCrypto.TSHA1Digest(digest));
end;

function HashSHA1(const data : RawByteString) : RawByteString;
var
   digest : TSHA1Digest;
begin
   HashSHA1p(Pointer(data), Length(data), digest);
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
   if data <> '' then
      PUInt32(Result)^ := System.ZLib.crc32(0, Pointer(data), Length(data))
   else PUInt32(Result)^ := 0;
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

      if not ConstantTimeCompareMem(@dataHash, @PByte(data)[n], SizeOf(dataHash)) then Exit;

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

type
   TAESCTR_nist = class(TAESCTR)
      constructor Create(const aKey; aKeySize: cardinal); override;
   end;

constructor TAESCTR_nist.Create(const aKey; aKeySize: cardinal);
begin
   inherited;
   fCTROffset := 15;
end;

// AES_nist_CTR
//
function AES_nist_CTR(const data, key, iv : RawByteString; encrypt : Boolean) : RawByteString;
var
   aes : TAESCTR_nist;
   keySize, dataSize : Integer;
begin
   Result := '';

   keySize := Length(key)*8;
   case keySize of
      128, 192, 256 : ;
   else
      raise Exception.CreateFmt('Invalid key length (%d bits) should be 128, 192 or 256', [ keySize ]);
   end;

   if Length(iv) <> SizeOf(TAESBlock) then
      raise Exception.CreateFmt('Invalid IV length (%d) should be %d', [ Length(iv), SizeOf(TAESBlock) ]);

   if data = '' then Exit;

   dataSize := Length(data);
   if (dataSize and (SizeOf(TAESBlock)-1)) <> 0 then
      raise Exception.CreateFmt('Invalid data length, should be a multiple of %d', [ SizeOf(TAESBlock) ]);

   aes := TAESCTR_nist.Create(Pointer(key)^, keySize);
   try
      aes.IV := PAESBlock(Pointer(iv))^;
      SetLength(Result, dataSize);
      if encrypt then
         aes.Encrypt(Pointer(data), Pointer(Result), dataSize)
      else aes.Decrypt(Pointer(data), Pointer(Result), dataSize);
   finally
      aes.Free;
   end;
end;

end.
