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
unit dwsCryptoLibModule;

interface

uses
   SysUtils, Classes, Types,
   dwsComp, dwsExprs, dwsUtils, dwsXPlatform;

type
  TdwsCryptoLib = class(TDataModule)
    dwsCrypto: TdwsUnit;
    procedure dwsCryptoClassesSHA256MethodsHashDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesSHA1MethodsHashDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesMD5MethodsHashDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionAESSHA256FullMethodsEncryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionAESSHA256FullMethodsDecryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesHashRIPEMD160MethodsHashDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesHashCRC32MethodsHashDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionCryptProtectMethodsEncryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionCryptProtectMethodsDecryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesHashSHA3_256MethodsHashDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesHashSHA256MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesHashMD5MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesHashSHA1MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesHashRIPEMD160MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesHashSHA3_256MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesHashCRC32MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoFunctionsCryptographicRandomEval(info: TProgramInfo);
  private
    { Private declarations }
  public
    { Public declaration }
  end;

implementation

{$R *.dfm}

uses SynCrypto, SynZip, dwsRipeMD160, dwsCryptProtect, dwsSHA3, wcrypt2;

type THashFunction = function (const data : RawByteString) : RawByteString;

procedure PerformHashData(Info: TProgramInfo; h : THashFunction);
var
   b : RawByteString;
begin
   b:=h(Info.ParamAsDataString[0]);
   Info.ResultAsString:=BinToHex(b[1], Length(b));
end;

procedure PerformHMAC(Info: TProgramInfo; h : THashFunction; blockSize : Integer);
var
   n : Integer;
   key, msg : RawByteString;
   oPad, iPad : AnsiString;
begin
   key:=Info.ParamAsDataString[0];
   msg:=Info.ParamAsDataString[1];

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

   Info.ResultAsString:=BinToHex(h(oPad+h(iPad+msg)));
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
   digest:=MD5Buf(data[1], Length(data));
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
   hash : array [0..256 div 8-1] of Byte;
begin
   SHA3_Init(sponge, SHA3_256);
   SHA3_Update(sponge, Pointer(data), Length(data));
   SHA3_FinalHash(sponge, @hash);

   SetLength(Result, SizeOf(hash));
   System.Move(hash, Result[1], SizeOf(hash));
end;

function HashCRC32(const data : RawByteString) : RawByteString;
begin
   SetLength(Result, 4);
   PCardinal(Result)^:=CRC32string(data);
end;

function DoAESFull(const data, key : RawByteString; encrypt : Boolean) : RawByteString;
var
   outbuf : TWriteOnlyBlockStream;
begin
   outbuf := TWriteOnlyBlockStream.AllocFromPool;
   try
      AESSHA256Full(Pointer(data), Length(data), outbuf, key, encrypt);
      Result := outbuf.ToRawBytes;
   finally
      outbuf.ReturnToPool;
   end;
end;

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionAESSHA256FullMethodsDecryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := DoAESFull(Info.ParamAsDataString[0], Info.ParamAsDataString[1], False);
end;

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionAESSHA256FullMethodsEncryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := DoAESFull(Info.ParamAsDataString[0], Info.ParamAsDataString[1], True);
end;

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionCryptProtectMethodsDecryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=CryptUnProtect(Info.ParamAsDataString[0], Info.ParamAsDataString[1]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionCryptProtectMethodsEncryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=CryptProtect(Info.ParamAsDataString[0], Info.ParamAsDataString[1]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashCRC32MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := IntToHex(CRC32string(Info.ParamAsDataString[0]), 8);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashCRC32MethodsHMACEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   // just for completeness sake, not very useful :)
   PerformHMAC(Info, HashCRC32, 4);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashRIPEMD160MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHashData(Info, HashRIPEMD160);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashRIPEMD160MethodsHMACEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHMAC(Info, HashRIPEMD160, 64);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA3_256MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHashData(Info, HashSHA3_256);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA3_256MethodsHMACEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   // not really needed as SHA-3 is resistant to length extension attacks
   PerformHMAC(Info, HashSHA3_256, 64);
end;

procedure TdwsCryptoLib.dwsCryptoClassesMD5MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHashData(Info, HashMD5);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashMD5MethodsHMACEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHMAC(Info, HashMD5, 64);
end;

procedure TdwsCryptoLib.dwsCryptoClassesSHA1MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHashData(Info, HashSHA1);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA1MethodsHMACEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHMAC(Info, HashSHA1, 64);
end;

procedure TdwsCryptoLib.dwsCryptoClassesSHA256MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHashData(Info, HashSHA256);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA256MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
begin
   PerformHMAC(Info, HashSHA256, 64);
end;

var
   hProv : THandle;
   hProvLock : TMultiReadSingleWrite;
   vXorShiftSeedMask : Int64;

procedure TdwsCryptoLib.dwsCryptoFunctionsCryptographicRandomEval(
  info: TProgramInfo);

   function RDTSC : UInt64;
   asm
      RDTSC;
   end;

   function XorShift(var seed : UInt64) : Cardinal; inline;
   var
      buf : UInt64;
   begin
      buf:=seed xor (seed shl 13);
      buf:=buf xor (buf shr 17);
      buf:=buf xor (buf shl 5);
      seed:=buf;
      Result:=seed and $FFFFFFFF;
   end;

var
   i, nb : Integer;
   buf : RawByteString;
   seed : Int64;
   p : PCardinal;
begin
   nb:=info.ParamAsInteger[0];
   if nb<0 then nb:=0;
   SetLength(buf, nb);

   if nb>0 then begin
      hProvLock.BeginWrite;
      try
         if hProv=0 then begin
            if not CryptAcquireContext(@hProv, nil, MS_ENHANCED_PROV, PROV_RSA_FULL,
                                       CRYPT_VERIFYCONTEXT) then begin
               CryptAcquireContext(@hProv, nil, MS_ENHANCED_PROV, PROV_RSA_FULL,
                                   CRYPT_NEWKEYSET + CRYPT_VERIFYCONTEXT);
            end;
            CryptGenRandom(hProv, SizeOf(vXorShiftSeedMask), @vXorShiftSeedMask);
         end;
         CryptGenRandom(hProv, nb, Pointer(buf));
      finally
         hProvLock.EndWrite;
      end;
   end;

   // further muddy things, in case Windows generator is later found vulnerable,
   // this will protect us from "generic" exploits
   seed:=RDTSC xor vXorShiftSeedMask;
   p:=PCardinal(buf);
   for i:=0 to (nb div 4)-1 do begin
      p^:=p^ xor XorShift(UInt64(seed));
      Inc(p);
   end;

   info.ResultAsDataString:=buf;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   hProvLock := TMultiReadSingleWrite.Create;

finalization

   FreeAndNil(hProvLock);
   if hProv>0 then
      CryptReleaseContext(hProv, 0);

end.
