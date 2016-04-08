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
   Windows, SysUtils, Classes, Types,
   dwsComp, dwsExprs, dwsUtils, dwsXPlatform, dwsTokenStore, dwsCryptoXPlatform;

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
    procedure dwsCryptoFunctionsProcessUniqueRandomEval(info: TProgramInfo);
    procedure dwsCryptoFunctionsCryptographicTokenEval(info: TProgramInfo);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure dwsCryptoClassesNoncesMethodsGenerateEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsClearEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsGetDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsRemoveEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsRemoveByDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsCheckAndKeepEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsCheckAndRemoveEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesNoncesMethodsRegisterEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    { Private declarations }
    FNonces : TdwsTokenStore;
    FNonceFilename : String;
    procedure SetNonceFilename(const name : String);
  public
    { Public declaration }
    property NonceFilename : String read FNonceFilename write SetNonceFilename;
    property Nonces : TdwsTokenStore read FNonces;

    procedure UseTemporaryStorageForNonces;
  end;

implementation

{$R *.dfm}

uses SynCrypto, SynZip, dwsRipeMD160, dwsCryptProtect, dwsSHA3;

type
   THashFunction = function (const data : RawByteString) : RawByteString;

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

procedure TdwsCryptoLib.DataModuleCreate(Sender: TObject);
begin
   FNonces:=TdwsTokenStore.Create;
end;

procedure TdwsCryptoLib.DataModuleDestroy(Sender: TObject);
begin
   if FNonceFilename<>'' then
      FNonces.SaveToFile(FNonceFilename);
   FNonces.Free;
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
var
   crc : Cardinal;
begin
   crc := SwapBytes(CRC32string(Info.ParamAsDataString[0]));
   Info.ResultAsString := BinToHex(crc, 4);
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

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsCheckAndKeepEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=FNonces.CheckAndKeep(Info.ParamAsString[0], Info.ParamAsString[1])
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsCheckAndRemoveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=FNonces.CheckAndRemove(Info.ParamAsString[0], Info.ParamAsString[1])
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsClearEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FNonces.Clear;
end;

// SetNonceFilename
//
procedure TdwsCryptoLib.SetNonceFilename(const name : String);
begin
   FNonceFilename:=name;
   if name<>'' then
      FNonces.LoadFromFile(FNonceFilename);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsGenerateEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   nonce : String;
begin
   nonce:=CryptographicToken(120);
   FNonces.Register(nonce, Info.ParamAsInteger[0]*0.001, Info.ParamAsString[1]);
   Info.ResultAsString:=nonce;
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsGetDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FNonces.TokenData[Info.ParamAsString[0]];
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsRegisterEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FNonces.Register(Info.ParamAsString[0], Info.ParamAsInteger[1],
                    Info.ParamAsString[2]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsRemoveByDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FNonces.RemoveByData(Info.ParamAsString[0]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsRemoveEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FNonces.Remove(Info.ParamAsString[0]);
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

procedure TdwsCryptoLib.dwsCryptoFunctionsCryptographicRandomEval(
  info: TProgramInfo);
begin
   info.ResultAsDataString:=CryptographicRandom(info.ParamAsInteger[0]);
end;

procedure TdwsCryptoLib.dwsCryptoFunctionsCryptographicTokenEval(
  info: TProgramInfo);
begin
   info.ResultAsString:=CryptographicToken(info.ParamAsInteger[0]);
end;

procedure TdwsCryptoLib.UseTemporaryStorageForNonces;
var
   signature : RawByteString;
begin
   signature:=UTF8Encode(GetCurrentUserName+','+ParamStr(0));
   NonceFilename:=IncludeTrailingPathDelimiter(TPath.GetTempPath)+UTF8ToString(SHA256(signature))+'.nonces';
end;

procedure TdwsCryptoLib.dwsCryptoFunctionsProcessUniqueRandomEval(
  info: TProgramInfo);
begin
   info.ResultAsString:=ProcessUniqueRandom;
end;

end.
