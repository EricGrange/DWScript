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
   dwsComp, dwsExprs, dwsUtils, dwsXPlatform, dwsTokenStore, dwsCryptoXPlatform,
   dwsXXHash;

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
    procedure dwsCryptoClassesNoncesMethodsCollectEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionAESSHA3CTRMethodsEncryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionAESSHA3CTRMethodsDecryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesECCsecp256r1MethodsMakeKeyEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesECCsecp256r1MethodsECDHSharedSecretEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesECCsecp256r1MethodsECDSASignEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesECCsecp256r1MethodsECDSAVerifyEval(
      Info: TProgramInfo; ExtObject: TObject);
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

uses dwsCryptoUtils, SynCrypto, dwsCryptProtect, SynZip, SynEcc;

procedure PerformHashData(Info: TProgramInfo; h : THashFunction);
var
   b : RawByteString;
begin
   b := h(Info.ParamAsDataString[0]);
   Info.ResultAsString := BinToHex(b[1], Length(b));
end;

procedure PerformHMAC(Info: TProgramInfo; h : THashFunction; blockSize : Integer);
begin
   Info.ResultAsString := HMAC(Info.ParamAsDataString[0], Info.ParamAsDataString[1], h, blockSize);
end;

function DoAESFull(const data, key : RawByteString; encrypt : Boolean) : RawByteString;
var
   outbuf : TWriteOnlyBlockStream;
begin
   if data = '' then Exit('');
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
   if FNonceFilename<>'' then begin
      if FNonces.Count > 0 then
         FNonces.SaveToFile(FNonceFilename)
      else DeleteFile(FNonceFilename);
   end;
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

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionAESSHA3CTRMethodsDecryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := AES_SHA3_CTR(Info.ParamAsDataString[0], Info.ParamAsDataString[1], False);
end;

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionAESSHA3CTRMethodsEncryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := AES_SHA3_CTR(Info.ParamAsDataString[0], Info.ParamAsDataString[1], True);
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

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsCollectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FNonces.Collect;
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
   signature : String;
   digest : TSHA256Digest;
begin
   signature := GetCurrentUserName+','+ParamStr(0);
   digest := SHA256Digest(Pointer(signature), Length(signature)*SizeOf(Char));
   NonceFilename :=  IncludeTrailingPathDelimiter(TPath.GetTempPath)
                   + DigestToSimplifiedBase64(@digest, SizeOf(digest) div 2)
                   + '.nonces';
end;

procedure TdwsCryptoLib.dwsCryptoFunctionsProcessUniqueRandomEval(
  info: TProgramInfo);
begin
   info.ResultAsString:=ProcessUniqueRandom;
end;

procedure TdwsCryptoLib.dwsCryptoClassesECCsecp256r1MethodsMakeKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   pub : TECCPublicKey;
   priv : TECCPrivateKey;
begin
   if ecc_make_key(pub, priv) then begin
      Info.ParamAsDataString[0] := BytesToRawByteString(@pub, SizeOf(pub));
      Info.ParamAsDataString[1] := BytesToRawByteString(@priv, SizeOf(priv));
      FillChar(priv, SizeOf(priv), 0);
      Info.ResultAsBoolean := True;
   end else begin
      Info.ParamAsString[0] := '';
      Info.ParamAsString[1] := '';
      Info.ResultAsBoolean := True;
   end;
end;

procedure TdwsCryptoLib.dwsCryptoClassesECCsecp256r1MethodsECDHSharedSecretEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   secret : TECCSecretKey;
   pubData, privData : RawByteString;
begin
   pubData := Info.ParamAsDataString[0];
   if Length(pubData) <> SizeOf(TECCPublicKey) then
      raise Exception.Create('Invalid public key size');

   privData := Info.ParamAsDataString[1];
   if Length(privData) <> SizeOf(TECCPrivateKey) then
      raise Exception.Create('Invalid private key size');

   if not ecdh_shared_secret(TECCPublicKey(Pointer(pubData)^), TECCPrivateKey(Pointer(privData)^), secret) then
      raise Exception.Create('Invalid ECC keys');

   Info.ResultAsDataString := BytesToRawByteString(@secret, SizeOf(secret));
end;

procedure TdwsCryptoLib.dwsCryptoClassesECCsecp256r1MethodsECDSASignEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   privData, hashData : RawByteString;
   sign : TECCSignature;
begin
   privData := Info.ParamAsDataString[0];
   if Length(privData) <> SizeOf(TECCPrivateKey) then
      raise Exception.Create('Invalid private key size');

   hashData := HexToBin(Info.ParamAsString[1]);
   if Length(hashData) <> SizeOf(TECCHash) then
      raise Exception.Create('Invalid hash hexadecimal size');

   if not ecdsa_sign(TECCPrivateKey(Pointer(privData)^), TECCHash(Pointer(hashData)^), sign) then
      raise Exception.Create('Invalid ECC key for signature');

   Info.ResultAsDataString := BytesToRawByteString(@sign, SizeOf(sign));
end;

procedure TdwsCryptoLib.dwsCryptoClassesECCsecp256r1MethodsECDSAVerifyEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   pubData, hashData, signData : RawByteString;
begin
   pubData := Info.ParamAsDataString[0];
   if Length(pubData) <> SizeOf(TECCPublicKey) then
      raise Exception.Create('Invalid public key size');

   hashData := HexToBin(Info.ParamAsString[1]);
   if Length(hashData) <> SizeOf(TECCHash) then
      raise Exception.Create('Invalid hash hexadecimal size');

   signData := Info.ParamAsDataString[2];
   if Length(signData) <> SizeOf(TECCSignature) then
      raise Exception.Create('Invalid signature size');

   Info.ResultAsBoolean := ecdsa_verify(
      TECCPublicKey(Pointer(pubData)^), TECCHash(Pointer(hashData)^),
      TECCSignature(Pointer(signData)^)
   );
end;

end.

