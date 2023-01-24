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
   Winapi.Windows, System.SysUtils, System.Classes, System.Types, System.Variants,
   dwsComp, dwsExprs, dwsUtils, dwsXPlatform, dwsTokenStore, dwsCryptoXPlatform,
   dwsXXHash, dwsExprList, dwsSymbols;

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
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
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
    procedure dwsCryptoClassesHashSHA512MethodsHashDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesHashSHA512MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
    function dwsCryptoFunctionsProcessUniqueRandomFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsCryptoFunctionsCryptographicRandomFastEval(
      const args: TExprBaseListExec): Variant;
    function dwsCryptoFunctionsCryptographicTokenFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsCryptoClassesNoncesMethodsRegisterFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsCryptoClassesNoncesMethodsGenerateFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    function dwsCryptoClassesNoncesMethodsCheckAndKeepFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    function dwsCryptoClassesNoncesMethodsCheckAndRemoveFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    procedure dwsCryptoClassesNoncesMethodsGetDataFastEvalString(
      baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
    procedure dwsCryptoClassesNoncesMethodsRemoveFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsCryptoClassesNoncesMethodsRemoveByDataFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsCryptoClassesNoncesMethodsClearFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsCryptoClassesNoncesMethodsCollectFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    function dwsCryptoFunctionsCompilationUniqueRandomFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsCryptoClassesTRSAKeyConstructorsGenerateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyCleanUp(ExternalObject: TObject);
    procedure dwsCryptoClassesTRSAKeyConstructorsImportJSONEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsDestroyKeyEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsSignHashEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsVerifyHashEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsExportJSONEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsEncryptEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsBlockLengthEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsCryptoClassesTRSAKeyMethodsDecryptEval(Info: TProgramInfo;
      ExtObject: TObject);
    function dwsCryptoFunctionsPBKDF2_HMAC_SHA256FastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsCryptoClassesEncryptionAESnistCTRMethodsEncryptDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsCryptoClassesEncryptionAESnistCTRMethodsDecryptDataEval(
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

uses
   dwsCryptoUtils, SynCrypto, dwsCryptProtect, SynZip, SynEcc,
   dwsInfo, dwsCompilerContext, dwsRSAKey, dwsBCryptCNG;

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

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionAESnistCTRMethodsDecryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := AES_nist_CTR(
      Info.ParamAsDataString[0], Info.ParamAsDataString[1], Info.ParamAsDataString[2],
      False
   );
end;

procedure TdwsCryptoLib.dwsCryptoClassesEncryptionAESnistCTRMethodsEncryptDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := AES_nist_CTR(
      Info.ParamAsDataString[0], Info.ParamAsDataString[1], Info.ParamAsDataString[2],
      True
   );
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

function TdwsCryptoLib.dwsCryptoClassesNoncesMethodsCheckAndKeepFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := FNonces.CheckAndKeep(args.AsString[0], args.AsString[1])
end;

function TdwsCryptoLib.dwsCryptoClassesNoncesMethodsCheckAndRemoveFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := FNonces.CheckAndRemove(args.AsString[0], args.AsString[1]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsClearFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   FNonces.Clear;
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsCollectFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
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

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsGenerateFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := CryptographicToken(120);
   FNonces.Register(result, args.AsInteger[0]*0.001, args.AsString[1]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsGetDataFastEvalString(
  baseExpr: TTypedExpr; const args: TExprBaseListExec; var result: string);
begin
   Result := FNonces.TokenData[args.AsString[0]];
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsRegisterFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   FNonces.Register(args.AsString[0], args.AsInteger[1],
                    args.AsString[2]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsRemoveByDataFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   FNonces.RemoveByData(args.AsString[0]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesNoncesMethodsRemoveFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
begin
   FNonces.Remove(args.AsString[0]);
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

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyCleanUp(ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyConstructorsGenerateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TdwsRSAKey.GenerateKeyPair(Info.ParamAsInteger[0]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyConstructorsImportJSONEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TdwsRSAKey.ImportJSON(Info.ParamAsString[0]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsDestroyKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   (ExtObject as TdwsRSAKey).DestroyKey;
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsBlockLengthEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := (ExtObject as TdwsRSAKey).BlockLength;
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsDecryptEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := (ExtObject as TdwsRSAKey).Decrypt(
      Info.ParamAsDataString[0], Info.ParamAsString[1], Info.ParamAsDataString[2]
   );
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsEncryptEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := (ExtObject as TdwsRSAKey).Encrypt(
      Info.ParamAsDataString[0], Info.ParamAsString[1], Info.ParamAsDataString[2]
   );
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsExportJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := (ExtObject as TdwsRSAKey).ExportJSON;
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsSignHashEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := (ExtObject as TdwsRSAKey).SignHash(Info.ParamAsString[0], Info.ParamAsString[1]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesTRSAKeyMethodsVerifyHashEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean := (ExtObject as TdwsRSAKey).VerifyHash(
      Info.ParamAsString[0], Info.ParamAsString[1], Info.ParamAsDataString[2]
   );
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA256MethodsHMACEval(Info: TProgramInfo;
      ExtObject: TObject);
begin
   PerformHMAC(Info, HashSHA256, 64);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA512MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHashData(Info, HashSHA512);
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA512MethodsHMACEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   PerformHMAC(Info, HashSHA512, 128);
end;

function TdwsCryptoLib.dwsCryptoFunctionsCompilationUniqueRandomFastEval(
  const args: TExprBaseListExec): Variant;
const
   cCompilationUniqueRandomGUID : TGUID = '{7EBA4F31-8255-4BB7-956D-76D001DB2C24}';

   procedure Initialize(context : TdwsCompilerContext; var result : Variant);
   var
      n : String;
   begin
      n := CryptographicToken(6*42);
      context.CustomStateCompareExchange(cCompilationUniqueRandomGUID, n, Unassigned, result);
      if VarIsEmpty(result) then
         VarCopySafe(result, n);
   end;

var
   context : TdwsCompilerContext;
begin
   context := (args.Exec as TdwsProgramExecution).CompilerContext;
   context.CustomStateGet(cCompilationUniqueRandomGUID, Result);
   if VarIsEmpty(Result) then
      Initialize(context, Result);
end;

function TdwsCryptoLib.dwsCryptoFunctionsCryptographicRandomFastEval(
  const args: TExprBaseListExec): Variant;
begin
   RawByteStringToScriptString(CryptographicRandom(args.AsInteger[0]), Result);
end;

function TdwsCryptoLib.dwsCryptoFunctionsCryptographicTokenFastEval(
  const args: TExprBaseListExec): Variant;
begin
   Result := CryptographicToken(args.AsInteger[0]);
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

function TdwsCryptoLib.dwsCryptoFunctionsPBKDF2_HMAC_SHA256FastEval(
  const args: TExprBaseListExec): Variant;
var
   hash : TSHA256Digest;
begin
   PBKDF2_HMAC_SHA256(
      UTF8Encode(args.AsString[0]), UTF8Encode(args.AsString[1]),
      args.AsInteger[2], hash
   );
   VarCopySafe(Result, BinToHex(@hash, SizeOf(hash)));
end;

function TdwsCryptoLib.dwsCryptoFunctionsProcessUniqueRandomFastEval(
  const args: TExprBaseListExec): Variant;
begin
   VarCopySafe(Result, ProcessUniqueRandom);
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

