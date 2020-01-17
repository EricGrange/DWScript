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
unit dwsRSAKey;

{$I dws.inc}
{$SCOPEDENUMS ON}

interface

uses Classes, SysUtils, dwsBCryptCNG;

type
   TdwsRSAKeyType = (
      Empty,
      PublicKey,
      PrivateKey
   );

type
   TdwsRSAKey = class (TInterfacedObject)
      private
         FAlgHandle : TBCryptHandle;
         FKeyHandle : TBCryptHandle;
         FKeyType : TdwsRSAKeyType;

      public
         constructor Create;
         constructor GenerateKeyPair(bitSize : Integer);
         constructor ImportJSON(const jsonData : String);
         destructor Destroy; override;

         function KeyType : TdwsRSAKeyType;

         procedure DestroyKey;

         function SignHash(const paddingAlgorithm : String;
                           const hashHex : String) : RawByteString;
         function VerifyHash(const paddingAlgorithm : String;
                             const hashHex : String;
                             const signature : RawByteString) : Boolean;

         function ExportJSON : String;

         function Encrypt(const data : RawByteString;
                          const paddingAlgorithm : String;
                          const initializationVector : RawByteString) : RawByteString;
         function Decrypt(const data : RawByteString;
                          const paddingAlgorithm : String;
                          const initializationVector : RawByteString) : RawByteString;
         function BlockLength : Cardinal;
   end;

   EdwsRSAKeyException = class (Exception);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsJSON, dwsUtils;

// ------------------
// ------------------ TdwsRSAKey ------------------
// ------------------

// Create
//
constructor TdwsRSAKey.Create;
begin
   inherited;
   BCryptCheck(
      BCryptOpenAlgorithmProvider(FAlgHandle, BCRYPT_RSA_ALGORITHM, nil, 0),
      'RSAKey.Create'
   );
end;

// GenerateKeyPair
//
constructor TdwsRSAKey.GenerateKeyPair(bitSize : Integer);
begin
   Create;
   BCryptCheck(
      BCryptGenerateKeyPair(FAlgHandle, FKeyHandle, bitSize, 0),
      'RSAKey.GenerateKey.1'
   );
   BCryptCheck(
      BCryptFinalizeKeyPair(FKeyHandle, 0),
      'RSAKey.GenerateKey.2'
   );
   FKeyType := TdwsRSAKeyType.PrivateKey;
end;

// ImportJSON
//
constructor TdwsRSAKey.ImportJSON(const jsonData : String);
var
   blob : PBCRYPT_RSAKEY_BLOB;
   publicExponent, modulus, prime1, prime2 : RawByteString;
   blobType : PWideChar;
   blobData : RawByteString;
   jv, jPrime, jSize : TdwsJSONValue;
begin
   Create;

   SetLength(blobData, SizeOf(blob^));
   blob := PBCRYPT_RSAKEY_BLOB(blobData);

   jv := TdwsJSONValue.ParseString(jsonData);
   try
      jSize := jv.Values['size'];
      if jSize <> nil then
         blob.BitLength := jSize.AsInteger
      else blob.BitLength := 0;

      try
         publicExponent := HexToBin(jv.Values['exponent'].AsString);
         blob.cbPublicExp := Length(publicExponent);
      except
         on E: Exception do
            raise EdwsRSAKeyException.CreateFmt('failed to parse exponent (%s)', [ E.Message ]);
      end;

      try
         modulus := HexToBin(jv.Values['modulus'].AsString);
         blob.cbModulus := Length(modulus);
      except
         on E: Exception do
            raise EdwsRSAKeyException.CreateFmt('failed to parse modulus (%s)', [ E.Message ]);
      end;

      jPrime := jv.Values['prime1'];
      if jPrime <> nil then begin
         prime1 := HexToBin(jPrime.AsString);
         jPrime := jv.Values['prime2'];
         if jPrime <> nil then
            prime2 := HexToBin(jPrime.AsString);
      end;
   finally
      jv.Free;
   end;

   if blob.BitLength = 0 then
      blob.BitLength := Length(modulus);

   if (prime1 <> '') and (prime2 <> '') then begin
      blob.Magic := BCRYPT_RSAPRIVATE_MAGIC;
      blobType := BCRYPT_RSAPRIVATE_BLOB;
      FKeyType := TdwsRSAKeyType.PrivateKey;
   end else begin
      blob.Magic := BCRYPT_RSAPUBLIC_MAGIC;
      blobType := BCRYPT_RSAPUBLIC_BLOB;
      prime1 := '';
      prime2 := '';
      FKeyType := TdwsRSAKeyType.PublicKey;
   end;
   blob.cbPrime1 := Length(prime1);
   blob.cbPrime2 := Length(prime2);
   blobData := blobData + publicExponent + modulus + prime1 + prime2;
   BCryptCheck(
      BCryptImportKeyPair(FAlgHandle, 0, blobType, FKeyHandle, Pointer(blobData), Length(blobData), 0),
      'RSAKey.ImportJSON'
   );
end;

// Destroy
//
destructor TdwsRSAKey.Destroy;
begin
   inherited;
   DestroyKey;
end;

// KeyType
//
function TdwsRSAKey.KeyType : TdwsRSAKeyType;
begin
   Result := FKeyType;
end;

// DestroyKey
//
procedure TdwsRSAKey.DestroyKey;
var
   ah, kh : TBCryptHandle;
begin
   FKeyType := TdwsRSAKeyType.Empty;
   ah := FAlgHandle;
   kh := FKeyHandle;
   FAlgHandle := 0;
   FKeyHandle := 0;
   try
      if kh <> 0 then begin
         BCryptCheck(
            BCryptDestroyKey(kh),
            'RSA.GenerateKey.Destroy'
         );
      end;
   finally
      if ah <> 0 then begin
         BCryptCheck(
            BCryptCloseAlgorithmProvider(ah, 0),
            'RSAKey.Destroy'
         );
      end;
   end;
end;

// SignHash
//
function TdwsRSAKey.SignHash(const paddingAlgorithm : String;
                             const hashHex : String) : RawByteString;
var
   paddingInfo : BCRYPT_PKCS1_PADDING_INFO;
   hashBin : RawByteString;
   signatureSize : Cardinal;
begin
   if FKeyType <> TdwsRSAKeyType.PrivateKey then
      raise EdwsRSAKeyException.Create('A private key is required to sign');

   paddingInfo.pszAlgId := Pointer(paddingAlgorithm);
   hashBin := HexToBin(hashHex);
   BCryptCheck(
      BCryptSignHash(FKeyHandle, @paddingInfo, Pointer(hashBin), Length(hashBin),
                     nil, 0, signatureSize, BCRYPT_PAD_PKCS1),
      'RSAKey.SignHash.Size'
   );
   SetLength(Result, signatureSize);
   BCryptCheck(
      BCryptSignHash(FKeyHandle, @paddingInfo, Pointer(hashBin), Length(hashBin),
                     Pointer(Result), signatureSize, signatureSize, BCRYPT_PAD_PKCS1),
      'RSAKey.SignHash.Sign'
   );
end;

// VerifyHash
//
function TdwsRSAKey.VerifyHash(const paddingAlgorithm : String;
                                  const hashHex : String;
                                  const signature : RawByteString) : Boolean;
var
   paddingInfo : BCRYPT_PKCS1_PADDING_INFO;
   hashBin : RawByteString;
   status : NTSTATUS;
begin
   if not (FKeyType in [ TdwsRSAKeyType.PublicKey, TdwsRSAKeyType.PrivateKey ]) then
      raise EdwsRSAKeyException.Create('A public or private key is required to verify');

   paddingInfo.pszAlgId := Pointer(paddingAlgorithm);
   hashBin := HexToBin(hashHex);
   status := BCryptVerifySignature(FKeyHandle, @paddingInfo, Pointer(hashBin), Length(hashBin),
                                   Pointer(signature), Length(signature), BCRYPT_PAD_PKCS1);
   case status of
      STATUS_SUCCESS : Result := True;
      STATUS_INVALID_SIGNATURE : Result := False;
   else
      BCryptCheck(status, 'RSAKey.Verify');
      Result := False;
   end;
end;

// ExportJSON
//
function TdwsRSAKey.ExportJSON : String;
var
   blobSize : Cardinal;
   blob : array of Byte;
   pblob : PBCRYPT_RSAKEY_BLOB;
   pszBlobType : PWideChar;
   i : Integer;
   wr : TdwsJSONWriter;
begin
   pszBlobType := nil;
   case FKeyType of
      TdwsRSAKeyType.Empty : Exit('{}');
      TdwsRSAKeyType.PublicKey : pszBlobType := BCRYPT_RSAPUBLIC_BLOB;
      TdwsRSAKeyType.PrivateKey : pszBlobType := BCRYPT_RSAPRIVATE_BLOB;
   else
      Assert(False);
   end;

   blobSize := 0;

   BCryptCheck(
      BCryptExportKey(FKeyHandle, 0, pszBlobType, nil, 0, blobSize, 0),
      'RSAKey.Export.Size'
   );
   SetLength(blob, blobSize);
   BCryptCheck(
      BCryptExportKey(FKeyHandle, 0, pszBlobType, Pointer(blob), blobSize, blobSize, 0),
      'RSAKey.Export.Data'
   );
   pblob := PBCRYPT_RSAKEY_BLOB(blob);

   wr := TdwsJSONWriter.Create;
   try
      wr.BeginObject;

      wr.WriteInteger('size', pblob.BitLength);

      i := SizeOf(BCRYPT_RSAKEY_BLOB);
      wr.WriteString('exponent', BinToHex(@blob[i], pblob.cbPublicExp));
      Inc(i, pblob.cbPublicExp);
      wr.WriteString('modulus', BinToHex(@blob[i], pblob.cbModulus));

      if FKeyType = TdwsRSAKeyType.PrivateKey then begin
         Inc(i, pblob.cbModulus);
         wr.WriteString('prime1', BinToHex(@blob[i], pblob.cbPrime1));
         Inc(i, pblob.cbPrime1);
         wr.WriteString('prime2', BinToHex(@blob[i], pblob.cbPrime2));
      end;

      wr.EndObject;
      Result := wr.ToString;
   finally
      wr.Free;
   end;
end;

// Encrypt
//
function TdwsRSAKey.Encrypt(const data : RawByteString;
                            const paddingAlgorithm : String;
                            const initializationVector : RawByteString) : RawByteString;
var
   paddingInfo : Pointer;
   flags : Cardinal;
   outputSize : Cardinal;
begin
   if not (FKeyType in [ TdwsRSAKeyType.PublicKey, TdwsRSAKeyType.PrivateKey ]) then
      raise EdwsRSAKeyException.Create('A public or private key is required to encrypt');

   if paddingAlgorithm = '' then begin
      paddingInfo := nil;
      flags := 0;
   end else if paddingAlgorithm = 'PKCS1' then begin
      paddingInfo := nil;
      flags := BCRYPT_PAD_PKCS1;
   end else raise EdwsRSAKeyException.CreateFmt('Unsupported encryption padding "%s"', [ paddingAlgorithm ]);

   BCryptCheck(
      BCryptEncrypt(
         FKeyHandle, Pointer(data), Length(data),
         paddingInfo, Pointer(initializationVector), Length(initializationVector),
         nil, 0, outputSize, flags
      ),
      'RSAKey.Encrypt.Size'
   );

   SetLength(Result, outputSize);
   BCryptCheck(
      BCryptEncrypt(
         FKeyHandle, Pointer(data), Length(data),
         paddingInfo, Pointer(initializationVector), Length(initializationVector),
         Pointer(Result), outputSize, outputSize, flags
      ),
      'RSAKey.Encrypt.Perform'
   );
end;

// Decrypt
//
function TdwsRSAKey.Decrypt(const data : RawByteString;
                            const paddingAlgorithm : String;
                            const initializationVector : RawByteString) : RawByteString;
var
   paddingInfo : Pointer;
   flags : Cardinal;
   outputSize : Cardinal;
begin
   if FKeyType <> TdwsRSAKeyType.PrivateKey then
      raise EdwsRSAKeyException.Create('A private key is required to decrypt');

   if paddingAlgorithm = '' then begin
      paddingInfo := nil;
      flags := 0;
   end else if paddingAlgorithm = 'PKCS1' then begin
      paddingInfo := nil;
      flags := BCRYPT_PAD_PKCS1;
   end else raise EdwsRSAKeyException.CreateFmt('Unsupported decryption padding "%s"', [ paddingAlgorithm ]);

   BCryptCheck(
      BCryptDecrypt(
         FKeyHandle, Pointer(data), Length(data),
         paddingInfo, Pointer(initializationVector), Length(initializationVector),
         nil, 0, outputSize, flags
      ),
      'RSAKey.Decrypt.Size'
   );

   SetLength(Result, outputSize);
   BCryptCheck(
      BCryptDecrypt(
         FKeyHandle, Pointer(data), Length(data),
         paddingInfo, Pointer(initializationVector), Length(initializationVector),
         Pointer(Result), outputSize, outputSize, flags
      ),
      'RSAKey.Decrypt.Perform'
   );
end;

// BlockLength
//
function TdwsRSAKey.BlockLength : Cardinal;
var
   outputSize : Cardinal;
begin
   BCryptCheck(
      BCryptGetProperty(
         FKeyHandle, BCRYPT_BLOCK_LENGTH,
         @Result, SizeOf(Result), outputSize, 0
      ),
      'RSAKey.BlockLength'
   );
   Assert(outputSize = SizeOf(Result));
end;

end.
