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
unit dwsBCryptCNG;

{$I dws.inc}

interface

uses WinAPI.Windows, SysUtils;

type
   NTSTATUS = Cardinal;
   TBCryptHandle = THandle;

   BCRYPT_RSAKEY_BLOB = record
      Magic : Cardinal;
      BitLength : Cardinal;
      cbPublicExp : Cardinal;
      cbModulus : Cardinal;
      cbPrime1 : Cardinal;
      cbPrime2 : Cardinal;
   end;
   PBCRYPT_RSAKEY_BLOB = ^BCRYPT_RSAKEY_BLOB;

   BCRYPT_PKCS1_PADDING_INFO = record
      pszAlgId : PWideChar;
   end;
   PBCRYPT_PKCS1_PADDING_INFO = ^BCRYPT_PKCS1_PADDING_INFO;

const
   STATUS_SUCCESS                   = $00000000;
   STATUS_NOT_SUPPORTED             = $C00000BB;
   STATUS_INTERNAL_ERROR            = $C00000E5;
   STATUS_PROCEDURE_NOT_FOUND       = $C000007A;
   STATUS_NOINTERFACE               = $C00002B9;
   STATUS_INFO_LENGTH_MISMATCH      = $C0000004;
   STATUS_BUFFER_TOO_SMALL          = $C0000023;
   STATUS_INVALID_PARAMETER         = $C000000D;
   STATUS_INSUFFICIENT_RESOURCES    = $C000009A;
   STATUS_UNHANDLED_EXCEPTION       = $C0000144;
   STATUS_NOT_FOUND                 = $C0000225;
   STATUS_NOT_IMPLEMENTED           = $C0000002;
   STATUS_ACCESS_DENIED             = $C0000022;
   STATUS_ALREADY_REGISTERED        = $C0000718;
   STATUS_WOW_ASSERTION             = $C0009898;
   STATUS_BUFFER_OVERFLOW           = $80000005;
   STATUS_DLL_INIT_FAILED           = $C0000142;

   STATUS_INVALID_SIGNATURE         = $C000A000;

   STATUS_INVALID_PARAMETER_1       = $C00000EF;
   STATUS_INVALID_PARAMETER_2       = $C00000F0;
   STATUS_INVALID_PARAMETER_3       = $C00000F1;
   STATUS_INVALID_PARAMETER_4       = $C00000F2;
   STATUS_INVALID_PARAMETER_5       = $C00000F3;
   STATUS_INVALID_PARAMETER_6       = $C00000F4;
   STATUS_INVALID_PARAMETER_7       = $C00000F5;
   STATUS_INVALID_PARAMETER_8       = $C00000F6;

   // Magic numbers for CNG blobs
   BCRYPT_RSAPUBLIC_MAGIC           = $31415352;
   BCRYPT_RSAPRIVATE_MAGIC          = $32415352;
   BCRYPT_RSAFULLPRIVATE_MAGIC      = $33415352;
   BCRYPT_ECDH_PUBLIC_P256_MAGIC    = $314B4345;
   BCRYPT_ECDH_PRIVATE_P256_MAGIC   = $324B4345;
   BCRYPT_ECDH_PUBLIC_P384_MAGIC    = $334B4345;
   BCRYPT_ECDH_PRIVATE_P384_MAGIC   = $344B4345;
   BCRYPT_ECDH_PUBLIC_P521_MAGIC    = $354B4345;
   BCRYPT_ECDH_PRIVATE_P521_MAGIC   = $364B4345;
   BCRYPT_ECDSA_PUBLIC_P256_MAGIC   = $31534345;
   BCRYPT_ECDSA_PRIVATE_P256_MAGIC  = $32534345;
   BCRYPT_ECDSA_PUBLIC_P384_MAGIC   = $33534345;
   BCRYPT_ECDSA_PRIVATE_P386_MAGIC  = $34534345;
   BCRYPT_ECDSA_PUBLIC_P521_MAGIC   = $35534345;
   BCRYPT_ECDSA_PRIVATE_P521_MAGIC  = $36534345;
   BCRYPT_DH_PUBLIC_MAGIC           = $42504844;
   BCRYPT_DH_PRIVATE_MAGIC          = $56504844;
   BCRYPT_DH_PARAMETERS_MAGIC       = $4D504844;
   BCRYPT_DSA_PUBLIC_MAGIC          = $42505344;
   BCRYPT_DSA_PRIVATE_MAGIC         = $56505344;
   BCRYPT_KEY_DATA_BLOB_MAGIC       = $4D42444B;
   BCRYPT_DSA_PARAMETERS_MAGIC      = $4D505344;

   // Supported padding types
   BCRYPT_SUPPORTED_PAD_ROUTER      = $00000001;
   BCRYPT_SUPPORTED_PAD_PKCS1_ENC   = $00000002;
   BCRYPT_SUPPORTED_PAD_PKCS1_SIG   = $00000004;
   BCRYPT_SUPPORTED_PAD_OAEP        = $00000008;
   BCRYPT_SUPPORTED_PAD_PSS         = $00000010;

   // Flags for various functions
   BCRYPT_PROV_DISPATCH             = $00000001;
   BCRYPT_BLOCK_PADDING             = $00000001;
   BCRYPT_PAD_NONE                  = $00000001;
   BCRYPT_PAD_PKCS1                 = $00000002;
   BCRYPT_PAD_OAEP                  = $00000004;
   BCRYPT_PAD_PSS                   = $00000008;

   // Algorithms
   BCRYPT_RSA_ALGORITHM                : PWideChar = 'RSA';
   BCRYPT_RSA_SIGN_ALGORITHM           : PWideChar = 'RSA_SIGN';
   BCRYPT_DH_ALGORITHM                 : PWideChar = 'DH';
   BCRYPT_DSA_ALGORITHM                : PWideChar = 'DSA';
   BCRYPT_RC2_ALGORITHM                : PWideChar = 'RC2';
   BCRYPT_RC4_ALGORITHM                : PWideChar = 'RC4';
   BCRYPT_AES_ALGORITHM                : PWideChar = 'AES';
   BCRYPT_DES_ALGORITHM                : PWideChar = 'DES';
   BCRYPT_DESX_ALGORITHM               : PWideChar = 'DESX';
   BCRYPT_3DES_ALGORITHM               : PWideChar = '3DES';
   BCRYPT_3DES_112_ALGORITHM           : PWideChar = '3DES_112';
   BCRYPT_MD2_ALGORITHM                : PWideChar = 'MD2';
   BCRYPT_MD4_ALGORITHM                : PWideChar = 'MD4';
   BCRYPT_MD5_ALGORITHM                : PWideChar = 'MD5';
   BCRYPT_SHA1_ALGORITHM               : PWideChar = 'SHA1';
   BCRYPT_SHA256_ALGORITHM             : PWideChar = 'SHA256';
   BCRYPT_SHA384_ALGORITHM             : PWideChar = 'SHA384';
   BCRYPT_SHA512_ALGORITHM             : PWideChar = 'SHA512';
   BCRYPT_SHA_GMAC_ALGORITHM           : PWideChar = 'AES-GMAC';
   BCRYPT_ECDSA_P256_ALGORITHM         : PWideChar = 'ECDSA_P256';
   BCRYPT_ECDSA_P386_ALGORITHM         : PWideChar = 'ECDSA_P384';
   BCRYPT_ECDSA_P521_ALGORITHM         : PWideChar = 'ECDSA_P521';
   BCRYPT_ECDH_P256_ALGORITHM          : PWideChar = 'ECDH_P256';
   BCRYPT_ECDH_P384_ALGORITHM          : PWideChar = 'ECDH_P384';
   BCRYPT_ECDH_P521_ALGORITHM          : PWideChar = 'ECDH_P521';
   BCRYPT_RNG_ALGORITHM                : PWideChar = 'RNG';
   BCRYPT_RNG_FIPS186_DSA_ALGORITHM    : PWideChar = 'FIPS128DSARNG';
   BCRYPT_RNG_DUAL_EC_ALGORITHM        : PWideChar = 'DUALECRNG';

   BCRYPT_RSAFULLPRIVATE_BLOB          : PWideChar = 'RSAFULLPRIVATEBLOB';
   BCRYPT_RSAPRIVATE_BLOB              : PWideChar = 'RSAPRIVATEBLOB';
   BCRYPT_RSAPUBLIC_BLOB               : PWideChar = 'RSAPUBLICBLOB';
   LEGACY_RSAPRIVATE_BLOB              : PWideChar = 'CAPIPRIVATEBLOB';
   LEGACY_RSAPUBLIC_BLOB               : PWideChar = 'CAPIPUBLICBLOB';

   // String properties
   BCRYPT_OPAQUE_KEY_BLOB              : PWideChar = 'OpaqueKeyBlob';
   BCRYPT_KEY_DATA_BLOB                : PWideChar = 'KeyDataBlob';
   BCRYPT_AES_WRAP_KEY_BLOB            : PWideChar = 'Rfc3565KeyWrapBlob';
   BCRYPT_OBJECT_LENGTH                : PWideChar = 'ObjectLength';
   BCRYPT_ALGORITHM_NAME               : PWideChar = 'AlgorithmName';
   BCRYPT_PROVIDER_HANDLE              : PWideChar = 'ProviderHandle';
   BCRYPT_CHAINING_MODE                : PWideChar = 'ChainingMode';
   BCRYPT_BLOCK_LENGTH                 : PWideChar = 'BlockLength';
   BCRYPT_KEY_LENGTH                   : PWideChar = 'KeyLength';
   BCRYPT_KEY_OBJECT_LENGTH            : PWideChar = 'KeyObjectLength';
   BCRYPT_KEY_STRENGTH                 : PWideChar = 'KeyStrength';
   BCRYPT_KEY_LENGTHS                  : PWideChar = 'KeyLengths';
   BCRYPT_BLOCK_SIZE_LIST              : PWideChar = 'BlockSizeList';
   BCRYPT_EFFECTIVE_KEY_LENGTH         : PWideChar = 'EffectiveKeyLength';
   BCRYPT_HASH_LENGTH                  : PWideChar = 'HashDigestLength';
   BCRYPT_HASH_OID_LIST                : PWideChar = 'HashOIDList';
   BCRYPT_PADDING_SCHEMES              : PWideChar = 'PaddingSchemes';
   BCRYPT_SIGNATURE_LENGTH             : PWideChar = 'SignatureLength';
   BCRYPT_HASH_BLOCK_LENGTH            : PWideChar = 'HashBlockLength';
   BCRYPT_AUTH_TAG_LENGTH              : PWideChar = 'AuthTagLength';
   BCRYPT_PRIMITIVE_TYPE               : PWideChar = 'PrimitiveType';
   BCRYPT_IS_KEYED_HASH                : PWideChar = 'IsKeyedHash';
   BCRYPT_INITIALIZATION_VECTOR        : PWideChar = 'IV';
   BCRYPT_CHAIN_MODE_NA                : PWideChar = 'ChainingModeN/A';
   BCRYPT_CHAIN_MODE_CBC               : PWideChar = 'ChainingModeCBC';
   BCRYPT_CHAIN_MODE_ECB               : PWideChar = 'ChainingModeECB';
   BCRYPT_CHAIN_MODE_CFB               : PWideChar = 'ChainingModeCFB';
   BCRYPT_CHAIN_MODE_CCM               : PWideChar = 'ChainingModeCCM';
   BCRYPT_CHAIN_MODE_GCM               : PWideChar = 'ChainingModeGCM';
   BCRYPT_DH_PARAMETERS                : PWideChar = 'DHParameters';
   BCRYPT_DSA_PARAMETERS               : PWideChar = 'DSAParameters';

function BCryptOpenAlgorithmProvider(
      out hAlgorithm: TBCryptHandle; pszAlgId, pszImplementation: PWideChar;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptCloseAlgorithmProvider(
      hAlgorithm: TBCryptHandle;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptGenerateKeyPair(
      hAlgorithm: TBCryptHandle;
      out phKey: THandle; dwLength: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptFinalizeKeyPair(
      hKey: TBCryptHandle; dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptImportKeyPair(
      hAlgorithm: TBCryptHandle; hImportKey: TBCryptHandle;
      pszBlobType: PWideChar; out phKey: TBCryptHandle;
      pbInput: Pointer; cbInput: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptExportKey(
      hKey: THandle; hExportKey: THandle; pszBlobType: PWideChar;
      pbOutput: Pointer; cbOutput: Cardinal; out cbResult: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptDestroyKey(
      hKey: TBCryptHandle
      ): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptSignHash(
      hKey: TBCryptHandle; pPaddingInfo: PBCRYPT_PKCS1_PADDING_INFO;
      pbInput: Pointer; cbInput: Cardinal; pbOutput: Pointer; cbOutput: Cardinal;
      out pcbResult: Cardinal; dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptVerifySignature(
      hKey: TBCryptHandle; pPaddingInfo: Pointer;
      pbHash: Pointer; cbHash: Cardinal; pbSignature: Pointer; cbSignature: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptEncrypt(
      hKey: TBCryptHandle;
      pbInput: Pointer; cbInput: Cardinal;
      pPaddingInfo: Pointer;
      pbIV: Pointer; cbIV: Cardinal;
      pbOutput: Pointer; cbOutput: Cardinal;
      out cbResult: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';
function BCryptDecrypt(
      hKey: TBCryptHandle;
      pbInput: Pointer; cbInput: Cardinal;
      pPaddingInfo: Pointer;
      pbIV: Pointer; cbIV: Cardinal;
      pbOutput: Pointer; cbOutput: Cardinal;
      out cbResult: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';

function BCryptGetProperty(
      hObject: TBCryptHandle; pszProperty: PWideChar;
      pbOutput: Pointer; cbOutput: Cardinal;
      out cbResult: Cardinal;
      dwFlags: Cardinal
      ): NTSTATUS; stdcall; external 'bcrypt.dll';

type
   EBCryptException = class (Exception)
      private
         FStatus : NTSTATUS;
         FDetail : String;

      public
         constructor Create(status : NTSTATUS; const detail : String);

         property Status : NTSTATUS read FStatus;
         property Detail : String read FDetail;
   end;

procedure BCryptCheck(status : NTSTATUS; const detail : String); inline;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ EBCryptException ------------------
// ------------------

// Create
//
constructor EBCryptException.Create(status : NTSTATUS; const detail : String);
var
   msg : String;
begin
   case status of
      STATUS_SUCCESS                : msg := 'success';
      STATUS_NOT_SUPPORTED          : msg := 'not supported';
      STATUS_INTERNAL_ERROR         : msg := 'internal error';
      STATUS_PROCEDURE_NOT_FOUND    : msg := 'procedure not found';
      STATUS_NOINTERFACE            : msg := 'no interface';
      STATUS_INFO_LENGTH_MISMATCH   : msg := 'info length mismatch';
      STATUS_BUFFER_TOO_SMALL       : msg := 'buffer too small';
      STATUS_INVALID_PARAMETER      : msg := 'invalid parameter';
      STATUS_INSUFFICIENT_RESOURCES : msg := 'insufficient resources';
      STATUS_UNHANDLED_EXCEPTION    : msg := 'unhandled exception';
      STATUS_NOT_FOUND              : msg := 'not found';
      STATUS_NOT_IMPLEMENTED        : msg := 'not implemented';
      STATUS_ACCESS_DENIED          : msg := 'access denied';
      STATUS_ALREADY_REGISTERED     : msg := 'already register';
      STATUS_WOW_ASSERTION          : msg := 'wow assertion';
      STATUS_BUFFER_OVERFLOW        : msg := 'buffer overflow';
      STATUS_DLL_INIT_FAILED        : msg := 'dll init failed';
   else
      msg := SysErrorMessage(status);
   end;
   inherited CreateFmt('BCrypt $%x: %s (%s)', [ status, msg, detail ]);
end;

// BCryptCheck
//
procedure BCryptCheck(status : NTSTATUS; const detail : String);
begin
   if status <> 0 then
      raise EBCryptException.Create(status, detail);
end;


end.
