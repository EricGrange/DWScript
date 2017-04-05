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
unit dwsCryptProtect;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses SysUtils, Types;

type
   CRYPT_INTEGER_BLOB = record
      cbData: DWORD;
      pbData: Pointer;
   end;
   PCRYPT_INTEGER_BLOB = ^CRYPT_INTEGER_BLOB;

   CRYPTPROTECT_PROMPTSTRUCT = record
      cbSize: DWORD;
      dwPromptFlags: DWORD;
      hwndApp: THandle;
      szPrompt: PWideChar;
   end;
   PCRYPTPROTECT_PROMPTSTRUCT = ^CRYPTPROTECT_PROMPTSTRUCT;

const
   CRYPTPROTECT_UI_FORBIDDEN = $1;
   CRYPTPROTECT_LOCAL_MACHINE = $4;

function CryptProtectData(pDataIn: PCRYPT_INTEGER_BLOB; szDataDescr: PWideChar;
                          pOptionalEntropy: PCRYPT_INTEGER_BLOB; pvReserved: Pointer;
                          pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                          dwFlags: DWORD; pDataOut: PCRYPT_INTEGER_BLOB): LongBool; stdcall; external 'crypt32.dll';

function CryptUnprotectData(pDataIn: PCRYPT_INTEGER_BLOB; ppszDataDescr: PPWideChar;
                            pOptionalEntropy: PCRYPT_INTEGER_BLOB; pvReserved: Pointer;
                            pPromptStruct: PCRYPTPROTECT_PROMPTSTRUCT;
                            dwFlags: DWORD; pDataOut: PCRYPT_INTEGER_BLOB): LongBool; stdcall; external 'crypt32.dll';

function LocalFree(hMem: Pointer): Pointer; stdcall; external 'kernel32.dll';

function CryptProtect(const data, key : RawByteString) : RawByteString;
function CryptUnProtect(const data, key : RawByteString) : RawByteString;

implementation

// CryptIntegerBlob
//
function CryptIntegerBlob(var blob : CRYPT_INTEGER_BLOB; const data : RawByteString) : PCRYPT_INTEGER_BLOB;
begin
   blob.cbData:=Length(data);
   blob.pbData:=Pointer(data);
   Result:=@blob;
end;

// CryptProtect
//
function CryptProtect(const data, key : RawByteString) : RawByteString;
var
   blobIn, blobKey, blobOut : CRYPT_INTEGER_BLOB;
   prompts : CRYPTPROTECT_PROMPTSTRUCT;
begin
   prompts.cbSize:=SizeOf(prompts);
   prompts.dwPromptFlags:=0;
   prompts.hwndApp:=0;
   prompts.szPrompt:=nil;

   blobOut.cbData:=0;
   blobOut.pbData:=nil;
   if not CryptProtectData(CryptIntegerBlob(blobIn, data), nil,
                           CryptIntegerBlob(blobKey, key), nil,
                           @prompts, CRYPTPROTECT_UI_FORBIDDEN,
                           @blobOut) then
      RaiseLastOSError;
   try
      SetLength(Result, blobOut.cbData);
      if Result<>'' then
         System.Move(blobOut.pbData^, Pointer(Result)^, blobOut.cbData);
   finally
      LocalFree(blobOut.pbData);
   end;
end;

// CryptUnProtect
//
function CryptUnProtect(const data, key : RawByteString) : RawByteString;
var
   blobIn, blobKey, blobOut : CRYPT_INTEGER_BLOB;
   prompts : CRYPTPROTECT_PROMPTSTRUCT;
begin
   prompts.cbSize:=SizeOf(prompts);
   prompts.dwPromptFlags:=0;
   prompts.hwndApp:=0;
   prompts.szPrompt:=nil;

   blobOut.cbData:=0;
   blobOut.pbData:=nil;
   if not CryptUnProtectData(CryptIntegerBlob(blobIn, data), nil,
                             CryptIntegerBlob(blobKey, key), nil,
                             @prompts, CRYPTPROTECT_UI_FORBIDDEN,
                             @blobOut) then
      RaiseLastOSError;
   try
      SetLength(Result, blobOut.cbData);
      if Result<>'' then
         System.Move(blobOut.pbData^, Pointer(Result)^, blobOut.cbData);
   finally
      LocalFree(blobOut.pbData);
   end;
end;

end.
