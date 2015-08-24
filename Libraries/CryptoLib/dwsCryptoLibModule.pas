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
   dwsComp, dwsExprs, dwsUtils;

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
  private
    { Private declarations }
  public
    { Public declaration }
  end;

implementation

{$R *.dfm}

uses SynCrypto, SynZip, dwsRipeMD160, dwsCryptProtect, dwsSHA3;

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

procedure TdwsCryptoLib.dwsCryptoClassesHashRIPEMD160MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   data : RawByteString;
   digest : TRipe160Digest;
   remaining : Integer;
   p : PRipe160Block;
begin
   data := Info.ParamAsDataString[0];
   p := PRipe160Block(data);
   remaining := Length(data);

   RipeMD160Init(digest);
   while remaining >= SizeOf(TRipe160Block) do begin
      RipeMD160(digest, p);
      Inc(p);
      Dec(remaining, SizeOf(TRipe160Block));
   end;
   RipeMD160Final(digest, p, remaining, Length(data));

   Info.ResultAsString := BinToHex(digest, SizeOf(digest));
end;

procedure TdwsCryptoLib.dwsCryptoClassesHashSHA3_256MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   data : RawByteString;
   sponge : TSpongeState;
   hash : array [0..256 div 8-1] of Byte;
begin
   data := Info.ParamAsDataString[0];

   SHA3_Init(sponge, SHA3_256);
   SHA3_Update(sponge, Pointer(data), Length(data));
   SHA3_FinalHash(sponge, @hash);

   Info.ResultAsString := BinToHex(hash, SizeOf(hash));
end;

procedure TdwsCryptoLib.dwsCryptoClassesMD5MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=MD5(Info.ParamAsDataString[0]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesSHA1MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=SHA1(Info.ParamAsDataString[0]);
end;

procedure TdwsCryptoLib.dwsCryptoClassesSHA256MethodsHashDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=SHA256(Info.ParamAsDataString[0]);
end;

end.
