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
unit dwsEncodingLibModule;

interface

uses
  SysUtils, Classes,
  dwsExprs, dwsComp, dwsWebUtils, dwsUtils,
  SynCommons;

type
  TdwsEncodingLib = class(TDataModule)
    dwsEncoding: TdwsUnit;
    procedure dwsEncodingClassesUTF8EncoderMethodsEncodeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsEncodingClassesUTF8EncoderMethodsDecodeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsEncodingClassesURLEncodedEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesURLEncodedEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase64EncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase64EncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLTextEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase58EncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase58EncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLAttributeEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesHTMLAttributeEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase32EncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesBase32EncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16BigEndianEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16BigEndianEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16LittleEndianEncoderMethodsEncodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsEncodingClassesUTF16LittleEndianEncoderMethodsDecodeEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsEncoding;

{$R *.dfm}

procedure TdwsEncodingLib.dwsEncodingClassesBase32EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=Base32Decode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase32EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Base32Encode(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase58EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := Base58Decode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase58EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := Base58Encode(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := Base64Decode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := Base64Encode(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := dwsUtils.HexToBin(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := dwsUtils.BinToHex(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLAttributeEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLAttributeDecode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLAttributeEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLAttributeEncode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLTextDecode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLTextEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.HTMLTextEncode(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesURLEncodedEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : RawByteString;
begin
   buf := Info.ParamAsDataString[0];
   Info.ResultAsString := WebUtils.DecodeURLEncoded(buf, 1, Length(buf));
end;

procedure TdwsEncodingLib.dwsEncodingClassesURLEncodedEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := WebUtils.EncodeURLEncoded(Info.ParamAsString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16BigEndianEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringWordsToBytes(buf, True);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16BigEndianEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringBytesToWords(buf, True);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16LittleEndianEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringWordsToBytes(buf, False);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF16LittleEndianEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
begin
   buf := Info.ParamAsString[0];
   StringBytesToWords(buf, False);
   Info.ResultAsString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF8EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := UTF8ToString(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF8EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := StringToUTF8(Info.ParamAsString[0]);
end;

end.
