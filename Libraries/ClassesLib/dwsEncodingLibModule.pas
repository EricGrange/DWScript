unit dwsEncodingLibModule;

interface

uses
  SysUtils, Classes,
  dwsExprs, dwsComp, dwsWebUtils,
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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := Base64ToBin(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := BinToBase64(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesHexadecimalEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   hex, buf : RawByteString;
   n : Integer;
begin
   hex:=Info.ParamAsDataString[0];
   n:=Length(hex);
   if (n and 1)<>0 then
      raise Exception.Create('Expect even hexadecimal character count');
   n:=n div 2;
   SetLength(buf, n);
   if n<>Classes.HexToBin(PAnsiChar(hex), PAnsiChar(buf), n) then
      raise Exception.Create('Invalid characters in hexadecimal');
   Info.ResultAsDataString := buf;
end;

procedure TdwsEncodingLib.dwsEncodingClassesHexadecimalEncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   hex, buf : RawByteString;
   n : Integer;
begin
   buf:=Info.ParamAsDataString[0];
   n:=Length(buf);
   SetLength(hex, n*2);
   Classes.BinToHex(PAnsiChar(buf), PAnsiChar(hex), n);
   Info.ResultAsDataString := hex;
end;

procedure TdwsEncodingLib.dwsEncodingClassesHTMLTextEncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   raise Exception.Create('Not supported... yet');
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

procedure TdwsEncodingLib.dwsEncodingClassesUTF8EncoderMethodsDecodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := UTF8ToUnicodeString(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesUTF8EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := UnicodeStringToUtf8(Info.ParamAsString[0]);
end;

end.
