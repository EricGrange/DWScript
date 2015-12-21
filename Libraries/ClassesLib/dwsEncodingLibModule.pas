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
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function Base58Encode(const data : RawByteString) : String;
function Base58Decode(const data : String) : RawByteString;

implementation

{$R *.dfm}

const
   cBase58 : String = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';

function Base58Encode(const data : RawByteString) : String;
var
   i, j, carry, n : Integer;
   digits : array of Integer;
begin
   if data = '' then exit;

   Result := '';
   n := -1;
   for j := 1 to Length(data) do begin
      if data[j] = #0 then
         Result := Result + cBase58[1]
      else begin
         SetLength(digits, 1);
         digits[0] := 0;
         n := 0;
         break;
      end;
   end;

   for i := Length(Result)+1 to Length(data) do begin

      for j := 0 to n do
         digits[j] := digits[j] shl 8;

      digits[0] := digits[0] + Ord(data[i]);

      carry := 0;

      for j := 0 to n do begin
         digits[j] := digits[j] + carry;
         carry := digits[j] div 58;
         digits[j] := digits[j] mod 58;
      end;

      while carry > 0 do begin
         Inc(n);
         SetLength(digits, n+1);
         digits[n] := carry mod 58;
         carry := carry div 58;
      end;
   end;

   for j := n downto 0 do
      Result := Result + cBase58[digits[j]+1];
end;

function Base58Decode(const data : String) : RawByteString;
var
   i, j, carry, n, d : Integer;
   bytes : array of Integer;
begin
   if data = '' then exit;

   Result := '';
   n := -1;
   for j := 1 to Length(data) do begin
      if data[j] = '1' then
         Result := Result + #0
      else begin
         SetLength(bytes, 1);
         bytes[0] := 0;
         n := 0;
         break;
      end;
   end;

   for i := Length(Result)+1 to Length(data) do begin
      d := Pos(data[i], cBase58)-1;
      if d<0 then
         raise Exception.Create('Non-base58 character');

      for j := 0 to n do
         bytes[j] := bytes[j]*58;

      bytes[0] := bytes[0]+d;

      carry := 0;
      for j := 0 to n do begin
         bytes[j] := bytes[j] + carry;
         carry := bytes[j] shr 8;
         bytes[j] := bytes[j] and $FF;
      end;

      while carry > 0 do begin
         Inc(n);
         SetLength(bytes, n+1);
         bytes[n] := carry and $FF;
         carry := carry shr 8;
      end;
   end;

   for j := n downto 0 do
      Result := Result + AnsiChar(bytes[j]);
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
   Info.ResultAsDataString := Base64ToBin(Info.ParamAsDataString[0]);
end;

procedure TdwsEncodingLib.dwsEncodingClassesBase64EncoderMethodsEncodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString := BinToBase64(Info.ParamAsDataString[0]);
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
