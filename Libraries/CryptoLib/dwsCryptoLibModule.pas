unit dwsCryptoLibModule;

interface

uses
   SysUtils, Classes,
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
  private
    { Private declarations }
  public
    { Public declaration }
  end;

implementation

{$R *.dfm}

uses SynCrypto;

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
