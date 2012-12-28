unit dwsWebLibModule;

interface

uses
  SysUtils, Classes, dwsComp, dwsExprs, dwsWebEnvironment;

type
  TdwsWebLib = class(TDataModule)
    dwsWeb: TdwsUnit;
    procedure dwsWebClassesWebRequestMethodsURLEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsMethodEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsPathInfoEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsQueryStringEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsRemoteIPEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsContentDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsContentTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsContentEncodingEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHeaderEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHeadersEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsCookieEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsCookiesEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsQueryFieldEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsQueryFieldsEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetContentTextEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsSecureEval(Info: TProgramInfo;
      ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookieEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Cookies.Values['Info.ParamAsString[0]'];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookiesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Cookies.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Header(Info.ParamAsString[0]);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHeadersEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Headers.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsMethodEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Method;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsPathInfoEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.PathInfo;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.QueryFields.Values[Info.ParamAsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryFieldsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.QueryFields.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsQueryStringEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.QueryString;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsRemoteIPEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.RemoteIP;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsSecureEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Security;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsURLEval(Info: TProgramInfo;
  ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.URL;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsContentDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentData:=Info.ParamAsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsContentEncodingEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentEncoding:=Info.ParamAsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentType:=Info.ParamAsDataString[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentText[Info.ParamAsDataString[0]]:=Info.ParamAsString[1];
end;

end.
