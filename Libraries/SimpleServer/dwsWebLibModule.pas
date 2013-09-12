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
    procedure dwsWebClassesWebRequestMethodsUserAgentEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsAuthenticatedUserEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetStatusCodeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetHeaderEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsAuthenticationEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsRequestAuthenticationEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsHasQueryFieldEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetCookieEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetCookie2Eval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebResponseMethodsSetCompressionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentTypeEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentDataEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebClassesWebRequestMethodsContentLengthEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsAuthenticatedUserEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.AuthenticatedUser;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsAuthenticationEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Ord(Info.WebRequest.Authentication);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=Info.WebRequest.ContentData;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentLengthEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=Length(Info.WebRequest.ContentData);
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsContentTypeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsDataString:=Info.WebRequest.ContentType;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookieEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Cookies.Values[Info.ParamAsString[0]];
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsCookiesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.Cookies.Text;
end;

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsHasQueryFieldEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=Info.WebRequest.HasQueryField(Info.ParamAsString[0]);
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

procedure TdwsWebLib.dwsWebClassesWebRequestMethodsUserAgentEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=Info.WebRequest.UserAgent;
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

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsRequestAuthenticationEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   wra : TWebRequestAuthentication;
begin
   wra:=TWebRequestAuthentication(Info.ParamAsInteger[0]);
   Info.WebResponse.Headers.Values['WWW-Authenticate']:=cWebRequestAuthenticationToString[wra];
   Info.WebResponse.StatusCode:=401;
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCompressionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.Compression:=Info.ParamAsBoolean[0];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetContentTextEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.ContentText[Info.ParamAsDataString[0]]:=Info.ParamAsString[1];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCookieEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   cookie : TWebResponseCookie;
begin
   cookie:=Info.WebResponse.Cookies.AddCookie(Info.ParamAsString[0]);
   cookie.Value:=Info.ParamAsString[1];
   cookie.ExpiresGMT:=Info.ParamAsFloat[2];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetCookie2Eval(
  Info: TProgramInfo; ExtObject: TObject);
var
   cookie : TWebResponseCookie;
begin
   cookie:=Info.WebResponse.Cookies.AddCookie(Info.ParamAsString[0]);
   cookie.Value:=Info.ParamAsString[1];
   cookie.ExpiresGMT:=Info.ParamAsFloat[2];
   cookie.Path:=Info.ParamAsString[3];
   cookie.Domain:=Info.ParamAsString[4];
   cookie.Flags:=Info.ParamAsInteger[5];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetHeaderEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.Headers.Values[Info.ParamAsString[0]]:=Info.ParamAsString[1];
end;

procedure TdwsWebLib.dwsWebClassesWebResponseMethodsSetStatusCodeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.WebResponse.StatusCode:=Info.ParamAsInteger[0];
end;

end.
