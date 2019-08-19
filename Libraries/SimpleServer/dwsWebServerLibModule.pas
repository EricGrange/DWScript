unit dwsWebServerLibModule;

interface

uses
   SysUtils, Classes, SynCommons,
   dwsComp, dwsExprs, dwsSymbols, dwsUtils,
   dwsWebServerInfo, dwsWebEnvironment;

type
  TdwsWebServerLib = class(TDataModule)
    dwsWebServer: TdwsUnit;
    procedure dwsWebServerClassesWebServerMethodsNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsHttpPortEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsHttpsPortEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsAuthenticationsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsFlushCompiledProgramsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsLiveQueriesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsCompilationInfoJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsGetRewriteRulesJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsSetURLRewriteRulesJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsCompiledProgramsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerMethodsExecutionInfoJSONEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
    FServer :  IWebServerInfo;
  public
    { Public declaration }
    property Server : IWebServerInfo read FServer write FServer;
  end;

implementation

{$R *.dfm}

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsAuthenticationsEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   auth : TWebRequestAuthentications;
begin
   auth := FServer.Authentications;
   Info.ResultAsInteger :=    (Ord(wraBasic in auth) shl Ord(wraBasic))
                           or (Ord(wraDigest in auth) shl Ord(wraDigest))
                           or (Ord(wraNTLM in auth) shl Ord(wraNTLM))
                           or (Ord(wraNegotiate in auth) shl Ord(wraNegotiate))
                           or (Ord(wraKerberos in auth) shl Ord(wraKerberos))
                           or (Ord(wraAuthorization in auth) shl Ord(wraAuthorization));
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsCompilationInfoJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := FServer.CompilationInfoJSON(Info.ParamAsString[0]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsCompiledProgramsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.CompiledPrograms;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsExecutionInfoJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := FServer.ExecutionInfoJSON(Info.ParamAsString[0]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsFlushCompiledProgramsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.FlushCompiledPrograms;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsHttpPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=FServer.HttpPort;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsHttpsPortEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=FServer.HttpsPort;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsLiveQueriesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FServer.LiveQueries;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FServer.Name;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsGetRewriteRulesJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := FServer.GetURLRewriteRules;
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerMethodsSetURLRewriteRulesJSONEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.SetURLRewriteRules(Info.ParamAsString[0]);
end;

end.
