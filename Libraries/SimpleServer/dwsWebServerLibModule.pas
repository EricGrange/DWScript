unit dwsWebServerLibModule;

interface

uses
   SysUtils, Classes,
   dwsComp, dwsExprs,
   dwsWebServerInfo;

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
begin
   Info.ResultAsInteger:=0;
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

end.
