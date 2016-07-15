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
    procedure dwsWebServerClassesWebServerSentEventsMethodsPostRawEventEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsCloseEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsConnectionsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsWebServerClassesWebServerSentEventsMethodsSourceNamesEval(
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

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsCloseEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   FServer.EventSourceClose(Info.ParamAsString[0]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsConnectionsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.EventSourceConnections(Info.ParamAsString[0]);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsPostRawEventEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   data : TWebServerEventData;
   dyn : IScriptDynArray;
   i : Integer;
begin
   dyn := Info.ParamAsScriptDynArray[3];
   SetLength(data, dyn.ArrayLength);
   for i := 0 to High(data) do
      data[i] := StringToUTF8(dyn.AsString[i]);
   FServer.EventSourcePost(Info.ParamAsString[0], Info.ParamAsString[1], Info.ParamAsString[2], data);
end;

procedure TdwsWebServerLib.dwsWebServerClassesWebServerSentEventsMethodsSourceNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := FServer.EventSourceList;
end;

end.
