program IndyWebServer;
{$APPTYPE CONSOLE}

uses
  SysUtils,
  Windows,
  IdHTTPWebBrokerBridge,
  WebReq,
  WebBroker,
  WMDWS in 'WMDWS.pas' {WebModuleDWS: TWebModule},
  DSimpleDWScript in '..\..\Libraries\SimpleServer\DSimpleDWScript.pas' {SimpleDWScript: TDataModule};

{$R *.res}

procedure RunServer(APort: Integer);
var
  LInputRecord: TInputRecord;
  LEvent: DWord;
  LHandle: THandle;
  LServer: TIdHTTPWebBrokerBridge;
begin
  Writeln(Format('Starting HTTP Server on port %d', [APort]));
  LServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    LServer.DefaultPort := APort;
    LServer.Active := True;
    Writeln('Press ESC to stop the server');
    LHandle := GetStdHandle(STD_INPUT_HANDLE);
    while True do
    begin
      if not ReadConsoleInput(LHandle, LInputRecord, 1, LEvent) then
         RaiseLastOSError;
      if (LInputRecord.EventType = KEY_EVENT) and
      LInputRecord.Event.KeyEvent.bKeyDown and
      (LInputRecord.Event.KeyEvent.wVirtualKeyCode = VK_ESCAPE) then
        break;
    end;
  finally
    LServer.Free;
  end;
end;

begin
  try
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(1080);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end
end.
