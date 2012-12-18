{
    Will serve static content and DWS dynamic content via http.sys
    kernel mode high-performance HTTP server (available since XP SP2).
    See http://blog.synopse.info/post/2011/03/11/HTTP-server-using-fast-http.sys-kernel-mode-server
    WARNING: you need to first register the server URI and port to the http.sys stack.
    That is, run the application at least once as administrator.

    The executable can be run either as an application or as service.

    Sample based on official mORMot's samples
    SQLite3\Samples\09 - HttpApi web server\HttpApiServer.dpr
    SQLite3\Samples\10 - Background Http service

    Synopse mORMot framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

    Original tri-license: MPL 1.1/GPL 2.0/LGPL 2.1

    You would need at least the following files from mORMot framework
    to be available in your project path:
    - SynCommons.pas
    - Synopse.inc
    - SynLZ.pas
    - SynZip.pas
    - SynCrtSock.pas
    - SynWinWock.pas
    - mORMotService.pas
    http://synopse.info/fossil/wiki?name=Downloads

}
program SynopseWebServer;

{$APPTYPE CONSOLE}

uses
   Windows, SysUtils, WinSvc,
   mORMotService,
   USynopseSimpleWebServer in 'USynopseSimpleWebServer.pas';

function CreateNewServer : TSynopseSimpleServer;
var
   basePath : String;
begin
   basePath:=ExtractFilePath(ParamStr(0));
   if DirectoryExists(basePath+'www') then
      basePath:=basePath+'www' // subfolder 'ww' of where the exe is placed
   else if FileExists(ChangeFileExt(ParamStr(0), '.dpr')) then
      basePath:=basePath+'..\Data\www' // if compiled alongside dpr
   else basePath:=basePath+'..\..\..\Data\www'; // assume compiled in platform/target
   Result:=TSynopseSimpleServer.Create(basePath);
end;


type
   TWebServerHttpService = class(TService)
      public
         Server: TSynopseSimpleServer;

         procedure DoStart(Sender: TService);
         procedure DoStop(Sender: TService);

         constructor Create; reintroduce;
         destructor Destroy; override;
   end;

const
   cHttpServiceName = 'DWSSynWebServer';
   cHttpServiceDisplayName = 'DWScript SynopseWebServer Service';

{ TWebServerHttpService }

constructor TWebServerHttpService.Create;
begin
   inherited Create(cHttpServiceName,cHttpServiceDisplayName);
   OnStart := DoStart;
   OnStop := DoStop;
   OnResume := DoStart;
   OnPause := DoStop;
end;

destructor TWebServerHttpService.Destroy;
begin
  DoStop(nil);
  inherited;
end;

procedure TWebServerHttpService.DoStart(Sender: TService);
begin
  if Server<>nil then
    DoStop(nil); // should never happen

   Server:=CreateNewServer;
end;

procedure TWebServerHttpService.DoStop(Sender: TService);
begin
   if Server=nil then
      exit;
   FreeAndNil(Server);
end;

procedure CheckParameters;
var
   i : Integer;
   param : String;
   ctrl : TServiceController;
begin
   ctrl:=TServiceController.CreateOpenService('','',cHttpServiceName);
   try
      if ctrl.State<>ssErrorRetrievingState then
         for i:=1 to ParamCount do begin
            param:=SysUtils.LowerCase(paramstr(i));
            if param='/install' then begin
               TServiceController.CreateNewService('','',cHttpServiceName,
                                                   cHttpServiceDisplayName, paramstr(0),'','','','',
                                                   SERVICE_ALL_ACCESS,
                                                   SERVICE_WIN32_OWN_PROCESS,
                                                   SERVICE_DEMAND_START)
               .Free
            end else if param='/uninstall' then begin
               ctrl.Stop;
               ctrl.Delete;
            end else if param='/stop' then
               ctrl.Stop
            else if param='/start' then
               ctrl.Start([])
            else begin
               Writeln( cHttpServiceName+#13#10#13#10
                       +'Parameters:'#13#10
                       +'* none : run as application'#13#10
                       +'* /install & /uninstall : install & uninstall service'#13#10
                       +'* /start & /stop : start & stop service');
            end;
         end;
   finally
      ctrl.Free;
   end;
end;

function LaunchedBySCM : Boolean;
var
	scHandle, svInfo : Integer;
	servStat : TServiceStatus;
begin
   Result:=False;
   scHandle:=OpenSCManager(nil, nil, GENERIC_READ);
   try
      svInfo:=OpenService(scHandle, PChar(cHttpServiceName), GENERIC_READ);
      if svInfo<>0 then begin
         try
            QueryServiceStatus(svInfo, servStat);
            Result:=(servStat.dwCurrentState=SERVICE_START_PENDING);
         finally
            CloseServiceHandle(svInfo);
         end;
      end;
   finally
      CloseServiceHandle(scHandle);
   end;
end;

begin
   if ParamCount<>0 then

      CheckParameters

   else if LaunchedBySCM then begin

      // started as service
      with TWebServerHttpService.Create do try
         ServicesRun;
      finally
         Free;
      end;

   end else begin

      // started as application
      with CreateNewServer  do try
         write( 'Server is now running on http://localhost:888/'#13#10#13#10
               +'Press [Enter] to quit');
         readln;
      finally
         Free;
      end

   end;
end.
