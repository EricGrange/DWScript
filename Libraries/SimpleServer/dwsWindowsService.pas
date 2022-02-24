{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    Portions inspired from mORMotService by Arnaud Bouchez            }
{    https://github.com/synopse  &  https://synopse.info               }
{                                                                      }
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsWindowsService;

{$I dws.inc}

interface

uses
   Windows, Winapi.WinSvc, ShellAPI, SysUtils,
   dwsJSON;

type
   EServiceException = class (Exception);

   TServiceState = (
      ssNotInstalled, ssStopped, ssStarting, ssStopping, ssRunning,
      ssResuming, ssPausing, ssPaused, ssErrorRetrievingState, ssAccessDenied
   );

   TdwsServiceController = class
      private
         FSCHandle : THandle;
         FHandle : THandle;
         FStatus : TServiceStatus;
         FName : String;

      private
         function GetStatus : TServiceStatus;
         function GetState : TServiceState;
         procedure SetDescription(const aDescription : String);

      public
         constructor CreateNewService(
            const TargetComputer, DatabaseName, Name, DisplayName, Path: String;
            const OrderGroup: String = ''; const Dependencies: String = '';
            const Username: String = ''; const Password: String = '';
            DesiredAccess: DWORD = SERVICE_ALL_ACCESS;
            ServiceType: DWORD = SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
            StartType: DWORD = SERVICE_DEMAND_START;
            ErrorControl: DWORD = SERVICE_ERROR_NORMAL
            );
      constructor CreateOpenService(
            const TargetComputer, DataBaseName, Name: String;
            DesiredAccess: DWORD = SERVICE_ALL_ACCESS
            );
      destructor Destroy; override;

      property SCHandle : THandle read FSCHandle;
      property Handle : THandle read FHandle;
      property Status : TServiceStatus read GetStatus;
      property State : TServiceState read GetState;

      function Stop : Boolean;
      function Delete : Boolean;
      function Start(const args : array of PChar) : Boolean;
   end;

   TdwsWindowsService = class;

   TServiceControlEvent = procedure (Sender : TdwsWindowsService; code : DWORD) of object;
   TServiceEvent = procedure (Sender : TdwsWindowsService) of object;

   TdwsWindowsService = class
      private
         FServiceName : String;
         FServiceDisplayName : String;
         FServiceDescription : String;

         FOptions : TdwsJSONValue;

         FStatusRec : TServiceStatus;
         FStatusHandle : THandle;

         FOnControl : TServiceControlEvent;
         FOnInterrogate : TServiceEvent;
         FOnPause : TServiceEvent;
         FOnShutdown : TServiceEvent;
         FOnStart : TServiceEvent;
         FOnExecute : TServiceEvent;
         FOnResume : TServiceEvent;
         FOnStop : TServiceEvent;

      protected
         procedure LoadOptions; virtual;
         procedure LoadServiceOptions(serviceOptions : TdwsJSONValue); virtual;

         procedure DoCtrlHandle(Code: DWORD); virtual;
         function  ReportStatus(dwState, dwExitCode, dwWait: DWORD) : Boolean;
         procedure Execute; virtual;

      public
         constructor Create(aOptions : TdwsJSONValue); reintroduce; virtual;
         destructor Destroy; override;

         class function DefaultServiceOptions : String; virtual; abstract;

         procedure ExecuteCommandLineParameters(var abortExecution : Boolean); virtual;
         procedure WriteCommandLineHelp; virtual;
         procedure WriteCommandLineHelpExtra; virtual;

         function ServiceStatus(var status : TServiceStatus) : Boolean;
         function LaunchedBySCM : Boolean;

         property Options : TdwsJSONValue read FOptions;

         property ServiceName : String read FServiceName;
         property ServiceDisplayName : String read FServiceDisplayName;
         property ServiceDescription : String read FServiceDescription;

         property OnStart : TServiceEvent read FOnStart write FOnStart;
         property OnExecute : TServiceEvent read FOnExecute write FOnExecute;
         property OnControl : TServiceControlEvent read FOnControl write FOnControl;
         property OnStop : TServiceEvent read FOnStop write FOnStop;
         property OnPause : TServiceEvent read FOnPause write FOnPause;
         property OnResume : TServiceEvent read FOnResume write FOnResume;
         property OnInterrogate : TServiceEvent read FOnInterrogate write FOnInterrogate;
         property OnShutdown : TServiceEvent read FOnShutdown write FOnShutdown;
   end;

function ServicesRun: Boolean;

function Process_ReRunIfNotElevated : Boolean;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

var
   vService : TdwsWindowsService;

// ServiceControlHandler
//
procedure ServiceControlHandler(Opcode: LongWord); stdcall;
begin
   if vService<>nil then
      vService.DoCtrlHandle(Opcode);
end;

// ServiceProc
//
procedure ServiceProc(ArgCount: DWORD; Args: PLPWSTR); stdcall;
begin
   if vService = nil then Exit;
   if vService.ServiceName <> Args^ then Exit;

   vService.FStatusHandle := RegisterServiceCtrlHandler(
      Pointer(vService.ServiceName),
      @ServiceControlHandler
   );
   if vService.FStatusHandle = 0 then begin
      vService.ReportStatus(SERVICE_STOPPED, GetLastError, 0);
   end else begin
      vService.ReportStatus(SERVICE_START_PENDING, 0, 0);
      vService.Execute;
   end;
end;

// ServicesRun
//
function ServicesRun : Boolean;
var
   serviceTable : TServiceTableEntry;
begin
   if vService = nil then Exit(False);
   serviceTable.lpServiceName := Pointer(vService.ServiceName);
   serviceTable.lpServiceProc := ServiceProc;
   Result := StartServiceCtrlDispatcher(serviceTable);
end;

// CurrentStateToServiceState
//
function CurrentStateToServiceState(currentState : DWORD) : TServiceState;
begin
   case CurrentState of
      SERVICE_STOPPED:          Result := ssStopped;
      SERVICE_START_PENDING:    Result := ssStarting;
      SERVICE_STOP_PENDING:     Result := ssStopping;
      SERVICE_RUNNING:          Result := ssRunning;
      SERVICE_CONTINUE_PENDING: Result := ssResuming;
      SERVICE_PAUSE_PENDING:    Result := ssPausing;
      SERVICE_PAUSED:           Result := ssPaused;
   else
      Result := ssNotInstalled;
   end;
end;

// IsElevated
//
function IsElevated : Integer;
const
   cTokenElevation = TTokenInformationClass(20);
type
   TOKEN_ELEVATION = record
      TokenIsElevated : DWORD;
   end;
var
   tokenHandle : THandle;
   resultLength : Cardinal;
   elevation : TOKEN_ELEVATION;
   gotToken : Boolean;
begin
   Result := 0;
   if not CheckWin32Version(6, 0) then Exit;

   tokenHandle := 0;
   gotToken := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, tokenHandle);
   if (not gotToken) and (GetLastError = ERROR_NO_TOKEN) then
      gotToken := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, tokenHandle);
   if gotToken then begin
      try
         resultLength := 0;
         if GetTokenInformation(tokenHandle, cTokenElevation, @elevation, SizeOf(elevation), resultLength) then begin
            if elevation.TokenIsElevated <> 0 then
               Result := 1
         end else Result := -1;
      finally
         CloseHandle(tokenHandle);
      end;
   end else Result := -1;
end;

// Process_ReRunIfNotElevated
//
function Process_ReRunIfNotElevated : Boolean;
var
   executeInfo : TShellExecuteInfo;
   exeName : String;
   parameters : String;
   i : Integer;
begin
   // loop protection
   if ParamStr(ParamCount) = '/is-rerun' then
      Exit(False);

   exeName := ParamStr(0);
   for i := 1 to ParamCount do
      parameters := parameters + '"' + StringReplace(ParamStr(i), '"', '""', [ rfReplaceAll ]) + '" ';
   parameters := parameters + ' /is-rerun';

   FillChar(executeInfo, SizeOf(executeInfo), 0);
   executeInfo.cbSize := SizeOf(executeInfo);
   executeInfo.lpVerb := 'runas';
   executeInfo.lpFile := PChar(exeName);
   executeInfo.lpParameters := PChar(parameters);

   Result := ShellExecuteEx(@executeInfo);
end;

// ------------------
// ------------------ TdwsServiceController ------------------
// ------------------

// CreateNewService
//
constructor TdwsServiceController.CreateNewService(
   const TargetComputer, DatabaseName, Name, DisplayName, Path, OrderGroup,
         Dependencies, Username, Password : String;
   DesiredAccess, ServiceType, StartType, ErrorControl : DWORD);
begin
   inherited Create;
   FName := Name;
   FSCHandle := OpenSCManager(Pointer(TargetComputer), Pointer(DatabaseName), SC_MANAGER_ALL_ACCESS);
   if FSCHandle = 0 then begin
      if GetLastError = 5 then begin
         FStatus.dwCurrentState := Ord(ssAccessDenied);
         Exit;
      end else RaiseLastOSError;
   end;
   FHandle := CreateService(
      FSCHandle, Pointer(Name), Pointer(DisplayName),
      DesiredAccess, ServiceType, StartType, ErrorControl, Pointer(Path),
      Pointer(OrderGroup), nil, Pointer(Dependencies),
      Pointer(Username), Pointer(Password)
   );
   if FHandle=0 then
      RaiseLastOSError;
end;

// CreateOpenService
//
constructor TdwsServiceController.CreateOpenService(
   const TargetComputer, DataBaseName, Name: String; DesiredAccess: DWORD);
begin
   inherited Create;
   FName := Name;
   FSCHandle := OpenSCManager(Pointer(TargetComputer), Pointer(DatabaseName), GENERIC_READ);
   if FSCHandle = 0 then
      RaiseLastOSError;
   FHandle := OpenService(FSCHandle, Pointer(Name), DesiredAccess);
end;

// Destroy
//
destructor TdwsServiceController.Destroy;
begin
   if FHandle <> 0 then
      CloseServiceHandle(FHandle);
   if FSCHandle <> 0 then
      CloseServiceHandle(FSCHandle);
   inherited;
end;

// Delete
//
function TdwsServiceController.Delete : Boolean;
begin
   Result := False;
   if FHandle <> 0 then begin
      if DeleteService(FHandle) then begin
         Result := CloseServiceHandle(FHandle);
         FHandle := 0;
      end else RaiseLastOSError;
   end;
end;

// GetState
//
function TdwsServiceController.GetState : TServiceState;
begin
   if (Self = nil) or (FSCHandle = 0) then
      Result := ssErrorRetrievingState
   else if FHandle = 0 then
      Result := ssNotInstalled
   else Result := CurrentStateToServiceState(Status.dwCurrentState);
end;

// SetDescription
//
procedure TdwsServiceController.SetDescription(const aDescription : String);
begin
  if aDescription <> '' then
     ChangeServiceConfig2(FHandle, SERVICE_CONFIG_DESCRIPTION, @aDescription);
end;

// GetStatus
//
function TdwsServiceController.GetStatus : TServiceStatus;
begin
   FillChar(FStatus, Sizeof(FStatus), 0);
   QueryServiceStatus(FHandle, FStatus);
   Result := FStatus;
end;

// Start
//
function TdwsServiceController.Start(const args : array of PChar) : Boolean;
var
   a : PWideChar;
begin
   a := @args[0];
   Result := StartService(FHandle, length(Args), a);
end;

// Stop
//
function TdwsServiceController.Stop: Boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_STOP, FStatus);
end;

// ------------------
// ------------------ TdwsWindowsService ------------------
// ------------------

// Create
//
constructor TdwsWindowsService.Create(aOptions : TdwsJSONValue);
begin
   Assert(vService = nil);
   vService := Self;
   try
      FOptions := aOptions.Clone;
      LoadOptions;

      if ServiceDisplayName = '' then
         FServiceDisplayName := ServiceName;
      FStatusRec.dwServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
      FStatusRec.dwCurrentState := SERVICE_STOPPED;
      FStatusRec.dwControlsAccepted := 31;
      FStatusRec.dwWin32ExitCode := NO_ERROR;
   except
      vService := nil;
      raise;
   end;
end;

// Destroy
//
destructor TdwsWindowsService.Destroy;
begin
   Assert((vService = nil) or (vService = Self));
   inherited;
   FOptions.Free;
   vService := nil;
end;

// LoadServiceOptions
//
procedure TdwsWindowsService.LoadServiceOptions(serviceOptions : TdwsJSONValue);
var
   options : TdwsJSONValue;
begin
   options := TdwsJSONValue.ParseString(DefaultServiceOptions);
   try
      options.Extend(serviceOptions);

      FServiceName := options['Name'].AsString;
      FServiceDisplayName := options['DisplayName'].AsString;
      FServiceDescription := options['Description'].AsString;
   finally
      options.Free;
   end;
end;

// LoadOptions
//
procedure TdwsWindowsService.LoadOptions;
begin
   LoadServiceOptions(Options.Items['Service']);
end;

// ExecuteCommandLineParameters
//
procedure TdwsWindowsService.ExecuteCommandLineParameters(var abortExecution : Boolean);
type
   TServiceStateEx = (ssNotInstalled, ssStopped, ssStarting, ssStopping, ssRunning,
        ssResuming, ssPausing, ssPaused, ssErrorRetrievingState, ssAccessDenied);
var
   i : Integer;
   param : String;
   ctrl : TdwsServiceController;
   serviceState : TServiceStateEx;
   deviceCheck : array [0..2] of Char;
   buffer : array [0..MAX_PATH] of Char;
   err : Integer;
begin
   ctrl := TdwsServiceController.CreateOpenService('', '', ServiceName);
   try
      err := GetLastError;
      serviceState := TServiceStateEx(ctrl.State);
      // clarify state
      case serviceState of
         ssNotInstalled : begin
            case err of
               ERROR_ACCESS_DENIED :
                  serviceState := ssAccessDenied;
               ERROR_INVALID_NAME : begin
                  WriteLn('Invalid service name');
                  serviceState := ssErrorRetrievingState;
               end;
            end;
         end;
         ssErrorRetrievingState : begin
            case err of
               ERROR_SERVICE_DOES_NOT_EXIST :
                  serviceState := ssNotInstalled;
            end;
         end;
      end;
      for i:=1 to ParamCount do begin
         abortExecution := True;
         param:=ParamStr(i);
         if param='/install' then begin
            case serviceState of
               ssNotInstalled : begin
                  ctrl.Free;
                  ctrl:=nil;
                  deviceCheck[0]:=ParamStr(0)[1];
                  deviceCheck[1]:=':';
                  deviceCheck[2]:=#0;
                  // subst'ed path and services don't mix
                  // if the drive is a subst'ed one will return \??\xxxx
                 if    (QueryDosDevice(@deviceCheck, @buffer, MAX_PATH)=0)
                     or (buffer[1]='?') then
                     writeln('Must install from actual, non-subst''ed path')
                  else begin
                     ctrl:=TdwsServiceController.CreateNewService(
                        '','', ServiceName, ServiceDisplayName,
                        ParamStr(0), '', '', '', '',
                        SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_DEMAND_START);
                     case TServiceStateEx(ctrl.State) of
                        ssNotInstalled :
                           Writeln('Failed to install');
                        ssErrorRetrievingState, ssAccessDenied : begin
                           if not Process_ReRunIfNotElevated then
                              Writeln('Account has insufficient rights, cannot install');
                        end;
                     else
                        ctrl.SetDescription(ServiceDescription);
                        Writeln('Installed successfully');
                     end;
                  end;
               end;
               ssErrorRetrievingState, ssAccessDenied :
                  if not Process_ReRunIfNotElevated then
                     Writeln('Account has insufficient rights');
            else
                Writeln('Already installed');
            end;
         end else if param='/uninstall' then begin
            case serviceState of
               ssNotInstalled :
                  Writeln('Not installed');
               ssErrorRetrievingState, ssAccessDenied :
                  if not Process_ReRunIfNotElevated then
                     Writeln('Account has insufficient rights');
            else
               ctrl.Stop;
               if ctrl.State<>TServiceState.ssStopped then
                  Writeln('Failed to stop')
               else begin
                  ctrl.Delete;
                  if ctrl.State in [TServiceState.ssNotInstalled, TServiceState.ssErrorRetrievingState] then
                     Writeln('Uninstalled successfully')
                  else Writeln('Failed to uninstall');
               end;
            end;
         end else if param='/stop' then begin
            case serviceState of
               ssNotInstalled :
                  Writeln('Not installed');
               ssErrorRetrievingState, ssAccessDenied :
                  if not Process_ReRunIfNotElevated then
                     Writeln('Account has insufficient rights');
            else
               ctrl.Stop;
               Writeln('Stop command issued')
            end;
         end else if param='/start' then begin
            case serviceState of
               ssNotInstalled :
                  Writeln('Not installed');
               ssErrorRetrievingState, ssAccessDenied :
                  if not Process_ReRunIfNotElevated then
                     Writeln('Account has insufficient rights');
            else
               ctrl.Start([]);
               Writeln('Start command issued')
            end;
         end else begin
            WriteCommandLineHelp;

            Write('Service is currently ');
            case serviceState of
               ssNotInstalled : Writeln('not installed');
               ssStopped : Writeln('stopped');
               ssStarting : Writeln('starting');
               ssStopping : Writeln('stopping');
               ssRunning : Writeln('running');
               ssPausing : Writeln('pausing');
               ssPaused : Writeln('paused');
               ssErrorRetrievingState : Writeln('in an unknown state (error retrieving state)');
               ssAccessDenied : Writeln('in an unknown state (insufficient rights)');
            else
               Writeln('in an unsupported state');
            end;
            Break;
         end;
      end;
   finally
      ctrl.Free;
   end;
end;

// WriteCommandLineHelp
//
procedure TdwsWindowsService.WriteCommandLineHelp;
begin
   Writeln( ServiceName + ' (' + ServiceDisplayName + ')' + #13#10#13#10
           +'Parameters:'#13#10
           +'* none : run as application'#13#10
           +'* /install & /uninstall : install & uninstall service'#13#10
           +'* /start & /stop : start & stop service');
   WriteCommandLineHelpExtra;
end;

// WriteCommandLineHelpExtra
//
procedure TdwsWindowsService.WriteCommandLineHelpExtra;
begin
   Writeln(#13#10'For more information, go to https://www.delphitools.info/'#13#10);
end;

// ServiceState
//
function TdwsWindowsService.ServiceStatus(var status : TServiceStatus) : Boolean;
var
   scHandle, svInfo : Integer;
begin
   Result := False;

   scHandle := OpenSCManager(nil, nil, GENERIC_READ);
   try
      svInfo:=OpenService(scHandle, PChar(ServiceName), GENERIC_READ);
      if svInfo<>0 then begin
         try
            QueryServiceStatus(svInfo, status);
         finally
            CloseServiceHandle(svInfo);
         end;
         Result:=True;
      end;
   finally
      CloseServiceHandle(scHandle);
   end;
end;

// LaunchedBySCM
//
function TdwsWindowsService.LaunchedBySCM : Boolean;
var
   status : TServiceStatus;
begin
   if ServiceStatus(status) then
      Result := (status.dwCurrentState=SERVICE_START_PENDING)
   else Result := False;
end;

// DoCtrlHandle
//
procedure TdwsWindowsService.DoCtrlHandle(Code: DWORD);
begin
   case Code of
      SERVICE_CONTROL_STOP: begin
         ReportStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
         try
            if Assigned(FOnStop) then
               FOnStop(Self);
            ReportStatus(SERVICE_STOPPED, NO_ERROR, 0);
         except
            ReportStatus(SERVICE_STOPPED, ERROR_CAN_NOT_COMPLETE, 0);
         end;
      end;
      SERVICE_CONTROL_PAUSE: begin
         ReportStatus(SERVICE_PAUSE_PENDING, NO_ERROR, 0);
         try
            if Assigned(FOnPause) then
               FOnPause(Self);
            ReportStatus(SERVICE_PAUSED, NO_ERROR, 0)
         except
            ReportStatus(SERVICE_PAUSED, ERROR_CAN_NOT_COMPLETE, 0)
         end;
      end;
      SERVICE_CONTROL_CONTINUE: begin
         ReportStatus(SERVICE_CONTINUE_PENDING, NO_ERROR, 0);
         try
            if Assigned(FOnResume) then
               FOnResume(Self);
            ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
         except
            ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
         end;
      end;
      SERVICE_CONTROL_SHUTDOWN: begin
         if Assigned(FOnShutdown) then
            FOnShutdown(Self);
         Code := 0;
      end;
      SERVICE_CONTROL_INTERROGATE: begin
         SetServiceStatus(FStatusHandle, FStatusRec);
         if Assigned(FOnInterrogate) then
            FOnInterrogate(Self);
      end;
   end;
   if Assigned(FOnControl) then
      FOnControl(Self, Code);
end;

// Execute
//
procedure TdwsWindowsService.Execute;
begin
   try
      if Assigned(FOnStart) then
         FOnStart(@Self);
      ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
      if Assigned(FOnExecute) then
         FOnExecute(@Self);
   except
      ReportStatus(SERVICE_RUNNING, ERROR_CAN_NOT_COMPLETE, 0);
   end;
end;

// ReportStatus
//
function TdwsWindowsService.ReportStatus(dwState, dwExitCode, dwWait: DWORD): Boolean;
begin
   if dwState = SERVICE_START_PENDING then
      FStatusRec.dwControlsAccepted := 0
   else FStatusRec.dwControlsAccepted := 31;
   FStatusRec.dwCurrentState  := dwState;
   FStatusRec.dwWin32ExitCode := dwExitCode;
   FStatusRec.dwWaitHint := dwWait;
   if (dwState = SERVICE_RUNNING) or (dwState = SERVICE_STOPPED) then
      FStatusRec.dwCheckPoint := 0
   else Inc(FStatusRec.dwCheckPoint);
   Result := SetServiceStatus(FStatusHandle, FStatusRec);
   if not Result then
      RaiseLastOSError;
end;

end.
