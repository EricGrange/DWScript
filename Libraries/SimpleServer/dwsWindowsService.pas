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
{    Copyright Creative IT.                                            }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsWindowsService;

interface

uses
   Windows, WinSvc,
   SysUtils, TypInfo,
   mORMotService,
   dwsJSON;

type
   TdwsWindowsService = class (TServiceSingle)
      private
         FOptions : TdwsJSONValue;

      protected
         procedure LoadOptions; virtual;
         procedure LoadServiceOptions(serviceOptions : TdwsJSONValue); virtual;

      public
         ServiceName : String;
         ServiceDisplayName : String;
         ServiceDescription : String;

         constructor Create(aOptions : TdwsJSONValue); reintroduce; virtual;
         destructor Destroy; override;

         class function DefaultServiceOptions : String; virtual; abstract;

         procedure ExecuteCommandLineParameters; virtual;
         procedure WriteCommandLineHelp; virtual;

         function ServiceStatus(var status : TServiceStatus) : Boolean;
         function LaunchedBySCM : Boolean;

         property Options : TdwsJSONValue read FOptions;
   end;

implementation

const
   SERVICE_CONFIG_DESCRIPTION = 1;

type
   TServiceDescription = record
      lpDescription : PWideChar;
   end;

function ChangeServiceConfig2(hService : SC_HANDLE; dwsInfoLevel : DWORD;
                              lpInfo : Pointer) : BOOL; stdcall;
                              external 'advapi32.dll' name 'ChangeServiceConfig2W';

// ------------------
// ------------------ TdwsWindowsService ------------------
// ------------------

// Create
//
constructor TdwsWindowsService.Create(aOptions : TdwsJSONValue);
begin
   FOptions:=aOptions.Clone;
   LoadOptions;
   inherited Create(ServiceName, ServiceDisplayName);
end;

// Destroy
//
destructor TdwsWindowsService.Destroy;
begin
   inherited;
   FOptions.Free;
end;

// LoadServiceOptions
//
procedure TdwsWindowsService.LoadServiceOptions(serviceOptions : TdwsJSONValue);
var
   options : TdwsJSONValue;
begin
   options:=TdwsJSONValue.ParseString(DefaultServiceOptions);
   try
      options.Extend(serviceOptions);

      ServiceName:=options['Name'].AsString;
      ServiceDisplayName:=options['DisplayName'].AsString;
      ServiceDescription:=options['Description'].AsString;
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
procedure TdwsWindowsService.ExecuteCommandLineParameters;
type
   TServiceStateEx = (ssNotInstalled, ssStopped, ssStarting, ssStopping, ssRunning,
        ssResuming, ssPausing, ssPaused, ssErrorRetrievingState, ssAccessDenied);
var
   i : Integer;
   param : String;
   ctrl : TServiceController;
   serviceState : TServiceStateEx;
   deviceCheck : array [0..2] of Char;
   buffer : array [0..MAX_PATH] of Char;
   description : TServiceDescription;
   err : Integer;
begin
   ctrl:=TServiceController.CreateOpenService('', '', ServiceName);
   try
      err := GetLastError;
      serviceState := TServiceStateEx(ctrl.State);
      if serviceState = ssNotInstalled then begin
         // clarify state
         case err of
            ERROR_ACCESS_DENIED :
               serviceState := ssAccessDenied;
            ERROR_INVALID_NAME : begin
               WriteLn('Invalid service name');
               serviceState := ssErrorRetrievingState;
            end;
         end;
      end;
      for i:=1 to ParamCount do begin
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
                     ctrl:=TServiceController.CreateNewService(
                        '','', ServiceName, ServiceDisplayName,
                        ParamStr(0), '', '', '', '',
                        SERVICE_ALL_ACCESS, SERVICE_WIN32_OWN_PROCESS, SERVICE_DEMAND_START);
                     case TServiceStateEx(ctrl.State) of
                        ssNotInstalled :
                           Writeln('Failed to install');
                        ssErrorRetrievingState :
                           Writeln('Account has insufficient rights, cannot install');
                     else
                        description.lpDescription:=PChar(ServiceDescription);
                        ChangeServiceConfig2(ctrl.Handle, SERVICE_CONFIG_DESCRIPTION, @description);
                        Writeln('Installed successfully');
                     end;
                  end;
               end;
               ssErrorRetrievingState, ssAccessDenied :
                  Writeln('Account has insufficient rights');
            else
                Writeln('Already installed');
            end;
         end else if param='/uninstall' then begin
            case serviceState of
               ssNotInstalled :
                  Writeln('Not installed');
               ssErrorRetrievingState, ssAccessDenied :
                  Writeln('Account has insufficient rights');
            else
               ctrl.Stop;
               if ctrl.State<>TServiceState.ssStopped then
                  Writeln('Failed to stop')
               else begin
                  ctrl.Delete;
                  if ctrl.State=TServiceState.ssNotInstalled then
                     Writeln('Uninstalled successfully')
                  else Writeln('Failed to uninstall');
               end;
            end;
         end else if param='/stop' then begin
            case serviceState of
               ssNotInstalled :
                  Writeln('Not installed');
               ssErrorRetrievingState, ssAccessDenied :
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
   Writeln( ServiceName+#13#10#13#10
           +'Parameters:'#13#10
           +'* none : run as application'#13#10
           +'* /install & /uninstall : install & uninstall service'#13#10
           +'* /start & /stop : start & stop service'#13#10
           +#13#10
           +'For more information, go to https://www.delphitools.info/');
end;

// ServiceState
//
function TdwsWindowsService.ServiceStatus(var status : TServiceStatus) : Boolean;
var
   scHandle, svInfo : Integer;
begin
   Result:=False;
   scHandle:=OpenSCManager(nil, nil, GENERIC_READ);
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
      Result:=status.dwCurrentState=SERVICE_START_PENDING
   else Result:=False;
end;

end.
