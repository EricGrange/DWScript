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
   SysUtils,
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

         procedure ExecuteCommandLineParameters;

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
var
   i : Integer;
   param : String;
   ctrl : TServiceController;
   deviceCheck : array [0..2] of Char;
   buffer : array [0..MAX_PATH] of Char;
   description : TServiceDescription;
begin
   ctrl:=TServiceController.CreateOpenService('', '', ServiceName);
   try
      if ctrl.State=ssErrorRetrievingState then
         writeln('Error retrieving state')
      else begin
         for i:=1 to ParamCount do begin
            param:=ParamStr(i);
            if param='/install' then begin
               if ctrl.State=ssNotInstalled then begin
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
                     if ctrl.State<>ssNotInstalled then begin
                        writeln('Installed successfully');
                        description.lpDescription:=PChar(ServiceDescription);
                        ChangeServiceConfig2(ctrl.Handle, SERVICE_CONFIG_DESCRIPTION, @description);
                     end else writeln('Failed to install');
                  end;
               end else writeln('Already installed');
            end else if param='/uninstall' then begin
               if ctrl.State=ssNotInstalled then
                  writeln('Not installed')
               else begin
                  ctrl.Stop;
                  if ctrl.State<>ssStopped then
                     writeln('Failed to stop')
                  else begin
                     ctrl.Delete;
                     if ctrl.State=ssNotInstalled then
                        writeln('Uninstalled successfully')
                     else writeln('Failed to uninstall');
                  end;
               end;
            end else if param='/stop' then begin
               if ctrl.State=ssNotInstalled then
                  writeln('Not installed')
               else begin
                  ctrl.Stop;
                  writeln('Stop command issued')
               end;
            end else if param='/start' then begin
               if ctrl.State=ssNotInstalled then
                  writeln('Not installed')
               else begin
                  ctrl.Start([]);
                  writeln('Start command issued')
               end;
            end else begin
               Writeln( ServiceName+#13#10#13#10
                       +'Parameters:'#13#10
                       +'* none : run as application'#13#10
                       +'* /install & /uninstall : install & uninstall service'#13#10
                       +'* /start & /stop : start & stop service'#13#10
                       +#13#10
                       +'For more information, go to http://delphitools.info/');
            end;
         end;
      end;
   finally
      ctrl.Free;
   end;
end;

// LaunchedBySCM
//
function TdwsWindowsService.LaunchedBySCM : Boolean;
var
   scHandle, svInfo : Integer;
   servStat : TServiceStatus;
begin
   Result:=False;
   scHandle:=OpenSCManager(nil, nil, GENERIC_READ);
   try
      svInfo:=OpenService(scHandle, PChar(ServiceName), GENERIC_READ);
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

end.
