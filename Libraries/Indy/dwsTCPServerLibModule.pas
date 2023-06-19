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
unit dwsTCPServerLibModule;

interface

uses
  System.SysUtils, System.Classes,
  dwsComp, dwsExprs, dwsExprList, dwsInfo, dwsXPlatform,
  dwsTCPServer, dwsWebEnvironment, dwsWebEnvironmentTypes;

type
   TTCPServerWorkEvent = procedure (const request : TWebRequest) of object;

   TTCPServerWebRequest = class (TWebRequest)
      private
         FHeaders : TStrings;

      protected
         function GetHeaders : TStrings; override;
         function GetUserAgent : String; override;

      public
         ServerName : String;
         ConnectionID : String;
         Verb : TWebRequestMethodVerb;
         Task : String;

         destructor Destroy; override;

         function RemoteIP : String; override;

         function RawURL : String; override;
         function URL : String; override;
         function FullURL : String; override;
         function Method : String; override;
         function MethodVerb : TWebRequestMethodVerb; override;

         function Security : String; override;
         function Secure : Boolean; override;
         function Host : String; override;

         function ContentLength : Integer; override;
         function ContentData : RawByteString; override;
         function ContentType : RawByteString; override;

         procedure Execute(Sender : TObject);
   end;

  TdwsSystemTCPServerLib = class(TDataModule)
    dwsTCPServerModule: TdwsUnit;
    procedure dwsTCPServerModuleClassesTCPServersMethodsServerNamesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsConnectionsCountEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsCreateServerEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsConnectionIDsEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsSendDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsReceiveDataEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsDestroyServerEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsDisconnectEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsTCPServerModuleClassesTCPServersMethodsConnectionStatusEval(
      Info: TProgramInfo; ExtObject: TObject);
  private
    { Private declarations }
    FOnTCPServerWork : TTCPServerWorkEvent;
    FTaskNamesLock : TMultiReadSingleWrite;
    FOnConnectTaskNames : TStrings;
    FOnDisconnectTaskNames : TStrings;
    FOnDataReceivedTaskNames : TStrings;

    procedure BindGlobalEvents;
    procedure UnBindGlobalEvents;

    procedure SetOnTCPServerWork(const val : TTCPServerWorkEvent);

    procedure GlobalOnConnect(const serverName, connectionID : String);
    procedure GlobalOnDisconnect(const serverName, connectionID : String);
    procedure GlobalOnDataReceived(const serverName, connectionID : String);

  public { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property OnTCPServerWork : TTCPServerWorkEvent read FOnTCPServerWork write SetOnTCPServerWork;
  end;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.dfm}

// ------------------
// ------------------ TTCPServerWebRequest ------------------
// ------------------

// Destroy
//
destructor TTCPServerWebRequest.Destroy;
begin
   inherited;
   FreeAndNil(FHeaders);
end;

// RemoteIP
//
function TTCPServerWebRequest.RemoteIP : String;
begin
   Result := TCPServer.ConnectionRemoteIP(ConnectionID)
end;

// RawURL
//
function TTCPServerWebRequest.RawURL : String;
begin
   Result := Task;
end;

// URL
//
function TTCPServerWebRequest.URL : String;
begin
   Result := Task;
end;

// FullURL
//
function TTCPServerWebRequest.FullURL : String;
begin
   Result := Task;
end;

// Method
//
function TTCPServerWebRequest.Method : String;
begin
   Result := cWebRequestMethodVerbs[Verb];
end;

// MethodVerb
//
function TTCPServerWebRequest.MethodVerb : TWebRequestMethodVerb;
begin
   Result := Verb;
end;

// Security
//
function TTCPServerWebRequest.Security : String;
begin
   Result := '';
end;

// Secure
//
function TTCPServerWebRequest.Secure : Boolean;
begin
   Result := False;
end;

// Host
//
function TTCPServerWebRequest.Host : String;
begin
   Result := ServerName;
end;

// ContentLength
//
function TTCPServerWebRequest.ContentLength : Integer;
begin
   Result := 0;
end;

// ContentData
//
function TTCPServerWebRequest.ContentData : RawByteString;
begin
   Result := '';
end;

// ContentType
//
function TTCPServerWebRequest.ContentType : RawByteString;
begin
   Result := '';
end;

// Execute
//
procedure TTCPServerWebRequest.Execute(Sender : TObject);
var
   lib : TdwsSystemTCPServerLib;
begin
   lib := (Sender as TdwsSystemTCPServerLib);
   try
      if Assigned(lib.FOnTCPServerWork) then
         lib.FOnTCPServerWork(Self);
   finally
      Free;
   end;
end;

// GetHeaders
//
function TTCPServerWebRequest.GetHeaders : TStrings;
begin
   if FHeaders = nil then begin
      FHeaders := TStringList.Create;
      FHeaders.Values['ServerName'] := ServerName;
      FHeaders.Values['ConnectionID'] := ConnectionID;
   end;
   Result := FHeaders;
end;

// GetUserAgent
//
function TTCPServerWebRequest.GetUserAgent : String;
begin
   Result := ConnectionID;
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsConnectionIDsEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := TCPServer.ConnectionIDs(Info.ParamAsString[0]);
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsConnectionsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TCPServer.ConnectionCount(Info.ParamAsString[0]);
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsConnectionStatusEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   result : IInfo;
   connID : String;
begin
   connID := Info.ParamAsString[0];
   var status := TCPServer.ConnectionStatus(connID);
   result := Info.ResultVars;
   if status.Active then begin
      result.Member['ID'].ValueAsString := connID;
      result.Member['Connected'].Value := status.Connected;
      result.Member['ClosedGracefully'].Value := status.ClosedGracefully;
      result.Member['InputSize'].ValueAsInteger := status.InputBuffer;
   end else begin
      result.Member['ID'].ValueAsString := '';
      result.Member['Connected'].Value := False;
      result.Member['ClosedGracefully'].Value := False;
      result.Member['InputSize'].ValueAsInteger := 0;
   end;
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsCreateServerEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   serverName : String;
   opts : IInfo;
begin
   serverName := Info.ParamAsString[0];
   opts := Info.Params[1];
   var options := Default(TdwsTCPServerOptions);
   options.Port := opts.Member['Port'].ValueAsInteger;
   options.MaxConnections := opts.Member['MaxConnections'].ValueAsInteger;
   options.BindIPs := opts.Member['BindIPs'].ScriptDynArray.ToStringArray;
   options.AllowedRemoteIPs := opts.Member['AllowedRemoteIPs'].ScriptDynArray.ToStringArray;
   options.KeepAliveTime := opts.Member['KeepAliveTime'].ValueAsInteger;
   options.KeepAliveInterval := opts.Member['KeepAliveInterval'].ValueAsInteger;
   options.DisableNagle := opts.Member['DisableNagle'].ValueAsBoolean;
   FTaskNamesLock.BeginWrite;
   try
      FOnConnectTaskNames.Values[serverName] := opts.Member['OnConnect'].ValueAsString;
      FOnDisconnectTaskNames.Values[serverName] := opts.Member['OnDisconnect'].ValueAsString;
      FOnDataReceivedTaskNames.Values[serverName] := opts.Member['OnData'].ValueAsString;
   finally
      FTaskNamesLock.EndWrite;
   end;
   TCPServer.CreateServer(serverName, options);
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsDestroyServerEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TCPServer.DestroyServer(Info.ParamAsString[0]);
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsDisconnectEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   TCPServer.Disconnect(Info.ParamAsString[0]);
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsReceiveDataEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : RawByteString;
begin
   TCPServer.ReceiveData(Info.ParamAsString[0], buf, Info.ParamAsInteger[1]);
   Info.ResultAsDataString := buf;
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsSendDataEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := TCPServer.SendData(Info.ParamAsString[0], info.ParamAsDataString[1]);
end;

procedure TdwsSystemTCPServerLib.dwsTCPServerModuleClassesTCPServersMethodsServerNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsStringArray := TCPServer.ServerNames;
end;

// BindGlobalEvents
//
procedure TdwsSystemTCPServerLib.BindGlobalEvents;
begin
   if csDesigning in ComponentState then Exit;
   if Assigned(TCPServer.OnConnection) then
      raise Exception.Create('TCPServer global events already bound');
   TCPServer.OnConnection := GlobalOnConnect;
   TCPServer.OnDisconnection := GlobalOnDisconnect;
   TCPServer.OnDataReceived := GlobalOnDataReceived;
end;

// UnBindGlobalEvents
//
procedure TdwsSystemTCPServerLib.UnBindGlobalEvents;
begin
   TCPServer.OnConnection := nil;
   TCPServer.OnDisconnection := nil;
   TCPServer.OnDataReceived := nil;
end;

// SetOnTCPServerWork
//
procedure TdwsSystemTCPServerLib.SetOnTCPServerWork(const val : TTCPServerWorkEvent);
begin
   if Assigned(val) then
      BindGlobalEvents
   else UnBindGlobalEvents;
   FOnTCPServerWork := val;
end;

// GlobalOnConnect
//
procedure TdwsSystemTCPServerLib.GlobalOnConnect(const serverName, connectionID : String);
var
   taskName : String;
begin
   FTaskNamesLock.BeginRead;
   try
      taskName := FOnConnectTaskNames.Values[serverName];
   finally
      FTaskNamesLock.EndRead;
   end;
   if taskName = '' then Exit;
   var wr := TTCPServerWebRequest.Create;
   try
      wr.ServerName := serverName;
      wr.ConnectionID := connectionID;
      wr.Task := taskName;
      wr.Verb := wrmvCONNECT;
      OnTCPServerWork(wr);
   finally
      wr.Free;
   end;
end;

// GlobalOnDisconnect
//
procedure TdwsSystemTCPServerLib.GlobalOnDisconnect(const serverName, connectionID : String);
var
   taskName : String;
begin
   FTaskNamesLock.BeginRead;
   try
      taskName := FOnDisconnectTaskNames.Values[serverName];
   finally
      FTaskNamesLock.EndRead;
   end;
   if taskName = '' then Exit;
   var wr := TTCPServerWebRequest.Create;
   try
      wr.ServerName := serverName;
      wr.ConnectionID := connectionID;
      wr.Task := taskName;
      wr.Verb := wrmvDELETE;
      OnTCPServerWork(wr);
   finally
      wr.Free;
   end;
end;

// GlobalOnDataReceived
//
procedure TdwsSystemTCPServerLib.GlobalOnDataReceived(const serverName, connectionID : String);
var
   taskName : String;
begin
   FTaskNamesLock.BeginRead;
   try
      taskName := FOnDataReceivedTaskNames.Values[serverName];
   finally
      FTaskNamesLock.EndRead;
   end;
   if taskName = '' then Exit;
   var wr := TTCPServerWebRequest.Create;
   try
      wr.ServerName := serverName;
      wr.ConnectionID := connectionID;
      wr.Task := taskName;
      wr.Verb := wrmvPUT;
      OnTCPServerWork(wr);
   finally
      wr.Free;
   end;
end;

// Create
//
constructor TdwsSystemTCPServerLib.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FTaskNamesLock := TMultiReadSingleWrite.Create;
   FOnConnectTaskNames := TStringList.Create;
   FOnDisconnectTaskNames := TStringList.Create;
   FOnDataReceivedTaskNames := TStringList.Create;
end;

// Destroy
//
destructor TdwsSystemTCPServerLib.Destroy;
begin
   if not (csDesigning in ComponentState) then
      UnBindGlobalEvents;
   FOnConnectTaskNames.Free;
   FOnDisconnectTaskNames.Free;
   FOnDataReceivedTaskNames.Free;
   FTaskNamesLock.Free;
   inherited;
end;

end.
