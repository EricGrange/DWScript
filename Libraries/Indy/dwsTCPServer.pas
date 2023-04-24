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
unit dwsTCPServer;

interface

uses
   System.Classes, System.SysUtils, System.Masks, System.Types,
   dwsUtils, dwsXPlatform,
   IdTCPServer, IdContext, IdCustomTCPServer;

type
   TdwsTCPServerOptions = record
      Port : Integer;
      MaxConnections : Integer;
      BindIPs : TStringDynArray;
      AllowedRemoteIPs : TStringDynArray;
      KeepAliveTime : Integer;
      KeepAliveInterval : Integer;
      DisableNagle : Boolean;
   end;

   TdwsTCPConnectionStatus = record
      Active, Connected : Boolean;
      InputBuffer : Integer;
   end;

   IdwsTCPServer = interface
      ['{0AE03BFB-CDBE-42FE-B441-697A66E7B3F2}']
      function GetName : String;
      function GetPort : Integer;

      procedure Shutdown;

      function ConnectionIDs : TStringDynArray;
      function ConnectionCount : Integer;
      function HasConnectionID(const connID : String) : Boolean;

      function ConnectionStatus(const connID : String) : TdwsTCPConnectionStatus;
      function ConnectionRemoteIP(const connID : String) : String;
      function SendData(const connID : String; const data : RawByteString) : Integer;
      procedure ReceiveData(const connID : String; var data : RawByteString; const maxBytes : Integer);
      procedure Disconnect(const connID : String);
   end;

   TdwsTCPServer = class (TInterfacedObject, IdwsTCPServer)
      private
         FName : String;
         FOptions : TdwsTCPServerOptions;
         FIdTCPServer : TIdTCPServer;
         FAllowedIPs : TStringDynArray;
         FAllowedIPMasks : TArray<TMask>;

      protected
         function GetName : String;
         function GetPort : Integer;

         procedure PrepareAllowedIPs;
         procedure CleanupAllowedIPs;

         procedure DoOnExecute(AContext: TIdContext);
         procedure DoOnConnect(AContext: TIdContext);
         procedure DoOnDisconnect(AContext: TIdContext);

      public
         constructor Create(const aName : String; const aOptions : TdwsTCPServerOptions);
         destructor Destroy; override;

         procedure Shutdown;

         property Name : String read FName;
         property Port : Integer read FOptions.Port;

         function ConnectionIDs : TStringDynArray;
         function ConnectionCount : Integer;
         function HasConnectionID(const connID : String) : Boolean;

         function ConnectionStatus(const connID : String) : TdwsTCPConnectionStatus;
         function ConnectionRemoteIP(const connID : String) : String;
         function SendData(const connID : String; const data : RawByteString) : Integer;
         procedure ReceiveData(const connID : String; var data : RawByteString; const maxBytes : Integer);
         procedure Disconnect(const connID : String);

         function RemoteIPAllowed(const ip : String) : Boolean;
   end;

   TTCPServerEvent = procedure (const serverName, connectionID : String) of object;

   TCPServer = class sealed
      private
         class var vOnConnection : TTCPServerEvent;
         class var vOnDisconnection : TTCPServerEvent;
         class var vOnDataReceived : TTCPServerEvent;

         // servers list assumed to be locked in this method
         class function ServerByName(const aName : String) : IdwsTCPServer; static;
         class function ServerByConnection(const connID : String) : IdwsTCPServer; static;

      public

         class function ServerNames : TStringDynArray; static;
         class function ServerPort(const serverName : String) : Integer; static;

         class procedure CreateServer(const serverName : String; const options : TdwsTCPServerOptions); static;
         class procedure DestroyServer(const serverName : String); static;
         class procedure DestroyAllServers; static;

         class function ConnectionIDs(const serverName : String) : TStringDynArray; static;
         class function ConnectionCount(const serverName : String) : Integer; static;

         class function ConnectionStatus(const connID : String) : TdwsTCPConnectionStatus; static;
         class function ConnectionRemoteIP(const connID : String) : String; static;
         class function SendData(const connID : String; const data : RawByteString) : Integer; static;
         class procedure ReceiveData(const connID : String; var data : RawByteString; const maxBytes : Integer); static;
         class procedure Disconnect(const connID : String); static;

         // Events will be invoked from a multi-threaded context

         class property OnConnection : TTCPServerEvent read vOnConnection write vOnConnection;
         class property OnDisconnection : TTCPServerEvent read vOnDisconnection write vOnDisconnection;
         class property OnDataReceived : TTCPServerEvent read vOnDataReceived write vOnDataReceived;

   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses
   IdGlobal,
   dwsCryptoXPlatform;

var
   vServers : TArray<IdwsTCPServer>;
   vServersLock : TMultiReadSingleWrite;

type
   TdwsIdContext = class (TIdServerContext)
      FIDString : String;
   end;

// ------------------
// ------------------ TCPServer ------------------
// ------------------

// ServerByName
//
class function TCPServer.ServerByName(const aName : String) : IdwsTCPServer;
begin
   for var i := 0 to High(vServers) do
      if vServers[i].GetName = aName then
         Exit(vServers[i]);
   Result := nil;
end;

// ServerByConnection
//
class function TCPServer.ServerByConnection(const connID : String) : IdwsTCPServer;
begin
   for var i := 0 to High(vServers) do
      if vServers[i].HasConnectionID(connID) then
         Exit(vServers[i]);
   Result := nil;
end;

// ServerNames
//
class function TCPServer.ServerNames : TStringDynArray;
begin
   vServersLock.BeginRead;
   try
      var nb := Length(vServers);
      SetLength(Result, nb);
      for var i := 0 to nb-1 do
         Result[i] := vServers[i].GetName;
   finally
      vServersLock.EndRead;
   end;
end;

// ServerPort
//
class function TCPServer.ServerPort(const serverName : String) : Integer;
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByName(serverName);
      if server <> nil then
         Result := server.GetPort
      else Result := 0;
   finally
      vServersLock.EndRead;
   end;
end;

// CreateServer
//
class procedure TCPServer.CreateServer(const serverName : String; const options : TdwsTCPServerOptions);
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginWrite;
   try
      if TCPServer.ServerByName(serverName) <> nil then
         raise Exception.CreateFmt('TCP server of name "%s" already exists', [ serverName ]);

      server := TdwsTCPServer.Create(serverName, options);

      var nb := Length(vServers);
      SetLength(vServers, nb + 1);
      vServers[nb] := server;
   finally
      vServersLock.EndWrite;
   end;
end;

// DestroyServer
//
class procedure TCPServer.DestroyServer(const serverName : String);
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginWrite;
   try
      for var i := 0 to High(vServers) do begin
         if vServers[i].GetName = serverName then begin
            server := vServers[i];
            Delete(vServers, i, 1);
            Break;
         end;
      end;
   finally
      vServersLock.EndWrite;
   end;
   server.Shutdown;
end;

// DestroyAllServers
//
class procedure TCPServer.DestroyAllServers;
var
   servers : TArray<IdwsTCPServer>;
begin
   vServersLock.BeginWrite;
   try
      servers := Copy(vServers);
      vServers := nil;
   finally
      vServersLock.EndWrite;
   end;
   for var i := 0 to High(servers) do
      servers[i].Shutdown;
end;

// ConnectionIDs
//
class function TCPServer.ConnectionIDs(const serverName : String) : TStringDynArray;
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByName(serverName);
      if server <> nil then
         Result := server.ConnectionIDs
      else Result := nil;
   finally
      vServersLock.EndRead;
   end;
end;

// ConnectionCount
//
class function TCPServer.ConnectionCount(const serverName : String) : Integer;
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByName(serverName);
      if server <> nil then
         Result := server.ConnectionCount
      else Result := 0;
   finally
      vServersLock.EndRead;
   end;
end;

// ConnectionStatus
//
class function TCPServer.ConnectionStatus(const connID : String) : TdwsTCPConnectionStatus;
var
   server : IdwsTCPServer;
begin
   Result := Default(TdwsTCPConnectionStatus);
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByConnection(connID);
      if server <> nil then
         Result := server.ConnectionStatus(connID);
   finally
      vServersLock.EndRead;
   end;
end;

// ConnectionRemoteIP
//
class function TCPServer.ConnectionRemoteIP(const connID : String) : String;
var
   server : IdwsTCPServer;
begin
   Result := '';
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByConnection(connID);
      if server <> nil then
         Result := server.ConnectionRemoteIP(connID);
   finally
      vServersLock.EndRead;
   end;
end;

// SendData
//
class function TCPServer.SendData(const connID : String; const data : RawByteString) : Integer;
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByConnection(connID);
      if server <> nil then
         Result := server.SendData(connID, data)
      else Result := 0;
   finally
      vServersLock.EndRead;
   end;
end;

// ReceiveData
//
class procedure TCPServer.ReceiveData(const connID : String; var data : RawByteString; const maxBytes : Integer);
var
   server : IdwsTCPServer;
begin
   data := '';
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByConnection(connID);
      if server <> nil then
         server.ReceiveData(connID, data, maxBytes);
   finally
      vServersLock.EndRead;
   end;
end;

// Disconnect
//
class procedure TCPServer.Disconnect(const connID : String);
var
   server : IdwsTCPServer;
begin
   vServersLock.BeginRead;
   try
      server := TCPServer.ServerByConnection(connID);
      if server <> nil then
         server.Disconnect(connID);
   finally
      vServersLock.EndRead;
   end;
end;

// ------------------
// ------------------ TdwsTCPServer ------------------
// ------------------

// Create
//
constructor TdwsTCPServer.Create(const aName : String; const aOptions : TdwsTCPServerOptions);
begin
   inherited Create;
   FName := aName;
   FOptions := aOptions;

   FIdTCPServer := TIdTCPServer.Create(nil);
   FIdTCPServer.DefaultPort := aOptions.Port;
   FIdTCPServer.MaxConnections := aOptions.MaxConnections;
   FIdTCPServer.ContextClass := TdwsIdContext;
   FIdTCPServer.UseNagle := not aOptions.DisableNagle;

   for var i := 0 to High(aOptions.BindIPs) do
      FIdTCPServer.Bindings.Add.IP := aOptions.BindIPs[i];

   FIdTCPServer.OnExecute := DoOnExecute;
   FIdTCPServer.OnConnect := DoOnConnect;
   FIdTCPServer.OnDisconnect := DoOnDisconnect;

   PrepareAllowedIPs;

   FIdTCPServer.Active := True;
end;

// Destroy
//
destructor TdwsTCPServer.Destroy;
begin
   FreeAndNil(FIdTCPServer);
   CleanupAllowedIPs;
   inherited;
end;

// Shutdown
//
procedure TdwsTCPServer.Shutdown;
begin
   FIdTCPServer.Active := False;
end;

// ConnectionIDs
//
function TdwsTCPServer.ConnectionIDs : TStringDynArray;
begin
   var list := FIdTCPServer.Contexts.LockList;
   try
      SetLength(Result, list.Count);
      for var i := 0 to list.Count-1 do
         Result[i] := TdwsIdContext(list[i]).FIDString;
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;
end;

// ConnectionCount
//
function TdwsTCPServer.ConnectionCount : Integer;
begin
   Result := FIdTCPServer.Contexts.Count
end;

// HasConnectionID
//
function TdwsTCPServer.HasConnectionID(const connID : String) : Boolean;
begin
   var list := FIdTCPServer.Contexts.LockList;
   try
      for var i := 0 to list.Count-1 do
         if TdwsIdContext(list[i]).FIDString = connID then
            Exit(True);
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;
   Result := False;
end;

// ConnectionStatus
//
function TdwsTCPServer.ConnectionStatus(const connID : String) : TdwsTCPConnectionStatus;
begin
   Result := Default(TdwsTCPConnectionStatus);
   var list := FIdTCPServer.Contexts.LockList;
   try
      for var i := 0 to list.Count-1 do begin
         var context := TdwsIdContext(list[i]);
         if context.FIDString = connID then begin
            Result.Active := True;
            Result.Connected := context.Connection.Connected;
            if Result.Connected then begin
               var socket := context.Connection.Socket;
               Result.InputBuffer := socket.InputBuffer.Size;
            end;
            Break;
         end;
      end;
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;
end;

// ConnectionRemoteIP
//
function TdwsTCPServer.ConnectionRemoteIP(const connID : String) : String;
begin
   Result := '';
   var list := FIdTCPServer.Contexts.LockList;
   try
      for var i := 0 to list.Count-1 do begin
         var context := TdwsIdContext(list[i]);
         if context.FIDString = connID then begin
            Result := context.Connection.Socket.Binding.PeerIP;
            Break;
         end;
      end;
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;
end;

// SendData
//
function TdwsTCPServer.SendData(const connID : String; const data : RawByteString) : Integer;
var
   bytes : TIdBytes;
begin
   Result := 0;

   var size := Length(data);
   if size = 0 then Exit;

   SetLength(bytes, size);
   System.Move(data[1], bytes[0], size);

   var list := FIdTCPServer.Contexts.LockList;
   try
      for var i := 0 to list.Count-1 do begin
         var context := TdwsIdContext(list[i]);
         if context.FIDString = connID then begin
            if context.Connection.Connected then begin
               context.Connection.Socket.Write(bytes);
               Result := size;
            end;
            Break;
         end;
      end;
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;
end;

// ReceiveData
//
procedure TdwsTCPServer.ReceiveData(const connID : String; var data : RawByteString; const maxBytes : Integer);
var
   bytes : TIdBytes;
begin
   var list := FIdTCPServer.Contexts.LockList;
   try
      for var i := 0 to list.Count-1 do begin
         var context := TdwsIdContext(list[i]);
         if context.FIDString = connID then begin
            if context.Connection.Connected then begin
               context.Connection.Socket.InputBuffer.ExtractToBytes(bytes, maxBytes);
            end;
            Break;
         end;
      end;
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;

   var nb := Length(bytes);
   SetLength(data, nb);
   if nb > 0 then
      System.Move(bytes[0], data[1], nb);
end;

// Disconnect
//
procedure TdwsTCPServer.Disconnect(const connID : String);
begin
   var list := FIdTCPServer.Contexts.LockList;
   try
      for var i := 0 to list.Count-1 do begin
         var context := TdwsIdContext(list[i]);
         if context.FIDString = connID then begin
            context.Connection.Disconnect;
            Break;
         end;
      end;
   finally
      FIdTCPServer.Contexts.UnlockList;
   end;
end;

// RemoteIPAllowed
//
function TdwsTCPServer.RemoteIPAllowed(const ip : String) : Boolean;
begin
   for var i := 0 to High(FAllowedIPs) do
      if FAllowedIPs[i] = ip then
         Exit(True);
   for var i := 0 to High(FAllowedIPMasks) do
      if FAllowedIPMasks[i].Matches(ip) then
         Exit(True);
   Result := False;
end;

// DoOnConnect
//
procedure TdwsTCPServer.DoOnConnect(AContext: TIdContext);
begin
   var context := AContext as TdwsIdContext;

   if Length(FOptions.AllowedRemoteIPs) > 0 then begin
      if not RemoteIPAllowed(context.Connection.Socket.Binding.PeerIP) then begin
         AContext.Connection.Disconnect;
         Exit;
      end;
   end;

   context.FIDString := CryptographicToken;
   if (FOptions.KeepAliveTime <> 0) and (FOptions.KeepAliveInterval <> 0) then
      context.Connection.Socket.Binding.SetKeepAliveValues(True, FOptions.KeepAliveTime, FOptions.KeepAliveInterval);
   if Assigned(TCPServer.vOnConnection) then
      TCPServer.vOnConnection(Name, context.FIDString);
end;

// DoOnDisconnect
//
procedure TdwsTCPServer.DoOnDisconnect(AContext: TIdContext);
begin
   if Assigned(TCPServer.vOnDisconnection) then begin
      var context := TdwsIdContext(AContext);
      TCPServer.vOnDisconnection(Name, context.FIDString);
   end;
end;

// DoOnExecute
//
procedure TdwsTCPServer.DoOnExecute(AContext: TIdContext);
begin
   var context := TdwsIdContext(AContext);
   if not AContext.Connection.Socket.InputBufferIsEmpty then begin
      if Assigned(TCPServer.vOnDataReceived) then
         TCPServer.vOnDataReceived(Name, context.FIDString);
   end;
   Sleep(50);
end;

// GetName
//
function TdwsTCPServer.GetName : String;
begin
   Result := FName;
end;

// GetPort
//
function TdwsTCPServer.GetPort : Integer;
begin
   Result := FOptions.Port;
end;

// PrepareAllowedIPs
//
procedure TdwsTCPServer.PrepareAllowedIPs;
begin
   CleanupAllowedIPs;
   for var filter in FOptions.AllowedRemoteIPs do begin
      if LastDelimiter('?*', filter) > 0 then begin
         var mask := TMask.Create(filter);
         Insert(mask, FAllowedIPMasks, Length(FAllowedIPMasks));
      end else Insert(filter, FAllowedIPs, Length(FAllowedIPs));
   end;
end;

// CleanupAllowedIPs
//
procedure TdwsTCPServer.CleanupAllowedIPs;
begin
   FAllowedIPs := nil;
   for var i := 0 to High(FAllowedIPMasks) do
      FAllowedIPMasks[i].Free;
   FAllowedIPMasks := nil;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vServersLock := TMultiReadSingleWrite.Create;

finalization

   TCPServer.DestroyAllServers;

   FreeAndNil(vServersLock);

end.
