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
unit dwsHTTPSysServerEvents;

{$I dws.inc}

interface

uses
   Windows, Classes,
   dwsUtils, dwsHTTPSysAPI, dwsXPlatform, dwsWebEnvironment, dwsXXHash;

type

   IdwsHTTPServerEvents = interface
      procedure AddRequest(const sourceName : String; aQueue : THandle; aID : HTTP_REQUEST_ID;
                           const remoteIP : String; response : PHTTP_RESPONSE_V2);
      procedure PostEvent(const sourceName : String; const payload : RawByteString);
      procedure CloseRequests(const sourceName : String);
      function SourceNames : TStringDynArray;
      function SourceRequests(const sourceName : String) : TStringDynArray;
   end;

   TdwsHTTPServerEventRequest = class
      private
         FMarkedForRemoval : Boolean;
         FRequestQueue : THandle;
         FRequestID : HTTP_REQUEST_ID;
         FRemoteIP : String;
         FNext, FPrev : TdwsHTTPServerEventRequest;

      public
         property MarkedForRemoval : Boolean read FMarkedForRemoval;
         property RequestQueue : THandle read FRequestQueue write FRequestQueue;
         property RequestID : HTTP_REQUEST_ID read FRequestID write FRequestID;
         property RemoteIP : String read FRemoteIP write FRemoteIP;
   end;


   TdwsHTTPServerEventSource = class
      private
         FHead, FTail : TdwsHTTPServerEventRequest;
         FCount : Integer;
         FLock : TMultiReadSingleWrite;

      protected
         procedure Clear;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddRequest(aQueue : THandle; aID : HTTP_REQUEST_ID;
                              const remoteIP : String; response : PHTTP_RESPONSE_V2);
         procedure Collect;
         property Count : Integer read FCount;

         procedure PostEvent(const payload : RawByteString);
         procedure CloseConnections;
         function ConnectedIPs : TStringDynArray;
   end;

   TdwsHTTPServerEvents = class (TInterfacedObject, IdwsHTTPServerEvents)
      private
         FItems : TNameObjectHash;
         FLock : TMultiReadSingleWrite;
         FGCTimer : ITimer;

      protected
         procedure GCEvent;

      public
         constructor Create;
         destructor Destroy; override;

         procedure AddRequest(const sourceName : String; aQueue : THandle; aID : HTTP_REQUEST_ID;
                              const remoteIP : String; response : PHTTP_RESPONSE_V2);
         function Count : Integer;

         procedure PostEvent(const sourceName : String;
                             const payload : RawByteString);
         procedure CloseRequests(const sourceName : String);
         function SourceNames : TStringDynArray;
         function SourceRequests(const sourceName : String) : TStringDynArray;

         procedure Collect;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cGCInterval = 3 * 60 * 1000; // every 3 minutes

// ------------------
// ------------------ TdwsHTTPServerEventSource ------------------
// ------------------

// Create
//
constructor TdwsHTTPServerEventSource.Create;
begin
   inherited;
   FLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TdwsHTTPServerEventSource.Destroy;
begin
   inherited;
   Clear;
   FLock.Free;
end;

// Clear
//
procedure TdwsHTTPServerEventSource.Clear;
var
   req : TdwsHTTPServerEventRequest;
begin
   FLock.BeginWrite;
   try
      while FHead <> nil do begin
         req := FHead;
         FHead := req.FNext;
         req.Free;
      end;
      FHead := nil;
      FTail := nil;
      FCount := 0;
   finally
      FLock.EndWrite;
   end;
end;

// AddRequest
//
procedure TdwsHTTPServerEventSource.AddRequest(aQueue : THandle; aID : HTTP_REQUEST_ID;
                                               const remoteIP : String; response : PHTTP_RESPONSE_V2);
var
   err : HRESULT;
   req : TdwsHTTPServerEventRequest;
   bytesSent : ULONG;
begin
   req := TdwsHTTPServerEventRequest.Create;
   req.FRequestQueue := aQueue;
   req.FRequestID := aID;
   req.FRemoteIP := remoteIP;

   err := HttpAPI.SendHttpResponse(aQueue, aID, HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
                                   response^, nil, bytesSent, nil, 0, nil, nil);
   if err <> NO_ERROR then begin

      req.Free;
      HttpAPI.Check(err, hSendHttpResponse, 'AddRequest');

   end else begin

      FLock.BeginWrite;
      try
         if FHead = nil then begin
            FTail := req;
         end else begin
            FHead.FPrev := req;
            req.FNext := FHead;
            FHead := req;
         end;
         FHead := req;
         Inc(FCount);
      finally
         FLock.EndWrite;
      end;

   end;
end;

// Collect
//
procedure TdwsHTTPServerEventSource.Collect;
var
   req, garbage, tmp : TdwsHTTPServerEventRequest;
begin
   garbage := nil;
   FLock.BeginWrite;
   try
      req := FHead;
      while req <> nil do begin
         if req.MarkedForRemoval then begin
            Dec(FCount);
            if req.FPrev = nil then
               FHead := req.FNext
            else req.FPrev.FNext := req.FNext;
            if req.FNext = nil then
               FTail := req.FPrev
            else req.FNext.FPrev := req.FPrev;
            tmp := req;
            req := req.FNext;
            tmp.FNext := garbage;
            garbage := tmp;
         end else req := req.FNext;
      end;
   finally
      FLock.EndWrite;
   end;
   // release objects outside of lock
   while garbage <> nil do begin
      tmp := garbage.FNext;
      garbage.Free;
      garbage := tmp;
   end;
end;

// PostEvent
//
procedure TdwsHTTPServerEventSource.PostEvent(const payload : RawByteString);
var
   nb : Integer;
   data : RawByteString;
   req : TdwsHTTPServerEventRequest;
   chunk : HTTP_DATA_CHUNK_INMEMORY;
   bytesSent : ULONG;
begin
   data := payload;
   // normalize end of message
   if not (StrEndsWithA(data, #10#10) or StrEndsWithA(data, #13#10#13#10)) then begin
      if StrEndsWithA(data, #13#10) then
         data := data + #13#10
      else if StrEndsWithA(data, #10) then
         data := data + #10
      else data := data + #10#10;
   end;

   chunk.DataChunkType := hctFromMemory;
   chunk.pBuffer := Pointer(data);
   chunk.BufferLength := Length(data);

   nb := 0;
   FLock.BeginRead;
   try
      req := FHead;
      while req <> nil do begin
         if not req.MarkedForRemoval then begin
            if HttpAPI.SendResponseEntityBody(req.FRequestQueue, req.FRequestID, HTTP_SEND_RESPONSE_FLAG_MORE_DATA,
                                              1, @chunk, bytesSent, nil, 0, nil, nil) <> NO_ERROR then
               req.FMarkedForRemoval := True
            else Inc(nb);
         end;
         req := req.FNext;
      end;
   finally
      FLock.EndRead;
   end;
   if nb > 2 * FCount + 4 then
      Collect;
end;

// CloseConnections
//
procedure TdwsHTTPServerEventSource.CloseConnections;
var
   req, next : TdwsHTTPServerEventRequest;
   chunk : HTTP_DATA_CHUNK_INMEMORY;
   bytesSent : ULONG;
begin
   chunk.DataChunkType := hctFromMemory;
   chunk.pBuffer := nil;
   chunk.BufferLength := 0;

   FLock.BeginWrite;
   try
      req := FHead;
      while req <> nil do begin
         next := req.FNext;
         if not req.MarkedForRemoval then begin
            HttpAPI.SendResponseEntityBody(req.FRequestQueue, req.FRequestID, HTTP_SEND_RESPONSE_FLAG_DISCONNECT,
                                           1, @chunk, bytesSent, nil, 0, nil, nil);
         end;
         req.Free;
         req := next;
      end;
      FHead := nil;
      FTail := nil;
      FCount := 0;
   finally
      FLock.EndWrite;
   end;
end;

// ConnectedIPs
//
function TdwsHTTPServerEventSource.ConnectedIPs : TStringDynArray;
var
   req : TdwsHTTPServerEventRequest;
   i : Integer;
begin
   i := 0;
   FLock.BeginRead;
   try
      SetLength(Result, FCount);
      req := FHead;
      while req <> nil do begin
         if not req.MarkedForRemoval then begin
            Result[i] := req.FRemoteIP;
            Inc(i);
         end;
         req := req.FNext;
      end;
   finally
      FLock.EndRead;
   end;
   SetLength(Result, i);
end;


// ------------------
// ------------------ TdwsHTTPServerEvents ------------------
// ------------------

// Create
//
constructor TdwsHTTPServerEvents.Create;
begin
   inherited;
   FLock := TMultiReadSingleWrite.Create;
   FItems := TNameObjectHash.Create;
   FGCTimer := TTimerTimeout.Create(cGCInterval, GCEvent);
end;

// Destroy
//
destructor TdwsHTTPServerEvents.Destroy;
begin
   FGCTimer := nil;
   FItems.Clean;
   FItems.Free;
   FLock.Free;
   inherited;
end;

// AddRequest
//
procedure TdwsHTTPServerEvents.AddRequest(const sourceName : String; aQueue : THandle; aID : HTTP_REQUEST_ID;
                                          const remoteIP : String; response : PHTTP_RESPONSE_V2);
var
   source : TdwsHTTPServerEventSource;
begin
   FLock.BeginWrite;
   try
      source := TdwsHTTPServerEventSource(FItems.Objects[sourceName]);
      if source = nil then begin
         source := TdwsHTTPServerEventSource.Create;
         FItems.Objects[sourceName] := source;
      end;
      source.AddRequest(aQueue, aID, remoteIP, response);
   finally
      FLock.EndWrite;
   end;
end;

// Count
//
function TdwsHTTPServerEvents.Count : Integer;
begin
   Result := FItems.Count;
end;

// PostEvent
//
procedure TdwsHTTPServerEvents.PostEvent(const sourceName : String;
                                         const payload : RawByteString);
var
   source : TdwsHTTPServerEventSource;
begin
   FLock.BeginRead;
   try
      source := TdwsHTTPServerEventSource(FItems.Objects[sourceName]);
      if source <> nil then
         source.PostEvent(payload);
   finally
      FLock.EndRead;
   end;
end;

// CloseRequests
//
procedure TdwsHTTPServerEvents.CloseRequests(const sourceName : String);
var
   source : TdwsHTTPServerEventSource;
begin
   FLock.BeginRead;
   try
      source := TdwsHTTPServerEventSource(FItems.Objects[sourceName]);
      if source <> nil then
         source.CloseConnections;
   finally
      FLock.EndRead;
   end;
end;

// SourceNames
//
function TdwsHTTPServerEvents.SourceNames : TStringDynArray;
begin
   FLock.BeginRead;
   try
      Result := FItems.Names;
   finally
      FLock.EndRead;
   end;
end;

// SourceRequests
//
function TdwsHTTPServerEvents.SourceRequests(const sourceName : String) : TStringDynArray;
var
   source : TdwsHTTPServerEventSource;
begin
   FLock.BeginRead;
   try
      source := TdwsHTTPServerEventSource(FItems.Objects[sourceName]);
      if source <> nil then
         Result := source.ConnectedIPs;
   finally
      FLock.EndRead;
   end;
end;

// Collect
//
procedure TdwsHTTPServerEvents.Collect;
var
   i : Integer;
   source : TdwsHTTPServerEventSource;
   p : PNameObjectHashBucket;
   needPack : Boolean;
begin
   needPack := False;
   FLock.BeginWrite;
   try
      for i := 0 to FItems.HighIndex do begin
         p := FItems.Bucket[i];
         if p.Obj <> nil then begin
            source := TdwsHTTPServerEventSource(p.Obj);
            source.Collect;
            if source.Count = 0 then begin
               needPack := True;
               source.Free;
               p.Obj := nil;
               p.HashCode := 0;
            end;
         end;
      end;
      if needPack then
         FItems.Pack;
   finally
      FLock.EndWrite;
   end;
end;

// GCEvent
//
procedure TdwsHTTPServerEvents.GCEvent;
begin
   Collect;
   FGCTimer := TTimerTimeout.Create(cGCInterval, GCEvent);
end;

end.
