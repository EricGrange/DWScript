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
unit dwsIOCPWorkerThreadPool;

interface

{$I dws.inc}

uses
   Windows, Classes, SysUtils, ActiveX,
   dwsXPlatform, dwsUtils;

type

   TAnonymousWorkUnit = reference to procedure;
   PAnonymousWorkUnit = ^TAnonymousWorkUnit;

   TProcedureWorkUnit = procedure;

   TIOCPWorkerThreadPool = class;

   TIOCPDelayedWork = class
      private
         FPool : TIOCPWorkerThreadPool;
         FTimer : THandle;
         FEvent : TNotifyEvent;
         FSender : TObject;
         FNext, FPrev : TIOCPDelayedWork;
         procedure Detach;
         procedure Cancel;

      public
         destructor Destroy; override;
   end;

   IWorkerThreadPool = interface (IGetSelf)
      ['{775D71DC-6F61-4A52-B9DC-250682F4D696}']
      procedure Shutdown(timeoutMilliSeconds : Cardinal = INFINITE);

      function GetWorkerCount : Integer;
      procedure SetWorkerCount(val : Integer);
      property WorkerCount : Integer read GetWorkerCount write SetWorkerCount;
      function LiveWorkerCount : Integer;
      function ActiveWorkerCount : Integer;

      procedure QueueWork(const workUnit : TAnonymousWorkUnit); overload;
      procedure QueueWork(const workUnit : TProcedureWorkUnit); overload;
      procedure QueueWork(const workUnit : TNotifyEvent; sender : TObject); overload;

      function QueueSize : Integer;

      function IsIdle : Boolean;
   end;

   TIOCPWorkerThreadPool = class (TInterfacedSelfObject, IWorkerThreadPool)
      private
         FIOCP : THandle;
         FWorkerCount : Integer;
         FLiveWorkerCount : Integer;
         FActiveWorkerCount : Integer;
         FQueueSize : Integer;
         FTimerQueue : THandle;
         FDelayed : TIOCPDelayedWork;
         FDelayedLock : TMultiReadSingleWrite;

      protected
         function GetWorkerCount : Integer;
         procedure SetWorkerCount(val : Integer);

      public
         constructor Create(aWorkerCount : Integer);
         destructor Destroy; override;

         procedure Shutdown(timeoutMilliSeconds : Cardinal = INFINITE);

         procedure QueueWork(const workUnit : TAnonymousWorkUnit); overload;
         procedure QueueWork(const workUnit : TProcedureWorkUnit); overload;
         procedure QueueWork(const workUnit : TNotifyEvent; sender : TObject); overload;

         procedure QueueDelayedWork(delayMillisecSeconds : Cardinal; const workUnit : TNotifyEvent; sender : TObject);

         function QueueSize : Integer;

         property WorkerCount : Integer read FWorkerCount write SetWorkerCount;
         function LiveWorkerCount : Integer;
         function ActiveWorkerCount : Integer;
         function IsIdle : Boolean;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   WORK_UNIT_TERMINATE = 0;
   WORK_UNIT_ANONYMOUS = 1;
   WORK_UNIT_PROCEDURE = 2;

type

   {$IFNDEF VER270}
   ULONG_PTR = {$IFDEF VER230}NativeUInt{$ELSE}DWORD{$ENDIF};
   {$ENDIF}

   TIOCPData = packed record
      case Integer of
         0 : (
            lpNumberOfBytesTransferred : DWORD;
            lpCompletionKey : ULONG_PTR;
            lpOverlapped : POverlapped;
         );
         1 : (
            notifyEvent : TNotifyEvent;
            sender : TObject;
         );
   end;

   TIOCPWorkerThread = class(TThread)
      FPool : TIOCPWorkerThreadPool;
      FIOCP : THandle;
      constructor Create(const pool : TIOCPWorkerThreadPool);
      destructor Destroy; override;
      procedure Execute; override;
   end;

procedure DelayedWorkCallBack(Context: Pointer; Success: Boolean); stdcall;
var
   dw : TIOCPDelayedWork;
begin
   dw := TIOCPDelayedWork(Context);
   try
      if dw.FPool <> nil then begin
         dw.FPool.QueueWork(dw.FEvent, dw.FSender);
         InterlockedDecrement(dw.FPool.FQueueSize);
      end;
   finally
      dw.Free;
   end;
end;

// ------------------
// ------------------ TIOCPDelayedWork ------------------
// ------------------

// Destroy
//
destructor TIOCPDelayedWork.Destroy;
begin
   Cancel;
   Detach;
end;

// Detach
//
procedure TIOCPDelayedWork.Detach;
begin
   if FPool = nil then Exit;
   FPool.FDelayedLock.BeginWrite;
   try
      if FPrev <> nil then
         FPrev.FNext := FNext
      else FPool.FDelayed := FNext;
      if FNext <> nil then
         FNext.FPrev := nil;
   finally
      FPool.FDelayedLock.EndWrite;
   end;
   FPrev := nil;
   FNext := nil;
   FPool := nil;
end;

// Cancel
//
procedure TIOCPDelayedWork.Cancel;
begin
   if FTimer <> 0 then begin
      DeleteTimerQueueTimer(FPool.FTimerQueue, FTimer, 0);
      FTimer := 0;
   end;
end;

// ------------------
// ------------------ TIOCPWorkerThread ------------------
// ------------------

// Create
//
constructor TIOCPWorkerThread.Create(const pool : TIOCPWorkerThreadPool);
begin
   inherited Create;
   FPool:=pool;
   FIOCP:=FPool.FIOCP;
   FastInterlockedIncrement(FPool.FLiveWorkerCount);
   FreeOnTerminate:=True;
end;

// Destroy
//
destructor TIOCPWorkerThread.Destroy;
begin
   FastInterlockedDecrement(FPool.FLiveWorkerCount);
   inherited;
end;

// Execute
//
procedure TIOCPWorkerThread.Execute;

   procedure ExecuteAnonymousFunction(p : PAnonymousWorkUnit);
   begin
      try
         p^();
      finally
         p^._Release;
      end;
   end;

var
   data : TIOCPData;
begin
   CoInitialize(nil);

   while not Terminated do begin
      if not GetQueuedCompletionStatus(FIOCP,
                                       data.lpNumberOfBytesTransferred,
                                       data.lpCompletionKey,
                                       data.lpOverlapped, INFINITE) then Break;
      if data.lpNumberOfBytesTransferred=WORK_UNIT_TERMINATE then
         Break
      else begin
         FastInterlockedDecrement(FPool.FQueueSize);
         FastInterlockedIncrement(FPool.FActiveWorkerCount);
         try
            case data.lpNumberOfBytesTransferred of
               WORK_UNIT_ANONYMOUS :
                  ExecuteAnonymousFunction(PAnonymousWorkUnit(@data.lpOverlapped));
               WORK_UNIT_PROCEDURE :
                  TProcedureWorkUnit(data.lpOverlapped)();
            else
               data.notifyEvent(data.sender);
            end;
         finally
            FastInterlockedDecrement(FPool.FActiveWorkerCount);
         end;
      end;
   end;

   CoUninitialize;
end;

// ------------------
// ------------------ TIOCPWorkerThreadPool ------------------
// ------------------

// Create
//
constructor TIOCPWorkerThreadPool.Create(aWorkerCount : Integer);
begin
   inherited Create;
   FIOCP:=CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, MaxInt);
   WorkerCount:=aWorkerCount;
   FTimerQueue:=CreateTimerQueue;
   FDelayedLock:=TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TIOCPWorkerThreadPool.Destroy;
var
   dw : TIOCPDelayedWork;
begin
   FDelayedLock.BeginWrite;
   try
      dw := FDelayed;
      while dw <> nil do begin
         dw.Cancel;
         dw := dw.FNext;
      end;
   finally
      FDelayedLock.EndWrite;
   end;
   DeleteTimerQueueEx(FTimerQueue, INVALID_HANDLE_VALUE);
   FTimerQueue := 0;
   while FDelayed <> nil do
      FDelayed.Free;

   CloseHandle(FIOCP);

   while LiveWorkerCount>0 do
      Sleep(10);

   FDelayedLock.Free;

   inherited;
end;

// Shutdown
//
procedure TIOCPWorkerThreadPool.Shutdown(timeoutMilliSeconds : Cardinal = INFINITE);
var
   t : Int64;
begin
   WorkerCount:=0;

   t:=GetSystemMilliseconds;

   while (LiveWorkerCount>0) and (GetSystemMilliseconds-t<timeoutMilliSeconds) do
      Sleep(10);
end;

// QueueWork
//
procedure TIOCPWorkerThreadPool.QueueWork(const workUnit : TAnonymousWorkUnit);
var
   lpOverlapped : Pointer;
begin
   InterlockedIncrement(FQueueSize);
   lpOverlapped:=nil;
   PAnonymousWorkUnit(@lpOverlapped)^:=workUnit;
   PostQueuedCompletionStatus(FIOCP, WORK_UNIT_ANONYMOUS, 0, lpOverlapped);
end;

// QueueWork
//
procedure TIOCPWorkerThreadPool.QueueWork(const workUnit : TProcedureWorkUnit);
var
   lpOverlapped : Pointer;
begin
   InterlockedIncrement(FQueueSize);
   lpOverlapped:=Pointer(@workUnit);
   PostQueuedCompletionStatus(FIOCP, WORK_UNIT_PROCEDURE, 0, lpOverlapped);
end;

// QueueWork
//
procedure TIOCPWorkerThreadPool.QueueWork(const workUnit : TNotifyEvent; sender : TObject);
var
   data : TIOCPData;
begin
   InterlockedIncrement(FQueueSize);
   data.notifyEvent:=workUnit;
   data.sender:=sender;
   PostQueuedCompletionStatus(FIOCP, data.lpNumberOfBytesTransferred, data.lpCompletionKey, data.lpOverlapped);
end;

// QueueDelayedWork
//
procedure TIOCPWorkerThreadPool.QueueDelayedWork(delayMillisecSeconds : Cardinal; const workUnit : TNotifyEvent; sender : TObject);
var
   dw : TIOCPDelayedWork;
begin
   dw := TIOCPDelayedWork.Create;
   try
      dw.FPool := Self;
      dw.FEvent := workUnit;
      dw.FSender := sender;
      FDelayedLock.BeginWrite;
      try
         if FDelayed <> nil then begin
            dw.FNext := FDelayed;
            FDelayed.FPrev := dw;
         end;
         FDelayed := dw;
      finally
         FDelayedLock.EndWrite;
      end;
      CreateTimerQueueTimer(dw.FTimer, FTimerQueue, @DelayedWorkCallBack, dw,
                            delayMillisecSeconds, 0,
                            WT_EXECUTEDEFAULT or WT_EXECUTELONGFUNCTION or WT_EXECUTEONLYONCE);
      InterlockedIncrement(FQueueSize);
   except
      dw.Free;
      raise;
   end;
end;

// QueueSize
//
function TIOCPWorkerThreadPool.QueueSize : Integer;
begin
   Result:=FQueueSize;
end;

// LiveWorkerCount
//
function TIOCPWorkerThreadPool.LiveWorkerCount : Integer;
begin
   Result:=FLiveWorkerCount;
end;

// ActiveWorkerCount
//
function TIOCPWorkerThreadPool.ActiveWorkerCount : Integer;
begin
   Result:=FActiveWorkerCount;
end;

// IsIdle
//
function TIOCPWorkerThreadPool.IsIdle : Boolean;
begin
   Result := (FQueueSize=0) and (FActiveWorkerCount=0);
end;

// SetWorkerCount
//
procedure TIOCPWorkerThreadPool.SetWorkerCount(val : Integer);
var
   lpOverlapped : Pointer;
   i : Integer;
begin
   Assert(val>=0);
   if val>FWorkerCount then begin
      for i:=FWorkerCount+1 to val do
         TIOCPWorkerThread.Create(Self);
      FWorkerCount:=val;
   end else if val<FWorkerCount then begin
      lpOverlapped:=nil;
      for i:=FWorkerCount-1 downto val do
         PostQueuedCompletionStatus(FIOCP, WORK_UNIT_TERMINATE, 0, lpOverlapped);
      FWorkerCount:=val;
   end;
end;

// GetWorkerCount
//
function TIOCPWorkerThreadPool.GetWorkerCount : Integer;
begin
   Result:=FWorkerCount;
end;

end.
