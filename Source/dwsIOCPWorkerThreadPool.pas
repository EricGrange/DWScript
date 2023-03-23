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
   Winapi.Windows, System.Classes, System.SysUtils,
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

   TWorkerThreadQueueSizeInfo = record
      Total : Integer;
      Delayed : Integer;
      Peak : Integer;
   end;

   IWorkerThreadPool = interface (IGetSelf)
      ['{775D71DC-6F61-4A52-B9DC-250682F4D696}']
      procedure Shutdown(timeoutMilliSeconds : Cardinal = INFINITE);

      function GetWorkerCount : Integer;
      procedure SetWorkerCount(val : Integer);
      property WorkerCount : Integer read GetWorkerCount write SetWorkerCount;
      function LiveWorkerCount : Integer;
      function ActiveWorkerCount : Integer;
      function PeakActiveWorkerCount : Integer;

      procedure QueueWork(const workUnit : TAnonymousWorkUnit); overload;
      procedure QueueWork(const workUnit : TProcedureWorkUnit); overload;
      procedure QueueWork(const workUnit : TNotifyEvent; sender : TObject); overload;

      procedure ParallelFor(fromIndex, toIndex : Integer; const aProc : TProc<Integer>);

      function QueueSize : Integer;
      function QueueSizeInfo : TWorkerThreadQueueSizeInfo;

      function IsIdle : Boolean;

      procedure ResetPeakStats;
   end;

   TIOCPWorkerThreadPool = class (TInterfacedSelfObject, IWorkerThreadPool)
      private
         FIOCP : THandle;
         FWorkerCount : Integer;
         FLiveWorkerCount : Integer;
         FActiveWorkerCount : Integer;
         FPeakActiveWorkerCount : Integer;
         FQueueSize : Integer;
         FPeakQueueSize : Integer;
         FTimerQueue : THandle;
         FDelayed : TIOCPDelayedWork;
         FDelayedLock : TMultiReadSingleWrite;

      protected
         function GetWorkerCount : Integer;
         procedure SetWorkerCount(val : Integer);

         procedure IncrementQueueSize; inline;

      public
         constructor Create(aWorkerCount : Integer);
         destructor Destroy; override;

         procedure Shutdown(timeoutMilliSeconds : Cardinal = INFINITE);

         procedure QueueWork(const workUnit : TAnonymousWorkUnit); overload;
         procedure QueueWork(const workUnit : TProcedureWorkUnit); overload;
         procedure QueueWork(const workUnit : TNotifyEvent; sender : TObject); overload;

         procedure QueueDelayedWork(delayMillisecSeconds : Cardinal; const workUnit : TNotifyEvent; sender : TObject);

         function QueueSize : Integer; inline;
         function QueueSizeInfo : TWorkerThreadQueueSizeInfo;

         procedure ParallelFor(fromIndex, toIndex : Integer; const aProc : TProc<Integer>);

         property WorkerCount : Integer read FWorkerCount write SetWorkerCount;
         function LiveWorkerCount : Integer;
         function ActiveWorkerCount : Integer;
         function PeakActiveWorkerCount : Integer;
         function IsIdle : Boolean;

         procedure ResetPeakStats;
   end;

   TIOCPTaskStatus = (
      tsUnscheduled,
      tsScheduled,
      tsRunning,
      tsCompleted,
      tsCanceled
   );

   TIOCPTask = class;

   IWorkerTask = interface
      ['{1002DCCE-2C06-4885-9F1F-3F81A3807A16}']
      function Name : String;
      function Status : TIOCPTaskStatus;
      function GetSelf : TIOCPTask;

      procedure DependsFrom(const aTask : IWorkerTask);

      procedure Cancel;
      procedure Schedule(const aPool : IWorkerThreadPool);
      procedure WaitFor(timeOutMSec : Cardinal = INFINITE);
   end;

   TIOCPTask = class abstract (TInterfacedObject, IWorkerTask)
      private
         FName : String;
         FDependsFrom : TArray<IWorkerTask>;
         FDependsTo : TArray<IWorkerTask>;
         FStatus : TIOCPTaskStatus;
         FLock : TMultiReadSingleWrite;
         FPool : IWorkerThreadPool;
         FWaitEvent : THandle;
         FExceptionMessage : String;

      protected
         procedure Run;

         function GetSelf : TIOCPTask;
         procedure ClearDependsFrom;
         procedure ClearDependsTo;

         procedure ThreadSafeRemoveDependsFrom(const aTask : IWorkerTask);
         procedure ThreadSafeRemoveDependsTo(const aTask : IWorkerTask);

         class function GetRunWorkUnit(task : IWorkerTask) : TAnonymousWorkUnit; static;

      public
         constructor Create(const aName : String = '');
         destructor Destroy; override;

         procedure DependsFrom(const aTask : IWorkerTask);

         procedure Schedule(const aPool : IWorkerThreadPool);
         procedure Cancel;
         procedure WaitFor(timeOutMSec : Cardinal = INFINITE);

         procedure Execute; virtual;

         function Name : String; inline;
         function Status : TIOCPTaskStatus; inline;
         function ExceptionMessage : String;
   end;

   TWorkerTaskProc<TParam> = reference to procedure (const aTask : IWorkerTask; const aParam : TParam);

   TWorkerTask<TParam> = class abstract (TIOCPTask)
      private
         FProc : TWorkerTaskProc<TParam>;
         FParam : TParam;

      public
         constructor Create(const aParam : TParam; const aProc : TWorkerTaskProc<TParam>;
                            const aName : String = ''); overload;

         procedure Execute; override;
   end;

   WorkerTask = class sealed
      public
         class function New(const aProc : TProc; const aName : String = '') : IWorkerTask; overload; static;
         class function New(idx : Integer; const aProc : TProc<Integer>; const aName : String = '') : IWorkerTask; overload; static;
         class function New<TParam>(const aParam : TParam; const aProc : TProc<TParam>;
                                    const aName : String = '') : IWorkerTask; overload; static;
         class function New<TParam>(const aParam : TParam; const aProc : TWorkerTaskProc<TParam>;
                                    const aName : String = '') : IWorkerTask; overload; static;
   end;


procedure ParallelFor(const pool : IWorkerThreadPool; fromIndex, toIndex : Integer; const aProc : TProc<Integer>);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

function CoInitialize(pvReserved: Pointer): HResult; stdcall; external 'ole32.dll';
procedure CoUninitialize; stdcall; external 'ole32.dll';

// ParallelFor
//
procedure ParallelFor(const pool : IWorkerThreadPool; fromIndex, toIndex : Integer; const aProc : TProc<Integer>);
var
   i : Integer;
begin
   if pool <> nil then
      pool.ParallelFor(fromIndex, toIndex, aProc)
   else for i := fromIndex to toIndex do
      aProc(i);
end;

const
   WORK_UNIT_TERMINATE = 0;
   WORK_UNIT_ANONYMOUS = 1;
   WORK_UNIT_PROCEDURE = 2;

type

   {$IFNDEF VER270}
   ULONG_PTR = {$IFDEF DELPHI_XE2_PLUS}NativeUInt{$ELSE}DWORD{$ENDIF};
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
         AtomicDecrement(dw.FPool.FQueueSize);
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
   inherited;
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
         FNext.FPrev := FPrev;
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

   {$ifdef DELPHI_TOKYO_PLUS}
   procedure ExecuteAnonymousFunction(p : PAnonymousWorkUnit);
   var
      wu : TAnonymousWorkUnit;
   begin
      PPointer(@wu)^ := PPointer(p)^;
      wu();
   end;
   {$else}
   procedure ExecuteAnonymousFunction(p : PAnonymousWorkUnit);
   begin
      try
         p^();
      finally
         p^._Release;
      end;
   end;
   {$endif}

var
   data : TIOCPData;
begin
   CoInitialize(nil);

   NameThreadForDebugging(ClassName);

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
            if FPool.FActiveWorkerCount > FPool.FPeakActiveWorkerCount then
               FPool.FPeakActiveWorkerCount := FPool.FActiveWorkerCount;
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

// IncrementQueueSize
//
procedure TIOCPWorkerThreadPool.IncrementQueueSize;
var
   n : Integer;
begin
   n := AtomicIncrement(FQueueSize);
   if n > FPeakQueueSize then
      FPeakQueueSize := n;
end;

// QueueWork
//
procedure TIOCPWorkerThreadPool.QueueWork(const workUnit : TAnonymousWorkUnit);
var
   lpOverlapped : Pointer;
begin
   IncrementQueueSize;
   lpOverlapped:=nil;
   PAnonymousWorkUnit(@lpOverlapped)^:=workUnit;
   if not PostQueuedCompletionStatus(FIOCP, WORK_UNIT_ANONYMOUS, 0, lpOverlapped) then begin
      FastInterlockedDecrement(FQueueSize);
      RaiseLastOSError;
   end;
end;

// QueueWork
//
procedure TIOCPWorkerThreadPool.QueueWork(const workUnit : TProcedureWorkUnit);
var
   lpOverlapped : Pointer;
begin
   IncrementQueueSize;
   lpOverlapped:=Pointer(@workUnit);
   if not PostQueuedCompletionStatus(FIOCP, WORK_UNIT_PROCEDURE, 0, lpOverlapped) then begin
      FastInterlockedDecrement(FQueueSize);
      RaiseLastOSError;
   end;
end;

// QueueWork
//
procedure TIOCPWorkerThreadPool.QueueWork(const workUnit : TNotifyEvent; sender : TObject);
var
   data : TIOCPData;
begin
   IncrementQueueSize;
   data.notifyEvent:=workUnit;
   data.sender:=sender;
   if not PostQueuedCompletionStatus(FIOCP, data.lpNumberOfBytesTransferred, data.lpCompletionKey, data.lpOverlapped) then begin
      FastInterlockedDecrement(FQueueSize);
      RaiseLastOSError;
   end;
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
      IncrementQueueSize;
   except
      dw.Free;
      raise;
   end;
end;

// QueueSize
//
function TIOCPWorkerThreadPool.QueueSize : Integer;
begin
   Result := FQueueSize;
end;

// QueueSizeInfo
//
function TIOCPWorkerThreadPool.QueueSizeInfo : TWorkerThreadQueueSizeInfo;
var
   p : TIOCPDelayedWork;
begin
   FDelayedLock.BeginRead;
   try
      Result.Delayed := 0;
      p := FDelayed;
      while p <> nil do begin
         Inc(Result.Delayed);
         p := p.FNext;
      end;
   finally
      FDelayedLock.EndRead;
   end;
   // prettify the info: minor incoherencies are possible since we do not do a full freeze
   Result.Total := FQueueSize;
   if Result.Delayed > Result.Total then
      Result.Total := Result.Delayed;
   if Result.Total > FPeakQueueSize then
      FPeakQueueSize := Result.Total;
   Result.Peak := FPeakQueueSize;
end;

// ParallelFor
//
procedure TIOCPWorkerThreadPool.ParallelFor(fromIndex, toIndex : Integer; const aProc : TProc<Integer>);
var
   counter : Integer;
   event : THandle;

   function CreateWorkUnit(index : Integer) : TAnonymousWorkUnit;
   begin
      Result := procedure
         begin
            try
               aProc(index);
            finally
               if AtomicDecrement(counter) = 0 then
                  SetEvent(event);
            end;
         end;
   end;

begin
   if fromIndex >= toIndex then begin
      if fromIndex = toIndex then
         aProc(fromIndex);
      Exit;
   end;
   event := CreateEvent(nil, True, False, nil);
   try
      counter := toIndex - fromIndex;
      for var index := fromIndex to toIndex-1 do
         QueueWork(CreateWorkUnit(index));
      aProc(toIndex);
   finally
      WaitForSingleObject(event, INFINITE);
      CloseHandle(event);
   end;
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

// PeakActiveWorkerCount
//
function TIOCPWorkerThreadPool.PeakActiveWorkerCount : Integer;
begin
   Result := FPeakActiveWorkerCount;
end;

// IsIdle
//
function TIOCPWorkerThreadPool.IsIdle : Boolean;
begin
   Result := (FQueueSize=0) and (FActiveWorkerCount=0);
end;

// ResetPeakStats
//
procedure TIOCPWorkerThreadPool.ResetPeakStats;
begin
   FPeakQueueSize := FQueueSize;
   FPeakActiveWorkerCount := FActiveWorkerCount;
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

// ------------------
// ------------------ TIOCPTask ------------------
// ------------------

// Create
//
constructor TIOCPTask.Create(const aName : String = '');
begin
   inherited Create;
   FName := aName;
   FLock := TMultiReadSingleWrite.Create;
end;

// Destroy
//
destructor TIOCPTask.Destroy;
begin
   inherited;
   ClearDependsFrom;
   ClearDependsTo;
   FreeAndNil(FLock);
   if FWaitEvent <> 0 then
      CloseHandle(FWaitEvent);
end;

// GetSelf
//
function TIOCPTask.GetSelf : TIOCPTask;
begin
   Result := Self;
end;

// Name
//
function TIOCPTask.Name : String;
begin
   Result := FName;
end;

// Status
//
function TIOCPTask.Status : TIOCPTaskStatus;
begin
   Result := FStatus;
end;

// ExceptionMessage
//
function TIOCPTask.ExceptionMessage : String;
begin
   Result := FExceptionMessage;
end;

// ClearDependsFrom
//
procedure TIOCPTask.ClearDependsFrom;
var
   tmp : TArray<IWorkerTask>;
begin
   tmp := nil;
   FLock.BeginWrite;
   try
      Pointer(tmp) := InterlockedExchangePointer(Pointer(FDependsFrom), Pointer(tmp));
   finally
      FLock.EndWrite;
   end;
   for var i := 0 to High(tmp) do begin
      var fromTask := tmp[i].GetSelf;
      fromTask.ThreadSafeRemoveDependsTo(Self);
      tmp[i] := nil;
   end;
end;

// ClearDependsTo
//
procedure TIOCPTask.ClearDependsTo;
var
   tmp : TArray<IWorkerTask>;
begin
   tmp := nil;
   FLock.BeginWrite;
   try
      Pointer(tmp) := InterlockedExchangePointer(Pointer(FDependsTo), Pointer(tmp));
   finally
      FLock.EndWrite;
   end;
   for var i := 0 to High(tmp) do begin
      tmp[i].GetSelf.ThreadSafeRemoveDependsFrom(Self);
      tmp[i] := nil;
   end;
end;

// ThreadSafeRemoveDependsFrom
//
procedure TIOCPTask.ThreadSafeRemoveDependsFrom(const aTask : IWorkerTask);
begin
   FLock.BeginWrite;
   try
      for var i := High(FDependsFrom) downto 0 do begin
         if FDependsFrom[i] = aTask then
            Delete(FDependsFrom, i, 1);
      end;
   finally
      FLock.EndWrite;
   end;
end;

// ThreadSafeRemoveDependsTo
//
procedure TIOCPTask.ThreadSafeRemoveDependsTo(const aTask : IWorkerTask);
begin
   FLock.BeginWrite;
   try
      for var i := High(FDependsTo) downto 0 do begin
         if FDependsTo[i] = aTask then
            Delete(FDependsTo, i, 1);
      end;
   finally
      FLock.EndWrite;
   end;
end;

// GetRunWorkUnit
//
class function TIOCPTask.GetRunWorkUnit(task : IWorkerTask) : TAnonymousWorkUnit;
begin
   Result := procedure begin
      task.GetSelf.Run;
   end;
end;

// DependsFrom
//
procedure TIOCPTask.DependsFrom(const aTask : IWorkerTask);
begin
   FLock.BeginWrite;
   try
      var n := Length(FDependsFrom);
      for var i := 0 to n-1 do
         if FDependsFrom[i] = aTask then
            raise Exception.Create('Attempting to add dependency duplicate');
      SetLength(FDependsFrom, n+1);
      FDependsFrom[n] := aTask;
   finally
      FLock.EndWrite;
   end;
   var task := aTask.GetSelf;
   task.FLock.BeginWrite;
   try
      var n := Length(task.FDependsTo);
      SetLength(task.FDependsTo, n+1);
      task.FDependsTo[n] := Self;
   finally
      task.FLock.EndWrite;
   end;
end;

// Schedule
//
procedure TIOCPTask.Schedule(const aPool : IWorkerThreadPool);
var
   dep : IWorkerTask;
begin
   if not (Status in [ tsScheduled, tsUnscheduled]) then Exit;

   FLock.BeginWrite;
   try
      if not (Status in [ tsScheduled, tsUnscheduled]) then Exit;

      if FStatus = tsUnscheduled then begin
         FStatus := tsScheduled;
         FPool := aPool;
      end;
   finally
      FLock.EndWrite;
   end;

   repeat
      dep := nil;
      FLock.BeginRead;
      try
         for var i := 0 to High(FDependsFrom) do begin
            if FDependsFrom[i].Status in [ tsUnscheduled, tsCanceled ] then begin
               dep := FDependsFrom[i];
               Break;
            end;
         end;
      finally
         FLock.EndRead;
      end;
      if dep <> nil then case dep.Status of
         tsUnscheduled :
            dep.Schedule(aPool);
         tsCanceled : begin
            Cancel;
            Exit;
         end;
      end;
   until dep = nil;

   var unmetDependsFrom := 0;
   FLock.BeginRead;
   try
      for var i := 0 to High(FDependsFrom) do begin
         dep := FDependsFrom[i];
         case dep.Status of
            tsUnscheduled : Assert(False, dep.Name);
            tsScheduled, tsRunning :
               Inc(unmetDependsFrom);
            tsCompleted : ;
            tsCanceled : begin
               unmetDependsFrom := -1;
               Break;
            end;
         else
            Assert(False);
         end;
      end;
   finally
      FLock.EndRead;
   end;

   if unmetDependsFrom = 0 then begin
      var shouldRun := False;
      FLock.BeginWrite;
      try
         if Status = tsScheduled then begin
            FStatus := tsRunning;
            shouldRun := True;
         end;
      finally
         FLock.EndWrite;
      end;
      if shouldRun then begin
         if FPool = nil then
            Run
         else begin
            FPool.QueueWork(GetRunWorkUnit(Self));
         end;
      end;
   end else if unmetDependsFrom = -1 then
      Cancel;
end;

// Cancel
//
procedure TIOCPTask.Cancel;
var
   tmp : TArray<IWorkerTask>;
begin
   if Status = tsCanceled then Exit;
   FLock.BeginRead;
   try
      tmp := Copy(FDependsTo, 0);
      if FWaitEvent <> 0 then
         SetEvent(FWaitEvent);
   finally
      FLock.EndRead;
   end;
   for var i := 0 to High(tmp) do
      tmp[i].Cancel;
end;

// WaitFor
//
procedure TIOCPTask.WaitFor(timeOutMSec : Cardinal = INFINITE);
begin
   if Status in [ tsCompleted, tsCanceled ] then Exit;
   if FWaitEvent = 0 then begin
      FLock.BeginWrite;
      try
         if FWaitEvent = 0 then
            FWaitEvent := CreateEvent(nil, True, False, nil);
      finally
         FLock.EndWrite;
      end;
   end;
   WaitForSingleObject(FWaitEvent, timeOutMSec);
end;

// Execute
//
procedure TIOCPTask.Execute;
begin
   // nothing
end;

// Run
//
procedure TIOCPTask.Run;

   procedure RecordException(E: Exception);
   begin
      FExceptionMessage := E.ClassName + ': ' + E.Message;
   end;

var
   tmp : TArray<IWorkerTask>;
begin
   try
      ClearDependsFrom;
      if Status = tsRunning then
         Execute;
      FLock.BeginWrite;
      try
         if Status = tsRunning then
            FStatus := tsCompleted;
         tmp := Copy(FDependsTo, 0);
         if FWaitEvent <> 0 then
            SetEvent(FWaitEvent);
      finally
         FLock.EndWrite;
      end;
      for var i := 0 to High(tmp) do
         tmp[i].Schedule(FPool);
      ClearDependsTo;
   except
      on E: Exception do begin
         RecordException(E);
         Cancel;
      end;
   end;
end;

// ------------------
// ------------------ TWorkerTask<TParam> ------------------
// ------------------

// Create
//
constructor TWorkerTask<TParam>.Create(const aParam : TParam; const aProc : TWorkerTaskProc<TParam>;
                                       const aName : String = '');
begin
   inherited Create(aName);
   FProc := aProc;
   FParam := aParam;
end;

// Execute
//
procedure TWorkerTask<TParam>.Execute;
begin
   FProc(Self, FParam);
end;

// ------------------
// ------------------ WorkerTask ------------------
// ------------------

// New
//
class function WorkerTask.New(const aProc : TProc; const aName : String = '') : IWorkerTask;
begin
   Result := TWorkerTask<Integer>.Create(
      0, procedure (const task : IWorkerTask; const param : Integer)
         begin
            aProc();
         end,
      aName);
end;

// New
//
class function WorkerTask.New(idx : Integer; const aProc : TProc<Integer>; const aName : String = '') : IWorkerTask;
begin
   Result := TWorkerTask<Integer>.Create(
      0, procedure (const task : IWorkerTask; const param : Integer)
         begin
            aProc(idx);
         end,
      aName);
end;

// New<TParam>
//
class function WorkerTask.New<TParam>(const aParam : TParam; const aProc : TProc<TParam>;
                                      const aName : String = '') : IWorkerTask;
var
   adapter : TWorkerTaskProc<TParam>;
begin
   adapter := procedure (const task : IWorkerTask; const param : TParam)
              begin
                 aProc(aParam);
              end;
   Result := TWorkerTask<TParam>.Create(aParam, adapter, aName);
end;

// New<TParam>
//
class function WorkerTask.New<TParam>(const aParam : TParam; const aProc : TWorkerTaskProc<TParam>;
                                    const aName : String = '') : IWorkerTask;
begin
   Result := TWorkerTask<TParam>.Create(aParam, aProc, aName);
end;

end.
