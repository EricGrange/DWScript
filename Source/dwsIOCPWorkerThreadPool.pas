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
   end;

   TIOCPWorkerThreadPool = class (TInterfacedSelfObject, IWorkerThreadPool)
      private
         FIOCP : THandle;
         FWorkerCount : Integer;
         FLiveWorkerCount : Integer;
         FActiveWorkerCount : Integer;
         FQueueSize : Integer;

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

         function QueueSize : Integer;

         property WorkerCount : Integer read FWorkerCount write SetWorkerCount;
         function LiveWorkerCount : Integer;
         function ActiveWorkerCount : Integer;
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
end;

// Destroy
//
destructor TIOCPWorkerThreadPool.Destroy;
begin
   CloseHandle(FIOCP);

   while LiveWorkerCount>0 do
      Sleep(10);

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
