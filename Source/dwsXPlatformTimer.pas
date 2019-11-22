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
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Dennis Balzuweit (dennis.balzuweit@cto.de)    }
{                                                                      }
{**********************************************************************}
unit dwsXPlatformTimer;

{$I dws.inc}

// -------------------------------------------------------------------------------
// Thanks to Grijjy!
// www.grijjy.com
//
// All Linux timer functions based on the Grijjy foundation
// See https://github.com/grijjy/GrijjyFoundation
//
// GrijjyFoundation is licensed under the Simplified BSD License.
//
// -------------------------------------------------------------------------------
//
// Copyright (c) 2017 by Grijjy, Inc.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// -------------------------------------------------------------------------------

interface

uses
   Classes,
   SysUtils,
   Types,
   SyncObjs,
   Generics.Collections,

   Posix.Base,
   Posix.Time,
   Posix.Fcntl,
   Posix.Signal,
   Posix.Unistd,
   Posix.ErrNo;

const
   TFD_NONBLOCK  = O_NONBLOCK;

   EPOLLIN       = $01;
   EPOLLPRI      = $02;
   EPOLLOUT      = $04;
   EPOLLERR      = $08;
   EPOLLHUP      = $10;
   EPOLLRDNORM   = $40;
   EPOLLRDBAND   = $80;
   EPOLLWRNORM   = $100;
   EPOLLWRBAND   = $200;
   EPOLLMSG      = $400;
   EPOLLRDHUP    = $2000;
   EPOLLWAKEUP   = 1 shl 29;
   EPOLLONESHOT  = 1 shl 30;
   EPOLLET       = UInt32(1 shl 31);

   EPOLL_CTL_ADD = 1;
   EPOLL_CTL_DEL = 2;
   EPOLL_CTL_MOD = 3;

   IGNORED       = 1;
   MAX_EVENTS    = 1024;

   INVALID_HANDLE_VALUE = THandle(-1);

type
   TWaitOrTimerCallback = procedure (Context: Pointer; Success: Boolean); stdcall;
   TTimerEvent = procedure of object;

type
   epoll_data =
   record
      case Integer of
         0: (ptr: Pointer);
         1: (fd: Integer);
         2: (u32: UInt32);
         3: (u64: UInt64);
   end;

   epoll_event =
   packed record
      events: UInt32;
      data : epoll_data;
   end;
   pepoll_event = ^epoll_event;

   ptsigset = ^sigset_t;

function epoll_create(size: Integer): Integer; cdecl; external libc name _PU + 'epoll_create';
function epoll_create1(flags: Integer): Integer; cdecl; external libc name _PU + 'epoll_create1';
function epoll_ctl(epfd: Integer; op: Integer; fd: Integer; event: pepoll_event): Integer; cdecl; external libc name _PU + 'epoll_ctl';
function epoll_wait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer): Integer; cdecl; external libc name _PU + 'epoll_wait';
function epoll_pwait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer; sigmask: ptsigset): Integer; cdecl; external libc name _PU + 'epoll_pwait';
function timerfd_create(clockid: Integer; flags: Integer): Integer; cdecl; external libc name _PU + 'timerfd_create';
function timerfd_settime(fd: Integer; flags: Integer; const new_value: Pitimerspec; old_value: Pitimerspec): Integer; cdecl; external libc name _PU + 'timerfd_settime';
function timerfd_gettime(fd: Integer; curr_value: Pitimerspec): Integer; cdecl; external libc name _PU + 'timerfd_gettime';

type
   TdwsXTimer = class;
   TOndwsXTimer = procedure(const ASender: TObject) of object;

   TdwsXTimer = class(TObject)
   private
      FHandle   : THandle;
      FInterval : Cardinal;
      FOnTimer  : TWaitOrTimerCallback;
      FEvent    : TTimerEvent;
   private
      FClose  : Boolean;
      FClosed : TEvent;
   public
      constructor Create;
      destructor Destroy; override;
   public
      property Handle: THandle read FHandle write FHandle;
      property Interval: Cardinal read FInterval write FInterval;
      property OnTimer: TWaitOrTimerCallback read FOnTimer write FOnTimer;
      property Event: TTimerEvent read FEvent write FEvent;
   end;

   TdwsXTimerQueue = class(TObject)
   private
      FReleaseOnDestroy : Boolean;

      function _SetInterval(const AHandle: THandle; const AInterval: Cardinal): Boolean;
      procedure _Release(const ATimer: TdwsXTimer);
   public
      constructor Create(const AReleaseOnDestroy : Boolean = true);
      destructor Destroy; override;

      procedure ReleaseAll;
   public
      function Add(const AInterval: Cardinal; AEvent : TTimerEvent; const AOnTimer: TWaitOrTimerCallback): THandle;
      procedure Release(const AHandle: THandle);
      function SetInterval(const AHandle: THandle; const AInterval: Cardinal): Boolean;
   end;

   TdwsXTimerQueuePool = class;
   TdwsXTimerQueueWorker = class(TThread)
   private
      FOwner: TdwsXTimerQueuePool;
      FEvents: array[0..MAX_EVENTS] of epoll_event;
   protected
      procedure Execute; override;
   public
      constructor Create(const AOwner: TdwsXTimerQueuePool);
      destructor Destroy; override;
   end;

   TdwsXTimerQueuePool = class(TObject)
   private
      FHandle: THandle;
      FWorkers: array of TdwsXTimerQueueWorker;
   public
      constructor Create(const AWorkers: Integer = 0);
      destructor Destroy; override;
   public
      property Handle: THandle read FHandle;
   end;

implementation

var
   _Timers         : TDictionary<THandle, TdwsXTimer>;
   _TimersLock     : TCriticalSection;
   _TimerQueuePool : TdwsXTimerQueuePool;

constructor TdwsXTimer.Create;
begin
   inherited;

   FHandle   := INVALID_HANDLE_VALUE;
   FInterval := 0;
   FOnTimer  := nil;
   FClose    := False;
   FClosed   := TEvent.Create(nil, True, False, '');
end;

destructor TdwsXTimer.Destroy;
begin
   FClosed.Free;
   inherited;
end;

constructor TdwsXTimerQueue.Create(const AReleaseOnDestroy : Boolean = true);
begin
   inherited Create();

   FReleaseOnDestroy := AReleaseOnDestroy;
end;

destructor TdwsXTimerQueue.Destroy;
begin
   if FReleaseOnDestroy = true then ReleaseAll;
   inherited;
end;

function TdwsXTimerQueue._SetInterval(const AHandle: THandle; const AInterval: Cardinal): Boolean;
var
   NewValue : itimerspec;
   TS       : timespec;
begin
   FillChar(NewValue, SizeOf(itimerspec), 0);
   TS.tv_sec := AInterval DIV 1000;
   TS.tv_nsec := (AInterval MOD 1000) * 1000000;
   NewValue.it_value := TS;
   NewValue.it_interval := TS;
   Result := timerfd_settime(AHandle, 0, @NewValue, nil) <> -1;
end;

function TdwsXTimerQueue.Add(const AInterval: Cardinal; AEvent : TTimerEvent; const AOnTimer: TWaitOrTimerCallback): THandle;
var
   Handle : THandle;
   Timer  : TdwsXTimer;
   Event  : epoll_event;
begin
   Result := INVALID_HANDLE_VALUE;

   Handle := timerfd_create(CLOCK_MONOTONIC, TFD_NONBLOCK);
   if Handle <> -1 then
   begin
      Timer          := TdwsXTimer.Create;
      Timer.Handle   := Handle;
      Timer.Interval := AInterval;
      Timer.OnTimer  := AOnTimer;
      Timer.Event    := AEvent;

      Event.data.ptr := Timer;
      Event.events   := EPOLLIN or EPOLLET;
      if epoll_ctl(_TimerQueuePool.Handle, EPOLL_CTL_ADD, Handle, @Event) <> -1 then
      begin
        if _SetInterval(Handle, AInterval) then
        begin
          _TimersLock.Enter;
           try
              _Timers.Add(Handle, Timer);
           finally
              _TimersLock.Leave;
           end;
           Result := Handle;
        end else Timer.Free;
      end else
      begin
         __close(Handle);
         Timer.Free;
      end;
   end;
end;

procedure TdwsXTimerQueue._Release(const ATimer: TdwsXTimer);
begin
   ATimer.FClose := True;

   _SetInterval(ATimer.Handle, 1);

   ATimer.FClosed.WaitFor(INFINITE);
   ATimer.DisposeOf;
end;

procedure TdwsXTimerQueue.Release(const AHandle: THandle);
var
   Timer: TdwsXTimer;
begin
   Timer := nil;
  _TimersLock.Enter;
   try
      if _Timers.TryGetValue(AHandle, Timer) then
         _Timers.Remove(AHandle);
   finally
      _TimersLock.Leave;
   end;
   if Timer <> nil then
      _Release(Timer);
end;

procedure TdwsXTimerQueue.ReleaseAll;
var
   Timer: TdwsXTimer;
begin
  _TimersLock.Enter;
   try
      for Timer in _Timers.Values do _Release(Timer);
      _Timers.Clear;
   finally
      _TimersLock.Leave;
   end;
end;

function TdwsXTimerQueue.SetInterval(const AHandle: THandle; const AInterval: Cardinal): Boolean;
var
   Timer: TdwsXTimer;
begin
   Result := False;
  _TimersLock.Enter;
   try
      if _Timers.TryGetValue(AHandle, Timer) then
         if _SetInterval(AHandle, AInterval) then
         begin
            Timer.Interval := AInterval;
            Result := True;
         end;
   finally
      _TimersLock.Leave;
   end;
end;

constructor TdwsXTimerQueueWorker.Create(const AOwner: TdwsXTimerQueuePool);
begin
   FOwner := AOwner;
   inherited Create(False);
end;

destructor TdwsXTimerQueueWorker.Destroy;
begin
   inherited;
end;

procedure TdwsXTimerQueueWorker.Execute;
var
   NumberOfEvents: Integer;
   I: Integer;
   Event: epoll_event;
   TotalTimeouts: Int64;
   Timer: TdwsXTimer;
   Error: Integer;
begin
   while not Terminated do
   begin
      NumberOfEvents := epoll_wait(FOwner.Handle, @FEvents, MAX_EVENTS, 100);
      if NumberOfEvents = 0 then Continue else
      if NumberOfEvents = -1 then
      begin
         Error := errno;
         if Error = EINTR then Continue else Break;
      end;
      for I := 0 to NumberOfEvents - 1 do
      begin
         try
            Timer := FEvents[I].data.ptr;
            if not Timer.FClose then
            begin
               if (FEvents[I].events AND EPOLLIN) = EPOLLIN then
               begin
                  if __read(Timer.Handle, @TotalTimeouts, SizeOf(TotalTimeouts)) >= 0 then
                  begin
                     if Assigned(Timer.FOnTimer) then Timer.FOnTimer(@Timer,true);
                  end else Timer.FClose := True;
               end;
            end;
        finally
           if Timer.FClose then
           begin
              epoll_ctl(_TimerQueuePool.Handle, EPOLL_CTL_DEL, Timer.Handle, @Event); { -1 on error }
             __close(Timer.Handle);
             Timer.FClosed.SetEvent;
           end;
        end;
      end;
   end;
end;

constructor TdwsXTimerQueuePool.Create(const AWorkers: Integer);
var
   I: Integer;
   Workers: Integer;
begin
   inherited Create;

   FHandle := epoll_create(IGNORED);
   if FHandle <> -1 then
   begin
      if AWorkers = 0
         then Workers := CPUCount
         else Workers := AWorkers;

      SetLength(FWorkers, Workers);
      for I := 0 to Workers - 1 do
         FWorkers[I] := TdwsXTimerQueueWorker.Create(Self);
   end else raise Exception.Create(Format('epoll_create failed %s',[SysErrorMessage(errno)]));
end;

destructor TdwsXTimerQueuePool.Destroy;
var
   Worker: TdwsXTimerQueueWorker;
begin
   for Worker in FWorkers do Worker.Terminate;
   for Worker in FWorkers do Worker.WaitFor;
   for Worker in FWorkers do Worker.DisposeOf;
   if FHandle <> -1 then __close(FHandle);

   inherited Destroy;
end;

initialization
   _TimerQueuePool := TdwsXTimerQueuePool.Create;
   _Timers         := TDictionary<THandle, TdwsXTimer>.Create;
   _TimersLock     := TCriticalSection.Create;

finalization
   _TimersLock.Enter;
   try
      _Timers.Free;
   finally
      _TimersLock.Leave;
   end;
   _TimersLock.Free;
   _TimerQueuePool.Free;

end.
