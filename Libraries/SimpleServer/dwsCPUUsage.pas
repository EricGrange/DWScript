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
unit dwsCPUUsage;

interface

uses Windows, MMSystem, SysUtils, Registry,
   dwsUtils, dwsXPlatform;

type
   SystemCPUCategory = (cpuUser, cpuKernel, cpuIdle);

   SystemCPU = class
      class procedure Track(everyMilliseconds : Integer = 500); static;
      class procedure UnTrack; static;

      class function Tracking : Boolean; static;

      class function Name : String; static;
      class function Count : Integer; static;
      class function Frequency : Integer; static;

      class function Usage : Single; static;
      class function ProcessUsage : Single; static;

      class function Category(cat : SystemCPUCategory) : Single; static;
      class function ProcessCategory(cat : SystemCPUCategory) : Single; static;

      class function SystemTimes(cat : SystemCPUCategory) : Double; static;
      class function ProcessTimes(cat : SystemCPUCategory) : Double; static;

      class property User : Single index cpuUser read Category;
      class property Kernel : Single index cpuKernel read Category;
      class property Idle : Single index cpuIdle read Category;

      class property ProcessUser : Single index cpuUser read ProcessCategory;
      class property ProcessKernel : Single index cpuKernel read ProcessCategory;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TCPUUsage = array [SystemCPUCategory] of TDateTime;

var
   vProcessorCount : Integer;
   vProcessorName : String;
   vProcessorFreq : Integer;

   vCPUUsageScaleFactor : Double;
   vCPUUsageLock : TMultiReadSingleWrite;

   vCycleSystem, vCycleProcess : TCPUUsage;
   vLastSystem, vLastProcess : TCPUUsage;

   vCPUUsageTimerID : Integer;
   vTrackInterval : Integer;

// FileTimeToDateTime
//
function FileTimeToDateTime(const fileTime : TFileTime) : TDateTime;
const
   cFileTimeStep : Double = 24*3600 * 1e7;
begin
   Result:=Int64(fileTime)/cFileTimeStep;
end;

// GetCurrentSystemTimes
//
function GetCurrentSystemTimes : TCPUUsage;
var
   lpIdleTime, lpKernelTime, lpUserTime : FILETIME;
begin
   if GetSystemTimes(lpIdleTime, lpKernelTime, lpUserTime) then begin
      Result[cpuIdle]:=FileTimeToDateTime(lpIdleTime);
      Result[cpuKernel]:=FileTimeToDateTime(lpKernelTime);
      Result[cpuUser]:=FileTimeToDateTime(lpUserTime);
   end else FillChar(Result, SizeOf(Result), 0);
end;

// GetCurrentSystemTimes
//
function GetCurrentProcessTimes(handle : THandle) : TCPUUsage;
var
   lpStartTime, lpExitTime, lpKernelTime, lpUserTime : FILETIME;
begin
   if GetProcessTimes(handle, lpStartTime, lpExitTime, lpKernelTime, lpUserTime) then begin
      Result[cpuIdle]:=0;
      Result[cpuKernel]:=FileTimeToDateTime(lpKernelTime);
      Result[cpuUser]:=FileTimeToDateTime(lpUserTime);
   end else FillChar(Result, SizeOf(Result), 0);
end;

// CPUUsageCallBack
//
var
   vAnalyzingCPUUsage : Boolean;
procedure CPUUsageCallBack(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD_PTR); stdcall;
var
   systemCurrent, processCurrent : TCPUUsage;
   cat : SystemCPUCategory;
begin
   if vAnalyzingCPUUsage then Exit;
   if vCPUUsageTimerID = 0 then Exit;

   vAnalyzingCPUUsage := True;

   systemCurrent:=GetCurrentSystemTimes;
   processCurrent:=GetCurrentProcessTimes(GetCurrentProcess);

   vCPUUsageLock.BeginWrite;
   try
      for cat:=Low(SystemCPUCategory) to High(SystemCPUCategory) do begin
         vCycleSystem[cat]:=systemCurrent[cat]-vLastSystem[cat];
         vCycleProcess[cat]:=processCurrent[cat]-vLastProcess[cat];
      end;
      vLastSystem:=systemCurrent;
      vLastProcess:=processCurrent;
   finally
      vCPUUsageLock.EndWrite;
   end;

   vAnalyzingCPUUsage:=False;
end;

procedure GetCPUStaticInfo;
var
   reg : TRegistry;
begin
   vProcessorCount:=1;

   try
      reg:=TRegistry.Create;
      try
         reg.RootKey:=HKEY_LOCAL_MACHINE;
         reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0');
         vProcessorName:=reg.ReadString('ProcessorNameString');
         vProcessorFreq:=reg.ReadInteger('~MHz');
         reg.CloseKey;

         while reg.KeyExists('HARDWARE\DESCRIPTION\System\CentralProcessor\'+IntToStr(vProcessorCount)) do
            Inc(vProcessorCount);
      finally
         reg.Free;
      end;
   except
      vProcessorName:='Unknown';
      vProcessorFreq:=1;
   end;
end;

// ------------------
// ------------------ SystemCPU ------------------
// ------------------

// Track
//
class procedure SystemCPU.Track(everyMilliseconds : Integer = 500);
begin
   if (vCPUUsageTimerID<>0) and (vTrackInterval=everyMilliseconds) then Exit;

   UnTrack;

   vTrackInterval:=everyMilliseconds;

   vLastSystem:=GetCurrentSystemTimes;

   vCycleSystem[cpuUser]:=0;
   vCycleSystem[cpuKernel]:=0;
   vCycleSystem[cpuIdle]:=0;

   vCPUUsageScaleFactor:=100/(Count*everyMilliseconds*(1/1000/86400));
   vCPUUsageTimerID:=timeSetEvent(everyMilliseconds, 0, @CPUUsageCallBack, 0, TIME_PERIODIC);
end;

// UnTrack
//
class procedure SystemCPU.UnTrack;
begin
   if vCPUUsageTimerID<>0 then begin
      TimeKillEvent(vCPUUsageTimerID);
      vCPUUsageTimerID:=0;
   end;
end;

// Tracking
//
class function SystemCPU.Tracking : Boolean;
begin
   Result:=(vCPUUsageTimerID<>0);
end;

// Name
//
class function SystemCPU.Name : String;
begin
   if vProcessorCount=0 then
      GetCPUStaticInfo;
   Result:=vProcessorName;
end;

// Count
//
class function SystemCPU.Count : Integer;
begin
   if vProcessorCount=0 then
      GetCPUStaticInfo;
   Result:=vProcessorCount;
end;

// Frequency
//
class function SystemCPU.Frequency : Integer;
begin
   if vProcessorCount=0 then
      GetCPUStaticInfo;
   Result:=vProcessorFreq;
end;

// Usage
//
class function SystemCPU.Usage : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.BeginRead;
   try
      Result:=(vCycleSystem[cpuKernel]+vCycleSystem[cpuUser]-vCycleSystem[cpuIdle])*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.EndRead;
   end;
end;

// ProcessUsage
//
class function SystemCPU.ProcessUsage : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.BeginRead;
   try
      Result:=(vCycleProcess[cpuKernel]+vCycleProcess[cpuUser])*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.EndRead;
   end;
end;

// Category
//
class function SystemCPU.Category(cat : SystemCPUCategory) : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.BeginRead;
   try
      Result:=vCycleSystem[cat]*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.EndRead;
   end;
end;

// ProcessCategory
//
class function SystemCPU.ProcessCategory(cat : SystemCPUCategory) : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.BeginRead;
   try
      Result:=vCycleProcess[cat]*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.EndRead;
   end;
end;

// SystemTimes
//
class function SystemCPU.SystemTimes(cat : SystemCPUCategory) : Double;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.BeginRead;
   try
      Result:=vLastSystem[cat];
   finally
      vCPUUsageLock.EndRead;
   end;
end;

// ProcessTimes
//
class function SystemCPU.ProcessTimes(cat : SystemCPUCategory) : Double;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.BeginRead;
   try
      Result:=vLastProcess[cat];
   finally
      vCPUUsageLock.EndRead;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vCPUUsageLock:=TMultiReadSingleWrite.Create;

finalization

   SystemCPU.UnTrack;
   FreeAndNil(vCPUUsageLock);

end.
