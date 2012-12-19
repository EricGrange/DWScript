unit dwsCPUUsage;

interface

uses Windows, MMSystem, SysUtils, dwsUtils;

type
   SystemCPUCategory = (cpuUser, cpuKernel, cpuIdle);

   SystemCPU = class
      class procedure Track(everyMilliseconds : Integer = 500); static;
      class procedure UnTrack; static;

      class function Tracking : Boolean; static;

      class function Count : Integer; static;
      class function Usage : Integer; static;

      class function Category(cat : SystemCPUCategory) : Integer; static;

      class property User : Integer index cpuUser read Category;
      class property Kernel : Integer index cpuKernel read Category;
      class property Idle : Integer index cpuIdle read Category;
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
   vCPUUsageScaleFactor : Double;
   vCPUUsageLock : TFixedCriticalSection;
   vCycleSystemCPUUsage : TCPUUsage;
   vLastSystemCPUUsage : TCPUUsage;
   vCPUUsageTimerID : Integer;
   vTrackInterval : Integer;

// FileTimeToDateTime
//
function FileTimeToDateTime(const fileTime : TFileTime) : TDateTime;
const
   cFileTimeBase : Double = -109205.0;
   cFileTimeStep : Double = 24*3000 * 1e7;
begin
   Result:=Int64(fileTime)/cFileTimeStep+cFileTimeBase;
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
   end;
end;

// CPUUsageCallBack
//
var
   vAnalyzingCPUUsage : Boolean;
procedure CPUUsageCallBack(TimerID, Msg: Uint; dwUser, dw1, dw2: DWORD); pascal;
var
   current : TCPUUsage;
   cat : SystemCPUCategory;
begin
   if vAnalyzingCPUUsage then Exit;

   vAnalyzingCPUUsage:=True;

   current:=GetCurrentSystemTimes;
   vCPUUsageLock.Enter;
   try
      for cat:=Low(SystemCPUCategory) to High(SystemCPUCategory) do
         vCycleSystemCPUUsage[cat]:=current[cat]-vLastSystemCPUUsage[cat];
      vLastSystemCPUUsage:=current;
   finally
      vCPUUsageLock.Leave;
   end;

   vAnalyzingCPUUsage:=False;
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

   vLastSystemCPUUsage:=GetCurrentSystemTimes;

   vCycleSystemCPUUsage[cpuUser]:=0;
   vCycleSystemCPUUsage[cpuKernel]:=0;
   vCycleSystemCPUUsage[cpuIdle]:=0;

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

// Count
//
class function SystemCPU.Count : Integer;
var
   processMask, mask : {$IFDEF VER230}NativeUInt{$ELSE}DWORD{$ENDIF};
begin
   if vProcessorCount=0 then begin
      vProcessorCount:=1;
      if GetProcessAffinityMask(GetCurrentProcess, processMask, mask) then begin
         mask:=mask shr 1;
         while mask>0 do begin
            Inc(vProcessorCount);
            mask:=mask shr 1;
         end;
      end;
   end;
   Result:=vProcessorCount;
end;

// Usage
//
class function SystemCPU.Usage : Integer;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.Enter;
   try
      Result:=Round( (  vCycleSystemCPUUsage[cpuKernel] + vCycleSystemCPUUsage[cpuUser]
                      - vCycleSystemCPUUsage[cpuIdle])
                    * vCPUUsageScaleFactor);
   finally
      vCPUUsageLock.Leave;
   end;
end;

// Category
//
class function SystemCPU.Category(cat : SystemCPUCategory) : Integer;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.Enter;
   try
      Result:=Round(vCycleSystemCPUUsage[cat]*vCPUUsageScaleFactor);
   finally
      vCPUUsageLock.Leave;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   vCPUUsageLock:=TFixedCriticalSection.Create;

finalization

   SystemCPU.UnTrack;
   FreeAndNil(vCPUUsageLock);

end.
