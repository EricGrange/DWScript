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
   vCPUUsageLock : TFixedCriticalSection;

   vCycleSystem, vCycleProcess : TCPUUsage;
   vLastSystem, vLastProcess : TCPUUsage;

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

// GetCurrentSystemTimes
//
function GetCurrentProcessTimes(handle : Cardinal) : TCPUUsage;
var
   lpStartTime, lpExitTime, lpKernelTime, lpUserTime : FILETIME;
begin
   if GetProcessTimes(handle, lpStartTime, lpExitTime, lpKernelTime, lpUserTime) then begin
      Result[cpuIdle]:=0;
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
   systemCurrent, processCurrent : TCPUUsage;
   cat : SystemCPUCategory;
begin
   if vAnalyzingCPUUsage then Exit;

   vAnalyzingCPUUsage:=True;

   systemCurrent:=GetCurrentSystemTimes;
   processCurrent:=GetCurrentProcessTimes(GetCurrentProcess);

   vCPUUsageLock.Enter;
   try
      for cat:=Low(SystemCPUCategory) to High(SystemCPUCategory) do begin
         vCycleSystem[cat]:=systemCurrent[cat]-vLastSystem[cat];
         vCycleProcess[cat]:=processCurrent[cat]-vLastProcess[cat];
      end;
      vLastSystem:=systemCurrent;
      vLastProcess:=processCurrent;
   finally
      vCPUUsageLock.Leave;
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

   vCPUUsageLock.Enter;
   try
      Result:=(vCycleSystem[cpuKernel]+vCycleSystem[cpuUser]-vCycleSystem[cpuIdle])*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.Leave;
   end;
end;

// ProcessUsage
//
class function SystemCPU.ProcessUsage : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.Enter;
   try
      Result:=(vCycleProcess[cpuKernel]+vCycleProcess[cpuUser])*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.Leave;
   end;
end;

// Category
//
class function SystemCPU.Category(cat : SystemCPUCategory) : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.Enter;
   try
      Result:=vCycleSystem[cat]*vCPUUsageScaleFactor;
   finally
      vCPUUsageLock.Leave;
   end;
end;

// ProcessCategory
//
class function SystemCPU.ProcessCategory(cat : SystemCPUCategory) : Single;
begin
   if vCPUUsageTimerID=0 then Exit(0);

   vCPUUsageLock.Enter;
   try
      Result:=vCycleProcess[cat]*vCPUUsageScaleFactor;
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
