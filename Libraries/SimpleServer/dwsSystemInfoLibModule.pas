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
unit dwsSystemInfoLibModule;

interface

uses
  Windows, SysUtils, Classes, Registry, PsAPI, ImageHlp,
  dwsExprList, dwsDataContext, dwsComp, dwsExprs, dwsUtils, dwsCPUUsage,
  dwsXPlatform, dwsInfo, dwsSymbols;

type
   TOSNameVersion = record
      Name : String;
      Version : String;
   end;

  TdwsSystemInfoLibModule = class(TDataModule)
    dwsSystemInfo: TdwsUnit;
    dwsSystemRegistry: TdwsUnit;
    procedure dwsSystemInfoClassesMemoryStatusConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsSystemInfoClassesOSVersionInfoMethodsNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure dwsSystemInfoClassesOSVersionInfoMethodsVersionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsSystemUsageEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsCountEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsKernelUsageEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsFrequencyMHzEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsProcessUsageEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsVersionEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsExeNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsExeLinkTimeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsRunningAsServiceEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesHostInfoMethodsNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsSystemInfoClassesHostInfoMethodsDNSDomainEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesHostInfoMethodsDNSFullyQualifiedEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesHostInfoMethodsDNSNameEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsGetEnvironmentVariableEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsSetEnvironmentVariableEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsMemoryCountersEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsSystemTimeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsKernelTimeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsProcessTimeEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesCPUInfoMethodsUpTimeEval(Info: TProgramInfo;
      ExtObject: TObject);
    function dwsSystemInfoFunctionsSystemMillisecondsFastEval(
      const args: TExprBaseListExec): Variant;
    procedure dwsSystemRegistryClassesRegistryMethodsReadValueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemRegistryClassesRegistryMethodsWriteValueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemRegistryClassesRegistryMethodsDeleteValueEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemRegistryClassesRegistryMethodsCreateKeyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemRegistryClassesRegistryMethodsDeleteKeyEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemRegistryClassesRegistryMethodsSubKeysEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemRegistryClassesRegistryMethodsValueNamesEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesApplicationInfoMethodsUserNameEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesHostInfoMethodsDomainControllerInfoEval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure dwsSystemInfoClassesPerformanceCounterCleanUp(
      ExternalObject: TObject);
    procedure dwsSystemInfoClassesPerformanceCounterConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsSystemInfoClassesPerformanceCounterMethodsRestartFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    procedure dwsSystemInfoClassesPerformanceCounterMethodsStopFastEvalNoResult(
      baseExpr: TTypedExpr; const args: TExprBaseListExec);
    function dwsSystemInfoClassesPerformanceCounterMethodsElapsedFastEvalFloat(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
    function dwsSystemInfoClassesPerformanceCounterMethodsRunningFastEvalBoolean(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
    function dwsSystemInfoClassesPerformanceCounterMethodsNowFastEvalFloat(
      baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
  private
    { Private declarations }
    FOSNameVersion : TOSNameVersion;
    FGlobalMemory : TThreadCached<TMemoryStatusEx>;
    class var FRunningAsService : Boolean;
    function GetScript : TDelphiWebScript;
    procedure SetScript(const val : TDelphiWebScript);

  public
    { Public declarations }
    class property RunningAsService : Boolean read FRunningAsService write FRunningAsService;
    property Script : TDelphiWebScript read GetScript write SetScript;
  end;

function ExecutableLinkTimeStamp : Int64;

implementation

{$R *.dfm}

var
   vPerformanceFrequency : Int64;
   vPerformanceInverseFrequency : Double;

type
   TPerformanceCounter = class
      StartTime : Int64;
      StopTime : Int64;
   end;

// ProcessIsWow64
//
function IsWow64Process(h: THandle; var isWow64: BOOL): BOOL; stdcall; external 'kernel32';
var
   vIsWow64 : ShortInt = 0;
function ProcessIsWow64 : Boolean;
var
   r : BOOL;
begin
   if vIsWow64 = 0 then begin
      if not IsWow64Process(GetCurrentProcess, r) then
         RaiseLastOSError;
      if r then
         vIsWow64 := 1
      else vIsWow64 := -1;
   end;
   Result := (vIsWow64 = 1);
end;

// GetWin32_OSNameVersion
//
procedure GetWin32_OSNameVersion(var osnv : TOSNameVersion);
var
   reg : TRegistry;
begin
   osnv.Name:='?';
   osnv.Version:=Format('%d.%d.%d', [Win32MajorVersion, Win32MinorVersion, Win32BuildNumber]);
   reg:=TRegistry.Create;
   try
      reg.RootKey:=HKEY_LOCAL_MACHINE;
      reg.OpenKeyReadOnly('\Software\Microsoft\Windows NT\CurrentVersion\');
      if reg.ValueExists('ProductName') then begin
         osnv.Name:=reg.ReadString('ProductName');
         if Win32CSDVersion<>'' then
            osnv.Name:=osnv.Name+' '+Win32CSDVersion;
      end;
   finally
      reg.Free;
   end;
end;

// GetGlobalMemory
//
function GetGlobalMemory(var ms : TMemoryStatusEx) : TSimpleCallbackStatus;
begin
   ms.dwLength:=SizeOf(ms);
   GlobalMemoryStatusEx(ms);
   Result:=csContinue;
end;

// GetHostName
//
function GetHostName(nameFormat : TComputerNameFormat) : UnicodeString;
var
   n : Cardinal;
begin
   n:=0;
   GetComputerNameExW(nameFormat, nil, n);
   SetLength(Result, n-1);
   GetComputerNameExW(nameFormat, PWideChar(Pointer(Result)), n);
end;

// CreateRootedRegistry
//
function CreateRootedRegistry(Info : TProgramInfo; access : LongWord) : TRegistry;
var
   rootKey : HKEY;
begin
   rootKey := HKEY(Info.ParamAsInteger[0]);
   if (rootKey = HKEY_LOCAL_MACHINE) and ProcessIsWow64 then
      access := access or $100;
   Result := TRegistry.Create(access);
   try
      Result.RootKey := rootKey;
   except
      Result.Free;
      raise;
   end;
end;

var
   vExeLinkTimeStamp : Int64;
function ExecutableLinkTimeStamp : Int64;

  procedure PrepareExeLinkTimeStamp;
   var
      li : TLoadedImage;
      fileName : AnsiString;
  begin
      fileName := AnsiString(ParamStr(0));
      if MapAndLoad(PAnsiChar(fileName), nil, @li, False, True) then begin
         try
            vExeLinkTimeStamp := li.FileHeader.FileHeader.TimeDateStamp;
         finally
            UnMapAndLoad(@li);
         end;
      end else vExeLinkTimeStamp := -1;
  end;

begin
   if vExeLinkTimeStamp = 0 then
      PrepareExeLinkTimeStamp;
   Result := vExeLinkTimeStamp;
end;

procedure TdwsSystemInfoLibModule.DataModuleCreate(Sender: TObject);
var
   f : Double;
   i : Integer;
begin
   // limit query rate to 10 Hz
   FGlobalMemory:=TThreadCached<TMemoryStatusEx>.Create(GetGlobalMemory, 100);

   GetWin32_OSNameVersion(FOSNameVersion);
   SystemCPU.Track;

   QueryPerformanceFrequency(vPerformanceFrequency);
   vPerformanceInverseFrequency := 1 / vPerformanceFrequency;

   f := vPerformanceFrequency;
   i := dwsSystemInfo.Classes.IndexOf('PerformanceCounter');
   ((dwsSystemInfo.Classes.Items[i] as TdwsClass).Constants.Items[0] as TdwsClassConstant).Value := f;
end;

procedure TdwsSystemInfoLibModule.DataModuleDestroy(Sender: TObject);
begin
   FGlobalMemory.Free;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsExeNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=ParamStr(0);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsExeLinkTimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger := ExecutableLinkTimeStamp;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsGetEnvironmentVariableEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=GetEnvironmentVariable(Info.ParamAsString[0]);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsMemoryCountersEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   pmc : PROCESS_MEMORY_COUNTERS;
   data : TData;
begin
   pmc.cb := SizeOf(pmc);
   if not GetProcessMemoryInfo(GetCurrentProcess, @pmc, pmc.cb) then
      RaiseLastOSError;

   SetLength(data, 4);
   data[0]:=Int64(pmc.WorkingSetSize);
   data[1]:=Int64(pmc.PeakWorkingSetSize);
   data[2]:=Int64(pmc.PagefileUsage);
   data[3]:=Int64(pmc.PeakPagefileUsage);

   Info.ResultVars.Data:=data;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsRunningAsServiceEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsBoolean:=RunningAsService;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsSetEnvironmentVariableEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   SetEnvironmentVariableW(PWideChar(Info.ParamAsString[0]), PWideChar(Info.ParamAsString[1]));
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsVersionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString := ApplicationVersion;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesApplicationInfoMethodsUserNameEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   buf : String;
   n : Cardinal;
begin
   n := 0;
   GetUserName(nil, n);
   if n <= 0 then
      RaiseLastOSError;
   SetLength(buf, n-1);
   if not GetUserName(PChar(buf), n) then
      RaiseLastOSError;
   Info.ResultAsString := buf;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsCountEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=SystemCPU.Count;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsFrequencyMHzEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsInteger:=SystemCPU.Frequency;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsKernelTimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.SystemTimes(cpuKernel)+SystemCPU.ProcessTimes(cpuKernel);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsKernelUsageEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.Kernel;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=SystemCPU.Name;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsProcessTimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.ProcessTimes(cpuUser);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsProcessUsageEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.ProcessUser+SystemCPU.ProcessKernel;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsSystemTimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.SystemTimes(cpuUser);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsSystemUsageEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.Usage;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsUpTimeEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=GetSystemMilliseconds*(1/864e5);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesHostInfoMethodsDNSDomainEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=GetHostName(ComputerNameDnsDomain);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesHostInfoMethodsDNSFullyQualifiedEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=GetHostName(ComputerNameDnsFullyQualified);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesHostInfoMethodsDNSNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=GetHostName(ComputerNameDnsHostname);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesHostInfoMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=GetHostName(ComputerNameNetBIOS);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesMemoryStatusConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);

   procedure SetDetail(const detailName : String; total, avail : Int64);
   var
      detail : IInfo;
   begin
      detail:=Info.Vars[detailName];
      detail.Member['Total'].ValueAsInteger:=total;
      detail.Member['Available'].ValueAsInteger:=avail;
   end;

var
   ms : TMemoryStatusEx;
begin
   ms:=FGlobalMemory.Value;
   SetDetail('Physical', ms.ullTotalPhys, ms.ullAvailPhys);
   SetDetail('Virtual', ms.ullTotalVirtual, ms.ullAvailVirtual);
   SetDetail('PageFile', ms.ullTotalPageFile, ms.ullAvailPageFile);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesOSVersionInfoMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FOSNameVersion.Name;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesOSVersionInfoMethodsVersionEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsString:=FOSNameVersion.Version;
end;

function TdwsSystemInfoLibModule.dwsSystemInfoFunctionsSystemMillisecondsFastEval(
  const args: TExprBaseListExec): Variant;
begin
   Result := GetSystemMilliseconds;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsReadValueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path, valueName : String;
   binary : RawByteString;
   dataInfo : TRegDataInfo;
begin
   reg := CreateRootedRegistry(Info, STANDARD_RIGHTS_READ or KEY_QUERY_VALUE);
   try
      path := Info.ParamAsString[1];
      valueName := Info.ParamAsString[2];
      if     reg.KeyExists(path)
         and reg.OpenKeyReadOnly(path)
         and reg.GetDataInfo(valueName, dataInfo) then begin

         case dataInfo.RegData of
            rdString, rdExpandString :
               Info.ResultAsString := reg.ReadString(valueName);
            rdInteger :
               Info.ResultAsInteger := reg.ReadInteger(valueName);
            rdBinary : begin
               SetLength(binary, dataInfo.DataSize);
               reg.ReadBinaryData(valueName, PAnsiChar(binary)^, dataInfo.DataSize);
               Info.ResultAsDataString := binary;
            end;
         else
            raise Exception.CreateFmt('Unsupported registry value type (%d)', [Ord(dataInfo.RegData)]);
         end;
         Exit;

      end else begin

         Info.ResultAsVariant := Info.ParamAsVariant[3];

      end;
   finally
      reg.Free;
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsWriteValueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path, valueName : String;
   value : Variant;
begin
   reg := CreateRootedRegistry(Info, KEY_WRITE);
   try
      path := Info.ParamAsString[1];
      if reg.OpenKey(path, True) then begin
         valueName := Info.ParamAsString[2];
         value := Info.ParamAsVariant[3];
         case VariantType(value) of
            varUString : reg.WriteString(valueName, VariantToString(value));
            varInt64, varBoolean : reg.WriteInteger(valueName, VariantToInt64(value));
         else
            reg.WriteString(valueName, VariantToString(value));
         end;
         Info.ResultAsBoolean := True;
      end else Info.ResultAsBoolean := False;
   finally
      reg.Free;
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsDeleteValueEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path, valueName : String;
begin
   reg := CreateRootedRegistry(Info, STANDARD_RIGHTS_WRITE or KEY_SET_VALUE);
   try
      path := Info.ParamAsString[1];
      if reg.KeyExists(path) and reg.OpenKey(path, False) then begin
         valueName := Info.ParamAsString[2];
         Info.ResultAsBoolean := reg.DeleteValue(valueName);
      end else Info.ResultAsBoolean := False;
   finally
      reg.Free;
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsCreateKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path : String;
begin
   reg := CreateRootedRegistry(Info, STANDARD_RIGHTS_WRITE or KEY_CREATE_SUB_KEY);
   try
      path := Info.ParamAsString[1];
      Info.ResultAsBoolean := reg.CreateKey(path);
   finally
      reg.Free;
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsDeleteKeyEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path : String;
begin
   reg := CreateRootedRegistry(Info, STANDARD_RIGHTS_WRITE or KEY_WRITE);
   try
      path := Info.ParamAsString[1];
      Info.ResultAsBoolean := reg.DeleteKey(path);
   finally
      reg.Free;
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsSubKeysEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path : String;
   result : TStringDynArray;
   i : Integer;
   sl : TStringList;
begin
   reg := CreateRootedRegistry(Info, KEY_READ);
   try
      path := Info.ParamAsString[1];
      if reg.KeyExists(path) and reg.OpenKey(path, False) then begin
         sl := TStringList.Create;
         try
            reg.GetKeyNames(sl);
            SetLength(result, sl.Count);
            for i := 0 to sl.Count-1 do
               result[i] := sl[i];
         finally
            sl.Free;
         end;
      end;
      Info.ResultAsStringArray := result;
   finally
      reg.Free;
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemRegistryClassesRegistryMethodsValueNamesEval(
  Info: TProgramInfo; ExtObject: TObject);
var
   reg : TRegistry;
   path : String;
   result : TStringDynArray;
   i : Integer;
   sl : TStringList;
begin
   reg := CreateRootedRegistry(Info, KEY_READ);
   try
      path := Info.ParamAsString[1];
      if reg.KeyExists(path) and reg.OpenKey(path, False) then begin
         sl := TStringList.Create;
         try
            reg.GetValueNames(sl);
            SetLength(result, sl.Count);
            for i := 0 to sl.Count-1 do
               result[i] := sl[i];
         finally
            sl.Free;
         end;
      end;
      Info.ResultAsStringArray := result;
   finally
      reg.Free;
   end;
end;

// GetScript
//
function TdwsSystemInfoLibModule.GetScript : TDelphiWebScript;
begin
   Result := dwsSystemInfo.Script;
end;

// SetScript
//
procedure TdwsSystemInfoLibModule.SetScript(const val : TDelphiWebScript);
begin
   dwsSystemInfo.Script := val;
   dwsSystemRegistry.Script := val;
end;

type
  NET_API_STATUS = DWORD;

  NETSETUP_JOIN_STATUS = (
    NetSetupUnknownStatus,
    NetSetupUnjoined,
    NetSetupWorkgroupName,
    NetSetupDomainName);

  PNETSETUP_JOIN_STATUS = ^NETSETUP_JOIN_STATUS;

  TDomainControllerInfoW = record
    DomainControllerName: LPCWSTR;
    DomainControllerAddress: LPCWSTR;
    DomainControllerAddressType: ULONG;
    DomainGuid: TGUID;
    DomainName: LPCWSTR;
    DnsForestName: LPCWSTR;
    Flags: ULONG;
    DcSiteName: LPCWSTR;
    ClientSiteName: LPCWSTR;
  end;
  PDomainControllerInfoW = ^TDomainControllerInfoW;

const
  NET_API_DLL='netapi32.dll';
  NERR_Success = 0;
  DS_IS_FLAT_NAME = $00010000;
  DS_RETURN_DNS_NAME = $40000000;

function DSGetDCName(ComputerName, DomainName: LPCWSTR; DomainGuid: PGUID; SiteName: LPCWSTR; Flags: ULONG; var DomainControllerInfo: PDomainControllerInfoW):  NET_API_STATUS; stdcall; external NET_API_DLL name 'DsGetDcNameW';
function NetApiBufferFree(Buffer: Pointer): NET_API_STATUS; stdcall; external NET_API_DLL;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesHostInfoMethodsDomainControllerInfoEval(
               Info: TProgramInfo; ExtObject: TObject);
var
   dcInfo : PDomainControllerInfoW;
   emptyDCInfo : TDomainControllerInfoW;
   result : IInfo;
begin
   result := Info.ResultVars;
   if DSGetDCName(nil, nil, nil, nil, 0, dcInfo) <> ERROR_SUCCESS then begin
      dcInfo := @emptyDCInfo;
      FillChar(emptyDCInfo, 0, SizeOf(emptyDCInfo));
   end;
   try
      result.Member['DCName'].ValueAsString := dcInfo.DomainControllerName;
      result.Member['DCAddress'].ValueAsString := dcInfo.DomainControllerAddress;
      result.Member['DCAddressType'].ValueAsInteger := dcInfo.DomainControllerAddressType;
      result.Member['GUID'].ValueAsString := GUIDToString(dcInfo.DomainGuid);
      result.Member['Name'].ValueAsString := dcInfo.DomainName;
      result.Member['ForestName'].ValueAsString := dcInfo.DomainName;
      result.Member['Name'].ValueAsString := dcInfo.DnsForestName;
      result.Member['DCSiteName'].ValueAsString := dcInfo.DcSiteName;
      result.Member['ClientSiteName'].ValueAsString := dcInfo.ClientSiteName;
      result.Member['Flags'].ValueAsInteger := dcInfo.Flags;
   finally
      if dcInfo <> @emptyDCInfo then
         NetApiBufferFree(dcInfo);
   end;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TPerformanceCounter.Create;
   QueryPerformanceCounter(TPerformanceCounter(ExtObject).StartTime);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterMethodsRestartFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   obj : IScriptObj;
   pc : TPerformanceCounter;
begin
   baseExpr.EvalAsSafeScriptObj(args.Exec, obj);
   pc := (obj.ExternalObject as TPerformanceCounter);
   pc.StopTime := 0;
   QueryPerformanceCounter(pc.StartTime);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterMethodsStopFastEvalNoResult(
  baseExpr: TTypedExpr; const args: TExprBaseListExec);
var
   obj : IScriptObj;
   pc : TPerformanceCounter;
begin
   baseExpr.EvalAsSafeScriptObj(args.Exec, obj);
   pc := (obj.ExternalObject as TPerformanceCounter);
   if pc.StopTime = 0 then
      QueryPerformanceCounter(pc.StopTime);
end;

function TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterMethodsElapsedFastEvalFloat(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
var
   obj : IScriptObj;
   pc : TPerformanceCounter;
   t : Int64;
begin
   baseExpr.EvalAsSafeScriptObj(args.Exec, obj);
   pc := (obj.ExternalObject as TPerformanceCounter);
   t := pc.StopTime;
   if t = 0 then
      QueryPerformanceCounter(t);
   t := t - pc.StartTime;
   Result := t * vPerformanceInverseFrequency;
end;

function TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterMethodsRunningFastEvalBoolean(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Boolean;
begin
   Result := ((baseExpr.EvalAsSafeScriptObj(args.Exec).ExternalObject as TPerformanceCounter).StopTime = 0);
end;

function TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterMethodsNowFastEvalFloat(
  baseExpr: TTypedExpr; const args: TExprBaseListExec): Double;
var
   t : Int64;
begin
   QueryPerformanceCounter(t);
   Result := t * vPerformanceInverseFrequency;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesPerformanceCounterCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

end.
