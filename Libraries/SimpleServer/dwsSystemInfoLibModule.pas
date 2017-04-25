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
  Windows, SysUtils, Classes, Registry, PsAPI,
  dwsExprList, dwsDataContext, dwsComp, dwsExprs, dwsUtils, dwsCPUUsage,
  dwsXPlatform, dwsInfo;

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

implementation

{$R *.dfm}

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

// Adapted from Ian Boyd code published in
// http://stackoverflow.com/questions/10854958/how-to-get-version-of-running-executable
function GetModuleVersion(instance : THandle; var major, minor, release, build : Integer) : Boolean;
var
   fileInformation : PVSFIXEDFILEINFO;
   verlen : Cardinal;
   rs : TResourceStream;
   m : TMemoryStream;
   resource : HRSRC;
begin
   Result:=False;

   // Workaround bug in Delphi if resource doesn't exist
   resource:=FindResource(instance, PChar(1), RT_VERSION);
   if resource=0 then Exit;

   m:=TMemoryStream.Create;
   try
      rs:=TResourceStream.CreateFromID(instance, 1, RT_VERSION);
      try
         m.CopyFrom(rs, rs.Size);
      finally
         rs.Free;
      end;

      m.Position:=0;
      if VerQueryValue(m.Memory, '\', Pointer(fileInformation), verlen) then begin
         major := fileInformation.dwFileVersionMS shr 16;
         minor := fileInformation.dwFileVersionMS and $FFFF;
         release := fileInformation.dwFileVersionLS shr 16;
         build := fileInformation.dwFileVersionLS and $FFFF;
         Result:=True;
      end;
   finally
      m.Free;
   end;
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
   rootKey := Info.ParamAsInteger[0];
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

procedure TdwsSystemInfoLibModule.DataModuleCreate(Sender: TObject);
begin
   // limit query rate to 10 Hz
   FGlobalMemory:=TThreadCached<TMemoryStatusEx>.Create(GetGlobalMemory, 100);

   GetWin32_OSNameVersion(FOSNameVersion);
   SystemCPU.Track;
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
var
   major, minor, release, build : Integer;
begin
   if GetModuleVersion(HInstance, major, minor, release, build) then
      Info.ResultAsString:=Format('%d.%d.%d.%d', [major, minor, release, build])
   else Info.ResultAsString:='?.?.?.?';
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

end.
