unit dwsSystemInfoLibModule;

interface

uses
  Windows, SysUtils, Classes, Registry,
  dwsComp, dwsExprs, dwsUtils, dwsCPUUsage;

type
   TOSNameVersion = record
      Name : String;
      Version : String;
   end;

  TdwsSystemInfoLibModule = class(TDataModule)
    dwsSystemInfo: TdwsUnit;
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
  private
    { Private declarations }
    FOSNameVersion : TOSNameVersion;
    FGlobalMemory : TThreadCached<TMemoryStatusEx>;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

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

function GetGlobalMemory(var ms : TMemoryStatusEx) : TSimpleCallbackStatus;
begin
   ms.dwLength:=SizeOf(ms);
   GlobalMemoryStatusEx(ms);
   Result:=csContinue;
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

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsProcessUsageEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.ProcessUser+SystemCPU.ProcessKernel;
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesCPUInfoMethodsSystemUsageEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   Info.ResultAsFloat:=SystemCPU.Usage;
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

end.
