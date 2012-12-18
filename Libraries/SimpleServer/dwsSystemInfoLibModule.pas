unit dwsSystemInfoLibModule;

interface

uses
  Windows, SysUtils, Classes, ActiveX, ComObj, dwsComp, dwsExprs;

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
  private
    { Private declarations }
    FOSNameVersion : TOSNameVersion;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

function GetWin32_OSNameVersion : TOSNameVersion;
var
   locator : OleVariant;
   objWMIService : OLEVariant;
   colItems : OLEVariant;
   colItem : OLEVariant;
   oEnum : IEnumvariant;
   iValue : LongWord;
begin
   CoInitialize(nil);
   Result.Name:='?';
   Result.Version:='?';
   try
      locator:=CreateOleObject('WbemScripting.SWbemLocator');
      objWMIService:=locator.ConnectServer('localhost', 'root\CIMV2', '', '');
      colItems:=objWMIService.ExecQuery('SELECT * FROM Win32_OperatingSystem','WQL',0);
      oEnum:=IUnknown(colItems._NewEnum) as IEnumVariant;
      if oEnum.Next(1, colItem, iValue)=0 then begin
         Result.Name:=colItem.Caption;
         Result.Version:=colItem.Version;
      end;
   except
      on E : Exception do
         Result.Name:=E.Message;
   end;
   CoUninitialize;
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
   ms.dwLength:=SizeOf(ms);
   GlobalMemoryStatusEx(ms);
   SetDetail('Physical', ms.ullTotalPhys, ms.ullAvailPhys);
   SetDetail('Virtual', ms.ullTotalVirtual, ms.ullAvailVirtual);
   SetDetail('PageFile', ms.ullTotalPageFile, ms.ullAvailPageFile);
end;

procedure TdwsSystemInfoLibModule.dwsSystemInfoClassesOSVersionInfoMethodsNameEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
   if FOSNameVersion.Name='' then
      FOSNameVersion:=GetWin32_OSNameVersion;
   Info.ResultAsString:=FOSNameVersion.Name;
end;

end.
