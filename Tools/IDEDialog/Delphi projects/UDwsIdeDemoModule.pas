unit UDwsIdeDemoModule;

interface

uses
  System.SysUtils, System.Classes, System.Variants, dwsExprs,
  dwsComp, dwsFunctions, dwsVCLGUIFunctions;

type
  TBaseObj = class(TObject)
  private
    FScriptObj: Variant;
  end;

  TSubObj = class(TBaseObj)
  public
    function GetOne: Integer;
  end;

  TDemoUnitObj = class(TBaseObj)
    constructor Create;
    destructor  Destroy; override;
  private
    FSubObj: TSubObj;
  public
    function GetOne: Integer;
    function GetSubObj : TSubObj;
  end;

  TDwsIdeDemoModule = class(TDataModule)
    DelphiWebScript: TDelphiWebScript;
    DemoUnit: TdwsUnit;
    function DelphiWebScriptNeedUnit(const unitName: string;
      var unitSource: string): IdwsUnit;
    procedure dwsUnit1FunctionsMyUnitRecEval(info: TProgramInfo);
    procedure DemoUnitClassesTDemoUnitObjMethodsGetOneEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure DemoUnitInstancesDemoUnitObjInstantiate(info: TProgramInfo;
      var ExtObject: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DemoUnitClassesTSubObj1MethodsGetOneEval(Info: TProgramInfo;
      ExtObject: TObject);
    procedure DemoUnitClassesTDemoUnitObjMethodsGetSubObj1Eval(
      Info: TProgramInfo; ExtObject: TObject);
    procedure DemoUnitClassesTDemoUnitObjConstructorsCreateEval(
      Info: TProgramInfo; var ExtObject: TObject);
    procedure DemoUnitClassesTSubObj1ConstructorsCreateEval(Info: TProgramInfo;
      var ExtObject: TObject);
    procedure DemoUnitClassesTDemoUnitObjCleanUp(ExternalObject: TObject);
    procedure DemoUnitClassesTSubObj1CleanUp(ExternalObject: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    FDemoUnitObj : TDemoUnitObj;
  end;

var
  DwsIdeDemoModule: TDwsIdeDemoModule;

implementation

uses
  SynHighlighterDWS,
  dwsXPlatform,
  UDwsIdeDefs,
  UDwsIdeForm;

resourcestring
  RStrUnitFileNameNotFound = 'Unit file name not found "%s"';


{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TDwsIdeDemoModule.DataModuleCreate(Sender: TObject);
var
  DwsIdeOptions: TDwsIdeOptions;
begin
  if System.SysUtils.Win32MajorVersion >= 6 then // Vista or later...
    DwsIdeOptions := IdeOptions_VistaOrLater
  else
    DwsIdeOptions := IdeOptions_Legacy;

  DwsIdeOptions.ScriptFolder := '..\DWS Script Files';

  DwsIDE_ShowModal(DelphiWebScript, DwsIdeOptions);
end;

function TDwsIdeDemoModule.DelphiWebScriptNeedUnit(const unitName: string;
  var unitSource: string): IdwsUnit;
var
  sFileName : TFileName;
  I : Integer;
begin
  for I := 0 to DelphiWebScript.Config.ScriptPaths.Count - 1 do
  begin
    sFileName := DelphiWebScript.Config.ScriptPaths[I] + '\' + UnitName + '.pas';
    if FileExists(sFileName) then
    begin
      UnitSource := LoadTextFromFile(sFileName);
      Exit;
    end;
  end;

  raise Exception.CreateFmt(RStrUnitFileNameNotFound, [unitName]);
end;




procedure TDwsIdeDemoModule.DemoUnitClassesTDemoUnitObjCleanUp(
  ExternalObject: TObject);
begin
  if ExternalObject <> FDemoUnitObj then
    FreeAndNil( ExternalObject );
end;

procedure TDwsIdeDemoModule.DemoUnitClassesTDemoUnitObjConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TDemoUnitObj.Create;
end;

procedure TDwsIdeDemoModule.DemoUnitClassesTDemoUnitObjMethodsGetOneEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := (ExtObject as TDemoUnitObj).GetOne;
end;


procedure TDwsIdeDemoModule.DemoUnitClassesTDemoUnitObjMethodsGetSubObj1Eval(
  Info: TProgramInfo; ExtObject: TObject);
var
  DemoUnitObj: TDemoUnitObj;
  SubObj: TSubObj;
begin
  DemoUnitObj := ExtObject as TDemoUnitObj;
  SubObj := DemoUnitObj.GetSubObj;

  if VarIsEmpty(SubObj.FScriptObj) then
    SubObj.FScriptObj := Info.Vars[SubObj.ClassName].GetConstructor('Create',
      SubObj).Call.Value;

  Info.ResultAsVariant := SubObj.FScriptObj;
end;

procedure TDwsIdeDemoModule.DemoUnitClassesTSubObj1CleanUp(
  ExternalObject: TObject);
begin
  FreeAndNil( ExternalObject );
end;

procedure TDwsIdeDemoModule.DemoUnitClassesTSubObj1ConstructorsCreateEval(
  Info: TProgramInfo; var ExtObject: TObject);
begin
  ExtObject := TSubObj.Create;
end;

procedure TDwsIdeDemoModule.DemoUnitClassesTSubObj1MethodsGetOneEval(
  Info: TProgramInfo; ExtObject: TObject);
begin
  Info.ResultAsInteger := (ExtObject as TSubObj).GetOne;
end;

procedure TDwsIdeDemoModule.DemoUnitInstancesDemoUnitObjInstantiate(
  info: TProgramInfo; var ExtObject: TObject);
begin
  If not Assigned( FDemoUnitObj ) then
   FDemoUnitObj := TDemoUnitObj.Create;

  ExtObject := FDemoUnitObj;
end;

procedure TDwsIdeDemoModule.dwsUnit1FunctionsMyUnitRecEval(info: TProgramInfo);
begin
  Info.Vars['Result'].Member['One'].Value := 1;
  Info.Vars['Result'].Member['Two'].Value := 2;
end;

procedure TDwsIdeDemoModule.FormDestroy(Sender: TObject);
begin
  FDemoUnitObj.Free;
end;

{ TDemoUnitObj }

constructor TDemoUnitObj.Create;
begin
  inherited;
  FSubObj := TSubObj.Create;
end;

destructor TDemoUnitObj.Destroy;
begin
  FSubObj.Free;
  inherited;
end;

function TDemoUnitObj.GetOne: Integer;
begin
  Result := 1;
end;

function TDemoUnitObj.GetSubObj: TSubObj;
begin
  Result := FSubObj;
end;


{ TSubObj }

function TSubObj.GetOne: Integer;
begin
  Result := 1;
end;

end.
