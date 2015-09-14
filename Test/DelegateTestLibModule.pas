unit DelegateTestLibModule;

interface

uses
  SysUtils, Classes,
  dwsComp,
  dwsExprs,
  dwsCompiler,
  dwsSymbols,
  dwsFunctions,
  dwsUtils,
  dwsErrors;

type
  TDelegateTestLib = class(TDataModule)
    dwsUnitDelegateTest: TdwsUnit;
    procedure dwsUnitDelegateTestClassesTFooMethodsSetOnTestEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitDelegateTestClassesTFooMethodsTestEval(Info: TProgramInfo; ExtObject: TObject);
    procedure dwsUnitDelegateTestClassesTFooConstructorsCreateEval(Info: TProgramInfo; var ExtObject: TObject);
    procedure dwsUnitDelegateTestFunctionsCallMeEval(info: TProgramInfo);
    procedure dwsUnitDelegateTestFunctionsCallFuncEval(info: TProgramInfo);
    procedure dwsUnitDelegateTestFunctionsCallEventEval(info: TProgramInfo);
    procedure dwsUnitDelegateTestClassesTFooCleanUp(ExternalObject: TObject);
  private
    FScript: TDelphiWebScript;
    procedure SetScript(const Value: TDelphiWebScript);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Script: TDelphiWebScript read FScript write SetScript;
  end;

type
  TFooItem = class
  private
    FOnTest: TNotifyEvent;
  public
    procedure Test;
    property OnTest: TNotifyEvent read FOnTest write FOnTest;
  end;

type
  IScriptObjectInfo = interface
    function Info: IInfo;
  end;

type
  TWrapper = class
  private
    FItem: TObject;
    FScriptObj: IScriptObj;
  public
    constructor Create(AItem: TObject; const AScriptObj: IScriptObj = nil);
    destructor Destroy; override;

    function AcquireInfo: IScriptObjectInfo;
    function CheckScriptObj: boolean;

    property Item: TObject read FItem;
    property ScriptObj: IScriptObj read FScriptObj write FScriptObj;

    procedure OnTestHandler(Sender: TObject);
  end;

implementation

{$R *.dfm}

uses
  Variants,
  dwsDataContext,
  dwsInfo;


{ TDataModuleDelegateTest }

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooConstructorsCreateEval(Info: TProgramInfo; var ExtObject: TObject);
var
  Item: TFooItem;
  Wrapper: TWrapper;
begin
  ASSERT(ExtObject = nil);
  Item := TFooItem.Create;

  Wrapper := TWrapper.Create(Item, Info.ScriptObj);

  ExtObject := Wrapper;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooMethodsSetOnTestEval(Info: TProgramInfo; ExtObject: TObject);
var
   v : Variant;
   e : TNotifyEvent;
begin
   v := Info.Params[0].Value;
   // Attach wrapper's event handler to the wrapped object's event
   if VarIsNull(v) then begin
      e := nil;
   end else begin
      Info.Vars['FOnTest'].Value := v;
      e := TWrapper(ExtObject).OnTestHandler;
   end;
   ((ExtObject as TWrapper).Item as TFooItem).OnTest := e;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooMethodsTestEval(Info: TProgramInfo; ExtObject: TObject);
var
   Wrapper: TWrapper;
   Item: TFooItem;
begin
   Wrapper := (ExtObject as TWrapper);

   if not Wrapper.CheckScriptObj then Exit;

   Item := (Wrapper.Item as TFooItem);
   Item.Test;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestFunctionsCallEventEval(
  info: TProgramInfo);
var
   f : IInfo;
   v : Variant;
begin
   f:=info.Params[0];
   v:=Info.ParamAsVariant[1];
   f.Call([v]);
end;

procedure TDelegateTestLib.dwsUnitDelegateTestFunctionsCallFuncEval(
  info: TProgramInfo);
var
   f : IInfo;
   v : Int64;
begin
   f:=info.Params[0];
   v:=Info.ParamAsInteger[1];
   Info.ResultAsString:=f.Call([v]).Value;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestFunctionsCallMeEval(
  info: TProgramInfo);
var
   f : IInfo;
   v : Int64;
begin
   f:=info.RootInfo(Info.ParamAsString[0]);
   v:=Info.ParamAsInteger[1];
   Info.ResultAsString:=f.Call([v]).Value;
end;

procedure TDelegateTestLib.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Script) then
    SetScript(nil)
end;

procedure TDelegateTestLib.SetScript(const Value: TDelphiWebScript);
var
  x: Integer;
begin
  if Assigned(FScript) then
    FScript.RemoveFreeNotification(Self);
  if Assigned(Value) then
    Value.FreeNotification(Self);

  FScript := Value;
  for x := 0 to ComponentCount - 1 do
    if Components[x] is TdwsUnit then
      TdwsUnit(Components[x]).Script := Value;
end;

{ TFooItem }

procedure TFooItem.Test;
begin
   if Assigned(FOnTest) then
      FOnTest(Self);
end;

{ TScriptObjectInfo }

type
  TScriptObjectInfo = class(TInterfacedObject, IScriptObjectInfo)
  private
    FData: TData;
    FInfo: IInfo;
  protected
    function Info: IInfo;
  public
    constructor Create(const AScriptObject: IScriptObj);
    destructor Destroy; override;
  end;

constructor TScriptObjectInfo.Create(const AScriptObject: IScriptObj);
var
  Execution: IdwsProgramExecution;
begin
  inherited Create;

  Execution := TScriptObjInstance(AScriptObject).ExecutionContext;

  SetLength(FData, 1);
  FData[0] := AScriptObject;
  CreateInfoOnSymbol(FInfo, Execution.Info, AScriptObject.ClassSym, FData, 0);
end;

destructor TScriptObjectInfo.Destroy;
begin
  FInfo := nil;
  inherited;
end;

function TScriptObjectInfo.Info: IInfo;
begin
  Result := FInfo;
end;

{ TWrapper }

function TWrapper.AcquireInfo: IScriptObjectInfo;
begin
  Result := TScriptObjectInfo.Create(FScriptObj);
end;

function TWrapper.CheckScriptObj: boolean;
begin
  ASSERT(FScriptObj <> nil);
  Result := (not FScriptObj.Destroyed) and (FScriptObj.ClassSym <> nil);
end;

constructor TWrapper.Create(AItem: TObject; const AScriptObj: IScriptObj);
begin
  inherited Create;
  FItem := AItem;
  FScriptObj := AScriptObj;
end;

destructor TWrapper.Destroy;
begin
  //
  inherited;
end;

procedure TWrapper.OnTestHandler(Sender: TObject);
var
   Item: TFooItem;
   ScriptObjectInfo: IScriptObjectInfo;
   Delegate: IInfo;
begin
   Item := (FItem as TFooItem);

   if (not CheckScriptObj) then begin
      //OutputDebugString('Script Object has been destroyed');
      Item.OnTest := nil;
      exit;
   end;

   ScriptObjectInfo := AcquireInfo;
   Delegate := ScriptObjectInfo.Info.Member['FOnTest'];

   if (Delegate <> nil) and (Delegate.ScriptObj <> nil) then begin
      try

         Delegate.Call([ScriptObj]);

      except
         Item.OnTest := nil;
         raise;
      end;
   end else begin
      Item.OnTest := nil;
   end;
end;

end.
