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

implementation

{$R *.dfm}

uses
  Variants,
  dwsDataContext,
  dwsInfo;

type

   TNotifyScriptEvent = procedure (Info : TProgramInfo; Sender : TObject) of object;

   TFooItem = class
      private
         FOnTest : TNotifyScriptEvent;

      public
         procedure Test(Info : TProgramInfo);
         property OnTest: TNotifyScriptEvent read FOnTest write FOnTest;
   end;

   TWrapper = class
      private
         FItem : TObject;
         FScriptObj : IScriptObj;

      public
         constructor Create(AItem: TObject; const AScriptObj: IScriptObj = nil);
         destructor Destroy; override;

         function CheckScriptObj: boolean;

         property Item: TObject read FItem;
         property ScriptObj: IScriptObj read FScriptObj write FScriptObj;

         procedure OnTestHandler(Info : TProgramInfo; Sender: TObject);
   end;


{ TDataModuleDelegateTest }

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooConstructorsCreateEval(Info: TProgramInfo; var ExtObject: TObject);
begin
   ExtObject := TFooItem.Create;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooCleanUp(
  ExternalObject: TObject);
begin
   ExternalObject.Free;
end;

procedure TDelegateTestLib.dwsUnitDelegateTestClassesTFooMethodsSetOnTestEval(Info: TProgramInfo; ExtObject: TObject);
var
   v : Variant;
   e : TNotifyScriptEvent;
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
   Item.Test(Info);
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

procedure TFooItem.Test(Info : TProgramInfo);
begin
   if Assigned(FOnTest) then
      FOnTest(Info, Self);
end;

{ TScriptObjectInfo }


{ TWrapper }

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
  inherited;
end;

procedure TWrapper.OnTestHandler(Info : TProgramInfo; Sender: TObject);
var
   Item: TFooItem;
   Delegate: IInfo;
begin
   Item := (FItem as TFooItem);

   if (not CheckScriptObj) then begin
      //OutputDebugString('Script Object has been destroyed');
      Item.OnTest := nil;
      exit;
   end;

   Delegate := Info.Vars['Self'].Member['FOnTest'];

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
