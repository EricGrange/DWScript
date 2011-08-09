function CreateElement : TObject;
begin
   Result:=new TObject;
end;

Type
TComponent = Class(TObject)
Private
  FObjRef:  TObject;
Public
  Property    Handle:TObject read FObjRef;
  Constructor Create(AOwner:TComponent);virtual;
End;

Constructor TComponent.Create(AOwner:TComponent);
Begin
  FObjRef:=createElement;
end;

Type
TCustomControl = Class(TComponent)
public
  Constructor Create(AOwner:TComponent);override;
End;

Constructor TCustomControl.Create(AOwner:TComponent);
Begin
  if assigned(Handle) then
  println('We have a handle') else
  println('We dont have a handle');
end;

Type
TCustomControl2 = Class(TComponent)
public
  Constructor Create(AOwner:TComponent);override;
End;

Constructor TCustomControl2.Create(AOwner:TComponent);
Begin
  inherited Create(AOwner);
  if assigned(Handle) then
  println('We have a handle') else
  println('We dont have a handle');
end;


var mObj: TComponent;
mObj:=TCustomControl.Create(NIL);
mObj:=TCustomControl2.Create(NIL);