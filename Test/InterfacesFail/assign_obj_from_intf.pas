type
  IMyInterface = interface
  end;

var Intf: IMyInterface;
var Obj: TObject;

Obj := Intf; // Should not work without cast: <TObject> := <IInterface>