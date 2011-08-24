type
  IMyInterface = interface 
  end;
type
  IOtherInterface = interface 
  end;
type  
  TImp = class(TObject, IMyInterface)
  end;

var Intf: IMyInterface;
var OtherIntf: IOtherInterface;
var Obj: TObject;

Obj := TImp.Create;
Intf := Obj;
Intf := TImp.Create;
OtherIntf := Intf; // Interfaces not compatible
