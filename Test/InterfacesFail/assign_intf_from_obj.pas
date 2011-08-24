type
  IMyInterface = interface 
  end;
type  
  TImp = class
  end;

var Intf: IMyInterface;
var Obj: TImp;

Obj := TImp.Create;
Intf := Obj; // Should not work without cast: <Interface> := <Object> (Object doesn't implement IMyInterface)
