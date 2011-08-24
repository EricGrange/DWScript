type
  IMyInterface = interface 
    procedure A;
  end;

var Intf: IMyInterface;
var Impl: TObject;

// Reference is nil
Impl := nil;

// <nil-Object> as <Interface> = nil, no AV/Exception
Intf := Impl as IMyInterface;
if Intf = nil then PrintLn('Ok');