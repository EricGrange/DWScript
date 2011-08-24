type
  IMyInterface = interface
    procedure A;
  end;

var Intf1, Intf2: IInterface;

// References are nil
Intf1 := nil;

// <nil-Interface> as <Interface> = nil, no AV/Exception
Intf2 := Intf1 as IMyInterface;
if Intf2 = nil then PrintLn('Ok');
