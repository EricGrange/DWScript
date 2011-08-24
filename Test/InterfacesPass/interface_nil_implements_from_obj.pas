type
  IMyInterface = interface 
    procedure A;
  end;

var Obj: TObject;

// References are nil
Obj := nil;

// <nil-Object> is <Interface> = False
if not (Obj implements IMyInterface) then PrintLn('Ok');