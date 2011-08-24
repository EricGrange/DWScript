type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplementation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
	 procedure B; begin PrintLn('B'); end;
  end;

var IntfRef: IMyInterface;
var Imp: TMyImplementation;
  
IntfRef := TMyImplementation.Create;
IntfRef.A;
Imp := IntfRef as TMyImplementation;
Imp.B;

type
   TDummyClass = class end;
try
   var d := IntfRef as TDummyClass;
except
   on E: Exception do PrintLn(E.Message);
end;