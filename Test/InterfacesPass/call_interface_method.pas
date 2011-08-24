type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplementationA = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
  end;

type
  TMyImplementationABC = class(TObject, IMyInterface)
    procedure A; begin PrintLn('ABC'); end;
  end;
  
var implem := TMyImplementationA.Create;

var intfRef : IMyInterface;

intfRef := implem as IMyInterface;

intfRef.A;

intfRef := new TMyImplementationABC;

intfRef.A;