type
  IMyInterface = interface 
    procedure B;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
  end;
