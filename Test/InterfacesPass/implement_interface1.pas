type
  IMyInterface = interface 
    procedure A;
  end;
  
type
  TMyImplemenation = class(TObject, IMyInterface)
    procedure A; begin PrintLn('A'); end;
  end;
