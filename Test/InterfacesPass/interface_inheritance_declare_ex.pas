type
  IBase = interface 
    procedure A;
  end;
  
type
  IDerived = interface(IBase)
    procedure B;
  end;
