type
  IBase = interface 
    procedure A;
  end;
  
type
  IDerived = interface(IBase)
    procedure B;
  end;

type
  TImp = class(TObject, IDerived)
    procedure A; begin PrintLn('A'); end;
    procedure B; begin PrintLn('B'); end;
  end;
  
var Imp: TImp;
var Base: IBase;
var Derived: IDerived;
  
Imp := TImp.Create;
Derived := Imp;
Derived.A;
Derived.B;

if not (Imp implements IBase) then PrintLn('Imp does not implement IBase');

Base := Derived;
Base.A;