type
  IBase = interface 
    procedure A;
  end;

type
  IDescendent = interface(IBase)
    procedure B;
  end;

type
  TImpDescendent = class(TObject, IDescendent)
    procedure A; begin PrintLn(ClassName + '.A'); end;
    procedure B; begin PrintLn(ClassName + '.B'); end;
  end;

var ImpDescendent: TImpDescendent = TImpDescendent.Create;
var Base: IBase;

Base := ImpDescendent; // Shouldn't compile because TImpDescent doesn't implement IBase although IDescendent is a child of IBase
