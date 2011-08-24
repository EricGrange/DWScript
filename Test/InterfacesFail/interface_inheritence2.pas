type
  IBase = interface 
    procedure A;
  end;

type
  IDescendent = interface(IBase) 
    procedure B;
  end;

type
  TImpBase = class(TObject, IBase)
    procedure A; begin PrintLn(ClassName + '.A'); end;
  end;

var Base: IBase = TImpBase.Create;
var Desc: IDescendent;

Desc := Base; // Shouldn't compile <Parent> := <Child>
