type
  IBase = interface
    procedure A;
  end;

type
  IDescendent = interface(IBase)
    procedure B;
  end;

type
  TImpAll = class(TObject, IDescendent, IBase)
    procedure A; begin PrintLn(ClassName + '.A'); end;
    procedure B; begin PrintLn(ClassName + '.B'); end;
  end;

type
  TImpBase = class(TObject, IBase)
    procedure A; begin PrintLn(ClassName + '.A'); end;
  end;

type  
  TImpDescendent = class(TObject, IDescendent)
    procedure A; begin PrintLn(ClassName + '.A'); end;
    procedure B; begin PrintLn(ClassName + '.B'); end;
  end;

procedure Test1;
begin
  PrintLn('Test1');
  var ImpBase: TImpBase = TImpBase.Create;
  var ImpDescendent: TImpDescendent = TImpDescendent.Create;
  var Base: IBase;
  var Desc: IDescendent;

  Base := ImpBase;
  Base.A;
  // Base := ImpDescendent; // Shouldn't compile!

  Desc := ImpDescendent;
  Desc.A;
  Desc.B;
  Base := Desc;
  Base.A;
end;

procedure Test2;
begin
  PrintLn('Test2');
  var Imp: TImpAll;
  var Base: IBase;
  var Desc: IDescendent;

  Imp := TImpAll.Create;
  Base := Imp;
  Desc := Imp;

  Imp.A;
  Imp.B;

  Base.A;

  Desc.A;
  Desc.B;

  // Assign <Parent> := <Child>
  Base := Desc;
  Base.A;
end;

Test1;
Test2;
