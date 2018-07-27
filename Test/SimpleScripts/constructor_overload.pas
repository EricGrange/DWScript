type
  TBase = class
    constructor Create; virtual; abstract;
  end;

type
  TDerived = class(TBase)
    constructor Create(Value: boolean); overload; begin PrintLn(Value); end;
    constructor Create; overload; override; begin PrintLn(1); end;
  end;

type
  TProblem = class(TDerived)
    constructor Create; override; begin PrintLn(2); end;
  end;

new TDerived(True);
new TDerived;
new TProblem;

TDerived.Create(False);
TDerived.Create;
TProblem.Create;
  