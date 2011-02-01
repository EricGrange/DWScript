{
Demonstrates that OOP really works in DWSII!
}

type 
  TClassA = class
  protected
    str: string;
  public
    constructor Create; virtual;
    class procedure Test; virtual;

	property strProp : String read str;
  end;

type
  TClassB = class(TClassA)
  public
    constructor Create; override;
    class procedure Test; reintroduce;
  end;

constructor TClassA.Create;
begin
  str := 'A';
end;

class procedure TClassA.Test;
begin
  PrintLn('TestA');
end;

constructor TClassB.Create;
begin
  str := 'B';
end; 

class procedure TClassB.Test;
begin
  PrintLn('TestB');
end;

var
  cls: class of TClassA;
var
  o: TClassA;

cls := TClassB;
o := cls.Create;

PrintLn(o.strProp);
o.test;  // call static method
TClassB(o).test;
