{
Demo: Object Orientated Programming (OOP)

}
type 
  AClass = class
    s, t: string;
    procedure P(param: string); virtual;
    function Q: string;
  end;

type
  BClass = class(AClass)
    u, v: string;
    procedure P(param: string); override;
    function Q: string;
  end;

procedure AClass.P;
begin
  PrintLn('AClass.P(' + param + ')');
end;

function AClass.Q: string;
begin
  Result := 'AClass.Q: Static method';
end;

procedure BClass.P;
begin
  PrintLn('BClass.P(' + param + ')');
  inherited P(param);
end;

function BClass.Q: string;
begin
  Result := 'BClass.Q: Static method';
  inherited P('inh');
end;

var o: AClass;

o := BClass.Create;

PrintLn('--- Virtual methods');
o.P('Hello World!');
BClass(o).P('Hello World!');

PrintLn('');

PrintLn('--- Static methods');
PrintLn(o.Q);
PrintLn(BClass(o).Q);

