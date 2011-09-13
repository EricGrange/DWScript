Var i : IInterface;

If not Assigned(i) Then PrintLn('ok1');

type TMY = class(IInterface)
  end;

i:=TMY.Create;

If Assigned(i) Then PrintLn('ok2');