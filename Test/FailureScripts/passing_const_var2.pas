var i : Integer;
const ci : Integer = 3;

function VarTest(var v : Integer) : Integer;
begin
   Result:=v;
end;

VarTest(i);
VarTest(5);
VarTest(VarTest(i));
VarTest(ci);

procedure Test(lazy a : Integer);
begin
   VarTest(a);
end;
