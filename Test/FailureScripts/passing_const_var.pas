var i : Integer;
const ci : Integer = 3;

Inc(i);
Inc(5);
Inc(Inc(i));
Inc(ci);

procedure Test(lazy a : Integer);
begin
   Inc(a);
end;
