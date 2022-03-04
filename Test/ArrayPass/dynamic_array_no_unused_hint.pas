procedure SubTest(a : array of integer);
begin
a.Add(1);
end;

procedure Test;
var a : array of integer;
begin
  SubTest(a);
  PrintLn(a[0]);
end;

Test;