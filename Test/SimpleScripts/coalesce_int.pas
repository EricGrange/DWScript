var i : Integer;

PrintLn(i ?? 123);

i := 456;
PrintLn(i ?? 123);

function Test(v : Integer) : Integer;
begin
	PrintLn(v);
	Result:=v;
end;

PrintLn(1 ?? Test(147));
PrintLn(0 ?? Test(258));