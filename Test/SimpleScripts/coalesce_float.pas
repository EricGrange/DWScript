var f : Float;

PrintLn(f ?? 12.5);
PrintLn(f ?? 12);

f := 45.5;
PrintLn(f ?? 12.5);

function Test(v : Float) : Float;
begin
	PrintLn(v);
	Result:=v;
end;

PrintLn(0.5 ?? Test(14.5));
PrintLn(0.0 ?? Test(25.5));