procedure Test(var d : array [0..0] of Integer);
begin
	PrintLn(d[0]);
	if d[0] > 0 then begin
		d[0] -= 1;
		Test(d);
	end;
		
end;

var d := [5];
Test(d);
