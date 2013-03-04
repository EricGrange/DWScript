var n := 130;

while n >= 130 do begin
	PrintLn(n);
	n:=2*n;
	n:=n*2;
	if n>High(Integer) then Break;
end;

n := -130;

while n <= -130 do begin
	PrintLn(n);
	n:=2*n;
	n:=n*2;
	if n<Low(Integer) then Break;
end;