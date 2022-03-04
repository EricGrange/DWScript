

type TData = array [0..1] of Integer;

procedure PrintData(const a : TData);
begin
	Print(a[0]);
	PrintLn(a[1]);
end;

procedure Test(var a : TData);
begin
	if a[1] = 9 then begin
		a[0] := a[0] + 1;
		a[1] := 0;
	end else a[1] := a[1] + 1;
end;

procedure Test2(var a : TData);
begin
	Test(a);
	Test(a);
end;

var a : TData;
a[0] := 1;
a[1] := 7;

for var i := 1 to 5 do begin
	PrintData(a);
	Test(a);
end;

a[1] := 7;

for var i := 1 to 5 do begin
	Test2(a);
	PrintData(a);
end;
