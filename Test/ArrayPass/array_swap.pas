type TElem = array [0..1] of Integer;

var a : array of TElem;

a.SetLength(2);

a[0, 0]:=1;
a[0, 1]:=2;
a[1, 0]:=3;
a[1, 1]:=4;

Print(a[0, 0]);
PrintLn(a[0, 1]);
Print(a[1, 0]);
PrintLn(a[1, 1]);

a.Swap(0, 1);

for var e in a do begin
	Print(e[0]);
	PrintLn(e[1]);
end;