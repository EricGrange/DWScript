var a : array of Float;

a.SetLength(1000);

for var i:=0 to a.High do
	a[i] := i-500;
	
var s, s2, mi, ma : Float;

for var i:=0 to a.High do begin
	s += a[i];
	s2 := s + a[i]*a[i];
	mi := Min(mi, a[i]);
	ma := Max(ma, a[i]);
end;

PrintLn(s);
PrintLn(s2);
PrintLn(mi);
PrintLn(ma);