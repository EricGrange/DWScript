with a := 1 do
	PrintLn(a);

with b := "a", d := 3 do begin
	with c := b + "c" do
		PrintLn(c);
	PrintLn(d);
	with c : Integer = d + 1 do
		PrintLn(c);
end;