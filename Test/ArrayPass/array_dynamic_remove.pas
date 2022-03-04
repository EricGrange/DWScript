var a : array of Float;

procedure Dump;
begin
	Print(Length(a));
	Print(' : ');
	for var f in a do
		Print(f);
	PrintLn('');
end;

a.Remove(0.5);

Dump;

a.Add(1.5);

a.Remove(0.5);

Dump;

a.Remove(1.5);

Dump;

a.Add(1.0, 2.0);

a.Remove(1.0, 1);

Dump;

a.Remove(1.0, 0);

Dump;
