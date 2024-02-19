type TEnum = (one, two);
type TSet = set of TEnum;

procedure PrintSet(a : TSet);
begin
	if one in a then Print(1);
	if two in a then Print(2);
	PrintLn(';');
end;

PrintSet([one]);
PrintSet([]);
for var i := 0 to 3 do begin
	PrintLn('loop ' + i.ToString);
	PrintSet(if Odd(i) then [one]);
	PrintSet(if not Odd(i) then [two] else []);
end;
PrintSet(if True then [one, two]);