type TEnum = (one, two);
type TSet = set of TEnum;

procedure PrintSet(s : TSet);
begin
	if one in s then Print('one,');
	if two in s then Print('two');
	PrintLn('');
end;

PrintSet(TSet(0));
PrintSet(TSet(1));
PrintSet(TSet(2));
PrintSet(TSet(3));

for var i := 0 to 3 do 
	PrintSet(TSet(i));