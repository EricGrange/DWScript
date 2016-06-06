for var i := 1 to 3 do 
	PrintLn(PadLeft('aa', i)+','+PadRight('bb', i)+';');
for var i := 1 to 3 do 
	PrintLn(PadLeft('aa', i, 'z')+','+PadRight('bb', i, 'z')+';');

var s := '1234567';
PrintLn(s.PadLeft(8, '0'));
PrintLn(s.PadRight(8, '8'));
