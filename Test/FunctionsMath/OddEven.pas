if Odd(2) then PrintLn('2 not odd');
if not Odd(1) then PrintLn('1 is odd');

for var i := -5 to 5 do begin
	if Odd(i) <> ((i and 1) <> 0) then PrintLn(i.ToString + ' bug');
	if Odd(i) then
		if (i and 1) = 0 then 
			PrintLn(i.ToString + ' rebug');
end;
