var i, j : Integer;
var result, remainder : Integer;

for i:=-20 to 20 do begin
   for j:=-3 to 3 do begin
		if j=0 then continue;
		
		DivMod(i, j, result, remainder);
		if (result<>i div j) or (remainder<>i mod j) then 
			PrintLn('Failed on '+IntToStr(i)+', '+IntToStr(j));
	end;
end;

DivMod(123456789123456789, 12345678912, result, remainder);
PrintLn(result);
PrintLn(remainder);