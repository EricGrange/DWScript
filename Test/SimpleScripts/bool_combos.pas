var i, j : Integer;

for i:=0 to 1 do begin
   for j:=0 to 1 do begin
		PrintLn('>> i='+IntToStr(i)+', j='+IntToStr(j));
		if (i=0) and (j=0) then PrintLn('=0 and');
		if (i=0) or (j=0) then PrintLn('=0 or');
		if (i=0) xor (j=0) then PrintLn('=0 xor');
		if (i<>0) and (j<>0) then PrintLn('<>0 and');
		if (i<>0) or (j<>0) then PrintLn('<>0 or');
		if (i<>0) xor (j<>0) then PrintLn('<>0 xor');
		if not((i=0) and (j=0)) then PrintLn('not =0 and');
		if not((i=0) or (j=0)) then PrintLn('not =0 or');
		if not((i=0) xor (j=0)) then PrintLn('not =0 xor');
		if not((i<>0) and (j<>0)) then PrintLn('not <>0 and');
		if not((i<>0) or (j<>0)) then PrintLn('not <>0 or');
		if not((i<>0) xor (j<>0)) then PrintLn('not <>0 xor');
	end;
end;