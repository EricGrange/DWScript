
for var i := 1 to 3 do begin
	case i of
		2 : break;
	end;
	PrintLn(i);
end;

for var i := 1 to 3 do begin
	case i of
		2 : begin
			PrintLn('two');
			break;
		end;
	end;
	PrintLn(i);
end;

for var i := 1 to 3 do begin
	case i of
		1 : PrintLn('one');
	else
		break;
	end;
	PrintLn(i);
end;

for var i := 1 to 3 do begin
	case i of
		1 : PrintLn('one');
	else
		PrintLn('else');
		if i=3 then 
			break;
	end;
	PrintLn(i);
end;
