procedure Test;
var 
	i, j : Integer;
begin
	for i:=-10 to 10 do begin
		for j:=-5 to 5 do begin
	
			var k := i*j;
			Print(k);
			Print('/');
			if j<>0 then 
				k:=i div j
			else k:=0;
			Print(k);
			Print(' ');
	
		end;
		PrintLn('');
	end;
end;

procedure Test2;
var 
	i, j : Integer;
begin
	for i:=-10 to 10 do begin
		for j:=-5 to 5 do begin
	
			var k := (i+1)*(j+1);
			Print(k);
			Print('/');
			if j<>-1 then 
				k:=(i+1) div (j+1)
			else k:=0;
			Print(k);
			Print(' ');
	
		end;
		PrintLn('');
	end;
end;

Test;

PrintLn('');

Test2;