procedure Test1;
begin
	while True do begin
		continue;
		PrintLn('a');
	end;
end;

procedure Test2;
begin
	repeat 
		exit;
		PrintLn('a');
	until False;
end;

procedure Test3;
begin
    while True do begin
	   break;
	   PrintLn('a');
	end;
end;

procedure Test4;
begin
	exit;
	PrintLn('a');
	exit;
end;

Test1;
raise Exception.Create;
Test2;