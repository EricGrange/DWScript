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

procedure Test5;
begin
	if Random > 0.5 then
		exit
	else exit;
	PrintLn('a');
end;

procedure Test6;
begin
	if True then
		exit;
	PrintLn('a');
end;

procedure Test7;
begin
	repeat 
		exit;
	until False;
	PrintLn('a');
end;

Test1;
raise Exception.Create('oops');
Test2;