procedure Test1(a : Integer);
begin
	case a of
		1 : exit;
	else
		exit;
	end;
	PrintLn(a);
end;

function Test2(a : Integer) : Integer;
begin
	case a of
		1..3 : raise Exception.Create('boo');
		4, 5 : exit;
	else
		exit a;
	end;
	PrintLn(a);
end;

Test1(1);
Test2(2);