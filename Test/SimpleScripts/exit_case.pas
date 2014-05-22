procedure Test(s : String);
begin
	case s of
		't' : begin
			PrintLn('here');
			exit;
		end;
		'r' : ;
	else
		PrintLn('there');
		exit;
	end;
	PrintLn('done');
end;

Test('a');
Test('r');
Test('t');