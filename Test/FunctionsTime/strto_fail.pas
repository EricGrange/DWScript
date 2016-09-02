try
	StrToDateTime('bug1');
except
	on E: Exception do
		PrintLn(E.Message);
end;

try
	StrToDate('bug2');
except
	on E: Exception do
		PrintLn(E.Message);
end;

try
	StrToTime('bug3');
except
	on E: Exception do
		PrintLn(E.Message);
end;