function Test1 : String;
begin
	raise Exception.Create('TEST');
end;

procedure Test2;
begin
	StrToInt(Test1);
end;

Test2;