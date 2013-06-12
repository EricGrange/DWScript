function test : String;
begin
	type a = class
		a1 : String;
		a2 : Integer;
	end;

	var toto:=a.Create;
	toto.a1:='hello';
	toto.a2:=123;
	Result:=toto.ClassName;
	PrintLn(toto.a1);
	PrintLn(toto.a2);
end;
PrintLn(test);