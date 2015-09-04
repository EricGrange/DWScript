procedure Test(var err : String);
begin
	procedure AddError(msg : String);
	begin
		PrintLn('1'+err);
		err:=err+msg+'!';
	end;

	PrintLn('2'+err);
	AddError('test');
end;

var err:='';
Test(err);
PrintLn(err);

err:='-';
Test(err);
PrintLn(err);