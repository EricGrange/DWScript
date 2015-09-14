function Test1(a : Integer) : String;
begin
	Result:=(a+a).ToString
end;

function Test2(a : Integer) : String;
begin
	Result:=Test1(a)+Test1(a+1);
end;

PrintLn(CallMe('Test1', 1));
PrintLn(CallMe('Test2', 2));