function Test1(a : Integer) : String;
begin
	Result:=(a+a).ToString
end;

function Test2(a : Integer) : String;
begin
	Result:=Test1(a)+Test1(a+1);
end;

PrintLn(CallFunc(Test1, 1));
PrintLn(CallFunc(test2, 2));