type 
	TTest = class
		function Triple(a : Integer) : String;
		begin
			Result:=(a*3).ToString;
		end;
	end;

function MyTest : TTest;
begin
	Result:=new TTest;
end;

function Test1(a : Integer) : String;
begin
	Result:=MyTest.Triple(a);
end;

function Test2(a : Integer) : String;
begin
	Result:=MyTest.Triple(a)+Test1(a);
end;

PrintLn(CallFunc(Test1, 1));
PrintLn(CallFunc(Test2, 2));