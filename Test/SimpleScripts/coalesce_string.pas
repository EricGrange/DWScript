var s : String;

PrintLn(s ?? 'hello');

s := 'world';
PrintLn(s ?? 'hello');

function Test(v : String) : String;
begin
	PrintLn(v);
	Result:=v;
end;

PrintLn('1' ?? Test('a'));
PrintLn('' ?? Test('b'));