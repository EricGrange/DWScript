var a : array of Integer;

function Ascending(a, b : Integer) : Integer;
begin
	Result:=a-b;
end;

function Descending(a, b : Integer) : Integer;
begin
	Result:=b-a;
end;

a:=[1,3,2,5,4];

a.Sort(@Ascending);

for var i in a do Print(i);
PrintLn('');

a:=[1,3,2,5,4];

a.Sort(Descending);

for var i in a do Print(i);
PrintLn('');