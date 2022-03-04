var a : array of Integer;

function MyFunc(a : Integer) : Boolean;
begin
	Result := a in [1, 4];
end;

a := [1,3,2,5,4];

var b := a.Filter(MyFunc);

for var i in b do Print(i);
PrintLn('');

var c := a.Filter(lambda (e) => e > 3);

for var i in c do Print(i);
PrintLn('');