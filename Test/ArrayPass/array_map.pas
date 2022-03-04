var a : array of Integer;

function MyFunc(a : Integer) : String;
begin
	Result:='('+IntToStr(a)+')';
end;

a:=[1,3,2,5,4];

var b := a.Map(Sqr);

for var i in b do Print(i);
PrintLn('');

var c := b.Map(MyFunc);

for var i in c do Print(i);
PrintLn('');