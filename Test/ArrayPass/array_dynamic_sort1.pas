var a : array of Integer;
var i : Integer;

function Cmp1(a, b : Integer) : Integer;
begin
   Result:=a-b;
end;

function Cmp2(a, b : Integer) : Integer;
begin
   Result:=b-a;
end;

a.Add(3,2,1);

a.Sort(@Cmp1);

for i:=0 to a.High do
   Print(a[i]);
PrintLn('');

a.Reverse;

for i:=0 to a.High do
   Print(a[i]);
PrintLn('');   
   
a.Reverse;   
a.Sort(@Cmp2);   

for i:=0 to High(a) do
   Print(a[i]);
PrintLn('');
