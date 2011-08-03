function Iterative(n : Integer) : Integer;
var 
   i : Integer;
begin
   Result:=1;
   for i:=2 to n do
      Result *= i;
end;

function Recursive(n : Integer) : Integer;
begin
   if n>1 then
      Result := Recursive(n-1)*n
   else Result := 1;
end;


var i : Integer;

PrintLn(Iterative(0));
PrintLn(Iterative(1));
PrintLn(Iterative(2));
PrintLn(Iterative(3));

for i:=4 to 10 do
	PrintLn(Iterative(i));
   
PrintLn(Recursive(0));
PrintLn(Recursive(1));
PrintLn(Recursive(2));
PrintLn(Recursive(3));

for i:=4 to 10 do
	PrintLn(Recursive(i));   