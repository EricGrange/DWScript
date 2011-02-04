//
// Fibonacci
//

function fib(N : Integer) : Integer;
begin
  if N < 2 then Result := 1
  else Result := fib(N-2) + fib(N-1);
End;

var i : Integer;
for i:=1 to 10 do
   PrintLn('F'+IntToStr(i+1)+' = '+IntToStr(fib(i)));
