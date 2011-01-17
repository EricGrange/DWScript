procedure TestPositive(i : Integer);
require
   i >= 0; // first check
   i>0 : 'must be strictly positive';
begin
   PrintLn('TestPositive '+IntToStr(i));
end;

function TestInc(i : Integer) : Integer;
begin
   Result:=i+1;
   PrintLn('TestInc '+IntToStr(Result));
ensure
   Result >= 0; // first check
   Result>10 : 'must be above 10, was '+IntToStr(Result);
end;

TestPositive(10);
try
   TestPositive(-10);
except
   on e: Exception do PrintLn(e.Message);
end;
try
   TestPositive(0);
except
   on e: Exception do PrintLn(e.Message);
end;

TestInc(10);
try
   TestInc(-10);
except
   on e: Exception do PrintLn(e.Message);
end;
try
   TestInc(0);
except
   on e: Exception do PrintLn(e.Message);
end;
