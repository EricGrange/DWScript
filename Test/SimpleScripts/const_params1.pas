procedure Test(const i : Integer);
begin
   PrintLn(i);
end;

Test(1);
Test(1+2);

var i = 10;
Test(i+1);
Test(2*i);
Test(Round(i*PI));
