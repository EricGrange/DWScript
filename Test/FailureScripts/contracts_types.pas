procedure Test(i : Integer);
require
   IntToStr(i) : i;
begin
ensure
   Test(i) : Test(i);
end;