procedure Test;
var
   a := 1;
   b := a+1;
begin
   a := 10;
   PrintLn(a);
   PrintLn(b);
end;

Test;