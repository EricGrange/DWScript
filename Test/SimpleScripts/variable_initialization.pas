procedure Test(a : Integer);
var
   b : Integer = a+1;
   c : Integer = 20;
   d := c+1;
   e : Integer;
begin
   a:=0;
   c:=30;
   var f:=40;
   e:=c+f;
   PrintLn(a);
   PrintLn(b);
   PrintLn(c);
   PrintLn(d);
   PrintLn(e);   
   PrintLn(f);
end;

var a : Integer := 1;
var b := a+1;
a := 10;
var c : Integer := a+1;

PrintLn(a);
PrintLn(b);
PrintLn(c);

Test(a);