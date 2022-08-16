var a := 1024;
var b := 1;

var k1, k2, k3 : Integer;

begin
   k1 := a div 16;
   k2 := a div b;
   k3 := a div (b+b);
end;
PrintLn(k1);
PrintLn(k2);
PrintLn(k3);

a := -1024;
begin
   k1 := a div 16;
   k2 := a div b;
   k3 := a div (b+b);
end;
PrintLn(k1);
PrintLn(k2);
PrintLn(k3);

b := -1;
begin
   k1 := (a+a) div 16;
   k2 := (a+a) div b;
   k3 := (a+a) div (b+b);
end;
PrintLn(k1);
PrintLn(k2);
PrintLn(k3);
