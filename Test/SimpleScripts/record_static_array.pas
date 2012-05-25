type 
   TPoint = record
      x, y : Integer;
   end;

var a, b : array [0..1] of TPoint;

a[0].x:=1;
a[0].y:=2;
a[1].x:=3;
a[1].y:=4;

b:=a;

a[0].x:=10;
a[1].y:=40;

PrintLn(b[0].x);
PrintLn(b[0].y);
PrintLn(b[1].x);
PrintLn(b[1].y);



