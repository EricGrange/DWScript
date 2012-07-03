type TRec1 = record
   a : Integer;
   end;
  
var r1 : TRec1 = (a: 1);
var r2 : TRec1 = (a: 2);

Print(r1.a);
PrintLn(r2.a);
Swap(r1, r2);
Print(r1.a);
PrintLn(r2.a);

type TRec2 = record
   a, b : Integer;
   end;
  
var z1 : TRec2 = (a: 10; b: 2);
var z2 : TRec2 = (a: 30; b: 4);

Print(z1.a+z1.b);
PrintLn(z2.a+z2.b);
Swap(z1, z2);
Print(z1.a+z1.b);
PrintLn(z2.a+z2.b);