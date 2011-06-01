const ac : array [1..4] of Integer = [11, 22, 33, 44];
var a := ac;

Print(a[1]);
Print(a[2]);
Print(a[3]);
PrintLn(a[4]);

var i : Integer;
for i:=Low(a) to High(a) do
   PrintLn(a[i]);