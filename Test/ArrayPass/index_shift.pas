var a : array of Integer := [ 123 ];
var s := 0;
for var j := 1 to 1 do
   s := a[j - 1];
PrintLn(s);
s := 0;
for var j := -1 to -1 do
   s := a[j + 1];
PrintLn(s);