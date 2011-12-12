var a : array [-1..10] of Integer;

var i, s : Integer;

for i:=1 to 100 do
   Inc(a[RandomInt(10)]);

for i:=-1 to 10 do
   s+=a[i];
   
PrintLn(s);
   
for i:=1 to 50 do
   Dec(a[RandomInt(10)]);

for i:=-1 to 10 do
   s+=a[i];

PrintLn(s);
