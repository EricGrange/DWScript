var a := '123 ABC';
var i : Integer;

for i:=Low(a) to High(a) do
   PrintLn(Ord(a[i]));