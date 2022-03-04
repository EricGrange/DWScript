var a : array of Integer;

var i : Integer;

PrintLn(Length(a));

a:=new Integer[3];

PrintLn(Length(a));

for i:=Low(a) to High(a) do
   a[i]:=i*i;

a.SetLength(4);
PrintLn(a.Length);

for i:=a.Low to a.High do
   PrintLn(a[i]);
