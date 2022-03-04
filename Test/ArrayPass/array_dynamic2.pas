var a : array of array of Integer;

var i, j : Integer;

a.SetLength(3);

for i:=a.Low to High(a) do begin
   a[i].SetLength(4);
   for j:=Low(a[i]) to a[i].High do begin
      a[i][j]:=(i+1)*10+j;
   end;
end;

for i:=0 to 2 do begin
   for j:=0 to 3 do
      Print(a[i][j]);
   PrintLn('');
end;