var az : array[-1..1] of Integer;
var a : array[-3..-1] of Integer;

var i : Integer;

PrintLn('minus one-based array');
for i:=Low(az) to High(az) do begin
   az[i]:=i;
   PrintLn(IntToStr(i));
end;
for i in az do
   PrintLn(i);
PrintLn(az[-1]);   
PrintLn(az[0]);   
PrintLn(az[1]);   

PrintLn('minus 3-based array');
for i:=Low(a) to High(a) do begin
   a[i]:=i;
   PrintLn(IntToStr(i));
end;
for i in a do
   PrintLn(i);
