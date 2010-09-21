var az : array[0..4] of Integer;
var a : array[5..9] of Integer;

var i : Integer;

PrintLn('zero-based array');
for i:=Low(az) to High(az) do begin
   az[i]:=i;
   PrintLn(IntToStr(i));
end;
try
   az[-1]:=0;
except
   on e: Exception do
      PrintLn(e.Message);
end;
try
   az[5]:=0;
except
   on e: Exception do
      PrintLn(e.Message);
end;

PrintLn('five-based array');
for i:=Low(a) to High(a) do begin
   a[i]:=i;
   PrintLn(IntToStr(i));
end;
try
   a[0]:=0;
except
   on e: Exception do
      PrintLn(e.Message);
end;
try
   az[10]:=0;
except
   on e: Exception do
      PrintLn(e.Message);
end;