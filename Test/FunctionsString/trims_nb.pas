var s = ' test ';

PrintLn(''.Trim(1,1));

for var i:=-1 to 4 do begin
   for var j:=-1 to 4 do begin
      Print(',');
      Print(Trim(s, i, j));
   end;
   PrintLn(',');
end;   

PrintLn(''.Trim(0,0));

for var i:=-1 to 4 do begin
   for var j:=-1 to 4 do begin
      Print(',');
      Print(s.Trim(i, j));
   end;
   PrintLn(',');
end;   
