var i : Integer;

for i:=Low(i) to High(i) step High(i) div 10 do
   PrintLn(i);

for i:=High(i) downto Low(i) step High(i) div 10 do
   PrintLn(i);
