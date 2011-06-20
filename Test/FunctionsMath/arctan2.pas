var i, j : Integer;

for i:=-1 to 1 do
   for j:=-1 to 1 do
      PrintLn(Round(RadToDeg(ArcTan2(i, j))));