var i : Integer;

for i:=-1 to 3 do
   PrintLn(ClampInt(i, 0, 2));

for i:=-1 to 3 do
   PrintLn(Clamp(i*2.5, 0, 4));
   