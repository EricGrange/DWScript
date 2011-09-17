SetRandSeed(0);
var i : Integer;
for i:=1 to 20 do
   if RandomInt(High(Integer))<0 then
      PrintLn('Bug');