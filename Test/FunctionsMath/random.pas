SetRandSeed(12);
if RandSeed<>12 then PrintLn('SetRandSeed failed');

Randomize;
if RandSeed=12 then PrintLn('Randomize failed');

var i : Integer;
for i:=1 to 100 do begin
   var r = Random;
   if (r<0) or (r>1) then PrintLn('Random failed');
end;

for i:=1 to 100 do begin
   var r = RandomInt(10);
   if (r<0) or (r>=10) then PrintLn('RandomInt failed');
end;

var s : Float;
for i:=1 to 100 do
   s:=s+RandG(100, 5);

s:=s/100;
if (s<95) or (s>105) then PrintLn('RandG oddity');
