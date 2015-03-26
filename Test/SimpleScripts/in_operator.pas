var i : Integer;
for i:=1 to 10 do
   if i in [2, 4, 6..8] then
      PrintLn(i);
for i:=1 to 10 do
   if (i+1) in [1, 3, 5] then
      PrintLn(i);
for i:=1 to 10 do
   if i in [7] then
      PrintLn(i);
for i:=1 to 10 do
   if (i+1) in [10..12, 13] then
      PrintLn(i);

PrintLn(BoolToStr(1 in []));

if 'A' in ['A'..'Z'] then
   PrintLn('A in A..Z');

if 'a' in ['A'..'Z'] then
   PrintLn('a in A..Z');
   

