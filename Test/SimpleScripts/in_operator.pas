var i : Integer;
for i:=1 to 10 do
   if i in [2, 4, 6..8] then
      PrintLn(i);

PrintLn(VarToStr(1 in []));

if 'A' in ['A'..'Z'] then
   PrintLn('A in A..Z');

if 'a' in ['A'..'Z'] then
   PrintLn('a in A..Z');
