var t := StrSplit('1 2', ' ');
for var i := 0 to t.High do
   t[i] := 'p' + t[i];
PrintLn(t.Join(','));

for var i := 0 to t.High do
   t[i] := t[i] + 's';
PrintLn(t.Join(',')); 
