var v : array of JSONvariant;

v.Add(JSON.Parse('0'), JSON.Parse('1'), JSON.Parse('2'));
v.Add(JSON.Parse('"0"'), JSON.Parse('"1"'), JSON.Parse('"2"'));
v.Add(JSON.Parse('"01"'), JSON.Parse('"01"'), JSON.Parse('"02"'));

PrintLn('=');
for var i := 0 to v.High do begin
   for var j := 0 to v.High do 
      Print(Ord(v[i] = v[j]));
   PrintLn('');
end;

PrintLn('<>');
for var i := 0 to v.High do begin
   for var j := 0 to v.High do 
      Print(Ord(v[i] <> v[j]));
   PrintLn('');
end;

PrintLn('<');
for var i := 0 to v.High do begin
   for var j := 0 to v.High do 
      Print(Ord(v[i] < v[j]));
   PrintLn('');
end;

PrintLn('<=');
for var i := 0 to v.High do begin
   for var j := 0 to v.High do 
      Print(Ord(v[i] <= v[j]));
   PrintLn('');
end;

PrintLn('>');
for var i := 0 to v.High do begin
   for var j := 0 to v.High do 
      Print(Ord(v[i] > v[j]));
   PrintLn('');
end;

PrintLn('>=');
for var i := 0 to v.High do begin
   for var j := 0 to v.High do 
      Print(Ord(v[i] >= v[j]));
   PrintLn('');
end;