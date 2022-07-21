var s : array [0..3] of Integer = (0, 1, 2, 3);
var d : array of Integer;
d.Add(0, 1, 2, 3);

for var i := -1 to 5 step 2 do begin
   Print(Ord(i in s)); 
	Print(Ord(s.Contains(i)));
   Print(Ord(i in d));
	PrintLn(Ord(d.Contains(i)));
end;
