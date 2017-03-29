var a : array [Integer] of Integer;

a[1] := 11;
a[2] := 22;

for var i := 0 to 3 do
    PrintLn(i in a);

a.Clear;

for var i := 0 to 1 do
    PrintLn(i in a);

var s : array [String] of Boolean;

s['a'] := True;
s['c'] := False;

PrintLn('a' in s);
PrintLn('b' in s);
for var i := Ord('c') to Ord('d') do
    PrintLn(Chr(i) in s);
