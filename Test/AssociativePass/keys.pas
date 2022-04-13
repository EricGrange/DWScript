var a : array [Integer] of Integer;

a[1] := 11;
a[2] := 22;
a[3] := 33;

PrintLn(a.Keys.Map(IntToStr).Sort.Join(','));

for var k in a.Keys.Sort do
    PrintLn(a[k]);

a.Clear;

PrintLn(a.Keys.Map(IntToStr).Sort.Join(','));

var s : array [String] of Boolean;

PrintLn(s.Keys.Sort.Join(','));

s['a'] := True;
s['b'] := False;
s['c'] := True;

PrintLn(s.Keys.Sort.Join(','));

for var k in s.Keys.Sort do
    PrintLn(s[k]);
