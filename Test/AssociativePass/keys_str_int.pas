var i : array [Integer] of Integer;
var s : array [String] of Integer;

i[1] := 2;
s['1'] := 3;

var ki := i.Keys;
var ks := s.Keys;

PrintLn(ki[0] + ki[0]);
PrintLn(ks[0] + ks[0]);
