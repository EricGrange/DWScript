var a1, a2 : array of Float;
a1.SetLength(5000);
a2.SetLength(5000);
for var i := 0 to a1.High do begin
   a1[i] := 1.0;
   a2[i] := 2.0;
end;

// Test counts from 1 to 257
for var c := 1 to 257 do
   PrintLn(c.ToString + ': ' + ArrayDotProduct(a1, a2, 0, 0, c).ToString);

// Test count 4000
PrintLn('4000: ' + ArrayDotProduct(a1, a2, 0, 0, 4000).ToString);

// offset + count = length is OK
PrintLn('End: ' + ArrayDotProduct(a1, a2, 4990, 4990, 10).ToString);

// 2-parameter overload
var a3, a4 : array of Float;
a3.Add(1.0, 2.0);
a4.Add(3.0, 4.0, 5.0);
PrintLn('Overload: ' + a3.DotProduct(a4).ToString);
