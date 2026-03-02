function Sum(a : array of Float) : Float;
begin
   Result := 0;
   for var v in a do Result += v;
end;

var a1 : array of Float := [1, 2, 3, 4, 5];
var a2 : array of Float := [10, 20, 30, 40, 50];
PrintLn('Offset: ' + Sum(a1.Offset(a2)).ToString);

var a3 : array of Float := [1, 1, 1, 1, 1];
var a4 : array of Float := [10, 20, 30, 40, 50];
PrintLn('Offset Offset: ' + Sum(a3.Offset(a4, 1, 2, 2)).ToString);

// Stress test SSE2 branches with diverse values in both arrays
var b1, b2 : array of Float;
b1.SetLength(100);
b2.SetLength(100);

for var c := 1 to 33 do begin
   for var i := 0 to 99 do begin
      b1[i] := i;
      b2[i] := 100 + i;
   end;
   b1.Offset(b2, 1, 5, c);
   PrintLn(c.ToString + ': ' + Sum(b1).ToString);
end;
