function Sum(a : array of Float) : Float;
begin
   Result := 0;
   for var v in a do Result += v;
end;

var a1 : array of Float := [10, 20, 30, 40, 50];
PrintLn('Max const: ' + Sum(a1.Max(35)).ToString);

var a2 : array of Float := [10, 20, 30, 40, 50];
PrintLn('Min const: ' + Sum(a2.Min(25)).ToString);

var a3 : array of Float := [1, 10, 1, 10, 1];
var a4 : array of Float := [5, 5, 5, 5, 5];
PrintLn('Max array: ' + Sum(a3.Max(a4)).ToString);

var a5 : array of Float := [1, 10, 1, 10, 1];
var a6 : array of Float := [5, 5, 5, 5, 5];
PrintLn('Min array: ' + Sum(a5.Min(a6)).ToString);

var a7 : array of Float := [10, 10, 10, 10, 10];
var a8 : array of Float := [1, 20, 1, 20, 1];
PrintLn('Max offset: ' + Sum(a7.Max(a8, 1, 1, 2)).ToString);

var a9 : array of Float := [10, 10, 10, 10, 10];
var a10 : array of Float := [1, 5, 1, 5, 1];
PrintLn('Min offset: ' + Sum(a9.Min(a10, 0, 1, 3)).ToString);

// Stress test SSE2 branches with diverse crossing values
var b1, b2 : array of Float;
b1.SetLength(100);
b2.SetLength(100);

for var c := 1 to 33 do begin
   // Max test (i vs 100-i)
   for var i := 0 to 99 do begin
      b1[i] := i;
      b2[i] := 100 - i;
   end;
   b1.Max(b2, 1, 1, c); // offset 1 to stay consistent with other tests
   var sMax := Sum(b1);

   // Min test (i vs 100-i)
   for var i := 0 to 99 do begin
      b1[i] := i;
      b2[i] := 100 - i;
   end;
   b1.Min(b2, 1, 1, c);
   var sMin := Sum(b1);

   PrintLn(c.ToString + ': ' + sMax.ToString + ' ' + sMin.ToString);
end;
