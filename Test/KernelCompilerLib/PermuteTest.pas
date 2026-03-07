var b := TKCLStridedBuffer.Create(TKCLDataType.Float32, [2, 3]);
b.SetData([0, 0], 1.0);
b.SetData([0, 1], 2.0);
b.SetData([0, 2], 3.0);
b.SetData([1, 0], 4.0);
b.SetData([1, 1], 5.0);
b.SetData([1, 2], 6.0);

PrintLn('Original [2, 3]:');
for var y := 0 to 1 do begin
   for var x := 0 to 2 do
      Print(FloatToStr(b.GetData([y, x])) + ' ');
   PrintLn('');
end;

b.Permute([1, 0]);

PrintLn('Permuted [3, 2]:');
for var y := 0 to 2 do begin
   for var x := 0 to 1 do
      Print(FloatToStr(b.GetData([y, x])) + ' ');
   PrintLn('');
end;
