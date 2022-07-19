var f : array of Float;

for var k := 1 to 3 do begin
   for var i := 1 to 10 do f.Add(i);

   f.Delete(7, 3);
   f.Delete(0, 2);

   PrintLn(f.Map(FloatToStr).Join(','));
end;
