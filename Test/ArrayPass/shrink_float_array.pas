var a : array of Float;

for var i := 1 to 100 do
   a.Add(i);

a.Delete(10, 80);
PrintLn(a.Map(FloatToStr).Join(','));

a.SetLength(100);
PrintLn(a.Length);

a.SetLength(10);
PrintLn(a.Map(FloatToStr).Join(','));

