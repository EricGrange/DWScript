var a : array of Integer;

a += 1;

a += [2, 3];

a.Add([4, 5]);

a += a;

PrintLn(a.Map(IntToStr).Join(','));