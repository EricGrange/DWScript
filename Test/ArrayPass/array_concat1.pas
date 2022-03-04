var a : array of Integer;

a := [1] + [2, 3] + [4];

PrintLn(a.Map(IntToStr).Join(','));

a := a + [5, 6];

PrintLn(a.Map(IntToStr).Join(','));

a := [0] + a;

PrintLn(a.Map(IntToStr).Join(','));