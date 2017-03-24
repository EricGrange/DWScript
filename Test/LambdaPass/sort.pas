var a : array of Integer := [1, 3, 2];

a.Sort(lambda (a, b) => a - b);

PrintLn(a.Map(IntToStr).Join(','));

a.Sort(lambda (a, b) => b - a);

PrintLn(a.Map(IntToStr).Join(','));