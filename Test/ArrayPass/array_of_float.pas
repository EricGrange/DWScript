var a : array of Float;


a.Add(1.5, 2.5);

a.Move(0, 1);
PrintLn(a.Map(FloatToStr).Join(','));

a.Move(1, 0);
PrintLn(a.Map(FloatToStr).Join(','));

a.Add(3.5);

a.Move(0, 2);
PrintLn(a.Map(FloatToStr).Join(','));

a.Move(2, 0);
PrintLn(a.Map(FloatToStr).Join(','));

PrintLn(a.IndexOf(4));
PrintLn(a.IndexOf(2.5));

a.Reverse;
PrintLn(a.Map(FloatToStr).Join(','));

a.Add(a);
PrintLn(a.Map(FloatToStr).Join(','));