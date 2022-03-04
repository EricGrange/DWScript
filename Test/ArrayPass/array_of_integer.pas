var a : array of Integer;


a.Add(1, 2);

a.Move(0, 1);
PrintLn(a.Map(IntToStr).Join(','));

a.Move(1, 0);
PrintLn(a.Map(IntToStr).Join(','));

a.Add(3);

a.Move(0, 2);
PrintLn(a.Map(IntToStr).Join(','));

a.Move(2, 0);
PrintLn(a.Map(IntToStr).Join(','));

PrintLn(a.IndexOf(4));
PrintLn(a.IndexOf(2));
