var a : array of Boolean;


a.Add(True, False);

a.Move(0, 1);
PrintLn(a.Map(BoolToStr).Join(','));

a.Move(1, 0);
PrintLn(a.Map(BoolToStr).Join(','));

a.Insert(1, True);

a.Move(0, 2);
PrintLn(a.Map(BoolToStr).Join(','));

a.Move(2, 0);
PrintLn(a.Map(BoolToStr).Join(','));

PrintLn(a.IndexOf(False));
PrintLn(a.IndexOf(True));

a.Reverse;
PrintLn(a.Map(BoolToStr).Join(','));

a.Add(a);
PrintLn(a.Map(BoolToStr).Join(','));

a.Swap(0, 1);
PrintLn(a.Map(BoolToStr).Join(','));

a.Delete(0, 3);
PrintLn(a.Map(BoolToStr).Join(','));
a.Delete(1);
PrintLn(a.Map(BoolToStr).Join(','));

a.Add(False);
a.Sort;
PrintLn(a.Map(BoolToStr).Join(','));