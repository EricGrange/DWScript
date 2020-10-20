var i := 0;

PrintLn((i = 0) is True);
PrintLn((i = 1) is False);

PrintLn((i = 0) is not True);
PrintLn((i = 1) is not False);

PrintLn((i = 0) is (i = 1));
PrintLn((i = 1) is (i = 1));

i := 1;

PrintLn((i = 0) is True);
PrintLn((i = 1) is False);

PrintLn((i = 0) is not True);
PrintLn((i = 1) is not False);

PrintLn((i = 0) is (i = 1));
PrintLn((i = 1) is (i = 1));

PrintLn('reverse');

PrintLn(True is (i = 0));
PrintLn(False is (i = 1));

PrintLn(not True is (i = 0));
PrintLn(not False is (i = 1));

PrintLn(True is True);
PrintLn(True is False);