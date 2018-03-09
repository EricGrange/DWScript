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
