PrintLn(Copy('bananas', 2, 3));
PrintLn(SubStr('bananas', 3));
PrintLn(SubString('bananas', 2, 3));

PrintLn(LeftStr('bananas', 3));
PrintLn(RightStr('bananas', 3));

PrintLn('-');

PrintLn(SubStr('bananas', 3, 1));
PrintLn(SubStr('bananas', 3, -10));
PrintLn(SubStr('bananas', 3, 100));
PrintLn(SubStr('bananas', 1, 3));

PrintLn('-');

PrintLn(SubString('bananas', 3, 1));
PrintLn(SubString('bananas', 3, -10));
PrintLn(SubString('bananas', 3, 100));
PrintLn(SubString('bananas', 1, 3));

PrintLn('---');

var s := 'bananas';
PrintLn(Copy(s, 2, 3));
PrintLn(SubStr(s, 3));
PrintLn(SubString(s, 2, 3));

PrintLn(LeftStr(s, 3));
PrintLn(RightStr(s, 3));

PrintLn('-');

PrintLn(SubStr(s, 3, 1));
PrintLn(SubStr(s, 3, -10));
PrintLn(SubStr(s, 3, 100));
PrintLn(SubStr(s, 1, 3));

PrintLn('-');

PrintLn(SubString(s, 3, 1));
PrintLn(SubString(s, 3, -10));
PrintLn(SubString(s, 3, 100));
PrintLn(SubString(s, 1, 3));
