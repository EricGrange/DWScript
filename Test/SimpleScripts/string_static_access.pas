var s := 'hello';
var i := 2;

PrintLn(s[1]);
PrintLn(s[i]);

PrintLn(Copy(s, 1, 1));
PrintLn(Copy(s, i, 1));
PrintLn(Copy(s, 1, 2));
PrintLn(Copy(s, i, i));
