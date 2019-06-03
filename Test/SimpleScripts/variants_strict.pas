var a : Variant = 1;
var b : Variant = 1.0;
var c : Variant = '1';

PrintLn('a vs b');
PrintLn(a = b);
PrintLn(a == b);
PrintLn(a === b);

PrintLn('b vs c');
PrintLn(b = c);
PrintLn(b == c);
PrintLn(b === c);

PrintLn('a vs c');
PrintLn(a = c);
PrintLn(a == c);
PrintLn(a === c);

PrintLn('strict self');
PrintLn(a === a);
PrintLn(b === b);
PrintLn(c === c);