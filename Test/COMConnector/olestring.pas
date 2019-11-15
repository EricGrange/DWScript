var v : ComVariant;

v := OleString('hello');

PrintLn(v = v);
PrintLn(v = 'hello');
PrintLn('hello' = v);
PrintLn(UpperCase(v));