var i := 123;

PrintLn(i mod 17);
PrintLn(i mod -17);
PrintLn((-i) mod -17);

var b := 17;
PrintLn(i mod b);
PrintLn(i mod -b);
PrintLn((-i) mod -b);

b := -16;
PrintLn(i mod b);

i := -i;
PrintLn(i mod b);

i := 123;
PrintLn(i mod 16);
i := -123;
PrintLn(i mod 16);

