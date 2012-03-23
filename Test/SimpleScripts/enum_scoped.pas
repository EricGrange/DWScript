type MyEnum = enum (a, b, c = 255, d);

type MyFlags = flags (a, b, c);

PrintLn(MyEnum.a);
PrintLn(MyEnum.b);
PrintLn(MyEnum.c);

var d := MyEnum.d;
PrintLn(MyEnum.d);

PrintLn(MyFlags.a);
PrintLn(MyFlags.b);
PrintLn(MyFlags.c);
