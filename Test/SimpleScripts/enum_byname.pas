type MyEnum = enum (a, b, c = 255, d);

type MyFlags = flags (a, b, c);

PrintLn(MyEnum.ByName(''));
PrintLn(MyEnum.ByName('a'));
PrintLn(MyEnum.ByName('MyEnum.b'));
PrintLn(MyEnum.ByName('c'));
PrintLn(MyEnum.ByName('D'));

PrintLn(MyFlags.ByName('a'));
PrintLn(MyFlags.ByName('myflags.b'));
PrintLn(MyFlags.ByName('C'));
