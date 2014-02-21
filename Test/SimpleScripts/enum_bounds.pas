type MyEnum = enum (a, b, c = 255);
type MyEnum2 = enum (High, Low, a, b, c = 255);
type MyFlags = flags (a, b, c);

PrintLn(MyEnum.Low);
PrintLn(MyEnum.High);

PrintLn(MyEnum2.Low);
PrintLn(MyEnum2.Low());
PrintLn(MyEnum2.High);
PrintLn(MyEnum2.High());

PrintLn(MyFlags.Low);
PrintLn(MyFlags.High);

