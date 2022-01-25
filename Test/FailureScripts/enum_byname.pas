type TMyEnum = enum (a, b, c = 255, d);

PrintLn(TMyEnum.ByName(123));
PrintLn(TMyEnum.ByName(nil));
PrintLn(TMyEnum.ByName(Print('bug')));
PrintLn(TMyEnum.ByName());