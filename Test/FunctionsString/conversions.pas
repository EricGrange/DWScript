PrintLn(IntToStr(123));
PrintLn(StrToInt('123'));
PrintLn(StrToIntDef('', 456));
PrintLn(StrToIntDef('A', 789));

PrintLn(FloatToStr(123));
PrintLn(StrToFloat('12.3'));
PrintLn(StrToFloatDef('', 45.6));
PrintLn(StrToFloatDef('A', 7.89));

PrintLn(IntToHex(123, 4));
PrintLn(HexToInt('123'));
