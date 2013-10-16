PrintLn(IntToStr(123));
PrintLn(StrToInt('123'));
PrintLn('233'.ToInteger+1);
PrintLn(StrToIntDef('', 456));
PrintLn(StrToIntDef('A', 789));

PrintLn(FloatToStr(123));
PrintLn(StrToFloat('12.3'));
PrintLn('12.5'.ToFloat+1);
PrintLn(StrToFloatDef('', 45.6));
PrintLn(StrToFloatDef('A', 7.89));

PrintLn(UpperCase(IntToHex(123, 4)));
PrintLn(HexToInt('123'));
