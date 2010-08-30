var i : Integer;

i:=0;
PrintLn(IntToStr(i and 15));
PrintLn(IntToStr(i or 15));
PrintLn(IntToStr(i xor 15));

i:=i or 4;

PrintLn(IntToStr(i and 15));
PrintLn(IntToStr(i or 15));
PrintLn(IntToStr(i xor 15));

i:=255;

PrintLn(IntToStr(i and not 7));
