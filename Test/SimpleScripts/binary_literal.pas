var i : Integer;

PrintLn(0b0);
PrintLn(0B1);
PrintLn(0b10);
PrintLn(0xa);

i:=0b1110;
PrintLn(i and 0b11);
PrintLn(0b0111 and i);
PrintLn(0b0111 and 0x4);

PrintLn(0b_101_111);
PrintLn(0b101_101);

PrintLn(%110011);
PrintLn(%110_011);
