var b : BigInteger;

b.SetBit(0);
PrintLn(b.ToString);
b.SetBit(7);
PrintLn(b.ToString);
b.SetBit(70);
PrintLn(b.ToString);

b.ClearBit(7);
PrintLn(b.ToString);

b.SetBit(70, False);
PrintLn(b.ToString);

b.SetBit(0, False);
b.SetBit(1, True);
PrintLn(b.ToString);

