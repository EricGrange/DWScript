var b := BigInteger(3);
PrintLn(b.ModInv(BigInteger(11)));

b := BigInteger(40);
PrintLn(b.ModInv(BigInteger(1)));

b := BigInteger(42);
PrintLn(b.ModInv(BigInteger(2017)));

b := BigInteger(52);
PrintLn(b.ModInv(BigInteger(-217)));

b := BigInteger(-486);
PrintLn(b.ModInv(BigInteger(217)));

b := BigInteger(154);
PrintLn(b.ModInv(BigInteger(3311)));


