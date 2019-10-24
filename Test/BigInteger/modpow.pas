var b : BigInteger;

b := BigInteger(4);
PrintLn(b.ModPow(13, BigInteger(497)));

b := BigInteger(0);
PrintLn(b.ModPow(4, BigInteger(20)));
PrintLn(b.ModPow(0, BigInteger(20)));

b := BigInteger(4);
try
   b.ModPow(9, BigInteger(0));
except
   PrintLn('div by zero');
end;

b := BigInteger(2);
PrintLn(b.ModPow(-3, BigInteger(11)));

b := BigInteger(76455);
PrintLn(b.ModPow(-3758223534, BigInteger(346346)));
