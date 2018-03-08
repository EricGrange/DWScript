// Int64 would overflow on 2^90, but Float won't

var b := BigInteger( IntPower(2, 90) );
PrintLn(b);
PrintLn(Float(b));

PrintLn(Log2(Float(b+b)));

// test Truncation

b := BigInteger(3.14);
PrintLn(b);

b := BigInteger(3.74);
PrintLn(b);

b := BigInteger(-3.14);
PrintLn(b);

b := BigInteger(-3.74);
PrintLn(b);

