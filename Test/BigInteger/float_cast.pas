// Int64 would overflow on 2^90
// Float will not overflow and be correct internally,
// but will lose precision when converted to scientific intermediate representation

var b := BigInteger( IntPower(2, 90) );
PrintLn(b);

var f := Float(b);
PrintLn(if (f > 1.237940039285e27) and (f < 1.237940039286e27) then 'True' else 'False');

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

