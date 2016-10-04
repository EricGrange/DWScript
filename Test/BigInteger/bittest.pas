var b := BigInteger( 125 );

for var i := 7 downto 0 do
   Print(Ord(b.TestBit(i)));
PrintLn(''); 

b *= 2;

for var i := 7 downto 0 do
   Print(Ord(b.TestBit(i)));
PrintLn(''); 

PrintLn(b.TestBit(10000000000));
b := -b;
PrintLn(b.TestBit(10000000000));
