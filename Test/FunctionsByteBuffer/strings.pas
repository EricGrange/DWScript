var b := ByteBuffer('hello');

PrintLn(b.ToHexString);

var s := '';
b := ByteBuffer(s);
PrintLn(b.ToHexString);
s := '0';
PrintLn(ByteBuffer(s).ToHexString);

s := #$1234#$5678;
PrintLn(ByteBuffer(s).ToHexString);
