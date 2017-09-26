var bb : ByteBuffer;

bb.SetLength(3);

var cc := new ByteBuffer;
PrintLn(cc.Length);

PrintLn(bb.Length);

bb := cc;
PrintLn(bb.Length);

cc.SetLength(2);
PrintLn(bb.Length);