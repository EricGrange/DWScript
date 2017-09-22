var bb : ByteBuffer;

bb.SetLength(2);
bb.SetByte(6);
bb.SetByte(9);

PrintLn(bb);

bb.SetLength(2);

PrintLn(bb);

bb.SetLength(1);

PrintLn(bb);

bb.SetLength(2);

PrintLn(bb);

bb.SetLength(0);

PrintLn(bb);


