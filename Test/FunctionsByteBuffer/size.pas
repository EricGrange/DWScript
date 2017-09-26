var bb : ByteBuffer;

bb.SetLength(2);
bb.SetByte(6);
bb.SetByte(9);

PrintLn(bb.ToJSON);

bb.SetLength(2);

PrintLn(bb.ToJSON);

bb.SetLength(1);

PrintLn(bb.ToJSON);

bb.SetLength(2);

PrintLn(bb.ToJSON);

bb.SetLength(0);

PrintLn(bb.ToJSON);


