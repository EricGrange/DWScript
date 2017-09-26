var bb : ByteBuffer;

PrintLn(bb.Length);
bb.SetLength(3);
PrintLn(bb.Length);

PrintLn(bb.ToJSON);

bb.SetByte(0, 1);
bb.SetByte(1, 2);
bb.SetByte(2, 3);

PrintLn(bb.GetByte(0));
PrintLn(bb.GetByte(1));
PrintLn(bb.GetByte(2));

PrintLn(bb.ToJSON);

bb.SetPosition(0);
PrintLn(bb.Position);

bb.SetByte(4);
bb.SetByte(5);
bb.SetByte(6);

PrintLn(bb.ToJSON);

PrintLn(bb.Position);
bb.SetPosition(0);

PrintLn(bb.GetByte);
PrintLn(bb.GetByte);
PrintLn(bb.GetByte);

PrintLn(bb.Position);