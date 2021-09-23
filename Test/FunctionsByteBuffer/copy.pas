var b : ByteBuffer;

b.AssignDataString('hello world');

var b2 := b.Copy;
PrintLn(b2.ToDataString);

b2 := b.Copy(6);
PrintLn(b2.ToDataString);

b2 := b.Copy(6, 10);
PrintLn(b2.ToDataString);

b2 := b.Copy(1, 2);
PrintLn(b2.ToDataString);

b2 := b.Copy(2, 0);
PrintLn(b2.ToDataString);
PrintLn(b2.Length);
