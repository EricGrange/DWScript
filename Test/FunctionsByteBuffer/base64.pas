var b : ByteBuffer;

b.AssignDataString('hello world');
PrintLn(b.ToBase64);

b.SetLength(0);
PrintLn(b.ToBase64);

b.SetLength(2);
PrintLn(b.ToBase64);
