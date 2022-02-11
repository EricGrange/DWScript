var b1 := ByteBuffer('hello');
var b2 : ByteBuffer;

PrintLn(b1.ToDataString);
PrintLn(b2.ToDataString);

b2.Assign(b1);
b1.SetLength(2);

PrintLn(b1.ToDataString);
PrintLn(b2.ToDataString);

b1.AssignBase64('dGVzdGluZw==');
b2.AssignJSON('[48,49,50]');

PrintLn(b1.ToDataString);
PrintLn(b2.ToDataString);

b1.AssignBase64('');
b2.AssignHexString('39383736');

PrintLn(b1.ToDataString);
PrintLn(b2.ToDataString);
