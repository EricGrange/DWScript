var k := TRSAKey.Generate(512);

PrintLn(k.BlockLength);

var c1 := k.Encrypt('test'+StringOfChar('-', 60), '', '');
PrintLn(k.Decrypt(c1, '', ''));

var c2 := k.Encrypt('test', 'PKCS1', '');
PrintLn(k.Decrypt(c2, 'PKCS1', ''));