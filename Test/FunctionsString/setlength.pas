var s := 'hello';

SetLength(s, 8);
PrintLn('"' + s + '"');

SetLength(s, 5);
PrintLn('"' + s + '"');

SetLength(s, -1);
PrintLn('"' + s + '"');

