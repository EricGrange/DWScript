
var s := '';

PrintLn(ChangeFileExt(s, ''));

s := 'test.txt';

PrintLn(ChangeFileExt(s, '.bin'));
PrintLn(ChangeFileExt(s, 'test'));
PrintLn(ChangeFileExt(s, ''));

s := 'z:\foo\bar';
PrintLn(ChangeFileExt(s, '.bin'));
PrintLn(ChangeFileExt(s, 'test'));
PrintLn(ChangeFileExt(s, ''));

