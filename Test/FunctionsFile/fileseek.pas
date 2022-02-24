uses TestTempPath;

var f := FileCreate(TempPath + 'test.txt');

PrintLn(f.Seek(0, soFromCurrent));

f.Write('helloworld');

PrintLn(f.Seek(0, soFromCurrent));
PrintLn(f.Seek(-5, soFromCurrent));
PrintLn(f.Read(3));
PrintLn(f.Seek(1, soFromBeginning));
PrintLn(f.Read(4));
PrintLn(f.Seek(-5, soFromEnd));
PrintLn(f.Read(4));


