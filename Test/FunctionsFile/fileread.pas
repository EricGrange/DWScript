uses TestTempPath;

var f := FileCreate(TempPath + 'test.txt');

f.Write('hello world');

PrintLn(f.Seek(0, soFromBeginning));

PrintLn(FilePos(f));

PrintLn(f.Read(1024*1024*1024*1024));  // 1 TB

PrintLn(f.Position);



