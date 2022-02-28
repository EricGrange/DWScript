uses TestTempPath;

var fileName := ExpandFileName(TempPath + 'test.txt');

PrintLn(fileName = ExpandFileName(TempPath + '.\test.txt'));

