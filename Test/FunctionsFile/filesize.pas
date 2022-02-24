uses TestTempPath;

var fileName := TempPath + 'test.txt';

DeleteFile(fileName);
if FileExists(fileName) then
   PrintLn('Failed cleanup');

PrintLn(FileSize(fileName));

var f := FileCreate(fileName);

Print(FileSize(fileName));
PrintLn(f.Size);

f.Write('foo');

Print(FileSize(fileName));
PrintLn(f.Size);

f.SetSize(1024);

Print(FileSize(fileName));
PrintLn(f.Size);

f.SetSize(123);

Print(FileSize(fileName));
PrintLn(f.Size);

f.Close;

PrintLn(DeleteFile(fileName));

