uses TestTempPath;

var f1 := TempPath + 'test1.txt';
var f2 := TempPath + 'test2.txt';

DeleteFile(f1);
DeleteFile(f2);
if FileExists(f1) or FileExists(f2) then
   PrintLn('Failed cleanup');

FileWrite(f1, 'hello');

PrintLn(RenameFile(f2, f1));
PrintLn(RenameFile(f1, f2));

Print(FileExists(f1)); PrintLn(FileExists(f2));

PrintLn(CopyFile(f1, f2, True));
PrintLn(CopyFile(f2, f1, True));

Print(FileExists(f1)); PrintLn(FileExists(f2));

PrintLn(CopyFile(f1, f2, True));
PrintLn(CopyFile(f1, f2, False));

DeleteFile(f1);
DeleteFile(f2);

