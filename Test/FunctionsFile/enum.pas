uses TestTempPath;

var subDir := TempPath + 'SubDir';

RemoveDir(subDir, True);
if DirectoryExists(subDir) then
   PrintLn('Failed to remove at setup');

PrintLn(ForceDirectories(subDir + '\foo\bar'));
PrintLn(CreateDir(subDir + '\foo\two'));

PrintLn(EnumerateSubDirs(subDir).Map(lambda (x) => x.After('SubDir\')).Join(','));
PrintLn(EnumerateSubDirs(subDir + 'dummy').Length);

FileWrite(subDir + '\foo\hello.txt', 'a');
FileWrite(subDir + '\foo\bar\world.bin', 'b');

PrintLn(EnumerateDir(subDir + '\foo', '*.txt', False).Length);
PrintLn(EnumerateDir(subDir + '\foo', '*.*', True).Length);
PrintLn(EnumerateDir(subDir + '\foo', '*.*', True).Map(lambda (x) => x.After('SubDir\')).Join(','));

PrintLn(RemoveDir(subDir));
PrintLn(DirectoryExists(subDir));
PrintLn(RemoveDir(subDir, True));
PrintLn(DirectoryExists(subDir));

