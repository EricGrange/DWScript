uses TestTempPath;

var subDir := TempPath + 'SubDir';

RemoveDir(subDir, True);
if DirectoryExists(subDir) then
   PrintLn('Failed to remove at setup');

PrintLn(CreateDir(subDir));
if not DirectoryExists(subDir) then
   PrintLn('Failed to create');

PrintLn(MoveFile(subDir, subDir + '_'));
if DirectoryExists(subDir) or not DirectoryExists(subDir + '_') then
   PrintLn('Failed to rename');

PrintLn(RemoveDir(subDir + '_'));
if DirectoryExists(subDir + '_') then
   PrintLn('Failed to cleanup');

PrintLn('done');

