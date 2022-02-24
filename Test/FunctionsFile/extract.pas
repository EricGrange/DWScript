
procedure Test(fileName : String);
begin
   PrintLn('"' + fileName + '"');
   PrintLn(#9'ext = ' + ExtractFileExt(fileName));
   PrintLn(#9'name = ' + ExtractFileName(fileName));
   PrintLn(#9'path = ' + ExtractFilePath(fileName));
end;

Test('');
Test('c:\dummy\filename.txt');
Test('filename.txt');
Test('\\server\folder\filename.txt');
Test('d:\base\sub\name');
Test('d:\base\sub\name\');


