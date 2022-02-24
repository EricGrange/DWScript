uses TestTempPath;

var fileName := TempPath + 'test.txt';

DeleteFile(fileName);
if FileExists(fileName) then
   PrintLn('Failed cleanup');

var t := Now;
FileWrite(fileName, 'hello');

if Abs(FileDateTime(fileName) - t)*86400 > 3 then
   PrintLn('Create datetime mismatch');

t := EncodeDate(2020, 1 , 2);
FileSetDateTime(fileName, t);

PrintLn(FileDateTime(fileName) - t);

var f := FileOpen(fileName, fmOpenReadWrite);
PrintLn(f.DateTime - t);
f.SetDateTime(t + 1);
PrintLn(f.DateTime - t);
f.Close;

try
   PrintLn(f.DateTime);
except
   on E : Exception do
      PrintLn(E.Message);
end;

try
   f.SetDateTime(Now);
except
   on E : Exception do
      PrintLn(E.Message);
end;

try
   var f2: File;
   f2.SetDateTime(Now);
except
   on E : Exception do
      PrintLn(E.Message);
end;

try
   FileSetDateTime(fileName + '.fubar', Now);
except
   on E : Exception do
      PrintLn(E.Message);
end;

