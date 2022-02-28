uses TestTempPath;

var fileName := TempPath + 'test.txt';
var f1, f2 : File;

FileWrite(fileName, 'hello');

try
   f2 := FileOpen(fileName, fmOpenReadWrite or fmExclusive);
   PrintLn(f2.Size);
   f2.Close;
except
   on E: Exception do
      PrintLn(E.Message);
end;

f1 := FileCreate(fileName);

try
   f2 := FileOpen(fileName, fmOpenReadWrite or fmExclusive);
except
   on E: Exception do
      PrintLn(E.Message);
end;

try
   f2.Close;
except
   on E: Exception do
      PrintLn(E.Message);
end;


