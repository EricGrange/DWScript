uses TestTempPath;

var fileName := TempPath + 'test.txt';

var f := FileCreate(fileName);
PrintLn(FileFlushBuffers(f));
f.Write('hello');
PrintLn(f.FlushBuffers);
f.Close;

try
   PrintLn(f.FlushBuffers);
except
   on E: Exception do
      PrintLn(E.Message);
end;
