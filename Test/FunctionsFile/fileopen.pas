uses TestTempPath;

var fileName := TempPath + 'test.txt';

var f := FileCreate(fileName);
f.Write('helloworld');
var fEmpty : File;
f := fEmpty;

PrintLn(f.IsValid);

f := FileOpenRead(fileName);
PrintLn(f.Read(5));
try
   f.Write('foo');
except
   on E: Exception do
      PrintLn(E.Message);
end;

var buf := '';
PrintLn(f.Read(buf, 10));
PrintLn(buf);

Print(f.IsValid);
f.Close;
PrintLn(f.IsValid);

f := FileOpen(fileName, fmOpenReadWrite or fmShareExclusive);
try
   FileOpenRead(fileName);
except
   on E: Exception do
      PrintLn(E.Message);
end;
f.Close;

f := FileOpen(fileName, fmOpenReadWrite or fmShareDenyNone);
PrintLn(FileOpenRead(fileName).Read);


