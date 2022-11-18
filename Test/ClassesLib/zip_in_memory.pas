var zip := TZipWriter.CreateInMemory;

var hello := StringOfString('hello', 10);

zip.AddData(hello, 1, 'hello.txt');
zip.AddData('', 0, 'empty.bin');

var data := zip.CloseInMemory;
PrintLn(data.Length);

var reader := TZipReader.FromData(data);

PrintLn(reader.Count);
for var i := 0 to reader.Count-1 do begin
   PrintLn(reader.Names[i]);
   PrintLn(reader.ZipSize[i]);
   PrintLn(reader.FullSize[i]);
end;

