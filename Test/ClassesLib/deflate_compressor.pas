var comp := new TDeflateCompressor(1);

PrintLn(comp.CRC32.ToHexString(8));
PrintLn(comp.SizeIn);
PrintLn(comp.SizeOut);

for var i := 1 to 10 do
   comp.WriteData('hello');

var data := comp.Flush;
for var i := 1 to data.High do
   Print(Ord(data[i]).ToHexString(2));

PrintLn('');
PrintLn(comp.CRC32.ToHexString(8));
PrintLn(comp.SizeIn);
PrintLn(comp.SizeOut);




