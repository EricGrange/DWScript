uses TestTempPath;

var fileName := TempPath + 'binary.bin';

procedure TestSize(n : Integer);
begin
	var buf := '';
	for var k := 1 to n do
		buf += Chr((n*19 + k) and 255);
		
	FileWrite(fileName, buf);
	
	if FileSize(fileName) <> n then
		PrintLn('invalid size for ' + n.ToString);
		
	var buf2 := FileRead(fileName);
	if buf <> buf2 then
		PrintLn('invalid content for ' + n.ToString);
end;

DeleteFile(fileName);
if FileExists(fileName) then
	PrintLn('Failed to delete');

for var i := 0 to 20 do
	TestSize(i);
	
TestSize(1023);
TestSize(4097);

PrintLn('done');
