uses TestTempPath;

var fileName := TempPath + 'test.txt';

DeleteFile(fileName);

var buf := FileReadLines(fileName);
PrintLn(buf.Length);

// basic

FileWrite(fileName, 'abcd');
PrintLn(FileReadLines(fileName).Join(','));
FileWrite(fileName, 'abcd'#10'defg');
PrintLn(FileReadLines(fileName).Join(','));
FileWrite(fileName, 'abcd'#13#10'ihj');
PrintLn(FileReadLines(fileName).Join(','));

// empty lines

FileWrite(fileName, #10);
PrintLn(FileReadLines(fileName).Join(','));
FileWrite(fileName, #10#10);
PrintLn(FileReadLines(fileName).Join(','));
FileWrite(fileName, #13#10#13#10#13#10);
PrintLn(FileReadLines(fileName).Join(','));

// utf-8 

FileWrite(fileName, #$C3#$A9#$C3#$A0);
PrintLn(FileReadLines(fileName).Join(','));
FileWrite(fileName, #$EF#$BB#$BF#$C3#$A9#13#10#$C3#$A0);
PrintLn(FileReadLines(fileName).Join(','));

// utf-16

FileWrite(fileName, #$FE#$FF#$00#$E9#$00#$E0);
PrintLn(FileReadLines(fileName).Join(','));
FileWrite(fileName, #$FF#$FE#10#$E9#$00#$E0#$00);
PrintLn(FileReadLines(fileName).Join(','));

// many lines

FileWrite(fileName, StringOfChar(#10, 2000));
PrintLn(FileReadLines(fileName).Length);
