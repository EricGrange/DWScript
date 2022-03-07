var b := False;

PrintLn(b.ToString);
PrintLn(BoolToStr(not b));

var vals : array of String = [
   '', 'true', 'false', 'True', 'False',
   'y', 'yes', 'n', 'no',
   '1', '0', '123', '000', '0001', '0000' ];
for var s in vals do
   PrintLn(s + ' = ' + StrToBool(s).ToString);
