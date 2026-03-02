var a1 : array of Float := [1, 2, 3];
var a2 : array of Float := [1, 2, 3];

try a1.Multiply(a2, -1, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Multiply(a2, 0, -1, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Multiply(a2, 3, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Multiply(a2, 0, 3, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Multiply(a2, 2, 0, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Multiply(a2, 0, 2, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Multiply(a2, 0, 0, -1); except on e: Exception do PrintLn(e.Message); end;

try a1.Offset(a2, -1, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Offset(a2, 0, -1, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Offset(a2, 3, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Offset(a2, 0, 3, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Offset(a2, 2, 0, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Offset(a2, 0, 2, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Offset(a2, 0, 0, -1); except on e: Exception do PrintLn(e.Message); end;
