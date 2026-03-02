var a1 : array of Float := [1, 2, 3];
var a2 : array of Float := [1, 2, 3];

PrintLn('--- Max Failures ---');
try a1.Max(a2, -1, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Max(a2, 0, -1, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Max(a2, 3, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Max(a2, 0, 3, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Max(a2, 2, 0, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Max(a2, 0, 2, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Max(a2, 0, 0, -1); except on e: Exception do PrintLn(e.Message); end;

PrintLn('--- Min Failures ---');
try a1.Min(a2, -1, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Min(a2, 0, -1, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Min(a2, 3, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Min(a2, 0, 3, 1); except on e: Exception do PrintLn(e.Message); end;
try a1.Min(a2, 2, 0, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Min(a2, 0, 2, 2); except on e: Exception do PrintLn(e.Message); end;
try a1.Min(a2, 0, 0, -1); except on e: Exception do PrintLn(e.Message); end;
