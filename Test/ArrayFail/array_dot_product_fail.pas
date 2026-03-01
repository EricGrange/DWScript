var a1, a2 : array of Float;
a1.SetLength(5000);
a2.SetLength(5000);

try ArrayDotProduct(a1, a2, -1, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try ArrayDotProduct(a1, a2, 0, -1, 1); except on e: Exception do PrintLn(e.Message); end;
try ArrayDotProduct(a1, a2, 5000, 0, 1); except on e: Exception do PrintLn(e.Message); end;
try ArrayDotProduct(a1, a2, 0, 5000, 1); except on e: Exception do PrintLn(e.Message); end;
try ArrayDotProduct(a1, a2, 4999, 0, 2); except on e: Exception do PrintLn(e.Message); end;
try ArrayDotProduct(a1, a2, 0, 4999, 2); except on e: Exception do PrintLn(e.Message); end;
try ArrayDotProduct(a1, a2, 0, 0, -1); except on e: Exception do PrintLn(e.Message); end;
