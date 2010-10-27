var x : ComVariant = DispCallProxy;

PrintLn('Naturals');
PrintLn(x.methodCall(1, 'string', 3.14, True));

PrintLn('OleCasts');
PrintLn(x.methodCall(OleInt32(1), OleInt64(2), OleDate(1)));
