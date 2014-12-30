CleanupGlobalVars;

PrintLn(ReadGlobalVar('test'));
PrintLn(CompareExchangeGlobalVar('test', 2, 1));
PrintLn(ReadGlobalVar('test'));

WriteGlobalVar('test', 1);

PrintLn(ReadGlobalVar('test'));
PrintLn(CompareExchangeGlobalVar('test', 2, 1));
PrintLn(ReadGlobalVar('test'));

PrintLn(CompareExchangeGlobalVar('test', 'alpha', 2));

PrintLn(CompareExchangeGlobalVar('test', 'alpha', 2));
PrintLn(CompareExchangeGlobalVar('test', 'beta', 'alpha'));

PrintLn(ReadGlobalVar('test'));
