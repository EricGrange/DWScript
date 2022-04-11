
WriteGlobalVar('alpha', 10);
WriteGlobalVar('beta', 20, 0.001);
WriteGlobalVar('gamma', 30, 0.001);
WriteGlobalVar('delta', 40);

PrintLn(IncrementGlobalVar('alpha', 1, 0.001));
PrintLn(IncrementGlobalVar('beta', 1));
PrintLn(IncrementGlobalVar('gamma', 1, 0.001));
PrintLn(IncrementGlobalVar('delta', 1));

Sleep(10);

PrintLn(ReadGlobalVarDef('alpha', 2));
PrintLn(ReadGlobalVarDef('beta', 3));
PrintLn(ReadGlobalVarDef('gamma', 4));
PrintLn(ReadGlobalVarDef('delta', 5));
