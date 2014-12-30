CleanupGlobalVars;

var backup = SaveGlobalVarsToString;

WriteGlobalVar('test', 'hello world');
WriteGlobalVar('testNum', 123);
WriteGlobalVar('testFloat', 2.5);
WriteGlobalVar('testBool', True);

var backup2 = SaveGlobalVarsToString;

PrintLn(ReadGlobalVar('test'));
PrintLn(ReadGlobalVar('testNum'));
PrintLn(ReadGlobalVar('testFloat'));
PrintLn(ReadGlobalVar('testBool'));

LoadGlobalVarsFromString(backup);

PrintLn(ReadGlobalVar('test'));
PrintLn(ReadGlobalVar('testNum'));
PrintLn(ReadGlobalVar('testFloat'));
PrintLn(ReadGlobalVar('testBool'));

LoadGlobalVarsFromString(backup2);

PrintLn(ReadGlobalVar('test'));
PrintLn(ReadGlobalVar('testNum'));
PrintLn(ReadGlobalVar('testFloat'));
PrintLn(ReadGlobalVar('testBool'));

LoadGlobalVarsFromString('');

try
   LoadGlobalVarsFromString('Z');
except
   on E: Exception do PrintLn(E.Message);
end;
