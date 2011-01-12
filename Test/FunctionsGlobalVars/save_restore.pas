var backup = SaveGlobalVarsToString;

WriteGlobalVar('test', 'hello world');
WriteGlobalVar('testNum', 123);

var backup2 = SaveGlobalVarsToString;

PrintLn(ReadGlobalVar('test'));
PrintLn(ReadGlobalVar('testNum'));

LoadGlobalVarsFromString(backup);

PrintLn(ReadGlobalVar('test'));
PrintLn(ReadGlobalVar('testNum'));

LoadGlobalVarsFromString(backup2);

PrintLn(ReadGlobalVar('test'));
PrintLn(ReadGlobalVar('testNum'));

LoadGlobalVarsFromString('');

try
   LoadGlobalVarsFromString('Z');
except
   on E: Exception do PrintLn(E.Message);
end;
