var ini := TIniFile.CreateInMemory('[test]'#10'alpha=beta');

ini.WriteString('test2', 'alpha', 'gamma');
ini.WriteString('test3', 'alpha', 'delta');

PrintLn(ini.ReadSections.Join(','));

PrintLn(ini.ReadString('test', 'alpha'));
PrintLn(ini.ReadString('test2', 'alpha'));
PrintLn(ini.ReadString('test3', 'alpha'));

ini.DeleteKey('test', 'alpha');

PrintLn(ini.ReadSections.Join(','));

PrintLn(ini.ReadString('test', 'alpha'));
PrintLn(ini.ReadString('test2', 'alpha'));
PrintLn(ini.ReadString('test3', 'alpha'));

ini.EraseSection('test2');

PrintLn(ini.ReadString('test', 'alpha'));
PrintLn(ini.ReadString('test2', 'alpha'));
PrintLn(ini.ReadString('test3', 'alpha'));

PrintLn(ini.ReadSections.Join(','));

