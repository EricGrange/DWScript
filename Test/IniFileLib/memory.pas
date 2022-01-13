var ini := TIniFile.CreateInMemory('[test]'#10'alpha=beta');

PrintLn(ini.ReadSectionNames('test').Join(','));
PrintLn(ini.ReadString('test', 'alpha'));

ini.WriteString('yo', 'aze', 'rty');

PrintLn(ini.ToString);
PrintLn('done');