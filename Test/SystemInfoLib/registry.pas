PrintLn('Initial:');
PrintLn(Registry.DeleteKey(HKEY.CurrentUser, '\Software\DWScript\Test'));
PrintLn(Registry.SubKeys(HKEY.CurrentUser, '\Software\DWScript').Join(','));
PrintLn(Registry.SubKeys(HKEY.CurrentUser, '\Software\DWScript\Test').Join(','));

PrintLn('Create Keys:');
PrintLn(Registry.CreateKey(HKEY.CurrentUser, '\Software\DWScript\Test'));
PrintLn(Registry.CreateKey(HKEY.CurrentUser, '\Software\DWScript\Test\Sub'));
PrintLn(Registry.SubKeys(HKEY.CurrentUser, '\Software\DWScript').Join(','));
PrintLn(Registry.SubKeys(HKEY.CurrentUser, '\Software\DWScript\Test').Join(','));

PrintLn('Write Values:');
PrintLn(Registry.WriteValue(HKEY.CurrentUser, '\Software\DWScript\Test', 'Hello', 'World'));
PrintLn(Registry.WriteValue(HKEY.CurrentUser, '\Software\DWScript\Test', '123', 123));
PrintLn(Registry.ValueNames(HKEY.CurrentUser, '\Software\DWScript\').Join(','));

PrintLn('Read Values:');
PrintLn(Registry.ReadValue(HKEY.CurrentUser, '\Software\DWScript\Test', 'Hello', '?'));
PrintLn(Registry.ReadValue(HKEY.CurrentUser, '\Software\DWScript\Test', '123', 456));

PrintLn('Delete Value:');
PrintLn(Registry.DeleteValue(HKEY.CurrentUser, '\Software\DWScript\Test', 'Hello'));
PrintLn(Registry.ValueNames(HKEY.CurrentUser, '\Software\DWScript\Test').Join(','));
PrintLn(Registry.ReadValue(HKEY.CurrentUser, '\Software\DWScript\Test', 'Hello', '?'));

PrintLn('Delete Keys:');
PrintLn(Registry.DeleteKey(HKEY.CurrentUser, '\Software\DWScript\Test'));

PrintLn(Registry.DeleteKey(HKEY.CurrentUser, '\Software\DWScript\Test\Sub'));
PrintLn(Registry.DeleteKey(HKEY.CurrentUser, '\Software\DWScript\Test'));

PrintLn(Registry.ValueNames(HKEY.CurrentUser, '\Software\DWScript\Test').Join(','));
PrintLn(Registry.SubKeys(HKEY.CurrentUser, '\Software\DWScript').Join(','));
PrintLn(Registry.ReadValue(HKEY.CurrentUser, '\Software\DWScript\Test', '123', 456));