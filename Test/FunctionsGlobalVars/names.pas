CleanupGlobalVars;

PrintLn(GlobalVarsNamesCommaText);
PrintLn(GlobalVarsNames('*').Join(','));
PrintLn(GlobalVarsNames('h*').Join(','));

WriteGlobalVar('hello', 'world');

PrintLn(GlobalVarsNamesCommaText);
PrintLn(GlobalVarsNames('*').Join(','));
PrintLn(GlobalVarsNames('h*').Join(';'));

WriteGlobalVar('Hello', 'world');
WriteGlobalVar('Byebye', 'world');

PrintLn(GlobalVarsNamesCommaText);
PrintLn(GlobalVarsNames('*').Join(','));
PrintLn(GlobalVarsNames('h*').Join(';'));

IncrementGlobalVar('hi', 123);

PrintLn(GlobalVarsNamesCommaText);
PrintLn(GlobalVarsNames('*').Join(','));
PrintLn(GlobalVarsNames('h*').Join(';'));

DeleteGlobalVar('hello');

PrintLn(GlobalVarsNamesCommaText);
PrintLn(GlobalVarsNames('*').Join(','));
PrintLn(GlobalVarsNames('h*').Join(';'));
