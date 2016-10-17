CleanupGlobalVars;

PrintLn(GlobalVarsNamesCommaText.Split(',').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('h*').Sort(CompareStr).Join(','));

WriteGlobalVar('hello', 'world');

PrintLn(GlobalVarsNamesCommaText.Split(',').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('h*').Sort(CompareStr).Join(';'));

WriteGlobalVar('Hello', 'world');
WriteGlobalVar('Byebye', 'world');

PrintLn(GlobalVarsNamesCommaText.Split(',').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('h*').Sort(CompareStr).Join(';'));

IncrementGlobalVar('hi', 123);

PrintLn(GlobalVarsNamesCommaText.Split(',').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('h*').Sort(CompareStr).Join(';'));

DeleteGlobalVar('hello');

PrintLn(GlobalVarsNamesCommaText.Split(',').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));
PrintLn(GlobalVarsNames('h*').Sort(CompareStr).Join(';'));
