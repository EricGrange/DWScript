CleanupGlobalVars;

WriteGlobalVar('Test.Alpha', 1);
WriteGlobalVar('Alpha.Test', 1);
WriteGlobalVar('Test.Beta', 1);

PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));

CleanupGlobalVars('Test.*');

PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));

WriteGlobalVar('Test.Alpha', 1);
WriteGlobalVar('Alpha.Test', 1);
WriteGlobalVar('Test.Beta', 1);

PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));

CleanupGlobalVars('*Alpha*');

PrintLn(GlobalVarsNames('*').Sort(CompareStr).Join(','));

