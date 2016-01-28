CleanupGlobalVars;

WriteGlobalVar('Test.Alpha', 1);
WriteGlobalVar('Alpha.Test', 1);
WriteGlobalVar('Test.Beta', 1);

PrintLn(GlobalVarsNames('*').Join(','));

CleanupGlobalVars('Test.*');

PrintLn(GlobalVarsNames('*').Join(','));

WriteGlobalVar('Test.Alpha', 1);
WriteGlobalVar('Alpha.Test', 1);
WriteGlobalVar('Test.Beta', 1);

PrintLn(GlobalVarsNames('*').Join(','));

CleanupGlobalVars('*Alpha*');

PrintLn(GlobalVarsNames('*').Join(','));

