unit unit_private_vars2;


procedure PrepareTest;
begin
    CleanupPrivateVars;
    
    WritePrivateVar('test', 'world');   
end;

procedure RunTestA;
begin
    PrintLn(ReadPrivateVar('test', 789));
    PrintLn(ReadPrivateVar('test2', 789));
    PrintLn(PrivateVarsNames('*2').Join(','));
    PrintLn(PrivateVarsNames('test*').Join(','));
end;

procedure RunTestB;
begin
    CleanupPrivateVars('t*');
    PrintLn(ReadPrivateVar('test', 147));
end;
