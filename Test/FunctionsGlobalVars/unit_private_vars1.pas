unit unit_private_vars1;


procedure PrepareTest;
begin
    CleanupPrivateVars;
    
    WritePrivateVar('test', 'hello');   
end;

procedure RunTestA;
begin
    PrintLn(ReadPrivateVar('test', 123));
    PrintLn(ReadPrivateVar('test2', 123));
    PrintLn(PrivateVarsNames('*2').Join(','));
    PrintLn(PrivateVarsNames('test*').Join(','));
end;

procedure RunTestB;
begin
    CleanupPrivateVars('t*');
    PrintLn(ReadPrivateVar('test', 456));
end;
