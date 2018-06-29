PrintLn(GetHostByAddr('127.0.0.1'));
PrintLn(GetHostByAddr('::1'));

if GetHostByAddr('1.1.1.1') = '1.1.1.1' then 
    PrintLn('Failed 1.1.1.1')
else PrintLn('1.1.1.1 passed');

PrintLn(GetHostByAddr('invalid ip address'));
