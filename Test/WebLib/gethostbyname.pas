PrintLn(GetHostByName('localhost'));

if GetHostByName('www.google.com') = 'www.google.com' then 
    PrintLn('Failed www.google.com')
else PrintLn('www.google.com passed');

PrintLn(GetHostByName('invalid name test'));
