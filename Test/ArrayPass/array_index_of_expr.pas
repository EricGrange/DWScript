var a : array of String;
var s : String;

if a.IndexOf('J' + s) >= 0 then PrintLn('bug1');

a.Add('J');

if a.IndexOf('J' + s) <> 0 then PrintLn('bug2');

s:='j';

if a.IndexOf('J' + s) >= 0 then PrintLn('bug3');

a.Add('Jj');

PrintLn(a.IndexOf('J' + s));

s:='';
PrintLn(a.IndexOf(s + 'j'));
s:='J';
PrintLn(a.IndexOf(s + 'j'));
