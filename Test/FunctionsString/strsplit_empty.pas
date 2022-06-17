procedure PrintArray(a : array of String);
begin
   PrintLn(a.Length.ToString + ': "' + a.Join(',') + '"');
end;

var s := 'hello';

PrintArray(s.Split(''));
PrintArray(s.Split('z'));
PrintArray(s.Split('l'));
PrintArray(s.Split('ll'));
PrintArray(s.Split('zz'));

PrintLn('---');

s := '';

PrintArray(s.Split(''));
PrintArray(s.Split('z'));
PrintArray(s.Split('l'));
PrintArray(s.Split('ll'));
PrintArray(s.Split('zz'));
