var s : String;

PrintLn('a' in 'banana');
PrintLn('z' in 'banana');

PrintLn('a' in s);
s:='hello';
PrintLn('el' in s);

PrintLn(s in 'hello');
PrintLn(s not in 'hello');