PrintLn(StrMatches('hello', 'h*o'));
PrintLn(StrMatches('hello', 'h??o'));
PrintLn(StrMatches('helo', 'h*o'));
PrintLn(StrMatches('helo', 'h??o'));

var s : String;

PrintLn(s.Matches('h*o'));
s := 'ho';
PrintLn(s.Matches('h*o'));
s := 'hello';
PrintLn(s.Matches('h*o'));
