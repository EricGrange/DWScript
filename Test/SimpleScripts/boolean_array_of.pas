var b : array [Boolean] of Integer;

const s : array [Boolean] of String = ['F', 'T'];

b[False]:=10;
b[True]:=20;

var v := False;

PrintLn(b[1=1]);
PrintLn(b[v]);

PrintLn(s[v]);
PrintLn(s[not v]);
   

