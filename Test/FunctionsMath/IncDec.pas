var a = 1;

Inc(a, 3);
if a<>4 then PrintLn('4 expected');
Dec(a, 2);
if a<>2 then PrintLn('2 expected');

Inc(a);
if a<>3 then PrintLn('3 expected');
Dec(a);
if a<>2 then PrintLn('2 expected (bis) ');

