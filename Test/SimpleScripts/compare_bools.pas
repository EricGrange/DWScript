var a = True;
var b = False;

if (a=b) then PrintLn('True = False');
if (a<>b) then PrintLn('True <> False');
if (b=a) then PrintLn('False = True');
if (b<>a) then PrintLn('False <> True');

a := b;

if (a=b) then PrintLn('False = False');
if (a<>b) then PrintLn('False <> False');

