var s : Variant = 'mummy';
var i : Variant = 10;

var vs := 'dummy';

case vs of
   s : PrintLn('bug');
   Variant('dummy') : PrintLn('ok');
else
   PrintLn('bug');
end;

var vi := 12;

case vi of
   i : PrintLn('bug');
   (i+1)..12 : PrintLn('ok');
else
   PrintLn('bug');
end;
