var s : Variant = 'dummy';
var i : Variant = 12;

case s of
   'mummy' : PrintLn('bug');
   'dummy' : PrintLn('ok');
else
   PrintLn('bug');
end;

case i of
   10 : PrintLn('bug');
   11..12 : PrintLn('ok');
else
   PrintLn('bug');
end;
