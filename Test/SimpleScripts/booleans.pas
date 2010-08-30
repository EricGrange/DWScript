procedure PrintOut(bool : Boolean);
begin
   if bool then
      PrintLn('True')
   else PrintLn('False');
end;

var t : Boolean = True;
var f : Boolean = False;

PrintLn(t and f);
PrintLn(t or f);
PrintLn(t xor f);
PrintLn(t xor t);
PrintLn(not t);

PrintLn('');

PrintLn(t and (f or t or f) and (t xor f) and not f);
