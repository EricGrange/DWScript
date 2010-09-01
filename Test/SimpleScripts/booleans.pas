procedure PrintOut(bool : Boolean);
begin
   if bool then
      PrintLn('True')
   else PrintLn('False');
end;

var t : Boolean = True;
var f : Boolean = False or False;

PrintLn(t and f);
PrintLn(t or f);
PrintLn(t xor f);
PrintLn(t xor t);
PrintLn(not t);

PrintLn(t and (f or t or f) and (t xor f) and not f);

PrintLn('');

PrintLn('True = '+IntToStr(Integer(t))+' = '+IntToStr(Integer(True)));
PrintLn('False = '+IntToStr(Integer(f))+' = '+IntToStr(Integer(False)));