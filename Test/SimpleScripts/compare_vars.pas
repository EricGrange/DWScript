procedure PrintOut(bool : Boolean);
begin
   case bool of
      False : PrintLn('False');
      True : PrintLn('True');
   end;
end;

var one : Variant = 1.5;
var two : Variant = 2.5;

PrintOut(one<two);
PrintOut(one<=two);
PrintOut(one=two);
PrintOut(one>=two);
PrintOut(one>two);
PrintOut(one<>two);

PrintLn('');

PrintOut(one<one);
PrintOut(one<=one);
PrintOut(one=one);
PrintOut(one>=one);
PrintOut(one>one);
PrintOut(one<>one);

PrintLn('');

PrintOut(two<one);
PrintOut(two<=one);
PrintOut(two=one);
PrintOut(two>=one);
PrintOut(two>one);
PrintOut(two<>one);

