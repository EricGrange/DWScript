procedure PrintOut(bool : Boolean);
begin
   case bool of
      False : PrintLn('False');
      True : PrintLn('True');
   end;
end;

var one : String = 'one';
var two : String = 'two';

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

