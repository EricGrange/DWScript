procedure PrintOut(bool : Boolean);
begin
   case bool of
      False : PrintLn('False');
      True : PrintLn('True');
   end;
end;

var one : Integer = 1;
var two : Integer = 2;

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

