function DigitInInteger(d : Integer; n : Integer) : Boolean;
begin
   Result:=Pos(IntToStr(d), IntToStr(n))>0;
end;
function DigitInFloat(d : Integer; n : Float) : Boolean;
begin
   Result:=Pos(IntToStr(d), FloatToStr(n))>0;
end;

operator in (Integer, Integer) : Boolean uses DigitInInteger;
operator in (Integer, Float) : Boolean uses DigitInFloat;

PrintLn(1 in 123);
PrintLn(4 in 123);
PrintLn(5 in 123.5);
PrintLn(4 in 123.5);
PrintLn(1 not in 123);
PrintLn(4 not in 123);
PrintLn(5 not in 123.5);
PrintLn(4 not in 123.5);