const ONE = 1;
const TWO = ONE + ONE;
const MINUS_ONE = -ONE;

procedure PrintOut(value : Integer = MINUS_ONE);
begin
   PrintLn(IntToStr(value));
end;

PrintOut();
PrintOut(TWO+MINUS_ONE-ONE);
PrintOut(ONE);
PrintOut(TWO);
PrintOut($100);