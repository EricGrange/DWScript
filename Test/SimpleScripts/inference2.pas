procedure MyProc;
const
   two = 2;
var
   i : Integer := two;
   j := i;
   iStr = IntToStr(i);
   obj := TObject.Create;
begin
   if i=j then
      PrintLn('match int');

   if iStr='2' then
      PrintLn('match str');
end;

MyProc;
