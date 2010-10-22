procedure Test(const a : array of const);
var
   i : Integer;
begin
   PrintLn('Length = '+IntToStr(Length(a)));
   for i:=Low(a) to High(a) Do
      PrintLn(a[i]);
end;

Test([4,5,6]);
Test([1,'one',true,1.1]);
Test([]);
