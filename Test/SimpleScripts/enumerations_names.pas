type
   TEnum = (enOne, enTwo, enThree);

PrintLn(TEnum.enOne.Name);
PrintLn(enTwo.Name);
PrintLn(TEnum(Ord(enTwo)+1).Name);

for var e in TEnum do
	PrintLn(e.Name);
	
type
   TDisj = (d0 = 0, d2 = 2, d4 = 4, d6 = 6, d8 = 8, d10 = 10, d12 = 12);
   
for var i:=0 to 12 step 3 do begin
	var e : TDisj;
	e := TDisj(i);
	PrintLn(e.Name);
end;