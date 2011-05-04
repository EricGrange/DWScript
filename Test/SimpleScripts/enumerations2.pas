type
   TEnum = (enOne, enTwo, enThree);

var e : TEnum = enTwo;

PrintLn('Ord: '+IntToStr(Ord(e)));
PrintLn('High: '+IntToStr(Ord(High(e))));
PrintLn('Low: '+IntToStr(Ord(Low(e))));

Inc(e); // supported
PrintLn(Ord(e));
Dec(e); // supported
PrintLn(Ord(e));

PrintLn(Succ(e));
PrintLn(Pred(e));

for e:=enOne to enThree do
   Print(e);
PrintLn('');

for e:=Low(TEnum) to High(TEnum) do
   Print(e);
PrintLn('');
