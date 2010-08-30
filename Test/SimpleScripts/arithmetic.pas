
var n, i : Integer;

n:=10;
n:=n*10;

for i:=1 to n do begin

   if (i mod 3)=0 then continue;
   if (i mod 5)=1 then continue;
   if (i mod 7)=0 then continue;

   if i>n div 2 then break;

   PrintLn(IntToStr(i));

end;

n := 142;
PrintLn('142 div 10 = '+IntToStr(142 div 10));
PrintLn('142 mod 10 = '+IntToStr(142 mod 10));

i:=10;
PrintLn(IntToStr(n+i));
PrintLn(IntToStr(n-i));
PrintLn(IntToStr(n+n*i-i));
PrintLn(IntToStr((n+n)*(i-i)));
PrintLn(IntToStr((n+i)*(n-i)));
PrintLn(IntToStr(n+n div i));
PrintLn(IntToStr(n div i+n));

