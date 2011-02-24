procedure Test(f : Float; i : Integer; s : String);
begin
   PrintLn(FloatToStr(f));
   PrintLn(IntToStr(i));
   PrintLn(s);
   f:=f*i;
   s:=s+s;
   PrintLn(f);
   PrintLn(i);
   PrintLn(s);
end;

var v : Variant;

v:=2;

Test(10, v, v);

v:=3.0;

Test(v, v, v);
