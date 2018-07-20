var v : Variant;
var s : String;
var f : Float;
var i : Integer;
var b : Boolean;

s:='abcd';
v:=s;
PrintLn(v);
v:='efgh';
s:=v;
PrintLn(s);
PrintLn(v);

f:=1.234;
v:=f;
PrintLn(v);
v:=5.678;
f:=v;
PrintLn(FloatToStr(f));
PrintLn(FloatToStr(v));

i:=1234;
v:=i;
PrintLn(v);
v:=5678;
i:=v;
PrintLn(IntToStr(i));
PrintLn(IntToStr(v));

b:=True;
v:=b;
PrintLn(v);
v:=False;
b:=v;
PrintLn(b);
PrintLn(v);

v := '123.5';
PrintLn(FloatToStr(v));
v := '12,35';
PrintLn(FloatToStr(v));


