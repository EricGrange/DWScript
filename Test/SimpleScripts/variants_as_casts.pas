var v : Variant;
var s : String;
var f : Float;
var i : Integer;
var b : Boolean;

s:='abcd';
v:=s;
PrintLn(v as String);

f:=1.234;
v:=f;
PrintLn(v as Float);
PrintLn(v as Float + 1.5);

i:=1234;
v:=i;
PrintLn(v as Integer);
PrintLn(v as Integer + 1);
PrintLn(v as Float + 1);

b:=True;
v:=b;
PrintLn(v as Boolean);
v := not b;
PrintLn(v as Boolean);
PrintLn(v as Integer);