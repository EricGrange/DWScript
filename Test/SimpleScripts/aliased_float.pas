type TMy = Float;

var v : TMy;
var f : Float;

v:=1.0;
f:=v;
v:=f+v;
PrintLn(v);

if v<1 then PrintLn('bug');

function Hello(a : TMy) : Float;
begin
   Result:=a;
end;

function World(a : Float) : TMy;
begin
   Result:=a;
end;

PrintLn(Hello(123));
PrintLn(World(1234));

PrintLn(Hello(2.0)*World(3.0));