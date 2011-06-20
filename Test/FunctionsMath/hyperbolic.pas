procedure PrintIt(v : Float);
begin
   PrintLn(FloatToStr(v, 8));
end;

var v = 1.5;

PrintIt(Sinh(v));
PrintIt(Cosh(v));
PrintIt(Tanh(v));
PrintIt(ArcCosh(v));
PrintIt(ArcSinh(v));
PrintIt(ArcTanh(v/2));

