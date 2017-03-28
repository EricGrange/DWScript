var f : array of Float;

f.Add(1);
f[0] := 123;
PrintLn(FloatToStr(f[0]));

var a : array [Float] of Float;

a[123] := 456;

PrintLn(FloatToStr(a[123]));

var ff : Float := 1;
ff := Float(3);
PrintLn(ff);