var n := NaN;
var minf := -Infinity;
var pinf := Infinity;

PrintLn(n);
PrintLn(minf);
PrintLn(pinf);
PrintLn(0*minf);
PrintLn(1/pinf);
PrintLn(minf+pinf);
PrintLn(IsNan(n));
PrintLn(IsNan(1));
PrintLn(IsInfinite(n));
PrintLn(IsInfinite(pinf));
PrintLn(IsInfinite(2.5));
PrintLn(IsFinite(n));
PrintLn(IsFinite(pinf));
PrintLn(IsFinite(2.5));