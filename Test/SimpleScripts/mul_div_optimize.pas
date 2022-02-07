var a := 1.5;
var b := 2.5;

PrintLn(a * a); // should optimize to Sqr(a)

PrintLn(a / (a / b));  // = b
a := 0;
PrintLn(IsNaN(a / (a / b)));  // True
