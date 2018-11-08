var a := 0;
var b := 4294967296;
var c := 1000000000000;

PrintLn('Min');
PrintLn(Min(a, b));
PrintLn(Min(a, c));
PrintLn(Min(b, c));
PrintLn(Min(b, a));
PrintLn(Min(c, a));
PrintLn(Min(c, b));

PrintLn('Max');
PrintLn(Max(a, b));
PrintLn(Max(a, c));
PrintLn(Max(b, c));
PrintLn(Max(b, a));
PrintLn(Max(c, a));
PrintLn(Max(c, b));