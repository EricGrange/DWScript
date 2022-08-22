var a := 0.5;
var b := 1.5;
var c := -0.5;

PrintLn('Min Max');
PrintLn(Min(Max(a, b), c));
PrintLn(Min(a, Max(b, c)));
PrintLn(Min(Max(a, b), Max(b, c)));

PrintLn('Max Min');
PrintLn(Max(Min(a, b), c));
PrintLn(Max(a, Min(b, c)));
PrintLn(Max(Min(a, b), Min(b, c)));
PrintLn(Max(Min(b, a+b), Min(a+c, b+a)));




