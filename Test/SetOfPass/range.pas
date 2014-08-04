type TMy = set of (A, B, C);

var v1 : TMy := [A..C];

PrintLn(A in v1);
PrintLn(B in v1);
PrintLn(C in v1);

v1 := [A..A, C..C];

PrintLn(A in v1);
PrintLn(B in v1);
PrintLn(C in v1);
