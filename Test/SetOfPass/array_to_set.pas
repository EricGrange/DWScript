type TElem = (A = 1, B = 64, C = 128);
type TMy = set of TElem;

const v : TMy = [A, C];

PrintLn(A in v);
PrintLn(B in v);
PrintLn(C in v);

