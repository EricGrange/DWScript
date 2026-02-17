var a : array of String = nil;
var b : array of String = nil;
var o : TObject = nil;

// Unsupported relational operator
PrintLn(a <= b);

// Comparing dynamic array with an incompatible type (object)
PrintLn(a = o);
PrintLn(o = a);
