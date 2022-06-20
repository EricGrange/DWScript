type TE1 = enum(val1, val2);
type TE2 = TE1;

var a = TE1.val1;
var b = TE2.val2;

PrintLn(a);
PrintLn(b);