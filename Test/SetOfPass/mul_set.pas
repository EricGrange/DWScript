type TEnum = (one, two);
type TSet = set of TEnum;

var s1 : TSet = [one];
var s2 : TSet = [two];
var s : TSet = [one, two];

PrintLn(Integer(s * s1));

PrintLn(Integer(s * s2));
PrintLn(Integer(s * s));

PrintLn(Integer(s1 * s2));
PrintLn(Integer(s2 * s1));
