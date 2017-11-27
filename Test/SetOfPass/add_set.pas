type TEnum = (one, two);
type TSet = set of TEnum;

var s1 : TSet = [one];
var s2 : TSet = [two];
var s : TSet;

PrintLn(Integer(s));

s := s1 + s1;
PrintLn(Integer(s));

s := s1 + s2;
PrintLn(Integer(s));

s := s2 + s2;
PrintLn(Integer(s));
