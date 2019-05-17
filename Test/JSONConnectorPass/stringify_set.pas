type TEnum = (one = 5, two = 2222);
type TSet = set of TEnum;

var e : TEnum;
var s1, s2 : TSet;
s1 := [one];
s1.Include(TEnum(3));
s2 := [two];

PrintLn(JSON.Stringify(s1));
PrintLn(JSON.Stringify(s1 + s2));

type TEnum2 = enum (one, two, three);
var e2 : set of TEnum2;

e2 := [ TEnum2.one, TEnum2.three ];
PrintLn(JSON.Stringify(e2));