type TEnum = (One = 1);

var a := TEnum(1);
var b := TEnum(One);
var c := TEnum(TEnum(One));
var d := TEnum(TEnum(1));

Print(Ord(a));
Print(Ord(b));
Print(Ord(c));
Print(Ord(d));

const ca = TEnum(1);
const cb = TEnum(One);
const cc = TEnum(TEnum(One));
const cd = TEnum(TEnum(1));

Print(Ord(ca));
Print(Ord(cb));
Print(Ord(cc));
PrintLn(Ord(cd));

