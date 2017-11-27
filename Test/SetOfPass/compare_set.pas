type TEnum = (one, two);
type TSet = set of TEnum;

var s1 : TSet = [one];
var s2 : TSet = [two];
var s : TSet = [one, two];

PrintLn('- equal');
PrintLn(s1 = s1);
PrintLn(s1 = s2);
PrintLn(s2 = s1);
PrintLn(s = (s1 + s2));

PrintLn('- diff');
PrintLn(s1 <> s1);
PrintLn(s1 <> s2);
PrintLn(s2 <> s1);
PrintLn(s <> (s1 + s2));

PrintLn('- left contains right');
PrintLn(s1 <= s1);
PrintLn(s1 <= s2);
PrintLn(s2 <= s1);
PrintLn(s <= (s1 + s2));
PrintLn(s1 <= s);
PrintLn(s <= s1);

PrintLn('- right contains left');
PrintLn(s1 >= s1);
PrintLn(s1 >= s2);
PrintLn(s2 >= s1);
PrintLn(s >= (s1 + s2));
PrintLn(s1 >= s);
PrintLn(s >= s1);

