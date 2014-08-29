type TEnum = (one, two);
type TSet = set of TEnum;

var s : TSet = [one, two];

PrintLn(Integer(s));

s.Exclude(one);

PrintLn(Integer(s));

s.Exclude(two);

PrintLn(Integer(s));

s.Include(one);

PrintLn(Integer(s));
