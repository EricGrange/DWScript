type TEnum = (one, two);
type TSet = set of TEnum;

var s : TSet = [one, two];

PrintLn(Integer(s - [one]));

PrintLn(Integer(s - [two]));

PrintLn(Integer(s - [one, two]));

PrintLn(Integer(s - []));

PrintLn(Integer(s - [one] - [two]));

s := [one];

PrintLn(Integer([one, two] - s));

s := [two];

PrintLn(Integer([one, two] - s));
