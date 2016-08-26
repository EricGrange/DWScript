type TEnum = (one = 1, two);
type TSet = set of TEnum;

var s : TSet := [one, two];

for var i := -1 to 3 do
	PrintLn(TEnum(i) in s);

type TBigEnum = (ten = 10, thousand = 1000);
type TBigSet = set of TBigEnum;

var bs : TBigSet := [ten, thousand];

for var i := 0 to 4 do
	PrintLn(if TBigEnum(IntPower(10, i)) in bs then i);