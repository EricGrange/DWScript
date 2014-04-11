type TEnum = (one, two);
type TSet = set of TEnum;

var s : TSet = [one, two];

var e : TEnum;

for e in s do
   PrintLn(;
   
