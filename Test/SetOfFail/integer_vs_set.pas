type TEnum = (one = 1, fifty = 50);
type TSet = set of TEnum;

var s : TSet;

var i := Integer(s);

s := TSet(i);
