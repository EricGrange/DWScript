type TEnum = (
	zzero deprecated, 
	zero = 0, 
	One = 1, 
	deux deprecated 'use two' = 2, 
	three = 3, 
	two = 2
	);

var z := zzero;
var b := deux;

PrintLn(Ord(z));
PrintLn(Ord(b));

z := zero;
b := two;

PrintLn(Ord(z));
PrintLn(Ord(b));

