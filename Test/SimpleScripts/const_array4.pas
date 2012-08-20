type TEnum = (en1, en2, en3);
type TEnumArray = array [TEnum] of string;

const ac : TEnumArray = ('one', 'two', 'three');

const ac2 : TEnumArray = ac;

var i : TEnum;

for i in TEnum do
   PrintLn(ac[i]);
   
for i in TEnum do
   PrintLn(ac2[i]);