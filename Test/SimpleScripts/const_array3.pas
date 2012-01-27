type TEnum = (en1, en2, en3);

const ac : array [TEnum] of String = ['one', 'two', 'three'];

const ac2 = ac;

var i : TEnum;

for i in TEnum do
   PrintLn(ac[i]);
   
for i in TEnum do
   PrintLn(ac2[i]);