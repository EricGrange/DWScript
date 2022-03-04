type TEnum = (One, Two);

var a : array [TEnum, 1..2] of String;
a[One, 1] := 'One1';
a[One, 2] := 'One2';
a[Two, 1] := 'Two1';
a[Two, 2] := 'Two2';

var b : array [1..2, TEnum] of String;
b[1, One] := 'One_1';
b[2, One] := 'One_2';
b[1, Two] := 'Two_1';
b[2, Two] := 'Two_2';

for var e in TEnum do for var i := 1 to 2 do
   PrintLn(a[e, i]);

for var e in TEnum do for var i := 1 to 2 do
   PrintLn(b[i, e]);
   