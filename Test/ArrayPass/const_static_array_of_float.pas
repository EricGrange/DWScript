const c : array [1..3]  of Float = ( 0.5, 2, 4.5 );

for var i := 1 to 3 do
   PrintLn(FloatToStr(c[i], 1));