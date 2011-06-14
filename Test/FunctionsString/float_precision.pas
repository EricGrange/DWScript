var f1 := 123.456789;
var f2 := 567.890123;

var i : Integer;

PrintLn(FloatToStr(f1));
for i:=-2 to 5 do
   PrintLn(FloatToStr(f1, i));

PrintLn(FloatToStr(f2));
for i:=-2 to 5 do
   PrintLn(FloatToStr(f2, i));