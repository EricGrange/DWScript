var b := if False then True;
PrintLn(b);

var i := if b then 1;
var f := if b then Pi;

PrintLn(IntToStr(i));

PrintLn(FloatToStr(f));

PrintLn((if b then 'bug')+'!');