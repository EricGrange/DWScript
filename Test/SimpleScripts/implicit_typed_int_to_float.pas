type TMyInt = Integer;

var i : TMyInt = 1;

PrintLn(FloatToStr(i, 1));

PrintLn(FloatToStr(TMyInt(2), 2));
PrintLn(FloatToStr(i+i+i, 3));