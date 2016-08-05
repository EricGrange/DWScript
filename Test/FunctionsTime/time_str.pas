var t := StrToTime('12:24:36');

var s := TimeToStr(t);
PrintLn(s);

PrintLn(t = StrToTimeDef(s, 12345));

PrintLn(StrToTimeDef('_'+s, 12345));