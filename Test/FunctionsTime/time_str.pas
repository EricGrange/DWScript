var t := StrToTime('12:24:36');
PrintLn(t.ToString(5));

var s := TimeToStr(t);
PrintLn(s);

PrintLn(t = StrToTimeDef(s, 12345));

PrintLn(StrToTimeDef('_'+s, 12345));

FormatSettings.LongTimeFormat := 'h.n.s.z';
PrintLn(TimeToStr(t));

FormatSettings.LongTimeFormat := 'h/n ampm';
PrintLn(TimeToStr(t));
PrintLn(TimeToStr(t-0.5));