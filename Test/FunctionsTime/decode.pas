var dt : Float;

dt:=EncodeDate(2010, 10, 25);

PrintLn(FormatDateTime('yyyy-mm-dd', dt));

var y, m, d : Integer;

DecodeDate(dt, y, m, d);
PrintLn(Format('%d-%d-%d', [y, m, d]));

const BoolToStr : array [False..True] of String = ['False', 'True'];

PrintLn('DayOfWeek '+IntToStr(DayOfWeek(dt)));
PrintLn('DayOfTheWeek '+IntToStr(DayOfTheWeek(dt)));
PrintLn('DayOfMonth '+IntToStr(DayOfMonth(dt)));
PrintLn('MonthOfYear '+IntToStr(MonthOfYear(dt)));
PrintLn('DayOfYear '+IntToStr(DayOfYear(dt)));
PrintLn('IsLeapYear 2010 '+BoolToStr[IsLeapYear(2010)]);
PrintLn('IsLeapYear 2012 '+BoolToStr[IsLeapYear(2012)]);

dt:=dt+EncodeTime(12, 34, 45, 567);

PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', dt));

var h, n, s, z : Integer;

DecodeTime(dt, h, n, s, z);
PrintLn(Format('%d:%d:%d.%d', [h, n, s, z]));

