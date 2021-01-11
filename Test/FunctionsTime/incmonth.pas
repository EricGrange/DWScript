FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
var d := StrToDateTime('2019-01-31 01:30:00');
PrintLn(d);
for var i := 1 to 2 do begin
   PrintLn('---');
   PrintLn(DateTimeToStr(d));
   PrintLn(DateTimeToStr(IncMonth(d, 1))); // beware 29/02 !
   PrintLn(DateTimeToStr(IncMonth(d, 2))); // DST unsupported !!
   PrintLn(DateTimeToStr(IncMonth(d, 3))); // DST unsupported !!
   FormatSettings.Zone := DateTimeZone.UTC;
end;
