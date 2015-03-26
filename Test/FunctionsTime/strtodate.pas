FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

var dtl := StrToDate('2010-12-23', DateTimeZone.Local);
var dtu := StrToDate('2010-12-23', DateTimeZone.UTC);

PrintLn(DateToStr(dtl, DateTimeZone.Local));
PrintLn(DateToStr(dtu, DateTimeZone.UTC));

FormatSettings.ShortDateFormat := 'yy-m-d';

PrintLn(DateToStr(StrToDate('2013-09-30')));
PrintLn(DateToStr(StrToDate('2014-12-24')));
PrintLn(DateToStr(StrToDate('2015-3-5')));
