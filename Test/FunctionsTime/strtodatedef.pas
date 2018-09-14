FormatSettings.ShortDateFormat := 'dd/mm/yyyy';

var a,b,c,d : float;
var myDate:='06/01/2017';
a:=StrToDateTime(myDate);
b:=StrToDateTimeDef(myDate,0);
PrintLn(DateToStr(a));
PrintLn(Round(b));

FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

var ds := '2010-12-23';

var dtl := StrToDateTimeDef(ds, 1, DateTimeZone.Local);
var dtu := StrToDateTimeDef(ds, 2, DateTimeZone.UTC);

PrintLn(DateToStr(dtl, DateTimeZone.Local));
PrintLn(DateToStr(dtu, DateTimeZone.UTC));

FormatSettings.ShortDateFormat := 'yy-m-d';

ds := '2013-09-30 12:34';
a := StrToDateTimeDef(ds, 3);
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss', a));
PrintLn(DateToStr(a));

ds := '2014-12-24 12:34';
PrintLn(DateToStr(StrToDateTimeDef(ds, 4)));
ds := '2015-3-5 21:42';
PrintLn(DateToStr(StrToDateTimeDef(ds, 5)));



