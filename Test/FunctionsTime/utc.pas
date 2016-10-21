FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

var delta := StrToDate('2000-01-01', DateTimeZone.Local)-StrToDate('2000-01-01', DateTimeZone.UTC);

if delta=0 then
	PrintLn('UTC test cannot be performed, local timezone should not be UTC');

// check delta is the same for all functions

PrintLn(StrToDateDef('2010-01-01', 0, DateTimeZone.Local)-StrToDateDef('2010-01-01', 0, DateTimeZone.UTC)-delta);
PrintLn(StrToDateTime('2010-01-01 12:34:56', DateTimeZone.Local)-StrToDateTime('2010-01-01 12:34:56', DateTimeZone.UTC)-delta);
PrintLn(StrToDateTimeDef('2010-01-01 12:34:56', 0, DateTimeZone.Local)-StrToDateTimeDef('2010-01-01 12:34:56', 0, DateTimeZone.UTC)-delta);

// roundtrip vs UTC delta second accuracy as time format does not include milliseconds

var t := Now;

PrintLn( Abs( StrToDateTime(DateTimeToStr(t, DateTimeZone.UTC), DateTimeZone.Local)-t-2*delta )*86400 <1 );
PrintLn( Abs( StrToDateTime(DateTimeToStr(t, DateTimeZone.Local), DateTimeZone.UTC)-t+2*delta )*86400 <1 );