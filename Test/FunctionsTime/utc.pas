FormatSettings.ShortDateFormat := 'yyyy-mm-dd';

var delta := StrToDate('2000-01-01', DateTimeZone.Local)-StrToDate('2000-01-01', DateTimeZone.UTC);

if delta=0 then
	PrintLn('UTC test cannot be performed, local timezone should not be UTC');

// check delta is the same for all functions

PrintLn(StrToDateDef('2010-01-01', 0, DateTimeZone.Local)-StrToDateDef('2010-01-01', 0, DateTimeZone.UTC)-delta);
PrintLn( Round( 1000*( StrToDateTime('2010-01-01 12:34:56', DateTimeZone.Local)-StrToDateTime('2010-01-01 12:34:56', DateTimeZone.UTC)-delta )) );
PrintLn( Round( 1000*( StrToDateTimeDef('2010-01-01 12:34:56', 0, DateTimeZone.Local)-StrToDateTimeDef('2010-01-01 12:34:56', 0, DateTimeZone.UTC)-delta)) );

// roundtrip vs UTC delta second accuracy as time format does not include milliseconds

var t := EncodeDate(2017, 1, 1, DateTimeZone.Local);
delta := t-EncodeDate(2017, 1, 1, DateTimeZone.UTC);

PrintLn( Abs( StrToDateTime(DateTimeToStr(t, DateTimeZone.UTC), DateTimeZone.Local)-t-delta )*86400 <1 );
PrintLn( Abs( StrToDateTime(DateTimeToStr(t, DateTimeZone.Local), DateTimeZone.UTC)-t+delta )*86400 <1 );

t := EncodeDate(2016, 8, 1, DateTimeZone.Local);
delta := t-EncodeDate(2016, 8, 1, DateTimeZone.UTC);

PrintLn( Abs( StrToDateTime(DateTimeToStr(t, DateTimeZone.UTC), DateTimeZone.Local)-t-delta )*86400 <1 );
PrintLn( Abs( StrToDateTime(DateTimeToStr(t, DateTimeZone.Local), DateTimeZone.UTC)-t+delta )*86400 <1 );

