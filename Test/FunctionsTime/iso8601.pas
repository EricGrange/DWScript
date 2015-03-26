PrintLn(DateToISO8601(0));
PrintLn(DateTimeToISO8601(0));

{$ifdef JS_CODEGEN}
var dt := EncodeDate(2011, 8, 16, DateTimeZone.UTC)+EncodeTime(11, 53, 37, 123);
{$else}
var dt := EncodeDate(2011, 8, 16)+EncodeTime(11, 53, 37, 123);
{$endif}

var dti := DateTimeToISO8601(dt);

PrintLn(dti);

var di := DateToISO8601(dt);

PrintLn(di);

PrintLn(Format('%.1f', [ISO8601ToDateTime(dti)]));
PrintLn(ISO8601ToDateTime(di));