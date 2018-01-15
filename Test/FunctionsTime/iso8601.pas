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
PrintLn(Format('%.5f', [ISO8601ToDateTime('1977-04-22 01:00:00-05:00')]) = Format('%.5f', [ISO8601ToDateTime('1977-04-22T06:00:00Z')]));
PrintLn(Format('%.5f', [ISO8601ToDateTime('19770422T010000+0500')]) = Format('%.5f', [ISO8601ToDateTime('1977-04-21T20:00:00z')]));
PrintLn(Format('%.5f', [ISO8601ToDateTime('1977-04-22t0100-05')]) = Format('%.5f', [ISO8601ToDateTime('1977-04-22T06:00:00Z')]));

