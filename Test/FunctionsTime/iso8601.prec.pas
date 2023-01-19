
var dt := EncodeDate(2011, 8, 16)+EncodeTime(11, 53, 00, 123);

PrintLn(DateTimeToISO8601(dt));
PrintLn(DateTimeToISO8601(dt, 'sec'));
PrintLn(DateTimeToISO8601(dt, 'msec'));