PrintLn(DateToISO8601(0));
PrintLn(DateTimeToISO8601(0));

var dt := EncodeDate(2011, 8, 16)+EncodeTime(11, 53, 37, 123);

PrintLn(DateTimeToISO8601(dt));
PrintLn(DateToISO8601(dt));