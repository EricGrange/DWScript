var ut := UnixTime;
var t := UTCDateTime;

PrintLn((t-25569)*86400-ut <= 1);

PrintLn(UnixTimeToDateTime(ut)-t <= 1);

PrintLn(DateTimeToUnixTime(t)-ut <= 1);