var ut := UnixTime;
var lt := UnixTimeToLocalDateTime(ut);

PrintLn(Abs(LocalDateTimeToUnixTime(lt) - ut) <= 1);

var utc := LocalDateTimeToUTCDateTime(lt);

PrintLn(Abs(UTCDateTimeToLocalDateTime(utc)-lt)*864e5 < 1);

PrintLn(utc*86400 - ut); 

