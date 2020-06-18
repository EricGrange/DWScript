var ut := UnixTime;
var lt := UnixTimeToLocalDateTime(ut);

PrintLn(Abs(LocalDateTimeToUnixTime(lt) - ut) <= 1);

var utc := LocalDateTimeToUTCDateTime(lt);

PrintLn(Abs(UTCDateTimeToLocalDateTime(utc)-lt)*864e5 < 1);

PrintLn((utc*86400 - ut).ToString(1)); 

const c17_12_2018_UnixTime = 1545008400; // 2018-12-17 GMT +1
const c14_08_2018_UnixTime = 1534212000; // 2018-08-14 GMT +2

PrintLn(Round(86400*(UnixTimeToLocalDateTime(c17_12_2018_UnixTime) - (EncodeDate(2018, 12, 17) + 2/24))));
PrintLn(Round(86400*(UnixTimeToLocalDateTime(c14_08_2018_UnixTime) - (EncodeDate(2018, 8, 14) + 4/24))));
