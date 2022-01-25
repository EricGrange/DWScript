FormatSettings.Zone := DateTimeZone.Local;

var d = EncodeDate(2011, 2, 21) + EncodeTime(13, 34, 45, 567);

if Abs(StrToDate(DateToStr(d))-Int(d))<>0 then PrintLn('Local Date roundtrip failed');
if Abs(StrToDateDef(DateToStr(d), 0)-Int(d))<>0 then PrintLn('Local Date roundtrip 2 failed');
if StrToDateDef('dummy', 1)<>1 then PrintLn('DateDef failed');

if Abs(StrToTime(TimeToStr(d))-Frac(d))>1/3600/24 then PrintLn('Local Time roundtrip failed');
if Abs(StrToTimeDef(TimeToStr(d), 0)-Frac(d))>1/3600/24 then PrintLn('Local Time roundtrip 2 failed');
if StrToTimeDef('dummy', 1)<>1 then PrintLn('Local TimeDef failed');

if Abs(StrToDateTime(DateTimeToStr(d))-d)>1/3600/24 then PrintLn('Local DateTime roundtrip failed');
if Abs(StrToDateTimeDef(DateTimeToStr(d), 0)-d)>1/3600/24 then PrintLn('Local DateTime roundtrip 2 failed');
if StrToDateTimeDef('dummy', 1)<>1 then PrintLn('Local DateTimeDef failed');

FormatSettings.Zone := DateTimeZone.UTC;

if Abs(UTCDateTimeToLocalDateTime(StrToDate(DateToStr(d)))-Int(d))<>0 then PrintLn('UTC Date roundtrip failed');
if Abs(UTCDateTimeToLocalDateTime(StrToDateDef(DateToStr(d), 0))-Int(d))<>0 then PrintLn('UTC Date roundtrip 2 failed');
if StrToDateDef('dummy', 1)<>1 then PrintLn('UTC DateDef failed');

if Abs(StrToDateTime(DateTimeToStr(d))-d)>1/3600/24 then PrintLn('UTC DateTime roundtrip failed');
if Abs(StrToDateTimeDef(DateTimeToStr(d), 0)-d)>1/3600/24 then PrintLn('UTC DateTime roundtrip 2 failed');
if StrToDateTimeDef('dummy', 1)<>1 then PrintLn('UTC DateTimeDef failed');

(* unstable  improvements TODO 
d := EncodeTime(13, 34, 45, 567);

if Abs(StrToTime(TimeToStr(d))-Frac(d))>1/3600/24 then PrintLn('UTC Time roundtrip failed');
if Abs(StrToTimeDef(TimeToStr(d), 0)-Frac(d))>1/3600/24 then PrintLn('UTC Time roundtrip 2 failed');
if StrToTimeDef('dummy', 1)<>1 then PrintLn('UTC TimeDef failed');
*)


