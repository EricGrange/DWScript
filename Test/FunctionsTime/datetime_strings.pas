var d = EncodeDate(2011, 2, 21) + EncodeTime(13, 34, 45, 567);

if Abs(StrToDate(DateToStr(d))-Int(d))<>0 then Print('Date roundtrip failed');
if Abs(StrToDateDef(DateToStr(d), 0)-Int(d))<>0 then Print('Date roundtrip 2 failed');
if StrToDateDef('dummy', 1)<>1 then Print('DateDef failed');

if Abs(StrToTime(TimeToStr(d))-Frac(d))>1/3600/24 then Print('Time roundtrip failed');
if Abs(StrToTimeDef(TimeToStr(d), 0)-Frac(d))>1/3600/24 then Print('Time roundtrip 2 failed');
if StrToTimeDef('dummy', 1)<>1 then Print('TimeDef failed');

if Abs(StrToDateTime(DateTimeToStr(d))-d)>1/3600/24 then Print('DateTime roundtrip failed');
if Abs(StrToDateTimeDef(DateTimeToStr(d), 0)-d)>1/3600/24 then Print('DateTime roundtrip 2 failed');
if StrToDateTimeDef('dummy', 1)<>1 then Print('DateTimeDef failed');
