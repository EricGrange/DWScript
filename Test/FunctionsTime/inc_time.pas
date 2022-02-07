var dt := EncodeDateTime(2022, 1, 3, 12, 23, 34, 456); 

PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', dt));

PrintLn('year');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncYear(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncYear(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncYear(dt, -123)));

PrintLn('month');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMonth(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMonth(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMonth(dt, -123)));

PrintLn('week');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncWeek(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncWeek(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncWeek(dt, -123)));

PrintLn('day');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncDay(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncDay(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncDay(dt, -123)));

PrintLn('hour');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncHour(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncHour(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncHour(dt, -123)));

PrintLn('minute');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMinute(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMinute(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMinute(dt, -123)));

PrintLn('second');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncSecond(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncSecond(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncSecond(dt, -123)));

PrintLn('millisecond');
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMilliSecond(dt, 1)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMilliSecond(dt, 123)));
PrintLn(FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', IncMilliSecond(dt, -123)));
