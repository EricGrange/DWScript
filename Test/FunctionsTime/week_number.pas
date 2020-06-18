var dt = EncodeDate(2010, 10, 25);

PrintLn(DateToWeekNumber(dt));
PrintLn(DateToYearOfWeek(dt));

dt := EncodeDate(2010, 12, 31);

PrintLn(WeekNumber(dt));
PrintLn(YearOfWeek(dt));

dt := EncodeDate(2011, 1, 2);

PrintLn(WeekNumber(dt));
PrintLn(YearOfWeek(dt));

dt := EncodeDate(2011, 1, 3);

PrintLn(WeekNumber(dt));
PrintLn(YearOfWeek(dt));

dt := EncodeDate(2007, 12, 31);

PrintLn(WeekNumber(dt));
PrintLn(YearOfWeek(dt));

dt := EncodeDate(2008, 1, 1);

PrintLn(WeekNumber(dt));
PrintLn(YearOfWeek(dt));

