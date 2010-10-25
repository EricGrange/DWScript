var y, m, d : Integer;

DecodeDate(Now, y, m, d);

if FirstDayOfYear(0)<>EncodeDate(y, 1, 1) then PrintLn('FirstDayOfYear');
if FirstDayOfNextYear(0)<>EncodeDate(y+1, 1, 1) then PrintLn('FirstDayOfNextYear');

if MonthOfYear(0)<>m then PrintLn('MonthOfYear');
if DayOfMonth(0)<>d then PrintLn('DayOfMonth');

if FirstDayOfMonth(0)<>EncodeDate(y, m, 1) then PrintLn('FirstDayOfMonth');
if m=12 then begin
   if FirstDayOfNextMonth(0)<>EncodeDate(y+1, 1, 1) then PrintLn('FirstDayOfNextMonth year leap')
end else begin
   if FirstDayOfNextMonth(0)<>EncodeDate(y, m+1, 1) then PrintLn('FirstDayOfNextMonth')
end;

if DayOfTheWeek(FirstDayOfWeek(0))<>1 then PrintLn('FirstDayOfWeek');

if DayOfYear(0)<>DayOfYear(Now) then PrintLn('DayOfYear');

if WeekNumber(0)<>WeekNumber(Now) then PrintLn('WeekNumber');
if YearOfWeek(0)<>YearOfWeek(Now) then PrintLn('YearOfWeek');
