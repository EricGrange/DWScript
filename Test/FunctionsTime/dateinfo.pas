procedure PrintDate(s : String; dt : Float);
begin
   PrintLn(s+' '+FormatDateTime('yyyy-mm-dd', dt));
end;

var dt = EncodeDate(2011, 10, 25);

PrintDate('>', dt);
PrintDate('IncMonth 1', IncMonth(dt, 1));
PrintDate('IncMonth 3', IncMonth(dt, 3));
PrintDate('FirstDayOfYear', FirstDayOfYear(dt));
PrintDate('FirstDayOfNextYear', FirstDayOfNextYear(dt));
PrintDate('FirstDayOfMonth', FirstDayOfMonth(dt));
PrintDate('FirstDayOfNextMonth', FirstDayOfNextMonth(dt));
PrintDate('FirstDayOfNextMonth 2', FirstDayOfNextMonth(IncMonth(dt, 2)));
PrintDate('FirstDayOfWeek', FirstDayOfWeek(dt));
PrintLn('YearOf '+YearOf(dt).ToString);
