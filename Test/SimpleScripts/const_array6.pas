type
   TDayTable = array[1..12] of Integer;
const
  { True=Leapyear }
   cMonthDays: array [boolean] of TDayTable =
     ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
      (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));

for var i := 1 to 12 do
   PrintLn(cMonthDays[False, i].ToString+' vs '+cMonthDays[True][i].ToString);