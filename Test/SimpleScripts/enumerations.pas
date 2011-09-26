
type TWeekdays = (Monday, Tuesday, Wednesday,
  Thursday, Friday, Saturday, Sunday);

var weekDay: TWeekdays;

weekDay := TWeekdays.Wednesday;

case weekDay of
  Monday: PrintLn('Monday');
  Tuesday: PrintLn('Tuesday');
  Wednesday: PrintLn('Wednesday');
  Thursday: PrintLn('Thursday');
  Friday: PrintLn('Friday');
  Saturday: PrintLn('Saturday');
  TWeekdays.Sunday: PrintLn('Sunday');
end;

PrintLn(Integer(Saturday));

type TMonths = (
  January = 1,
  February = 2,
  March = 3,
  April = 4,
  May = 5,
  June = 6,
  July = 7,
  August = 8,
  September = 9,
  October = 10,
  November = 11,
  December = 12,
  FirstMonth = 1,
  SecondMonth = 2);

Print(Ord(Monday));
Print('..');
PrintLn(Ord(Sunday));
