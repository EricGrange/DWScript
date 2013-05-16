const INPUT = '[{"Value": 0}, {"Value": 1}, {"Value": 2}]';

function processor(value: JSONVariant): array of integer;
begin
   for var i := 0 to value.high() do
      result.add(JSON.Stringify(value[i].Value).ToInteger);
end;

var JsonObj = JSON.Parse(INPUT);

var values := from JsonObj
              where Value > 0
              order by Value desc
              into @processor;

for var value in values do
   printLn(value);