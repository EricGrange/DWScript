const INPUT = '[{"Value": 0}, {"Value": 1}, {"Value": 2}]';

var JsonObj = JSON.Parse(INPUT);

var value := from JsonObj
             where Value = 1;

PrintLn(JSON.Stringify(value));