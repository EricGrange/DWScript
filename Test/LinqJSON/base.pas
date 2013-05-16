const INPUT = '[{"Value": 0}, {"Value": 1}, {"Value": 2}]';

var JsonObj = JSON.Parse(INPUT);

var value := from JsonObj;

PrintLn(JSON.Stringify(value));