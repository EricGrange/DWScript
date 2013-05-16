const INPUT = '[{"ID": 0, "Value": 0}, {"ID": 1, "Value": 1}, {"ID": 2, "Value": 2}, {"ID": 3, "Value": 0}, {"ID": 4, "Value": 1}, {"ID": 5, "Value": 2}]';

var JsonObj = JSON.Parse(INPUT);

var value := from JsonObj
             group by Value
             order by ID desc;

PrintLn(JSON.Stringify(value));