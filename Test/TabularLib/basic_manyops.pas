var db := new DataBase('SQLite');

db.Exec('create table test (a, b)');
for var i := 1 to 100 do
   db.Exec('insert into test values (?, ?)', [ i, i * 1.5 ]);

var tab := TabularData.CreateFromDataSet(db.Query('select * from test'));

var ops : array of String;
for var i := 1 to 20 do begin
   ops.Add('"a"', i.ToString, '*', '"b"', '3', '*', '+');
   if i > 1 then ops.Add('+');
end;

PrintLn('shallow stack');
PrintLn(tab.EvaluateAggregate('sum', ops));

ops.Clear;
for var i := 1 to 20 do
   ops.Add('"a"', i.ToString, '*', '"b"', '3', '*');
for var i := 1 to 20+19 do
   ops.Add('+');
PrintLn('deep stack');
PrintLn(tab.EvaluateAggregate('sum', ops));

