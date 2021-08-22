var db := new DataBase('SQLite');

db.Exec('create table test (a)');

for var i := 1 to 20 do begin
   db.Exec('insert into test values (?)', [ i ]);

   var tab := TabularData.CreateFromDataSet(db.Query('select * from test'));

   PrintLn(tab.EvaluateAggregate('sum', [ '"a"', i.ToString, '*' ]));
end;

