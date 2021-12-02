var db := new DataBase('SQLite');

db.Exec('create table test (a)');
for var i := 1 to 100 do
	db.Exec('insert into test (a) values (?)', [ i/100 ]);

var tab := TabularData.CreateFromDataSet(db.Query('select a from test'), [ 'nojit' ]);

PrintLn(tab.EvaluateAggregate('sum', [ '"a"', 'round' ]).ToString(3));
PrintLn(tab.EvaluateAggregate('sum', [ '"a"', 'floor' ]).ToString(3));
PrintLn(tab.EvaluateAggregate('sum', [ '"a"', 'ceil' ]).ToString(3));

tab := TabularData.CreateFromDataSet(db.Query('select a from test'), [ 'jit' ]);

PrintLn(tab.EvaluateAggregate('sum', [ '"a"', 'round' ]).ToString(3));
PrintLn(tab.EvaluateAggregate('sum', [ '"a"', 'floor' ]).ToString(3));
PrintLn(tab.EvaluateAggregate('sum', [ '"a"', 'ceil' ]).ToString(3));

