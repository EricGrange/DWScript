var db := new DataBase('SQLite');

db.Exec('create table test (a, b)');
db.Exec('insert into test values (1.0, 2)');
db.Exec('insert into test values (3, 4)');
db.Exec('insert into test values (5, 7.5)');
db.Exec('insert into test values (7, 11.25)');

var tab := TabularData.CreateFromDataSet(db.Query('select * from test'));

PrintLn(tab.ExportToSeparated(['a', 'b']));

tab.EvaluateNewColumn('c', [ '"a"', '"b"', '+' ]);
tab.EvaluateNewColumn('d', [ '"b"', '"a"', '-' ]);
tab.EvaluateNewColumn('e', [ '"c"', '"d"', '*' ]);

tab.DropColumn('d');

PrintLn(tab.ExportToSeparated(['a', 'b', 'c', 'e']));
