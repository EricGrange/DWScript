var db := new DataBase('SQLite');

db.Exec('create table test (a, b)');
for var i := 1 to 5 do
   db.Exec('insert into test values (?, ?)', [ i, StringOfChar('a', i) ]);

var tab := TabularData.CreateFromDataSet(db.Query('select * from test'));

PrintLn(tab.ColumnNames.Join(' '));
PrintLn(tab.Evaluate([ '"a"', '2', '*' ]).Map(FloatToStr).Join(' '));
PrintLn(tab.ColumnStrings('a').Join(' '));
PrintLn(tab.ColumnStrings('b').Join(' '));

