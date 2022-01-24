var db := new DataBase('SQLite');

db.Exec('create table test (a)');
db.Exec('insert into test values (1.0)');

var tab1 := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

for var i := 2 to 100 do
	db.Exec('insert into test values (?)', [ Float(i) ]);

var tab100 := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

procedure Test(ops : array of Variant);
begin
   PrintLn(ops.Map(lambda (e) => String(e)).Join(','));
   PrintLn(tab1.EvaluateAggregate('sum', ops).ToString(5));
   PrintLn(tab100.EvaluateAggregate('sum', ops).ToString(5));
end;

Test([ '"a"', 'dup', 'smape' ]);
Test([ 2, '"a"', 'smape' ]);
Test([ 2, '"a"', 'sqr', 'smape' ]);


