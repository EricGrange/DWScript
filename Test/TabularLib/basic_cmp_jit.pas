var db := new DataBase('SQLite');

db.Exec('create table test (a, b, c)');
db.Exec('insert into test values (1.0, 2, 2.5)');

var tab1 := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

for var i := 2 to 100 do
	db.Exec('insert into test values (?, ?, ?)', [ Float(i), 2*i, 2*i + 0.5 ]);

var tab100 := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

procedure Test(ops : array of String);
begin
   PrintLn(ops.Join(','));
   PrintLn(tab1.EvaluateAggregate('sum', ops).ToString(5));
   if 'exp' in ops then
	  PrintLn('N/A')
   else
      PrintLn(tab100.EvaluateAggregate('sum', ops).ToString(5));
end;

Test([ '"a"', '"b"', '>=' ]);
Test([ '"b"', '"a"', '>=' ]);
Test([ '"b"', '0', '>=' ]);
Test([ '"b"', '-1', '>=' ]);
Test([ '"b"', '3', '>=' ]);
Test([ '"b"', '2', '=' ]);
Test([ '"b"', '3', '=' ]);
Test([ '"b"', '2', '<>' ]);
Test([ '"b"', '3', '<>' ]);

