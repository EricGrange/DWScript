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

Test([ '"a"', '"b"', '+', '"c"', '+' ]);
Test([ '"a"', '"b"', '-', '"c"', '-' ]);
Test([ '"a"', '"b"', '*', '"c"', '*' ]);
Test([ '"a"', '"b"', '/', '"c"', '/' ]);
Test([ '"a"', '0.5', '+' ]);
Test([ '"a"', '0.5', '-' ]);
Test([ '"a"', '0.5', '*' ]);
Test([ '"a"', '2', '*' ]);
Test([ '"a"', '0.5', '/' ]);
Test([ '"a"', '3', '*', '0.1', '+' ]);

Test([ '"a"', 'ln' ]);

Test([ '"a"', 'exp' ]);

Test([ '"a"', 'abs' ]);
Test([ '"a"', '-1', '*', 'abs' ]);

Test([ '"a"', '"b"', 'min' ]);
Test([ '"a"', '"b"', 'max' ]);

Test([ '"b"', 'sqr' ]);
Test([ '"b"', 'sqr', 'sqrt' ]);

Test([ '"a"', '"b"', 'relu' ]);
Test([ '"a"', '-10', 'relu' ]);

