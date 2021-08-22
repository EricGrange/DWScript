var db := new DataBase('SQLite');

db.Exec('create table test (a, b, c)');
db.Exec('insert into test values (1.0, 2, 2.5)');

var tab := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'nojit' ]);

procedure Test(ops : array of String);
begin
   PrintLn(ops.Join(','));
   PrintLn(tab.EvaluateAggregate('sum', ops));
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

Test([ '"a"', '"b"', '>=' ]);
Test([ '"b"', '"a"', '>=' ]);
Test([ '"b"', '0', '>=' ]);
Test([ '"b"', '-1', '>=' ]);
Test([ '"b"', '3', '>=' ]);

