var db := new DataBase('SQLite');

db.Exec('create table test (a, b, c)');

db.Exec("insert into test values (1, 2, 3)");
db.Exec("insert into test values ('', 1, 2)");
db.Exec("insert into test values (3, '', 4)");
db.Exec("insert into test values (5, 6, '')");
db.Exec("insert into test values ('', '', 7)");
db.Exec("insert into test values (8, '', '')");
db.Exec("insert into test values ('', 9, '')");

var tabNoJIT := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'nojit' ]);
var tabJIT := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

var fields = [ '"a" 10', '"b" 100', '"c" 1000' ];
var operators = [ '+', '*', 'max', 'min', 'relu' ];

for var i := 1 to 100 do begin

   var ops : array of String = [];
   var n := 2 + i mod 20;
   for var k := 1 to n do
      ops.Add(fields[(i*11 + k*13) mod fields.Length]);
   for var k := 1 to n-1 do
      ops.Add(operators[(i*257 + k*17) mod operators.Length]);

   var ref := tabNoJIT.EvaluateAggregate('sum', ops).ToString;
   var eval := tabJIT.EvaluateAggregate('sum', ops).ToString;
   if eval <> ref then begin
      PrintLn('At step ' + i.ToString);
      PrintLn(ops.Join(','));
      PrintLn('expected ' + ref);
      PrintLn('but got  ' + eval);
      exit;
   end;

end;

PrintLn('Passed');
