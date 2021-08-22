var db := new DataBase('SQLite');

db.Exec('create table test (a, b, c)');

db.Exec("insert into test values ('a1', 'b1', 'c1')");
db.Exec("insert into test values ( '',  'b2', 'c1')");
db.Exec("insert into test values ('a2',  '',  'c1')");
db.Exec("insert into test values ('a1', 'b3',   '')");
db.Exec("insert into test values ('a1',   '', 'c1')");
db.Exec("insert into test values ('a2',   '',  '')");
db.Exec("insert into test values (  '', 'b4',   '')");

var tabNoJIT := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'nojit' ]);
var tabJIT := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

var fields = [ 
	'"a" 10 {"a1":1,"a2":2}', 
	'"b" 100 {"b1":107,"b2":113,"b3":119,"b4":123}', 
	'"c" 1000 {"c1":10000}'
	];
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
