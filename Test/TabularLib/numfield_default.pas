var db := new DataBase('SQLite');

db.Exec('create table test (a)');

for var i := 1 to 10 do begin

   db.Exec('insert into test values (?)', [ if Odd(i) then Null else 1 shl i ] );

   var tab := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'nojit' ]);

   Print(tab.EvaluateAggregate('sum', [ '"a" 0.1' ]).ToString(5));
   Print(#9);
   PrintLn(tab.EvaluateAggregate('sum', [ '"a"' ]).ToString(5));

   tab := TabularData.CreateFromDataSet(db.Query('select * from test'), [ 'jit' ]);

   Print(tab.EvaluateAggregate('sum', [ '"a" 0.1' ]).ToString(5));
   Print(#9);
   PrintLn(tab.EvaluateAggregate('sum', [ '"a"' ]).ToString(5));

end;

