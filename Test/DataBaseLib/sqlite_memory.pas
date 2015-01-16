var db := DataBase.Create('SQLite');

db.Exec('create table test (id INTEGER PRIMARY KEY, val TEXT)');

db.StartTransaction;
for var i := 1 to 4 do
   db.Exec('insert into test (id, val) values (?, ?)', [i, 'v'+i.ToString]);
db.Commit;

db.Exec('insert into test (id, val) values (?, ?)', [5, 'v5']);

var ds := db.Query(#"
   select group_concat(id, ','), group_concat(val, '-')
   from test
   ");
PrintLn(ds.AsString(0));
PrintLn(ds.AsString(1));

db.StartTransaction;

db.Exec('delete from test where id=?', [2]);
ds := nil;
db.Exec('delete from test where id=?', [4]);

db.Commit;

ds := db.Query(#"
   select group_concat(id, ','), group_concat(val, '-')
   from test
   ");
PrintLn(ds.AsString(0));
PrintLn(ds.AsString(1));

db.StartTransaction;

db.Exec('delete from test where id=?', [3]);
ds := db.Query(#"
   select group_concat(id, ','), group_concat(val, '-')
   from test
   ");
PrintLn(ds.AsString(0));

db.Rollback;

ds := db.Query(#"
   select group_concat(id, ','), group_concat(val, '-')
   from test
   ");
PrintLn(ds.AsString(0));

PrintLn(db.Query('select count(*) from test where id=?', [1]).AsInteger(0));

PrintLn(db.Query('select count(*) from test where id=?').AsInteger(0));


