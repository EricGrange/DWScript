var db := new DataBase('SQLite', [ 'file:dummy.bin?mode=memory' ]);

db.Exec('drop table if exists test');
db.Exec('create table test (a integer)');
PrintLn(db.Query('select count(*) from test').AsInteger(0));

db := new DataBase('SQLite', [ 'file:dummy2.bin?mode=memory&immutable=1' ]);
try
   db.Exec('create table test (a integer)');
except
   on E: Exception do
      PrintLn(E.Message.Before(' using'));
end;

