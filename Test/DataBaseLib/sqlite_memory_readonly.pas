var db := DataBase.Create('SQLite', ['file:?mode=memory', 'open_uri']);
db.Exec('create table test(id integer)');

db := DataBase.Create('SQLite', [':memory:']);
db.Exec('create table test(id integer)');

db := DataBase.Create('SQLite', [':memory:', 'read_only']);
try
   db.Exec('create table test(id integer)');
except
   on E: Exception do
      PrintLn(E.Message.Between(' - ', ','));
end;


