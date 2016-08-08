var db := new DataBase('SQLite');

var ds := db.Query('select * from (select 1 f) where f = 0');
PrintLn(ds.Eof);
PrintLn(ds.StringifyAll);