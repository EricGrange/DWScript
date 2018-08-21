var db := DataBase.Create('SQLite');

var ds := db.Query('select 1 a, 2 b');

PrintLn(ds.IndexOfField('a'));
PrintLn(ds.IndexOfField('b'));
PrintLn(ds.IndexOfField('c'));
PrintLn(ds.FindField('a').Name);
PrintLn(ds.FindField('b').Name);
PrintLn(ds.FindField('c')<>nil);