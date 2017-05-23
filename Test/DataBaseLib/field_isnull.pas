var db := new DataBase('SQLite');

PrintLn(db.Query('select 0').IsNull(0));
PrintLn(db.Query('select 1').IsNull(0));
PrintLn(db.Query('select null').IsNull(0));

PrintLn(db.Query('select 0').Fields[0].IsNull);
PrintLn(db.Query('select 1').Fields[0].IsNull);
PrintLn(db.Query('select null').Fields[0].IsNull);