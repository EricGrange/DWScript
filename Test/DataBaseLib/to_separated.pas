var db := new DataBase('SQLite');

PrintLn(db.Query('select 1 as a, 2.5 as b').ToSeparated);
PrintLn(db.Query('select 1 as ''he"llo''').ToSeparated);
PrintLn(db.Query("select 1 as 'hel,lo'").ToSeparated);
PrintLn(db.Query("select ? as abc", [ 'bye'#9'bye' ]).ToSeparated);
PrintLn(db.Query(#'
   select 1 as a, 2 as b
   union all select 3, 4
   ').ToSeparated);
PrintLn(db.Query(#'
   select 1 as a, 2 as b
   union all select 3, 4
   ').ToSeparated(1));

PrintLn(db.Query('select 123, 456').ToSeparated(0, ';', '*'));



