var db := new DataBase('SQLite');

PrintLn(db.Query("select null as A, null as B").Stringify);
PrintLn(db.Query("select 1 as A, 'two' as B union all select null, null").StringifyAll);




