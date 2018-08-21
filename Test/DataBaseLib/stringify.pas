var db := new DataBase('SQLite');

PrintLn(db.Query("select 1, 'two', 1.5, null").Stringify);

PrintLn(db.Query("select 1 a, 'hello' TXT union all select 2 a, 'World' TXT").StringifyAll);

db.SetLowerCaseStringify(True);
PrintLn(db.Query("select 1 a, 'hello' TXT union all select 2 a, 'World' TXT").StringifyAll);

db.SetLowerCaseStringify(False);
PrintLn(db.Query("select 1 a, 'hello' TXT union all select 2 a, 'World' TXT").StringifyAll(1));

PrintLn(db.Query("select 1 a, 'hello' TXT union all select 2 a, 'World' TXT").StringifyMap(2));
