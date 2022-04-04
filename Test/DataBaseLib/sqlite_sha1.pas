var db := new DataBase('SQLite');

PrintLn(db.Query("select sha1(null)").IsNull(0));
PrintLn(db.Query("select sha1(1)").AsString(0));
PrintLn(db.Query("select sha1(1.0)").AsString(0));
PrintLn(db.Query("select sha1('hello')").AsString(0));
PrintLn(db.Query("select sha1(x'01')").AsString(0));
PrintLn(db.Query("select sha1(x'010203040506')").AsString(0));
