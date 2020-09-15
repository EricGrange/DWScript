var db := new DataBase('SQLite');

PrintLn(db.Query("select base64(null)").IsNull(0));
PrintLn(db.Query("select base64(1)").AsString(0));
PrintLn(db.Query("select base64(1.0)").AsString(0));
PrintLn(db.Query("select base64('hello')").AsString(0));
PrintLn(db.Query("select base64(x'01')").AsString(0));
PrintLn(db.Query("select base64(x'010203040506')").AsString(0));
