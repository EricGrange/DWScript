var db := new DataBase('SQLite');

PrintLn(db.Query("select bool_and(value) from json_each('[1,0]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[1,1]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[1,1,0]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[0,1,1]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[0,0,0]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[0,0]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[1,1]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[0]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[1]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[1,null]')").AsString(0));
PrintLn(db.Query("select bool_and(value) from json_each('[]')").IsNull(0));

PrintLn(db.Query("select bool_or(value) from json_each('[1,0]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[1,1]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[1,1,0]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[0,1,1]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[0,0,0]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[0,0]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[1,1]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[0]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[1]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[1,null]')").AsString(0));
PrintLn(db.Query("select bool_or(value) from json_each('[]')").IsNull(0));
