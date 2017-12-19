var db := new DataBase('SQLite');

PrintLn(db.Query("select median(value) from json_each('[1,2,4,5]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[5,4,1,2]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[5,4,1]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[1,5,4]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[1,4,5]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[1,4]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[4,1]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[1]')").AsString(0));
PrintLn(db.Query("select median(value) from json_each('[]')").IsNull(0));
PrintLn(db.Query("select median(value) from json_each('[null]')").IsNull(0));
PrintLn(db.Query("select median(value) from json_each('[null,3]')").AsString(0));
