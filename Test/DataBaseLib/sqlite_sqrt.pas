var db := new DataBase('SQLite');

PrintLn(db.Query("select sqrt(4)").AsString(0));
PrintLn(db.Query("select sqrt(?)", [ 9 ]).AsString(0));
PrintLn(db.Query("select sqrt(?)", [ 56.25 ]).AsString(0));
PrintLn(db.Query("select sqrt(2.25) s").AsFloat('s'));
