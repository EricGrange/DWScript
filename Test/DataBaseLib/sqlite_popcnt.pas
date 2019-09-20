var db := new DataBase('SQLite');

PrintLn(db.Query("select bit_popcount(4)").AsString(0));
PrintLn(db.Query("select bit_popcount(5)").AsString(0));

PrintLn(db.Query("select bit_popcount(1.5)").AsString(0));

PrintLn(db.Query("select bit_popcount(x'FF00FF00FF00FF00FF00')").AsString(0));
PrintLn(db.Query("select bit_popcount(x'FFFFFFFFFFFFFFFFFFFF')").AsString(0));

PrintLn(db.Query("select bit_popcount(x'')").AsString(0));
PrintLn(db.Query("select bit_popcount(x'0FFF')").AsString(0));

PrintLn(db.Query("select bit_popcount('foo')").AsString(0));
PrintLn(db.Query("select bit_popcount(2173896)").AsString(0));