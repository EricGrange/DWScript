var db := new DataBase('SQLite');

PrintLn(db.Query("select hamming_distance(4, 1)").AsString(0));
PrintLn(db.Query("select hamming_distance(4, 5)").AsString(0));

PrintLn(db.Query("select hamming_distance(4, 1.5)").AsString(0));

PrintLn(db.Query("select hamming_distance(x'FF00', x'FFFF')").AsString(0));
PrintLn(db.Query("select hamming_distance(x'', x'')").AsString(0));
PrintLn(db.Query("select hamming_distance(x'FF', x'FFFF')").AsString(0));

PrintLn(db.Query("select hamming_distance('karolin', 'kathrin')").AsString(0));
PrintLn(db.Query("select hamming_distance('karolin', 'kathrine')").AsString(0));
PrintLn(db.Query("select hamming_distance('2173896', '2233796')").AsString(0));
PrintLn(db.Query("select hamming_distance(2173896, 2233796)").AsString(0));