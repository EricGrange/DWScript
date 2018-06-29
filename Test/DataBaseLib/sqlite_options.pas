var db := new DataBase('SQLite');

PrintLn(db.OptionList.Join(','));

PrintLn(db.Options['soft_heap_limit64']);
db.Options['soft_heap_limit64'] := (1024*1024*1024).ToString;
PrintLn(db.Options['soft_heap_limit64']);
db.Options['soft_heap_limit64'] := '0';
PrintLn(db.Options['soft_heap_limit64']);
