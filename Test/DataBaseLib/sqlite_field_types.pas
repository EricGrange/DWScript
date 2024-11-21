var db := DataBase.Create('SQLite');

PrintLn(db.Query('select 2').AsInteger(0) + 10);
PrintLn(db.Query("select '3'").AsInteger(0) + 10);

PrintLn(db.Query('select 2').AsString(0) + '1');
PrintLn(db.Query("select 'a'").AsString(0) + '1');
PrintLn(db.Query('select 2.5 as s').AsString('s') + '1');

PrintLn(db.Query('select 0.5').AsFloat(0) + 1);
PrintLn(db.Query('select 1.5 as f').AsFloat('f') + 1);

PrintLn(db.Query('select 0').AsBoolean(0));
PrintLn(db.Query('select 1 as b').AsBoolean('b'));

var b := db.Query("select x'0001FF'").AsBlob(0);
for var i := 1 to b.Length do
   Print(Ord(b[i]).ToHexString(2));
PrintLn('');