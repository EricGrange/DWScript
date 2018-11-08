var db := DataBase.Create('SQLite');

var i := 123;
var f := i / 2;

PrintLn(db.Query('select ?, ?, ?', [1, 2, 3]).Stringify);
PrintLn(db.Query('select ?, ?, ?', [true, '02', 3.5]).Stringify);
PrintLn(db.Query('select ?, ?, ?', [BlobHexParameter('212223'), i, f]).Stringify);
PrintLn(db.Query('select ?, ?, ?', [(i + i).ToString, f + f, i = i]).Stringify);
PrintLn(db.Query('select ?, ?, ?', [f * f, i <> i, DateParameter(0)]).Stringify);
