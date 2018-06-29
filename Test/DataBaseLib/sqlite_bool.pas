var db := new DataBase('SQLite');

var ds := db.Query('select 1=1 cTrue, 1=0 cFalse, ? pTrue, ? pFalse', [ True, False ]);

for var i := 0 to 3 do
    PrintLn(ds.Fields[i].AsBoolean);

PrintLn(ds.FieldByName('cTrue').AsBoolean);
PrintLn(ds.FieldByName('pFalse').AsBoolean);

