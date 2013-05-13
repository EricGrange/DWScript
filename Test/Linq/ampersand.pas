uses System.Data;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);
var id := 1;

var ds := from DB.TBL1
          where VALUE_2 = id
          select &id;
PrintLn(ds.FieldByName('ID').AsString);
ds.free;
