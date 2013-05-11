uses System.Data;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);

var ds := from DB.TBL1
          select max(id);
PrintLn(ds.FieldByName('max(id)').AsString);
ds.free;
