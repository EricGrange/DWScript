uses System.Data;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);

for var i := 1 to 4 do
begin
   var ds := from DB.TBL1
             where ID = i
             select VALUE_1, VALUE_2;
   PrintLn(format('%d, %d', [ds.FieldByName('VALUE_1').AsInteger, ds.FieldByName('VALUE_2').AsInteger]));
   ds.free;
end;