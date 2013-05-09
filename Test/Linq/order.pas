uses System.Data;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);

var ds := from DB.TBL1
          order by VALUE_2, ID desc;
while not ds.EOF do
begin
   PrintLn(format('%d, %d, %d', [ds.FieldByName('ID').AsInteger, ds.FieldByName('VALUE_1').AsInteger, ds.FieldByName('VALUE_2').AsInteger]));
   ds.Next;
end;
ds.free;
