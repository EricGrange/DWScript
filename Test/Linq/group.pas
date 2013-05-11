uses System.Data;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);

var ds := from DB.TBL1
          group by value_2
          order by value_1
          select max(id);
while not ds.eof do
begin
   PrintLn(ds.FieldByName('max(id)').AsString);
   ds.next;
end;
ds.free;
