uses System.Data;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);

for var i := 1 to 2 do
begin
   var ds := from DB.TBL1
             join TBL2 on TBL2.T1_ID = TBL1.ID
             where TBL1.ID = i
             select TBL1.*, TBL2.NAME;
   PrintLn(format('%d, %d, %s', [ds.FieldByName('VALUE_1').AsInteger, ds.FieldByName('VALUE_2').AsInteger, ds.FieldByName('NAME').AsString]));
   ds.free;
end;