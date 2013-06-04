uses System.Data;

function processor(ds: Dataset): integer;
begin
   result := ds.FieldByName('VALUE_1').AsInteger;
end;

function processor2(ds: Dataset): array of integer;
begin
   var value := ds.FieldByName('VALUE_1');
   while ds.step do
      result.add(value.AsInteger);
end;

var db := DataBase.Create('SQLite', ['.\Linq\linqtest.s3db']);

var values := from DB.TBL1
              order by ID
              select VALUE_1
              into @processor;
for var value in values do
   printLn(value);

PrintLn('');

var values2 := from DB.TBL1
               order by ID
               select VALUE_1
               into processor2;
for var value2 in values2 do
   printLn(value2);
