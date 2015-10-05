var db := DataBase.Create('SQLite');

db.Exec('create table test (id INTEGER PRIMARY KEY, val TEXT, f FLOAT)');

for var i := 1 to 4 do
   db.Exec('insert into test (id, val, f) values (?, ?, ?)', [i, 'v'+i.ToString, i*0.5]);

var ds := db.Query('select * from test order by id asc');

var f1 := ds.FieldByName('id');
var f2 := ds.FieldByName('val');
var f3 := ds.Fields[2];
while ds.Step do begin
	Print(f1.AsInteger);
	Print(f2.AsString);
	PrintLn(f3.AsFloat);
end;

ds := db.Query('select * from test order by id desc');

f3 := ds.Fields[2];
f2 := ds.FieldByName('val');
f1 := ds.FieldByName('id');
while ds.Step do begin
	Print(f1.AsInteger);
	Print(f2.AsString);
	PrintLn(f3.AsFloat);
end;



