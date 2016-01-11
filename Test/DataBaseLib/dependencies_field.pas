function ReturnField1 : DataField;
begin
	var db := DataBase.Create('SQLite');
	var ds := db.Query('select 1 a');
	var fld := ds.FieldByName('a');
	exit fld;
end;

function ReturnField2 : DataField;
begin
	var db := DataBase.Create('SQLite');
	var ds := db.Query('select 2 a');
	db := nil;
	var fld := ds.FieldByName('a');
	ds := nil;
	exit fld;
end;

function ReturnField3 : DataField;
begin
	var db := DataBase.Create('SQLite');
	var ds := db.Query('select 3 a');
	var fld := ds.FieldByName('a');
	ds := nil;
	db := nil;
	exit fld;
end;

function ReturnField4 : DataField;
begin
	var db := DataBase.Create('SQLite');
	var ds := db.Query('select 4 a');
	var fld := ds.FieldByName('a');
	db := nil;
	ds := nil;
	exit fld;
end;

function ReturnField5 : DataField;
begin
	exit DataBase.Create('SQLite').Query('select 5 a').FieldByName('a');
end;

PrintLn(ReturnField1.AsString);
PrintLn(ReturnField2.AsString);
PrintLn(ReturnField3.AsString);
PrintLn(ReturnField4.AsString);
PrintLn(ReturnField5.AsString);