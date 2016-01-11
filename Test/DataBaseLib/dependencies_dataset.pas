function ReturnDSet1 : DataSet;
begin
	var db := DataBase.Create('SQLite');
	var ds := db.Query('select 1');
	exit ds;
end;

function ReturnDSet2 : DataSet;
begin
	var db := DataBase.Create('SQLite');
	var ds := db.Query('select 2');
	db := nil;
	exit ds;
end;

function ReturnDSet3 : DataSet;
begin
	exit DataBase.Create('SQLite').Query('select 3');
end;

PrintLn(ReturnDSet1.StringifyAll);
PrintLn(ReturnDSet2.StringifyAll);
PrintLn(ReturnDSet3.StringifyAll);