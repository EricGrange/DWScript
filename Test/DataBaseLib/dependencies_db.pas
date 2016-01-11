function ReturnDB1 : Database;
begin
	var db := DataBase.Create('SQLite');
	exit db;
end;

function ReturnDB2 : Database;
begin
	exit DataBase.Create('SQLite');
end;

PrintLn(ReturnDB1.ClassName);

PrintLn(ReturnDB2.ClassName);