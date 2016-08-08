var db := new DataBase('SQLite');

try
	db.Query('select json(?)', ['bug']);
except
	on E : Exception do
		PrintLn(E.Message.After('- '));
end;
