var db := new DataBase('SQLite');

db.Exec('pragma user_version');

try
	db.Exec("select json('bug')");
except
	on E : Exception do
		PrintLn(E.Message.After('- '));
end;

try
	db.Exec('select json(?)', ['bug']);
except
	on E : Exception do
		PrintLn(E.Message.After('- '));
end;
