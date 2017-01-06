var db := DataBase.Create('SQLite', [':memory:']);

try
	db.Exec('');
except
	on e : Exception do
		PrintLn(e.Message);
end;

try
	db.Exec('bug');
except
	on e : Exception do
		PrintLn(e.Message);
end;

try
	db.Query('');
except
	on e : Exception do
		PrintLn(e.Message);
end;

try
	db.Query('nope');
except
	on e : Exception do
		PrintLn(e.Message);
end;

try
	db.Query('select ?', [db]);
except
	on e : Exception do
		PrintLn(e.Message);
end;

if db.InTransaction then
	PrintLn('bug tx');
try
	db.Commit;
except
	on e : Exception do
		PrintLn(e.Message);
end;

db.StartTransaction;
if not db.InTransaction then
	PrintLn('rebug tx');
db.Rollback;

