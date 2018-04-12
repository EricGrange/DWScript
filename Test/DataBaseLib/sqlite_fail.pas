var db := DataBase.Create('SQLite', [':memory:']);

procedure PrintExcept(e : Exception);
begin
    PrintLn(e.Message.Before(' using') + e.Message.After(' -'));
end;

try
	db.Exec('');
except
	on e : Exception do
		PrintExcept(e);
end;

try
	db.Exec('bug');
except
	on e : Exception do
		PrintExcept(e);
end;

try
	db.Query('');
except
	on e : Exception do
		PrintExcept(e);
end;

try
	db.Query('nope');
except
	on e : Exception do
		PrintExcept(e);
end;

try
	db.Query('select ?', [db]);
except
	on e : Exception do
		PrintExcept(e);
end;

if db.InTransaction then
	PrintLn('bug tx');
try
	db.Commit;
except
	on e : Exception do
		PrintExcept(e);
end;

db.StartTransaction;
if not db.InTransaction then
	PrintLn('rebug tx');
db.Rollback;

