var db := new DataBase('SQLite');

var ds := db.Query('select * from (select 1 f) where f = 0');
PrintLn(ds.Eof);
PrintLn(ds.StringifyAll);

try
	PrintLn(ds.IsNull(0));
except
end;
try
	PrintLn(ds.AsInteger(0));
except
end;
try
	PrintLn(ds.AsString(0));
except
end;
try
	PrintLn(ds.AsFloat(0));
except
end;
try
	PrintLn(ds.AsBlob(0));
except
end;
try
	PrintLn('"'+ds.Fields[0].DeclaredType+'"');
except
end;
