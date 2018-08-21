var db := DataBase.Create('GUID');
var query := db.Query('select * from dual');

if query.FindField('bug')<>nil then 
	PrintLn('bug');
	
try
	query.FieldByName('bug');
except
   on E: Exception do begin
		PrintLn(E.ClassName);
		PrintLn(E.Message);
	end;
end;

try
	query.AsInteger('wtf');
except
   on E: Exception do begin
		PrintLn(E.ClassName);
		PrintLn(E.Message);
	end;
end;


