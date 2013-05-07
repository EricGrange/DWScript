var db := DataBase.Create('GUID');
var query := db.Query('select * from dual');

for var i := 0 to query.FieldCount-1 do begin
	var fld := query.Fields[i];
	var fldName := fld.Name;
	Print(fld.Name);
	Print(': ');
	PrintLn(fld.DeclaredType);
	fld := query.FieldByName(fldName);
	if (fld=nil) or (fld.Name<>fldName) then
		PrintLn('FieldByName failed');
end;

if query.AsString('GUID').Length>query.AsString('GUID32').Length then
	PrintLn('Length passed');
	
var guid := query.AsString(0);

query.Next;

if guid<>query.AsString(0) then
	PrintLn('Changed passed');
