var db := DataBase.Create('SQLite');

var ds := db.Query(#'
	select 
		?, ?, ?, 
		?, ?,
		?, ?,
		?
	', [
		123456789123456789, 1.5, 'hello',
		True, False,
		Null, BlobParameter('world'),
		DateParameter(42000)
	]);

PrintLn(ds.Fields[0].DataType.Name);
PrintLn(ds.AsInteger(0));
PrintLn(ds.Fields[1].DataType.Name);
PrintLn(ds.AsFloat(1));
PrintLn(ds.Fields[2].DataType.Name);
PrintLn(ds.AsString(2));

PrintLn(ds.Fields[3].DataType.Name);
PrintLn(ds.AsInteger(3));
PrintLn(ds.Fields[4].DataType.Name);
PrintLn(ds.AsInteger(4));

PrintLn(ds.Fields[5].DataType.Name);
PrintLn(ds.IsNull(5));
PrintLn(ds.Fields[6].DataType.Name);
PrintLn(ds.AsString(6));

PrintLn(ds.Fields[7].DataType.Name);
PrintLn(ds.AsString(7));
