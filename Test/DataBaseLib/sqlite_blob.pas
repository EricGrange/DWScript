var db := new DataBase('SQLite');

var ds := db.Query("select x'CAFEBABE' c, ? p", [ BlobParameter(#0#1#2) ]);

procedure PrintBlob(b : String);
begin
   for var i := 1 to b.Length do
      Print(IntToHex(Ord(b[i]), 2));
   PrintLn('');
end;

PrintBlob(ds.AsBlob(0));
PrintBlob(ds.AsBlob('p'));
PrintBlob(ds.Fields[0].AsBlob);

PrintLn(BlobHexParameter('000102') = BlobParameter(#0#1#2));
PrintLn(db.Query("select hex(?) || hex(?)",
                 [ BlobHexParameterDef('a', #0#1#2), BlobHexParameterDef('0a', #0#1#2)]).AsString(0));
