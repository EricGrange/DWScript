var v : Variant;

for var i := -1 to 1 do begin
   PrintLn(i.ToString + ': ' + (v = i).ToString);
   var f = i * 0.5;
   PrintLn(f.ToString(1) + ': ' + (v = f).ToString);
end;

PrintLn('"": ' + (v = '').ToString);
PrintLn('"hello": ' + (v = 'hello').ToString);

PrintLn('Null: ' + (v = Null).ToString);
PrintLn('nil: ' + (v = nil).ToString);
PrintLn('Unassigned: ' + (v = Unassigned).ToString);
