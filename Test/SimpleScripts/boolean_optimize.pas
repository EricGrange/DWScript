procedure PrintBool(v : Variant);
begin
	PrintLn(if v then 'True' else 'False');
end;

var v : Variant := 1;

PrintBool(True and v);
PrintBool(v and True);

PrintBool(False and v);
PrintBool(v and False);

PrintBool(True or v);
PrintBool(v or True);

PrintBool(False or v);
PrintBool(v or False);

v := 0;
PrintLn(v);

PrintBool(True and v);
PrintBool(v and True);

PrintBool(False and v);
PrintBool(v and False);

PrintBool(True or v);
PrintBool(v or True);

PrintBool(False or v);
PrintBool(v or False);
