var v : Variant;

PrintLn('- Undefined');
PrintLn(v is True);
PrintLn(v is False);

v := Null;
PrintLn('- Null');
PrintLn(v is True);
PrintLn(v is False);

for var  i := -1 to 1 do begin
   v := i;
   PrintLn('- Integer ' + v);
   PrintLn(v is True);
   PrintLn(v is False);
   v := i + 0.5;
   PrintLn('- Float ' + v);
   PrintLn(v is True);
   PrintLn(v is False);
end;

v := '';
PrintLn('- Empty string');
PrintLn(v is True);
PrintLn(v is False);

v := 'true';
PrintLn('- true string');
PrintLn(v is True);
PrintLn(v is False);
