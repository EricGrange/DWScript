var v : Variant;

PrintLn(v ?? 1);

v := Null;
PrintLn(v ?? 2);
PrintLn(v ?? Null ?? 2);

v := 'hello';
PrintLn(v ?? 2);

v := 0.0;
PrintLn(v ?? 3);
v := 0.5;
PrintLn(v ?? 3);

v := 0;
PrintLn(v ?? 4);
v := 123;
PrintLn(v ?? 4);

v := False;
PrintLn(v ?? 5);
v := True;
PrintLn(v ?? 5);
